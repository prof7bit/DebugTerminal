{ simple wrapper for serial.pp on unix and implementation of
  a subset of serial.pp for windows, using overlapped IO

  Copyright (C) 2014 Bernd Kreuss <prof7bit@gmail.com>

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General
  Public License as published by the Free Software Foundation; either version 2 of the License, or (at your option)
  any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with
  independent modules to produce an executable, regardless of the license terms of these independent modules,and to
  copy and distribute the resulting executable under terms of your choice, provided that you also meet, for each
  linked independent module, the terms and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this library, you may extend this exception to
  your version of the library, but you are not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more
  details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit ComPort;

{$mode objfpc}{$H+}

interface
uses
  Classes;

type
  { TSimpleComPort }

  TSimpleComPort = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
    function Send(B: Byte): LongInt;
    function Send(var Buffer; Count: LongInt): LongInt;
    function Send(Text: String): LongInt;
    function ReceiveByte(TimeoutMilli: Integer; out RecvByte: Byte): LongInt;
    procedure Close;
  private
    FHandle: THandle;
    FIsOpen: Boolean;
  public
    property IsOpen: Boolean read FIsOpen;
  end;

procedure EnumerateSerialPorts(List: TStrings);

implementation
uses
  sysutils,
  {$ifdef windows}
  windows;
  {$endif}
  {$ifdef unix}
  baseunix, Serial;
  {$endif}


{$ifdef linux}
procedure EnumSerial_Linux(List: TStrings);
var
  SR: TSearchRec;
begin
  begin
    if FindFirst('/sys/class/tty/*', faAnyFile, SR) = 0 then begin
      repeat
        if FileExists('/sys/class/tty/' + SR.Name + '/device/driver') then begin
          List.Append('/dev/' + SR.Name);
        end;
      until FindNext(SR) <> 0;
      FindClose(SR);
    end;
  end;
end;
{$endif}

{$ifdef windows}
procedure EnumSerial_Windows(List: TStrings);
var
  I: Integer;
  FN: String;
  F: THandle;
begin
  for I := 1 to 255 do begin
    FN := '\\.\COM' + IntToStr(I);
    F := CreateFile(PChar(FN), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if GetLastOSError <> ERROR_FILE_NOT_FOUND then
      List.Append('COM' + IntToStr(I));
    if F <> INVALID_HANDLE_VALUE then
      FileClose(F);
  end;
end;
{$endif}

procedure EnumerateSerialPorts(List: TStrings);
begin
  List.BeginUpdate;
  List.Clear;
  {$ifdef linux}
  EnumSerial_Linux(List);
  {$endif linux}
  {$ifdef windows}
  EnumSerial_Windows(List);
  {$endif}
  List.EndUpdate;
end;

{$ifdef unix}
{$if FPC_FULLVERSION < 20700} // old fpc without SerReadTimeout()
function SerReadTimeout(Handle: TSerialHandle; var Buffer; mSec: LongInt): LongInt;
var
  readSet: TFDSet;
  selectTimeout: TTimeVal;
begin
  fpFD_ZERO(readSet);
  fpFD_SET(Handle, readSet);
  selectTimeout.tv_sec := mSec div 1000;
  selectTimeout.tv_usec := (mSec mod 1000) * 1000;
  result := 0;
  if fpSelect(Handle + 1, @readSet, nil, nil, @selectTimeout) > 0 then
    result := fpRead(Handle, Buffer, 1)
end;
{$endif} // old fpc without SerReadTimeout()
{$endif} // unix

{$ifdef windows}
// windows has no working serial.pp and the new implementation in FPC trunk
// does not (yet) use overlapped IO and unfortunately it seems that not using
// overlapped IO for SerReadTimeout() will not work properly, so I have to
// implement the needed functions of serial.pp for windows from scratch.

type
  TSerialHandle = THandle;
  TParityType = (NoneParity, OddParity, EvenParity);
  TSerialFlags = set of (RtsCtsFlowControl);


function SerSetParamsPrivate(Handle: TSerialHandle; BitsPerSec: LongInt;
  ByteSize: Integer; Parity: TParityType; StopBits: Integer;
  Flags: TSerialFlags): boolean;

const   bufSize= 2048;

        dcb_Binary           = $00000001;
        // dcb_Parity           = $00000002;
        dcb_OutxCtsFlow      = $00000004;
        // dcb_OutxDsrFlow      = $00000008;
        // dcb_DtrControl       = $00000030;
        // dcb_DsrSensivity     = $00000040;
        // dcb_TXContinueOnXOff = $00000080;
        // dcb_OutX             = $00000100;
        // dcb_InX              = $00000200;
        // dcb_ErrorChar        = $00000400;
        // dcb_Null             = $00000800;
        dcb_RtsControl       = $00003000;
        // dcb_AbortOnError     = $00004000;

var     DCB: TDCB;

begin
  result := true;
  FillChar(DCB{%H-}, SizeOf(DCB), 0);
  DCB.DCBlength := SizeOf(DCB);
  DCB.XonChar := #17;
  DCB.XoffChar := #19;
  DCB.XonLim := bufSize div 4;
  DCB.XoffLim := 1;
  DCB.Flags := DCB.Flags or dcb_Binary;
  DCB.BaudRate := BitsPerSec;
  DCB.ByteSize := ByteSize;
  case Parity of
    OddParity:  DCB.Parity := Windows.ODDPARITY;
    EvenParity: DCB.Parity := Windows.EVENPARITY
  else
    DCB.Parity := Windows.NOPARITY
  end;
  if StopBits > 1 then
    DCB.StopBits := TWOSTOPBITS
  else
    DCB.StopBits := ONESTOPBIT;
  if RtsCtsFlowControl in Flags then
    DCB.Flags := DCB.Flags or dcb_OutxCtsFlow or
                (dcb_RtsControl and (RTS_CONTROL_HANDSHAKE shl 12));
  if not SetCommState(Handle, DCB) then
    result := false;
  if not SetupComm(Handle, bufSize, bufSize) then
    result := false
end { SerSetParamsPrivate } ;


procedure SerSetParams(Handle: TSerialHandle; BitsPerSec: LongInt;
  ByteSize: Integer; Parity: TParityType; StopBits: Integer;
  Flags: TSerialFlags);

begin
  if SerSetParamsPrivate(Handle, BitsPerSec, ByteSize, Parity, StopBits, Flags) then begin
  end
end { SerSetParams } ;


function SerOpen(const DeviceName: String): TSerialHandle;

var     securityAttributes: TSecurityAttributes;

begin
  securityAttributes.nLength := SizeOf(TSecurityAttributes);
  securityAttributes.lpSecurityDescriptor := nil;
  securityAttributes.bInheritHandle := true;

  result := CreateFile(
    PChar(DeviceName),
    GENERIC_READ or GENERIC_WRITE,
    0,
    @securityAttributes,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED,
    0);
  if result = INVALID_HANDLE_VALUE then
    result := 0
  else

(* Don't trust Windows's initial state. If the internal variant (returning a    *)
(* result) of SerSetParams() fails it indicates that part of the comms API is   *)
(* unavailable, assume that this is fatal because it will probably mess up      *)
(* things like read timeouts.                                                   *)

    if not SerSetParamsPrivate(result, 9600, 8, NoneParity, 1, []) then begin
      CloseHandle(result);
      result := 0
    end
end { SerOpen } ;


function SerWrite(Handle: TSerialHandle; var Buffer; Count: LongInt): LongInt;
var
  BytesWritten: DWORD;
  Overlapped: TOVERLAPPED;

begin
  FillChar(Overlapped{%H-}, SizeOf(Overlapped), 0);
  WriteFile(Handle, Buffer, Count, BytesWritten{%H-}, @Overlapped);
  Result := Count; // assume (for now) it will always succeed; FIXME?
end { SerWrite } ;


function SerReadTimeout(Handle: TSerialHandle; var Buffer; mSec: LongInt): LongInt;
var
  Overlapped: TOVERLAPPED;
  WaitResult: DWORD = 0;
begin
  Result := 0;
  FillChar(Overlapped{%H-}, SizeOf(Overlapped), 0);
  ReadFile(Handle, Buffer, 1, DWORD(Result), @Overlapped);
  if Result = 0 then begin
    Overlapped.hEvent := CreateEvent(nil, True, False, nil);
    SetCommMask(Handle, EV_RXCHAR);
    if not WaitCommEvent(Handle, WaitResult, @Overlapped) then begin
      if GetLastError = ERROR_IO_PENDING then begin
        WaitForSingleObject(Overlapped.hEvent, mSec);
        if WaitResult and EV_RXCHAR = EV_RXCHAR then begin
           ReadFile(Handle, Buffer, 1, DWORD(Result), @Overlapped);
        end;
      end;
    end;
    SetCommMask(Handle, 0);
    CloseHandle(Overlapped.hEvent);
  end;
end;


procedure SerClose(Handle: TSerialHandle);
begin
  CloseHandle(Handle);
end { SerClose } ;

{$endif} // windows


{ TSimpleComPort }

constructor TSimpleComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsOpen := False;
end;

destructor TSimpleComPort.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TSimpleComPort.Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
var
  Par: TParityType;
begin
  if IsOpen then
    Result := True
  else begin
    Result := False;
    FHandle := SerOpen(POrt);
    if FHandle > 0 then begin
      case Parity of
        'N': Par := NoneParity;
        'E': Par := EvenParity;
        'O': Par := OddParity;
      end;
      SerSetParams(FHandle, Baud, Bits, Par, StopBits, []);
      Result := True;
      FIsOpen := True;
    end;
  end;
end;

function TSimpleComPort.Send(B: Byte): LongInt;
begin
  Result := Send(B, 1);
end;

function TSimpleComPort.Send(var Buffer; Count: LongInt): LongInt;
begin
  if IsOpen then begin
    Result := SerWrite(FHandle, Buffer, Count);
  end
  else
    Result := 0;
end;

function TSimpleComPort.Send(Text: String): LongInt;
begin
  Result := Send(Text[1], Length(Text));
end;

function TSimpleComPort.ReceiveByte(TimeoutMilli: Integer; out RecvByte: Byte): LongInt;
begin
  Result := 0;
  if IsOpen then begin
    Result := SerReadTimeout(FHandle, RecvByte{%H-}, TimeoutMilli);
  end;
end;

procedure TSimpleComPort.Close;
begin
  if FIsOpen then begin
    SerClose(FHandle);
    FIsOpen := False;
  end;
end;

end.

