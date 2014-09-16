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
  {$ifdef windows}
  windows,
  {$endif}
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
    {$ifdef windows}
    Overlapped_read: TOVERLAPPED;
    Overlapped_write: TOVERLAPPED;
    RxBuf: array[0..1024] of byte;
    RxBufNext: DWORD;
    RxBufLength: DWORD;
    {$endif}
  public
    property IsOpen: Boolean read FIsOpen;
  end;

procedure EnumerateSerialPorts(List: TStrings);

implementation
{$ifdef windows}
uses
  sysutils;

const
  bufSize = 2048;
  dcb_Binary           = $00000001;
  // dcb_Parity           = $00000002;
  // dcb_OutxCtsFlow      = $00000004;
  // dcb_OutxDsrFlow      = $00000008;
  // dcb_DtrControl       = $00000030;
  // dcb_DsrSensivity     = $00000040;
  // dcb_TXContinueOnXOff = $00000080;
  // dcb_OutX             = $00000100;
  // dcb_InX              = $00000200;
  // dcb_ErrorChar        = $00000400;
  // dcb_Null             = $00000800;
  // dcb_RtsControl       = $00003000;
  // dcb_AbortOnError     = $00004000;

procedure EnumerateSerialPorts(List: TStrings);
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

{ TSimpleComPort }

constructor TSimpleComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsOpen := False;
  FillChar(Overlapped_write, SizeOf(Overlapped_write), 0);
  FillChar(Overlapped_read, SizeOf(Overlapped_read), 0);
  Overlapped_read.hEvent := CreateEvent(nil, True, False, nil);
end;

destructor TSimpleComPort.Destroy;
begin
  Close;
  CloseHandle(Overlapped_read.hEvent);
  inherited Destroy;
end;

function TSimpleComPort.Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
var
  DCB: TDCB;
  Timeouts: TCommTimeouts;
begin
  Result := False;
  FHandle := CreateFile(
    PChar(Port),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL or FILE_FLAG_OVERLAPPED,
    0);

  if FHandle <> INVALID_HANDLE_VALUE then begin
    FillChar(DCB{%H-}, SizeOf(DCB), 0);
    DCB.DCBlength := SizeOf(DCB);
    DCB.flags := dcb_Binary;
    DCB.BaudRate := Baud;
    DCB.ByteSize := Bits;
    case Parity of
      'O':  DCB.Parity := Windows.ODDPARITY;
      'E': DCB.Parity := Windows.EVENPARITY;
      'N': DCB.Parity := Windows.NOPARITY;
    end;
    if StopBits > 1 then
      DCB.StopBits := TWOSTOPBITS
    else
      DCB.StopBits := ONESTOPBIT;
    if SetCommState(FHandle, DCB) then begin
      FillChar(Timeouts{%H-}, SizeOf(Timeouts), 0);
      Timeouts.ReadIntervalTimeout := MAXDWORD;
      if SetCommTimeouts(FHandle, Timeouts) then begin
        if SetupComm(FHandle, bufSize, bufSize) then begin
          Result := True;
          FIsOpen := True;
          RxBufNext := 0;
          RxBufLength := 0;
        end;
      end;
    end;
    if not Result then begin
      CloseHandle(FHandle);
    end;
  end;
end;

function TSimpleComPort.Send(B: Byte): LongInt;
begin
  Result := Send(B, 1);
end;

function TSimpleComPort.Send(var Buffer; Count: LongInt): LongInt;
var
  Dummy: DWORD;
begin
  if IsOpen then begin
    Dummy := 0;
    Result := 0;
    WriteFile(FHandle, Buffer, Count, Dummy, @Overlapped_write);
    GetOverlappedResult(FHandle, Overlapped_write, DWORD(Result), False);
  end
  else
    Result := 0;
end;

function TSimpleComPort.Send(Text: String): LongInt;
begin
  Result := Send(Text[1], Length(Text));
end;

function TSimpleComPort.ReceiveByte(TimeoutMilli: Integer; out RecvByte: Byte): LongInt;
var
  EvMask: DWORD;
begin
  // On windows it seems problematic with some USB/Serial drivers to
  // read one byte at a time, the overhead is immense and strange
  // behavior can be observed with some serial drivers. (On Linux
  // this was not a problem at all). Therefore I will try to read
  // in as many bytes as it is willing to give me on every call to
  // ReadFile() and buffer them myself.

  // Is the Buffer empty? Then read in a new chunk of bytes.
  if RxBufNext = RxBufLength then begin
    RxBufLength := 0;
    RxBufNext := 0;
    ReadFile(FHandle, RxBuf, SizeOf(RxBuf), RxBufLength, @Overlapped_read);
    if RxBufLength = 0 then begin
      // there was noting immediately available, therefore now wait blocking.
      SetCommMask(FHandle, EV_RXCHAR);
      if not WaitCommEvent(FHandle, EvMask, @Overlapped_read) then begin
        if GetLastError = ERROR_IO_PENDING then begin
          if WaitForSingleObject(Overlapped_read.hEvent, TimeoutMilli) = WAIT_OBJECT_0 then begin
            ReadFile(FHandle, RxBuf, SizeOf(RxBuf), RxBufLength, @Overlapped_read);
            ResetEvent(Overlapped_read.hEvent);
          end;
        end;
      end;
      SetCommMask(FHandle, 0);
    end;
  end;

  // serve one byte directly from RAM, this is quite fast.
  if RxBufLength > RxBufNext then begin
    RecvByte := RxBuf[RxBufNext];
    inc(RxBufNext);
    Result := 1;
  end
  else
    Result := 0;
end;

procedure TSimpleComPort.Close;
begin
  if IsOpen then begin
    FIsOpen := False;
    FileClose(FHandle);
  end;
end;

{$endif windows}

{$ifdef unix}
uses
  BaseUnix,
  Unix,
  serial,
  sysutils;

{$ifdef linux}
procedure EnumerateSerialPorts(List: TStrings);
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
{$endif linux}

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
  FHandle := SerOpen(Port);
  if FHandle > 0 then begin
    case Parity of
      'N': Par := NoneParity;
      'E': Par := EvenParity;
      'O': Par := OddParity;
    end;
    SerSetParams(FHandle, Baud, Bits, Par, StopBits, []);
    FIsOpen := True;
  end;
  Result := IsOpen;
end;

function TSimpleComPort.Send(B: Byte): LongInt;
begin
  Result := Send(B, 1);
end;

function TSimpleComPort.Send(var Buffer; Count: LongInt): LongInt;
begin
  if IsOpen then
    Result := SerWrite(FHandle, Buffer, Count)
  else
    Result := 0;
end;

function TSimpleComPort.Send(Text: String): LongInt;
begin
  Result := Send(Text[1], Length(Text));
end;

function TSimpleComPort.ReceiveByte(TimeoutMilli: Integer; out RecvByte: Byte): LongInt;
var
  readSet: TFDSet;
  selectTimeout: TTimeVal;
begin
  fpFD_ZERO(readSet);
  fpFD_SET(FHandle, readSet);
  selectTimeout.tv_sec := TimeoutMilli div 1000;
  selectTimeout.tv_usec := (TimeoutMilli mod 1000) * 1000;
  Result := 0;
  if fpSelect(FHandle + 1, @readSet, nil, nil, @selectTimeout) > 0 then
    Result := fpRead(FHandle, RecvByte{%H-}, 1);
end;

procedure TSimpleComPort.Close;
begin
  if IsOpen then begin
    SerClose(FHandle);
    FIsOpen := False;
  end;
end;

{$endif unix}

end.