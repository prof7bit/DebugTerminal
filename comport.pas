{ simple wrapper around serial.pps

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
  Classes, sysutils, Serial;

type
  { TComPort }

  TComPort = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
    procedure Send(B: Byte);
    procedure Send(var Buffer; Count: LongInt);
    procedure Send(Text: String);
    function Receice(TimeoutMilli: Integer; var RecvByte: Byte): LongInt;
    procedure Close;
  private
    FHandle: THandle;
    FIsOpen: Boolean;
  public
    property Handle: THandle read FHandle;
    property IsOpen: Boolean read FIsOpen;
  end;

procedure EnumerateSerialPorts(List: TStrings);

implementation

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
    if F = INVALID_HANDLE_VALUE then begin
      FileClose(F);
      List.Append('COM' + IntToStr(I));
    end;
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

function ParityTypeFromChar(C: Char): TParityType;
begin
  case C of
    'N': Result := serial.NoneParity;
    'O': Result := serial.OddParity;
    'E': Result := serial.EvenParity;
  end;
end;

{ TComPort }

constructor TComPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsOpen := False;
end;

destructor TComPort.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TComPort.Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
begin
  if IsOpen then
    Result := True
  else begin
    FHandle := SerOpen(Port);
    if FHandle <> 0 then begin
      SerSetParams(FHandle, Baud, Bits, ParityTypeFromChar(Parity), StopBits, []);
      Result := True;
      FIsOpen := True;
    end
    else
      Result := False;
  end;
end;

procedure TComPort.Send(B: Byte);
begin
  Send(B, 1);
end;

procedure TComPort.Send(var Buffer; Count: LongInt);
begin
  if IsOpen then begin
    SerWrite(FHandle, Buffer, Count);
    SerSync(FHandle);
  end;
end;

procedure TComPort.Send(Text: String);
begin
  Send(Text[1], Length(Text));
end;

{$ifdef windows}
function TComPort.Receice(TimeoutMilli: Integer; var RecvByte: Byte): LongInt;
const
  MILLISEC = 1/24/60/60/1000;
var
  BytesRead: LongWord;
  StartTime: Double;
begin
  // serial.pp for windows is broken, it would need to be reworked to
  // use overlapped io, until then the only way to implement a reliable
  // read with timeout is to use polling :-(
  StartTime := Now;
  repeat
    ReadFile(FHandle, RecvByte, 1, BytesRead, nil);
    if BytesRead = 0 then
      Sleep(1);
  until (BytesRead <> 0) or (Now > StartTime + TimeoutMilli * MILLISEC);
  Result := BytesRead;
end;
{$else}
function TComPort.Receice(TimeoutMilli: Integer; var RecvByte: Byte): LongInt;
begin
  Result := SerReadTimeout(FHandle, RecvByte, TimeoutMilli);
end;
{$endif}


procedure TComPort.Close;
begin
  if FIsOpen then begin
    SerClose(FHandle);
    FIsOpen := False;
  end;
end;

end.
