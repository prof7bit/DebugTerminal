{ simple wrapper for serial.pp on unix and implementation of serial.pp for windows

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
  {$else}
  Serial
  {$endif}
  Classes, sysutils;

type
  { TSimpleComPort }

  TSimpleComPort = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Open(Port: String; Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer): Boolean;
    procedure Send(B: Byte);
    procedure Send(var Buffer; Count: LongInt);
    procedure Send(Text: String);
    function Receive(TimeoutMilli: Integer; var RecvByte: Byte): LongInt;
    procedure Close;
  private
    FHandle: THandle;
    FIsOpen: Boolean;
  public
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
begin
  if IsOpen then
    Result := True
  else begin
  end;
end;

procedure TSimpleComPort.Send(B: Byte);
begin
  Send(B, 1);
end;

procedure TSimpleComPort.Send(var Buffer; Count: LongInt);
begin
  if IsOpen then begin
  end;
end;

procedure TSimpleComPort.Send(Text: String);
begin
  Send(Text[1], Length(Text));
end;

function TSimpleComPort.Receive(TimeoutMilli: Integer; var RecvByte: Byte): LongInt;
begin
  Result := 0;
  if IsOpen then begin
  end;
end;

procedure TSimpleComPort.Close;
begin
  if FIsOpen then begin
    FIsOpen := False;
  end;
end;

end.

