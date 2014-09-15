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
    procedure Reconfigure(Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer);
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
{$ifdef windows}
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
begin
  FHandle := SerOpen(Port);
  if FHandle > 0 then begin
    FIsOpen := True;
    Reconfigure(Baud, Bits, Parity, StopBits);
  end;
  Result := IsOpen;
end;

procedure TSimpleComPort.Reconfigure(Baud: Integer; Bits: Integer; Parity: Char; StopBits: Integer);
var
  Par: TParityType;
begin
  case Parity of
    'N': Par := NoneParity;
    'E': Par := EvenParity;
    'O': Par := OddParity;
  end;
  if IsOpen then begin
    SerSetParams(FHandle, Baud, Bits, Par, StopBits, []);
  end;
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