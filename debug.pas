unit Debug;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  StreamIO;

type
  TDebugStream = class(TStream)
    function Write(const Buffer; Count : Longint) : Longint; override;
  end;

implementation
{$ifdef windows}
uses
  Windows;
{$endif}

var
  S: TDebugStream;

procedure RedirectStdOut;
begin
  S := TDebugStream.Create;
  AssignStream(Output,S);
  Rewrite(Output);
end;

function TDebugStream.Write(const Buffer; Count : Longint) : Longint;
var
  msg : ansistring;

begin
  result := count;
  SetLength(msg, count);
  move(buffer, PChar(msg)[0], count);
  {$ifdef windows}
  OutputDebugString(PChar(TrimRight(msg)));
  {$else}
  WriteLn(StdErr, TrimRight(msg));
  {$endif}
end;

initialization
  RedirectStdOut;
end.

