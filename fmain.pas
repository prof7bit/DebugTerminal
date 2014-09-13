{ A helpful terminal app for embedded development - Main Window

  Copyright (C) 2014 Bernd Kreuss <prof7bit@gmail.com>

  This source is free software; you can redistribute it and/or modify it under the terms of the GNU General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web at
  <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing to the Free Software Foundation, Inc., 59
  Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit FMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, IniPropStorage, ComPort,
  FConfigButton, LCLType, ExtCtrls, Spin, SynEdit, SynEditKeyCmds, TAGraph, TASeries, syncobjs,
  TAChartUtils, Math;

const
  HISTSIZE = 100;

type
  TPlotDataRaw = array[0..7] of Byte;

  { THistory }

  THistory = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(S: String);
    function Prev: String;
    function Next: String;
    procedure Reset;
  private
    List: TStringList;
    Ptr: Integer;
    procedure Save;
    procedure Load;
    function Find(S: String; out I: Integer): Boolean;
    procedure Remove(S: String);
  end;

  TReceiver = class;

  { TFormMain }

  TFormMain = class(TForm)
    Btn1: TButton;
    Btn2: TButton;
    Btn3: TButton;
    Btn4: TButton;
    Btn5: TButton;
    Btn6: TButton;
    Btn7: TButton;
    Btn8: TButton;
    BtnCfg1: TButton;
    BtnCfg2: TButton;
    BtnCfg3: TButton;
    BtnCfg4: TButton;
    BtnCfg5: TButton;
    BtnCfg6: TButton;
    BtnCfg7: TButton;
    BtnCfg8: TButton;
    BtnSync: TButton;
    BtnClear: TButton;
    CbEncodingRecv: TComboBox;
    CbPort: TComboBox;
    CbBaud: TComboBox;
    CbEncodingSend: TComboBox;
    Chart: TChart;
    Channel1: TLineSeries;
    Channel2: TLineSeries;
    Channel3: TLineSeries;
    Channel4: TLineSeries;
    CheckBoxSigned: TCheckBox;
    IniProp: TIniPropStorage;
    FOutput: TSynEdit;
    LblChannels: TLabel;
    LblBits: TLabel;
    LblZoom: TLabel;
    SpinChannels: TSpinEdit;
    SpinBits: TSpinEdit;
    Timer: TTimer;
    FInput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl: TPageControl;
    TrackZoom: TTrackBar;
    TsTerminal: TTabSheet;
    TsPlot: TTabSheet;
    TbConnect: TToggleBox;
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure Btn3Click(Sender: TObject);
    procedure Btn4Click(Sender: TObject);
    procedure Btn5Click(Sender: TObject);
    procedure Btn6Click(Sender: TObject);
    procedure Btn7Click(Sender: TObject);
    procedure Btn8Click(Sender: TObject);
    procedure BtnCfg1Click(Sender: TObject);
    procedure BtnCfg2Click(Sender: TObject);
    procedure BtnCfg3Click(Sender: TObject);
    procedure BtnCfg4Click(Sender: TObject);
    procedure BtnCfg5Click(Sender: TObject);
    procedure BtnCfg6Click(Sender: TObject);
    procedure BtnCfg7Click(Sender: TObject);
    procedure BtnCfg8Click(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure BtnSyncClick(Sender: TObject);
    procedure CbBaudChange(Sender: TObject);
    procedure CbEncodingRecvChange(Sender: TObject);
    procedure CbEncodingSendChange(Sender: TObject);
    procedure CbPortChange(Sender: TObject);
    procedure CbPortGetItems(Sender: TObject);
    procedure CheckBoxSignedChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpinBitsChange(Sender: TObject);
    procedure SpinChannelsChange(Sender: TObject);
    procedure TbConnectChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrackZoomChange(Sender: TObject);
  private
    Receiver: TReceiver;
    History: THistory;
    OutputNewText: String;
    procedure OutputPrepareAddByte(B: Byte);
    procedure OutputLineBreak;
    procedure PlotApplyZoom;
    procedure PlotInit;
    procedure PlotAddByte(B: Byte);
    procedure UpdateConnectButton;
    function GetSequence(AButton: TButton): String;
    procedure ConfigButton(AButton: TButton);
    function SendHex(S: String): Boolean;
    procedure IniWrite(Section, Key, Value: String);
    function IniRead(Section, Key, DefaultValue: String): String;
    function IniReadInt(Section, Key: String; DefaultValue: Integer): Integer;
    procedure DoButtonAction(B: TButton);
  public
    ComPort: TSimpleComPort;
    RxLock: TCriticalSection;
    RxBuf: String;
    RxByteColumn: Integer;
    PlotX: Integer;
    PlotDataRaw: TPlotDataRaw;
    PlotDataByteCount: Byte;
    LastByte: Byte;
    ByteCounter: Byte;
    destructor Destroy; override;
  end;

  { TReceiver }

  TReceiver = class(TThread)
    ReceiveByte: Byte;
    constructor Create();
    procedure Execute; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

function ByteToBinary(B: Byte): String;
var
  I: Integer;
begin
  SetLength(Result, 8);
  for I := 1 to 8 do begin
    if B and $80 <> 0 then
      Result[I] := '1'
    else
      Result[I] := '0';
    B := B shl 1;
  end;
end;

{ THistory }

constructor THistory.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  List := TStringList.Create;
  Load;
  Ptr := 0;
end;

destructor THistory.Destroy;
begin
  Save;
  List.Free;
  inherited Destroy;
end;

procedure THistory.Add(S: String);
begin
  Remove(S);
  List.Insert(0, S);
  Save;
  Reset;
end;

function THistory.Prev: String;
begin
  Result := List.Strings[Ptr];
  Ptr += 1;
  if Ptr > List.Count - 1 then
    Ptr := List.Count - 1;
end;

function THistory.Next: String;
begin
  Ptr -= 1;
  if Ptr < 0 then begin
    Ptr := 0;
    Result := ''
  end
  else
    Result := List.Strings[Ptr];
end;

procedure THistory.Reset;
begin
  Ptr := 0;
end;

procedure THistory.Save;
var
  I: Integer;
begin
  for I := 1 to HISTSIZE do begin
    if I-1 < List.Count then begin
      FormMain.IniWrite('History', 'Item' + IntToStr(I), List.Strings[I-1]);
    end
    else begin
      FormMain.IniWrite('History', 'Item' + IntToStr(I), '');
    end;
  end;
end;

procedure THistory.Load;
var
  I: Integer;
  S: String;
begin
  List.Clear;
  for I := 1 to HISTSIZE do begin
    S := FormMain.IniRead('History', 'Item' + IntToStr(I), '');
    if S <> '' then begin
      List.Append(S);
    end;
  end;
  Reset;
end;

function THistory.Find(S: String; out I: Integer): Boolean;
var
  J: Integer;
begin
  Result := False;
  for J := 0 to List.Count - 1 do begin
    if List.Strings[J] = S then begin
      I := J;
      Result := True;
      break;
    end;
  end;
end;

procedure THistory.Remove(S: String);
var
  I: Integer;
begin
  while Find(S, I) do begin
    List.Delete(I);
  end;
end;

{ TReceiver }

constructor TReceiver.Create;
begin
  inherited Create(False);
end;

procedure TReceiver.Execute;
begin
  repeat
    if FormMain.ComPort.IsOpen then begin
      if FormMain.ComPort.Receive(50, ReceiveByte) = 1 then begin
        FormMain.RxLock.Acquire;
        FormMain.RxBuf += Chr(ReceiveByte);
        FormMain.RxLock.Release;
      end
    end
    else begin
      Sleep(50);
    end;
  until Terminated;
end;

{ TFormMain }

procedure TFormMain.CbPortGetItems(Sender: TObject);
var
  sel: String;
begin
  sel := CbPort.Text;
  EnumerateSerialPorts(CbPort.Items);
  CbPort.Text := sel;
end;

procedure TFormMain.CheckBoxSignedChange(Sender: TObject);
begin
  IniWrite('Plot', 'Signed', IntToStr(Integer(CheckBoxSigned.Checked)));
  BtnClearClick(Sender);
end;

procedure TFormMain.BtnCfg1Click(Sender: TObject);
begin
  ConfigButton(Btn1);
end;

procedure TFormMain.Btn1Click(Sender: TObject);
begin
  DoButtonAction(Btn1);
end;

procedure TFormMain.Btn2Click(Sender: TObject);
begin
  DoButtonAction(Btn2);
end;

procedure TFormMain.Btn3Click(Sender: TObject);
begin
  DoButtonAction(Btn3);
end;

procedure TFormMain.Btn4Click(Sender: TObject);
begin
  DoButtonAction(Btn4);
end;

procedure TFormMain.Btn5Click(Sender: TObject);
begin
  DoButtonAction(Btn5);
end;

procedure TFormMain.Btn6Click(Sender: TObject);
begin
  DoButtonAction(Btn6);
end;

procedure TFormMain.Btn7Click(Sender: TObject);
begin
  DoButtonAction(Btn7);
end;

procedure TFormMain.Btn8Click(Sender: TObject);
begin
  DoButtonAction(Btn8);
end;

procedure TFormMain.BtnCfg2Click(Sender: TObject);
begin
  ConfigButton(Btn2);
end;

procedure TFormMain.BtnCfg3Click(Sender: TObject);
begin
  ConfigButton(Btn3);
end;

procedure TFormMain.BtnCfg4Click(Sender: TObject);
begin
  ConfigButton(Btn4);
end;

procedure TFormMain.BtnCfg5Click(Sender: TObject);
begin
  ConfigButton(Btn5);
end;

procedure TFormMain.BtnCfg6Click(Sender: TObject);
begin
  ConfigButton(Btn6);
end;

procedure TFormMain.BtnCfg7Click(Sender: TObject);
begin
  ConfigButton(Btn7);
end;

procedure TFormMain.BtnCfg8Click(Sender: TObject);
begin
  ConfigButton(Btn8);
end;

procedure TFormMain.BtnClearClick(Sender: TObject);
begin
  PlotInit;
end;

procedure TFormMain.BtnSyncClick(Sender: TObject);
begin
  if PlotDataByteCount > 0 then begin
    PlotDataByteCount -= 1;
  end
  else begin
    PlotDataByteCount := (SpinBits.Value div 8) * SpinChannels.Value - 1;
  end;
  PlotInit;
end;

procedure TFormMain.CbBaudChange(Sender: TObject);
begin
  IniWrite('Serial', 'Baud', CbBaud.Text);
end;

procedure TFormMain.CbEncodingRecvChange(Sender: TObject);
begin
  IniWrite('Encoding', 'Receive', CbEncodingRecv.Text);
end;

procedure TFormMain.CbEncodingSendChange(Sender: TObject);
begin
  IniWrite('Encoding', 'Send', CbEncodingSend.Text);
end;

procedure TFormMain.CbPortChange(Sender: TObject);
begin
  IniWrite('Serial', 'Port', CbPort.Text);
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  I: Integer;
  B: TControl;
begin
  History := THistory.Create(Self);
  EnumerateSerialPorts(CbPort.Items);
  ComPort := TSimpleComPort.Create(self);
  IniProp.IniSection := 'Buttons';
  for I := 0 to TsTerminal.ControlCount - 1 do begin
    B := TsTerminal.Controls[I];
    if B is TButton then begin
      if (B.Tag > 0) and (B.Tag < 9) then begin
        B.Caption := IniProp.ReadString('Name' + IntToStr(B.Tag), 'Button ' + IntToStr(B.Tag));
      end;
    end;
  end;
  RxByteColumn := 0;
  CbPort.Text := IniRead('Serial', 'Port', '');
  CbBaud.Text := IniRead('Serial', 'Baud', '9600');
  CbEncodingRecv.Text := IniRead('Encoding', 'Receive', 'Hex');
  CbEncodingSend.Text := IniRead('Encoding', 'Send', 'Hex');
  TrackZoom.Position := IniReadInt('Plot', 'Zoom', 40);
  SpinChannels.Value := IniReadInt('Plot', 'Channels', 1);
  SpinBits.Value := IniReadInt('Plot', 'Bits', 8);
  CheckBoxSigned.Checked := Bool(IniReadInt('Plot', 'Signed', 0));
  PlotDataByteCount := 0;
  PlotInit;
  RxLock := TCriticalSection.Create;
  Receiver := TReceiver.Create;
end;

procedure TFormMain.SpinBitsChange(Sender: TObject);
begin
  IniWrite('Plot', 'Bits', IntToStr(SpinBits.Value));
  BtnClearClick(Sender);
end;

procedure TFormMain.SpinChannelsChange(Sender: TObject);
begin
  IniWrite('Plot', 'Channels', IntToStr(SpinChannels.Value));
  BtnClearClick(Sender);
end;

procedure TFormMain.TbConnectChange(Sender: TObject);
begin
  if TbConnect.Checked then begin
    ComPort.Open(CbPort.Text, StrToInt(CbBaud.Text), 8, 'N', 2);
  end
  else begin
    ComPort.Close;
  end;
  UpdateConnectButton;
end;

procedure TFormMain.TimerTimer(Sender: TObject);
var
  C: Char;
  Buf: String;
begin
  RxLock.Acquire;
  Buf := RxBuf;
  SetLength(RxBuf, 0);
  RxLock.Release;
  Chart.DisableRedrawing;
  Chart.DisableAutoSizing;
  for C in Buf do begin
    OutputPrepareAddByte(Ord(C));
    PlotAddByte(Ord(C));
  end;
  FOutput.ExecuteCommand(ecEditorBottom, '', nil);
  FOutput.InsertTextAtCaret(OutputNewText);
  SetLength(OutputNewText, 0);
  Chart.EnableAutoSizing;
  Chart.EnableRedrawing;
  PlotApplyZoom;
end;

procedure TFormMain.FInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    if SendHex(FInput.Text) then begin
      History.Add(FInput.Text);
      FInput.Text := '';
    end;
  end;

  if Key = VK_UP then begin
    FInput.Text := History.Prev;
  end;
  if Key = VK_DOWN then begin
    FInput.Text := History.Next;
  end;
end;

procedure TFormMain.TrackZoomChange(Sender: TObject);
begin
  PlotApplyZoom;
  IniWrite('Plot', 'Zoom', IntToStr(TrackZoom.Position));
end;

{ Its not added to the text control directly,
  instead we first collect all the new bytes
  in a string and then latter in a separate
  method we add it all at once }
procedure TFormMain.OutputPrepareAddByte(B: Byte);
var
  S: String;
  L1, L2: Integer;
begin
  L1 := 8;
  L2 := 16;
  case CbEncodingRecv.Text of
    'Hex': S := IntToHex(B, 2) + ' ';
    'Dec': S := Format('%3d ', [B]);
    'ASCII': begin
      S := Chr(B);
      L1 := 0;
      L2 := 200;
    end;
    'Bin': begin
      S := ByteToBinary(B) + ' ';
      L1 := 4;
      L2 := 8;
    end;
  end;
  OutputNewText += S;
  RxByteColumn += 1;
  if RxByteColumn = L1 then begin
    OutputNewText += '  ';
  end;
  if RxByteColumn >= L2 then begin
    OutputNewText += LineEnding;
    RxByteColumn := 0;
  end;
end;

procedure TFormMain.OutputLineBreak;
begin
  FOutput.InsertTextAtCaret(LineEnding);
  RxByteColumn := 0;
end;

procedure TFormMain.PlotApplyZoom;
var
  E: TDoubleRect;
  F: Double;
begin
  F := power(2,  TrackZoom.Position / 10) / 10;
  E := Chart.GetFullExtent;
  E.A.X := PlotX - Chart.Width * F;
  Chart.LogicalExtent := E;
end;

procedure TFormMain.PlotInit;
begin
  Channel1.Clear;
  Channel2.Clear;
  Channel3.Clear;
  Channel4.Clear;
  PlotX := 0;
  PlotApplyZoom;
end;

procedure TFormMain.PlotAddByte(B: Byte);
var
  BytesPerSample: Byte;
  Signed: Boolean;
  FrameSize: Byte;
  Value: LongInt;
  I: Byte;
begin
  BytesPerSample := SpinBits.Value div 8;
  Signed := CheckBoxSigned.Checked;
  FrameSize := BytesPerSample * SpinChannels.Value;
  PlotDataRaw[PlotDataByteCount] := B;
  PlotDataByteCount += 1;
  if PlotDataByteCount = FrameSize then begin
    for I := 0 to SpinChannels.Value-1 do begin
      if BytesPerSample = 1 then
        if Signed then
          Value := Int8(PlotDataRaw[I])
        else
          Value := UInt8(PlotDataRaw[I])
      else
        if Signed then
          Value := PInt16(@PlotDataRaw[I shl 1])^
        else
          Value := PUInt16(@PlotDataRaw[I shl 1])^;

      TLineSeries(Chart.Series.Items[I]).AddXY(PlotX, Value);
    end;
    PlotX += 1;
    PlotDataByteCount := 0;
  end;
end;

procedure TFormMain.UpdateConnectButton;
begin
  FInput.Enabled := ComPort.IsOpen;
  if ComPort.IsOpen then begin
    TbConnect.State := cbChecked;
    TbConnect.Caption := 'Disconnect';
    FOutput.ExecuteCommand(ecEditorBottom, '', nil);
    OutputLineBreak;
    OutputLineBreak;
    if FInput.IsVisible then begin
      FInput.SetFocus;
    end;
  end
  else begin
    TbConnect.State := cbUnchecked;
    TbConnect.Caption := 'Connect';
  end;
end;

function TFormMain.GetSequence(AButton: TButton): String;
begin
  // sequence in the ini is always hex encoded
  Result := IniRead('Buttons', 'Sequence' + IntToStr(AButton.Tag), '');
end;

procedure TFormMain.ConfigButton(AButton: TButton);
begin
  AButton.Font.Bold := True;
  FormConfigButton.IniProp := IniProp;
  FormConfigButton.Button := AButton;
  FormConfigButton.ShowModal;
  AButton.Font.Bold := False;
  IniProp.Save;
end;

function TFormMain.SendHex(S: String): Boolean;
var
  L: Integer;
  Buf: PChar;
begin
  Result := False;
  S := StringReplace(S, ' ', '', [rfReplaceAll]);
  L := Length(S) div 2;
  Getmem(Buf, L);
  if HexToBin(PChar(S), Buf, L) = L then begin
    FOutput.ExecuteCommand(ecEditorBottom, '', nil);
    OutputLineBreak;
    ComPort.Send(Buf^, L);
    Result := True;
  end;
  Freemem(Buf);
end;

procedure TFormMain.IniWrite(Section, Key, Value: String);
begin
  IniProp.IniSection := Section;
  IniProp.WriteString(Key, Value);
  IniProp.Save;
end;

function TFormMain.IniRead(Section, Key, DefaultValue: String): String;
begin
  IniProp.IniSection := Section;
  Result := IniProp.ReadString(Key, DefaultValue);
end;

function TFormMain.IniReadInt(Section, Key: String; DefaultValue: Integer): Integer;
begin
  try
    Result := StrToInt(IniRead(Section, Key, IntToStr(DefaultValue)));
  except
    Result := DefaultValue;
  end;
end;

procedure TFormMain.DoButtonAction(B: TButton);
begin
  if ComPort.IsOpen then begin
    SendHex(GetSequence(B));
  end;
end;

destructor TFormMain.Destroy;
begin
  Receiver.Terminate;
  Receiver.WaitFor;
  Receiver.Free;
  RxLock.Free;
  inherited Destroy;
end;

end.

