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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls, IniPropStorage,
  ComPort, FConfigButton, LCLType, ExtCtrls, SynEdit, SynEditKeyCmds, syncobjs;

type

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
    CbEncodingRecv: TComboBox;
    CbPort: TComboBox;
    CbBaud: TComboBox;
    CbEncodingSend: TComboBox;
    IniProp: TIniPropStorage;
    FOutput: TSynEdit;
    Timer: TTimer;
    FInput: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    PageControl: TPageControl;
    TsTerminal: TTabSheet;
    TsPlot: TTabSheet;
    TbConnect: TToggleBox;
    procedure BtnCfg1Click(Sender: TObject);
    procedure BtnCfg2Click(Sender: TObject);
    procedure BtnCfg3Click(Sender: TObject);
    procedure BtnCfg4Click(Sender: TObject);
    procedure BtnCfg5Click(Sender: TObject);
    procedure BtnCfg6Click(Sender: TObject);
    procedure BtnCfg7Click(Sender: TObject);
    procedure BtnCfg8Click(Sender: TObject);
    procedure CbBaudChange(Sender: TObject);
    procedure CbEncodingRecvChange(Sender: TObject);
    procedure CbEncodingSendChange(Sender: TObject);
    procedure CbPortChange(Sender: TObject);
    procedure CbPortGetItems(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TbConnectChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OutputAddByte(B: Byte);
    procedure OutputLineBreak;
  private
    Receiver: TReceiver;
    procedure UpdateConnectButton;
    function GetSequence(AButton: TButton): String;
    procedure ConfigButton(AButton: TButton);
    function SendHex(S: String): Boolean;
    procedure IniWrite(Section, Key, Value: String);
    function IniRead(Section, Key, DefaultValue: String): String;
  public
    ComPort: TSimpleComPort;
    RxLock: TCriticalSection;
    RxBuf: String;
    RxByteColumn: Integer;
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

{ TReceiver }

constructor TReceiver.Create;
begin
  inherited Create(False);
end;

procedure TReceiver.Execute;
begin
  repeat
    if FormMain.ComPort.IsOpen then begin
      if FormMain.ComPort.Receice(50, ReceiveByte) = 1 then begin
        FormMain.RxLock.Acquire;
        FormMain.RxBuf += Chr(ReceiveByte);
        FormMain.RxLock.Release;
      end
    end
    else begin
      Sleep(1);
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

procedure TFormMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Receiver.Terminate;
  Receiver.WaitFor;
  Receiver.Free;
  RxLock.Free;
end;

procedure TFormMain.BtnCfg1Click(Sender: TObject);
begin
  ConfigButton(Btn1);
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
  RxLock := TCriticalSection.Create;
  Receiver := TReceiver.Create;
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
begin
  RxLock.Acquire;
  FOutput.BeginUpdate(False);
  for C in RxBuf do begin
    OutputAddByte(Ord(C));
  end;
  FOutput.EndUpdate;
  RxBuf := '';
  RxLock.Release;
end;

procedure TFormMain.FInputKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    if SendHex(FInput.Text) then begin
      FInput.Text := '';
    end;
  end;
end;

procedure TFormMain.OutputAddByte(B: Byte);
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
  FOutput.ExecuteCommand(ecEditorBottom, '', nil);
  FOutput.InsertTextAtCaret(S);
  RxByteColumn += 1;
  if RxByteColumn = L1 then begin
    FOutput.InsertTextAtCaret('  ');
  end;
  if RxByteColumn >= L2 then begin
    OutputLineBreak;
  end;
end;

procedure TFormMain.OutputLineBreak;
begin
  FOutput.ExecuteCommand(ecEditorBottom, '', nil);
  FOutput.InsertTextAtCaret(LineEnding);
  RxByteColumn := 0;
end;

procedure TFormMain.UpdateConnectButton;
begin
  if ComPort.IsOpen then begin
    TbConnect.State := cbChecked;
    TbConnect.Caption := 'Disconnect';
    OutputLineBreak;
    OutputLineBreak;
  end
  else begin
    TbConnect.State := cbUnchecked;
    TbConnect.Caption := 'Connect';
  end;
  FInput.Enabled := ComPort.IsOpen;
end;

function TFormMain.GetSequence(AButton: TButton): String;
begin
  // sequence in the ini is always hex encoded
  IniProp.IniSection := 'Buttons';
  Result := IniProp.ReadString('Name' + IntToStr(AButton.Tag), '');
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
  L := Length(S) div 2;
  Getmem(Buf, L);
  if HexToBin(PChar(S), Buf, L) = L then begin
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

end.

