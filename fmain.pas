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
  ComPort, FConfigButton, LCLType;

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
    CbPort: TComboBox;
    CbBaud: TComboBox;
    IniProp: TIniPropStorage;
    TxtTX: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    TxtRX: TMemo;
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
    procedure CbPortChange(Sender: TObject);
    procedure CbPortGetItems(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TbConnectChange(Sender: TObject);
    procedure TxtTXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure AddRXText(S: String);
  private
    Receiver: TReceiver;
    procedure UpdateConnectButton;
    function GetSequence(AButton: TButton): String;
    procedure ConfigButton(AButton: TButton);
    function SendHex(S: String): Boolean;
  public
    ComPort: TSimpleComPort;
  end;

  { TReceiver }

  TReceiver = class(TThread)
    ReceiveByte: Byte;
    constructor Create();
    procedure Execute; override;
    procedure SyncOnReceive;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TReceiver }

constructor TReceiver.Create;
begin
  inherited Create(False);
end;

procedure TReceiver.Execute;
begin
  repeat
    if FormMain.ComPort.Receice(1, ReceiveByte) = 1 then begin
      Synchronize(@SyncOnReceive);
    end;
  until Terminated;
end;

procedure TReceiver.SyncOnReceive;
begin
  FormMain.AddRXText(IntToHex(ReceiveByte, 2) + ' ');
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
  IniProp.IniSection := 'Serial';
  IniProp.WriteString('Baud', CbBaud.Text);
  IniProp.Save;
end;

procedure TFormMain.CbPortChange(Sender: TObject);
begin
  IniProp.IniSection := 'Serial';
  IniProp.WriteString('Port', CbPort.Text);
  IniProp.Save;
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
  IniProp.IniSection := 'Serial';
  CbPort.Text := IniProp.ReadString('Port', '');
  CbBaud.Text := IniProp.ReadString('Baud', '9600');
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

procedure TFormMain.TxtTXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    if SendHex(TxtTX.Text) then begin
      TxtTX.Text := '';
    end;
  end;
end;

procedure TFormMain.AddRXText(S: String);
begin
  TxtRX.Text := TxtRX.Text + S;
end;

procedure TFormMain.UpdateConnectButton;
begin
  if ComPort.IsOpen then begin
    TbConnect.State := cbChecked;
    TbConnect.Caption := 'Disconnect';
  end
  else begin
    TbConnect.State := cbUnchecked;
    TbConnect.Caption := 'Connect';
    AddRXText(LineEnding);
  end;
  TxtTX.Enabled := ComPort.IsOpen;
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

end.

