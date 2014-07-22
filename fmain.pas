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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ComPort;

type

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
    TxtTX: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    TxtRX: TMemo;
    PageControl: TPageControl;
    TsTerminal: TTabSheet;
    TsPlot: TTabSheet;
    TbConnect: TToggleBox;
    procedure CbPortGetItems(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.CbPortGetItems(Sender: TObject);
var
  sel: String;
begin
  sel := CbPort.Text;
  EnumerateSerialPorts(CbPort.Items);
  CbPort.Text := sel;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  EnumerateSerialPorts(CbPort.Items);
end;

end.

