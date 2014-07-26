{ A helpful terminal app for embedded development - Button config Dialog

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
unit FConfigButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage;

type

  { TFormConfigButton }

  TFormConfigButton = class(TForm)
    BtnOK: TButton;
    BtnCancel: TButton;
    TxtName: TEdit;
    TxtSequence: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    Button: TButton;
    IniProp: TIniPropStorage;
  end;

var
  FormConfigButton: TFormConfigButton;

implementation

{$R *.lfm}

{ TFormConfigButton }

procedure TFormConfigButton.BtnOKClick(Sender: TObject);
begin
  if TxtName.Text <> '' then begin
    Button.Caption := TxtName.Text;
    IniProp.IniSection := 'Buttons';
    IniProp.WriteString('Name' + IntToStr(Button.Tag), TxtName.Text);
    IniProp.WriteString('Sequence' + IntToStr(Button.Tag), TxtSequence.Text);
    Close;
  end
  else begin
    MessageDlg('Empty button name', 'The button must have a name', mtWarning, [mbOK], '');
    TxtName.SetFocus;
  end;
end;

procedure TFormConfigButton.FormShow(Sender: TObject);
begin
  IniProp.IniSection := 'Buttons';
  TxtName.Text := Button.Caption;
  TxtSequence.Text := IniProp.ReadString('Sequence' + IntToStr(Button.Tag), '');
end;

end.

