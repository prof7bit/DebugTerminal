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

