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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TFormMain }

  TFormMain = class(TForm)
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    PageControl: TPageControl;
    TsTerminal: TTabSheet;
    TsPlot: TTabSheet;
    TbConnect: TToggleBox;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

end.

