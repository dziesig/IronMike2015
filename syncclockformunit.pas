//Copyright (c) 1995-2015 by Robert A. Raymond and Donald R. Ziesig
//
//Donald.at.Ziesig.org
//
//This file is part of IronMike.
//
//IronMike is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//IronMike is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with IronMike.  If not, see <http://www.gnu.org/licenses/>.

unit SyncClockFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  MagicTimeEdit;

type

  { TSyncClockForm }

  TSyncClockForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    MagicTimeEdit1: TMagicTimeEdit;
    procedure FormShow(Sender: TObject);
  private
    vTime0, vTime1 : TDateTime;
    function GetDateTime: TDateTime;
    { private declarations }
  public
    { public declarations }
    property DateTime : TDateTime read GetDateTime;
  end;

var
  SyncClockForm: TSyncClockForm;

implementation

uses
  MainFormUnit,
  IronMikeDataUnit;

{$R *.lfm}

{ TSyncClockForm }

procedure TSyncClockForm.FormShow(Sender: TObject);
begin
  // The screwy code here compensates for the fact that
  // MagicTimeEdit removes the date and keeps only the time of day.
  vTime0 := MainForm.MagicClock1.DateTime;
  MagicTimeEdit1.DateTime := vTime0;
  vTime1 := MagicTimeEdit1.DateTime;
end;

function TSyncClockForm.GetDateTime: TDateTime;
var
  theDay : Integer;
begin
  theDay := Trunc(Double(vTime0));
  Result := MagicTimeEdit1.DateTime;
  if Result < vTime1 then
    Inc(theDay);
  Result := Result + Double(theDay);
end;

end.

