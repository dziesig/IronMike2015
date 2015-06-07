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

unit SelectWhereFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, StationsUnit;

type

  { TSelectWhereForm }

  TSelectWhereForm = class(TForm)
    ButtonYes: TBitBtn;
    BitBtn2: TBitBtn;
    ListBox1: TListBox;
    procedure ListBox1Click(Sender: TObject);
  private
    fStation     : TStationData;
    fStationName: String;
    fStationPos: Integer;
    { private declarations }
  public
    { public declarations }

    function ShowModal : Integer; override;
    property StationPos : Integer read fStationPos;
    property StationName : String read fStationName;
    property Station     : TStationData read fStation;
  end;

var
  SelectWhereForm: TSelectWhereForm;

implementation

uses
  IronMikeDataUnit;

{$R *.lfm}

{ TSelectWhereForm }

procedure TSelectWhereForm.ListBox1Click(Sender: TObject);
var
  Index : Integer;
begin
  Index := ListBox1.ItemIndex;
  if Index >= 0 then
    begin
      fStation := TStationData(ListBox1.Items.Objects[Index]);
      fStationPos := fStation.Pos;
      fStationName := fStation.Name;
    end;
  ButtonYes.Enabled := Index > 0;
end;

function TSelectWhereForm.ShowModal: Integer;
var
  I : Integer;
  aStation : TStationData;
begin
  ButtonYes.Enabled := False;
  ListBox1.Clear;
  fStationPos := 0; // Stations are numbered 1 .. 5
  fStationName := '';
  for I := 1 to IronMikeData.StationList.Count do
    begin
      aStation := IronMikeData.StationList[I];
      ListBox1.AddItem(aStation.Name,aStation);
    end;
  Result:=inherited ShowModal;
end;

end.

