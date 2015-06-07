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

unit TrainInfoFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, TrainsUnit;

type

  { TTrainInfoForm }

  TTrainInfoForm = class(TForm)
    ButtonOk: TBitBtn;
    BitBtn2: TBitBtn;
    TrainListBox: TListBox;
    procedure TrainListBoxClick(Sender: TObject);
  private
    fNumber: Integer;
    fTrain: TTrainData;
    { private declarations }
  public
    { public declarations }
    function ShowModal : Integer; override;

    property Train  : TTrainData read fTrain;
    property Number : Integer    read fNumber;
  end;

var
  TrainInfoForm: TTrainInfoForm;

implementation

uses
  CommonDebug,
  Stringsubs,
  IronMikeDataUnit;

{$R *.lfm}

{ TTrainInfoForm }

function TTrainInfoForm.ShowModal: Integer;
var
  I : Integer;
  T : TTrainData;
begin
  ButtonOk.Enabled := False;
  TrainListBox.Clear;
  TrainListBox.Sorted := True;
  for I := 0 to pred(IronMikeData.TrainList.Count) do
    begin
      T := IronMikeData.TrainList[I];
      TrainListBox.AddItem( IntToStr( T.Number, 4, ' '), T );
    end;

  Result:=inherited ShowModal;
end;

procedure TTrainInfoForm.TrainListBoxClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := TrainListBox.ItemIndex;
  ButtonOk.Enabled := Idx >= 0;
  if ButtonOk.Enabled then
    begin
      fTrain := TTrainData(TrainListBox.Items.Objects[Idx]);
      FNumber := fTrain.Number;
    end;
end;

end.

