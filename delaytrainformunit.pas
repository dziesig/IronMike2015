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

unit DelayTrainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, MagicTimeEdit, TrainsUnit;

type

  { TDelayTrainForm }

  TDelayTrainForm = class(TForm)
    ButtonOk: TBitBtn;
    BitBtn2: TBitBtn;
    Label1: TLabel;
    TrainListBox: TListBox;
    MagicTimeEdit1: TMagicTimeEdit;
    procedure ButtonOkClick(Sender: TObject);
    procedure TrainListBoxClick(Sender: TObject);
  private
    fNewTime: TDateTime;
    fTrain: TTrainData;
    { private declarations }
  public
    { public declarations }
    function ShowModal : Integer; override;

    property Train   : TTrainData read fTrain;
    property NewTime : TDateTime  read fNewTime;
  end;

var
  DelayTrainForm: TDelayTrainForm;

implementation

uses
  IronMikeDataUnit,
  Preferences;

{$R *.lfm}

{ TDelayTrainForm }

procedure TDelayTrainForm.ButtonOkClick(Sender: TObject);
begin
  fNewTime := MagicTimeEdit1.DateTime;
end;

function TDelayTrainForm.ShowModal: Integer;
var
  I : Integer;
  T : TTrainData;
begin
  ButtonOk.Enabled := False;
  MagicTimeEdit1.DateTime := 0;
  MagicTimeEdit1.AMPM  := PreferencesForm.DefaultAMPM;
  for I := 0 to pred(IronMikeData.TrainList.Count) do
    begin
      T := IronMikeData.TrainList[I];
      if T.Status = tsReady then
        TrainListBox.AddItem( IntToStr(T.Number), T );
    end;
  Result:=inherited ShowModal;
end;

procedure TDelayTrainForm.TrainListBoxClick(Sender: TObject);
var
  Idx : Integer;
  //T   : TTrainData;
begin
  Idx := TrainListBox.ItemIndex;
  ButtonOk.Enabled := Idx >= 0;
  if Idx >= 0 then
    begin
      fTrain := TTrainData(TrainListBox.Items.Objects[Idx]);
      MagicTimeEdit1.DateTime := fTrain.Schedule.Departs;
    end;
end;

end.

