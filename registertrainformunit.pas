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

unit RegisterTrainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, MagicTimeEdit;

type

  { TRegisterTrainForm }

  TRegisterTrainForm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonOk: TBitBtn;
    ButtonCancel: TBitBtn;
    EndComboBox: TComboBox;
    Label2: TLabel;
    Label3: TLabel;
    MagicTimeEdit1: TMagicTimeEdit;
    StartComboBox: TComboBox;
    TrainListBox: TListBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure MagicTimeEdit1TimeChange(Sender: TObject);
    procedure TrainListBoxClick(Sender: TObject);
    procedure TrainListBoxDblClick(Sender: TObject);
  private
    fSelectedTrain: String;
    { private declarations }
    procedure CheckOk;
  public
    { public declarations }
    function ShowModal : Integer; override;

    property SelectedTrain: String read fSelectedTrain;
  end;

var
  RegisterTrainForm: TRegisterTrainForm;

implementation

uses
  CommonMath,
  MainFormUnit,
  DispatcherFormUnit,
  IronMikeDataUnit,
  TrainsUnit;

{$R *.lfm}

{ TRegisterTrainForm }

procedure TRegisterTrainForm.BitBtn1Click(Sender: TObject);
begin
  MagicTimeEdit1.DateTime := MainForm.MagicClock1.DateTime;
  CheckOk;
end;

procedure TRegisterTrainForm.ButtonCancelClick(Sender: TObject);
begin
  ;
end;

procedure TRegisterTrainForm.ButtonOkClick(Sender: TObject);
var
  Idx : Integer;
  T : TTrainData;
  AdvanceBy : TDateTime;
  T0, T1    : TDateTime;
  Starts, Ends : Integer;
begin
  Idx := TrainListBox.ItemIndex;
  fSelectedTrain := TrainListBox.Items[Idx];
  Starts := StartComboBox.ItemIndex+1;
  Ends   := EndComboBox.ItemIndex+1;
  T := IronMikeData.TrainList.TrainByNum[StrtoInt(fSelectedTrain)];
  T0 := T.Schedule.StartDate;
  T1 := T.Schedule.Departs;
  AdvanceBy := RealMod(T0 - T1,1.0);
  // See if we have to update the train's schedule (i.e. if it leaves at midnight)
  if AdvanceBy <= 0.0 then
    begin
      T.Schedule.Clear;
      T0 := MagicTimeEdit1.DateTime;
      T.Schedule.Add( Starts, 0, Starts, T0, T0 );
      T0 := T0 + ( 1.0/24.0);  // End of run is 1 hour after start.
      T.Schedule.Add( Ends, 0, Ends, T0, T0 );
      T.Schedule.Sort;
    end;
  TrainListBox.Items.Delete(Idx);
end;

procedure TRegisterTrainForm.CheckOk;
var
  Idx : Integer;
begin
  Idx := TrainListBox.ItemIndex;
  ButtonOk.Enabled := (TrainListBox.Count > 0) and (Idx >= 0) and
                      (MagicTimeEdit1.DateTime > 0.0);
end;

procedure TRegisterTrainForm.MagicTimeEdit1TimeChange(Sender: TObject);
begin
  CheckOk;
end;

procedure TRegisterTrainForm.TrainListBoxDblClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := TrainListBox.ItemIndex;
  if Idx < 0 then exit;
  fSelectedTrain := TrainListBox.Items[Idx];
  TrainListBox.Items.Delete(Idx);
  ModalResult := mrOK;
end;

function TRegisterTrainForm.ShowModal : Integer;
var
  I : Integer;
  T : TTrainData;
begin
  TrainListBox.Clear;
  TrainListBox.Items.Assign(DispatcherForm.CrewCallListBox.Items);
  with IronMikeData do
    begin
      for I := 0 to pred( TrainList.Count ) do
        begin
          T := TrainList[I];
          if T.IsExtra and (T.Status = tsReady) then
            begin
              TrainListBox.AddItem( IntToStr(T.Number),T);
            end;
        end;
      StartComboBox.Clear;
      EndComboBox.Clear;
      //for I := 1 to StationsCount do    Change to stationlist when implemented
      //  begin
      //    StartComboBox.AddItem( StationName[I],TObject(I) );
      //    EndComboBox.AddItem( StationName[I],TObject(I) );
      //  end;
      for I := 1 to StationList.Count do
        begin
          StartComboBox.AddItem( StationList[I].Name, TObject(I) );
          EndComboBox.AddItem( StationList[I].Name, TObject(I) );
        end;
      StartComboBox.ItemIndex := -1;
      EndComboBox.ItemIndex   := -1;
     end;
  ButtonOk.Enabled := False;
  Result := inherited ShowModal;
end;

procedure TRegisterTrainForm.TrainListBoxClick(Sender: TObject);
var
  Idx : Integer;
  Pos : Integer;
  T   : TTrainData;
  C   : Integer; // Count of schedule
begin
  Idx := TrainListBox.ItemIndex;
  fSelectedTrain := TrainListBox.Items[Idx];
  //ButtonOk.Enabled := (TrainListBox.Count > 0) and (Idx >= 0);
  MagicTimeEdit1.DateTime := 0.0;
  if Idx >= 0 then
    begin
      T := IronMikeData.TrainList.TrainByNum[StrToInt(fSelectedTrain)];
      C := T.Schedule.Count;
      if C = 0 then
        begin
          StartComboBox.ItemIndex := -1; // LOOK HERE
          EndComboBox.ItemIndex   := -1;
        end
      else
        begin
          if T.Schedule.Departs > 0.0 then
            MagicTimeEdit1.DateTime := T.Schedule.Departs;
          Pos := T.Schedule[1].Pos - 1;
          StartComboBox.ItemIndex := Pos;
          Pos := T.Schedule[T.Schedule.Count].Pos - 1;
          EndComboBox.ItemIndex := Pos;
        end
    end;
  CheckOk;
end;

end.

