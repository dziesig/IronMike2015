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

unit TrainsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, FormPanel, MealyFiniteStateMachine, MagicStringGrid, Grids;

type

  { TTrainsForm }

  TTrainsForm = class(TPanelForm)
    ButtonTrainAbort: TSpeedButton;
    ButtonTrainAppend: TSpeedButton;
    ButtonTrainDelete: TSpeedButton;
    ButtonTrainEdit: TSpeedButton;
    ButtonTrainPost: TSpeedButton;
    GroupBox1: TGroupBox;
    TrainFSM: TMealyFiniteStateMachine;
    TrainGrid: TMagicStringGrid;
    procedure ButtonTrainAbortClick(Sender: TObject);
    procedure ButtonTrainAppendClick(Sender: TObject);
    procedure ButtonTrainDeleteClick(Sender: TObject);
    procedure ButtonTrainEditClick(Sender: TObject);
    procedure ButtonTrainPostClick(Sender: TObject);
    procedure TrainFSMEnterState(Sender: TComponent; AState: String);
    procedure TrainGridMagicSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure TrainGridValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    fSelectedCol: Integer;
    fSelectedRow: Integer;
    procedure SetSelectedRow(AValue: Integer);
  private
    { private declarations }
    procedure DoAppending;
    procedure DoStart;
    procedure DoCancelAppend;
    procedure DoEditing;
    procedure DoPosting;
    procedure DoDelete;

    property SelectedRow : Integer read fSelectedRow write SetSelectedRow;
    property SelectedCol : Integer read fSelectedCol write fSelectedCol;

  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
    procedure PreferencesChanged; override;
  end;

var
  TrainsForm: TTrainsForm;

implementation

uses
  CommonDebug, CommonMisc, Stringsubs,
  IronMikeDataUnit;

{$R *.lfm}

{ TTrainsForm }

procedure TTrainsForm.ButtonTrainAbortClick(Sender: TObject);
begin
  TrainFSM.Event('Cancel');
end;

procedure TTrainsForm.ButtonTrainAppendClick(Sender: TObject);
begin
  TrainFSM.Event('Append');
end;

procedure TTrainsForm.ButtonTrainDeleteClick(Sender: TObject);
begin
  TrainFSM.Event('Delete');
end;

procedure TTrainsForm.ButtonTrainEditClick(Sender: TObject);
begin
  TrainFSM.Event('Edit');
end;

procedure TTrainsForm.ButtonTrainPostClick(Sender: TObject);
begin
  TrainFSM.Event('Post');
end;

procedure TTrainsForm.DoAppending;
var
  Row : Integer;
begin
  ButtonTrainAppend.Enabled := False;
  ButtonTrainDelete.Enabled := False;
  ButtonTrainAbort.Enabled  := True;
  ButtonTrainEdit.Enabled   := False;
  ButtonTrainPost.Enabled   := False;  // Set true when row validation succeeds
  Row := TrainGrid.RowCount;
  TrainGrid.RowCount := Row + 1;
  SelectedCol := 0; // Start with Train name
  SelectedRow := Row;
  TrainGrid.Options := TrainGrid.Options + [goEditing];
end;

procedure TTrainsForm.DoCancelAppend;
var
  Row : Integer;
begin
  Row := TrainGrid.RowCount;
  Dec(Row);
  TrainGrid.RowCount := Row;
  SelectedCol := 0;
  SelectedRow := Row - 1;
  TrainFSM.Event('Done');
end;

procedure TTrainsForm.DoDelete;
var
  aName : String;
  aDirection : String;
  Idx : Integer;
begin
  aName := TrainGrid.Cells[0,SelectedRow];
  aDirection := TrainGrid.Cells[2,SelectedRow];
  if AreYouSure('Delete Train ' + aName + ' ' + aDirection) = mrYes then
    begin
      Idx := Integer(TrainGrid.Objects[0,SelectedRow]);
      TrainGrid.DeleteRow(SelectedRow);
      IronMikeData.TrainDelete(Idx);
      IronMikeData.TrainsUpdate( TrainGrid );
      IronMikeData.UpdateTrains( TrainGrid );
      TrainFSM.Event('Done');
    end
  else
    TrainFSM.Event('Cancel');
end;

procedure TTrainsForm.DoEditing;
begin
  ButtonTrainAppend.Enabled := False;
  ButtonTrainDelete.Enabled := False;
  ButtonTrainAbort.Enabled  := True;
  ButtonTrainEdit.Enabled   := False;
  ButtonTrainPost.Enabled   := False;  // Set true when row validation succeeds
  SelectedRow := SelectedRow;          // Highlight selected row
  TrainGrid.Options := TrainGrid.Options + [goEditing];
end;

procedure TTrainsForm.DoPosting;
begin
  IronMikeData.TrainsUpdate( TrainGrid );
  IronMikeData.UpdateTrains( TrainGrid );
  SelectedCol := 0;
  SelectedRow := SelectedRow; // Highlights the current row
  TrainFSM.Event('Done');
end;

procedure TTrainsForm.DoStart;
begin
  ButtonTrainAppend.Enabled := True;
  ButtonTrainDelete.Enabled := SelectedRow > 0;
  ButtonTrainPost.Enabled   := False;
  ButtonTrainAbort.Enabled  := False;
  ButtonTrainEdit.Enabled   := SelectedRow > 0;
  TrainGrid.Options := TrainGrid.Options - [goEditing];
end;

procedure TTrainsForm.Hiding;
begin

end;

procedure TTrainsForm.PreferencesChanged;
begin
  inherited PreferencesChanged;
  TrainGrid.Columns[2].Picklist.Clear;
  TrainGrid.Columns[2].Picklist.Add(IronMikeData.SuperiorDirection);
  TrainGrid.Columns[2].Picklist.Add(IronMikeData.InferiorDirection);
end;

procedure TTrainsForm.SetSelectedRow(AValue: Integer);
var
  Rect : TGridRect;
begin
  fSelectedRow:=AValue;
  Rect.Top := AValue;
  Rect.Bottom := AValue;
  Rect.Left := SelectedCol;
  Rect.Right := SelectedCol;
  TrainGrid.Selection := Rect;
  TrainGrid.SetFocus;
end;

procedure TTrainsForm.Showing;
var
  State : String;
begin
  PreferencesChanged;
  IronMikeData.UpdateTrains( TrainGrid );
  State := TrainFSM.CurrentState;
  if State = '' then
    TrainFSM.Event('Starting');
end;

procedure TTrainsForm.TrainFSMEnterState(Sender: TComponent; AState: String);
begin
  if      aState = 'START'               then DoStart
  else if aState = 'APPENDING'           then DoAppending
  else if aState = 'EDITING'             then DoEditing
  else if aState = 'POSTING'             then DoPosting
  else if aState = 'CANCELAPPEND'        then DoCancelAppend
  else if aState = 'DELETING'            then DoDelete
  else if aState = 'START0'              then begin end
  //else if aState = '' then Do;
  else
    Stub('State [' + aState + ']');
end;

procedure TTrainsForm.TrainGridMagicSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  State : String;
begin
  State := TrainFSM.CurrentState;
  if (State = 'APPENDING') or (State = 'EDITING') then
    CanSelect := aRow = SelectedRow
  else
    begin
      SelectedCol := aCol;
      SelectedRow := aRow;
      CanSelect := aRow > 0;
      ButtonTrainEdit.Enabled := CanSelect;
    end;
end;

procedure TTrainsForm.TrainGridValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
var
  Num : Integer;
begin
  if aCol = 0 then
    try
      if Empty(TrainGrid.Cells[0,aRow]) then
        begin
          NewValue := '';
          TrainGrid.Cells[0,aRow] := '';
        end
      else
        Num := StrToInt(TrainGrid.Cells[0,aRow])
    except
      NewValue := '';
      TrainGrid.Cells[0,aRow] := '';
    end
  else if aCol = 1 then
    NewValue := UpCase(NewValue)
  else if aCol = 2 then
    begin
      if Empty(NewValue) then
        NewValue := 'Extra'
      else if UpperCase(NewValue)[1] = IronMikeData.SuperiorDirection[1] then
        NewValue := IronMikeData.SuperiorDirection
      else if
        UpperCase(NewValue)[1] = IronMikeData.InferiorDirection[1] then
          NewValue := IronMikeData.InferiorDirection;
    end
  else if aCol = 3 then
    begin
      if Empty(NewValue) then
        begin end
      else if NewValue[1] in ['p','P'] then
        NewValue := 'PASS'
      else if NewValue[1] in ['f','F'] then
        NewValue := 'FRT';
    end
  else if aCol = 4 then
    if Empty(NewValue) then
        begin end
    else if NewValue[1] in ['f','F'] then
      NewValue := 'First Class'
    else if NewValue[1] in ['s','S'] then
      NewValue := 'Second Class'
    else if NewValue[1] in ['t','T'] then
      NewValue := 'Third Class'
    else if NewValue[1] in ['o','O'] then
      NewValue := 'Fourth Class'
    else if NewValue[1] in ['i','I'] then
      NewValue := 'Fifth Class';

  ButtonTrainPost.Enabled := not (Empty(TrainGrid.Cells[0,aRow]) or
                                  Empty(TrainGrid.Cells[0,aRow]) or
                                  Empty(TrainGrid.Cells[2,aRow]) or
                                  Empty(TrainGrid.Cells[3,aRow]) or
                                  Empty(TrainGrid.Cells[4,aRow]) );
end;

end.

