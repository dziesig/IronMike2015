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

unit StationsFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Buttons, FormPanel, MealyFiniteStateMachine, MagicStringGrid,
  db;

type

  { TStationsForm }

  TStationsForm = class(TPanelForm)
    ButtonEastward: TSpeedButton;
    ButtonStationAbort: TSpeedButton;
    ButtonStationAppend: TSpeedButton;
    ButtonStationDelete: TSpeedButton;
    ButtonStationEdit: TSpeedButton;
    ButtonStationPost: TSpeedButton;
    ButtonTrackAbort: TSpeedButton;
    ButtonTrackAppend: TSpeedButton;
    ButtonTrackDelete: TSpeedButton;
    ButtonTrackEdit: TSpeedButton;
    ButtonTrackPost: TSpeedButton;
    ButtonWestward: TSpeedButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StationFSM: TMealyFiniteStateMachine;
    StationGrid1: TMagicStringGrid;
    StationsDataSource: TDataSource;
    StationTracksDataSource: TDataSource;
    TrackFSM: TMealyFiniteStateMachine;
    TrackGrid: TStringGrid;
    TrackGrid1: TMagicStringGrid;
    procedure ButtonEastwardClick(Sender: TObject);
    procedure ButtonStationAbortClick(Sender: TObject);
    procedure ButtonStationAppendClick(Sender: TObject);
    procedure ButtonStationDeleteClick(Sender: TObject);
    procedure ButtonStationEditClick(Sender: TObject);
    procedure ButtonStationPostClick(Sender: TObject);
    procedure ButtonTrackAbortClick(Sender: TObject);
    procedure ButtonTrackAppendClick(Sender: TObject);
    procedure ButtonTrackDeleteClick(Sender: TObject);
    procedure ButtonTrackEditClick(Sender: TObject);
    procedure ButtonTrackPostClick(Sender: TObject);
    procedure ButtonWestwardClick(Sender: TObject);
    procedure StationFSMEnterState(Sender: TComponent; AState: String);
    procedure StationGrid1MagicSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StationGrid1ValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure TrackFSMEnterState(Sender: TComponent; AState: String);
    procedure TrackGrid1MagicSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure TrackGrid1ValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
  private
    fSelectedStnCol: Integer;
    fSelectedStnRow: Integer;
    fSelectedTrkCol: Integer;
    fSelectedTrkRow: Integer;
    fStationIdx: Integer;
    { private declarations }
    procedure DoStationStart;
    procedure DoStationAppending;
    procedure DoStationEditing;
    procedure DoStationPosting;
    procedure DoStationCancelAppend;
    procedure DoStationDelete;

    procedure DoTrackStart;
    procedure DoTrackAppending;
    procedure DoTrackEditing;
    procedure DoTrackPosting;
    procedure DoTrackCancelAppend;
    procedure DoTrackDelete;

    procedure DirectionButtonsEnable(No: Boolean = False);
    procedure SetSelectedStnRow(AValue: Integer);
    procedure SetSelectedTrkRow(AValue: Integer);
    procedure SetStationIdx(AValue: Integer);

    procedure StationButtonsEnable;
    procedure StationButtonsDisable;
    procedure TrackButtonsEnable;
    procedure TrackButtonsDisable;
  protected
    // Set SelectedStnCol before setting SelectedStnRow
    property SelectedStnCol : Integer read fSelectedStnCol write fSelectedStnCol;
    property SelectedStnRow : Integer read fSelectedStnRow write SetSelectedStnRow;

    property StationIdx : Integer read fStationIdx write SetStationIdx;

    property SelectedTrkCol : Integer read fSelectedTrkCol write fSelectedTrkCol;
    property SelectedTrkRow : Integer read fSelectedTrkRow write SetSelectedTrkRow;
  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
    procedure PreferencesChanged; override;
  end;

var
  StationsForm: TStationsForm;

implementation

uses
  CommonDebug, CommonMisc, StringSubs,
  IronMikeDataUnit;

{$R *.lfm}

{ TStationsForm }

procedure TStationsForm.ButtonEastwardClick(Sender: TObject);
begin
  StationGrid1.ExchangeColRow(False,SelectedStnRow,SelectedStnRow+1);
  IronMikeData.StationsUpdate( StationGrid1 );
  SelectedStnRow := SelectedStnRow + 1;
end;

procedure TStationsForm.ButtonStationAbortClick(Sender: TObject);
begin
   StationFSM.Event('Cancel');
end;

procedure TStationsForm.ButtonStationAppendClick(Sender: TObject);
begin
  StationFSM.Event('Append');
end;

procedure TStationsForm.ButtonStationDeleteClick(Sender: TObject);
begin
  StationFSM.Event('Delete');
end;

procedure TStationsForm.ButtonStationEditClick(Sender: TObject);
begin
  StationFSM.Event('Edit');
end;

procedure TStationsForm.ButtonStationPostClick(Sender: TObject);
begin
  StationFSM.Event('Post');
end;

procedure TStationsForm.ButtonTrackAbortClick(Sender: TObject);
begin
  TrackFSM.Event('Cancel');
end;

procedure TStationsForm.ButtonTrackAppendClick(Sender: TObject);
begin
  TrackFSM.Event('Append');
end;

procedure TStationsForm.ButtonTrackDeleteClick(Sender: TObject);
begin
  TrackFSM.Event('Delete');
end;

procedure TStationsForm.ButtonTrackEditClick(Sender: TObject);
begin
  TrackFSM.Event('Edit');
end;

procedure TStationsForm.ButtonTrackPostClick(Sender: TObject);
begin
  TrackFSM.Event('Post');
end;

procedure TStationsForm.ButtonWestwardClick(Sender: TObject);
begin
  StationGrid1.ExchangeColRow(False,SelectedStnRow,SelectedStnRow-1);
  IronMikeData.StationsUpdate( StationGrid1 );
  SelectedStnRow := SelectedStnRow - 1;
end;

procedure TStationsForm.DirectionButtonsEnable(No: Boolean);
var
  Rect : TGridRect;
begin
  Rect := StationGrid1.Selection;
  if (Rect.Top < 1) or No then
    begin
      ButtonEastward.Enabled := False;
      ButtonWestward.Enabled := False
    end
  else
    begin
      ButtonEastward.Enabled := Rect.Top < pred( StationGrid1.RowCount );
      ButtonWestward.Enabled := Rect.Top > 1;
    end;
end;

procedure TStationsForm.DoStationAppending;
var
  Row : Integer;
begin
  ButtonStationAppend.Enabled := False;
  ButtonStationDelete.Enabled := False;
  ButtonStationAbort.Enabled  := True;
  ButtonStationEdit.Enabled   := False;
  ButtonStationPost.Enabled   := False;  // Set true when row validation succeeds
  Row := StationGrid1.RowCount;
  StationGrid1.RowCount := Row + 1;
  SelectedStnCol := 0; // Start with station name
  SelectedStnRow := Row;
  StationGrid1.Options := StationGrid1.Options + [goEditing];
  DirectionButtonsEnable(True);
  TrackButtonsDisable;
end;

procedure TStationsForm.DoStationCancelAppend;
var
  Row : Integer;
begin
  Row := StationGrid1.RowCount;
  Dec(Row);
  StationGrid1.RowCount := Row;
  SelectedStnCol := 0;
  SelectedStnRow := Row - 1;
  TrackButtonsEnable;
  StationFSM.Event('Done');
end;

procedure TStationsForm.DoStationDelete;
var
  aName : String;
  Idx : Integer;
begin
  aName := StationGrid1.Cells[0,SelectedStnRow];
  if AreYouSure('Delete Station ' + aName) = mrYes then
    begin
      Idx := Integer(StationGrid1.Objects[0,SelectedStnRow]);
      StationGrid1.DeleteRow(SelectedStnRow);
      IronMikeData.StationDelete(Idx);
      IronMikeData.StationsUpdate( StationGrid1 );
      IronMikeData.UpdateStations( StationGrid1 );
      TrackButtonsEnable;
      StationFSM.Event('Done');
    end
  else
    StationFSM.Event('Cancel');
end;

procedure TStationsForm.DoStationEditing;
begin
  ButtonStationAppend.Enabled := False;
  ButtonStationDelete.Enabled := False;
  ButtonStationAbort.Enabled  := True;
  ButtonStationEdit.Enabled   := False;
  ButtonStationPost.Enabled   := False;  // Set true when row validation succeeds
  SelectedStnRow := SelectedStnRow;  // Highlight selected row
  StationGrid1.Options := StationGrid1.Options + [goEditing];
  TrackButtonsDisable;
  DirectionButtonsEnable(True);
end;

procedure TStationsForm.DoStationPosting;
begin
  IronMikeData.StationsUpdate( StationGrid1 );
  IronMikeData.UpdateStations( StationGrid1 );
  SelectedStnCol := 0;
  SelectedStnRow := SelectedStnRow; // Highlights the current row
  TrackButtonsEnable;
  StationFSM.Event('Done');
end;

procedure TStationsForm.DoStationStart;
begin
  ButtonStationAppend.Enabled := True;
  ButtonStationDelete.Enabled := SelectedStnRow > 0;
  ButtonStationPost.Enabled   := False;
  ButtonStationAbort.Enabled  := False;
  ButtonStationEdit.Enabled   := SelectedStnRow > 0;
  StationGrid1.Options := StationGrid1.Options - [goEditing];
end;

procedure TStationsForm.DoTrackAppending;
var
  Row : Integer;
begin
  ButtonTrackAppend.Enabled := False;
  ButtonTrackDelete.Enabled := False;
  ButtonTrackAbort.Enabled  := True;
  ButtonTrackEdit.Enabled   := False;
  ButtonTrackPost.Enabled   := False;  // Set true when row validation succeeds
  StationButtonsDisable;
  Row := TrackGrid1.RowCount;
  TrackGrid1.RowCount := Row + 1;
  SelectedTrkCol := 0; // Start with Track name
  SelectedTrkRow := Row;
  TrackGrid1.Options := TrackGrid1.Options + [goEditing];
end;

procedure TStationsForm.DoTrackCancelAppend;
var
  Row : Integer;
begin
  Row := TrackGrid1.RowCount;
  Dec(Row);
  TrackGrid1.RowCount := Row;
  SelectedTrkCol := 0;
  SelectedTrkRow := Row - 1;
  TrackFSM.Event('Done');
  TrackButtonsEnable;
  StationButtonsEnable;
end;

procedure TStationsForm.DoTrackDelete;
var
  aName : String;
  Idx : Integer;
begin
  aName := TrackGrid1.Cells[0,SelectedTrkRow];
  StationButtonsDisable;
  if AreYouSure('Delete Track ' + aName) = mrYes then
    begin
      Idx := Integer(TrackGrid1.Objects[0,SelectedTrkRow]);
      TrackGrid1.DeleteRow(SelectedTrkRow);
      IronMikeData.TrackDelete(Idx);
      IronMikeData.TracksUpdate( TrackGrid1, StationIdx );
      IronMikeData.UpdateTracks( TrackGrid1, StationIdx );
      TrackButtonsEnable;
      TrackFSM.Event('Done');
    end
  else
    TrackFSM.Event('Cancel');
  StationButtonsEnable;
end;

procedure TStationsForm.DoTrackEditing;
begin
  ButtonTrackAppend.Enabled := False;
  ButtonTrackDelete.Enabled := False;
  ButtonTrackAbort.Enabled  := True;
  ButtonTrackEdit.Enabled   := False;
  ButtonTrackPost.Enabled   := False;  // Set true when row validation succeeds
  StationButtonsDisable;
  SelectedTrkRow := SelectedTrkRow;  // Highlight selected row
  TrackGrid1.Options := TrackGrid1.Options + [goEditing];
end;

procedure TStationsForm.DoTrackPosting;
begin
  IronMikeData.TracksUpdate( TrackGrid1, StationIdx );
  IronMikeData.UpdateTracks( TrackGrid1, StationIdx );
  SelectedTrkCol := 0;
  SelectedTrkRow := SelectedTrkRow; // Highlights the current row
  StationButtonsEnable;
  TrackFSM.Event('Done');
end;

procedure TStationsForm.DoTrackStart;
begin
  ButtonTrackAppend.Enabled := SelectedStnRow > 0;
  ButtonTrackDelete.Enabled := SelectedTrkRow > 0;
  ButtonTrackPost.Enabled   := False;
  ButtonTrackAbort.Enabled  := False;
  ButtonTrackEdit.Enabled   := SelectedTrkRow > 0;
  TrackGrid1.Options := TrackGrid1.Options - [goEditing];
end;

procedure TStationsForm.Hiding;
begin
  //Stub('Hiding');
end;

procedure TStationsForm.PreferencesChanged;
begin
  inherited PreferencesChanged;
end;

procedure TStationsForm.SetSelectedStnRow(AValue: Integer);
var
  Rect : TGridRect;
begin
  fSelectedStnRow:=AValue;
  Rect.Top := AValue;
  Rect.Bottom := AValue;
  Rect.Left := SelectedStnCol;
  Rect.Right := SelectedStnCol;
  StationGrid1.Selection := Rect;
  DirectionButtonsEnable;
  StationIdx := Integer( StationGrid1.Objects[0,AValue] );
  StationGrid1.SetFocus;
end;

procedure TStationsForm.SetSelectedTrkRow(AValue: Integer);
var
  Rect : TGridRect;
begin
  fSelectedTrkRow:=AValue;
  Rect.Top := AValue;
  Rect.Bottom := AValue;
  Rect.Left := SelectedTrkCol;
  Rect.Right := SelectedTrkCol;
  TrackGrid1.Selection := Rect;
  TrackGrid1.SetFocus;
end;

procedure TStationsForm.SetStationIdx(AValue: Integer);
begin
  if fStationIdx=AValue then Exit;
  fStationIdx:=AValue;
  IronMikeData.UpdateTracks( TrackGrid1, fStationIdx );
end;

procedure TStationsForm.Showing;
var
  State : String;
begin
  IronMikeData.UpdateStations( StationGrid1 );
  IronMikeData.UpdateTracks(TrackGrid1,StationIdx);
  State := StationFSM.CurrentState;
  if State = '' then
    StationFSM.Event('Starting');
  State := TrackFSM.CurrentState;
  if State = '' then
    TrackFSM.Event('Starting');
end;

procedure TStationsForm.StationButtonsDisable;
begin
  ButtonWestward.Enabled := False;
  ButtonEastward.Enabled := False;
  ButtonStationAppend.Enabled := False;
  ButtonStationDelete.Enabled := False;
  ButtonStationPost.Enabled := False;
  ButtonStationAbort.Enabled := False;
  ButtonStationEdit.Enabled := False;
end;

procedure TStationsForm.StationButtonsEnable;
begin
  DirectionButtonsEnable;
  ButtonStationAppend.Enabled := True;
  ButtonStationDelete.Enabled := SelectedStnRow > 0;
  ButtonStationPost.Enabled := False;
  ButtonStationAbort.Enabled := False;
  ButtonStationEdit.Enabled := False;
end;

procedure TStationsForm.StationFSMEnterState(Sender: TComponent; AState: String
  );
begin
  if      aState = 'START'               then DoStationStart
  else if aState = 'APPENDING'           then DoStationAppending
  else if aState = 'EDITING'             then DoStationEditing
  else if aState = 'POSTING'             then DoStationPosting
  else if aState = 'CANCELAPPEND'        then DoStationCancelAppend
  else if aState = 'DELETING'            then DoStationDelete
  else if aState = 'START0'              then begin end
  //else if aState = '' then Do;
  else
    Stub('State [' + aState + ']');
end;

procedure TStationsForm.StationGrid1MagicSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  State : String;
begin
  State := StationFSM.CurrentState;
  if (State = 'APPENDING') or (State = 'EDITING') then
    CanSelect := aRow = SelectedStnRow
  else
    begin
      SelectedStnCol := aCol;
      SelectedStnRow := aRow;
      CanSelect := aRow > 0;
      ButtonStationEdit.Enabled := CanSelect;
    end;
end;

procedure TStationsForm.StationGrid1ValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  ButtonStationPost.Enabled := not (Empty(StationGrid1.Cells[0,aRow]) or
                                    Empty(StationGrid1.Cells[1,aRow]));
  if aCol = 0 then
    NewValue := UpCase( NewValue )
  else
    NewValue := UpperCase( NewValue );
end;

procedure TStationsForm.TrackButtonsDisable;
begin
  ButtonTrackAppend.Enabled := False;
  ButtonTrackDelete.Enabled := False;
  ButtonTrackEdit.Enabled := False;
  ButtonTrackAbort.Enabled := False;
  ButtonTrackPost.Enabled := False;
end;

procedure TStationsForm.TrackButtonsEnable;
begin
  ButtonTrackAppend.Enabled := SelectedStnRow > 0;
  ButtonTrackDelete.Enabled := False;
  ButtonTrackEdit.Enabled := False;
  ButtonTrackAbort.Enabled := False;
  ButtonTrackPost.Enabled := False;
end;

procedure TStationsForm.TrackFSMEnterState(Sender: TComponent; AState: String);
begin
  if      aState = 'START'               then DoTrackStart
  else if aState = 'APPENDING'           then DoTrackAppending
  else if aState = 'EDITING'             then DoTrackEditing
  else if aState = 'POSTING'             then DoTrackPosting
  else if aState = 'CANCELAPPEND'        then DoTrackCancelAppend
  else if aState = 'DELETING'            then DoTrackDelete
  else if aState = 'START0'              then begin end
  //else if aState = '' then Do;
  else
    Stub('State [' + aState + ']');
end;

procedure TStationsForm.TrackGrid1MagicSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  State : String;
begin
  State := TrackFSM.CurrentState;
  if (State = 'APPENDING') or (State = 'EDITING') then
    CanSelect := aRow = SelectedTrkRow
  else
    begin
      SelectedTrkCol := aCol;
      SelectedTrkRow := aRow;
      CanSelect := aRow > 0;
      ButtonTrackEdit.Enabled := CanSelect;
      ButtonTrackDelete.Enabled := CanSelect;
    end;
end;

procedure TStationsForm.TrackGrid1ValidateEntry(sender: TObject; aCol,
  aRow: Integer; const OldValue: string; var NewValue: String);
begin
  ButtonTrackPost.Enabled := not (Empty(TrackGrid1.Cells[0,aRow]) or
                                  Empty(TrackGrid1.Cells[1,aRow]));
  if aCol = 0 then
    NewValue := UpCase( NewValue )
  else
    NewValue := UpperCase( NewValue );
end;

end.

