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

unit TimetableFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, FormPanel, MagicStringGrid, MagicTimeEdit, Grids;

type

  { TTimetableForm }

  TTimetableForm = class(TPanelForm)
    Button1: TButton;
    TTStopGroupBox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TracksListBox: TListBox;
    TTGrid: TMagicStringGrid;
    ArrivalTimeEdit: TMagicTimeEdit;
    DepartureTimeEdit: TMagicTimeEdit;
    StaticText1: TStaticText;
    procedure ArrivalTimeEditTimeChange(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TracksListBoxClick(Sender: TObject);
    procedure TTGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure TTGridMagicSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
  private
    fSelectedStop: Pointer;
    procedure SetSelectedStop(AValue: Pointer);
  private
    { private declarations }
    RecursiveLoad : Boolean;
    procedure ShowTimetable;

    property SelectedStop : Pointer read fSelectedStop write SetSelectedStop;
  public
    { public declarations }
    procedure Hiding; override;
    procedure Showing; override;
    procedure PreferencesChanged; override;
  end;

var
  TimetableForm: TTimetableForm;

implementation

uses
  CommonDebug, CommonMath, CommonLog,
  Windows,
  IronMikeDataUnit;

{$R *.lfm}

{ TTimetableForm }

procedure TTimetableForm.ArrivalTimeEditTimeChange(Sender: TObject);
var
  Arrive, Depart : TDateTime;
  Idx            : Integer;
begin
  with IronMikeData do
    begin
      if RecursiveLoad then exit;
      if SelectedStop = nil then
        exit;
      RecursiveLoad := True;
      Arrive := ArrivalTimeEdit.DateTime;
      Depart := DepartureTimeEdit.DateTime;
      TimetableQuery2.Close;
      Idx := Int64(SelectedStop);
      TimetableQuery2.ParamByName('idx').AsInteger :=  Idx;
      TimetableQuery2.Open;
      TimetableQuery2.Edit;
      TimetableQuery2.FieldByName('arrives').AsFloat := Arrive;
      TimetableQuery2.FieldByName('departs').AsFloat := Depart;
      TimetableQuery2.Post;
      ShowTimetable;
      RecursiveLoad := False;
    end;
end;

procedure TTimetableForm.Button1Click(Sender: TObject);
var
  A, D : TDateTime;
begin
  A := ArrivalTimeEdit.DateTime;
  D := DepartureTimeEdit.DateTime;
  if (A = 0.0) and ( D <> 0.0 ) then
    ArrivalTimeEdit.DateTime := D
  else if (A <> 0.0) and (D = 0.0 ) then
    DepartureTimeEdit.DateTime := A
  else
    DepartureTimeEdit.DateTime := A;
  ArrivalTimeEditTimeChange(Sender);
end;

procedure TTimetableForm.FormCreate(Sender: TObject);
begin
  fSelectedStop := nil;
end;

procedure TTimetableForm.Hiding;
begin

end;

procedure TTimetableForm.PreferencesChanged;
begin
  inherited PreferencesChanged;
end;

procedure TTimetableForm.SetSelectedStop(AValue: Pointer);
begin
  if fSelectedStop=AValue then Exit;
  fSelectedStop:=AValue;
  if fSelectedStop = nil then
    begin end;
end;

procedure TTimetableForm.Showing;
begin
  IronMikeData.PrepareTimetable;  // Reconciles changes to stations/trains
  ShowTimetable;
end;

procedure TTimetableForm.ShowTimetable;
var
  I, J : Integer;
  W : Integer;
  TrnIdx, StnIdx : Integer;
  StopIdx : Int64;
  Arrives, Departs : Double;
  ArrivesStr, DepartsStr : String;
  HasMeet : Boolean;
begin
  //Windows.Beep(440,100);
  IronMikeData.TTFindScheduledMeets;
  TTGrid.RowCount := IronMikeData.TrainsCount + TTGrid.FixedRows;
  TTGrid.ColCount := IronMikeData.StationsCount + TTGrid.FixedCols;
  TTGrid.ColWidths[0] := 50;
  TTGrid.ColWidths[1] := 154;
  TTGrid.ColWidths[2] := 32;

  W := 0;
  for I := 0 to 2 do
    W := W + TTGrid.ColWidths[I];
  W := W + TTGrid.DefaultColWidth*Min( 5, IronMikeData.StationsCount );
  TTGrid.ClientWidth := W;
  TTGrid.Cells[0,0] := 'Train;No.';
  TTGrid.Cells[1,0] := 'Train;Name';

  TTGrid.ClientWidth := W;

  for I := 0 to pred(IronMikeData.TrainsCount) do
    begin
      TTGrid.Cells[2,I + TTGrid.FixedRows] := 'Arr.;Dep.';
    end;
  Application.ProcessMessages;
  I := 0;
  IronMikeData.StationsQuery.Open;
  IronMikeData.StationsQuery.First;
  while not IronMikeData.StationsQuery.Eof do
    begin
      TTGrid.Cells[I + TTGrid.FixedCols, 0] :=
        IronMikeData.StationsQuery.FieldByName('name').AsString;
      Inc(I);
      IronMikeData.StationsQuery.Next;
    end;

  IronMikeData.TrainsQuery.Open;
  IronMikeData.TrainsQuery.First;
  I := 0;
  while not IronMikeData.TrainsQuery.Eof do
    begin
      TrnIdx := IronMikeData.TrainsQuery.FieldByName('idx').AsInteger;
      TTGrid.Cells[0,I + TTGrid.FixedRows] := IronMikeData.TrainsQuery.FieldByName('number').AsString;
      TTGrid.Cells[1,I + TTGrid.FixedRows] := IronMikeData.TrainsQuery.FieldByName('name').AsString;
      J := 0;
      IronMikeData.StationsQuery.First;
      while not IronMikeData.StationsQuery.Eof do
        begin
          Application.ProcessMessages;
          StnIdx := IronMikeData.StationsQuery.FieldByName('stn_idx').AsInteger;

          IronMikeData.TimetableQuery1.Close;
          IronMikeData.TimetableQuery1.ParamByName('stn_idx').AsInteger := StnIdx;
          IronMikeData.TimetableQuery1.ParamByName('trn_idx').AsInteger := TrnIdx;
          IronMikeData.TimetableQuery1.Open;

          StopIdx := IronMikeData.TimetableQuery1.FieldByName('idx').AsInteger;
          Arrives := IronMikeData.TimetableQuery1.FieldByName('arrives').AsFloat;
          Departs := IronMikeData.TimetableQuery1.FieldByName('departs').AsFloat;
          HasMeet := IronMikeData.TimetableQuery1.FieldByName('has_meet').AsBoolean;

          ArrivesStr := IronMikeData.RRTime( Arrives );
          if HasMeet then ArrivesStr := ArrivesStr + '*';
          DepartsStr := IronMikeData.RRTime( Departs );
          if HasMeet then DepartsStr := DepartsStr + '*';
          TTGrid.Objects[J+TTGrid.FixedCols, I+TTGrid.FixedRows] := TObject( Pointer(StopIdx) );
          TTGrid.Cells[J+TTGrid.FixedCols, I+TTGrid.FixedRows] :=
            ArrivesStr + ';' + DepartsStr;
          Inc(J);
          IronMikeData.StationsQuery.Next;
          Application.ProcessMessages;
        end;
      Inc(I);
      IronMikeData.TrainsQuery.Next;
    end;
end;

procedure TTimetableForm.TracksListBoxClick(Sender: TObject);
begin
  with IronMikeData do
    begin
      TimetableQuery2.Close;
      TimetableQuery2.ParamByName('idx').AsInteger := Int64(SelectedStop);
      TimetableQuery2.Open;
      TimetableQuery2.Edit;
      TimetableQuery2.FieldByName('trk_idx').AsInteger :=
        Int64( Pointer(TracksListBox.Items.Objects[TracksListBox.ItemIndex]) );
      TimetableQuery2.Post;
    end;
end;

procedure TTimetableForm.TTGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
   Line1 : string ;
   Line2 : string ;
   ptr : integer ;
   padding : integer ;
   H : Integer;
begin
  H := aRect.Bottom - aRect.Top;
  padding := 3;
  ptr := Pos(';', TTGrid.Cells[aCol, aRow]) ;
  if ptr > 0 then begin
     Line1 := Copy(TTGrid.Cells[aCol, aRow], 1, ptr - 1) ;
     Line2 := Copy(TTGrid.Cells[aCol, aRow], ptr + 1,
                   Length(TTGrid.Cells[aCol,aRow]) - ptr) ;
  end
  else
     Line1 := TTGrid.Cells[aCol, aRow];
  if (SelectedStop <> nil) and
    ( Pointer ( TTGrid.Objects[aCol,aRow] ) = SelectedStop) then
    TTGrid.Canvas.Brush.Color := clLime;
  TTGrid.Canvas.FillRect(aRect);
  // Center the times, right justified.
  if (aCol >= TTGrid.FixedCols) and (aRow >= TTGrid.FixedRows) then
    begin
      padding := 30;
    end;
  TTGrid.Canvas.TextOut(aRect.Left + padding, aRect.Top + 2, Line1) ;
  if ptr > 0 then
     TTGrid.Canvas.TextOut(aRect.Left + padding, aRect.Top + (H div 2), Line2) ;
end;

procedure TTimetableForm.TTGridMagicSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
var
  I, J      : Integer;
  SelectedTrnIdx : Integer;
  SelectedStnIdx : Integer;
  SelectedTrkIdx : Integer;
  TrainCaption   : String;
  StationCaption : String;
  TrkIdx         : Int64;
  TrackListIdx   : Integer;
begin
  if (aRow >= TTGrid.FixedRows) and (aCol >= TTGrid.FixedCols) then
    begin
      CanSelect := True;
      SelectedStop := Pointer( TTGrid.Objects[aCol,aRow] );
      if SelectedStop = nil then exit;
      with IronMikeData do
        begin
          TimetableQuery2.Close;
          TimetableQuery2.ParamByName('idx').AsInteger := Int64(SelectedStop);
          TimetableQuery2.Open;
          SelectedTrnIdx := TimetableQuery2.FieldByName('trn_idx').AsInteger;
          SelectedStnIdx := TimetableQuery2.FieldByName('stn_idx').AsInteger;
          SelectedTrkIdx := TimetableQuery2.FieldByName('trk_idx').AsInteger;
        end;
      if (SelectedStnIdx = 0) and (SelectedTrnIdx = 0) then exit;
      with IronMikeData do
        begin
          TrainsQuery1.Close;
          TrainsQuery1.ParamByName('idx').AsInteger := SelectedTrnIdx;
          TrainsQuery1.Open;
          TrainCaption := TrainsQuery1.FieldByName('Number').AsString + ' - ' +
            TrainsQuery1.FieldByName('Direction').AsString;
          TTStopGroupBox.Caption := TrainCaption;

          StationsQuery1.Close;
          StationsQuery1.ParamByName('stn_idx').AsInteger := SelectedStnIdx;
          StationsQuery1.Open;
          StationCaption := StationsQuery1.FieldByName('Name').AsString;
          StaticText1.Caption := StationCaption;

          ArrivalTimeEdit.DateTime   := TimetableQuery2.FieldByName('arrives').AsFloat;
          DepartureTimeEdit.DateTime := TimetableQuery2.FieldByName('departs').AsFloat;

          TracksQuery.Close;
          TracksQuery.ParamByName('station_idx').AsInteger := SelectedStnIdx;
          TracksQuery.Open;
          TracksQuery.First;
          TracksListBox.Clear;
          TracksListBox.Items.Add('<None>');
          TrackListIdx := 0; // Highlights <None> by default
          while not TracksQuery.EOF do
            begin
              TrkIdx := TracksQuery.FieldByName('stn_trk_idx').AsInteger;
              I := TracksListBox.Items.AddObject(
                TracksQuery.FieldByName('Name').AsString,
                TObject( Pointer(TrkIdx) ) );
              if TrkIdx = SelectedTrkIdx then
                TrackListIdx := I;
              TracksQuery.Next;
            end;
          TracksListBox.ItemIndex := TrackListIdx;
        end;
    end;
end;

end.

