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

unit ScheduleUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TScheduleEntry }

  TScheduleEntry = class
  private
    fStation: String;
    function  GetTrack: String;
    procedure SetArrives(AValue: TDateTime);
    procedure SetDeparts(AValue: TDateTime);
  protected
    fStnIdx : Integer;   // Index of station in table (1=Starting point, Count=Ending point)
    fStnPos : Integer;   // Position of station in table (1=SLO, 5=WAT);
    fTrkIdx : Integer;
    fArrives,
    fDeparts : TDateTime;
  public
    constructor Create( theStn, theTrk, thePos : Integer; Arrival, Departure : TDateTime ); overload;
    constructor Create( aScheduleEntry : TScheduleEntry ); overload;

    procedure Advance( By : TDateTime; DepartOnly : Boolean );

    property Idx     : Integer read fStnIdx;
    property Pos     : Integer read fStnPos;
    property Arrives : TDateTime read fArrives write SetArrives;
    property Departs : TDateTime read fDeparts write SetDeparts;
    property Track   : String    read GetTrack;
    property Station : String    read fStation;
  end;

  TSchedule = class
  private
    TimeList : TObjectList; // Both lists point to the same data.
    PosList  : TObjectList; // TimeList is the owner
    function GetArrives: TDateTime;   // When train arrives at last station
    function GetAssigned: Boolean;
    function GetByPos(Pos : Integer): TScheduleEntry;
    function GetCount: Integer;
    function GetDeparts: TDateTime;   // When train departs from first station
    function GetScheduleEntry(Idx : Integer): TScheduleEntry;
    function GetStartDate: TDateTime;
    procedure SetArrives(AValue: TDateTime);
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Clear; // If we need to create a new schedule for an extra
    procedure   Assign( aSchedule : TSchedule );

    procedure Advance( By : TDateTime; From : Integer = 0);

    procedure Add( theStn, theTrk, thePos : Integer;
                   Arrival, Departure : TDateTime );

    procedure Debuglog( Where : String );
    procedure Sort;
    // NOTE:  Pos is a 1-based index so that the station pos agrees with
    //        the schedule pos.  This eliminates a lot of hacking code.
    // The entry property is ordered by departure time (ends of line should have
    // arrival set equal to departure) such that entry[1] is the starting station
    // and entry[Count] is the last station on the line.
    property Entry[Idx : Integer] : TScheduleEntry read  GetScheduleEntry; default;
    // ByPos returns the schedule entry by geographic location (i.e. WAT = 1,
    // SLO = 5)
    property ByPos[Pos : Integer] : TScheduleEntry read GetByPos;
    property Count : Integer read GetCount;
    property Departs : TDateTime read GetDeparts;
    property Arrives : TDateTime read GetArrives write SetArrives;
    property Assigned : Boolean read GetAssigned;
    property StartDate : TDateTime read GetStartDate;
  end;


implementation

uses
  CommonLog,
  IronMikeDataUnit,
  DateUtils;

{ TSchedule }

procedure TSchedule.Add(theStn, theTrk, thePos: Integer; Arrival,
  Departure: TDateTime);
var
  theEntry : TScheduleEntry;
begin
  theEntry := TScheduleEntry.Create( theStn, theTrk, thePos, Arrival, Departure);
  TimeList.Add( theEntry );
  PosList.Add( theEntry )
end;

procedure TSchedule.Advance(By: TDateTime; From: Integer);
var
  I : Integer;
  E : TScheduleEntry;
  //s1, s2: string;
begin
  if From <= 0 then
    From := 1;
  for I := pred(From) to pred(TimeList.Count) do
    begin
      Log.FormatLn( 'Advance at %d',[i] );
      if I < 0 then
        raise Exception.Create('Advance error');
      E := TScheduleEntry(TimeList.Items[I]);
      //s1 := ironmikedata.RRTime(e.Departs);
      E.Advance( By, I = pred(From) );
      //s2 := ironmikedata.RRTime(e.Departs);
    end;
end;

procedure TSchedule.Assign( aSchedule : TSchedule );
var
  I : Integer;
  E : TScheduleEntry;
begin
  TimeList.Clear;
  PosList.Clear;
  for I := 1 to aSchedule.TimeList.Count do
    begin
      E :=  TScheduleEntry.Create( aSchedule[I] );
      TimeList.Add( E );
      PosList.Add( E );
    end;
  Sort;
end;

procedure TSchedule.Clear;
begin
  TimeList.Clear;
  PosList.Clear;
end;

constructor TSchedule.Create;
begin
  TimeList := TObjectList.Create;
  PosList  := TObjectList.Create( False );
end;

procedure TSchedule.Debuglog(Where: String);
var
  I : Integer;
  E : TScheduleEntry;
begin
  Log.FormatLn('Schedule %s has %d Entries',[Where,Count]);
  for I := 1 to Count do
    begin
      E := Entry[I];
      if E.Arrives < 0.0 then
        Log.FormatLn('%s NotSet',[E.Station])
      else
        Log.FormatLn('%s %d %s %s',
          [ E.Station,
            E.Pos,
            IronMikeData.RRTime(E.Arrives),
            IronMikeData.RRTime(E.Departs) ] );
    end;
end;

destructor TSchedule.Destroy;
begin
  TimeList.Free;
  PosList.Free;
  inherited Destroy;
end;

function TSchedule.GetArrives: TDateTime;
begin
  Result := Entry[Count].Arrives;
end;

function TSchedule.GetAssigned: Boolean;
begin
  Result := Timelist.Count > 0;
end;

function TSchedule.GetByPos(Pos : Integer): TScheduleEntry;
begin
  Result := TScheduleEntry(PosList[Pos-1]);
end;

function TSchedule.GetCount: Integer;
begin
  Result := TimeList.Count;
end;

function TSchedule.GetDeparts: TDateTime;
begin
  Result := Entry[1].Departs;
end;

function TSchedule.GetScheduleEntry(Idx: Integer): TScheduleEntry;
begin
  Result := TScheduleEntry(TimeList.Items[Idx-1])
end;

function TSchedule.GetStartDate: TDateTime;
begin
  Result := IronMikeData.OpSession.StartDate;
end;

procedure TSchedule.SetArrives(AValue: TDateTime);
begin
  Entry[Count].Arrives := AValue;
end;

function ScheduleTimeCompare( L, R : Pointer ) : Integer;
begin
  if TScheduleEntry(L).Departs < TScheduleEntry(R).Departs then
    Result := -1
  else if TScheduleEntry(L).Departs > TScheduleEntry(R).Departs then
    Result := 1
  else // Default order is eastbound
    Result := TScheduleEntry(L).Pos - TScheduleEntry(R).Pos;
end;

function SchedulePosCompare( L, R : Pointer ) : Integer;
begin
  Result := TScheduleEntry(L).Pos - TScheduleEntry(R).Pos;
end;


procedure TSchedule.Sort;
begin
  TimeList.Sort(@ScheduleTimeCompare);
  PosList.Sort(@SchedulePosCompare);
end;

{ TScheduleEntry }

procedure TScheduleEntry.Advance(By: TDateTime; DepartOnly : Boolean );
begin
  if not DepartOnly then
    fArrives := fArrives + By;
  fDeparts := fDeparts + By;
end;

constructor TScheduleEntry.Create(theStn, theTrk, thePos: Integer; Arrival,
  Departure: TDateTime);
begin
  fStnIdx  := theStn;
  fStnPos  := thePos;
  //fStation := IronMikeData.StationName[Pos];  change to stations list when implemented
  fStation := IronMikeData.StationList[thePos].Name;
  if not (Arrival < 0.0) or (Departure < 0.0) then
    begin
      if Departure < IronMikeData.OpSession.StartTime then
        begin
          Departure := IncDay(Departure,1);
          Arrival  := IncDay(Arrival,1);
        end;
      Departure := Departure +  IronMikeData.OpSession.StartDate;
      Arrival  := Arrival  +  IronMikeData.OpSession.StartDate;
    end;
  fArrives  := Arrival;
  fDeparts  := Departure;
  fTrkIdx   := theTrk;

end;

constructor TScheduleEntry.Create(aScheduleEntry: TScheduleEntry);
begin
  fStnIdx     := aScheduleEntry.Idx;
  fStnPos     := aScheduleEntry.Pos;
  fArrives    := aScheduleEntry.Arrives;
  fDeparts    := aScheduleEntry.Departs;
  fTrkIdx     := aScheduleEntry.fTrkIdx;
end;

// This isn't called very often so it doesn't pay to cache the value
function TScheduleEntry.GetTrack: String;
begin
  IronMIkeData.TracksQuery1.Close;
  IronMIkeData.TracksQuery1.ParamByName('stn_trk_idx').AsInteger := fTrkIdx;
  IronMIkeData.TracksQuery1.Open;
  Result := IronMIkeData.TracksQuery1.FieldByName('name').AsString;
  IronMIkeData.TracksQuery1.Close;
end;

procedure TScheduleEntry.SetArrives(AValue: TDateTime);
begin
  if fArrives=AValue then Exit;
  fArrives:=AValue;
end;

procedure TScheduleEntry.SetDeparts(AValue: TDateTime);
begin
  if fDeparts=AValue then Exit;
  fDeparts:=AValue;
end;

end.

