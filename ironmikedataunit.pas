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

unit IronMikeDataUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn, FileUtil, DB, Grids, Contnrs,
  ScheduleUnit, TrainsUnit, StationsUnit;

type

  TIronMikeDataError = class(Exception);

  {TOpSessionDataRec}

  TOpSessionDataRec = record
    UseYear       : Integer;
    TodaysDate    : Boolean;
    StartDate     : Double;
    StartTime     : Double;
    FastClock     : Integer;
    CrewCallLead  : Double;
  end;

  {TRailroadDataRec}

  TRailroadDataRec = record
    Name               : String;
    President          : String;
    Dispatcher         : String;
    SuperiorDirection  : Integer;
    StandardDirections : Integer;
    ClockAMPM          : Boolean;
  end;

  { TCrewCallList }

  TCrewCallList = class(TObjectList)
  public
    constructor Create;
    procedure   Assign( Src : TObjectList );
  end;

  { TIronMikeData }

  TIronMikeData = class(TDataModule)
    Connection: TSQLite3Connection;
    RailroadQuery: TSQLQuery;
    OpSessionQuery: TSQLQuery;
    StationsQuery1: TSQLQuery;
    StationsCountQuery: TSQLQuery;
    StationsQuery2: TSQLQuery;
    StationsQuery3: TSQLQuery;
    TimetableQuery: TSQLQuery;
    StationsQuery: TSQLQuery;
    StationsQueryDup: TSQLQuery;
    TimetableQuery1: TSQLQuery;
    TimetableQuery2: TSQLQuery;
    TimetableQuery3: TSQLQuery;
    TimetableQuery4: TSQLQuery;
    TracksQuery: TSQLQuery;
    TracksQuery1: TSQLQuery;
    TracksQueryDup: TSQLQuery;
    TrainsQuery: TSQLQuery;
    TrainsQuery1: TSQLQuery;
    TrainsCountQuery: TSQLQuery;
    TrainsQuery2: TSQLQuery;
    Transaction1: TSQLTransaction;
    procedure AfterPost(DataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure StationsQueryAfterDelete(DataSet: TDataSet);
  private
    fCrewCallList: TCrewCallList;
    fDB: TDatabase;
    fFileName: String;
    fRailroadRec: TRailroadDataRec;
    fStationList: TStationList;
    fTrainList: TTrainList;
    { private declarations }
    procedure CreateATable(SQL : String);

    procedure CreateRailroadTable;
    function GetInferiorDirection: String;
    function GetOpSession: TOpSessionDataRec;
    function GetRailroadRec: TRailroadDataRec;
    function GetStationName(Pos : Integer): String;
    function GetStationPos(StnIdx : Integer): Integer;
    function GetStationPosByName(aName : String): Integer;
    function GetStationsCount: Integer;
    function GetSuperiorDirection: String;
    function GetTrain(Index : Integer): TTrainData;
    //function GetTrainByNum(theNumber : String): TTrainData; DRZ
    function GetTrainsCount: Integer;

    procedure InitializeRailroad;
    procedure InitializeOpSession;

    procedure CreateTrainsTable;

    procedure CreateStationsTable;

    procedure CreateTimetableTable;

    procedure CreateOpSessionTable;
    procedure SetOpSession(AValue: TOpSessionDataRec);
    //procedure SetTrainByNum(theNumber : String; AValue: TTrainData);   DRZ

    procedure TTRemoveDeletedTrainsAndStations;
    procedure TTAddNewTrainsAndStations;

    procedure SetFileName(AValue: String);
    procedure SetRailroadRec(AValue: TRailroadDataRec);
    procedure SetTrain(Index : Integer; AValue: TTrainData);
  public
    { public declarations }

    procedure Close(Force : Boolean = False);
    procedure SaveAs( NewPath : String );
    function  Restore( OldPath : String ) : String; // returns restored file name

    procedure TrainDelete( Idx : Integer );
    procedure TrainsUpdate( Grid : TStringGrid );
    procedure UpdateTrains( Grid : TStringGrid );
    procedure TTFindScheduledMeets;

    procedure PopulateTrainList; // From Tables to internal object;
    procedure PopulateStationList; // From Tables to internal object

    function IsSuperiorDirection( aDirection : String ) : Boolean;

    function Priority( aDirection, aSuperiority : String) : Integer;

    property FileName : String read fFileName write SetFileName;
    property Railroad : TRailroadDataRec read GetRailroadRec write SetRailroadRec;
    property SuperiorDirection : String read GetSuperiorDirection;
    property InferiorDirection : String read GetInferiorDirection;
    property OpSession         : TOpSessionDataRec read GetOpSession write SetOpSession;
    function RRTime( theTime : TDateTime ) : String;

    property Train[Index : Integer] : TTrainData    read GetTrain    write SetTrain;
    //property TrainByNum[theNumber : String] : TTrainData read GetTrainByNum write SetTrainByNum;

    function  StationIsDup( aName, aCode : String; Idx : Integer ) : Boolean;
    procedure StationDelete( Idx : Integer );
    procedure StationsUpdate( Grid : TStringGrid ); // Grid => Database
    procedure UpdateStations( Grid : TStringGrid ); // Database => Grid

    function  TrackIsDup( aName, aCode : String; Idx, StnIdx : Integer ) : Boolean;
    procedure TrackDelete( Idx : Integer );
    procedure TracksUpdate( Grid : TStringGrid; StationIdx : Integer );
    procedure UpdateTracks( Grid : TStringGrid; StationIdx : Integer );

    procedure PrepareTimetable;

    property StationsCount : Integer read GetStationsCount;
    property TrainsCount   : Integer read GetTrainsCount;

    //property StationName[Pos : Integer] : String read GetStationName;
    //property StationPos[StnIdx : Integer] : Integer read GetStationPos;
    //property StationPosByName[aName : String] :Integer read GetStationPosByName;

    property StationList   : TStationList read fStationList;
    property TrainList     : TTrainList read fTrainList;
    property CrewCallList  : TCrewCallList read fCrewCallList;
  end;

var
  IronMikeData: TIronMikeData;

implementation

uses
  CommonDebug, CommonIO, CommonMath, CommonLog,
  MainFormUnit,
  DateUtils,
  Stringsubs,
  Preferences;

{$R *.lfm}

const
  BackupTimeFormat = 'yyyy-mm-dd-hh-nn-ss';

{ TCrewCallList }

function CrewCallCompare( L, R : Pointer ) : Integer;
var
  LDeparts, RDeparts : TDateTime;
begin
  LDeparts := TTrainData(L).Schedule.Departs;
  RDeparts := TTrainData(R).Schedule.Departs;
  if LDeparts > RDeparts then
    Result := 1
  else if LDeparts < RDeparts then
    Result := -1
  else
    Result := 0;
end;

procedure TCrewCallList.Assign(Src: TObjectList);
var
  I : Integer;
  aTrain : TTrainData;
begin
  Clear;
  for I := 0 to pred(Src.Count) do
    begin
      aTrain := TTrainData(Src[I]);
      if (not aTrain.IsExtra) and (aTrain.Status = tsReady) then
        if IndexOf( aTrain ) < 0 then
          Add( aTrain );
    end;
  Sort( @CrewCallCompare );
end;

constructor TCrewCallList.Create;
begin
  inherited Create(False); // CrewCallList does NOT free its stored objects
end;

{ TIronMikeData }

procedure TIronMikeData.AfterPost(DataSet: TDataSet);
begin
  (Dataset as TSQLQuery).ApplyUpdates;
  Transaction1.CommitRetaining;
  (Dataset as TSQLQuery).Refresh;
end;

procedure TIronMikeData.Close(Force: Boolean);
begin
  Connection.Close(Force);
end;

procedure TIronMikeData.CreateATable(SQL: String);
begin
  try
    Connection.ExecuteDirect( 'CREATE TABlE IF NOT EXISTS' + SQL );
  except
    raise TIronMikeDataError.create('SQL:  [CREATE TABLE ' + SQL + ']');
  end;
end;

procedure TIronMikeData.CreateOpSessionTable;
begin
  CreateATable(  ' opsessions ( idx          INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                              ' use_year     INTEGER, ' +
                              ' todays_date  BOOLEAN, ' +
                              ' start_date   FLOAT, ' +
                              ' start_time   FLOAT, ' +
                              ' CrewCallLead FLOAT NOT NULL, ' +
                              ' fast_clock   INTEGER); ' );

  OpSessionQuery.DataBase := Connection;
  OpSessionQuery.Open;
  OpSessionQuery.First;
  if OpSessionQuery.EOF then
    InitializeOpSession;
end;

procedure TIronMikeData.CreateRailroadTable;
begin
  CreateATable(' railroad ( RRName     CHAR(63) NOT NULL, ' +
                          ' President  CHAR(63), ' +
                          ' Dispatcher CHAR(63) NOT NULL, ' +
                          ' SuperiorDirection INTEGER, ' +
                          ' StandardDirections INTEGER, ' +
                          ' ClockStyle INTEGER, ' +
                          ' Logo BLOB );' );

  RailroadQuery.DataBase := Connection;
  RailroadQuery.Open;
  RailroadQuery.First;
  if Empty(RailroadQuery.FieldByName('RRName').AsString) and
     Empty(RailroadQuery.FieldByName('President').AsString) and
     Empty(RailroadQuery.FieldByName('Dispatcher').AsString) then
    InitializeRailroad;
end;

procedure TIronMikeData.CreateStationsTable;
begin
  CreateATable(' stations ( stn_idx INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                          ' pos INTEGER, ' +
                          ' Name CHAR(31) UNIQUE NOT NULL, ' +
                          ' Code CHAR(4) UNIQUE NOT NULL );' );

  CreateATable(' station_tracks ( stn_trk_idx INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                                ' Name CHAR(31) NOT NULL, ' +
                                ' Code CHAR(4) NOT NULL, ' +
                                ' station_idx INTEGER NOT NULL, ' +
                                ' FOREIGN KEY(station_idx) REFERENCES stations(stn_idx));' );

  StationsQuery.DataBase         := Connection;
  StationsQuery1.DataBase        := Connection;
  StationsQuery2.DataBase        := Connection;
  StationsQuery3.DataBase        := Connection;
  StationsQueryDup.Database      := Connection;
  StationsCountQuery.DataBase    := Connection;
  TracksQuery.Database           := Connection;
  TracksQuery1.Database          := Connection;
  TracksQueryDup.Database        := Connection;
  StationsQuery.Open;
  TracksQuery.Open;
end;

procedure TIronMikeData.CreateTimetableTable;
begin
  CreateATable(' stops ( idx INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                       ' arrives REAL, ' +
                       ' departs REAL, ' +
                       ' trn_idx INTEGER NOT NULL, ' +
                       ' stn_idx INTEGER NOT NULL, ' +
                       ' trk_idx INTEGER, ' +
                       ' has_meet BOOLEAN DEFAULT FALSE ' +
                       // The following are boolean test columns that I needed
                       // to discover that the %f format rounds aggressively and
                       // needed to be replaced by %19.16f for timetable queries.
                       //  &R*W^&E&Qqq&&%&!!!!!

                       //' L0 BOOLEAN DEFAULT FALSE, ' +
                       //' L1 BOOLEAN DEFAULT FALSE, ' +
                       //' L2 BOOLEAN DEFAULT FALSE, ' +
                       //' L3 BOOLEAN DEFAULT FALSE, ' +
                       //' L4 BOOLEAN DEFAULT FALSE, ' +
                       //' L5 BOOLEAN DEFAULT FALSE, ' +
                       //' L6 BOOLEAN DEFAULT FALSE, ' +
                       //' L7 BOOLEAN DEFAULT FALSE ' +
                       ');' );

  TimetableQuery.Database := Connection;
  TimetableQuery1.Database := Connection;
  TimetableQuery2.Database := Connection;
  TimetableQuery3.Database := Connection;
  TimetableQuery.Open;
  TimetableQuery1.Open;
end;

procedure TIronMikeData.CreateTrainsTable;
begin
  CreateATable(' trains ( idx INTEGER PRIMARY KEY AUTOINCREMENT, ' +
                        ' number INTEGER NOT NULL, ' +
                        ' name CHAR(31), ' +
                        ' direction CHAR(10), ' +
                        ' consist CHAR(4), ' +
                        ' superiority CHAR(15), ' +   // superiority because 'class' is reserved word in SQL.
                        ' counterpart INTEGER, ' +
                        ' status INTEGER, ' +
                        ' flags INTEGER, ' +
                        ' stops_at INTEGER, ' + // Bit vector (allows up to 32 stations)
                        ' print_stops_at INTEGER );' );

  TrainsQuery.DataBase  := Connection;
  TrainsQuery1.Database := Connection;
  TrainsQuery2.Database := Connection;
  TrainsCountQuery.DataBase  := Connection;
  TrainsQuery.Open;
end;

procedure TIronMikeData.DataModuleCreate(Sender: TObject);
begin
  fTrainList    := TTrainList.Create;
  fCrewCallList := TCrewCallList.Create;
  fStationList  := TStationList.Create;
end;

procedure TIronMikeData.DataModuleDestroy(Sender: TObject);
begin
  //if Assigned( fTrainList ) then
  //  fTrainList.Free;
  if Assigned (fCrewCallList ) then
    fCrewCallList.Destroy;
end;

function TIronMikeData.GetInferiorDirection: String;
var
  RRsDirections : Integer; // 0 = East/West, 1 = North/South
  RRSuperior    : Integer; // 0 = East/North, 1 = West/South
begin
  RailroadQuery.Open;
  RailroadQuery.First;
  RRsDirections := RailroadQuery.FieldByName('StandardDirections').AsInteger;
  RRSuperior    := RailroadQuery.FieldByName('SuperiorDirection').AsInteger;
  case RRsDirections of
    0:
      case RRSuperior of
        0: Result := 'Eastbound';
        1: Result := 'Westbound';
      end;
    1:
      case RRSuperior of
        0: Result := 'Northbound';
        1: Result := 'Southbound';
      end;
  end;
end;

function TIronMikeData.GetOpSession: TOpSessionDataRec;
begin
  with OpSessionQuery do
    begin
      Close;
      Open;
      First;
      Result.UseYear      := FieldByName('use_year').AsInteger;
      Result.TodaysDate   := FieldByName('todays_date').AsBoolean;
      Result.StartDate    := FieldByName('start_date').AsFloat;
      Result.StartTime    := FieldByName('start_time').AsFloat;
      Result.FastClock    := FieldByName('fast_clock').AsInteger;
      Result.CrewCallLead := FieldByName('CrewCallLead').AsFloat;
      Close;
    end;
end;

function TIronMikeData.GetRailroadRec: TRailroadDataRec;
begin
  with Result, RailroadQuery do
    begin
      Open;
      Result.Name := FieldByName('RRName').AsString;
      President := FieldByName('President').AsString;
      Dispatcher := FieldByName('Dispatcher').AsString;
      SuperiorDirection := FieldByName('SuperiorDirection').AsInteger;
      StandardDirections := FieldByName('StandardDirections').AsInteger;
      ClockAMPM := FieldByName('ClockStyle').AsInteger <> 0;
    end;
end;

function TIronMikeData.GetStationName(Pos : Integer): String;
begin
  with StationsQuery2 do
    begin
      Close;
      ParamByName('pos').AsInteger := Pos;
      Open;
      if EOF then
        Result := '<none>'
      else
        Result := FieldByName('name').AsString;
      Close;
    end;
end;

function TIronMikeData.GetStationPos(StnIdx : Integer): Integer;
begin
  with StationsQuery1 do
    begin
      Close;
      ParamByName('stn_idx').AsInteger := StnIdx;
      Open;
      if EOF then
        Result := 0
      else
        Result := FieldByName('Pos').AsInteger;
      Close;
    end;
end;

function TIronMikeData.GetStationPosByName(aName : String): Integer;
begin
  with StationsQuery3 do
    begin
      Close;
      ParamByName('Name').AsString := aName;
      Open;
      if EOF then
        Result := 0
      else
        Result := FieldByName('Pos').AsInteger;
      Close;
    end;

end;

function TIronMikeData.GetStationsCount: Integer;
begin
  StationsCountQuery.Open;
  Result := StationsCountQuery.FieldByName('count(*)').AsInteger;
  StationsCountQuery.Close;
end;

function TIronMikeData.GetSuperiorDirection: String;
var
  RRsDirections : Integer; // 0 = East/West, 1 = North/South
  RRSuperior    : Integer; // 0 = East/North, 1 = West/South
begin
  RailroadQuery.Open;
  RailroadQuery.First;
  RRsDirections := RailroadQuery.FieldByName('StandardDirections').AsInteger;
  RRSuperior    := RailroadQuery.FieldByName('SuperiorDirection').AsInteger;
  case RRsDirections of
    0:
      case RRSuperior of
        0: Result := 'Westbound';
        1: Result := 'Eastbound';
      end;
    1:
      case RRSuperior of
        0: Result := 'Southbound';
        1: Result := 'Northbound';
      end;
  end;
end;

function TIronMikeData.GetTrain(Index : Integer): TTrainData;
var
  StnIdx : Integer;
begin
  with TrainsQuery1 do
    begin
      Close;
      ParamByName('Idx').AsInteger := Index;
      Open;
      // Must be stored in Trainlist which will free the data when its done.
      Result := TTrainData.Create( FieldByName('idx').AsInteger,
                                   FieldByName('Number').AsInteger,
                                   FieldByName('Name').AsString,
                                   FieldByName('Direction').AsString,
                                   FieldByName('Consist').AsString,
                                   //FieldByName('Status').AsInteger,
                                   FieldByName('Stops_at').AsInteger,
                                   FieldByName('print_Stops_At').AsInteger,
                                   FieldByName('Counterpart').AsInteger,
                                   FieldByName('Superiority').AsString );
      Close;
      // Get the timeout from the timetable database.
      TimetableQuery3.Close;
      TimetableQuery3.ParamByName('trn_idx').AsInteger := Index;
      TimetableQuery3.Open;
      TimetableQuery3.First;
      while not TimetableQuery3.EOF do
        begin
{ TODO -odonz -cModeling Issue : Worry about tracks }
          StnIdx := TimetableQuery3.FieldByName('stn_idx').AsInteger;
          Result.Schedule.Add( StnIdx,
                               TimetableQuery3.FieldByName('trk_idx').AsInteger,
                               StationList.ByIdx[StnIdx].Pos,//StationPos[StnIdx],
                               TimetableQuery3.FieldByName('Arrives').AsFloat,
                               TimetableQuery3.FieldByName('Departs').AsFloat );
          TimetableQuery3.Next;
        end;
      TimetableQuery3.Close;
      Result.Schedule.Sort;  // By departure time.
    end;
end;

//function TIronMikeData.GetTrainByNum(theNumber : String): TTrainData;
//var
//  Idx : Integer;
//begin
//  TrainsQuery2.Close;
//  TrainsQuery2.ParamByName('Number').AsString := theNumber;
//  TrainsQuery2.Open;
//  Idx := TrainsQuery2.FieldByName('idx').AsInteger;
//  TrainsQuery2.Close;
//  Result := Train[Idx];
//end;

function TIronMikeData.GetTrainsCount: Integer;
begin
  TrainsCountQuery.Open;
  Result := TrainsCountQuery.FieldByName('count(*)').AsInteger;
  TrainsCountQuery.Close;
end;

procedure TIronMikeData.InitializeOpSession;
begin
  with OpSessionQuery do
    begin
      Open;
      Insert;
      FieldByName('start_date').AsFloat := PreferencesForm.DefaultStartDate;
      FieldByName('use_year').AsInteger := 1950;
      FieldByName('Start_time').AsFloat := 0.0;
      FieldByName('fast_clock').AsInteger := 10;
      FieldByName('todays_date').AsBoolean := False;
      FieldByName('CrewCallLead').AsFloat := StrToDateTime( '1:30' );
      Post;
    end;
end;


procedure TIronMikeData.InitializeRailroad;
var
  vRailroad : TRailroadDataRec;
begin
  with RailroadQuery, vRailroad do
    begin
      Name := 'The Railroad';
      President := 'The President';
      Dispatcher := 'Mike R. O''Soft';
      SuperiorDirection := 0; // Eastbound (Northbound)
      StandardDirections := 0; // East/West
      ClockAMPM := PreferencesForm.DefaultAMPM;

      Active := True;
      Insert;
      FieldByName('RRName').AsString := Name;
      FieldByName('President').AsString := President;
      FieldByName('Dispatcher').AsString := Dispatcher;
      FieldByName('SuperiorDirection').AsInteger := SuperiorDirection;
      FieldByName('StandardDirections').AsInteger := StandardDirections;
      FieldByName('ClockStyle').AsInteger := ord( ClockAMPM );
      Post;
      First;
    end;
end;

function TIronMikeData.IsSuperiorDirection(aDirection: String): Boolean;
var
  S : String;
begin
  case Railroad.StandardDirections of
    0:
      case Railroad.SuperiorDirection of
        0: S := 'Eastbound';
        1: S := 'Westbound';
      end;
    1:
      case Railroad.SuperiorDirection of
        0: S := 'Northbound';
        1: S := 'Southbound';
      end;
  end;
  Result := UpperCase(S) = UpperCase(aDirection);
end;

procedure TIronMikeData.PopulateStationList;
var
  S : TStationData;
  T : TTrackData;
  sName, sCode : String;
  sPos,  sIdx  : Integer;
  tName, tCode : String;
  tPos,  tIdx  : Integer;
begin
  StationList.Clear;
  StationsQuery.Close;
  StationsQuery.Open;
  StationsQuery.First;

  while not StationsQuery.EOF do with StationsQuery do
    begin
      sName := FieldByName('Name').AsString;
      sCode := FieldByName('Code').AsString;
      sPos  := FieldByName('Pos').AsInteger;
      sIdx  := FieldByName('stn_idx').AsInteger;
      S := TStationData.Create(sName, sCode, sPos, sIdx);
      TracksQuery.Close;
      TracksQuery.ParamByName('station_idx').AsInteger := sIdx;
      TracksQuery.Open;
      TracksQuery.First;
      while not TracksQuery.EOF do with TracksQuery do
        begin
          tName := FieldByName('Name').AsString;
          tCode := FieldByName('Code').AsString;
          tPos  := FieldByName('stn_trk_idx').AsInteger; { TODO 1 -odonz -cInteface : Add Pos field to Table and forms.  then change stn_trk_idx here to pos }
          tIdx  := FieldByName('stn_trk_idx').AsInteger;
          T := TTrackData.Create( tName, tCode, tPos, tIdx );
          S.Tracks.Add( T );
          Next;
        end;
      StationList.Add( S );
      Next;
    end;
end;

procedure TIronMikeData.PopulateTrainList;
var
  Idx : Integer;
  TrainData : TTrainData;
begin
  TrainList.StartDate := OpSession.StartDate;
  TrainList.StartTime := OpSession.StartTime;
  TrainList.Clear;

  TrainsQuery.Close;
  TrainsQuery.Open;
  TrainsQuery.First;
  while not TrainsQuery.EOF do
    begin
      Idx := TrainsQuery.FieldByName('idx').AsInteger;
      TrainData := Train[Idx];
      TrainList.Add(TrainData);
      TrainsQuery.Next;
    end;
  TrainsQuery.Close;

  TrainList.Sort;

end;

procedure TIronMikeData.PrepareTimetable;
begin
  TTRemoveDeletedTrainsAndStations;
  TTAddNewTrainsAndStations;
  TTFindScheduledMeets;
end;

function TIronMikeData.Priority(aDirection, aSuperiority: String): Integer;
const
  TrainClasses : array[1..5] of String = ( 'FIRST CLASS','SECOND CLASS',
                                           'THIRD CLASS',
                                           'FOURTH CLASS', 'FIFTH CLASS' );
var
  I : Integer;
  TrainClass : Integer;
begin
  aSuperiority := UpperCase( aSuperiority );
  TrainClass := 6; // Lowest (im)possible class
  for I := 1 to 5 do
    if aSuperiority = TrainClasses[I] then
      begin
        TrainClass := I;
        break;
      end;
  Result := TrainClass*10;
  if not IsSuperiorDirection( aDirection ) then
    Result := Result + 1;
end;

function TIronMikeData.Restore(OldPath: String) : String;
var
  NewPath : String;
  Len     : Integer;
begin
  Connection.Close;
  Len := Length(RemoveExt(OldPath)) - Length(BackupTimeFormat);
  NewPath := Copy(OldPath,1,Len) + '.opsx';
  CopyFile( OldPath, NewPath );
  FileName := NewPath;
  Result := NewPath;
end;

function TIronMikeData.RRTime(theTime: TDateTime): String;
begin
  RailroadQuery.Open;
  RailroadQuery.First;
  case RailroadQuery.FieldByName('ClockStyle').AsInteger of
    0:  Result := FormatDateTime( 'hh:nn', theTime );  // 24 Hour Time
    1:  Result := FormatDateTime( 'hh:nn am/pm', theTime );  // 12 Hour Time
  end;
end;

procedure TIronMikeData.SaveAs(NewPath: String);
begin
  Connection.Close(True);
  CopyFile( FileName, NewPath );
end;

procedure TIronMikeData.SetFileName(AValue: String);
var
  BackupFile : String;
  DTS        : String;
begin
  fFileName := AValue;
  Connection.Close;
  if Empty(AValue) then exit; // We sometimes need a blank file name
  Connection.DatabaseName := AValue;
  Connection.Open;
  Transaction1.Active := True;
  CreateRailroadTable;   // Creates table iff it doesn't already exist.
  CreateTrainsTable;
  CreateStationsTable;
  CreateTimetableTable;
  CreateOpSessionTable;
  Transaction1.Commit;
  Connection.Close;
  DTS := FormatDateTime(BackupTimeFormat,Now);
  BackupFile := RemoveExt(AValue) + DTS + '.opsbak';
  CopyFile( aValue, BackupFile );
  Connection.Open;
end;

procedure TIronMikeData.SetOpSession(AValue: TOpSessionDataRec);
begin
  with AValue, OpSessionQuery do
    begin
      Close;
      Open;
      First;
      Edit;
      FieldByName('use_year').AsInteger    := UseYear;
      FieldByName('todays_date').AsBoolean := TodaysDate;
      FieldByName('start_date').AsFloat    := StartDate;
      FieldByName('start_time').AsFloat    := StartTime;
      FieldByName('fast_clock').AsInteger  := FastClock;
      FieldByName('CrewCallLead').AsFloat  := CrewCallLead;
      Post;
      Close;
    end;
end;

procedure TIronMikeData.SetRailroadRec(AValue: TRailroadDataRec);
begin
  fRailroadRec:=AValue;
  with RailroadQuery, Railroad do
    begin
      Active := True;
      Edit;
      FieldByName('RRName').AsString := Name;
      FieldByName('President').AsString := President;
      FieldByName('Dispatcher').AsString := Dispatcher;
      FieldByName('SuperiorDirection').AsInteger := SuperiorDirection;
      FieldByName('StandardDirections').AsInteger := StandardDirections;
      FieldByName('ClockStyle').AsBoolean := PreferencesForm.DefaultAMPM;
      Post;
      First;
    end;
end;

procedure TIronMikeData.SetTrain(Index : Integer; AValue: TTrainData);
var
  Idx : Integer;
begin
  Idx := Index;
  with AValue, TrainsQuery1 do
    begin
      Close;
      ParamByName('idx').AsInteger := Idx;
      Open;
      Edit;
      FieldByName('Number').AsInteger         := Number;
      FieldByName('Name').AsString            := Name;
      FieldByName('Direction').AsString       := Direction;
      FieldByName('Consist').AsString         := Consist;
      FieldByName('Status').AsInteger         := ord(Status);
      FieldByName('Stops_at').AsInteger       := StopsAt;
      FieldByName('print_Stops_At').AsInteger := PrintStopsAt;
      FieldByName('Counterpart').AsInteger    := Counterpart;
      Post;
      Close;
    end;
end;

//procedure TIronMikeData.SetTrainByNum(theNumber : String; AValue: TTrainData);
//var
//  Idx : Integer;
//begin
//  TrainsQuery2.Close;
//  TrainsQuery2.ParamByName('Number').AsString := theNumber;
//  TrainsQuery2.Open;
//  Idx := TrainsQuery2.FieldByName('idx').AsInteger;
//  TrainsQuery2.Close;
//  Train[Idx] := AValue;
//end;

procedure TIronMikeData.StationDelete(Idx: Integer);
begin
  Transaction1.Active := True;
  (StationsQuery.DataBase as TSQLConnection).ExecuteDirect(
    'DELETE FROM stations WHERE stn_idx=' + IntToStr(Idx) );
  Transaction1.CommitRetaining;
  Transaction1.Active := False;
end;

function TIronMikeData.StationIsDup(aName, aCode: String; Idx: Integer
  ): Boolean;
var
  Count : Integer;
begin
  StationsQueryDup.Close;
  StationsQueryDup.ParamByName('name').AsString := aName;
  StationsQueryDup.ParamByName('code').AsString := aCode;
  StationsQueryDup.ParamByName('stn_idx').AsInteger := Idx;
  StationsQueryDup.Open;
  Count := StationsQueryDup.FieldByName('count(*)').AsInteger;
  Result := Count <> 0;
end;

procedure TIronMikeData.StationsQueryAfterDelete(DataSet: TDataSet);
begin
   AfterPost( DataSet );
end;

procedure TIronMikeData.StationsUpdate(Grid: TStringGrid);
var
  I : Integer;
  Idx : Integer;
begin
  StationsQuery.Active := True;
  for I := 1 to pred(Grid.RowCount) do
    begin
      if Grid.Objects[0,I] = nil then
        begin
          StationsQuery.Insert;
          StationsQuery.FieldByName('name').AsString := UpCase(Grid.Cells[0,I]);
          StationsQuery.FieldByName('code').AsString := UpperCase(Grid.Cells[1,I]);
          StationsQuery.FieldByName('pos').AsInteger := I;
          StationsQuery.Post;
        end
      else
        begin
          Idx := Integer( Pointer( Grid.Objects[0,I] ) );
          StationsQuery.Edit;
          StationsQuery.FieldByName('stn_idx').AsInteger := Idx;
          StationsQuery.FieldByName('name').AsString := UpCase(Grid.Cells[0,I]);
          StationsQuery.FieldByName('code').AsString := UpperCase(Grid.Cells[1,I]);
          StationsQuery.FieldByName('pos').AsInteger := I;
          StationsQuery.Post;
        end;
    end;
  StationsQuery.Active := False;
end;

procedure TIronMikeData.TrackDelete(Idx: Integer);
var
  SQL : String;
begin
  Transaction1.Active := True;
  SQL := 'DELETE FROM StationTracks WHERE stn_trk_idx=' + IntToStr(Idx) + ';';

  (TracksQuery.DataBase as TSQLConnection).ExecuteDirect( SQL );
  Transaction1.CommitRetaining;
  Transaction1.Active := False;
end;

function TIronMikeData.TrackIsDup(aName, aCode: String; Idx, StnIdx: Integer
  ): Boolean;
var
  Count : Integer;
begin
  TracksQueryDup.Close;
  TracksQueryDup.ParamByName('name').AsString := aName;
  TracksQueryDup.ParamByName('code').AsString := aCode;
  TracksQueryDup.ParamByName('stn_trk_idx').AsInteger := Idx;
  TracksQueryDup.ParamByName('stn_idx').AsInteger := StnIdx;
  TracksQueryDup.Open;
  Count := TracksQueryDup.FieldByName('count(*)').AsInteger;
  Result := Count <> 0;
end;

procedure TIronMikeData.TracksUpdate(Grid: TStringGrid; StationIdx: Integer);
var
  I : Integer;
  Idx : Integer;
begin
  if StationIdx < 1 then
    begin
      Stub('Invalid station index:  ' + IntToStr(StationIdx));
      exit;
    end;
  TracksQuery.Active := True;
  for I := 1 to pred(Grid.RowCount) do
    begin
      if Grid.Objects[0,I] = nil then
        begin
          TracksQuery.Insert;
          TracksQuery.FieldByName('name').AsString := UpCase(Grid.Cells[0,I]);
          TracksQuery.FieldByName('code').AsString := UpperCase(Grid.Cells[1,I]);
          TracksQuery.FieldByName('station_idx').AsInteger := StationIdx;
          TracksQuery.Post;
        end
      else
        begin
          Idx := Integer( Pointer( Grid.Objects[0,I] ) );
          TracksQuery.Edit;
          TracksQuery.FieldByName('stn_trk_idx').AsInteger := Idx;
          TracksQuery.FieldByName('name').AsString := UpCase(Grid.Cells[0,I]);
          TracksQuery.FieldByName('code').AsString := UpperCase(Grid.Cells[1,I]);
          TracksQuery.Post;
        end;
    end;
  StationsQuery.Active := False;
end;

procedure TIronMikeData.TrainDelete(Idx: Integer);
var
  SQL : String;
begin
  Transaction1.Active := True;
  SQL := 'DELETE FROM trains WHERE idx=' + IntToStr(Idx) + ';';

  (TrainsQuery.DataBase as TSQLConnection).ExecuteDirect( SQL );
  Transaction1.CommitRetaining;
  Transaction1.Active := False;
end;

procedure TIronMikeData.TrainsUpdate(Grid: TStringGrid);
var
  I : Integer;
  Idx : Integer;
begin
  TrainsQuery.Active := True;
  for I := 1 to pred(Grid.RowCount) do
    begin
      if Grid.Objects[0,I] = nil then
        begin
          TrainsQuery.Insert;
          TrainsQuery.FieldByName('number').AsInteger := StrToInt(Grid.Cells[0,I]);
          TrainsQuery.FieldByName('name').AsString := Grid.Cells[1,I];
          TrainsQuery.FieldByName('direction').AsString := Grid.Cells[2,I];
          TrainsQuery.FieldByName('consist').AsString := Grid.Cells[3,I];
          TrainsQuery.FieldByName('superiority').AsString := Grid.Cells[4,I];
          TrainsQuery.FieldByName('counterpart').AsString := Grid.Cells[5,I];
          TrainsQuery.Post;
        end
      else
        begin
          Idx := Integer( Pointer( Grid.Objects[0,I] ) );
          TrainsQuery.Edit;
          TrainsQuery.FieldByName('idx').AsInteger := Idx;
          TrainsQuery.FieldByName('number').AsInteger := StrToInt(Grid.Cells[0,I]);
          TrainsQuery.FieldByName('name').AsString := Grid.Cells[1,I];
          TrainsQuery.FieldByName('direction').AsString := Grid.Cells[2,I];
          TrainsQuery.FieldByName('consist').AsString := Grid.Cells[3,I];
          TrainsQuery.FieldByName('superiority').AsString := Grid.Cells[4,I];
          TrainsQuery.FieldByName('counterpart').AsString := Grid.Cells[5,I];
          TrainsQuery.Post;
        end;
    end;
  TrainsQuery.Active := False;
end;

procedure TIronMikeData.TTAddNewTrainsAndStations;
var
  StnIdx, TrnIdx : Integer;
begin
  TrainsQuery.Open;
  TrainsQuery.First;
  while not TrainsQuery.EOF do
    begin
      TrnIdx := TrainsQuery.FieldByName('idx').AsInteger;
      StationsQuery.Open;
      StationsQuery.First;
      while not StationsQuery.EOF do
        begin
          StnIdx := StationsQuery.FieldByName('stn_idx').AsInteger;

          TimetableQuery1.Close;
          TimetableQuery1.ParamByName('stn_idx').AsInteger := StnIdx;
          TimetableQuery1.ParamByName('trn_idx').AsInteger := TrnIdx;
          TimetableQuery1.Open;
          TimetableQuery1.First;
          if TimetableQuery1.EOF then
            begin
              TimetableQuery.Open;
              TimetableQuery.Insert;
              TimetableQuery.FieldByName('stn_idx').AsInteger := StnIdx;
              TimetableQuery.FieldByName('trn_idx').AsInteger := TrnIdx;
              TimetableQuery.FieldByName('Trk_idx').AsInteger := 0; // None to start
              TimetableQuery.FieldByName('Arrives').AsFloat := 0.0;
              TimetableQuery.FieldByName('Departs').AsFloat := 0.0;
              TimetableQuery.Post;
            end;
          StationsQuery.Next;
        end;
      TrainsQuery.Next;
    end;
end;

procedure TIronMikeData.TTFindScheduledMeets;
var
  Arrives, Departs : TDateTime;
  StpIdx           : Integer; // For development testing
  TrnIdx           : Integer;
  StnIdx           : Integer;
  Query            : String;
  Bookmark         : TBookmark;

  DBArrives, DBDeparts : TDateTime;
  DBStpIdx           : Integer; // For development testing
  DBTrnIdx           : Integer;
  DBStnIdx           : Integer;

begin
  // Identify all meets (this is for development so far, but may be usable
  // for production later.)

  Connection.ExecuteDirect('UPDATE stops SET has_meet=0;');
  Connection.Transaction.CommitRetaining;
  TimetableQuery.Close; // Need this because TSQLQuery buffers and I missed the edits
  TimetableQuery.Open;
  TimetableQuery.First;
  while not TimetableQuery.EOF do
    begin
      Bookmark := TimetableQuery.GetBookmark;
      StpIdx   := TimetableQuery.FieldByName('Idx').AsInteger;
      TrnIdx   := TimetableQuery.FieldByName('Trn_Idx').AsInteger;
      StnIdx   := TimetableQuery.FieldByName('stn_Idx').AsInteger;
      Arrives  := TimetableQuery.FieldByName('Arrives').AsFloat;
      Departs  := TimetableQuery.FieldByName('Departs').AsFloat;

      if (Arrives > 0) and (Departs > 0) then
        begin
          // Note to self:  Do NOT use %f when formatting a query.  It rounds
          // aggressively for human output causing all sorts of strangeness.
          Query := Format( 'UPDATE stops SET ' +
                            //'  L0 = arrives >= %19.16f,' +
                            //'  L1 = arrives <= %19.16f,' +
                            //'  L2 = departs >= %19.16f,' +
                            //'  L3 = departs <= %19.16f,' +
                            //'  L4 = arrives <= %19.16f,' +
                            //'  L5 = departs >= %19.16f,' +
                            //'  L6 = arrives <= %19.16f,' +
                            //'  L7 = departs >= %19.16f,' +
                           '  has_meet = has_meet OR ' +
                           '((arrives >= %19.16f AND arrives <= %19.16f) OR ' +
                           '(departs >= %19.16f AND departs <= %19.16f)) OR ' +
                           '((arrives <= %19.16f AND departs >= %19.16f) OR ' +
                            '(arrives <= %19.16f AND departs >= %19.16f))' +
                           'WHERE ((stn_idx = %d) AND ' +
                                  '(trn_idx != %d));',
                                  [
                                  //Arrives,Departs,Arrives,Departs,
                                  //Arrives,Arrives,Departs,Departs,
                                  Arrives,Departs,Arrives,Departs,
                                  Arrives,Arrives,Departs,Departs,
                                    StnIdx, TrnIdx]);
          //Log.PutLn('Query: ' + Query);
          Connection.ExecuteDirect( Query );
          Connection.Transaction.CommitRetaining;
        end;
      TimetableQuery.GotoBookmark(Bookmark);
      TimetableQuery.Next;
    end;
end;

procedure TIronMikeData.TTRemoveDeletedTrainsAndStations;
var
  Query : String;
begin
  Query := 'DELETE FROM stops ' +
           'WHERE NOT EXISTS( SELECT 1 FROM trains ' +
                              'WHERE trains.idx = stops.trn_idx);';
  Connection.ExecuteDirect( Query );
  Connection.Transaction.CommitRetaining;
  Query := 'DELETE FROM stops ' +
           'WHERE NOT EXISTS( SELECT 1 FROM stations ' +
                              'WHERE stations.stn_idx = stops.stn_idx);';
  Connection.ExecuteDirect( Query );
  Connection.Transaction.CommitRetaining;
end;

procedure TIronMikeData.UpdateStations(Grid: TStringGrid);
var
  Idx : Integer;
  I   : Integer;
  RC  : Integer; // For Debug
begin
  Grid.RowCount := 1;   // Don't use Grid.Clear here, it confuses the component.
  RC := Grid.RowCount;
  StationsQuery.Active := True;
  StationsQuery.First;
  while not StationsQuery.EOF do
    begin
      I := StationsQuery.FieldByName('Pos').AsInteger;
      Grid.RowCount := Max( Grid.RowCount, I + 1);
      RC := Grid.RowCount;
      Grid.Cells[0,I] := StationsQuery.FieldByName('Name').AsString;
      Grid.Cells[1,I] := StationsQuery.FieldByName('Code').AsString;
      Idx := StationsQuery.FieldByName('stn_idx').AsInteger;
      Grid.Objects[0,I] := TObject( Pointer( Idx ) );
      StationsQuery.Next;
    end;
end;

procedure TIronMikeData.UpdateTracks(Grid: TStringGrid; StationIdx: Integer);
var
  Idx : Integer;
  I   : Integer;
  RC  : Integer; // For Debug
begin
  Grid.RowCount := 1;   // Don't use Grid.Clear here, it confuses the component.
  RC := Grid.RowCount;
  TracksQuery.Active := False;
  TracksQuery.ParamByName('station_idx').AsInteger := StationIdx;
  TracksQuery.Active := True;
  TracksQuery.First;
  I := 1;
  while not TracksQuery.EOF do
    begin
      Grid.RowCount := Max( Grid.RowCount, I + 1);
      RC := Grid.RowCount;
      Grid.Cells[0,I] := TracksQuery.FieldByName('Name').AsString;
      Grid.Cells[1,I] := TracksQuery.FieldByName('Code').AsString;
      Idx := TracksQuery.FieldByName('stn_trk_idx').AsInteger;
      Grid.Objects[0,I] := TObject( Pointer( Idx ) );
      Inc(I);
      TracksQuery.Next;
    end;
end;

procedure TIronMikeData.UpdateTrains(Grid: TStringGrid);
var
  I : Integer;
begin
  Grid.RowCount := 1;
  TrainsQuery.Active := True;
  TrainsQuery.First;
  I := 1;
  while not TrainsQuery.Eof do
    begin
      Grid.RowCount := I + 1;
      Grid.Cells[0,I] := TrainsQuery.FieldByName('Number').AsString;
      Grid.Cells[1,I] := TrainsQuery.FieldByName('Name').AsString;
      Grid.Cells[2,I] := TrainsQuery.FieldByName('Direction').AsString;
      Grid.Cells[3,I] := TrainsQuery.FieldByName('Consist').AsString;
      Grid.Cells[4,I] := TrainsQuery.FieldByName('Superiority').AsString;
      try
        Grid.Cells[5,I] := TrainsQuery.FieldByName('counterpart').AsString;
      except
      end;
      Grid.Objects[0,I] := TObject(Pointer(TrainsQuery.FieldByName('idx').AsInteger));
      Inc(I);
      TrainsQuery.Next;
    end;
end;

end.

