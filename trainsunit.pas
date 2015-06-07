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

unit TrainsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs,
  OrdersUnit, ScheduleUnit, DisplayUnit;

type
  { TTrainData }

  TTrainFlag = ( tfNone,
                 tfWhite,        // Extra
                 tfMultiWhite,   // Extra with multiple uses (e.g. helpers)
                 tfGreen1,       // First Section(s)
                 tfGreen2 );     // Last Section (actually no signals)

  TTrainStatus = ( tsReady,
                   tsRunning,
                   tsComplete );


  TTrainData = class
  private
    fIndex                : Integer;
    fNumber               : Integer;
    fPriority             : Integer;
    fName                 : String;
    fDirection            : String;
    fConsist              : String;
    fOrders               : TOrdersList;
    fSchedule             : TSchedule;
    fStatus               : TTrainStatus;
    fFlags                : TTrainFlag;
    fStopsAt              : Integer;   // Is bit vector 0 = None, 1 = SAL, 2 = KC, 3 = BOTH
    fPrintStopsAt         : Integer;
    fCounterpart          : Integer;
    function GetHasOrders: Boolean;
    function GetIsExtra: Boolean;
    function GetNumber: Integer;
    function GetNumber1: Integer;
    procedure SetFlags(AValue: TTrainFlag);
    procedure SetPrintStopsAt(AValue: Integer);
    procedure SetStatus(AValue: TTrainStatus);
  public
    constructor Create; overload;
    constructor Create( aIndex, aNumber : Integer;
                        aName, aDirection, aConsist : String;
                        aStopsAt, aPrintStopsAt, aCounterpart : Integer;
                        aSuperiority : String ); overload;
    // Starts and Ends are by Station Pos such that 1=WAT, 5=SLO
    constructor Create( aNumber : Integer; aConsist: String;
                        Starts, Ends : Integer;
                        theFlags : TTrainFlag;
                        aDirection, aSuperiority : String ); overload;
    constructor Create( aTrain : TTrainData ); overload; // Creates Second Section and sets flags
    destructor  Destroy; override;
  //
    procedure TrainIO( out EastIO, WestIO : String );
    function  TrainFlag : String;
    procedure StartRun; // Starts train (late if necessary);
    procedure RunLateFrom( StnPos : Integer; HowLate : TDateTime );
    procedure PrintClearance( vDisplay : TDisplay; vDisplayList : TDisplayList );
    procedure PrintInfo( vDisplay : TDisplay; vDisplayList : TDisplayList );
    procedure BuildTimetable;
    procedure DebugLog( Where : String; Disable : Boolean );
    function StationPos( When : TDateTime = -1.0) : Integer;
    function StationName( When : TDateTime = -1.0) : String;

    function IsSuperiorDirection : Boolean;

    function SetStopsAt( NewStops : Integer ) : Integer;

    property Index           : Integer read fIndex;
    property Number          : Integer read GetNumber;
    property Number1         : Integer read GetNumber1;
    property Name            : String  read fName;
    property Direction       : String read fDirection;
    property Consist         : String read fConsist;
    property Status          : TTrainStatus read fStatus write SetStatus;
    property Flags           : TTrainFlag   read fFlags write SetFlags;
    property StopsAt         : Integer      read fStopsAt;
    property PrintStopsAt    : Integer      read fPrintStopsAt write SetPrintStopsAt;
    property Counterpart     : Integer      read fCounterpart;
    property Schedule        : TSchedule    read fSchedule;
    function NextStationPos( Pos : Integer ) : integer;
    property IsExtra         : Boolean      read GetIsExtra;
    property HasOrders       : Boolean      read GetHasOrders;
    property Orders          : TOrdersList  read fOrders;
    property Priority        : Integer      read fPriority;
  end;

  { TTrainList }

  // A list of train objects which are owned by the list and sorted
  // by train departure time (corrected for the start time of the session)
  // This list is copied to the CrewCallList on initialization.  The CrewCallList
  // does NOT own the trains.  Individual trains are copied one-by-one to the
  // TrainRegister when they are registered by the dispatcher.
  TTrainList = class(TObjectList)
  private
    function GetTrain(ListIndex : Integer): TTrainData;
    function GetTrainByNum( theNumber : Integer ): TTraindata;
    procedure SetStartDate(AValue: TDateTime);
    procedure SetStartTime(AValue: TDateTime);
  protected
    fStartTime : TDateTime;
    fStartDate : TDateTime;
  public
    constructor Create;
    function Add(aTrain : TTrainData) : Integer;
    procedure Sort;
    property StartTime : TDateTime read fStartTime write SetStartTime;
    property StartDate : TDateTime read fStartDate write SetStartDate;

    property Train[ListIndex : Integer] : TTrainData read GetTrain; default;
    property TrainByNum[ theNumber : Integer ] : TTraindata read GetTrainByNum;
  end;

implementation

uses
  CommonDebug, CommonMath,
  StringSubs,
  MainFormUnit,
  IronMikeDataUnit, CommonLog,
  ConstantsUnit,
  RailroadAnnouncements,
  RRSubsUnit;

{ TTrainData }

procedure TTrainData.BuildTimetable;
begin
  Stub1('TTrainData.BuildTimetable');
end;

constructor TTrainData.Create(aIndex, aNumber: Integer; aName, aDirection,
  aConsist: String; aStopsAt, aPrintStopsAt, aCounterpart : Integer;
  aSuperiority : String );
begin
  Create;
  fIndex            := aIndex;
  fNumber           := aNumber;
  fPriority         := IronMikeData.Priority(aDirection,aSuperiority);
  fName             := aName;
  fDirection        := aDirection;
  fConsist          := aConsist;
  fStopsAt          := aStopsAt;
  fPrintStopsAt     := aPrintStopsAt;
  fCounterpart      := aCounterpart;
end;

constructor TTrainData.Create(aNumber: Integer; aConsist: String; Starts,
  Ends: Integer; theFlags: TTrainFlag; aDirection, aSuperiority : String );
begin
  Create;
  fNumber := aNumber;
  fConsist := aConsist;
  fPriority := IronMikeData.Priority(aDirection,aSuperiority);
  //fPriority := aPriority;{ TODO 1 -odonz -cRR Operations : See if this is really needed here.  This train is running EXTRA }
  fDirection := 'X';
  fCounterpart := 0;
  fFlags := theFlags;
  fSchedule.Add( Starts,0, Starts,-2.0,-2.0 );
  fSchedule.Add( Ends,0, Ends,-1.0,-1.0 );
  fSchedule.Sort;
end;

// The source train (aTrain) stays as the first section but shows GREEN flags.
// The new train becomes the second section with no flags and is renumbered so
// its number ends in 02 (e.g. 98 becomes 9802, 920 becomes 9202
constructor TTrainData.Create(aTrain: TTrainData);
begin
  Create;
  fNumber := aTrain.fNumber;  // Uses fFlags to set train number as appropriate
  aTrain.fFlags := tfGreen1;
  fFlags := tfGreen2;
  if Empty(aTrain.Name) then
    fName := 'Second ' + IntToStr(aTrain.Number)
  else
    fName := aTrain.Name + '-2';
  if Empty(aTrain.Name) then
    aTrain.fName := 'First ' + IntToStr(aTrain.Number)
  else
    aTrain.fName := aTrain.Name + '-1';
  fDirection := aTrain.Direction;
  fPriority  := aTrain.Priority;
  fConsist   := aTrain.Consist;
  fStopsAt   := aTrain.StopsAt;
  fPrintStopsAt := aTrain.PrintStopsAt;
  fCounterPart := aTrain.Counterpart;
  aTrain.fCounterpart := 0;
  fSchedule.Assign( aTrain.Schedule );
end;

constructor TTrainData.Create;
begin
  fSchedule := TSchedule.Create;
  fOrders   := TOrdersList.Create(false);
end;

destructor TTrainData.Destroy;
begin
  fSchedule.Free;
  //if Assigned(fOrders) then fOrders.Free;
  inherited Destroy;
end;

function TTrainData.GetHasOrders: Boolean;
var
  I : Integer; // for diagnostics
begin
  I := Orders.Count;
  Result := Orders.Count > 0;
end;

function TTrainData.GetIsExtra: Boolean;
begin
  // This is a hack.  Need to add Extra to Train Form to set  the flags
  Result := (Direction = 'X') or (Flags in [tfWhite,tfMultiWhite]);
end;

function TTrainData.GetNumber: Integer;
begin
  if fFlags = tfGREEN2 then
    if fNumber < 100 then
      Result := fNumber*100+2
    else
      Result := fNumber*10+2
  else
    Result := fNumber;
end;

function TTrainData.GetNumber1: Integer;
begin
  Result := fNumber;
end;

function TTrainData.IsSuperiorDirection: Boolean;
begin
  Result := IronMikeData.IsSuperiorDirection( Direction );
end;

procedure TTrainData.DebugLog(Where: String; Disable: Boolean);
begin
  if Disable then exit;
  CommonLog.Log.FormatLn( '%s Train: %4d %15s Status: %d, %d Orders',
                [ Where, Number, Name, ord(Status), fOrders.Count ] );
end;

function TTrainData.NextStationPos(Pos: Integer): integer;
var
  I : Integer;
begin
  if (Pos < 1) or (Pos > Schedule.Count) then
    Result := 0
  else
    for I := 1 to Schedule.Count do
      if Schedule[I].Pos = Pos then
        begin
          if I >= Schedule.Count then
            Result := 0
          else
            Result := Schedule[I+1].Pos;
          exit;
        end;
end;

procedure TTrainData.PrintClearance( vDisplay : TDisplay; vDisplayList : TDisplayList );
var
  Station       : String;
  Today         : String;
  NumberStr     : String;
  OrderCount    : String; // 'No' or '1', ...
  OrderNumbers  : array [1..9] of String;
  OKAt          : String;
  Dispatcher    : String;
  DoNotLeave    : String;
  MissingTrains : String;
  MissingTrainList : TStringList;

  StnIdx        : Integer;
  StnPos        : Integer;
  I             : Integer;
  Now           : TDateTime;
  DepartTime    : TDateTime;
  DepartTrack   : String;
  ArrivesAt     : String;
  ArriveTrack   : String;
  AdviseTrack   : String;

  theTrain      : TTrainData;
  OrderString   : String;
  P             : Integer;
  TheOrder      : TOrder;
  C             : Integer;
  OrdersTop     : Integer;
begin
  theTrain := self; // Fix this when finished.
  StnPos := theTrain.Schedule[1].Pos;
  DepartTrack := theTrain.Schedule[1].Track;
  Station     := IronMikeData.StationList[StnPos].Name;
  I := theTrain.Schedule.Count;
  StnPos := theTrain.Schedule[I].Pos;
  ArrivesAt := IronMikeData.StationList[StnPos].Name;
  ArriveTrack := theTrain.Schedule[I].Track;

  Now := MainForm.MagicClock1.DateTime;
  Today := FormatDateTime('mmm d, yyyy', Now);
  OkAt := IronMikeData.RRTime( Now );
  NumberStr := IntToStr(theTrain.Number);
  if theTrain.Orders.Count = 0 then
    OrderCount := 'No'
  else
    OrderCount := IntToStr(theTrain.Orders.Count);
  for I := 1 to 9 do
    OrderNumbers[I] := 'X';
  for I := 0 to min(pred(theTrain.Orders.Count),8) do
    OrderNumbers[I+1] := IntToStr(TOrder(theTrain.Orders.Items[I]).OrderNumber);

  Dispatcher := IronMikeData.Railroad.Dispatcher;
  DepartTime := theTrain.Schedule[1].Departs;
  if DepartTime > Now then
    DoNotLeave := IronMikeData.RRTime( DepartTime );
  MissingTrainList := TStringList.Create;
  try
    GetMissingTrains( theTrain, MissingTrainList );
    if MissingTrainList.Count = 0 then
      MissingTrains := 'None'
    else
      begin
        MissingTrains := ' ';
        for I := 0 to pred(MissingTrainList.Count) do
          MissingTrains := MissingTrains + MissingTrainList[I] + ' ';
      end;
  finally
    MissingTrainList.Free;
  end;

  VDisplay.TopMargin := 20;
  VDisplay.LeftMargin := 25;
  VDisplay.Clear;
  VDisplay.MaxLineLen:=VDisplay.TextWidth('Conductors and each engineer must have access to');
  VDisplay.Center('C L E A R A N C E');
  vDisplay.NewLine(2);
  vDisplay.Underscore(Station,140);
  vDisplay.Put('Station');
  vDisplay.Underscore(Today);
  vDisplay.NewLIne(2);
  VDisplay.Put( 'To C&E' );
  vDisplay.Underscore(NumberStr);
  vDisplay.NewLine(2);
  vDisplay.Put( 'I have ');
  vDisplay.Underscore(OrderCount,50);
  vDisplay.PutLine('orders for your train, as Follows:');
  vDisplay.NewLine;
  vDisplay.Put('No.',80);
  vDisplay.Underscore(OrderNumbers[1],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[2],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[3],50);
  vDisplay.NewLine(2);
  vDisplay.Put('Orders');
  vDisplay.Put('No.',80);
  vDisplay.Underscore(OrderNumbers[4],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[5],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[6],50);
  vDisplay.NewLine(2);
  vDisplay.Put('No.',80);
  vDisplay.Underscore(OrderNumbers[7],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[8],50);
  vDisplay.Put('No.');
  vDisplay.Underscore(OrderNumbers[9],50);
  vDisplay.NewLine(2);
  vDisplay.Put('OK at');
  vDisplay.Underscore(OKAt,65);
  vDisplay.Put(', ');
  vDisplay.Underscore(Dispatcher,120);
  vDisplay.RightJustify('Dispatcher');
  vDIsplay.NewLine(2);
  vDisplay.Put('Do not leave before');
  vDisplay.Underscore(DoNotLeave,70);
  vDisplay.NewLine(2);
  vDisplay.Put('Trains due at ');
  vDisplay.Underscore(Station,140);
  vDisplay.PutLine('have arrived');
  vDisplay.Put('except');
  vDisplay.Underscore(MissingTrains);
  vDisplay.NewLine(2);
  VDisplay.PutLine('Conductors and each engineer must have access to');
  vDisplay.PutLine('this copy and see that their train is correctly');
  vDisplay.PutLine('designated above, and also that the number of');
  vDisplay.PutLine('orders and the numbers of all orders received');
  vDisplay.PutLine('correspond with the numbers inserted above.');
  vDisplay.Outline(5,5);
  OrdersTop := vDisplay.XMax;
  vDisplay.Reset;
  vDisplay.LeftMargin := 150;
  vDisplay.NewLine(5);
  vDisplay.Center('-- Departs --');
  vDisplay.NewLine(2);
  vDisplay.Center(Station + ' - ' + DepartTrack);
  vDisplay.NewLine(2);

  if not theTrain.IsExtra then
    for I := 2 to pred(pred(IronMikeData.StationsCount)) do
      begin
        AdviseTrack := theTrain.Schedule[I].Track;
        if not Empty(AdviseTrack) then
          begin
            StnPos := theTrain.Schedule[I].Pos;
            Station := IronMikeData.StationList[StnPos].Name;
            vDisplay.Center('-- ' + Station + ' Routing --');
            vDisplay.NewLine(2);
            vDisplay.Center(AdviseTrack);
            vDisplay.NewLine;
            vDisplay.Center('Advisory');
            vDisplay.NewLine(2);
          end;
      end;

  vDisplay.Center('-- Arrives --');
  vDisplay.NewLine(2);
  vDisplay.Center(ArrivesAt + ' - ' + ArriveTrack);

  VDisplay.TopMargin := OrdersTop;
  vDisplay.LeftMargin := 10;
  VDisplay.NewLine;
  C := theTrain.Orders.Count;
  for I := 0 to min(pred(theTrain.Orders.Count),8) do
    begin
      TheOrder := TOrder(theTrain.Orders.Items[I]);
      OrderString := TheOrder.OrderStrings;
      P := Pos(#10,OrderString);
      if P = 0 then
        vDIsplay.TypeText(OrderString)
      else
        begin
          vDisplay.TypeText(Copy(OrderString,1,P));
          vDisplay.TypeText(Copy(OrderString,P+1,255));
        end;
    end;
  theTrain.Orders.Clear;   // train orders must not destroy object!!!!!

  if Assigned(vDisplayList) then
    vDisplayList.Append( vDisplay.Image.Picture );
end;

procedure TTrainData.PrintInfo(vDisplay: TDisplay; vDisplayList: TDisplayList);
var
  I : Integer;
begin
  VDisplay.TopMargin := 20;
  VDisplay.LeftMargin := 25;
  VDisplay.Clear;
  VDisplay.MaxLineLen:=VDisplay.TextWidth('San luis obsispo                12;34 am ');
  VDisplay.Center('Train ' + IntToStr(Number));
  vDisplay.NewLine(2);
  for I := 1 to Schedule.Count do
    begin
      vDisplay.Put( Schedule[I].Station);
      vDisplay.RightJustify( 'Arr. ' + IronMikeData.RRTime(Schedule[I].Arrives ),0);
      vDisplay.NewLine;
      vDisplay.RightJustify( 'Dep. ' + IronMikeData.RRTime(Schedule[I].Departs ),0);
      vDisplay.NewLine(2);
    end;
  vDisplay.Outline(15,15);

  if Assigned(vDisplayList) then
    vDisplayList.Append( vDisplay.Image.Picture );
end;

procedure TTrainData.RunLateFrom(StnPos: Integer; HowLate: TDateTime);
var
  I, Idx : Integer;
begin
  for I := 1 to Schedule.Count do
    begin
      if Schedule[I].Pos = StnPos then
        begin
          Idx := I;
          break;
        end;
    end;
  Schedule.Advance( HowLate, Idx );
end;

procedure TTrainData.SetFlags(AValue: TTrainFlag);
begin
  if fFlags=AValue then Exit;
  fFlags:=AValue;
end;

procedure TTrainData.SetPrintStopsAt(AValue: Integer);
begin
  //note - most of the logic for this has been handled when the order was set in setStops
  //which returns the actual stops needed. The only thing this routine needs to watch for is
  //if one station is set and the other is chosen, which should force a BOTH result.

  if AValue = 0 then
    fPrintStopsAt := 0 //to reset after printing
  else
    if fPrintStopsAt = 0 then
      fPrintStopsAt := AValue
    else
      fPrintStopsAt := 3; // KC and SAL
end;

procedure TTrainData.SetStatus(AValue: TTrainStatus);
begin
  if fStatus=AValue then Exit;
  fStatus:=AValue;
end;

function TTrainData.SetStopsAt(NewStops: Integer): Integer;
begin
  //NewStops is the new value of what will be appended to the existing fStopsAt
  //this function returns what was ADDED (for use in wait forms).
  //values already set for stops should not allow for time increase

  // This code exactly duplicates the (much more complicated) code in c++.
  Result := NewStops and (not fStopsAt);  // Only added stops
  fStopsAt := fStopsAt or NewStops;       // Can't Eliminate Stops once added !!!!!
end;

procedure TTrainData.StartRun;
var
  Now : TDateTime;
  DeltaT : TDateTime;
  I      : Integer;
begin
  Status := tsRunning;
  Now := MainForm.MagicClock1.DateTime;
  DeltaT := Now - Schedule[1].Departs;
  if DeltaT > 0 then
    begin
      Schedule.Advance( DeltaT );
      IronMikeData.TrainList.Sort;
    end;
end;

function TTrainData.StationName(When: TDateTime): String;
begin
  Result := IronMikeData.StationList[ StationPos(When) ].Name;
end;

function TTrainData.StationPos(When: TDateTime): Integer;
var
  Pos : Integer;
  I : Integer;
  Now : TDateTime;
begin
  if When < 0 then
    When := MainForm.MagicClock1.DateTime;
  case Status of
    tsReady :
      begin
        Result := Schedule[1].Pos;
      end;
    tsRunning :
      begin
        I := Schedule.Count; // Setup so train is at destination if it is past destination timein
        if When > Schedule[I].Departs then
          Result := Schedule[I].Pos
        else
          for I := Schedule.Count downto 1 do
            if When < Schedule[I].Departs then
              Result := Schedule[I].Pos;
        if Result > Schedule.Count then
          Result := 0;
      end;
    tsComplete : Result := 0;
  end;
end;

function TTrainData.TrainFlag: String;
begin
  case Flags of
    tfNone, tfGreen2      : Result := '<None>';
    tfWhite, tfMultiWhite : Result := 'White';
    tfGreen1              : Result := 'Green';
  end;
end;

procedure TTrainData.TrainIO(out EastIO, WestIO: String);
var
  EastIdx, WestIdx : Integer;
  EastTime, WestTime : TDateTime;
begin
  EastIO := '';
  WestIO := '';
  WestIdx := Schedule[1].Pos;
  EastIdx := Schedule[Schedule.Count].Pos;
  if EastIdx > WestIdx then
    begin
      WestIo := IronMikeData.RRTime(Schedule[1].Departs);
      if Status = tsComplete then
        EastIO := IronMikeData.RRTime(Schedule.Arrives);
    end
  else
    begin
      EastIo := IronMikeData.RRTime(Schedule[1].Departs);
      if Status = tsComplete then
        WestIO := IronMikeData.RRTime(Schedule.Arrives);
    end;
end;

{ TTrainList }

function TTrainList.Add(aTrain: TTrainData): Integer;
begin
  Result := inherited Add( ATrain );
end;

constructor TTrainList.Create;
begin
  inherited Create(True); // The TrainList does free its stored objects
end;

function TTrainList.GetTrain(ListIndex : Integer): TTrainData;
begin
  Result := TTrainData(Items[ListIndex]);
end;

function TTrainList.GetTrainByNum( theNumber : Integer ): TTraindata;
var
  I : Integer;
begin
  Result := nil; // Just in case
  for I := 0 to pred(Count) do
    if Train[I].Number = theNumber then
      begin
        Result := Train[I];
        break;
      end;
end;

procedure TTrainList.SetStartDate(AValue: TDateTime);
begin
  if fStartDate=AValue then Exit;
  fStartDate:=AValue;
end;

procedure TTrainList.SetStartTime(AValue: TDateTime);
begin
  if fStartTime=AValue then Exit;
  fStartTime:=AValue;
end;

// TrainTimeCompare is my mis-interpretation of the train list order.
// It may be removed when I'm sure that it is truly not needed.
function TrainTimeCompare( L, R : Pointer ) : Integer;
begin
  if (L = nil) or (R = nil) then
    begin
      Result := -2;
      exit;
    end;
  if TTrainData(L).Schedule.Assigned and (not TTrainData(R).Schedule.Assigned) then
    Result := -1
  else if (not TTrainData(L).Schedule.Assigned) and TTrainData(R).Schedule.Assigned then
      Result := 1
  else if not (TTrainData(L).Schedule.Assigned and TTrainData(R).Schedule.Assigned) then
    Result := 0
  else if TTrainData(L).Schedule.Departs < TTrainData(R).Schedule.Departs then
    Result := -1
  else if TTrainData(L).Schedule.Departs > TTrainData(R).Schedule.Departs then
    Result := 1
  else
    Result := 0;
end;

// TrainPriorityCompare is the equivalent of c++'s SortPredicate
function TrainPriorityCompare( L, R : Pointer ) : Integer;
var
  Lhs, Rhs : TTrainData;
begin
  Lhs := TTrainData(L);
  Rhs := TTrainData(R);
  Result := ord(Lhs.Status) - ord(Rhs.Status); // Ready, Running, Complete
  if Result = 0 then   // Both statuses (stati?) identical;
    Result := Lhs.Priority - Rhs.Priority;  // This prioritizes the list without
                                            // regards to train status. Lowest
                                            // priority values go to the head of the list
end;

procedure TTrainList.Sort;
begin
  inherited Sort( @TrainPriorityCompare );
end;

end.

