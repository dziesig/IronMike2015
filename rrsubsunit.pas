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

unit RRSubsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IronMikeDataUnit, TrainsUnit, DisplayUnit, OrdersUnit;

procedure RunRailroad( NewTrain : TTrainData; vDisplay : TDisplay; vDisplayList : TDisplayList );
procedure Post2Extras( theOrder : TOrder; aTrain1, aTrain2 : TTrainData );
procedure GetMissingTrains( theTrain : TTrainData; Missing : TStringList );

implementation

uses
  CommonDebug, CommonLog, CommonMath,
  Windows, // For beep during development
  RailroadAnnouncements,
  ConstantsUnit,
  DispatcherFormUnit, // Gives us access to the Orders Display and Orders Display List
  MainFormUnit;       // Gives us access to the Clock.

//this function is used to check the trains and their timetables to
//determine if any orders are issued. If orders have been issued, they
//are immediatly printed (for trains who are RUNNING) or added to clearance
//(for those who have yet to register).

function FindOrderedMeet( aTrain1, aTrain2 : TTrainData; theStnPos : Integer ) : TOrder;
var
  I : Integer;
  anOrder : TOrder;
  C : Integer;
begin
  Result := nil;
  C := TimetableList.Count;
  for I := 0 to pred(TimetableList.Count) do
    begin
      anOrder := TOrder(TimetableList[I]);
      if anOrder.TrainsMeetAt(aTrain1,ATrain2,theStnPos) then
        begin
          Result := AnOrder;
          exit;
        end;
    end;
  C := OrdersList.Count;
  for I := 0 to pred(OrdersList.Count) do
    begin
      anOrder := TOrder(OrdersList[I]);
      if anOrder.TrainsMeetAt(aTrain1,ATrain2,theStnPos) then
        begin
          Result := AnOrder;
          exit;
        end;
    end;

end;

function FindLastMeet( aTrain1, aTrain2 : TTrainData; theStnPos : Integer ) : Integer;
var
  I : Integer;
  LMP : Integer;
begin
  //this function searches backwards through order list to see if trains met somewhere
  //prior to this location. If so, it returns that station  ### Comment from c++
  //says it searches backwards.  c++ code searches forward.in the Windows version,
  //backward in the command line version.

  // Searching BACKWARD here
  LMP := NotSet;
  for I := pred(OrdersList.Count) downto 0 do
    begin
      LMP := OrdersList[I].WhereDoTrainsMeet(aTrain1,aTrain2);
      if LMP <> NotSet then
        break;
    end;
  Result := LMP;
end;

procedure Post2Extras( theOrder : TOrder; aTrain1, aTrain2 : TTrainData );
var
  I : Integer;
  T : TTrainData;
begin
  with IronMikeData do
    begin
      for I := 0 to pred(TrainList.Count) do
        begin
          T := TrainList.Train[I];
          if T.IsExtra and (T.Status = tsRunning) and
             (T <> aTrain1) and (T <> aTrain2) then
            T.Orders.Add( theOrder );
        end;
    end;
end;

procedure GetMissingTrains(theTrain: TTrainData; Missing: TStringList);
var
  I : Integer;
  aTrain : TTrainData;
  Direction : String;
  TimeNow : TDateTime;
begin
   Missing.Clear;
   with IronMikeData do
     begin
       Direction := theTrain.Direction;
       TimeNow   := MainForm.MagicClock1.DateTime;
       for I := 0 to pred(TrainList.Count) do
         begin
           aTrain := TrainList[I];
           if (aTrain <> theTrain) and
              (aTrain.Direction <> Direction) and
              (aTrain.Status <> tsComplete) then
             begin
               if not aTrain.IsExtra then
                 if aTrain.Schedule.Arrives < TimeNow then
                   Missing.Add( IntToStr( aTrain.Number ) );
             end;
         end;
     end;
end;

//newTrain is the pointer to the new train who caused this to be run. Since his clearance
//has yet to be printed, his orders should NOT be printed at this point

procedure RunRailroad(NewTrain: TTrainData; vDisplay : TDisplay; vDisplayList : TDisplayList);
var
  currTrain  : TTrainData;
  otherTrain : TTrainData;
  overTaker  : TTrainData;
  overTaken  : TTrainData;

  Station    : Integer; // CurrentStation of CurrentTrain;

  I, J       : Integer;

  MyDepart, MyArrive, HisDepart, HisArrive : TDateTime;

  MyStation : Integer;
  NextStation : Integer; // Not to be confused with Train.NextStation;
  MeetStation : Integer;
  FixedMeet   : Boolean;
  LastMeetStation : Integer;
  Done : Boolean;
  T0, T1, T2, T3 : TDateTime;

  theOrder : TOrder;
  MeetTime : TDateTime;
  DepartTime : TDateTime;

  procedure HandleTrainsPass;
  var
    Done : Boolean;
    CurrArrive, CurrDepart, OtherArrive, OtherDepart : TDateTime;
  begin
    //Log.FormatLn('HandleTrainsPass %d  %d',[currTrain.Number,otherTrain.Number]);

    // there is a BUG here westbound train times are reversed as a f(station).
    MeetStation := NotSet;
    MyStation := Station; // start at current location
    Done := False;
    repeat

      CurrArrive  := currTrain.Schedule.ByPos[MyStation].Arrives;
      CurrDepart  := currTrain.Schedule.ByPos[MyStation].Departs;
      OtherArrive := otherTrain.Schedule.ByPos[MyStation].Arrives;
      OtherDepart := otherTrain.Schedule.ByPos[MyStation].Departs;
      if (CurrArrive < OtherArrive) and ((CurrDepart + OrderAfter ) > OtherDepart) then
        begin
          MeetStation := MyStation;
          Done := True;
          Overtaker := OtherTrain;
          Overtaken := CurrTrain;
        end;
      if (CurrArrive > OtherArrive) and (CurrDepart  < (OtherDepart + OrderAfter )) then
        begin
          MeetStation := MyStation;
          Done := True;
          Overtaker := currTrain;
          Overtaken := OtherTrain;
        end;
      MyStation := currTrain.NextStationPos( MyStation );
      if MyStation = NotSet then Done := True;
    until Done;
    if MeetStation <> NotSet then
      if Overtaker = currTrain then
        Log.FormatLn( 'Train %d overtakes %d at %s',
                      [ currTrain.Number,otherTrain.Number,
                        IronMikeData.StationList[meetStation].Name ])
      else
         Log.FormatLn( 'Train %d overtakes %d at %s',
                      [ otherTrain.Number,currTrain.Number,
                        IronMikeData.StationList[meetStation].Name ]);

  end;

  procedure HandleTrainsMeet;
  var
    Done : Boolean;
    CurrArrive, CurrDepart, OtherArrive, OtherDepart : TDateTime;
    CurrArriveNext, OtherDepartNext : TDateTime;
    NextStnPos : Integer;
  begin
    //Log.FormatLn( 'Start of HandleTrainsMeet %d %d',[currTrain.number,othertrain.number] );
    //if (newtrain.number = 98) and (currTrain.number = 71) and (otherTrain.Number = 98) then
    //  Log.FormatLn('HandleTrainsMeet %d  %d',[currTrain.Number,otherTrain.Number]);
    MeetStation := NotSet;
    MyStation := Station; // start at current location
    //Log.FormatLn('Start at current location:  %d',[MyStation]);
    Done := False;
    repeat

      NextStation := currTrain.NextStationPos( MyStation );
      CurrArrive  := currTrain.Schedule.ByPos[MyStation].Arrives;
      CurrDepart  := currTrain.Schedule.ByPos[MyStation].Departs;
      OtherArrive := otherTrain.Schedule.ByPos[MyStation].Arrives;
      OtherDepart := otherTrain.Schedule.ByPos[MyStation].Departs;
      NextStnPos := currTrain.NextStationPos(MyStation);
      //Log.FormatLn('Checking for meet at %s',[IronMikeData.StationName[MyStation]]);
      //Log.FormatLn('%s %s - %s %s',[ IronMikeData.RRTime(currArrive),
      //                               IronMikeData.RRtime(currDepart),
      //                               IronMikeData.RRTime(otherArrive),
      //                               IronMikeData.RRTime(otherDepart)] );
      //Log.FormatLn('%f %f - %f %f',[ currArrive,
      //                               currDepart,
      //                               otherArrive,
      //                               otherDepart] );
      //Log.FormatLn('NextStationPos(%d):  %d',[MyStation,currTrain.NextStationPos(MyStation)]);
      if NextStnPos > 0 then
        begin
          CurrArriveNext := currTrain.Schedule.ByPos[currTrain.NextStationPos(MyStation)].Arrives;
          OtherDepartNext := otherTrain.Schedule.ByPos[currTrain.NextStationPos(MyStation)].Departs;
        end
      else
        begin
          CurrArriveNext := 0.0;
          OtherDepartNext := 0.0;
        end;
      if (Station = MyStation) and
         (MyStation = OtherTrain.StationPos(currTrain.Schedule.ByPos[MyStation].Arrives )) then
        begin
//trains are both due here with no chance of recall - meet here
//NOTE - this only works if its the first loop through (station = mystation)
	  meetStation := myStation;
          Done := True;
          //Log.PutLn('AAAAAAAAAAAAAAAA');
        end
      else if CBLI( currArrive, otherArrive, currDepart ) or
              CBLI( currArrive, otherDepart, currDepart ) or
              CBLI( otherArrive, currArrive, otherDepart ) or
              CBLI( otherArrive, currDepart, otherDepart ) then
        begin
//did trains share time in station?
          Done := True;
	  meetStation := myStation;
          //Log.PutLn('BBBBBBBBBBB');
        end
      else if (currDepart < otherArrive) and
              (currArriveNext > otherDepartNext) then
        begin
  	//are we on the next single section of track? If so, move meet back.
	  meetStation := nextStation;
          Done := True;
          //Log.PutLn('CCCCCCCCCCCCCCCCCC');
        end;
      //else
      //  Log.PutLn('DDDDDDDDDDDDDDD');

      //Log.FormatLn('Old value of myStation:  %d',[mystation]);
      MyStation := currTrain.NextStationPos( MyStation );
      //Log.FormatLn('Next value of myStation:  %d',[mystation]);
      if MyStation = NotSet then Done := True;
    until Done;

    //Log.FormatLn( 'Trains %d and %d meet at %s',
    //              [ currTrain.Number,otherTrain.Number,
    //                IronMikeData.StationName[meetStation]]);
  end;

begin
{$define DEVELOP}
  // Assumptions:
  //
  // TrainList is ordered by priority
  // Each Train's Schedule is ordered by arrival time at the station
  // (Eastbound trains are reversed with respect to Westbound trains)


  with IronMikeData do
    begin
      // Loop over the entire train list in priority order
      T0 := Now;
      for I := 0 to pred(TrainList.Count) do
        begin
          currTrain := TrainList[I];
          //currTrain.DebugLog('RunRailroad', false);
          if currTrain.Status = tsComplete then continue; // Two less levels of indent.
          Station := currTrain.StationPos; // Where is current train?  This is loop-constant
          MyArrive := currTrain.Schedule[currTrain.Schedule.Count].Arrives;
          MyDepart := currTrain.Schedule[1].Departs;
{$ifdef DEVELOP}
          //Log.FormatLn( '%4d MyDepart, MyArrive:  %s, %s station: %d',
          //              [ currTrain.Number,IronMikedata.RRTime(MyDepart),
          //                IronMikedata.RRTime(MyArrive),station]);
{$endif DEVELOP}
          // Loop over trains that depart after the current train
          for J := I+1 to pred(TrainList.Count) do
            begin
              //Windows.Beep(1760,50);
              //T2 := Now;
              ////Continue;
              otherTrain := TrainList[J];
              if otherTrain.IsExtra then Continue;   // Ditto on indent.
              if otherTrain.Status = tsComplete then continue;
              //currTrain.DebugLog('Start of Loop',false);
              //otherTrain.DebugLog('Start of Loop',false);
              T3 := Now;
              HisArrive := otherTrain.Schedule[otherTrain.Schedule.Count].Arrives;
              HisDepart := otherTrain.Schedule[1].Departs;
              //Log.FormatLn( '  %4d HisDepart, HisArrive:  %s, %s',[otherTrain.Number,
              //  ronMikedata.RRTime(HisDepart),IronMikedata.RRTime(HisArrive)]);
              if not ( (MyDepart <= HisArrive) and
                       (MyArrive >= HisDepart) and
                       (currTrain.IsExtra = otherTrain.IsExtra) ) then continue;
              //Log.FormatLn( '  %4d HisDepart, HisArrive:  %s, %s  station: %d',[otherTrain.Number,
              //IronMikedata.RRTime(HisDepart),IronMikedata.RRTime(HisArrive),otherTrain.StationPos]);
              //Log.FormatLn('    Trains %4d and %4d Overlap',[currTrain.Number,otherTrain.Number]);
              MeetStation := NotSet;
              if currTrain.Direction = otherTrain.Direction then
                HandleTrainsPass
              else
                HandleTrainsMeet;
              //Log.FormatLn('MeetStation %d',[MeetStation]);
            //end;
          if MeetStation <> NotSet then
            begin
              //currTrain.DebugLog('Meet Station: ' + IntToStr(MeetStation),false);
              //otherTrain.DebugLog('Meet Station: ' + IntToStr(MeetStation),false);
// meet found - can the order be placed?
              fixedMeet := False;
              theOrder := FindOrderedMeet(currTrain, OtherTrain, 5);
              if Assigned( theOrder ) then
                fixedMeet := theOrder.FixedMeet;
              if not FixedMeet then
                begin
                  theOrder := FindOrderedMeet(currTrain,OtherTrain,MeetStation);
                  if not Assigned(theOrder) then   //meet does not exist
                    begin
// Generate the order
                      MeetTime := currTrain.Schedule[MeetStation].Arrives;
                      DepartTime := OtherTrain.Schedule[MeetStation].Departs;

                      if MeetTime > DepartTime then
                        begin
                          DepartTime := MeetTime - DepartTime; // How long to hold train
                          OtherTrain.RunLateFrom(MeetStation,DepartTime);
                        end;
                      // Any previous orders?
                      LastMeetStation := FindLastMeet( CurrTrain, OtherTrain, MeetStation );

                      theOrder := nil;
                      if currTrain.Direction = otherTrain.Direction then
                        // PASS
                        theOrder := TFormSB.Create( overtaker, overtaken,
                                                    MeetStation, LastMeetStation,
                                                    MeetTime )
                      else
                        // MEET
                        theOrder := TFormSA.Create( currTrain, OtherTrain,
                                                    MeetStation, LastMeetStation,
                                                    MeetTime );

                      Post2Extras( theOrder, currTrain, OtherTrain );

                      if Assigned( theOrder ) then
                        begin
                          OrdersList.Add( theOrder );
                          currTrain.Orders.Add( theOrder );
                          OtherTrain.Orders.Add( theOrder );
                          Log.FormatLn( 'Orders %d, Train %d %d, Train %d %d',
                                        [ OrdersList.Count, currTrain.Number,
                                          currTrain.Orders.Count, otherTrain.Number,
                                          otherTrain.Orders.Count ] );
                        end;
                    end;
                end;
            end;
            end;
        end; // for I := 0 to pred(TrainList.Count)

      //Log.Timing(T0,'all trains');
      //Windows.Beep(440,100);

      for I := 0 to pred(TrainList.Count) do
        begin
          currTrain := TrainList[I];
          //Log.FormatLn( 'Train: %d, Status: %d, NewTrain: %d, HasOrders: %d',
          //              [ currTrain.Number, ord(currTrain.Status),
          //                newTrain.Number, ord(currTrain.HasOrders) ] );
          if ( currTrain.Status = tsRunning ) and
             ( currTrain <> newTrain ) and
             ( currTrain.HasOrders ) then
            begin
              Log.FormatLn('New Orders for train %d',[currTrain.Number]);
              currTrain.printClearance( vDisplay, vDisplayList );
              NewOrdersFor(currTrain.Number);
            end;
        end;
    end; // with IronMikeData
end;

end.

