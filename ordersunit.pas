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

unit OrdersUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, DisplayUnit,
  Contnrs;

type
  TStopsAt = (saNone, saKC, saSal, saBoth);

  { TOrder }

  TOrder = class
  private
    // TotalOrders is a clsss variable since we will have separate lists
    // for timetable vs. generated orders.
    class var
      fTotalOrders : Integer;
    var
      fTimeOccurred : TDateTime;
      fOrderNumber  : Integer;
      fTrain1       : TObject;
      fTrain2       : TObject;
      whereMeet : Integer;
      whereLastMeet : Integer;
      mustMeet : Boolean;
    function  GetFixedMeet    : Boolean;  virtual;
    function  GetOrderStrings : String;   virtual; abstract;
    function  GetStops        : TStopsAt; virtual;
    function  GetTotalOrders  : Integer;
    procedure SetFixedMeet(AValue: Boolean); virtual;
  protected
    function  OrderNumberStr : String;
  public
    constructor Create( theTime : TDateTime = 0.0);

    class procedure ResetTotalOrders;  // Call as TOrder.ResetTotalOrders

    function TrainsMeetAt( aTrain1, aTrain2 : TObject; StnPos : Integer ) : Boolean; virtual;
    function WhereDoTrainsMeet( aTrain1, aTrain2 : TObject ) : Integer; virtual;
    // OrderStrings may return two lines separated by <lf>
    // This greatly simplifies the code in descendant printOrder from the c++ version
    property OrderStrings : String    read GetOrderStrings;
    property TotalOrders  : Integer   read GetTotalOrders;
    property FixedMeet    : Boolean   read GetFixedMeet write SetFixedMeet;
    property Time         : TDateTime read fTimeOccurred;
    property OrderNumber  : Integer   read fOrderNumber;

    // In the c++ version this had a train as an argument.  If that argument
    // did not match the train specified in the orders it returned NotSet
    // I have chosen to only work with the train from the orders.  We will see
    // if that was a good choice.
    property Stops        : TStopsAt  read GetStops;
  end;

  { TFormE }

  TFormE = class(TOrder)  //WAIT ORDER (used for reporting delayed trains to extras)
  private
    fTrain : TObject;
    fKCTime,
    fSalTime : TDateTime;

    function GetStops : TStopsAt; override;
    function GetOrderStrings : String; override;
  public
    constructor Create( aTrain : TObject; theTime : TDateTime ); overload;
    constructor Create( aTrain : TObject; theKCTime, theSalTime, theTime : TDateTime ); overload;

    property KCTime    : TDateTime read fKCTime;
    property SALTime   : TDateTime read fSALTime;
  end;

  { TFormF }

  TFormF = class(TOrder) // For sections
  protected
    EngineNum : String;
    function GetOrderStrings : String; override;
  public
    constructor Create( theTrain : TObject; theTime : TDateTime; Engine : String = '');
  end;

  { TFormG }

  TFormG = class(TOrder) // For extras
  protected
    EngineNum : String;
    function GetOrderStrings : String; override;
  public
    constructor Create( theTrain : TObject; theTime : TDateTime; Engine : String );
  end;


  { TFormH }

  TFormH = class(TOrder)   //WORK FORM
  protected
    fWorkAt : Integer;
    function GetOrderStrings : String; override;
   public
    constructor Create( theTrain : TObject; theTime : TDateTime; workAt : Integer );
    property  whereAt : Integer read fWorkAt;
  end;

  { TFormSA }

  TFormSA = class(TOrder)
  protected
    function GetOrderStrings : String; override;
    function GetFixedMeet    : Boolean; override;
    procedure SetfixedMeet( AValue : Boolean ); override;
  public
    constructor Create( aTrain1, aTrain2 : TObject; WhereAt : Integer; theTime : TDateTime ); overload;
    constructor Create( aTrain1, aTrain2 : TObject; WhereAt, WhereLast : Integer; theTime : TDateTime ); overload;
    // Create an order from the timetable == this may be updated or redone when
    // A form for timetable meets is developed.
    constructor Create( Trn1, Trn2 : Integer; WhereAt : String;
                        theTime : String; isFixed : Boolean = False);
    function TrainsMeetAt( aTrain1, aTrain2 : TObject; StnPos : Integer ) : Boolean; override;
    function WhereDoTrainsMeet( aTrain1, aTrain2 : TObject ) : Integer; override;
  end;

  { TFormSB }

  TFormSB = class(TOrder)
  protected
    function GetOrderStrings : String; override;
  public
    constructor Create( aTrain1, aTrain2 : TObject; WhereAt : Integer; theTime : TDateTime ); overload;
    constructor Create( aTrain1, aTrain2 : TObject; WhereAt, WhereLast : Integer; theTime : TDateTime ); overload;

    function TrainsMeetAt( aTrain1, aTrain2 : TObject; StnPos : Integer ) : Boolean; override;
    function WhereDoTrainsMeet( aTrain1, aTrain2 : TObject ) : Integer; override;
  end;

  { TOrdersList }

  TOrdersList = class(TObjectList)
  private
    function GetOrder( Idx : Integer ): TOrder;
  public
    procedure Add( theOrder : TOrder );

    property Order[ Idx : Integer ] : TOrder read GetOrder; default;
  end;

var
  OrdersList, TimetableList : TOrdersList;

implementation

uses
  CommonDebug, CommonLog, CommonMath,
  MainFormUnit,
  StringSubs,
  ScheduleUnit,
  ConstantsUnit,
  TrainsUnit,
  IronMikeDataUnit;

{ TFormH }

constructor TFormH.Create(theTrain: TObject; theTime: TDateTime; workAt: Integer
  );
begin
  inherited Create( theTime );
  fTrain1 := theTrain;
  fWorkAt := workAt;
end;

function TFormH.GetOrderStrings: String;
var
  aTrain : TTRainData;
begin
  aTrain := TTrainData( fTrain1 );
  Result := Format( '%s %d WORK EXTRA AT %s',
                    [ OrderNumberStr, aTrain.Number,IronMikeData.StationList[fWorkAt].Name] );
end;

{ TOrdersList }

procedure TOrdersList.Add(theOrder: TOrder);
begin
  inherited Add( theOrder );
end;

function TOrdersList.GetOrder( Idx : Integer ): TOrder;
begin
  Result := TOrder( Items[Idx] );
end;

{ TFormG }

constructor TFormG.Create(theTrain: TObject; theTime : TDateTime ; Engine : String);
var
  aTrain : TTrainData;
begin
  inherited Create(theTime);
  aTrain := TTrainData(theTrain);
  if Empty(Engine) then
    EngineNum := IntToStr( aTrain.Number )
  else
    EngineNum := Engine;
  fTrain1   := theTrain;
end;

function TFormG.GetOrderStrings: String;
var
  aTrain : TTrainData;
begin
  aTrain := TTrainData(fTrain1);

  Result := Format( '%s ENGINE %s RUN EXTRA FROM %s TO %s',
                    [ OrderNumberStr, EngineNum, aTrain.Schedule[1].Station,
                      aTrain.Schedule[aTrain.Schedule.Count].Station ] );
end;

{ TFormF }

constructor TFormF.Create(theTrain: TObject; theTime : TDateTime; Engine: String);
begin
  inherited Create(theTime);
  EngineNum := Engine;
  fTrain1   := theTrain;
end;

function TFormF.GetOrderStrings: String;
var
  aTrain : TTrainData;
begin
  aTrain := TTrainData(fTrain1);
  case aTrain.Flags of
    tfGreen1:
      begin
        Result := Format( '%s %d DISPLAY SIGNALS AND RUN AS FIRST %d',
                          [OrderNumberStr, aTrain.Number, aTrain.Number] );
      end;
    tfGreen2:
      begin
        Result := Format( '%s ENG %s DISPLAY SIGNALS AND RUN AS SECOND %d - TRAIN %d',
                          [OrderNumberStr, EngineNum, aTrain.Number1, aTrain.Number] );

      end;
  end;
end;


{ TFormSB }

constructor TFormSB.Create(aTrain1, aTrain2: TObject; WhereAt,
  WhereLast: Integer; theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain1 := aTrain1;
  fTrain2 := aTrain2;
  whereMeet := WhereAt;
  WhereLastMeet := WhereLast;
  Log.FormatLn( 'CreateFormSB for %d and %d at %d instead of %d',
                [ TTrainData(aTrain1).number, TTrainData(aTrain2).Number,
                  WhereAt, WhereLastMeet] );
end;

constructor TFormSB.Create(aTrain1, aTrain2: TObject; WhereAt: Integer;
  theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain1 := aTrain1;
  fTrain2 := aTrain2;
  whereMeet := WhereAt;
  WhereLastMeet := NotSet;
  Log.FormatLn( 'CreateFormSB for %d and %d at %d instead of %d',
                [ TTrainData(aTrain1).number, TTrainData(aTrain2).Number,
                  WhereAt, WhereLastMeet] );
end;

function TFormSB.GetOrderStrings: String;
begin
  Result := Format( '%s %d PASS %d AT %s',
                    [ OrderNumberStr, TTrainData(fTrain1).Number,
                      TTrainData(fTrain2).Number,
                      IronMikeData.StationList[whereMeet].Name ] );
  if whereLastMeet <> NotSet then
    Result := Result + Format( ' INSTEAD OF %s',
                               [ IronMikeData.StationList[whereMeet].Name ] );
end;

function TFormSB.TrainsMeetAt(aTrain1, aTrain2: TObject; StnPos: Integer
  ): Boolean;
begin
  //Result:=inherited TrainsMeetAt(aTrain1, aTrain2, StnPos);
  Result:=inherited TrainsMeetAt(aTrain1, aTrain2, StnPos);
  if ( (aTrain1 = fTrain1) and (aTrain2 = fTrain2) ) or
     ( (aTrain1 = fTrain2) and (aTrain2 = fTrain1) ) then
    if StnPos = whereMeet then
      Result := True;
end;

function TFormSB.WhereDoTrainsMeet(aTrain1, aTrain2: TObject): Integer;
begin
  // This code is directly translated from the c++ implementation.
  // It works there, but I have yet to figure out why we need to test
  // the trains for equality rather than just looking at the trains
  // from which the class was created.
  // Figured it out, this is used to search the train list so we see
  // all possible candidates.
  if ( (aTrain1 = fTrain1) and (aTrain2 = fTrain2) ) or
     ( (aTrain1 = fTrain2) and (aTrain2 = fTrain1) ) then
    Result := whereMeet
  else
    Result := NotSet;
end;

{ TFormSA }

constructor TFormSA.Create(Trn1, Trn2: Integer; WhereAt: String;
  theTime: String; isFixed: Boolean);
var
  aTrain1, aTrain2 : TTrainData;
  StnPos : Integer;
  T1, T2 : TDateTime;
  T      : TDateTime;
  S      : String; // For testing
begin
  StnPos := IronMikeData.StationList.PosByName[ WhereAt ];

  aTrain1 := IronMikeData.TrainList.TrainByNum[Trn1];
  aTrain2 := IronMikeData.TrainList.TrainByNum[Trn2];

  if (aTrain1 = nil) or (aTrain2 = nil) then exit; // In case we have not initialized timetable.
  T1 := aTrain1.Schedule.ByPos[StnPos].Arrives;
  T2 := aTrain2.Schedule.ByPos[StnPos].Arrives;
  T := Max(T1,T2);
  S := IronMikeData.RRTime(T);
  inherited Create( T );
  fTrain1 := aTrain1;
  fTrain2 := aTrain2;
  whereMeet := StnPos;
  mustMeet := isFixed;
end;

constructor TFormSA.Create(aTrain1, aTrain2: TObject; WhereAt,
  WhereLast: Integer; theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain1 := aTrain1;
  fTrain2 := aTrain2;
  whereMeet := WhereAt;
  WhereLastMeet := WhereLast;
  mustMeet := False;
end;

constructor TFormSA.Create(aTrain1, aTrain2: TObject; WhereAt: Integer;
  theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain1 := aTrain1;
  fTrain2 := aTrain2;
  whereMeet := WhereAt;
  WhereLastMeet := NotSet;
  mustMeet := False;
end;

function TFormSA.GetFixedMeet: Boolean;
begin
  Result := mustMeet;
end;

function TFormSA.GetOrderStrings: String;
begin
  Result := Format( '%s %d MEET %d AT %s',
                    [ OrderNumberStr, TTrainData(fTrain1).Number,
                      TTrainData(fTrain2).Number,
                      IronMikeData.StationList[whereMeet].Name ] );
  if whereLastMeet <> NotSet then
    Result := Result + Format( ' INSTEAD OF %s',
                               [ IronMikeData.StationList[whereLastMeet].Name ] );
end;

procedure TFormSA.SetfixedMeet(AValue: Boolean);
begin
  mustMeet := AValue;
end;

function TFormSA.TrainsMeetAt(aTrain1, aTrain2: TObject; StnPos: Integer
  ): Boolean;
begin
  Result:=inherited TrainsMeetAt(aTrain1, aTrain2, StnPos);
  if ( (aTrain1 = fTrain1) and (aTrain2 = fTrain2) ) or
     ( (aTrain1 = fTrain2) and (aTrain2 = fTrain1) ) then
    if StnPos = whereMeet then
      Result := True;
end;

function TFormSA.WhereDoTrainsMeet(aTrain1, aTrain2: TObject): Integer;
begin
  // This code is directly translated from the c++ implementation.
  // It works there, but I have yet to figure out why we need to test
  // the trains for equality rather than just looking at the trains
  // from which the class was created.
  // Figured it out, this is used to search the train list so we see
  // all possible candidates.
  if ( (aTrain1 = fTrain1) and (aTrain2 = fTrain2) ) or
     ( (aTrain1 = fTrain2) and (aTrain2 = fTrain1) ) then
    Result := whereMeet
  else
    Result := NotSet;
end;


{ TFormE }

constructor TFormE.Create(aTrain: TObject; theKCTime, theSalTime,
  theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain := aTrain;
  fKCTime  := theKCTime;
  fSALTime := theSalTime;
end;

constructor TFormE.Create(aTrain: TObject; theTime: TDateTime);
begin
  inherited Create( theTime );
  fTrain := aTrain;
  fKCTime  := 0.0;
  fSALTime := 0.0;
end;

function TFormE.GetOrderStrings: String;
var
  Station : String;
begin
  Result := Format( '%s %d WAIT AT ',
                    [OrderNumberStr,TTrainData(fTrain).Number ] );
  case Stops of
    saNone :
      begin
        Result := Result + Format( '%s UNTIL %s',
                                   [ IronMikeData.StationList[TTrainData(fTrain).Schedule[1].Pos].Name,
                                     IronMikeData.RRTime( Time ) ] );
      end;
    saSAL :
      begin
        Result := Result + Format( 'SALINAS UNTIL %s',
                                   [IronMikeData.RRTime( fSALTime ) ]);
      end;
    saKC :
      begin
        Result := Result + Format( 'KING CITY UNTIL %s',
                                   [IronMikeData.RRTime( fKCTime ) ]);
      end;
    saBoth:
      begin
        if TTrainData(fTrain).Direction = 'Eastbound' then
          begin
            Result := Result + Format( 'SALINAS UNTIL %s',
                                       [IronMikeData.RRTime( fSALTime ) ]) + ' ';
            Result := Result + Format( 'KING CITY UNTIL %s',
                                       [IronMikeData.RRTime( fKCTime ) ]);
         end
        else
          begin
            Result := Result + Format( 'KING CITY UNTIL %s',
                                       [IronMikeData.RRTime( fKCTime ) ]) + ' ';
            Result := Result + Format( 'SALINAS UNTIL %s',
                                       [IronMikeData.RRTime( fSALTime ) ]);

          end;
      end;
  end;
end;

function TFormE.GetStops: TStopsAt;
begin
  Result:= saNone;
  if (fKCTime > 0.0) and (fSALTime > 0.0) then
    Result := saBoth
  else if fKcTime > 0.0 then
    Result := saKC
  else
    Result := saSAL;
end;

{ TOrder }

constructor TOrder.Create(theTime: TDateTime);
begin
  Inc(fTotalOrders);
  fOrderNumber := fTotalOrders;
  fTimeOccurred := theTime;
end;

function TOrder.GetFixedMeet: Boolean;
begin
  Result := False;
end;

function TOrder.GetStops: TStopsAt;
begin
  Result := saNone;
end;

function TOrder.GetTotalOrders: Integer;
begin
  Result := fTotalOrders;
end;

function TOrder.OrderNumberStr: String;
begin
  Result := Format('ORDER NO. %d - ',[OrderNumber]);
end;

class procedure TOrder.ResetTotalOrders;
begin
  fTotalOrders := 0;
end;

procedure TOrder.SetFixedMeet(AValue: Boolean);
begin
  // Does nothing in TOrder
end;

function TOrder.TrainsMeetAt(aTrain1, aTrain2: TObject; StnPos: Integer
  ): Boolean;
begin
  Result := False;
  //if ( (aTrain1 = fTrain1) and (aTrain2 = fTrain2) ) or
  //   ( (aTrain1 = fTrain2) and (aTrain2 = fTrain1) ) then
  //  if StnPos = whereMeet then
  //    Result := True;
end;

function TOrder.WhereDoTrainsMeet(aTrain1, aTrain2: TObject): Integer;
begin
  Result := NotSet;
end;

initialization

  TOrder.ResetTotalOrders; // Ensure that we start from 0
  OrdersList := TOrdersList.Create;
  TimetableList := TOrdersList.Create;

finalization

  OrdersList.Free;
  TimetableList.Free;

end.

