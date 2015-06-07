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

unit SalinasShedFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, FormPanel, MealyFiniteStateMachine;

type

  TTracks = (trkIcing, trkNY, trkFH, trkTokyo, trkKansas, trkState); // trkState for debug

  { TSalinasShedForm }

  TSalinasShedForm = class(TPanelForm)
    ButtonIce: TButton;
    ButtonFinalIce: TButton;
    ButtonForceIce: TButton;
    ButtonPrint: TButton;
    ButtonDepart: TButton;
    ButtonNewYork: TButton;
    ButtonFreightHouse: TButton;
    ButtonTokyo: TButton;
    ButtonKansas: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    FSM: TMealyFiniteStateMachine;
    Memo1: TMemo;
    procedure ButtonDepartClick(Sender: TObject);
    procedure ButtonForceIceClick(Sender: TObject);
    procedure ButtonIceClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FSMEnterState(Sender: TComponent; AState: String);
    procedure ButtonSpottedClick(Sender: TObject);
  private
    { private declarations }
    Cars : array [TTracks] of Integer;
    CarsFinal : array [TTracks] of Boolean;
    fText : array [TTracks] of String;
    SpottedCar : Integer; // Set by the car spotted button tag
    Spotted : Integer; // Bit vector 15 means all done
    BaseText : String;
    fTick: TDateTime;
    fIcingDone : TDateTime;
    fTracksDone : array [TTracks] of TDateTime;

    procedure SetTick(AValue: TDateTime);
    procedure UpdateText;

    procedure DoStart;
    procedure DoIcing;
    procedure DoIcingTimer;
    procedure DoStartSpotting;
    procedure DoSpotting;
    procedure DoLoading;
    procedure DoLoadingBack;
    procedure DoBackForIcing;
    procedure DoReadyToDepart;
    procedure DoFinalIcing;
    procedure DoStartFinalIcing;
    procedure DoStartLoading;
    procedure CommonBackForIcing;

  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;

    property Tick : TDateTime read fTick write SetTick;
  end;

var
  SalinasShedForm: TSalinasShedForm;

implementation

uses
  CommonDebug, CommonLog,
  IronMikeDataUnit,
  Math,
  MainFormUnit,
  ConstantsUnit,
  RailroadAnnouncements;

{$R *.lfm}

const
  TrackFields : array [TTracks] of String = ( '#Icing','#NY','#FH','#Tok', '#Kan', '#state' );

{ TSalinasShedForm }

procedure TSalinasShedForm.ButtonDepartClick(Sender: TObject);
begin
  FSM.Event('ButtonDepart');
end;

procedure TSalinasShedForm.ButtonForceIceClick(Sender: TObject);
begin
  FSM.Event('ButtonForce');
end;

procedure TSalinasShedForm.ButtonIceClick(Sender: TObject);
begin
  FSM.Event('ButtonIce');
end;

procedure TSalinasShedForm.ButtonSpottedClick(Sender: TObject);
begin
  SpottedCar := (Sender as TButton).Tag;
  FSM.Event('ButtonSpotted');
end;

procedure TSalinasShedForm.CommonBackForIcing;
var
  Trk : TTracks;
  //TimeNow : TDateTime;
begin
  //TimeNow := MainForm.MagicClock1.DateTime;
  case SpottedCar of
    0:
      begin
        ButtonNewYork.Enabled := False;
      end;
    1:
      begin
        ButtonFreightHouse.Enabled := False;
      end;
    2:
      begin
        ButtonTokyo.Enabled := False;
      end;
    3:
      begin
        ButtonKansas.Enabled := False;
      end;
  end;
  Trk := TTracks(SpottedCar + 1);
  CarsFinal[Trk] := True;
  //fTracksDone[Trk] := Random*(ProduceLoadMax - ProduceLoadMin) + ProduceLoadMin;
  ////fTracksDone[Trk] := fTracksDone[Trk]/8; // Really fast for testing
  //fTracksDone[Trk] := fTracksDone[Trk] + TimeNow;
  Inc(Cars[trkIcing],Cars[Trk]);
  Cars[Trk] := 0;
  fText[trkIcing] := Format( '%d cars returned for final icing',[Cars[trkIcing]]);
  fText[Trk] := 'EMPTY';
  //fText[trkState] := FSM.CurrentState;
  UpdateText;

end;

procedure TSalinasShedForm.DoBackForIcing;
//var
//  Trk : TTracks;
begin
  if SpottedCar < 0 then Exit;
  CommonBackForIcing;
  //case SpottedCar of
  //  0:
  //    begin
  //      ButtonNewYork.Enabled := False;
  //    end;
  //  1:
  //    begin
  //      ButtonFreightHouse.Enabled := False;
  //    end;
  //  2:
  //    begin
  //      ButtonTokyo.Enabled := False;
  //    end;
  //  3:
  //    begin
  //      ButtonKansas.Enabled := False;
  //    end;
  //end;
  //Trk := TTracks(SpottedCar + 1);
  //fTracksDone[Trk] := Random*(ProduceLoadMax - ProduceLoadMin) + ProduceLoadMin;
  ////fTracksDone[Trk] := fTracksDone[Trk]/8; // Really fast for testing
  //fTracksDone[Trk] := fTracksDone[Trk] + TimeNow;
  //Inc(Cars[trkIcing],Cars[Trk]);
  //Cars[Trk] := 0;
  //fText[trkIcing] := Format( '%d cars returned for final icing',[Cars[trkIcing]]);
  //fText[Trk] := 'EMPTY';
  //UpdateText;
  Spotted := Spotted and (not (1 shl SpottedCar));
  Log.FormatLn( 'BackForIcing:  Spotted = %d  SpottedCar = %d',[Spotted, SpottedCar] );
  if Spotted = 0 then  // all four tracks have been returned for icing
    FSM.Event('Done');
end;

procedure TSalinasShedForm.DoFinalIcing;
begin
  if fTick > fIcingDone then
    begin
      fText[trkIcing] := 'FINAL ICING COMPLETE:  Ready to Depart';
      FinalIcingComplete;
      FSM.Event('Done');
    end;
end;

procedure TSalinasShedForm.DoIcing;
var
  Trk : TTracks;
  CarCount : Integer;
  function Roll : Integer;
  begin
    Result := RandomRange(1,7); // should return values in 1..6 inclusive
    case Result of
      1: Result := 2;
      2,3,4: Result := 3;
      5,6: Result := 4;
      else
        Result := 4; // Just in case.  This should never happen
    end;
  end;

begin
  ButtonIce.Enabled := False;
  ButtonForceIce.Enabled := True;
  ButtonPrint.Enabled := True;
  ButtonDepart.Enabled := False;
  ButtonNewYork.Enabled := False;
  ButtonFreightHouse.Enabled := False;
  ButtonTokyo.Enabled := False;
  ButtonKansas.Enabled := False;
  fIcingDone := Random*(IceLoadMax-IceLoadMin) + IceLoadMin;
  //fIcingDone := fIcingDone / 8; // Extra fast for testing
  fIcingDone := MainForm.MagicClock1.DateTime + fIcingDone;
  fText[trkIcing] := 'PRE-ICING:  Complete at ' + IronMikeData.RRTime(fIcingDone);

  CarCount := 12;
  for Trk := trkNY to trkTokyo do
    begin
      Cars[Trk] := Roll;
      Dec(CarCount,Cars[Trk]);
      fText[Trk] := IntToStr(Cars[Trk]) + ' cars requested.';
    end;
  Cars[trkKansas] := CarCount;
  fText[trkKansas] := IntToStr(Cars[trkKansas]) + ' cars requested.';

  //fText[trkState] := FSM.CurrentState;
  UpdateText;
end;

procedure TSalinasShedForm.DoIcingTimer;
begin
  if fTick >= fIcingDone then
    FSM.Event('Done');
end;

procedure TSalinasShedForm.DoLoading;
var
  Count : Integer;
  Trk   : TTracks;
begin
  Count := 0;
  for Trk := trkNY to trkKansas do
    begin
      if (fTick > fTracksDone[Trk]) and (not CarsFinal[Trk]) then
        begin
          Inc(Count);
          case Trk of
            trkNY:
              begin
                if not ButtonNewYork.Enabled then
                  begin
                    fText[Trk] := Format( '%d cars loaded, ready for final icing.',
                                          [ Cars[Trk] ]);
                    CarsLoaded('New York');
                  end;
                ButtonNewYork.Enabled := True;
              end;
            trkFH:
              begin
                if not ButtonFreightHouse.Enabled then
                  begin
                    fText[Trk] := Format( '%d cars loaded, ready for final icing.',
                                          [ Cars[Trk] ]);
                    CarsLoaded('Freight House');
                  end;
                ButtonFreightHouse.Enabled := True;
              end;
            trkTokyo:
              begin
                if not ButtonTokyo.Enabled then
                  begin
                    fText[Trk] := Format( '%d cars loaded, ready for final icing.',
                                          [ Cars[Trk] ]);
                    CarsLoaded('Tokyo');
                  end;
                ButtonTokyo.Enabled := True;
              end;
            trkKansas:
              begin
                if not ButtonKansas.Enabled then
                  begin
                    fText[Trk] := Format( '%d cars loaded, ready for final icing.',
                                          [ Cars[Trk] ]);
                    CarsLoaded('Kansas');
                  end;
                ButtonKansas.Enabled := True;
              end;
          end;
        end;
    end;
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
  SpottedCar := -1;
  if Count > 3 then
    FSM.Event('Done');
end;

procedure TSalinasShedForm.DoLoadingBack;
begin
  if SpottedCar >= 0 then
    begin
      CommonBackForIcing;
      Spotted := Spotted and (not (1 shl SpottedCar));
      Log.FormatLn( 'DoLoadingBack:  Spotted = %d  SpottedCar = %d',[Spotted, SpottedCar] );
    end;
  if Spotted = 0 then
    FSM.Event('Done');
end;

procedure TSalinasShedForm.DoReadyToDepart;
begin
  ButtonDepart.Enabled := True;
end;

procedure TSalinasShedForm.DoSpotting;
var
  Trk : TTracks;
  TimeNow : TDateTime;
begin
  if SpottedCar < 0 then Exit;
  TimeNow := MainForm.MagicClock1.DateTime;
  case SpottedCar of
    0:
      begin
        ButtonNewYork.Enabled := False;
      end;
    1:
      begin
        ButtonFreightHouse.Enabled := False;
      end;
    2:
      begin
        ButtonTokyo.Enabled := False;
      end;
    3:
      begin
        ButtonKansas.Enabled := False;
      end;
  end;
  Trk := TTracks(SpottedCar + 1);
  fTracksDone[Trk] := Random*(ProduceLoadMax - ProduceLoadMin) + ProduceLoadMin;
  fTracksDone[Trk] := FTracksDone[Trk] + Cars[Trk]*HalfHour;
  //fTracksDone[Trk] := fTracksDone[Trk]/8; // Really fast for testing
  fTracksDone[Trk] := fTracksDone[Trk] + TimeNow;
  fText[Trk] := Format('%d cars loading, done at %s',[Cars[Trk],IronMikeData.RRTime(fTracksDone[Trk])] );
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
  Spotted := Spotted or (1 shl SpottedCar);
  if Spotted = 15 then  // all four tracks have been spotted
    FSM.Event('Done');
end;

procedure TSalinasShedForm.DoStart;
begin
  ButtonIce.Enabled := True;
  ButtonFinalIce.Enabled := False;
  ButtonForceIce.Enabled := False;
  ButtonPrint.Enabled := False;
  ButtonDepart.Enabled := False;
  ButtonNewYork.Enabled := False;
  ButtonFreightHouse.Enabled := False;
  ButtonTokyo.Enabled := False;
  ButtonKansas.Enabled := False;
  Spotted := 0;
  fText[trkIcing] := 'EMPTY';
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
end;

procedure TSalinasShedForm.DoStartFinalIcing;
begin
  fIcingDone := Random*(IceLoadMax-IceLoadMin) + IceLoadMin;
  fIcingDone := MainForm.MagicClock1.DateTime + fIcingDone;
  fText[trkIcing] := Format( 'FINAL ICING:  will be done at %s',
                             [IronMikeData.RRTime( fIcingDone )] );
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
  FSM.Event('Done');
end;

procedure TSalinasShedForm.DoStartLoading;
var
  Trk : TTracks;
begin
  fText[trkIcing] := 'EMPTY:  awaiting loaded cars';
  for Trk := trkIcing to trkKansas do
    CarsFinal[Trk] := False;
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
  FSM.Event('Done');
end;

procedure TSalinasShedForm.DoStartSpotting;
begin
  ButtonNewYork.Enabled := True;
  ButtonFreightHouse.Enabled := True;
  ButtonTokyo.Enabled := True;
  ButtonKansas.Enabled := True;
  ButtonForceIce.Enabled := False;
  SpottedCar := -1;
  fText[trkIcing] := 'PRE-ICING COMPLETE:  Ready for spotting at sheds.';
  //fText[trkState] := FSM.CurrentState;
  UpdateText;
  PreIcingComplete;
  FSM.Event('Done');
end;

procedure TSalinasShedForm.FormCreate(Sender: TObject);
var
  Trk : TTracks;
begin
  BaseText := Memo1.Text;
  for Trk := trkIcing to trkKansas do
    fText[Trk] := 'EMPTY';
  fText[trkState] := ''; // FSM.CurrentState;
  UpdateText;
  FSM.Event('Start');
end;

procedure TSalinasShedForm.FSMEnterState(Sender: TComponent; AState: String);
begin
  if      AState = 'START'            then DoStart
  else if AState = 'ICING'            then DoIcing
  else if AState = 'ICINGTIMER'       then DoIcingTimer
  else if AState = 'STARTSPOTTING'    then DoStartSpotting
  else if AState = 'SPOTTING'         then DoSpotting
  else if AState = 'LOADING'          then DoLoading
  else if AState = 'LOADINGBACK'      then DoLoadingBack
  else if AState = 'STARTLOADING'     then DoStartLoading
  else if AState = 'BACKFORICING'     then DoBackForIcing
  else if AState = 'READYTODEPART'    then DoReadyToDepart
  else if AState = 'STARTFINALICING'  then DoStartFinalIcing
  else if AState = 'FINALICING'       then DoFinalIcing
  else
    Stub1('State:  [' + aState + ']');
end;

procedure TSalinasShedForm.Hiding;
begin

end;

procedure TSalinasShedForm.SetTick(AValue: TDateTime);
var
  S : String;
begin
  if fTick=AValue then Exit;
  fTick:=AValue;
  S := FSM.CurrentState;
  FSM.Event('TICK');
end;

procedure TSalinasShedForm.Showing;
begin
  UpdateText;
end;

procedure TSalinasShedForm.UpdateText;
var
  TrK : TTracks;
  Lines : String;
  function StrValue( T : TTracks ) : String;
  begin
    if Cars[T] = 0 then
      Result := 'EMPTY'
    else
      Result := IntToStr( Cars[T] ) + ' cars.';
  end;

begin
  Lines := BaseText;
  for Trk := trkIcing to trkState do
    Lines := StringReplace( Lines, TrackFields[Trk],fText[ Trk ], [rfReplaceAll, rfIgnoreCase] );
  Memo1.Text := Lines;
end;

end.

