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

unit DispatcherFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Grids, Buttons, FormPanel, MagicStringGrid, IronMikeDataUnit,
  TrainsUnit,
  DisplayUnit; //  Temp for development


type

  { TDispatcherForm }

  TDispatcherForm = class(TPanelForm)
    BitBtnPrint: TBitBtn;
    BitBtnPrintAll: TBitBtn;
    BitBtnCurrent: TBitBtn;
    BitBtnNext: TBitBtn;
    BitBtnPrev: TBitBtn;
    BitBtnFirst: TBitBtn;
    ButtonAddTrain: TButton;
    AutoPrintCB: TCheckBox;
    ButtonStartOver: TButton;
    CrewCallListBox: TListBox;
    ButtonCrewChange: TButton;
    ButtonDelayTrain: TButton;
    ButtonFreightEnv: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    ButtonInfo: TButton;
    OrdersImage: TImage;
    OrdersScrollBox: TScrollBox;
    TrainRegisterGrid: TMagicStringGrid;
    ButtonOSReport: TButton;
    ButtonRegisterTrain: TButton;
    ButtonSecondSection: TButton;
    ButtonStartClock: TButton;
    ButtonStopClock: TButton;
    ButtonSyncClock: TButton;
    ButtonTerminateTrain: TButton;
    procedure BitBtnPrintAllClick(Sender: TObject);
    procedure BitBtnPrintClick(Sender: TObject);
    procedure BitBtnCurrentClick(Sender: TObject);
    procedure BitBtnFirstClick(Sender: TObject);
    procedure BitBtnNextClick(Sender: TObject);
    procedure BitBtnPrevClick(Sender: TObject);
    procedure ButtonDelayTrainClick(Sender: TObject);
    procedure ButtonRegisterTrainClick(Sender: TObject);
    procedure ButtonAddTrainClick(Sender: TObject);
    procedure ButtonCrewChangeClick(Sender: TObject);
    procedure ButtonFreightEnvClick(Sender: TObject);
    procedure ButtonInfoClick(Sender: TObject);
    procedure ButtonOSReportClick(Sender: TObject);
    procedure ButtonSecondSectionClick(Sender: TObject);
    procedure ButtonStartClockClick(Sender: TObject);
    procedure ButtonStartOverClick(Sender: TObject);
    procedure ButtonStopClockClick(Sender: TObject);
    procedure ButtonSyncClockClick(Sender: TObject);
    procedure ButtonTerminateTrainClick(Sender: TObject);
    procedure CrewCallListBoxDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GroupBox3DblClick(Sender: TObject);
    procedure OrdersImageDblClick(Sender: TObject);
    procedure TrainRegisterGridMagicSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
  { private declarations }
  private
    fSelectedTrain: TTrainData;
    fSelectedRegisterRow: Integer;
    fTick: TDateTime;
    vAllTrainsRun : Boolean;
    vDisplay : TDisplay;
    vDisplayList : TDisplayList;
    procedure SetSelectedRegisterRow(AValue: Integer);
    procedure SetSelectedTrain(AValue: TTrainData);
    procedure SetTick(AValue: TDateTime);

  protected
    procedure CrewCallListBoxAdd( ATime : TDateTime );
    procedure RegisterTrain( aNumber : String );
    procedure CrewChange( aTrain : TTrainData );
    procedure DelayTrain( aTrain : TTrainData; NewStart : TDateTime );
    procedure AddSecondSection( aTrain : TTrainData; Engine : String );
    procedure AddTrain( Number : Integer; Consist : String; Starts, Ends : Integer );

    property  SelectedRegisterRow : Integer    read fSelectedRegisterRow
                                               write SetSelectedRegisterRow;
    property  SelectedTrain       : TTrainData read fSelectedTrain
                                               write SetSelectedTrain;

  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
    procedure PreferencesChanged; override;

    procedure Initialize;

    property Tick : TDateTime read fTick write SetTick;

    property Display : TDisplay read vDisplay;
    property DisplayList : TDisplayList read vDisplayList;

  end;

var
  DispatcherForm: TDispatcherForm;

implementation

uses
  CommonDebug, CommonLog, CommonMath, CommonMisc,
  ConstantsUnit,
  Windows, // For beep during development
  MainFormUnit,
  Preferences,
  RailroadAnnouncements,
  RegisterTrainFormUnit,
  DelayTrainFormUnit,
  MeetVisualizationFormUnit, // To visualize meets during development
  OrdersUnit,
  RRSubsUnit,
  SelectWhereFormUnit,
  SecondSectionFormUnit,
  AddTrainFormUnit,
  TrainInfoFormUnit,
  SyncClockFormUnit,
  EnvelopeUnit;

{$R *.lfm}

{ TDispatcherForm }

procedure TDispatcherForm.BitBtnPrintClick(Sender: TObject);
begin
  vDisplay.Print;
end;

procedure TDispatcherForm.AddSecondSection(aTrain: TTrainData; Engine: String);
var
  SecondSect : TTrainData;
  Orders     : TFormF;
begin
   SecondSect := TTrainData.Create( aTrain );
   SecondSect.Schedule.Advance( 0.25/24.0 ); // Advance second section 15 min.
   // First Section
   Orders := TFormF.Create( aTrain,MainForm.MagicClock1.DateTime );
   OrdersList.Add( Orders );
   aTrain.Orders.Add( Orders );

   // Second Section
   IronMikeData.TrainList.Add( SecondSect );
   Orders := TFormF.Create( SecondSect, MainForm.MagicClock1.DateTime, Engine );
   OrdersList.Add( Orders );
   SecondSect.Orders.Add( Orders );

   IronMikeData.TrainList.Add( SecondSect );
   IronMikeData.TrainList.Sort;
   IronMikeData.CrewCallList.Clear;
   IronMikeData.CrewCallList.Assign(IronMikeData.TrainList);

   RunRailroad( SecondSect, vDisplay, vDisplayList );

end;

procedure TDispatcherForm.AddTrain(Number: Integer; Consist: String; Starts,
  Ends: Integer);
var
  NewTrain : TTrainData;
begin
   NewTrain := TTrainData.Create(Number,Consist,Starts,Ends,tfWhite,'Fifth Class',''); // Creates extra

   IronMikeData.TrainList.Add(NewTrain);
   IronMikeData.TrainList.Sort;
end;

procedure TDispatcherForm.BitBtnCurrentClick(Sender: TObject);
begin
  if vDisplayList.Count > 0 then
    OrdersImage.Picture.Assign( vDisplayList.LastImage );
end;

procedure TDispatcherForm.BitBtnFirstClick(Sender: TObject);
begin
  if vDisplayList.Count > 0 then
    OrdersImage.Picture.Assign( vDisplayList.FirstImage );
end;

procedure TDispatcherForm.BitBtnNextClick(Sender: TObject);
begin
  if vDisplayList.Count > 0 then
    OrdersImage.Picture.Assign( vDisplayList.NextImage );
end;

procedure TDispatcherForm.BitBtnPrevClick(Sender: TObject);
begin
  if vDisplayList.Count > 0 then
    OrdersImage.Picture.Assign( vDisplayList.PrevImage );
end;

procedure TDispatcherForm.BitBtnPrintAllClick(Sender: TObject);
var
  I : Integer;
begin
  if vDisplayList.Count > 0 then
    begin
      OrdersImage.Picture.Assign( vDisplayList.FirstImage );
      vDisplay.Print;
      for I := 1 to pred(vDisplayList.Count) do
        begin
          OrdersImage.Picture.Assign( vDisplayList.NextImage );
          vDisplay.Print;
        end;
    end;
   OrdersImage.Picture.Assign( vDisplayList.LastImage ); // Just in case.
end;

procedure TDispatcherForm.ButtonAddTrainClick(Sender: TObject);
begin
  if AddTrainForm.ShowModal = mrOK then
    begin
      AddTrain( AddTrainForm.TrainNumber,
                AddTrainForm.Consist,
                AddTrainForm.StartPos,
                AddTrainForm.EndPos );
    end;
end;

procedure TDispatcherForm.ButtonCrewChangeClick(Sender: TObject);
begin
  CrewChange(SelectedTrain);
end;

procedure TDispatcherForm.ButtonDelayTrainClick(Sender: TObject);
begin
  if DelayTrainForm.ShowModal = mrOk then
    begin
      DelayTrain( DelayTrainForm.Train, DelayTrainForm.NewTime );
    end;
end;

procedure TDispatcherForm.ButtonFreightEnvClick(Sender: TObject);
begin
  { TODO 5 -odonz -cOperational consistency : Need to do a little more here or in Watsonville form - see c++ }
  MainForm.GoToWatsonville;
end;

procedure TDispatcherForm.ButtonInfoClick(Sender: TObject);
var
  aTrain : TTrainData;
begin
  if TrainInfoForm.ShowModal = mrOk then
    begin
      aTrain := TrainInfoForm.Train;
      aTrain.PrintInfo(vDisplay,vDisplayList);
    end;
end;

procedure TDispatcherForm.ButtonOSReportClick(Sender: TObject);
var
  TimeNow : TDateTime;
  TrainTime : TDateTime;
  Where : Integer;
begin
  if SelectWhereForm.ShowModal = mrYes then
    begin
      TimeNow := MainForm.MagicClock1.DateTime;
      Where := SelectWhereForm.StationPos;
      TrainTime := SelectedTrain.Schedule.ByPos[Where].Departs;
      TrainTime := TimeNow-TrainTime;
      if TrainTime > 0.0 then
        begin
          SelectedTrain.RunLateFrom(Where,TrainTime);
          IronMikeData.TrainList.Sort;
          RunRailroad(SelectedTrain,vDisplay,vDisplayList);
        end;
    end;
end;

procedure TDispatcherForm.ButtonRegisterTrainClick(Sender: TObject);
var
  Idx : Integer;
begin
  if RegisterTrainForm.ShowModal = mrOk then
    begin
      Idx := CrewCallListBox.Items.IndexOf(RegisterTrainForm.SelectedTrain);
      if Idx >= 0 then
        CrewCallListBox.Items.Delete(Idx);
      ButtonRegisterTrain.Enabled := CrewCallListBox.Count > 0;
      RegisterTrain(RegisterTrainForm.SelectedTrain);
    end;
end;

procedure TDispatcherForm.ButtonSecondSectionClick(Sender: TObject);
begin
  if SecondSectionForm.ShowModal = mrOk then
    AddSecondSection(SecondSectionForm.Train, SecondSectionForm.Engine);
end;

procedure TDispatcherForm.ButtonStartClockClick(Sender: TObject);
begin
  MainForm.ClockStart;
  ButtonStartClock.Visible := False;
  ButtonStopClock.Visible  := True
end;

procedure TDispatcherForm.ButtonStartOverClick(Sender: TObject);
begin
  if AreYouSure('Start the Op Session at the Beginning') = mrYes then
    begin
      MainForm.MagicClock1.Running:=False;
      DispatcherForm.Initialize;
      ButtonStartClock.Visible := True;
      ButtonStopClock.Visible  := False
    end;

end;

procedure TDispatcherForm.ButtonStopClockClick(Sender: TObject);
begin
  MainForm.ClockStop;
  ButtonStartClock.Visible := True;
  ButtonStopClock.Visible  := False
end;

procedure TDispatcherForm.ButtonSyncClockClick(Sender: TObject);
begin
  if SyncClockForm.ShowModal = mrOk then
    MainForm.MagicClock1.DateTime := SyncClockForm.DateTime;
end;

procedure TDispatcherForm.ButtonTerminateTrainClick(Sender: TObject);
var
  EastIO, WestIO : String;
  aRow : Integer;
begin
  if Assigned( SelectedTrain ) then // Just in case
    if AreYouSure('Terminate train ' + IntToStr( SelectedTrain.Number ) ) = mrYes then
      begin
        SelectedTrain.Status := tsComplete;
        SelectedTrain.Schedule.Arrives := MainForm.MagicClock1.DateTime;
        with TrainRegisterGrid do
          begin
            aRow := SelectedRegisterRow;
            SelectedTrain.TrainIO(EastIO, WestIO);
            Cells[3,aRow] := WestIO;
            Cells[4,aRow] := EastIO;
            ButtonTerminateTrain.Enabled := False;
          end;
      end;
end;

procedure TDispatcherForm.CrewCallListBoxAdd(ATime: TDateTime);
var
  aTrain : TTrainData;
  LeadTime : TDateTime;
  TrainTime : TDateTime;
  S0, S1 : String;
  OpSessionData : TOpSessionDataRec;
  TrainNumber : String;
begin
  if IronMikeData.CrewCallList.Count > 0 then
    begin
      OpSessionData := IronMikeData.OpSession;
      LeadTime := ATime + OpSessionData.CrewCallLead;
      aTrain := TTrainData(IronMikeData.CrewCallList.Items[0]);
      TrainTime := aTrain.Schedule.Departs;
      S0 := DateTimeToStr(LeadTime);
      s1 := DateTimeToStr(TrainTime);
      if aTrain.Status <> tsReady then
        IronMikeData.CrewCallList.Delete(0)
      else if (LeadTime >= TrainTime) and (aTrain.Status = tsReady) then
        begin
          TrainNumber := IntToStr( aTrain.Number );
          if CrewCallListBox.Items.IndexOf( TrainNumber ) < 0 then
            begin
              CrewCallListBox.AddItem( TrainNumber, aTrain );
              CrewCallFor( aTrain.Number );
              IronMikeData.CrewCallList.Delete(0);
              ButtonRegisterTrain.Enabled := True;
            end;
        end;
    end
  else
    begin
      if not vAllTrainsRun then
        begin
          vAllTrainsRun := True;
          Application.ProcessMessages;
          MessageDlg( 'Iron Mike 2015',
                      'Call for last train in Op Session',
                      mtInformation,[mbOk],'');
        end;
    end;
end;

procedure TDispatcherForm.CrewCallListBoxDblClick(Sender: TObject);
var
  Idx : Integer;
  Train : String;
begin
  Idx := CrewCallListBox.ItemIndex;
  if Idx < 0 then exit; // Handle op being too quick on the draw
  Train := CrewCallListBox.Items[Idx];
  CrewCallListBox.Items.Delete(Idx);
  //ButtonRegisterTrain.Enabled := CrewCallListBox.Count > 0;
  Application.ProcessMessages;
  RegisterTrain( Train );
end;

procedure TDispatcherForm.CrewChange(aTrain: TTrainData);
var
  NewTrain : TTrainData;
  TimeNow  : TDateTime;
  TrainTime : TDateTime;
  Idx       : Integer;
  aRow      : Integer;
  EastIO, WestIO : String;
begin
  TimeNow := MainForm.MagicClock1.DateTime;
  NewTrain := IronMikeData.TrainList.TrainByNum[aTrain.Counterpart];
  // Terminate the original train;
  aTrain.Schedule.Arrives := TimeNow;
  atrain.Status := tsComplete;
  // Start the new train as appropriate
  TrainTime := NewTrain.Schedule.Departs;
  TrainTime := TimeNow - TrainTime;
  if TrainTime > 0 then
    NewTrain.RunLateFrom(5,TrainTime);
  //NewTrain.Status := tsRunning;
  IronMikeData.TrainList.Sort;
  Idx := CrewCallListBox.Items.IndexOf(IntToStr(NewTrain.Number));
  if Idx >= 0 then
    CrewCallListBox.Items.Delete(Idx);
  //ButtonRegisterTrain.Enabled := CrewCallListBox.Count > 0;
  with TrainRegisterGrid do
    begin
      aRow := SelectedRegisterRow;
      SelectedTrain.TrainIO(EastIO, WestIO);
      Cells[3,aRow] := WestIO;
      Cells[4,aRow] := EastIO;
      ButtonTerminateTrain.Enabled := False;
    end;
  Application.ProcessMessages;
  RegisterTrain( IntToStr(NewTrain.Number) );
end;

procedure TDispatcherForm.DelayTrain(aTrain: TTrainData; NewStart: TDateTime);
var
  OrigDepart : TDateTime;
  Order      : TOrder;
begin
  OrigDepart := aTrain.Schedule.Departs;
  Order := TFormE.Create( aTrain, NewStart);
  OrdersList.Add( Order );

  Post2Extras( Order, aTrain, nil);

  //IF SCEHDULED, FIGURE IT OUT NOW - EXTRA, DO WHEN LEAVE
  if not aTrain.IsExtra then
    begin
      NewStart := NewStart - Frac(OrigDepart);
      if NewStart > 0.0 then
        begin
          aTrain.RunLateFrom(aTrain.Schedule[1].Pos,NewStart );
          IronMikeData.TrainList.Sort;
          IronMikeData.CrewCallList.Clear;
          IronMikeData.CrewCallList.Assign(IronMikeData.TrainList);
          RunRailroad(aTrain,vDisplay,vDisplayList);
        end;
    end;
end;

procedure TDispatcherForm.FormCreate(Sender: TObject);
var
  UFont : TFont;
begin
  UFont := TFont.Create;
  UFont.Name := PreferencesForm.OrdersFontName;
  UFont.Size := 9;
  vDisplay := TDisplay.Create( OrdersImage, OrdersScrollBox.Font );
  vDisplay.UnderscoreFont :=  UFont;
  vDisplayList := TDisplayList.Create;
end;

procedure TDispatcherForm.GroupBox3DblClick(Sender: TObject);
begin
  MeetVisualizationForm.ShowModal;
end;

procedure TDispatcherForm.Hiding;
begin

end;

procedure TDispatcherForm.Initialize;
var
  OpSessionData : TOpsessionDataRec;
  RailroadData  : TRailroadDataRec;
  TrainData     : TTrainData;
  Idx           : Integer;
begin
  with IronMikeData do
    begin
      // First get the Data needed for defaults and initialization.
      OpSessionData := OpSession;
      RailroadData  := Railroad;
      // Now Populate the Lists
      TimetableList.Clear;
      StationList.Clear;
      TrainList.Clear;
      CrewCallList.Clear;
      OrdersList.Clear;

      PopulateStationList;

      vDisplayList.Clear;
      vDisplay.Image.Canvas.FillRect( vDisplay.Image.BoundsRect );

      PopulateTrainList; { TODO 1 -odonz -cOperational : Move to just after opening db tables }

      CrewCallList.Clear;
      CrewCallList.Assign(TrainList);

      CrewCallListBox.Clear;

      vAllTrainsRun := False;

      // Set the controls to the appropriate values.

      TrainRegisterGrid.RowCount := 1;

      MainForm.MagicClock1.AMPM := RailroadData.ClockAMPM;
      MainForm.MagicClock1.SpeedMultiplier := OpSessionData.FastClock;
      MainForm.MagicClock1.DateTime := OpSessionData.StartDate + OPSessionData.StartTime;
      { TODO 2 -odonz -cPractical User Interface : Add speed vernir data to initialize }
    end;
  InitEnvelopes;
end;

procedure TDispatcherForm.OrdersImageDblClick(Sender: TObject);
begin
  vDisplay.Print;
end;

procedure TDispatcherForm.PreferencesChanged;
var
  aFont : TFont;
begin
  aFont.Create;
  aFont.Name := PreferencesForm.OrdersFontName;
  aFont.Size := 9;
  vDisplay.UnderscoreFont := aFont;
  aFont.Free;
end;

procedure TDispatcherForm.TrainRegisterGridMagicSelectCell(Sender: TObject;
  aCol, aRow: Integer; var CanSelect: Boolean);
var
  theTrain : TTrainData;
begin
  CanSelect := aRow > 0;
  if CanSelect then
    begin
      SelectedRegisterRow := aRow;
      theTrain := TTrainData( TrainRegisterGrid.Objects[0,aRow] );
      ButtonTerminateTrain.Enabled := theTrain.Status = tsRunning;
      ButtonCrewChange.Enabled     := (theTrain.Status = tsRunning) and
                                      (theTrain.Counterpart > 0);
    end;
end;

procedure TDispatcherForm.RegisterTrain(aNumber: String);
var
  ATrain : TTrainData;
  aRow   : Integer;
  EastIO, WestIO : String;
  Orders : TFormG;
  I      : Integer;
  TimeNow : TDateTime;
begin
  ATrain := IronMikeData.TrainList.TrainByNum[StrToInt(aNumber)];
  if ATrain = nil then
    Stub( Format('can''t find Train by num %d',[aNumber]) );
  if aTrain.Status in [tsRunning, tsComplete] then
    begin
      I := 0;
      exit; // train is already registered
    end;
  try
    ATrain.StartRun;
  except
    Stub( Format( ' Error starting run of train %d',[ATrain.Number] ) );
  end;
  if aTrain.IsExtra then
    begin
      Orders := TFormG.Create( aTrain,MainForm.MagicClock1.DateTime,aNumber );
      OrdersList.Add( Orders );
      post2Extras(Orders,aTrain,nil);
      aTrain.BuildTimetable();

      TimeNow := MainForm.MagicClock1.DateTime;
      for I := 0 to pred(OrdersList.Count) do
        if CBLX( TimeNow-ExtraOrdersOffset,
                 TOrder(OrdersList[I]).Time,
                 TimeNow+ExtraOrdersOffset ) then
          aTrain.Orders.Add(OrdersList[I]);
    end;
  //Atrain.Schedule.DebugLog('Register Train ' + aNumber);
  Atrain.DebugLog('RegisterTrain',false);
  RunRailroad( ATrain, vDisplay, vDisplayList );
  with TrainRegisterGrid do
    begin
      aRow := RowCount;
      RowCount := aRow + 1;
      Cells[0,aRow] := IntToStr(ATrain.Number);
      Cells[1,aRow] := ATrain.Direction;
      Cells[2,aRow] := ATrain.TrainFlag;
      ATrain.TrainIO( EastIO, WestIO );
      Cells[3,aRow] := WestIO;
      Cells[4,aRow] := EastIO;
      Objects[0,aRow] := ATrain;
      SelectedRegisterRow := aRow;
      aTrain.PrintClearance( vDisplay, vDisplayList );
      //vDisplayList.Append( vDisplay.Image.Picture );
      ClearanceFor( aTrain.Number );
      if AutoPrintCB.Checked then
        vDisplay.Print;
      BitBtnCurrent.Enabled := True;
      BitBtnNext.Enabled := True;
      BitBtnPrev.Enabled := True;
      BitBtnFirst.Enabled := True;
    end;
end;

procedure TDispatcherForm.SetSelectedRegisterRow(AValue: Integer);
begin
  if fSelectedRegisterRow=AValue then Exit;
  fSelectedRegisterRow:=AValue;
  TrainRegisterGrid.Row := AValue;
  SelectedTrain := TTrainData(TrainRegisterGrid.Objects[0,AValue]);
end;

procedure TDispatcherForm.SetSelectedTrain(AValue: TTrainData);
begin
  if fSelectedTrain=AValue then Exit;
  fSelectedTrain:=AValue;
  if fSelectedTrain = nil then
    begin
      ButtonTerminateTrain.Enabled     := False;
      ButtonCrewChange.Enabled         := False;
      ButtonOsReport.Enabled           := False;
    end
  else
    begin
      ButtonTerminateTrain.Enabled     := True;
      ButtonCrewChange.Enabled         := fSelectedTrain.Counterpart > 0;
      ButtonOSReport.Enabled           := True;
    end;
end;

procedure TDispatcherForm.SetTick(AValue: TDateTime);
var
  s : string;
begin
  if fTick=AValue then Exit;
  fTick:=AValue;
  S := IronMikeData.RRTime(fTick) ;
  CrewCallListBoxAdd( fTick );
end;

procedure TDispatcherForm.Showing;
begin
  { TODO -odonz -cRequired : populate timetable list here }
  // Actually this should be input via a form perhaps using the automatic
  // meet identifying routine I wrote for development.
  //Stub('Timetable meet setup');


  // 924
  TimetableList.Add( TFormSA.Create(924,373, 'King City',       '03:05 am',false));
  TimetableList.Add( TFormSA.Create(924,75,  'King City',       '03:18 am',false));
  TimetableList.Add( TFormSA.Create(924,919, 'San Luis Obispo', '04:22 am',false));

  // 922
  TimetableList.Add( TFormSA.Create(922,923, 'Serrano',         '07:02 pm',false));

  // 920
  TimetableList.Add( TFormSA.Create(920,71,  'Serrano',         '09:49 am',false));
  TimetableList.Add( TFormSA.Create(920,921, 'San Luis Obispo', '10:20 am',true)); // Check out "true" here.

  // 76
  TimetableList.Add( TFormSA.Create( 76, 95, 'Watsonville',     '01:51 am',false));
  TimetableList.Add( TFormSA.Create( 76,373, 'San Luis Obispo', '02:35 am',false));

  // 374
  TimetableList.Add( TFormSA.Create(374, 95, 'Salinas',         '01:23 am',false));

  // 98
  TimetableList.Add( TFormSA.Create( 98, 71, 'Salinas',         '10:39 am',false));
  TimetableList.Add( TFormSA.Create( 98,921, 'Serrano',         '10:57 am',false));

  //fixed meets - trains cannot depart until meet happens in SLO
  //76
  TimetableList.Add( TFormSA.Create( 76, 75, 'San Luis Obispo', '02:50 am',true));

  //374
  TimetableList.Add( TFormSA.Create(374,373, 'San Luis Obispo', '02:35 am',true));

  //94
  TimetableList.Add( TFormSA.Create( 94, 95, 'San Luis Obispo', '12:55 am',true));

  //98
  TimetableList.Add( TFormSA.Create( 98, 99, 'San Luis Obispo', '12:01 pm',true));

  TOrder.ResetTotalOrders; // Ensure that we re-start from 0
end;

end.

