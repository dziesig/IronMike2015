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

unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  Menus, ActnList, ExtCtrls, ComCtrls, StdCtrls, Buttons, FormPanel,
  MealyFiniteStateMachine, sqldb, db, sqlite3conn, CommonStacks,
  MagicClock, IronMikeDataUnit, EnvelopeUnit;

type

  TEnvelopeWarning = array[ TEnvelopeType] of TLabel;
  TEnvelopeActive  = array[ TEnvelopeType] of TLabel;
  TEnvelopeButton  = array[ TEnvelopeType] of TButton;

  { TMainForm }

  TMainForm = class(TForm)
    ButtonEnvelopeAPickup: TButton;
    ButtonEnvelopeBPickup: TButton;
    ButtonSLOFreightEnvPickup: TButton;
    FileTimetableAction: TAction;
    FileSystemMapAction: TAction;
    ActionList1: TActionList;
    DispatcherAction: TAction;
    DispatcherMenuItem: TMenuItem;
    EventPanel: TPanel;
    FileCloseAction: TAction;
    FileExitAction: TAction;
    FileFastClockSetupAction: TAction;
    FileNewAction: TAction;
    FileOpenAction: TAction;
    FilePreferencesAction: TAction;
    FilePrinterSetupAction: TAction;
    FileRestoreAction: TAction;
    FileSaveAsAction: TAction;
    FileVoiceSetupAction: TAction;
    FreightAgentMenuItem: TMenuItem;
    FreightAgentOrderAction: TAction;
    FreightAgentSalinasAction: TAction;
    FreightAgentWatsonvilleAction: TAction;
    HelpAboutAction: TAction;
    ImageList1: TImageList;
    LabelEnvelopeBWarning: TLabel;
    LabelSLOFreightEnvWarning: TLabel;
    LabelEnvelopeAReady: TLabel;
    LabelEnvelopeAWarning: TLabel;
    LabelEnvelopeBReady: TLabel;
    LabelSLOFreightEnvReady: TLabel;
    MagicClock1: TMagicClock;
    MagicFormPanel1: TMagicFormPanel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem22: TMenuItem;
    SystemMapMenuItem: TMenuItem;
    RunMenuItem: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MenuItemFile: TMenuItem;
    LastOpSessionMenuItem: TMenuItem;
    SetupMenuItem: TMenuItem;
    MFSM: TMealyFiniteStateMachine;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PrintDialog1: TPrintDialog;
    RailroadGeneralAction: TAction;
    RailroadMenuItem: TMenuItem;
    RailroadOpSessionAction: TAction;
    RailroadStationsAction: TAction;
    RailroadTimetablesAction: TAction;
    RailroadTrainsAction: TAction;
    RRTimetableAction: TAction;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure ButtonEnvelopeAPickupClick(Sender: TObject);
    procedure ButtonEnvelopeBPickupClick(Sender: TObject);
    procedure ButtonSLOFreightEnvPickupClick(Sender: TObject);
    procedure DispatcherActionExecute(Sender: TObject);
    procedure DispatcherMenuItemClick(Sender: TObject);
    procedure FileCloseActionExecute(Sender: TObject);
    procedure FileExitActionExecute(Sender: TObject);
    procedure FileFastClockSetupActionExecute(Sender: TObject);
    procedure FileNewActionExecute(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FilePreferencesActionExecute(Sender: TObject);
    procedure FilePrinterSetupActionExecute(Sender: TObject);
    procedure FileRestoreActionExecute(Sender: TObject);
    procedure FileSaveAsActionExecute(Sender: TObject);
    procedure FileSystemMapActionExecute(Sender: TObject);
    procedure FileTimetableActionExecute(Sender: TObject);
    procedure FileVoiceSetupActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FreightAgentOrderActionExecute(Sender: TObject);
    procedure FreightAgentSalinasActionExecute(Sender: TObject);
    procedure FreightAgentWatsonvilleActionExecute(Sender: TObject);
    procedure HelpAboutActionExecute(Sender: TObject);
    procedure LastOpSessionMenuItemClick(Sender: TObject);
    procedure MFSMEnterState(Sender: TComponent; AState: String);
    procedure RailroadGeneralActionExecute(Sender: TObject);
    procedure RailroadOpSessionActionExecute(Sender: TObject);
    procedure RailroadStationsActionExecute(Sender: TObject);
    procedure RailroadTimetablesActionExecute(Sender: TObject);
    procedure RailroadTrainsActionExecute(Sender: TObject);
    procedure MagicClock1HeartBeat(Sender: TObject; out Time: TDateTime);
    procedure RRTimetableActionExecute(Sender: TObject);
    procedure RunMenuItemClick(Sender: TObject);
    procedure SetupMenuItemClick(Sender: TObject);
    procedure SystemMapMenuItemClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    CursorStack : TAppCursorStack;
    EnvelopeWarning : TEnvelopeWarning;
    EnvelopeActive  : TEnvelopeActive;
    EnvelopeButton  : TEnvelopeButton;

    procedure DoStart;
    procedure DoCreatingFile;
    procedure DoExited;
    procedure DoExiting;
    procedure DoOpeningFile;
    procedure DoOpenLastFile;
    procedure DoRestoreFile;
    procedure DoReady;
    procedure DoSaveAs;
    procedure DoClosing;
    procedure DoRunning;
    procedure DoStopping;
    procedure DoSetupVoice;
    procedure DoSetupClock;
    procedure DoPreferences;
    procedure DoDispatcher;
    procedure DoWatsonville;
    procedure DoSalinas;
    procedure DoOrders;
    procedure DoRailroad;
    procedure DoStations;
    procedure DoTrains;
    procedure DoTimetable;
    procedure DoOpSession;
    procedure DoExitRunning;
    procedure DoResume;
    procedure HideEnvelopes;
    procedure EnvelopeTick( theTime : TDateTime );

    procedure OpenDBFile( Path : String );

    function GetFileName: String;
    function GetLastFileName: String;
    procedure SetFileName(AValue: String);
    procedure SetLastFileName(AValue: String);
  public
    { public declarations }
    procedure ClockStart;   // So dispatcher form can start/stop clock
    procedure ClockStop;
    procedure BackToDispatcher; // So Freight Agent can return to dispatcher
    procedure GoToWatsonville;  // So Dispatcher can directly go to watsonville form.

    property FileName : String read GetFileName write SetFileName;
    property LastFileName : String read GetLastFileName write SetLastFileName;
  end;

var
  MainForm: TMainForm;

implementation

uses
  CommonDebug, CommonIo, CommonMisc, CommonIni, CommonApp, Stringsubs,
  Preferences, AboutIronMike, VoiceForm, FastClockWizardForm,
  SplashFormUnit,
  RailroadFormUnit,
  DispatcherFormUnit,
  WatsonvilleFormUnit,
  SalinasShedFormUnit,
  SystemMapFormUnit,
  OrdersFormUnit,
  StationsFormUnit,
  TrainsFormUnit,
  TimetableFormUnit,
  OpSessionFormUnit,
  RailroadAnnouncements,
  ShowTimetableFormUnit,
  TimetableDisplayFormUnit,
  Math,
  Windows; // For ding when envelope warning issued.

const
  LastOpSession = 'Last Op Session';

{$R *.lfm}

{ TMainForm }

procedure TMainForm.BackToDispatcher;
begin
  MagicFormPanel1.Form := DispatcherForm;
end;

procedure TMainForm.ButtonEnvelopeAPickupClick(Sender: TObject);
begin
  Envelopes[etEnvelopeA].SetComplete;
  MagicFormPanel1.Form := WatsonvilleForm;
  WatsonvilleForm.ShowEnvelopes;
end;

procedure TMainForm.ButtonEnvelopeBPickupClick(Sender: TObject);
begin
  Envelopes[etEnvelopeB].SetComplete;
  MagicFormPanel1.Form := WatsonvilleForm;
  WatsonvilleForm.ShowEnvelopes;
end;

procedure TMainForm.ButtonSLOFreightEnvPickupClick(Sender: TObject);
begin
  Envelopes[etSLOFreight].SetComplete;
  MagicFormPanel1.Form := WatsonvilleForm;
  WatsonvilleForm.ShowEnvelopes;
end;

procedure TMainForm.ClockStart;
begin
  MagicClock1.Running := True;
  SetupMenuItem.Enabled := False;
end;

procedure TMainForm.ClockStop;
begin
  MagicClock1.Running := False;
  SetupMenuItem.Enabled := True;
end;

procedure TMainForm.DispatcherActionExecute(Sender: TObject);
begin
  MFSM.Event('Dispatcher');
end;

procedure TMainForm.DispatcherMenuItemClick(Sender: TObject);
begin
  MFSM.Event('Dispatcher');
end;

procedure TMainForm.DoClosing;
begin
  MagicFormPanel1.Form := SplashForm;   // Must come before closing IronMikeData
                                        // so we can clean up changes
  IronMikeData.Close(True);
  FileName := '';
  FileSystemMapAction.Enabled := False;
  FileTimetableAction.Enabled := False;
  MFSM.Event('Done');
end;

//==============================================================================
// In the CreatingFile state, the user is prompted for the name of an new
// op session file, and opens it if one is entered.  If the user cancels the
// operation, it signals the Cancel event.
//==============================================================================

procedure TMainForm.DoCreatingFile;
begin
  if SaveDialog1.Execute then
    begin
      OpenDBFile( SaveDialog1.FileName );
      MFSM.Event('Done');
    end
  else
    MFSM.Event('Cancel');
end;

procedure TMainForm.DoDispatcher;
begin
  MagicFormPanel1.Form := DispatcherForm;
end;

procedure TMainForm.DoExited;
begin
  Close;
end;

procedure TMainForm.DoExiting;
var
  I :INTEGER                  ;
begin
  MFSM.Event('Done');
end;

procedure TMainForm.DoExitRunning;
begin
  if AreYouSure('Exiting from Run Mode') = mrYes then
    MFSM.Event('Done')
  else
    MFSM.Event('Cancel');
end;

//==============================================================================
// In the OpeningFile state, the user is prompted for the name of an existing
// op session file, and opens it if one is selected.  If the user cancels the
// operation, it signals the Cancel event.
//==============================================================================

procedure TMainForm.DoOpeningFile;
begin
  if OpenDialog1.Execute then
    begin
      OpenDBFile( OpenDialog1.FileName );
      FileSystemMapAction.Enabled := True;
      FileTimetableAction.Enabled := True;
      MFSM.Event('Done');
    end
  else
    MFSM.Event('Cancel');
end;

//==============================================================================
// In the OpenLastFile state, the program opens the most-recently opened
// op session file.
//==============================================================================

procedure TMainForm.DoOpenLastFile;
var
  N : String;
begin
  N := GetConfig('File','LastFile',LastOpSession);
  if N <> LastOpSession then  // Belt and Suspenders
    begin
      OpenDBFile( N );
      FileSystemMapAction.Enabled := True;
      FileTimetableAction.Enabled := True;
    end;
  MFSM.Event('Done');
end;

procedure TMainForm.DoOpSession;
begin
  MagicFormPanel1.Form := OpSessionForm;
end;

procedure TMainForm.DoOrders;
begin
  MagicFormPanel1.Form := OrdersForm;
end;

procedure TMainForm.DoPreferences;
begin
  PreferencesForm.ShowModal;
  MFSM.Event('Done');
end;

procedure TMainForm.DoRailroad;
begin
  MagicFormPanel1.Form := RailroadForm;
end;

//==============================================================================
// In the Ready state, the program has an open op session file, and is ready to
// update the op session, save it with a different name, run the trains, etc.
//==============================================================================

procedure TMainForm.DoReady;
begin
  FileNewAction.Enabled              := False;
  FileOpenAction.Enabled             := False;
  FileRestoreAction.Enabled          := False;
  FileSaveAsAction.Enabled           := True;
  FileCloseAction.Enabled            := True;
  FilePrinterSetupAction.Enabled     := True;
  FileVoiceSetupAction.Enabled       := True;
  FileFastClockSetupAction.Enabled   := False;
  FilePreferencesAction.Enabled      := False;
  FileExitAction.Enabled             := True;
  RailRoadMenuItem.Enabled           := True;
  DispatcherMenuItem.Enabled         := False;
  FreightAgentMenuItem.Enabled       := False;
  SetupMenuItem.Enabled              := True;
  LastOpSessionMenuItem.Enabled      := False;
  SetupMenuItem.Enabled              := False;
  RunMenuItem.Enabled                := True;

  MagicFormPanel1.Form := RailroadForm;
end;

procedure TMainForm.DoRestoreFile;
begin
  if OpenDialog2.Execute then
    begin
      FileName := IronMikeData.Restore(Opendialog2.FileName);
      OpenDBFile( FileName );
      MFSM.Event('Done');
    end
  else
    MFSM.Event('Cancel');
end;

procedure TMainForm.DoResume;
begin
  MFSM.ForceState('Running'); // Resume is a no op
end;

procedure TMainForm.DoRunning;
var
  E : TEnvelopeType;
begin
  FileNewAction.Enabled              := False;
  FileOpenAction.Enabled             := False;
  FileRestoreAction.Enabled          := False;
  FileSaveAsAction.Enabled           := False;
  FileCloseAction.Enabled            := False;
  FilePrinterSetupAction.Enabled     := True;
  FileVoiceSetupAction.Enabled       := True;
  FileFastClockSetupAction.Enabled   := False;
  FilePreferencesAction.Enabled      := False;
  //FileExitAction.Enabled             := False;
  RailRoadMenuItem.Enabled           := False;
  DispatcherMenuItem.Enabled         := True;
  FreightAgentMenuItem.Enabled       := True;
  LastOpSessionMenuItem.Enabled      := False;
  SetupMenuItem.Enabled              := True;
  SetupMenuItem.Checked              := False;
  RunMenuItem.Enabled                := False;
  RunMenuItem.Checked                := True;

  InitEnvelopes;
  HideEnvelopes;

  MagicFormPanel1.Form := DispatcherForm;
end;

procedure TMainForm.DoSalinas;
begin
  MagicFormPanel1.Form := SalinasShedForm;
end;

//==============================================================================
// In the SaveAs state the user is prompted for a new file name.  If one is
// given, the currently open op session file is copied to that file.
//==============================================================================

procedure TMainForm.DoSaveAs;
begin
  if SaveDialog2.Execute then
    begin
      CursorStack.Push( crHourglass );
      try
        IronMikeData.SaveAs( SaveDialog2.FileName );
        FileName := SaveDialog2.FileName;
        //Connection.Close(True);
        //CopyFile( FileName, SaveDialog2.FileName );
        //FileName := SaveDialog2.FileName;
        //Connection.Open;
      finally
        CursorStack.Pop;
      end;
      MFSM.Event('Done');
    end
  else
    MFSM.Event('Cancel');
end;

procedure TMainForm.DoSetupClock;
begin
  FastClockWizard.ShowModal;
  MFSM.Event('Done');
end;

procedure TMainForm.DoSetupVoice;
begin
  VoicePreferencesForm.TestString('Crew Call for Train 2 oh 2');
  VoicePreferencesForm.ShowModal;
  RailroadAnnouncements.SetVoiceParams;
  MFSM.Event('Done');
end;

//==============================================================================
// In the Start state the program is ready to create or open files, set
// preferences, calibrate the clock, configure the voice, etc.
//==============================================================================

procedure TMainForm.DoStart;
begin
  FileNewAction.Enabled              := True;
  FileOpenAction.Enabled             := True;
  FileRestoreAction.Enabled          := True;
  FileSaveAsAction.Enabled           := False;
  FileCloseAction.Enabled            := False;
  FilePrinterSetupAction.Enabled     := True;
  FileVoiceSetupAction.Enabled       := True;
  FileFastClockSetupAction.Enabled   := True;
  FilePreferencesAction.Enabled      := True;
  FileExitAction.Enabled             := True;
  RailRoadMenuItem.Enabled           := False;
  DispatcherMenuItem.Enabled         := False;
  FreightAgentMenuItem.Enabled       := False;
  SetupMenuItem.Enabled              := False;
  RunMenuItem.Enabled                := False;
  LastOpSessionMenuItem.Enabled      := GetConfig('File','LastFile',LastOpSession) <> LastOpSession;
end;

procedure TMainForm.DoStations;
begin
  MagicFormPanel1.Form := StationsForm;
end;

procedure TMainForm.DoStopping;
begin
  Stub('DoStopping');
end;

procedure TMainForm.DoTimetable;
begin
  MagicFormPanel1.Form := TimetableForm;
end;

procedure TMainForm.DoTrains;
begin
  MagicFormPanel1.Form := TrainsForm;
end;

procedure TMainForm.DoWatsonville;
begin
  WatsonvilleForm.ShowRolls;
  MagicFormPanel1.Form := WatsonvilleForm;
end;

procedure TMainForm.EnvelopeTick(theTime: TDateTime);
var
  E : TEnvelopeType;
  T0, T1 : TDateTime;
begin
  for E := Low(TEnvelopeType) to High(TEnvelopeType) do
    begin
      if Envelopes[E].Status = esComplete then
        begin
          EnvelopeActive[E].Visible := False;
          EnvelopeWarning[E].Visible := False;
          EnvelopeButton[E].Visible := False;
        end
      else
        begin
          T0 := Envelopes[E].WarningTime;
          T1 := Envelopes[E].ActiveTime;
          if (not Envelopes[E].PrintedWarning) and (theTime >= Envelopes[E].WarningTime) then
            begin
              EnvelopeActive[E].Visible := False;
              EnvelopeWarning[E].Visible := True;
              EnvelopeButton[E].Visible := False;
              Envelopes[E].PrintedWarning := True;
              Windows.Beep(1760,90);
            end
          else if (not Envelopes[E].PrintedActive) and (theTime >= Envelopes[E].ActiveTime) then
            begin
              EnvelopeActive[E].Visible := True;
              EnvelopeWarning[E].Visible := False;
              EnvelopeButton[E].Visible := True;
              Envelopes[E].PrintedActive := True;
              Envelops;
            end;
        end;
    end;

end;

procedure TMainForm.FileCloseActionExecute(Sender: TObject);
begin
  MFSM.Event('FileClose');
end;

procedure TMainForm.FileExitActionExecute(Sender: TObject);
begin
  MFSM.Event('FileExit');
end;

procedure TMainForm.FileFastClockSetupActionExecute(Sender: TObject);
begin
  MFSM.Event('FileFastClock');
end;

procedure TMainForm.FileNewActionExecute(Sender: TObject);
begin
  MFSM.Event('FileNew');
end;

procedure TMainForm.FileOpenActionExecute(Sender: TObject);
begin
  MFSM.Event('FileOpen');
end;

procedure TMainForm.FilePreferencesActionExecute(Sender: TObject);
begin
  MFSM.Event('FilePreferences'); // Stub('FilePreferencesActionExecute');
end;

procedure TMainForm.FilePrinterSetupActionExecute(Sender: TObject);
begin
  PrintDialog1.Execute;
  { TODO -odonz -cFunctionality : Call "preferences changed" on all ironmike forms }
end;

procedure TMainForm.FileRestoreActionExecute(Sender: TObject);
begin
  MFSM.Event('FileRestore');
end;

procedure TMainForm.FileSaveAsActionExecute(Sender: TObject);
begin
  MFSM.Event('FileSaveAs');
end;

procedure TMainForm.FileSystemMapActionExecute(Sender: TObject);
begin
  ;
end;

procedure TMainForm.FileTimetableActionExecute(Sender: TObject);
begin
  ShowTimetableForm.Show;
  //TimetableDisplayForm.Show;
end;

procedure TMainForm.FileVoiceSetupActionExecute(Sender: TObject);
begin
  MFSM.Event('FileVoiceSetup');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Randomize; // Different strokes
  SaveDialog1.InitialDir := DefaultSaveLocation;
  SaveDialog2.InitialDir := DefaultSaveLocation;
  OpenDialog1.InitialDir := DefaultSaveLocation;
  OpenDialog2.InitialDir := DefaultSaveLocation;
  Top := 3;
  CursorStack := TAppCursorStack.Create;
  CursorStack.Push( crDefault );
  LastOpSessionMenuItem.Caption := ExtractFileName(GetConfig('File','LastFile',LastOpSession));
  Timer1.Enabled := True;  // Show the Splash form
  EnvelopeActive[etEnvelopeA] := LabelEnvelopeAReady;
  EnvelopeActive[etEnvelopeB] := LabelEnvelopeBReady;
  EnvelopeActive[etSLOFreight] := LabelSLOFreightEnvReady;
  EnvelopeWarning[etEnvelopeA] := LabelEnvelopeAWarning;
  EnvelopeWarning[etEnvelopeB] := LabelEnvelopeBWarning;
  EnvelopeWarning[etSLOFreight] := LabelSLOFreightEnvWarning;
  EnvelopeButton[etEnvelopeA]   := ButtonEnvelopeAPickup;
  EnvelopeButton[etEnvelopeB]   := ButtonEnvelopeBPickup;
  EnvelopeButton[etSLOFreight]  := ButtonSLOFreightEnvPickup;
  HideEnvelopes;
  MFSM.Event('Start');
end;

procedure TMainForm.FreightAgentOrderActionExecute(Sender: TObject);
begin
  MFSM.Event('FreightAgentOrder');
end;

procedure TMainForm.FreightAgentSalinasActionExecute(Sender: TObject);
begin
  MFSM.Event('FreightAgentSalinas');
end;

procedure TMainForm.FreightAgentWatsonvilleActionExecute(Sender: TObject);
begin
  MFSM.Event('FreightAgentWatsonville');
end;

function TMainForm.GetFileName: String;
begin
  Result := IronMikeData.FileName;
end;

function TMainForm.GetLastFileName: String;
begin
  Result := GetConfig('File','LastFile',LastOpSession);
  LastOpSessionMenuItem.Caption := ExtractFileName(Result);
  LastOpSessionMenuItem.Enabled := Result <> LastOpSession;
end;

procedure TMainForm.GoToWatsonville;
begin
  MagicFormPanel1.Form := WatsonvilleForm;
  WatsonvilleForm.ShowEnvelopes;
end;

procedure TMainForm.HelpAboutActionExecute(Sender: TObject);
begin
  AboutForm.ShowModal;
end;

procedure TMainForm.HideEnvelopes;
var
  E : TEnvelopeType;
begin
  for E := Low(TEnvelopeType) to High(TEnvelopeType) do
    begin
      EnvelopeActive[E].Visible := False;
      EnvelopeWarning[E].Visible := False;
      EnvelopeButton[E].Visible := False;
    end ;
end;

procedure TMainForm.LastOpSessionMenuItemClick(Sender: TObject);
begin
  MFSM.Event('LastFile');
end;

procedure TMainForm.MFSMEnterState(Sender: TComponent; AState: String);
begin
  if      AState = 'START'        then DoStart
  else if AState = 'OPENLASTFILE' then DoOpenLastFile
  else if AState = 'OPENINGFILE'  then DoOpeningFile
  else if AState = 'CREATINGFILE' then DoCreatingFile
  else if AState = 'RESTOREFILE'  then DoRestoreFile
  else if AState = 'CLOSING'      then DoClosing
  else if AState = 'READY'        then DoReady
  else if AState = 'SAVEAS'       then DoSaveAs
  else if AState = 'RUNNING'      then DoRunning
  else if AState = 'DISPATCHER'   then DoDispatcher
  else if AState = 'WATSONVILLE'  then DoWatsonville
  else if AState = 'SALINAS'      then DoSalinas
  else if AState = 'ORDERS'       then DoOrders
  else if AState = 'RAILROAD'     then DoRailroad
  else if AState = 'TRAINS'       then DoTrains
  else if AState = 'TIMETABLE'    then DoTimetable
  else if AState = 'STATIONS'     then DoStations
  else if AState = 'OPSESSION'    then DoOpSession
  else if AState = 'SETUPVOICE'   then DoSetupVoice
  else if AState = 'SETUPCLOCK'   then DoSetupClock
  else if AState = 'PREFERENCES'  then DoPreferences
  else if AState = 'EXITING'      then DoExiting
  else if AState = 'EXITRUNNING'  then DoExitRunning
  else if AState = 'RESUME'       then DoResume
  else if AState = 'DOEXIT'       then DoExited
  else
    Stub('State:  [' + aState + ']');
end;

procedure TMainForm.OpenDBFile(Path: String);
begin
  CursorStack.Push( crHourglass );
  try
    IronMikeData.FileName := Path; // Sets connection databaseName
    FileName := Path;
    DispatcherForm.Initialize;
  finally
    CursorStack.Pop;
  end;
end;

procedure TMainForm.RailroadGeneralActionExecute(Sender: TObject);
begin
  MFSM.Event('RailroadGeneral');
end;

procedure TMainForm.RailroadOpSessionActionExecute(Sender: TObject);
begin
  MFSM.Event('OpSession');
end;

procedure TMainForm.RailroadStationsActionExecute(Sender: TObject);
begin
  MFSM.Event('RailroadStations');
end;

procedure TMainForm.RailroadTimetablesActionExecute(Sender: TObject);
begin
  MFSM.Event('RRTimetable');
end;

procedure TMainForm.RailroadTrainsActionExecute(Sender: TObject);
begin
  MFSM.Event('RailroadTrains');
end;

procedure TMainForm.MagicClock1HeartBeat(Sender: TObject; out Time: TDateTime);
begin
  DispatcherForm.Tick := Time;
  SalinasShedForm.Tick := Time;
  EnvelopeTick( Time );
end;

procedure TMainForm.RRTimetableActionExecute(Sender: TObject);
begin
  MFSM.Event('RRTimetable');
end;

procedure TMainForm.RunMenuItemClick(Sender: TObject);
begin
  RunMenuItem.Checked := True;
  SetupMenuItem.Checked := False;
  MFSM.Event('RunStart');
end;

procedure TMainForm.SetFileName(AValue: String);
begin
  StatusBar1.Panels[1].Text := AValue;
  IronMikeData.FileName := AValue;
  LastFileName := AValue;
end;

procedure TMainForm.SetLastFileName(AValue: String);
begin
  if Empty( AValue ) then exit;
  SetConfig('File','LastFile',AValue);
  LastOpSessionMenuItem.Caption := ExtractFileName(AValue);
  LastOpSessionMenuItem.Enabled := AValue <> LastOpSession;
end;

procedure TMainForm.SetupMenuItemClick(Sender: TObject);
begin
  SetupMenuItem.Checked := True;
  RunMenuItem.Checked := False;
  MFSM.Event('Setup');
end;

procedure TMainForm.SystemMapMenuItemClick(Sender: TObject);
begin
  MagicFormPanel1.FOrm := SystemMapForm;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  H1, H2 : Integer;
begin
  Timer1.Enabled := False;
  if not Assigned( MagicFormPanel1.Form ) then
    begin
      H1 := MagicFormPanel1.Height;         // For Debug - Strange SplashForm too high error
      H2 := SplashForm.Height;
      MagicFormPanel1.Form := SplashForm;
    end;
end;

end.

