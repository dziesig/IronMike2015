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

program IronMike2015c;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, printer4lazarus, MainFormUnit, aboutironmike, preferences,
  fastclockwizardform
  { you can add units after this },
  VoiceForm, RailroadFormUnit, SplashFormUnit, IronMikeDataUnit,
  DispatcherFormUnit, WatsonvilleFormUnit, systemmapformUnit,
  SalinasShedFormUnit, OrdersFormUnit, StationsFormUnit, TrainsFormUnit,
  TimetableFormUnit, OpSessionFormUnit, RailroadAnnouncements,
  RegisterTrainFormUnit, OrdersUnit, DisplayUnit, RRSubsUnit, ConstantsUnit,
  ScheduleUnit, TrainsUnit, SelectWhereFormUnit,
  DelayTrainFormUnit, SecondSectionFormUnit, AddTrainFormUnit,
  TrainInfoFormUnit, DeltaTimeFormUnit, StationsUnit, ShowTimetableFormUnit,
  TimetableDisplayFormUnit, SyncClockFormUnit,
MeetVisualizationFormUnit, EnvelopeUnit;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TVoicePreferencesForm, VoicePreferencesForm);
  Application.CreateForm(TFastClockWizard, FastClockWizard);
  Application.CreateForm(TPreferencesForm, PreferencesForm);
  Application.CreateForm(TIronMikeData, IronMikeData);
  Application.CreateForm(TRailroadForm, RailroadForm);
  Application.CreateForm(TSplashForm, SplashForm);
  Application.CreateForm(TDispatcherForm, DispatcherForm);
  Application.CreateForm(TWatsonvilleForm, WatsonvilleForm);
  Application.CreateForm(TSystemMapForm, SystemMapForm);
  Application.CreateForm(TSalinasShedForm, SalinasShedForm);
  Application.CreateForm(TOrdersForm, OrdersForm);
  Application.CreateForm(TStationsForm, StationsForm);
  Application.CreateForm(TTrainsForm, TrainsForm);
  Application.CreateForm(TTimetableForm, TimetableForm);
  Application.CreateForm(TOpSessionForm, OpSessionForm);
  Application.CreateForm(TRegisterTrainForm, RegisterTrainForm);
  Application.CreateForm(TSelectWhereForm, SelectWhereForm);
  Application.CreateForm(TDelayTrainForm, DelayTrainForm);
  Application.CreateForm(TSecondSectionForm, SecondSectionForm);
  Application.CreateForm(TAddTrainForm, AddTrainForm);
  Application.CreateForm(TTrainInfoForm, TrainInfoForm);
  Application.CreateForm(TDeltaTimeForm, DeltaTimeForm);
  Application.CreateForm(TShowTimetableForm, ShowTimetableForm);
  Application.CreateForm(TTimetableDisplayForm, TimetableDisplayForm);
  Application.CreateForm(TSyncClockForm, SyncClockForm);
  Application.CreateForm(TMeetVisualizationForm, MeetVisualizationForm);
  Application.Run;
end.

