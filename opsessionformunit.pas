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

unit OpSessionFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Calendar, FormPanel, MagicTimeEdit, IronMikeDataUnit;

type

  { TOpSessionForm }

  TOpSessionForm = class(TPanelForm)
    Calendar1: TCalendar;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    CrewCallLeadTimeEdit: TMagicTimeEdit;
    GroupBox1: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    StartTimeEdit: TMagicTimeEdit;
    MultiplierListBox: TListBox;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    YearComboBox: TComboBox;
    procedure Calendar1Change(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure CrewCallLeadTimeEditTimeChange(Sender: TObject);
    procedure MultiplierListBoxClick(Sender: TObject);
    procedure StartTimeEditTimeChange(Sender: TObject);
    procedure YearComboBoxCloseUp(Sender: TObject);
  private
    { private declarations }
    OpSessionData : TOpSessionDataRec;
    RailroadData  : TRailroadDataRec;
    procedure ControlsLoad;
  public
    { public declarations }
    procedure Hiding; override;
    procedure Showing; override;
    procedure PreferencesChanged; override;
  end;

var
  OpSessionForm: TOpSessionForm;

implementation

uses
  CommonDebug, CommonIni,
  Preferences;

{$R *.lfm}

{ TOpSessionForm }

procedure TOpSessionForm.Calendar1Change(Sender: TObject);
begin
  OpSessionData.StartDate := Calendar1.DateTime;
  IronMikeData.OpSession := OpSessionData;
end;

procedure TOpSessionForm.CheckBox1Change(Sender: TObject);
begin
  OpSessionData.TodaysDate := CheckBox1.Checked;
  IronMikeData.OpSession := OpSessionData;
end;

procedure TOpSessionForm.ComboBox1CloseUp(Sender: TObject);
var
  Idx : Integer;
  Y, M, D : Word;
  NewDate : TDateTime;
begin
  Idx := ComboBox1.ItemIndex;
  Y := StrToInt( ComboBox1.Items[Idx] );
  M := 1;
  D := 1;
  NewDate :=  EncodeDate(Y,M,D);
  //Calendar1.DateTime := NewDate;
  OpSessionData.StartDate := NewDate;
  IronMikeData.OpSession := OpSessionData;
  ControlsLoad;
end;

procedure TOpSessionForm.ControlsLoad;
var
  Idx : Integer;
  procedure LoadMultipliers;
  var
    I : Integer;
    C : Integer;
  begin
    MultiplierListBox.Clear;
    C := GetConfig('Multipliers','Count',0);
    for I := 0 to pred(C) do
      MultiplierListBox.Items.AddObject( GetConfig('Multipliers','Multiplier_' + IntToStr( I ), 'ERR'),
                                         TObject(Single(GetConfig('Multipliers','Vernir_' + IntToStr( I ), Double(0.0) )) ) );
  end;

begin
  LoadMultipliers;
  with IronMikeData do
    begin
      RailroadData := Railroad;
      OpsessionData := OpSession;

      Idx := MultiplierListBox.Items.IndexOf(IntToStr(OpSessionData.FastClock));
      MultiplierListBox.ItemIndex := Idx;

      YearComboBox.Clear;
      for Idx := PreferencesForm.EarliestYear to PreferencesForm.LatestYear do
        YearComboBox.AddItem( IntToStr( Idx ), nil );
      Idx := YearComboBox.Items.IndexOf( IntToStr(OpSessionData.UseYear) );
      YearComboBox.ItemIndex := Idx;

      StartTimeEdit.AMPM := RailroadData.ClockAMPM;
      StartTimeEdit.DateTime := OpSessionData.StartTime;
      CrewCallLeadTimeEdit.DateTime := OpSessionData.CrewCallLead;
      Application.ProcessMessages;
      Calendar1.DateTime := OpSessionData.StartDate;
      Application.ProcessMessages;

    end;
end;

procedure TOpSessionForm.CrewCallLeadTimeEditTimeChange(Sender: TObject);
begin
  OpSessionData.CrewCallLead := CrewCallLeadTimeEdit.DateTime;
  IronMikeData.OpSession := OpSessionData;
end;

procedure TOpSessionForm.Hiding;
begin

end;

procedure TOpSessionForm.MultiplierListBoxClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := MultiplierListBox.ItemIndex;
  OpSessionData.FastClock := StrToInt( MultiplierListBox.Items[Idx] );
  IronMikeData.OpSession := OpSessionData;
end;

procedure TOpSessionForm.PreferencesChanged;
begin
  inherited PreferencesChanged;
end;

procedure TOpSessionForm.Showing;
begin
  ControlsLoad;
end;

procedure TOpSessionForm.StartTimeEditTimeChange(Sender: TObject);
begin
  OpSessionData.StartTime := StartTimeEdit.DateTime;
  IronMikeData.OpSession := OpSessionData;
end;

procedure TOpSessionForm.YearComboBoxCloseUp(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := YearComboBox.ItemIndex;
  OpSessionData.UseYear := StrToInt( YearComboBox.Items[Idx] );
  IronMikeData.OpSession := OpSessionData;
end;

end.

