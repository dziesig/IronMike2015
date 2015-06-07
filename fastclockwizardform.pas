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

unit fastclockwizardform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, ComCtrls, Calendar, MagicTimeEdit, MagicClock;

type

  { TFastClockWizard }

  TFastClockWizard = class(TForm)
    BitBtn1: TBitBtn;
    BackButton: TButton;
    Calendar1: TCalendar;
    CalibrateButton: TButton;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    MagicClock1: TMagicClock;
    MagicClock2: TMagicClock;
    NextButton: TButton;
    AddMultiplierButton: TButton;
    RadioGroup1: TRadioGroup;
    MagicClockStartEdit: TMagicTimeEdit;
    MagicClockStopEdit: TMagicTimeEdit;
    StartStopButton: TButton;
    DeleteMultiplierButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MultiplierListBox: TListBox;
    MultiplierEdit: TLabeledEdit;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    procedure AddMultiplierButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure CalibrateButtonClick(Sender: TObject);
    procedure MagicClockStopEditExit(Sender: TObject);
    procedure StartStopButtonClick(Sender: TObject);
    procedure Calendar1Change(Sender: TObject);
    procedure NextButtonClick(Sender: TObject);
    procedure DeleteMultiplierButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MultiplierListBoxClick(Sender: TObject);
    procedure MultiplierEditChange(Sender: TObject);
    procedure MultiplierEditKeyPress(Sender: TObject; var Key: char);
    procedure PageControl1Change(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure MagicClockStartEditTimeChange(Sender: TObject);
    procedure TabSheet1Exit(Sender: TObject);
    procedure TabSheet2Show(Sender: TObject);
    procedure TabSheet3Show(Sender: TObject);
  private
    { private declarations }
    vStartTime : TDateTime;
    vSpeedVernir : Double;
    procedure EnableAddMultiplierButton;
    procedure SaveMultipliers;
    procedure LoadMultipliers;

  public
    { public declarations }
  end;

var
  FastClockWizard: TFastClockWizard;

implementation

{$R *.lfm}

uses
  StringSubs, CommonIni;

function ListBox1Compare( LB : TStringList; Idx1, Idx2 : Integer ) : Integer;
var
  V1, V2 : Integer;
begin
  V1 := StrToInt( LB[Idx1] );
  V2 := StrToInt( LB[Idx2] );
  Result := V1 - V2;
end;

{ TFastClockWizard }

procedure TFastClockWizard.AddMultiplierButtonClick(Sender: TObject);
var
  L : TStringList;
  V : Single;
begin
  V := 0.0;
  MultiplierListBox.Items.AddObject( MultiplierEdit.Text, TObject(Pointer(V)) );
  AddMultiplierButton.Enabled := MultiplierListBox.Items.IndexOf( MultiplierEdit.Text ) < 0;
  L := TStringList.Create;
  L.AddStrings( MultiplierListBox.Items );
  L.CustomSort( @ListBox1Compare );
  MultiplierListBox.Clear;
  MultiplierListBox.Items.AddStrings( L );
  L.Free;
  SaveMultipliers;
  MultiplierEdit.Text := '';
  MultiplierEdit.SetFocus;
end;

procedure TFastClockWizard.BackButtonClick(Sender: TObject);
begin
  PageControl1.TabIndex := PageControl1.TabIndex - 1;
end;

procedure TFastClockWizard.Calendar1Change(Sender: TObject);
begin
  MagicClockStartEdit.DateTime := Calendar1.DateTime;
  MagicClockStopEdit.DateTime := Calendar1.DateTime;
  MagicClock1.DateTime := Calendar1.DateTime;
end;

procedure TFastClockWizard.CalibrateButtonClick(Sender: TObject);
begin
  MagicClock1.SpeedVernir := MagicClock1.SpeedVernir  - vSpeedVernir * 0.5;
  with MultiplierListBox do
    Items.Objects[ItemIndex] := TObject( Single(MagicClock1.SpeedVernir) );
  SaveMultipliers;
end;

procedure TFastClockWizard.DeleteMultiplierButtonClick(Sender: TObject);
begin
  MultiplierListBox.Items.Delete( MultiplierListBox.ItemIndex );
  SaveMultipliers;
  MultiplierEdit.SetFocus;
end;

procedure TFastClockWizard.EnableAddMultiplierButton;
var
  Mult  : String;
begin
Mult := MultiplierEdit.Text;
  AddMultiplierButton.Enabled := (not Empty(Mult)) and (MultiplierListBox.Items.IndexOf( Mult ) < 0);
end;

procedure TFastClockWizard.FormCreate(Sender: TObject);
begin
  PageControl1.TabIndex := 0;
  LoadMultipliers;
end;

procedure TFastClockWizard.FormShow(Sender: TObject);
begin
  LoadMultipliers;
  NextButton.Enabled := MultiplierListBox.ItemIndex >= 0;
  Calendar1.DateTime := Now;
  MagicClockStartEdit.DateTime := 0.0;
  PageControl1.TabIndex := 0;
end;

procedure TFastClockWizard.LoadMultipliers;
var
  Count : Integer;
  I     : Integer;
begin
  MultiplierListBox.Clear;
  Count := GetConfig('Multipliers','Count',0);
  for I := 0 to pred(Count) do
    MultiplierListBox.Items.AddObject( GetConfig('Multipliers','Multiplier_' + IntToStr( I ), 'ERR'),
                                       TObject(Single(GetConfig('Multipliers','Vernir_' + IntToStr( I ), Double(0.0) )) ) );
  EnableAddMultiplierButton
end;

procedure TFastClockWizard.MultiplierEditChange(Sender: TObject);
var
  Value : Integer;
  Mult  : String;
begin
  Mult := MultiplierEdit.Text;
  Value := StringToInt( Mult );
  if Value = 0 then
    MultiplierEdit.Color := clSilver
  else if Value > 60000 then
    MultiplierEdit.Color := clRed
  else if Value > 60 then
    MultiplierEdit.Color := clYellow
  else
    MultiplierEdit.Color := clWhite;
  EnableAddMultiplierButton
end;

procedure TFastClockWizard.MultiplierEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if not (Key in ['0'..'9',#8]) then
    Key := #0;
end;

procedure TFastClockWizard.MultiplierListBoxClick(Sender: TObject);
begin
  DeleteMultiplierButton.Enabled := MultiplierListBox.ItemIndex >= 0;
  NextButton.Enabled := MultiplierListBox.ItemIndex >= 0;
end;

procedure TFastClockWizard.NextButtonClick(Sender: TObject);
begin
  PageControl1.TabIndex := PageControl1.TabIndex + 1;
end;

procedure TFastClockWizard.PageControl1Change(Sender: TObject);
var
  Index : Integer;
begin
  Index := PageControl1.TabIndex;
  BackButton.Enabled := Index > 0;
  NextButton.Enabled := (Index < 2) and (MultiplierListBox.ItemIndex >= 0) and
    ( Index <> 1 );
end;

procedure TFastClockWizard.RadioGroup1Click(Sender: TObject);
begin
  MagicClockStartEdit.AMPM := RadioGroup1.ItemIndex = 1;
  MagicClockStopEdit.AMPM := RadioGroup1.ItemIndex = 1;
  MagicClock1.AMPM := RadioGroup1.ItemIndex = 1;
end;

procedure TFastClockWizard.MagicClockStartEditTimeChange(Sender: TObject);
var
  DT : TDateTime; // For Debugging
begin
  DT := MagicClockStartEdit.DateTime;
  MagicClockStopEdit.DateTime := DT;
  MagicClock1.DateTime := DT;
end;

procedure TFastClockWizard.MagicClockStopEditExit(Sender: TObject);
begin
  with MultiplierListBox do
    MagicClock1.SpeedVernir := Single( Items.Objects[ ItemIndex ] );
end;

procedure TFastClockWizard.SaveMultipliers;
var
  OldCount : Integer;
  I        : Integer;
begin
  OldCount := GetConfig('Multipliers','Count',0);
  for I := 0 to pred(OldCount) do
    begin
      DelConfig('Multipliers','Multiplier_' + IntToStr( I ));
      DelConfig('Multipliers','Vernir_' + IntToStr( I ));
    end;
  SetConfig('Multipliers','Count',MultiplierListBox.Count);
  for I := 0 to pred(MultiplierListBox.Count) do
    begin
      SetConfig('Multipliers','Multiplier_' + IntToStr( I ), MultiplierListBox.Items[I] );
      SetConfig('Multipliers','Vernir_' + IntToStr( I ), Double(Single(MultiplierListBox.Items.Objects[I])) );
    end;
end;

procedure TFastClockWizard.StartStopButtonClick(Sender: TObject);
begin
  with StartStopButton do
    begin
      if Caption = 'Start' then
        begin
          Caption := 'Stop';
          vStartTime := MagicClock1.DateTime;
          MagicClock1.Running := True;
          NextButton.Enabled := False;
          BackButton.Enabled := False;
        end
      else
        begin
          MagicClock1.Running := False;
          Caption := 'Start';
          NextButton.Enabled := True;
          BackButton.Enabled := True;
          MagicClock2.DateTime := MagicClock1.DateTime;
          MagicClock2.AMPM     := MagicClock1.AMPM;
        end;
    end;
end;

procedure TFastClockWizard.TabSheet1Exit(Sender: TObject);
begin
  with MultiplierListBox do
    begin
      if ItemIndex < 0 then exit;
      MagicClock1.SpeedVernir := Single( Items.Objects[ ItemIndex ] );
    end;
end;

procedure TFastClockWizard.TabSheet2Show(Sender: TObject);
var
  Idx : Integer;
  Mult : Integer;
begin
  NextButton.Enabled := False;
  MagicClockStartEdit.DateTime := Calendar1.DateTime;
  MagicClockStopEdit.DateTime := Calendar1.DateTime;
  MagicClock1.DateTime := Calendar1.DateTime;
  Idx := MultiplierListBox.ItemIndex;
  if Idx < 0 then
    Mult := 1  // Just in case
  else
    Mult := StrToInt( MultiplierListBox.Items[Idx] );
  MagicClock1.SpeedMultiplier := Mult;
end;

procedure TFastClockWizard.TabSheet3Show(Sender: TObject);
var
  FastClockDeltaT : TDateTime;
  IronMikeDeltaT  : TDateTime;
begin
  NextButton.Enabled := False;
  FastClockDeltaT := MagicClockStopEdit.DateTime - MagicClockStartEdit.DateTime;
  IronMikeDeltaT  := MagicClock1.DateTime - MagicClockStartEdit.DateTime;
  if FastClockDeltaT = 0 then
    vSpeedVernir := 0
  else
    vSpeedVernir := IronMikeDeltaT/FastClockDeltaT - 1.0;
  Label10.Caption := FloatToPercent( vSpeedVernir );
end;


end.

