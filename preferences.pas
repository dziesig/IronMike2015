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

unit preferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Calendar, Buttons, FontPicker;

type

  { TPreferencesForm }

  TPreferencesForm = class(TForm)
    BitBtn2: TBitBtn;
    Calendar1: TCalendar;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ClockStyleRG: TRadioGroup;
    RadioGroup1: TRadioGroup;
    procedure Calendar1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure CheckBox3Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
    procedure ClockStyleRGClick(Sender: TObject);
    procedure ComboBox1CloseUp(Sender: TObject);
    procedure ComboBox2CloseUp(Sender: TObject);
    procedure ComboBox3CloseUp(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    function GetAnnounceCrewCall: Boolean;
    function GetAnnounceEnvelopes: Boolean;
    function GetAnnounceOrders: Boolean;
    function GetDefaultAMPM: Boolean;
    function GetDefaultStartDate: TDateTime;
    function GetEarliestYear: Word;
    function GetLatestYear: Word;
    function GetOrdersFontName: String;
    { private declarations }
    function ThisYear : Word;
  public
    { public declarations }
    property DefaultStartDate  : TDateTime read GetDefaultStartDate;
    property DefaultAMPM       : Boolean read GetDefaultAMPM;
    property OrdersFontName    : String read GetOrdersFontName;
    property EarliestYear      : Word read GetEarliestYear;
    property LatestYear        : Word read GetLatestYear;
    property AnnounceCrewCall  : Boolean read GetAnnounceCrewCall;
    property AnnounceOrders    : Boolean read GetAnnounceOrders;
    property AnnounceEnvelopes : Boolean read GetAnnounceEnvelopes;
  end;


var
  PreferencesForm: TPreferencesForm;

implementation

uses
  CommonIni;

const
  TheEarliestYear = 1840;

{$R *.lfm}

{ TPreferencesForm }
procedure TPreferencesForm.Calendar1Click(Sender: TObject);
begin
  SetConfig( 'Preferences','StartDate', Double(Calendar1.DateTime) );
end;

procedure TPreferencesForm.CheckBox1Click(Sender: TObject);
begin
  SetConfig( 'Preferences', 'AlwaysUseThisYear', CheckBox1.Checked );
end;

procedure TPreferencesForm.CheckBox2Click(Sender: TObject);
begin
  SetConfig( 'Preferences', 'CrewCall', CheckBox2.Checked );
end;

procedure TPreferencesForm.CheckBox3Click(Sender: TObject);
begin
  SetConfig( 'Preferences', 'Orders', CheckBox3.Checked );
end;

procedure TPreferencesForm.CheckBox4Click(Sender: TObject);
begin
  SetConfig( 'Preferences', 'Envelopes', CheckBox4.Checked );
end;

procedure TPreferencesForm.ClockStyleRGClick(Sender: TObject);
begin
  SetConfig( 'Preferences', 'AMPM', ClockStyleRG.ItemIndex = 1 );
end;

procedure TPreferencesForm.ComboBox1CloseUp(Sender: TObject);
begin
  SetConfig( 'Preferences', 'EarliestYear', ComboBox1.ItemIndex + TheEarliestYear );
end;

procedure TPreferencesForm.ComboBox2CloseUp(Sender: TObject);
begin
  SetConfig( 'Preferences', 'LatestYear', ComboBox2.ItemIndex + TheEarliestYear );
end;

procedure TPreferencesForm.ComboBox3CloseUp(Sender: TObject);
begin
  Calendar1.Date := '01/01/' + ComboBox3.Items[ComboBox3.ItemIndex];
end;

procedure TPreferencesForm.FormCreate(Sender: TObject);
var
  I : Integer;
  S       : String;
begin
  ComboBox1.Clear;
  ComboBox2.Clear;
  for I := TheEarliestYear to ThisYear do
    begin
      S := IntToStr(I);
      ComboBox1.Items.Add( S );
      ComboBox2.Items.Add( S );
    end;
  ComboBox1.ItemIndex := 0; // Earliest Year
  ComboBox2.ItemIndex := pred(ComboBox2.Items.Count);
end;

procedure TPreferencesForm.FormShow(Sender: TObject);
var
  Idx : Integer;
begin
  ClockStyleRG.ItemIndex := ord( DefaultAMPM );
  Calendar1.DateTime     := DefaultStartDate;
  RadioGroup1.ItemIndex  := RadioGroup1.Items.IndexOf( OrdersFontName );;
  CheckBox1.Checked      := GetConfig( 'Preferences','AlwaysUseThisYear',False);
  ComboBox1.ItemIndex    := EarliestYear - TheEarliestYear;
  Idx := LatestYear;
  Idx := Idx - TheEarliestYear;
  ComboBox2.ItemIndex    := Idx;
  CheckBox2.Checked := AnnounceCrewCall;
  CheckBox3.Checked := AnnounceOrders;
  CheckBox4.Checked := AnnounceEnvelopes;
end;

function TPreferencesForm.GetAnnounceCrewCall: Boolean;
begin
  Result := GetConfig( 'Preferences', 'CrewCall', True);
end;

function TPreferencesForm.GetAnnounceEnvelopes: Boolean;
begin
  Result := GetConfig( 'Preferences', 'Envelopes', True);
end;

function TPreferencesForm.GetAnnounceOrders: Boolean;
begin
  Result := GetConfig( 'Preferences', 'Orders', True);
end;

function TPreferencesForm.GetDefaultAMPM: Boolean;
begin
  Result := GetConfig( 'Preferences', 'AMPM', False );
end;

function TPreferencesForm.GetDefaultStartDate: TDateTime;
begin
  Result := TDateTime(GetConfig( 'Preferences', 'StartDate', Double( 0.0 ) ) );
end;

function TPreferencesForm.GetEarliestYear: Word;
begin
  Result := GetConfig( 'Preferences','EarliestYear',TheEarliestYear);
end;

function TPreferencesForm.GetLatestYear: Word;
begin
  if CheckBox1.Checked then
    Result := ThisYear
  else
    Result := GetConfig( 'Preferences','LatestYear', ThisYear );
end;

function TPreferencesForm.GetOrdersFontName: String;
begin
  Result := GetConfig( 'Preferences', 'OrdersFont', 'Arial' );
end;

procedure TPreferencesForm.RadioGroup1Click(Sender: TObject);
var
  Index : Integer;
begin
  Index := RadioGroup1.ItemIndex;
  SetConfig( 'Preferences', 'OrdersFont', RadioGroup1.Items[Index] );
end;

function TPreferencesForm.ThisYear: Word;
var
  Y, M, D : Word;
begin
  DecodeDate( Now, Y, M, D );
  Result := Y;
end;

end.

