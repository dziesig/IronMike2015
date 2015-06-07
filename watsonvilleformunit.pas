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

unit WatsonvilleFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, FormPanel;

type

  { TWatsonvilleForm }

  TWatsonvilleForm = class(TPanelForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    GroupBox1: TGroupBox;
    Memo1: TMemo;
    Memo2: TMemo;
    Memo3: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    CarsLocal, CarsSF, CarsOakland : Integer;
    BaseText : String;
    EnvText  : String;
  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;

    procedure ShowEnvelopes;
    procedure ShowRolls;
  end;

var
  WatsonvilleForm: TWatsonvilleForm;

implementation

uses
  Stringsubs,
  CommonDebug,
  Math,
  Printers,
  MainFormUnit,
  EnvelopeUnit,
  IronMIkeDataUnit;

{$R *.lfm}

{ TWatsonvilleForm }

procedure TWatsonvilleForm.BitBtn1Click(Sender: TObject);
begin
  CarsLocal   := RandomRange(2,4);
  CarsSF      := RandomRange(4,6);
  CarsOakland := RandomRange(4,6);
  ShowRolls;
end;

procedure TWatsonvilleForm.BitBtn2Click(Sender: TObject);
begin
  Stub1('Print Watsonville Consists');
  with Printer do
    begin
      Orientation := poLandscape;
      BeginDoc;
      GroupBox1.PaintTo(Canvas,0,0); // needs to print to larger area after I get ink.
      EndDoc;
    end;
end;

procedure TWatsonvilleForm.BitBtn3Click(Sender: TObject);
begin
  MainForm.BackToDispatcher;
end;

procedure TWatsonvilleForm.FormCreate(Sender: TObject);
begin
  CarsLocal   := 0;
  CarsSF      := 0;
  CarsOakland := 0;
  BaseText := Memo1.Lines.Text;
  EnvText  := Memo3.Lines.Text;
end;

procedure TWatsonvilleForm.Hiding;
begin

end;

procedure TWatsonvilleForm.ShowEnvelopes;
var
  Lines : String;
begin
  Lines := EnvText;
  Lines := StringReplace( Lines, '#AW',
                          IronMikeData.RRTime( Envelopes[etEnvelopeA].WarningTime),
                          [rfReplaceAll] );
  Lines := StringReplace( Lines, '#AA',
                          IronMikeData.RRTime( Envelopes[etEnvelopeA].ActiveTime),
                          [rfReplaceAll] );
  Lines := StringReplace( Lines, '#BW',
                          IronMikeData.RRTime( Envelopes[etEnvelopeB].WarningTime),
                          [rfReplaceAll] );
  Lines := StringReplace( Lines, '#BA',
                          IronMikeData.RRTime( Envelopes[etEnvelopeB].ActiveTime),
                          [rfReplaceAll] );
  Lines := StringReplace( Lines, '#SW',
                          IronMikeData.RRTime( Envelopes[etSLOFreight].WarningTime),
                          [rfReplaceAll] );
  Lines := StringReplace( Lines, '#SA',
                          IronMikeData.RRTime( Envelopes[etSLOFreight].ActiveTime),
                          [rfReplaceAll] );
  Memo3.Text := Lines;
  Memo3.Visible := True;
  Memo1.Visible := False;
end;

procedure TWatsonvilleForm.Showing;
begin
  ShowRolls;
end;

procedure TWatsonvilleForm.ShowRolls;
var
  Lines : String;
  Str   : String;
begin
  Lines := BaseText;
  if CarsLocal = 0 then
    Str := 'Not Set'
  else
    Str := IntToStr( CarsLocal );

  Lines := StringReplace(Lines,'#1',Str,  [rfReplaceAll]);

  if CarsSF = 0 then
    Str := 'Not Set'
  else
    Str := IntToStr( CarsSF );

  Lines := StringReplace(Lines,'#2',Str,  [rfReplaceAll]);

  if CarsOakland = 0 then
    Str := 'Not Set'
  else
    Str := IntToStr( CarsOakland );

  Lines := StringReplace(Lines,'#3',Str,  [rfReplaceAll]);

  Memo1.Clear;
  Memo1.Lines.Text := Lines;
  Memo3.Visible := False;
  Memo1.Visible := True;
end;


end.

