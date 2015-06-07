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

unit MeetVisualizationFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, IronMikeDataUnit, TrainsUnit;

type

  { TMeetVisualizationForm }

  TMeetVisualizationForm = class(TForm)
    BitBtn1: TBitBtn;
    CheckGroup1: TCheckGroup;
    Image: TImage;
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure ImagePaint(Sender: TObject);
    procedure ImageResize(Sender: TObject);
  private
    { private declarations }
    vStationY : array of Integer;
    vStationCount : Integer;
    vTxtHgt       : Integer;
    vLeftTime     : TDateTime;
    vRightTime    : TDateTime;
    vPlotWidth    : Integer;
    vLeft, vRight : Integer;
    vDT           : TDateTime;
    procedure Clear;
    procedure DrawAxes;
    procedure DrawTrain( aTrain : TTrainData; LineColor : TColor );
    procedure DrawTrains;
  public
    { public declarations }
    function ShowModal : Integer; override;
  end;

var
  MeetVisualizationForm: TMeetVisualizationForm;

implementation

uses
  CommonDebug, CommonLog, CommonMath,
  StringSubs,
  Math;


{$R *.lfm}

const
  Hour   = 0.0416666666666667;
  Hour_2 = 0.0208333333333333;
  Hour_4 = 0.0104166666666667;

{ TMeetVisualizationForm }

procedure TMeetVisualizationForm.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
begin
  Repaint;
end;

procedure TMeetVisualizationForm.Clear;
var
  Rect : TRect;
begin
  Rect.Left      := 0;
  Rect.Top       := 0;
  Rect.Right     := Width;
  Rect.Bottom    := Height;

  Image.Canvas.Brush.Color := clWhite;
  Image.Canvas.Brush.Style := bsSolid;
  Image.Canvas.FillRect( Rect );

  Image.Canvas.Pen.Color := clBlack;
  Image.Canvas.Pen.Style := psSolid;

end;

procedure TMeetVisualizationForm.DrawAxes;
var
  MinTime, MaxTime : TDateTime; // Just in case no trains are selected.
  ATrain           : TTrainData;
  I                : Integer;
  TrainCount       : Integer;
  QuantTime        : Integer;
  Rem, LastRem     : Double;
  Time             : TDateTime;
  TimeWidth        : Integer;
  H                : Integer;
begin
  TimeWidth := Image.Canvas.TextWidth(IronMikeData.RRTime( 0.0 ));
  H := vTxtHgt div 2;
  MinTime := 1.0e12;  // Way in the future
  MaxTime := -1.0e12; // Way in the past.
  vLeftTime := MinTime;
  vRightTime := MaxTime;
  TrainCount := 0;
  for I := 0 to pred(CheckGroup1.Items.Count) do
    begin
      ATrain := TTrainData(CheckGroup1.Items.Objects[I]);
      MinTime := Min(ATrain.Schedule.Departs,MinTime);
      MaxTime := Max(ATrain.Schedule.Arrives,MaxTime);
      if CheckGroup1.Checked[I] then
        begin
          Inc(TrainCount);
          vLeftTime := Min(ATrain.Schedule.Departs,vLeftTime);
          vRightTime := Max(ATrain.Schedule.Arrives,vRIghtTime);
        end;
    end;
  if TrainCount = 0 then
    begin
      vLeftTime := MinTime;
      vRightTime := MaxTime;
    end;
  QuantTime := Floor( (vLeftTime-Hour_4)/Hour_2);
  vLeftTime := Hour_2*QuantTime;
  QuantTime := Ceil( (vRightTime)/Hour_2 );
  vRightTime := Hour_2*QuantTime;
  Time := vLeftTime;
  vDT := (vRightTime - vLeftTime)/vPlotWidth;

  for I := 0 to pred(vPlotWidth) do
    begin
      Rem := RealMod(Time,Hour);
      if LastRem > Rem then
        begin
          //Log.FormatLn('%4d:  %s %19.16f',[I,IronMikeData.RRTime( T ),RealMod(T,Hour)/Hour]);
          Image.Canvas.MoveTo(I+vLeft,vStationY[0]);
          Image.Canvas.LineTo(I+vLeft,vStationY[4]);
          Image.Canvas.TextOut( I+vLeft-(TimeWidth div 2), vStationY[4] + H,IronMikeData.RRTime( Time ) );
        end;
      LastRem := Rem;
      Time := Time + vDT;
    end;
  Image.Canvas.MoveTo(vRight,vStationY[0]);
  Image.Canvas.LineTo(vRight,vStationY[4]);
end;

procedure TMeetVisualizationForm.DrawTrain(aTrain: TTrainData; LineColor : TColor);
var
  X, Y     : Integer;
  I        : Integer;
  NumX, NumY : Integer;
begin
  Image.Canvas.Pen.Color := LineColor;
  Image.Canvas.Pen.Width := 3;
  X := Round((aTrain.Schedule.Entry[1].Arrives-vLeftTime) / vDT);
  Y := aTrain.Schedule.Entry[1].Pos-1;
  Image.Canvas.MoveTo(X+vLeft,vStationY[Y]);
  for I := 1 to aTrain.Schedule.Count do
    begin
      X := Round((aTrain.Schedule.Entry[I].Arrives-vLeftTime) / vDT);
      Y := aTrain.Schedule.Entry[I].Pos-1;
      if Y = 0 then
        begin
          NumX := X + vLeft;
          NumY := vStationY[0];
        end;
      //Log.FormatLn('Stn:  %d %s',[Train1.Schedule.Entry[I].Pos, IronMikeData.RRTime(Train1.Schedule.Entry[I].Arrives)]);
      //Log.FormatLn('Arrival:  %d, %d',[X,StationY[Y]]);
      Image.Canvas.LineTo(X+vLeft,vStationY[Y]);
      X := Round((aTrain.Schedule.Entry[I].Departs-vLeftTime) / vDT);
      //Log.FormatLn('Departure:  %d, %d',[X,StationY[Y]]);
      Image.Canvas.LineTo(X+vLeft,vStationY[Y]);
    end;
  Image.Canvas.TextOut( NumX, NumY - 14,IntToStr( aTrain.Number ) );
  Image.Canvas.Pen.Color := clBlack;
  Image.Canvas.Pen.Width := 1;
end;

procedure TMeetVisualizationForm.DrawTrains;
const
  LineColors : array [0..5] of TColor =
    ( clBlue, clGreen, clRed, clLime, clPurple, clTeal );
var
  I, J : Integer;
  aTrain : TTrainData;
begin
  J := 0;
  for I := 0 to pred(CheckGroup1.Items.Count) do
    begin
      if CheckGroup1.Checked[I] then
        begin
          aTrain := TTrainData( CheckGroup1.Items.Objects[I] );
          DrawTrain( aTrain, LineColors[J mod 6] );
          Inc(J);
        end;
    end;
end;

procedure TMeetVisualizationForm.FormCreate(Sender: TObject);

begin
  SetLength(vStationY,0);
end;

procedure TMeetVisualizationForm.ImagePaint(Sender: TObject);
const
  LM = 10; // Left Margin
  RM = 10; // Right Margin
  Gutter = 10; // Between Station name and line.
var
  I : Integer;
  //theLeft : Integer;
  theName : String;
  //PlotWidth : Integer;
  //theRight   : Integer;
  H : Integer;
  //T, DT : TDateTime;
  LastRem, Rem : Double;
  X : Integer;
  Y : Integer;
begin
  vLeft := 0;
  vTxtHgt := Image.Canvas.TextHeight('ABC');
  H := vTxtHgt div 2;

  for I := 1 to IronMikeData.StationList.Count do
    begin
      theName := IronMikeData.StationList[I].Name;
      vLeft := Max( vLeft, Image.Canvas.TextWidth( theName ) );
      Image.Canvas.TextOut(LM,vStationY[I-1] - H,theName);
    end;
  vLeft := vLeft + LM + Gutter;
  vRight := Image.Width;
  vRight := vRight - RM;
  vPlotWidth := vRight - vLeft;
  for I := 1 to vStationCount do
    begin
      Image.Canvas.MoveTo(vLeft,vStationY[I-1]);
      Image.Canvas.LineTo(vRight,vStationY[I-1]);
    end;

  DrawAxes;
  DrawTrains;
  //DT := (RightTime - LeftTime)/PlotWidth;
  //T := LeftTime;
  //TimeWidth := Image.Canvas.TextWidth( IronMikeData.RRTime( T ));
  //TimeWidth := Image.Canvas.TextWidth('12:00 AM');
  //LastRem := 0.0;
end;

procedure TMeetVisualizationForm.ImageResize(Sender: TObject);
var
  DY : Integer;
  I  : Integer;
begin
  DY := Image.Height div vStationCount;
  vStationY[0] := DY div 2;
  for I := 1 to pred(vStationCount) do
    vStationY[I] := vStationY[I-1] + DY;
  Repaint;
end;

function TMeetVisualizationForm.ShowModal: Integer;
var
  ATrain : TTrainData;
  I      : Integer;
begin
  with IronMikeData do
    begin
      vStationCount := StationsCount;
      SetLength(vStationY,vStationCount);
      Clear;
      CheckGroup1.Items.Clear;
      for I := 0 to pred(TrainList.Count) do
        begin
          ATrain := TrainList[I];
          if not ATrain.IsExtra then
            CheckGroup1.Items.AddObject(IntToStr(ATrain.Number,4,' '), ATrain);
        end;
      TStringList(CheckGroup1.Items).Sort;
    end;
  Result:=inherited ShowModal;
end;

end.

