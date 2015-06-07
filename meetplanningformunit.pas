unit MeetPlanningFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqlite3conn, FileUtil, Forms, Controls, Graphics,
  Dialogs, DbCtrls, DBGrids, StdCtrls, Buttons, ExtCtrls, IronMikeDataUnit,
  TrainsUnit;

type

  { TMeetPlanningForm }

  TMeetPlanningForm = class(TForm)
    BitBtn1: TBitBtn;
    Image: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure ImageResize(Sender: TObject);
    procedure ImagePaint(Sender: TObject);
  private
    { private declarations }
    //Train1, Train2 : TTrainData;
    Departure,                // Actual arrival and departure of the trains
    Arrival    : TDateTime;
    LeftTime,                 // Times of the Left and Right extents of
    RightTime  : TDateTime;   // the plot
    //StationY : array of Integer;
    TxtHgt   : Integer;
    //procedure Clear;
    //function  Overlap : Boolean;
    procedure ProcessMeet;
  public
    { public declarations }
    procedure ShowModal;
  end;

var
  MeetPlanningForm: TMeetPlanningForm;

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

{ TMeetPlanningForm }

//procedure TMeetPlanningForm.Clear;
//var
//  Rect : TRect;
//begin
//  //Rect.Left      := 0;
//  //Rect.Top       := 0;
//  //Rect.Right     := Width;
//  //Rect.Bottom    := Height;
//
//  //Image.Canvas.Brush.Color := clWhite;
//  //Image.Canvas.Brush.Style := bsSolid;
//  ////Image.Canvas.FillRect( Rect );
//  //
//  //Image.Canvas.Pen.Color := clBlack;
//  //Image.Canvas.Pen.Style := psSolid;
//
//end;

procedure TMeetPlanningForm.FormCreate(Sender: TObject);
begin
  //SetLength(StationY,0);
end;

procedure TMeetPlanningForm.FormHide(Sender: TObject);
begin
  //IronMikeData.TrainsQuery.Close;
end;

procedure TMeetPlanningForm.ImageResize(Sender: TObject);
var
  DY : Integer;
  I  : Integer;
begin
  //DY := Image.Height div IronMikeData.StationsCount;
  //StationY[0] := DY div 2;
  //for I := 1 to pred(IronMikeData.StationsCount) do
  //  StationY[I] := StationY[I-1] + DY;
  //Repaint;
end;

//function TMeetPlanningForm.Overlap: Boolean;
//begin
//  Result := False;
  //Result := True; // presumes success
  //if (Train1 = nil) or (Train2 = nil) then
  //  Result := False
  //else
  //  begin
  //    Result := ((Train1.Schedule.Departs <= Train2.Schedule.Departs) and
  //               (Train1.Schedule.Arrives >= Train2.Schedule.Departs)) or
  //              ((Train1.Schedule.Departs <= Train2.Schedule.Arrives) and
  //               (Train1.Schedule.Arrives >= Train2.Schedule.Arrives)) or
  //              ((Train2.Schedule.Departs <= Train1.Schedule.Departs) and
  //               (Train2.Schedule.Arrives >= Train1.Schedule.Departs)) or
  //              ((Train2.Schedule.Departs <= Train1.Schedule.Arrives) and
  //               (Train2.Schedule.Arrives >= Train1.Schedule.Arrives));
  //  end;
//end;

procedure TMeetPlanningForm.ProcessMeet;
var
  Duration : TDateTime;
  SSS, TTT : String;
  qqq : Integer;
begin
  //Clear;
  //NoOverlapLabel.Visible := not Overlap;
  //if (Train1 = nil) or (Train2 = nil) then exit;
  //Departure := Min( Train1.Schedule.Departs, Train2.Schedule.Departs);
  //Arrival   := Max( Train1.Schedule.Arrives, Train2.Schedule.Arrives);
  //with IronMikeData do
  //  begin
  //    Duration := Arrival - Departure;
  //    //SSS := IronMikeData.RRTime( Departure );
  //    //TTT := IronMikeData.RRTime( Arrival );
  //    //Stub('Start/Finish:  ' + SSS + ' / ' + TTT);
  //    Log.FormatLn('Trains:  %d %d',[Train1.Number, Train2.Number]);
  //    qqq := Floor( (Departure-Hour_4)/Hour_2);
  //    LeftTime := Hour_2*qqq;
  //    Log.FormatLn('Departure:  %d %s %s',[qqq, RRTime(LeftTime),RRTime(Departure)]);
  //    qqq := Ceil( (Arrival)/Hour_2 );
  //    RightTime := Hour_2*qqq;
  //    Log.FormatLn('Arrival:    %d %s %s',[qqq, RRTime(RightTime * qqq),RRTime(Arrival)]);
  //  end;
end;

procedure TMeetPlanningForm.ShowModal;
var
  I      : Integer;
  ATrain : TTrainData;
  N      : String;
begin
  with IronMikeData do
    begin
      I := StationsCount;
      //SetLength(StationY,I);
      //SetLength(Trains,TrainList.Count);
      //CheckGroupTrains.Items.Clear;
      for I := 0 to pred(TrainList.Count) do
        begin
          ATrain := TrainList[I];
          N := IntToStr( ATrain.Number,4,' ');
          //CheckGroupTrains.Items.AddObject( N, ATrain )
        end;
    end;
  //TStringList(ComboBox1.Items).Sort;
  //Clear;
  //RePaint;
  inherited ShowModal;
end;

procedure TMeetPlanningForm.ImagePaint(Sender: TObject);
const
  LM = 10; // Left Margin
  RM = 10; // Right Margin
  Gutter = 10; // Between Station name and line.
var
  I : Integer;
  theLeft : Integer;
  theName : String;
  PlotWidth : Integer;
  theRight   : Integer;
  H : Integer;
  T, DT : TDateTime;
  LastRem, Rem : Double;
  TimeWidth : Integer;
  X : Integer;
  Y : Integer;
begin
  theLeft := 0;
  TxtHgt := Image.Canvas.TextHeight('ABC');
  H := TxtHgt div 2;
  //for I := 0 to pred(IronMikeData.StationsCount) do
  //  begin
  //    //theName := IronMikeData.StationName[I+1];   Change to stations list when implementd
  //    theLeft := Max( theLeft, Image.Canvas.TextWidth( theName ) );
  //    Image.Canvas.TextOut(LM,StationY[I] - H,theName);
  //  end;
  for I := 1 to IronMikeData.StationList.Count do
    begin
      theName := IronMikeData.StationList[I].Name;
      theLeft := Max( theLeft, Image.Canvas.TextWidth( theName ) );
      //Image.Canvas.TextOut(LM,StationY[I-1] - H,theName);
    end;
  theLeft := theLeft + LM + Gutter;
  theRight := Image.Width;
  theRight := theRight - RM;
  PlotWidth := theRight - theLeft;
  for I := 1 to IronMikeData.StationList.Count do
    begin
      //Image.Canvas.MoveTo(theLeft,StationY[I-1]);
      //Image.Canvas.LineTo(theRight,StationY[I-1]);
    end;
  DT := (RightTime - LeftTime)/PlotWidth;
  T := LeftTime;
  TimeWidth := Image.Canvas.TextWidth( IronMikeData.RRTime( T ));
  LastRem := 0.0;
  //if (Train1 = nil) or (Train2 = nil) then exit;
  for I := 0 to pred(PlotWidth) do
    begin
      Rem := RealMod(T,Hour);
      if LastRem > Rem then
        begin
          Log.FormatLn('%4d:  %s %19.16f',[I,IronMikeData.RRTime( T ),RealMod(T,Hour)/Hour]);
          //Image.Canvas.MoveTo(I+theLeft,StationY[0]);
          //Image.Canvas.LineTo(I+theLeft,StationY[4]);
          //Image.Canvas.TextOut( I+theLeft-(TimeWidth div 2), StationY[4] + H,IronMikeData.RRTime( T ) );
        end;
      LastRem := Rem;
      T := T + DT;
    end;
  //Image.Canvas.MoveTo(theRight,StationY[0]);
  //Image.Canvas.LineTo(theRight,StationY[4]);
  // Now Comes the fun part.
  //Image.Canvas.Pen.Color := clRed;
  //Image.Canvas.Pen.Width := 3;
  //X := Round((Train1.Schedule.Entry[1].Arrives-LeftTime) / DT);
  //Y := Train1.Schedule.Entry[1].Pos-1;
  //Image.Canvas.MoveTo(X+theLeft,StationY[Y]);
  //for I := 1 to Train1.Schedule.Count do
  //  begin
  //    X := Round((Train1.Schedule.Entry[I].Arrives-LeftTime) / DT);
  //    Y := Train1.Schedule.Entry[I].Pos-1;
  //    Log.FormatLn('Stn:  %d %s',[Train1.Schedule.Entry[I].Pos, IronMikeData.RRTime(Train1.Schedule.Entry[I].Arrives)]);
  //    Log.FormatLn('Arrival:  %d, %d',[X,StationY[Y]]);
  //    Image.Canvas.LineTo(X+theLeft,StationY[Y]);
  //    X := Round((Train1.Schedule.Entry[I].Departs-LeftTime) / DT);
  //    Log.FormatLn('Departure:  %d, %d',[X,StationY[Y]]);
  //    Image.Canvas.LineTo(X+theLeft,StationY[Y]);
  //  end;
  //Image.Canvas.Pen.Color := clBlue;
  //Image.Canvas.Pen.Width := 3;
  //X := Round((Train2.Schedule.Entry[1].Arrives-LeftTime) / DT);
  //Y := Train2.Schedule.Entry[1].Pos-1;
  //Image.Canvas.MoveTo(X+theLeft,StationY[Y]);
  //for I := 1 to Train2.Schedule.Count do
  //  begin
  //    X := Round((Train2.Schedule.Entry[I].Arrives-LeftTime) / DT);
  //    Y := Train2.Schedule.Entry[I].Pos-1;
  //    //Log.FormatLn('Stn:  %d %s',[Train1.Schedule.Entry[I].Pos, IronMikeData.RRTime(Train1.Schedule.Entry[I].Arrives)]);
  //    //Log.FormatLn('Arrival:  %d, %d',[X,StationY[Y]]);
  //    Image.Canvas.LineTo(X+theLeft,StationY[Y]);
  //    X := Round((Train2.Schedule.Entry[I].Departs-LeftTime) / DT);
  //    //Log.FormatLn('Departure:  %d, %d',[X,StationY[Y]]);
  //    Image.Canvas.LineTo(X+theLeft,StationY[Y]);
  //  end;
  //Image.Canvas.Pen.Color := clBlack;
  //Image.Canvas.Pen.Width := 1;
end;

end.

