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

unit ShowTimetableFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  DbCtrls, StdCtrls, ExtCtrls, Buttons, TrainsUnit, Contnrs, db;

type

  { TSupTrainList }

  { TTTTrainList }

  TTTTrainList = class( TObjectList )
  private
    function GetTrain(Idx : Integer): TTrainData;
    procedure SortTrains; virtual; abstract;
  public
    procedure Add( aTrain : TTrainData );
    property Train[Idx : Integer] : TTrainData read GetTrain; default;
  end;

  TSupTrainList = class(TTTTrainList)
    procedure SortTrains; override;
  end;

  { TInfTrainList }

  TInfTrainList = class(TTTTrainList)
    procedure SortTrains; override;
  end;


  { TShowTimetableForm }

  TShowTimetableForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    DataSource1: TDataSource;
    DBText1: TDBText;
    Label1: TLabel;
    LogoDBImage: TDBImage;
    Panel1: TPanel;
    TTGrid: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TTGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
    InfTrainList : TInfTrainList;
    SupTrainList : TSupTrainList;
    StnCol         : Integer;
    procedure DrawTimetable;
  public
    { public declarations }
  end;

var
  ShowTimetableForm: TShowTimetableForm;

implementation

uses
  CommonDebug, CommonLog,
  Stringsubs,
  IronMikeDataUnit,
  Printers;

{$R *.lfm}

{ TShowTimetableForm }

{ TSupTrainList }

function SupCompare( L, R : Pointer ) : Integer;
begin
  if TTrainData(L).Schedule.Departs < TTrainData(R).Schedule.Departs then
    Result := 1
  else if TTrainData(L).Schedule.Departs > TTrainData(R).Schedule.Departs then
    Result := -1
  else
    Result := 0;
end;

{ TTTTrainList }

procedure TTTTrainList.Add(aTrain: TTrainData);
begin
  inherited Add(aTrain);
  SortTrains;
end;

function TTTTrainList.GetTrain(Idx : Integer): TTrainData;
begin
  Result := TTrainData( Items[Idx] );
end;

procedure TSupTrainList.SortTrains;
begin
  inherited Sort( @SupCompare );
end;

{ TInfTrainList }

function InfCompare( L, R : Pointer ) : Integer;
begin
  Result := - SupCompare( L, R );
end;

procedure TInfTrainList.SortTrains;
begin
  inherited Sort( @InfCompare );
end;

procedure TShowTimetableForm.BitBtn1Click(Sender: TObject);
var
  Rect : TRect;
  PixX : Integer;
  BitMap : TBitmap;
begin
  Printer.Orientation := poLandscape;
  PixX := Printer.PaperSize.Width;
  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Right := Panel1.Width;
  Rect.Bottom := Panel1.Height;
  BitMap := TBitmap.Create;
  BitMap.Width := Panel1.Width;
  BitMap.Height := Panel1.Height;
  BitMap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.Brush.Style := bsSolid;
  Bitmap.Canvas.FillRect( Rect );
  Panel1.PaintTo(Bitmap.Canvas,0,0);
  Bitmap.SaveToFile('Timetable.bmp');
  //Rect.Bottom := (Panel1.Height * PixX) div Panel1.Width;
  //Printer.BeginDoc;
  //try
  //  //Panel1.PaintTo(Printer.Canvas,0,0);
  //  //Printer.Canvas.StretchDraw(Rect,Panel1.);
  //  Printer.Canvas.CopyRect(Classes.Rect(0, 0, Printer.PaperSize.Width, Printer.PaperSize.Height),
  //     BitMap.Canvas, Classes.Rect(0, 0, BitMap.Width, BitMap.Height));
  //finally
  //  Printer.EndDoc;
  //end;
end;

procedure TShowTimetableForm.BitBtn2Click(Sender: TObject);
begin
  Hide;
end;

procedure TShowTimetableForm.DrawTimetable;
const
  StnColWid = 130;
var
  I, J : Integer;
  TrainCount     : Integer;
  StationCount   : Integer;
  aTrain         : TTrainData;
  InfCol1        : Integer;
  Arrive, Depart : TDateTime;
begin
  with IronMikeData do
    begin
      TTGrid.DefaultColWidth := 72;
      TTGrid.RowCount := IronMikeData.StationsCount + 1;
      TTGrid.RowHeights[0] := 50;
      TrainCount := IronMikeData.TrainsCount;
      InfTrainList.Clear;
      SupTrainList.Clear;
      for I := 0 to pred(TrainCount) do
        if not TrainList[I].IsExtra then
          begin
            aTrain := TrainList[I];
            if aTrain.IsSuperiorDirection then
              SupTrainList.Add(aTrain)
            else
              InfTrainList.Add(aTrain);
          end;
      TTGrid.ColCount := SupTrainList.Count + InfTrainList.Count + 1; // Includes Stations column
      for I := 0 to pred(SupTrainList.Count) do
        begin
          aTrain := SupTrainList[I];
          TTGrid.Cells[I,0] := IntToStr( aTrain.Number );
          if not Empty( aTrain.Name ) then
            TTGrid.Cells[I,0] := TTGrid.Cells[I,0] + ';' + aTrain.Name;
        end;
      StnCol := SupTrainList.Count;
      TTGrid.ColWidths[StnCol] := StnColWid;
      ttGrid.Cells[StnCol,0] := 'Stations';
      InfCol1 := SupTrainList.Count + 1;
      for I := 0 to pred(InfTrainList.Count) do
        begin
          aTrain := InfTrainList[I];
          TTGrid.Cells[I + InfCol1,0] := IntToStr( aTrain.Number );
          if not Empty( aTrain.Name ) then
            TTGrid.Cells[I + InfCol1,0] := TTGrid.Cells[I + InfCol1,0] + ';' + aTrain.Name;
        end;

      for I := 1 to StationList.Count do
        begin
          TTGrid.Cells[StnCol,I] := StationList[I].Name;
        end;

      for I := 0 to pred(SupTrainList.Count) do
        begin
          aTrain := SupTrainList[I];
          for J := 1 to StationList.Count do
            begin
              Arrive := atrain.Schedule.ByPos[J].Arrives;
              Depart := atrain.Schedule.ByPos[J].Departs;
              if Arrive = Depart then
                TTGrid.Cells[I,J] := RRTime( Arrive )
              else
                TTGrid.Cells[I,J] := RRTime( Arrive ) + ';' + RRTime( Depart );
            end;
        end;
      for I := 0 to pred(InfTrainList.Count) do
        begin
          aTrain := InfTrainList[I];
          for J := 1 to StationList.Count do
            begin
              Arrive := atrain.Schedule.ByPos[J].Arrives;
              Depart := atrain.Schedule.ByPos[J].Departs;
              if Arrive = Depart then
                TTGrid.Cells[I+InfCol1,J] := RRTime( Arrive )
              else
                TTGrid.Cells[I+InfCol1,J] := RRTime( Arrive ) + ';' + RRTime( Depart );
            end;
        end;
    end;
  TTGrid.ClientWidth := TTGrid.DefaultColWidth * pred(TTGrid.ColCount) + StnColWid;
  TTGrid.ClientHeight := TTGrid.DefaultRowHeight * (TTGrid.RowCount - 1) +
                         TTGrid.RowHeights[0];
end;

procedure TShowTimetableForm.FormActivate(Sender: TObject);
begin
  DrawTimetable;
end;

procedure TShowTimetableForm.FormCreate(Sender: TObject);
begin
  SupTrainList := TSupTrainList.Create(False);
  InfTrainList := TInfTrainList.Create(False);
end;

procedure TShowTimetableForm.FormResize(Sender: TObject);
begin
  DrawTimetable;
end;

procedure TShowTimetableForm.TTGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
var
  Line,  Line1 : String;
  Line2, Line3 : String;
  SepPos       : Integer;
  Hgt          : Integer;
  ColWid       : Integer;
  TxtWid       : Integer;
  TxtLeft      : Integer;
  NameSize     : Integer;
begin
  Hgt := aRect.Bottom - aRect.Top;
  Line := TTGrid.Cells[aCol,aRow];
  SepPos := Pos( ';', Line );
  if SepPos > 0 then
    begin
     Line1 := Copy(Line, 1, SepPos - 1) ;
     Line2 := Copy(Line, SepPos + 1,
                   Length(Line) - SepPos) ;
    end
  else
     Line1 := LIne;

  if (aCol = StnCol) or (aRow = 0) then
    TTGrid.Canvas.Font.Style := TTGrid.Canvas.Font.Style + [fsBold]
  else
    TTGrid.Canvas.Font.Style := TTGrid.Canvas.Font.Style - [fsBold];

  ColWid := TTGrid.ColWidths[aCol];

  TxtWid := TTGrid.Canvas.TextWidth(Line1);
  TxtLeft := (ColWid - TxtWid) div 2;

  TTGrid.Canvas.FillRect(aRect);
  TTGrid.Canvas.TextOut(aRect.Left + TxtLeft, aRect.Top + 2, Line1) ;
  if SepPos > 0 then
    begin
      if aRow = 0 then
        begin
          SepPos := Pos(' ',Line2);
          if SepPos > 0 then
            begin
              Line3 := Copy(Line2,SepPos+1,255);
              Line2 := Copy(Line2,1,SepPos-1);
            end;
          NameSize := TTGrid.Canvas.Font.Size;
          TTGrid.Canvas.Font.Size := (2*NameSize) div 3;
          TxtWid := TTGrid.Canvas.TextWidth(Line2);
          TxtLeft := (ColWid - TxtWid) div 2;
          TTGrid.Canvas.TextOut(aRect.Left + TxtLeft, aRect.Top + (Hgt div 2), Line2) ;
          if SepPos > 0 then
            begin
              TxtWid := TTGrid.Canvas.TextWidth(Line3);
              TxtLeft := (ColWid - TxtWid) div 2;
              TTGrid.Canvas.TextOut(aRect.Left + TxtLeft, aRect.Top + ((3*Hgt) div 4), Line3) ;
            end;
          TTGrid.Canvas.Font.Size := NameSize;
        end
      else
        begin
          TxtWid := TTGrid.Canvas.TextWidth(Line2);
          TxtLeft := (ColWid - TxtWid) div 2;
          TTGrid.Canvas.TextOut(aRect.Left + TxtLeft, aRect.Top + (Hgt div 2), Line2) ;
        end;
    end;


end;

end.

