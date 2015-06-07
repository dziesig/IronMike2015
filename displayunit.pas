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

unit DisplayUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ExtCtrls, Controls, Forms, Contnrs;

type

  { TDisplay }

  TDisplay = class
  private
    fMaxLineLen: Integer;
    fUnderscoreFont: TFont;
    fXMax: Integer;
    fXMin: Integer;
    fYMax: Integer;
    fYMin: Integer;
    vBaseFont : TFont;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetLeftMargin(AValue: Integer);
    procedure SetMaxLineLen(AValue: Integer);
    procedure SetTopMargin(AValue: Integer);
    procedure SetUnderscoreFont(AValue: TFont);
  protected
    vNextLine   : Integer;
    vLineEnd    : Integer;
    vUnderscore : Boolean; // Hack so I can keep default value in Center
    fLeftMargin, fTopMargin : Integer;

    procedure AdjustX( Value : Integer );
    procedure AdjustY( Value : Integer );
  public
    Image       : TImage;

    constructor Create( Img : TImage; BaseFont : TFont );

    procedure Clear;
    procedure Reset;
    procedure Put( Str : String; Left : Integer = 0 );
    procedure PutLine( Str : String; Left : Integer = 0 );
    procedure Center( Str : String; Value : Integer = 0 );
    procedure RightJustify( Str : String; rMargin : Integer = 0 );
    procedure NewLine( Count : Integer = 1 );
    function  NextLine : Integer;
    procedure Outline( XM, YM : Integer );
    // Caution - TextWidth uses BASEFONT
    function  TextWidth( Str : String ) : Integer;

    procedure Underscore( Str : String; Value : Integer = 0 );
    procedure TypeText( Str : String );

    procedure MoveTo( X, Y : Integer ); // Includes margins
    procedure LineTo( X, Y : Integer );

    procedure Print;

    property Height         : Integer read GetHeight;
    property Width          : Integer read GetWidth;
    property MaxLineLen     : Integer read fMaxLineLen write SetMaxLineLen;
    property TopMargin      : Integer read fTopMargin write SetTopMargin;
    property LeftMargin     : Integer read fLeftMargin   write SetLeftMargin;
    property XMin           : Integer read fXMin;
    property YMin           : Integer read fYMin;
    property XMax           : Integer read fXMax;
    property YMax           : Integer read fYMax;
    property UnderscoreFont : TFont   read fUnderscoreFont write SetUnderscoreFont;
  end;

  { TDisplayList }

  TDisplayListError = class( Exception );

  TDisplayList = class(TList)
  private
    vCurrentImage : Integer;
  public
    //constructor Create;
    procedure Append( Img : TPicture );
    function NextImage : TPicture;
    function PrevImage : TPicture;
    function LastImage : TPicture;
    function FirstImage : TPicture;
  end;

implementation

uses
  CommonMath,
  Printers, LCLIntf, WinUtilPrn;

{ TDisplayList }

procedure TDisplayList.Append(Img: TPicture);
var
  anImage : TPicture;
  H0, H1, w0, w1 : Integer;
begin
   anImage := TPicture.Create;
   anImage.Assign( Img );
   H0 := Img.Graphic.height;
   H1 := anImage.Graphic.Height;
   w0 := Img.Graphic.Width;
   w1 := anImage.Graphic.Width;
  vCurrentImage := Add( anImage );
end;

function TDisplayList.FirstImage: TPicture;
begin
  if Count >= 0 then
    begin
      vCurrentImage := 0;
      Result := TPicture(Items[vCurrentImage]);
    end
  else
    raise TDisplayListError.Create('No images available');
end;

function TDisplayList.LastImage: TPicture;
begin
  if Count >= 0 then
    begin
      vCurrentImage := pred(Count);
      Result := TPicture(Items[vCurrentImage]);
    end
  else
    raise TDisplayListError.Create('No images available');
end;

function TDisplayList.NextImage: TPicture;
begin
  if Count >= 0 then
    begin
      vCurrentImage := Min(vCurrentImage + 1,pred(Count));
      Result := TPicture(Items[vCurrentImage]);
    end
  else
    raise TDisplayListError.Create('No images available');
end;

function TDisplayList.PrevImage: TPicture;
begin
  if Count >= 0 then
    begin
      vCurrentImage := Max(vCurrentImage - 1,0);
      Result := TPicture(Items[vCurrentImage]);
    end
  else
    raise TDisplayListError.Create('No images available');
end;

{ TDisplay }

procedure TDisplay.AdjustX(Value: Integer);
var
  NewX : Integer;
begin
  NewX := Value + LeftMargin;
  if NewX > Width then
    begin
      Image.Width := NewX;
      Image.Picture.Bitmap.Width := NewX;
      Application.ProcessMessages;
    end;
end;

procedure TDisplay.AdjustY(Value: Integer);
var
  NewY : Integer;
  OldY : Integer;
  Rect : TRect;
begin
  NewY := Value + TopMargin + 30; // Look here! What is 30?
  OldY := Image.Height;
  if NewY > Image.Height then
    begin
      Image.Height := NewY;
      Image.Picture.Bitmap.Height := NewY;
      Rect.Top := OldY;
      Rect.Bottom := NewY;
      Rect.Left := 0;
      Rect.Right := Width;
      Image.Canvas.FillRect( Rect );
      //Application.ProcessMessages;
      NewY := Image.Picture.Bitmap.Height;
      OldY := Image.Height;
      //Application.ProcessMessages;
    end;
end;

procedure TDisplay.Center(Str: String; Value: Integer);
var
  TxtWid : Integer;
  X      : Integer;
  H      : Integer;
begin
  if Value = 0 then
    if MaxLineLen = 0 then
      Value := Width - vLineEnd
    else
      Value := fMaxLineLen - vLineEnd;
  if vUnderscore then
    begin
      Image.Canvas.Font.Assign(fUnderscoreFont);
      H      := Image.Canvas.TextHeight('M');
      MoveTo(vLineEnd,vNextLine + H);
      LineTo(vLineEnd+Value,vNextLine + H);
    end;
  TxtWid := Image.Canvas.TextWidth(Str);
  X      := vLineEnd + ((Value - TxtWid) div 2);
  Image.Canvas.TextOut( X + LeftMargin, vNextLine + TopMargin, Str );
  Image.Canvas.Font.Assign(vBaseFont);
  vLineEnd := vLineEnd+Value;
  fXMin := Min(XMin, X);
  fXMax := Max(XMax,vLineEnd);
end;

procedure TDisplay.Clear;
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

  Reset;
end;

constructor TDisplay.Create(Img: TImage; BaseFont : TFont);
begin
  Image := Img;
  vBaseFont := BaseFont;
  fUnderscoreFont := TFont.Create;
  fUnderscoreFont.Assign( BaseFont );
  Image.Canvas.Font := BaseFont;
  fLeftMargin    := 0;
  fTopMargin     := 0;
  Image.Canvas.MoveTo(0,0);
  Clear;
end;

function TDisplay.GetHeight: Integer;
begin
  Result := Image.Height;
end;

function TDisplay.GetWidth: Integer;
begin
  Result := Image.Width;
end;

procedure TDisplay.LineTo(X, Y: Integer);
begin
  AdjustX( X );
  AdjustY( Y );
  Image.Canvas.LineTo( X + LeftMargin, Y + TopMargin );
end;

procedure TDisplay.MoveTo(X, Y: Integer);
begin
  AdjustX( X );
  AdjustY( Y );
  Image.Canvas.MoveTo( X + LeftMargin, Y + TopMargin );
end;

procedure TDisplay.NewLine(Count: Integer);
var
  TxtHgt : Integer;
begin
  fYMin := Min(YMin, vNextLine);
  vLineEnd := 0;
  TxtHgt := (Image.Canvas.TextHeight('M') * 11) div 10;
  VNextLine := VNextLine + TxtHgt * Count;
  fYMax := Max(YMax,vNextLine);
  AdjustY( vNextLine );
end;

function TDisplay.NextLine: Integer;
begin
  Result := vNextLine;
end;

procedure TDisplay.Outline(XM, YM: Integer);
begin
  MoveTo( XMin - XM, YMin - YM );
  LineTo( XMax + XM, YMin - YM );
  LineTo( XMax + XM, YMax + YM );
  LineTo( XMin - XM, YMax + YM );
  LineTo( XMin - XM, YMin - YM );
end;

procedure TDisplay.Print;
var
  Rect : TRect;
  PixX : Integer;

begin
  PixX := Printer.PaperSize.Width;
  Rect.Top := 0;
  Rect.Left := 0;
  Rect.Right := PixX;
  Rect.Bottom := (Image.Height * PixX) div Image.Width;
  Printer.BeginDoc;
  try
    Printer.Canvas.StretchDraw(Rect,Image.Picture.Bitmap);
  finally
    Printer.EndDoc;
  end;
end;

procedure TDisplay.Put(Str: String; Left: Integer);
var
  TxtWid : Integer;
begin
  if Left > 0 then
    vLineEnd := Left;
  fXMin := Min(XMin, vLineEnd);
  Image.Canvas.TextOut(vLineEnd + LeftMargin,vNextLine+TopMargin,Str);
  TxtWid := Image.Canvas.TextWidth(Str);
  vLineEnd := vLineEnd + TxtWid;
  fXMax := Max(XMax, vLineEnd);
end;

procedure TDisplay.PutLine(Str: String; Left: Integer);
begin
  Put( Str, Left );
  NewLine;
end;

procedure TDisplay.Reset;
begin
  vLineEnd := 0;
  vNextLine := 0;

  fXmin := Width;
  fYmin := Height;
  fXMax := 0;
  fYMax := 0;
  MaxLineLen := 0;
  vUnderscore := False;
end;

procedure TDisplay.RightJustify(Str: String; rMargin: Integer);
var
  Right : Integer;
  X     : Integer;
  TxtWid : Integer;
begin
  if fMaxLineLen = 0 then
    Right := Width
  else
    Right := fMaxLineLen;
  Right := Right - rMargin;
  TxtWid := Image.Canvas.TextWidth( Str );
  X := Right - TxtWid;
  Image.Canvas.TextOut( X + LeftMargin, vNextLine + TopMargin, Str );
  vLineEnd := Right;
end;

procedure TDisplay.SetLeftMargin(AValue: Integer);
begin
  if fLeftMargin=AValue then Exit;
  fLeftMargin:=AValue;
end;

procedure TDisplay.SetMaxLineLen(AValue: Integer);
begin
  if fMaxLineLen=AValue then Exit;
  fMaxLineLen:=AValue;
end;

procedure TDisplay.SetTopMargin(AValue: Integer);
begin
  if fTopMargin=AValue then Exit;
  fTopMargin:=AValue;
end;

procedure TDisplay.SetUnderscoreFont(AValue: TFont);
begin
  fUnderscoreFont.Assign(AValue);
end;

function TDisplay.TextWidth(Str: String): Integer;
begin
  Result := Image.Canvas.TextWidth( Str );
end;

procedure TDisplay.TypeText(Str: String);
begin
  Image.Canvas.Font.Assign(fUnderscoreFont);
  PutLine(Str);
  Image.Canvas.Font.Assign(vBaseFont);
end;

procedure TDisplay.Underscore(Str: String; Value: Integer);
begin
  vUnderscore := True;
  try
    Center( Str, Value );
  finally
    vUnderscore := False;
  end;
end;

end.

