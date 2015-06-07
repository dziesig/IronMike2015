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

unit TimetableDisplayFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TTimetableDisplayForm }

  TTimetableDisplayForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ScrollBox1: TScrollBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Paint(Sender: TObject);
    procedure ScrollBox1Resize(Sender: TObject);
  private
    { private declarations }
    theBitmap : TBitmap;
    //FirstWidth, FirstHeight : Integer;
    //Inch2Image, Image2Inch  : Double;
    //ScaleFactor : Double;
    //Recursion   : Boolean;
    //function X( XInches : Double ) : Integer;
    //function Y( YInches : Double ) : Integer;
  public
    { public declarations }
  end;

var
  TimetableDisplayForm: TTimetableDisplayForm;

implementation

uses
  CommonLog;
{$R *.lfm}

{ TTimetableDisplayForm }

const
  //AspectRatio = 11.0 / 8.5; // Letter paper

  //FormDesignHeight = 611;
  //FormDesignWidth  = 977;

  LeftMargin = 0.5;
  TopMargin  = 0.5;

  //Inch2Image = 600.0/8.5;
  //Image2Inch = 8.5/600.0;

procedure TTimetableDisplayForm.BitBtn1Click(Sender: TObject);
begin
  Close;
end;

procedure TTimetableDisplayForm.FormCreate(Sender: TObject);
begin
  theBitmap := TBitmap.Create;
  theBitmap.Width := Image1.Width;
  theBitmap.Height := Round( Double(Image1.Width) * 8.5/11.0 );
  theBitmap.Canvas.Brush.Color := clWhite;
  theBitmap.Canvas.Brush.Style := bsSolid;

  theBitmap.Canvas.FillRect( 0, 0,theBitmap.Width,theBitmap.Height);
  //Height := FormDesignHeight;
  //Width  := FormDesignWidth;
  //FirstHeight := Image1.ClientHeight;
  //FirstWidth  := Image1.ClientWidth;
  //Recursion   := False;
  Image1.Invalidate;
end;

procedure TTimetableDisplayForm.FormShow(Sender: TObject);
begin
  ScrollBox1Resize(Sender);
  Image1Paint(Sender);
end;

procedure TTimetableDisplayForm.Image1Paint(Sender: TObject);
var
  X0, Y0, X1, Y1 : Integer;
  R : TRect;
begin
  //Image1.Canvas.Brush.Color := clWhite;
  //Image1.Canvas.Brush.Style := bsSolid;
  R := Image1.Canvas.ClipRect;
  //Image1.Canvas.fillRect( R );
  Canvas.StretchDraw(R,theBitmap);
  //Image1.Canvas.Pen.Color   := clBlack;
  //Image1.Canvas.Pen.Style   := psSolid;
  //Image1.Canvas.Pen.Width   := 1;
  //Image1.Canvas.FillRect( X(0),Y(0),X(11),Y(8.5) );
  //X0 := X(LeftMargin);
  //X1 := X(11.0-LeftMargin);
  //Y0 := Y(TopMargin);
  //Y1 := Y(8.5-TopMargin);
  //Image1.Canvas.Brush.Color := clWhite;
  //Image1.Canvas.Brush.Style := bsSolid;
  //Image1.Canvas.Rectangle(X0,Y0,X1,Y1);
end;

procedure TTimetableDisplayForm.ScrollBox1Resize(Sender: TObject);
begin
  //ScaleFactor   := 1.0;//Double(Image1.Width) / Double(FirstWidth);
  //Inch2Image    := Double(ScrollBox1.ClientWidth) / 11.0;
  //Image2Inch   := 1.0/Inch2Image;
  //Label1.Caption := IntToStr(Width);
  //Label2.Caption := FloatToStrF( Inch2Image, ffExponent, 5, 2);
  Image1.Width := ScrollBox1.ClientWidth;
  Image1.Height := Round(Double(Image1.Width) * 8.5 / 11.0 );
  //Image1.Height := Round( Double(FirstHeight) * ScaleFactor );
  Label3.Caption := IntToStr(Image1.Height);
  //Image1Paint(Sender);
  Image1.Invalidate;
end;

//function TTimetableDisplayForm.X(XInches: Double): Integer;
//begin
//  Result := Round(XInches * Inch2Image * ScaleFactor);
//end;
//
//function TTimetableDisplayForm.Y(YInches: Double): Integer;
//begin
//  Result := Round(YInches * Inch2Image * ScaleFactor);
//end;

end.

