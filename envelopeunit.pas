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

unit EnvelopeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  TEnvelopeStatus = (esReady, esComplete);
  TEnvelopeType   = (etEnvelopeA, etEnvelopeB, etSLOFreight);

  { TEnvelope }

  TEnvelope = class
  private
    fName : String;
    fActiveTime : TDateTime;
    fPrintedActive: Boolean;
    fPrintedWarning: Boolean;
    fStatus: TEnvelopeStatus;
    fWarningTime : TDateTime;
    procedure SetPrintedActive(AValue: Boolean);
    procedure SetPrintedWarning(AValue: Boolean);
  public
    constructor Create( theActiveTime, theWarningTime : TDateTime; theName : String);

    procedure ResetPrintedFlags;
    procedure SetComplete;

    property ActiveTime  : TDateTime read fActiveTime;
    property WarningTime : TDateTime read fWarningTime;
    property Name        : String    read fName;

    property PrintedWarning : Boolean read fPrintedWarning write SetPrintedWarning;
    property PrintedActive  : Boolean read fPrintedActive  write SetPrintedActive;

    property Status         : TEnvelopeStatus read fStatus;
  end;

  TEnvelopes = array[TEnvelopeType] of TEnvelope;

var
  Envelopes : TEnvelopes;

procedure InitEnvelopes;

implementation

uses
  ConstantsUnit,
  IronMikeDataUnit;

procedure InitEnvelopes;
var
  Active, Warning : TDateTime;
  StartDate       : TDateTime;
begin
  if Assigned(Envelopes[etEnvelopeA] ) then
    Envelopes[etEnvelopeA].Free;
  if Assigned(Envelopes[etEnvelopeB] ) then
    Envelopes[etEnvelopeB].Free;
  if Assigned(Envelopes[etSLOFreight] ) then
    Envelopes[etSLOFreight].Free;

  StartDate := IronMikeData.OpSession.StartDate + IronMikeData.OpSession.StartTime;
  Active := 6.0*OneHour + 10.0*OneHour*Random;// Same results as c++ code
  Active := Active + StartDate;
  Warning := Active - HalfHour - 3*HalfHour*Random;
  Envelopes[etEnvelopeA] := TEnvelope.Create(Active,Warning,'Envelope A');

  Active := 6.0*OneHour + 10.0*OneHour*Random;// Same results as c++ code
  Active := Active + StartDate;
  Warning := Active - HalfHour - 3*HalfHour*Random;
  Envelopes[etEnvelopeB] := TEnvelope.Create(Active,Warning,'Envelope B');

  Active := 17.0*OneHour;
  Active := Active + StartDate;
  Warning := Active - HalfHour - 3*HalfHour*Random;
  Envelopes[etSLOFreight] := TEnvelope.Create(Active,Warning,'SLO Freight Envelope');
end;

{ TEnvelope }

constructor TEnvelope.Create(theActiveTime, theWarningTime: TDateTime;
  theName: String);
begin
  fName := theName;
  fActiveTime := theActiveTime;
  fWarningTime := theWarningTime;
  fStatus      := esReady;
  ResetPrintedFlags;
end;

procedure TEnvelope.ResetPrintedFlags;
begin
  fPrintedWarning := False;
  fPrintedActive  := False;
end;

procedure TEnvelope.SetComplete;
begin
  fStatus := esComplete;
end;

procedure TEnvelope.SetPrintedActive(AValue: Boolean);
begin
  if fPrintedActive=AValue then Exit;
  fPrintedActive:=AValue;
end;

procedure TEnvelope.SetPrintedWarning(AValue: Boolean);
begin
  if fPrintedWarning=AValue then Exit;
  fPrintedWarning:=AValue;
end;

initialization;
  Envelopes[etEnvelopeA] := nil;
  Envelopes[etEnvelopeB] := nil;
  Envelopes[etSLOFreight] := nil;

end.

