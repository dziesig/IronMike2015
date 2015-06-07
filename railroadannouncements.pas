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

unit RailroadAnnouncements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Voiceunit;

var
  VoiceQueue : TVoiceQueue;

  procedure CrewCallFor( TrainNumber : Integer ); overload;
  procedure CrewCallFor( TrainNumber : String ); overload; // Will need work for extras
  procedure NewOrdersFor( TrainNumber : Integer ); overload;
  procedure NewOrdersFor( TrainNumber : String ); overload;
  procedure ClearanceFor( TrainNumber : Integer ); overload;
  procedure ClearanceFor( TrainNumber : String ); overload;
  procedure PreIcingComplete;
  procedure FinalIcingComplete;
  procedure CarsLoaded( TrackName : String );
  procedure Envelops;
  procedure SetVoiceParams;

  function Number( Num : Integer ) : String;  overload;
  function Number( Num : String )  : String;  overload;



implementation

uses
  CommonIni;

procedure CrewCallFor(TrainNumber: Integer);
begin
 VoiceQueue.Say('Crew call for train ' + Number(TrainNumber) + '.');
end;

procedure CrewCallFor(TrainNumber: String);
begin
  VoiceQueue.Say('Crew call for train ' + Number(TrainNumber) + '.');
end;

procedure NewOrdersFor(TrainNumber: Integer);
begin
  VoiceQueue.Say('New Orders for train ' + Number(TrainNumber) + '.');
end;

procedure NewOrdersFor(TrainNumber: String);
begin
  VoiceQueue.Say('New Orders for train ' + Number(TrainNumber) + '.');
end;

procedure ClearanceFor(TrainNumber: Integer);
begin
  VoiceQueue.Say('Clearance for train ' + Number(TrainNumber) + ' is ready.');
end;

procedure ClearanceFor(TrainNumber: String);
begin
  VoiceQueue.Say('Clearance for train ' + Number(TrainNumber) + ' is ready.');
end;

procedure PreIcingComplete;
begin
  VoiceQueue.Say('pre icing is done. -- the cars are ready to be spotted.');
end;

procedure FinalIcingComplete;
begin
  VoiceQueue.Say('Final Icing is done. --  the train is ready to Depart.')
end;

procedure CarsLoaded(TrackName: String);
begin
  VoiceQueue.Say(Format( '%s cars are loaded.',
                         [TrackName] ) );
end;

procedure Envelops;
begin
  VoiceQueue.Say('An Envelope is ready.');
end;

procedure SetVoiceParams;
begin
  VoiceQueue.Voice := GetConfig('Voice','Speaker',0);
  VoiceQueue.Speed := GetConfig('Voice','Speed',5);
end;

function Number(Num: Integer): String;
var
  Hundreds, Decade, Century, Units : Integer;
begin
  if Num < 100 then
    Result := IntToStr(Num)
  else
    begin
      Century := Num div 100;
      Hundreds := Num mod 100;
      Decade  := (Num mod 100) div 10;
      Units   := (Num mod 10);
      if Decade = 0 then
        Result := IntToStr(Century) + ' Oh ' + IntToStr(Units)
      else
        Result := IntToStr(Century) + ' ' + IntToStr(Hundreds);
    end;
end;

function Number(Num: String): String;
begin
  Result := Number( StrToInt( Num ) );
end;

initialization
  if not Assigned(VoiceQueue) then
    VoiceQueue := TVoiceQueue.Create;
finalization
  VoiceQueue.Destroy;
end.

