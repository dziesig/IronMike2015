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

unit StationsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type

  { TTrackData }

  TTrackData=class
  private
    fPos   : Integer;
    fName  : String;
    fCode  : String;
    fIdx   : Integer;
  public
    constructor Create( aName, aCode : String; aPos, aIdx : Integer );

    property Name : String read fName;
    property Code : String read fCode;
    property Pos  : Integer read fPos; // 1..Count (1-based for consistency)
    property Idx  : Integer read fIdx; // Sequential (Slow)
  end;

  { TTrackList }

  TTrackList=class(TObjectList)
  private
    function GetByIdx(Idx : Integer): TTrackData;
    function GetTrack(Pos : Integer): TTrackData;
  public
    constructor Create;

    procedure Add( Trk : TTrackData );
    procedure Sort;

    property Track[Pos : Integer] : TTrackData read GetTrack; default; // Sorted by Pos
                                                                       // WAT = 1 .. SLO = 5
    property ByIdx[Idx : Integer] : TTrackData read GetByIdx;          // Sequential (slow)

   end;

  { TStationData }

  TStationData=class
  private
    fPos : Integer;
    fName : String;
    fCode : String;
    fIdx  : Integer;
    fTracks: TTrackList;
  public
    constructor Create( aName, aCode : String; aPos, aIdx : Integer );
    destructor  Destroy; override;

    property Name   : String read fName;
    property Code   : String read fCode;
    property Pos    : Integer read fPos;
    property Idx    : Integer read fIdx;
    property Tracks : TTrackList read fTracks;
  end;

  { TStationList }

  TStationList=class(TObjectList)
  private
    function GetByIdx(Idx : Integer): TStationData;
    function GetPosByName( aName : String ): Integer;
    function GetStation(Pos : Integer): TstationData;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add( Stn : TStationData );
    procedure Sort;

    property Station[Pos : Integer] : TstationData read GetStation; default; // Sorted by Pos
                                                                             // WAT = 1 .. SLO = 5
    property ByIdx[Idx : Integer] : TStationData read GetByIdx;              // Sequential (slow)

    property PosByName[ aName : String ] : Integer read GetPosByName;
  end;

implementation

{ TTrackList }

procedure TTrackList.Add(Trk: TTrackData);
begin
  inherited Add( Trk );
  Sort;
end;

constructor TTrackList.Create;
begin
  inherited Create;
end;

function TTrackList.GetByIdx(Idx : Integer): TTrackData;
var
  I : Integer;
  T : TTrackData;
begin
  Result := nil;
  for I := 0 to pred(Count) do
    begin
      T := TTrackData( Items[I] );
      if T.fIdx = Idx then
        begin
          Result := T;
          exit;
        end;
    end;
end;

function TTrackList.GetTrack(Pos : Integer): TTrackData;
begin
  Result := TTrackData( Items[Pos-1] );
end;

function TrackPosCompare( Left, Right : Pointer ) : Integer;
begin
  Result := TTrackData(Left).Pos - TTrackData(Right).Pos;
end;

procedure TTrackList.Sort;
begin
  inherited Sort( @TrackPosCompare );
end;

{ TTrackData }

constructor TTrackData.Create(aName, aCode : String; aPos, aIdx: Integer);
begin
  fName := aName;
  fCode := aCode;
  fPos  := aPos;
  fIdx  := aIdx;
end;

{ TStationList }

procedure TStationList.Add(Stn: TStationData);
begin
  inherited Add( Stn );
  Sort;
end;

constructor TStationList.Create;
begin
  inherited Create
end;

destructor TStationList.Destroy;
begin
  inherited Destroy;
end;

function TStationList.GetByIdx(Idx : Integer): TStationData;
var
  I : Integer;
  C : Integer;
  S : TStationData;
begin
  Result := nil;
  c := Count;
  for I := 0 to pred(Count) do
    begin
      S := Items[I] as TStationData;
      if S.fIdx = Idx then
        Result := S;
    end;
end;

function TStationList.GetPosByName( aName : String ): Integer;
var
  I : Integer;
  S : TStationData;
begin
  aName := Upcase( aName );
  Result := 0; // None found
  for I := 1 to Count do
    begin
      S := Station[I];
      if UpCase( S.Name ) = aName then
        begin
          Result := S.Pos;
          exit;
        end;
    end;
end;

function TStationList.GetStation(Pos : Integer): TstationData;
begin
  Result := TstationData(Items[Pos - 1]);
end;

function StnPosCompare( Left, Right : Pointer ) : Integer;
var
  L, R : TStationData;
begin
  L := TStationData(Left);
  R := TStationData(Right);
  Result := L.Pos - R.Pos;
end;

procedure TStationList.Sort;
begin
  inherited Sort( @StnPosCompare );
end;

{ TStationData }

constructor TStationData.Create(aName, aCode : String; aPos, aIdx: Integer);
begin
  fName := aName;
  fCode := aCode;
  fPos  := aPos;
  fIdx  := aIdx;
  fTracks := TTrackList.Create;
end;

destructor TStationData.Destroy;
begin
  fTracks.Free;
  inherited Destroy;
end;

end.

