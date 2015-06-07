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

unit ConstantsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  NotSet = 0; // Stations in particular

  OrderAfter = 0.0; // Trains passing in the night

  ExtraOrdersOffset = 90.0/(24.0*60);

  NOWHERE = 0;
  SAL     = 1;
  KC      = 2;

  OneHour = 1.0/24.0;
  HalfHour = OneHour/2.0;
  FifteemMinutes = OneHour/4.0;

  ProduceLoadMin = HalfHour;
  ProduceLoadMax = HalfHour*3.0;
  IceLoadMin     = OneHour;
  IceLoadMax     = OneHour*2.0;

implementation

end.

