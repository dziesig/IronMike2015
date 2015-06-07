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

unit systemmapformUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FormPanel;

type

  { TSystemMapForm }

  TSystemMapForm = class(TPanelForm)
    Panel1: TPanel;
  private
    { private declarations }
  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
  end;

var
  SystemMapForm: TSystemMapForm;

implementation

{$R *.lfm}

{ TSystemMapForm }

procedure TSystemMapForm.Hiding;
begin

end;

procedure TSystemMapForm.Showing;
begin

end;

end.

