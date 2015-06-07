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

unit AddTrainFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type

  { TAddTrainForm }

  TAddTrainForm = class(TForm)
    ButtonOK: TBitBtn;
    BitBtn2: TBitBtn;
    ConsistComboBox: TComboBox;
    StartComboBox: TComboBox;
    EndComboBox: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrainEdit: TLabeledEdit;
    procedure StartComboBoxCloseUp(Sender: TObject);
    procedure TrainEditChange(Sender: TObject);
    procedure TrainEditKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    procedure CheckOK;
    function GetConsist: String;
    function GetEndPos: Integer;
    function GetStartPos: Integer;
    function GetTrainNumber: Integer;
  public
    { public declarations }
    function ShowModal : Integer; override;
    property TrainNumber : Integer read GetTrainNumber;
    property StartPos    : Integer read GetStartPos;
    property EndPos      : Integer read GetEndPos;
    property Consist     : String  read GetConsist;
  end;

var
  AddTrainForm: TAddTrainForm;

implementation

uses
  IronMikeDataUnit,
  StringSubs;

{$R *.lfm}

{ TAddTrainForm }

procedure TAddTrainForm.CheckOK;
begin
  ButtonOk.Enabled := (not Empty(TrainEdit.Text)) and
                      (StartComboBox.ItemIndex <> EndComboBox.ItemIndex );
end;

function TAddTrainForm.GetConsist: String;
var
  Idx : Integer;
begin
  Idx := ConsistComboBox.ItemIndex;
  Result := ConsistComboBox.Items[Idx];
end;

function TAddTrainForm.GetEndPos: Integer;
var
  Idx : Integer;
begin
  Idx := EndComboBox.ItemIndex;
  Result := Idx + 1;
  //Result := Integer( EndComboBox.Items.Objects[Idx]  )- 1;
end;

function TAddTrainForm.GetStartPos: Integer;
var
  Idx : Integer;
begin
  Idx := StartComboBox.ItemIndex;
  Result := Idx + 1;
  //Result := Integer( StartComboBox.Items.Objects[Idx] ) - 1;
end;

function TAddTrainForm.GetTrainNumber: Integer;
begin
  Result := StrToInt(TrainEdit.Text);
end;

function TAddTrainForm.ShowModal: Integer;
var
  I : Integer;
begin
  ButtonOk.Enabled := False;
  StartComboBox.Clear;
  EndComboBox.Clear;
  TrainEdit.Text := '';
  with IronMikeData do
    begin
      //for I := 1 to StationsCount do   Change to stations list when implemented
      //  begin
      //    StartComboBox.AddItem( StationName[I],TObject(I) );
      //    EndComboBox.AddItem( StationName[I],TObject(I) );
      //  end;
      StartComboBox.ItemIndex := 0;
      EndComboBox.ItemIndex   := pred(StationsCount);
    end;
  Result:=inherited ShowModal;
end;

procedure TAddTrainForm.StartComboBoxCloseUp(Sender: TObject);
begin
  CheckOk;
end;

procedure TAddTrainForm.TrainEditChange(Sender: TObject);
begin
  CheckOk;
end;

procedure TAddTrainForm.TrainEditKeyPress(Sender: TObject; var Key: char);
begin
  if not (Key in ['0'..'9',#8]) then
    Key := #0;
end;

end.

