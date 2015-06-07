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

unit SecondSectionFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, TrainsUnit;

type

  { TSecondSectionForm }

  TSecondSectionForm = class(TForm)
    ButtonOK: TBitBtn;
    BitBtn2: TBitBtn;
    EngineEdit: TLabeledEdit;
    Label1: TLabel;
    TrainListBox: TListBox;
    procedure EngineEditChange(Sender: TObject);
    procedure EngineEditKeyPress(Sender: TObject; var Key: char);
    procedure TrainListBoxClick(Sender: TObject);
  private
    fTrain: TTrainData;
    function GetEngine: String;
    { private declarations }
  public
    { public declarations }
    function ShowModal : Integer; override;
    property Train     : TTrainData read fTrain;
    property Engine    : String     read GetEngine;
  end;

var
  SecondSectionForm: TSecondSectionForm;

implementation

uses
  IronMikeDataUnit,
  StringSubs;

{$R *.lfm}

{ TSecondSectionForm }

function TSecondSectionForm.GetEngine: String;
begin
  Result := EngineEdit.Text;
end;

procedure TSecondSectionForm.EngineEditChange(Sender: TObject);
begin
  ButtonOk.Enabled := not (Empty(EngineEdit.Text) or (fTrain = nil));
end;

procedure TSecondSectionForm.EngineEditKeyPress(Sender: TObject; var Key: char
  );
begin
  if not (Key in ['0'..'9',#8]) then
    Key := #0;
end;

function TSecondSectionForm.ShowModal: Integer;
var
  aTrain : TTrainData;
  I      : Integer;
begin
  ButtonOk.Enabled := False;
  EngineEdit.Text := '';
  fTrain := nil;
  TrainListBox.Clear;
  with IronMikeData do
    begin
      for I := 0 to pred(TrainList.Count) do
        begin
          aTrain := TrainList[I];
          if (aTrain.Status = tsReady) and  // Can't add second section to running train
             (not (aTrain.Flags in [tfGreen1, tfGreen2, tfWhite, tfMultiWhite])) and
             (not aTrain.IsExtra) then
            if TrainListBox.Items.IndexOf(IntToStr(aTrain.Number)) < 0 then
              TrainListBox.AddItem(IntToStr(aTrain.Number,4,' '),aTrain);
        end;
    end;
  TStringList(TrainListBox.Items).Sort;
  Result:=inherited ShowModal;
end;

procedure TSecondSectionForm.TrainListBoxClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := TrainListBox.ItemIndex;
  if Idx >= 0 then
    fTrain := TTrainData( TrainListBox.Items.Objects[Idx] );
  EngineEditChange(Sender);
end;

end.

