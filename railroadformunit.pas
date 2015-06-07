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

unit RailroadFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, FileUtil, Forms, Controls, Graphics, Dialogs, ExtDlgs,
  StdCtrls, ExtCtrls, DbCtrls, Buttons, MagicTimeEdit, FormPanel, IronMikeDataUnit;

type

  { TRailroadForm }

  TRailroadForm = class(TPanelForm)
    ButtonUseLogo: TBitBtn;
    DataSource1: TDataSource;
    DBEdit1: TDBEdit;
    DBEdit2: TDBEdit;
    DBEdit3: TDBEdit;
    DBRadioGroup1: TDBRadioGroup;
    DBRadioGroup2: TDBRadioGroup;
    DBRadioGroup3: TDBRadioGroup;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    LogoDBImage: TDBImage;
    OpenPictureDialog1: TOpenPictureDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    procedure ButtonUseLogoClick(Sender: TObject);
    //procedure CrewCallLeadTimeEditTimeChange(Sender: TObject);
    procedure DataSource1StateChange(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    { private declarations }
    RailroadData : TRailroadDataRec;
    procedure ControlsLoad;
  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
  end;

var
  RailroadForm: TRailroadForm;

implementation

uses
  CommonDebug;

{$R *.lfm}

{ TRailroadForm }

procedure TRailroadForm.ButtonUseLogoClick(Sender: TObject);
begin
  with IronMikeData do
    begin
      RailroadQuery.Open;
      RailroadQuery.Edit;
      LogoDBImage.Picture := Image1.Picture;
      RailroadQuery.Post;
    end;
end;

procedure TRailroadForm.ControlsLoad;
begin
  RailroadData := IronMikeData.Railroad;
  //CrewCallLeadTimeEdit.DateTime := RailroadData.CrewCallLead;
end;

//procedure TRailroadForm.CrewCallLeadTimeEditTimeChange(Sender: TObject);
//var
//  LeadTime : TDateTime;
//begin
//  with IronMikeData do
//    begin
//      LeadTime := CrewCallLeadTimeEdit.DateTime;
//      RailroadQuery.Edit;
//      RailroadQuery.FieldByName('CrewCallLead').AsFloat := LeadTime;
//      RailroadQuery.Post;
//      RailroadQuery.Edit;
//    end;
//end;

procedure TRailroadForm.DataSource1StateChange(Sender: TObject);
var
  S : TDataSetState;
begin
  S := DataSource1.State;
end;

procedure TRailroadForm.Hiding;
var
  S : TDataSetState;
begin
  S := DataSource1.State;
  if S in [dsEdit, dsInsert] then
    IronMikeData.RailroadQuery.Post;
end;

procedure TRailroadForm.Image1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    with RailroadData do
      begin
        Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
      end;
end;

procedure TRailroadForm.Showing;
begin
  ControlsLoad;
end;

end.

