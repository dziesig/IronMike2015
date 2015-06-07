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

unit OrdersFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, FormPanel,
  TrainsUnit;

type

  { TOrdersForm }

  TOrdersForm = class(TPanelForm)
    BitBtn1: TBitBtn;
    ButtonNone: TBitBtn;
    ButtonSalinas: TBitBtn;
    ButtonKingCity: TBitBtn;
    ButtonBoth: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    LabelKCEastbound: TLabel;
    LabelKCWestbound: TLabel;
    LabelSalinas: TLabel;
    LabelKingCity: TLabel;
    ListBoxTrains: TListBox;
    procedure BitBtn1Click(Sender: TObject);
    procedure ListBoxTrainsClick(Sender: TObject);
    procedure WorkAtClick(Sender: TObject);
  private
    { private declarations }
    SelectedTrain : TTrainData;

    procedure FreightOrders( aStops : Integer );
    procedure UpdateFreightOrdersForm;

  public
    { public declarations }
    procedure Showing; override;
    procedure Hiding;  override;
  end;

var
  OrdersForm: TOrdersForm;

implementation

uses
  CommonDebug,
  IronMikeDataUnit,
  MainFormUnit,
  DispatcherFormUnit,
  OrdersUnit,
  RRSubsUnit,
  ConstantsUnit;

{$R *.lfm}

{ TOrdersForm }

procedure TOrdersForm.BitBtn1Click(Sender: TObject);
begin
  MainForm.BackToDispatcher;
end;

procedure TOrdersForm.FreightOrders(aStops: Integer);
var
  WhereVal  : Integer;
  TimeNow   : TDateTime;
  TSAL, TKC : TDateTime;
  Order     : TOrder;
begin
  TimeNow := MainForm.MagicClock1.DateTime;
  TKC := 0.0;
  TSAL := 0.0;
  WhereVal := SelectedTrain.SetStopsAt( aStops );
  { TODO 3 -odonz -cFuture Maintenance : Fix conflicting references to SAL (1 or 2) and KC (2 or 3) }
  if (WhereVal and SAL) = SAL then
    SelectedTrain.RunLateFrom(2,OneHour);
  if (WhereVal and KC) = KC then
    SelectedTrain.RunLateFrom(3,OneHour);
  if (WhereVal and SAL) = SAL then
    TSAL := SelectedTrain.Schedule.ByPos[2].Departs;
  if (WhereVal and KC) = KC then
    TKC := SelectedTrain.Schedule.ByPos[3].Departs;
  if WhereVal <> NOWHERE then
    begin
      Order := TFormE.Create(SelectedTrain,TKC,TSAL,TimeNow);
      OrdersList.Add(Order);
      SelectedTrain.Orders.Add( Order );
      Post2Extras(Order, SelectedTrain, nil );
      IronMikeData.TrainList.Sort;
      RunRailroad( SelectedTrain, DispatcherForm.Display, DispatcherForm. DisplayList );
    end;
  UpdateFreightOrdersForm;
  //RegisterTrain....
  //SwitchToDispatcherForm;
end;

procedure TOrdersForm.Hiding;
begin

end;

procedure TOrdersForm.ListBoxTrainsClick(Sender: TObject);
var
  Idx : Integer;
begin
  Idx := ListBoxTrains.ItemIndex;
  if Idx >= 0 then
    begin
      SelectedTrain := TTrainData( ListBoxTrains.Items.Objects[Idx] );
      ButtonNone.Enabled := True;
      ButtonSalinas.Enabled := True;
      ButtonKingCity.Enabled := True;
      ButtonBoth.Enabled := True;
    end
  else
    SelectedTrain := nil;
  UpdateFreightOrdersForm;

end;

procedure TOrdersForm.Showing;
var
  I : Integer;
  T : TTrainData;
begin
  ListBoxTrains.Clear;
  SelectedTrain := nil;
  with IronMikeData do
    begin
      for I := 0 to pred( TrainList.Count ) do
        begin
          T := TrainList[I];
          if (T.Consist = 'FRT') and (T.Status in [tsReady,tsRunning]) and
             (not T.IsExtra) then
            ListBoxTrains.AddItem( IntToStr(T.Number), T );
        end;
    end;
  ButtonNone.Enabled := False;
  ButtonSalinas.Enabled := False;
  ButtonKingCity.Enabled := False;
  ButtonBoth.Enabled := False;
end;

procedure TOrdersForm.UpdateFreightOrdersForm;
begin
  LabelKCEastbound.Visible := False;
  LabelKCWestbound.Visible := False;
  if SelectedTrain = nil then
    begin
      LabelSalinas.Enabled  := False;
      LabelKingCity.Enabled := False;
    end
  else
    begin
      LabelSalinas.Enabled  := (SelectedTrain.StopsAt and SAL) = SAL;
      LabelKingCity.Enabled := (SelectedTrain.StopsAt and KC) = KC;
      LabelKCEastbound.Visible := False;
      LabelKCWestbound.Visible := False;
      if LabelKingCity.Enabled then
        begin
          if SelectedTrain.Direction = 'Eastbound' then
            LabelKCEastbound.Visible := True
          else
            LabelKCWestbound.Visible := True;
        end;
    end;
end;

procedure TOrdersForm.WorkAtClick(Sender: TObject);
var
  Button : TBitBtn;
begin
  Button := Sender as TBitBtn;
  FreightOrders( Button.Tag );
end;

end.

