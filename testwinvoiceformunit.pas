unit TestWinVoiceFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Variants;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    SPVoice : Variant;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ComObj;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SpVoice.Speak('Hello World', 0);;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SPVoice := CreateOleObject('SAPI.SpVoice');
end;

end.

