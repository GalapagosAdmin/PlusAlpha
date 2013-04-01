unit fmeWelcomeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls;

type

  { TfmeWelcome }

  TfmeWelcome = class(TFrame)
    Label1: TLabel;
    procedure Label1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TfmeWelcome }

procedure TfmeWelcome.Label1Click(Sender: TObject);
begin

end;

end.

