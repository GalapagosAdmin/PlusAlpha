program imp_amex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, libpa, padatabase, patext, pacliutil,  CustApp,
  paImport
  { you can add units after this };

type

  { TAmexJPImporter }

  TAmexJPImporter = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TAmexJPImporter }

procedure TAmexJPImporter.DoRun;
var
  ErrorMsg: String;
  r, c:Integer;
begin

  If ParamCount = 0 then
    begin
      WriteHelp;
      Terminate;
      exit;
    end;

  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  CSVImporter.SetFileName(ParamStr(1));
  for r := 0 to CSVImporter.GetRowCount do
    begin
      Write(r,': ');
      for c := 0 to CSVImporter.ColCount do
        Write('['+CSVImporter.GetValue(c,r)+']');
      writeln;
    end;


  // stop program loop
  Terminate;
end;

constructor TAmexJPImporter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TAmexJPImporter.Destroy;
begin
  inherited Destroy;
end;

procedure TAmexJPImporter.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('Usage: ',ExeName,' filename.csv');
end;

var
  Application: TAmexJPImporter;
begin
  Application:=TAmexJPImporter.Create(nil);
  Application.Title:='PlusAlpha American Express Japan Importer';
  Application.Run;
  Application.Free;
end.

