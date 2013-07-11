program imp_amex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, libpa, padatabase, patext, pacliutil,  CustApp,
  paImport, md5, paImportMap
  { you can add units after this };

type

  { TAmexJPImporter }

  TAmexJPImporter = class(TCustomApplication)
    private
    AmexJPCSVImport:TAmexJPCSVImport;
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

  AmexJPCSVImport.SetFileName(ParamStr(1));
  while not AmexJPCSVImport.eof do
    begin
      AmexJPCSVImport.GetNext;
//      Write(r,': ');
//        Write('['+AmexJPCSVImport.GetValue(c,r)+']');
      Write(DatetoStr(AmexJPCSVImport.data.TransactionDate));
      Write(chr(9));
      Write(FloatToStr(AmexJPCSVImport.data.LocalCurrencyAmount));
      Write(chr(9));
      Write(UTF8ToANSI(AmexJPCSVImport.data.memo));
      Write(chr(9));
      Write(MD5Print(AmexJPCSVImport.data.MD5Hash));
      Write(chr(9));
      Write(AmexJPCSVImport.data.ForeignCurrencyCode);
      Write(' ');
      Write(FloatToStr(AmexJPCSVImport.data.ForeignCurrencyAmount));
      writeln;
      AmexJPCSVImport.CreateTransaction;
    end;


  // stop program loop
  Terminate;
end;

constructor TAmexJPImporter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AmexJPCSVImport := TAmexJPCSVImport.Create;
  StopOnException:=True;
end;

destructor TAmexJPImporter.Destroy;
begin
  inherited Destroy;
  AmexJPCSVImport.free;
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

