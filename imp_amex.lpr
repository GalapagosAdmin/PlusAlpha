program imp_amex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, libpa, padatabase, patext, pacliutil,  CustApp,
  paImport, md5, paImportMap,
  { you can add units after this }
  paJournal,  // needed for testing
  lcltype, paimport_amexjp;    // Used for VK_*

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
  ErrorMsg: UTF8String;
  LineItem:Integer;
begin

  If ParamCount = 0 then
    begin
      WriteHelp;
      Terminate;
      exit;
    end;

  // quick check parameters
  ErrorMsg:=CheckOptions('ht','help test');
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
  AmexJPCSVImport.TestMode := HasOption('t','test') ;

  while not AmexJPCSVImport.eof do
    begin
      AmexJPCSVImport.GetNext;
//      Write(r,': ');
//        Write('['+AmexJPCSVImport.GetValue(c,r)+']');
      Write(DatetoStr(AmexJPCSVImport.data.TransactionDate));
      Write(VK_TAB);
      Write(FloatToStr(AmexJPCSVImport.data.LocalCurrencyAmount));
      Write(vk_tab);
      Write(UTF8ToANSI(AmexJPCSVImport.data.memo));
      Write(vk_tab);
      Write(MD5Print(AmexJPCSVImport.data.MemoHash));
      Write(vk_tab);
      Write(AmexJPCSVImport.data.ForeignCurrencyCode);
      Write(' ');
      Write(FloatToStr(AmexJPCSVImport.data.ForeignCurrencyAmount));
      writeln;
      AmexJPCSVImport.CreateTransaction;
      if HasOption('t','test') then
        begin
          Writeln('--- Test Mode ---');
          Writeln(CompleteJournalEntry._JournalHeader.HdrMemo);
          for LineItem := 0 to CompleteJournalEntry.Rows - 1 do
            begin
              PrintTransRow(CompleteJournalEntry._JournalDetailEntries[LineItem].TransRow);
              Write(CompleteJournalEntry._JournalDetailEntries[LineItem].DisplayDate);
              Write(GuidToString(CompleteJournalEntry._JournalDetailEntries[LineItem].AcctGUID));
              PrintDrCr(CompleteJournalEntry._JournalDetailEntries[LineItem].DrCr);
              PrintAmount(CompleteJournalEntry._JournalDetailEntries[LineItem].Amount);
              Write(CompleteJournalEntry._JournalDetailEntries[LineItem].Currency);
              Writeln(CompleteJournalEntry._JournalDetailEntries[LineItem].Text);
            end;
        end;  // text mode
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

