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
  lcltype, paimport_amexjp, fileutil;    // Used for VK_*

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
  {$DEFINE SYSTEMDEBUG}
  {$IFDEF WINDOWS}
 //  writeln(GetConsoleOutputCP);
 // writeln(getACP);
 // UTF8toconsole
 // WritelnUTF8(('あいうえお秋葉原'));
 // READLN;
  {$ENDIF}
  AmexJPCSVImport.SetFileName(ParamStr(1));
  AmexJPCSVImport.TestMode := HasOption('t','test') ;

  while not AmexJPCSVImport.eof do
    begin
      AmexJPCSVImport.GetNext;
//      Write(r,': ');
//        Write('['+AmexJPCSVImport.GetValue(c,r)+']');
      writelnUTF8('--- Raw Data: ---');
      WriteUTF8(DatetoStr(AmexJPCSVImport.data.TransactionDate));
      WriteUTF8(VK_TAB);
      WriteUTF8(FloatToStr(AmexJPCSVImport.data.LocalCurrencyAmount));
      WriteUTF8(vk_tab);
      WriteUTF8(UTF8ToANSI(AmexJPCSVImport.data.memo));
      WriteUTF8(vk_tab);
      WriteUTF8(MD5Print(AmexJPCSVImport.data.MemoHash));
      WriteUTF8(vk_tab);
      WriteUTF8(AmexJPCSVImport.data.ForeignCurrencyCode);
      WriteUTF8(' ');
      WriteUTF8(FloatToStr(AmexJPCSVImport.data.ForeignCurrencyAmount));
      writelnUTF8;
      AmexJPCSVImport.CreateTransaction;
      if HasOption('t','test') then
        begin
          writelnUTF8('--- Transaction Header ---');
                    WritelnUTF8((CompleteJournalEntry._JournalHeader.HdrMemo));
          writelnUTF8('--- Transaction Details ---');
          for LineItem := 0 to CompleteJournalEntry.Rows - 1 do
            begin
              PrintTransRow(CompleteJournalEntry._JournalDetailEntries[LineItem].TransRow);
              WriteUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].DisplayDate);
              WriteUTF8(GuidToString(CompleteJournalEntry._JournalDetailEntries[LineItem].AcctGUID));
              PrintDrCr(CompleteJournalEntry._JournalDetailEntries[LineItem].DrCr);
              PrintAmount(CompleteJournalEntry._JournalDetailEntries[LineItem].Amount);
              WriteUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].Currency);
              WritelnUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].Text);
            end;
        end;  // text mode
    end;


  // stop program loop
  readln;
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

