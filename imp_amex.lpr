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
  lcltype, paimport_amexjp,
  fileutil;    // Used for cross-platform VK_*

ResourceString
  APPTITLE = 'PlusAlpha American Express Japan Importer';
  HLPOPTH = 'Display Help / Program Options';
  HLPOPTT = 'Test mode (do not save transactions)';
  HLPOPTV = 'Verbose mode - Show parsed input';
  HDRRAWDATA = 'Raw Data:';
  HDRTXNHDR = 'Transaction Header';
  HDRTXNDTL = 'Transaction Detail';

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
  VerboseMode:Boolean;
  TextMode:Boolean;

Procedure WriteRawTransactionData;
  begin
      WritelnUTF8('--- ' + HDRRAWDATA + ' ---');
      WriteUTF8(DatetoStr(AmexJPCSVImport.data.TransactionDate));
      WriteUTF8(VK_TAB);
      WriteUTF8(FloatToStr(AmexJPCSVImport.data.LocalCurrencyAmount));
      WriteUTF8(vk_tab);
      WriteUTF8(UTF8ToANSI(AmexJPCSVImport.data.memo)); // Don't even ask...
      WriteUTF8(vk_tab);
      WriteUTF8(MD5Print(AmexJPCSVImport.data.MemoHash));
      WriteUTF8(vk_tab);
      WriteUTF8(AmexJPCSVImport.data.ForeignCurrencyCode);
      WriteUTF8(' ');
      WriteUTF8(FloatToStr(AmexJPCSVImport.data.ForeignCurrencyAmount));
      writelnUTF8;
  end;

Procedure WriteTransactionDetail;
  begin
    PrintTransRow(CompleteJournalEntry._JournalDetailEntries[LineItem].TransRow);
    WriteUTF8(vk_tab);
    WriteUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].DisplayDate);
    WriteUTF8(vk_tab);
    WriteUTF8(GuidToString(CompleteJournalEntry._JournalDetailEntries[LineItem].AcctGUID));
    WriteUTF8(vk_tab);
    PrintDrCr(CompleteJournalEntry._JournalDetailEntries[LineItem].DrCr);
    WriteUTF8(vk_tab);
    PrintAmount(CompleteJournalEntry._JournalDetailEntries[LineItem].Amount);
    WriteUTF8(vk_tab);
    WriteUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].Currency);
    WriteUTF8(vk_tab);
    WritelnUTF8(CompleteJournalEntry._JournalDetailEntries[LineItem].Text);
  end;

begin
  If ParamCount = 0 then
    begin
      WriteHelp;
      Terminate;
      exit;
    end;

  // quick check parameters
  // Or we could use FindCmdLineSwitch
  ErrorMsg:=CheckOptions('htv','help test verbose');
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

  VerboseMode := HasOption('v','verbose');
  TestMode : = HasOption('t','test') ;
  { add your program here }
 // {.$DEFINE SYSTEMDEBUG}
  {$IFDEF WINDOWS}
 //  writeln(GetConsoleOutputCP);
 // writeln(getACP);
 // UTF8toconsole
 // WritelnUTF8(('あいうえお秋葉原'));
 // READLN;
  {$ENDIF}
  // File shoule be last.
  AmexJPCSVImport.SetFileName(ParamStr(ParamCount));
  AmexJPCSVImport.TestMode := TestMode;

  while not AmexJPCSVImport.eof do
    begin
      AmexJPCSVImport.GetNext;
//      Write(r,': ');
//        Write('['+AmexJPCSVImport.GetValue(c,r)+']');
      If VerboseMode then
         WriteRawTransactionData;
      AmexJPCSVImport.CreateTransaction;
      if TextMode then
        begin
          writelnUTF8('--- ' + HDRTXNHDR + ' ---');
          WritelnUTF8((CompleteJournalEntry._JournalHeader.HdrMemo));
          writelnUTF8('--- ' + HDRTXNDTL + ' ---');
          for LineItem := 0 to CompleteJournalEntry.Rows - 1 do
              WriteTransactionDetail;
        end;  // text mode
    end;


  // stop program loop
//  readln;   // for Running within Lazarus
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
  writelnUTF8('Usage: '+ExeName+' -h');
  writelnUTF8('Usage: '+ExeName+' [-t][-v] filename.csv');
  writelnUTF8('h: ' + HLPOPTH);
  writelnUTF8('t: ' + HLPOPTT);
  writelnUTF8('v: ' + HLPOPTV);
end;

var
  Application: TAmexJPImporter;
begin

  Application:=TAmexJPImporter.Create(nil);
  Application.Title := APPTITLE;
  Application.Run;
  Application.Free;
end.

