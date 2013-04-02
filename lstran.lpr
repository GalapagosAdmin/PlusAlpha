program lstran;
// list transactions
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  paTransactionList, paJournal, paLedger, 
  libpa, crt, paCurrency;

type

  { Tlsacct }

  Tlstran = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Tlsacct }

procedure Tlstran.DoRun;
var
  ErrorMsg: String;
  Account:TLedgerAccount;
  ResultIndex:Integer;
  AcctNo:Integer; // part of the search key
  JDE:TJournalDetailEntry;

  Procedure PrintTransaction(re:TResultEntry);
    begin
      TextColor(Green);
      Write(IntToStr(re.TransactionNo):4);
      Write(IntToStr(re.TransactionRow):4);
      JDE.load(re.TransactionNo, re.TransactionRow);
      Write(chr(9), JDE.DisplayDate);
      Case JDE.DrCr of Cr:TextColor(Red) end;
      Write(
         Chr(9), abap_translate(IntToStr(Ord(JDE.DrCr)), '0D1C'),
         chr(9), IntToStr(JDE.Amount):8);
      TextColor (Yellow);
      Write(' ',
          JDE.Currency);
         TextColor(white);
        
	Write(chr(9), JDE.text);
      Writeln;
    end;

begin
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
  
  If not TryStrToInt(Paramstr(1), AcctNo)  then
   begin
     WriteHelp;
     terminate;
     exit;
   end;

  JDE := TJournalDetailEntry.Create;

  TransactionList.Account := AcctNo;
  Write('Transaction Listing for Account: '+ IntToStr(AcctNo));
  
  Account := TLedgerAccount.Create;
  if 
  Account.Load(AcctNo) then
   Writeln(': '+Account.Text)
  else
    begin
      Writeln(': No Such Account!'); 
      terminate;
    end;

  For ResultIndex := low(TransactionList.TransNos) to high(TransactionList.TransNos) do
    PrintTransaction(TransactionList.TransNos[ResultIndex]);
  Writeln(IntToStr(High(TransactionList.TransNos) - Low(TransactionList.TransNos)+1) 
    + ' Entrie(s).');
(*

  if not AccountList.EOF then
    begin
      Account := AccountList.GetFirstAccount;
      PrintAccount(Account);
    end;
  while not accountlist.EOF do
    begin
    Account := AccountList.GetNextAccount;
    PrintAccount(Account);
    end;

*)

  NormVideo;
  // stop program loop
  Terminate;
end;

constructor Tlstran.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tlstran.Destroy;
begin
  inherited Destroy;
end;

procedure Tlstran.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('Usage: ',ExeName,' <ActNo>');
  writeln('<ActNo> = Internal Account Number');
end;

var
  Application: Tlstran;
begin
  Application:=Tlstran.Create(nil);
  Application.Title:='transaction listing';
  Application.Run;
  Application.Free;
end.

