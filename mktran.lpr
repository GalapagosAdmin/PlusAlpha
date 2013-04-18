program mktran;
// Create transaction
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  paTransactionList, paJournal, paLedger, 
  libpa, crt, paCurrency, paCLIUtil;

type

  { Tlsacct }

  Tmktran = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;


ResourceString
  MSGTRANSINSERTED = 'Transaction Inserted';
  ERRTRANSNOTINSERT = 'Error Inserting Transaction.';
  ERRTRANSNOTVALID = 'Transaction is not valid.';
  ERRTRANSNOTBAL = 'Transaction is not balanced.';



{ Tmktran }

procedure Tmktran.DoRun;
var
  ErrorMsg: String;
  Account:TLedgerAccount;
  ResultIndex:Integer;
  AcctNo:Integer; 
  TotalAmt:Longint;
//  JDE:TJournalDetailEntry;
  HdrMemoText:UTF8String='mktran test';
  TrnNo:Integer;  
  AcctNo1:Integer=2;
  AcctNo2:Integer=3;
  Posted:Boolean=False;
  DtlMemo1:UTF8String='mktran Test left';
  DtlMemo2:UTF8String='mktran Test right';
  Amt1:Longint=50;
  Amt2:Longint=50;
  DrCr1:TDrCr=Dr;
  DrCr2:TDrCr=Cr;

  Procedure GetInput;
    Begin
// Transaction Header
      HdrMemoText := GetText('Transaction Header Text:');      
      Write('Total Transaction Amount:');
      Readln(TotalAmt);
// Detail Record 1
      AcctNo1 := GetAccount('Account No. 1 (Debit):');
      Amt1 := TotalAmt;
      DtlMemo1 := HdrMemoText;
// Detail Record 2
      AcctNo2 := GetAccount('Account No. 2 (Credit):');
      Amt2 := TotalAmt;
      DtlMemo2 := HdrMemoText;

    End;

  procedure DoRegister;
    begin

  With CompleteJournalEntry do
  begin
  // Update Object
  WriteLn('Set Journal Header');
  With _JournalHeader do
    begin
      HdrMemo := HdrMemoText;
      HdrTransNo:= TrnNo;
      EffDate := now; // allow date entry later
      _JournalHeader.HdrPosted := Posted;
  // Retrieve Updates back
    end;

  Writeln('Set Journal Detail Entry 1');
  with _JournalDetailEntries[0] do
    begin
      TransNo := TrnNo;
      AcctNo :=  AcctNo1;
      TransRow := 0;
      Text := DtlMemo1;
      Amount := Amt1;
      DrCr := DrCr1;
    end;

  Writeln('Set Journal Detail Entry 2');
  with _JournalDetailEntries[1] do
    begin
      TransNo:=TrnNo;
      AcctNo:=AcctNo2;
      TransRow:=1;
      Text:=DtlMemo2;
      Amount:= Amt2;
      DrCr:=DrCr2;
    end;

  Writeln('Performing final checks....');

try
  If not IsBalanced then
     begin
       WriteErr(ERRTRANSNOTBAL);
       exit;
     end;
except
 WriteErr('Error during balance check!');
end;

try
   If not Validate then
     begin
       WriteErr(ERRTRANSNOTVALID);
       exit;
     end;
except
 WriteErr('Error during validation check!');
end;

   If Insert then
     begin
      Writeln(MsgTransInserted);
     end
    else
      WriteErr(ERRTRANSNOTINSERT);
    Writeln('Registered as Transaction No.:' + IntToStr(CompleteJournalEntry.TransNo));

    end;
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
  // Get the input
 // Writeln('Initializing...');
  GetInput;
  // Run the transaction creation routine
  DoRegister;
  NormVideo;
  // stop program loop
  Terminate;
end;

constructor Tmktran.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tmktran.Destroy;
begin
  inherited Destroy;
end;

procedure Tmktran.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('Usage: ',ExeName,' ');
  writeln(' Enter a new transaction interactively.');
end;

var
  Application: Tmktran;
begin
  Application:=Tmktran.Create(nil);
  Application.Title:='make transaction';
  Application.Run;
  Application.Free;
end.

