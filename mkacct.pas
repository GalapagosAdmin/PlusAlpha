program mkacct;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this }
  , paledger, libpa, crt;

type

  { TPANewAccount }

  TPANewAccount = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPANewAccount }

Procedure WritelnErr(Const s:UTF8String);
  begin
    TextColor(Red);
    Writeln(s);
  end;

procedure TPANewAccount.DoRun;
var
  ErrorMsg: String;
  AccountTitle:String;
  Currency:String;
  AcctTypeInt:Integer;
  LedgerAccount:TLedgerAccount;

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

  NormVideo;
  write('Account Title:');
  HighVideo;
  readln(AccountTitle);
  if AccountTitle = '' then begin terminate; exit end;

  NormVideo;
  Write('Currency:');
  HighVideo;
  Readln(Currency);
  NormVideo;
  if Currency = '' then begin terminate; exit end;
  writeln('Possible Account Types:');
  TextColor(Yellow);
  Writeln('1. Asset');
  Writeln('2. Liability');
  Writeln('3. Equity');
  Writeln('4. Expense');
  Writeln('5. Income');
  Writeln('6. Placeholder only');
  Writeln('7. Other');
  Writeln('or 0 to cancel.');
  NormVideo;
  Write('Account Type: ');
  HighVideo;
  Readln(AcctTypeInt);
  NormVideo;
//  Write('External Account Number:');
  { add your program here }
  If AcctTypeInt = 0 then begin terminate; exit end;
  Write('Creating Acount...');
  LedgerAccount := TLedgerAccount.Create;
  LedgerAccount.Text := AccountTitle;
  LedgerAccount.Currency := Currency;
  LedgerAccount.AccountType := TAcctType(AcctTypeInt);
  LedgerAccount.AcctNo := AccountList.GetNextFreeAccountNo;
  LedgerAccount.CreateAcctGUID;
  if LedgerAccount.Synch then
    begin
        Writeln('Done');
        LedgerAccount.Commit;
        Writeln('Account No:' + IntToStr(LedgerAccount.AcctNo));
        Writeln('Account GUID:' + GUIDToString(LedgerAccount.AcctGUID));
    end
  else
   WritelnErr('Error');
  TextColor(White);
  // stop program loop
  Terminate;
end;

constructor TPANewAccount.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPANewAccount.Destroy;
begin
  inherited Destroy;
end;

procedure TPANewAccount.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: TPANewAccount;
begin
  Application:=TPANewAccount.Create(nil);
  Application.Title:='PlusAlpha New Account';
  Application.Run;
  Application.Free;
end.

