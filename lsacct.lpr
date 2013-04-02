program lsacct;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  paLedger, libpa, crt, paCurrency;

type

  { Tlsacct }

  Tlsacct = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ Tlsacct }

procedure Tlsacct.DoRun;
var
  ErrorMsg: String;
  Account:TLedgerAccount;

  Procedure PrintAccount(Account:TLedgerAccount);
    begin
      TextColor(Green);
      Write(IntToStr(account.AcctNo):4);
      TextColor(White);
      Write('  ', 
        Abap_Translate(
          IntToStr(Ord(account.AccountType)), 
          AcctTransMap)
           );
      Case Account.DrCr of Cr:TextColor(Red) end;
      Write(
         Chr(9), abap_translate(IntToStr(Ord(account.DrCr)), '0D1C'),
         chr(9), IntToStr(Account.Balance):8);
      TextColor (Yellow);
      Write(
         chr(9), account.Currency);
         TextColor(white);
         Write(
         chr(9), Account.Text);
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


  NormVideo;
  // stop program loop
  Terminate;
end;

constructor Tlsacct.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor Tlsacct.Destroy;
begin
  inherited Destroy;
end;

procedure Tlsacct.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

var
  Application: Tlsacct;
begin
  Application:=Tlsacct.Create(nil);
  Application.Title:='account listing';
  Application.Run;
  Application.Free;
end.

