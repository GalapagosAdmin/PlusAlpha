program lsacct;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  {$IFDEF WINDOWS}
  //Windows, {for setconsoleoutputcp}
  {$ENDIF}
  paLedger, libpa, crt, paCurrency, pacliutil;

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
  ErrorMsg: UTF8String;
  Account:TLedgerAccount;
  SearchString:UTF8String;



begin
  // quick check parameters
  ErrorMsg:=CheckOptions('ha','help account:');
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
  SearchString := GetOptionValue('a', 'account');
  If SearchString <> '' then
      begin
       Writeln('Searching for:' + SearchString);
       AccountList.Free;
       AccountList := TAccountList.Create(SearchString);
      end;
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
  {$IFDEF WINDOWS}
  // Set output code page to UTF8 - Seems not to work in Windows 7 64bit
  //SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}

  Application:=Tlsacct.Create(nil);
  Application.Title:='account listing';
  Application.Run;
  Application.Free;
end.

