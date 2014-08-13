unit pacliutil; // PlusAlpha Command Line Interface Utilities
{$MODE DELPHI } // for RESULT

interface

uses  sysutils, LibPa, paLedger, fileutil;

Procedure WriteErr(const Err:UTF8String);
//Function GetAccount:Longint;
Function  GetAccount(const Prompt:UTF8String):Longint;
Function  GetText(const Prompt:UTF8String):UTF8String;
Procedure PrintAcctNo(AcctNo:Integer);
Procedure PrintAcctType(AccountType:TAcctType);
Procedure PrintDrCr(Const DrCr:TDrCr);
Procedure PrintAccount(Const Account:TLedgerAccount; Const ShowGUID:Boolean);
Procedure PrintTransRow(Const Row:Integer);
Procedure PrintAmount(Const Amount:Integer);

// Accept UTF8 String, manually convert it to Console encoding, and write it
// to the Console in Windows. (In OS X, the console should be UTF8 anyway...)
// We could set the console to UTF8 and then just write directly, but that has
// two problems:
// 1. Some versions of the FPC runtime apparently reset the console code-page
// every time we use writeln, etc.
// 2. Older versions of Windows can't handle UTF8 as a codepage.
procedure WriteLnUTF8(s:UTF8String); overload;
procedure WriteLnUTF8; overload;
procedure WriteUTF8(s:UTF8String); overload;
procedure WriteUTF8(i:LongInt); overload;

implementation

uses
  {$IFDEF WINDOWS}
  windows,
  {$ENDIF}

  StrUtils, Crt;

procedure WriteLnUTF8(s:UTF8String);
var
  bw:dword;
begin
 {$IFDEF WINDOWS}
  s := s + #13#10;
  s := UTF8ToConsole(s);
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE),@s[1],length(s),bw,nil);
  {$ELSE}
  writeln(s);
  {$ENDIF}
end;

procedure WriteLnUTF8; overload;
  var
    s:UTF8String;
  begin
   {$IFDEF WINDOWS}
    s := '';
    WritelnUTF8(s);
    {$ELSE}
    writeln;
    {$ENDIF}
  end;

procedure WriteUTF8(s:UTF8String);
var
  bw:dword;
begin
 {$IFDEF WINDOWS}
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE),@s[1],length(s),bw,nil);
  {$ELSE}
  write(s);
  {$ENDIF}
end;

procedure WriteUTF8(i:LongInt); overload;
var
  bw:dword;
  s:string;
begin
 {$IFDEF WINDOWS}
  s := IntToStr(i);
  s := UTF8ToConsole(s);
  WriteConsole(GetStdHandle(STD_OUTPUT_HANDLE),@s[1],length(s),bw,nil);
  {$ELSE}
  write(i);
  {$ENDIF}
end;


Procedure PrintAccount(Const Account:TLedgerAccount; Const ShowGUID:Boolean);
  begin
    PrintAcctNo(account.AcctNo);
    WriteUTF8('  ');
    PrintAcctType(Account.AccountType);
    Case Account.DrCr of Cr:TextColor(Red) end;
    WriteUTF8(chr(VK_TAB));
    PrintDrCr(account.DrCr);
//    Write(chr(VK_TAB) + IntToStr(Account.Balance):8);
    WriteUTF8(chr(VK_TAB) + IntToStr(Account.Balance));
    TextColor (Yellow);
    WriteUTF8(chr(VK_TAB) + account.Currency);
       TextColor(white);
       If ShowGUID then
         WriteUTF8(chr(VK_TAB) + GUIDToString(Account.AcctGUID));
       WriteUTF8(chr(VK_TAB) + Account.Text);
    WritelnUTF8;
  end;

Procedure WriteErr(const Err:UTF8String);
  begin
    TextColor(Red);
    WritelnUTF8(Err);
    NormVideo;
  end;

Function GetText(const Prompt:UTF8String):UTF8String;
  begin
    NormVideo;
    WriteUTF8(Prompt);
    HighVideo;
    Readln(Result);
    NormVideo;
  end;

Function GetAccount(const Prompt:UTF8String):Longint;
  var
   tmpstr:utf8string;
   done:boolean=false;
   TestAccount:TLedgerAccount;
   SearchAL:TAccountList;
  begin
   repeat
    NormVideo;
    if prompt = '' then
      Write('Please enter Account:')
    else
      Write(Prompt);
    HighVideo;
    Readln(tmpstr);
    NormVideo;
    case TryStrToInt(tmpstr, result) of
      true:begin
        TestAccount := TLedgerAccount.Create;
        Try
          Done := TestAccount.Load(result);
          If Not Done then
            begin
              WriteErr('Account #' + IntToStr(Result) + ' does not exist.');
              Result := -1;
            end;
        finally
          TestAccount.free;
        end;

      end; // of True
      false: begin
        WriteErr(tmpstr + ' is not a valid account number');
        SearchAL := TAccountList.Create(tmpstr);
        try
          Case SearchAL.EOF  of
            True:WriteErr('No Matching Account(s).');
            False:Begin
              TestAccount := SearchAL.GetFirstAccount;
              If not Assigned (TestAccount) then // no match
                 begin
                   WriteErr('No matching accounts found. (Error)');
                 end
              else // We found a matching account
                begin
                  Writeln(testAccount.AcctNo, TestAccount.Text);
                end;
            End; // of CASE false
          end; // of CASE
        finally
          SearchAL.Free;
        end;
      end; // of FALSE
    end; // of case
   until done;
  end; // of FUNCTION

Procedure PrintAcctNo(AcctNo:Integer);
  Const
   W=5; // Width
  begin
   TextColor(Green);
   Write(AddChar('0', IntToStr(AcctNo), W));
  end;


Procedure PrintAcctType(AccountType:TAcctType);
  begin
    TextColor(White);
    Write(  Abap_Translate(
          IntToStr(Ord(AccountType)),
          AcctTransMap)
          ) ;
  end;

Procedure PrintDrCr(Const DrCr:TDrCr);
  begin
      Case DrCr of 
        Cr:TextColor(Red);
        Dr:TextColor(Green);
      end;
      WriteUTF8(abap_translate(IntToStr(Ord(DrCr)), '0D1C'));
  end;

Procedure PrintTransRow(Const Row:Integer);
  begin
   TextColor(White);
   WriteUTF8(Row);
  end;

Procedure PrintAmount(Const Amount:Integer);
  begin
   TextColor(White);
   WriteUTF8(Amount);
  end;


initialization
  NormVideo;
  // Startup code goes here
{$IFDEF WINDOWS}
 //SetConsoleOutputCP(CP_UTF8);
{$ENDIF}

finalization
  NormVideo;

end.
