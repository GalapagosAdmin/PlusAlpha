unit pacliutil; // PlusAlpha Command Line Interface Utilities
{$MODE DELPHI } // for RESULT

interface

uses  sysutils, LibPa, paLedger;

Procedure WriteErr(const Err:UTF8String);
//Function GetAccount:Longint;
Function  GetAccount(const Prompt:UTF8String):Longint;
Function  GetText(const Prompt:UTF8String):UTF8String;
Procedure PrintAcctNo(AcctNo:Integer);
Procedure PrintAcctType(AccountType:TAcctType);
Procedure PrintDrCr(Const DrCr:TDrCr);
Procedure PrintAccount(Const Account:TLedgerAccount);

implementation

uses
  StrUtils, Crt;

Procedure PrintAccount(Const Account:TLedgerAccount);
  begin
    PrintAcctNo(account.AcctNo);
    Write('  ');
    PrintAcctType(Account.AccountType);
    Case Account.DrCr of Cr:TextColor(Red) end;
    Write(Chr(9));
    PrintDrCr(account.DrCr);
    Write(Chr(9), IntToStr(Account.Balance):8);
    TextColor (Yellow);
    Write(
       chr(9), account.Currency);
       TextColor(white);
       Write(
       chr(9), Account.Text);
    Writeln;
  end;

Procedure WriteErr(const Err:UTF8String);
  begin
    TextColor(Red);
    Writeln(Err);
    NormVideo;
  end;

Function GetText(const Prompt:UTF8String):UTF8String;
  begin
    NormVideo;
    Write(Prompt);
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
      Write(abap_translate(IntToStr(Ord(DrCr)), '0D1C'));
  end;

initialization
  // Startup code goes here
finalization
  NormVideo
end.
