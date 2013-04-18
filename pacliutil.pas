unit pacliutil; // PlusAlpha Command Line Interface Utilities
{$MODE DELPHI } // for RESULT

interface

uses  sysutils, LibPa;

Procedure WriteErr(const Err:UTF8String);
//Function GetAccount:Longint;
Function GetAccount(const Prompt:UTF8String):Longint;
Function GetText(const Prompt:UTF8String):UTF8String;
Procedure PrintAcctNo(AcctNo:Integer);
Procedure PrintAcctType(AccountType:TAcctType);
Procedure PrintDrCr(Const DrCr:TDrCr);

implementation

uses
  StrUtils, Crt;

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
      true:done := true;
      false: WriteErr(tmpstr + ' is not a valid account integer');
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
