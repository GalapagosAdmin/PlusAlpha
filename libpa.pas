unit libpa;
// Contains common items to be used by various parts of PlusAlpha
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TLangCode=String[2];
  TDrCr = (Dr, Cr);
  TAcctType = (atAsset:=0, atLiability:=1, atEquity:=2, atExpense:=3,
               atIncome:=4, atRevenue:=5, atPlaceholder:=6, atOther:=7);
  TUTF8String=UTF8String;
  Tstr8=String[8];
  TInteger=integer;



  TPAUtility = Class(TObject)
  //  Class Function Display
  end;

     CONST
       // map for DB account type codes to TAcctType
       AcctTransMap='A0L1C2Q2E3X3I4R5P7O77';
       // Map from TAcctType to DB Codes
       AcctTransMapRev='0A1L2C3E4I5R6P7OO';


  // Converts the "pretty" form of an account text (which includes the number
  // and a text description of the account in the current language)
  // back into the account number.
  Function ActToInt(AccountText:TUTF8String):Integer;
  // Converts DateTime from Pascal Internal format to DB format (NUMC)
  Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;
  // Converts input string into an output string using mask
  Function abap_translate(const instring, Mask:UTF8String):UTF8String;


implementation


// Converts input string into an output string using mask
// similar to ABAP function of the same name
  Function abap_translate(const instring, Mask:UTF8String):UTF8String;
    var
     p:integer;
     c:char;
     default:Boolean;
    begin
     Result := '';
     default := odd(length(Mask));
     for c in instring do
       begin
         p := pos(c, mask);
         if p > 0 then
           result := Result + copy(mask, p+1, 1)
         else
           begin
             If default then
               result := result + copy(mask, length(mask), 1)
             else
               result := Result + c;

           end;
       end;
    end;

 // converts account display text back to integer
 Function ActToInt(AccountText:TUTF8String):Integer;
   var
    p:integer;
   begin
    p := pos('-', AccountText);
    if p < 2 then exit(-1);
    Dec(p, 1);
    Result := StrToInt(Trim(copy(AccountText,1,p)));
   end;

   Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;
     begin
       DateTimeToString(Result, 'yyyymmdd', Date)
     end;

initialization





end.

