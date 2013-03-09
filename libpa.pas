unit libpa;
// Contains common items to be used by various parts of PLusAlpha
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TCurrCode=String[3];
  TDrCr = (Dr, Cr);
  TAcctType = (atAsset, atLiability, atEquity, atExpense, atIncome, atRevenue, atPlaceholder);
  TUTF8String=UTF8String;
  Tstr8=String[8];
  TInteger=integer;



  TPAUtility = Class(TObject)
  //  Class Function Display
  end;

  Function ActToInt(AccountText:TUTF8String):Integer;
  Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;


implementation

// Converts input string into an output string using mask
  Function translate(const instring, Mask:UTF8String):UTF8String;
    var
     p:integer;
     c:char;
    begin
     Result := '';
     for c in instring do
       begin
         p := pos(mask, c);
         if p > 0 then
           result := Result + copy(mask, p, 1)
         else
           result := Result + c;
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

