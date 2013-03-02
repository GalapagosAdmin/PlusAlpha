unit libpa;

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

