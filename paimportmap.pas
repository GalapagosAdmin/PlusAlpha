unit paImportMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MD5, libpa;

Type
TImportMapEntry = Class(TObject)   // Ledger Account
protected
    _IntGUID:TGUID; // GUID for this Interface.
    _drcr:Tdrcr; // debit/credit indicator for Entry to make.
    _dirty:boolean; // Indicates if synch to db is required
    _new:boolean;    // specifies that this entry has not yet been written to the database
    _Memo:UTF8String; // Memo to search for
    _MemoHash:TMD5Digest; // Hash of memo to search for.
    _AcctGUID:TGUID;  // Account GUID as String;
private
  _HasIntGUID:Boolean; // Internal Interface GUID.
  _HasAcctGUID:Boolean; // Internal Account GUID.
//    Procedure SetDrCr(NewDrCr:TDrCr);
    Procedure SetMemo(NewMemo:TUTF8String);
//    Procedure SetAcctGUID(NewAcctGUID:TGUID);
    Property HasAcctGUID:Boolean read _HasAcctGUID;
//    Function Insert:Boolean;
//    Function Update:Boolean;
    Function Select:Boolean;
  Public
    Constructor create;
    Property AcctGUID:TGUID read _AcctGUID;// write SetAcctGUID;
    Property Memo:TUTF8String read _Memo write SetMemo;
//    Property DrCr:Tdrcr read _drcr write SetDrCr;
    Function Load(Const InterfaceGUID:TGUID; Const NewMemo:UTF8String):Boolean;
//    Function Synch:boolean;
//    Procedure Commit;
end;

Var
  ImportMapEntry:TImportMapEntry;

implementation

uses
    paDatabase, SQLDB, LazLogger;

Constructor TImportMapEntry.Create();
  begin
    _HasAcctGUID := False;
    _New   := True;
    _Dirty := False
  end;

Procedure TImportMapEntry.SetMemo(NewMemo:TUTF8String);
  begin
    _Memo := NewMemo;
    _MemoHash := MD5String(_Memo);
  end;

Function TImportMapEntry.Load(Const InterfaceGUID:TGUID; Const NewMemo:UTF8String):Boolean;
  begin
    Memo := NewMemo;
    _IntGUID := InterfaceGUID;
    _HasIntGUID := True;
    _Dirty := True;
    _New := False;
    Result := Select;
  end;


Function TImportMapEntry.Select:Boolean;
var
//  i:TInteger;
  SQLQuery1:TSQLQuery;
  DrBal, CrBal:TInteger;
  rows:integer;
  test:boolean;
begin
//  DebugLn(IntToStr(AccountNo));
SQLQuery1 := TSQLQuery.Create(nil);
SQLQuery1.Transaction := SQLTransaction1;
SQLQuery1.SQL.Text := 'select AcctGUID, DrCr, Ignore '
+ ' from ACCOUNTMAP where IntGUID = :IntGUID '
+ ' and MemoHash = :MemoHash ';
//   + ' from ledger where rowid = :rowid';
//test := assigned(SQLTransaction1);
SqlQuery1.ParamByName('IntGUID').AsString := GuidToString(_IntGUID);
SqlQuery1.ParamByName('MemoHash').AsString := MD5Print(_MemoHash);

SQLQuery1.Open;
//writeln(SQLQuery1.SQL.Text);
//writeln( GuidToString(_IntGUID));
//writeln(MD5Print(_MemoHash));
rows := SQLQuery1.RowsAffected;
// The following doesn't work for SELECT statements
//  Result := Rows = 1;
// Not working for some reason, rows always = 0, EOF = True.
//   While not SQLQuery1.EOF do
Result := Not SQLQuery1.EOF;
If not SQLQuery1.EOF then
 begin
   With SQLQuery1 do
     begin
       try
         self._AcctGUID := StringToGUID(FieldByName('AcctGuid').AsString);
       except
        Raise Exception.Create('TLedgerAccount.Load:Error while reading acctguid');
       end;
       try
        self._DrCr := TDrCr(StrtoInt(abap_translate(
                                             FieldByName('DrCr').AsString[1], DrCrTransMap)));
       except
        DebugLn('TLedgerAccount.Load:Error while DrCr');
       end;
       try
        // Read Ignore Field
       except
        // Error reading DrCr Field
       end;
//     SQLQuery1.Next;
       // No need to set AcctGUID since that's what we are using to search with.
     end; // of WITH
 end; // of IF
 SQLQuery1.Close;
 SQLQuery1.Destroy;

   // We just loaded the data, so it's clean.
  _Dirty := False;
  _New := False;
    // Duplicate AccNo version of Load, but refactor code first to avoid too
    // much duplicate code.
  end;


initialization
 ImportMapEntry := TImportMapEntry.create;

finalization
  ImportMapEntry.Free;


end.

