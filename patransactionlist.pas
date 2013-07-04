unit paTransactionList;
// Generates a list of transactions
// Example: Used for listing all of the transactions involving a certain account
// for a certain period of time.
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TResultEntry=Record
    TransactionNo:Integer;
    TransactionGUID:TGUID;
    TransactionRow:Integer;
    HasGUID:Boolean;
  end;

  TResultArray=Array of TResultEntry;
  TTransactionList=Class(TObject)
  Private
    _SearchAccount:Integer; // Account of Interest
    _SearchAccountGUID:TGUID; // Account of Interest
    _Array:TResultArray;
    Procedure Select;
    Procedure SetAccount(const NewAccount:Integer);
    Procedure SetAccount(const NewAccount:TGUID); overload;
  Public
    Property Account:Integer read _SearchAccount write SetAccount; deprecated;
    Property AccountGUID:TGUID read _SearchAccountGUID write SetAccount;
    Property TransNos:TResultArray read _Array;
  end;

 var
    TransactionList : TTransactionList;

implementation

Uses
   sqldb, paDatabase;

Procedure TTransactionList.Select;
var
   SQLQuery1:TSQLQuery;
  begin
    SetLength(_Array, 0);
    SQLQuery1 := TSQLQuery.Create(nil);

  //Journal Header should always be inserted first, so it's safer to take that
  // number
  With SQLQuery1 do
    begin
      Transaction := SQLTransaction1;
      // If we have an account number, use that
      If _SearchAccount >= 0 then
        begin
          SQL.Text := 'SELECT TRANSNO, TRANSROW, TRANSGUID FROM JOURNAL WHERE DRACCTCD = :AcctNo ';
          ParamByName('AcctNo').AsInteger := _SearchAccount;
        end
      else  // Otherwise, the account GUID is set, so use that
        begin
          SQL.Text := 'SELECT TRANSNO, TRANSROW, TRANSGUID FROM JOURNAL WHERE ACCTGUID = :AcctGUID ';
          ParamByName('AcctGUID').AsString := GuidToString(_SearchAccountGUID);
        end;
      Open;
      While not EOF do
        begin
           SetLength(_Array, Length(_Array)+1);
           _Array[High(_Array)].TransactionNo := FieldByName('TRANSNO').AsInteger;
           _Array[High(_Array)].TransactionRow := FieldByName('TRANSROW').AsInteger;
           try
             _Array[High(_Array)].TransactionGUID := StringToGuid(FieldByName('TRANSGUID').AsString);
             _Array[High(_Array)].HasGuid := True;
           EXCEPT
             _Array[High(_Array)].HasGUID := False;
           end;
           next;
        end;
      Close;
      Destroy;
    end;
  end;

Procedure TTransactionList.SetAccount(const NewAccount:Integer);
  begin
    _SearchAccount := NewAccount;
    // Set a Null GUID, if there is such a thing?
    Select;
  end;

Procedure TTransactionList.SetAccount(const NewAccount:TGUID);
  begin
    _SearchAccountGUID := NewAccount;
    // Disable the old style account number so we won't use it
    _SearchAccount := -1;
    Select;
  end;

initialization
 TransactionList := TTransactionList.Create;

end.

