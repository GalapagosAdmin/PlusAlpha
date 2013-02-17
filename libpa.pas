unit libpa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, sqlite3conn;

Type
  TCurrCode=String[3];
  TDrCr = (Dr, Cr);
  TAcctType = (atAsset, atLiability, atEquity, atExpense, atIncome, atRevenue, atPlaceholder);
  TUTF8String=UTF8String;
  Tstr8=String[8];

 TJournalHeader = class(TObject)
   private
     //var
       _TransNo:Integer;     // Transaction number - Matches with journal detail
       _Memo:TUTF8String;    // Memo in entry language
       _EffDate:TDate;       // Effective Date  GMT
       _EffTime:TTime;       // Effective time  GMT
       _EntDate:TDate;       // Entry Date      GMT
       _EntTime:TTime;       // Entry Time      GMT
       _Posted:Boolean;      // Posted Flag
       _CurrCode:TCurrCode;  // Currency Code
     public
       Constructor Create; //overload;
//       Destructor Destroy; override;
       Procedure Commit;
//     Procedure Revert;
       Property HdrMemo:TUTF8String read _memo write _memo;
       Property HdrTransNo:Integer read _TransNo write _TransNo;
       Property HdrPosted:Boolean read _Posted write _Posted;
       Property CurrCode:TCurrCode read _CurrCode write _CurrCode;
       Function Insert:Boolean;
 end;  // of TJournalHeader



 TJournalDetailEntry = class(TObject)
   private
   //var
     _TransNo:Integer;     // Transaction number - Matches with header
     _TransRow:Integer;    // Entry number for this Transaction number
     _amount:Integer;      // Amount, without decimal point
     _currency:TCurrCode;  // Currency Code
     _drcr:Tdrcr;          // Debit/Credit Indicator
     _acctno:Integer;      // Account no. (Internal)
     _bal:Integer;         // Ledger Balance
     _dirty:boolean;       // Needs Database synch
     _Text:TUTF8String;
   public
    Constructor Create; //overload;
//       Destructor Destroy; override;
//     Procedure Commit;
//     Procedure Revert;
    Property TransNo:Integer read _TransNo write _TransNo;
    Property AcctNo:Integer read _acctno write _acctno;
    Property Amount:Integer read _amount write _amount;
    Property DrCr:Tdrcr read _drcr write _drcr;
    Property Currency:TCurrCode read _currency write _currency;
    Property TransRow:Integer read _TransRow write _TransRow;
    Property Text:TUTF8String read _Text write _Text;
    function insert:boolean;

 end;

// Class for holding the complete transaction, header and detail.
 TCompleteJournalEntry=Class(TObject)
   private
     _TransNo:Integer;     // Transaction number - Matches with header
     _Rows:Integer; // Number of Line Items
     // Two entries for testing
     _JournalDetailEntry1 : TJournalDetailEntry;
     _JournalDetailEntry2 : TJournalDetailEntry;
     // Use Dynamic Array or collection later
//     _JournalDetailEntries : Array[0..1] of TJournalDetailEntry;
     _TotalDr:Integer;
     _TotalCr:Integer;
     _TransHighWaterMark:Integer;
     Procedure UpdateHighWaterMark;
     Procedure UpdateDrCr;
     Procedure TransNoSet(TransNo:Integer);
     Function GetHighWaterMark:Integer;
   Public
    _JournalHeader : TJournalHeader;
    _JournalDetailEntries : Array[0..1] of TJournalDetailEntry;
     Constructor Create; //overload;
     Property Rows:Integer Read _Rows;
     Property TransNo:Integer read _TransNo write TransNoSet;
     Function IsBalanced:Boolean;
     Function Insert:Boolean;
     Property HighWaterMark:Integer read GetHighWaterMark;

 end;

  TAccount = Class(TObject)
    _AcctNo:Integer; // Internal Account No.
    _Text:TUTF8String; // Account Description
    _AcctType:TAcctType;
    _currency:TCurrCode;  // Currency Code
    _bal:Integer; // Ledger balance
    _drcr:Tdrcr; // debit/credit indicator
    _dirty:boolean; // Indicates if synch to db is required

  end;

  TAccountList = Class(TObject)
    private
      _AccountList : Array of TAccount;
    public
     Constructor Create; //overload;
     Procedure AddAccount(const AcctNo:Integer; Currency:TCurrCode;
                                   AccountType:TAcctType; AcctText:TUTF8String;
                                   DrBal:Integer; CrBal:Integer);
     Function AccountStringList:TStringList;
  end;


  Function ActToInt(AccountText:TUTF8String):Integer;
  Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;

var
  JournalHeader:TJournalHeader;
  CompleteJournalEntry:TCompleteJournalEntry;
  AccountList:TAccountList;

implementation

 uses sdfdata, db;

 var
        SQLite3Connection1:TSQLite3Connection;
        SQLTransaction1:TSQLTransaction;


  Constructor TJournalHeader.Create;
    begin
      inherited;
      _TransNo := 0;
      // other stuff goes here.

    end;

  Function TJournalHeader.Insert:boolean;
   var
     SQLQuery1:TSQLQuery;
     TmpStr : TUTF8String;
   begin
     // validations
     If self._TransNo = 0 then exit(false);
     if self._Memo = '' then exit(False);

     SQLQuery1 := TSQLQuery.Create(nil);
     SQLQuery1.Transaction := SQLTransaction1;
     SQLQuery1.SQL.Text := 'insert into "main"."JOURNALHDR" ('
         + '"TRANSNO", "MEMO", "ENT_DATE") '
         + 'values ( :TransNo, :Memo, :entdate )';

     SqlQuery1.ParamByName('TransNo').AsInteger := _TransNo;
     SqlQuery1.ParamByName('Memo').AsString := _Memo;
     SqlQuery1.ParamByName('entdate').AsString := DateTimetoYYYYMMDD(now);

     SQLQuery1.ExecSQL;
     SQLQuery1.Close;
     Result := (SQLQuery1.RowsAffected = 1);
     SQLQuery1.Destroy;

   end;

  Procedure TJournalHeader.Commit;
    begin
      // write out to database
      //insert into "main"."JOURNALHDR" ( "MEMO", "TRANSNO") values ( 'test entry 6', 6)
    end;

  Constructor TJournalDetailEntry.Create;
    begin
      _Currency := 'JPY';
    end;

  Function TJournalDetailEntry.insert:boolean;
   var
     SQLQuery1:TSQLQuery;
     TmpStr : TUTF8String;
   begin
     SQLQuery1 := TSQLQuery.Create(nil);
     SQLQuery1.Transaction := SQLTransaction1;
     SQLQuery1.SQL.Text := 'insert into "main"."JOURNAL" ('
         + '"TRANSNO", "CRTRNSTSCD", "CRCURRKEY", "DRAMT", "KATASA", "TRANSROW",'
         + '"TRNSKBNCD", "TEXT", "TEXTKEY", "CRAMT", "DRCURRKEY", "DRTRNSTSCD", '
         + ' "DRACCTCD", "CRACCTCD") '
         + 'values ( :TransNo, :CrTrnStsCd, :CrCurrKey, :DrAmt, :Katasa, :TransRow, '
         + ':TrnsKbnCd, :Text, :TextKey, :CrAmt, :DrCurrKey, :DrTrnStsCd, 0, 0)';

     SqlQuery1.ParamByName('TransNo').AsInteger:=_TransNo;
     SqlQuery1.ParamByName('CrTrnStsCd').AsString:='X';
     SqlQuery1.ParamByName('CrCurrKey').AsString:=_Currency;
     SqlQuery1.ParamByName('DrAmt').AsInteger:=_Amount;
     SqlQuery1.ParamByName('Katasa').AsString:='H';
     SqlQuery1.ParamByName('TransRow').AsInteger:=_TransRow;

     SqlQuery1.ParamByName('TrnsKbnCd').AsString:='X';
     SqlQuery1.ParamByName('Text').AsString:=_Text;
     SqlQuery1.ParamByName('TextKey').AsInteger:=0;
     SqlQuery1.ParamByName('CrAmt').AsInteger:=_Amount;
     SqlQuery1.ParamByName('DrCurrKey').AsString:=_Currency;
     SqlQuery1.ParamByName('DrTrnStsCd').AsString:='X';

     SQLQuery1.ExecSQL;
     SQLQuery1.Close;

   end;


 Constructor TCompleteJournalEntry.Create;
   begin
      _JournalHeader := TJournalHeader.Create;
      _Rows := 2; // Static for now
      // This can be made dynamic to support more than 2 entries
      _JournalDetailEntry1 := TJournalDetailEntry.Create;
      _JournalDetailEntry2 := TJournalDetailEntry.Create;
      // Enable array access now for external use
      _JournalDetailEntries[0] := _JournalDetailEntry1;
      _JournalDetailEntries[1] := _JournalDetailEntry2;
   end;

 Function TCompleteJournalEntry.Insert:boolean;
   var
     i:integer;
   begin
     // Assume Failure
      Result := False;
      // Set up the transaction entry number
      UpdateHighWaterMark;
      TransNoSet(_TransHighWaterMark+1);
      // Insert the Journal Header
      If _JournalHeader.Insert then
         begin
          // Insert the journal Entry Rows
           for i := low(_JournalDetailEntries) to High(_JournalDetailEntries) do
             _JournalDetailEntries[i].insert;
          SQLTransaction1.commit;
          Result := True;
         end;
   end;

 Procedure TCompleteJournalEntry.UpdateDrCr;
   begin
     _TotalDr := 0;
     _TotalCr := 0;
     If _JournalDetailEntry1._drcr = Dr then
       _TotalDr := _TotalDr + _JournalDetailEntry1._amount
     else
       _TotalCr := _TotalCr + _JournalDetailEntry1._amount;
    // Change this into a loop when we want to support more than 2 detail entries
      If _JournalDetailEntry2._drcr = Dr then
       _TotalDr := _TotalDr + _JournalDetailEntry2._amount
     else
       _TotalCr := _TotalCr + _JournalDetailEntry2._amount

   end;

 Procedure TCompleteJournalEntry.TransNoSet(TransNo:Integer);
   begin
     // Update our own internal status
     _TransNo := TransNo;
     // Update our children to be in synch
     _JournalHeader._TransNo:=_TransNo;
     _JournalDetailEntry1._TransNo:=_TransNo;
     _JournalDetailEntry2._TransNo:=_TransNo;
   end;

 Function TCompleteJournalEntry.IsBalanced:boolean;
   begin
     UpdateDrCr;
     Result := _TotalDR = _TotalCr;
   end;

 Procedure TCompleteJournalEntry.UpdateHighWaterMark;
   var
    SQLQuery1:TSQLQuery;
   begin
     SQLQuery1 := TSQLQuery.Create(nil);
     SQLQuery1.Transaction := SQLTransaction1;

   //Journal Header should always be inserted first, so it's safer to take that
   // number
   SQLQuery1.SQL.Text := 'select max(transno) as hwm from journalhdr';
   SQLQuery1.open;
   If not SQLQuery1.EOF then
     _TransHighWaterMark := SqlQuery1.FieldByName('hwm').AsInteger;
   SQLQuery1.Close;
   SQLQuery1.Destroy;

   end;

 Procedure TAccountList.AddAccount(const AcctNo:Integer; Currency:TCurrCode;
                                           AccountType:TAcctType; AcctText:TUTF8String;
                                           DrBal, CrBal:Integer);
   var
     TmpAcct:TAccount;
   begin
     TmpAcct := TAccount.Create;
     // Add entry to the in-memory array
     SetLength(_AccountLIst, Length(_AccountList)+1);

     TmpAcct._AcctNo := AcctNo;
     TmpAcct._currency := Currency;
     TmpAcct._AcctType := AccountType;
     TmpAcct._Text := AcctText;
     // Convert the DR/CR balances to a single balance and DrCr indicator
     If DrBal > 0 then
       begin
        TmpAcct._bal := DrBal;
        TmpAcct._DRCR := Dr;
       end
     else
      begin
        TmpAcct._bal := CrBal;
        TmpAcct._DRCR := Cr;
      end;

   //  _AccountList[0] := TmpAcct;
    _AccountList[High(_AccountList)] := TmpAcct;

   end;

 Constructor TAccountList.Create();
   var
//     FDataset: TSdfDataset;
     i:integer;
     SQLQuery1:TSQLQuery;
//     SQLTransaction1:TSQLTransaction;
//     SQLite3Connection1:TSQLite3Connection;
     TmpStr : TUTF8String;
   begin


   SQLQuery1 := TSQLQuery.Create(nil);
//   SQLTransaction1 := TSQLTransaction.Create(nil);
//   SQLite3Connection1 := TSQLite3Connection.Create(nil);

//   SQLite3Connection1.DatabaseName :=  '/Users/shiruba/develop/plusalpha/plusalpha.sqlite';
//   SQLite3Connection1.Connected := True;
//   SQLTransaction1.DataBase := SQLite3Connection1;
   SQLQuery1.Transaction := SQLTransaction1;
   SQLQuery1.SQL.Text := 'select AcctNo, DrBal, CrBal, CurrKey, Text from ledger';
   SQLQuery1.open;
   While not SQLQuery1.EOF do
    begin
      With SQLQuery1 do
      AddAccount(StrToInt(FieldByName('AcctNo').AsString),
                 FieldByName('CurrKey').AsString,
                 atAsset, FieldByName('Text').AsString,
                 FieldByName('DrBal').AsInteger,
                 FieldByName('CrBal').AsInteger
                 );
      SQLQuery1.Next;
    end;
   SQLQuery1.Close;
   SQLQuery1.Destroy;

   end;

 Function TAccountList.AccountStringList:TStringList;
   var
     i:Integer;
   begin
     Result := TStringList.Create;
     for i := low(_AccountList) to high(_AccountList) do
       Result.Append(IntToStr(_AccountList[i]._AcctNo) + ' - ' + _AccountList[i]._Text);
   end;

 Function TCompleteJournalEntry.GetHighWaterMark:Integer;
  begin
   self.UpdateHighWaterMark;
   result := _TransHighWaterMark;
  end;

 Function ActToInt(AccountText:TUTF8String):Integer;
   begin
    Result := StrToInt(Trim(copy(AccountText,1,2)));
    //Works in the following two cases.  If the account number hits 3 digits,
    // We'll have to search for the dash.
    // 12
    // 1 - Account
    // 10 - Account
   end;

   Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;
     begin
       DateTimeToString(Result, 'yyyymmdd', Date)
     end;

initialization

  SQLite3Connection1 := TSQLite3Connection.Create(nil);
  SQLite3Connection1.DatabaseName :=  'plusalpha/plusalpha.sqlite';
  SQLite3Connection1.Connected := True;
  SQLTransaction1 := TSQLTransaction.Create(nil);
  SQLTransaction1.DataBase := SQLite3Connection1;


  JournalHeader := TJournalHeader.Create;
  CompleteJournalEntry := TCompleteJournalEntry.Create;
  AccountList := TAccountList.Create;


end.

