unit libpa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb;

Type
  TCurrCode=String[3];
  TDrCr = (Dr, Cr);
  TAcctType = (atAsset, atLiability, atEquity, atExpense, atIncome, atRevenue, atPlaceholder);
  TUTF8String=UTF8String;
  Tstr8=String[8];
  TInteger=integer;

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
       Property EffDate:TDate read _EffDate write _EffDate;
       Property Posted:Boolean read _Posted write _Posted;
       Function Insert:Boolean;
       Function Validate:Boolean;
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
//     _bal:Integer;         // Ledger Balance
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
    Function Insert:boolean;
    Function Validate:Boolean;

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
     Function Validate:Boolean;

 end;

  TPAUtility = Class(TObject)
  //  Class Function Display
  end;

  Function ActToInt(AccountText:TUTF8String):Integer;
  Function DateTimeToYYYYMMDD(Const Date:TDateTime):AnsiString;

var
 // JournalHeader:TJournalHeader;
  CompleteJournalEntry:TCompleteJournalEntry;

implementation

 uses sdfdata, db, paLedger, paDatabase;


  Constructor TJournalHeader.Create;
    begin
      inherited;
      _TransNo := 0;
      // other stuff goes here.

    end;

  Function TJournalHeader.Validate:boolean;
    begin
      Result := true;
      If length(_memo) = 0 then
        Result := False;
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
         + '"TRANSNO", "MEMO", "EFF_DATE", "ENT_DATE") '
         + 'values ( :TransNo, :Memo, :effdate, :entdate )';

     SqlQuery1.ParamByName('TransNo').AsInteger := _TransNo;
     SqlQuery1.ParamByName('Memo').AsString := _Memo;
     SqlQuery1.ParamByName('entdate').AsString := DateTimetoYYYYMMDD(now);
     SqlQuery1.ParamByName('effdate').AsString := DateTimetoYYYYMMDD(_EffDate);

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
      self.AcctNo := -1;
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

  Function TJournalDetailEntry.validate:boolean;
    begin
      Result := (Self.AcctNo <> -1);
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


 Function TCompleteJournalEntry.GetHighWaterMark:Integer;
  begin
   self.UpdateHighWaterMark;
   result := _TransHighWaterMark;
  end;

  Function TCompleteJournalEntry.Validate:boolean;
    var
      i:integer;
    begin
      result := _JournalHeader.Validate;
      If result then result := self.IsBalanced;
      If result then
        for i := low(_JournalDetailEntries) to High(_JournalDetailEntries) do
          begin
             if not _JournalDetailEntries[i].Validate then
               begin
                 result := false;
                 exit;
               end;
          end;

      // Then loop through the detail records and validate them one by one
      // if anything fails, then the entire result fails
    end;

 Function TCompleteJournalEntry.Insert:boolean;
   var
     i:integer;
     tmpAcct:TLedgerAccount;
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
             Begin
               _JournalDetailEntries[i].insert;
               // Update Ledger
               tmpAcct := AccountList.GetAccountNo(_JournalDetailEntries[i].AcctNo);
               if assigned(tmpAcct) then
                 begin
                   // Update the balance
                   // FIXME ignoring dr/cr for the moment for testing updates
                   tmpAcct.Balance:=tmpAcct.Balance + _JournalDetailEntries[i]._amount;
                   tmpAcct.Synch;
                 end;
             end;

           // Tidy Up
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



 // JournalHeader := TJournalHeader.Create;
  CompleteJournalEntry := TCompleteJournalEntry.Create;


end.

