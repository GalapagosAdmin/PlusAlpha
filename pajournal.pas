unit paJournal;
// Objects related to Journal Entries

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, libpa, sqldb;

type
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
     _TransRow:Integer;    // Entry number for this Transaction number (line item)
     _amount:Integer;      // Amount, without decimal point
     _currency:TCurrCode;  // Currency Code - Should match account being used
     _drcr:Tdrcr;          // Debit/Credit Indicator
     _acctno:Integer;      // Account no. (Internal)
//     _bal:Integer;         // Ledger Balance
     _dirty:boolean;       // Needs Database synch
     _new:boolean;    // specifies that this entry has not yet been written to the database
     _Text:TUTF8String;
     Procedure SetAcctNo(AcctNo:Integer);
      // performs a database update if the record already existed.
     Function Update:Boolean;
     Function Insert:boolean;
   public
    Constructor Create; //overload;
//       Destructor Destroy; override;
//     Procedure Commit;
//     Procedure Revert;
    Property TransNo:Integer read _TransNo write _TransNo;
    Property AcctNo:Integer read _acctno write SetAcctNo;
    Property Amount:Integer read _amount write _amount;
    Property DrCr:Tdrcr read _drcr write _drcr;
    Property Currency:TCurrCode read _currency;// write _currency;
    Property TransRow:Integer read _TransRow write _TransRow;
    Property Text:TUTF8String read _Text write _Text;
    Function Validate:Boolean;
    Function Synch:boolean;
 end;

// Class for holding the complete transaction, header and detail.
 TCompleteJournalEntry=Class(TObject)
   private
     _TransNo:Integer;     // Transaction number - Matches with header
     _Rows:Integer; // Number of Line Items
     // Two entries for testing
  //   _JournalDetailEntry1 : TJournalDetailEntry;
  //   _JournalDetailEntry2 : TJournalDetailEntry;
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

    var
      // JournalHeader:TJournalHeader;
       CompleteJournalEntry:TCompleteJournalEntry;


implementation

uses sdfdata, db, paLedger, paDatabase, paCalculator;


  Constructor TJournalHeader.Create;
    begin
      inherited;
      _TransNo := -1;
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
     If self._TransNo < 0 then exit(false);
     if self._Memo = '' then exit(False);

     SQLQuery1 := TSQLQuery.Create(nil);
     SQLQuery1.Transaction := SQLTransaction1;
     SQLQuery1.SQL.Text := 'insert into "main"."JOURNALHDR" ('
         + '"TRANSNO", "MEMO", "EFF_DATE", "ENT_DATE", POSTED) '
         + 'values ( :TransNo, :Memo, :effdate, :entdate, :posted )';

     SqlQuery1.ParamByName('TransNo').AsInteger := _TransNo;
     SqlQuery1.ParamByName('Memo').AsString := _Memo;
     SqlQuery1.ParamByName('entdate').AsString := DateTimetoYYYYMMDD(now);
     SqlQuery1.ParamByName('effdate').AsString := DateTimetoYYYYMMDD(_EffDate);
     SqlQuery1.ParamByName('posted').AsInteger := Ord(_Posted);

     SQLQuery1.ExecSQL;
     SQLQuery1.Close;
     Result := (SQLQuery1.RowsAffected = 1);
     SQLQuery1.Destroy;

   end;

  Procedure TJournalHeader.Commit;
    begin
      // write out to database
      //insert into "main"."JOURNALHDR" ( "MEMO", "TRANSNO") values ( 'test entry 6', 6)
      // NoOp because we don't want a database commit here
    end;

  Constructor TJournalDetailEntry.Create;
    begin
      _Currency := 'XXX';
      self._AcctNo := -1;
// New should be set to false when after loading a record from the database so
// that the existing record can be updated inserting a new record.
      _new := True;
// Dirty should normally be set to false on load or creation, and then
// set to dirty if anything is changed.  Since the properties are only set to
// directly update variables for now, dirty has to be set to true all the time.
      _Dirty := True;
//      _Dirty := False;

    end;

  Procedure TJournalDetailEntry.SetAcctNo(AcctNo:Integer);
    var
      tmpAcct:TLedgerAccount;
    begin
      // Make sure we set the line item currency to the currency of the account
      // used.
      if AcctNo < 0 then exit;
      tmpAcct := AccountList.GetAccountNo(AcctNo);
      if Assigned(tmpAcct) then
        _Currency := tmpAcct.Currency
      else
        _Currency := 'XXX';
      _AcctNo := AcctNo;
      _Dirty := True;
    end;

  Function TJournalDetailEntry.Update:boolean;
  var
      SQLQuery1:TSQLQuery;
      TmpStr : TUTF8String;
      DrAmt, CrAmt:Integer;
    begin

      Case self.DrCr of
       Dr:begin
            DrAmt := self.Amount;
            CrAmt := 0;
          end;
       Cr:Begin
            CrAmt := self.Amount;
            DrAmt := 0;
          end;
      end;

      With SQLQuery1 Do
      try
      SQLQuery1 := TSQLQuery.Create(nil);
      Transaction := SQLTransaction1;
      SQL.Text := 'update "main"."JOURNAL" '
          + 'set  "CRTRNSTSCD"=:CrTrnStsCd, '
          + ' "CRCURRKEY"=:CrCurrKey, '
          + '"DRAMT"=:DrAmt,'
          + '"KATASA"=:Katasa,'
          + '"TRNSKBNCD"=:TrnsKbnCd, '
          + '"TEXT"=:Text,'
          + '"TEXTKEY"=:TextKey,'
          + '"CRAMT"=:CrAmt, '
          + '"DRCURRKEY"=:DrCurrKey, '
          + '"DRTRNSTSCD"=:DrTrnStsCd, '
          + '"DRACCTCD"=:DrAcctCd,'
          + '"CRACCTCD"=:CrAcctCd, '
          + 'ENT_DATE=:Ent_Date '
          + 'WHERE transno = :transno'
          + 'AND "TRANSROW"= :TransRow';

      ParamByName('TransNo').AsInteger:=_TransNo;
      ParamByName('CrTrnStsCd').AsString:='X';
      ParamByName('CrCurrKey').AsString:=_Currency;
      ParamByName('DrAmt').AsInteger:=DrAmt;
      ParamByName('Katasa').AsString:='H';
      ParamByName('TransRow').AsInteger:=_TransRow;

      ParamByName('TrnsKbnCd').AsString:='X';
      ParamByName('Text').AsString:=_Text;
      ParamByName('TextKey').AsInteger:=0;
      ParamByName('CrAmt').AsInteger:=CrAmt;
      ParamByName('DrCurrKey').AsString:=Currency;
      ParamByName('DrTrnStsCd').AsString:='X';

      // We only store one line item per row in the journal detail table,
      // which means we actually need only one field.  For now, we will store
      // the account number in both the CR and CR fields.
      ParamByName('DrAcctCd').AsInteger:=self.AcctNo;
      ParamByName('CrAcctCd').AsInteger:=self.AcctNo;

      ParamByName('Ent_Date').AsString:=DateTimeToYYYYMMDD(now);

      ExecSQL;
      Close;
      finally
        Destroy; // or else memory leak
      end; // of TRY..FINALLY
    end;

  Function TJournalDetailEntry.insert:boolean;
   var
     SQLQuery1:TSQLQuery;
     TmpStr : TUTF8String;
     DrAmt, CrAmt:Integer;
   begin

     Case self.DrCr of
      Dr:begin
           DrAmt := self.Amount;
           CrAmt := 0;
         end;
      Cr:Begin
           CrAmt := self.Amount;
           DrAmt := 0;
         end;
     end;

     try
     SQLQuery1 := TSQLQuery.Create(nil);
     SQLQuery1.Transaction := SQLTransaction1;
     SQLQuery1.SQL.Text := 'insert into "main"."JOURNAL" ('
         + '"TRANSNO", "CRTRNSTSCD", "CRCURRKEY", "DRAMT", "KATASA", "TRANSROW",'
         + '"TRNSKBNCD", "TEXT", "TEXTKEY", "CRAMT", "DRCURRKEY", "DRTRNSTSCD", '
         + ' "DRACCTCD", "CRACCTCD", ENT_DATE) '
         + 'values ( :TransNo, :CrTrnStsCd, :CrCurrKey, :DrAmt, :Katasa, :TransRow, '
         + ':TrnsKbnCd, :Text, :TextKey, :CrAmt, :DrCurrKey, :DrTrnStsCd, :DrAcctCd, '
         + ' :CrAcctCd, :Ent_Date)';

     SqlQuery1.ParamByName('TransNo').AsInteger:=_TransNo;
     SqlQuery1.ParamByName('CrTrnStsCd').AsString:='X';
     SqlQuery1.ParamByName('CrCurrKey').AsString:=_Currency;
     SqlQuery1.ParamByName('DrAmt').AsInteger:=DrAmt;
     SqlQuery1.ParamByName('Katasa').AsString:='H';
     SqlQuery1.ParamByName('TransRow').AsInteger:=_TransRow;

     SqlQuery1.ParamByName('TrnsKbnCd').AsString:='X';
     SqlQuery1.ParamByName('Text').AsString:=_Text;
     SqlQuery1.ParamByName('TextKey').AsInteger:=0;
     SqlQuery1.ParamByName('CrAmt').AsInteger:=CrAmt;
     SqlQuery1.ParamByName('DrCurrKey').AsString:=Currency;
     SqlQuery1.ParamByName('DrTrnStsCd').AsString:='X';

     // We only store one line item per row in the journal detail table,
     // which means we actually need only one field.  For now, we will store
     // the account number in both the CR and CR fields.
     SqlQuery1.ParamByName('DrAcctCd').AsInteger:=self.AcctNo;
     SqlQuery1.ParamByName('CrAcctCd').AsInteger:=self.AcctNo;

     SqlQuery1.ParamByName('Ent_Date').AsString:=DateTimeToYYYYMMDD(now);

     SQLQuery1.ExecSQL;
     SQLQuery1.Close;
     finally
     SQLQuery1.Destroy; // or else memory leak
     end; // of TRY..FINALLY
   end;

  Function TJournalDetailEntry.Synch:Boolean;
   begin
     if not _dirty then
       begin
          Result := true;
          exit;
       end;
     if _new then result := self.insert else result := self.update;
   end;

  Function TJournalDetailEntry.validate:boolean;
    begin
      Result := (Self.AcctNo <> -1);
    end;

 Constructor TCompleteJournalEntry.Create;
   var
     i:integer;
   begin
      _JournalHeader := TJournalHeader.Create;
      _Rows := length(_JournalDetailEntries); // Static for now
      for i := low(_JournalDetailEntries) to high(_JournalDetailEntries) do
         _JournalDetailEntries[i] := TJournalDetailEntry.Create
    (*  // This can be made dynamic to support more than 2 entries
      _JournalDetailEntry1 := TJournalDetailEntry.Create;
      _JournalDetailEntry2 := TJournalDetailEntry.Create;
      // Enable array access now for external use
      _JournalDetailEntries[0] := _JournalDetailEntry1;
      _JournalDetailEntries[1] := _JournalDetailEntry2;   *)
   end;


 Function TCompleteJournalEntry.GetHighWaterMark:Integer;
  begin
   self.UpdateHighWaterMark;
   result := _TransHighWaterMark;
  end;

  Function TCompleteJournalEntry.Validate:boolean;
    var
//      i:integer;
      JE:TJournalDetailEntry;
    begin
      // first, validate the header itself
      result := _JournalHeader.Validate;
      // then, check to see if this transaction is balanced
      If result then result := self.IsBalanced;
      // next, if we are still balanced,
      If result then
        // loop through the detail records and validate them one by one
        // if anything fails, then the entire result fails
//        for i := low(_JournalDetailEntries) to High(_JournalDetailEntries) do
        for JE in _JournalDetailEntries do
//             if not _JournalDetailEntries[i].Validate then
            if not je.Validate then
               exit(False);

    end;

 // This is called directly by the GUI now, but the design will be changed to
 // match the ledger object.  (i.e. the object will decide whether to perform
 // an insert or update at the database level).
 Function TCompleteJournalEntry.Insert:boolean;
   var
     i:integer;
     tmpAcct:TLedgerAccount;
     Calc:TDrCrCalculator;
     JE:TJournalDetailEntry;
   begin
     assert(not assigned(calc));
     calc := nil; // don't trust this
     // Assume Failure
      Result := False;
      // Set up the transaction entry number
      UpdateHighWaterMark;
      TransNoSet(_TransHighWaterMark+1);
      // Insert the Journal Header
      If _JournalHeader.Insert then
         begin
          // Insert the journal Entry Rows
//           for i := low(_JournalDetailEntries) to High(_JournalDetailEntries) do
           for je in _JournalDetailEntries do
             Begin
               je.insert;
               // Update Ledger
               tmpAcct := AccountList.GetAccountNo(je.AcctNo);
               if assigned(tmpAcct) then
                 begin
                   // Create the calculator if this is our first time through.
                   if not assigned(calc) then
                      Calc := TDrCrCalculator.Create(je._currency);
                   // Update the balance
                   calc.clear;
                   calc.AddEntry(tmpAcct.Balance, tmpAcct.DrCr, tmpAcct.Currency);
                   with je do
                     begin
                       tmpAcct.TransNo := TransNo;
                       calc.AddEntry(_amount, _drcr, _currency);
                     end;
                   tmpAcct.Balance := Calc.Balance;
                   tmpAcct.DrCr := calc.DrCr;
                   tmpAcct.Synch;
                 end;
             end;

           // Tidy Up
           SQLTransaction1.commit;
           Result := True;
         end;
      Calc.Destroy;
   end;

 Procedure TCompleteJournalEntry.UpdateDrCr;
   var
     i:integer;
     je:TJournalDetailEntry;
   begin
     _TotalDr := 0;
     _TotalCr := 0;
     // This code works as-is, but should be changed to use TDrCrCalculator.
//     for i := low(_JournalDetailEntries) to high(_JournalDetailEntries) do
       for je in _JournalDetailEntries do
//       If _JournalDetailEntries[i]._drcr = Dr then
         with je do case _DrCr of
           Dr:inc(_TotalDr, _Amount);//_TotalDr := _TotalDr + _amount
           Cr:inc(_TotalCr, _Amount);//_TotalCr := _TotalCr + _amount;
         end;

   end;

 Procedure TCompleteJournalEntry.TransNoSet(TransNo:Integer);
   var
   //  i:integer;
     je:TJournalDetailEntry;
   begin
     // Update our own internal status
     _TransNo := TransNo;
     // Update our children to be in synch
     _JournalHeader._TransNo:=_TransNo;
//     for i := low(_JournalDetailEntries) to high(_JournalDetailEntries) do
       for je in _JournalDetailEntries do
         je._TransNo := self._TransNo;
//        _JournalDetailEntries[i]._TransNo := _TransNo;

   end;

 Function TCompleteJournalEntry.IsBalanced:boolean;
   begin
     UpdateDrCr;
     Result := (_TotalDr = _TotalCr);
   end;

 Procedure TCompleteJournalEntry.UpdateHighWaterMark;
   var
    SQLQuery1:TSQLQuery;
   begin
     SQLQuery1 := TSQLQuery.Create(nil);

   //Journal Header should always be inserted first, so it's safer to take that
   // number
   With SQLQuery1 do
     begin
       Transaction := SQLTransaction1;
       SQL.Text := 'select max(transno) as hwm from journalhdr';
       Open;
       If not EOF then
         _TransHighWaterMark := FieldByName('hwm').AsInteger;
       Close;
       Destroy;
     end;
   end;

initialization

  CompleteJournalEntry := TCompleteJournalEntry.Create;

end.

