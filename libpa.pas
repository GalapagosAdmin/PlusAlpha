unit libpa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
 TUTF8String=UTF8String;
 TJournalHeader = class(TObject)
   private
     //var
       _TransNo:Integer;     // Transaction number - Matches with journal detail
       _Memo:TUTF8String;    // Memo in entry language
       _EffDate:TDate;       // Effective Date  GMT
       _EffTime:TTime;       // Effective time  GMT
       _EntDate:TDate;       // Entry Date      GMT
       _EntTime:TTime;       // Entry Time      GMT
       _Posted:Boolean;       // Posted Flag
     public
       Constructor Create; //overload;
//       Destructor Destroy; override;
       Procedure Commit;
//     Procedure Revert;
       Property HdrMemo:TUTF8String read _memo write _memo;
       Property HdrTransNo:Integer read _TransNo write _TransNo;
       Property HdrPosted:Boolean read _Posted write _Posted;
 end;  // of TJournalHeader

 TCurrCode=String[3];
 TDrCr = (Dr, Cr);

 TJournalDetailEntry = class(TObject)
   private
   //var
     _TransNo:Integer;     // Transaction number - Matches with header
     _TransRow:Integer;    // Entry number for this Transaction number
     _amount:Integer;      // Amount, without decimal point
     _currency:TCurrCode;  // Currency Code
     _drcr:Tdrcr;          // Debit/Credit Indicator
     _acctno:Integer;      // Account no.
   public
//     Constructor Create; //overload;
//       Destructor Destroy; override;
//     Procedure Commit;
//     Procedure Revert;
     Property HdrTransNo:Integer read _TransNo write _TransNo;
 end;

// Class for holding the complete transaction, header and detail.
 TCompleteJournalEntry=Class(TObject)
   private
     _TransNo:Integer;     // Transaction number - Matches with header
     _Rows:Integer; // Number of Line Items
     _JournalHeader : TJournalHeader;
     // Two entries for testing
     _JournalDetailEntry1 : TJournalDetailEntry;
     _JournalDetailEntry2 : TJournalDetailEntry;
     // Use Dynamic Array or collection later
     _JournalDetailEntries : Array[0..1] of TJournalDetailEntry;
     _TotalDr:Integer;
     _TotalCr:Integer;
     Procedure UpdateDrCr;
     Procedure TransNoSet(TransNo:Integer);
   Public
     Constructor Create; //overload;
     Property Rows:Integer Read _Rows;
     Property TransNo:Integer read _TransNo write TransNoSet;
     Function IsBalanced:Boolean;
 end;

var
  JournalHeader:TJournalHeader;
  CompleteJournalEntry:TCompleteJournalEntry;

implementation

  Constructor TJournalHeader.Create;
    begin
      inherited;
      _TransNo := 0;
      // other stuff goes here.
    end;

  Procedure TJournalHeader.Commit;
    begin
      // write out to database
      //insert into "main"."JOURNALHDR" ( "MEMO", "TRANSNO") values ( 'test entry 6', 6)
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

initialization
   JournalHeader := TJournalHeader.Create;
   CompleteJournalEntry := TCompleteJournalEntry.Create;


end.

