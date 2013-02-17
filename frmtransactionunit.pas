unit frmTransactionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TfrmTransaction }

  TfrmTransaction = class(TForm)
    ebAcctNo1: TComboBox;
    bbHdrUpdate: TBitBtn;
    bbSave: TBitBtn;
    cbPosted: TCheckBox;
    ebAcctNo2: TComboBox;
    leAmt1: TLabeledEdit;
    leAmt2: TLabeledEdit;
    leHdrMemo: TLabeledEdit;
    leMemo1: TLabeledEdit;
    leMemo2: TLabeledEdit;
    leTrnNo: TLabeledEdit;
    stDetail: TStaticText;
    procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransaction: TfrmTransaction;

implementation

uses libpa;

{ TfrmTransaction }

procedure TfrmTransaction.FormShow(Sender: TObject);
begin
  ebAcctNo1.Items := AccountList.AccountStringList;
  ebAcctNo2.Items := AccountList.AccountStringList;
end;

procedure TfrmTransaction.bbSaveClick(Sender: TObject);
begin
   With CompleteJournalEntry do
  begin
  // Update Object
  _JournalHeader.HdrMemo := leHdrMemo.Text;
  _JournalHeader.HdrTransNo:= StrToInt(leTrnNo.Text);
//  _JournalHeader.HdrPosted := cbPosted.Checked;
  // Retrieve Updates back


  CompleteJournalEntry._JournalDetailEntries[0].TransNo:=StrToInt(LeTrnNo.Text);
  CompleteJournalEntry._JournalDetailEntries[0].AcctNo:= ActToInt(ebAcctNo1.Text);
  CompleteJournalEntry._JournalDetailEntries[0].TransRow:=0;
  CompleteJournalEntry._JournalDetailEntries[0].Text:=lememo1.Text;
  CompleteJournalEntry._JournalDetailEntries[0].Amount:=StrtoInt(leAmt1.Text);

  CompleteJournalEntry._JournalDetailEntries[1].TransNo:=StrToInt(LeTrnNo.Text);
  CompleteJournalEntry._JournalDetailEntries[1].AcctNo:=ActToInt(ebAcctNo2.Text);
  CompleteJournalEntry._JournalDetailEntries[1].TransRow:=1;
  CompleteJournalEntry._JournalDetailEntries[1].Text:=lememo2.Text;
  CompleteJournalEntry._JournalDetailEntries[1].Amount:= StrToInt(leAmt2.Text);

  If CompleteJournalEntry.Insert then
   ShowMessage('Transaction Inserted')
  else
   ShowMessage('Error Inserting Transaction.');

  leTrnNo.Text:= IntToStr(CompleteJournalEntry.TransNo);
  end;
end;

procedure TfrmTransaction.FormCreate(Sender: TObject);
begin

end;



{$R *.lfm}

end.

