unit frmTransactionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Calendar, EditBtn, defaulttranslator, ActnList;

type

  { TfrmTransaction }

  TfrmTransaction = class(TForm)
    acInit: TAction;
    alNewTransaction: TActionList;
    deHeaderEffDate: TDateEdit;
    bbSave: TBitBtn;
    cbPosted: TCheckBox;
    ebAcctNo1: TComboBox;
    ebAcctNo2: TComboBox;
    ebAcctNo3: TComboBox;
    ebAcctNo4: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    lblDate: TLabel;
    leAmt1: TLabeledEdit;
    leAmt2: TLabeledEdit;
    leAmt3: TLabeledEdit;
    leAmt4: TLabeledEdit;
    leHdrMemo: TLabeledEdit;
    leMemo1: TLabeledEdit;
    leMemo2: TLabeledEdit;
    leMemo3: TLabeledEdit;
    leMemo4: TLabeledEdit;
    leTrnNo: TLabeledEdit;
    pnlDetail1: TPanel;
    pnlDetail2: TPanel;
    pnlDetail3: TPanel;
    pnlDetail4: TPanel;
    rbCr1: TRadioButton;
    rbCr2: TRadioButton;
    rbCr3: TRadioButton;
    rbCr4: TRadioButton;
    rbDr1: TRadioButton;
    rbDr2: TRadioButton;
    rbDr3: TRadioButton;
    rbDr4: TRadioButton;
    stDetail: TStaticText;
    stDetail1: TStaticText;
    procedure acInitExecute(Sender: TObject);
    procedure bbHdrUpdateClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure leTrnNoChange(Sender: TObject);
    procedure pnlDetail1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransaction: TfrmTransaction;

implementation

uses libpa, paLedger, paJournal, paMessage;

ResourceString
  MSGTRANSINSERTED = 'Transaction Inserted';
  ERRTRANSNOTINSERT = 'Error Inserting Transaction.';
  ERRTRANSNOTVALID = 'Transaction is not valid.';
  ERRTRANSNOTBAL = 'Transaction is not balanced.';

{ TfrmTransaction }

procedure TfrmTransaction.FormShow(Sender: TObject);
  begin
    ebAcctNo1.Items := AccountList.AccountStringList;
    ebAcctNo2.Items := AccountList.AccountStringList;
    leTrnNo.Text := IntToStr(CompleteJournalEntry.HighWaterMark+1);
    deHeaderEffDate.Date := now;
  end;

procedure TfrmTransaction.leTrnNoChange(Sender: TObject);
begin
end;

procedure TfrmTransaction.pnlDetail1Click(Sender: TObject);
  begin

  end;

procedure TfrmTransaction.bbSaveClick(Sender: TObject);
var
  tmpGUID:TGUID;
  CurrRow:Integer;
begin
  With CompleteJournalEntry do
  begin
    Reset; // Delete any garbage rows left over from before
  // Update Object
  With _JournalHeader do
    begin
      HdrMemo := leHdrMemo.Text;
      HdrTransNo:= StrToInt(leTrnNo.Text);
      // The GUID will be created automatically if we don't make it here
      Assert(CreateGUID(tmpGUID)=0);  // Even if we checked this for error...
      HDRTransGUID := tmpGUID;
      EffDate := deHeaderEffDate.Date;
      _JournalHeader.HdrPosted := cbPosted.Checked;
  // Retrieve Updates back
    end;

  CurrRow := CompleteJournalEntry.AddDetailEntry;
  Assert(CurrRow=0);
  with _JournalDetailEntries[CurrRow] do
    begin
    // Transaction GUID should be automatically carried from the header.
      TransNo := StrToInt(LeTrnNo.Text);
      // where can we grab the Account GUID from?
      // (Not currently tracked in the GUI but TAccountList has it).
      AcctNo :=  ActToInt(ebAcctNo1.Text);
      TransRow := CurrRow;
      Text := leMemo1.Text;
      Amount := StrtoInt(leAmt1.Text);
      Case rbDr1.Checked of
        True:  DrCr:=Dr;
        False: DrCr:=Cr;
      end;
    end;

  CurrRow := CompleteJournalEntry.AddDetailEntry;
  Assert(CurrRow=1);
  with _JournalDetailEntries[CurrRow] do
    begin
      TransNo:=StrToInt(LeTrnNo.Text);
      AcctNo:=ActToInt(ebAcctNo2.Text);
      TransRow:=CurrRow;
      Text:=lememo2.Text;
      Amount:= StrToInt(leAmt2.Text);
      Case rbDr2.Checked of
        True:DrCr:=Dr;
        False:DrCr:=Cr;
      end;
    end;

  If ebAcctNo3.text <> '' then
  begin
  CurrRow := CompleteJournalEntry.AddDetailEntry;
  Assert(CurrRow=2);
  with _JournalDetailEntries[CurrRow] do
    begin
      TransNo:=StrToInt(LeTrnNo.Text);
      AcctNo:=ActToInt(ebAcctNo3.Text);
      TransRow:=CurrRow;
      Text:=lememo3.Text;
      Amount:= StrToInt(leAmt3.Text);
      Case rbDr3.Checked of
        True:DrCr:=Dr;
        False:DrCr:=Cr;
      end;
    end;
  end; // line item 3 has text

  If ebAcctNo4.text <> '' then
  begin
  CurrRow := CompleteJournalEntry.AddDetailEntry;
  Assert(CurrRow=3);
  with _JournalDetailEntries[CurrRow] do
    begin
      TransNo:=StrToInt(LeTrnNo.Text);
      AcctNo:=ActToInt(ebAcctNo4.Text);
      TransRow:=CurrRow;
      Text:=lememo4.Text;
      Amount:= StrToInt(leAmt4.Text);
      Case rbDr4.Checked of
        True:DrCr:=Dr;
        False:DrCr:=Cr;
      end;
    end;
  end; // line item 4 has text

  If not IsBalanced then
     begin
       ShowMessage(ERRTRANSNOTBAL);
       exit;
     end;
   If not Validate then
     begin
       ShowMessage(ERRTRANSNOTVALID);
       exit;
     end;

  If Insert then
    begin
     DisplayMessage(MsgTransInserted);
     // re-init everything
     acInit.Execute;
   end
  else
   ShowMessage(ERRTRANSNOTINSERT);

  leTrnNo.Text:= IntToStr(CompleteJournalEntry.TransNo);
  self.Close;
  end;  // of WITH

end;  // of Procedure

procedure TfrmTransaction.bbHdrUpdateClick(Sender: TObject);
begin

end;

procedure TfrmTransaction.acInitExecute(Sender: TObject);
begin
     LeTrnNo.Clear;
     LeHdrMemo.Clear;
     deHeaderEffDate.Date := now;
     cbPosted.Checked := False;
     ebAcctNo1.Clear;
     leMemo1.Clear;
     leAmt1.Clear;
     ebAcctNo2.Clear;
     leMemo2.Clear;
     leAmt2.Clear;
end;

Procedure TfrmTransaction.FormCreate(Sender: TObject);
  begin

  end;



{$R *.lfm}

initialization

end.

