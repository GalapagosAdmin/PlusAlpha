unit frmTransactionUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Calendar, EditBtn;

type

  { TfrmTransaction }

  TfrmTransaction = class(TForm)
    deHeaderEffDate: TDateEdit;
    bbSave: TBitBtn;
    cbPosted: TCheckBox;
    ebAcctNo1: TComboBox;
    ebAcctNo2: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblDate: TLabel;
    leAmt1: TLabeledEdit;
    leAmt2: TLabeledEdit;
    leHdrMemo: TLabeledEdit;
    leMemo1: TLabeledEdit;
    leMemo2: TLabeledEdit;
    leTrnNo: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    rbCr1: TRadioButton;
    rbCr2: TRadioButton;
    rbDr1: TRadioButton;
    rbDr2: TRadioButton;
    stDetail: TStaticText;
    procedure bbHdrUpdateClick(Sender: TObject);
    procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure leTrnNoChange(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransaction: TfrmTransaction;

implementation

uses libpa, paLedger;

{ TfrmTransaction }

procedure TfrmTransaction.FormShow(Sender: TObject);
  begin
    ebAcctNo1.Items := AccountList.AccountStringList;
    ebAcctNo2.Items := AccountList.AccountStringList;
    leTrnNo.Text := IntToStr(CompleteJournalEntry.HighWaterMark+1);
  end;

procedure TfrmTransaction.leTrnNoChange(Sender: TObject);
begin
end;

procedure TfrmTransaction.Panel1Click(Sender: TObject);
  begin

  end;

procedure TfrmTransaction.bbSaveClick(Sender: TObject);
begin
  With CompleteJournalEntry do
  begin
  // Update Object
  With _JournalHeader do
    begin
      HdrMemo := leHdrMemo.Text;
      HdrTransNo:= StrToInt(leTrnNo.Text);
      EffDate := deHeaderEffDate.Date;
      _JournalHeader.HdrPosted := cbPosted.Checked;
  // Retrieve Updates back
    end;

  with _JournalDetailEntries[0] do
    begin
      TransNo := StrToInt(LeTrnNo.Text);
      AcctNo :=  ActToInt(ebAcctNo1.Text);
      TransRow := 0;
      Text := lememo1.Text;
      Amount := StrtoInt(leAmt1.Text);
      Case rbDr1.Checked of
        True:  DrCr:=Dr;
        False: DrCr:=Cr;
      end;
    end;

  with _JournalDetailEntries[1] do
    begin
      TransNo:=StrToInt(LeTrnNo.Text);
      AcctNo:=ActToInt(ebAcctNo2.Text);
      TransRow:=1;
      Text:=lememo2.Text;
      Amount:= StrToInt(leAmt2.Text);
      Case rbDr2.Checked of
        True:DrCr:=Dr;
        False:DrCr:=Cr;
      end;
    end;

  If not IsBalanced then
     begin
       ShowMessage('Transaction is not balanced.');
       exit;
     end;
   If not Validate then
     begin
       ShowMessage('Transaction is not valid.');
       exit;
     end;

  If Insert then
    begin
     ShowMessage('Transaction Inserted');
     // re-init everything
     LeTrnNo.Clear;
     LeHdrMemo.Clear;
     deHEaderEffDate.Date := now;
     cbPosted.Checked := False;
     ebAcctNo1.Clear;
     leMemo1.Clear;
     leAmt1.Clear;
     ebAcctNo2.Clear;
     leMemo2.Clear;
     leAmt2.Clear;
   end
  else
   ShowMessage('Error Inserting Transaction.');

  leTrnNo.Text:= IntToStr(CompleteJournalEntry.TransNo);
  end;  // of WITH

end;  // of Procedure

procedure TfrmTransaction.bbHdrUpdateClick(Sender: TObject);
begin

end;

Procedure TfrmTransaction.FormCreate(Sender: TObject);
  begin

  end;



{$R *.lfm}

end.

