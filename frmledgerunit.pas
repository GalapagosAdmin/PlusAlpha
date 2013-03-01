unit frmLedgerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls;

type

  { TfrmLedger }

  TfrmLedger = class(TForm)
    cbAcctCurr: TComboBox;
    cbAccType: TComboBox;
    GroupBox1: TGroupBox;
    lblAccType: TLabel;
    leAcctNo: TLabeledEdit;
    leAcctBal: TLabeledEdit;
    rbAcctBalDr: TRadioButton;
    rbAcctBalCr: TRadioButton;
    Splitter1: TSplitter;
    tvAccountList: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure leAcctNoChange(Sender: TObject);
    procedure tvAccountListClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLedger: TfrmLedger;

implementation

 uses
   libpa, paLedger;

{$R *.lfm}

{ TfrmLedger }

procedure TfrmLedger.FormShow(Sender: TObject);
 Type
   TStruct=record
 //       AcctNo:Integer;
     AccountObject:TTreeNode;
   end;
 var
  i:integer;
  tmpAcct:TLedgerAccount;
  lookup:array of TStruct; // index for quick lookups.
  AssetRoot:TTreeNode;
  LiabilityRoot:TTreeNode;
  IncomeRoot:TTreeNode;
  ExpenseRoot:TTreeNode;
  EquityRoot:TTreeNode;

  procedure update_Lookup(Acct:TTreeNode);
    begin
      With TObject(Acct.Data) as TLedgerAccount do
        begin
          if length(lookup) < AcctNo then
           SetLength(lookup, AcctNo); // -1
          with lookup[acctno] do
            AccountObject := acct;
        end;
    end;

  procedure AddToTree(tmpAcct:TLedgerAccount);
    begin
       With tmpAcct do
       case AccountType of
          'A' : tvAccountList.Items.AddChildObject(AssetRoot, text, tmpAcct) ;
          'L' : tvAccountList.Items.AddChildObject(LiabilityRoot, text, tmpAcct) ;
          'I' : tvAccountList.Items.AddChildObject(IncomeRoot, text, tmpAcct) ;
          'E' : tvAccountList.Items.AddChildObject(ExpenseRoot, text, tmpAcct) ;
          'C' : tvAccountList.Items.AddChildObject(EquityRoot, text, tmpAcct) ;
          'P':; // leave placeholder accounts out for now
          else
          tvAccountList.Items.AddObject(nil, Text, tmpAcct)
        end;  // of case
    end;

begin
  // Populate the account tree
  try

  tvAccountList.BeginUpdate;
  tvAccountList.Items.Clear;
  with tvAccountLIst do
    begin
      AssetRoot := items.Add(nil, 'Assets');
      LiabilityRoot := items.Add(nil, 'Liabilities');
      IncomeRoot := items.Add(nil, 'Income');
      ExpenseRoot := items.Add(nil, 'Expense');
      EquityRoot := items.Add(nil, 'Equity');
    end;

  // technically, if there were no accounts, this could fail...
  // but then I suppose it should fail if we have no accounts.
  AddToTree(AccountList.GetFirstAccount);
  While not AccountList.EOF do
        AddToTree(AccountList.GetNextAccount);

  finally
    tvAccountList.EndUpdate;
  end;
  // Populate the Account Type Popup
  // This should really be read from the DB via an object in LibPa, but for now...
  cbAccType.Items.CommaText:='C, P, A, L, E, I';

end;

procedure TfrmLedger.leAcctNoChange(Sender: TObject);
begin

end;

procedure TfrmLedger.tvAccountListClick(Sender: TObject);
begin
  // make sure something is actually selected
  if not assigned(tvAccountList.Selected) then exit;
  // if this is a placeholder text only item, don't try to access it
  if not assigned(tvAccountList.Selected.Data) then exit;
  // if it is a useful object, update the display panel
  with TObject(tvAccountList.Selected.Data) as TLedgerAccount do
    begin
      leAcctNo.Text := IntToStr(AcctNo);
      leAcctBal.Text := IntToStr(Balance);
      cbAcctCurr.Text := Currency;
      Case DrCr of
        Dr: rbAcctBalDr.Checked := True;
        Cr: rbAcctBalCr.Checked := True;
      end;
      cbAccType.Text := AccountType;
    end;
end;  // of Procedure

end.

