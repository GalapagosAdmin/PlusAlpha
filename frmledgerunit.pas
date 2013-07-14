unit frmLedgerUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, StdCtrls, Buttons, ActnList, defaulttranslator;

type

  { TfrmLedger }

  TfrmLedger = class(Tframe)
    acTreeRefresh: TAction;
    alFrmLedger: TActionList;
    bbSynch: TBitBtn;
    cbAcctCurr: TComboBox;
    cbAccType: TComboBox;
    cbAccSubType: TComboBox;
    ebAccountTitle: TEdit;
    gbAcctInfo: TGroupBox;
    ImageList1: TImageList;
    leAcctGUID: TLabeledEdit;
    leExtAcctNo: TLabeledEdit;
    lblAccType: TLabel;
    lblAccType1: TLabel;
    leAcctNo: TLabeledEdit;
    leAcctBal: TLabeledEdit;
    mmWelcomeText: TMemo;
    nbLedgerContent: TNotebook;
    pgWelcome: TPage;
    pgAccountInfo: TPage;
    rbAcctBalDr: TRadioButton;
    rbAcctBalCr: TRadioButton;
    splLedger: TSplitter;
    tvAccountList: TTreeView;
    procedure acTreeRefreshExecute(Sender: TObject);
    procedure bbSynchClick(Sender: TObject);
    procedure FrameClick(Sender: TObject);
    procedure lblAccType1Click(Sender: TObject);
 //   procedure FormShow(Sender: TObject);
    procedure leAcctNoChange(Sender: TObject);
    procedure pgWelcomeBeforeShow(ASender: TObject; ANewPage: TPage;
      ANewIndex: Integer);
    procedure tvAccountListClick(Sender: TObject);
//    procedure tvAccountListSelectionChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmLedger: TfrmLedger;

implementation

 uses
   libpa, paLedger, paText;

{$R *.lfm}

{ TfrmLedger }

var
  CurrentAccount:TLedgerAccount;

procedure TfrmLedger.acTreeRefreshExecute(Sender: TObject);

// Map account number to Tree Node
Type
  TStruct=record
//       AcctNo:Integer;
    AccountObject:TTreeNode;
  end;

var
 //i:integer;
 //tmpAcct:TLedgerAccount;
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
         // This assumes that the account numbers are contiguous.
         // Perhaps we could use CaseGUID here
         if length(lookup) < AcctNo then
          SetLength(lookup, AcctNo); // -1
         with lookup[acctno] do
           AccountObject := acct;
       end;
   end;

 procedure AddToTree(tmpAcct:TLedgerAccount);
   begin
      if not Assigned(tmpAcct) then
         Raise Exception.Create('frmLedgerUnit.TFrmLedger.acTreeRefreshExecute.AddToTree: tmpAccount not assigned!');
      With tmpAcct do
      case AccountType of
         atAsset: tvAccountList.Items.AddChildObject(AssetRoot, text, tmpAcct) ;
         atLiability: tvAccountList.Items.AddChildObject(LiabilityRoot, text, tmpAcct) ;
         atIncome: tvAccountList.Items.AddChildObject(IncomeRoot, text, tmpAcct) ;
         atExpense: tvAccountList.Items.AddChildObject(ExpenseRoot, text, tmpAcct) ;
         atEquity: tvAccountList.Items.AddChildObject(EquityRoot, text, tmpAcct) ;
         atPlaceholder:; // leave placeholder accounts out for now
         else
         tvAccountList.Items.AddObject(nil, Text, tmpAcct)
       end;  // of case
   end;

begin



 // Populate the account tree
 try
 if not assigned(tvAccountList) then
  Raise Exception.Create('frmLedgerUnit: tvAccountList not assigned!');
 tvAccountList.BeginUpdate;
 tvAccountList.Items.Clear;
 try
 with tvAccountList do
   begin
//     AssetRoot := items.Add(nil, 'Assets');
     AssetRoot := items.Add(nil, DBText.GetText(3));
     //     LiabilityRoot := items.Add(nil, 'Liabilities');
     LiabilityRoot := items.Add(nil, DBText.GetText(4));
//     IncomeRoot := items.Add(nil, 'Income');
     IncomeRoot := items.Add(nil, DBText.GetText(7));
//     ExpenseRoot := items.Add(nil, 'Expense');
     ExpenseRoot := items.Add(nil, DBText.GetText(6));
     //     EquityRoot := items.Add(nil, 'Equity');
     EquityRoot := items.Add(nil, DBText.GetText(5));
   end;
 except
   raise exception.Create('frmLedgerUnit:Error Adding Tree Roots.');
 end;

 // This should NOT be required!
 //AccountList.Reload;

 If not assigned(AccountList) then
    Raise Exception.Create('frmLedgerUnit:AccountList not assigned!');

 try
 // technically, if there were no accounts, this could fail...
 // but then I suppose it should fail if we have no accounts.
 AddToTree(AccountList.GetFirstAccount);
 While not AccountList.EOF do
       AddToTree(AccountList.GetNextAccount);
 except
   raise exception.Create('frmLedgerUnit: Error updating Account Tree!');
 end;

 finally
   tvAccountList.EndUpdate;
 end;
 // Populate the Account Type Popup
 // This should really be read from the DB via an object in LibPa, but for now...
 cbAccType.Items.CommaText:='C, P, A, L, E, I';

end;


procedure TfrmLedger.bbSynchClick(Sender: TObject);
begin
  //ebAccountTitle.Text:=Text;
  If not Assigned(CurrentAccount) then exit;
  With CurrentAccount do
    begin
      Text := ebAccountTitle.Text;
      Synch;
      Commit;
    end;
   // Update GUI tree
   acTreeRefresh.Execute;
end;

procedure TfrmLedger.FrameClick(Sender: TObject);
begin

end;

procedure TfrmLedger.lblAccType1Click(Sender: TObject);
begin

end;

procedure TfrmLedger.leAcctNoChange(Sender: TObject);
begin

end;

procedure TfrmLedger.pgWelcomeBeforeShow(ASender: TObject; ANewPage: TPage;
  ANewIndex: Integer);
begin

end;

procedure TfrmLedger.tvAccountListClick(Sender: TObject);
begin
  nbLedgerContent.PageIndex:=0;
  // make sure something is actually selected
  if not assigned(tvAccountList.Selected) then exit;
  // if this is a placeholder text only item, don't try to access it
  if not assigned(tvAccountList.Selected.Data) then exit;
  // if it is a useful object, update the display panel
  CurrentAccount := TObject(tvAccountList.Selected.Data) as TLedgerAccount;
  with CurrentAccount do
    begin
      leAcctNo.Text := IntToStr(AcctNo);
      leAcctGUID.Text := GUIDToString(AcctGUID);
      leAcctBal.Text := IntToStr(Balance);
 //     cbAcctCurr.Items.Add(Currency);
      cbAcctCurr.Text := Currency;
      Case DrCr of
        Dr: rbAcctBalDr.Checked := True;
        Cr: rbAcctBalCr.Checked := True;
      end;
      ebAccountTitle.Text:=Text;
      leExtAcctNo.Text:=IntToStr(ExtAcctNo);
      // Set Account Type dropdown text
      cbAccType.Text := ABAP_Translate(IntToStr(Ord(AccountType)), AcctTransMapRev);
      // Add subtype to dropdown if needed
      if cbAccSubType.Items.IndexOf(AccountSubType) < 0 then
        cbAccSubType.AddItem(AccountSubType,nil);
      // Set subtype dropdown text
      self.cbAccSubType.Text:=AccountSubType;
      nbLedgerContent.PageIndex:=1;

    end;
end;  // of Procedure

Initialization

Finalization

end.   // of Unit

