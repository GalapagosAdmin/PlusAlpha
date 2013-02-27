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
var
  i:integer;
  tmpAcct:TLedgerAccount;

begin
  // Populate the account tree
  try
  tvAccountList.BeginUpdate;
  tvAccountList.Items.Clear;
  tmpAcct := AccountList.GetFirstAccount;
  With tmpAcct do
    with tvAccountList.Items.AddObject(nil, Text, tmpAcct) do
      tag := tmpAcct.AcctNo;
                              //   AccountLIst.GetFirstAccount);
  While not AccountList.EOF do
    begin
      tmpAcct := AccountList.GetNextAccount;
      With tmpAcct do
        tvAccountList.Items.AddObject(nil, Text, tmpAcct);
                                  //   AccountLIst.GetNextAccount);
    end;
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
  with tvAccountList.Selected.Data as TLedgerAccount do
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

