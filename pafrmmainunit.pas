unit paFrmMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ExtendedNotebook, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Buttons, StdCtrls, Grids, EditBtn, ComCtrls, ButtonPanel,
  ActnList, libpa, fmeWelcomeUnit, anchordocking;

type

  { TFormPlusAlphaMain }

  TFormPlusAlphaMain = class(TForm)
    acAccountShow: TAction;
    acTransactionListShow: TAction;
    ActionList1: TActionList;
    bbNewTran: TBitBtn;
    bbDebug: TBitBtn;
    bbLedger: TBitBtn;
    fmeWelcome1: TfmeWelcome;
    ImageList1: TImageList;
    pnlMain: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    tvMainMenu: TTreeView;
    procedure acAccountShowExecute(Sender: TObject);
    procedure acTransactionListShowExecute(Sender: TObject);
    procedure bbDebugClick(Sender: TObject);
    procedure bbHdrUpdateClick(Sender: TObject);
    procedure bbNewTranClick(Sender: TObject);
    procedure bbLedgerClick(Sender: TObject);
    procedure bbTransactionSearchClick(Sender: TObject);
    procedure Bevel1ChangeBounds(Sender: TObject);
  //  procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
    procedure tvMainMenuSelectionChanged(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormPlusAlphaMain: TFormPlusAlphaMain;

implementation

uses
  frmTransactionUnit, frmledgerunit, frmDebugUnit, frmTransactionListUnit, defaulttranslator;

{$R *.lfm}
var
 F1 :TfrmLedger;
 F2 :TfrmTransactionList;

{ TFormPlusAlphaMain }

procedure TFormPlusAlphaMain.bbHdrUpdateClick(Sender: TObject);
begin
end;

procedure TFormPlusAlphaMain.bbDebugClick(Sender: TObject);
begin
  frmDebug.Show;
end;

procedure TFormPlusAlphaMain.acAccountShowExecute(Sender: TObject);

begin
  // fmeWelcome1.Destroy;
   fmeWelcome1.Free;
   if assigned(f1) then exit;
   F1 := TFrmLedger.Create(self);
  // F1.Create();
   f1.Parent := FormPlusAlphaMain;

   f1.Align := alClient;
   f1.acTreeRefresh.Execute;

  // frmLedger.Show;
end;

procedure TFormPlusAlphaMain.acTransactionListShowExecute(Sender: TObject);
begin
   fmeWelcome1.Free;
   if assigned(f2) then exit;
   F2 := TFrmTransactionList.Create(self);
   f2.Parent := FormPlusAlphaMain;

   f2.Align := alClient;
 //  f2.acTreeRefresh.Execute;

end;

procedure TFormPlusAlphaMain.bbNewTranClick(Sender: TObject);
begin
  frmTransaction.show;
end;

procedure TFormPlusAlphaMain.bbLedgerClick(Sender: TObject);

begin
 frmTransactionList.show;
end;

procedure TFormPlusAlphaMain.bbTransactionSearchClick(Sender: TObject);
begin
  frmTransactionList.Show;
end;

procedure TFormPlusAlphaMain.Bevel1ChangeBounds(Sender: TObject);
begin

end;


procedure TFormPlusAlphaMain.FormCreate(Sender: TObject);
begin
   DockMaster.MakeDockSite(Self,[akRight],admrpChild);
end;

procedure TFormPlusAlphaMain.FormShow(Sender: TObject);
 begin
 end;

procedure TFormPlusAlphaMain.SpeedButton1Click(Sender: TObject);
begin
 // f1 := TFrame1.Create(FMain);

 // f1.OnChanged := @F1Onchanged;

 // f1.Parent := TabSheet1;

 // f1.Align := alClient;
end;

procedure TFormPlusAlphaMain.SpeedButton2Click(Sender: TObject);
begin
end;

procedure TFormPlusAlphaMain.ToolBar1Click(Sender: TObject);
begin

end;

procedure TFormPlusAlphaMain.tvMainMenuSelectionChanged(Sender: TObject);
begin
  case tvMainMenu.Selected.AbsoluteIndex of
    0: acAccountShow.Execute; //accounts
    1: acTransactionListShow.Execute; // Transactions
  end;
end;

end.

