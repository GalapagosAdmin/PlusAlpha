unit paFrmMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids, EditBtn, libpa;

type

  { TFormPlusAlphaMain }

  TFormPlusAlphaMain = class(TForm)
    bbNewTran: TBitBtn;
    bbDebug: TBitBtn;
    bbLedger: TBitBtn;
    bbTransactionSearch: TBitBtn;
    procedure bbDebugClick(Sender: TObject);
    procedure bbHdrUpdateClick(Sender: TObject);
    procedure bbNewTranClick(Sender: TObject);
    procedure bbLedgerClick(Sender: TObject);
    procedure bbTransactionSearchClick(Sender: TObject);
  //  procedure bbSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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

{ TFormPlusAlphaMain }

procedure TFormPlusAlphaMain.bbHdrUpdateClick(Sender: TObject);
begin
end;

procedure TFormPlusAlphaMain.bbDebugClick(Sender: TObject);
begin
  frmDebug.Show;
end;

procedure TFormPlusAlphaMain.bbNewTranClick(Sender: TObject);
begin
  frmTransaction.show;
end;

procedure TFormPlusAlphaMain.bbLedgerClick(Sender: TObject);
begin
  frmLedger.Show;
end;

procedure TFormPlusAlphaMain.bbTransactionSearchClick(Sender: TObject);
begin
  frmTransactionList.Show;
end;


procedure TFormPlusAlphaMain.FormCreate(Sender: TObject);
begin

end;

procedure TFormPlusAlphaMain.FormShow(Sender: TObject);
 begin
 end;

end.

