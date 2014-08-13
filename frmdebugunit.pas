unit frmDebugUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, ActnList;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    acUpdateDBPath: TAction;
    ActionList1: TActionList;
    bbTransNo: TBitBtn;
    bbTextCode: TBitBtn;
    leDBPath: TLabeledEdit;
    leTextCd: TLabeledEdit;
    leTranNo: TLabeledEdit;
    procedure acUpdateDBPathExecute(Sender: TObject);
    procedure bbTextCodeClick(Sender: TObject);
    procedure bbTransNoClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDebug: TfrmDebug;

implementation

uses
  libpa, paJournal, paText, paDatabase;

{$R *.lfm}

{ TfrmDebug }

procedure TfrmDebug.bbTransNoClick(Sender: TObject);
  begin
  //  leTranNo.Text := IntToStr(CompleteJournalEntry.HighWaterMark);
    leTranNo.Text:=IntToStr(CompleteJournalEntry.HighWaterMark);
  end;

procedure TfrmDebug.FormShow(Sender: TObject);
begin
  acUpdateDBPath.Execute;
end;

procedure TfrmDebug.bbTextCodeClick(Sender: TObject);
begin
  ShowMessage(DBText.GetText(StrToInt(leTextCd.Text)));
end;

procedure TfrmDebug.acUpdateDBPathExecute(Sender: TObject);
begin
  leDBPath.Text:=CompleteDBPathGet;
end;

end.

