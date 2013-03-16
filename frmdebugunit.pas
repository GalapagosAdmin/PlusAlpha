unit frmDebugUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons;

type

  { TfrmDebug }

  TfrmDebug = class(TForm)
    bbTransNo: TBitBtn;
    bbTextCode: TBitBtn;
    leTextCd: TLabeledEdit;
    leTranNo: TLabeledEdit;
    procedure bbTextCodeClick(Sender: TObject);
    procedure bbTransNoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDebug: TfrmDebug;

implementation

uses
  libpa, paJournal, paText;

{$R *.lfm}

{ TfrmDebug }

procedure TfrmDebug.bbTransNoClick(Sender: TObject);
  begin
  //  leTranNo.Text := IntToStr(CompleteJournalEntry.HighWaterMark);
    leTranNo.Text:=IntToStr(CompleteJournalEntry.HighWaterMark);
  end;

procedure TfrmDebug.bbTextCodeClick(Sender: TObject);
begin
  ShowMessage(DBText.GetText(StrToInt(leTextCd.Text)));
end;

end.

