unit paFrmMainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, Grids, libpa;

type

  { TFormPlusAlphaMain }

  TFormPlusAlphaMain = class(TForm)
    bbHdrUpdate: TBitBtn;
    cbPosted: TCheckBox;
    leHdrMemo: TLabeledEdit;
    leTrnNo: TLabeledEdit;
    StringGrid1: TStringGrid;
    procedure bbHdrUpdateClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormPlusAlphaMain: TFormPlusAlphaMain;

implementation

{$R *.lfm}

{ TFormPlusAlphaMain }

procedure TFormPlusAlphaMain.bbHdrUpdateClick(Sender: TObject);
begin
  // Update Object
  JournalHeader.HdrMemo := leHdrMemo.Text;
  JournalHeader.HdrPosted:=cbPosted.Checked;
  // Retrieve Updates back
  With JournalHeader do
    begin
      leHdrMemo.Text:= HdrMemo;
      leTrnNo.Text:=IntToStr(HdrTransNo);
      cbPosted.Checked := HdrPosted;
    end;
end;

end.

