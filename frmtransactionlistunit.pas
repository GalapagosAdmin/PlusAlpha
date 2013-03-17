unit frmTransactionListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids;

type

  { TfrmTransactionList }

  TfrmTransactionList = class(TForm)
    pnlToolbar: TPanel;
    StringGrid1: TStringGrid;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransactionList: TfrmTransactionList;

implementation

uses paTransactionList, paJournal;

ResourceString
 HdrTransNo='TransNo';
 HdrTransRow='TransRow';
 HdrText='Text';
 HdrAmt='Amount';
{$R *.lfm}

{ TfrmTransactionList }

procedure TfrmTransactionList.FormShow(Sender: TObject);
var
  ResultEntry : TResultEntry;
  JournalDetailEntry:TJournalDetailEntry;
 begin
   JournalDetailEntry := TJournalDetailEntry.Create;
   // following is only hard-coded for testing
   TransactionList.Account := 3;
  StringGrid1.Clear;
  StringGrid1.RowCount:=Length(TransactionList.TransNos)+1;
  StringGrid1.cols[0].Append(HdrTransNo);
  StringGrid1.cols[1].Append(HdrTransRow);
  StringGrid1.cols[2].Append(HdrText);
  StringGrid1.cols[3].Append(HdrAmt);
   With TransactionList do for ResultEntry in TransNos do
     begin
       StringGrid1.cols[0].Append(IntToStr(ResultEntry.TransactionNo));
       StringGrid1.cols[1].Append(IntToStr(ResultEntry.TransactionRow));
       With ResultEntry do
         JournalDetailEntry.Load(TransactionNo, TransactionRow);
       StringGrid1.cols[2].Append(JournalDetailEntry.Text);
       StringGrid1.cols[3].Append(IntToStr(JournalDetailEntry.Amount));
     end;
   JournalDetailEntry.Free;
 end;

end.

