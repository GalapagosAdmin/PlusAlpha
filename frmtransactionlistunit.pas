unit frmTransactionListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, ListFilterEdit, Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Grids, StdCtrls, ActnList, libpa, paTransactionList;

type

  { TfrmTransactionList }

  TfrmTransactionList = class(TForm)
    acUpdate: TAction;
    ActionList1: TActionList;
    cbAccount: TComboBox;
    pnlToolbar: TPanel;
    StringGrid1: TStringGrid;
    procedure acUpdateExecute(Sender: TObject);
    procedure cbAccountChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTransactionList: TfrmTransactionList;

implementation

uses  paJournal, paLedger;

ResourceString
 HdrTransNo='TransNo';
 HdrTransRow='TransRow';
 HdrText='Text';
 HdrAmt='Amount';
 HdrDr='Dr';
 HdrCr='Cr';
{$R *.lfm}

{ TfrmTransactionList }

procedure TfrmTransactionList.FormShow(Sender: TObject);

 begin
  cbAccount.Items  := AccountList.AccountStringList;
 end;

procedure TfrmTransactionList.acUpdateExecute(Sender: TObject);
var
  ResultEntry : TResultEntry;
  JournalDetailEntry:TJournalDetailEntry;
  begin

   TransactionList.Account := ActToInt(cbAccount.Text);
   JournalDetailEntry := TJournalDetailEntry.Create;
   StringGrid1.Clear;
   StringGrid1.RowCount:=Length(TransactionList.TransNos)+1;
   StringGrid1.cols[0].Append(HdrTransNo);
   StringGrid1.cols[1].Append(HdrTransRow);
   StringGrid1.cols[2].Append(HdrText);
   StringGrid1.cols[3].Append(HdrDr);
   StringGrid1.cols[4].Append(HdrCr);
   With TransactionList do for ResultEntry in TransNos do
      begin
        StringGrid1.cols[0].Append(IntToStr(ResultEntry.TransactionNo));
        StringGrid1.cols[1].Append(IntToStr(ResultEntry.TransactionRow));
        With ResultEntry do
          JournalDetailEntry.Load(TransactionNo, TransactionRow);
        StringGrid1.cols[2].Append(JournalDetailEntry.Text);
        case JournalDetailEntry.DrCr of
          dr:begin
               StringGrid1.cols[3].Append(IntToStr(JournalDetailEntry.Amount));
               StringGrid1.cols[4].Append('');
          end;
          cr:begin
               StringGrid1.cols[3].Append('');
               StringGrid1.cols[4].Append(IntToStr(JournalDetailEntry.Amount));
          end;
        end;
      end;
    JournalDetailEntry.Free;
  end;

procedure TfrmTransactionList.cbAccountChange(Sender: TObject);
begin
  acUpdate.Execute;
end;

end.

