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
    row:integer;
  begin
   StringGrid1.Row:=0;
   TransactionList.Account := ActToInt(cbAccount.Text);
   JournalDetailEntry := TJournalDetailEntry.Create;
   StringGrid1.Clear;
   StringGrid1.RowCount:=Length(TransactionList.TransNos)+1;
   StringGrid1.Cells[0,0] := HdrTransNo;
  // StringGrid1.cols[0].Append(HdrTransNo);
  StringGrid1.Cells[1,0] := HdrTransRow;
  //  StringGrid1.cols[1].Append(HdrTransRow);
  StringGrid1.Cells[2,0] := HdrText;
  //  StringGrid1.cols[2].Append(HdrText);
  StringGrid1.Cells[3,0] := HdrDr;
  //  StringGrid1.cols[3].Append(HdrDr);
  StringGrid1.Cells[4,0] := HdrCr;
  //  StringGrid1.cols[4].Append(HdrCr);
  row := 0;
  With TransactionList do for ResultEntry in TransNos do
      begin
        Inc(row);
        StringGrid1.Cells[0,row] := IntToStr(ResultEntry.TransactionNo);
//        StringGrid1.cols[0].Append(IntToStr(ResultEntry.TransactionNo));
        StringGrid1.Cells[1,row] := IntToStr(ResultEntry.TransactionRow);
//        StringGrid1.cols[1].Append(IntToStr(ResultEntry.TransactionRow));
        With ResultEntry do
          JournalDetailEntry.Load(TransactionNo, TransactionRow);
        StringGrid1.Cells[2,row] := JournalDetailEntry.Text;
//        StringGrid1.cols[2].Append(JournalDetailEntry.Text);
        case JournalDetailEntry.DrCr of
          dr:begin
            StringGrid1.Cells[3,row] := IntToStr(JournalDetailEntry.Amount);
//            StringGrid1.cols[3].Append(IntToStr(JournalDetailEntry.Amount));
            StringGrid1.Cells[4,row] := '';
//               StringGrid1.cols[4].Append('');
          end;
          cr:begin
               StringGrid1.Cells[3,row] := '';
//               StringGrid1.cols[3].Append('');
               StringGrid1.Cells[4,row] := IntToStr(JournalDetailEntry.Amount);
//               StringGrid1.cols[4].Append(IntToStr(JournalDetailEntry.Amount));
          end;
        end;
        // Then, show the opposite side...
      end;
    JournalDetailEntry.Free;
  end;

procedure TfrmTransactionList.cbAccountChange(Sender: TObject);
begin
  acUpdate.Execute;
end;

end.

