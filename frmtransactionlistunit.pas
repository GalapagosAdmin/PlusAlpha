unit frmTransactionListUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  //ListFilterEdit,
  Forms, Controls, Graphics,
  Dialogs, ExtCtrls, Grids, StdCtrls, ActnList, libpa, paTransactionList;

type

  { TfrmTransactionList }

  TfrmTransactionList = class(TFrame)
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
 HdrDate='Date';
 HdrText='Text';
 HdrAmt='Amount';
 HdrDr='Dr';
 HdrCr='Cr';
 DefAccountDropdownText = ' --- Select Account ---';
{$R *.lfm}

{ TfrmTransactionList }

procedure TfrmTransactionList.FormShow(Sender: TObject);

 begin
  cbAccount.Items  := AccountList.AccountStringList;
  cbAccount.Text :=  DefAccountDropdownText
 end;

procedure TfrmTransactionList.acUpdateExecute(Sender: TObject);
  Const
    HeaderRow  = 0;
    TransNoCol = 0;
    TransRowCol= 1;
    DateCol    = 2;
    TextCol    = 3;
    DrCol      = 4;
    CrCol      = 5;
    MaxCol     = 5;

  var
    ResultEntry : TResultEntry;
    JournalDetailEntry:TJournalDetailEntry;
    GridRow:integer;
  begin
   JournalDetailEntry := TJournalDetailEntry.Create;
   StringGrid1.Clear;
   StringGrid1.Row := HeaderRow;
   StringGrid1.RowCount := Length(TransactionList.TransNos)+1;
   StringGrid1.ColCount := MaxCol+1;
   StringGrid1.Cells[TransNoCol, HeaderRow] := HdrTransNo;
  // StringGrid1.cols[0].Append(HdrTransNo);
  StringGrid1.Cells[TransRowCol, HeaderRow] := HdrTransRow;
  //  StringGrid1.cols[1].Append(HdrTransRow);
  StringGrid1.Cells[DateCol, HeaderRow] := HdrDate;
  StringGrid1.Cells[TextCol, HeaderRow] := HdrText;
  //  StringGrid1.cols[2].Append(HdrText);
  StringGrid1.Cells[DrCol, HeaderRow] := HdrDr;
  //  StringGrid1.cols[3].Append(HdrDr);
  StringGrid1.Cells[CrCol, HeaderRow] := HdrCr;
  //  StringGrid1.cols[4].Append(HdrCr);
  GridRow := 0;
  With TransactionList do for ResultEntry in TransNos do
      begin
        Inc(GridRow);
        With ResultEntry do
          JournalDetailEntry.Load(TransactionNo, TransactionRow);
        StringGrid1.Cells[TransNoCol, GridRow] := IntToStr(ResultEntry.TransactionNo);
//        StringGrid1.cols[0].Append(IntToStr(ResultEntry.TransactionNo));
        StringGrid1.Cells[TransRowCol, GridRow] := IntToStr(ResultEntry.TransactionRow);
//        StringGrid1.cols[1].Append(IntToStr(ResultEntry.TransactionRow));
        StringGrid1.Cells[DateCol, GridRow] := JournalDetailEntry.DisplayDate;
        StringGrid1.Cells[TextCol, GridRow] := JournalDetailEntry.Text;
//        StringGrid1.cols[2].Append(JournalDetailEntry.Text);
        case JournalDetailEntry.DrCr of
          dr:begin
            StringGrid1.Cells[DrCol, GridRow] := IntToStr(JournalDetailEntry.Amount);
//            StringGrid1.cols[3].Append(IntToStr(JournalDetailEntry.Amount));
            StringGrid1.Cells[CrCol, GridRow] := '';
//               StringGrid1.cols[4].Append('');
          end;
          cr:begin
               StringGrid1.Cells[DrCol,GridRow] := '';
//               StringGrid1.cols[3].Append('');
               StringGrid1.Cells[CrCol, GridRow] := IntToStr(JournalDetailEntry.Amount);
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

