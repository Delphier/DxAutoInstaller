unit DxAutoInstaller.UI.ProgressForm;

interface

uses
  System.Classes, System.Actions, Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.Menus, Vcl.ActnList,
  cxClasses, cxGraphics, cxControls, cxContainer, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutControl, dxLayoutContainer, dxLayoutControlAdapters, dxLayoutcxEditAdapters,
  cxEdit, cxTextEdit, cxMemo, cxButtons, cxProgressBar;

type
  TProgressForm = class(TForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    ProgressBar: TcxProgressBar;
    Log: TcxMemo;
    BtnOperation: TcxButton;
    ActionList1: TActionList;
    ActAbort: TAction;
    ActClose: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActAbortExecute(Sender: TObject);
    procedure ActCloseExecute(Sender: TObject);
  private
    { Private declarations }
    function DoAbort: Boolean;
  public
    { Public declarations }
    procedure Done;
  end;

implementation

uses
  System.Threading,
  DxAutoInstaller.Resources, DxAutoInstaller.Tasks, DxAutoInstaller.Utils;

{$R *.dfm}

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

procedure TProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := (BtnOperation.Action = ActClose) or DoAbort;
end;

function TProgressForm.DoAbort: Boolean;
begin
  Result := TMessageBox.Confirm('Do you want to abort this task?', ['Abort', 'Cancel']);
  if not Result then Exit;

  TTask.Aborted := True;
  ActAbort.Enabled := False;
  Screen.Cursor := crHourGlass;
  try
    while TTask.Instance.Status <> TTaskStatus.Completed do Application.ProcessMessages;
  finally
    Screen.Cursor := crDefault;
  end;
  Done;
end;

procedure TProgressForm.ActAbortExecute(Sender: TObject);
begin
  DoAbort;
end;

procedure TProgressForm.Done;
begin
  BtnOperation.Action := ActClose;
end;

procedure TProgressForm.ActCloseExecute(Sender: TObject);
begin
  Close;
end;

end.
