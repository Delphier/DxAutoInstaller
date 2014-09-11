{*******************************************************}
{                                                       }
{          DxAutoInstaller Progress Classes             }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, DxIDE, DxProfile, DxInstaller, ActnList,
  ImgList, cxGraphics, DxUtils;

type
  TDxProgressForm = class(TDxForm)
    PanLogs: TPanel;
    ProgressLogs: TMemo;
    ProgressTitle: TLabel;
    Actions: TActionList;
    ActionStop: TAction;
    ActionClose: TAction;
    Images: TcxImageList;
    BtnAction: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ActionCloseExecute(Sender: TObject);
    procedure ActionStopExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FInstaller: TDxInstaller;
    FTarget: String;
    FTargetLogs: TStringList;
  public
    { Public declarations }
    property Installer: TDxInstaller read FInstaller write FInstaller;
    procedure Initial();
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
  end;

implementation

{$R *.dfm}

{ TDxProgressForm }

procedure TDxProgressForm.ActionCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TDxProgressForm.ActionStopExecute(Sender: TObject);
begin
  Installer.Stop;
end;

procedure TDxProgressForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := Installer.State = dxisNormal;
end;

procedure TDxProgressForm.FormCreate(Sender: TObject);
begin
  SetWindowLong(Handle, GWL_EXSTYLE, WS_EX_APPWINDOW);
  Caption := Application.Title;
  FInstaller := nil;
  FTarget := EmptyStr;
  FTargetLogs := TStringList.Create;
end;

procedure TDxProgressForm.FormDestroy(Sender: TObject);
begin
  FTargetLogs.Free;
end;

procedure TDxProgressForm.Initial;
begin
  ProgressLogs.Clear;
  BtnAction.Action := ActionClose;
  Show;
end;

procedure TDxProgressForm.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  if Installer = nil then Exit;
  if Installer.State = dxisRunning then BtnAction.Action := ActionStop;
  ProgressTitle.Caption := IDE.Name;
  if Component <> nil then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Component.ComponentName;
  if Task <> EmptyStr then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Task;
  if FTarget <> Target then begin
    FTargetLogs.Clear;
    ProgressLogs.Lines.Add(StringOfChar('-', 100));
  end;
  FTarget := Target;
  if Target <> EmptyStr then ProgressTitle.Caption := ProgressTitle.Caption + ' > ' + Target;
end;

procedure TDxProgressForm.UpdateProgressState(const StateText: String);
begin
  if Installer = nil then Exit;
  ProgressLogs.Lines.Add(StateText);
  FTargetLogs.Add(StateText);
  case Installer.State of
    dxisNormal:  begin
                   BtnAction.Action := ActionClose;
                   ProgressTitle.Caption := StateText;
                   Close;
                   ShowModal;
                 end;
    dxisError:   if Application.MessageBox(PChar(FTargetLogs.Text + CRLF + 'An error has occurred, do you want to continue?'),
                                           'Confirmation', MB_ICONQUESTION + MB_OKCANCEL) = IDCANCEL then ActionStop.Execute;
  end;
end;

end.
