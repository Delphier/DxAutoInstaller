{*******************************************************}
{                                                       }
{          DxAutoInstaller MainForm Classes             }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit MainFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, dxGDIPlusClasses, StdCtrls, ComCtrls, ImgList, cxGraphics,
  ActnList, Buttons, DxQuantumTreeList, DxInstaller, DxProgress, DxIDE, DxUtils;

{$WARN UNIT_PLATFORM OFF}

type
  TMainForm = class(TDxForm)
    Panel1: TPanel;
    Image1: TImage;
    LblAppName: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageFuns: TPageControl;
    TabInstall: TTabSheet;
    TabUninstall: TTabSheet;
    TabTools: TTabSheet;
    TabAbout: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LinkDownApp: TLinkLabel;
    LinkDownDoc: TLinkLabel;
    LinkEmail: TLinkLabel;
    Label7: TLabel;
    Label8: TLabel;
    EditInstallFileDir: TButtonedEdit;
    EditVersion: TEdit;
    PanTreeList: TPanel;
    ActionBase: TActionList;
    ImageSmall: TcxImageList;
    Install: TAction;
    Uninstall: TAction;
    ExitApp: TAction;
    BtnRun: TButton;
    BtnExit: TButton;
    Label9: TLabel;
    IDEListView: TListView;
    ChkHideBaseComponents: TCheckBox;
    GroupBox1: TGroupBox;
    LblCurrentProfile: TLabel;
    BtnProfile: TButton;
    ProfileExport: TAction;
    ProfileDelete: TAction;
    LblCustomProfile: TLinkLabel;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure URLLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure InstallExecute(Sender: TObject);
    procedure UninstallExecute(Sender: TObject);
    procedure ExitAppExecute(Sender: TObject);
    procedure PageFunsChange(Sender: TObject);
    procedure EditInstallFileDirRightButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InitialIDEListView();
    procedure InitialProfileInfo();
    procedure RefreshTreeList(Sender: TObject);
    procedure ProfileExportExecute(Sender: TObject);
    procedure ProfileDeleteExecute(Sender: TObject);
  private
    { Private declarations }
    FInstaller: TDxInstaller;
    FTreeList: TDxQuantumTreeList;
    FProgressForm: TDxProgressForm;
    procedure RunInstaller(Action: TDxInstallerAction; const IDEArray: TDxIDEArray);
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  ShellAPI, FileCtrl;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  LblAppName.Caption := Application.Title;
  PageFuns.ActivePage := TabInstall;

  // Initial Install Page;
  FInstaller := TDxInstaller.Create;
  PanTreeList.BevelKind := bkNone;
  FTreeList := TDxQuantumTreeList.Create(FInstaller, PanTreeList);
  FProgressForm := TDxProgressForm.Create(nil);
  FProgressForm.Installer := FInstaller;
  FInstaller.OnUpdateProgress := FProgressForm.UpdateProgress;
  FInstaller.OnUpdateProgressState := FProgressForm.UpdateProgressState;

  // Initial Uninstall Page;
  InitialIDEListView();

  // Initial Tools Page;
  InitialProfileInfo();

  // Initial About Page;
  LinkDownApp.Caption := Format('<a href="%s">%s</a>', [LinkDownApp.Caption, LinkDownApp.Caption]);
  LinkDownDoc.Caption := Format('<a href="%s">%s</a>', [LinkDownDoc.Caption, LinkDownDoc.Caption]);
  LinkEmail.Caption := Format('<a href="mailto:%s">%s</a>', [LinkEmail.Caption, LinkEmail.Caption]);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FProgressForm.Free;
  FTreeList.Free;
  FInstaller.Free;
end;

procedure TMainForm.InitialIDEListView;
var
  I: Integer;
begin
  IDEListView.Clear;
  for I := 0 to FInstaller.IDEs.Count - 1 do IDEListView.AddItem(FInstaller.IDEs[I].Name, nil);
end;

procedure TMainForm.InitialProfileInfo;
var
  FileName: String;
begin
  if FInstaller.Profile.IsCustomProfile
    then LblCurrentProfile.Caption := 'Current Profile: <Custom>'
    else LblCurrentProfile.Caption := 'Current Profile: <Built-in>';

  FileName := FInstaller.Profile.GetCustomProfileFileName;
  LblCustomProfile.Caption := Format('Custom Profile: <a href="%s">%s</a>', [FileName, FileName]);
  LblCustomProfile.Visible := FileExists(FileName);
  if FileExists(FileName)
    then BtnProfile.Action := ProfileDelete
    else BtnProfile.Action := ProfileExport;
end;

procedure TMainForm.PageFunsChange(Sender: TObject);
begin
  if PageFuns.ActivePage = TabInstall then BtnRun.Action := Install
  else if PageFuns.ActivePage = TabUninstall then BtnRun.Action := Uninstall
  else BtnRun.Visible := False;
  ChkHideBaseComponents.Visible := PageFuns.ActivePage = TabInstall;
end;

procedure TMainForm.ProfileDeleteExecute(Sender: TObject);
begin
  DeleteFile(FInstaller.Profile.GetCustomProfileFileName);
  InitialProfileInfo;
end;

procedure TMainForm.ProfileExportExecute(Sender: TObject);
begin
  FInstaller.Profile.ExportBuiltInProfile(FInstaller.Profile.GetCustomProfileFileName);
  InitialProfileInfo;
end;

procedure TMainForm.RefreshTreeList(Sender: TObject);
begin
  FTreeList.DispData(TCheckBox(Sender).Checked);
end;

procedure TMainForm.RunInstaller(Action: TDxInstallerAction; const IDEArray: TDxIDEArray);
begin
  if FInstaller.IDEs.AnyInstanceRunning then begin
    Application.MessageBox('Please close all running IDEs.', 'Information', MB_ICONINFORMATION);
    Exit;
  end;
  Hide;
  try
    FProgressForm.Initial;
    Action(IDEArray);
  finally
    Show;
  end;
end;

procedure TMainForm.InstallExecute(Sender: TObject);
var
  IDEs: TDxIDEArray;
  I: Integer;
begin
  if FTreeList.GetSelectedComponentCount = 0 then Exit;
  for I := 0 to FInstaller.IDEs.Count - 1 do begin
    if FInstaller.GetInstallComponentCount(FInstaller.IDEs[I]) > 0 then begin
      SetLength(IDEs, Length(IDEs) + 1);
      IDEs[Length(IDEs) - 1] := FInstaller.IDEs[I];
    end;
  end;
  RunInstaller(FInstaller.Install, IDEs);
end;

procedure TMainForm.UninstallExecute(Sender: TObject);
var
  IDEs: TDxIDEArray;
  ListItem: TListItem;
begin
  for ListItem in IDEListView.Items do begin
    if ListItem.Checked then begin
      SetLength(IDEs, Length(IDEs) + 1);
      IDEs[Length(IDEs) - 1] := FInstaller.IDEs[ListItem.Index];
    end;
  end;

  if Length(IDEs) = 0 then Exit;
  RunInstaller(FInstaller.Uninstall, IDEs);
end;

procedure TMainForm.EditInstallFileDirRightButtonClick(Sender: TObject);
var
  S: String;
  I: Integer;
begin
  S := EditInstallFileDir.Text;
  if not SelectDirectory('Select Installation File Directory:', '', S, [sdNewUI], Self) then Exit;
  if not SysUtils.DirectoryExists(S) then Exit;
  EditInstallFileDir.Text := S;
  I := FInstaller.Profile.GetDxBuildNumber(S);
  EditVersion.Text := FInstaller.Profile.GetDxBuildNumberAsVersion(I);
  Screen.Cursor := crHourGlass;
  try
    FInstaller.InstallFileDir := S;
  finally
    Screen.Cursor := crDefault;
  end;
  RefreshTreeList(ChkHideBaseComponents);
end;

procedure TMainForm.ExitAppExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.URLLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(Application.Handle, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

end.
