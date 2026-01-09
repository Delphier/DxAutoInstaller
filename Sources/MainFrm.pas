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
  ActnList, Buttons, DxQuantumTreeList, DxInstaller, DxProgress, DxIDE, DxUtils,
  System.Actions, System.ImageList, cxLookAndFeels, cxLookAndFeelPainters,
  Vcl.Menus, cxImageList, cxButtons;

{$WARN UNIT_PLATFORM OFF}

type
  TMainForm = class(TDxForm)
    PanelTop: TPanel;
    ImageLogo: TImage;
    LblAppName: TLabel;
    Label2: TLabel;
    LblVersion: TLabel;
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
    MemoChangelog: TMemo;
    GroupBox2: TGroupBox;
    Button1: TButton;
    SearchNewPackages: TAction;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BtnRun: TcxButton;
    BtnExit: TcxButton;
    PanelBottom: TPanel;
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
    procedure SearchNewPackagesExecute(Sender: TObject);
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
  ShellAPI, FileCtrl, IOUtils, DxAutoInstaller.Core, DxAutoInstaller.Utils;

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Height > Screen.WorkAreaHeight then Height := Screen.WorkAreaHeight;
  Caption := Application.Title;
  LblAppName.Caption := Application.Title;
  LblVersion.Caption := TApp.Version;
  LoadResourceToStream(ImageLogo.Picture.LoadFromStream, 'Logo');
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
  LoadResourceToStream(MemoChangelog.Lines.LoadFromStream, 'Changelog');
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
    ShowInformation('Please close all running IDEs.');
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

procedure TMainForm.SearchNewPackagesExecute(Sender: TObject);
var
  List: TStringList;
begin
  Screen.Cursor := crHourGlass;
  List := TStringList.Create;
  try
    FInstaller.SearchNewPackages(List);
    if List.Count > 0 then ShowInformation(List.Text);
  finally
    List.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TMainForm.InstallExecute(Sender: TObject);
var
  IDEs: TDxIDEArray;
begin
  IDEs := FTreeList.GetSelectedIDEs;
  if Length(IDEs) = 0 then Exit;
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
  Arr: TArray<String>;
  Dir: String;
  I: Integer;
begin
  if Win32MajorVersion < 6 then begin
    if not SelectDirectory('Select Installation File Directory:', '', Dir, [sdNewUI], Self) then Exit;
  end else begin
    if not SelectDirectory('', Arr) then Exit;
    Dir := Arr[0];
  end;
  if not SysUtils.DirectoryExists(Dir) then Exit;
  EditInstallFileDir.Text := Dir;
  I := FInstaller.Profile.GetDxBuildNumber(Dir);
  EditVersion.Text := FInstaller.Profile.GetDxBuildNumberAsVersion(I);
  Screen.Cursor := crHourGlass;
  try
    FInstaller.InstallFileDir := Dir;
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
