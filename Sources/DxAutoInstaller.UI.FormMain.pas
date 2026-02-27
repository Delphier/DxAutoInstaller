{*******************************************************}
{                                                       }
{               DxAutoInstaller Library                 }
{                                                       }
{      https://github.com/Delphier/DxAutoInstaller      }
{                                                       }
{      Copyright(c) 2014-2026 faceker@gmail.com         }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DxAutoInstaller.UI.FormMain;

interface

uses
  System.Classes, System.Actions,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ActnList, Vcl.Menus,
  cxControls, cxContainer, cxGraphics, dxCoreGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  cxButtons, cxEdit, cxTextEdit, cxMaskEdit, cxButtonEdit,
  DxAutoInstaller.UI.TreeList,
  DxAutoInstaller.Installations;

type
  TFormMain = class(TForm)
    PanelTop: TPanel;
    ImageLogo: TImage;
    LblAppName: TLabel;
    Label2: TLabel;
    LblVersion: TLabel;
    PageControl: TPageControl;
    TabInstall: TTabSheet;
    TabUninstall: TTabSheet;
    TabTools: TTabSheet;
    TabAbout: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LinkGitHub: TLinkLabel;
    LinkDevExpressDocs: TLinkLabel;
    LinkEmail: TLinkLabel;
    Label7: TLabel;
    Label8: TLabel;
    EditRootDir: TcxButtonEdit;
    EditVersion: TEdit;
    PanTreeList: TPanel;
    ActionList1: TActionList;
    ActInstall: TAction;
    ActUninstall: TAction;
    ActExit: TAction;
    Label9: TLabel;
    ListViewUninstall: TListView;
    ChkShowAllComponents: TCheckBox;
    GroupBox1: TGroupBox;
    BtnManifest: TcxButton;
    ActManifestExport: TAction;
    ActManifestDelete: TAction;
    MemoChangelog: TMemo;
    GroupBox2: TGroupBox;
    Button1: TcxButton;
    ActSearchNewPackages: TAction;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    BtnExecute: TcxButton;
    BtnExit: TcxButton;
    PanelBottom: TPanel;
    LblCurrentManifest: TLabel;
    LblCustomManifest: TLinkLabel;
    LblCustomManifestNote: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure EditRootDirPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure ChkShowAllComponentsClick(Sender: TObject);
    procedure ActInstallExecute(Sender: TObject);
    procedure ActUninstallExecute(Sender: TObject);
    procedure ActManifestDeleteExecute(Sender: TObject);
    procedure ActManifestExportExecute(Sender: TObject);
    procedure LblCustomManifestLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure ActSearchNewPackagesExecute(Sender: TObject);
    procedure LinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
    procedure ActExitExecute(Sender: TObject);
  private
    { Private declarations }
    FTreeList: TTreeList;
    FInstallations: TInstallations;
    procedure ShowManifestStatus;
    procedure FormatLinkLabel(ALinkLable: TLinkLabel; const AIsEmail: Boolean = False);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  System.SysUtils, System.StrUtils,
  Winapi.Windows, Winapi.ShellAPI,
  Vcl.FileCtrl,
  DxAutoInstaller.Core,
  DxAutoInstaller.DevExpress,
  DxAutoInstaller.Utils,
  DxAutoInstaller.Resources;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
  LblAppName.Caption := Application.Title;
  LblVersion.Caption := TApp.Version;
  LoadResourceToStream(ImageLogo.Picture.LoadFromStream, 'Logo');
  PageControl.ActivePage := TabInstall;
  PageControl.OnChange(PageControl);
  if Height > Screen.WorkAreaHeight then Height := Screen.WorkAreaHeight;

  PanTreeList.BevelKind := TBevelKind.bkNone;
  FTreeList := TTreeList.Create(PanTreeList);
  for var IDE in TIDEList.Default do ListViewUninstall.AddItem(IDE.Name, IDE);
  FormatLinkLabel(LinkGitHub);
  FormatLinkLabel(LinkDevExpressDocs);
  FormatLinkLabel(LinkEmail, True);
  LoadResourceToStream(MemoChangelog.Lines.LoadFromStream, 'Changelog');
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FTreeList.Free;
  FInstallations.Free;
end;

procedure TFormMain.FormActivate(Sender: TObject);
begin
  OnActivate := nil;
  TManifest.CreateInstance;
  ShowManifestStatus;
end;

procedure TFormMain.PageControlChange(Sender: TObject);
begin
  ChkShowAllComponents.Visible := PageControl.ActivePage = TabInstall;
  if PageControl.ActivePage = TabInstall then BtnExecute.Action := ActInstall
  else if PageControl.ActivePage = TabUninstall then BtnExecute.Action := ActUninstall
  else BtnExecute.Visible := False;
end;

procedure TFormMain.EditRootDirPropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  Dirs: TArray<string>;
  RootDir: TRootDir;
begin
  if not SelectDirectory('', Dirs) then Exit;
  Screen.Cursor := crHourGlass;
  try
    RootDir := Dirs[0];
    EditRootDir.Text := RootDir;
    EditVersion.Text := RootDir.Version.ToText;

    FInstallations.Free;
    FInstallations := TInstallations.Create(RootDir, TManifest.Instance);
    for var Installation in FInstallations do Installation.Components.CheckAll(True);
    FTreeList.Installations := FInstallations;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TFormMain.ChkShowAllComponentsClick(Sender: TObject);
begin
  FTreeList.ShowAllComponents := ChkShowAllComponents.Checked;
end;

procedure TFormMain.ActInstallExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ActUninstallExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.ShowManifestStatus;
begin
  LblCurrentManifest.Caption := Format('Current Manifest: <%s>', [IfThen(TManifest.Instance.IsCustom, 'Custom', 'Built-in')]);
  LblCustomManifest.Caption := Format('Custom Manifest: <a href="%s">%s</a>', [TManifest.CustomFileName, TManifest.CustomFileName]);
  LblCustomManifest.Hint := TManifest.CustomFileName;
  BtnManifest.Action := if TManifest.CustomFileExists then ActManifestDelete else ActManifestExport;
  LblCustomManifest.Visible := TManifest.CustomFileExists;
  LblCustomManifestNote.Visible := LblCustomManifest.Visible;
end;

procedure TFormMain.ActManifestDeleteExecute(Sender: TObject);
begin
  System.SysUtils.DeleteFile(TManifest.CustomFileName);
  ShowManifestStatus;
end;

procedure TFormMain.ActManifestExportExecute(Sender: TObject);
begin
  TManifest.Export;
  ShowManifestStatus;
end;

procedure TFormMain.LblCustomManifestLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(Application.Handle, 'Open', 'explorer.exe', PChar(Format('/select,"%s"', [Link])), nil, SW_SHOWNORMAL);
end;

procedure TFormMain.ActSearchNewPackagesExecute(Sender: TObject);
begin
//
end;

procedure TFormMain.FormatLinkLabel(ALinkLable: TLinkLabel; const AIsEmail: Boolean);
begin
  ALinkLable.ShowHint := True;
  ALinkLable.Hint := ALinkLable.Caption;
  ALinkLable.Caption := Format('<a href="%s%s">%s</a>', [IfThen(AIsEmail, 'mailto:'), ALinkLable.Caption, ALinkLable.Caption]);
end;

procedure TFormMain.LinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(Application.Handle, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

procedure TFormMain.ActExitExecute(Sender: TObject);
begin
  Close;
end;

end.
