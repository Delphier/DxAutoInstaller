program DxAutoInstaller;

{$R *.dres}

uses
  Vcl.Forms,
  MainFrm in 'Sources\MainFrm.pas' {MainForm},
  DxAutoInstaller.Core in 'Sources\DxAutoInstaller.Core.pas',
  DxAutoInstaller.Utils in 'Sources\DxAutoInstaller.Utils.pas',
  DxAutoInstaller.Options in 'Sources\DxAutoInstaller.Options.pas',
  DxAutoInstaller.Resources in 'Sources\DxAutoInstaller.Resources.pas' {DMResources: TDataModule},
  DxAutoInstaller.DevExpress in 'Sources\DxAutoInstaller.DevExpress.pas',
  DxAutoInstaller.UI.TreeList in 'Sources\DxAutoInstaller.UI.TreeList.pas',
  DxAutoInstaller.Installations in 'Sources\DxAutoInstaller.Installations.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := TApp.Name;
  Application.CreateForm(TDMResources, DMResources);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
