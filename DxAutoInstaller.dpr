program DxAutoInstaller;

{$R *.dres}

uses
  Forms,
  MainFrm in 'Sources\MainFrm.pas' {MainForm},
  DxProfile in 'Sources\DxProfile.pas',
  DxComponent in 'Sources\DxComponent.pas',
  DxComponentFactory in 'Sources\DxComponentFactory.pas',
  DxInstaller in 'Sources\DxInstaller.pas',
  DxIDE in 'Sources\DxIDE.pas',
  DxQuantumTreeList in 'Sources\DxQuantumTreeList.pas' {DxResourceModule: TDataModule},
  DxCookies in 'Sources\DxCookies.pas',
  DxProgress in 'Sources\DxProgress.pas' {DxProgressForm},
  DxUtils in 'Sources\DxUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DxAutoInstaller';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
