program DxAutoInstaller;

{$R *.dres}

uses
  Vcl.Forms,
  MainFrm in 'Sources\MainFrm.pas' {MainForm},
  DxComponent in 'Sources\DxComponent.pas',
  DxComponentFactory in 'Sources\DxComponentFactory.pas',
  DxIDE in 'Sources\DxIDE.pas',
  DxInstaller in 'Sources\DxInstaller.pas',
  DxProfile in 'Sources\DxProfile.pas',
  DxProgress in 'Sources\DxProgress.pas' {DxProgressForm},
  DxQuantumTreeList in 'Sources\DxQuantumTreeList.pas' {DxResourceModule: TDataModule},
  DxUtils in 'Sources\DxUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DxAutoInstaller';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
