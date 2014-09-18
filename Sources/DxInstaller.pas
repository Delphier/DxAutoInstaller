{*******************************************************}
{                                                       }
{          DxAutoInstaller Installer Classes            }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxInstaller;

interface

uses
  Forms, Classes, SysUtils, DxIDE, DxComponent, DxProfile;

type
  TDxInstallOption = (dxioAddBrowsingPath, dxioInstallIBXPackages, dxioInstallTeeChartPackages, dxioCompileWin64Library);
  TDxInstallOptions = set of TDxInstallOption;

  TDxInstallerState = (dxisNormal, dxisRunning, dxisStopped, dxisError);
  TDxInstallerAction = procedure(const IDEArray: TDxIDEArray) of object;

  TDxUpdateProgressEvent = procedure(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String) of object;
  TDxUpdateProgressStateEvent = procedure(const StateText: String) of object;

  TDxInstaller = class
  const
    DxEnvironmentVariableName = 'DXVCL';
  private
    FIDEs: TDxIDEs;
    FProfile: TDxProfile;
    FInstallFileDir: String;
    FComponents: array of TDxComponentList;
    FOptions: array of TDxInstallOptions;
    FState: TDxInstallerState;
    FOnUpdateProgress: TDxUpdateProgressEvent;
    FOnUpdateProgressState: TDxUpdateProgressStateEvent;
    procedure SetInstallFileDir(const Value: String);
    function GetComponents(IDE: TDxIDE): TDxComponentList;
    function GetOptions(IDE: TDxIDE): TDxInstallOptions;
    procedure SetOptions(IDE: TDxIDE; const Value: TDxInstallOptions);
    procedure SetState(const Value: TDxInstallerState);
    procedure Install(IDE: TDxIDE); overload;
    procedure InstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponent; Package: TDxPackage);
    procedure Uninstall(IDE: TDxIDE); overload;
    procedure UninstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponentProfile; const PackageBaseName: String);
    procedure UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
    procedure UpdateProgressState(const StateText: String);
    procedure CheckStoppedState();
  public
    constructor Create();
    destructor Destroy; override;
    property IDEs: TDxIDEs read FIDEs;
    property Profile: TDxProfile read FProfile;
    property InstallFileDir: String read FInstallFileDir write SetInstallFileDir;
    property Components[IDE: TDxIDE]: TDxComponentList read GetComponents;
    property Options[IDE: TDxIDE]: TDxInstallOptions read GetOptions write SetOptions;
    property State: TDxInstallerState read FState;
    property OnUpdateProgress: TDxUpdateProgressEvent read FOnUpdateProgress write FOnUpdateProgress;
    property OnUpdateProgressState: TDxUpdateProgressStateEvent read FOnUpdateProgressState write FOnUpdateProgressState;
    procedure Install(const IDEArray: TDxIDEArray); overload;
    procedure Uninstall(const IDEArray: TDxIDEArray); overload;
    procedure Stop();
    function GetInstallComponentCount(IDE: TDxIDE): Integer;
    procedure SearchNewPackages(List: TStringList);
    class function GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform = Win32): String;
    class function GetInstallSourcesDir(const InstallFileDir: String): String;
  end;

const
  DxInstallOptionNames: array[TDxInstallOption] of String = ('Add Browsing Path', 'Install IBX Packages', 'Install TeeChart Packages', 'Compile Win64 Library');

implementation

uses
  DxComponentFactory, DxUtils;

{ TDxInstaller }

constructor TDxInstaller.Create;
var
  I: Integer;
begin
  inherited Create;
  FIDEs := TDxIDEs.Create;
  for I := 0 to FIDEs.Count - 1 do FIDEs[I].OutputCallback := UpdateProgressState;
  FProfile := TDxProfile.Create;
  FInstallFileDir := EmptyStr;
  SetLength(FComponents, FIDEs.Count);
  SetLength(FOptions, FIDEs.Count);
  FState := dxisNormal;
end;

destructor TDxInstaller.Destroy;
var
  I: Integer;
begin
  for I := Low(FComponents) to High(FComponents) do FreeAndNil(FComponents[I]);
  FProfile.Free;
  FIDEs.Free;
  inherited;
end;

function TDxInstaller.GetInstallComponentCount(IDE: TDxIDE): Integer;
var
  Comp: TDxComponent;
begin
  Result := 0;
  for Comp in Components[IDE] do if Comp.State = dxcsInstall then Inc(Result);
end;

class function TDxInstaller.GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
begin
  Result := IncludeTrailingPathDelimiter(InstallFileDir) + 'Library';
  if IDE <> nil then begin
    Result := Result + '\' + IDE.Name;
    if IDEPlatform = Win64 then Result := Result + '\' + DxIDEPlatformNames[IDEPlatform];
  end;
end;

class function TDxInstaller.GetInstallSourcesDir(const InstallFileDir: String): String;
begin
  Result := GetInstallLibraryDir(InstallFileDir, nil) + '\Sources';
end;

function TDxInstaller.GetComponents(IDE: TDxIDE): TDxComponentList;
begin
  Result := FComponents[IDEs.IndexOf(IDE)];
end;

function TDxInstaller.GetOptions(IDE: TDxIDE): TDxInstallOptions;
begin
  Result := FOptions[IDEs.IndexOf(IDE)];
end;

procedure TDxInstaller.SetOptions(IDE: TDxIDE; const Value: TDxInstallOptions);
var
  Options: TDxInstallOptions;
begin
  Options := Value;
  if (dxioCompileWin64Library in Options) and (not IsSupportWin64(IDE)) then Exclude(Options, dxioCompileWin64Library);
  FOptions[IDEs.IndexOf(IDE)] := Options;
end;

procedure TDxInstaller.SearchNewPackages(List: TStringList);
var
  Packages, DPKFileList: TStringList;
  Component: TDxComponentProfile;
  S, FileName: String;
  I: Integer;
begin
  List.Clear;
  if InstallFileDir = EmptyStr then Exit;

  Packages := TStringList.Create;
  DPKFileList := TStringList.Create;
  try
    for Component in Profile.Components do begin
      Packages.AddStrings(Component.RequiredPackages);
      Packages.AddStrings(Component.OptionalPackages);
      Packages.AddStrings(Component.OutdatedPackages);
    end;
    BuildFileList(IncludeTrailingPathDelimiter(InstallFileDir) + '*.dpk', DPKFileList, faAnyFile, True, True);
    for S in DPKFileList do begin
      FileName := ChangeFileExt(ExtractFileName(S), '');
      for I := Length(FileName) downto 1 do if CharInSet(FileName[I], ['0'..'9']) then Delete(FileName, I, 1) else Break;
      if SameText(FileName[Length(FileName)], 'D') then FileName := Copy(FileName, 1, Length(FileName) - 1)
      else if SameText(Copy(FileName, Length(FileName) - 1, 2), 'RS') then FileName := Copy(FileName, 1, Length(FileName) - 2);
      if Packages.IndexOf(FileName) < 0 then List.Add(S);
    end;
  finally
    Packages.Free;
    DPKFileList.Free;
  end;
end;

procedure TDxInstaller.SetInstallFileDir(const Value: String);
var
  Factory: TDxComponentFactory;
  I: Integer;
begin
  FInstallFileDir := Value;
  Factory := TDxComponentFactory.Create(Self);
  try
    for I := 0 to IDEs.Count - 1 do begin
      if FComponents[I] = nil then FComponents[I] := TDxComponentList.Create;
      Factory.BuildComponentList(IDEs[I], FComponents[I]);
    end;
  finally
    Factory.Free;
  end;
  for I := 0 to IDEs.Count - 1 do FOptions[I] := [dxioAddBrowsingPath];
end;

procedure TDxInstaller.SetState(const Value: TDxInstallerState);
begin
  if State = Value then Exit;
  if (Value = dxisStopped) and (State = dxisNormal) then Exit;
  FState := Value;
  case State of
    dxisNormal:   UpdateProgressState('Finished!');
    dxisStopped:  UpdateProgressState('Stopped.');
    dxisError:    UpdateProgressState('Error.');
  end;
  if State = dxisError then SetState(dxisRunning); // On Ignore Error.
end;

procedure TDxInstaller.Stop;
begin
  SetState(dxisStopped);
end;

procedure TDxInstaller.CheckStoppedState;
begin
  Application.ProcessMessages;
  if State = dxisStopped then begin
    SetState(dxisNormal);
    Abort;
  end;
end;

procedure TDxInstaller.Install(const IDEArray: TDxIDEArray);
var
  IDE: TDxIDE;
begin
  SetState(dxisRunning);
  for IDE in IDEArray do Install(IDE);
  SetState(dxisNormal);
end;

procedure TDxInstaller.Uninstall(const IDEArray: TDxIDEArray);
var
  IDE: TDxIDE;
begin
  SetState(dxisRunning);
  for IDE in IDEArray do Uninstall(IDE);
  SetState(dxisNormal);
end;

procedure TDxInstaller.Install(IDE: TDxIDE);
var
  Comp: TDxComponent;
  Package: TDxPackage;
  SourcesFileDir, InstallSourcesDir: String;
begin
  Uninstall(IDE);
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  for Comp in Components[IDE] do begin
    if Comp.State <> dxcsInstall then Continue;
    SourcesFileDir := IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, Comp.Profile.ComponentName));
    UpdateProgress(IDE, Comp.Profile, 'Copying', 'Sources Files');
    UpdateProgressState('Copying Files: ' + SourcesFileDir + '*.*');
    CopyFilesToDirectory(SourcesFileDir + '*.*', InstallSourcesDir);
    CopyFilesToDirectory(SourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win32));
    if dxioCompileWin64Library in Options[IDE] then
      CopyFilesToDirectory(SourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win64));
    for Package in Comp.Packages do if Package.Required then begin
      InstallPackage(IDE, Win32, Comp, Package);
      InstallPackage(IDE, Win64, Comp, Package);
    end;
  end;

  for Comp in Components[IDE] do begin
    if Comp.State <> dxcsInstall then Continue;
    for Package in Comp.Packages do if not Package.Required then begin
      InstallPackage(IDE, Win32, Comp, Package);
      InstallPackage(IDE, Win64, Comp, Package);
    end;
  end;

  IDE.AddToLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win32), Win32);
  if dxioCompileWin64Library in Options[IDE] then IDE.AddToLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win64), Win64);

  if dxioAddBrowsingPath in Options[IDE] then begin
    IDE.AddToLibraryBrowsingPath(InstallSourcesDir, Win32);
    if dxioCompileWin64Library in Options[IDE] then IDE.AddToLibraryBrowsingPath(InstallSourcesDir, Win64);
  end else begin
    IDE.AddToLibrarySearchPath(installSourcesDir, Win32);
    if dxioCompileWin64Library In Options[IDE] then IDE.AddToLibrarySearchPath(InstallSourcesDir, Win64)
  end;

  SetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName, InstallFileDir);
end;

procedure TDxInstaller.InstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponent; Package: TDxPackage);
var
  BPLPath, DCPPath: String;
  I: Integer;
  R: Boolean;
  ExtraOptions: String;
  InstallSourcesDir, InstallLibraryDir: String;
begin
  CheckStoppedState;
  if not Package.Exists then Exit;
  if (Package.Category = dxpcIBX) and (not (dxioInstallIBXPackages in Options[IDE])) then Exit;
  if (Package.Category = dxpcTeeChart) and (not (dxioInstallTeeChartPackages in Options[IDE])) then Exit;
  if (Package.Category = dxpcBDE) and (IDEPlatform = Win64) then Exit;
  if (IDEPlatform = Win64) and (not (dxioCompileWin64Library in Options[IDE])) then Exit;
  if (IDEPlatform = Win64) and (Package.Usage = dxpuDesigntimeOnly) then Exit;
  if not Package.Required then
    for I := 0 to Package.DependentComponents.Count - 1 do
      if Package.DependentComponents[I].State <> dxcsInstall then Exit;

  UpdateProgress(IDE, Component.Profile, 'Install Package', DxIDEPlatformNames[IDEPlatform] + ' > ' + Package.FullFileName);
  if IDEPlatform = Win32 then IDE.DCC := IDE.DCC32 else
  if IDEPlatform = Win64 then TDxBDSIDE(IDE).DCC := TDxBDSIDE(IDE).DCC64;

  BPLPath := IDE.BPLOutputPath[IDEPlatform];
  DCPPath := IDE.DCPOutputPath[IDEPlatform];
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE, IDEPlatform);
  ForceDirectories(BPLPath);
  ForceDirectories(DCPPath);
  // -$D- Debug Information;
  // -$L- Local Debug Symbols;
  // -$Y- Symbol Reference Info;
  // -Q   Quiet Compile;
  // -U   Unit Search Paths;
  // -R   Resource Search Paths;
  // -B   Build All Units;
  // -NU  Unit .dcu Output Directory; XE2+
  // -N0  Unit .dcu Output Directory; XE2-
  // -NO  Unit .obj Output Directory;
  // -A   Unit Alias;
  // -NS  Namespaces Search Paths;
  ExtraOptions :=
    ' -$D- -$L- -$Y- -Q ' +
    Format(' -U"%s" -U"%s" -R"%s" ', [DCPPath, InstallSourcesDir, InstallSourcesDir]) +
    Format(' -B -NU"%s" -N0"%s" -NO"%s" ', [InstallLibraryDir, InstallLibraryDir, InstallLibraryDir]) +
    '-AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE ' +
    '-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;IBX;VclTee; ';

    // **NOTE** Editing JclIDEUtils: Move CompileDelphiPackage(..., ExtraOptions) from Protected to Public:
    // 1. TJclBorRADToolInstallation.CompileDelphiPackage(..., ExtraOptions)
    // 2. TJclBDSInstallation.CompileDelphiPackage(..., ExtraOptions)
    R := IDE.CompileDelphiPackage(Package.FullFileName, BPLPath, DCPPath, ExtraOptions);
    if R and (IDEPlatform = Win32) and (Package.Usage <> dxpuRuntimeOnly) then
      R := IDE.RegisterPackage(Package.FullFileName, BPLPath, Package.Description);
    //then R := IDE.CompilePackage(Package.FullFileName, BPLPath, DCPPath)
    //else R := IDE.InstallPackage(Package.FullFileName, BPLPath, DCPPath);
  if R = False then SetState(dxisError);
end;

procedure TDxInstaller.Uninstall(IDE: TDxIDE);
var
  Comp: TDxComponentProfile;
  InstallFileDir, InstallLibraryDir, InstallSourcesDir: String;

  procedure UninstallPackages(List: TStringList);
  var
    Package: String;
  begin
    for Package in List do begin
      UninstallPackage(IDE, Win32, Comp, Package);
      UninstallPackage(IDE, Win64, Comp, Package);
    end;
  end;
begin
  for Comp in Profile.Components do begin
    UninstallPackages(Comp.RequiredPackages);
    UninstallPackages(Comp.OptionalPackages);
    UninstallPackages(Comp.OutdatedPackages);
  end;

  InstallFileDir := GetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName);
  if InstallFileDir = EmptyStr then Exit;
  InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE);
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  UpdateProgress(IDE, nil, 'Deleting', 'Installation Files');
  UpdateProgressState('Deleting Directory: ' + InstallLibraryDir);
  DxUtils.DeleteDirectory(InstallLibraryDir);
  IDE.RemoveFromLibrarySearchPath(InstallLibraryDir, Win32);
  IDE.RemoveFromLibrarySearchPath(InstallSourcesDir, Win32);
  IDE.RemoveFromLibraryBrowsingPath(InstallSourcesDir, Win32);
  if IsSupportWin64(IDE) then begin
    IDE.RemoveFromLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win64), Win64);
    IDE.RemoveFromLibrarySearchPath(InstallSourcesDir, Win64);
    IDE.RemoveFromLibraryBrowsingPath(InstallSourcesDir, Win64);
  end;

  InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, nil);
  if IsEmptyDirectory(InstallLibraryDir, InstallSourcesDir) then begin
    UpdateProgressState('Deleting Directory: ' + InstallSourcesDir);
    DxUtils.DeleteDirectory(InstallSourcesDir);
    RemoveDir(InstallLibraryDir);
  end;

  SetIDEOverrideEnvironmentVariable(IDE, DxEnvironmentVariableName, EmptyStr);
end;

procedure TDxInstaller.UninstallPackage(IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform; Component: TDxComponentProfile; const PackageBaseName: String);
var
  BPLPath, DCPPath, PackageName, FileName: String;
begin
  CheckStoppedState;
  if (IDEPlatform = Win64) and (not IsSupportWin64(IDE)) then Exit;

  BPLPath := IncludeTrailingPathDelimiter(IDE.BPLOutputPath[IDEPlatform]);
  DCPPath := IncludeTrailingPathDelimiter(IDE.DCPOutputPath[IDEPlatform]);
  PackageName := Profile.GetPackageName(PackageBaseName, IDE);

  FileName := BPLPath + PackageName + BPLExtName;
  UpdateProgress(IDE, Component, 'Unistall Package', FileName);
  IDE.UnregisterPackage(FileName);

  FileName := ChangeFileExt(FileName, '.*');
  UpdateProgressState('Deleing BPL Files: ' + FileName);
  DeleteFiles(FileName);

  FileName := DCPPath + PackageName + '.*';
  UpdateProgressState('Deleing DCP Files: ' + FileName);
  DeleteFiles(FileName);
end;

procedure TDxInstaller.UpdateProgress(IDE: TDxIDE; Component: TDxComponentProfile; const Task, Target: String);
begin
  if Assigned(FOnUpdateProgress) then FOnUpdateProgress(IDE, Component, Task, Target);
end;

procedure TDxInstaller.UpdateProgressState(const StateText: String);
begin
  if Assigned(FOnUpdateProgressState) then FOnUpdateProgressState(StateText)
end;




end.
