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
  TDxInstallOption = (dxioAddBrowsingPath, dxioNativeLookAndFeel, dxioCompileWin64Library, dxioInstallToCppBuilder);
  TDxInstallOptions = set of TDxInstallOption;

  TDxThirdPartyComponent = (dxtpcIBX, dxtpcTeeChart, dxtpcFireDAC, dxtpcBDE);
  TDxThirdPartyComponents = set of TDxThirdPartyComponent;

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
    FThirdPartyComponents: array of TDxThirdPartyComponents;
    FState: TDxInstallerState;
    FOnUpdateProgress: TDxUpdateProgressEvent;
    FOnUpdateProgressState: TDxUpdateProgressStateEvent;
    procedure SetInstallFileDir(const Value: String);
    function GetComponents(IDE: TDxIDE): TDxComponentList;
    function GetOptions(IDE: TDxIDE): TDxInstallOptions;
    procedure SetOptions(IDE: TDxIDE; const Value: TDxInstallOptions);
    function GetThirdPartyComponents(IDE: TDxIDE): TDxThirdPartyComponents;
    procedure SetThirdPartyComponents(IDE: TDxIDE; const Value: TDxThirdPartyComponents);
    procedure DetectionThirdPartyComponents(IDE: TDxIDE);
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
    property ThirdPartyComponents[IDE: TDxIDE]: TDxThirdPartyComponents read GetThirdPartyComponents write SetThirdPartyComponents;
    property State: TDxInstallerState read FState;
    property OnUpdateProgress: TDxUpdateProgressEvent read FOnUpdateProgress write FOnUpdateProgress;
    property OnUpdateProgressState: TDxUpdateProgressStateEvent read FOnUpdateProgressState write FOnUpdateProgressState;
    procedure Install(const IDEArray: TDxIDEArray); overload;
    procedure Uninstall(const IDEArray: TDxIDEArray); overload;
    procedure Stop();
    procedure SearchNewPackages(List: TStringList);
    class function GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform = Win32): String;
    class function GetInstallSourcesDir(const InstallFileDir: String): String;
  end;

const
  DxInstallOptionNames: array[TDxInstallOption] of String = ('Add Browsing Path', 'Use Native Look and Feel as Default', 'Compile Win64 Library', 'Install to C++Builder');

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
  SetLength(FThirdPartyComponents, FIDEs.Count);
  for I := 0 to FIDEs.Count - 1 do DetectionThirdPartyComponents(FIDEs[I]);
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

procedure TDxInstaller.DetectionThirdPartyComponents(IDE: TDxIDE);
var
  I: Integer;
  FileName: String;
  Components: TDxThirdPartyComponents;
begin
  Components := [];
  for I := 0 to IDE.IdePackages.Count[false] - 1 do
  begin
    FileName := IDE.IdePackages.PackageFileNames[I, false];
    if (not(dxtpcIBX in Components)) and (Pos('\dclib', FileName) > 0) then Include(Components, dxtpcIBX)
    else if (not(dxtpcTeeChart in Components)) and (Pos('\dcltee', FileName) > 0) then Include(Components, dxtpcTeeChart)
    else if (not(dxtpcFireDAC in Components)) and ((Pos('\dclFireDAC', FileName) > 0) or (Pos('\AnyDAC_', FileName) > 0)) then Include(Components, dxtpcFireDAC)
    else if (not(dxtpcBDE in Components)) and (Pos('\dclbde', FileName) > 0) then Include(Components, dxtpcBDE);
  end;
  ThirdPartyComponents[IDE] := Components;
end;

class function TDxInstaller.GetInstallLibraryDir(const InstallFileDir: String; IDE: TDxIDE; const IDEPlatform: TDxIDEPlatform): String;
begin
  Result := IncludeTrailingPathDelimiter(InstallFileDir) + 'Library';
  if IDE <> nil then begin
    Result := Result + '\' + TDxProfile.GetIDEVersionNumberStr(IDE);
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
  if (dxioInstallToCppBuilder in Options) and (not IsSupportCppBuilder(IDE)) then Exclude(Options, dxioInstallToCppBuilder);
  FOptions[IDEs.IndexOf(IDE)] := Options;
end;


function TDxInstaller.GetThirdPartyComponents(IDE: TDxIDE): TDxThirdPartyComponents;
begin
  Result := FThirdPartyComponents[IDEs.IndexOf(IDE)];
end;

procedure TDxInstaller.SetThirdPartyComponents(IDE: TDxIDE; const Value: TDxThirdPartyComponents);
begin
  FThirdPartyComponents[IDEs.IndexOf(IDE)] := Value;
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
  for I := 0 to IDEs.Count - 1 do Options[IDEs[I]] := [dxioAddBrowsingPath, dxioNativeLookAndFeel];
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
  dxBuildNumber: Cardinal;
  I: Integer;

  procedure AddLibrarySearchPath(const Dir: String; const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.AddToLibrarySearchPath(Dir, IDEPlatform);
    if (dxioInstallToCppBuilder in Options[IDE]) and IsRADStudio(IDE) then TDxBDSIDE(IDE).AddToCppLibraryPath(Dir, IDEPlatform);
  end;
  procedure AddLibraryBrowsingPath(const Dir: String; const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.AddToLibraryBrowsingPath(Dir, IDEPlatform);
    if (dxioInstallToCppBuilder in Options[IDE]) and IsRADStudio(IDE) then TDxBDSIDE(IDE).AddToCppBrowsingPath(Dir, IDEPlatform);
  end;
  procedure CopyFiles(const ASourcesFileDir: String);
  begin
    CopyFilesToDirectory(ASourcesFileDir + '*.*', InstallSourcesDir);
    CopyFilesToDirectory(ASourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win32));
    if dxioCompileWin64Library in Options[IDE] then
      CopyFilesToDirectory(ASourcesFileDir + '*.dfm;*.res', GetInstallLibraryDir(InstallFileDir, IDE, Win64));
  end;
begin
  Uninstall(IDE);
  dxBuildNumber := Profile.GetDxBuildNumber(InstallFileDir);
  InstallSourcesDir := GetInstallSourcesDir(InstallFileDir);
  for Comp in Components[IDE] do begin
    if Comp.State <> dxcsInstall then Continue;
    SourcesFileDir := IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, Comp.Profile.ComponentName));
    UpdateProgress(IDE, Comp.Profile, 'Copying', 'Sources Files');
    UpdateProgressState('Copying Files: ' + SourcesFileDir + '*.*');
    CopyFiles(SourcesFileDir);
    // Fix for version >= 18.2.x
    if (dxBuildNumber >= 20180200) and (Comp.Profile.ComponentName = 'ExpressLibrary') then begin
      for I := 0 to Profile.Components.Count - 1 do
        if DirectoryExists(Profile.GetComponentSourcesDir(InstallFileDir, Profile.Components[I].ComponentName)) and
          not DirectoryExists(Profile.GetComponentPackagesDir(InstallFileDir, Profile.Components[I].ComponentName)) then
            CopyFiles(IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, Profile.Components[I].ComponentName)));
      CopyFiles(IncludeTrailingPathDelimiter(Profile.GetComponentSourcesDir(InstallFileDir, 'ExpressPageControl')));
    end;
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

  AddLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win32), Win32);
  if dxioCompileWin64Library in Options[IDE] then AddLibrarySearchPath(GetInstallLibraryDir(InstallFileDir, IDE, Win64), Win64);
  if dxioAddBrowsingPath in Options[IDE] then begin
    AddLibraryBrowsingPath(InstallSourcesDir, Win32);
    if dxioCompileWin64Library in Options[IDE] then AddLibraryBrowsingPath(InstallSourcesDir, Win64);
  end else begin
    AddLibrarySearchPath(installSourcesDir, Win32);
    if dxioCompileWin64Library In Options[IDE] then AddLibrarySearchPath(InstallSourcesDir, Win64)
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
  case Package.Category of
    dxpcIBX:      if not (dxtpcIBX in ThirdPartyComponents[IDE]) then Exit;
    dxpcTeeChart: if not (dxtpcTeeChart in ThirdPartyComponents[IDE]) then Exit;
    dxpcFireDAC:  if not (dxtpcFireDAC in ThirdPartyComponents[IDE]) then Exit;
    dxpcBDE:      if not (dxtpcBDE in ThirdPartyComponents[IDE]) or (IDEPlatform = Win64) then Exit;
  end;
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
  // -A   Unit Alias;
  // -NS  Namespaces Search Paths;

  // -D   Define Donditionals;

  // -JL  Generate package .lib, .bpi, and all .hpp files for C++;
  // -NB  Unit .bpi Output Directory - DCP Path;
  // -NH  Unit .hpp Output Directory;
  // -NO  Unit .obj Output Directory - DCP Path;
  ExtraOptions :=
    ' -$D- -$L- -$Y- -Q ' +
    Format(' -U"%s" -U"%s" -R"%s" ', [DCPPath, InstallSourcesDir, InstallSourcesDir]) +
    Format(' -B -NU"%s" -N0"%s" ', [InstallLibraryDir, InstallLibraryDir]) +
    '-AWinTypes=Windows;WinProcs=Windows;DbiTypes=BDE;DbiProcs=BDE ' +
    '-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;IBX;VclTee; ';

  if dxioNativeLookAndFeel in Options[IDE] then ExtraOptions := ExtraOptions + ' -DUSENATIVELOOKANDFEELASDEFAULT ';
  if dxioInstallToCppBuilder in Options[IDE] then
    ExtraOptions := ExtraOptions + Format(' -JL -NB"%s" -NH"%s" -NO"%s" ', [DCPPath, InstallLibraryDir, DCPPath]);

  R := IDE.CompileDelphiPackageEx(Package.FullFileName, BPLPath, DCPPath, ExtraOptions);
  if R then begin
    // Fix issue that the skin names not listed since v18.2.x. dxSkinXxxxx.bpl should be placed in the library install directory.
    if Package.Name.StartsWith('dxSkin') and CharInSet(Package.Name.Chars[6], ['A'..'Z']) then
      CopyFile(IncludeTrailingPathDelimiter(BPLPath) + Package.Name + BPLExtName, IncludeTrailingPathDelimiter(InstallLibraryDir) + Package.Name + BPLExtName, True);

    if (IDEPlatform = Win32) and (Package.Usage <> dxpuRuntimeOnly) then
      R := IDE.RegisterPackage(Package.FullFileName, BPLPath, Package.Description);
  end;
  if not R then SetState(dxisError);
end;

procedure TDxInstaller.Uninstall(IDE: TDxIDE);
var
  Comp: TDxComponentProfile;
  InstallFileDir, InstallLibraryDir, InstallSourcesDir: String;

  procedure RemoveLibraryPath(const IDEPlatform: TDxIDEPlatform);
  begin
    IDE.RemoveFromLibrarySearchPath(InstallLibraryDir, IDEPlatform);
    IDE.RemoveFromLibrarySearchPath(InstallSourcesDir, IDEPlatform);
    IDE.RemoveFromLibraryBrowsingPath(InstallSourcesDir, IDEPlatform);
    if IsRADStudio(IDE) and IsSupportCppBuilder(IDE) then begin
      TDxBDSIDE(IDE).RemoveFromCppLibraryPath(InstallLibraryDir, IDEPlatform);
      TDxBDSIDE(IDE).RemoveFromCppLibraryPath(InstallSourcesDir, IDEPlatform);
      TDxBDSIDE(IDE).RemoveFromCppBrowsingPath(InstallSourcesDir, IDEPlatform);
    end;
  end;

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

  RemoveLibraryPath(Win32);
  if IsSupportWin64(IDE) then begin
    InstallLibraryDir := GetInstallLibraryDir(InstallFileDir, IDE, Win64);
    RemoveLibraryPath(Win64);
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
  UpdateProgress(IDE, Component, 'Uninstall Package', FileName);
  IDE.UnregisterPackage(FileName);

  FileName := ChangeFileExt(FileName, '.*');
  UpdateProgressState('Deleting BPL Files: ' + FileName);
  DeleteFiles(FileName);

  FileName := DCPPath + PackageName + '.*';
  UpdateProgressState('Deleting DCP Files: ' + FileName);
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
