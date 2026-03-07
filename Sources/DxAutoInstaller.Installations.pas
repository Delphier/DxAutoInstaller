unit DxAutoInstaller.Installations;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  DxAutoInstaller.Core,
  DxAutoInstaller.DevExpress,
  DxAutoInstaller.Options;

type
  TInstallation = class
  private
    FManifest: TManifest;
    FRootDir: TRootDir;
    FIDE: TIDE;
    FComponents: TComponents;
    FOptions: TOptions;
    function Valid: Boolean;
    function InstallPackage(APackage: TPackage; const APlatform: TPlatform): Boolean;
    procedure Execute;
  public
    constructor Create(AIDE: TIDE; const ARootDir: TRootDir; AManifest: TManifest);
    destructor Destroy; override;
    property IDE: TIDE read FIDE;
    property Components: TComponents read FComponents;
    property Options: TOptions read FOptions;
  end;

  TInstallations = class(TObjectList<TInstallation>)
  private
    FRootDir: TRootDir;
    FManifest: TManifest;
    function StepCount: Integer;
  public
    constructor Create(const AIDEList: TIDEList; const ARootDir: TRootDir; AManifest: TManifest); overload;
    property Manifest: TManifest read FManifest;
    procedure Execute;
  end;

  TUninstallation = record
  private
    class procedure Execute(AIDE: TIDE; AManifest: TManifest); overload; static;
    class function StepCount(AIDEList: TIDEList; AManifest: TManifest): Integer; static;
  public
    class procedure Execute(AIDEList: TIDEList; AManifest: TManifest); overload; static;
  end;

implementation

uses
  System.Classes, System.IOUtils, DxAutoInstaller.Tasks, DxAutoInstaller.Utils;

{ TInstallation }

constructor TInstallation.Create(AIDE: TIDE; const ARootDir: TRootDir; AManifest: TManifest);
begin
  FManifest := AManifest;
  FRootDir := ARootDir;
  FIDE := AIDE;
  FComponents := TComponents.Create(FManifest, FRootDir, FIDE);
  FOptions := TOptions.Create(FIDE);
end;

destructor TInstallation.Destroy;
begin
  FComponents.Free;
  FOptions.Free;
  inherited;
end;

function TInstallation.Valid: Boolean;
begin
  Result := (Components.CheckedCount > 0) and (Options.Platforms <> []);
end;

function TInstallation.InstallPackage(APackage: TPackage; const APlatform: TPlatform): Boolean;
begin
  Result := True;
  if APackage.Name.IsDesigntime and (APlatform not in Options.DesigntimePlatforms) then Exit;
  if APackage.Dependencies.CheckedCount <> APackage.Dependencies.Count then Exit;

  var OutputDir := FRootDir.OutputDir(IDE, APlatform);
  var CompilerOptions := TStringList.Create;
  try
    CompilerOptions.LineBreak := ' ';
    CompilerOptions.Add(PlatformCompilerOptions[APlatform]);
    if Options.UseNativeLookAndFeel then CompilerOptions.Add('-DUSENATIVELOOKANDFEELASDEFAULT');
    // -U   Unit Search Paths
    // -R   Resource Search Paths
    CompilerOptions.Add(Format('-U"%s" -R"%s"', [FRootDir.SourcesDir, FRootDir.ResourcesDir]));
    // -B   Build All Units
    // -NU  Unit .dcu Output Directory
    CompilerOptions.Add(Format('-B -NU"%s"', [OutputDir]));
    // Build Delphi Packages for C++: https://docwiki.embarcadero.com/RADStudio/en/Build_Delphi_Packages_for_C++
    // Output - C/C++: https://docwiki.embarcadero.com/RADStudio/en/Output_-_C/C++
    // -JL  Generate all C++Builder files
    // -NB  C/C++ .bpi output directory
    // -NH  C/C++ .hpp output directory
    // -NO  C/C++ .obj/.lib output directory
    if Options.CppBuilderPlatforms <> [] then
      if APlatform in Options.CppBuilderPlatforms + Options.DesigntimePlatforms * CppBuilderSupportedDesigntimePlatforms[APlatform] then
        CompilerOptions.Add(Format('-JL -NB"%s" -NH"%s" -NO"%s"', [OutputDir, OutputDir, OutputDir]));

    TTask.WriteLogSeparator;
    Result := IDE.Core.CompilePackage(APackage.FileName, OutputDir, OutputDir, CompilerOptions.Text);
    if Result and APackage.Name.IsDesigntime then begin
      TTask.WriteLogSeparator;
      Result := IDE.Core.RegisterPackage(TPath.Combine(OutputDir, APackage.Name + '.bpl'), APackage.Description);
    end;
  finally
    CompilerOptions.Free;
  end;
end;

procedure TInstallation.Execute;
  procedure InstallPackages(APackages: TPackages; const APlatform: TPlatform);
  begin
    for var Package in APackages do begin
      if TTask.Aborted then Exit;
      if Package.Exists then InstallPackage(Package, APlatform);
      TTask.StepIt;
    end;
  end;

begin
  FRootDir.WriteToIDEEnvironmentVariable(IDE);
  for var Platform in Options.InstallPlatforms do begin
    IDE.SwitchCompiler(Platform);
    var OutputDir := FRootDir.OutputDir(IDE, Platform);
    TDirectory.CreateDirectory(OutputDir);
    TUserEnvironmentVariablePath.Add(OutputDir);

    if Platform in Options.DelphiPlatforms then begin
      IDE.Core.AddToLibrarySearchPath(OutputDir, Platform.ToJclValue);
      IDE.Core.AddToLibrarySearchPath(FRootDir.ResourcesDir, Platform.ToJclValue);
      if Options.AddBrowsingPath then IDE.Core.AddToLibraryBrowsingPath(FRootDir.SourcesDir, Platform.ToJclValue) else IDE.Core.AddToLibrarySearchPath(FRootDir.SourcesDir, Platform.ToJclValue);
    end;

    if Platform in Options.CppBuilderPlatforms then begin
      IDE.Core.AddToCppIncludePath(OutputDir, Platform.ToJclValue);
      IDE.Core.AddToCppLibraryPath(OutputDir, Platform.ToJclValue);
      IDE.Core.AddToCppLibraryPath(FRootDir.ResourcesDir, Platform.ToJclValue);
      if Options.AddBrowsingPath then IDE.Core.AddToCppBrowsingPath(FRootDir.SourcesDir, Platform.ToJclValue) else IDE.Core.AddToCppLibraryPath(FRootDir.SourcesDir, Platform.ToJclValue);
    end;

    for var Comp in Components do if TTask.Aborted then Exit else if Comp.Checked then InstallPackages(Comp.RequiredPackages, Platform);
    for var Comp in Components do if TTask.Aborted then Exit else if Comp.Checked then InstallPackages(Comp.OptionalPackages, Platform);
  end;
end;

{ TInstallations }

constructor TInstallations.Create(const AIDEList: TIDEList; const ARootDir: TRootDir; AManifest: TManifest);
begin
  inherited Create;
  FRootDir := ARootDir;
  FManifest := AManifest;
  for var IDE in AIDEList do Add(TInstallation.Create(IDE, FRootDir, FManifest));
end;

procedure TInstallations.Execute;
const
  MsgNoneSelected   = 'No IDEs selected for installation.'#13#10'Please select the components and platforms to install';
  MsgTaskConfirm = 'Do you want to install DevExpress VCL in the following IDEs:'#13#10;
  MsgTaskTitle = 'Install DevExpress VCL to ';
begin
  var IDENames: TArray<string> := [];
  for var Installation in Self do if Installation.Valid then
    IDENames := IDENames + [Format('%s (%s)', [Installation.IDE.Name, Installation.Options.Platforms.ToString])];

  if Length(IDENames) = 0 then raise Exception.Create(MsgNoneSelected);
  if not TMessageBox.Confirm(MsgTaskConfirm + string.Join(#13#10, IDENames), ['Install', 'Cancel']) then Exit;

  for var Installation in Self do if Installation.Valid then Installation.IDE.CheckRunning;

  TTask.Execute(MsgTaskTitle + string.Join(', ', IDENames), StepCount, procedure begin
    for var Installation in Self do if TTask.Aborted then Exit else if Installation.Valid then TUninstallation.Execute(Installation.IDE, Manifest);
    FRootDir.CreateSourcesDir(Manifest);
    for var Installation in Self do if TTask.Aborted then Exit else if Installation.Valid then Installation.Execute;
  end);
end;

function TInstallations.StepCount: Integer;
begin
  Result := 0;
  var IDEList: TIDEList := [];
  for var Installation in Self do if Installation.Valid then begin
    IDEList := IDEList + [Installation.IDE];
    for var Comp in Installation.Components do if Comp.Checked then
      Result := Result + Length(Comp.Packages) * Installation.Options.InstallPlatforms.Count;
  end;
  Result := Result + TUninstallation.StepCount(IDEList, Manifest);
end;

{ TUninstallation }

class procedure TUninstallation.Execute(AIDE: TIDE; AManifest: TManifest);
begin
  var RootDir := TRootDir.ReadFromIDEEnvironmentVariable(AIDE);

  for var Platform in AIDE.SupportedPlatforms do begin
    AIDE.SwitchCompiler(Platform);
    var OutputDir := RootDir.OutputDir(AIDE, Platform);

    for var I := High(AManifest.Packages) downto 0 do begin
      if TTask.Aborted then Exit;
      var Package := AManifest.Packages[I];
      var Name := TPackageName.Create(Package.BaseName, AIDE);
      var DefaultBplFileName := TPath.Combine(AIDE.Core.BPLOutputPath[Platform.ToJclValue], Name + '.bpl');
      var DefaultDcpFileName := TPath.Combine(AIDE.Core.DCPOutputPath[Platform.ToJclValue], Name + '.dcp');

      if Name.IsDesigntime and (Platform in AIDE.SupportedDesigntimePlatforms) then begin
        TTask.WriteLogSeparator;
        AIDE.Core.UnregisterPackage(DefaultBplFileName);
        if RootDir <> '' then AIDE.Core.UnregisterPackage(ChangeFilePath(DefaultBplFileName, OutputDir));
      end;

      DeleteFile(DefaultBplFileName);
      DeleteFile(DefaultDcpFileName);

      TTask.StepIt;
    end;

    AIDE.Core.RemoveFromLibrarySearchPath(OutputDir, Platform.ToJclValue);
    AIDE.Core.RemoveFromLibrarySearchPath(RootDir.SourcesDir, Platform.ToJclValue);
    AIDE.Core.RemoveFromLibrarySearchPath(RootDir.ResourcesDir, Platform.ToJclValue);
    AIDE.Core.RemoveFromLibraryBrowsingPath(RootDir.SourcesDir, Platform.ToJclValue);
    if AIDE.CppBuilderInstalled and (Platform in CppBuilderSupportedPlatforms) then begin
      AIDE.Core.RemoveFromCppIncludePath(OutputDir, Platform.ToJclValue);
      AIDE.Core.RemoveFromCppLibraryPath(OutputDir, Platform.ToJclValue);
      AIDE.Core.RemoveFromCppLibraryPath(RootDir.SourcesDir, Platform.ToJclValue);
      AIDE.Core.RemoveFromCppLibraryPath(RootDir.ResourcesDir, Platform.ToJclValue);
      AIDE.Core.RemoveFromCppBrowsingPath(RootDir.SourcesDir, Platform.ToJclValue);
    end;
    TUserEnvironmentVariablePath.Remove(OutputDir);
    if TDirectory.Exists(RootDir) then RootDir.DeleteOutputDir(AIDE, Platform);
  end;

  RootDir := '';
  RootDir.WriteToIDEEnvironmentVariable(AIDE);
end;

class procedure TUninstallation.Execute(AIDEList: TIDEList; AManifest: TManifest);
const
  MsgNoneSelected = 'No IDEs selected for uninstallation';
  MsgTaskConfirm  = 'Do you want to uninstall DevExpress VCL from the following IDEs:'#13#10;
  MsgTaskTitle     = 'Uninstall DevExpress VCL from ';
begin
  var IDENames: TArray<string> := [];
  for var IDE in AIDEList do IDENames := IDENames + [IDE.Name];
  if Length(IDENames) = 0 then raise Exception.Create(MsgNoneSelected);
  if not TMessageBox.Confirm(MsgTaskConfirm + string.Join(sLineBreak, IDENames), ['Uninstall', 'Cancel']) then Exit;

  for var IDE in AIDEList do IDE.CheckRunning;

  TTask.Execute(MsgTaskTitle + string.Join(', ', IDENames), StepCount(AIDEList, AManifest), procedure begin
    for var IDE in AIDEList do if TTask.Aborted then Exit else Execute(IDE, AManifest);
  end);
end;

class function TUninstallation.StepCount(AIDEList: TIDEList; AManifest: TManifest): Integer;
begin
  Result := 0;
  for var IDE in AIDEList do Inc(Result, IDE.SupportedPlatforms.Count);
  Result := Result * Length(AManifest.Packages);
end;

end.
