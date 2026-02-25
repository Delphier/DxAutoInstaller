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

unit DxAutoInstaller.DevExpress;

interface

uses
  System.Generics.Collections,
  DxAutoInstaller.Core;

type
  TPackageMetadata = record
    BaseName: string;
    Dir: string;
  end;

  PComponentMetadata = ^TComponentMetadata;
  TComponentMetadata = record
    Name: string;
    Visible: Boolean;
    RequiredPackages: TArray<TPackageMetadata>; // Install in component order.
    OptionalPackages: TArray<TPackageMetadata>; // Install after the RequiredPackages for all components have been installed.
    OutdatedPackages: TArray<TPackageMetadata>; // Uninstall only.
    Sources: TArray<string>; // Sources dirs.
    Help: TArray<string>;    // Help dirs.
  end;

  TManifest = class
  const
    CustomFileBaseName  = 'DevExpress.yaml';
    ResourceName        = 'DevExpressManifest';
  private class var
    FInstance: TManifest;
  private
    FIsCustom: Boolean;
    FComponents: TArray<TComponentMetadata>;
    procedure ReadComponents;
  public
    class destructor Destroy;
    class property Instance: TManifest read FInstance;
    class procedure CreateInstance;
    class procedure Export;
    class function CustomFileName: string;
    class function CustomFileExists: Boolean;
    constructor Create(const AUseCustomFile: Boolean);
    property IsCustom: Boolean read FIsCustom;
    property Components: TArray<TComponentMetadata> read FComponents;
  end;

  TPackageName = type string;
  TPackageNameHelper = record helper for TPackageName
    constructor Create(const APackageBaseName: string; AIDE: TIDE);
    function IsDesigntime: Boolean;
  end;

  TComponent = class;

  TPackage = class
  private
    FName: TPackageName;
    FFileName: string;
    FExists: Boolean;
    FDescription: string;
    FRequires: TArray<string>;
    FDependencies: TArray<TComponent>;
  public
    class procedure ParseFile(const AFileName: string; out ADescription: string; out ARequires: TArray<string>); static;
    constructor Create(const AName: TPackageName; const AFileName: string);
    property Name: TPackageName read FName;
    property FileName: string read FFileName;
    property Exists: Boolean read FExists;
    property Description: string read FDescription;
    property Dependencies: TArray<TComponent> read FDependencies;
  end;

  TPackages = TObjectList<TPackage>;

  TPackageListHelper = record helper for TArray<TPackage>
    function ValidCount: NativeUInt;
  end;

  TVersion = type Cardinal;
  TVersionHelper = record helper for TVersion
    function ToText: string;
  end;

  TRootDir = type string;
  TRootDirHelper = record helper for TRootDir
    function Version: TVersion;
  end;

  TComponent = class
  private
    FMetadata: PComponentMetadata;
    FDir: string;
    FError: TError;
    FChecked: Boolean;

    FRequiredPackages: TPackages;
    FOptionalPackages: TPackages;
    FPackages: TArray<TPackage>;

    FDependencies: TArray<TComponent>;
    FDependents: TArray<TComponent>;

    procedure SetChecked(const AChecked: Boolean);
  public
    constructor Create(const AMetadata: PComponentMetadata; const ARootDir: TRootDir; AIDE: TIDE);
    destructor Destroy; override;
    property Metadata: PComponentMetadata read FMetadata;
    property Dir: string read FDir;
    property Error: TError read FError;
    property Checked: Boolean read FChecked write SetChecked;

    property RequiredPackages: TPackages read FRequiredPackages;
    property OptionalPackages: TPackages read FOptionalPackages;
    property Packages: TArray<TPackage> read FPackages;

    property Dependencies: TArray<TComponent> read FDependencies;
    property Dependents: TArray<TComponent> read FDependents;

    function Valid: Boolean;
    procedure CheckError;
  end;

  TComponents = class(TObjectList<TComponent>)
  strict private
    procedure InitDependencies;
  public
    constructor Create(AManifest: TManifest; const ARootDir: TRootDir; AIDE: TIDE);
    procedure CheckAll(const AChecked: Boolean);
    function ValidCount: NativeUInt;
    function VisibleValidCount: NativeUInt;
    function CheckedCount: NativeUInt;
    function VisibleCheckedCount: NativeUInt;
  end;

  TComponentListHelper = record helper for TArray<TComponent>
    function Count: NativeUInt;
    function ValidCount: NativeUInt;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  System.RegularExpressions,
  DxAutoInstaller.Utils,
  VSoft.YAML;

{ TManifest }

class destructor TManifest.Destroy;
begin
  FInstance.Free;
end;

class procedure TManifest.CreateInstance;
const
  Message = 'Found [' + CustomFileBaseName + ']'#13#10 +
            'Do you want to use this custom manifest file?';
begin
  if Assigned(FInstance) then Exit;
  FInstance := Self.Create(CustomFileExists and TMessageBox.Confirm(Message));
end;

class procedure TManifest.Export;
begin
  ExportResourceToFile(CustomFileName, ResourceName);
end;

class function TManifest.CustomFileName: string;
begin
  Result := TPath.Combine(TApp.Dir, CustomFileBaseName);
end;

class function TManifest.CustomFileExists: Boolean;
begin
  Result := TFile.Exists(CustomFileName);
end;

constructor TManifest.Create(const AUseCustomFile: Boolean);
begin
  FIsCustom := AUseCustomFile and CustomFileExists;
  ReadComponents;
end;

procedure TManifest.ReadComponents;
var
  Doc: IYAMLDocument;
  Value: IYAMLValue;

  function ParsePackages(const ACompName: string; ACompValue: IYAMLValue; const AKey: string): TArray<TPackageMetadata>;
  begin
    Result := [];
    var Packages := ACompValue.Values[AKey];
    if Packages.IsNull then Exit;

    for var I := 0 to Packages.Count - 1 do begin
      var Path := Packages.AsSequence[I].AsString;
      var Metadata := Default(TPackageMetadata);
      Metadata.BaseName := TPath.GetFileName(Path);
      Metadata.Dir := TPath.GetDirectoryName(Path);
      if Metadata.Dir.IsEmpty then Metadata.Dir := TPath.Combine(ACompName, 'Packages');
      Result := Result + [Metadata];
    end;
  end;

  function ParseStrings(ACompValue: IYAMLValue; const AKey: string): TArray<string>;
  begin
    Result := [];
    var Strings := ACompValue.Values[AKey];
    if Strings.IsNull then Exit;
    for var I := 0 to Strings.Count - 1 do Result := Result + [Strings.AsSequence[I].AsString];
  end;

begin
  if IsCustom then Doc := TYAML.LoadFromFile(CustomFileName) else begin
    var Stream := CreateResourceStream(ResourceName);
    try
      Doc := TYAML.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;

  var Root := Doc.Root.AsMapping;
  for var I := 0 to Root.Count - 1 do begin
    var CompName := Root.Keys[I];
    var CompValue := Root[CompName];
    var Metadata := Default(TComponentMetadata);
    Metadata.Name := CompName;
    Metadata.Visible := if CompValue.TryGetValue('Visible', Value) then Value.AsBoolean else True;
    Metadata.RequiredPackages := ParsePackages(CompName, CompValue, 'RequiredPackages');
    Metadata.OptionalPackages := ParsePackages(CompName, CompValue, 'OptionalPackages');
    Metadata.OutdatedPackages := ParsePackages(CompName, CompValue, 'OutdatedPackages');
    Metadata.Sources := [TPath.Combine(CompName, 'Sources')] + ParseStrings(CompValue, 'Sources');
    Metadata.Help := [TPath.Combine(CompName, 'Help')] + ParseStrings(CompValue, 'Help');
    FComponents := FComponents + [Metadata];
  end;
end;

{ TPackage }

constructor TPackage.Create(const AName: TPackageName; const AFileName: string);
begin
  FName := AName;
  FFileName := AFileName;
  FExists := TFile.Exists(FFileName);
  if FExists then ParseFile(FFileName, FDescription, FRequires);
end;

class procedure TPackage.ParseFile(const AFileName: string; out ADescription: string; out ARequires: TArray<string>);
begin
  ADescription := '';
  ARequires := [];

  var Requires := '';
  var InRequires := False;
  for var Line in TFile.GetLinesEnumerator(AFileName) do begin
    if InRequires then begin
      var I := Line.IndexOf(';');
      if I >= 0 then begin
        Requires := Requires + Line.Remove(I);
        ARequires := Requires.Split([',', ' '], TStringSplitOptions.ExcludeEmpty);
        Break;
      end else
        Requires := Requires + Line;
    end else if SameText(Line.Trim, 'requires') then
      InRequires := True
    else begin
      var Match := TRegEx.Match(Line, '{\$DESCRIPTION ''(.*)''}'); // {$DESCRIPTION 'ExpressCharts by Developer Express Inc.'}
      if Match.Success then ADescription := Match.Groups[1].Value;
    end;
  end;
end;

{ TPackageListHelper }

function TPackageListHelper.ValidCount: NativeUInt;
begin
  Result := 0;
  for var Package in Self do if Package.Exists then Inc(Result);
end;

{ TComponent }

constructor TComponent.Create(const AMetadata: PComponentMetadata; const ARootDir: TRootDir; AIDE: TIDE);
  function CreatePackages(const AMetadataList: TArray<TPackageMetadata>): TPackages;
  begin
    Result := TPackages.Create;
    for var Metadata in AMetadataList do begin
      var PackageName := TPackageName.Create(Metadata.BaseName, AIDE);
      var FileName := TPath.Combine(ARootDir, Metadata.Dir, PackageName + '.dpk');
      var Package := TPackage.Create(PackageName, FileName);
      Result.Add(Package);
    end;
  end;

begin
  FMetadata := AMetadata;
  FDir := TPath.Combine(ARootDir, FMetadata.Name);
  FRequiredPackages := CreatePackages(FMetadata.RequiredPackages);
  FOptionalPackages := CreatePackages(FMetadata.OptionalPackages);
  FPackages := FRequiredPackages.ToArray + FOptionalPackages.ToArray;
end;

destructor TComponent.Destroy;
begin
  FRequiredPackages.Free;
  FOptionalPackages.Free;
  inherited;
end;

function TComponent.Valid: Boolean;
begin
  Result := Error = errNone;
end;

procedure TComponent.CheckError;
begin
  if not TDirectory.Exists(Dir) then FError := errComponentNotFound
  else if Packages.ValidCount = 0 then FError := errComponentPackagesMissing
  else if Dependencies.ValidCount <> Dependencies.Count then FError := errComponentDependenciesMissing
  else FError := errNone;
end;

procedure TComponent.SetChecked(const AChecked: Boolean);
begin
  if not Valid then Exit;
  if FChecked = AChecked then Exit;

  if AChecked then for var Comp in Dependencies do Comp.Checked := True
              else for var Comp in Dependents do Comp.Checked := False;
  FChecked := AChecked;
end;

{ TComponents }

constructor TComponents.Create(AManifest: TManifest; const ARootDir: TRootDir; AIDE: TIDE);
begin
  inherited Create;
  for var I := 0 to High(AManifest.Components) do begin
    var Component := TComponent.Create(@AManifest.Components[I], ARootDir, AIDE);
    Add(Component);
  end;

  InitDependencies;
  for var Comp in Self do Comp.CheckError;
end;

procedure TComponents.InitDependencies;
type
  TPkgCompDict = TDictionary<string, TComponent>;
var
  Dict: TPkgCompDict;

  procedure SetDependencies(AComponent: TComponent; APackages: TPackages; const AIsRequiredPackage: Boolean);
  var
    Component: TComponent;
  begin
    for var Package in APackages do
      for var PkgName in Package.FRequires do
        if Dict.TryGetValue(PkgName.ToUpper, Component) then
          if Component <> AComponent then
            if not TArray.Contains(Package.Dependencies, Component) then begin
              Package.FDependencies := Package.FDependencies + [Component];
              if AIsRequiredPackage then
                if not TArray.Contains(AComponent.Dependencies, Component) then begin
                  AComponent.FDependencies := AComponent.FDependencies + [Component];
                  Component.FDependents := Component.FDependents + [AComponent];
                end;
            end;
  end;

begin
  Dict := TPkgCompDict.Create;
  try
    for var Comp in Self do
      for var Package in Comp.Packages do
        Dict.Add(UpperCase(Package.Name), Comp);

    for var Comp in Self do begin
      SetDependencies(Comp, Comp.RequiredPackages, True);
      SetDependencies(Comp, Comp.OptionalPackages, False);
    end;

  finally
    Dict.Free;
  end;
end;

procedure TComponents.CheckAll(const AChecked: Boolean);
begin
  for var Comp in Self do Comp.Checked := AChecked;
end;

function TComponents.ValidCount: NativeUInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Valid then Inc(Result);
end;

function TComponents.VisibleValidCount: NativeUInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Metadata.Visible and Comp.Valid then Inc(Result);
end;

function TComponents.CheckedCount: NativeUInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Checked then Inc(Result);
end;

function TComponents.VisibleCheckedCount: NativeUInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Metadata.Visible and Comp.Checked then Inc(Result);
end;

{ TComponentListHelper }

function TComponentListHelper.Count: NativeUInt;
begin
  Result := Length(Self);
end;

function TComponentListHelper.ValidCount: NativeUInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Valid then Inc(Result);
end;

{ TPackageNameHelper }

constructor TPackageNameHelper.Create(const APackageBaseName: string; AIDE: TIDE);
begin
  Self := APackageBaseName + AIDE.PackageVersionStr;
end;

function TPackageNameHelper.IsDesigntime: Boolean;
begin
  Result := string.StartsText('dcl', Self);
end;

{ TVersionHelper }

function TVersionHelper.ToText: string;
begin
  Result := if Self > 0 then Format('%d.%d.%d', [Self div 10000 mod 100, Self div 100 mod 100, Self mod 100]) else 'n/a';
end;

{ TRootDirHelper }

function TRootDirHelper.Version: TVersion;
begin
  Result := 0;
  var FileName := TPath.Combine(Self, 'ExpressCore Library\Sources\dxCore.pas');
  if not TFile.Exists(FileName) then Exit;

  for var Line in TFile.GetLinesEnumerator(FileName) do begin
    var Match := TRegEx.Match(Line, 'dxVersion = (\d{8});');
    if Match.Success then Exit(Match.Groups[1].Value.ToInteger);
  end;
end;

end.
