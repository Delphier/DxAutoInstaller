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
  DxAutoInstaller.Core;

type
  TComponentMetadata = record
    Name: string;
    Visible: Boolean;
    RequiredPackages: TArray<string>; // Install in component order.
    OptionalPackages: TArray<string>; // Install after the RequiredPackages for all components have been installed.
    OutdatedPackages: TArray<string>; // Uninstall only.
  end;

  PComponentMetadata = ^TComponentMetadata;
  TComponentMetadataList = TArray<PComponentMetadata>;

  TManifest = class
  const
    CustomFileBaseName  = 'DevExpress.ini';
    ResourceName        = 'DevExpressManifest';
  private
    FIsCustom: Boolean;
    FComponents: TComponentMetadataList;
    procedure ReadComponents;
  public
    class function CustomFileName: string;
    class function CustomFileExists: Boolean;
    constructor Create(const AUseCustomFile: Boolean);
    property IsCustom: Boolean read FIsCustom;
    property Components: TComponentMetadataList read FComponents;
    procedure Export;
  end;

  TPackageName = type string;
  TPackageNameHelper = record helper for TPackageName
    constructor Create(const APackageBaseName: string; AIDE: TIDE);
    function IsDesigntime: Boolean;
  end;

  PComponent = ^TComponent;
  TComponentList = TArray<PComponent>;

  TPackage = record
  private
    FName: TPackageName;
    FFileName: string;
    FExists: Boolean;
    FDescription: string;
    FRequires: TArray<string>;
    FDependencies: TComponentList;
  public
    class procedure ParseFile(const AFileName: string; out ADescription: string; out ARequires: TArray<string>); static;
    constructor Create(const AName: TPackageName; const AFileName: string);
    property Name: TPackageName read FName;
    property FileName: string read FFileName;
    property Exists: Boolean read FExists;
    property Description: string read FDescription;
    property Dependencies: TComponentList read FDependencies;
  end;

  PPackage = ^TPackage;
  TPackageList = TArray<PPackage>;
  TPackageListHelper = record helper for TPackageList
    function ValidCount: NativeInt;
  end;

  TComponentDir = type string;
  TComponentDirHelper = record helper for TComponentDir
    function PackagesDir: string;
    function SourcesDir: string;
  end;

  TRootDir = type string;
  TRootDirHelper = record helper for TRootDir
    function ComponentDir(const AComponentName: string): TComponentDir;
  end;

  TComponent = record
  private
    FName: string;
    FVisible: Boolean;
    FDir: TComponentDir;
    FError: TError;
    FChecked: Boolean;

    FRequiredPackages: TPackageList;
    FOptionalPackages: TPackageList;
    FDependencies: TComponentList;
    FDependents: TComponentList;

    procedure SetChecked(const AChecked: Boolean);
  public
    constructor Create(AMetadata: PComponentMetadata; const ARootDir: TRootDir; AIDE: TIDE);
    property Name: string read FName;
    property Visible: Boolean read FVisible;
    property Dir: TComponentDir read FDir;
    property Error: TError read FError;
    property Checked: Boolean read FChecked write SetChecked;

    property RequiredPackages: TPackageList read FRequiredPackages;
    property OptionalPackages: TPackageList read FOptionalPackages;
    property Dependencies: TComponentList read FDependencies;
    property Dependents: TComponentList read FDependents;

    function Packages: TPackageList;
    function Valid: Boolean;
    procedure CheckError;
  end;

  TComponentListHelper = record helper for TComponentList
  strict private
    procedure InitDependencies;
  public
    constructor Create(AManifest: TManifest; const ARootDir: TRootDir; AIDE: TIDE);
    function ValidCount: NativeInt;
    function VisibleValidCount: NativeInt;
    function CheckedCount: NativeInt;
    function VisibleCheckedCount: NativeInt;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.IOUtils,
  System.IniFiles, System.RegularExpressions, System.Generics.Collections,
  Vcl.Forms, DxAutoInstaller.Utils;

{ TManifest }

class function TManifest.CustomFileName: string;
begin
  Result := TPath.Combine(ExtractFilePath(Application.ExeName), CustomFileBaseName);
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
const
  Separators: TArray<Char> = [',', ' '];
var
  IniFile: TMemIniFile;
begin
  if IsCustom then IniFile := TMemIniFile.Create(CustomFileName) else begin
    var Stream := CreateResourceStream(ResourceName);
    try
      IniFile := TMemIniFile.Create(Stream);
    finally
      Stream.Free;
    end;
  end;

  try
    var Names := TStringList.Create;
    try
      IniFile.ReadSections(Names);
      for var Name in Names do begin
        var Component := Default(TComponentMetadata);
        Component.Name := Name;
        Component.Visible := IniFile.ReadBool(Name, 'Visible', True);
        Component.RequiredPackages := IniFile.ReadString(Name, 'RequiredPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        Component.OptionalPackages := IniFile.ReadString(Name, 'OptionalPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        Component.OutdatedPackages := IniFile.ReadString(Name, 'OutdatedPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        FComponents := FComponents + [@Component];
      end;
    finally
      Names.Free;
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TManifest.Export;
begin
  ExportResourceToFile(CustomFileName, ResourceName);
end;

{ TPackage }

constructor TPackage.Create(const AName: TPackageName; const AFileName: string);
begin
  Self := Default(TPackage);
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

function TPackageListHelper.ValidCount: NativeInt;
begin
  Result := 0;
  for var Package in Self do if Package.Exists then Inc(Result);
end;

{ TComponent }

constructor TComponent.Create(AMetadata: PComponentMetadata; const ARootDir: TRootDir; AIDE: TIDE);
  function BuildPackageList(ABaseNames: TArray<string>): TPackageList;
  begin
    Result := [];
    for var BaseName in ABaseNames do begin
      var PackageName := TPackageName.Create(BaseName, AIDE);
      var FileName := TPath.Combine(FDir.PackagesDir, PackageName + '.dpk');
      var Package := TPackage.Create(PackageName, FileName);
      Result := Result + [@Package];
    end;
  end;

begin
  Self := Default(TComponent);
  FName := AMetadata.Name;
  FVisible := AMetadata.Visible;
  FDir := ARootDir.ComponentDir(FName);
  FRequiredPackages := BuildPackageList(AMetadata.RequiredPackages);
  FOptionalPackages := BuildPackageList(AMetadata.OptionalPackages);
end;

function TComponent.Packages: TPackageList;
begin
  Result := RequiredPackages + OptionalPackages;
end;

function TComponent.Valid: Boolean;
begin
  Result := Error = errNone;
end;

procedure TComponent.CheckError;
begin
  if not TDirectory.Exists(Dir) then FError := errComponentNotFound
  else if RequiredPackages.ValidCount < 1 then FError := errComponentMissingPackages
  else if Dependencies.ValidCount < 1 then FError := errComponentMissingDependencies
  else FError := errNone;
end;

procedure TComponent.SetChecked(const AChecked: Boolean);
begin
  if not Valid then Exit;
  if AChecked then for var Component in Dependencies do Component.Checked := True
              else for var Component in Dependents do Component.Checked := False;
  FChecked := AChecked;
end;

{ TComponentListHelper }

constructor TComponentListHelper.Create(AManifest: TManifest; const ARootDir: TRootDir; AIDE: TIDE);
begin
  Self := [];
  for var Metadata in AManifest.Components do begin
    var Component := TComponent.Create(Metadata, ARootDir, AIDE);
    Self := Self + [@Component];
  end;

  InitDependencies;
  for var Comp in Self do Comp.CheckError;
end;

procedure TComponentListHelper.InitDependencies;
type
  TPkgCompDict = TDictionary<string, PComponent>;
var
  Dict: TPkgCompDict;

  procedure SetDependencies(AComponent: PComponent; APackages: TPackageList; const AIsRequiredPackage: Boolean);
  var
    Component: PComponent;
  begin
    for var Package in APackages do
      for var Requires in Package.FRequires do
        if Dict.TryGetValue(Requires.ToUpper, Component) then
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

function TComponentListHelper.ValidCount: NativeInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Valid then Inc(Result);
end;

function TComponentListHelper.VisibleValidCount: NativeInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Visible and Comp.Valid then Inc(Result);
end;

function TComponentListHelper.CheckedCount: NativeInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Checked then Inc(Result);
end;

function TComponentListHelper.VisibleCheckedCount: NativeInt;
begin
  Result := 0;
  for var Comp in Self do if Comp.Visible and Comp.Checked then Inc(Result);
end;

{ TPackageNameHelper }

constructor TPackageNameHelper.Create(const APackageBaseName: string; AIDE: TIDE);
begin
  Self := APackageBaseName + AIDE.PackageVersionCode;
end;

function TPackageNameHelper.IsDesigntime: Boolean;
begin
  Result := string.StartsText('dcl', Self);
end;

{ TComponentDirHelper }

function TComponentDirHelper.PackagesDir: string;
begin
  Result := TPath.Combine(Self, 'Packages');
end;

function TComponentDirHelper.SourcesDir: string;
begin
  Result := TPath.Combine(Self, 'Sources');
end;

{ TRootDirHelper }

function TRootDirHelper.ComponentDir(const AComponentName: string): TComponentDir;
begin
  Result := TPath.Combine(Self, AComponentName);
end;

end.
