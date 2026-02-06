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
    IsBase: Boolean;
    RequiredPackages: TArray<string>;
    OptionalPackages: TArray<string>;
    OutdatedPackages: TArray<string>; // Uninstall only.
  end;

  TManifest = class
  const
    CustomFileBaseName  = 'DevExpress.ini';
    ResourceName        = 'DevExpressManifest';
  private
    FIsCustom: Boolean;
    FComponents: TArray<TComponentMetadata>;
    procedure ReadComponents;
  public
    class function CustomFileName: string;
    class function CustomFileExists: Boolean;
    constructor Create(const AUseCustomFile: Boolean);
    property IsCustom: Boolean read FIsCustom;
    property Components: TArray<TComponentMetadata> read FComponents;
    procedure Export;
  end;

  TPackageName = type string;
  TPackageNameHelper = record helper for TPackageName
    constructor Create(const APackageBaseName: string; AIDE: TIDE);
    function IsDesigntime: Boolean;
  end;

  TPackage = record
    Name: TPackageName;
    FileName: string;
    Description: string;
    Requires: TArray<string>;
    class procedure ParseFile(const AFileName: string; out ADescription: string; out ARequires: TArray<string>); static;
    constructor Create(const AName: TPackageName; const AFileName: string);
    function Exists: Boolean;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.IOUtils, System.IniFiles, System.RegularExpressions,
  Vcl.Forms, DxAutoInstaller.Utils;

{ TManifest }

class function TManifest.CustomFileName: string;
begin
  Result := TPath.Combine(ExtractFilePath(Application.ExeName), CustomFileBaseName);
end;

class function TManifest.CustomFileExists: Boolean;
begin
  Result := FileExists(CustomFileName);
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
  if FIsCustom then IniFile := TMemIniFile.Create(CustomFileName) else begin
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
        Component.IsBase := IniFile.ReadBool(Name, 'IsBase', False);
        Component.RequiredPackages := IniFile.ReadString(Name, 'RequiredPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        Component.OptionalPackages := IniFile.ReadString(Name, 'OptionalPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        Component.OutdatedPackages := IniFile.ReadString(Name, 'OutdatedPackages', '').Split(Separators, TStringSplitOptions.ExcludeEmpty);
        FComponents := FComponents + [Component];
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
  Name := AName;
  FileName := AFileName;
  ParseFile(AFileName, Description, Requires);
end;

function TPackage.Exists: Boolean;
begin
  Result := TFile.Exists(FileName);
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

{ TPackageNameHelper }

constructor TPackageNameHelper.Create(const APackageBaseName: string; AIDE: TIDE);
begin
  Self := APackageBaseName + AIDE.PackageVersionStr;
end;

function TPackageNameHelper.IsDesigntime: Boolean;
begin
  Result := string.StartsText('dcl', Self);
end;

end.
