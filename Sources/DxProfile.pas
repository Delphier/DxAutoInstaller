{*******************************************************}
{                                                       }
{          DxAutoInstaller Profile Classes              }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxProfile;

interface

uses
  SysUtils, Classes, Generics.Collections, DxIDE;

type
  TDxComponentProfile = class
  private
    FComponentName: String;
    FRequiredPackages: TStringList;
    FOptionalPackages: TStringList;
    FOutdatedPackages: TStringList;
    FIsBase: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property ComponentName: String read FComponentName write FComponentName;
    property RequiredPackages: TStringList read FRequiredPackages;
    property OptionalPackages: TStringList read FOptionalPackages;
    property OutdatedPackages: TStringList read FOutdatedPackages;
    property IsBase: Boolean read FIsBase write FIsBase;
  end;

  TDxComponentProfileList = TObjectList<TDxComponentProfile>;

  TDxProfile = class
  const
    RequiredPackagesIdent = 'RequiredPackages';
    OptionalPackagesIdent = 'OptionalPackages';
    OutdatedPackagesIdent = 'OutdatedPackages';
    IsBaseIdent = 'IsBase';

    ProfileResourceName = 'Profile';
  private
    FFileName: String;
    FComponents: TDxComponentProfileList;
    procedure LoadComponents();
  public
    constructor Create();
    destructor Destroy; override;
    property FileName: String read FFileName;
    property Components: TDxComponentProfileList read FComponents;
    function IsCustomProfile(): Boolean;
    procedure ExportBuiltInProfile(const FileName: String);
    class function GetCustomProfileFileName(): String;
    class function GetIDEVersionNumberStr(IDE: TDxIDE): String;
    class function GetComponentDir(const InstallFileDir, ComponentName: String): String;
    class function GetComponentSourcesDir(const InstallFileDir, ComponentName: String): String;
    class function GetComponentPackagesDir(const InstallFileDir, ComponentName: String): String;
    class function GetPackageName(const PackageBaseName: String; IDE: TDxIDE): String;
    class function GetPackageFullFileName(const InstallFileDir, ComponentName, PackageBaseName: String; IDE: TDxIDE): String;
    class function GetDxBuildNumber(const InstallFileDir: String): Cardinal;
    class function GetDxBuildNumberAsVersion(const BuildNumber: Cardinal): String;
  end;


implementation

uses
  IniFiles, IOUtils, Forms, DxUtils;

{ TDxComponentProfile }

constructor TDxComponentProfile.Create;
begin
  inherited Create;
  FComponentName := '';
  FRequiredPackages := TStringList.Create;
  FOptionalPackages := TStringList.Create;
  FOutdatedPackages := TStringList.Create;
  FIsBase := False;
end;

destructor TDxComponentProfile.Destroy;
begin
  FRequiredPackages.Free;
  FOptionalPackages.Free;
  FOutDatedPackages.Free;
  inherited;
end;

{ TDxProfile }

constructor TDxProfile.Create();
begin
  FFileName := GetCustomProfileFileName;
  if not FileExists(FFileName) then begin
    FFileName := TPath.GetTempFileName;
    ExportBuiltInProfile(FFileName);
  end;
  FComponents := TDxComponentProfileList.Create();
  LoadComponents();
end;

destructor TDxProfile.Destroy;
begin
  FComponents.Free;
  inherited;
end;

procedure TDxProfile.ExportBuiltInProfile(const FileName: String);
begin
  ExportResourceToFile(FileName, ProfileResourceName);
end;

function TDxProfile.IsCustomProfile: Boolean;
begin
  Result := FileName = GetCustomProfileFileName;
end;

procedure TDxProfile.LoadComponents;
var
  Ini: TIniFile;
  Names: TStringList;
  Name: String;
  Comp: TDxComponentProfile;

  procedure StrToList(const S: String; List: TStringList);
  var
    I: Integer;
  begin
    List.CommaText := S;
    for I := List.Count - 1 downto 0 do if List[I] = EmptyStr then List.Delete(I);
  end;
begin
  FComponents.Clear;
  Ini := TIniFile.Create(FileName);
  Names := TStringList.Create;
  try
    Ini.ReadSections(Names);
    for Name in Names do begin
      Comp := TDxComponentProfile.Create;
      Comp.ComponentName := Trim(Name);
      StrToList(Ini.ReadString(Name, RequiredPackagesIdent, ''), Comp.RequiredPackages);
      StrToList(Ini.ReadString(Name, OptionalPackagesIdent, ''), Comp.OptionalPackages);
      StrToList(Ini.ReadString(Name, OutdatedPackagesIdent, ''), Comp.OutdatedPackages);
      Comp.IsBase := Ini.ReadBool(Name, IsBaseIdent, False);
      Components.Add(Comp);
    end;
  finally
    Names.Free;
    Ini.Free;
  end;
end;

class function TDxProfile.GetCustomProfileFileName: String;
begin
  Result := TPath.Combine(ExtractFilePath(Application.ExeName), ProfileResourceName + '.ini');
end;

class function TDxProfile.GetComponentDir(const InstallFileDir, ComponentName: String): String;
begin
  Result := IncludeTrailingPathDelimiter(InstallFileDir) + ComponentName;
end;

class function TDxProfile.GetComponentSourcesDir(const InstallFileDir, ComponentName: String): String;
begin
  Result := GetComponentDir(InstallFileDir, ComponentName) + '\Sources';
end;

class function TDxProfile.GetComponentPackagesDir(const InstallFileDir, ComponentName: String): String;
begin
  Result := GetComponentDir(InstallFileDir, ComponentName) + '\Packages';
end;

class function TDxProfile.GetPackageName(const PackageBaseName: String; IDE: TDxIDE): String;
begin
  Result := PackageBaseName + GetIDEVersionNumberStr(IDE);
end;

class function TDxProfile.GetPackageFullFileName(const InstallFileDir, ComponentName, PackageBaseName: String; IDE: TDxIDE): String;
begin
  Result := GetComponentPackagesDir(InstallFileDir, ComponentName) + '\' +
            GetPackageName(PackageBaseName, IDE) + IDE.PackageSourceFileExtension;
end;

class function TDxProfile.GetDxBuildNumber(const InstallFileDir: String): Cardinal;
const
  VersionIdent     = 'dxVersion = ';
  BuildNumberIdent = 'dxBuildNumber: Cardinal = ';
var
  SourceFile: TextFile;
  S: String;
begin
  Result := 0;
  S := IncludeTrailingPathDelimiter(InstallFileDir) + 'ExpressCore Library\Sources\dxCore.pas';
  if not FileExists(S) then Exit;

  AssignFile(SourceFile, S);
  Reset(SourceFile);
  while not Eof(SourceFile) do begin
    ReadLn(SourceFile, S);
    S := S.Trim;
    if S.StartsWith(VersionIdent) or S.StartsWith(BuildNumberIdent) then begin
      S := S.Substring(S.IndexOf('=') + 1);
      S := S.Remove(S.IndexOf(';'));
      Result := StrToIntDef(S.Trim, 0);
      Break;
    end;
  end;
  CloseFile(SourceFile);
end;

class function TDxProfile.GetDxBuildNumberAsVersion(const BuildNumber: Cardinal): String;
var
  I: Integer;
begin
  if BuildNumber > 0 then begin
    I := BuildNumber mod 10000;
    Result := Format('%d.%d.%d', [BuildNumber div 10000 mod 100, I div 100, I mod 100]);
  end else
    Result := 'n/a';
end;

class function TDxProfile.GetIDEVersionNumberStr(IDE: TDxIDE): String;
begin
  Result := IDE.VersionNumberStr;
  if IsRADStudio(IDE) then Result := StringReplace(Result, 'd', 'rs', [rfIgnoreCase]);
  Result := UpperCase(Result);
end;


end.
