{*******************************************************}
{                                                       }
{             DxAutoInstaller Core Library              }
{                                                       }
{      https://github.com/Delphier/DxAutoInstaller      }
{                                                       }
{      Copyright(c) 2014-2026 faceker@gmail.com         }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DxAutoInstaller.Core;

interface

uses
  JclIDEUtils, JclCompilerUtils, DxAutoInstaller.Utils;

type
  TApp = class
  const
    {$I ..\Resources\App.inc}
  strict private
    class var FDir: string;
    class var FLog: TLog;
  public
    class constructor Create;
    class destructor Destroy;
    class property Dir: string read FDir;
    class property Log: TLog read FLog;
  end;

  TJclIDE = TJclBDSInstallation;
  TJclIDEs = TJclBorRADToolInstallations;
  TJclPlatform = TJclBDSPlatform;
  TJclDCCCompiler = TJclDCC32;

  TCompiler = TJclDCCCompiler;

  TPlatform = (pfWin32, pfWin64, pfWin64x);
  TPlatforms = set of TPlatform;

  TIDE = class
  const
    EnvironmentVariableSectionName = 'Environment Variables';
  strict private
    FCore: TJclIDE;
    FName: string;
    FPackageVersionStr: string;
    FDelphiInstalled: Boolean;
    FCppBuilderInstalled: Boolean;
    FSupportedPlatforms: TPlatforms;
    FSupportedDesigntimePlatforms: TPlatforms;
  private
    class function DCC32(AIDE: TIDE): TCompiler; static;
    class function DCC64(AIDE: TIDE): TCompiler; static;
  public
    constructor Create(ACore: TJclIDE);
    property Core: TJclIDE read FCore;
    property Name: string read FName;
    property PackageVersionStr: string read FPackageVersionStr;
    property DelphiInstalled: Boolean read FDelphiInstalled;
    property CppBuilderInstalled: Boolean read FCppBuilderInstalled;
    property SupportedPlatforms: TPlatforms read FSupportedPlatforms;
    property SupportedDesigntimePlatforms: TPlatforms read FSupportedDesigntimePlatforms;
    procedure CheckRunning;
    function ReadEnvironmentVariable(const AKey: string): string;
    procedure WriteEnvironmentVariable(const AKey, AValue: string);
    procedure SwitchCompiler(const APlatform: TPlatform);
  end;

  TIDEList = TArray<TIDE>;
  TIDEListHelper = record helper for TIDEList
  strict private
    class var FJclIDEs: TJclIDEs;
    class var FDefault: TIDEList;
  public
    class constructor Create;
    class destructor Destroy;
    class property Default: TIDEList read FDefault;
  end;

  TPlatformHelper = record helper for TPlatform
  public
    function ToJclValue: TJclPlatform;
  end;

  TPlatformsHelper = record helper for TPlatforms
    function Count: Integer;
    function ToString: string;
  end;

  TCompilerGetter = function(AIDE: TIDE): TCompiler;

const
  PlatformJclValues    : array[TPlatform] of TJclPlatform = (bpWin32, bpWin64, bpWin64x);
  PlatformNames        : array[TPlatform] of string       = ('Win32', 'Win64', 'Win64x');
  PlatformDescriptions : array[TPlatform] of string       = ('Windows 32-bit', 'Windows 64-bit', 'Windows 64-bit (Modern)');

  PlatformCompilers        : array[TPlatform] of TCompilerGetter  = (TIDE.DCC32, TIDE.DCC64, TIDE.DCC64);
  PlatformCompilerOptions  : array[TPlatform] of string           = ('', '', '-jf:coffi');
  PlatformCommandLineTools : array[TPlatform] of TCommandLineTool = (clDcc32, clDcc64, clBcc64x);

  DelphiSupportedPlatforms            = [pfWin32, pfWin64];
  DelphiSupportedPlatformsDefault     = [pfWin32];
  CppBuilderSupportedPlatforms        = [pfWin32, pfWin64, pfWin64x];
  CppBuilderSupportedPlatformsDefault = [pfWin64x];

  // 32-bit/64-bit IDE. Requires a synchronized update to IDE.SupportedDesigntimePlatforms.
  DesigntimeSupportedPlatforms = [pfWin32, pfWin64];
  // 64-bit IDE only supports Delphi Win64 and WinARM64EC.
  DelphiSupportedDesigntimePlatforms     : array[TPlatform] of TPlatforms = ([pfWin32], [pfWin32, pfWin64], []);
  // 64-bit IDE only supports C++Builder Win64x.
  CppBuilderSupportedDesigntimePlatforms : array[TPlatform] of TPlatforms = ([pfWin32], [pfWin32], [pfWin32, pfWin64]);

type
  TError = (errNone,
            errComponentNotFound, errComponentPackagesMissing, errComponentDependenciesMissing,
            errDelphiNotInstalled, errCppBuilderNotInstalled);
const
  ErrorImageIndexes: array[TError] of Integer = (-1, 6, 6, 6, 6, 6);
  ErrorMessages:     array[TError] of string  = ('',
                                                 'Component Not Found',
                                                 'Component Packages Missing',
                                                 'Component Dependencies Missing',
                                                 'Delphi Not Installed',
                                                 'C++Builder Not Installed');

implementation

uses
  System.SysUtils, System.Generics.Collections, Vcl.Forms,
  DxAutoInstaller.Tasks;


{ TApp }

class constructor TApp.Create;
begin
  FDir := ExtractFileDir(Application.ExeName);
  FLog := TLog.Create;
end;

class destructor TApp.Destroy;
begin
  FLog.Free;
end;

{ TIDE }

constructor TIDE.Create(ACore: TJclIDE);
begin
  FCore := ACore;
  FCore.OutputCallback := TTask.WriteLog;
  FName := FCore.Name;
  FPackageVersionStr := FCore.VersionNumberStr.Replace('d', 'RS');
  FDelphiInstalled := FCore.Personalities * [bpDelphi32, bpDelphi64] <> [];
  FCppBuilderInstalled := FCore.Personalities * [bpBCBuilder32, bpBCBuilder64] <> [];
  for var I := Low(TPlatform) to High(TPlatform) do if PlatformCommandLineTools[I] in FCore.CommandLineTools then Include(FSupportedPlatforms, I);
  FSupportedDesigntimePlatforms := [pfWin32];
  // 64-bit IDE: RAD Studio 12.3+
  if FileExists(FCore.IdeExeFileName[True]) then Include(FSupportedDesigntimePlatforms, pfWin64);
end;

procedure TIDE.CheckRunning;
const
  Message = '%s is currently running. Please close it to proceed';
begin
  if FCore.AnyInstanceRunning then raise Exception.CreateFmt(Message, [Name]);
end;

function TIDE.ReadEnvironmentVariable(const AKey: string): string;
begin
  Result := FCore.ConfigData.ReadString(EnvironmentVariableSectionName, AKey, '');
end;

procedure TIDE.WriteEnvironmentVariable(const AKey, AValue: string);
begin
  if AValue.IsEmpty then
    FCore.ConfigData.DeleteKey(EnvironmentVariableSectionName, AKey)
  else
    FCore.ConfigData.WriteString(EnvironmentVariableSectionName, AKey, AValue);
end;

procedure TIDE.SwitchCompiler(const APlatform: TPlatform);
begin
  FCore.DCC := PlatformCompilers[APlatform](Self);
end;

class function TIDE.DCC32(AIDE: TIDE): TCompiler;
begin
  Result := AIDE.FCore.DCC32;
end;

class function TIDE.DCC64(AIDE: TIDE): TCompiler;
begin
  Result := AIDE.FCore.DCC64;
end;

{ TIDEListHelper }

class constructor TIDEListHelper.Create;
begin
  FJclIDEs := TJclIDEs.Create;
  for var I := 0 to FJclIDEs.Count - 1 do begin
    var JclIDE := FJclIDEs[I];
    if JclIDE is TJclIDE then FDefault := FDefault + [TIDE.Create(JclIDE as TJclIDE)];
  end;
end;

class destructor TIDEListHelper.Destroy;
begin
  TArray.FreeValues(FDefault);
  FJclIDEs.Free;
end;

{ TPlatformHelper }

function TPlatformHelper.ToJclValue: TJclPlatform;
begin
  Result := PlatformJclValues[Self];
end;

{ TPlatformsHelper }

function TPlatformsHelper.Count: Integer;
begin
  Result := 0;
  for var I in Self do Inc(Result);
end;

function TPlatformsHelper.ToString: string;
begin
  var Strings: TArray<string> := [];
  for var I in Self do Strings := Strings + [PlatformNames[I]];
  Result := string.Join(',', Strings);
end;

end.
