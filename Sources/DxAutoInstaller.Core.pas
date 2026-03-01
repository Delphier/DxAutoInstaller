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
  JclIDEUtils, JclCompilerUtils;

type
  TApp = class
  const
    {$I ..\Resources\App.inc}
  public
    class function Dir: string;
  end;

  TJclIDE = TJclBDSInstallation;
  TJclIDEs = TJclBorRADToolInstallations;
  TJclPlatform = TJclBDSPlatform;
  TJclCompiler = TJclBorlandCommandLineTool;

  TPlatform = (pfWin32, pfWin64, pfWin64x);
  TPlatforms = set of TPlatform;

  TIDE = class
  const
    EnvironmentVariableSectionName = 'Environment Variables';
  private
    FIDE: TJclIDE;
    FName: string;
    FPackageVersionStr: string;
    FDelphiInstalled: Boolean;
    FCppBuilderInstalled: Boolean;
    FSupportedPlatforms: TPlatforms;
    FSupportedDesigntimePlatforms: TPlatforms;
  public
    constructor Create(AIDE: TJclIDE);
    property Name: string read FName;
    property PackageVersionStr: string read FPackageVersionStr;
    property DelphiInstalled: Boolean read FDelphiInstalled;
    property CppBuilderInstalled: Boolean read FCppBuilderInstalled;
    property SupportedPlatforms: TPlatforms read FSupportedPlatforms;
    property SupportedDesigntimePlatforms: TPlatforms read FSupportedDesigntimePlatforms;
    procedure CheckRunning;
    function ReadEnvironmentVariable(const AKey: string): string;
    procedure WriteEnvironmentVariable(const AKey, AValue: string);
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

  TCompiler = TJclCompiler;
  TCompilerGetter = function(AIDE: TIDE): TCompiler;

  TPlatformHelper = record helper for TPlatform
  private
    class function DCC32Compiler(AIDE: TIDE): TCompiler; static;
    class function DCC64Compiler(AIDE: TIDE): TCompiler; static;
  public
    function ToJclValue: TJclPlatform;
    function Compiler(AIDE: TIDE): TCompiler;
  end;

  TPlatformsHelper = record helper for TPlatforms
    function ToString: string;
  end;

const
  PlatformJclValues    : array[TPlatform] of TJclPlatform = (bpWin32, bpWin64, bpWin64x);
  PlatformNames        : array[TPlatform] of string       = ('Win32', 'Win64', 'Win64x');
  PlatformDescriptions : array[TPlatform] of string       = ('Windows 32-bit', 'Windows 64-bit', 'Windows 64-bit (Modern)');

  PlatformCompilers        : array[TPlatform] of TCompilerGetter  = (TPlatform.DCC32Compiler, TPlatform.DCC64Compiler, TPlatform.DCC64Compiler);
  PlatformCompilerOptions  : array[TPlatform] of string           = ('', '', '');
  PlatformCommandLineTools : array[TPlatform] of TCommandLineTool = (clDcc32, clDcc64, clBcc64x);

  DelphiSupportedPlatforms            = [pfWin32, pfWin64];
  DelphiSupportedPlatformsDefault     = [pfWin32];
  CppBuilderSupportedPlatforms        = [pfWin32, pfWin64, pfWin64x];
  CppBuilderSupportedPlatformsDefault = [pfWin64x];

  // 32-bit/64-bit IDE. Requires a synchronized update to IDE.SupportedDesigntimePlatforms.
  DesigntimeSupportedPlatforms = [pfWin32, pfWin64];
  // 64-bit IDE only supports Delphi Win64 and WinARM64EC.
  PlatformSupportedDesigntimePlatformsDelphi    : array[TPlatform] of TPlatforms = ([pfWin32], [pfWin32, pfWin64], []);
  // 64-bit IDE only supports C++Builder Win64x.
  PlatformSupportedDesigntimePlatformsCppBuilder: array[TPlatform] of TPlatforms = ([pfWin32], [pfWin32], [pfWin64]);

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
  System.SysUtils, System.Generics.Collections, Vcl.Forms;


{ TApp }

class function TApp.Dir: string;
begin
  Result := ExtractFileDir(Application.ExeName);
end;

{ TIDE }

constructor TIDE.Create(AIDE: TJclIDE);
begin
  FIDE := AIDE;
  FName := FIDE.Name;
  FPackageVersionStr := FIDE.VersionNumberStr.Replace('d', 'RS');
  FDelphiInstalled := FIDE.Personalities * [bpDelphi32, bpDelphi64] <> [];
  FCppBuilderInstalled := FIDE.Personalities * [bpBCBuilder32, bpBCBuilder64] <> [];
  for var I := Low(TPlatform) to High(TPlatform) do if PlatformCommandLineTools[I] in FIDE.CommandLineTools then Include(FSupportedPlatforms, I);
  FSupportedDesigntimePlatforms := [pfWin32];
  // 64-bit IDE: RAD Studio 12.3+
  if FileExists(FIDE.IdeExeFileName[True]) then Include(FSupportedDesigntimePlatforms, pfWin64);
end;

procedure TIDE.CheckRunning;
const
  Message = '%s is currently running. Please close it to proceed';
begin
  if FIDE.AnyInstanceRunning then raise Exception.CreateFmt(Message, [Name]);
end;

function TIDE.ReadEnvironmentVariable(const AKey: string): string;
begin
  Result := FIDE.ConfigData.ReadString(EnvironmentVariableSectionName, AKey, '');
end;

procedure TIDE.WriteEnvironmentVariable(const AKey, AValue: string);
begin
  if AValue.IsEmpty then
    FIDE.ConfigData.DeleteKey(EnvironmentVariableSectionName, AKey)
  else
    FIDE.ConfigData.WriteString(EnvironmentVariableSectionName, AKey, AValue);
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

function TPlatformHelper.Compiler(AIDE: TIDE): TCompiler;
begin
  Result := PlatformCompilers[Self](AIDE);
end;

class function TPlatformHelper.DCC32Compiler(AIDE: TIDE): TCompiler;
begin
  Result := AIDE.FIDE.DCC32;
end;

class function TPlatformHelper.DCC64Compiler(AIDE: TIDE): TCompiler;
begin
  Result := AIDE.FIDE.DCC64;
end;

{ TPlatformsHelper }

function TPlatformsHelper.ToString: string;
begin
  var Strings: TArray<string> := [];
  for var I in Self do Strings := Strings + [PlatformNames[I]];
  Result := string.Join(',', Strings);
end;

end.
