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
  private
    FIDE: TJclIDE;
    FName: string;
    FPackageVersionStr: string;
    FDelphiInstalled: Boolean;
    FCppBuilderInstalled: Boolean;
    FSupportedPlatforms: TPlatforms;
  public
    constructor Create(AIDE: TJclIDE);
    property Name: string read FName;
    property PackageVersionStr: string read FPackageVersionStr;
    property DelphiInstalled: Boolean read FDelphiInstalled;
    property CppBuilderInstalled: Boolean read FCppBuilderInstalled;
    property SupportedPlatforms: TPlatforms read FSupportedPlatforms;
  end;

  TIDEs = TArray<TIDE>;
  TIDEsHelper = record helper for TIDEs
  private
    class var FJclIDEs: TJclIDEs;
    class var FAll: TIDEs;
  public
    class constructor Create;
    class destructor Destroy;
    class property All: TIDEs read FAll;
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

const
  PlatformJclValues          : array[TPlatform] of TJclPlatform      = (bpWin32, bpWin64, bpWin64x);
  PlatformNames              : array[TPlatform] of string            = ('Win32', 'Win64', 'Win64x');
  PlatformDescriptions       : array[TPlatform] of string            = ('Windows 32-bit', 'Windows 64-bit', 'Windows 64-bit (Modern)');
  PlatformCompilers          : array[TPlatform] of TCompilerGetter   = (TPlatform.DCC32Compiler, TPlatform.DCC64Compiler, TPlatform.DCC64Compiler);
  PlatformCommandLineTools   : array[TPlatform] of TCommandLineTool  = (clDcc32, clDcc64, clBcc64x);
  DelphiPlatforms            = [pfWin32, pfWin64];
  DelphiPlatformsDefault     = [pfWin32];
  CppBuilderPlatforms        = [pfWin32, pfWin64, pfWin64x];
  CppBuilderPlatformsDefault = [pfWin64x];

type
  TError = (errNone,
            errComponentNotFound, errComponentMissingPackages, errComponentMissingDependencies,
            errDelphiNotInstalled, errCppBuilderNotInstalled);
const
  ErrorImageIndexes: array[TError] of Integer = (-1, 6, 6, 6, 6, 6);
  ErrorMessages:     array[TError] of string  = ('',
                                                 'Component Not Found.',
                                                 'Component Missing Packages.',
                                                 'Component Missing Dependencies.',
                                                 'Delphi is not Installed.',
                                                 'C++Builder is not Installed.');

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
end;

{ TIDEsHelper }

class constructor TIDEsHelper.Create;
begin
  FJclIDEs := TJclIDEs.Create;
  for var I := 0 to FJclIDEs.Count - 1 do begin
    var JclIDE := FJclIDEs[I];
    if JclIDE is TJclIDE then FAll := FAll + [TIDE.Create(JclIDE as TJclIDE)];
  end;
end;

class destructor TIDEsHelper.Destroy;
begin
  TArray.FreeValues(FAll);
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

end.
