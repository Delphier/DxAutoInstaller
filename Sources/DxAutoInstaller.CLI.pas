{*******************************************************}
{                                                       }
{             DxAutoInstaller CLI Library               }
{                                                       }
{      https://github.com/Delphier/DxAutoInstaller      }
{                                                       }
{      Copyright(c) 2014-2026 faceker@gmail.com         }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DxAutoInstaller.CLI;

interface

uses
  DxAutoInstaller.Core, DxAutoInstaller.DevExpress, VSoft.CommandLine.Options;

type
  TAppCLI = class
  type
    TOptions = record
      Version: Boolean;
      Help: Boolean;
      IDEList: TIDEList;
      RootDir: TRootDir;
    end;
  private
    FOptions: TOptions;
    FCmdInstall: TCommandDefinition;
    FCmdUninstall: TCommandDefinition;
    procedure RegisterOptions;
    procedure RegisterIDEListOption(const ACommand: TCommandDefinition);
    procedure PrintVersion;
    procedure PrintBanner;
    procedure PrintUsage(const ACommand: string);
  public
    constructor Create;
    procedure Run;
  end;

implementation

uses
  System.SysUtils, DxAutoInstaller.Installations, DxAutoInstaller.Utils;

{ TAppCLI }

constructor TAppCLI.Create;
begin
  RegisterOptions;
end;

procedure TAppCLI.RegisterOptions;
var
  Option: IOptionDefinition;
begin
  TOptionsRegistry.NameValueSeparator := '=';
  TOptionsRegistry.DescriptionTab := 25;

  Option := TOptionsRegistry.RegisterOption<Boolean>('version', 'v', 'Show version information', procedure(const AValue: Boolean) begin FOptions.Version := AValue end);
  Option.HasValue := False;

  Option := TOptionsRegistry.RegisterOption<Boolean>('help', 'h', 'Get help', procedure(const AValue: Boolean) begin FOptions.Help := AValue end);
  Option.HasValue := False;

  Option := TOptionsRegistry.RegisterOption<Boolean>('yes', 'y', 'Automatically answer "yes" to all prompts (non-interactive mode)', procedure(const AValue: Boolean) begin TMessageBox.AssumeYes := AValue end);
  Option.HasValue := False;

  FCmdInstall := TOptionsRegistry.RegisterCommand('install', '', 'Install DevExpress VCL components to IDEs', '', 'install [IDE list] [options]');
  RegisterIDEListOption(FCmdInstall);
  FCmdInstall.RegisterOption<string>('rootdir', 'd', 'Specify the DevExpress root directory', procedure(const AValue: string) begin FOptions.RootDir := AValue end);

  FCmdUninstall := TOptionsRegistry.RegisterCommand('uninstall', '', 'Uninstall DevExpress VCL components from IDEs', '', 'uninstall [IDE list] [options]');
  RegisterIDEListOption(FCmdUninstall);
end;

procedure TAppCLI.RegisterIDEListOption(const ACommand: TCommandDefinition);
begin
  ACommand.RegisterUnNamedOption<string>('Specify the target IDEs (e.g. RAD Studio 13, RS13, RADStudio13)', 'IDE list', procedure(const AValue: string) begin FOptions.IDEList := TIDEList.Create(AValue) end);
end;

procedure TAppCLI.PrintVersion;
begin
  Writeln(TApp.Name, ' CLI ', TApp.Version);
end;

procedure TAppCLI.PrintBanner;
begin
  Writeln('''
 ____           _         _       ___           _        _ _
|  _ \__  __   / \  _   _| |_ ___|_ _|_ __  ___| |_ __ _| | | ___ _ __
| | | \ \/ /  / _ \| | | | __/ _ \| || '_ \/ __| __/ _` | | |/ _ \ '__|
| |_| |>  <  / ___ \ |_| | || (_) | || | | \__ \ || (_| | | |  __/ |
|____//_/\_\/_/   \_\__,_|\__\___/___|_| |_|___/\__\__,_|_|_|\___|_|

''');
  PrintVersion;
  var IDENames := TIDEList.Default.Names;
  Writeln('Installed IDEs: ', if Length(IDENames) = 0 then '<None>' else string.Join(', ', IDENames));
end;

procedure TAppCLI.PrintUsage(const ACommand: string);
begin
  TOptionsRegistry.PrintUsage(ACommand, procedure(const AText: string) begin Writeln(AText) end);
end;

procedure TAppCLI.Run;
begin
  var ParseResult := TOptionsRegistry.Parse;
  if FOptions.Version then
    PrintVersion
  else if FOptions.Help then
    PrintUsage(ParseResult.Command)
  else begin
    if ParseResult.HasErrors then raise Exception.Create(ParseResult.ErrorText);
    PrintBanner;

    var Command := TOptionsRegistry.GetCommandByName(ParseResult.Command);
    if Command = nil then Exit;

    TManifest.CreateInstance;
    if TManifest.Instance.IsCustom then Writeln('Loaded custom manifest file: ', TManifest.CustomFileName);

    Writeln;
    if Length(FOptions.IDEList) = 0 then FOptions.IDEList := TIDEList.Default;
    if Command = FCmdInstall.Command then begin
      if FOptions.RootDir = '' then FOptions.RootDir := TApp.Dir;
      Writeln('DevExpress Root Directory: ', FOptions.RootDir);
      var Version := FOptions.RootDir.Version;
      if Version = 0 then raise Exception.Create('Invalid DevExpress root directory');
      Writeln('DevExpress VCL Version: ', Version.ToText);
      var Installations := TInstallations.Create(FOptions.IDEList, FOptions.RootDir, TManifest.Instance);
      try
        for var Installation in Installations do Installation.Components.CheckAll(True);
        Installations.Execute;
      finally
        Installations.Free;
      end;
    end else
    if Command = FCmdUninstall.Command then
      TUninstallation.Execute(FOptions.IDEList, TManifest.Instance);
  end;
end;

end.
