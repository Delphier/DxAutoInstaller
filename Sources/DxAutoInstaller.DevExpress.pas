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

type
  TManifest = class
  type
    TComponent = record
      Name: string;
      IsBase: Boolean;
      RequiredPackages: TArray<string>;
      OptionalPackages: TArray<string>;
      OutdatedPackages: TArray<string>;
    end;

  const
    CustomFileBaseName  = 'DevExpress.ini';
    ResourceName        = 'DevExpressManifest';
  private
    FIsCustom: Boolean;
    FComponents: TArray<TComponent>;
    procedure ReadComponents;
  public
    class function CustomFileName: string;
    class function CustomFileExists: Boolean;
    constructor Create(const AUseCustomFile: Boolean);
    property IsCustom: Boolean read FIsCustom;
    property Components: TArray<TComponent> read FComponents;
    procedure Export;
  end;

implementation

uses
  System.Classes, System.SysUtils, System.IOUtils, System.IniFiles, Vcl.Forms, DxAutoInstaller.Utils;

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
        var Component := Default(TComponent);
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

end.
