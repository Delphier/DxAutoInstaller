{*******************************************************}
{                                                       }
{          DxAutoInstaller Cookies Classes              }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxCookies;

interface

uses
  Windows, Registry, Forms;

type
  TDxCookies = class
  const
    InstallFileDirIdent = 'InstallFileDir';
  private
    FRegistry: TRegistry;
    function GetInstallFileDir(): String;
    procedure SetInstallFileDir(const Value: String);
  public
    constructor Create();
    destructor Destroy; override;
    property InstallFileDir: String read GetInstallFileDir write SetInstallFileDir;
  end;


implementation

{ TDxCookies }

constructor TDxCookies.Create;
begin
  inherited Create;
  FRegistry := TRegistry.Create;
  FRegistry.RootKey := HKEY_CURRENT_USER;
  FRegistry.OpenKey('Software\' + Application.Title, True);
end;

destructor TDxCookies.Destroy;
begin
  FRegistry.CloseKey;
  FRegistry.Free;
  inherited;
end;

function TDxCookies.GetInstallFileDir: String;
begin
  Result := FRegistry.ReadString(InstallFileDirIdent);
end;

procedure TDxCookies.SetInstallFileDir(const Value: String);
begin
  FRegistry.WriteString(InstallFileDirIdent, Value);
end;

end.
