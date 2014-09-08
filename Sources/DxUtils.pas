{*******************************************************}
{                                                       }
{            DxAutoInstaller Utils Library              }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxUtils;

interface

uses
  Windows, SysUtils, Classes, Forms;

type
  TDxForm = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

  procedure DeleteFiles(const FilePath: String);
  procedure DeleteDirectory(const DirPath: String);
  procedure CopyFile(const ExistingFileName, NewFileName: String; const ReplaceExisting: Boolean);
  procedure CopyFilesToDirectory(const FilePath, DirPath: String);
  function IsEmptyDirectory(const DirPath, ExcludeFile: String): Boolean;

const
  CRLF = #13#10;

implementation

uses
  JclFileUtils;

// Example: C:\Windows\System32\*.exe;*.dll
procedure DeleteFiles(const FilePath: String);
var
  List: TStringList;
  S: String;
begin
  List := TStringList.Create;
  try
    BuildFileList(FilePath, faAnyFile, List, True);
    for S in List do Windows.DeleteFile(PChar(S));
  finally
    List.Free;
  end;
end;

procedure DeleteDirectory(const DirPath: String);
begin
  JclFileUtils.DeleteDirectory(DirPath, False);
end;

procedure CopyFile(const ExistingFileName, NewFileName: String; const ReplaceExisting: Boolean);
begin
  Windows.CopyFile(PChar(ExistingFileName), PChar(NewFileName), not ReplaceExisting);
end;

procedure CopyFilesToDirectory(const FilePath, DirPath: String);
var
  List: TStringList;
  S: String;
begin
  List := TStringList.Create;
  try
    BuildFileList(FilePath, faAnyFile, List, True);
    ForceDirectories(DirPath);
    for S in List do CopyFile(S, IncludeTrailingPathDelimiter(DirPath) + ExtractFileName(S), True);
  finally
    List.Free;
  end;
end;

function IsEmptyDirectory(const DirPath, ExcludeFile: String): Boolean;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    BuildFileList(IncludeTrailingPathDelimiter(DirPath) + '*.*', faAnyFile, List, True);
    Result := (List.Count = 0) or ((List.Count = 1) and (List.IndexOf(ExcludeTrailingPathDelimiter(ExcludeFile)) = 0));
  finally
    List.Free;
  end;
end;

{ TDxForm }

constructor TDxForm.Create(AOwner: TComponent);
begin
  inherited;
  if Screen.Fonts.IndexOf(Font.Name) < 0 then Font.Name := 'Tahoma';
end;

end.



