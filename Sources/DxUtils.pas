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
  function GetVersionStr(): String;

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

function GetVersionStr(): String;
var
  Major, Minor, Release: Cardinal;
  RS: TResourceStream;
  MS: TMemoryStream;
  Buffer: PVSFIXEDFILEINFO;
  BufferLen: Cardinal;
begin
  Result := '';
  try
    RS := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
  except
    Exit;
  end;

  MS := TMemoryStream.Create;
  try
    MS.CopyFrom(RS, RS.Size);
    if VerQueryValue(MS.Memory, '\', Pointer(Buffer), BufferLen) then begin
      Major := Buffer.dwFileVersionMS shr 16;
      Minor := Buffer.dwFileVersionMS and $FFFF;
      Release := Buffer.dwFileVersionLS shr 16;
      //Build := Buffer.dwFileVersionLS and $FFFF;

      Result := Format('v%d.%d', [Major, Minor]);
      if Release <> 0 then Result := Result + '.' + IntToStr(Release);
    end;
  finally
    MS.Free;
    RS.Free;
  end;
end;

{ TDxForm }

constructor TDxForm.Create(AOwner: TComponent);
begin
  inherited;
  if Screen.Fonts.IndexOf(Font.Name) < 0 then Font.Name := 'Tahoma';
end;

end.



