{*******************************************************}
{                                                       }
{            DxAutoInstaller Utils Library              }
{                                                       }
{      https://github.com/Delphier/DxAutoInstaller      }
{                                                       }
{      Copyright(c) 2014-2026 faceker@gmail.com         }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DxAutoInstaller.Utils;

interface

uses
  System.Classes, System.Win.Registry;

type
  TMessageBox = record
    class function Confirm(const AMessage: string; AButtonCaptions: TArray<string> = []): Boolean; static;
  end;

  TUserEnvironmentVariablePath = record
  const
    ENVIRONMENT = 'Environment';
    PATH        = 'Path';
  private
    class var FRegistry: TRegistry;
    class destructor Destroy;
    class function Registry: TRegistry; static;
    class function Read: TArray<string>; static;
    class procedure Write(const AValue: TArray<string>); static;
    class function SameItem(const A, B: string): Boolean; static;
  public
    class procedure Add(const AItem: string); static;
    class procedure Remove(const AItem: string); static;
  end;

  TLoadFromStreamProc = procedure(AStream: TStream) of object;

function CreateResourceStream(const AResName: string): TResourceStream;
procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
procedure ExportResourceToFile(const AFileName, AResName: string);

implementation

uses
  System.SysUtils, System.UITypes, Winapi.Windows, Winapi.Messages, Vcl.Dialogs;

{ TMessageBox }

class function TMessageBox.Confirm(const AMessage: string; AButtonCaptions: TArray<string>): Boolean;
begin
  MsgDlgIcons[mtConfirmation] := TMsgDlgIcon.mdiInformation;
  Result := MessageDlg(AMessage, mtConfirmation, mbYesNo, 0, mbNo, AButtonCaptions) = mrYes;
end;

{ TUserEnvironmentVariablePath }

class function TUserEnvironmentVariablePath.Registry: TRegistry;
begin
  if not Assigned(FRegistry) then FRegistry := TRegistry.Create;
  Result := FRegistry;
end;

class destructor TUserEnvironmentVariablePath.Destroy;
begin
  FRegistry.Free;
end;

class function TUserEnvironmentVariablePath.Read: TArray<string>;
begin
  Registry.OpenKeyReadOnly(ENVIRONMENT);
  Result := Registry.ReadString(PATH).Split([';']);
  Registry.CloseKey;
end;

class procedure TUserEnvironmentVariablePath.Write(const AValue: TArray<string>);
begin
  Registry.OpenKey(ENVIRONMENT, True);
  Registry.WriteString(PATH, string.Join(';', AValue));
  Registry.CloseKey;
  SendMessage(HWND_BROADCAST, WM_SETTINGCHANGE, 0, LPARAM(PChar(ENVIRONMENT)));
end;

class function TUserEnvironmentVariablePath.SameItem(const A, B: string): Boolean;
begin
  Result := SameFileName(ExcludeTrailingPathDelimiter(A.Trim), ExcludeTrailingPathDelimiter(B.Trim));
end;

class procedure TUserEnvironmentVariablePath.Add(const AItem: string);
begin
  var List := Read;
  for var Item in List do if SameItem(Item, AItem) then Exit;
  Write(List + [AItem]);
end;

class procedure TUserEnvironmentVariablePath.Remove(const AItem: string);
begin
  var List := Read;
  var Count := Length(List);
  for var I := High(List) downto Low(List) do if SameItem(List[I], AItem) then Delete(List, I, 1);
  if Count <> Length(List) then Write(List);
end;

function CreateResourceStream(const AResName: string): TResourceStream;
begin
  Result := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
end;

procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
begin
  var Stream := CreateResourceStream(AResName);
  try
    ALoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure ExportResourceToFile(const AFileName, AResName: string);
begin
  with CreateResourceStream(AResName) do begin
    try
      SaveToFile(AFileName);
    finally
      Free;
    end;
  end;
end;

end.
