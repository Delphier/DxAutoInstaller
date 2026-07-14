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

{$I DxAutoInstaller.inc}

interface

uses
  Winapi.Windows, System.Classes, JclSysUtils;

type
  TLog = class
  const
    Separator = '-------------------------------------------------------------';
  private
    FFile: TJclSimpleLog;
    FLast: TStrings;
    procedure DoWrite(const AText: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Last: TStrings read FLast;
    procedure Write(const AText: string);
    procedure WriteSeparator;
  end;

  TMessageBox = record
    class var AssumeYes: Boolean;
    class procedure Information(const AMessage: string); static;
    class function Confirm(const AMessage: string; AButtonCaptions: TArray<string> = []): Boolean; static;
  end;

  TLoadFromStreamProc = procedure(AStream: TStream) of object;

function CreateResourceStream(const AResName: string): TResourceStream;
procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
procedure ExportResourceToFile(const AFileName, AResName: string);

procedure RegistryDeleteKey(const ARootKey: HKEY; const AKey: string);
procedure RegistryDeleteValue(const ARootKey: HKEY; const AKey, AName: string);
procedure RegistryWriteString(const ARootKey: HKEY; const AKey, AName, AValue: string);
procedure RegistryWriteBool(const ARootKey: HKEY; const AKey, AName: string; const AValue: Boolean);

implementation

uses
  System.SysUtils, System.StrUtils, System.UITypes, System.Win.Registry, Vcl.Forms, Vcl.Dialogs;

{ TLog }

constructor TLog.Create;
begin
  FFile := TJclSimpleLog.Create(ChangeFileExt(Application.ExeName, '.log'));
  FLast := TStringList.Create;
end;

destructor TLog.Destroy;
begin
  FFile.Free;
  FLast.Free;
  inherited;
end;

procedure TLog.DoWrite(const AText: string);
begin
  FFile.Write(AText);
  {$IFDEF CLI}
  Writeln(AText);
  {$ENDIF}
end;

procedure TLog.Write(const AText: string);
begin
  DoWrite(AText);
  FLast.Add(AText);
end;

procedure TLog.WriteSeparator;
begin
  DoWrite(Separator);
  FLast.Clear;
end;

{ TMessageBox }

class procedure TMessageBox.Information(const AMessage: string);
begin
  ShowMessage(AMessage);
end;

class function TMessageBox.Confirm(const AMessage: string; AButtonCaptions: TArray<string>): Boolean;
begin
  if AssumeYes then Exit(True);
  {$IFDEF GUI}
  MsgDlgIcons[mtConfirmation] := TMsgDlgIcon.mdiInformation;
  Result := MessageDlg(AMessage, mtConfirmation, mbYesNo, 0, mbNo, AButtonCaptions) = mrYes;
  {$ELSE}
  Writeln(AMessage);
  Write('[y/N] ? ');
  var Yes := '';
  Readln(Yes);
  Result := MatchText(Yes.Trim, ['Y', 'Yes']);
  {$ENDIF}
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

procedure RegistryWrite(const ARootKey: HKEY; const AKey: string; AProc: TProc<TRegistry>);
begin
  var Registry := TRegistry.Create;
  try
    Registry.RootKey := ARootKey;
    if not AKey.IsEmpty then Registry.OpenKey(AKey, True);
    AProc(Registry);
  finally
    Registry.Free;
  end;
end;

procedure RegistryDeleteKey(const ARootKey: HKEY; const AKey: string);
begin
  RegistryWrite(ARootKey, '', procedure(ARegistry: TRegistry) begin ARegistry.DeleteKey(AKey) end);
end;

procedure RegistryDeleteValue(const ARootKey: HKEY; const AKey, AName: string);
begin
  RegistryWrite(ARootKey, '', procedure(ARegistry: TRegistry) begin
    if not ARegistry.KeyExists(AKey) then Exit;
    ARegistry.OpenKey(AKey, False);
    ARegistry.DeleteValue(AName);
  end);
end;

procedure RegistryWriteString(const ARootKey: HKEY; const AKey, AName, AValue: string);
begin
  RegistryWrite(ARootKey, AKey, procedure(ARegistry: TRegistry) begin ARegistry.WriteString(AName, AValue) end);
end;

procedure RegistryWriteBool(const ARootKey: HKEY; const AKey, AName: string; const AValue: Boolean);
begin
  RegistryWrite(ARootKey, AKey, procedure(ARegistry: TRegistry) begin ARegistry.WriteBool(AName, AValue) end);
end;

end.
