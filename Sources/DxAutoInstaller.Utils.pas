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
  System.Classes, JclSysUtils;

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
    class procedure Information(const AMessage: string); static;
    class function Confirm(const AMessage: string; AButtonCaptions: TArray<string> = []): Boolean; static;
  end;

  TLoadFromStreamProc = procedure(AStream: TStream) of object;

function CreateResourceStream(const AResName: string): TResourceStream;
procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
procedure ExportResourceToFile(const AFileName, AResName: string);

implementation

uses
  System.SysUtils, System.UITypes, Winapi.Windows, Vcl.Forms, Vcl.Dialogs;

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
  MsgDlgIcons[mtConfirmation] := TMsgDlgIcon.mdiInformation;
  Result := MessageDlg(AMessage, mtConfirmation, mbYesNo, 0, mbNo, AButtonCaptions) = mrYes;
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
