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
  System.Classes, Winapi.Windows;

type
  TLoadFromStreamProc = procedure(AStream: TStream) of object;

function CreateResourceStream(const AResName: string): TResourceStream;
procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
procedure ExportResourceToFile(const AFileName, AResName: string);

implementation

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
