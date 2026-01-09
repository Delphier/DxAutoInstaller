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

procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
procedure ExportResourceToFile(const AFileName, AResName: string);

implementation

procedure LoadResourceToStream(ALoadFromStream: TLoadFromStreamProc; const AResName: string);
begin
  var Stream := TResourceStream.Create(HInstance, AResName, RT_RCDATA);
  try
    ALoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure ExportResourceToFile(const AFileName, AResName: string);
begin
  with TResourceStream.Create(HInstance, AResName, RT_RCDATA) do begin
    try
      SaveToFile(AFileName);
    finally
      Free;
    end;
  end;
end;

end.
