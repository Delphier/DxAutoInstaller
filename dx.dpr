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

program dx;

{$APPTYPE CONSOLE}

{$R *.res}
{$R Shared.res}

uses
  System.SysUtils,
  DxAutoInstaller.CLI in 'Sources\DxAutoInstaller.CLI.pas';

begin
  ReportMemoryLeaksOnShutdown := True;

  var App := TAppCLI.Create;
  try
    try
      App.Run;
    except
      on E: Exception do Writeln(E.ClassName, ': ', E.Message);
    end;
  finally
    App.Free;
  end;
end.
