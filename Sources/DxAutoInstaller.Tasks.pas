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

unit DxAutoInstaller.Tasks;

interface

uses
  System.SysUtils, System.Threading, DxAutoInstaller.UI.ProgressForm;

type
  TTask = class
  private
    class var FInstance: ITask;
    class var FAborted: Boolean;
    class var FProgressForm: TProgressForm;
    class procedure DoExecute(AProc: TProc);
  public
    class property Instance: ITask read FInstance;
    class property Aborted: Boolean read FAborted write FAborted;
    class procedure Execute(const ATitle: string; const AStepCount: Cardinal; AProc: TProc);
    class function LastLog: string;
    class procedure WriteLog(const AText: string);
    class procedure WriteLogSeparator;
    class procedure StepIt;
  end;

implementation

uses
  System.Classes, System.DateUtils, System.Diagnostics, Vcl.Forms,
  DxAutoInstaller.Core;

{ TTask }

class procedure TTask.Execute(const ATitle: string; const AStepCount: Cardinal; AProc: TProc);
begin
  FInstance := nil;
  FAborted := False;
  Application.MainForm.Hide;
  try
    FProgressForm := TProgressForm.Create(nil);
    try
      FProgressForm.ProgressBar.Properties.Max := AStepCount;

      WriteLogSeparator;
      WriteLog(Format('%s v%s', [TApp.Name, TApp.Version]));
      WriteLog(ATitle);
      WriteLog('Starts: ' + Now.ToString);

      DoExecute(AProc);
      FProgressForm.ShowModal;
    finally
      FreeAndNil(FProgressForm);
    end;
  finally
    Application.MainForm.Show;
  end;
end;

class procedure TTask.DoExecute(AProc: TProc);
begin
  FInstance := System.Threading.TTask.Run(procedure begin
    var Stopwatch := TStopwatch.StartNew;
    try
      try
        AProc;
      except
        var E := AcquireExceptionObject;
        TThread.Queue(nil, procedure begin raise E end);
      end;
    finally
      Stopwatch.Stop;
      WriteLogSeparator;
      WriteLog(if Aborted then 'Aborted.' else 'Done!');
      WriteLog('Ends: ' + Now.ToString);
      WriteLog('Elapsed Time: ' + Stopwatch.Elapsed.ToString);
      TThread.Queue(nil, procedure begin FProgressForm.Done end);
    end;
  end);
end;

class function TTask.LastLog: string;
begin
  Result := TApp.Log.Last.Text;
end;

class procedure TTask.WriteLog(const AText: string);
begin
  TApp.Log.Write(AText);
  TThread.Queue(nil, procedure begin FProgressForm.Log.Lines.Add(AText) end);
end;

class procedure TTask.WriteLogSeparator;
begin
  TApp.Log.WriteSeparator;
  TThread.Queue(nil, procedure begin FProgressForm.Log.Lines.Add(TApp.Log.Separator) end);
end;

class procedure TTask.StepIt;
begin
  TThread.Queue(nil, procedure begin FProgressForm.ProgressBar.Position := FProgressForm.ProgressBar.Position + 1 end);
end;

end.
