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

{$I DxAutoInstaller.inc}

interface

uses
  {$IFDEF GUI}
  DxAutoInstaller.UI.ProgressForm,
  {$ENDIF}
  System.SysUtils, System.Threading;

type
  TTask = class
  private
    class var FInstance: ITask;
    class var FAborted: Boolean;
    {$IFDEF GUI}
    class var FProgressForm: TProgressForm;
    {$ENDIF}
    class procedure DoExecute(const ATitle: string; AProc: TProc);
  public
    class property Instance: ITask read FInstance;
    class property Aborted: Boolean read FAborted write FAborted;
    class procedure Execute(const ATitle: string; const AStepCount: Cardinal; AProc: TProc);
    class function LastLog: string;
    class procedure WriteLog(const AText: string);
    class procedure WriteLogSeparator; overload;
    class procedure WriteLogSeparator(const AStartText, AEndText: string; AProc: TProc); overload;
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
  {$IFDEF GUI}
  Application.MainForm.Hide;
  try
    FProgressForm := TProgressForm.Create(nil);
    try
      FProgressForm.ProgressBar.Properties.Max := AStepCount;
      DoExecute(ATitle, AProc);
      FProgressForm.ShowModal;
    finally
      FreeAndNil(FProgressForm);
    end;
  finally
    Application.MainForm.Show;
  end;
  {$ELSE}
  DoExecute(ATitle, AProc);
  {$ENDIF}
end;

class procedure TTask.DoExecute(const ATitle: string; AProc: TProc);
begin
  WriteLogSeparator;
  WriteLog(Format('%s v%s', [TApp.Name, TApp.Version]));
  WriteLog(ATitle);
  WriteLog('Starts: ' + Now.ToString);

  {$IFDEF GUI}
  FInstance := System.Threading.TTask.Run(procedure begin
  {$ENDIF}
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
      {$IFDEF GUI}
      TThread.Queue(nil, procedure begin FProgressForm.Done end);
      {$ENDIF}
    end;
  {$IFDEF GUI}
  end);
  {$ENDIF}
end;

class function TTask.LastLog: string;
begin
  Result := TApp.Log.Last.Text;
end;

class procedure TTask.WriteLog(const AText: string);
begin
  TApp.Log.Write(AText);
  {$IFDEF GUI}
  TThread.Queue(nil, procedure begin FProgressForm.Log.Lines.Add(AText) end);
  {$ENDIF}
end;

class procedure TTask.WriteLogSeparator;
begin
  TApp.Log.WriteSeparator;
  {$IFDEF GUI}
  TThread.Queue(nil, procedure begin FProgressForm.Log.Lines.Add(TApp.Log.Separator) end);
  {$ENDIF}
end;

class procedure TTask.WriteLogSeparator(const AStartText, AEndText: string; AProc: TProc);
begin
  WriteLogSeparator;
  WriteLog(AStartText);
  var Stopwatch := TStopwatch.StartNew;
  AProc;
  Stopwatch.Stop;
  WriteLog(Format('%s (Elapsed Time: %s)', [AEndText, Stopwatch.Elapsed.ToString]));
end;

class procedure TTask.StepIt;
begin
  {$IFDEF GUI}
  TThread.Queue(nil, procedure begin FProgressForm.ProgressBar.Position := FProgressForm.ProgressBar.Position + 1 end);
  {$ENDIF}
end;

end.
