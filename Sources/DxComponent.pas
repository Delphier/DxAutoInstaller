{*******************************************************}
{                                                       }
{          DxAutoInstaller Component Classes            }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxComponent;

interface

uses
  SysUtils, Classes, Generics.Collections, DxProfile;

type
  TDxComponent = class;
  TDxComponentList = TObjectList<TDxComponent>;

  TDxPackageCategory = (dxpcNormal, dxpcIBX, dxpcTeeChart, dxpcFireDAC, dxpcBDE);
  TDxPackageUsage = (dxpuDesigntimeOnly, dxpuRuntimeOnly, dxpuDesigntimeAndRuntime);

  TDxPackage = class
  private
    FFullFileName: String;
    FName: String;
    FCategory: TdxPackageCategory;
    FDescription: String;
    FUsage: TDxPackageUsage;
    FRequires: TStringList;
    FExists: Boolean;
    FRequired: Boolean;
    FDependentComponents: TDxComponentList;
    procedure ReadOptions();
  public
    constructor Create(const FullFileName: String);
    destructor Destroy; override;
    property FullFileName: String read FFullFileName;
    property Name: String read FName;
    property Category: TDxPackageCategory read FCategory;
    property Description: String read FDescription;
    property Usage: TDxPackageUsage read FUsage;
    property Requires: TStringList read FRequires;
    property Exists: Boolean read FExists;
    property Required: Boolean read FRequired write FRequired;
    property DependentComponents: TDxComponentList read FDependentComponents;
  end;

  TDxPackageList = TObjectList<TDxPackage>;
  TDxComponentState = (dxcsInstall, dxcsNotInstall, dxcsNotFound, dxcsNotSupported, dxcsMissing);

  TDxComponent = class
  private
    FProfile: TDxComponentProfile;
    FPackages: TDxPackageList;
    FParentComponents: TDxComponentList;
    FSubComponents: TDxComponentList;
    FState: TDxComponentState;
    procedure SetState(const Value: TDxComponentState);
  public
    constructor Create(ComponentProfile: TDxComponentProfile);
    destructor Destroy; override;
    property Profile: TDxComponentProfile read FProfile;
    property Packages: TDxPackageList read FPackages;
    property ParentComponents: TDxComponentList read FParentComponents write FParentComponents;
    property SubComponents: TDxComponentList read FSubComponents write FSubComponents;
    property State: TDxComponentState read FState write SetState;
    function GetExistsPackageCount(): Integer;
    function IsMissingDependents(): Boolean;
  end;

const
  DPKDescriptionOptionIdent     = '{$DESCRIPTION ''';
  DPKDesigntimeOnlyOptionIdent  = '{$DESIGNONLY';
  DPKRuntimeOnlyOptionIdent     = '{$RUNONLY';
  DPKRequiresOptionIdent        = 'requires';

  dxcsEditModes = [dxcsInstall, dxcsNotInstall];


implementation

{ TDxPackage }

constructor TDxPackage.Create(const FullFileName: String);
begin
  inherited Create;
  FFullFileName := FullFileName;
  FName := ChangeFileExt(ExtractFileName(FullFileName), '');
  if Pos('IBX', FName) > 0 then FCategory := dxpcIBX
    else if Pos('TeeChart', FName)> 0 then FCategory := dxpcTeeChart
    else if Pos('FireDAC', FName) > 0 then FCategory := dxpcFireDAC
    else if Pos('BDE', FName) > 0 then FCategory := dxpcBDE
    else FCategory := dxpcNormal;
  FDescription := '';
  FUsage := dxpuDesigntimeAndRuntime;
  FRequires := TStringList.Create;
  FExists := FileExists(FullFileName);
  FRequired := True;
  FDependentComponents := TDxComponentList.Create(False);
  ReadOptions;
end;

destructor TDxPackage.Destroy;
begin
  FRequires.Free;
  FDependentComponents.Free;
  inherited;
end;

procedure TDxPackage.ReadOptions;
var
  DPK: TStringList;
  S: String;
  IsInRequiresPart: Boolean;
begin
  if not Exists then Exit;
  DPK := TStringList.Create;
  try
    DPK.LoadFromFile(FullFileName);
    IsInRequiresPart := False;
    for S in DPK do begin
      if IsInRequiresPart then begin
        if Pos(',', S) > 0 then FRequires.Add(Trim(StringReplace(S, ',', '', []))) else begin
          FRequires.Add(Trim(StringReplace(S, ';', '', [])));
          Break;
        end;
      end else begin
        if Pos(DPKDescriptionOptionIdent, S) > 0 then
          FDescription := Copy(S, Length(DPKDescriptionOptionIdent) + 1, Length(S) - Length(DPKDescriptionOptionIdent) - 2)
        else if Pos(DPKDesigntimeOnlyOptionIdent, S) > 0 then FUsage := dxpuDesigntimeOnly
        else if Pos(DPKRuntimeOnlyOptionIdent, S) > 0 then FUsage := dxpuRuntimeOnly
        else if Trim(S) = DPKRequiresOptionIdent then IsInRequiresPart := True;
      end;
    end;
  finally
    DPK.Free;
  end;
end;

{ TDxComponent }

constructor TDxComponent.Create(ComponentProfile: TDxComponentProfile);
begin
  inherited Create;
  FProfile := ComponentProfile;
  FPackages := TDxPackageList.Create();
  FParentComponents := TDxComponentList.Create(False);
  FSubComponents := TDxComponentList.Create(False);
  FState := dxcsInstall;
end;

destructor TDxComponent.Destroy;
begin
  FPackages.Free;
  FParentComponents.Free;
  FSubComponents.Free;
  inherited;
end;

function TDxComponent.GetExistsPackageCount: Integer;
var
  Package: TDxPackage;
begin
  Result := 0;
  for Package in Packages do if Package.Exists then Inc(Result);
end;

function TDxComponent.IsMissingDependents: Boolean;
var
  Comp: TDxComponent;
begin
  Result := False;
  for Comp in ParentComponents do
    if not (Comp.State in dxcsEditModes) then begin
      Result := True;
      Break;
    end;
end;

procedure TDxComponent.SetState(const Value: TDxComponentState);
var
  Comp: TDxComponent;
begin
  if State = Value then Exit;
  if not (State in dxcsEditModes) then Exit;
  case Value of
    dxcsInstall:    for Comp in ParentComponents do Comp.State := dxcsInstall;
    dxcsNotInstall: for Comp in SubComponents do Comp.State := dxcsNotInstall;
  end;
  FState := Value;
end;

end.
