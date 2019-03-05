{*******************************************************}
{                                                       }
{       DxAutoInstaller Component Factory Classes       }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxComponentFactory;

interface

uses
  SysUtils, Classes, DxComponent, DxInstaller, DxProfile, DxIDE;

type
  TDxComponentFactory = class
  private
    FInstaller: TDxInstaller;
    function CreateComponent(IDE: TDxIDE; ComponentProfile: TDxComponentProfile): TDxComponent;
    procedure CreatePackageList(Component: TDxComponent; IDE: TDxIDE; IsRequiredPackages: Boolean);
  public
    constructor Create(Installer: TDxInstaller);
    property Installer: TDxInstaller read FInstaller;
    procedure BuildComponentList(IDE: TDxIDE; List: TDxComponentList);
  end;


implementation

{ TDxComponentFactory }

constructor TDxComponentFactory.Create(Installer: TDxInstaller);
begin
  inherited Create;
  FInstaller := Installer;
end;

procedure TDxComponentFactory.BuildComponentList(IDE: TDxIDE; List:TDxComponentList);
var
  CompProfile: TDxComponentProfile;
  Comp, C: TDxComponent;
  Package, P: TDxPackage;
begin
  List.Clear;
  for CompProfile in Installer.Profile.Components do List.Add(CreateComponent(IDE, CompProfile));

  for Comp in List do begin
    for Package in Comp.Packages do begin
      // Set Package Dependents;
      for C in List do if C <> Comp then
        for P in C.Packages do
          if Package.Requires.IndexOf(P.Name) >= 0 then begin
            Package.DependentComponents.Add(C);
            // Set Component Dependents;
            if Package.Required then begin
              if Comp.ParentComponents.IndexOf(C) < 0 then begin
                Comp.ParentComponents.Add(C);
                if C.SubComponents.IndexOf(Comp) < 0 then C.SubComponents.Add(Comp);
              end;
            end;
            Break;
          end;
    end;
    // Set Component State;
    if not DirectoryExists(TDxProfile.GetComponentDir(Installer.InstallFileDir, Comp.Profile.ComponentName)) then
      Comp.State := dxcsNotFound
    else if Comp.GetExistsPackageCount = 0 then
      Comp.State := dxcsNotSupported
    else if Comp.IsMissingDependents then
      Comp.State := dxcsMissing;
  end;
end;

function TDxComponentFactory.CreateComponent(IDE: TDxIDE; ComponentProfile: TDxComponentProfile): TDxComponent;
begin
  Result := TDxComponent.Create(ComponentProfile);
  CreatePackageList(Result, IDE, True);
  CreatePackageList(Result, IDE, False);
end;

procedure TDxComponentFactory.CreatePackageList(Component: TDxComponent; IDE: TDxIDE; IsRequiredPackages: Boolean);
var
  List: TStringList;
  PackageName, FileName: String;
  Package: TDxPackage;
begin
  if IsRequiredPackages then List := Component.Profile.RequiredPackages else List := Component.Profile.OptionalPackages;
  for PackageName in List do begin
    FileName := TDxProfile.GetPackageFullFileName(Installer.InstallFileDir, Component.Profile.ComponentName, PackageName, IDE);
    if not FileExists(FileName) then Continue;
    Package := TDxPackage.Create(FileName);
    Package.Required := IsRequiredPackages;
    Component.Packages.Add(Package);
  end;
end;


end.
