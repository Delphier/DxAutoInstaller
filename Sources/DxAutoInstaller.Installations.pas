unit DxAutoInstaller.Installations;

interface

uses
  System.Generics.Collections,
  DxAutoInstaller.Core,
  DxAutoInstaller.DevExpress,
  DxAutoInstaller.Options;

type
  TInstallation = class
  private
    FIDE: TIDE;
    FComponents: TComponents;
    FOptions: TOptions;
  public
    constructor Create(AIDE: TIDE; const ARootDir: TRootDir; AManifest: TManifest);
    destructor Destroy; override;
    property IDE: TIDE read FIDE;
    property Components: TComponents read FComponents;
    property Options: TOptions read FOptions;
  end;

  TInstallations = class(TObjectList<TInstallation>)
  private
    FManifest: TManifest;
  public
    constructor Create(AIDEs: TIDEs; const ARootDir: TRootDir; AManifest: TManifest); overload;
    constructor Create(const ARootDir: TRootDir; AManifest: TManifest); overload;
    property Manifest: TManifest read FManifest;
  end;

implementation

{ TInstallation }

constructor TInstallation.Create(AIDE: TIDE; const ARootDir: TRootDir; AManifest: TManifest);
begin
  FIDE := AIDE;
  FComponents := TComponents.Create(AManifest, ARootDir, AIDE);
  FOptions := TOptions.Create(FIDE);
end;

destructor TInstallation.Destroy;
begin
  FComponents.Free;
  FOptions.Free;
  inherited;
end;

{ TInstallations }

constructor TInstallations.Create(AIDEs: TIDEs; const ARootDir: TRootDir; AManifest: TManifest);
begin
  inherited Create;
  FManifest := AManifest;
  for var IDE in AIDEs do Add(TInstallation.Create(IDE, ARootDir, AManifest));
end;

constructor TInstallations.Create(const ARootDir: TRootDir; AManifest: TManifest);
begin
  Create(TIDEs.All, ARootDir, AManifest);
end;

end.
