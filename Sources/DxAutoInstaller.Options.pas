{*******************************************************}
{                                                       }
{            DxAutoInstaller Options Library            }
{                                                       }
{      https://github.com/Delphier/DxAutoInstaller      }
{                                                       }
{      Copyright(c) 2014-2026 faceker@gmail.com         }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit DxAutoInstaller.Options;

interface

uses
  cxEdit,
  System.Generics.Collections,
  DxAutoInstaller.Core;

type
  TOptionValue = Variant;
  TOptionValueHelper = record helper for TOptionValue
    function ToPlatforms: TPlatforms;
  end;

  TOption = class abstract
  private
    FIDE: TIDE;
    FValue: TOptionValue;
    FEditProperties: TcxCustomEditProperties;
    FOwnsEditProperties: Boolean;
    FError: TError;
    procedure SetValue(const AValue: TOptionValue);
  protected
    procedure Init; virtual;
  public
    constructor Create(AIDE: TIDE);
    destructor Destroy; override;
    class function Name: string; virtual; abstract;
    property Value: TOptionValue read FValue write SetValue;
    property EditProperties: TcxCustomEditProperties read FEditProperties;
    property Error: TError read FError;
    function Valid: Boolean;
  end;

  TOptionClass = class of TOption;

  TOptionPlatforms = class abstract(TOption)
  private
    procedure DoInit(APersonalityInstalled: Boolean; const APlatforms, ADefaults: TPlatforms; const AError: TError);
  end;

  TOptionDelphiPlatforms = class(TOptionPlatforms)
  protected
    procedure Init; override;
  public
    class function Name: string; override;
  end;

  TOptionCppBuilderPlatforms = class(TOptionPlatforms)
  protected
    procedure Init; override;
  public
    class function Name: string; override;
  end;

  TOptionAddBrowsingPath = class(TOption)
  public
    class function Name: string; override;
  end;

  TOptionUseNativeLookAndFeel = class(TOption)
  public
    class function Name: string; override;
  end;

  TOptions = class(TObjectList<TOption>)
  const
    Classes: array[0..3] of TOptionClass = (TOptionDelphiPlatforms,
                                            TOptionCppBuilderPlatforms,
                                            TOptionAddBrowsingPath,
                                            TOptionUseNativeLookAndFeel);
  private
    FIDE: TIDE;
    function Find(AClass: TOptionClass): TOption;
  public
    constructor Create(AIDE: TIDE);
    function DelphiPlatforms: TPlatforms;
    function CppBuilderPlatforms: TPlatforms;
    function AddBrowsingPath: Boolean;
    function UseNativeLookAndFeel: Boolean;
  end;

implementation

uses
  cxCheckComboBox,
  System.Classes,
  System.Variants,
  DxAutoInstaller.Resources;

{ TOption }

constructor TOption.Create(AIDE: TIDE);
begin
  FIDE := AIDE;
  Init;
end;

destructor TOption.Destroy;
begin
  if FOwnsEditProperties then FEditProperties.Free;
  inherited;
end;

procedure TOption.Init;
begin
  FValue := True;
  FEditProperties := DMResources.CheckBoxEditor.Properties;
end;

function TOption.Valid: Boolean;
begin
  Result := Error = errNone;
end;

procedure TOption.SetValue(const AValue: TOptionValue);
begin
  if Valid then FValue := AValue;
end;

{ TOptionPlatforms }

procedure TOptionPlatforms.DoInit(APersonalityInstalled: Boolean; const APlatforms, ADefaults: TPlatforms; const AError: TError);
begin
  if APersonalityInstalled then begin
    FValue := Byte(ADefaults * APlatforms * FIDE.SupportedPlatforms);

    var Properties := TcxCheckComboBoxProperties.Create(nil);
    FEditProperties := Properties;
    FOwnsEditProperties := True;

    Properties.Alignment.Horz := taCenter;
    Properties.Delimiter := ',';
    Properties.EmptySelectionText := '<None Selected>';
    for var I in APlatforms do begin
      var Item := Properties.Items.Add;
      Item.Description := PlatformDescriptions[I];
      Item.ShortDescription := PlatformNames[I];
      Item.Enabled := I in FIDE.SupportedPlatforms;
    end;
  end else begin
    FValue := 0;
    FEditProperties := DMResources.ErrorEditor.Properties;
    FError := AError;
  end;
end;

{ TOptionDelphiPlatforms }

class function TOptionDelphiPlatforms.Name: string;
begin
  Result := 'Install to Delphi';
end;

procedure TOptionDelphiPlatforms.Init;
begin
  DoInit(FIDE.DelphiInstalled, DelphiPlatforms, DelphiPlatformsDefault, errDelphiNotInstalled);
end;

{ TOptionCppBuilderPlatforms }

class function TOptionCppBuilderPlatforms.Name: string;
begin
  Result := 'Install to C++Builder';
end;

procedure TOptionCppBuilderPlatforms.Init;
begin
  DoInit(FIDE.CppBuilderInstalled, CppBuilderPlatforms, CppBuilderPlatformsDefault, errCppBuilderNotInstalled);
end;

{ TOptionAddBrowsingPath }

class function TOptionAddBrowsingPath.Name: string;
begin
  Result := 'Add Browsing Path';
end;

{ TOptionUseNativeLookAndFeel }

class function TOptionUseNativeLookAndFeel.Name: string;
begin
  Result := 'Use Native Look and Feel as Default';
end;

{ TOptions }

constructor TOptions.Create(AIDE: TIDE);
begin
  inherited Create;
  FIDE := AIDE;
  for var OptionClass in Classes do Add(OptionClass.Create(FIDE));
end;

function TOptions.Find(AClass: TOptionClass): TOption;
begin
  Result := nil;
  for var Item in Self do if Item is AClass then Exit(Item);
end;

function TOptions.DelphiPlatforms: TPlatforms;
begin
  Result := Find(TOptionDelphiPlatforms).Value.ToPlatforms;
end;

function TOptions.CppBuilderPlatforms: TPlatforms;
begin
  Result := Find(TOptionCppBuilderPlatforms).Value.ToPlatforms;
end;

function TOptions.AddBrowsingPath: Boolean;
begin
  Result := Find(TOptionAddBrowsingPath).Value;
end;

function TOptions.UseNativeLookAndFeel: Boolean;
begin
  Result := Find(TOptionUseNativeLookAndFeel).Value;
end;

{ TOptionValueHelper }

function TOptionValueHelper.ToPlatforms: TPlatforms;
begin
  Result := TPlatforms(Byte(Self));
end;

end.
