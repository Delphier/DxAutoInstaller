unit DxAutoInstaller.Resources;

interface

uses
  System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls,
  cxClasses, cxGraphics, cxImageList, cxEdit, cxEditRepositoryItems, cxExtEditRepositoryItems;

type
  TDMResources = class(TDataModule)
    cxEditRepository1: TcxEditRepository;
    CheckBoxEditor: TcxEditRepositoryCheckBoxItem;
    ErrorEditor: TcxEditRepositoryImageComboBoxItem;
    cxImageList1: TcxImageList;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMResources: TDMResources;

implementation

uses
  DxAutoInstaller.Core;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDMResources.DataModuleCreate(Sender: TObject);
begin
  for var I := Low(TError) to High(TError) do begin
    var Item := ErrorEditor.Properties.Items.Add;
    Item.Value := I;
    Item.ImageIndex := ErrorImageIndexes[I];
  end;
end;

end.
