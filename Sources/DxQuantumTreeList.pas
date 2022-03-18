{*******************************************************}
{                                                       }
{        DxAutoInstaller QuantumTreeList Classes        }
{                                                       }
{        http://www.delphier.com/DxAutoIntaller         }
{        Copyright(c) 2014 by faceker@gmail.com         }
{                                                       }
{*******************************************************}

unit DxQuantumTreeList;

interface

uses
  SysUtils, Classes, DxInstaller, DxIDE, Controls, cxTL, cxEdit, cxGraphics,
  cxExtEditRepositoryItems, cxEditRepositoryItems, cxStyles, cxClasses,
  dxScreenTip, dxCustomHint, cxHint, Windows;

type
  TDxQuantumTreeList = class;
  TDxResourceModule = class(TDataModule)
    EditRepository: TcxEditRepository;
    CheckBoxItem: TcxEditRepositoryCheckBoxItem;
    MissingItem: TcxEditRepositoryCheckBoxItem;
    NotFoundItem: TcxEditRepositoryCheckBoxItem;
    NotSupportedItem: TcxEditRepositoryCheckBoxItem;
    HintStyleController: TcxHintStyleController;
    procedure HintStyleControllerShowHintEx(Sender: TObject; var Caption,
      HintStr: string; var CanShow: Boolean; var HintInfo: THintInfo);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FTreeList: TDxQuantumTreeList;
  public
    { Public declarations }
  end;

  TDxQuantumTreeList = class
  private
    FResources: TDxResourceModule;
    FInstaller: TDxInstaller;
    FTreeList: TcxTreeList;
    FComponentsNode: TcxTreeListNode;
    FOptionsNode: TcxTreeListNode;
    procedure InitialHeader();
    procedure RefreshIDEColumn(Column: TcxTreeListColumn; ParentNode: TcxTreeListNode);
    property Resources: TDxResourceModule read FResources;
    property Installer: TDxInstaller read FInstaller;
    property TreeList: TcxTreeList read FTreeList;

    procedure TreeListColumnGetEditProperties(Sender: TcxTreeListColumn;
      ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    procedure TreeListCustomDrawDataCell(Sender: TcxCustomTreeList;
      ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    procedure TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
  public
    constructor Create(Installer: TDxInstaller; Parent: TWinControl);
    destructor Destroy; override;
    procedure DispData(const IsHideBaseComponents: Boolean);
    function GetSelectedIDEs(): TDxIDEArray;
  end;

implementation

{$R *.dfm}

uses
  DxProfile, DxComponent, Variants, Graphics, Forms;

{ TDxResourceModule }

procedure TDxResourceModule.DataModuleCreate(Sender: TObject);
begin
  FTreeList := nil;
end;

procedure TDxResourceModule.HintStyleControllerShowHintEx(Sender: TObject;
  var Caption, HintStr: string; var CanShow: Boolean; var HintInfo: Controls.THintInfo);
var
  Node: TcxTreeListNode;
  Column: TcxTreeListColumn;
  Comp: TDxComponent;
  HintStyle: TcxHintStyle;
  Hint: String;
begin
  CanShow := False;
  if FTreeList = nil then Exit;
  if FTreeList.TreeList <> HintInfo.HintControl then Exit;
  Node := FTreeList.TreeList.HitTest.HitNode;
  Column := FTreeList.TreeList.HitTest.HitColumn;
  if Node = nil then Exit;
  if Column = nil then Exit;
  if Column.Tag < 0 then Exit;
  if Column.Values[Node] <> Null then Exit;

  HintStyle := TcxHintStyle(HintStyleController.HintStyle);
  HintStyle.Font.Assign(Application.MainForm.Font);
  HintStyle.CaptionFont.Assign(HintStyle.Font);
  HintStyle.CaptionFont.Style := [fsBold];

  Hint := EmptyStr;
  if Node.Parent = FTreeList.FComponentsNode then begin
    Comp := FTreeList.Installer.Components[FTreeList.Installer.IDEs[Column.Tag]][Node.Index];
    case Comp.State of
      dxcsNotFound:     Hint := 'Not Found|Component installation file is not found.';
      dxcsNotSupported: Hint := 'Not Supported|Component does not support the current IDE.';
      dxcsMissing:      Hint := 'Missing Components|Dependent components are missing.';
    end;
  end else if Node.Parent = FTreeList.FOptionsNode then
    Hint := 'Not Supported|Current IDE does not support this option.';

  if Hint <> EmptyStr then begin
    Caption := GetShortHint(Hint);
    HintStr := GetLongHint(Hint);
    CanShow := True;
  end;
end;


{ TDxQuantumTreeList }

constructor TDxQuantumTreeList.Create(Installer: TDxInstaller; Parent: TWinControl);
var
  Style: TcxStyle;
begin
  inherited Create;
  FResources := TDxResourceModule.Create(nil);
  FResources.FTreeList := Self;
  FInstaller := Installer;

  FTreeList := TcxTreeList.Create(nil);
  FTreeList.Parent := Parent;
  FTreeList.Align := alClient;
  FTreeList.ShowHint := True;
  FTreeList.OptionsBehavior.Sorting := False;
  FTreeList.OptionsBehavior.ExpandOnDblClick := False;
  FTreeList.OptionsBehavior.HotTrack := True;
  FTreeList.OptionsCustomizing.ColumnVertSizing := False;
  FTreeList.OptionsCustomizing.ColumnMoving := False;
  FTreeList.OptionsData.Deleting := False;
  FTreeList.OptionsView.ColumnAutoWidth := True;
  FTreeList.OptionsSelection.HideSelection := True;

  Style := TcxStyle.Create(FResources);
  Style.Color := clWindow;
  Style.TextColor := FTreeList.Font.Color;
  FTreeList.Styles.Selection := Style;

  Style := TcxStyle.Create(FResources);
  Style.TextColor := clBlue;
  FTreeList.Styles.HotTrack := Style;

  FTreeList.OnCustomDrawDataCell := TreeListCustomDrawDataCell;
  FTreeList.OnEditValueChanged := TreeListEditValueChanged;
  FTreeList.OnEditing := TreeListEditing;
end;

destructor TDxQuantumTreeList.Destroy;
begin
  FTreeList.Free;
  FResources.Free;
  inherited;
end;

procedure TDxQuantumTreeList.InitialHeader;
var
  Column: TcxTreeListColumn;
  I: Integer;
begin
  Column := TreeList.CreateColumn();
  Column.Caption.Text := 'Name';
  Column.Tag := -1;
  Column.Width := 200;
  Column.Options.Editing := False;

  for I := 0 to Installer.IDEs.Count - 1 do begin
    Column := TreeList.CreateColumn();
    Column.Caption.AlignHorz := taCenter;
    Column.Tag := I;
    Column.Width := 150;
    Column.OnGetEditProperties := TreeListColumnGetEditProperties;
  end;
end;

procedure TDxQuantumTreeList.DispData(const IsHideBaseComponents: Boolean);
var
  I: Integer;
  Option: TDxInstallOption;
begin
  if Installer.InstallFileDir = EmptyStr then Exit;
  if TreeList.ColumnCount = 0 then InitialHeader;
  TreeList.Clear;
  FComponentsNode := TreeList.Add;
  FOptionsNode := TreeList.Add;

  // Display Components;
  for I := 0 to Installer.Profile.Components.Count - 1 do
    with FComponentsNode.AddChild do begin
      Texts[0] := Installer.Profile.Components[I].ComponentName;
      if IsHideBaseComponents and Installer.Profile.Components[I].IsBase then Visible := False;
    end;

  FComponentsNode.Texts[0] := Format('Components (%d)', [FComponentsNode.ChildVisibleCount]);
  FComponentsNode.Expanded := True;

  // Display Options;
  for Option := Low(TDxInstallOption) to High(TDxInstallOption) do
    with FOptionsNode.AddChild do Texts[0] := DxInstallOptionNames[Option];

  FOptionsNode.Texts[0] := Format('Options (%d)', [FOptionsNode.ChildVisibleCount]);;
  FOptionsNode.Expanded := True;

  for I := 0 to TreeList.ColumnCount - 1 do begin
    RefreshIDEColumn(TreeList.Columns[I], FComponentsNode);
    RefreshIDEColumn(TreeList.Columns[I], FOptionsNode);
  end;
end;

function TDxQuantumTreeList.GetSelectedIDEs(): TDxIDEArray;
var
  I, N, Tag: Integer;
  Node: TcxTreeListNode;
begin
  for I := 0 to TreeList.ColumnCount - 1 do begin
    Tag := TreeList.Columns[I].Tag;
    if Tag < 0 then Continue;
    for N := 0 to FComponentsNode.Count - 1 do begin
      Node := FComponentsNode[N];
      if Node.Visible and (TreeList.Columns[I].Values[Node] = True) then begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := Installer.IDEs[Tag];
        Break;
      end;
    end;
  end;
end;

procedure TDxQuantumTreeList.RefreshIDEColumn(Column: TcxTreeListColumn; ParentNode: TcxTreeListNode);
var
  Node: TcxTreeListNode;
  Comp: TDxComponent;
  I, TotalCount, SelectedCount: Integer;
begin
  if Column.Tag < 0 then Exit;

  TotalCount := 0;
  SelectedCount := 0;
  for I := 0 to ParentNode.Count - 1 do begin
    Node := ParentNode.Items[I];
    if ParentNode = FComponentsNode then begin
      Comp := Installer.Components[Installer.IDEs[Column.Tag]].Items[Node.Index];
      case Comp.State of
        dxcsInstall:     Column.Values[Node] := True;
        dxcsNotInstall:  Column.Values[Node] := False;
        else             Column.Values[Node] := Null;
      end;
    end else if ParentNode = FOptionsNode then begin
      if ( (TDxInstallOption(Node.Index) = dxioCompileWin64Library) and not IsSupportWin64(Installer.IDEs[Column.Tag]) ) or
         ( (TDxInstallOption(Node.Index) = dxioInstallToCppBuilder) and not IsSupportCppBuilder(Installer.IDEs[Column.Tag]) ) then
        Column.Values[Node] := Null
      else if TDxInstallOption(Node.Index) in Installer.Options[Installer.IDEs[Column.Tag]] then
        Column.Values[Node] := True
      else
        Column.Values[Node] := False;
    end;
    if Node.Visible then begin
      if Column.Values[Node] <> Null then Inc(TotalCount);
      if Column.Values[Node] = True then Inc(SelectedCount);
    end;
  end;

  if ParentNode = FComponentsNode then
    Column.Caption.Text := Format('%s (%d)', [Installer.IDEs[Column.Tag].Name, SelectedCount]);

  if SelectedCount = 0 then Column.Values[ParentNode] := False
  else if TotalCount = SelectedCount then Column.Values[ParentNode] := True
  else Column.Values[ParentNode] := Null;
end;

procedure TDxQuantumTreeList.TreeListColumnGetEditProperties(Sender: TcxTreeListColumn;
  ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
begin
  if ANode.Parent = FComponentsNode then
    case Installer.Components[Installer.IDEs[Sender.Tag]][ANode.Index].State of
      dxcsNotFound:     EditProperties := Resources.NotFoundItem.Properties;
      dxcsNotSupported: EditProperties := Resources.NotSupportedItem.Properties;
      dxcsMissing:      EditProperties := Resources.MissingItem.Properties;
      else              EditProperties := Resources.CheckBoxItem.Properties;
    end
  else if (ANode.Parent = FOptionsNode) and (Sender.Values[ANode] = Null) then
    EditProperties := Resources.NotSupportedItem.Properties
  else
    EditProperties := Resources.CheckBoxItem.Properties;
end;

procedure TDxQuantumTreeList.TreeListCustomDrawDataCell(Sender: TcxCustomTreeList;
  ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if (AViewInfo.Node = FComponentsNode) or (AViewInfo.Node = FOptionsNode) then
    ACanvas.Font.Style := [fsBold];
end;

procedure TDxQuantumTreeList.TreeListEditing(Sender: TcxCustomTreeList;
  AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  Allow := (Sender.FocusedNode.Count <> 0) or (AColumn.Values[Sender.FocusedNode] <> Null);
end;

procedure TDxQuantumTreeList.TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
var
  Node: TcxTreeListNode;
  Checked: Boolean;
  IDE: TDxIDE;
  Options: TDxInstallOptions;
  I: Integer;
  procedure SetComponentNodeState(Node: TcxTreeListNode);
  begin
    if Checked then Installer.Components[IDE][Node.Index].State := dxcsInstall
               else Installer.Components[IDE][Node.Index].State := dxcsNotInstall;
  end;
  procedure SetOptionNodeState(Node: TcxTreeListNode);
  begin
  if AColumn.Values[Node] = Null then Exit;
  Options := Installer.Options[IDE];
  if Checked then Include(Options, TDxInstallOption(Node.Index))
             else exclude(Options, TDxInstallOption(Node.Index));
  Installer.Options[IDE] := Options;
  end;
begin
  Node := Sender.FocusedNode;
  Checked := AColumn.Values[Node] = True;
  IDE := Installer.IDEs[AColumn.Tag];
  if Node.Parent = FComponentsNode then SetComponentNodeState(Node)
  else if Node.Parent = FOptionsNode then SetOptionNodeState(Node)
  else if Node = FComponentsNode then for I := 0 to Node.Count - 1 do SetComponentNodeState(Node[I])
  else if Node = FOptionsNode then for I := 0 to Node.Count - 1 do SetOptionNodeState(Node[I]);

  if (Node = FComponentsNode) or (Node.Parent = FComponentsNode) then RefreshIDEColumn(AColumn, FComponentsNode);
  if (Node = FOptionsNode) or (Node.Parent = FOptionsNode) then RefreshIDEColumn(AColumn, FOptionsNode);
end;

end.
