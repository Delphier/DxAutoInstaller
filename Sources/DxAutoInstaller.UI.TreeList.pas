unit DxAutoInstaller.UI.TreeList;

interface

uses
  System.Classes,
  Vcl.Controls,
  cxTL, cxGraphics, cxEdit,
  DxAutoInstaller.DevExpress,
  DxAutoInstaller.Options,
  DxAutoInstaller.Installations;

type
  TTreeList = class
  private
    FTreeList: TcxTreeList;
    FInstallations: TInstallations;
    FShowAllComponents: Boolean;
    procedure SetInstallations(AValue: TInstallations);
    procedure SetShowAllComponents(const AValue: Boolean);

    procedure CreateHeaders;
    procedure CreateNodes;
    procedure Refresh; overload;
    procedure Refresh(AColumn: TcxTreeListColumn); overload;
    procedure RefreshColumn(const AIndex: NativeInt);

    procedure TreeListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
    procedure TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    procedure TreeListColumnGetEditProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
  protected
    FComponentsNode: TcxTreeListNode;
    FOptionsNode: TcxTreeListNode;
    procedure SetComponentNodeVisible(ANode: TcxTreeListNode);
  public
    constructor Create(AParent: TWinControl);
    destructor Destroy; override;
    property Installations: TInstallations read FInstallations write SetInstallations;
    property ShowAllComponents: Boolean read FShowAllComponents write SetShowAllComponents;
  end;

  TCell = record
    TreeList: TTreeList;
    Node: TcxTreeListNode;
    Column: TcxTreeListColumn;
  public
    constructor Create(ATreeList: TTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
    function Installation: TInstallation;
    function Component: DxAutoInstaller.DevExpress.TComponent;
    function Option: TOption;
    function IsComponents: Boolean;
    function IsComponent: Boolean;
    function IsOption: Boolean;
    function IsValidComponent: Boolean;
    function IsValidOption: Boolean;
    function IsError: Boolean;
  end;

implementation

uses
  System.Types, System.Variants, System.SysUtils, Vcl.Graphics, Vcl.Forms,
  cxStyles, cxImageComboBox,
  DxAutoInstaller.Core, DxAutoInstaller.Resources;

{ TTreeList }

constructor TTreeList.Create(AParent: TWinControl);
begin
  FTreeList := TcxTreeList.Create(nil);
  FTreeList.Parent := AParent;
  FTreeList.Align := alClient;
  FTreeList.OptionsView.ColumnAutoWidth := True;
  FTreeList.OptionsCustomizing.ColumnMoving := False;
  FTreeList.OptionsCustomizing.ColumnVertSizing := False;
  FTreeList.OptionsData.Deleting := False;
  FTreeList.OptionsBehavior.Sorting := False;
  FTreeList.OptionsBehavior.HotTrack := True;
  FTreeList.OptionsSelection.HideSelection := True;
  FTreeList.Styles.HotTrack := TcxStyle.Create(FTreeList);
  FTreeList.Styles.HotTrack.TextColor := clBlue;
  FTreeList.Styles.Selection := TcxStyle.Create(FTreeList);
  FTreeList.Styles.Selection.Color := clWindow;
  FTreeList.Styles.Selection.TextColor := clWindowText;
  FTreeList.OnMouseMove := TreeListMouseMove;
  FTreeList.OnCustomDrawDataCell := TreeListCustomDrawDataCell;
  FTreeList.OnEditing := TreeListEditing;
  FTreeList.OnEditValueChanged := TreeListEditValueChanged;
end;

destructor TTreeList.Destroy;
begin
  FTreeList.Free;
  inherited;
end;

procedure TTreeList.SetInstallations(AValue: TInstallations);
begin
  FInstallations := AValue;
  CreateNodes;
end;

procedure TTreeList.SetShowAllComponents(const AValue: Boolean);
begin
  FShowAllComponents := AValue;
  if not Assigned(Installations) then Exit;

  FTreeList.BeginUpdate;
  try
    for var I := 0 to FComponentsNode.Count - 1 do SetComponentNodeVisible(FComponentsNode[I]);
    Refresh;
  finally
    FTreeList.EndUpdate;
  end;
end;

procedure TTreeList.SetComponentNodeVisible(ANode: TcxTreeListNode);
begin
  ANode.Visible := ShowAllComponents or Installations.Manifest.Components[ANode.Index].Visible;
end;

procedure TTreeList.CreateHeaders;
var
  Column: TcxTreeListColumn;
begin
  FTreeList.DeleteAllColumns;
  Column := FTreeList.CreateColumn;
  Column.Tag := -1;
  Column.Caption.Text := 'Name';
  Column.Width := 300;
  Column.Options.Editing := False;
  Column.Options.Sizing := False;

  for var I := 0 to Installations.Count - 1 do begin
    Column := FTreeList.CreateColumn;
    Column.Tag := I;
    Column.Caption.AlignHorz := taCenter;
    Column.OnGetEditProperties := TreeListColumnGetEditProperties;
  end;
end;

procedure TTreeList.CreateNodes;
var
  Node: TcxTreeListNode;
begin
  FTreeList.BeginUpdate;
  try
    FTreeList.Clear;
    CreateHeaders;

    FComponentsNode := FTreeList.Add;
    for var I := 0 to High(Installations.Manifest.Components) do begin
      Node := FComponentsNode.AddChild;
      Node.Texts[0] := Installations.Manifest.Components[I].Name;
      SetComponentNodeVisible(Node);
    end;

    FOptionsNode := FTreeList.Add;
    for var I := Low(TOptions.Classes) to High(TOptions.Classes) do begin
      Node := FOptionsNode.AddChild;
      Node.Texts[0] := TOptions.Classes[I].Name;
      for var J := 1 to Installations.Count do begin
        var Option := Installations[J-1].Options[I];
        Node.Values[J] := if Option.Valid then Option.Value else Variant(Option.Error);
      end;
    end;

    Refresh;
    FTreeList.FullExpand;
  finally
    FTreeList.EndUpdate;
  end;
end;

procedure TTreeList.Refresh;
begin
  for var I := 0 to FTreeList.ColumnCount - 1 do RefreshColumn(I);
end;

procedure TTreeList.RefreshColumn(const AIndex: NativeInt);
begin
  Refresh(FTreeList.Columns[AIndex]);
end;

procedure TTreeList.Refresh(AColumn: TcxTreeListColumn);
begin
  if AColumn.Tag < 0 then begin
    FComponentsNode.Texts[0] := Format('Components (%d)', [FComponentsNode.ChildVisibleCount]);
    FOptionsNode.Texts[0] := Format('Options (%d)', [FOptionsNode.ChildVisibleCount]);
  end else begin
    var Installation := Installations[AColumn.Tag];
    var Components := Installation.Components;
    var Count := if ShowAllComponents then Components.ValidCount else Components.VisibleValidCount;
    var CheckedCount := if ShowAllComponents then Components.CheckedCount else Components.VisibleCheckedCount;

    AColumn.Caption.Text := Format('%s (%d)', [Installation.IDE.Name, CheckedCount]);
    AColumn.Values[FComponentsNode] := if CheckedCount = 0 then Variant(False) else if CheckedCount = Count then Variant(True) else Null;

    for var I := 0 to FComponentsNode.Count - 1 do begin
      var Node := FComponentsNode[I];
      var Component := Components[Node.Index];
      AColumn.Values[Node] := if Component.Valid then Variant(Component.Checked) else Variant(Component.Error);
    end;
  end;
end;

procedure TTreeList.TreeListMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  var Pos := Point(X, Y);
  var HitTest := FTreeList.HitTest;
  HitTest.HitPoint := Pos;

  if HitTest.HitAtNode and HitTest.HitAtColumn then
    if HitTest.EditCellViewInfo.EditViewInfo.EditProperties = DMResources.ErrorEditor.Properties then begin
      var ImageRect := (HitTest.EditCellViewInfo.EditViewInfo as TcxImageComboBoxViewInfo).ImageRect;
      var Node := HitTest.HitNode;
      var Column := HitTest.HitColumn;
      ImageRect.Offset(FTreeList.CellRect(Node, Column).TopLeft);
      if ImageRect.Contains(Pos) then begin
        FTreeList.Hint := ErrorMessages[TError(Column.Values[Node])];
        FTreeList.ShowHint := True;
        Application.ActivateHint(FTreeList.ClientToScreen(Pos));
        Exit;
      end;
    end;

  FTreeList.ShowHint := False;
end;

procedure TTreeList.TreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if AViewInfo.Node.HasChildren then
    ACanvas.Font.Style := [fsBold]
  else if (AViewInfo.Node.Parent = FComponentsNode) and not Installations.Manifest.Components[AViewInfo.Node.Index].Visible then
    ACanvas.Font.Color := clGrayText;
end;

procedure TTreeList.TreeListEditing(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; var Allow: Boolean);
begin
  var Cell := TCell.Create(Self, Sender.FocusedNode, AColumn);
  Allow := Cell.IsComponents or Cell.IsValidComponent or Cell.IsValidOption;
end;

procedure TTreeList.TreeListEditValueChanged(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  var Cell := TCell.Create(Self, Sender.FocusedNode, AColumn);
  var Value := Sender.InplaceEditor.EditValue;

  if Cell.IsComponents then
    Cell.Installation.Components.CheckAll(Value)
  else if Cell.IsComponent then
    Cell.Component.Checked := Value
  else if Cell.IsOption then
    Cell.Option.Value := Value;

  FTreeList.BeginUpdate;
  try
    Refresh(AColumn);
  finally
    FTreeList.EndUpdate;
  end;
end;

procedure TTreeList.TreeListColumnGetEditProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
begin
  var Cell := TCell.Create(Self, ANode, Sender);
  if Cell.IsComponents then
    EditProperties := DMResources.CheckBoxEditor.Properties
  else if Cell.IsComponent then
    EditProperties := if Cell.Component.Valid then DMResources.CheckBoxEditor.Properties else DMResources.ErrorEditor.Properties
  else if Cell.IsOption then
    EditProperties := Cell.Option.EditProperties
  else
    EditProperties := nil;
end;

{ TCell }

constructor TCell.Create(ATreeList: TTreeList; ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
begin
  TreeList := ATreeList;
  Node := ANode;
  Column := AColumn;
end;

function TCell.Installation: TInstallation;
begin
  Result := TreeList.Installations[Column.Tag];
end;

function TCell.Component: DxAutoInstaller.DevExpress.TComponent;
begin
  Result := Installation.Components[Node.Index];
end;

function TCell.Option: TOption;
begin
  Result := Installation.Options[Node.Index];
end;

function TCell.IsComponents: Boolean;
begin
  Result := Node = TreeList.FComponentsNode;
end;

function TCell.IsComponent: Boolean;
begin
  Result := Node.Parent = TreeList.FComponentsNode;
end;

function TCell.IsOption: Boolean;
begin
  Result := Node.Parent = TreeList.FOptionsNode;
end;

function TCell.IsValidComponent: Boolean;
begin
  Result := IsComponent and Component.Valid;
end;

function TCell.IsValidOption: Boolean;
begin
  Result := IsOption and Option.Valid;
end;

function TCell.IsError: Boolean;
begin
  Result := (IsComponent and not Component.Valid) or (IsOption and not Option.Valid);
end;

end.
