object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'MainForm'
  ClientHeight = 854
  ClientWidth = 618
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 618
    Height = 97
    Align = alTop
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    Color = clWhite
    ParentBackground = False
    TabOrder = 0
    ExplicitWidth = 616
    DesignSize = (
      618
      95)
    object ImageLogo: TImage
      Left = 530
      Top = 16
      Width = 64
      Height = 64
      Anchors = [akTop, akRight]
      Stretch = True
      ExplicitLeft = 723
    end
    object LblAppName: TLabel
      Left = 24
      Top = 28
      Width = 167
      Height = 25
      Caption = 'DxAutoInstaller'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label2: TLabel
      Left = 24
      Top = 60
      Width = 257
      Height = 15
      Caption = 'DevExpress VCL Components Automatic Installer'
    end
    object LblVersion: TLabel
      Left = 203
      Top = 37
      Width = 20
      Height = 15
      Caption = 'v2.x'
    end
  end
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 8
    Top = 109
    Width = 602
    Height = 705
    Margins.Left = 8
    Margins.Top = 12
    Margins.Right = 8
    Margins.Bottom = 0
    ActivePage = TabInstall
    Align = alClient
    TabOrder = 1
    OnChange = PageControlChange
    ExplicitWidth = 600
    ExplicitHeight = 697
    object TabInstall: TTabSheet
      Caption = 'Install'
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 675
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 592
        ExplicitHeight = 667
        DesignSize = (
          594
          675)
        object Label7: TLabel
          Left = 8
          Top = 10
          Width = 140
          Height = 15
          Caption = 'DevExpress Root Directory:'
        end
        object Label8: TLabel
          Left = 523
          Top = 10
          Width = 41
          Height = 15
          Anchors = [akTop, akRight]
          Caption = 'Version:'
          ExplicitLeft = 710
        end
        object EditRootDir: TcxButtonEdit
          Left = 8
          Top = 30
          Anchors = [akLeft, akTop, akRight]
          Properties.Buttons = <
            item
              Default = True
              Kind = bkEllipsis
            end>
          Properties.ReadOnly = True
          Properties.OnButtonClick = EditRootDirPropertiesButtonClick
          TabOrder = 0
          ExplicitWidth = 507
          Width = 509
        end
        object EditVersion: TEdit
          Left = 523
          Top = 30
          Width = 61
          Height = 23
          Anchors = [akTop, akRight]
          ReadOnly = True
          TabOrder = 1
          ExplicitLeft = 521
        end
        object PanTreeList: TPanel
          Left = 8
          Top = 62
          Width = 576
          Height = 604
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelKind = bkTile
          BevelOuter = bvNone
          TabOrder = 2
          ExplicitWidth = 574
          ExplicitHeight = 596
        end
      end
    end
    object TabUninstall: TTabSheet
      Caption = 'Uninstall'
      ImageIndex = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 675
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label9: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 10
          Width = 122
          Height = 15
          Margins.Left = 8
          Margins.Top = 10
          Margins.Right = 8
          Margins.Bottom = 10
          Align = alTop
          Caption = 'Select IDEs to Uninstall:'
        end
        object ListViewUninstall: TListView
          AlignWithMargins = True
          Left = 8
          Top = 35
          Width = 578
          Height = 630
          Margins.Left = 8
          Margins.Top = 0
          Margins.Right = 8
          Margins.Bottom = 10
          Align = alClient
          Checkboxes = True
          Columns = <
            item
              AutoSize = True
              Caption = 'IDE Name'
            end>
          ReadOnly = True
          TabOrder = 0
          ViewStyle = vsReport
        end
      end
    end
    object TabTools: TTabSheet
      Caption = 'Tools'
      ImageIndex = 2
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 675
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object GroupBox1: TGroupBox
          AlignWithMargins = True
          Left = 8
          Top = 102
          Width = 578
          Height = 563
          Margins.Left = 8
          Margins.Right = 8
          Margins.Bottom = 10
          Align = alClient
          Caption = 'Manifest'
          TabOrder = 1
          DesignSize = (
            578
            563)
          object LblCurrentManifest: TLabel
            Left = 16
            Top = 32
            Width = 102
            Height = 15
            Caption = 'LblCurrentManifest'
          end
          object LblCustomManifestNote: TLabel
            Left = 16
            Top = 109
            Width = 246
            Height = 15
            Caption = 'Note: Restart the application to apply changes.'
          end
          object BtnManifest: TcxButton
            Left = 16
            Top = 55
            Width = 225
            Height = 25
            Caption = 'BtnManifest'
            TabOrder = 0
          end
          object LblCustomManifest: TLinkLabel
            Left = 16
            Top = 89
            Width = 553
            Height = 19
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            Caption = 'LblCustomManifest'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            OnLinkClick = LblCustomManifestLinkClick
          end
        end
        object GroupBox2: TGroupBox
          AlignWithMargins = True
          Left = 8
          Top = 10
          Width = 578
          Height = 79
          Margins.Left = 8
          Margins.Top = 10
          Margins.Right = 8
          Margins.Bottom = 10
          Align = alTop
          Caption = 'Packages'
          TabOrder = 0
          object Button1: TcxButton
            Left = 16
            Top = 32
            Width = 225
            Height = 25
            Action = ActSearchNewPackages
            TabOrder = 0
          end
        end
      end
    end
    object TabAbout: TTabSheet
      Caption = 'About'
      ImageIndex = 3
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 594
        Height = 675
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          594
          675)
        object Label4: TLabel
          Left = 8
          Top = 21
          Width = 160
          Height = 15
          Caption = 'GitHub Downloads and Issues:'
        end
        object Label5: TLabel
          Left = 8
          Top = 119
          Width = 143
          Height = 15
          Caption = 'Feedback and Bug Reports:'
        end
        object Label6: TLabel
          Left = 8
          Top = 69
          Width = 171
          Height = 15
          Caption = 'DevExpress VCL Documentation:'
        end
        object LinkGitHub: TLinkLabel
          Left = 8
          Top = 42
          Width = 240
          Height = 19
          Caption = 'https://github.com/Delphier/DxAutoInstaller'
          TabOrder = 0
          OnLinkClick = LinkClick
        end
        object LinkDevExpressDocs: TLinkLabel
          Left = 8
          Top = 89
          Width = 187
          Height = 19
          Caption = 'https://docs.devexpress.com/VCL/'
          TabOrder = 1
          OnLinkClick = LinkClick
        end
        object LinkEmail: TLinkLabel
          Left = 8
          Top = 138
          Width = 110
          Height = 19
          Caption = 'faceker@gmail.com'
          TabOrder = 2
          OnLinkClick = LinkClick
        end
        object MemoChangelog: TMemo
          Left = 8
          Top = 172
          Width = 576
          Height = 493
          Anchors = [akLeft, akTop, akRight, akBottom]
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 3
        end
      end
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 814
    Width = 618
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 806
    ExplicitWidth = 616
    DesignSize = (
      618
      40)
    object BtnExit: TcxButton
      Left = 511
      Top = 7
      Width = 85
      Height = 25
      Action = ActExit
      Anchors = [akTop, akRight]
      TabOrder = 2
      ExplicitLeft = 509
    end
    object BtnExecute: TcxButton
      Left = 420
      Top = 7
      Width = 85
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'BtnExecute'
      TabOrder = 1
      ExplicitLeft = 418
    end
    object ChkShowAllComponents: TCheckBox
      Left = 20
      Top = 11
      Width = 167
      Height = 17
      Caption = 'Show All Components'
      TabOrder = 0
      OnClick = ChkShowAllComponentsClick
    end
  end
  object ActionList1: TActionList
    Images = DMResources.cxImageList1
    Left = 400
    Top = 288
    object ActInstall: TAction
      Caption = '&Install'
      ImageIndex = 0
      OnExecute = ActInstallExecute
    end
    object ActUninstall: TAction
      Caption = '&Uninstall'
      ImageIndex = 1
      OnExecute = ActUninstallExecute
    end
    object ActExit: TAction
      Caption = 'E&xit'
      ImageIndex = 2
      OnExecute = ActExitExecute
    end
    object ActSearchNewPackages: TAction
      Caption = 'Search New Packages'
      ImageIndex = 3
      OnExecute = ActSearchNewPackagesExecute
    end
    object ActManifestExport: TAction
      Caption = 'Export Built-in Manifest'
      ImageIndex = 4
      OnExecute = ActManifestExportExecute
    end
    object ActManifestDelete: TAction
      Caption = 'Delete Custom Manifest'
      ImageIndex = 5
      OnExecute = ActManifestDeleteExecute
    end
  end
end
