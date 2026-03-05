object ProgressForm: TProgressForm
  Left = 0
  Top = 0
  Caption = 'ProgressForm'
  ClientHeight = 470
  ClientWidth = 891
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  ShowInTaskBar = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  TextHeight = 15
  object dxLayoutControl1: TdxLayoutControl
    Left = 0
    Top = 0
    Width = 891
    Height = 470
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 869
    ExplicitHeight = 462
    object ProgressBar: TcxProgressBar
      Left = 12
      Top = 12
      Style.TransparentBorder = False
      TabOrder = 0
      Width = 867
    end
    object BtnOperation: TcxButton
      Left = 804
      Top = 433
      Width = 75
      Height = 25
      Action = ActAbort
      TabOrder = 2
    end
    object Log: TcxMemo
      Left = 12
      Top = 42
      Properties.ReadOnly = True
      Properties.ScrollBars = ssVertical
      Style.HotTrack = False
      Style.TransparentBorder = False
      TabOrder = 1
      Height = 384
      Width = 867
    end
    object dxLayoutControl1Group_Root: TdxLayoutGroup
      AlignHorz = ahClient
      AlignVert = avClient
      Hidden = True
      ItemIndex = 2
      ShowBorder = False
      Index = -1
    end
    object dxLayoutItem1: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      CaptionOptions.Text = 'cxProgressBar1'
      CaptionOptions.Visible = False
      Control = ProgressBar
      ControlOptions.OriginalHeight = 23
      ControlOptions.OriginalWidth = 121
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutGroup1: TdxLayoutGroup
      Parent = dxLayoutControl1Group_Root
      AlignHorz = ahRight
      CaptionOptions.Text = 'New Group'
      ShowBorder = False
      Index = 2
    end
    object dxLayoutItem3: TdxLayoutItem
      Parent = dxLayoutGroup1
      CaptionOptions.Text = 'cxButton1'
      CaptionOptions.Visible = False
      Control = BtnOperation
      ControlOptions.OriginalHeight = 25
      ControlOptions.OriginalWidth = 75
      ControlOptions.ShowBorder = False
      Index = 0
    end
    object dxLayoutItem2: TdxLayoutItem
      Parent = dxLayoutControl1Group_Root
      AlignVert = avClient
      CaptionOptions.Text = 'cxMemo1'
      CaptionOptions.Visible = False
      Control = Log
      ControlOptions.OriginalHeight = 89
      ControlOptions.OriginalWidth = 185
      ControlOptions.ShowBorder = False
      Index = 1
    end
  end
  object ActionList1: TActionList
    Images = DMResources.cxImageList1
    Left = 440
    Top = 240
    object ActAbort: TAction
      Caption = '&Abort'
      ImageIndex = 7
      OnExecute = ActAbortExecute
    end
    object ActClose: TAction
      Caption = '&Close'
      ImageIndex = 2
      OnExecute = ActCloseExecute
    end
  end
end
