object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Template detect (camera)'
  ClientHeight = 286
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pbFrame: TPaintBox
    Left = 8
    Top = 8
    Width = 320
    Height = 240
    OnMouseDown = pbFrameMouseDown
    OnMouseMove = pbFrameMouseMove
    OnMouseUp = pbFrameMouseUp
    OnPaint = pbFramePaint
  end
  object gbSettings: TGroupBox
    Left = 334
    Top = 8
    Width = 244
    Height = 233
    Caption = ' Template detect '
    TabOrder = 0
    object lblFlagCaption: TLabel
      Left = 11
      Top = 152
      Width = 66
      Height = 13
      Caption = 'Object found:'
    end
    object lblFlag: TLabel
      Left = 120
      Top = 152
      Width = 18
      Height = 13
      Caption = 'NO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblScoreCaption: TLabel
      Left = 11
      Top = 176
      Width = 66
      Height = 13
      Caption = 'Match score:'
    end
    object lblScore: TLabel
      Left = 120
      Top = 176
      Width = 22
      Height = 13
      Caption = '0.00'
    end
    object lblThresholdCaption: TLabel
      Left = 11
      Top = 96
      Width = 77
      Height = 13
      Caption = 'Threshold (%):'
    end
    object lblStreakCaption: TLabel
      Left = 11
      Top = 124
      Width = 95
      Height = 13
      Caption = 'Stable frames:'
    end
    object lblMs: TLabel
      Left = 190
      Top = 59
      Width = 13
      Height = 13
      Caption = 'ms'
      Enabled = False
    end
    object rgTimerMode: TRadioGroup
      Left = 11
      Top = 16
      Width = 209
      Height = 68
      Caption = ' Capture '
      ItemIndex = 0
      Items.Strings = (
        'On application idle'
        'On timer')
      TabOrder = 0
      OnClick = rgTimerModeClick
    end
    object seInterval: TSpinEdit
      Left = 123
      Top = 56
      Width = 61
      Height = 22
      Enabled = False
      Increment = 50
      MaxValue = 10000
      MinValue = 20
      TabOrder = 1
      Value = 100
      OnChange = seIntervalChange
    end
    object seThreshold: TSpinEdit
      Left = 120
      Top = 93
      Width = 61
      Height = 22
      MaxValue = 100
      MinValue = 1
      TabOrder = 2
      Value = 80
    end
    object seStreak: TSpinEdit
      Left = 120
      Top = 121
      Width = 61
      Height = 22
      MaxValue = 30
      MinValue = 1
      TabOrder = 3
      Value = 3
    end
    object btnLoadTemplate: TButton
      Left = 11
      Top = 200
      Width = 209
      Height = 25
      Caption = 'Load template from file...'
      TabOrder = 4
      OnClick = btnLoadTemplateClick
    end
  end
  object btnStartStop: TButton
    Left = 334
    Top = 247
    Width = 115
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartStopClick
  end
  object btnCaptureTemplate: TButton
    Left = 455
    Top = 247
    Width = 123
    Height = 25
    Caption = 'Capture template'
    Enabled = False
    TabOrder = 2
    OnClick = btnCaptureTemplateClick
  end
  object tmrFrame: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmrFrameTimer
    Left = 160
    Top = 32
  end
  object dlgOpenTemplate: TOpenDialog
    Left = 208
    Top = 32
  end
end
