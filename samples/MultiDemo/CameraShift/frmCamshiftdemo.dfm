object fCamshiftdemo: TfCamshiftdemo
  Left = 321
  Top = 186
  Width = 380
  Height = 431
  Caption = 'CAMshift demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 372
    Height = 118
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 20
      Top = 20
      Width = 30
      Height = 16
      Caption = 'Vmin'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 20
      Top = 52
      Width = 34
      Height = 16
      Caption = 'Vmax'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 20
      Top = 89
      Width = 30
      Height = 16
      Caption = 'Smin'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object tbVmin: TTrackBar
      Left = 65
      Top = 20
      Width = 287
      Height = 20
      Max = 256
      Orientation = trHorizontal
      PageSize = 8
      Frequency = 16
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 0
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object tbVmax: TTrackBar
      Left = 65
      Top = 52
      Width = 287
      Height = 20
      Max = 256
      Orientation = trHorizontal
      PageSize = 8
      Frequency = 16
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 1
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
    object tbSmin: TTrackBar
      Left = 65
      Top = 85
      Width = 287
      Height = 20
      Max = 256
      Orientation = trHorizontal
      PageSize = 8
      Frequency = 16
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      TickMarks = tmBottomRight
      TickStyle = tsAuto
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 118
    Width = 372
    Height = 279
    Align = alClient
    TabOrder = 1
    object formImage: TImage
      Left = 1
      Top = 1
      Width = 370
      Height = 277
      Align = alClient
      Stretch = True
      OnMouseDown = formImageMouseDown
      OnMouseMove = formImageMouseMove
      OnMouseUp = formImageMouseUp
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 200
    OnTimer = Timer1Timer
    Left = 480
    Top = 40
  end
end
