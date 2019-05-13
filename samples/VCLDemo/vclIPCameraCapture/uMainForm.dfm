object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'IPCamera capture'
  ClientHeight = 352
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 8
    Top = 52
    Width = 225
    Height = 181
  end
  object pb1: TPaintBox
    Left = 248
    Top = 52
    Width = 205
    Height = 181
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 64
    Height = 13
    Caption = 'RTSP source:'
  end
  object lbl1: TLabel
    Left = 200
    Top = 313
    Width = 13
    Height = 13
    Caption = 'ms'
  end
  object rb1: TRadioButton
    Left = 8
    Top = 29
    Width = 113
    Height = 17
    Caption = 'Variant 1'
    Checked = True
    TabOrder = 2
    TabStop = True
  end
  object rb2: TRadioButton
    Left = 248
    Top = 29
    Width = 113
    Height = 17
    Caption = 'Variant 2'
    TabOrder = 3
  end
  object chk1: TCheckBox
    Left = 8
    Top = 240
    Width = 97
    Height = 17
    Caption = 'Draw text'
    TabOrder = 4
  end
  object rg1: TRadioGroup
    Left = 8
    Top = 263
    Width = 445
    Height = 77
    Caption = ' Draw type '
    ItemIndex = 0
    Items.Strings = (
      'On application idle'
      'On timer')
    TabOrder = 5
    OnClick = rg1Click
  end
  object se1: TSpinEdit
    Left = 96
    Top = 309
    Width = 98
    Height = 22
    Enabled = False
    Increment = 100
    MaxValue = 10000
    MinValue = 1
    TabOrder = 6
    Value = 100
    OnChange = se1Change
  end
  object RTSPCapture: TEdit
    Left = 78
    Top = 5
    Width = 294
    Height = 21
    TabOrder = 1
    Text = 'rtsp://80.78.116.125:1500/rtplive/5goda_hd.stream'
  end
  object ButtonStartCapture: TButton
    Left = 378
    Top = 3
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = ButtonStartCaptureClick
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmr1Timer
    Left = 352
    Top = 240
  end
end
