object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Camera capture'
  ClientHeight = 337
  ClientWidth = 471
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object img1: TImage
    Left = 8
    Top = 28
    Width = 225
    Height = 181
  end
  object pb1: TPaintBox
    Left = 248
    Top = 28
    Width = 205
    Height = 181
  end
  object lbl1: TLabel
    Left = 188
    Top = 296
    Width = 13
    Height = 13
    Caption = 'ms'
    Enabled = False
  end
  object rb1: TRadioButton
    Left = 8
    Top = 5
    Width = 113
    Height = 17
    Caption = 'Variant 1'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
  object rb2: TRadioButton
    Left = 248
    Top = 5
    Width = 113
    Height = 17
    Caption = 'Variant 2'
    TabOrder = 1
  end
  object chk1: TCheckBox
    Left = 8
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Draw text'
    TabOrder = 2
  end
  object rg1: TRadioGroup
    Left = 8
    Top = 242
    Width = 445
    Height = 77
    Caption = ' Draw type '
    ItemIndex = 0
    Items.Strings = (
      'On application idle'
      'On timer')
    TabOrder = 3
    OnClick = rg1Click
  end
  object se1: TSpinEdit
    Left = 84
    Top = 292
    Width = 98
    Height = 22
    Enabled = False
    Increment = 100
    MaxValue = 10000
    MinValue = 1
    TabOrder = 4
    Value = 100
    OnChange = se1Change
  end
  object tmr1: TTimer
    Enabled = False
    Interval = 100
    OnTimer = tmr1Timer
    Left = 264
    Top = 192
  end
end
