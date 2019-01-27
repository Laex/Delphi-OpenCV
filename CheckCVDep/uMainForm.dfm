object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Verifying OpenCV dependencies'
  ClientHeight = 407
  ClientWidth = 643
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  DesignSize = (
    643
    407)
  PixelsPerInch = 96
  TextHeight = 13
  object mmo1: TMemo
    Left = 0
    Top = 88
    Width = 643
    Height = 319
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'mmo1')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object btn1: TButton
    Left = 8
    Top = 24
    Width = 189
    Height = 25
    Caption = 'Verifying OpenCV dependencies'
    TabOrder = 0
    OnClick = btn1Click
  end
  object pb1: TProgressBar
    Left = 8
    Top = 64
    Width = 627
    Height = 13
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object chk1: TCheckBox
    Left = 232
    Top = 28
    Width = 113
    Height = 17
    Caption = 'Show dll names'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
end
