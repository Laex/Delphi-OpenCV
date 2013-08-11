object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Color Detection & Object Tracking '
  ClientHeight = 342
  ClientWidth = 670
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
  object pb1: TPaintBox
    Left = 8
    Top = 52
    Width = 325
    Height = 281
  end
  object pb2: TPaintBox
    Left = 339
    Top = 52
    Width = 325
    Height = 281
  end
  object rb1: TRadioButton
    Left = 8
    Top = 31
    Width = 165
    Height = 17
    Caption = 'Tracking Red objects'
    TabOrder = 1
  end
  object rb2: TRadioButton
    Left = 8
    Top = 8
    Width = 165
    Height = 17
    Caption = 'Detecting Red objects'
    Checked = True
    TabOrder = 0
    TabStop = True
  end
end
