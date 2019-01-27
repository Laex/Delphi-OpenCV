object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 467
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object chk1: TCheckBox
    Left = 8
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Camera enabled'
    TabOrder = 0
  end
  object ocvw1: TocvView
    Left = 8
    Top = 31
    Width = 428
    Height = 350
    VideoSource = ocvmgprtn1
    Frames = <>
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Left = 168
    Top = 120
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvEditor'
    Operation.Step = 10
    Operation.EditorOperation = eopNone
    Operations = <>
    Left = 204
    Top = 120
  end
end
