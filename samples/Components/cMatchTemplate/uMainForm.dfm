object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'cMatchTemplate'
  ClientHeight = 342
  ClientWidth = 747
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object ocvw1: TocvView
    Left = 8
    Top = 16
    Width = 320
    Height = 320
    VideoSource = ocvmgprtn1
  end
  object ocvw2: TocvView
    Left = 424
    Top = 14
    Width = 320
    Height = 320
    OnAfterPaint = ocvw2AfterPaint
    OnMouseDown = ocvw2MouseDown
    OnMouseUp = ocvw2MouseUp
    OnMouseMove = ocvw2MouseMove
  end
  object btn1: TButton
    Left = 334
    Top = 148
    Width = 75
    Height = 25
    Caption = 'Snap ->'
    TabOrder = 2
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 334
    Top = 188
    Width = 75
    Height = 25
    Caption = '<- Math'
    Enabled = False
    TabOrder = 3
    OnClick = btn2Click
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r320x240
    Left = 364
    Top = 12
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvMatchTemplate'
    Operation.DrawRect.Thickness = 1
    Operation.DrawRect.Offset.X = 0
    Operation.DrawRect.Offset.Y = 0
    Operations = <>
    Left = 364
    Top = 76
  end
end
