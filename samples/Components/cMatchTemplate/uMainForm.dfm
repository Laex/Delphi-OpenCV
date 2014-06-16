object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'cMatchTemplate'
  ClientHeight = 348
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 451
    Top = 3
    Width = 121
    Height = 13
    Caption = 'Select the rectangle area'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object ocvw1: TocvView
    Left = 8
    Top = 22
    Width = 320
    Height = 320
    VideoSource = ocvmgprtn1
    Frames = <>
  end
  object ocvw2: TocvView
    Left = 451
    Top = 22
    Width = 320
    Height = 320
    Frames = <>
    OnAfterPaint = ocvw2AfterPaint
    OnMouseDown = ocvw2MouseDown
    OnMouseUp = ocvw2MouseUp
    OnMouseMove = ocvw2MouseMove
  end
  object btn1: TButton
    Left = 334
    Top = 154
    Width = 111
    Height = 25
    Caption = 'Get snapshot ->'
    TabOrder = 2
    OnClick = btn1Click
  end
  object btn2: TButton
    Left = 334
    Top = 194
    Width = 111
    Height = 25
    Caption = '<- Match template'
    Enabled = False
    TabOrder = 3
    OnClick = btn2Click
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r320x240
    Left = 364
    Top = 18
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvMatchTemplate'
    Operation.DrawRect.Thickness = 1
    Operation.DrawRect.Offset.X = 0
    Operation.DrawRect.Offset.Y = 0
    Operations = <>
    Left = 364
    Top = 82
  end
end
