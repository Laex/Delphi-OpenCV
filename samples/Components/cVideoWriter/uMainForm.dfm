object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'OCV Video Writer demo'
  ClientHeight = 317
  ClientWidth = 304
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
    Left = 8
    Top = 260
    Width = 104
    Height = 13
    Caption = 'Destanation directory'
  end
  object ocvw1: TocvView
    Left = 8
    Top = 8
    Width = 285
    Height = 241
    VideoSource = ocvcmrsrc1
    Frames = <>
  end
  object edt1: TEdit
    Left = 118
    Top = 257
    Width = 175
    Height = 21
    TabOrder = 1
    Text = 'capture.avi'
  end
  object btn1: TButton
    Left = 118
    Top = 284
    Width = 99
    Height = 25
    Caption = 'Save media'
    TabOrder = 2
    OnClick = btn1Click
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Left = 212
    Top = 28
  end
  object ocvdwrtr1: TocvVideoWriter
    VideoSource = ocvcmrsrc1
    FourCC = 'XVID'
    FrameSize.Width = 640
    FrameSize.Height = 480
    Left = 212
    Top = 84
  end
end
