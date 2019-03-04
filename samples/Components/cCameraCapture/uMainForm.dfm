object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OpenCV - Component demo'
  ClientHeight = 285
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ocvView3: TocvView
    Left = 0
    Top = 0
    Width = 333
    Height = 285
    VideoSource = ocvmgprtn1
    Frames = <>
    Align = alClient
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvHaarCascade'
    Operation.Equalize = True
    Operation.Scale = 1.300000000000000000
    Operation.MinNeighbors = 3
    Operation.MinSize.X = 30
    Operation.MinSize.Y = 30
    Operation.MaxSize.X = 0
    Operation.MaxSize.Y = 0
    Operation.DrawHaarCascade.Thickness = 1
    Operation.DrawHaarCascade.Offset.X = 0
    Operation.DrawHaarCascade.Offset.Y = 0
    Operation.NotifyOnlyWhenFound = False
    Operations = <>
    Left = 16
    Top = 12
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r1280x720
    Left = 52
    Top = 12
  end
end
