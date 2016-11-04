object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OpenCV - Component demo'
  ClientHeight = 669
  ClientWidth = 613
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 338
    Width = 79
    Height = 13
    Caption = 'Image operation'
  end
  object cbb1: TComboBox
    Left = 93
    Top = 335
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = cbb1Change
    Items.Strings = (
      'None'
      'Convert to grayscale'
      'Canny'
      'Smooth'
      'Erode'
      'Dilate'
      'Laplace'
      'Sobel'
      'Threshold')
  end
  object chk1: TCheckBox
    Left = 8
    Top = 5
    Width = 117
    Height = 17
    Caption = 'Camera enabled'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = chk1Click
  end
  object chk2: TCheckBox
    Left = 314
    Top = 5
    Width = 129
    Height = 17
    Caption = 'IP Camera enabled'
    TabOrder = 1
    OnClick = chk2Click
  end
  object chk3: TCheckBox
    Left = 314
    Top = 338
    Width = 187
    Height = 17
    Caption = 'File source'
    TabOrder = 3
    OnClick = chk3Click
  end
  object ocvView1: TocvView
    Left = 8
    Top = 28
    Width = 285
    Height = 297
    VideoSource = ocvcmrsrc1
    Frames = <
      item
        VideoSource = ocvflsrc1
        DrawRect.Left = 0
        DrawRect.Top = 0
        DrawRect.Right = 100
        DrawRect.Bottom = 100
        Enabled = True
      end>
  end
  object ocvView2: TocvView
    Left = 314
    Top = 28
    Width = 285
    Height = 297
    VideoSource = ocvpcmsrc1
    Frames = <>
  end
  object ocvView3: TocvView
    Left = 8
    Top = 362
    Width = 285
    Height = 297
    VideoSource = ocvmgprtn1
    Frames = <>
  end
  object ocvView4: TocvView
    Left = 314
    Top = 361
    Width = 285
    Height = 297
    VideoSource = ocvflsrc1
    Frames = <>
  end
  object ocvflsrc1: TocvFileSource
    Enabled = True
    FileName = '..\..\resource\media\768x576.avi'
    Left = 420
    Top = 428
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvNoneOperation'
    Operations = <>
    Left = 108
    Top = 448
  end
  object ocvpcmsrc1: TocvIPCamSource
    IP = '80.78.116.125'
    URI = '/rtplive/plot_hd.stream'
    Port = 1935
    Left = 428
    Top = 116
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r1280x720
    Left = 124
    Top = 140
  end
end
