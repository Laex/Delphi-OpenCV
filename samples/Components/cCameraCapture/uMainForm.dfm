object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OpenCV - Component demo'
  ClientHeight = 550
  ClientWidth = 616
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
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Image operation'
  end
  object cbb1: TComboBox
    Left = 93
    Top = 5
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 0
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
    Top = 29
    Width = 97
    Height = 17
    Caption = 'Camera enabled'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chk1Click
  end
  object ocvw1: TocvView
    Left = 8
    Top = 52
    Width = 285
    Height = 229
    VideoSource = ocvcmrsrc1
  end
  object ocvw2: TocvView
    Left = 8
    Top = 307
    Width = 285
    Height = 229
    VideoSource = ocvmgprtn1
  end
  object ocvw3: TocvView
    Left = 316
    Top = 307
    Width = 285
    Height = 229
    VideoSource = ocvflsrc1
  end
  object ocvw4: TocvView
    Left = 316
    Top = 52
    Width = 285
    Height = 229
    VideoSource = ocvpcmsrc1
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvContoursOperation'
    Operation.OperationClassName = 'TocvAdaptiveThresholdOperation'
    Operation.Preprocessing.MaxValue = 250.000000000000000000
    Operation.Preprocessing.BlockSize = 3
    Operation.Preprocessing.Param = 5.000000000000000000
    Operation.Offset.X = 0
    Operation.Offset.Y = 0
    Operation.MinArea = 100
    Operation.ContourDraw.Thickness = 1
    Operation.ContourDraw.Offset.X = 0
    Operation.ContourDraw.Offset.Y = 0
    Operation.ApproxPoly.Eps = 3.000000000000000000
    Operations = <
      item
        OperationClassName = 'TocvContoursOperation'
        Operation.OperationClassName = 'TocvThresholdOperation'
        Operation.Preprocessing.MaxValue = 255.000000000000000000
        Operation.Preprocessing.ThresholdType = THRESH_BINARY_INV
        Operation.Preprocessing.Threshold = 128.000000000000000000
        Operation.Offset.X = 0
        Operation.Offset.Y = 0
        Operation.MinArea = 100
        Operation.ContourDraw.Thickness = 1
        Operation.ContourDraw.Offset.X = 0
        Operation.ContourDraw.Offset.Y = 0
        Operation.ApproxPoly.Eps = 3.000000000000000000
      end
      item
        OperationClassName = 'TocvNoneOperation'
      end>
    OperationsEnabled = False
    Left = 24
    Top = 316
  end
  object ocvflsrc1: TocvFileSource
    Enabled = True
    Delay = 120
    FileName = '.\Resource\768x576.avi'
    Left = 336
    Top = 328
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r160x120
    Left = 32
    Top = 64
  end
  object ocvpcmsrc1: TocvIPCamSource
    Enabled = True
    UserName = 'admin'
    Password = 'admin'
    IP = '10.1.1.202'
    Postfix = 'cam/realmonitor?channel=1&subtype=0'
    Left = 336
    Top = 68
  end
end
