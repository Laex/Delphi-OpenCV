object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OpenCV - Component demo'
  ClientHeight = 669
  ClientWidth = 624
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
    Top = 5
    Width = 117
    Height = 17
    Caption = 'Camera enabled'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = chk1Click
  end
  object ocvw1: TocvView
    Left = 8
    Top = 28
    Width = 300
    Height = 300
    VideoSource = ocvcmrsrc1
  end
  object ocvw2: TocvView
    Left = 8
    Top = 358
    Width = 300
    Height = 300
    VideoSource = ocvmgprtn1
  end
  object ocvw3: TocvView
    Left = 316
    Top = 358
    Width = 300
    Height = 300
    VideoSource = ocvflsrc1
  end
  object ocvw4: TocvView
    Left = 316
    Top = 28
    Width = 300
    Height = 300
    VideoSource = ocvpcmsrc1
  end
  object chk2: TCheckBox
    Left = 314
    Top = 5
    Width = 129
    Height = 17
    Caption = 'IP Camera enabled'
    TabOrder = 6
    OnClick = chk2Click
  end
  object chk3: TCheckBox
    Left = 314
    Top = 338
    Width = 187
    Height = 17
    Caption = 'File source'
    TabOrder = 7
    OnClick = chk3Click
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvHaarCascade'
    Operation.HaarCascade = hcLeftEye2Splits
    Operation.Equalize = True
    Operation.Scale = 1.300000000000000000
    Operation.MinNeighbors = 3
    Operation.MinSize.X = 30
    Operation.MinSize.Y = 30
    Operation.MaxSize.X = 0
    Operation.MaxSize.Y = 0
    Operation.DrawHaarCascade.Thickness = 3
    Operation.DrawHaarCascade.Offset.X = 0
    Operation.DrawHaarCascade.Offset.Y = 0
    Operation.DrawHaarCascade.Color = clRed
    Operation.CascadeFlags = [HAAR_DO_CANNY_PRUNING, HAAR_SCALE_IMAGE, HAAR_FIND_BIGGEST_OBJECT]
    Operation.NotifyOnlyWhenFound = False
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
        Operation.ContourDraw.Enabled = False
        Operation.ContourDraw.Thickness = 1
        Operation.ContourDraw.LineType = LT_FILLED
        Operation.ContourDraw.Offset.X = 0
        Operation.ContourDraw.Offset.Y = 0
        Operation.ContourDraw.ExternalColor = clBlack
        Operation.ContourDraw.HoleColor = clBlack
        Operation.ContourDraw.MaxLevel = 0
        Operation.ApproxPoly.Eps = 3.000000000000000000
      end
      item
        OperationClassName = 'TocvNoneOperation'
      end>
    OperationsEnabled = False
    Left = 36
    Top = 392
  end
  object ocvflsrc1: TocvFileSource
    Delay = 120
    FileName = '.\Resource\768x576.avi'
    Left = 372
    Top = 396
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r960x544
    Left = 32
    Top = 40
  end
  object ocvpcmsrc1: TocvIPCamSource
    UserName = 'admin'
    Password = 'admin'
    IP = '10.1.1.202'
    Postfix = 'cam/realmonitor?channel=1&subtype=0'
    Left = 336
    Top = 44
  end
end
