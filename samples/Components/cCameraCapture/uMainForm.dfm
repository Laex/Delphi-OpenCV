object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'OpenCV - Component demo'
  ClientHeight = 573
  ClientWidth = 466
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
    Left = 308
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Image operation'
  end
  object cbb1: TComboBox
    Left = 308
    Top = 24
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
      'Sobel')
  end
  object chk1: TCheckBox
    Left = 308
    Top = 60
    Width = 97
    Height = 17
    Caption = 'Camera enabled'
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = chk1Click
  end
  object ocvw1: TocvView
    Left = 8
    Top = 4
    Width = 294
    Height = 269
    VideoSource = ocvcmr1
  end
  object ocvw2: TocvView
    Left = 8
    Top = 296
    Width = 294
    Height = 269
    VideoSource = ocvmgprtn1
  end
  object ocvcmr1: TocvCamera
    Resolution = r640x360
    Left = 368
    Top = 192
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmr1
    OperationClassName = 'TocvLaplace'
    Operations = <
      item
        OperationClassName = 'TovcErode'
      end
      item
        OperationClassName = 'TovcImageOperation_Canny'
        Operation.Threshold1 = 10.000000000000000000
        Operation.Threshold2 = 100.000000000000000000
        Operation.ApertureSize = 3
      end>
    Left = 368
    Top = 228
  end
end
