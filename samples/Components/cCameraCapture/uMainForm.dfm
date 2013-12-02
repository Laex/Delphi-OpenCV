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
    TabOrder = 1
    OnChange = cbb1Change
    Items.Strings = (
      'None'
      'Convert to grayscale'
      'Canny'
      'Smooth')
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
    Top = 8
    Width = 294
    Height = 269
  end
  object ocvw2: TocvView
    Left = 8
    Top = 296
    Width = 294
    Height = 269
  end
  object ocvcmr1: TocvCamera
    Resolution = r800x448
    Left = 368
    Top = 132
  end
  object ocvmgprtn1: TocvImageOperation
    Operation = ioNone
    Left = 368
    Top = 228
  end
  object ocvspltr1: TocvSplitter
    VideoSource = ocvcmr1
    Channels = <
      item
      end
      item
      end>
    Left = 368
    Top = 180
  end
end
