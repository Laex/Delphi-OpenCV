object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 482
  ClientWidth = 595
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
    Left = 4
    Top = 36
    Width = 292
    Height = 292
    Cursor = crCross
    VideoSource = ocvcmrsrc1
    Frames = <>
    OnBeforePaint = ocvw1BeforePaint
    OnMouseUp = ocvw1MouseUp
  end
  inline frm: TFrame2
    Left = 4
    Top = 371
    Width = 292
    Height = 34
    TabOrder = 5
    inherited lbl1: TLabel
      Caption = 'H max:'
    end
    inherited trckbr1: TTrackBar
      Max = 360
      OnChange = frm3trckbr1Change
    end
  end
  inline frm21: TFrame2
    Left = 299
    Top = 334
    Width = 292
    Height = 34
    TabOrder = 4
    inherited lbl1: TLabel
      Caption = 'V min:'
    end
    inherited trckbr1: TTrackBar
      OnChange = frm3trckbr1Change
    end
  end
  inline frm2: TFrame2
    Left = 4
    Top = 408
    Width = 292
    Height = 34
    TabOrder = 7
    inherited lbl1: TLabel
      Caption = 'S min:'
    end
    inherited trckbr1: TTrackBar
      OnChange = frm3trckbr1Change
    end
  end
  inline frm3: TFrame2
    Left = 4
    Top = 334
    Width = 292
    Height = 34
    TabOrder = 3
    inherited lbl1: TLabel
      Caption = 'H min:'
    end
    inherited trckbr1: TTrackBar
      Max = 360
      OnChange = frm3trckbr1Change
    end
  end
  inline frm4: TFrame2
    Left = 4
    Top = 445
    Width = 292
    Height = 34
    TabOrder = 9
    inherited lbl1: TLabel
      Caption = 'S max:'
    end
    inherited trckbr1: TTrackBar
      OnChange = frm3trckbr1Change
    end
  end
  inline frm5: TFrame2
    Left = 299
    Top = 371
    Width = 292
    Height = 34
    TabOrder = 6
    inherited lbl1: TLabel
      Caption = 'V max:'
    end
    inherited trckbr1: TTrackBar
      OnChange = frm3trckbr1Change
    end
  end
  object ocvw2: TocvView
    Left = 297
    Top = 36
    Width = 292
    Height = 292
    VideoSource = ocvmgprtn1
    Frames = <>
  end
  object pnl1: TPanel
    Left = 443
    Top = 445
    Width = 148
    Height = 34
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Selected color'
    ParentBackground = False
    TabOrder = 11
  end
  inline frm22: TFrame2
    Left = 299
    Top = 408
    Width = 292
    Height = 34
    TabOrder = 8
    inherited lbl1: TLabel
      Caption = 'Range'
    end
    inherited trckbr1: TTrackBar
      Max = 40
      Min = 1
      Frequency = 5
      Position = 40
      OnChange = frm22trckbr1Change
    end
  end
  object pnl2: TPanel
    Left = 299
    Top = 445
    Width = 138
    Height = 34
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Caption = 'Selected color'
    ParentBackground = False
    TabOrder = 10
  end
  object chk1: TCheckBox
    Left = 4
    Top = 8
    Width = 97
    Height = 17
    Caption = 'Mask'
    Enabled = False
    TabOrder = 0
  end
  object ocvcmrsrc1: TocvCameraSource
    Enabled = True
    Resolution = r1280x720
    Left = 52
    Top = 72
  end
  object ocvmgprtn1: TocvImageOperation
    VideoSource = ocvcmrsrc1
    OperationClassName = 'TocvNoneOperation'
    Operations = <
      item
        OperationClassName = 'TocvCvtColorOperation'
        Operation.ColorConversion = BGR2HSV
        Operation.Depth = DEPTH_8U
        Operation.Channels = 3
      end
      item
        OperationClassName = 'TocvSmoothOperation'
        Operation.size1 = 3
        Operation.size2 = 3
        Operation.SmoothType = BLUR
      end
      item
        OperationClassName = 'TocvInRangeSOperation'
      end>
    Left = 52
    Top = 128
  end
end
