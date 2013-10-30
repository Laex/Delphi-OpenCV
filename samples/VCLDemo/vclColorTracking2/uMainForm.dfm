object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Color Detection & Object Tracking '
  ClientHeight = 610
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pb2: TPaintBox
    Left = 8
    Top = 8
    Width = 485
    Height = 425
  end
  object lbl1: TLabel
    Left = 8
    Top = 497
    Width = 7
    Height = 13
    Caption = 'H'
  end
  object lbl2: TLabel
    Left = 8
    Top = 534
    Width = 6
    Height = 13
    Caption = 'S'
  end
  object lbl3: TLabel
    Left = 8
    Top = 571
    Width = 6
    Height = 13
    Caption = 'V'
  end
  object chk1: TCheckBox
    Left = 8
    Top = 444
    Width = 261
    Height = 17
    Caption = 'Use Morph operation'
    TabOrder = 0
  end
  object chk2: TCheckBox
    Left = 8
    Top = 467
    Width = 261
    Height = 17
    Caption = 'Track objects'
    TabOrder = 1
  end
  object trckbr_H_min: TTrackBar
    Left = 28
    Top = 490
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 2
    OnChange = trckbr_H_minChange
  end
  object trckbr_S_min: TTrackBar
    Left = 28
    Top = 527
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 6
    OnChange = trckbr_S_minChange
  end
  object trckbr_V_min: TTrackBar
    Left = 28
    Top = 564
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 10
    OnChange = trckbr_V_minChange
  end
  object trckbr_H_max: TTrackBar
    Left = 259
    Top = 490
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 3
    OnChange = trckbr_H_maxChange
  end
  object trckbr_S_max: TTrackBar
    Left = 259
    Top = 527
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 7
    OnChange = trckbr_S_maxChange
  end
  object trckbr_V_max: TTrackBar
    Left = 259
    Top = 564
    Width = 170
    Height = 31
    Max = 255
    TabOrder = 11
    OnChange = trckbr_V_maxChange
  end
  object edt1: TEdit
    Left = 201
    Top = 494
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 4
  end
  object Edit1: TEdit
    Left = 201
    Top = 531
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 8
  end
  object edt2: TEdit
    Left = 201
    Top = 568
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 12
  end
  object edt3: TEdit
    Left = 435
    Top = 494
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 5
  end
  object edt4: TEdit
    Left = 435
    Top = 531
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 9
  end
  object edt5: TEdit
    Left = 435
    Top = 568
    Width = 41
    Height = 21
    Color = clScrollBar
    ReadOnly = True
    TabOrder = 13
  end
end
