object Frame2: TFrame2
  Left = 0
  Top = 0
  Width = 292
  Height = 34
  TabOrder = 0
  object lbl1: TLabel
    AlignWithMargins = True
    Left = 5
    Top = 0
    Width = 50
    Height = 34
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Align = alLeft
    AutoSize = False
    Caption = 'lbl1'
    Layout = tlCenter
  end
  object lbl2: TLabel
    AlignWithMargins = True
    Left = 257
    Top = 0
    Width = 30
    Height = 34
    Margins.Left = 5
    Margins.Top = 0
    Margins.Right = 5
    Align = alRight
    Alignment = taCenter
    AutoSize = False
    Caption = 'lbl2'
    Layout = tlCenter
  end
  object trckbr1: TTrackBar
    Left = 60
    Top = 0
    Width = 192
    Height = 34
    Align = alClient
    Max = 255
    PageSize = 16
    Frequency = 16
    TabOrder = 0
    OnChange = trckbr1Change
  end
end
