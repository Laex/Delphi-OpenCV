object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'CameraCapture - Component Demo'
  ClientHeight = 471
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object grdpnl1: TGridPanel
    Left = 0
    Top = 0
    Width = 641
    Height = 471
    Align = alClient
    Caption = 'grdpnl1'
    ColumnCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = opncvw1
        Row = 0
      end
      item
        Column = 1
        Control = opncvw2
        Row = 0
      end
      item
        Column = 0
        Control = opncvw3
        Row = 1
      end
      item
        Column = 1
        Control = opncvw4
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 0
    ExplicitLeft = 236
    ExplicitTop = 160
    ExplicitWidth = 185
    ExplicitHeight = 41
    object opncvw1: TOpenCVView
      Left = 1
      Top = 1
      Width = 319
      Height = 234
      VideoSource = opncvcmr1
      Align = alClient
      ExplicitLeft = 164
      ExplicitTop = 100
      ExplicitWidth = 100
      ExplicitHeight = 41
    end
    object opncvw2: TOpenCVView
      Left = 320
      Top = 1
      Width = 320
      Height = 234
      VideoSource = opncvcmr1
      Align = alClient
      ExplicitLeft = 164
      ExplicitTop = 100
      ExplicitWidth = 100
      ExplicitHeight = 41
    end
    object opncvw3: TOpenCVView
      Left = 1
      Top = 235
      Width = 319
      Height = 235
      VideoSource = opncvcmr1
      Align = alClient
      ExplicitLeft = 164
      ExplicitTop = 100
      ExplicitWidth = 100
      ExplicitHeight = 41
    end
    object opncvw4: TOpenCVView
      Left = 320
      Top = 235
      Width = 320
      Height = 235
      VideoSource = opncvcmr1
      Align = alClient
      ExplicitLeft = 164
      ExplicitTop = 100
      ExplicitWidth = 100
      ExplicitHeight = 41
    end
  end
  object opncvcmr1: TOpenCVCamera
    Enabled = True
    Left = 188
    Top = 60
  end
end
