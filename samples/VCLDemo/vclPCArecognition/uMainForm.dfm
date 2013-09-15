object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'PCA recognition'
  ClientHeight = 617
  ClientWidth = 742
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
  object Label1: TLabel
    Left = 8
    Top = 35
    Width = 195
    Height = 13
    Caption = #1054#1073#1091#1095#1072#1102#1097#1072#1103' '#1074#1099#1073#1086#1088#1082#1072' (The training set)'
    Visible = False
  end
  object Label2: TLabel
    Left = 8
    Top = 55
    Width = 63
    Height = 13
    Caption = #1041#1072#1079#1080#1089' (basis)'
    Visible = False
  end
  object Label3: TLabel
    Left = 8
    Top = 79
    Width = 91
    Height = 13
    Caption = #1056#1077#1079#1091#1083#1100#1090#1072#1090' (result)'
    Visible = False
  end
  object Label4: TLabel
    Left = 8
    Top = 103
    Width = 217
    Height = 13
    Caption = #1058#1077#1089#1090#1080#1088#1091#1077#1084#1086#1077' '#1080#1079#1086#1073#1088#1072#1078#1077#1085#1080#1077' (The test image)'
    Visible = False
  end
  object btn1: TButton
    Left = 8
    Top = 8
    Width = 133
    Height = 25
    Caption = 'Start PCA recognition'
    TabOrder = 0
    OnClick = btn1Click
  end
end
