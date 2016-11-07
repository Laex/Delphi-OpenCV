object IPCameraLoginDlgDlg: TIPCameraLoginDlgDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Dialog'
  ClientHeight = 116
  ClientWidth = 276
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  OnActivate = FormActivate
  DesignSize = (
    276
    116)
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 16
    Width = 22
    Height = 13
    Caption = 'User'
  end
  object lbl2: TLabel
    Left = 8
    Top = 43
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object OKBtn: TButton
    Left = 109
    Top = 83
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 193
    Top = 83
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 1
  end
  object edt1: TEdit
    Left = 92
    Top = 13
    Width = 176
    Height = 21
    TabOrder = 2
  end
  object edt2: TEdit
    Left = 92
    Top = 40
    Width = 176
    Height = 21
    TabOrder = 3
  end
end
