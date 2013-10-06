object FormMain: TFormMain
  Left = 0
  Top = 154
  Caption = 'Chessboard Corners'
  ClientHeight = 484
  ClientWidth = 1284
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
  object ImageCaptured: TImage
    Left = 727
    Top = 0
    Width = 640
    Height = 480
  end
  object ImageOut: TImage
    Left = 0
    Top = 0
    Width = 640
    Height = 480
  end
  object Panel1: TPanel
    Left = 640
    Top = 0
    Width = 89
    Height = 480
    BevelInner = bvRaised
    TabOrder = 0
    object ButtonClose: TButton
      Left = 6
      Top = 16
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 0
      OnClick = ButtonCloseClick
    end
    object ButtonAR: TButton
      Left = 6
      Top = 112
      Width = 75
      Height = 25
      Caption = 'Active'
      TabOrder = 2
      OnClick = ButtonARClick
    end
    object ButtonShow: TButton
      Left = 6
      Top = 47
      Width = 75
      Height = 25
      Caption = 'Show cap. >>'
      TabOrder = 1
      OnClick = ButtonShowClick
    end
  end
end
