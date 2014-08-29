object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FFMpeg IP Camera source'
  ClientHeight = 376
  ClientWidth = 378
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ocvView1: TocvView
    Left = 0
    Top = 41
    Width = 378
    Height = 335
    VideoSource = ocvFFMpegIPCamSource1
    Frames = <>
    Align = alClient
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 378
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 1
    DesignSize = (
      378
      41)
    object CBCameraSampleList: TComboBox
      Left = 16
      Top = 9
      Width = 345
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      OnChange = CBCameraSampleListChange
    end
  end
  object ocvFFMpegIPCamSource1: TocvFFMpegIPCamSource
    Port = 1935
    OnIPCamEvent = ocvFFMpegIPCamSource1IPCamEvent
    ReconnectDelay = 1500
    Left = 264
    Top = 56
  end
end
