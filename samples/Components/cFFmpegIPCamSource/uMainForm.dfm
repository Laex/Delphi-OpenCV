object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FFMpeg IP Camera source'
  ClientHeight = 273
  ClientWidth = 356
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
    Left = 0
    Top = 0
    Width = 356
    Height = 273
    VideoSource = ocvfmpgpcmsrc1
    Align = alClient
  end
  object ocvfmpgpcmsrc1: TocvFFMpegIPCamSource
    Enabled = True
    UserName = 'admin'
    Password = 'admin'
    IP = '10.1.1.202'
    URI = '/cam/realmonitor?channel=1&subtype=0'
    Left = 76
    Top = 40
  end
end
