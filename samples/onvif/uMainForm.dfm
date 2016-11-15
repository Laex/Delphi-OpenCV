object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'ONVIF demo'
  ClientHeight = 547
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    631
    547)
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 548
    Top = 514
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Probe'
    TabOrder = 0
    OnClick = btn1Click
  end
  object tv1: TTreeView
    Left = 8
    Top = 8
    Width = 615
    Height = 493
    Anchors = [akLeft, akTop, akRight, akBottom]
    Indent = 19
    TabOrder = 1
    OnDblClick = tv1DblClick
  end
  object onvfprb1: TONVIFProbe
    OnCompleted = onvfprb1Completed
    OnProbeMath = onvfprb1ProbeMath
    ProbeType = [ptNetworkVideoTransmitter, ptDevice]
    Left = 28
    Top = 24
  end
end
