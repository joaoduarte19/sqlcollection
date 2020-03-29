object SQLCollectionEditor: TSQLCollectionEditor
  Left = 0
  Top = 0
  Caption = 'SQLCollection Editor'
  ClientHeight = 659
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object splCatItems: TSplitter
    Left = 217
    Top = 70
    Height = 529
    ExplicitLeft = 464
    ExplicitTop = 192
    ExplicitHeight = 100
  end
  object tlbMain: TToolBar
    Left = 0
    Top = 0
    Width = 786
    Height = 29
    Caption = 'tlbMain'
    TabOrder = 0
  end
  object statbar: TStatusBar
    Left = 0
    Top = 640
    Width = 786
    Height = 19
    Panels = <>
  end
  object pnHeader: TPanel
    Left = 0
    Top = 29
    Width = 786
    Height = 41
    Align = alTop
    TabOrder = 1
  end
  object pnFooter: TPanel
    Left = 0
    Top = 599
    Width = 786
    Height = 41
    Align = alBottom
    TabOrder = 4
  end
  object lstCategories: TListBox
    Left = 0
    Top = 70
    Width = 217
    Height = 529
    Align = alLeft
    ItemHeight = 15
    Sorted = True
    TabOrder = 2
    OnClick = lstCategoriesClick
  end
  object lstItems: TListBox
    Left = 220
    Top = 70
    Width = 566
    Height = 529
    Align = alClient
    ItemHeight = 15
    TabOrder = 3
  end
  object actlst1: TActionList
    Left = 376
    Top = 136
  end
end
