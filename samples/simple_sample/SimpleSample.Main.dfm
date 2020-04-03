object FMain: TFMain
  Left = 0
  Top = 0
  Caption = 'FMain'
  ClientHeight = 380
  ClientWidth = 616
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btn1: TButton
    Left = 256
    Top = 56
    Width = 75
    Height = 25
    Caption = 'btn1'
    TabOrder = 0
    OnClick = btn1Click
  end
  object SQLCollection1: TSQLCollection
    Items = <
      item
        Name = 'Category1'
        SQLItems = <
          item
            Name = 'SQL1'
            SQL.Strings = (
              'SELECT'
              ' *'
              'FROM customers')
          end
          item
            Name = 'SQL2'
            SQL.Strings = (
              'UPDATE customers'
              'SET'
              ' name = '#39'Jo'#227'o Ant'#244'nio'#39
              'WHERE Id = 1')
          end
          item
            Name = 'SQL3'
          end
          item
            Name = 'SQL4'
          end
          item
            Name = 'SQL5'
          end
          item
            Name = 'SQL0'
          end
          item
            Name = 'SQL6'
            SQL.Strings = (
              'SELECT NOW()')
          end>
      end
      item
        Name = 'Category2'
        SQLItems = <
          item
            Name = 'SQL1'
          end>
      end
      item
        Name = 'Category5'
        SQLItems = <>
      end
      item
        Name = 'Category4'
        SQLItems = <>
      end
      item
        Name = 'Category0'
        SQLItems = <
          item
            Name = 'SQL1'
          end
          item
            Name = 'SQL2'
          end>
      end
      item
        Name = 'Category6'
        SQLItems = <>
      end
      item
        Name = 'Category7'
        SQLItems = <>
      end>
    Left = 240
    Top = 128
  end
end
