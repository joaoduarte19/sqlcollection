object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 426
  ClientWidth = 727
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object dbgrd1: TDBGrid
    Left = 0
    Top = 72
    Width = 727
    Height = 354
    Align = alBottom
    DataSource = dsPerson
    TabOrder = 1
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'Tahoma'
    TitleFont.Style = []
  end
  object btn1: TButton
    Left = 96
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 0
    OnClick = btn1Click
  end
  object fdConnection: TFDConnection
    Params.Strings = (
      'DriverID=PG'
      'CharacterSet=UTF8'
      'Database=test'
      'Password=postgres'
      'Port=5432'
      'Server=localhost'
      'User_Name=postgres')
    Connected = True
    LoginPrompt = False
    Left = 624
    Top = 16
  end
  object qryPerson: TFDQuery
    Connection = fdConnection
    Left = 272
    Top = 120
  end
  object dsPerson: TDataSource
    DataSet = qryPerson
    Left = 320
    Top = 128
  end
  object FDPhysPgDriverLink1: TFDPhysPgDriverLink
    Left = 656
    Top = 16
  end
  object SQLCollection1: TSQLCollection
    Items = <
      item
        Name = 'Person'
        SQLItems = <
          item
            Name = 'Select person'
            SQL.Strings = (
              'SELECT'
              ' *'
              ' FROM person')
          end>
      end>
    Left = 576
    Top = 16
  end
end
