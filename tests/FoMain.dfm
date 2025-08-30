object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Test McParam'
  ClientHeight = 403
  ClientWidth = 690
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  Position = poScreenCenter
  OnCreate = FormCreate
  OnResize = FormResize
  DesignSize = (
    690
    403)
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 684
    Height = 358
    ActivePage = TabControls
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    Constraints.MinHeight = 358
    Constraints.MinWidth = 600
    TabOrder = 0
    OnChange = PageControlChange
    object TabControls: TTabSheet
      Caption = 'Controls'
      object LbDesc: TLabel
        Left = 357
        Top = 80
        Width = 153
        Height = 91
        Caption = 
          'Parameters groups will manage:'#13#10' . MyEdit'#13#10' . MyCheckBox'#13#10' . MyR' +
          'adioButtons'#13#10' . MyMemo lines'#13#10' . MyListBox items'#13#10' . Form size'
      end
      object MyGroupBox: TGroupBox
        Left = 9
        Top = 0
        Width = 337
        Height = 313
        Caption = 'My GroupBox caption'
        TabOrder = 0
        DesignSize = (
          337
          313)
        object MyLabel: TLabel
          Left = 16
          Top = 24
          Width = 80
          Height = 13
          Caption = 'My Label caption'
        end
        object MyEdit: TEdit
          Left = 160
          Top = 21
          Width = 169
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          Text = 'My Edit text'
        end
        object MyCheckBox: TCheckBox
          Left = 16
          Top = 48
          Width = 160
          Height = 17
          Caption = 'My CheckBox caption'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object MyRadioButton1: TRadioButton
          Left = 48
          Top = 72
          Width = 145
          Height = 17
          Caption = 'My RadioButton 1 caption'
          Checked = True
          TabOrder = 2
          TabStop = True
        end
        object MyMemo: TMemo
          Left = 16
          Top = 119
          Width = 155
          Height = 107
          Lines.Strings = (
            'MyMemo line 1'
            'MyMemo line 2')
          TabOrder = 3
        end
        object MyButton: TButton
          Left = 254
          Top = 272
          Width = 75
          Height = 25
          Anchors = [akRight, akBottom]
          Caption = 'My Button'
          TabOrder = 4
        end
        object GbxFormSize: TGroupBox
          Left = 16
          Top = 232
          Width = 155
          Height = 65
          Anchors = [akLeft, akTop, akRight]
          Caption = 'This form size'
          TabOrder = 5
          object LbHeight: TLabel
            Left = 8
            Top = 16
            Width = 35
            Height = 13
            Caption = 'Height:'
          end
          object LbWidth: TLabel
            Left = 8
            Top = 36
            Width = 32
            Height = 13
            Caption = 'Width:'
          end
        end
        object MyRadioButton2: TRadioButton
          Left = 48
          Top = 92
          Width = 145
          Height = 17
          Caption = 'My RadioButton 2 caption'
          TabOrder = 6
        end
        object MyListBox: TListBox
          Left = 178
          Top = 120
          Width = 151
          Height = 77
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          Items.Strings = (
            'MyListBox item 1'
            'MyListBox item 2')
          TabOrder = 7
          OnClick = MyListBoxClick
        end
        object EdListItem: TEdit
          Left = 178
          Top = 205
          Width = 124
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 8
        end
        object BtUp: TButton
          Left = 309
          Top = 203
          Width = 20
          Height = 25
          Anchors = [akTop, akRight]
          Caption = '^'
          TabOrder = 9
          OnClick = BtUpClick
        end
      end
      object GbxParamGroups: TGroupBox
        Left = 351
        Top = 0
        Width = 322
        Height = 65
        Caption = 'Parameters groups'
        TabOrder = 1
      end
    end
    object TabInternal: TTabSheet
      Caption = 'Internal'
      ImageIndex = 2
      DesignSize = (
        676
        330)
      object LbInternal: TLabel
        Left = 3
        Top = 4
        Width = 378
        Height = 13
        Caption = 
          'Internal JSON structure from frame ParamsGroups (see "ParamsGrou' +
          'ps.json"):'
      end
      object MemoInternal: TMemo
        Left = 3
        Top = 23
        Width = 669
        Height = 304
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Consolas'
        Font.Style = []
        Lines.Strings = (
          'MemoInternal')
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
    object TabReport: TTabSheet
      Caption = 'Report'
      ImageIndex = 1
      object MemoReport: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 670
        Height = 324
        Align = alClient
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 0
        ExplicitWidth = 690
      end
    end
  end
  object BtClose: TButton
    Left = 607
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 1
    OnClick = BtCloseClick
  end
  object BtRun: TButton
    Left = 500
    Top = 370
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Run Tests'
    TabOrder = 2
    OnClick = BtRunClick
  end
  object MainMenu: TMainMenu
    Left = 648
    Top = 280
    object MnFile: TMenuItem
      Caption = 'File'
      object MnFileClose: TMenuItem
        Caption = 'Close'
        OnClick = BtCloseClick
      end
    end
  end
end
