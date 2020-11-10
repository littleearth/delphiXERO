object frmXERODemo: TfrmXERODemo
  Left = 0
  Top = 0
  Caption = 'delphiXERO Demo'
  ClientHeight = 569
  ClientWidth = 852
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
  object PageControlMenu: TPageControl
    Left = 0
    Top = 66
    Width = 852
    Height = 503
    ActivePage = tabAPISearch
    Align = alClient
    TabOrder = 1
    OnChange = PageControlMenuChange
    object tabAPISearch: TTabSheet
      Caption = 'Search'
      object PageControlSearchData: TPageControl
        AlignWithMargins = True
        Left = 3
        Top = 263
        Width = 838
        Height = 209
        ActivePage = tabDataJSON
        Align = alClient
        TabOrder = 1
        object tabDataJSON: TTabSheet
          Caption = 'JSON'
          object memoJSON: TMemo
            Left = 0
            Top = 41
            Width = 830
            Height = 140
            Align = alClient
            ScrollBars = ssBoth
            TabOrder = 1
          end
          object Panel16: TPanel
            Left = 0
            Top = 0
            Width = 830
            Height = 41
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object btnAPIJSONCopy: TButton
              AlignWithMargins = True
              Left = 103
              Top = 3
              Width = 138
              Height = 35
              Align = alLeft
              Caption = 'Copy to clipboard'
              TabOrder = 1
              OnClick = btnAPIJSONCopyClick
            end
            object btnAPIJsonToDataset: TButton
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 94
              Height = 35
              Align = alLeft
              Caption = 'To dataset'
              TabOrder = 0
              OnClick = btnAPIJsonToDatasetClick
            end
          end
        end
      end
      object pnlAPISearch: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 838
        Height = 254
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        TabStop = True
        object Panel1: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 192
          Width = 832
          Height = 59
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
          object btnSearch: TButton
            AlignWithMargins = True
            Left = 754
            Top = 3
            Width = 75
            Height = 53
            Align = alRight
            Caption = 'Search'
            TabOrder = 2
            OnClick = btnSearchClick
          end
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 137
            Height = 59
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object Label1: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 131
              Height = 13
              Align = alTop
              Caption = 'Page'
              ExplicitWidth = 24
            end
            object editPage: TEdit
              AlignWithMargins = True
              Left = 3
              Top = 22
              Width = 131
              Height = 21
              Align = alTop
              TabOrder = 0
              Text = '0'
            end
          end
          object Panel6: TPanel
            Left = 137
            Top = 0
            Width = 137
            Height = 59
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object Label5: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 131
              Height = 13
              Align = alTop
              Caption = 'Order By'
              ExplicitWidth = 43
            end
            object editOrderBy: TEdit
              AlignWithMargins = True
              Left = 3
              Top = 22
              Width = 131
              Height = 21
              Align = alTop
              TabOrder = 0
            end
          end
        end
        object PageControlSearch: TPageControl
          Left = 0
          Top = 0
          Width = 838
          Height = 189
          ActivePage = tabCustomSearch
          Align = alClient
          MultiLine = True
          TabOrder = 0
          TabPosition = tpLeft
          object tabContacts: TTabSheet
            Caption = 'Contacts'
            ImageIndex = 1
            object Panel2: TPanel
              Left = 0
              Top = 0
              Width = 790
              Height = 49
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Panel5: TPanel
                Left = 0
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 0
                object Label2: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 131
                  Height = 13
                  Align = alTop
                  Caption = 'Contact ID'
                  ExplicitWidth = 52
                end
                object editContactsContactID: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 131
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                end
              end
              object Panel7: TPanel
                Left = 137
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 1
                object Label6: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 131
                  Height = 13
                  Align = alTop
                  Caption = 'Contact Number'
                  ExplicitWidth = 78
                end
                object editContactsContactNumber: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 131
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                end
              end
              object Panel8: TPanel
                Left = 274
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 2
                object cbContactsIncludeArchived: TCheckBox
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 131
                  Height = 17
                  Align = alTop
                  Caption = 'Include Archived'
                  TabOrder = 0
                end
              end
            end
            object Panel3: TPanel
              Left = 0
              Top = 49
              Width = 790
              Height = 49
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
            end
          end
          object tabItems: TTabSheet
            Caption = 'Items'
            ImageIndex = 1
          end
          object tabAccounts: TTabSheet
            Caption = 'Accounts'
            ImageIndex = 2
          end
          object tabInvoices: TTabSheet
            Caption = 'Invoices'
            ImageIndex = 3
            object Panel9: TPanel
              Left = 0
              Top = 0
              Width = 790
              Height = 49
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Panel10: TPanel
                Left = 137
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 1
                object Label7: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 131
                  Height = 13
                  Align = alTop
                  Caption = 'Invoice Number'
                  ExplicitWidth = 75
                end
                object editInvoicesInvoiceNumber: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 131
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                end
              end
              object Panel11: TPanel
                Left = 0
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 0
                object Label8: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 131
                  Height = 13
                  Align = alTop
                  Caption = 'Invoice ID'
                  ExplicitWidth = 49
                end
                object editInvoicesInvoiceID: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 131
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                end
              end
              object Panel12: TPanel
                Left = 274
                Top = 0
                Width = 137
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 2
              end
            end
          end
          object tabCustomSearch: TTabSheet
            Caption = 'Custom'
            ImageIndex = 4
            object Panel17: TPanel
              Left = 0
              Top = 0
              Width = 790
              Height = 49
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object Panel18: TPanel
                Left = 149
                Top = 0
                Width = 641
                Height = 49
                Align = alClient
                BevelOuter = bvNone
                TabOrder = 1
                object Label9: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 635
                  Height = 13
                  Align = alTop
                  Caption = 'URL Params'
                  ExplicitWidth = 57
                end
                object editSearchCustomParams: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 635
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                  Text = 'page=1'
                end
              end
              object Panel19: TPanel
                Left = 0
                Top = 0
                Width = 149
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 0
                object Label10: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 143
                  Height = 13
                  Align = alTop
                  Caption = 'URL'
                  ExplicitWidth = 19
                end
                object editSearchCustomURL: TEdit
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 143
                  Height = 21
                  Align = alTop
                  TabOrder = 0
                  Text = 'Contacts'
                end
              end
            end
            object Panel20: TPanel
              Left = 0
              Top = 49
              Width = 790
              Height = 49
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object Panel22: TPanel
                Left = 0
                Top = 0
                Width = 149
                Height = 49
                Align = alLeft
                BevelOuter = bvNone
                TabOrder = 0
                object Label12: TLabel
                  AlignWithMargins = True
                  Left = 3
                  Top = 3
                  Width = 143
                  Height = 13
                  Align = alTop
                  Caption = 'Last Modified'
                  ExplicitWidth = 63
                end
                object editSearchCustomLastModified: TDateTimePicker
                  AlignWithMargins = True
                  Left = 3
                  Top = 22
                  Width = 143
                  Height = 21
                  Align = alTop
                  Date = 43371.000000000000000000
                  Time = 0.571546493047208100
                  ShowCheckbox = True
                  Checked = False
                  TabOrder = 0
                end
              end
            end
          end
        end
      end
    end
    object tabAPIStore: TTabSheet
      Caption = 'Store'
      ImageIndex = 5
      object Panel21: TPanel
        Left = 0
        Top = 0
        Width = 844
        Height = 233
        Align = alTop
        TabOrder = 0
        object Panel23: TPanel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 836
          Height = 59
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object btnStoreExecute: TButton
            AlignWithMargins = True
            Left = 758
            Top = 3
            Width = 75
            Height = 53
            Align = alRight
            Caption = 'Execute'
            TabOrder = 3
            OnClick = btnStoreExecuteClick
          end
          object Panel24: TPanel
            Left = 0
            Top = 0
            Width = 245
            Height = 59
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object Label11: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 239
              Height = 13
              Align = alTop
              Caption = 'Model'
              ExplicitWidth = 28
            end
            object comboStoreModel: TComboBox
              AlignWithMargins = True
              Left = 3
              Top = 22
              Width = 239
              Height = 21
              Align = alTop
              ItemIndex = 0
              TabOrder = 0
              Text = 'Accounts'
              OnChange = comboStoreModelChange
              Items.Strings = (
                'Accounts'
                'Contacts'
                'Invoices')
            end
          end
          object Panel25: TPanel
            Left = 245
            Top = 0
            Width = 192
            Height = 59
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object RadioGroupStoreMethod: TRadioGroup
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 186
              Height = 53
              Align = alClient
              Caption = 'Method'
              Columns = 2
              ItemIndex = 0
              Items.Strings = (
                'Post'
                'Put')
              TabOrder = 0
            end
          end
          object Panel27: TPanel
            Left = 437
            Top = 0
            Width = 318
            Height = 59
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object Label14: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 312
              Height = 13
              Align = alTop
              Caption = 'GUID'
              ExplicitWidth = 25
            end
            object editStoreGUID: TEdit
              AlignWithMargins = True
              Left = 3
              Top = 22
              Width = 312
              Height = 21
              Align = alTop
              TabOrder = 0
            end
          end
        end
        object memoStoreJSON: TMemo
          AlignWithMargins = True
          Left = 4
          Top = 69
          Width = 836
          Height = 160
          Align = alClient
          TabOrder = 1
        end
      end
      object Panel26: TPanel
        Left = 0
        Top = 233
        Width = 844
        Height = 242
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label13: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 838
          Height = 13
          Align = alTop
          Caption = 'Response'
          ExplicitWidth = 47
        end
        object memoStoreResponse: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 22
          Width = 838
          Height = 217
          Align = alClient
          TabOrder = 0
        end
      end
    end
    object tabDataset: TTabSheet
      Caption = 'Dataset'
      ImageIndex = 1
      object Panel14: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 838
        Height = 133
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object GroupBoxJSONDatasetType: TGroupBox
          AlignWithMargins = True
          Left = 556
          Top = 3
          Width = 279
          Height = 127
          Align = alRight
          Caption = 'Options'
          TabOrder = 1
          object Panel15: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 81
            Width = 269
            Height = 41
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 0
            object btnJSONtoDataset: TButton
              AlignWithMargins = True
              Left = 191
              Top = 3
              Width = 75
              Height = 35
              Align = alRight
              Caption = 'Convert'
              TabOrder = 2
              OnClick = btnJSONtoDatasetClick
            end
            object btnJSONClear: TButton
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 75
              Height = 35
              Align = alLeft
              Caption = 'Clear'
              TabOrder = 0
              OnClick = btnJSONClearClick
            end
            object btnJSONPaste: TButton
              AlignWithMargins = True
              Left = 84
              Top = 3
              Width = 75
              Height = 35
              Align = alLeft
              Caption = 'Paste'
              TabOrder = 1
              OnClick = btnJSONPasteClick
            end
          end
        end
        object memoDatasetJSON: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 547
          Height = 127
          Align = alClient
          TabOrder = 0
        end
      end
      object DBGridDataset: TDBGrid
        AlignWithMargins = True
        Left = 3
        Top = 142
        Width = 838
        Height = 330
        Align = alClient
        DataSource = DataSourceDataset
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
    end
    object tabLog: TTabSheet
      Caption = 'Log'
      ImageIndex = 1
      object Panel13: TPanel
        Left = 0
        Top = 0
        Width = 844
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object btnRefreshLog: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 75
          Height = 35
          Align = alLeft
          Caption = 'Refresh'
          TabOrder = 0
          OnClick = btnRefreshLogClick
        end
      end
      object memoLog: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 838
        Height = 428
        Align = alClient
        TabOrder = 1
        WordWrap = False
      end
    end
    object tabAbout: TTabSheet
      Caption = 'About'
      ImageIndex = 2
      object GroupBox4: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 838
        Height = 469
        Align = alClient
        Caption = 'About'
        TabOrder = 0
        object Image1: TImage
          Left = 2
          Top = 15
          Width = 179
          Height = 452
          Align = alLeft
          Center = True
          Picture.Data = {
            0954506E67496D61676589504E470D0A1A0A0000000D49484452000001000000
            010008060000005C72A866000000017352474200AECE1CE90000000467414D41
            0000B18F0BFC6105000000097048597300000EC300000EC301C76FA864000000
            1874455874536F667477617265007061696E742E6E657420342E302E338CE697
            50000030634944415478DAED9D079C5C55D9FFCFCCEEECCC6CEFBDA417524848
            B2698484F40DE50FFAAA140D4A55444411449AD2A2EF2B209617040421A022E2
            1B0BA6572021857420C9A66EAFB3333BBDDFFF79EEEE864D76676E99DB66F6F9
            7E3E415376EE997BEFF99DA79DE7E80892103C71D2D671C613CC6288CE00BFDF
            6DF5596A5D01FA5B5D7E0C1F1B220CD33936DD4066661B8BE00F920871FF7172
            5E9ADADF1791069DDA0340F8F348AD8DD9D6E56B3BD8EDCF6508317843E1309D
            E07A35C79444152245AF87F7C8735996A16B41AEA9EC9931D9F85EC509F8A034
            CA251FB6347842E1B2B3EE908F3E2593DAE311439A5E172C3225E9E7651B9D6F
            5E9A97A5F6789081A0006880EF7E66B11E7104CD1F5A7DF03C52D41E8FBC30EE
            45B926C3944C83FDB9F1B9B1B8278804A000A8C043C76DDD6BDA3D8153EE405E
            88517B34EA92AC23E1F16986D6A3F34ACAD41ECB5004054021EE3C6AF1BCD7E6
            4EE9F28775D46FC7FB3E180C13CE4F49225F294E75BD34313753EDE10C05F045
            9491057B5AEDDBBBFCB0C6E3CB2C8292147DA0A6C0147A7D72BE59EDB1242A28
            001273F7675DBE971B9C849AF609EECB2B4B6A92CE776B595AE8771372310529
            21280012F08363D6B6D71A9CD9F660D880E6BDDC30E14A63B2BF7E61195A0512
            802F6B0CCCDCD562DBD31D801711577B15C84822E1D9D946FBC6EAA21CB5C712
            AFA00088A06C5B93ADC91BC2BCB686589A6F0A6F9C5198A4F638E20D14009EDC
            79D4D2B4BAD95DE00D3306B5C7824426DFA00B742EAE408B8C2728001CDC76C4
            E278ABD965F2332459EDB120FC31EB75FE6F96A5595E9A985BAAF658B40C0A40
            04BEF35997F3A53A6752BC96E1223DE80971DD5795117CFE929C6CB5C7A24550
            002EE2F6A39DB6D79BDCE96186A03F99405021F03E3022C3F9DF63730AD41E8B
            964001E8E581E3D6B6DF9C73E4FAD0D44F68F20CFA906571393EE35E50002895
            DB9B02F59E10BE1443882A7392B36E415986DAE3509B212D008BF7B6FB375BBC
            18D51FC22CCD33B9365617A6AB3D0EB518920270EFE75DBEDFD6399219A26E33
            0D441B98F424FCE311999E2746670F392118720290BEB1C1E30C3118D9470630
            C29CE43BB3A06C48BD1B434600BE74A0C3F37F6D9E21F5701111304CF8B6F274
            F76B93F386447C60480880714383D717668C6A8F03891F8C3AE2F42DAF4C7811
            486801B8E15047E89D663736E040C4125A5996E65F3D392F55ED81C845C24E8C
            CA6D4DA17A6F08837C48CC0C3327FBCE2D284D48F731E104E047C7ACCEE7CE39
            D28678AB3D446274840933355509571D9A500250B8A5B1ADDD1F2E547B1C4882
            C2304C4D8139B06E4661C2C49312460072363784AC01064D7E4476F253F4BECE
            45E509E112248600ACAB0F1176BF078228838E6102CC8AAAB8EF3B10D7023073
            574BD79EEE00B683425401569C0579C6B35BAB8B46A83D16B1C4AD004CF9A839
            7CC8118CDBF12389C3DCEC14FFCED9C571191788CB0964585FE70E303AEC0A8B
            6886CC645DA77D4945DCF51A883B01485E5FCF0431C7876810A38EF87DCB2BE3
            CA12881B01F8D1716BD3B367ED4574C809978B451287D4241D712FAD889B7915
            1703FDE131ABEBF9B3763396F4227101C3049E1E93D5F5E8A8EC62B587C285E6
            27D4F78F75F97F7DCE894D3B907823FCE8A84CE7D3A3B3357D7E84A605E0EECF
            BA9817EB9D6A0F0341C4C23C303C3DF8CB71B99AAD17D0AC00DCF37917F3BB3A
            9CFC489CC330CCC3233383ABC6E66852043429003F3866657E75CEA1F6301044
            2218E6112A02CF8CD19E08684E00BE4BCDFEFF45B31F4940A83B10D09A3BA029
            01B8874EFEDFE1E44712150DBA039A1180FB8F5B03CF9D75606F7E24D1093D35
            2ACBF9D8E82C4D1C55A60901F8E171ABE3F9B38E21D7921919AA30E15563B29B
            1E1E9955A9F6485417809F1CB736FCFCAC034E70C5EDBCC8D0816182644595EA
            F52DAA0B00595717C4F25E64289295A463BA9756A8BAF0A92A00C9EBEB83413C
            851719C2A42511B76B69659A5AD7574D004C1BEAEDDE3049F8BEEB08C2C5D44C
            43F0E0DC1255DC015504A07A576B786FB75F7DF7034134C2A23CA37D4B7591E2
            FB06149F84F3F7B4B5EEE8F215297D5D04D13AF70FCF687B6E5C8EA23B081517
            00DDBA7A06FB7920C840F4840987153E7B405901585B1FA057C4621F0489407E
            8A9E742E2A576C5E2A76A1BCCD8D3E4B20AC99124804D12A6546BDAD6961B922
            DDAE15118015FBDAFD6B3BBDAA173D2048BC705349AAF3CF53F265CF92C92E00
            8F9FECB63C79AA3B57EEEB20488211223595B2BBCBF25B00EBEA42F43258E68B
            200231E9755EEFB20A59DBDFCB2A00C3B63705CF794258E9872022199796DC75
            FC8AD23CB93E5F3601B8E548A7EBCD2677AA5C9F8F204385874764DA578D95A7
            B9A87C16C05A6AFAEBD0F4479058C948228C6369A52C7349160130AC6FB00518
            46D3ED9011249E98939312DC35AB58F24C9AE40270E3E1CEB6BF34BB0B95B92D
            08326460488DF45680F416009AFE0822173E2A0226293F505201A8DCDEE4A8F7
            84B0B51782C8C4978BCDC1BF4F2D90CC15904C001EABB5763E75DA215BBA0241
            1080A1AE40956416B66402605CDF10F0310C6EF4411099C937E83D9D8BCB2549
            B14B220057EF6FEF7CBFDD8BAB3F8228454DA52473571A0B605D5D987E1476F8
            4110854866C2A1E08A61315BDC314FDA891FB53A3F75F8556B6A88204395EB0B
            CDCE35D30A62DA3118FBAABDAE1E1BFC20881A304C98AC88AD83504C0290BDB9
            C1650B3058EF8F202A715581D9FF9FE90546B13F1F9B0580AB3F82A80BC330D4
            0A109D16142D00255B1B832DBE306EF545109519959AE43835BF2C53CCCF8AB7
            0070F547106D108315204A00466E6F0C9DF684B1DE1F4134C27843C8736CF170
            C1F138711600AEFE08A23D44140709FE81C22D8DBE763FB6F74610AD21A67D98
            700B00577F04D1284C98083C59489000E46F6EECE80C84F3D5FE9A08820CCEF5
            8566FF9A69FCEB028459006BEBC2448735FF08A2558C7A12F22DE37F9E00EFC9
            BC625F876B6DA707ABFE1044E3DC5696DAF9DAE4FC023EFF96FF6A8E077C2048
            5C60D491806F7925AF403D2F0178F884CDBDEA8C5DD6134A100491109E29415E
            FF286773A3D71A088BDE70802088B25C9661F01CB8BC84D365E7E70260EA0F41
            E20B9EE5C19C0230EA83E6FA53AE6085DADF07411061DC5E9E66FBC3A4BC9C68
            FF8653000CEBEA42010CFE2148DC519ACC049B9754456D21CEED02A0F98F20F1
            0B473030EA5F5EBEBBD5FB91D58FC13F04895316E6992C5BAB0B2356EF46B700
            70F54790B886DAFFC1404D65443700050041129D286E40C4BF98B1AB35B8AFDB
            8F2DBF1024CE599A67726EAC2E1CB47D78640B004FF94590048171909AAA417B
            0646160034FF11247188E0060CFA8795DB9A5AEBBDA122B5C78C208834DC5696
            667F6D725ED6C57F3EA800646E6A08D9830C9AFF0892205498923D0D57960ED8
            1B30B80B80E63F8224184C88D4540D6814324000EEFBB433F042833BE6534711
            04D116F7677A5A9E9B3BB6B4FF9F0D1080DCCD8D96AE403857EDC12208222D57
            17985CEF4F2F4CEFFF670304404FCDFFB0DA2345104472720DFA70D7E2F20B6A
            7B06C600D0FF4790C4241C0E91AB865DE0DE5F2000371EECE8FA4BAB2747D8A7
            0AC7A0D391CC641D81FEC28E20437CE1F8D31C3D1D7B56B29ED0AF41DC2186B8
            42F27F07B8A689FEC7487FC1750B5392D83F63E8A53BFC61E2677AEEA597FE8A
            C35BAA0A30014C493AF6BEC23B99499F6901DC57FAE76DFE10FB6CAD81307B5F
            3D89705F2FAA07B8E03797ED6C751DB0FB65EDFC3B3D2B85BC74490E999299C2
            5EFCB82B401EAAED26EFB77BD4BE35BC293626919727E490C57926924A5F9E26
            6F88FCAACE417E75CE21F90B42CD367265AE915C49AF7519BD67A5463DC9A32F
            687AD240E30D5E560B7D595B7C21B2DFEE27DB2D3EB2BDCB47DAE98BAC34D974
            22D51498D87B75CC19209B2D5E128C706FCA4D49645E8E91CCA1BF46A726B393
            B007869C7405C92FCF3AC8A7F433A40216A099D92964519E91CCCF3591E1E624
            564C5393064F8AC1E407816DA4CFF923AB8F6CA2DF6517FD5FA702A22F35FF55
            6C0EBD37B5E0BC1570E137FEF7691F4936C876ECD7847403F9785611C948BEF0
            B230696EFFAC8BBCD1E8225ABFA515F465DD34A3908C4DBB305102E37EFC6437
            79FAB43DE66B94D049734DA1897CBD348DCCA62F6A720C4731C0B876DBFCE4AD
            6617F9171559102BB959966F227FBE348F15AF3E8E3802E4DA031DA4CED3737D
            986C5F2A4A25DFA94C23B3B28D245AD1095834371EB6907FB4895F240C74859F
            45179F9B4B53E92448257986D8CA5CC0E2FB37BD9FEFB4B8597153C20294045B
            673BB9F1B2F3457E17BE5932FBFF6F4DCE655FEAC10852F3F58E4FADE4CD26ED
            8A409539996CAB2EA02BC6E059525881AB7634934EBFF0302A3C881174F57B70
            78067B8F22AD46B1E0A1E3FB5BAB9BFCFC8C9D9CA02BAB1CF7B9948A57ED1525
            246D90F1EFB4F9C8C2BD1DE42A6A193C3F2E9B0C33F3CF36B7D37B3AF6C31662
            0B08BBB7663A8EEB0ACDE4D19199645C9A817599A404EE21585CFF7DC6415653
            91153A3E55E8E706282A00A7E7979011511EBA9F2AFD9D9FF58880D628A52BFF
            07D58564646AE4F1C3A39FBFA79D351385904357A367C664915B649AF817032B
            2AAC5C0F9CB08912AB68DC5D994EFEF792C1C348F072C16AB9889ADD422722FC
            EC927D1D640BFD79BE2CCDEF111AB03C95A0890AC14FA915F8BAD62DD9C104E0
            BAFD1D967FB47B64CDFF1F9E5B4C2667447F186049DD41DD813F366A470460F2
            6F9A5E402EE17891C09599B3A78DECA126371F60AEDF50924A7E352E8714A428
            5F790DC1AD1F1CB791D5125A5DBFA593FF9ECAF4D83F6810AE3FD049FEC12356
            04FEFCB374E27F9D9AFB6A9C63B7A3CB47EEA2EF3058595AE4C95199AEC74767
            B30FE9FCFD19BEBD8939EB91D73F7C7C542679625416E7BF8360D1ED9F7649FA
            628AA5925A2C5B6714445DF9FB68A62BC0F01D2DAC25C345365DF561A5BCB144
            9D97B40F18E9FF51DFFA2E7ABF2D1298AF720900582D933E6A25A7DC912715DC
            C72BF38CE44F93F3D8E0A39A404CE0FBC7ACE48FF41DD65AE6609631E8DCBD70
            04DB1FE0FCBB97BCBEC14DFD70594FFF81E8EEEE5985643C0F930C261158026F
            35B9551301884EEFA066FF081E931F5ED06BF777B211622EC0F77D7F5ABE62A6
            291F60625DB5BF83D4C6B86AC92100F0FCC1ACBE838A54A477015EE4BBABD2A9
            3595CD46F9B5004CFCDF3738C97DD4CA0A684805AAB30CCCDE3925ACC9F9C59D
            52A800A88C2AF3C619DCE63410601872E7A756F2860A310108666DA6937F7C1A
            F7E48734D1AD47BBC89FA95FCD05A4F2FE4D277F690C2B143C2830DF61C5861A
            00F0A7C185C84A8ECD8D8074E152EA671F76884FB9492D000DDE1079B1DE499E
            3FE7886859812BF5336A5942A02F56E059F6A5F7A036204D82980C58585F3F62
            6183B09AC0E7F590EBC6B0E97EC50500001F6D3315814919DC2200F7EC3685DD
            0148F56D9E5148C6F098FCF052DE4E450AD26C5C40EDC3F6EA02C113B53B1826
            FBBBFDD4BAF0910FAD3E72C8EE678B52FADF105D6F61D28CAC147245AE11DA40
            9189F4FE9A0446DB400420D87644A40888110010FAE3CE205BB3B0BBDB478ED2
            6B37D3896FA3DF1BBE62B4C513E6E77374D5FF7E5506EFEBF561A79F7FC01E20
            DBBABC6CAA14BE7307FDFE7D97833B0756EBF834035B37B098ADC53088722FD6
            7678C9570F756A275DD81B0864FFF3F0E79DFE55756E45EDD1222A025BABF959
            0250E1063181B715700760F26F1760F67F8BAEFCEFF058F9C19D801A08F85F3E
            C0F73C4BCDF217A909091650973F2CE8BBC3C4807A82EFD189F1ADB234414146
            287A99B3BB2DAABF1D092102007301827A503F51EB0A442C148A04BCBC0F0CCF
            203F1F9B4DF87E3B8872809BF302B52820250A969490CB4201D6B27C33F911BD
            2E14B5250BD057B0106FA19680D0EF2907CFE4B81B1F9935AEA247006A6DCC2A
            090A588402EEC096EA81453583D1171358DDC43DD9C4021306263F9F95DFD73B
            F9FFC263F2C34BB36366216BFEF30126E023B5DDAC55E195C07784829CFB8665
            B03506469E16C167CE0099BDBB9D3882C202837C052000D99E182DBB6B0ACD64
            CDD47CC2D74AAFA756C583276C640D35C9F9046AA301D7842AC267C76691A93C
            9F2B5CF1C95376F2B353DD315D5B0A1EA6EED2AA31D93DD112F3C68666EA9F94
            A8311098749BA83BC0272006CFEC5B9F4260507A77A08CAECC1B79A4FA0098FC
            109B58CDC3EC87F9F68789B9EC2ACC05AC88FFA42BE2DD9F5B499B4FFA8C0C08
            EDEB9372C99C6C7E67BD403D06C4368448001F01806777EF312BF95D9D53F477
            8162ACFD738AD81A0A2EC0C578B5C1C5D63DB82536C14108405C21BBC5275E00
            EFCE35073AC9A64EFEF50C72705745BAEFE589B92676C4A60D0D8C142B8D580A
            A979BA95AEBC7C44001EE66DF4A57CBB593A7700CC7EB8FE281E663F3C408849
            FCA9999F25726DEF2AC5B5F0824FFF836336F26AA353D6B41144C89F1D974527
            6906E79860E25F475FD67F0BD8A7C14700208631EDE336D1DF13E21ADBA94535
            338B7BE585F8098818ACFA72BEE16005AC999AC7568B72019B8C267FD4A6CA1E
            8D3ECA43EEEEC6ABC7656B420080D25EF37B344FF35BC8248C065820609E8FE6
            31F9C16CFC264FB31F8000D2F179C5EC35B83EF7CB872C8A6D8882890FEE0035
            01396B10200A0FF9F76E9EAE001F01F82AFDAEE07F8BE58E8A34F2CA04EE9A35
            281F5EB2AF5D7440532845C69EE0F6441E0BD91F7AD39A6A01B1A8C62BCB7A13
            A6EBEAA814A97F064079AF19CEA74E000229B77E6A61EB04C402D15CB81E9F6C
            84AFB74C7935CF9424DCD89F53FFF0C7C3A3A7A64078BF4627C4BF14DE0D0922
            F0D0884CF2CC68EEC2AC2704F8AD5C020091F7511FB4B0710E314006E91815D5
            5C0ED31F3E7FB18293BF8F925E11E07225C113A9A656D0013BBFAA51A92948D1
            853A1655F4C63035D404A4A0D71DE0A3A22002DF3A6A612D01A15F00C4060290
            6378AEFC770898FC005834C7E7950CD8F9D81F7809A064F43595CA9E6164BFE1
            B16243FE7A049DB4AD3CE2125C0200DBBF27508B42ACC1F9D351996CCE3F1AF0
            BCAEDADFC9EE3B5003702521E393CF917981A2B1E59F74A85329180CF8C93523
            8D9A13004048341E1EF6ADD494FAB3001128EDADF0E3EBF3F77DBE106013CA0F
            8645CF4D8370AD3C62111464931AF0A777CD2AE48C64FFE28C9DFCA496DB0AE0
            1280F73B3CE41A3A39C500750EF50B4AFAF50B1808BC033F3E61637B08A8096C
            445A3BAD206A860276C0CEF8B89D8D89A8424DA54E93020094F716E3F04D1142
            4CE06D1E9394AF8906C0E4BF8367914F7F60AFF9C92B4AA246A8615FFEA49DAD
            6C1E5A6D6083D6EED945C41C252A08CD30467EC0BDCF814B00A034F63BD49A12
            03082A086B34A0A067C1DE76D5BB4CC19DFCFD841C72674574EBEADD5637EB02
            AA0208C01D472D1DAF36BAF263FF34E929EE9DAC7CB2037C2A06CB7A9B79F029
            EF8D656BF24DA5A9EC869448C0F86E39D2255858E4E4D7E373C8BD55D15FD62F
            1FEC64CB5AA3C12500104B80988250A07EE1C09CA2A8C20D937E06F5AB8F2AEC
            F747021602E88D102D5EE1A47E2C08AB2A1901108095873BC3AB9BDDDAD83D31
            082002D084039A3970012270FF711BF94D9D638008C0E4DF56CD2FDAEF0BF756
            1E8ACC3280682DCA3345FC7B28B2B994AEFE5AA90A05E065857E0DD1CA94A146
            01D282D1E0120028705A7546B8005C4AAD9483738BA3662D6035BD81AEA61ABA
            AD6CA5E2FF8C8D6EB54041991AFB5DEEAE4CABD7511F941112DC52037007B6CE
            E0972284C0E08FA808FCBAEE0B1FB047440AA98848EB4E0C0604319BAE2C8DBA
            23ED1B47E0F3B577CFB9262F0401C77CD8C2367215FB1990461563553D362A93
            3CC911FC83D5FF936E95FCE90840C9FB192AACD11ABDFCA7C34BAEDEDFA1F8D8
            56161A7C71210040596F530E3E2942001A5DFCFA9CA327D5C733370B93FFAECF
            62DB7D7875A199FCFBB2C81E15A4A74653938F6F5E5D49A6641AA8991D799585
            693F934EB27D5126991C0200E3F9849AFFD14AA9A109EA8C5D6DA2577F1DE9D9
            ACB53CDF442ACD49ECEFA18E002AF6F6D0EF1B4BE9F0EAC9B9E41BA5912B4141
            50F3B634B1456E4AB2B22C8DC48D0000A0A6E00EF01101785E4F9EB6939B4A52
            79D7F60BC9F347E297D4DC838D2291001F1A7C692D022F3DF8ACD1B2234FD17B
            0A9B7722218700805575667EE9A09D90FB783086C83F58982F4DC82135F9E601
            517B90E9E3D4655B49ADB6FD22A3F54BA8A840BD4934A6ED52BE2680158079BB
            DBDC1F5A7DB236029192B2DE141E9F0E3D7C09B066BFF068FFC5C0BB03FEFFC2
            28FE3FEC6D57BAE84708DFA5931736D94402AA15AF891207904300C0FF07CB24
            5292023C9299228B6AE03D824AD0328E6A4DD81405B5051F0AECF708F089AFC0
            FE8F97EAC5EF8D10032B00645D7D0BFDFFC58A5E39462AA16290678A900B58F9
            BF436FBE143D08A1E6E7345DA92A796EF98D47A0E1E9BC3DED11FF5E0E018036
            DE7F9B1239AB02D585C5DB9A0537DC48A18AF2219DFCD53CF61400E0BE4DD9D9
            CAB67E130208D7FED945AC8B1109D89E0C6EAB92C4AD00005077BD95673E3F12
            E0737D9B9AFDAF4B5489074535EEA5E5AAF6F8931B686C71559480951C0270FF
            B00CB6C967240E3902642A9D9842B9AB229DCDD50B01E24310B517CA5B93F3D8
            26A591E09361919A1E01585BD74A74BAA2D83F4E79C06C835D617C2AFA2E464C
            792F17E04B362C288DFD8334CC23D4FF8FD63B420E01E08AAB405391EB054E1E
            F0F5F7CE2EE2DDA3A10F08DE96516B4368679F47466692A7A3ECBBD865F391B9
            BBDB057C62ECF408C0FB67032429493A875A612A7B2B06F9A408FB9022DA3F18
            505107ADCF1315E89537E1A316521FA57BB41C02F0C2F8E82DBFC4ECAC832ACD
            531C453A8301F186E9BB5A05F74DBC854EB6372645DEC178D0EE27D363D8222D
            863E17404B7513A2803AF67D54CDF976860193FFF6281D66C502FDF860554954
            9E3DEB609B6A44430D01F86D9D936D3022043E9985C180857FD66EE1F5069006
            84746024A0380CE20B4AB60B9B9896EC887B01805D775B79B615EB032C8095F4
            457CB745DA1E83D0236E5F020A00DCA3BFB6B8D9541857AE5A0D0180262AD0A1
            490839BDFB35849E1128D60280D5F6CD2816C0217B804CFB58FC2E49315C12B2
            77E8C8DA7A0FB5034CB17F9CF2C41208F4F636F7F82BCFE61E7C807EFF67E7AB
            D2594D36A07B0DD4EEBFDAE0E4B53AA9110310134083857FDFEC626A3D0A7B77
            200650B1BD396A35E460400FBE68BD173EB6F9C81C556200719C05D830BD80CD
            118B05CCB995472D82B7FA46028EC4B62E2E8B7ABDFF7478347752CCC5C0BB7D
            DA1D6073DED08A5C48159C1C02C0B50BF0205D3D2FDB253C0B003BF55E169805
            78A5C1C5F670100AACFE2BA3F4858496655F52B8402C6E05A0B4B79B309FDA7E
            2EC0A485DA7C29DC01A8FF6F59581AD5AC2CDDD6CC9E269BA8C821005F294E25
            EFCA50070056C01EEAB24DE39909801D7B97EE6CE3D518E5E2EB1C9C531CB5F3
            14EC5DB9EF18D60170022DA1B6CF2C600F6B900A7007BE71C442DE6B8DAD424F
            CF9A95D1534B371CB648EA76680D3904600A9D38FB392A01A1BDD64111958090
            3D82622028338F067413BE96BA19424E27EE03320DA7AF2861CF838C04D4A3BC
            DCA04225E0DCADA7ED3B7D06E1C7AAA8004CFEF5D4ECE7E3B781D90A7B0120FD
            C2ABE1673F4B201620D5734B14530F0E87B8F9B04A0D2014201EF7024057E817
            A92BB022DF3C4064C0A6805399EE3966E57DEAF3C54069F89619D1F70254736C
            B29283B8DA0C24E4FC0030EBBF4B15F5D54617FB73DB786609A470076E2F4F23
            AF4E8C1CEDEDD952DB2AF8C08D7841AEEDC087E616478DF7C03E80E931EC0684
            B5791C7DB7BE5C64668F54D353E3B8CE1B24FFA0BE3904E862E9DDF0E664EAFF
            47D90D082E4CCEE626C55BC3C58D0014F54E62BE9D7CBE7D516D7F79EF715F7C
            3610C5EA0EC0CEC3639797443457E13D020B806F6BF178432E0180833F9F8A12
            4587FB3A8BAEA27B35D60F00AC56D80814CD7AE12AAF968BB81080A2DED6DD93
            7944FB61F2C3AEAAC1BAEC428A0E76EAF111015FB8C71210D3BB1E1EF3B17925
            512D8E13AE2099BCB335E6E3A9B4885C020001B44351E20080163B02F1E963B8
            52A5F670D45AB5EA6E39DC197CB3D9ADC9ED6BC5BDA93EBE931FFC3438022A12
            6C2B709EA7FE4260098E741613B0E36A03052F289422BFAA70D0271AB0330EDC
            2BF0B7E1309093220EEB04E41200E80978706E51D4E0AFBFB727A0D267014402
            B241D01A3E5A7B7030FFC77CD0CAD65B28CDCF466536EBEE3A6A6979B9D1A5B9
            2C40B1B1E7F4603ED1FE682BFFC508D94004960088C0DF5B851D2B05D6C6E797
            17137314B30F1EF8C48F5A49A7C80332A404C6FBF7A979644A460ABBC2C2C4DF
            43FDDEFFA2ABA9D094975C0200FC90AEA6CF71ACA61FDBFC64FE9E76C5BBEB5C
            0C4CF9DF4FCC257794473F1312ACCCAFAAD91598FD3F1A2B0706BF694B35BF36
            5EF0A0A1CDB490C335E0FC36E82C349CC7396E6262027053FF3E359F5C5F14BD
            CF0A3C7C480BAAE90940492CEC5F184C10C19F9EBBBB4D902520A700C058EBE6
            97463D6C05E4F4C1E336F2DC3975CF0558966F22EF4F2B887A7C38DCD769BB5A
            D5B358B4280005293D663FDF54DFF78ED9C82B224C69E8FBC6EE22E46109C083
            BA998A8090142188D7016AB2466B0E0A37BDAF77A11AC0C8DEE0A8509B4D0560
            B780F4979C0200C029BC8F8F8A7EDC1A44EC977ED241B6AA7832109CB3C0B5CF
            00C6B744AD938118BA72AEA8D26B4A00849C03002BFF3D9F8B9BFC7D083D81E8
            6601EE003CFA37389A41F6FFDC588B9084024F1E2613EC538FB61F0EB6D9FE41
            807525B7004050F81875AFB88E05870357A0B61E8E22531238E91A32565CFB53
            62295E9202EA9DFA42CB2B7B8E0727FF3917247ABDAA81C082DE1BC767F2077B
            CD7E212F6624C012F8805E97CFB1CE708437A4F0D6B4F19BAC700459EDBC12CE
            73E3A1CA0C5C0121C770C7028CE627237BDA6C73ED8615DAC25C6E0100BE5D91
            CE36F1E4025A77ADA02BACD09D7B6281601F58AF7C9A8CFC91DE835B45741692
            8A0B4E0756FB78F002B6C22F9FD78D8315F35E6AF64B593609B1804D0252847D
            96001FBE4D27C38B745270ED3A87FB7F0FD42FD01743CE47016DCB9E1993C5A6
            A7B8C604C20427F90AD9BBA08400C077D85C5D40E6661B39FFAD8D5A0270B623
            5FD116CBB4AC1476BFC2081E0B09DCCF293BDBD4390DA8978B04A03EE80D1355
            2C0030E936F13CA21BCCFEEF7D2EEDE4EF833D398867672118C74D87F9B903B0
            C2423008FACD730113FFED1617F92115388B0C670682C0BD4E7DFE2B72B8270E
            C0F740D0FE28210000642EF6CF29E2D5D107EEEB0B750EB6A5B94DE2FB0AE9C9
            3B2BD2C8B363B3D9542A1720F4B0A700CE1B5093891906EBA79797E4B2234E5F
            BDF394B3A062A4D28380683FA4FA8496F7CA05D484438B68BED9819B78BA03E0
            DE40A47D188FCF05E0E050080EAE69734BD22106564C6880F9B3D199EC96653E
            40751AECB1179A4E534A0080EB8ACCE4BD29F9BC3B41D5D3FBFAD0091B9B7D89
            F5BEC25C9F432D905F8FCFE6DD57102EF94CEFB90A6A07DDA03FC1AA31D93D16
            C0C3B536265AA3473980C9BF819ADD53F8ACFC6C914F6C013FBEC0E4DF222045
            083101AE0333011039D875C615BCEA035E10A86F7FE15CCF390276117B074078
            BE56924AEEADCAE095EDE803D25257EE6D275D22564B250500F8C9884CF23475
            6984F4F5F9DC1920BFAB77B2E22DB4CE01E239350566F2BDAA747239B5A4845C
            F79DD69E8D605A28007D644446F099B1390656007E7AD2E67CE2943D2DD60FE5
            8BA0C94F57A07BA9D9FF7B05ABE6A0D128F41BE0532C042BC94D873BE9AAC22D
            02B3B253D833E3F98A401FE08B8308C0AA0CA2004544767AE1FEA5C4906ECCA6
            8F13EE2DF4B987D5B1A6C014350D3918B5AE20B9626F1B69F389339595160058
            89618FC043C333090F0BFC02205DB8ADCB4BD6D1FB0A3DFE9AA81840A71F7728
            CC0A705A929E3DD30F22FBE0E32FC93391A5D495E30AEA0E066C23868345D43E
            B6FC3C3595EC97F8E29B28940A2CEA4DF5F12DF291CBE7E742C8062278A837F2
            74074004D65111C81628027DC04BEBA22F28581FD09A1ACEAFCBA79F05C53160
            EAC34B2BE2FD6439ED0E92659F74B0FF2B16A50500E84B69823520F6BB0320A8
            A07BF0DEC16448A1E2098F09FC7CB11F0B9F03EFC5CA2316C1ADC465658000AC
            AD0B139DC0E54220F0D2C3A4E2D3C64B8A3C7FAC801B00310A3EBE3BA408AFD9
            CFAF61046C37850344F9C6049400CC7ED891D6E88D2D32AD860000F0E2DE43CD
            F2E7C7E544ADBE531AE8587CFF091BEBC66A0686749315956C4DF5F95B95B7E1
            5CC012D6CBFA463E414DB5C7476672FE3B50E2FB8EDB143F2B6D306092423387
            113C2C0108DEC1C9BF1E1E0F1B72C6705A0C948CAAF9BEC248DFA3BE29A4CA9C
            12441CD512803E16E619C91B93F2D880AE9A40F3D0074F74ABBA804502ACD0DD
            B38B2FB400866F6F0A9EF58464BD6B07E71673FAFD604E7F97E7C61EA528EB75
            07B8620230EFE7EC69E3DD3906562AA81380821CA1710129806AB9876ABBE9BD
            76C6D4F0A23FBF199FC306C82221B70000702FA1F602CE1454DA1A8077604FB7
            8FDC42BFE7499778574A4E96E6196D1BAB8BD84AAAF3B7E72B073BEAFFD6EAA9
            90F3C270124B349FDAD79B5AE31355571AB004204518EDE04F98430BF6B6930F
            BA849D200B5588B0CBED4B456645AC0198ECD090E427B5B6984DFE8BF931F5C3
            7F312672E30E7093DEEF90FFF9829C2ECA33B116891487C8F201E2310FD37BBA
            BAC9ADFA6EC4A8F4FAFFC085EF9BCC81C0B7A9C97B73840312A1A32BAC0EEF8A
            68C2A114205E503118294508D1FAE13B9AD917410C9056FAF1880C36DA6C141A
            D2E601DC63684BFECC193B7B10851CC0294D50F330D8CA0BC54DC3B637B3478C
            29058CE3E6D234B6F211FA4AC821B0E73C4136AD082EAB5B4B81BE48A8250010
            FCFB80AEA2991715A3C0CA0F5D51B43CF9FB000B60EB20D90130FDA0D2EC67A7
            8455CE5D0C3C904A2A306046834500D908A1A9BCFE40B6004C51B8B7904D81FE
            0372BFA20F532B00E23DFD4500C4E7CECF605F813ACF18B203901E05970B0416
            B251B1385D701FA1572014A6ADEFF06A7BC5EF4FEF2EC0BEDF5EF066E9FF59EB
            0EA798CCC23F953F33B353C82B1372C9F8F464F6E2759E105B03BF5EE5D24821
            C0AEC55726E4B0013C3D9D9CD0E073155D557F75CE21992F0D801500B5E55717
            9AC862FAD25E9A69604B5FA161251808FD1F1E5C164428449F2FE4B3A1C3EC06
            7A4F212B01C14925CF9C83B15D996B628381B021EA9833405EA0F7E690463AF5
            401E7F7CBA815C53682673E8FB383923856427EBD86779B128B0F795F4A45CE1
            7BC0D668D8B475907E17A9CB8A956004F1DACFD47CE1A35D2000577DD2C1FC47
            01FF0C2E0A2B5B12BDE175D47C8A13ED1C001488E41892D8EFA0D4662A58B960
            1B33084166BF25165E46A8DC83937B6D09DA71584EE07E16F7DED7FE40A0B493
            FEEAF0873451C1172B5F2D4EF5BE3B35FFFC227F81007C63DBE7CD6F79D313EB
            703B0441BEA09FFF0F0C742E35D21C04411069D1314C90595175411E7E8000E8
            A900A00189208907756FFC5D8BCB2FD80B3E40004ADF3BD0D49C965FAAF66011
            0491961A93CFBAEECAD1171C5B3540001E3EDAEA5ED5E89735138020880A5CE4
            FF03832798D7D58723FE1D82207108E323355503DA520D3AC933373504ED4146
            93A7052108229CC919C9C12397970ED88833A8007CE56087EB6FAD9E54EE8F45
            10242E18C4FC07229BF9980E4490C441B000FCEBA487188CDCAD6C1104D13405
            218FB3E3EAB11983FD5D440198F371AB6B97CD8F6E0082C439B794A639DFBC34
            4F9800B0A01B8020710EC3909AAA881B1F3904A02E44FF89F2AD6A10049104A3
            9EB87DCB2A2376FC8E2A00F3F7B4597674F97209822071C9772AD39D2F4DC8CD
            88F4F7DCC53EE80620489CC284A8F91FB51F1AA700A46D6C6034D5CF1C41105E
            5418933C0D0BCBA206F23905E0A6431DB63FB778B2B8FE1D82201A2342EEBF3F
            FCEAFD153834044110E930EB49C8B3AC92B31D32AF493D6A7B93FF9427C47D9C
            0F82209A60719EC9B5B9BA309DEBDFF1128087769F6CFD85D558A4F697421084
            273CCC7F80B7599FB5FE5CA09B91F7E830044162273BE076D8AE1DC77D061F11
            2000B71DEDEA7EADD1C9EB43110451119EAB3F2028B0A75F571F0E63A31004D1
            2C79067DC8B2B89CB7A52E6832AFD8D7EE5DDBE9350AF919044194E36B45E6F6
            BF5E56C03B5E277C35C7FD0108A2490C3A12082CAF4C11F2338205A0646B93A5
            C517C2FD0108A2316E283107DE995220AF00B0E0FE0004D1160C13262BAA04F7
            F1142500A37634394FB94369627E164110E9F946696AE0AD4BF305ADFE80F888
            3E5A0108A209527484F12FAF141597132D00C3B737D9CF7A4219627F1E411069
            B8AED0ECFDC7B4025187F9C496D3C74D4208A22A463D09FB96558A3EC323A6C9
            BB786FBB7FB3C58B9B84104425BE569CEAF8EBD47CD115BAB1AFDE6BEB42D40A
            C0BA0004511A8678C98ACA98CEF18C5900AEDDDFEEFA57BB17DB872388C23C31
            2A33F0D3D1D98223FFFD91C47F37AFAF0B7918B40210442932427E8FE3EA5131
            2FBCD205F0302D88200A11BDD7BF1024138091DB9BC2A73D21CC082088CCCC4C
            23BE3D57544A726C9FB413765D7D88FE175D01049109938E305E91453F8321A9
            00FCBFFD1DBE7FB67B620A4A20081299BB2AD25D2F4FCCE5ECF5C717E94DF675
            752EFAB19815401089294ED1075B17954B5A77238FCF8E0141049194646AFA07
            2534FDFB90450066EC6C75EFB3FB632A504010E40B96E71B6DEB6714E548FDB9
            B245ED4DEBEBC35E06FB072248AC6427E9BCB6A515B22CA8B24DD0C76A6D4D4F
            9DB697CA775B106428C084498DF0461F7C9175855EB6AFDDB3A1D32B49BE1241
            86220F8DC808FF626C4E7C0A007B81B5754E46A7C3EE410822904BD293439FCF
            2B95F5301E457CF4E4F5F54C10F30208C21BB38E097B96CB67FAF7A188007CFD
            70A7F5ED6677B612D74290B8876118B2429A5A7F2E148BD24FFAA0D97ED415C4
            166208C2C1CAD2D4F0EA4BF3655FFD0145D374A55B1B43CDBE30EE1540900894
            19933C4D0BCB14ABA4553C4FAF5B571F6270C310820C203B591FB02D2957742F
            8D3A853AD84C14412E20893AFE2189F6F80B419549B8686F7BDB168BB7508D6B
            238816797A7496F3D151598AC7C8545B8517EC690B6DEFF2A12B800C796A0A4C
            8E75D30B4577F68D0555CDF0B44D8D1657308C078D22439665F926B26146A16A
            F350753FDCB4BE3EE06588ACD54E08A2457292F536EB9272C977F80941750100
            32373532F66058ED61208862E426EB035D0A47FC07431302C0B2AEDE4FFF8BA7
            0C21094F76B29ED896946B62EE696210E7C153869004278B4EFE6E8D4C7E4033
            03011EA9B5599F39D59D89228024283E52234D3B6FA9D09400008F531178F2B4
            1DF2A18AD442238842F8E9E437AA3D888BD19C00004F9DECB63D76AA1B44002D
            0124EED1314C805951A57AC06FD0B1A93D80483C462D81A74E537780A03B80C4
            310CE3272BAA34B7F2F7A15901001EADB5763D7DCA9E4E743ACC0E207147B641
            4F6C8BB513F01B0C4D0FEE3C6BEB025404B05808891BB292754CF7920ACD5BAF
            F1210014E38606C617C6BE6288F6A1933F44277F5C2C5871230040C6C6069F23
            C46832988220408E416FB72E2ECF527B1C7C892B0100D23736743A434C9EDAE3
            40908B999F93C2EC9855AC79B3BF3F712700C0CC5D2DA13DDD81B8BAD14862B3
            28CFE8DA525D24D9A9BD4A119702005CB1A7ADF5832E5F91DAE340863CE13B2A
            D23A5E9D9857ACF640C410B70270FE0BACAB0B324487558388E224EB882FB85C
            5BA5BD42897B010072363584AD412621BE0B121F941A93FCCD0BCB345BE0C397
            849934D5BB5ABD7BBBFD71FF4010ED7363B199F9CBD482848841258C00002B0F
            5B1C7F6A71A587B05C009101938E30B756A437BF3821B75CEDB148454209401F
            582F8048CDD8D464E6C4FCD28458F5FB939002002CDADBE6D962F1C5758006D1
            020C736D81B9FB5FD30B55EDDD2717092B00C083C7ADEDBFA973E479C3B8AD18
            114E46B2DEE758529ED08B48420B401FD377B6F83FB107704721C20BBA5A3057
            E41A7DDB671699D51E8BDC0C090138FF65D7D60518DC55884421CFA00B5B1657
            0C99BA92212500C0B27DED9E0D9DDE8436EB1011300C737D71AA63CD650571B3
            91470A869C00F491BFB9D1D7190863A60021A35293DDA7E697A6A93D0E3518B2
            02003C79AADBFBE4295B7290C152E2A18859AF0B7896550CE94560480B401FCB
            F7B5D9D77778D3F1C8F22102C3046F284D75BE33A52021537B42C017BE1FF33F
            6EF5EFB0F9315B90A024D1997F75516AF89F97156020B81714804128DFDAE86C
            F48652D122481018263C36DDE038714569B6DA43D11AF8824761E48E66CF6977
            10330671CCA8D464DBA9F9A543DED48F040A000FC6EC68EEAE750733D51E07C2
            1348E915999935D30A31B8CB010A800056EC6BF76FB078F521068F2DD322293A
            125C9A6FF2BE3FBD3043EDB1C40B280022B8ED88C5F17AB3CBCCA010680298F8
            DF2C4BEB7E65525EBEDA638937500062E0B11336DB9A768FF953676048E792D5
            6252BAC17D745EC9902CE0910A140089B8E150A77B9BC56B6CF38771E7A18C14
            19F5A1C5B9A6EE3F4DC9C7D6F012800220033377B53A0FDAFDA97E06EFAF14A4
            E849685AA6B1EBE3D945856A8F25D1C0175466E67DDCEAFFD0E68758015A0642
            60887749BE896CAA2E4CF82DB96A8202A020D377B5B6357843D96DBE10C60C06
            C030452949BEF9B9A6D0BB53F3E3EE808D78050540251E3866F5BED9EC72B842
            4C0EFD05D6C1D07A160CC3A425E94339467DE74DC5A999FF332E0783792A30B4
            5E3A0DB3F288A5ED90DD9F72C41E48A74F25416BD519DF978A5253720CFAE6D7
            26E5254C67DD78060540C3ACD8D7DE610D86F377DBFC568690F82967A5AB7B89
            29C93FCC9C6C2C3725B7FC6D6A7EA9DA434206070520CE78B4D61A748618E6EF
            AD6E17437459C130C3B4FAC341FA574AEF62F4971AF506BD4EA7A3FE4BFB8D25
            A9F9297A5DF0A931D978384B1C810290603C74C25ADBE10F150618DDF9D656AD
            DE9073A3C5EB647FC3301944A78BE46F5BE92F9F4947F45F2D4DBB20E566D411
            CBAB58699770FC7F63204970D5E43E870000000049454E44AE426082}
          Proportional = True
          Stretch = True
          ExplicitHeight = 203
        end
        object memoAbout: TMemo
          AlignWithMargins = True
          Left = 184
          Top = 18
          Width = 649
          Height = 446
          Align = alClient
          Alignment = taCenter
          Lines.Strings = (
            'XERO accounting API for Delphi'
            'https://github.com/littleearth/delphiXERO'
            ''
            'Developed by: Tristan Marlow'
            ''
            'http://developer.xero.com/'
            ''
            
              'This component merges code from different sources to create an A' +
              'PI for Delphi.'
            ''
            'Currently tested for Delphi Tokyo.'
            ''
            'Thanks to'
            'https://github.com/frogonwheels'
            ''
            'Initial XERO RSA/OAuth code (Flow Software)'
            'ftp://ftp.flow.net.nz/release/code/OAuthWithXero.zip'
            ''
            'DCPcrypt Cryptographic Component Library'
            'http://sourceforge.net/projects/dcpcrypt/'
            ''
            'Fundamentals 4.00'
            'https://code.google.com/p/fundamentals/'
            ''
            ''
            'Use this source code in open source or commercial software.'
            'You do not need to provide any credit.'
            
              'However please provide any fixes or enhancements to keep the com' +
              'ponent'
            'alive and helpful to everyone.')
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object Panel28: TPanel
    Left = 0
    Top = 0
    Width = 852
    Height = 66
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object btnAuthenticate: TButton
      AlignWithMargins = True
      Left = 418
      Top = 3
      Width = 114
      Height = 60
      Align = alRight
      Caption = 'Authenticate'
      TabOrder = 3
      OnClick = btnAuthenticateClick
    end
    object Panel29: TPanel
      Left = 535
      Top = 0
      Width = 317
      Height = 66
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      object Label15: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 311
        Height = 13
        Align = alTop
        Caption = 'Tenant'
        ExplicitWidth = 34
      end
      object comboTenants: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 311
        Height = 21
        Align = alTop
        Style = csDropDownList
        TabOrder = 0
      end
    end
    object Panel30: TPanel
      Left = 0
      Top = 0
      Width = 120
      Height = 66
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label3: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 114
        Height = 13
        Align = alTop
        Caption = 'Client ID'
        ExplicitWidth = 41
      end
      object editClientID: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 114
        Height = 21
        Align = alTop
        PasswordChar = '*'
        TabOrder = 0
      end
    end
    object Panel31: TPanel
      Left = 120
      Top = 0
      Width = 295
      Height = 66
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object Label4: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 289
        Height = 13
        Align = alTop
        Caption = 'Scope'
        ExplicitWidth = 29
      end
      object editScope: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 289
        Height = 21
        Align = alTop
        TabOrder = 0
      end
    end
  end
  object DataSourceDataset: TDataSource
    Left = 36
    Top = 444
  end
end
