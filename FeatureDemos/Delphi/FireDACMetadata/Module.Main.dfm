object WebModuleMain: TWebModuleMain
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = WebModuleMainDefaultHandlerAction
    end>
  Height = 230
  Width = 415
  object WSProcessor: TWebStencilsProcessor
    InputFileName = '../../templates/BaseLayout.html'
    PathTemplate = '../../templates/'
    Left = 192
    Top = 80
  end
  object Customers: TFDQuery
    Connection = Connection
    FormatOptions.AssignedValues = [fvFmtDisplayDate]
    FormatOptions.FmtDisplayDate = 'yyyy-mm-dd'
    SQL.Strings = (
      'SELECT *'
      'FROM customers')
    Left = 325
    Top = 88
    object CustomersID: TFDAutoIncField
      DisplayLabel = 'Id'
      FieldName = 'ID'
      Origin = 'ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = False
      Required = True
      Visible = False
    end
    object CustomersCOMPANY: TStringField
      DisplayLabel = 'Company'
      FieldName = 'COMPANY'
      Origin = 'COMPANY'
      Required = True
      Size = 13
    end
    object CustomersFIRST_NAME: TStringField
      DisplayLabel = 'First Name'
      FieldName = 'FIRST_NAME'
      Origin = 'FIRST_NAME'
      Required = True
      Size = 12
    end
    object CustomersLAST_NAME: TStringField
      DisplayLabel = 'Last Name'
      FieldName = 'LAST_NAME'
      Origin = 'LAST_NAME'
      Required = True
      Size = 15
    end
    object CustomersGENDER: TStringField
      DisplayLabel = 'Gender'
      FieldName = 'GENDER'
      Origin = 'GENDER'
      Size = 11
    end
    object CustomersAGE: TIntegerField
      DisplayLabel = 'Age'
      DisplayWidth = 2
      FieldName = 'AGE'
      Origin = 'AGE'
    end
    object CustomersPOSTAL_CODE: TStringField
      DisplayLabel = 'Postal Code'
      FieldName = 'POSTAL_CODE'
      Origin = 'POSTAL_CODE'
      Size = 14
    end
    object CustomersADDRESS: TStringField
      DisplayLabel = 'Address'
      FieldName = 'ADDRESS'
      Origin = 'ADDRESS'
      Size = 27
    end
    object CustomersCITY: TStringField
      DisplayLabel = 'City'
      FieldName = 'CITY'
      Origin = 'CITY'
      Size = 40
    end
    object CustomersCOUNTRY: TStringField
      DisplayLabel = 'Country'
      FieldName = 'COUNTRY'
      LookupKeyFields = 'COUNTRY'
      Origin = 'COUNTRY'
      Size = 34
    end
    object CustomersPHONE: TStringField
      DisplayLabel = 'Phone'
      FieldName = 'PHONE'
      Origin = 'PHONE'
      Size = 17
    end
    object CustomersEMAIL: TStringField
      DisplayLabel = 'e-mail'
      FieldName = 'EMAIL'
      Origin = 'EMAIL'
      Size = 34
    end
    object CustomersIP_ADDRESS: TStringField
      DisplayLabel = 'IP Address'
      FieldName = 'IP_ADDRESS'
      Origin = 'IP_ADDRESS'
      Size = 15
    end
    object CustomersACTIVATION_DATE: TDateField
      DisplayLabel = 'Activation Date'
      FieldName = 'ACTIVATION_DATE'
      Origin = 'ACTIVATION_DATE'
      DisplayFormat = 'yyyy-mm-dd'
    end
    object CustomersACTIVE: TBooleanField
      DisplayLabel = 'Active'
      FieldName = 'ACTIVE'
      Origin = 'ACTIVE'
    end
    object CustomersCOMMENTS: TWideMemoField
      DisplayLabel = 'Comments'
      FieldName = 'COMMENTS'
      Origin = 'COMMENTS'
      BlobType = ftWideMemo
    end
  end
  object Connection: TFDConnection
    Params.Strings = (
      
        'Database=C:\Users\antonio\Documents\WebSt_Demo\resources\data\da' +
        'tabase.sqlite3'
      'DriverID=SQLite')
    Left = 328
    Top = 24
  end
end

