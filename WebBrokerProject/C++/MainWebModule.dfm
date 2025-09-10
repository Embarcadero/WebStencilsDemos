object MainWebModule: TMainWebModule
  OnCreate = WebModuleCreate
  Actions = <>
  AfterDispatch = WebModuleAfterDispatch
  Height = 361
  Width = 429
  object WebStencilsEngine: TWebStencilsEngine
    Dispatcher = WebFileDispatcher
    PathTemplates = <
      item
        Template = '/'
        Redirect = '/home.html'
      end
      item
        Template = '/{filename}'
      end>
    RootDirectory = '../../html/'
    OnValue = WebStencilsEngineValue
    Left = 64
    Top = 24
  end
  object WebFileDispatcher: TWebFileDispatcher
    WebFileExtensions = <
      item
        MimeType = 'text/css'
        Extensions = 'css'
      end
      item
        MimeType = 'text/html'
        Extensions = 'html;htm'
      end
      item
        MimeType = 'application/javascript'
        Extensions = 'js'
      end
      item
        MimeType = 'image/jpeg'
        Extensions = 'jpeg;jpg'
      end
      item
        MimeType = 'image/png'
        Extensions = 'png'
      end
      item
        MimeType = 'image/svg+xml'
        Extensions = 'svg;svgz'
      end
      item
        MimeType = 'image/x-icon'
        Extensions = 'ico'
      end>
    WebDirectories = <
      item
        DirectoryAction = dirInclude
        DirectoryMask = '*'
      end
      item
        DirectoryAction = dirExclude
        DirectoryMask = '\templates\*'
      end>
    RootDirectory = '../../html/'
    VirtualPath = '/'
    Left = 64
    Top = 88
  end
  object Customers: TFDQuery
    Connection = Connection
    FormatOptions.AssignedValues = [fvFmtDisplayDate]
    FormatOptions.FmtDisplayDate = 'yyyy-mm-dd'
    SQL.Strings = (
      'SELECT *'
      'FROM customers')
    Left = 341
    Top = 24
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
      LookupDataSet = Countries
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
      
        'Database=C:\Users\azapater\Documents\GitHub\WebStencilsDemos\Web' +
        'StencilsDemoProject\resources\data\database.sqlite3'
      'DriverID=SQLite')
    LoginPrompt = False
    Left = 253
    Top = 24
  end
  object WebSessionManager: TWebSessionManager
    OnCreated = WebSessionManagerCreated
    Left = 64
    Top = 152
  end
  object WebFormsAuthenticator: TWebFormsAuthenticator
    LoginURL = '/login'
    HomeURL = '/'
    LogoutURL = '/logout'
    OnAuthenticate = WebFormsAuthenticatorAuthenticate
    Left = 64
    Top = 216
  end
  object WebAuthorizer: TWebAuthorizer
    UnauthorizedURL = '/forbidden'
    Zones = <
      item
        PathInfo = '/static/*'
        Kind = zkFree
      end
      item
        PathInfo = '/'
        Kind = zkFree
      end
      item
        PathInfo = '/basics*'
        Kind = zkFree
      end
      item
        PathInfo = '/keywords*'
        Kind = zkFree
      end
      item
        PathInfo = '/components*'
        Kind = zkFree
      end
      item
        PathInfo = '/templates*'
        Kind = zkFree
      end
      item
        PathInfo = '/sessionInfo*'
        Kind = zkFree
      end
      item
        PathInfo = '/forbidden*'
        Kind = zkFree
      end
      item
        PathInfo = '/health*'
        Kind = zkFree
      end
      item
        PathInfo = '/tasks*'
      end
      item
        PathInfo = '/bigtable*'
        Roles = 'admin'
      end
      item
        PathInfo = '/pagination*'
        Roles = 'admin'
      end
      item
        PathInfo = '/customers*'
        Roles = 'admin'
      end>
    Left = 176
    Top = 152
  end
  object Countries: TFDQuery
    Connection = Connection
    SQL.Strings = (
      'SELECT *'
      'FROM countries')
    Left = 341
    Top = 88
    object CountriesCOUNTRY: TStringField
      DisplayLabel = 'country'
      FieldName = 'COUNTRY'
      Origin = 'COUNTRY'
      Required = True
      Size = 255
    end
  end
end
