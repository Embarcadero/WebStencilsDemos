object WebModuleMain: TWebModuleMain
  OnCreate = WebModuleCreate
  Actions = <>
  Height = 270
  Width = 415
  object WebSessionManager: TWebSessionManager
    Left = 64
    Top = 32
  end
  object WebFormsAuthenticator: TWebFormsAuthenticator
    LoginURL = '/login'
    HomeURL = '/'
    LogoutURL = '/logout'
    OnAuthenticate = WebFormsAuthenticatorAuthenticate
    Left = 64
    Top = 158
  end
  object WebAuthorizer: TWebAuthorizer
    UnauthorizedURL = '/forbidden'
    Zones = <
      item
        PathInfo = '/'
        Kind = zkFree
      end
      item
        PathInfo = '/authenticated*'
      end
      item
        PathInfo = '/admin*'
        Roles = 'admin'
      end>
    Left = 64
    Top = 96
  end
  object WSEngine: TWebStencilsEngine
    Dispatcher = WebFileDispatcher
    PathTemplates = <
      item
        Template = '/'
        Redirect = '/home.html'
      end
      item
        Template = '/{filename}'
      end>
    RootDirectory = '..\..\templates\'
    Left = 192
    Top = 32
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
    RootDirectory = '/../../templates/'
    VirtualPath = '/'
    Left = 312
    Top = 32
  end
end

