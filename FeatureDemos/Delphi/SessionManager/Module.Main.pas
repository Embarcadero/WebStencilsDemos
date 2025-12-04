unit Module.Main;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  System.Generics.Collections,
  System.IOUtils,
  Web.Stencils;

type
  TWebModuleMain = class(TWebModule)
    WebSessionManager: TWebSessionManager;
    WebFormsAuthenticator: TWebFormsAuthenticator;
    WebAuthorizer: TWebAuthorizer;
    WSEngine: TWebStencilsEngine;
    WebFileDispatcher: TWebFileDispatcher;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebFormsAuthenticatorAuthenticate(Sender: TCustomWebAuthenticator;
      Request: TWebRequest; const UserName, Password: string; var Roles: string;
      var Success: Boolean);
  private
    FResourcesPath: string;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleMain;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModuleMain.WebModuleCreate(Sender: TObject);
var
  BinaryPath: string;
  EnvResourcesPath: string;
begin
  // Try to get paths from environment variables
  EnvResourcesPath := GetEnvironmentVariable('APP_RESOURCES_PATH');

  // Set the path for resources based on the platform and build configuration
  BinaryPath := TPath.GetDirectoryName(ParamStr(0));
{$IFDEF MSWINDOWS}
  if EnvResourcesPath = '' then
    FResourcesPath := TPath.Combine(BinaryPath, '../../')
  else
    FResourcesPath := EnvResourcesPath;
{$ELSE}
  if EnvResourcesPath = '' then
    FResourcesPath := BinaryPath
  else
    FResourcesPath := EnvResourcesPath;
{$ENDIF}
  // TWebStencilsEngine provides template routing and rendering
  // It works with WebBroker authentication/authorization components
  WSEngine.RootDirectory := TPath.Combine(FResourcesPath, 'templates');
  WebFileDispatcher.RootDirectory := WSEngine.RootDirectory;
end;

procedure TWebModuleMain.WebFormsAuthenticatorAuthenticate(
  Sender: TCustomWebAuthenticator; Request: TWebRequest; const UserName,
  Password: string; var Roles: string; var Success: Boolean);
begin
  // Demo hardcoded credentials
  // In production, authenticate against a database or identity provider
  Success := False;
  Roles := '';
  if SameText(UserName, 'demo') and SameText(Password, 'demo123') then
  begin
    Success := True;
    Roles := 'user';
  end
  else if SameText(UserName, 'admin') and SameText(Password, 'admin123') then
  begin
    Success := True;
    Roles := 'admin';
  end;
end;

end.

