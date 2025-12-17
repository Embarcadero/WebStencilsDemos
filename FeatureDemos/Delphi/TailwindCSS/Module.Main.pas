unit Module.Main;

interface

uses
  System.SysUtils,
  System.Classes,
  Web.HTTPApp,
  System.Generics.Collections,
  System.IOUtils,
  Web.Stencils,
  Models,
  Utils.MockData;

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
    procedure InitMockData;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleMain;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModuleMain.InitMockData;
var
  Users: TObjectList<TUser>;
  Stats: TObjectList<TStat>;
  Metrics: TObjectList<TAnalyticsMetric>;
  Products, RecentProducts: TObjectList<TProduct>;
begin
  Users := GenerateMockUsers;
  Stats := GenerateDashboardStats;
  Metrics := GenerateAnalyticsMetrics;
  Products := GenerateMockProducts;
  RecentProducts := GenerateMockProducts;
  // Limit to 3 products for recent products
  while RecentProducts.Count > 3 do
    RecentProducts.Delete(RecentProducts.Count - 1);

  WSEngine.AddVar('users', Users);
  WSEngine.AddVar('dashboardStats', Stats);
  WSEngine.AddVar('analyticsMetrics', Metrics);
  WSEngine.AddVar('products', Products);
  WSEngine.AddVar('recentProducts', RecentProducts);
end;

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

  WSEngine.AddVar('env', nil, false,
    function (AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
    begin
      if APropName.ToUpper = 'APP_VERSION' then
        AValue := '1.0.0'
      else if APropName.ToUpper = 'DEBUG' then
        AValue := {$IFDEF DEBUG}'True'{$ELSE}'False'{$ENDIF}
      else
      begin
        Result := False;
        Exit;
      end;
      Result := True;
    end);

  InitMockData;
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

