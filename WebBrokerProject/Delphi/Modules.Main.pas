unit Modules.Main;

interface

uses
   // System units
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  System.DateUtils,
  System.StrUtils,

  // Data units
  Data.DB,

  // Web units
  Web.HTTPApp,
  Web.Stencils,

  // FireDAC
  FireDAC.Stan.Async,
  FireDAC.Stan.Def,
  FireDAC.Stan.Error,
  FireDAC.Stan.ExprFuncs,
  FireDAC.Stan.Intf,
  FireDAC.Stan.Option,
  FireDAC.Stan.Param,
  FireDAC.Stan.Pool,
  FireDAC.Stan.StorageJSON,
  FireDAC.DApt,
  FireDAC.DApt.Intf,
  FireDAC.DatS,
  FireDAC.Phys,
  FireDAC.Phys.Intf,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef,
  FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.ConsoleUI.Wait,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.UI.Intf,
  FireDAC.VCLUI.Wait,

  // Own units
  Helpers.WebModule,
  Helpers.FDQuery,
  Helpers.Messages,
  Controllers.Base,
  Controllers.Tasks,
  Models.Tasks,
  Controllers.Customers,
  Services.CodeExamples,
  Utils.Logger;

type

  TMainWebModule = class(TWebModule)
    WebStencilsEngine: TWebStencilsEngine;
    // Adding to WebStencils an object/component using attributes
    WebFileDispatcher: TWebFileDispatcher;
    [WebStencilsVar('customers', false)]
    Customers: TFDQuery;
    Connection: TFDConnection;
    WebSessionManager: TWebSessionManager;
    WebFormsAuthenticator: TWebFormsAuthenticator;
    WebAuthorizer: TWebAuthorizer;
    CustomersID: TFDAutoIncField;
    CustomersCOMPANY: TStringField;
    CustomersFIRST_NAME: TStringField;
    CustomersLAST_NAME: TStringField;
    CustomersGENDER: TStringField;
    CustomersEMAIL: TStringField;
    CustomersPHONE: TStringField;
    CustomersADDRESS: TStringField;
    CustomersPOSTAL_CODE: TStringField;
    CustomersCITY: TStringField;
    CustomersCOUNTRY: TStringField;
    CustomersIP_ADDRESS: TStringField;
    [WebStencilsVar('countries', false)]
    Countries: TFDQuery;
    CustomersAGE: TIntegerField;
    CustomersACTIVATION_DATE: TDateField;
    CustomersACTIVE: TBooleanField;
    CustomersCOMMENTS: TWideMemoField;
    CountriesCOUNTRY: TStringField;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebStencilsEngineValue(Sender: TObject;
      const AObjectName, APropName: string; var AReplaceText: string;
      var AHandled: Boolean);
    procedure WebModule1ActHealthAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebSessionManagerCreated(Sender: TCustomWebSessionManager;
      Request: TWebRequest; Session: TWebSession);
    procedure WebModuleAfterDispatch(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
    procedure WebFormsAuthenticatorAuthenticate(Sender: TCustomWebAuthenticator;
      Request: TWebRequest; const UserName, Password: string; var Roles: string;
      var Success: Boolean);
  private
    FTasksController: TTasksController;
    FCustomersController: TCustomersController;
    FCodeExamples: TCodeExamples;
    FResourcesPath: string;
    procedure DefineRoutes;
    procedure InitRequiredData;
    procedure InitControllers;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TMainWebModule;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}

{ TMainWebModule }

constructor TMainWebModule.Create(AOwner: TComponent);
begin
  inherited;
  Logger.Info('WebStencils demo module constructor called');
end;

procedure TMainWebModule.WebModuleCreate(Sender: TObject);
begin
  Logger.Info('Initializing WebStencils demo module in OnCreate event...');
  InitControllers;
  InitRequiredData;
  DefineRoutes;
  Logger.Info('WebStencils demo module initialized successfully');
end;

destructor TMainWebModule.Destroy;
begin
  Logger.Info('Shutting down WebStencils demo module...');
  Customers.Active := false;
  FTasksController.Free;
  FCustomersController.Free;
  FCodeExamples.Free;
  inherited;
  Logger.Info('WebStencils demo module shutdown complete');
end;

procedure TMainWebModule.InitControllers;
begin
  Logger.Info('Initializing controllers...');
  FTasksController := TTasksController.Create(WebStencilsEngine);
  FCustomersController := TCustomersController.Create(WebStencilsEngine, Customers);
  Logger.Info('Controllers initialized successfully');
end;

procedure TMainWebModule.InitRequiredData;
var
  BinaryPath: string;
  EnvResourcesPath: string;
  EnvDbPath: string;
  DataDir: string;
  BackupDbPath: string;
begin
  Logger.Info('Initializing required data...');
  // Try to get paths from environment variables
  EnvResourcesPath := GetEnvironmentVariable('APP_RESOURCES_PATH');
  EnvDbPath := GetEnvironmentVariable('APP_DB_PATH');

  // Set the path for resources based on the platform and build configuration
  BinaryPath := TPath.GetDirectoryName(ParamStr(0));
{$IFDEF MSWINDOWS}
  if EnvResourcesPath = '' then
    FResourcesPath := TPath.Combine(BinaryPath, '../../../resources')
  else
    FResourcesPath := EnvResourcesPath;
{$ELSE}
  if EnvResourcesPath = '' then
    FResourcesPath := BinaryPath
  else
    FResourcesPath := EnvResourcesPath;
{$ENDIF}
  Logger.Info(Format('Resources path set to: %s', [FResourcesPath]));
  WebStencilsEngine.RootDirectory := TPath.Combine(FResourcesPath, 'html');
  WebFileDispatcher.RootDirectory := WebStencilsEngine.RootDirectory;

  // Set database path
  if EnvDbPath = '' then
    Connection.Params.Database := TPath.Combine(FResourcesPath, 'data/database.sqlite3')
  else
    Connection.Params.Database := EnvDbPath;

  // Check if database exists in data directory
  DataDir := ExtractFilePath(Connection.Params.Database);
  if not DirectoryExists(DataDir) then
  begin
    Logger.Info(Format('Creating data directory: %s', [DataDir]));
    ForceDirectories(DataDir);
  end;

  // If database doesn't exist in data directory, copy it from the backup location in the container
  if not FileExists(Connection.Params.Database) then
  begin
    // The backup database is stored in /app/backup/database.sqlite3 in the container
    BackupDbPath := '/app/backup/database.sqlite3';
    if FileExists(BackupDbPath) then
    begin
      Logger.Info(Format('Initializing database from backup: %s', [BackupDbPath]));
      TFile.Copy(BackupDbPath, Connection.Params.Database);
      Logger.Info(Format('Database initialized at: %s', [Connection.Params.Database]));
    end
    else
      Logger.Error(Format('Backup database not found at: %s', [BackupDbPath]));
  end
  else
    Logger.Info(Format('Using existing database at: %s', [Connection.Params.Database]));

  try
    Connection.Connected := True;
    Logger.Info('Database connection established successfully');
  except
    on E: Exception do
      Logger.Error(Format('Failed to connect to database: %s', [E.Message]));
  end;

  FCodeExamples := TCodeExamples.Create(WebStencilsEngine);

  WebStencilsEngine.AddVar('env', nil, false,
                            function (AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
                            begin
                              if APropName = 'app_name' then
                                AValue := 'WebStencils demo'
                              else if APropName = 'version' then
                                AValue := '1.5.2'
                              else if APropName = 'edition' then
                                AValue := 'WebBroker Delphi' {$IFDEF CONTAINER} + ' in Docker' {$ENDIF}
                              else if APropName = 'company' then
                                Avalue := 'Embarcadero Inc.'
                              else if APropName = 'resource' then
                                AValue := ''
                              else if APropName = 'is_rad_server' then
                                AValue := 'False'
                              else if APropName = 'debug' then
                                Avalue := {$IFDEF DEBUG} 'True' {$ELSE} 'False' {$ENDIF}
                              else
                              begin
                                Result := False;
                                Exit;
                              end;
                              Result := True;
                            end);


  TWebStencilsProcessor.Whitelist.Configure(TField, ['DisplayText', 'Value', 'DisplayLabel', 'FieldName', 'Required', 'LookupDataSet', 'LookupKeyFields', 'Visible', 'DataType', 'Size', 'IsNull'], nil, False);

  Logger.Info('Required data initialization complete');
end;

procedure TMainWebModule.WebStencilsEngineValue(Sender: TObject;
  const AObjectName, APropName: string; var AReplaceText: string;
  var AHandled: Boolean);
begin  
  // Handle dynamic system information
  if SameText(AObjectName, 'system') then
  begin      
    if SameText(APropName, 'timestamp') then
      AReplaceText := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
    else if SameText(APropName, 'year') then
      AReplaceText := FormatDateTime('yyyy', Now)
    else
      AReplaceText := Format('SYSTEM_%s_NOT_FOUND', [APropName.ToUpper]);
    AHandled := True;      
  end;
end;

procedure TMainWebModule.DefineRoutes;
begin
  Logger.Info('Defining application routes...');
  // Define the application's routes using a declarative approach.
  // This class helper maps HTTP methods and paths to their respective handler methods.
  AddRoutes([
    // Task routes (protected)
    TRoute.Create(mtDelete, '/tasks', FTasksController.DeleteTask),
    TRoute.Create(mtPost, '/tasks/add', FTasksController.CreateTask),
    TRoute.Create(mtGet, '/tasks/edit', FTasksController.GetEditTask),
    TRoute.Create(mtPut, '/tasks/toggleCompleted', FTasksController.TogglecompletedTask),
    TRoute.Create(mtPut, '/tasks', FTasksController.EditTask),
    // Customers routes (admin only)
    TRoute.Create(mtGet, '/bigtable', FCustomersController.GetAllCustomers),
    TRoute.Create(mtGet, '/customers', FCustomersController.GetCustomers),
    TRoute.Create(mtGet, '/customers/add', FCustomersController.GetAddCustomer),
    TRoute.Create(mtPost, '/customers/create', FCustomersController.CreateCustomer),
    TRoute.Create(mtGet, '/customers/edit', FCustomersController.GetEditCustomer),
    TRoute.Create(mtPost, '/customers/update', FCustomersController.UpdateCustomer),
    TRoute.Create(mtPost, '/customers/delete', FCustomersController.DeleteCustomer),
    // System routes
    TRoute.Create(mtGet, '/health', WebModule1ActHealthAction)
    ]);
  Logger.Info('Application routes defined successfully');
end;

procedure TMainWebModule.WebModule1ActHealthAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  HealthData: string;
begin
  Response.ContentType := 'application/json';
  HealthData := Format('''
    {
      "status": "healthy",
      "timestamp": "%s",
      "uptime": "%s",
      "version": "%s",
      "environment": "%s",
      "container": %s,
      "resources_path": "%s",
      "database_path": "%s"
    }
  ''', [
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"Z"', TTimeZone.Local.ToUniversalTime(Now)),
    TimeToStr(Now),
    {$IFDEF LINUX}'Linux'{$ELSE}'Windows'{$ENDIF},
    {$IFDEF CONTAINER}'true'{$ELSE}'false'{$ENDIF},
    FResourcesPath,
    Connection.Params.Database
  ]);

  Response.Content := HealthData;
  Handled := True;
end;

procedure TMainWebModule.WebModuleAfterDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // Message clearing - only when not redirecting
  var IsRedirect := (Response.StatusCode >= 300) and (Response.StatusCode < 400);
  if not (IsRedirect) and Assigned(Request.Session) then
    TMessageManager.ClearMessages(Request.Session);
end;

procedure TMainWebModule.WebSessionManagerCreated(Sender: TCustomWebSessionManager;
  Request: TWebRequest; Session: TWebSession);
begin
  Logger.Info(Format('New session created: %s', [Session.Id]));
  Logger.Info(Format('Request Path: %s', [Request.PathInfo]));
  Logger.Info(Format('Request Method: %s', [Request.Method]));

  // Add session creation timestamp for demo purposes
  Session.DataVars.Values['created'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  TMessageManager.EnsureMessageProvider(Session);
  if Assigned(Session.User) then
    Logger.Info(Format('Session created for authenticated user: %s', [Session.User.UserName]))
  else
    Logger.Info('Session created for anonymous user');
end;

procedure TMainWebModule.WebFormsAuthenticatorAuthenticate(
  Sender: TCustomWebAuthenticator; Request: TWebRequest; const UserName,
  Password: string; var Roles: string; var Success: Boolean);
begin
  Logger.Info(Format('Authentication attempt for user: %s', [UserName]));

  // Demo hardcoded credentials
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
  if Success then
    Logger.Info(Format('User %s authenticated successfully with role: %s', [UserName, Roles]))
  else
    Logger.Info(Format('Authentication failed for user: %s', [UserName]));
end;

end.
