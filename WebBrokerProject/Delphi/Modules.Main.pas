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
  FireDAC.Comp.UI,

  // Own units
  Helpers.WebModule,
  Helpers.FDQuery,
  Helpers.Messages,
  Controllers.Base,
  Controllers.Tasks,
  Models.Tasks,
  Controllers.Customers,
  Services.CodeExamples,
  Utils.Logger,
  Utils.DemoReset,
  Utils.Config;

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
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
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
    procedure WebModuleBeforeDispatch(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
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
  TDemoReset.Finalize;
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
  DefaultResourcesPath: string;
  DefaultDatabaseFile: string;
  DataDir: string;
begin
  Logger.Info('Initializing required data...');
  
  // Determine platform-specific defaults
  BinaryPath := TPath.GetDirectoryName(ParamStr(0));
{$IFDEF MSWINDOWS}
  DefaultResourcesPath := TPath.Combine(BinaryPath, '../../../resources');
{$ELSE}
  DefaultResourcesPath := BinaryPath;
{$ENDIF}
  
  // Resolve resources path (INI > Environment > Default)
  FResourcesPath := TAppConfig.ReadString('Paths', 'ResourcesPath', 
    DefaultResourcesPath, 'APP_RESOURCES_PATH');
  Logger.Info(Format('Resources path: %s', [FResourcesPath]));
  
  WebStencilsEngine.RootDirectory := TPath.Combine(FResourcesPath, 'html');
  WebFileDispatcher.RootDirectory := WebStencilsEngine.RootDirectory;

  // Resolve database file (INI > Environment > Default)
  DefaultDatabaseFile := TPath.Combine(FResourcesPath, 'data/database.sqlite3');
  Connection.Params.Database := TAppConfig.ReadString('Paths', 'DatabaseFile', 
    DefaultDatabaseFile, 'APP_DB_FILE');
  Logger.Info(Format('Database file: %s', [Connection.Params.Database]));

  // Initialize database directory if it doesn't exist
  DataDir := ExtractFilePath(Connection.Params.Database);
  if not DirectoryExists(DataDir) then
  begin
    Logger.Info(Format('Creating data directory: %s', [DataDir]));
    ForceDirectories(DataDir);
  end;

  if FileExists(Connection.Params.Database) then
    Logger.Info(Format('Using existing database at: %s', [Connection.Params.Database]))
  else
    Logger.Info(Format('Database will be created on first use at: %s', [Connection.Params.Database]));

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
                                AValue := '1.6.0'
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
                              else if APropName = 'demo_mode' then
                                AValue := TDemoReset.IsEnabled.ToString(TUseBoolStrs.True)
                              else
                              begin
                                Result := False;
                                Exit;
                              end;
                              Result := True;
                            end);


  TWebStencilsProcessor.Whitelist.Configure(TField, ['DisplayText', 'Value', 'DisplayLabel', 'FieldName', 'Required', 'LookupDataSet', 'LookupKeyFields', 'Visible', 'DataType', 'Size', 'IsNull'], nil, False);

  // Initialize demo reset if enabled (only active when DEMO_MODE environment variable is set)
  // Backup database will be auto-created if it doesn't exist
  TDemoReset.Initialize(Connection.Params.Database, Connection);

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
      "environment": "%s",
      "container": %s,
      "resources_path": "%s",
      "database_file": "%s"
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
  if Connection.Connected then
    Connection.Connected := False;
end;

procedure TMainWebModule.WebModuleBeforeDispatch(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  if not(Connection.Connected) then
    Connection.Connected := True;
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
  // Initialize Tasks for this session
  TTasks.GetInstanceForSession(Session);
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
