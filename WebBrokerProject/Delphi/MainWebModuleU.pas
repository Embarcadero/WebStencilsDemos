unit MainWebModuleU;

interface

uses
   // System units
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,
  System.DateUtils,

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

  // Own units
  Helpers.WebModule,
  Helpers.FDQuery,
  Controllers.Tasks,
  Models.Tasks,
  Controllers.Customers,
  CodeExamplesU,
  LoggerU;

type
  { TEnvironmentSettings: Class to hold environment/application settings for WebStencils }
  TEnvironmentSettings = class(TPersistent)
  private
    FAppVersion: string;
    FAppName: string;
    FAppEdition: string;
    FCompanyName: string;
    FResource: string;
    FDebugMode: Boolean;
    FIsRadServer: Boolean;
  public
    constructor Create;
  published
    property AppVersion: string read FAppVersion;
    property AppName: string read FAppName;
    property AppEdition: string read FAppEdition;
    property CompanyName: string read FCompanyName;
    property Resource: string read FResource; // Required for RAD Server compatibility
    property DebugMode: Boolean read FDebugMode;
    property IsRadServer: Boolean read FIsRadServer;
  end;

  TMainWebModule = class(TWebModule)
    WebStencilsEngine: TWebStencilsEngine;
    // Adding to WebStencils an object/component using attributes
    WebFileDispatcher: TWebFileDispatcher;
    [WebStencilsVar('customers', false)]
    Customers: TFDQuery;
    Connection: TFDConnection;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure WebStencilsEngineValue(Sender: TObject;
      const AObjectName, APropName: string; var AReplaceText: string;
      var AHandled: Boolean);
    procedure WebModule1ActHealthAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
    FTasksController: TTasksController;
    FCustomersController: TCustomersController;
    FCodeExamples: TCodeExamples;
    FEnvironmentSettings: TEnvironmentSettings;
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

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

{ TEnvironmentSettings }

constructor TEnvironmentSettings.Create;
begin
  inherited Create;
  // Initialize properties
  FAppVersion := '1.0.0';
  FAppName := 'WebStencils demo';
  FAppEdition := ' WebBroker Delphi';
  {$IFDEF CONTAINER}
  FAppEdition := FAppEdition + ' in Docker';
  {$ENDIF}
  FCompanyName := 'Embarcadero Inc.';
  // This RESOURCE env is required to make the WebStencils templates reusable for RAD Server
  FResource := '';
{$IFDEF DEBUG}
  FDebugMode := True;
{$ELSE}
  FDebugMode := False;
{$ENDIF}
  FIsRadServer := False;
end;

{ TMainWebModule }

constructor TMainWebModule.Create(AOwner: TComponent);
begin
  inherited;
  Logger.Info('Initializing WebStencils demo module...');
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
  FEnvironmentSettings.Free;
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
    FResourcesPath := TPath.Combine(BinaryPath, '../../../../resources')
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
  FEnvironmentSettings := TEnvironmentSettings.Create;
  WebStencilsEngine.AddVar('env', FEnvironmentSettings);
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
  AddRoutes([TRoute.Create(mtDelete, '/tasks', FTasksController.DeleteTask),
    TRoute.Create(mtPost, '/tasks/add', FTasksController.CreateTask),
    TRoute.Create(mtGet, '/tasks/edit', FTasksController.GetEditTask),
    TRoute.Create(mtPut, '/tasks/toggleCompleted', FTasksController.TogglecompletedTask),
    TRoute.Create(mtPut, '/tasks', FTasksController.EditTask),
    // Customers routes
    TRoute.Create(mtGet, '/bigtable', FCustomersController.GetAllCustomers),
    TRoute.Create(mtGet, '/pagination', FCustomersController.GetCustomers),
    // Add health check endpoint
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
    FEnvironmentSettings.AppVersion,
    {$IFDEF LINUX}'Linux'{$ELSE}'Windows'{$ENDIF},
    {$IFDEF CONTAINER}'true'{$ELSE}'false'{$ENDIF},
    FResourcesPath,
    Connection.Params.Database
  ]);
  
  Response.Content := HealthData;
  Handled := True;
end;

end.
