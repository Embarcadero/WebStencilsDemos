﻿unit MainWebModuleU;

interface

uses
   // System units
  System.Classes,
  System.Generics.Collections,
  System.IOUtils,
  System.SysUtils,

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
  CodeExamplesU;

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
  FAppEdition := 'WebBroker Delphi';
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
  InitControllers;
  InitRequiredData;
  DefineRoutes;
end;

destructor TMainWebModule.Destroy;
begin
  Customers.Active := false;
  FTasksController.Free;
  FCustomersController.Free;
  FCodeExamples.Free;
  FEnvironmentSettings.Free; // Free the settings object
  inherited;
end;

procedure TMainWebModule.InitControllers;
begin
  FTasksController := TTasksController.Create(WebStencilsEngine);
  FCustomersController := TCustomersController.Create(WebStencilsEngine, Customers);
  // More Controllers can be initialized here
end;

procedure TMainWebModule.InitRequiredData;
begin
  // Set the path for resources based on the platform and build configuration
  var BinaryPath := TPath.GetDirectoryName(ParamStr(0));
{$IFDEF MSWINDOWS}
  FResourcesPath := TPath.Combine(BinaryPath, '../../../../resources');
{$ELSE}
  FResourcesPath := BinaryPath;
{$ENDIF}
  WebStencilsEngine.RootDirectory := TPath.Combine(FResourcesPath, 'html');
  WebFileDispatcher.RootDirectory := WebStencilsEngine.RootDirectory;
  Connection.Params.Database := TPath.Combine(FResourcesPath, 'data/database.sqlite3');
  FCodeExamples := TCodeExamples.Create(WebStencilsEngine);

  // Create the environment settings object
  FEnvironmentSettings := TEnvironmentSettings.Create;
  // Add the settings object itself to the engine under the name 'env'
  WebStencilsEngine.AddVar('env', FEnvironmentSettings);
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
  // Define the application's routes using a declarative approach.
  // This class helper maps HTTP methods and paths to their respective handler methods.
  AddRoutes([TRoute.Create(mtDelete, '/tasks', FTasksController.DeleteTask),
    TRoute.Create(mtPost, '/tasks/add', FTasksController.CreateTask),
    TRoute.Create(mtGet, '/tasks/edit', FTasksController.GetEditTask),
    TRoute.Create(mtPut, '/tasks/toggleCompleted', FTasksController.TogglecompletedTask),
    TRoute.Create(mtPut, '/tasks', FTasksController.EditTask),
    // Customers routes
    TRoute.Create(mtGet, '/bigtable', FCustomersController.GetAllCustomers),
    TRoute.Create(mtGet, '/pagination', FCustomersController.GetCustomers)
    ]);
end;

end.
