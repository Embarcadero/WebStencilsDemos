unit WebRresource;

// EMS Resource Module

interface

uses
  System.SysUtils, System.Classes, System.JSON,
  EMS.Services, EMS.ResourceAPI, EMS.ResourceTypes,
  Web.Stencils, Web.HTTPApp, EMS.FileResource, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.ConsoleUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Phys.IB, FireDAC.Phys.IBDef,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.Comp.DataSet,
  FireDAC.VCLUI.Wait, FireDAC.DApt,
  System.Generics.Collections,
  System.IOUtils, // Added for TPath

  CodeExamplesU,
  Controllers.Tasks,
  Controllers.Customers;

type
  { TEnvironmentSettings: Class to hold environment/application settings for WebStencils }
  TEnvironmentSettings = class(TPersistent)
  private
    FAppVersion: string;
    FAppName: string;
    FAppEdition: string;
    FCompanyName: string;
    FResource: string;
    FIsRadServer: Boolean;
    FDebugMode: Boolean;
  public
    constructor Create;
  published
    property AppVersion: string read FAppVersion;
    property AppName: string read FAppName;
    property AppEdition: string read FAppEdition;
    property CompanyName: string read FCompanyName;
    property Resource: string read FResource; // Required for RAD Server compatibility
    property IsRadServer: Boolean read FIsRadServer;
    property DebugMode: Boolean read FDebugMode;
  end;

  [ResourceName('web')]
  TTasksResource1 = class(TDataModule)
//    [ResourceSuffix('./')]
    [ResourceSuffix('get', '/{filename}')]
    html: TEMSFileResource;
    [ResourceSuffix('get', '/static/css/{filename}')]
    css: TEMSFileResource;
    [ResourceSuffix('get', '/static/js/{filename}')]
    js: TEMSFileResource;
    [ResourceSuffix('get', '/static/img/{filename}')]
    img: TEMSFileResource;
    WebStencilsEngine1: TWebStencilsEngine;
    WebStencilsProcessor: TWebStencilsProcessor;
    FDConnection: TFDConnection;
    [WebStencilsVar('customers', false)]
    customers: TFDQuery;
    procedure DataModuleCreate(Sender: TObject);
    procedure WebStencilsEngine1Value(Sender: TObject; const AObjectName,
      APropName: string; var AReplaceText: string; var AHandled: Boolean);
  private
    FCodeExamples: TCodeExamples;
    FTasksController: TTasksController;
    FCustomersController: TCustomersController;
    FEnvironmentSettings: TEnvironmentSettings; // Changed from TDictionary
  published
    [ResourceSuffix('./')]
    procedure Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./tasks')]
    procedure DeleteTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./tasks/add')]
    procedure PostTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./tasks/edit')]
    procedure GetTasksEdit(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./tasks/toggleCompleted')]
    procedure PutTaskToggleCompleted(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./tasks')]
    procedure PutTask(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./pagination')]
    procedure GetPaginatedCustomers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
    [ResourceSuffix('./bigtable')]
    procedure GetAllCustomersEndpoint(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
  end;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

{ TEnvironmentSettings }

constructor TEnvironmentSettings.Create;
begin
  inherited Create;
  // Initialize properties for RAD Server context
  FAppVersion := '1.0.0';
  FAppName := 'WebStencils demo';
  FAppEdition := 'RAD Server Delphi';
  FCompanyName := 'Embarcadero Inc.';
  FResource := '/web'; // Set RAD Server resource endpoint
  FIsRadServer := True;
{$IFDEF DEBUG}
  FDebugMode := True;
{$ELSE}
  FDebugMode := False;
{$ENDIF}
end;

{ TTasksResource1 }

procedure TTasksResource1.DataModuleCreate(Sender: TObject);
const
  //////////////////////
  // Replace the constant LProjectPath with the absolute path to the resources folder of the project
  //////////////////////
  LProjectPath: string = 'C:\replace\with\your\absolute\path\to\the\resources\folder';
begin

  FDConnection.Params.Database := TPath.Combine(LProjectPath, 'data\tasks.ib');
  html.PathTemplate := TPath.Combine(LProjectPath, 'html\{filename}');
  css.PathTemplate := TPath.Combine(LProjectPath, 'html\static\css\{filename}');
  js.PathTemplate := TPath.Combine(LProjectPath, 'html\static\js\{filename}');
  img.PathTemplate := TPath.Combine(LProjectPath, 'html\static\img\{filename}');
  WebStencilsProcessor.PathTemplate := TPath.Combine(LProjectPath, 'html');
  WebStencilsEngine1.RootDirectory := TPath.Combine(LProjectPath, 'html');
  AddProcessor(html, WebStencilsEngine1);
  FCodeExamples := TCodeExamples.Create(WebStencilsEngine1);
  FTasksController := TTasksController.Create(WebStencilsEngine1, FDConnection);
  FCustomersController := TCustomersController.Create(WebStencilsEngine1, customers);

  // Create and register the environment settings object
  FEnvironmentSettings := TEnvironmentSettings.Create;
  WebStencilsEngine1.AddVar('env', FEnvironmentSettings);
end;

procedure TTasksResource1.Get(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
var
  LHTMLContent: string;
begin
  WebStencilsProcessor.InputFileName := TPath.Combine(WebStencilsProcessor.PathTemplate, 'home.html');
  LHTMLContent := WebStencilsProcessor.Content;
  AResponse.Body.SetString(LHTMLContent);
end;

procedure TTasksResource1.DeleteTask(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FtasksController.DeleteTask(ARequest, AResponse);
end;

procedure TTasksResource1.PostTask(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FtasksController.CreateTask(ARequest, AResponse);
end;

procedure TTasksResource1.GetTasksEdit(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FTasksController.GetEditTask(ARequest, Aresponse);
end;

procedure TTasksResource1.PutTaskToggleCompleted(
  const AContext: TEndpointContext; const ARequest: TEndpointRequest;
  const AResponse: TEndpointResponse);
begin
  FtasksController.TogglecompletedTask(ARequest, AResponse);
end;

procedure TTasksResource1.PutTask(const AContext: TEndpointContext;
  const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FtasksController.EditTask(ARequest, AResponse);
end;

procedure TTasksResource1.GetPaginatedCustomers(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FCustomersController.GetCustomers(ARequest, AResponse);
end;

procedure TTasksResource1.GetAllCustomersEndpoint(const AContext: TEndpointContext; const ARequest: TEndpointRequest; const AResponse: TEndpointResponse);
begin
  FCustomersController.GetAllCustomers(ARequest, AResponse);
end;

procedure TTasksResource1.WebStencilsEngine1Value(Sender: TObject;
  const AObjectName, APropName: string; var AReplaceText: string;
  var AHandled: Boolean);
begin
  // Handle dynamic system information
  if SameText(AObjectName, 'system') then
  begin
    AHandled := True; // Handle all system properties here
    if SameText(APropName, 'timestamp') then
      AReplaceText := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)
    else if SameText(APropName, 'year') then
      AReplaceText := FormatDateTime('yyyy', Now)
    else
      AReplaceText := Format('SYSTEM_%s_NOT_FOUND', [APropName.ToUpper]);
  end;
end;

procedure Register;
begin
  RegisterResource(TypeInfo(TTasksResource1));
end;

initialization
  Register;
end.
