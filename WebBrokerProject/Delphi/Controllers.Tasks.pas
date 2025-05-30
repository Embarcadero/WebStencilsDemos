{
  This unit implements the controller for the Tasks.
  It handles CRUD operations for tasks and renders the appropriate templates.
}

unit Controllers.Tasks;

interface

uses
  System.SysUtils,
  System.IOUtils,
  Web.HTTPApp,
  Web.Stencils,

  Models.Tasks;

type

  TTasksController = class
  private
    FTasks: TTasks;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    function RenderTemplate(ATemplate: string; ATask: TTaskItem = nil): string;
  public
    procedure CreateTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetEditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure EditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure DeleteTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure TogglecompletedTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    constructor Create(AWebStencilsEngine: TWebStencilsEngine);
    destructor Destroy; override;
  end;

implementation

uses
  System.NetEncoding;

{ TTasksController }

function TTasksController.RenderTemplate(ATemplate: string; ATask: TTaskItem = nil): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.rootDirectory, 'partials/tasks/' + ATemplate + '.html');
  if Assigned(ATask) then
    FWebStencilsProcessor.AddVar('Task', ATask, False);
  Result := FWebStencilsProcessor.Content;
  if Assigned(ATask) then
    FWebStencilsProcessor.DataVars.Remove('Task');
end;

constructor TTasksController.Create(AWebStencilsEngine: TWebStencilsEngine);
begin
  inherited Create;
  try
    FWebStencilsEngine := AWebStencilsEngine;
    FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FTasks := TTasks.GetInstance;
    FWebStencilsEngine.AddVar('Tasks', FTasks);
  except
    on E: Exception do
      WriteLn('TTasksController.Create: ' + E.Message);
  end;
end;

destructor TTasksController.Destroy;
begin
  FWebStencilsProcessor.Free;
  FTasks.Free;
  inherited;
end;

procedure TTasksController.CreateTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lTask := Request.ContentFields.Values['task'];
  lTask := TNetEncoding.HTML.Encode(lTask);
  FTasks.AddTask(lTask);
  Response.Content := RenderTemplate('card');
  Handled := True;
end;

procedure TTasksController.DeleteTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lId := Request.QueryFields.Values['id'];
  FTasks.DeleteTask(lId.ToInteger);
  Response.Content := RenderTemplate('card');
  Handled := True;
end;

procedure TTasksController.EditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lId := Request.QueryFields.Values['id'];
  var lTask := Request.ContentFields.Values['task'];
  FTasks.EditTask(lId.ToInteger, lTask);
  Response.Content := RenderTemplate('card');
  Handled := True;
end;

procedure TTasksController.GetEditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lId := Request.QueryFields.Values['id'];
  var lTask := FTasks.FindTaskById(lId.ToInteger);
  Response.Content := RenderTemplate('itemEdit', lTask);
  Handled := True;
end;

procedure TTasksController.TogglecompletedTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var lId := Request.QueryFields.Values['id'];
  FTasks.TogglecompletedTask(lId.ToInteger);
  Response.Content := RenderTemplate('card');
  Handled := True;
end;

end.
