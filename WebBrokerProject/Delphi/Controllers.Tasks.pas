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

  Models.Tasks,
  Utils.Logger;

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
    Logger.Info('TTasksController created successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('TTasksController.Create: %s', [E.Message]));
      WriteLn('TTasksController.Create: ' + E.Message);
    end;
  end;
end;

destructor TTasksController.Destroy;
begin
  try
    Logger.Info('TTasksController destroying...');
    FWebStencilsProcessor.Free;
    FTasks.Free;
    Logger.Info('TTasksController destroyed successfully');
  except
    on E: Exception do
      Logger.Error(Format('Error destroying TTasksController: %s', [E.Message]));
  end;
  inherited;
end;

procedure TTasksController.CreateTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var lTask := Request.ContentFields.Values['task'];
    Logger.Info(Format('Creating task: %s', [lTask]));
    lTask := TNetEncoding.HTML.Encode(lTask);
    FTasks.AddTask(lTask);
    Response.Content := RenderTemplate('card');
    Handled := True;
    Logger.Info('Task created successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error creating task: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TTasksController.DeleteTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var lId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Deleting task with ID: %s', [lId]));
    FTasks.DeleteTask(lId.ToInteger);
    Response.Content := RenderTemplate('card');
    Handled := True;
    Logger.Info('Task deleted successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error deleting task: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TTasksController.EditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var lId := Request.QueryFields.Values['id'];
    var lTask := Request.ContentFields.Values['task'];
    Logger.Info(Format('Editing task with ID: %s, new description: %s', [lId, lTask]));
    FTasks.EditTask(lId.ToInteger, lTask);
    Response.Content := RenderTemplate('card');
    Handled := True;
    Logger.Info('Task edited successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error editing task: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TTasksController.GetEditTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var lId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Getting edit task with ID: %s', [lId]));
    var lTask := FTasks.FindTaskById(lId.ToInteger);
    Response.Content := RenderTemplate('itemEdit', lTask);
    Handled := True;
    Logger.Info('Edit task template rendered successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting edit task: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TTasksController.TogglecompletedTask(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    var lId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Toggling completed status for task with ID: %s', [lId]));
    FTasks.TogglecompletedTask(lId.ToInteger);
    Response.Content := RenderTemplate('card');
    Handled := True;
    Logger.Info('Task completion status toggled successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error toggling task completion: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

end.
