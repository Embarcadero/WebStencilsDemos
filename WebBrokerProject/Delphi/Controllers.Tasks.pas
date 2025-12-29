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
    FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    function RenderTemplate(ATemplate: string; ARequest: TWebRequest; ATask: TTaskItem = nil): string;
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

{ TTasksController }

function TTasksController.RenderTemplate(ATemplate: string; ARequest: TWebRequest; ATask: TTaskItem = nil): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.rootDirectory, 'partials/tasks/' + ATemplate + '.html');
  if Assigned(ARequest) then
    FWebStencilsProcessor.WebRequest := ARequest;
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
    var LTasks := TTasks.GetInstanceForSession(Request.Session);
    var lTask := Request.ContentFields.Values['task'];
    Logger.Info(Format('Creating task: %s', [lTask]));
    LTasks.AddTask(lTask);
    Response.Content := RenderTemplate('card', Request);
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
    var LTasks := TTasks.GetInstanceForSession(Request.Session);
    var lId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Deleting task with ID: %s', [lId]));
    LTasks.DeleteTask(lId.ToInteger);
    Response.Content := RenderTemplate('card', Request);
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
    var LTasks := TTasks.GetInstanceForSession(Request.Session);
    var LId := Request.QueryFields.Values['id'];
    var LTask := Request.ContentFields.Values['task'];
    Logger.Info(Format('Editing task with ID: %s, new description: %s', [LId, LTask]));
    LTasks.EditTask(LId.ToInteger, LTask);
    Response.Content := RenderTemplate('card', Request);
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
    var LTasks := TTasks.GetInstanceForSession(Request.Session);
    var LId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Getting edit task with ID: %s', [LId]));
    var LTask := LTasks.FindTaskById(LId.ToInteger);
    Response.Content := RenderTemplate('itemEdit', Request, LTask);
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
    var LTasks := TTasks.GetInstanceForSession(Request.Session);
    var LId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Toggling completed status for task with ID: %s', [LId]));
    LTasks.TogglecompletedTask(LId.ToInteger);
    Response.Content := RenderTemplate('card', Request);
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
