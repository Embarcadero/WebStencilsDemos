﻿{
  This unit implements the controller for the Tasks.
  It handles CRUD operations for tasks and renders the appropriate templates.
}

unit Tasks.Controller;

interface

uses
  System.SysUtils, Web.HTTPApp, Web.Stencils, EMS.ResourceAPI, FireDAC.Comp.Client,

  Tasks.Model;

type

  TTasksController = class
  private
    FTasks: TTasks;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    function RenderTemplate(ATemplate: string; ATask: TTaskItem = nil): string;
  public
		procedure CreateTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
		procedure GetEditTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
		procedure EditTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
		procedure DeleteTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
		procedure TogglecompletedTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
		constructor Create(AWebStencilsEngine: TWebStencilsEngine; AFDConnection: TFDConnection);
    destructor Destroy; override;
  end;

implementation

uses
	System.NetEncoding,
	System.JSON;

{ TTasksController }

function TTasksController.RenderTemplate(ATemplate: string; ATask: TTaskItem = nil): string;
begin
  var rootDirectory := FWebStencilsEngine.rootDirectory;
  FWebStencilsProcessor.InputFileName := FWebStencilsEngine.rootDirectory + 'partials/tasks/' + ATemplate + '.html';
  if Assigned(ATask) then
    FWebStencilsProcessor.AddVar('Task', ATask, False);
  Result := FWebStencilsProcessor.Content;
  if Assigned(ATask) then
    FWebStencilsProcessor.DataVars.Remove('Task');
end;

constructor TTasksController.Create(AWebStencilsEngine: TWebStencilsEngine; AFDConnection: TFDConnection);
begin
  inherited Create;
  try
		FWebStencilsEngine := AWebStencilsEngine;
		FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
		FWebStencilsProcessor.Engine := FWebStencilsEngine;
		FTasks := TTasks.Create(AFDConnection);
		FWebStencilsEngine.AddVar('Tasks', FTasks.AllTasks);
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

procedure TTasksController.CreateTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
var
	lTaskDescription: string;
	lJSON: TJSONObject;
begin
	if not(ARequest.Body.TryGetObject(lJSON) and lJSON.TryGetValue<string>('task', lTaskDescription)) then
		AResponse.RaiseBadRequest('Bad request', 'Missing data');
	lTaskDescription := TNetEncoding.HTML.Encode(lTaskDescription);
	var lNewTaskItem := FTasks.AddTask(lTaskDescription);
	AResponse.Body.SetString(RenderTemplate('item', lNewTaskItem));
end;

procedure TTasksController.DeleteTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
begin
	var lId := ARequest.Params.Values['id'];
	FTasks.DeleteTask(lId.ToInteger);
	AResponse.Body.SetString(RenderTemplate('card'));
end;

procedure TTasksController.EditTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
var
	lTaskDescription: string;
	lJSON: TJSONObject;
begin
	if not(ARequest.Body.TryGetObject(lJSON) and lJSON.TryGetValue<string>('task', lTaskDescription)) then
		AResponse.RaiseBadRequest('Bad request', 'Missing data');
	var lId := ARequest.Params.Values['id'];
	FTasks.EditTask(lId.ToInteger, lTaskDescription);
	AResponse.Body.SetString(RenderTemplate('card'));
end;

procedure TTasksController.GetEditTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
begin
	var lId := ARequest.Params.Values['id'];
	var lTask := FTasks.FindTaskById(lId.ToInteger);
	AResponse.Body.SetString(RenderTemplate('itemEdit', lTask));
end;

procedure TTasksController.TogglecompletedTask(ARequest: TEndpointRequest; AResponse: TEndpointResponse);
begin
	var lId := ARequest.Params.Values['id'];
	var lTask := FTasks.TogglecompletedTask(lId.ToInteger);
	AResponse.Body.SetString(RenderTemplate('item', lTask));
end;

end.
