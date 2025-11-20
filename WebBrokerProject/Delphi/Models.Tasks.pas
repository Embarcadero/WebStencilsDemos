{
  This unit defines the model/repository for the Tasks.
  Tasks are stored per-session in memory.
}

unit Models.Tasks;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Web.HTTPApp;

type
  TTaskItem = class
  private
    FId: Integer;
    FDescription: string;
    FCompleted: Boolean;
  public
    constructor Create(AId: Integer; const ADescription: string);
    property Id: Integer read FId;
    property Description: string read FDescription write FDescription;
    property Completed: Boolean read FCompleted write FCompleted;
  end;

  // Tasks are stored in memory per-session
  TTasks = class
  private
    FItems: TObjectList<TTaskItem>;
    FNextId: Integer;

    constructor Create;
    function GetCount: Integer;
    function GetCompletedCount: Integer;
    function GetAllTasks: TObjectList<TTaskItem>;
    function GetNextId: Integer;
    procedure InitializeDefaultTasks;
  public
    class function GetInstanceForSession(ASession: TWebSession): TTasks;

    destructor Destroy; override;
    function FindTaskById(AId: Integer): TTaskItem;
    function AddTask(const ADescription: string): TTaskItem;
    procedure EditTask(AId: Integer; ADescription: string);
    procedure DeleteTask(AId: Integer);
    function ToggleCompletedTask(AId: Integer): TTaskItem;
    procedure ResetToDefaults;
    property Count: Integer read GetCount;
    property CompletedCount: Integer read GetCompletedCount;
    property NextId: Integer read GetNextId;
    property AllTasks: TObjectList<TTaskItem> read GetAllTasks;
  end;

implementation

{ TTaskItem }

constructor TTaskItem.Create(AId: Integer; const ADescription: string);
begin
  inherited Create;
  FId := AId;
  FDescription := ADescription;
  FCompleted := False;
end;

{ TTasks }

class function TTasks.GetInstanceForSession(ASession: TWebSession): TTasks;
const
  SESSION_KEY = 'Tasks';
begin
  if not Assigned(ASession) then
    raise Exception.Create('Session is required for TTasks');

  var LIndex := ASession.DataVars.IndexOf(SESSION_KEY);
  if LIndex >= 0 then
    Result := TTasks(ASession.DataVars.Objects[LIndex])
  else
  begin
    Result := TTasks.Create;
    Result.InitializeDefaultTasks;
    ASession.DataVars.AddObject(SESSION_KEY, Result);
  end;
end;

constructor TTasks.Create;
begin
  inherited;
  FItems := TObjectList<TTaskItem>.Create(True);
  FNextId := 1;
end;

destructor TTasks.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TTasks.GetNextId: Integer;
begin
  Result := FNextId;
end;

procedure TTasks.EditTask(AId: Integer; ADescription: string);
begin
  var Item := FindTaskById(AId);
  if Assigned(Item) then
    Item.Description := ADescription;
end;

function TTasks.AddTask(const ADescription: string): TTaskItem;
begin
  var NewItem := TTaskItem.Create(FNextId, ADescription);
  FItems.Add(NewItem);
  Result := NewItem;
  Inc(FNextId);
end;

function TTasks.ToggleCompletedTask(AId: Integer): TTaskItem;
begin
  var Item := FindTaskById(AId);
  if Assigned(Item) then
  begin
    Item.Completed := not(Item.Completed);
    Exit(Item);
  end;
  Result := nil;
end;

procedure TTasks.DeleteTask(AId: Integer);
begin
  var Item := FindTaskById(AId);
  if Assigned(Item) then
    FItems.Remove(Item);
end;

function TTasks.GetCompletedCount: Integer;
begin
  Result := 0;
  for var Item in FItems do
    if Item.Completed then
      Inc(Result);
end;

function TTasks.GetAllTasks: TObjectList<TTaskItem>;
begin
  Result := FItems;
end;

function TTasks.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TTasks.FindTaskById(AId: Integer): TTaskItem;
begin
  for var Item in FItems do
    if Item.Id = AId then
      Exit(Item);
  Result := nil;
end;

procedure TTasks.InitializeDefaultTasks;
var
  DefaultTasks: array[0..6] of string;
  I: Integer;
begin
  // Clear all existing tasks
  FItems.Clear;
  FNextId := 1;
  
  // Add default demo tasks
  DefaultTasks[0] := 'Refactor that spaghetti code written in the 90s.';
  DefaultTasks[1] := 'Convince management to upgrade from Delphi 7.';
  DefaultTasks[2] := 'Remove all the captions of the TPanels.';
  DefaultTasks[3] := 'Use `with` statement responsibly... and then regret it immediately.';
  DefaultTasks[4] := 'Refactor all the business logic inside OnClick events.';
  DefaultTasks[5] := 'Convince the team that VCL is still cool.';
  DefaultTasks[6] := 'Document those "temporary" global variables created 5 years ago.';
  
  for I := 0 to High(DefaultTasks) do
    AddTask(DefaultTasks[I]);
  
  // Mark task 2 as completed (id 2)
  ToggleCompletedTask(2);
end;

procedure TTasks.ResetToDefaults;
begin
  InitializeDefaultTasks;
end;

end.
