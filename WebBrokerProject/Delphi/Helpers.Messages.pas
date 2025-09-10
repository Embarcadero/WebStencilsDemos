unit Helpers.Messages;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp,
  Web.Stencils,
  Utils.Logger;

type
  TMessageType = (mtSuccess, mtWarning, mtError, mtInfo);

  TFlashMessage = class
  private
    FMessageType: TMessageType;
    FMessage: string;
    function GetCssClass: string;
    function GetIcon: string;
  public
    constructor Create(AMessageType: TMessageType; const AMessage: string);
    property MessageType: TMessageType read FMessageType write FMessageType;
    property Message: string read FMessage write FMessage;
    property CssClass: string read GetCssClass;
    property Icon: string read GetIcon;
  end;

  TMessagesObjectList = TObjectList<TFlashMessage>;

  TMessageProvider = class
  private
    FMessages: TMessagesObjectList;
    FHasMessages: Boolean;
    function GetHasMessages: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property HasMessages: Boolean read GetHasMessages;
    property Messages: TMessagesObjectList read FMessages;
  end;

  TMessageManager = class
  private
    const SESSION_KEY = 'messages';
    class function GetMessageProvider(ASession: TWebSession): TMessageProvider;
  public
    class procedure EnsureMessageProvider(ASession: TWebSession);
    class procedure AddMessage(ASession: TWebSession; AMessageType: TMessageType; const AMessage: string);
    class procedure ClearMessages(ASession: TWebSession);
    class function HasMessages(ASession: TWebSession): Boolean;
  end;

implementation

{ TFlashMessage }

constructor TFlashMessage.Create(AMessageType: TMessageType; const AMessage: string);
begin
  inherited Create;
  FMessageType := AMessageType;
  FMessage := AMessage;
end;

function TFlashMessage.GetCssClass: string;
begin
  case FMessageType of
    mtSuccess: Result := 'alert-success';
    mtWarning: Result := 'alert-warning';
    mtError: Result := 'alert-danger';
    mtInfo: Result := 'alert-info';
  else
    Result := 'alert-secondary';
  end;
end;

function TFlashMessage.GetIcon: string;
begin
  case FMessageType of
    mtSuccess: Result := 'bi-check-circle';
    mtWarning: Result := 'bi-exclamation-triangle';
    mtError: Result := 'bi-x-circle';
    mtInfo: Result := 'bi-info-circle';
  else
    Result := 'bi-bell';
  end;
end;

{ TMessageProvider }

constructor TMessageProvider.Create;
begin
  inherited Create;
  FMessages := TMessagesObjectList.Create(True);
  FHasMessages := False;
end;

destructor TMessageProvider.Destroy;
begin
  FMessages.Free;
  inherited;
end;

procedure TMessageProvider.Clear;
begin
  FMessages.Clear;
  FHasMessages := False;
end;

function TMessageProvider.GetHasMessages: Boolean;
begin
  Result := FMessages.Count > 0;
end;

{ TMessageManager }

class procedure TMessageManager.AddMessage(ASession: TWebSession; AMessageType: TMessageType; const AMessage: string);
var
  LMessageProvider: TMessageProvider;
  LNewMessage: TFlashMessage;
begin
  if not Assigned(ASession) then
    Exit;

  try
    EnsureMessageProvider(ASession);
    LMessageProvider := GetMessageProvider(ASession);

    LNewMessage := TFlashMessage.Create(AMessageType, AMessage);
    LMessageProvider.Messages.Add(LNewMessage);
  except
    on E: Exception do
      Logger.Error(Format('Error adding message: %s', [E.Message]));
  end;
end;

class function TMessageManager.GetMessageProvider(ASession: TWebSession): TMessageProvider;
var
  LIndex: Integer;
begin
  Result := nil;
  if not Assigned(ASession) then
    Exit;
    
  try
    LIndex := ASession.DataVars.IndexOf(SESSION_KEY);
    if LIndex >= 0 then
      Result := TMessageProvider(ASession.DataVars.Objects[LIndex]);
  except
    on E: Exception do
      Logger.Warning(Format('Error getting message provider: %s', [E.Message]));
  end;
end;

class procedure TMessageManager.EnsureMessageProvider(ASession: TWebSession);
var
  LIndex: Integer;
begin
  if not Assigned(ASession) then
    Exit;
    
  try
    LIndex := ASession.DataVars.IndexOf(SESSION_KEY);
    if (LIndex < 0) or (ASession.DataVars.Objects[LIndex] = nil) then
    begin
      ASession.DataVars.AddObject(SESSION_KEY, TMessageProvider.Create);
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error ensuring message provider: %s', [E.Message]));
  end;
end;

class procedure TMessageManager.ClearMessages(ASession: TWebSession);
var
  LMessageProvider: TMessageProvider;
begin
  if not Assigned(ASession) then
    Exit;
    
  try
    LMessageProvider := GetMessageProvider(ASession);
    if Assigned(LMessageProvider) then
      LMessageProvider.Clear;
  except
    on E: Exception do
      Logger.Warning(Format('Error clearing messages: %s', [E.Message]));
  end;
end;

class function TMessageManager.HasMessages(ASession: TWebSession): Boolean;
var
  LMessageProvider: TMessageProvider;
begin
  Result := False;
  if not Assigned(ASession) then
    Exit;
    
  LMessageProvider := GetMessageProvider(ASession);
  if Assigned(LMessageProvider) then
    Result := LMessageProvider.HasMessages;
end;

end.
