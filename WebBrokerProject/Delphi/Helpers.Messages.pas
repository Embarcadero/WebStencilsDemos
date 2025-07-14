unit Helpers.Messages;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Web.HTTPApp,
  Web.Stencils;

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
    function ToString: string; override;
    class function FromString(const AString: string): TFlashMessage; static;
    property MessageType: TMessageType read FMessageType write FMessageType;
    property Message: string read FMessage write FMessage;
    property CssClass: string read GetCssClass;
    property Icon: string read GetIcon;
  end;

  TMessagesObjectList = TObjectList<TFlashMessage>;

  TMessageProvider = class
  private
    FMessages: TObjectList<TFlashMessage>;
    FHasMessages: Boolean;
  public
    constructor Create(const AMessages: TObjectList<TFlashMessage>; AOwnsMessages: Boolean = True);
    destructor Destroy; override;
    property HasMessages: Boolean read FHasMessages;
    property Messages: TObjectList<TFlashMessage> read FMessages;
    function HasSuccess: Boolean;
    function HasWarning: Boolean;
    function HasError: Boolean;
    function HasInfo: Boolean;
    function GetSuccessMessages: TObjectList<TFlashMessage>;
    function GetWarningMessages: TObjectList<TFlashMessage>;
    function GetErrorMessages: TObjectList<TFlashMessage>;
    function GetInfoMessages: TObjectList<TFlashMessage>;
  end;

  TMessageManager = class
  private
    const SESSION_KEY = 'flash_messages';
    function MessagesToString(const AMessages: TObjectList<TFlashMessage>): string;
    function StringToMessages(const AString: string): TObjectList<TFlashMessage>;
  public
    procedure AddMessage(ASession: TWebSession; AMessageType: TMessageType; const AMessage: string);
    function GetMessages(ASession: TWebSession): TObjectList<TFlashMessage>;
    procedure ClearMessages(ASession: TWebSession);
    function HasMessages(ASession: TWebSession): Boolean;
    procedure AddMessagesToTemplate(AProcessor: TWebStencilsProcessor; ASession: TWebSession);
  end;

implementation

{ TFlashMessage }

constructor TFlashMessage.Create(AMessageType: TMessageType; const AMessage: string);
begin
  inherited Create;
  FMessageType := AMessageType;
  FMessage := AMessage;
end;

function TFlashMessage.ToString: string;
begin
  // Format: "MessageType|Message" (using pipe as delimiter)
  Result := IntToStr(Ord(FMessageType)) + '|' + FMessage;
end;

class function TFlashMessage.FromString(const AString: string): TFlashMessage;
var
  LPipePos: Integer;
begin
  Result := TFlashMessage.Create(mtInfo, '');
  LPipePos := Pos('|', AString);
  if LPipePos > 0 then
  begin
    Result.FMessageType := TMessageType(StrToIntDef(Copy(AString, 1, LPipePos - 1), 0));
    Result.FMessage := Copy(AString, LPipePos + 1, Length(AString));
  end
  else
  begin
    // Fallback if format is invalid
    Result.FMessageType := mtInfo;
    Result.FMessage := AString;
  end;
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

constructor TMessageProvider.Create(const AMessages: TObjectList<TFlashMessage>; AOwnsMessages: Boolean = True);
begin
  inherited Create;
  if AOwnsMessages then
    FMessages := AMessages
  else
  begin
    // Create a copy if we don't own the original
    FMessages := TObjectList<TFlashMessage>.Create(True);
    for var LMessage in AMessages do
      FMessages.Add(TFlashMessage.Create(LMessage.MessageType, LMessage.Message));
  end;
  FHasMessages := FMessages.Count > 0;
end;

destructor TMessageProvider.Destroy;
begin
  FMessages.Free;
  inherited;
end;

function TMessageProvider.HasSuccess: Boolean;
var
  LMessage: TFlashMessage;
begin
  Result := False;
  for LMessage in FMessages do
    if LMessage.MessageType = mtSuccess then
      Exit(True);
end;

function TMessageProvider.HasWarning: Boolean;
var
  LMessage: TFlashMessage;
begin
  Result := False;
  for LMessage in FMessages do
    if LMessage.MessageType = mtWarning then
      Exit(True);
end;

function TMessageProvider.HasError: Boolean;
var
  LMessage: TFlashMessage;
begin
  Result := False;
  for LMessage in FMessages do
    if LMessage.MessageType = mtError then
      Exit(True);
end;

function TMessageProvider.HasInfo: Boolean;
var
  LMessage: TFlashMessage;
begin
  Result := False;
  for LMessage in FMessages do
    if LMessage.MessageType = mtInfo then
      Exit(True);
end;

function TMessageProvider.GetSuccessMessages: TObjectList<TFlashMessage>;
var
  LMessage: TFlashMessage;
begin
  Result := TObjectList<TFlashMessage>.Create(False); // Don't own objects
  for LMessage in FMessages do
    if LMessage.MessageType = mtSuccess then
      Result.Add(LMessage);
end;

function TMessageProvider.GetWarningMessages: TObjectList<TFlashMessage>;
var
  LMessage: TFlashMessage;
begin
  Result := TObjectList<TFlashMessage>.Create(False); // Don't own objects
  for LMessage in FMessages do
    if LMessage.MessageType = mtWarning then
      Result.Add(LMessage);
end;

function TMessageProvider.GetErrorMessages: TObjectList<TFlashMessage>;
var
  LMessage: TFlashMessage;
begin
  Result := TObjectList<TFlashMessage>.Create(False); // Don't own objects
  for LMessage in FMessages do
    if LMessage.MessageType = mtError then
      Result.Add(LMessage);
end;

function TMessageProvider.GetInfoMessages: TObjectList<TFlashMessage>;
var
  LMessage: TFlashMessage;
begin
  Result := TObjectList<TFlashMessage>.Create(False); // Don't own objects
  for LMessage in FMessages do
    if LMessage.MessageType = mtInfo then
      Result.Add(LMessage);
end;

{ TMessageManager }

procedure TMessageManager.AddMessage(ASession: TWebSession; AMessageType: TMessageType; const AMessage: string);
var
  LCurrentMessages: TObjectList<TFlashMessage>;
  LNewMessage: TFlashMessage;
begin
  if not Assigned(ASession) then
    Exit;
    
  LCurrentMessages := GetMessages(ASession);
  try
    LNewMessage := TFlashMessage.Create(AMessageType, AMessage);
    LCurrentMessages.Add(LNewMessage);
    
    ASession.DataVars.Values[SESSION_KEY] := MessagesToString(LCurrentMessages);
  finally
    LCurrentMessages.Free;
  end;
end;

function TMessageManager.GetMessages(ASession: TWebSession): TObjectList<TFlashMessage>;
var
  LMessagesString: string;
begin
  Result := TObjectList<TFlashMessage>.Create(True); // Own objects
  if not Assigned(ASession) then
    Exit;
    
  LMessagesString := ASession.DataVars.Values[SESSION_KEY];
  if LMessagesString <> '' then
  begin
    Result.Free;
    Result := StringToMessages(LMessagesString);
  end;
end;

procedure TMessageManager.ClearMessages(ASession: TWebSession);
begin
  if Assigned(ASession) then
    ASession.DataVars.Values[SESSION_KEY] := '';
end;

function TMessageManager.HasMessages(ASession: TWebSession): Boolean;
var
  LMessages: TObjectList<TFlashMessage>;
begin
  LMessages := GetMessages(ASession);
  try
    Result := LMessages.Count > 0;
  finally
    LMessages.Free;
  end;
end;

procedure TMessageManager.AddMessagesToTemplate(AProcessor: TWebStencilsProcessor; ASession: TWebSession);
var
  LMessages: TObjectList<TFlashMessage>;
  LMessageProvider: TMessageProvider;
begin
  if not Assigned(ASession) then
    Exit;
    
  LMessages := GetMessages(ASession);
  LMessageProvider := TMessageProvider.Create(LMessages, True); // Transfer ownership
  
  // Add to template context
  AProcessor.AddVar('messages', LMessageProvider, True);
  
  // Clear messages after adding to template (flash behavior)
  ClearMessages(ASession);
end;

function TMessageManager.MessagesToString(const AMessages: TObjectList<TFlashMessage>): string;
var
  LStringList: TStringList;
  LMessage: TFlashMessage;
begin
  LStringList := TStringList.Create;
  try
    for LMessage in AMessages do
      LStringList.Add(LMessage.ToString);
    Result := LStringList.Text;
  finally
    LStringList.Free;
  end;
end;

function TMessageManager.StringToMessages(const AString: string): TObjectList<TFlashMessage>;
var
  LStringList: TStringList;
  I: Integer;
begin
  Result := TObjectList<TFlashMessage>.Create(True); // Own objects
  if AString = '' then
    Exit;
    
  LStringList := TStringList.Create;
  try
    LStringList.Text := AString;
    for I := 0 to LStringList.Count - 1 do
    begin
      if Trim(LStringList[I]) <> '' then
        Result.Add(TFlashMessage.FromString(LStringList[I]));
    end;
  finally
    LStringList.Free;
  end;
end;

end. 