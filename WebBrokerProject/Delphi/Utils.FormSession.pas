unit Utils.FormSession;

interface

uses
  Web.HTTPApp, 
  System.Generics.Collections,
  System.SysUtils,
  Utils.Logger;

type
  TFormSessionManager = class
  private
    const
      FORM_DATA_PREFIX = 'FormData_';
      FORM_ERRORS_PREFIX = 'FormErrors_';
      FORM_TIMESTAMP_SUFFIX = '_timestamp';
  public
    class procedure StoreFormData(ASession: TWebSession; const AFormName: string; ARequest: TWebRequest);
    class procedure StoreValidationErrors(ASession: TWebSession; const AFormName: string; const AErrors: TArray<string>);
    class function GetFormData(ASession: TWebSession; const AFormName: string): TDictionary<string, string>;
    class function GetValidationErrors(ASession: TWebSession; const AFormName: string): TArray<string>;
    class procedure ClearFormData(ASession: TWebSession; const AFormName: string);
    class function HasFormData(ASession: TWebSession; const AFormName: string): Boolean;
  end;

implementation

uses
  System.DateUtils;

{ TFormSessionManager }

class procedure TFormSessionManager.StoreFormData(ASession: TWebSession; const AFormName: string; ARequest: TWebRequest);
var
  I: Integer;
  SessionKey, SessionContent: string;
begin
  if not Assigned(ASession) or not Assigned(ARequest) then
  begin
    Logger.Warning('StoreFormData: Invalid parameters');
    Exit;
  end;
    
  try
    // Store each form field in session
    for I := 0 to ARequest.ContentFields.Count - 1 do
    begin
      SessionKey := FORM_DATA_PREFIX + AFormName + '_' + ARequest.ContentFields.Names[I];
      SessionContent := ARequest.ContentFields.ValueFromIndex[I];
      ASession.DataVars.AddPair(SessionKey, SessionContent);
    end;
    
    // Set timestamp for cleanup
    ASession.DataVars.AddPair(FORM_DATA_PREFIX + AFormName + FORM_TIMESTAMP_SUFFIX, DateTimeToStr(Now));
  except
    on E: Exception do
      Logger.Error(Format('Error storing form data: %s', [E.Message]));
  end;
end;

class procedure TFormSessionManager.StoreValidationErrors(ASession: TWebSession; const AFormName: string; const AErrors: TArray<string>);
var
  I: Integer;
  SessionKey: string;
begin
  if not Assigned(ASession) or (Length(AErrors) = 0) then
  begin
    Logger.Warning('StoreValidationErrors: Invalid parameters');
    Exit;
  end;
    
  try
    // Store each error in session
    for I := 0 to High(AErrors) do
    begin
      SessionKey := FORM_ERRORS_PREFIX + AFormName + '_' + IntToStr(I);
      ASession.DataVars.Values[SessionKey] := AErrors[I];
    end;
    
    // Store error count
    ASession.DataVars.Values[FORM_ERRORS_PREFIX + AFormName + '_count'] := IntToStr(Length(AErrors));
  except
    on E: Exception do
      Logger.Error(Format('Error storing validation errors: %s', [E.Message]));
  end;
end;

class function TFormSessionManager.GetFormData(ASession: TWebSession; const AFormName: string): TDictionary<string, string>;
var
  I: Integer;
  SessionKey, FieldName, FieldValue: string;
  Prefix: string;
begin
  Result := TDictionary<string, string>.Create;
  
  if not Assigned(ASession) then
  begin
    Logger.Warning('GetFormData: Invalid parameters');
    Exit;
  end;
    
  try
    Prefix := FORM_DATA_PREFIX + AFormName + '_';
    
    // Extract form data from session using DataVars
    for I := 0 to ASession.DataVars.Count - 1 do
    begin
      SessionKey := ASession.DataVars.Names[I];
      if SessionKey.StartsWith(Prefix) and not SessionKey.EndsWith(FORM_TIMESTAMP_SUFFIX) then
      begin
        FieldName := SessionKey.Replace(Prefix, '');
        FieldValue := ASession.DataVars.Values[SessionKey];
        Result.Add(FieldName, FieldValue);
      end;
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error getting form data: %s', [E.Message]));
  end;
end;

class function TFormSessionManager.GetValidationErrors(ASession: TWebSession; const AFormName: string): TArray<string>;
var
  I, ErrorCount: Integer;
  SessionKey: string;
  ErrorList: TList<string>;
begin
  SetLength(Result, 0);
  
  if not Assigned(ASession) then
    Exit;
    
  // Get error count
  SessionKey := FORM_ERRORS_PREFIX + AFormName + '_count';
  ErrorCount := StrToIntDef(ASession.DataVars.Values[SessionKey], 0);
  
  if ErrorCount = 0 then
    Exit;
    
  ErrorList := TList<string>.Create;
  try
    // Retrieve all errors
    for I := 0 to ErrorCount - 1 do
    begin
      SessionKey := FORM_ERRORS_PREFIX + AFormName + '_' + IntToStr(I);
      ErrorList.Add(ASession.DataVars.Values[SessionKey]);
    end;
    
    Result := ErrorList.ToArray;
  finally
    ErrorList.Free;
  end;
end;

class procedure TFormSessionManager.ClearFormData(ASession: TWebSession; const AFormName: string);
var
  I: Integer;
  SessionKey: string;
  KeysToRemove: TList<string>;
begin
  if not Assigned(ASession) then
  begin
    Logger.Warning('ClearFormData: Invalid parameters');
    Exit;
  end;
    
  try
    KeysToRemove := TList<string>.Create;
    try
      // Find all keys related to this form
      for I := 0 to ASession.DataVars.Count - 1 do
      begin
        SessionKey := ASession.DataVars.Names[I];
        if SessionKey.StartsWith(FORM_DATA_PREFIX + AFormName + '_') or
           SessionKey.StartsWith(FORM_ERRORS_PREFIX + AFormName + '_') then
        begin
          KeysToRemove.Add(SessionKey);
        end;
      end;
      
      // Remove all found keys by setting them to empty
      for SessionKey in KeysToRemove do
      begin
        I := ASession.DataVars.IndexOfName(SessionKey);
        if I >= 0 then
          ASession.DataVars.Delete(I);
      end;
    finally
      KeysToRemove.Free;
    end;
    
    Logger.Debug(Format('Cleared form data for form: %s', [AFormName]));
  except
    on E: Exception do
      Logger.Error(Format('Error clearing form data: %s', [E.Message]));
  end;
end;

class function TFormSessionManager.HasFormData(ASession: TWebSession; const AFormName: string): Boolean;
var
  I: Integer;
  SessionKey: string;
  Prefix: string;
begin
  Result := False;
  
  if not Assigned(ASession) then
  begin
    Logger.Warning('HasFormData: Invalid parameters');
    Exit;
  end;
    
  try
    Prefix := FORM_DATA_PREFIX + AFormName + '_';
    
    // Check if any form data exists
    for I := 0 to ASession.DataVars.Count - 1 do
    begin
      SessionKey := ASession.DataVars.Names[I];
      if SessionKey.StartsWith(Prefix) and not SessionKey.EndsWith(FORM_TIMESTAMP_SUFFIX) then
      begin
        Result := True;
        Break;
      end;
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error checking form data: %s', [E.Message]));
  end;
end;

end. 