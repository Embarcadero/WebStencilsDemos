unit Controllers.Base;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Generics.Collections,
  System.DateUtils,
  Web.HTTPApp,
  Web.Stencils,
  FireDAC.Comp.Client,
  Data.DB,
  
  Helpers.Messages,
  Utils.FormSession,
  Utils.Logger;

type

  // Field error manager for WebStencils compatibility
  TFieldErrorManager = class
  private
    FErrors: TDictionary<string, string>;
    function GetErrorCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddError(const AFieldName, AMessage: string);
    procedure Clear;
  published
    function GetError(const AFieldName: string): string;
    function HasError(const AFieldName: string): Boolean;
    property ErrorCount: integer read GetErrorCount;
  end;

  TBaseController = class
  protected
    FWebStencilsEngine: TWebStencilsEngine;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FControllerName: string;
    FFieldErrorManager: TFieldErrorManager;

    function RenderTemplate(const ATemplatePath: string; ARequest: TWebRequest = nil): string;
    procedure Redirect(AResponse: TWebResponse; const ALocation: string);
    function GetCurrentSession(ARequest: TWebRequest): TWebSession;

    // Global message methods
    procedure AddSuccessMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddWarningMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddErrorMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddInfoMessage(ARequest: TWebRequest; const AMessage: string);

    // Form session management
    procedure StoreFormDataInSession(ARequest: TWebRequest; const AFormName: string);
    function GetFormDataFromSession(ARequest: TWebRequest; const AFormName: string): TDictionary<string, string>;
    procedure ClearFormSession(ARequest: TWebRequest; const AFormName: string);
    function HasFormDataInSession(ARequest: TWebRequest; const AFormName: string): Boolean;
    
    // Form data helpers
    procedure AssignFieldValue(AField: TField; const AValue: string);
    procedure RestoreFormDataToDataset(ADataset: TDataset; AFormData: TDictionary<string, string>);
    procedure PopulateDatasetFromRequest(ADataset: TDataset; ARequest: TWebRequest; const ASkipFields: TArray<string> = []);
    procedure RestoreFormData(ARequest: TWebRequest; const AFormName: string; ADataset: TDataset);
    procedure ClearFormSessionAfterProcessing(ARequest: TWebRequest; const AFormName: string);
    
    // Validation error management
    procedure InitializeValidationErrors;
    procedure AddValidationError(const AFieldName, AMessage: string);
    procedure ClearValidationErrors;
    function GetFieldError(const AFieldName: string): string;
    function HasFieldError(const AFieldName: string): Boolean;
    
    // Basic validation helpers
    function ValidateEmailField(const AFieldName, AValue: string): string;
    function ValidateMaxLength(const AFieldName, AValue: string; AMaxLength: Integer): string;
  public
    constructor Create(AWebStencilsEngine: TWebStencilsEngine; const AControllerName: string);
    destructor Destroy; override;
  end;

implementation

{ TFieldErrorManager }

constructor TFieldErrorManager.Create;
begin
  inherited Create;
  FErrors := TDictionary<string, string>.Create;
end;

destructor TFieldErrorManager.Destroy;
begin
  FErrors.Free;
  inherited;
end;

procedure TFieldErrorManager.AddError(const AFieldName, AMessage: string);
begin
  FErrors.AddOrSetValue(AFieldName.ToUpper, AMessage);
end;

function TFieldErrorManager.GetError(const AFieldName: string): string;
begin
  if FErrors.ContainsKey(AFieldName.ToUpper) then
    Result := FErrors[AFieldName.ToUpper]
  else
    Result := '';
end;

function TFieldErrorManager.HasError(const AFieldName: string): Boolean;
begin
  Result := FErrors.ContainsKey(AFieldName.ToUpper);
end;

procedure TFieldErrorManager.Clear;
begin
  FErrors.Clear;
end;

function TFieldErrorManager.GetErrorCount: Integer;
begin
  Result := FErrors.Count;
end;

{ TBaseController }

constructor TBaseController.Create(AWebStencilsEngine: TWebStencilsEngine; const AControllerName: string);
begin
  inherited Create;
  try
    FWebStencilsEngine := AWebStencilsEngine;
    FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FControllerName := AControllerName;
    
    // Initialize field error manager
    FFieldErrorManager := TFieldErrorManager.Create;
    
    // Initialize validation errors in template processor
    InitializeValidationErrors;
    
    Logger.Debug(Format('Created base controller: %s', [FControllerName]));
  except
    on E: Exception do
      Logger.Error(Format('TBaseController constructor error: %s', [E.Message]));
  end;
end;

destructor TBaseController.Destroy;
begin
  try
    Logger.Debug(Format('Destroyed base controller: %s', [FControllerName]));
    FFieldErrorManager.Free;
    FWebStencilsProcessor.Free;
  except
    on E: Exception do
      Logger.Error(Format('Error destroying base controller: %s', [E.Message]));
  end;
  inherited;
end;

function TBaseController.RenderTemplate(const ATemplatePath: string; ARequest: TWebRequest = nil): string;
begin
  try
    FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.RootDirectory, ATemplatePath);
    if Assigned(ARequest) then
    begin
      FWebStencilsProcessor.WebRequest := ARequest;
    end;
    Result := FWebStencilsProcessor.Content;
    
    // Clear validation errors after template processing
    ClearValidationErrors;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error rendering template %s: %s', [ATemplatePath, E.Message]));
      Result := '';
    end;
  end;
end;

function TBaseController.GetCurrentSession(ARequest: TWebRequest): TWebSession;
begin
  Result := nil;
  if Assigned(ARequest) then
    Result := ARequest.Session;
end;

procedure TBaseController.AddSuccessMessage(ARequest: TWebRequest; const AMessage: string);
begin
  var LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtSuccess, AMessage);
end;

procedure TBaseController.AddWarningMessage(ARequest: TWebRequest; const AMessage: string);
begin
  var LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtWarning, AMessage);
end;

procedure TBaseController.AddErrorMessage(ARequest: TWebRequest; const AMessage: string);
begin
  var LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtError, AMessage);
end;

procedure TBaseController.AddInfoMessage(ARequest: TWebRequest; const AMessage: string);
begin
  var LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtInfo, AMessage);
end;

procedure TBaseController.Redirect(AResponse: TWebResponse; const ALocation: string);
begin
  AResponse.StatusCode := 302;
  AResponse.Location := ALocation;
  AResponse.Content := '';
end;

{ Form Session Management }

procedure TBaseController.StoreFormDataInSession(ARequest: TWebRequest; const AFormName: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TFormSessionManager.StoreFormData(LSession, AFormName, ARequest);
end;



function TBaseController.GetFormDataFromSession(ARequest: TWebRequest; const AFormName: string): TDictionary<string, string>;
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    Result := TFormSessionManager.GetFormData(LSession, AFormName)
  else
    Result := TDictionary<string, string>.Create;
end;



procedure TBaseController.ClearFormSession(ARequest: TWebRequest; const AFormName: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TFormSessionManager.ClearFormData(LSession, AFormName);
end;

function TBaseController.HasFormDataInSession(ARequest: TWebRequest; const AFormName: string): Boolean;
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    Result := TFormSessionManager.HasFormData(LSession, AFormName)
  else
    Result := False;
end;

{ Form Data Helpers }

procedure TBaseController.AssignFieldValue(AField: TField; const AValue: string);
var
  DateValue: TDateTime;
begin
  if not Assigned(AField) then
    Exit;
    
  try
    if AValue = '' then
      AField.Clear
    else
    begin
      // Handle date/time field conversions using ISO 8601 functions
      if (AField.DataType = ftDate) or (AField.DataType = ftDateTime) or (AField.DataType = ftTime) then
      begin
        if TryISO8601ToDate(AValue, DateValue, False) then
          AField.AsDateTime := DateValue
        else
          AField.AsString := AValue; // Fallback to string
      end
      else if AField.DataType = ftBoolean then
      begin
        // For boolean fields, if the field is present in POST data, it's True
        // If not present, it's False (handled by the hidden field)
        if AValue = 'True' then
          AField.AsBoolean := True
        else
          AField.AsBoolean := False;
      end
      else
        AField.AsString := AValue;
    end;
  except
    on E: Exception do
      Logger.Warning(Format('Field assignment warning for %s: %s', [AField.FieldName, E.Message]));
  end;
end;

procedure TBaseController.RestoreFormDataToDataset(ADataset: TDataset; AFormData: TDictionary<string, string>);
var
  Pair: TPair<string, string>;
  Field: TField;
begin
  if not Assigned(ADataset) or not Assigned(AFormData) then
    Exit;
    
  for Pair in AFormData do
  begin
    Field := ADataset.FindField(Pair.Key);
    if Assigned(Field) then
      AssignFieldValue(Field, Pair.Value);
  end;
end;

procedure TBaseController.PopulateDatasetFromRequest(ADataset: TDataset; ARequest: TWebRequest; const ASkipFields: TArray<string> = []);
var
  I: Integer;
  FieldName, FieldValue: string;
  Field: TField;
  SkipField: Boolean;
  SkipName: string;
begin
  if not Assigned(ADataset) or not Assigned(ARequest) then
    Exit;
    
  for I := 0 to ARequest.ContentFields.Count - 1 do
  begin
    FieldName := ARequest.ContentFields.Names[I];
    FieldValue := ARequest.ContentFields.ValueFromIndex[I];
    
    // Check if this field should be skipped
    SkipField := False;
    for SkipName in ASkipFields do
    begin
      if SameText(FieldName, SkipName) then
      begin
        SkipField := True;
        Break;
      end;
    end;
    
    if not SkipField then
    begin
      Field := ADataset.FindField(FieldName);
      if Assigned(Field) then
        AssignFieldValue(Field, FieldValue);
    end;
  end;
end;

procedure TBaseController.RestoreFormData(ARequest: TWebRequest; const AFormName: string; ADataset: TDataset);
var
  FormData: TDictionary<string, string>;
begin
  // Check if we have saved form data from previous error
  if HasFormDataInSession(ARequest, AFormName) then
  begin
    FormData := GetFormDataFromSession(ARequest, AFormName);
    try
      // Restore form data to dataset
      RestoreFormDataToDataset(ADataset, FormData);
      
      // NOTE: Session data will be cleared AFTER template processing
      // to ensure data is available during template rendering
    finally
      FormData.Free;
    end;
  end;
end;

procedure TBaseController.ClearFormSessionAfterProcessing(ARequest: TWebRequest; const AFormName: string);
begin
  // Clear session data AFTER template has been processed
  ClearFormSession(ARequest, AFormName);
end;

{ Validation Error Management }

procedure TBaseController.InitializeValidationErrors;
begin
  // Always add field error manager to template processor (empty by default)
  FWebStencilsProcessor.AddVar('fieldErrors', FFieldErrorManager, False);
end;

procedure TBaseController.AddValidationError(const AFieldName, AMessage: string);
begin
  FFieldErrorManager.AddError(AFieldName, AMessage);
end;

procedure TBaseController.ClearValidationErrors;
begin
  FFieldErrorManager.Clear;
end;

function TBaseController.GetFieldError(const AFieldName: string): string;
begin
  Result := FFieldErrorManager.GetError(AFieldName);
end;

function TBaseController.HasFieldError(const AFieldName: string): Boolean;
begin
  Result := FFieldErrorManager.HasError(AFieldName);
end;

function TBaseController.ValidateEmailField(const AFieldName, AValue: string): string;
begin
  Result := '';
  if (AValue <> '') and (Pos('@', AValue) = 0) then
    Result := Format('%s must be a valid email address', [AFieldName.Replace('_', ' ')]);
end;

function TBaseController.ValidateMaxLength(const AFieldName, AValue: string; AMaxLength: Integer): string;
begin
  Result := '';
  if Length(AValue) > AMaxLength then
    Result := Format('%s cannot exceed %d characters', [AFieldName.Replace('_', ' '), AMaxLength]);
end;

end. 