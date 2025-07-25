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
  Utils.FormSession;

type
  // Simple wrapper class for validation errors (WebStencils compatible)
  TValidationError = class
  private
    FMessage: string;
  public
    constructor Create(const AMessage: string);
    property Message: string read FMessage;
  end;

  TBaseController = class
  protected
    FWebStencilsEngine: TWebStencilsEngine;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FControllerName: string;
    FValidationErrors: TObjectList<TValidationError>;

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
    procedure StoreValidationErrors(ARequest: TWebRequest; const AFormName: string; const AErrors: TArray<string>);
    function GetFormDataFromSession(ARequest: TWebRequest; const AFormName: string): TDictionary<string, string>;
    function GetValidationErrors(ARequest: TWebRequest; const AFormName: string): TArray<string>;
    procedure ClearFormSession(ARequest: TWebRequest; const AFormName: string);
    function HasFormDataInSession(ARequest: TWebRequest; const AFormName: string): Boolean;
    
    // Form data helpers
    procedure AssignFieldValue(AField: TField; const AValue: string);
    procedure RestoreFormDataToDataset(ADataset: TDataset; AFormData: TDictionary<string, string>);
    procedure PopulateDatasetFromRequest(ADataset: TDataset; ARequest: TWebRequest; const ASkipFields: TArray<string> = []);
    procedure AddValidationErrorsToTemplate(ARequest: TWebRequest; const AFormName: string);
    procedure ClearValidationErrorsFromTemplate;
    procedure RestoreFormDataAndErrors(ARequest: TWebRequest; const AFormName: string; ADataset: TDataset);
    procedure ClearFormSessionAfterProcessing(ARequest: TWebRequest; const AFormName: string);
    
    // Validation error management
    procedure InitializeValidationErrors;
    procedure AddValidationError(const AMessage: string);
    procedure ClearValidationErrors;
    function GetValidationErrorsVarName: string;
    
    // Basic validation helpers
    function ValidateRequiredFields(ARequest: TWebRequest; const ARequiredFields: TArray<string>): TArray<string>;
    function ValidateEmailField(const AFieldName, AValue: string): string;
    function ValidateMaxLength(const AFieldName, AValue: string; AMaxLength: Integer): string;
  public
    constructor Create(AWebStencilsEngine: TWebStencilsEngine; const AControllerName: string);
    destructor Destroy; override;
  end;

implementation

{ TValidationError }

constructor TValidationError.Create(const AMessage: string);
begin
  inherited Create;
  FMessage := AMessage;
end;

{ TBaseController }

constructor TBaseController.Create(AWebStencilsEngine: TWebStencilsEngine; const AControllerName: string);
begin
  inherited Create;
  FWebStencilsEngine := AWebStencilsEngine;
  FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
  FWebStencilsProcessor.Engine := FWebStencilsEngine;
  FControllerName := AControllerName;
  
  // Initialize validation errors list
  FValidationErrors := TObjectList<TValidationError>.Create(True);
  
  // Initialize validation errors in template processor
  InitializeValidationErrors;
end;

destructor TBaseController.Destroy;
begin
  FValidationErrors.Free;
  FWebStencilsProcessor.Free;
  inherited;
end;

function TBaseController.RenderTemplate(const ATemplatePath: string; ARequest: TWebRequest = nil): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.RootDirectory, ATemplatePath);
  if Assigned(ARequest) then
  begin
    FWebStencilsProcessor.WebRequest := ARequest;
  end;
  Result := FWebStencilsProcessor.Content;
  
  // Clear validation errors after template processing
  ClearValidationErrors;
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

procedure TBaseController.StoreValidationErrors(ARequest: TWebRequest; const AFormName: string; const AErrors: TArray<string>);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TFormSessionManager.StoreValidationErrors(LSession, AFormName, AErrors);
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

function TBaseController.GetValidationErrors(ARequest: TWebRequest; const AFormName: string): TArray<string>;
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    Result := TFormSessionManager.GetValidationErrors(LSession, AFormName)
  else
    SetLength(Result, 0);
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

procedure TBaseController.AddValidationErrorsToTemplate(ARequest: TWebRequest; const AFormName: string);
var
  Errors: TArray<string>;
  ErrorText: string;
begin
  // Clear existing validation errors first
  ClearValidationErrors;
  
  // Get errors from session and add them to our global list
  Errors := GetValidationErrors(ARequest, AFormName);
  for ErrorText in Errors do
    AddValidationError(ErrorText);
end;

procedure TBaseController.ClearValidationErrorsFromTemplate;
begin
  // Clear validation errors from our global list
  ClearValidationErrors;
end;

procedure TBaseController.RestoreFormDataAndErrors(ARequest: TWebRequest; const AFormName: string; ADataset: TDataset);
var
  FormData: TDictionary<string, string>;
begin
  // Clear any existing validation errors first
  ClearValidationErrorsFromTemplate;
  
  // Check if we have saved form data from previous error
  if HasFormDataInSession(ARequest, AFormName) then
  begin
    FormData := GetFormDataFromSession(ARequest, AFormName);
    try
      // Restore form data to dataset
      RestoreFormDataToDataset(ADataset, FormData);
      
      // Add validation errors to template
      AddValidationErrorsToTemplate(ARequest, AFormName);
      
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
  // Always add validation errors variable to template processor (empty by default)
  FWebStencilsProcessor.AddVar(GetValidationErrorsVarName, FValidationErrors, False);
end;

procedure TBaseController.AddValidationError(const AMessage: string);
begin
  FValidationErrors.Add(TValidationError.Create(AMessage));
end;

procedure TBaseController.ClearValidationErrors;
begin
  FValidationErrors.Clear;
end;

function TBaseController.GetValidationErrorsVarName: string;
begin
  Result := FControllerName + 'ValidationErrors';
end;

{ Basic Validation Helpers }

function TBaseController.ValidateRequiredFields(ARequest: TWebRequest; const ARequiredFields: TArray<string>): TArray<string>;
var
  Errors: TList<string>;
  FieldName, FieldValue: string;
begin
  Errors := TList<string>.Create;
  try
    for FieldName in ARequiredFields do
    begin
      FieldValue := Trim(ARequest.ContentFields.Values[FieldName]);
      if FieldValue = '' then
        Errors.Add(Format('%s is required', [FieldName.Replace('_', ' ')]));
    end;
    
    Result := Errors.ToArray;
  finally
    Errors.Free;
  end;
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