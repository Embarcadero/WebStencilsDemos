unit Controllers.Customers;

interface

uses
  System.SysUtils,
  System.IOutils,
  System.Generics.Collections,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  Web.HTTPApp,
  Web.Stencils,
  Data.DB,

  Helpers.FDQuery,
  Utils.PaginationParams,
  Controllers.Base,
  Utils.Search;

type

  TCustomersController = class(TBaseController)
  private
    FCustomers: TFDQuery;
    FCustomerSearch: TBaseSearch;
    function RenderCustomerTemplate(ATemplate: string; ARequest: TWebRequest; APaginationParams: TPaginationParams = nil; ASearchParams: TSearchParams = nil): string;
    procedure ResetQuery;
    procedure ApplySearchToQuery(ASearchParams: TSearchParams);
    function ValidateCustomerForm(ARequest: TWebRequest): TArray<string>;
  public
    procedure GetCustomers(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetAllCustomers(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetAddCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure CreateCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetEditCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure UpdateCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure DeleteCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    constructor Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
    destructor Destroy; override;
  end;

implementation

function TCustomersController.RenderCustomerTemplate(ATemplate: string; ARequest: TWebRequest; APaginationParams: TPaginationParams = nil; ASearchParams: TSearchParams = nil): string;
begin
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.AddVar('customersPagination', APaginationParams, False);
  
  if Assigned(ASearchParams) then
    FWebStencilsProcessor.AddVar('customersSearchParams', ASearchParams, False);
  
  Result := RenderTemplate('customers/' + ATemplate + '.html', ARequest);
  
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.DataVars.Remove('customersPagination');
    
  if Assigned(ASearchParams) then
    FWebStencilsProcessor.DataVars.Remove('customersSearchParams');
end;

constructor TCustomersController.Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
begin
  inherited Create(AWebStencilsEngine, 'customers');
  try
    FCustomers := ACustomers;
    // Initialize customer search with the desired fields
    FCustomerSearch := TBaseSearch.Create(['FIRST_NAME', 'LAST_NAME', 'EMAIL', 'COMPANY', 'PHONE', 'CITY']);
  except
    on E: Exception do
      WriteLn('TCustomersController.Create: ' + E.Message);
  end;
end;

destructor TCustomersController.Destroy;
begin
  FCustomerSearch.Free;
  inherited;
end;



procedure TCustomersController.ResetQuery;
begin
  FCustomers.Close;
  FCustomers.SQL.Clear;
  FCustomers.SQL.Add('SELECT * FROM customers');
  FCustomers.Params.Clear;
end;

procedure TCustomersController.ApplySearchToQuery(ASearchParams: TSearchParams);
var
  SearchSQL: string;
begin
  if not Assigned(ASearchParams) or not ASearchParams.HasSearch then
    Exit;

  SearchSQL := ASearchParams.GetSearchSQL;
  if SearchSQL <> '' then
  begin
    FCustomers.SQL.Add(SearchSQL);
    FCustomers.ParamByName('search').AsString := '%' + ASearchParams.SearchTerm + '%';
  end;
end;

function TCustomersController.ValidateCustomerForm(ARequest: TWebRequest): TArray<string>;
var
  Errors: TList<string>;
  Email, FirstName, LastName: string;
  EmailError, MaxLengthError: string;
begin
  Errors := TList<string>.Create;
  try
    // Validate required fields
    Errors.AddRange(ValidateRequiredFields(ARequest, ['first_name', 'last_name', 'email']));
    
    // Validate email format
    Email := ARequest.ContentFields.Values['email'];
    EmailError := ValidateEmailField('email', Email);
    if EmailError <> '' then
      Errors.Add(EmailError);
    
    // Validate field lengths
    FirstName := ARequest.ContentFields.Values['first_name'];
    MaxLengthError := ValidateMaxLength('first_name', FirstName, 50);
    if MaxLengthError <> '' then
      Errors.Add(MaxLengthError);
      
    LastName := ARequest.ContentFields.Values['last_name'];
    MaxLengthError := ValidateMaxLength('last_name', LastName, 50);
    if MaxLengthError <> '' then
      Errors.Add(MaxLengthError);
    
    MaxLengthError := ValidateMaxLength('email', Email, 100);
    if MaxLengthError <> '' then
      Errors.Add(MaxLengthError);
    
    Result := Errors.ToArray;
  finally
    Errors.Free;
  end;
end;

procedure TCustomersController.GetCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPaginationParams: TPaginationParams;
  LSearchParams: TSearchParams;
begin
  LPaginationParams := TPaginationParams.Create(Request, 'customers');
  LSearchParams := TSearchParams.Create(Request, FCustomerSearch);
  try
    // Reset query and apply search filter if present
    ResetQuery;
    ApplySearchToQuery(LSearchParams);
    
    // Apply pagination
    FCustomers.PageSize := LPaginationParams.PageSize;
    FCustomers.PageNumber := LPaginationParams.PageNumber;
    FCustomers.ApplyPagination;
    LPaginationParams.TotalPages := FCustomers.TotalPages;
    
    Response.Content := RenderCustomerTemplate('index', Request, LPaginationParams, LSearchParams);
    Handled := True;
  finally
    LPaginationParams.Free;
    LSearchParams.Free;
  end;
end;

procedure TCustomersController.GetAllCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPaginationParams: TPaginationParams;
  LSearchParams: TSearchParams;
begin
  LPaginationParams := TPaginationParams.Create(Request, 'bigtable');
  LSearchParams := TSearchParams.Create(Request, FCustomerSearch);
  try
    // Reset query and apply search filter if present
    ResetQuery;
    ApplySearchToQuery(LSearchParams);
    
    FCustomers.CancelPagination;
    Response.Content := RenderCustomerTemplate('bigtable', Request, LPaginationParams, LSearchParams);
    Handled := True;
  finally
    LPaginationParams.Free;
    LSearchParams.Free;
  end;
end;

procedure TCustomersController.GetAddCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  FCustomers.CancelPagination;
  FCustomers.Active := True;
  try
    FCustomers.Append;
    
    // Restore form data and errors if available (from validation errors)
    RestoreFormDataAndErrors(Request, 'customer_add', FCustomers);
    
    Response.Content := RenderCustomerTemplate('add', Request);
    
    // Clear session data AFTER template has been processed
    ClearFormSessionAfterProcessing(Request, 'customer_add');
    
    Handled := True;
  finally
    FCustomers.Cancel;
    FCustomers.Active := False;
  end;
end;

procedure TCustomersController.CreateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  ValidationErrors: TArray<string>;
begin
  // 1. Validate form data first
  ValidationErrors := ValidateCustomerForm(Request);
  if Length(ValidationErrors) > 0 then
  begin
    StoreFormDataInSession(Request, 'customer_add');
    StoreValidationErrors(Request, 'customer_add', ValidationErrors);
    AddErrorMessage(Request, 'Please correct the errors below');
    Redirect(Response, '/customers/add');
    Handled := True;
    Exit;
  end;
  
  // 2. Try to save the customer
  FCustomers.Active := True;
  try
    FCustomers.CancelPagination;
    FCustomers.Append;
    
    // Populate dataset from form data
    PopulateDatasetFromRequest(FCustomers, Request, ['id']);
    
    // Save the new record
    FCustomers.Post;
    FCustomers.Close;
    
    // Clear any saved form data on success
    ClearFormSession(Request, 'customer_add');
    
    AddSuccessMessage(Request, 'Customer created successfully');
    Redirect(Response, '/customers');
    
  except
    on E: Exception do
    begin
      FCustomers.Cancel;
      
      // Store form data for redisplay on error
      StoreFormDataInSession(Request, 'customer_add');
      AddErrorMessage(Request, 'Error creating customer: ' + E.Message);
      Redirect(Response, '/customers/add');
    end;
  end;
  
  Handled := True;
end;

procedure TCustomersController.GetEditCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
begin
  LCustomerId := Request.QueryFields.Values['id'];
  if LCustomerId = '' then
  begin
    AddErrorMessage(Request, 'Customer ID is required');
    Redirect(Response, '/customers');
    Handled := True;
    Exit;
  end;

  FCustomers.CancelPagination;
  FCustomers.Active := True;
  try
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      AddErrorMessage(Request, 'Customer not found');
      Redirect(Response, '/customers');
      Handled := True;
      Exit;
    end;
    
    FCustomers.Edit;
    
    // Restore form data and errors if available (from validation errors)
    RestoreFormDataAndErrors(Request, 'customer_edit', FCustomers);

    Response.Content := RenderCustomerTemplate('edit', Request);
    
    // Clear session data AFTER template has been processed
    ClearFormSessionAfterProcessing(Request, 'customer_edit');
    
    Handled := True;
  finally
    FCustomers.Cancel;
    FCustomers.Active := False;
  end;
end;

procedure TCustomersController.UpdateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
  LRedirectUrl: string;
  ValidationErrors: TArray<string>;
begin
  LCustomerId := Request.ContentFields.Values['id'];
  if LCustomerId = '' then
  begin
    AddErrorMessage(Request, 'Customer ID is required');
    Redirect(Response, '/customers');
    Handled := True;
    Exit;
  end;
  
  // 1. Validate form data first
  ValidationErrors := ValidateCustomerForm(Request);
  if Length(ValidationErrors) > 0 then
  begin
    StoreFormDataInSession(Request, 'customer_edit');
    StoreValidationErrors(Request, 'customer_edit', ValidationErrors);
    AddErrorMessage(Request, 'Please correct the errors below');
    Redirect(Response, '/customers/edit?id=' + LCustomerId);
    Handled := True;
    Exit;
  end;
  
  // 2. Try to update the customer
  FCustomers.Active := True;
  try
    FCustomers.CancelPagination;
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      AddErrorMessage(Request, 'Customer not found');
      Redirect(Response, '/customers');
      Handled := True;
      Exit;
    end;

    FCustomers.Edit;
    
    // Populate dataset from form data
    PopulateDatasetFromRequest(FCustomers, Request, ['id']);
    
    FCustomers.Post;
    FCustomers.Close;
    
    // Clear any saved form data on success
    ClearFormSession(Request, 'customer_edit');
    
    AddSuccessMessage(Request, 'Customer updated successfully');
    
    LRedirectUrl := Request.GetFieldByName('HTTP_REFERER');
    if LRedirectUrl = '' then
      LRedirectUrl := '/customers';
    Redirect(Response, LRedirectUrl);

  except
    on E: Exception do
    begin
      FCustomers.Cancel;

      // Store form data for redisplay on error
      StoreFormDataInSession(Request, 'customer_edit');
      AddErrorMessage(Request, 'Error updating customer: ' + E.Message);
      Redirect(Response, '/customers/edit?id=' + LCustomerId);
    end;
  end;
  
  Handled := True;
end;

procedure TCustomersController.DeleteCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
begin
  LCustomerId := Request.ContentFields.Values['id'];
  if LCustomerId = '' then
  begin
    AddErrorMessage(Request, 'Customer ID is required');
    Redirect(Response, '/customers');
    Handled := True;
    Exit;
  end;

  try
    // Navigate to the specific customer record
    FCustomers.Active := True;
    FCustomers.CancelPagination;
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      AddErrorMessage(Request, 'Customer not found');
      Redirect(Response, '/customers');
      Handled := True;
      Exit;
    end;

    // Delete the record
    FCustomers.Delete;
    FCustomers.Close;

    // Add success message
    AddSuccessMessage(Request, 'Customer deleted successfully');
    
    // Redirect back to pagination view
    Redirect(Response, '/customers');
    
  except
    on E: Exception do
    begin
      AddErrorMessage(Request, 'Error deleting customer: ' + E.Message);
      Redirect(Response, '/customers');
    end;
  end;
  
  Handled := True;
end;

end.
