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
  Utils.Search,
  Utils.Logger;

type

  TCustomersController = class(TBaseController)
  private
    FCustomers: TFDQuery;
    FCustomerSearch: TBaseSearch;
    function RenderCustomerTemplate(ATemplate: string; ARequest: TWebRequest; APaginationParams: TPaginationParams = nil; ASearchParams: TSearchParams = nil): string;
    procedure ResetQuery;
    procedure ApplySearchToQuery(ASearchParams: TSearchParams);
    procedure ValidateCustomerForm(ARequest: TWebRequest);
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
    Logger.Info('TCustomersController created successfully');
  except
    on E: Exception do
    begin
      Logger.Error(Format('TCustomersController.Create: %s', [E.Message]));
      WriteLn('TCustomersController.Create: ' + E.Message);
    end;
  end;
end;

destructor TCustomersController.Destroy;
begin
  try
    Logger.Info('TCustomersController destroying...');
    FCustomerSearch.Free;
    Logger.Info('TCustomersController destroyed successfully');
  except
    on E: Exception do
      Logger.Error(Format('Error destroying TCustomersController: %s', [E.Message]));
  end;
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

procedure TCustomersController.ValidateCustomerForm(ARequest: TWebRequest);
var
  Email, FirstName, LastName: string;
  EmailError, MaxLengthError: string;
begin
  // Clear any existing validation errors
  ClearValidationErrors;
  
  // Validate required fields
  if Trim(ARequest.ContentFields.Values['first_name']) = '' then
    AddValidationError('first_name', 'First name is required');
    
  if Trim(ARequest.ContentFields.Values['last_name']) = '' then
    AddValidationError('last_name', 'Last name is required');
    
  if Trim(ARequest.ContentFields.Values['email']) = '' then
    AddValidationError('email', 'Email is required');
  
  // Validate email format
  Email := ARequest.ContentFields.Values['email'];
  if Email <> '' then
  begin
    EmailError := ValidateEmailField('email', Email);
    if EmailError <> '' then
      AddValidationError('email', EmailError);
  end;
  
  // Validate field lengths
  FirstName := ARequest.ContentFields.Values['first_name'];
  if FirstName <> '' then
  begin
    MaxLengthError := ValidateMaxLength('first_name', FirstName, 50);
    if MaxLengthError <> '' then
      AddValidationError('first_name', MaxLengthError);
  end;
      
  LastName := ARequest.ContentFields.Values['last_name'];
  if LastName <> '' then
  begin
    MaxLengthError := ValidateMaxLength('last_name', LastName, 50);
    if MaxLengthError <> '' then
      AddValidationError('last_name', MaxLengthError);
  end;
  
  if Email <> '' then
  begin
    MaxLengthError := ValidateMaxLength('email', Email, 100);
    if MaxLengthError <> '' then
      AddValidationError('email', MaxLengthError);
  end;
end;

procedure TCustomersController.GetCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPaginationParams: TPaginationParams;
  LSearchParams: TSearchParams;
begin
  try
    Logger.Info('Getting customers with pagination');
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
      Logger.Info(Format('Customers retrieved successfully. Page %d of %d', [LPaginationParams.PageNumber, LPaginationParams.TotalPages]));
    finally
      LPaginationParams.Free;
      LSearchParams.Free;
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting customers: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TCustomersController.GetAllCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPaginationParams: TPaginationParams;
  LSearchParams: TSearchParams;
begin
  try
    Logger.Info('Getting all customers (big table view)');
    LPaginationParams := TPaginationParams.Create(Request, 'bigtable');
    LSearchParams := TSearchParams.Create(Request, FCustomerSearch);
    try
      // Reset query and apply search filter if present
      ResetQuery;
      ApplySearchToQuery(LSearchParams);
      
      FCustomers.CancelPagination;
      Response.Content := RenderCustomerTemplate('bigtable', Request, LPaginationParams, LSearchParams);
      Handled := True;
      Logger.Info('All customers retrieved successfully');
    finally
      LPaginationParams.Free;
      LSearchParams.Free;
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting all customers: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TCustomersController.GetAddCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    Logger.Info('Getting add customer form');
    FCustomers.CancelPagination;
    FCustomers.Active := True;
    try
      FCustomers.Append;
      
      // Restore form data if available (from validation errors)
      RestoreFormData(Request, 'customer_add', FCustomers);
      
      Response.Content := RenderCustomerTemplate('add', Request);
      
      // Clear session data AFTER template has been processed
      ClearFormSessionAfterProcessing(Request, 'customer_add');
      
      Handled := True;
      Logger.Info('Add customer form rendered successfully');
    finally
      FCustomers.Cancel;
      FCustomers.Active := False;
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting add customer form: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TCustomersController.CreateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  try
    Logger.Info('Creating new customer');
    
    // 1. Validate form data first
    ValidateCustomerForm(Request);
    if HasFieldError('first_name') or HasFieldError('last_name') or HasFieldError('email') then
    begin
      Logger.Warning('Customer validation failed');
      StoreFormDataInSession(Request, 'customer_add');
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
      Logger.Info('Customer created successfully');
      Redirect(Response, '/customers');
      
    except
      on E: Exception do
      begin
        FCustomers.Cancel;
        Logger.Error(Format('Error creating customer: %s', [E.Message]));
        
        // Store form data for redisplay on error
        StoreFormDataInSession(Request, 'customer_add');
        AddErrorMessage(Request, 'Error creating customer: ' + E.Message);
        Redirect(Response, '/customers/add');
      end;
    end;
    
    Handled := True;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Unexpected error in CreateCustomer: %s', [E.Message]));
      AddErrorMessage(Request, 'Unexpected error occurred');
      Redirect(Response, '/customers');
      Handled := True;
    end;
  end;
end;

procedure TCustomersController.GetEditCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
begin
  try
    LCustomerId := Request.QueryFields.Values['id'];
    Logger.Info(Format('Getting edit customer form for ID: %s', [LCustomerId]));
    
    if LCustomerId = '' then
    begin
      Logger.Warning('Customer ID is required for edit');
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
        Logger.Warning(Format('Customer not found for ID: %s', [LCustomerId]));
        AddErrorMessage(Request, 'Customer not found');
        Redirect(Response, '/customers');
        Handled := True;
        Exit;
      end;
      FCustomers.Edit;
      // Restore form data if available (from validation errors)
      RestoreFormData(Request, 'customer_edit', FCustomers);

      Response.Content := RenderCustomerTemplate('edit', Request);
      
      // Clear session data AFTER template has been processed
      ClearFormSessionAfterProcessing(Request, 'customer_edit');
      
      Handled := True;
      Logger.Info(Format('Edit customer form rendered successfully for ID: %s', [LCustomerId]));
    finally
      FCustomers.Cancel;
      FCustomers.Active := False;
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting edit customer form: %s', [E.Message]));
      Handled := True;
    end;
  end;
end;

procedure TCustomersController.UpdateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
  LRedirectUrl: string;
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
  ValidateCustomerForm(Request);
  if HasFieldError('first_name') or HasFieldError('last_name') or HasFieldError('email') then
  begin
    StoreFormDataInSession(Request, 'customer_edit');
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
