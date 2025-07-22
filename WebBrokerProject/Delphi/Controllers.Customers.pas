unit Controllers.Customers;

interface

uses
  System.SysUtils,
  System.IOutils,
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
    function GetPageUrl(APaginationParams: TPaginationParams; ASearchParams: TSearchParams; APage: Integer): string;
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
  inherited Create(AWebStencilsEngine);
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

function TCustomersController.GetPageUrl(APaginationParams: TPaginationParams; ASearchParams: TSearchParams; APage: Integer): string;
begin
  Result := APaginationParams.GetPageUrl(APage, ASearchParams);
end;

procedure TCustomersController.GetCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LPaginationParams: TPaginationParams;
  LSearchParams: TSearchParams;
begin
  LPaginationParams := TPaginationParams.Create(Request, 'pagination');
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
    
    Response.Content := RenderCustomerTemplate('pagination', Request, LPaginationParams, LSearchParams);
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
  // Prepare empty customer record for form fields
  FCustomers.CancelPagination;
  FCustomers.Active := True;
  try
    // Add a temporary empty record to provide field structure
    FCustomers.Append;
    
    Response.Content := RenderCustomerTemplate('add', Request);
    Handled := True;
  finally
    // Cancel the append operation without saving
    FCustomers.Cancel;
    FCustomers.Active := False;
  end;
end;

procedure TCustomersController.CreateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LField: TField;
  LFieldName: string;
  LFieldValue: string;
begin
  FCustomers.Active := True;
  try
    FCustomers.CancelPagination;
    
    // Begin adding new record
    FCustomers.Append;
    
    // Set all fields from the form
    for var i := 0 to Request.ContentFields.Count - 1 do
    begin
      LFieldName := Request.ContentFields.Names[i];
      LFieldValue := Request.ContentFields.Values[LFieldName];
      
      // Skip the ID field (should be auto-generated)
      if SameText(LFieldName, 'id') then
        Continue;
        
      // Find the field in the dataset
      LField := FCustomers.FindField(LFieldName);
      if Assigned(LField) then
      begin
        if LFieldValue = '' then
          LField.Clear
        else
          LField.AsString := LFieldValue;
      end;
    end;
    
    // Save the new record
    FCustomers.Post;
    FCustomers.Close;
    
    // Add success message
    AddSuccessMessage(Request, 'Customer created successfully');
    
    // Redirect back to pagination view
    RedirectWithMessage(Response, '/pagination');
    
  except
    on E: Exception do
    begin
      FCustomers.Cancel;
      AddErrorMessage(Request, 'Error creating customer: ' + E.Message);
      RedirectWithMessage(Response, '/customers/add');
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
    RedirectWithMessage(Response, '/pagination');
    Handled := True;
    Exit;
  end;

  // Navigate to the specific customer record
  FCustomers.CancelPagination;
  FCustomers.Active := True;
  try
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      AddErrorMessage(Request, 'Customer not found');
      RedirectWithMessage(Response, '/pagination');
      Handled := True;
      Exit;
    end;

    Response.Content := RenderCustomerTemplate('edit', Request);
    Handled := True;
  finally
    FCustomers.Active := False;
  end;
end;

procedure TCustomersController.UpdateCustomer(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  LCustomerId: string;
  LField: TField;
  LFieldName: string;
  LFieldValue: string;
  LRedirectUrl: string;
begin
  LCustomerId := Request.ContentFields.Values['id'];
  if LCustomerId = '' then
  begin
    AddErrorMessage(Request, 'Customer ID is required');
    RedirectWithMessage(Response, '/pagination');
    Handled := True;
    Exit;
  end;
  
  FCustomers.Active := True;
  try
    // Navigate to the specific customer record
    FCustomers.CancelPagination;
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      AddErrorMessage(Request, 'Customer not found');
      RedirectWithMessage(Response, '/pagination');
      Handled := True;
      Exit;
    end;

    // Begin transaction
    FCustomers.Edit;
    
    // Update all fields from the form
    for var i := 0 to Request.ContentFields.Count - 1 do
    begin
      LFieldName := Request.ContentFields.Names[i];
      LFieldValue := Request.ContentFields.Values[LFieldName];
      
      // Skip the ID field
      if SameText(LFieldName, 'id') then
        Continue;
        
      // Find the field in the dataset
      LField := FCustomers.FindField(LFieldName);
      if Assigned(LField) then
      begin
        if LFieldValue = '' then
          LField.Clear
        else
          LField.AsString := LFieldValue;
      end;
    end;
    
    // Save changes
    FCustomers.Post;
    FCustomers.Close;
    
    // Add success message
    AddSuccessMessage(Request, 'Customer updated successfully');
    
    // Redirect back to pagination view
    LRedirectUrl := Request.GetFieldByName('HTTP_REFERER');
    if LRedirectUrl = '' then
      LRedirectUrl := '/pagination';
    RedirectWithMessage(Response, LRedirectUrl);
    
  except
    on E: Exception do
    begin
      FCustomers.Cancel;
      AddErrorMessage(Request, 'Error updating customer: ' + E.Message);
      RedirectWithMessage(Response, '/customers/edit?id=' + LCustomerId);
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
    RedirectWithMessage(Response, '/pagination');
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
      RedirectWithMessage(Response, '/pagination');
      Handled := True;
      Exit;
    end;

    // Delete the record
    FCustomers.Delete;
    FCustomers.Close;

    // Add success message
    AddSuccessMessage(Request, 'Customer deleted successfully');
    
    // Redirect back to pagination view
    RedirectWithMessage(Response, '/pagination');
    
  except
    on E: Exception do
    begin
      AddErrorMessage(Request, 'Error deleting customer: ' + E.Message);
      RedirectWithMessage(Response, '/pagination');
    end;
  end;
  
  Handled := True;
end;

end.
