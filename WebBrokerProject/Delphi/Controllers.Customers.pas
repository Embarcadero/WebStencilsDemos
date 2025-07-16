unit Controllers.Customers;

interface

uses
  System.SysUtils,
  System.IOutils,
  FireDAC.Comp.Client,
  Web.HTTPApp,
  Web.Stencils,
  Data.DB,

  Helpers.FDQuery,
  Models.PaginationParams,
  Controllers.Base;

type

  TCustomersController = class(TBaseController)
  private
    FCustomers: TFDQuery;
    function RenderCustomerTemplate(ATemplate: string; ARequest: TWebRequest; APaginationParams: TPaginationParams = nil): string;
    procedure ApplySearchFilter(APaginationParams: TPaginationParams);
    procedure ResetQuery;
  public
    procedure GetCustomers(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetAllCustomers(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure GetEditCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure UpdateCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure DeleteCustomer(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    constructor Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
    destructor Destroy; override;
  end;

implementation

function TCustomersController.RenderCustomerTemplate(ATemplate: string; ARequest: TWebRequest; APaginationParams: TPaginationParams = nil): string;
begin
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.AddVar('customersPagination', APaginationParams, False);
  
  Result := RenderTemplate('customers/' + ATemplate + '.html', ARequest);
  
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.DataVars.Remove('customersPagination');
end;

constructor TCustomersController.Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
begin
  inherited Create(AWebStencilsEngine);
  try
    FCustomers := ACustomers;
  except
    on E: Exception do
      WriteLn('TCustomersController.Create: ' + E.Message);
  end;
end;

destructor TCustomersController.Destroy;
begin
  inherited;
end;

procedure TCustomersController.ApplySearchFilter(APaginationParams: TPaginationParams);
begin
  // Reset query to base state
  ResetQuery;
  if APaginationParams.HasSearch then
  begin
    // Add search WHERE clause
    FCustomers.SQL.Add(APaginationParams.GetSearchSQL);

    // Set search parameter
    FCustomers.ParamByName('search').AsString := APaginationParams.SearchTerm;
  end;
end;

procedure TCustomersController.ResetQuery;
begin
  FCustomers.Close;
  FCustomers.SQL.Clear;
  FCustomers.SQL.Add('SELECT * FROM customers');
  FCustomers.Params.Clear;
end;

procedure TCustomersController.GetCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var LPaginationParams := TPaginationParams.Create(Request, 'pagination');
  try
    // Apply search filter if present
    ApplySearchFilter(LPaginationParams);
    
    // Apply pagination
    FCustomers.PageSize := LPaginationParams.PageSize;
    FCustomers.PageNumber := LPaginationParams.PageNumber;
    FCustomers.ApplyPagination;
    LPaginationParams.TotalPages := FCustomers.TotalPages;
    
    Response.Content := RenderCustomerTemplate('pagination', Request, LPaginationParams);
    Handled := True;
  finally
    LPaginationParams.Free;
  end;
end;

procedure TCustomersController.GetAllCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var LPaginationParams := TPaginationParams.Create(Request, 'bigtable');
  try
    // Apply search filter if present
    ApplySearchFilter(LPaginationParams);
    
    FCustomers.CancelPagination;
    Response.Content := RenderCustomerTemplate('bigtable', Request, LPaginationParams);
    Handled := True;
  finally
    LPaginationParams.Free;
  end;
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
