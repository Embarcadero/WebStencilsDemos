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
  Models.PaginationParams;

type

  TCustomersController = class
  private
    FCustomers: TFDQuery;
    FWebStencilsProcessor: TWebStencilsProcessor;
    FWebStencilsEngine: TWebStencilsEngine;
    function RenderTemplate(ATemplate: string; APaginationParams: TPaginationParams = nil): string;
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

function TCustomersController.RenderTemplate(ATemplate: string; APaginationParams: TPaginationParams = nil): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.rootDirectory, 'customers/' + ATemplate + '.html');
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.AddVar('customersPagination', APaginationParams, False);
  Result := FWebStencilsProcessor.Content;
  if Assigned(APaginationParams) then
    FWebStencilsProcessor.DataVars.Remove('customersPagination');
end;

constructor TCustomersController.Create(AWebStencilsEngine: TWebStencilsEngine; ACustomers: TFDQuery);
begin
  inherited Create;
  try
    FWebStencilsEngine := AWebStencilsEngine;
    FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
    FWebStencilsProcessor.Engine := FWebStencilsEngine;
    FCustomers := ACustomers;
  except
    on E: Exception do
      WriteLn('TCustomersController.Create: ' + E.Message);
  end;
end;

destructor TCustomersController.Destroy;
begin
  FWebStencilsProcessor.Free;
  inherited;
end;

procedure TCustomersController.GetCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  var LPaginationParams := TPaginationParams.Create(Request, 'pagination');
  try
    FCustomers.PageSize := LPaginationParams.PageSize;
    FCustomers.PageNumber := LPaginationParams.PageNumber;
    FCustomers.ApplyPagination;
    LPaginationParams.TotalPages := FCustomers.TotalPages;
    FWebStencilsProcessor.WebRequest := Request;
    Response.Content := RenderTemplate('pagination', LPaginationParams);
    Handled := True;
  finally
    LPaginationParams.Free;
  end;
end;

procedure TCustomersController.GetAllCustomers(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  FCustomers.CancelPagination;
  FWebStencilsProcessor.WebRequest := Request;
  Response.Content := RenderTemplate('bigtable', nil);
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
    Response.StatusCode := 400;
    Response.Content := 'Customer ID is required';
    Handled := True;
    Exit;
  end;

  // Navigate to the specific customer record
  FCustomers.CancelPagination;
  FCustomers.Active := True;
  try
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      Response.StatusCode := 404;
      Response.Content := 'Customer not found';
      Handled := True;
      Exit;
    end;

    FWebStencilsProcessor.WebRequest := Request;
    Response.Content := RenderTemplate('edit', nil);
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
begin
  LCustomerId := Request.ContentFields.Values['id'];
  if LCustomerId = '' then
  begin
    Response.StatusCode := 400;
    Response.Content := 'Customer ID is required';
    Handled := True;
    Exit;
  end;
  FCustomers.Active := True;
  try
    // Navigate to the specific customer record
    FCustomers.CancelPagination;
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      Response.StatusCode := 404;
      Response.Content := 'Customer not found';
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
    
    // Redirect back to pagination view
    Response.StatusCode := 302;
    Response.Location := Request.GetFieldByName('HTTP_REFERER');
    if Response.Location = '' then
      Response.Location := '/pagination';
    Response.Content := 'Customer updated successfully';
    
  except
    on E: Exception do
    begin
      FCustomers.Cancel;
      Response.StatusCode := 500;
      Response.Content := 'Error updating customer: ' + E.Message;
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
    Response.StatusCode := 400;
    Response.Content := 'Customer ID is required';
    Handled := True;
    Exit;
  end;

  try
    // Navigate to the specific customer record
    FCustomers.CancelPagination;
    if not FCustomers.Locate('id', LCustomerId, []) then
    begin
      Response.StatusCode := 404;
      Response.Content := 'Customer not found';
      Handled := True;
      Exit;
    end;

    // Delete the record
    FCustomers.Delete;
    
    // Redirect back to pagination view
    Response.StatusCode := 302;
    Response.Location := '/customers/pagination';
    Response.Content := 'Customer deleted successfully';
    
  except
    on E: Exception do
    begin
      Response.StatusCode := 500;
      Response.Content := 'Error deleting customer: ' + E.Message;
    end;
  end;
  
  Handled := True;
end;

end.
