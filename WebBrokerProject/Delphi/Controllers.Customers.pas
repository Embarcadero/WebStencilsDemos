unit Controllers.Customers;

interface

uses
  System.SysUtils,
  System.IOutils,
  FireDAC.Comp.Client,
  Web.HTTPApp,
  Web.Stencils,

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

end.
