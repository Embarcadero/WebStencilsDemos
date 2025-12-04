unit Module.Main;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, FireDAC.Stan.Async,
  FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef,
  FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat,
  FireDAC.ConsoleUI.Wait;

type

  TWebModuleMain = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    [WebStencilsVar('customers', false)]
    Customers: TFDQuery;
    CustomersID: TFDAutoIncField;
    CustomersCOMPANY: TStringField;
    CustomersFIRST_NAME: TStringField;
    CustomersLAST_NAME: TStringField;
    CustomersGENDER: TStringField;
    CustomersAGE: TIntegerField;
    CustomersPOSTAL_CODE: TStringField;
    CustomersADDRESS: TStringField;
    CustomersCITY: TStringField;
    CustomersCOUNTRY: TStringField;
    CustomersPHONE: TStringField;
    CustomersEMAIL: TStringField;
    CustomersIP_ADDRESS: TStringField;
    CustomersACTIVATION_DATE: TDateField;
    CustomersACTIVE: TBooleanField;
    CustomersCOMMENTS: TWideMemoField;
    Connection: TFDConnection;
    procedure WebModuleMainDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleMain;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

procedure TWebModuleMain.WebModuleMainDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  Customers.Locate('ID', Random(500), []);
  Response.Content := WSProcessor.Content;
end;

procedure TWebModuleMain.WebModuleCreate(Sender: TObject);
begin
  // Configure whitelist to expose FireDAC field metadata properties
  // This allows templates to access field properties like DisplayText, DataType, Required, etc.
  TWebStencilsProcessor.Whitelist.Configure(TField, ['DisplayText', 'Value', 'DisplayLabel', 'FieldName', 'Required', 'Visible', 'DataType', 'Size', 'IsNull'], nil, False);
  Connection.Params.Database := '..\..\database.sqlite3';
  customers.Active := True;
end;

end.

