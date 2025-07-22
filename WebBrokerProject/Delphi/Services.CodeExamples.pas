{
  This unit contains code examples used in the WebStencils demo.
  It provides a centralized location for all code snippets displayed in the demo pages.
}

unit Services.CodeExamples;

interface

uses
  System.Generics.Collections, System.SysUtils, Web.Stencils;

type

  TCodeExample = class
  private
    FId: string;
    FCode: string;
  public
    constructor Create(AId: string; const ACode: string);
    property Id: string read FId;
    property Code: string read FCode write FCode;
  end;

{
  TCodeExamples manages a collection of code examples used throughout the demo.
  It uses a TObjectDictionary to store and retrieve examples efficiently.
}
  TCodeExamples = class
    FItems: TObjectDictionary<string, TCodeExample>;
    FWebStencilsEngine: TWebStencilsEngine;
  private
    procedure InitValues;
  public
    procedure Add(AId: string; const ACode: string);
    function Find(AId: string): TCodeExample;
    constructor Create(AWebStencilsEngine: TWebStencilsEngine);
    destructor Destroy; override;
  end;

implementation

{ TCodeExample }

constructor TCodeExample.Create(AId: string; const ACode: string);
begin
  FId := AId;
  FCode := ACode;
end;

{ TCodeExamples }

constructor TCodeExamples.Create(AWebStencilsEngine: TWebStencilsEngine);
begin
  inherited Create;
  FItems := TObjectDictionary<string, TCodeExample>.Create;
  FWebStencilsEngine := AWebStencilsEngine;
  self.InitValues;
  for var Pair in FItems do
  begin
    var Key := Pair.Key;
    var CodeExample := Pair.Value;
    FWebStencilsEngine.AddVar(Key, CodeExample);
  end;
end;

destructor TCodeExamples.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TCodeExamples.Add(AId: string; const ACode: string);
begin
  FItems.Add(AId, TCodeExample.Create(AId, ACode));
end;

function TCodeExamples.Find(AId: string): TCodeExample;
begin
  FItems.TryGetValue(AId, Result);
end;

procedure TCodeExamples.InitValues;
begin
  Add('codeAtSymbol', '@object.value');
  Add('codeDotNotation',
      '''
      <h2>User Profile</h2>
      <p>Name: @user.name</p>
      <p>Email: @user.email</p>
      ''');
  Add('codeComments',
      '''
      @* This is a comment and will not appear in the output *@
      <p>This will appear in the output</p>
      ''');
  Add('codeIfAndElse',
      '''
      @if user.isLoggedIn {
          <p>Welcome, @user.name!</p>
      }
      @else {
          <p>Please log in to continue.</p>
      }
      ''');
  Add('codeIfNot',
      '''
      @if not cart.isEmpty {
        <p>You have @cart.itemCount items in your cart.</p>
      }
      @else {
        <p>Your cart is empty.</p>
      }
      ''');
  Add('codeForeach',
      '''
      <ul>
      @ForEach (var product in productList) {
          <li>@product.name - @product.price</li>
      }
      </ul>
      ''');
  Add('codeQuery', '<p>You searched for: @query.searchTerm</p>');
  Add('codePage', '<p>Current page is: @page.pagename</p>');
  Add('codeDataObject', 'WebStencilsProcessor1.AddVar(''user'', UserObject);');
  Add('codeDataMethod',
      '''
      WebStencilsProcessor1.AddVar('products',
        function: TObject
        begin
          Result := GetProductList;
        end);
      ''');
  Add('codeDataAttribute',
      '''
      type
        TMyDataModule = class(TDataModule)
          [WebStencilsVar('customers', false)]
          FDMemTable1: TFDMemTable;
          [WebStencilsVar('users')]
          FUsers: TUsers;
        end;

      WebStencilsProcessor1.AddModule(DataModule1);
      ''');
  Add('codeLayoutPage',
      '''
      @LayoutPage BaseTemplate
      <h1>Welcome to My Page</h1>
      <p>This is the content of my page.</p>
      ''');
  Add('codeRenderBody',
      '''
      <!DOCTYPE html>
      <html>
      <head>
          <title>My Website</title>
      </head>
      <body>
          <header>
              <!-- Common header content -->
          </header>

          <main>
              @RenderBody
          </main>

          <footer>
              <!-- Common footer content -->
          </footer>
      </body>
      </html>
      ''');
  Add('codeImport',
      '''
      @Import Sidebar.html

      @* Same behaviour as the previous one *@
      @Import Sidebar

      @* Use nested templates for better organization *@
      @Import folder/Sidebar
      ''');

  Add('codeHeaderBodyFooter',
      '''
      @Import Header.html

      <main>
          <!-- Page-specific content here -->
      </main>

      @Import Footer.html
      ''');

  Add('codeReusableComponents',
      '''
      <div class="product-list">
          @Import ProductList { @list = @ProductList }
      </div>

      <div class="tasks">
        @ForEach (var Task in Tasks.AllTasks) {
              @Import partials/tasks/item { @Task }
          }
      </div>
      ''');
  Add('codeSessionProperties',
      '''
      @session.Id
      @session.Authenticated
      @session.UserName
      @session.UserRoles
      @session.CreatedTime
      @session.AccessedTime
      @session.AccessedCount
      @session.Timeout
      ''');
  Add('codeSessionInformation',
      '''
      @session.LastURL
      @session.IP
      @session.LastReferer
      @session.LastUserAgent
      ''');
end;

end.
