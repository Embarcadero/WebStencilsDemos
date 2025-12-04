unit Module.Main;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils;

type

  TMap = TDictionary<string, string>;

  TWebModuleMain = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    procedure WebModuleMainDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
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
var
  LDict: TMap;
begin
  LDict := TMap.Create;
  LDict.Add('APP_VERSION', '1.0.0');
  LDict.Add('DEBUG_MODE', 'True');

  // Example 1: Using anonymous method with an object (TDictionary)
  // The anonymous method accesses properties from the associated object
  WSProcessor.AddVar('env1', LDict, True,
    function(AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
    begin
      Result := TMap(AVar.TheObject).TryGetValue(APropName.ToUpper, AValue);
    end);

  // Example 2: Fully anonymous method without an associated object
  // The anonymous method provides values directly without an object reference
  WSProcessor.AddVar('env2', nil, false,
    function (AVar: TWebStencilsDataVar; const APropName: string; var AValue: string): Boolean
    begin
      if APropName.ToUpper = 'APP_VERSION' then
        AValue := '1.0.0'
      else if APropName.ToUpper = 'DEBUG_MODE' then
        AValue := 'True'
      else
      begin
        Result := False;
        Exit;
      end;
      Result := True;
    end);

  Response.Content := WSProcessor.Content;
end;

end.

