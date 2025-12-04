unit Module.Main;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, System.Generics.Collections, Web.Stencils;

type

  TStatusObject = class
  private
    function RandomStatus: string;
  public
    property Name: string read RandomStatus;
  end;

  TWebModuleMain = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    procedure WebModuleMainDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
  private
    FStatus: TStatusObject;
  public
    { Public declarations }
  end;

var
  WebModuleClass: TComponentClass = TWebModuleMain;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}

{$R *.dfm}

function TStatusObject.RandomStatus: string;
const
  Statuses: array[0..4] of string = ('a', 'i', 'p', 's', 'm');
begin
  Result := Statuses[Random(5)];
end;

procedure TWebModuleMain.WebModuleMainDefaultHandlerAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
    Response.Content := WSProcessor.Content;
end;

procedure TWebModuleMain.WebModuleCreate(Sender: TObject);
begin
  // Add a status object that returns random status values
  // The template uses @switch to render different badges based on status.Name
  FStatus := TStatusObject.Create;
  WSProcessor.AddVar('status', FStatus, False);
end;

procedure TWebModuleMain.WebModuleDestroy(Sender: TObject);
begin
  FStatus.Free;
end;

end.

