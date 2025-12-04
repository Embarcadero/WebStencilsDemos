unit Module.Main;

interface

uses
  System.SysUtils, System.Classes, Web.HTTPApp, Web.Stencils;

type
  TWebModuleMain = class(TWebModule)
    WSProcessor: TWebStencilsProcessor;
    procedure WebModuleMainDefaultHandlerAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleMainWebActionItem2Action(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleMainWebActionItem1Action(Sender: TObject;
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
begin
  // home.html uses MainLayout which uses BaseLayout
  // Demonstrates nested @ExtraHeader: BaseLayout -> MainLayout -> page content
  WSProcessor.InputFileName := '..\..\templates\home.html';
  Response.Content := WSProcessor.Content;
end;

procedure TWebModuleMain.WebModuleMainWebActionItem1Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  // link1.html adds its own @ExtraHeader, creating a three-level nesting
  WSProcessor.InputFileName := '..\..\templates\link1.html';
  Response.Content := WSProcessor.Content;
end;

procedure TWebModuleMain.WebModuleMainWebActionItem2Action(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  WSProcessor.InputFileName := '..\..\templates\link2.html';
  Response.Content := WSProcessor.Content;
end;

end.

