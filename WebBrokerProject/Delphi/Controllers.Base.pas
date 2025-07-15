unit Controllers.Base;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Web.HTTPApp,
  Web.Stencils,
  
  Helpers.Messages;

type
  TBaseController = class
  protected
    FWebStencilsEngine: TWebStencilsEngine;
    FWebStencilsProcessor: TWebStencilsProcessor;

    function RenderTemplate(const ATemplatePath: string; ARequest: TWebRequest = nil): string;
    function GetCurrentSession(ARequest: TWebRequest): TWebSession;
    procedure AddSuccessMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddWarningMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddErrorMessage(ARequest: TWebRequest; const AMessage: string);
    procedure AddInfoMessage(ARequest: TWebRequest; const AMessage: string);
    procedure RedirectWithMessage(AResponse: TWebResponse; const ALocation: string);
  public
    constructor Create(AWebStencilsEngine: TWebStencilsEngine);
    destructor Destroy; override;
  end;

implementation

{ TBaseController }

constructor TBaseController.Create(AWebStencilsEngine: TWebStencilsEngine);
begin
  inherited Create;
  FWebStencilsEngine := AWebStencilsEngine;
  FWebStencilsProcessor := TWebStencilsProcessor.Create(nil);
  FWebStencilsProcessor.Engine := FWebStencilsEngine;
end;

destructor TBaseController.Destroy;
begin
  FWebStencilsProcessor.Free;
  inherited;
end;

function TBaseController.RenderTemplate(const ATemplatePath: string; ARequest: TWebRequest = nil): string;
begin
  FWebStencilsProcessor.InputFileName := TPath.Combine(FWebStencilsEngine.RootDirectory, ATemplatePath);
  if Assigned(ARequest) then
  begin
    FWebStencilsProcessor.WebRequest := ARequest;
  end;
  Result := FWebStencilsProcessor.Content;
end;

function TBaseController.GetCurrentSession(ARequest: TWebRequest): TWebSession;
begin
  Result := nil;
  if Assigned(ARequest) then
    Result := ARequest.Session;
end;

procedure TBaseController.AddSuccessMessage(ARequest: TWebRequest; const AMessage: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtSuccess, AMessage);
end;

procedure TBaseController.AddWarningMessage(ARequest: TWebRequest; const AMessage: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtWarning, AMessage);
end;

procedure TBaseController.AddErrorMessage(ARequest: TWebRequest; const AMessage: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtError, AMessage);
end;

procedure TBaseController.AddInfoMessage(ARequest: TWebRequest; const AMessage: string);
var
  LSession: TWebSession;
begin
  LSession := GetCurrentSession(ARequest);
  if Assigned(LSession) then
    TMessageManager.AddMessage(LSession, mtInfo, AMessage);
end;

procedure TBaseController.RedirectWithMessage(AResponse: TWebResponse; const ALocation: string);
begin
  AResponse.StatusCode := 302;
  AResponse.Location := ALocation;
  AResponse.Content := '';
end;

end. 