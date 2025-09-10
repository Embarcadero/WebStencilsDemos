program WebStencilsDemo;
{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  System.Types,
  IPPeerServer,
  IPPeerAPI,
  IdHTTPWebBrokerBridge,
  Web.WebReq,
  Web.WebBroker,
  {$IFDEF MSWINDOWS}
  Winapi.Windows,
  {$ENDIF }
  {$IFDEF LINUX}
  Posix.Signal,
  {$ENDIF }
  Modules.Main in 'Modules.Main.pas' {MainWebModule: TWebModule},
  Constants.Server in 'Constants.Server.pas',
  Helpers.WebModule in 'Helpers.WebModule.pas',
  Models.Tasks in 'Models.Tasks.pas',
  Controllers.Tasks in 'Controllers.Tasks.pas',
  Controllers.Customers in 'Controllers.Customers.pas',
  Helpers.FDQuery in 'Helpers.FDQuery.pas',
  Utils.PaginationParams in 'Utils.PaginationParams.pas',
  Services.CodeExamples in 'Services.CodeExamples.pas',
  Utils.Logger in 'Utils.Logger.pas',
  Helpers.Messages in 'Helpers.Messages.pas',
  Controllers.Base in 'Controllers.Base.pas',
  Utils.Search in 'Utils.Search.pas',
  Utils.FormSession in 'Utils.FormSession.pas';

{$R *.res}

var
  GServer: TIdHTTPWebBrokerBridge;  // Global server reference for signal handler
  GShouldExit: Boolean = False;     // Flag to control the main loop

{$IFDEF LINUX}
// Linux signal handler
procedure SignalHandler(SigNum: Integer); cdecl;
begin
  if (SigNum = SIGTERM) and (GServer <> nil) then
  begin
    Logger.Info('Received SIGTERM signal, stopping server...');
    if GServer.Active then
    begin
      GServer.Active := False;
      GServer.Bindings.Clear;
      Logger.Info('Server stopped gracefully');
    end;
    GShouldExit := True;  // Set the exit flag
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
// Windows console handler
function ConsoleHandler(CtrlType: DWORD): BOOL; stdcall;
begin
  Result := True;
  case CtrlType of
    CTRL_C_EVENT,
    CTRL_BREAK_EVENT,
    CTRL_CLOSE_EVENT,
    CTRL_SHUTDOWN_EVENT:
    begin
      if GServer <> nil then
      begin
        Logger.Info('Received shutdown signal, stopping server...');
        if GServer.Active then
        begin
          GServer.Active := False;
          GServer.Bindings.Clear;
          Logger.Info('Server stopped gracefully');
        end;
      end;
      GShouldExit := True;  // Set the exit flag
      Result := False; // Allow the system to continue processing the signal
    end;
  end;
end;
{$ENDIF}

function BindPort(APort: Integer): Boolean;
var
  LTestServer: IIPTestServer;
begin
  Result := True;
  try
    LTestServer := PeerFactory.CreatePeer('', IIPTestServer) as IIPTestServer;
    LTestServer.TestOpenPort(APort, nil);
  except
    Result := False;
  end;
end;

function CheckPort(APort: Integer): Integer;
begin
	if BindPort(APort) then
    Result := APort
  else
    Result := 0;
end;

procedure SetPort(const AServer: TIdHTTPWebBrokerBridge; APort: String);
begin
  if not AServer.Active then
  begin
    APort := APort.Replace(cCommandSetPort, '').Trim;
    if CheckPort(APort.ToInteger) > 0 then
    begin
      AServer.DefaultPort := APort.ToInteger;
      Writeln(Format(sPortSet, [APort]));
    end
    else
      Writeln(Format(sPortInUse, [APort]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StartServer(const AServer: TIdHTTPWebBrokerBridge);
begin
  if not AServer.Active then
  begin
    if CheckPort(AServer.DefaultPort) > 0 then
    begin
      Writeln(Format(sStartingServer, [AServer.DefaultPort]));
      AServer.Bindings.Clear;
      AServer.Active := True;
    end
    else
      Writeln(Format(sPortInUse, [AServer.DefaultPort.ToString]));
  end
  else
    Writeln(sServerRunning);
  Write(cArrow);
end;

procedure StopServer(const AServer: TIdHTTPWebBrokerBridge);
begin
  if AServer.Active then
  begin
    Writeln(sStoppingServer);
    AServer.Active := False;
    AServer.Bindings.Clear;
    Writeln(sServerStopped);
  end
  else
    Writeln(sServerNotRunning);
  Write(cArrow);
end;

procedure WriteCommands;
begin
  Writeln(sCommands);
  Write(cArrow);
end;

procedure WriteStatus(const AServer: TIdHTTPWebBrokerBridge);
begin
  Writeln(sIndyVersion + AServer.SessionList.Version);
  Writeln(sActive + AServer.Active.ToString(TUseBoolStrs.True));
  Writeln(sPort + AServer.DefaultPort.ToString);
  Writeln(sSessionID + AServer.SessionIDCookieName);
  Write(cArrow);
end;

procedure RunServer(APort: Integer);
var
  LResponse: string;
  {$IFDEF LINUX}
  OldAct: sigaction_t;
  NewAct: sigaction_t;
  {$ENDIF}
begin
  Logger.Info('Starting server...');
  Writeln(sWelcomeText);
  {$IFDEF CONTAINER}
  Logger.Info('Running in container mode');
  Logger.ConsoleLogging := True;
  {$ELSE}
  Logger.Info('Running in interactive mode');
  WriteCommands;
  {$ENDIF}
  
  GServer := TIdHTTPWebBrokerBridge.Create(nil);
  try
    {$IFDEF LINUX}
    // Set up Linux signal handler
    FillChar(NewAct, SizeOf(NewAct), 0);
    NewAct._u.sa_handler := SignalHandler;
    NewAct.sa_flags := 0;
    sigemptyset(NewAct.sa_mask);
    if sigaction(SIGTERM, @NewAct, @OldAct) = 0 then
      Logger.Info('Signal handler installed successfully')
    else
      Logger.Error('Failed to install signal handler');
    {$ENDIF}

    {$IFDEF MSWINDOWS}
    // Set up Windows console handler
    if SetConsoleCtrlHandler(@ConsoleHandler, True) then
      Logger.Info('Console handler installed successfully')
    else
      Logger.Error('Failed to install console handler');
    {$ENDIF}

    GServer.DefaultPort := APort;
    StartServer(GServer);
    Logger.Info(Format('Server started on port %d', [APort]));
    
    while not GShouldExit do
    begin
      if not GServer.Active then
      begin
        Logger.Info('Server is no longer active, exiting...');
        Break;
      end;
      
      {$IFNDEF CONTAINER}
      if Eof then
      begin
        Logger.Info('End of input stream detected, exiting...');
        Break;
      end;
        
      Readln(LResponse);
      LResponse := Trim(LowerCase(LResponse));
      
      // Skip empty input
      if LResponse = '' then
        Continue;
        
      if LResponse.StartsWith(cCommandSetPort) then
        SetPort(GServer, LResponse)
      else if sametext(LResponse, cCommandStart) then
        StartServer(GServer)
      else if sametext(LResponse, cCommandStatus) then
        WriteStatus(GServer)
      else if sametext(LResponse, cCommandStop) then
        StopServer(GServer)
      else if sametext(LResponse, cCommandHelp) then
        WriteCommands
      else if sametext(LResponse, cCommandExit) then
        if GServer.Active then
        begin
          StopServer(GServer);
          break
        end
        else
          break
      else
      begin
        Logger.Info(Format('Received invalid command: %s', [LResponse]));
        Writeln(sInvalidCommand);
        Write(cArrow);
      end;
      {$ELSE}
      // In container mode, just keep the server running
      Sleep(1000); // Sleep for 1 second to prevent CPU spinning
      {$ENDIF}
    end;
  finally
    if GServer.Active then
    begin
      Logger.Info('Cleaning up server...');
      GServer.Active := False;
      GServer.Bindings.Clear;
    end;
    GServer.Free;
    GServer := nil;
    Logger.Info('Server cleanup completed');
  end;
end;

begin
  {$IFDEF MSWINDOWS}
  SetConsoleOutputCP(CP_UTF8);
  {$ENDIF}
  try
    Logger.Info('Application starting...');
    if WebRequestHandler <> nil then
      WebRequestHandler.WebModuleClass := WebModuleClass;
    RunServer(8080);
  except
    on E: Exception do
    begin
      Logger.Error(Format('Unhandled exception: %s', [E.Message]));
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;
  Logger.Info('Application shutting down...');
end.
