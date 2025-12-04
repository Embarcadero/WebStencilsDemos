
//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <Web.WebReq.hpp>
#include <IdHTTPWebBrokerBridge.hpp>
#include <tchar.h>
#include <stdio.h>
#include <memory>
#include <string>

//---------------------------------------------------------------------------
USEFORM("ModuleMain.cpp", WebModuleMain); /* TWebModule: File Type */
//---------------------------------------------------------------------------
#pragma link "Web.WebReq"
#ifdef USEPACKAGES
#pragma link "IndySystem.bpi"
#pragma link "IndyCore.bpi"
#pragma link "IndyProtocols.bpi"
#else
#pragma comment(lib, "IndySystem")
#pragma comment(lib, "IndyCore")
#pragma comment(lib, "IndyProtocols")
#endif
#pragma link "IdHTTPWebBrokerBridge"

#define sStartingServer "Starting HTTP Server on port %d\n"
#define sPortInUse "- Error: Port %d already in use\n"
#define sPortSet "- Port set to %d\n"
#define sServerRunning "- The Server is already running\n"
#define sStoppingServer "- Stopping Server\n"
#define sServerStopped "- Server Stopped\n"
#define sServerNotRunning "- The Server is not running\n"
#define sInvalidCommand "- Error: Invalid Command\n"
#define sInvalidPort "- Error: Invalid Port\n"
#define sIndyVersion "- Indy Version:"
#define sActive "- Active:"
#define sPort "- Port:"
#define sSessionID "- Session ID CookieName:"
#define sCommands "Enter a Command: \n" \
  "   - \"start\" to start the server\n" \
  "   - \"stop\" to stop the server\n" \
  "   - \"set port\" to change the default port\n" \
  "   - \"status\" for Server status\n" \
  "   - \"help\" to show commands\n" \
  "   - \"exit\" to close the application\n"
#define sArrow "->"
#define sCommandStart "start"
#define sCommandStop "stop"
#define sCommandStatus "status"
#define sCommandHelp "help"
#define sCommandSetPort "set port"
#define sCommandExit "exit"

//---------------------------------------------------------------------------
void startServer(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
  if (!server->Active) {
    try {
      printf(sStartingServer, server->DefaultPort);
      server->Bindings->Clear();
      server->Active = true;

    } catch (Exception &exception) {
      printf(sPortInUse, server->DefaultPort);
    }
  }
  else {
    printf(sServerRunning);
  }
  printf(sArrow);
}

void setPort(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server, int port)
{
  if (!server->Active) {
    server->DefaultPort = port;
    printf(sPortSet, port);
  }
  else {
    printf(sServerRunning);
  }

  printf(sArrow);
}

void writeStatus(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
  printf("%s %ls\n", sIndyVersion, server->SessionList->Version.c_str());
  printf("%s %s\n", sActive, (server->Active) ? "true" : "false");
  printf("%s %d\n", sPort, server->DefaultPort);
  printf("%s %ls\n", sSessionID, server->SessionIDCookieName.c_str());
  printf(sArrow);
}

void stopServer(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
  if (server->Active) {
    printf(sStoppingServer);
    server->Active = false;
    server->Bindings->Clear();
    printf(sServerStopped);
  }
  else {
    printf(sServerNotRunning);
  }

  printf(sArrow);
}

void writeCommands()
{
  printf(sCommands);
  printf(sArrow);
}

void runServer(int port)
{
  std::wstring wsResponse;
  String sResponse;
  int iPort = 0;

  writeCommands();
  std::unique_ptr<TIdHTTPWebBrokerBridge> server(new TIdHTTPWebBrokerBridge(NULL));
  server->DefaultPort = port;
  while (true)
  {
    std::getline(std::wcin, wsResponse);

    sResponse = wsResponse.c_str();
    sResponse = sResponse.LowerCase();

    if (SameText(sResponse.SubString(1, strlen(sCommandSetPort)), sCommandSetPort)) {

      iPort = sResponse.SubString(strlen(sCommandSetPort) + 1,
          sResponse.Length() - strlen(sCommandSetPort)).Trim().ToInt();

      if (iPort > 0)
        setPort(server, iPort);
      else {
        printf(sInvalidPort);
        printf(sArrow);
      }
    }
    else if (SameText(sResponse, sCommandStart))
      startServer(server);
    else if (SameText(sResponse, sCommandStop))
      stopServer(server);
    else if (SameText(sResponse, sCommandStatus))
      writeStatus(server);
    else if (SameText(sResponse, sCommandHelp))
      writeCommands();
    else if (SameText(sResponse, sCommandExit)) {
      stopServer(server);
      break;
    }
    else {
      printf(sInvalidCommand);
      printf(sArrow);
    }
  }
}
//---------------------------------------------------------------------------
extern PACKAGE TComponentClass WebModuleClass;
//---------------------------------------------------------------------------
#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
  try
  {
    if (WebRequestHandler() != NULL)
    {
      WebRequestHandler()->WebModuleClass = WebModuleClass;
    }
    runServer(8080);
  }
  catch (Exception &exception)
  {
    printf("%ls: %ls\n", exception.ClassName().c_str(), exception.Message.c_str());
  }
  return 0;
}
//---------------------------------------------------------------------------


