//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
#include <Web.WebReq.hpp>
#include <IdHTTPWebBrokerBridge.hpp>
#include <tchar.h>
#include <stdio.h>
#include <memory>
#include <string>
#include "ConstantsServer.h"
#include <windows.h>
#include <fcntl.h>
#include <io.h>

USEFORM("MainWebModule.cpp", MainWebModule); /* TWebModule: File Type */
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

extern PACKAGE TComponentClass WebModuleClass;

//---------------------------------------------------------------------------
void startServer(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
    if (!server->Active) {
        try {
            printf(sStartingServer, server->DefaultPort);
            server->Bindings->Clear();
            server->Active = true;
        }
        catch (Exception &exception) {
            printf(sPortInUse, server->DefaultPort);
        }
    }
    else {
        printf("%s", sServerRunning);
    }
    printf("%s", cArrow);
}

void setPort(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server, int port)
{
    if (!server->Active) {
        server->DefaultPort = port;
        printf(sPortSet, port);
    }
    else {
        printf("%s", sServerRunning);
    }
    printf("%s", cArrow);
}

void writeStatus(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
    printf("%ls%ls", sIndyVersion, server->SessionList->Version.c_str());
    printf("%ls%ls", sActive, server->Active ? "true" : "false");
    printf("%ls%d", sPort, server->DefaultPort);
    printf("%ls%ls", sSessionID, server->SessionIDCookieName.c_str());
    printf("%ls", cArrow);
}

void stopServer(std::unique_ptr<TIdHTTPWebBrokerBridge>const& server)
{
    if (server->Active) {
        printf("%s", sStoppingServer);
        server->Active = false;
        server->Bindings->Clear();
        printf("%s", sServerStopped);
    }
    else {
        printf("%s", sServerNotRunning);
    }
    printf("%s", cArrow);
}

void writeCommands()
{
    printf("%s", sCommands);
    printf("%s", cArrow);
}

void runServer(int port)
{
    String sResponse;
    int iPort = 0;
    char buffer[256];

    // Show welcome message and ASCII art
    printf("%s", sWelcomeText);

    std::unique_ptr<TIdHTTPWebBrokerBridge> server(new TIdHTTPWebBrokerBridge(NULL));
    server->DefaultPort = port;

    // Auto-start the server
    startServer(server);
    printf(sServerReady, port);

    printf("%s", sCommands);

    while (true)
    {
        if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
            // Remove newline
            buffer[strcspn(buffer, "\n")] = 0;
            // Convert to lowercase for case-insensitive comparison
            String input = String(buffer).LowerCase();
            AnsiString command = input.c_str();

            if (strncmp(command.c_str(), cCommandSetPort, strlen(cCommandSetPort)) == 0) {
                // Extract port number from the remaining part of the command
                const char* portStr = command.c_str() + strlen(cCommandSetPort);
                while (*portStr == ' ') portStr++; // Skip spaces
                iPort = atoi(portStr);

                if (iPort > 0)
                    setPort(server, iPort);
                else {
                    printf("%s", sInvalidPort);
                    printf("%s", cArrow);
                }
            }
            else if (strcmp(command.c_str(), cCommandStart) == 0)
                startServer(server);
            else if (strcmp(command.c_str(), cCommandStop) == 0)
                stopServer(server);
            else if (strcmp(command.c_str(), cCommandStatus) == 0)
                writeStatus(server);
            else if (strcmp(command.c_str(), cCommandHelp) == 0)
                printf("%s", sCommands);
            else if (strcmp(command.c_str(), cCommandExit) == 0) {
                stopServer(server);
                break;
            }
            else {
                printf("%s", sInvalidCommand);
                printf("%s", cArrow);
            }
        }
    }
}

//---------------------------------------------------------------------------
bool SetConsoleFont(const wchar_t* fontName)
{
    CONSOLE_FONT_INFOEX cfi;
    cfi.cbSize = sizeof(cfi);
    cfi.nFont = 0;
    cfi.dwFontSize.X = 0;  // Width of each character
    cfi.dwFontSize.Y = 16; // Height of each character
    cfi.FontFamily = FF_DONTCARE;
    cfi.FontWeight = FW_NORMAL;
    wcscpy_s(cfi.FaceName, LF_FACESIZE, fontName);
    return SetCurrentConsoleFontEx(GetStdHandle(STD_OUTPUT_HANDLE), FALSE, &cfi);
}

#pragma argsused
int _tmain(int argc, _TCHAR* argv[])
{
  try
  {
    #ifdef _WIN32
    // Set console mode to handle UTF-8
    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);

    // Try to set a font that supports UTF-8 characters
    if (!SetConsoleFont(L"Consolas") && !SetConsoleFont(L"Lucida Console")) {
        // If both fail, try to set any TrueType font
        CONSOLE_FONT_INFOEX cfi;
        cfi.cbSize = sizeof(cfi);
        GetCurrentConsoleFontEx(GetStdHandle(STD_OUTPUT_HANDLE), FALSE, &cfi);
        cfi.FontFamily = FF_DONTCARE | TMPF_TRUETYPE;
        SetCurrentConsoleFontEx(GetStdHandle(STD_OUTPUT_HANDLE), FALSE, &cfi);
    }

    // Get stdout handle and enable virtual terminal processing
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut != INVALID_HANDLE_VALUE) {
        DWORD dwMode = 0;
        GetConsoleMode(hOut, &dwMode);
        dwMode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING | ENABLE_PROCESSED_OUTPUT;
        SetConsoleMode(hOut, dwMode);
    }
    #endif

    if (WebRequestHandler() != NULL)
    {
      WebRequestHandler()->WebModuleClass = WebModuleClass;
    }
    runServer(8080);
  }
  catch (Exception &exception)
  {
    printf("Error: %s: %s\n", AnsiString(exception.ClassName()).c_str(), AnsiString(exception.Message).c_str());
  }
  return 0;
}
//---------------------------------------------------------------------------
