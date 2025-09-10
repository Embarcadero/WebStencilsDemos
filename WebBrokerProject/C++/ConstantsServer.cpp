#include <vcl.h>
#pragma hdrstop

#include "ConstantsServer.h"
#pragma package(smart_init)

// Resource strings (ASCII only for maximum compatibility)
const char* sStartingServer = "Starting HTTP Server on port %d\n";
const char* sPortInUse = "Error: Port %s already in use\n";
const char* sPortSet = "Port set to %s\n";
const char* sServerRunning = "The Server is already running\n";
const char* sStoppingServer = "Stopping Server\n";
const char* sServerStopped = "Server Stopped\n";
const char* sServerNotRunning = "The Server is not running\n";
const char* sInvalidCommand = "Error: Invalid Command\n";
const char* sInvalidPort = "Error: Invalid Port\n";
const char* sIndyVersion = "Indy Version: ";
const char* sActive = "Active: ";
const char* sPort = "Port: ";
const char* sSessionID = "Session ID CookieName: ";

const char* sCommands = "\n"
    "Enter a Command:\n"
    "\n"
    "  - \"start\" to start the server\n"
    "  - \"stop\" to stop the server\n"
    "  - \"set port\" to change the default port\n"
    "  - \"status\" for Server status\n"
    "  - \"help\" to show commands\n"
    "  - \"exit\" to close the application\n";

const char* sWelcomeText = "\n"
    "___       __    ______ ____________                  ___________\n"
    "__ |     / /_______  /___  ___/_  /_____________________(_)__  /_______\n"
    "__ | /| / /_  _ \\_  __ \\____ \\_  __/  _ \\_  __ \\  ___/_  /__  /__  ___/\n"
    "__ |/ |/ / /  __/  /_/ /___/ // /_ /  __/  / / / /__ _  / _  / _(__  )\n"
    "____/|__/  \\___//_.___//____/ \\__/ \\___//_/ /_/\\___/ /_/  /_/  /____/\n"
    "\n"
    "C++Builder Edition\n"
    "\n"
    "Welcome to the WebStencils demo!\n"
    "\n";

const char* sServerReady = "Ready! Access http://localhost:%d in your browser\n";

// Constants
const char* cArrow = "\n> ";
// Commands are ASCII; keep as const char* for strcmp()
const char* cCommandStart = "start";
const char* cCommandStop = "stop";
const char* cCommandStatus = "status";
const char* cCommandHelp = "help";
const char* cCommandSetPort = "set port";
const char* cCommandExit = "exit";
