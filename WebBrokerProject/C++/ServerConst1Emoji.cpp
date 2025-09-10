#include <vcl.h>
#pragma hdrstop

#include "ServerConst1.h"
#pragma package(smart_init)

// Resource strings
const char* sStartingServer = u8"🟢 Starting HTTP Server on port %d\n";
const char* sPortInUse = u8"❌ Error: Port %s already in use\n";
const char* sPortSet = u8"🌐 Port set to %s\n";
const char* sServerRunning = u8"✅ The Server is already running\n";
const char* sStoppingServer = u8"🛑 Stopping Server\n";
const char* sServerStopped = u8"🔴 Server Stopped\n";
const char* sServerNotRunning = u8"⚠ The Server is not running\n";
const char* sInvalidCommand = u8"❌ Error: Invalid Command\n";
const char* sInvalidPort = u8"❌ Error: Invalid Port\n";
const char* sIndyVersion = u8"ℹ Indy Version: ";
const char* sActive = u8"✅ Active: ";
const char* sPort = u8"🌐 Port: ";
const char* sSessionID = u8"🍪 Session ID CookieName: ";

const char* sCommands = u8"\n"
    u8"Enter a Command:\n"
    u8"\n"
    u8"  🔸 \"start\" to start the server\n"
    u8"  🔸 \"stop\" to stop the server\n"
    u8"  🔸 \"set port\" to change the default port\n"
    u8"  🔸 \"status\" for Server status\n"
    u8"  🔸 \"help\" to show commands\n"
    u8"  🔸 \"exit\" to close the application\n";

const char* sWelcomeText = u8"\n"
    u8"___       __    ______ ____________                  ___________\n"
    u8"__ |     / /_______  /___  ___/_  /_____________________(_)__  /_______\n"
    u8"__ | /| / /_  _ \\_  __ \\____ \\_  __/  _ \\_  __ \\  ___/_  /__  /__  ___/\n"
    u8"__ |/ |/ / /  __/  /_/ /___/ // /_ /  __/  / / / /__ _  / _  / _(__  )\n"
    u8"____/|__/  \\___//_.___//____/ \\__/ \\___//_/ /_/\\___/ /_/  /_/  /____/\n"
    u8"\n"
    u8"C++Builder Edition\n"
    u8"\n"
    u8"👋 Welcome to the WebStencils demo!\n"
    u8"\n";

const char* sServerReady = u8"Ready! Access http://localhost:%d in your browser\n";

// Constants
const char* cArrow = u8"\n➡  ";
const char* cCommandStart = "start";
const char* cCommandStop = "stop";
const char* cCommandStatus = "status";
const char* cCommandHelp = "help";
const char* cCommandSetPort = "set port";
const char* cCommandExit = "exit";
