unit Constants.Server;

interface

resourcestring
  sStartingServer = '🟢 Starting HTTP Server on port %d';
  sPortInUse = '❌ Error: Port %s already in use';
  sPortSet = '🌐Port set to %s';
  sServerRunning = '✅ The Server is already running';

  sStoppingServer = '🛑 Stopping Server';
  sServerStopped = '🔴 Server Stopped';
  sServerNotRunning = '⚠ The Server is not running';
  sInvalidCommand = '❌ Error: Invalid Command';
  sIndyVersion = 'ℹ ️Indy Version: ';
  sActive = '✅ Active: ';
  sPort = '🌐 Port: ';
  sSessionID = '🍪 Session ID CookieName: ';
  sCommands = '''
      Enter a Command:

       🔸 "start" to start the server
       🔸 "stop" to stop the server
       🔸 "set port" to change the default port
       🔸 "status" for Server status
       🔸 "help" to show commands
       🔸 "exit" to close the application
      ''';
  sWelcomeText = '''
  ___       __    ______ ____________                  ___________
  __ |     / /_______  /___  ___/_  /_____________________(_)__  /_______
  __ | /| / /_  _ \_  __ \____ \_  __/  _ \_  __ \  ___/_  /__  /__  ___/
  __ |/ |/ / /  __/  /_/ /___/ // /_ /  __/  / / / /__ _  / _  / _(__  )
  ____/|__/  \___//_.___//____/ \__/ \___//_/ /_/\___/ /_/  /_/  /____/

  👋 Welcome to the WebStencils demo!

  ''';

  sServerReady = 'Ready! Access http://localhost:%d in your browser';

const
  cArrow = #10 + '➡  ';
  cCommandStart = 'start';
  cCommandStop = 'stop';
  cCommandStatus = 'status';
  cCommandHelp = 'help';
  cCommandSetPort = 'set port';
  cCommandExit = 'exit';

implementation

end.
