unit Utils.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils;

type
  TLogLevel = (llInfo, llError, llDebug, llWarning);
  
  TLogger = class
  private
    FLogFile: TextFile;
    FLogPath: string;
    FLogLevel: TLogLevel;
    FFileOpen: Boolean;
    FConsoleLogging: Boolean;
    procedure WriteLog(Level: TLogLevel; const Message: string);
    function GetLogLevelString(Level: TLogLevel): string;
    procedure EnsureLogDirectory;
    procedure EnsureLogFileOpen;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Info(const Message: string);
    procedure Error(const Message: string);
    procedure Debug(const Message: string);
    procedure Warning(const Message: string);
    property LogPath: string read FLogPath write FLogPath;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
    property ConsoleLogging: Boolean read FConsoleLogging write FConsoleLogging;
  end;

var
  Logger: TLogger;

implementation

const
  DEFAULT_LOG_PATH = 'logs';
  DEFAULT_LOG_FILE = 'app.log';
  LOG_LEVEL_STRINGS: array[TLogLevel] of string = ('INFO', 'ERROR', 'DEBUG', 'WARN');

constructor TLogger.Create;
var
  EnvLogPath: string;
begin
  inherited Create;
  FLogLevel := llInfo; // Default to Info level
  FFileOpen := False;
  FConsoleLogging := False;
  
  // Try to get log path from environment variable
  EnvLogPath := GetEnvironmentVariable('APP_LOG_PATH');
  if EnvLogPath <> '' then
    FLogPath := EnvLogPath
  else
    FLogPath := TPath.Combine(DEFAULT_LOG_PATH, DEFAULT_LOG_FILE);
    
  EnsureLogDirectory;
  
  try
    EnsureLogFileOpen;
    WriteLog(llInfo, 'Logger initialized');
  except
    on E: Exception do
      System.Writeln(Format('Failed to initialize logger: %s', [E.Message]));
  end;
end;

destructor TLogger.Destroy;
begin
  try
    if FFileOpen then
    begin
      WriteLog(llInfo, 'Logger shutting down');
      CloseFile(FLogFile);
      FFileOpen := False;
    end;
  except
    on E: Exception do
      System.Writeln(Format('Error during logger shutdown: %s', [E.Message]));
  end;
  inherited;
end;

procedure TLogger.EnsureLogDirectory;
var
  LogDir: string;
begin
  LogDir := ExtractFilePath(FLogPath);
  if (LogDir <> '') and not DirectoryExists(LogDir) then
  begin
    try
      TDirectory.CreateDirectory(LogDir);
      System.Writeln(Format('Created log directory: %s', [LogDir]));
    except
      on E: Exception do
        System.Writeln(Format('Failed to create log directory: %s', [E.Message]));
    end;
  end;
end;

procedure TLogger.EnsureLogFileOpen;
begin
  if not FFileOpen then
  begin
    AssignFile(FLogFile, FLogPath);
    if not FileExists(FLogPath) then
      Rewrite(FLogFile)
    else
      Append(FLogFile);
    FFileOpen := True;
  end;
end;

function TLogger.GetLogLevelString(Level: TLogLevel): string;
begin
  Result := LOG_LEVEL_STRINGS[Level];
end;

procedure TLogger.WriteLog(Level: TLogLevel; const Message: string);
var
  LogMessage: string;
begin
  if Level < FLogLevel then
    Exit;
    
  LogMessage := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
     GetLogLevelString(Level),
     Message]);
     
  // Output to console if enabled
  if FConsoleLogging then
    System.WriteLn(LogMessage);

  try
    // Ensure log file is open
    EnsureLogFileOpen;
    
    if FFileOpen then
    begin
      System.Writeln(FLogFile, LogMessage);
      Flush(FLogFile); // Ensure immediate write to disk
    end;
  except
    on E: Exception do
    begin
      System.Writeln(Format('Failed to write to log file: %s', [E.Message]));
      // Try to reopen the file on next write
      FFileOpen := False;
    end;
  end;
end;

procedure TLogger.Info(const Message: string);
begin
  WriteLog(llInfo, Message);
end;

procedure TLogger.Error(const Message: string);
begin
  WriteLog(llError, Message);
end;

procedure TLogger.Debug(const Message: string);
begin
  WriteLog(llDebug, Message);
end;

procedure TLogger.Warning(const Message: string);
begin
  WriteLog(llWarning, Message);
end;

initialization
  Logger := TLogger.Create;

finalization
  Logger.Free;

end. 