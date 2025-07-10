unit LoggerU;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils;

type
  TLogLevel = (llInfo, llError, llDebug);
  
  TLogger = class
  private
    FLogFile: TextFile;
    FLogPath: string;
    FLogLevel: TLogLevel;
    procedure WriteLog(Level: TLogLevel; const Message: string);
    function GetLogLevelString(Level: TLogLevel): string;
    procedure EnsureLogDirectory;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Info(const Message: string);
    procedure Error(const Message: string);
    procedure Debug(const Message: string);
    property LogPath: string read FLogPath write FLogPath;
    property LogLevel: TLogLevel read FLogLevel write FLogLevel;
  end;

var
  Logger: TLogger;

implementation

const
  DEFAULT_LOG_PATH = 'logs';
  DEFAULT_LOG_FILE = 'app.log';
  LOG_LEVEL_STRINGS: array[TLogLevel] of string = ('INFO', 'ERROR', 'DEBUG');

constructor TLogger.Create;
var
  EnvLogPath: string;
begin
  inherited Create;
  FLogLevel := llInfo; // Default to Info level
  
  // Try to get log path from environment variable
  EnvLogPath := GetEnvironmentVariable('APP_LOG_PATH');
  if EnvLogPath <> '' then
    FLogPath := EnvLogPath
  else
    FLogPath := TPath.Combine(DEFAULT_LOG_PATH, DEFAULT_LOG_FILE);
    
  EnsureLogDirectory;
  
  // Open the log file
  try
    AssignFile(FLogFile, FLogPath);
    if not FileExists(FLogPath) then
      Rewrite(FLogFile)
    else
      Append(FLogFile);
      
    // Now that the file is open, we can log
    WriteLog(llInfo, 'Logger initialized');
  except
    on E: Exception do
      Writeln(Format('Failed to initialize logger: %s', [E.Message]));
  end;
end;

destructor TLogger.Destroy;
begin
  try
    if TFile.Exists(FLogPath) then
    begin
      WriteLog(llInfo, 'Logger shutting down');
      CloseFile(FLogFile);
    end;
  except
    on E: Exception do
      Writeln(Format('Error during logger shutdown: %s', [E.Message]));
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
      // Don't log here as the file isn't open yet
      Writeln(Format('Created log directory: %s', [LogDir]));
    except
      on E: Exception do
        Writeln(Format('Failed to create log directory: %s', [E.Message]));
    end;
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
     
  try
    if TFile.Exists(FLogPath) then
    begin
      Writeln(FLogFile, LogMessage);
      Flush(FLogFile); // Ensure immediate write to disk
    end
    else
      Writeln(LogMessage); // Fallback to console if file isn't open
  except
    on E: Exception do
      Writeln(Format('Failed to write to log file: %s', [E.Message]));
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

initialization
  Logger := TLogger.Create;

finalization
  Logger.Free;

end. 