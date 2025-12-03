unit Utils.Logger;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.DateUtils,
  Utils.Config;

type
  TLogLevel = (llInfo, llError, llDebug, llWarning);
  
  TLogger = class
  private
    FLogFile: TextFile;
    FLogPath: string;
    FLogLevel: TLogLevel;
    FFileOpen: Boolean;
    FConsoleLogging: Boolean;
    FLastCleanupCheck: TDateTime;
    FCleaningUp: Boolean;
    procedure WriteLog(Level: TLogLevel; const AMessage: string);
    function GetLogLevelString(Level: TLogLevel): string;
    procedure EnsureLogDirectory;
    procedure EnsureLogFileOpen;
    procedure CheckAndCleanupLogs;
    procedure CleanupOldLogs(RetentionDays: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Info(const AMessage: string);
    procedure Error(const AMessage: string);
    procedure Debug(const AMessage: string);
    procedure Warning(const AMessage: string);
    procedure CleanupLogs(RetentionDays: Integer = 7);
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
  DEFAULT_LOG_RETENTION_DAYS = 7; // Keep last 7 days by default
  DEFAULT_MAX_LOG_SIZE_MB = 10; // 10 MB default max size
  CLEANUP_CHECK_INTERVAL_HOURS = 1; // Check every hour

constructor TLogger.Create;
var
  EnvLogPath: string;
begin
  inherited Create;
  FLogLevel := llInfo; // Default to Info level
  FFileOpen := False;
  FConsoleLogging := False;
  FLastCleanupCheck := 0;
  FCleaningUp := False;
  
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

procedure TLogger.WriteLog(Level: TLogLevel; const AMessage: string);
var
  LogMessage: string;
begin
  if Level < FLogLevel then
    Exit;
    
  LogMessage := Format('[%s] [%s] %s',
    [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
     GetLogLevelString(Level),
     AMessage]);
     
  // Output to console if enabled
  if FConsoleLogging then
    System.WriteLn(LogMessage);

  try
    // Periodically check if log cleanup is needed (skip if already cleaning up)
    if not FCleaningUp then
      CheckAndCleanupLogs;
    
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

procedure TLogger.Info(const AMessage: string);
begin
  WriteLog(llInfo, AMessage);
end;

procedure TLogger.Error(const AMessage: string);
begin
  WriteLog(llError, AMessage);
end;

procedure TLogger.Debug(const AMessage: string);
begin
  WriteLog(llDebug, AMessage);
end;

procedure TLogger.Warning(const AMessage: string);
begin
  WriteLog(llWarning, AMessage);
end;

procedure TLogger.CheckAndCleanupLogs;
var
  MaxSizeMB: Integer;
  RetentionDays: Integer;
  FileSize: Int64;
  HoursSinceLastCheck: Double;
  EnvRetention: string;
  EnvMaxSize: string;
begin
  // Only check periodically to avoid performance impact
  HoursSinceLastCheck := HoursBetween(Now, FLastCleanupCheck);
  if HoursSinceLastCheck < CLEANUP_CHECK_INTERVAL_HOURS then
    Exit;
  
  FLastCleanupCheck := Now;
  
  if not FileExists(FLogPath) then
    Exit;
  
  try
    // Get configuration (INI file > Environment variable > Default)
    RetentionDays := TAppConfig.ReadInteger('Logging', 'LogRetentionDays', 0);
    if RetentionDays <= 0 then
    begin
      EnvRetention := GetEnvironmentVariable('LOG_RETENTION_DAYS');
      if EnvRetention <> '' then
        RetentionDays := StrToIntDef(EnvRetention, DEFAULT_LOG_RETENTION_DAYS)
      else
        RetentionDays := DEFAULT_LOG_RETENTION_DAYS;
    end;
    
    MaxSizeMB := TAppConfig.ReadInteger('Logging', 'MaxLogSizeMB', 0);
    if MaxSizeMB <= 0 then
    begin
      EnvMaxSize := GetEnvironmentVariable('LOG_MAX_SIZE_MB');
      if EnvMaxSize <> '' then
        MaxSizeMB := StrToIntDef(EnvMaxSize, DEFAULT_MAX_LOG_SIZE_MB)
      else
        MaxSizeMB := DEFAULT_MAX_LOG_SIZE_MB;
    end;
    
    // Check file size
    FileSize := TFile.GetSize(FLogPath);
    if FileSize > (MaxSizeMB * 1024 * 1024) then
    begin
      Info(Format('Log file size (%d MB) exceeds limit (%d MB), cleaning up old entries...', 
        [FileSize div (1024 * 1024), MaxSizeMB]));
      CleanupOldLogs(RetentionDays);
    end;
  except
    on E: Exception do
    begin
      // Silently fail - don't break logging if cleanup fails
      System.Writeln(Format('Log cleanup check failed: %s', [E.Message]));
    end;
  end;
end;

procedure TLogger.CleanupOldLogs(RetentionDays: Integer);
var
  LogLines: TStringList;
  FilteredLines: TStringList;
  CutoffDate: TDateTime;
  Line: string;
  LogDate: TDateTime;
  DateStr: string;
  i: Integer;
  WasOpen: Boolean;
begin
  if FCleaningUp then
    Exit; // Prevent concurrent cleanup operations
  
  if not FileExists(FLogPath) then
    Exit;
  
  if RetentionDays <= 0 then
    RetentionDays := DEFAULT_LOG_RETENTION_DAYS;
  
  FCleaningUp := True;
  CutoffDate := Now - RetentionDays;
  LogLines := TStringList.Create;
  FilteredLines := TStringList.Create;
  WasOpen := FFileOpen;
  
  try
    try
      // Close file if open
      if WasOpen then
      begin
        CloseFile(FLogFile);
        FFileOpen := False;
      end;

      // Read all log lines
      LogLines.LoadFromFile(FLogPath);
      
      // Filter lines: keep only entries from the last N days
      // Log format: [yyyy-mm-dd hh:nn:ss.zzz] [LEVEL] message
      for i := 0 to LogLines.Count - 1 do
      begin
        Line := LogLines[i];
        
        // Try to extract date from log line
        // Format: [yyyy-mm-dd hh:nn:ss.zzz]
        if (Length(Line) >= 20) and (Line[1] = '[') then
        begin
          DateStr := Copy(Line, 2, 19); // Extract 'yyyy-mm-dd hh:nn:ss'
          try
            DateStr := StringReplace(DateStr, ' ', 'T', []);
            LogDate := ISO8601ToDate(DateStr);
            if LogDate >= CutoffDate then
              FilteredLines.Add(Line);
          except
            // If date parsing fails, keep the line
            FilteredLines.Add(Line);
          end;
        end
        else
        begin
          // If line doesn't match expected format, keep it
          FilteredLines.Add(Line);
        end;
      end;
      
      // Write filtered lines back to file
      FilteredLines.SaveToFile(FLogPath);
      
      Info(Format('Log cleanup completed: kept %d of %d entries (last %d days)', 
        [FilteredLines.Count, LogLines.Count, RetentionDays]));
      
      // Reopen file if it was open before
      if WasOpen then
        EnsureLogFileOpen;
        
    except
      on E: Exception do
      begin
        Error(Format('Error during log cleanup: %s', [E.Message]));
        // Try to reopen file even if cleanup failed
        if WasOpen and not FFileOpen then
        begin
          try
            EnsureLogFileOpen;
          except
            // Ignore reopen errors
          end;
        end;
        raise;
      end;
    end;
  finally
    LogLines.Free;
    FilteredLines.Free;
    FCleaningUp := False;
  end;
end;

procedure TLogger.CleanupLogs(RetentionDays: Integer = 7);
begin
  if RetentionDays <= 0 then
    RetentionDays := DEFAULT_LOG_RETENTION_DAYS;
  CleanupOldLogs(RetentionDays);
end;

initialization
  Logger := TLogger.Create;

finalization
  Logger.Free;

end. 