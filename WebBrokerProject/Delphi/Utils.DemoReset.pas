{
  Demo Reset Utility
  
  This unit provides auto-reset functionality for public demo deployments.
  It is only active when the DEMO_MODE environment variable is set to 'true'.
  
  For GitHub users: This code can be safely ignored if you're not deploying
  a public demo. Simply don't set the DEMO_MODE environment variable.
  
  Usage:
    - Set environment variable: DEMO_MODE=true
    - Optional: DEMO_RESET_INTERVAL=900 (seconds, default: 900 = 15 minutes)
    - Call TDemoReset.Initialize in your web module OnCreate
}

unit Utils.DemoReset;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.SyncObjs,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Phys.SQLite,
  Utils.Logger;

type
  TDemoResetThread = class(TThread)
  private
    FResetInterval: Integer;
    FStopEvent: TEvent;
    FBackupDatabaseFile: string;
    FActiveDatabaseFile: string;
    FConnection: TFDConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(AResetInterval: Integer; ABackupDatabaseFile, AActiveDatabaseFile: string; AConnection: TFDConnection);
    destructor Destroy; override;
    procedure Stop;
  end;

  TDemoReset = class
  private
    class var FEnabled: Boolean;
    class var FResetInterval: Integer; // in seconds
    class var FResetThread: TDemoResetThread;
    class var FInitializationLock: TCriticalSection;
    class var FBackupDatabaseFile: string;
    class var FActiveDatabaseFile: string;
    class var FConnection: TFDConnection;
    class constructor Create;
    class destructor Destroy;
    class function DetermineBackupFile(const AActiveDatabaseFile: string): string;
    class procedure EnsureBackupExists(const ABackupDatabaseFile, AActiveDatabaseFile: string);
    class procedure ResetDatabaseInternal(ABackupDatabaseFile, AActiveDatabaseFile: string; AConnection: TFDConnection);
  public
    class function IsEnabled: Boolean;
    class procedure Initialize(AActiveDatabaseFile: string; AConnection: TFDConnection);
    class procedure Finalize;
  end;

implementation

uses
  Utils.Config;

{ TDemoReset }

class constructor TDemoReset.Create;
begin
  FInitializationLock := TCriticalSection.Create;
end;

class destructor TDemoReset.Destroy;
begin
  Finalize;
  FInitializationLock.Free;
end;

{ TDemoResetThread }

constructor TDemoResetThread.Create(AResetInterval: Integer; ABackupDatabaseFile, AActiveDatabaseFile: string; AConnection: TFDConnection);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FResetInterval := AResetInterval;
  FBackupDatabaseFile := ABackupDatabaseFile;
  FActiveDatabaseFile := AActiveDatabaseFile;
  FConnection := AConnection;
  FStopEvent := TEvent.Create(nil, True, False, '');
end;

destructor TDemoResetThread.Destroy;
begin
  FStopEvent.Free;
  inherited;
end;

procedure TDemoResetThread.Stop;
begin
  if Assigned(FStopEvent) then
    FStopEvent.SetEvent;
end;

procedure TDemoResetThread.Execute;
var
  ElapsedSeconds: Integer;
  WaitResult: TWaitResult;
begin
  Logger.Info('Demo reset thread started, waiting for first reset interval...');
  
  ElapsedSeconds := 0;
  
  while not Terminated do
  begin
    // Wait for stop event or timeout (1 second) - cross-platform compatible
    WaitResult := FStopEvent.WaitFor(1000);
    
    if WaitResult = wrSignaled then
    begin
      Logger.Info('Demo reset thread received stop signal');
      Break;
    end;
    
    if Terminated then
      Break;
    
    ElapsedSeconds := ElapsedSeconds + 1;
    
    // Check if it's time to reset
    if ElapsedSeconds >= FResetInterval then
    begin
      if not TDemoReset.IsEnabled then
      begin
        Logger.Info('Demo reset skipped: demo mode is disabled');
        ElapsedSeconds := 0;
        Continue;
      end;
      
      try
        Logger.Info('=== DEMO RESET: Starting automatic database reset ===');
        Logger.Info('Note: Current database data will be lost and replaced with backup data');
        
        // Reset database: Restore from backup, overwriting current database
        TDemoReset.ResetDatabaseInternal(FBackupDatabaseFile, FActiveDatabaseFile, FConnection);
        
        // Cleanup old log entries (runs automatically, but trigger it here too)
        Logger.CleanupLogs;
        
        Logger.Info('=== DEMO RESET: Database reset completed successfully ===');
      except
        on E: Exception do
          Logger.Error(Format('Error during demo reset: %s', [E.Message.Replace(#13, ' ').Replace(#10, ' ')]));
      end;
      
      // Reset counter for next interval
      ElapsedSeconds := 0;
    end;
  end;
  
  Logger.Info('Demo reset thread exiting');
end;

{ TDemoReset }

class function TDemoReset.IsEnabled: Boolean;
begin
  Result := FEnabled;
end;

class function TDemoReset.DetermineBackupFile(const AActiveDatabaseFile: string): string;
var
  DefaultBackupFile: string;
  ActiveDbDir: string;
  NormalizedDir: string;
begin
  // Determine default backup file path based on active database file location
  ActiveDbDir := ExtractFilePath(AActiveDatabaseFile);
  NormalizedDir := TPath.GetFullPath(ActiveDbDir);
  
  // Normalize path separators for comparison
  NormalizedDir := StringReplace(NormalizedDir, '\', '/', [rfReplaceAll]);
  
  // If database is in /app/data or /app/resources/data, use /app/backup (Docker convention)
  if (Pos('/app/data', NormalizedDir) > 0) or (Pos('/app/resources/data', NormalizedDir) > 0) or 
     (Pos('\app\data', ActiveDbDir) > 0) or (Pos('\app\resources\data', ActiveDbDir) > 0) then
    DefaultBackupFile := TPath.Combine('/app/backup', ExtractFileName(AActiveDatabaseFile))
  else
    // Otherwise, use same directory with backup prefix
    DefaultBackupFile := TPath.Combine(ActiveDbDir, 'backup.sqlite3');
  
  // Priority: INI file > Environment variable > Default
  Result := TAppConfig.ReadString('Paths', 'BackupDatabaseFile', DefaultBackupFile, 'APP_BACKUP_DB_FILE');
end;

class procedure TDemoReset.EnsureBackupExists(const ABackupDatabaseFile, AActiveDatabaseFile: string);
var
  BackupDir: string;
begin
  // Check if backup already exists
  if FileExists(ABackupDatabaseFile) then
  begin
    Logger.Info(Format('Backup database already exists at: %s', [ABackupDatabaseFile]));
    Exit;
  end;
  
  // Check if active database exists (source for backup)
  if not FileExists(AActiveDatabaseFile) then
  begin
    Logger.Warning(Format('Cannot create backup: active database not found at %s', [AActiveDatabaseFile]));
    Exit;
  end;
  
  // Ensure backup directory exists
  BackupDir := ExtractFilePath(ABackupDatabaseFile);
  if (BackupDir <> '') and not DirectoryExists(BackupDir) then
  begin
    Logger.Info(Format('Creating backup directory: %s', [BackupDir]));
    ForceDirectories(BackupDir);
  end;
  
  // Create backup by copying active database
  Logger.Info(Format('Creating backup database from active database: %s -> %s', [AActiveDatabaseFile, ABackupDatabaseFile]));
  try
    TFile.Copy(AActiveDatabaseFile, ABackupDatabaseFile);
    Logger.Info(Format('Backup database created successfully at: %s', [ABackupDatabaseFile]));
  except
    on E: Exception do
    begin
      Logger.Error(Format('Failed to create backup database: %s', [E.Message]));
      raise;
    end;
  end;
end;

class procedure TDemoReset.Initialize(AActiveDatabaseFile: string; AConnection: TFDConnection);
begin
  // Thread-safe guard: prevent multiple initializations
  FInitializationLock.Acquire;
  try
    if Assigned(FResetThread) then
    begin
      Logger.Warning('Demo reset thread already initialized, skipping duplicate initialization');
      Exit;
    end;
    
    // Check if demo mode is enabled (INI file > Environment variable > Default: false)
    FEnabled := TAppConfig.ReadBool('Demo', 'DemoMode', False, 'DEMO_MODE');
    
    if not FEnabled then
    begin
      Logger.Info('Demo reset mode is disabled');
      Exit;
    end;
    
    Logger.Info('Demo reset mode is ENABLED');
    
    // Determine backup file path automatically
    FActiveDatabaseFile := AActiveDatabaseFile;
    FBackupDatabaseFile := DetermineBackupFile(AActiveDatabaseFile);
    FConnection := AConnection;
    
    Logger.Info(Format('Active database file: %s', [FActiveDatabaseFile]));
    Logger.Info(Format('Backup database file: %s', [FBackupDatabaseFile]));
    
    // Auto-create backup if it doesn't exist
    EnsureBackupExists(FBackupDatabaseFile, FActiveDatabaseFile);
    
    // Get reset interval (INI file > Environment variable > Default: 900 seconds = 15 minutes)
    FResetInterval := TAppConfig.ReadInteger('Demo', 'DemoResetInterval', 900, 'DEMO_RESET_INTERVAL');
    
    Logger.Info(Format('Demo reset will occur every %d seconds (%d minutes)', 
      [FResetInterval, FResetInterval div 60]));
    
    // Create and start the reset thread
    FResetThread := TDemoResetThread.Create(FResetInterval, FBackupDatabaseFile, FActiveDatabaseFile, FConnection);
    FResetThread.Start;
    
    Logger.Info('Demo reset thread started');
  finally
    FInitializationLock.Release;
  end;
end;

class procedure TDemoReset.Finalize;
begin
  // Signal thread to stop and wait for it
  if Assigned(FResetThread) then
  begin
    try
      FResetThread.Stop;
      FResetThread.WaitFor;
      FResetThread.Free;
      FResetThread := nil;
      Logger.Info('Demo reset thread stopped');
    except
      on E: Exception do
        Logger.Error(Format('Error stopping demo reset thread: %s', [E.Message]));
    end;
  end;
end;

class procedure TDemoReset.ResetDatabaseInternal(ABackupDatabaseFile, AActiveDatabaseFile: string; AConnection: TFDConnection);
var
  SQLiteBackup: TFDSQLiteBackup;
  SQLiteDriverLink: TFDPhysSQLiteDriverLink;
  WasConnected: Boolean;
begin
  if (ABackupDatabaseFile = '') or (AActiveDatabaseFile = '') or not Assigned(AConnection) then
  begin
    Logger.Warning('Cannot reset database: file paths or connection not set');
    Exit;
  end;

  if not FileExists(ABackupDatabaseFile) then
  begin
    Logger.Warning(Format('Cannot reset database: backup file not found at %s', [ABackupDatabaseFile]));
    Exit;
  end;

  WasConnected := AConnection.Connected;

  try
    Logger.Info('=== Starting database reset: Restoring from backup ===');

    // Close connection to release file handles
    if WasConnected then
    begin
      AConnection.Connected := False;
      Logger.Info('Main database connection closed');
      Sleep(300); // Let SQLite release file handles
    end;

    // Restore using SQLite's native backup API (handles overwrites and locks)
    Logger.Info(Format('Restoring database from backup: %s -> %s', [ABackupDatabaseFile, AActiveDatabaseFile]));

    SQLiteBackup := TFDSQLiteBackup.Create(nil);
    SQLiteDriverLink := TFDPhysSQLiteDriverLink.Create(nil);
    try
      SQLiteBackup.DriverLink := SQLiteDriverLink;
      SQLiteBackup.Database := ABackupDatabaseFile;
      SQLiteBackup.DestDatabase := AActiveDatabaseFile;
      SQLiteBackup.WaitForLocks := True;
      SQLiteBackup.BusyTimeout := 5000;

      SQLiteBackup.Backup;
      Logger.Info('Database restore completed successfully');
    finally
      SQLiteBackup.Free;
      SQLiteDriverLink.Free; // Fix memory leak
    end;

    // Reconnect to restored database
    if WasConnected then
    begin
      Sleep(200);
      AConnection.Connected := True;
      Logger.Info('Database connection restored');
      Logger.Info('=== Database reset completed successfully ===');
    end;

  except
    on E: Exception do
    begin
      Logger.Error(Format('Error resetting database: %s', [E.Message.Replace(#13, ' ').Replace(#10, ' ')]));

      // Try to reconnect even if reset failed
      if WasConnected and not AConnection.Connected then
      begin
        try
          Sleep(500);
          AConnection.Connected := True;
          Logger.Info('Database connection restored after error');
        except
          on E2: Exception do
            Logger.Error(Format('Failed to restore connection: %s', [E2.Message]));
        end;
      end;
      raise;
    end;
  end;
end;

end.

