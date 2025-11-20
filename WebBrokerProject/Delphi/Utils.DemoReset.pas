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
  Utils.Logger;

type
  TDemoResetThread = class(TThread)
  private
    FResetInterval: Integer;
    FStopEvent: TEvent;
    FBackupDbPath: string;
    FActiveDbPath: string;
    FConnection: TFDConnection;
  protected
    procedure Execute; override;
  public
    constructor Create(AResetInterval: Integer; ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
    destructor Destroy; override;
    procedure Stop;
  end;

  TDemoReset = class
  private
    class var FEnabled: Boolean;
    class var FResetInterval: Integer; // in seconds
    class var FResetThread: TDemoResetThread;
    class var FInitializationLock: TCriticalSection;
    class var FBackupDbPath: string;
    class var FActiveDbPath: string;
    class var FConnection: TFDConnection;
    class constructor Create;
    class destructor Destroy;
    class procedure ResetDatabaseInternal(ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
  public
    class function IsEnabled: Boolean;
    class procedure Initialize(ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
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

constructor TDemoResetThread.Create(AResetInterval: Integer; ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
begin
  inherited Create(True); // Create suspended
  FreeOnTerminate := False;
  FResetInterval := AResetInterval;
  FBackupDbPath := ABackupDbPath;
  FActiveDbPath := AActiveDbPath;
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
        Logger.Info('=== DEMO RESET: Starting automatic data reset ===');
        
        // Reset database using thread's instance variables
        TDemoReset.ResetDatabaseInternal(FBackupDbPath, FActiveDbPath, FConnection);
        
        // Cleanup old log entries (runs automatically, but trigger it here too)
        Logger.CleanupLogs;
        
        Logger.Info('=== DEMO RESET: Data reset completed successfully ===');
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

class procedure TDemoReset.Initialize(ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
var
  EnvDemoMode: string;
  EnvInterval: string;
  IniDemoMode: Boolean;
  IniInterval: Integer;
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
    IniDemoMode := TAppConfig.ReadBool('Demo', 'DemoMode', False);
    EnvDemoMode := GetEnvironmentVariable('DEMO_MODE');
    
    if IniDemoMode then
      FEnabled := True
    else
      FEnabled := SameText(EnvDemoMode, 'true') or SameText(EnvDemoMode, '1');
    
    if not FEnabled then
    begin
      Logger.Info('Demo reset mode is disabled');
      Exit;
    end;
    
    Logger.Info('Demo reset mode is ENABLED');
    
    // Get reset interval (INI file > Environment variable > Default: 900 seconds = 15 minutes)
    IniInterval := TAppConfig.ReadInteger('Demo', 'DemoResetInterval', 0);
    EnvInterval := GetEnvironmentVariable('DEMO_RESET_INTERVAL');
    
    if IniInterval > 0 then
      FResetInterval := IniInterval
    else if EnvInterval <> '' then
      FResetInterval := StrToIntDef(EnvInterval, 900)
    else
      FResetInterval := 900; // Default: 15 minutes
    
    FBackupDbPath := ABackupDbPath;
    FActiveDbPath := AActiveDbPath;
    FConnection := AConnection;
    
    Logger.Info(Format('Demo reset will occur every %d seconds (%d minutes)', 
      [FResetInterval, FResetInterval div 60]));
    
    // Create and start the reset thread
    FResetThread := TDemoResetThread.Create(FResetInterval, FBackupDbPath, FActiveDbPath, FConnection);
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

class procedure TDemoReset.ResetDatabaseInternal(ABackupDbPath, AActiveDbPath: string; AConnection: TFDConnection);
var
  WasConnected: Boolean;
begin
  WasConnected := False;
  if (ABackupDbPath = '') or (AActiveDbPath = '') or not Assigned(AConnection) then
  begin
    Logger.Warning('Cannot reset database: paths or connection not set');
    Exit;
  end;
  
  if not FileExists(ABackupDbPath) then
  begin
    Logger.Warning(Format('Cannot reset database: backup file not found at %s', [ABackupDbPath]));
    Exit;
  end;
  
  try
    // Disconnect from database if connected
    WasConnected := AConnection.Connected;
    if WasConnected then
    begin
      AConnection.Connected := False;
      Logger.Info('Database connection closed for reset');
    end;
    
    // Copy backup database over active database
    if FileExists(AActiveDbPath) then
    begin
      // On Windows, we might need to wait a bit for file handles to be released
      Sleep(100);
      TFile.Delete(AActiveDbPath);
    end;
    
    TFile.Copy(ABackupDbPath, AActiveDbPath);
    Logger.Info(Format('Database reset: copied from %s to %s', [ABackupDbPath, AActiveDbPath]));
    
    // Reconnect if it was connected before
    if WasConnected then
    begin
      AConnection.Connected := True;
      Logger.Info('Database connection restored after reset');
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error resetting database: %s', [E.Message.Replace(#13, ' ').Replace(#10, ' ')]));
      // Try to reconnect even if reset failed
      if WasConnected and not AConnection.Connected then
      begin
        try
          AConnection.Connected := True;
        except
          Logger.Error('Failed to restore database connection after reset error');
        end;
      end;
      raise;
    end;
  end;
end;



end.

