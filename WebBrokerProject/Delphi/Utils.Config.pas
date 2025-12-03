{
  Configuration Utility
  
  Provides optional INI file support for application configuration.
  Configuration priority: INI file > Environment variables > Defaults
  
  The INI file is optional - if it doesn't exist, the application will
  fall back to environment variables and defaults as before.
  
  INI file location: Same directory as the executable, named "config.ini"
  
  Example config.ini:
    [Paths]
    ResourcesPath=C:\MyApp\resources
    DatabaseFile=C:\MyApp\data\database.sqlite3
    BackupDatabaseFile=C:\MyApp\backup\database.sqlite3
    
    [Demo]
    DemoMode=true
    DemoResetInterval=900
}

unit Utils.Config;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.IniFiles;

type
  TAppConfig = class
  private
    class var FIniFile: TIniFile;
    class var FConfigPath: string;
    class var FConfigExists: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
    // Check INI file > Environment variable > Default
    // EnvVarName: If empty string, skips environment variable check (INI > Default only)
    class function ReadString(const Section, Key, Default: string; const EnvVarName: string = ''): string;
    class function ReadInteger(const Section, Key: string; Default: Integer; const EnvVarName: string = ''): Integer;
    class function ReadBool(const Section, Key: string; Default: Boolean; const EnvVarName: string = ''): Boolean;
    class function ConfigFileExists: Boolean;
    class property ConfigPath: string read FConfigPath;
  end;

implementation

{ TAppConfig }

class constructor TAppConfig.Create;
var
  BinaryPath: string;
begin
  FConfigExists := False;
  BinaryPath := TPath.GetDirectoryName(ParamStr(0));
  FConfigPath := TPath.Combine(BinaryPath, 'config.ini');
  
  if FileExists(FConfigPath) then
  begin
    try
      FIniFile := TIniFile.Create(FConfigPath);
      FConfigExists := True;
    except
      on E: Exception do
      begin
        FConfigExists := False;
      end;
    end;
  end;
end;

class destructor TAppConfig.Destroy;
begin
  if Assigned(FIniFile) then
    FIniFile.Free;
end;

class function TAppConfig.ReadString(const Section, Key, Default: string; const EnvVarName: string = ''): string;
var
  IniValue: string;
  EnvValue: string;
begin
  // Priority 1: INI file
  if FConfigExists and Assigned(FIniFile) then
  begin
    IniValue := FIniFile.ReadString(Section, Key, '');
    if IniValue <> '' then
    begin
      Result := IniValue;
      Exit;
    end;
  end;
  
  // Priority 2: Environment variable (if EnvVarName provided)
  if EnvVarName <> '' then
  begin
    EnvValue := GetEnvironmentVariable(EnvVarName);
    if EnvValue <> '' then
    begin
      Result := EnvValue;
      Exit;
    end;
  end;
  
  // Priority 3: Default
  Result := Default;
end;

class function TAppConfig.ReadInteger(const Section, Key: string; Default: Integer; const EnvVarName: string = ''): Integer;
var
  IniValue: Integer;
  EnvValue: string;
  EnvIntValue: Integer;
begin
  // Priority 1: INI file
  if FConfigExists and Assigned(FIniFile) then
  begin
    if FIniFile.ValueExists(Section, Key) then
    begin
      IniValue := FIniFile.ReadInteger(Section, Key, Default);
      Result := IniValue;
      Exit;
    end;
  end;
  
  // Priority 2: Environment variable (if EnvVarName provided)
  if EnvVarName <> '' then
  begin
    EnvValue := GetEnvironmentVariable(EnvVarName);
    if EnvValue <> '' then
    begin
      EnvIntValue := StrToIntDef(EnvValue, Default);
      Result := EnvIntValue;
      Exit;
    end;
  end;
  
  // Priority 3: Default
  Result := Default;
end;

class function TAppConfig.ReadBool(const Section, Key: string; Default: Boolean; const EnvVarName: string = ''): Boolean;
var
  IniValueStr: string;
  EnvValue: string;
begin
  // Priority 1: INI file
  if FConfigExists and Assigned(FIniFile) then
  begin
    if FIniFile.ValueExists(Section, Key) then
    begin
      // Use existing boolean parsing logic
      IniValueStr := FIniFile.ReadString(Section, Key, '');
      IniValueStr := Trim(LowerCase(IniValueStr));
      
      if (IniValueStr = 'true') or (IniValueStr = '1') or (IniValueStr = 'yes') or (IniValueStr = 'on') then
        Result := True
      else if (IniValueStr = 'false') or (IniValueStr = '0') or (IniValueStr = 'no') or (IniValueStr = 'off') then
        Result := False
      else
        Result := FIniFile.ReadBool(Section, Key, Default);
      Exit;
    end;
  end;
  
  // Priority 2: Environment variable (if EnvVarName provided)
  if EnvVarName <> '' then
  begin
    EnvValue := GetEnvironmentVariable(EnvVarName);
    if EnvValue <> '' then
    begin
      EnvValue := LowerCase(Trim(EnvValue));
      if (EnvValue = 'true') or (EnvValue = '1') or (EnvValue = 'yes') or (EnvValue = 'on') then
      begin
        Result := True;
        Exit;
      end
      else if (EnvValue = 'false') or (EnvValue = '0') or (EnvValue = 'no') or (EnvValue = 'off') then
      begin
        Result := False;
        Exit;
      end;
      // Invalid value falls through to default
    end;
  end;
  
  // Priority 3: Default
  Result := Default;
end;

class function TAppConfig.ConfigFileExists: Boolean;
begin
  Result := FConfigExists;
end;

end.

