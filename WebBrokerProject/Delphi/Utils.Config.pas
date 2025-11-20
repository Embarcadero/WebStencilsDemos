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
    DatabasePath=C:\MyApp\data\database.sqlite3
    BackupDbPath=C:\MyApp\backup\database.sqlite3
    
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
    class function ReadString(const Section, Key, Default: string): string;
    class function ReadInteger(const Section, Key: string; Default: Integer): Integer;
    class function ReadBool(const Section, Key: string; Default: Boolean): Boolean;
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

class function TAppConfig.ReadString(const Section, Key, Default: string): string;
begin
  if FConfigExists and Assigned(FIniFile) then
    Result := FIniFile.ReadString(Section, Key, Default)
  else
    Result := Default;
end;

class function TAppConfig.ReadInteger(const Section, Key: string; Default: Integer): Integer;
begin
  if FConfigExists and Assigned(FIniFile) then
    Result := FIniFile.ReadInteger(Section, Key, Default)
  else
    Result := Default;
end;

class function TAppConfig.ReadBool(const Section, Key: string; Default: Boolean): Boolean;
var
  ValueStr: string;
  OriginalValue: string;
begin
  if FConfigExists and Assigned(FIniFile) then
  begin
    // Read as string first to handle various boolean representations
    OriginalValue := FIniFile.ReadString(Section, Key, '');
    ValueStr := Trim(OriginalValue);
    
    // If key doesn't exist or is empty, return default
    if ValueStr = '' then
    begin
      Result := Default;
      Exit;
    end;
    
    // Parse common boolean string representations
    // We'll handle more variations: true, false, 1, 0, yes, no, on, off
    ValueStr := LowerCase(ValueStr);
    if (ValueStr = 'true') or (ValueStr = '1') or (ValueStr = 'yes') or (ValueStr = 'on') then
    begin
      Result := True;
    end
    else if (ValueStr = 'false') or (ValueStr = '0') or (ValueStr = 'no') or (ValueStr = 'off') then
    begin
      Result := False;
    end
    else
    begin
      // Fall back to TIniFile.ReadBool for standard Delphi INI boolean values
      Result := FIniFile.ReadBool(Section, Key, Default);
    end;
  end
  else
  begin
    Result := Default;
  end;
end;

class function TAppConfig.ConfigFileExists: Boolean;
begin
  Result := FConfigExists;
end;

end.

