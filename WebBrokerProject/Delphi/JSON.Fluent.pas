unit JSON.Fluent;

interface

uses
  System.SysUtils, System.Classes, System.JSON.Builders, System.JSON.Writers,
  System.JSON.Readers, System.JSON.Types;

type
  /// <summary>
  /// Enhanced JSON builder that can start with either objects or arrays
  /// Solves the root type limitation of TJSONObjectBuilder/TJSONArrayBuilder
  /// </summary>
  TJSONFlexibleBuilder = class(TJSONCollectionBuilder)
  private
    FStringWriter: TStringWriter;
    FJsonWriter: TJsonTextWriter;
    FOwnsWriter: Boolean;
  protected
    // Override the callback methods to provide simple defaults
    function DoGetReader(AWriter: TJsonWriter): TJsonReader; override;
    procedure DoReleaseReader(AWriter: TJsonWriter; AReader: TJsonReader); override;
    procedure DoResetWriter(AWriter: TJsonWriter); override;
  public
    /// <summary>Create with automatic writer setup</summary>
    constructor Create; overload;
    /// <summary>Create with existing writer (for advanced scenarios)</summary>
    constructor Create(const AJSONWriter: TJSONWriter); overload;
    destructor Destroy; override;

    /// <summary>Start building a JSON object - can be called as root</summary>
    function BeginObject: TJSONCollectionBuilder.TPairs;
    /// <summary>Start building a JSON array - can be called as root</summary>
    function BeginArray: TJSONCollectionBuilder.TElements;

    /// <summary>Get JSON with pretty formatting</summary>
    function ToPrettyJSON: string;
  end;

  /// <summary>Factory class for convenient JSON building</summary>
  TJSON = class
  public
    /// <summary>Create a new flexible builder</summary>
    class function NewBuilder: TJSONFlexibleBuilder;
    /// <summary>Start building an object directly</summary>
    class function NewObject: TJSONCollectionBuilder.TPairs;
    /// <summary>Start building an array directly</summary>
    class function NewArray: TJSONCollectionBuilder.TElements;
  end;

implementation

{ TJSONFlexibleBuilder }

constructor TJSONFlexibleBuilder.Create;
begin
  FStringWriter := TStringWriter.Create;
  FJsonWriter := TJsonTextWriter.Create(FStringWriter);
  FOwnsWriter := True;

  // Call inherited with our writer
  inherited Create(FJsonWriter);
end;

constructor TJSONFlexibleBuilder.Create(const AJSONWriter: TJSONWriter);
begin
  FOwnsWriter := False;
  inherited Create(AJSONWriter);
end;

destructor TJSONFlexibleBuilder.Destroy;
begin
  if FOwnsWriter then
  begin
    FJsonWriter.Free;
    FStringWriter.Free;
  end;
  inherited;
end;

function TJSONFlexibleBuilder.DoGetReader(AWriter: TJsonWriter): TJsonReader;
var
  StringReader: TStringReader;
begin
  if FOwnsWriter and (AWriter = FJsonWriter) then
  begin
    // Create reader from our string content
    StringReader := TStringReader.Create(FStringWriter.ToString);
    Result := TJsonTextReader.Create(StringReader);
  end
  else
    // Fallback - let base class handle it
    Result := inherited DoGetReader(AWriter);
end;

procedure TJSONFlexibleBuilder.DoReleaseReader(AWriter: TJsonWriter; AReader: TJsonReader);
begin
  if FOwnsWriter and (AWriter = FJsonWriter) then
  begin
    // Clean up our reader
    if AReader is TJsonTextReader then
    begin
      TJsonTextReader(AReader).Close;
      AReader.Free;
    end;
  end
  else
    // Fallback - let base class handle it
    inherited DoReleaseReader(AWriter, AReader);
end;

procedure TJSONFlexibleBuilder.DoResetWriter(AWriter: TJsonWriter);
begin
  if FOwnsWriter and (AWriter = FJsonWriter) then
  begin
    // We'll handle reset by recreating writers in Reset method
    // For now, just rewind the JSON writer
    FJsonWriter.Rewind;
  end
  else
    // Fallback - let base class handle it
    inherited DoResetWriter(AWriter);
end;

function TJSONFlexibleBuilder.BeginObject: TJSONCollectionBuilder.TPairs;
begin
  // Use the base class DoBeginObject method directly
  // This bypasses the CheckEmpty restriction in derived classes
  Result := DoBeginObject;
end;

function TJSONFlexibleBuilder.BeginArray: TJSONCollectionBuilder.TElements;
begin
  // Use the base class DoBeginArray method directly
  // This bypasses the CheckEmpty restriction in derived classes
  Result := DoBeginArray;
end;

function TJSONFlexibleBuilder.ToPrettyJSON: string;
begin
  if FOwnsWriter then
  begin
    FJsonWriter.Formatting := TJsonFormatting.Indented;
    Result := AsJSON;
  end
  else
    Result := AsJSON; // Can't control formatting on external writer
end;

{ TJSON }

class function TJSON.NewBuilder: TJSONFlexibleBuilder;
begin
  Result := TJSONFlexibleBuilder.Create;
end;

class function TJSON.NewObject: TJSONCollectionBuilder.TPairs;
var
  Builder: TJSONFlexibleBuilder;
begin
  Builder := TJSONFlexibleBuilder.Create;
  Result := Builder.BeginObject;
end;

class function TJSON.NewArray: TJSONCollectionBuilder.TElements;
var
  Builder: TJSONFlexibleBuilder;
begin
  Builder := TJSONFlexibleBuilder.Create;
  Result := Builder.BeginArray;
end;

end.
