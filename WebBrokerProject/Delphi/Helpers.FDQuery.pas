unit Helpers.FDQuery;

interface

uses
  System.SysUtils,
  System.Classes,
  FireDAC.Comp.Client,
  FireDAC.Stan.Param,
  FireDAC.Stan.Option,
  Utils.Logger;

type
  TFDQueryHelper = class helper for TFDQuery
  private
    class var FCachedRecordCount: Integer;
    class var FLastCountTime: TDateTime;
    const CACHE_TIMEOUT = 5; // seconds
  private
    function GetPageSize: Integer;
    procedure SetPageSize(const Value: Integer);
    function GetPageNumber: Integer;
    procedure SetPageNumber(const Value: Integer);
    function GetTotalPages: Integer;
    function GetTotalRecords: Integer;
    function GetActualSkipCount: Integer;
  public
    property PageSize: Integer read GetPageSize write SetPageSize;
    property PageNumber: Integer read GetPageNumber write SetPageNumber;
    property TotalPages: Integer read GetTotalPages;
    property TotalRecords: Integer read GetTotalRecords;
    procedure ApplyPagination;
    procedure CancelPagination;
  end;

implementation

uses
  System.DateUtils;

{ TFDQueryHelper }

// To be sure we are not getting a -1
function TFDQueryHelper.GetActualSkipCount: Integer;
begin
  Result := FetchOptions.RecsSkip;
  if Result < 0 then
    Result := 0;
end;

function TFDQueryHelper.GetPageSize: Integer;
begin
  Result := FetchOptions.RecsMax;
  if Result <= 0 then
    Result := MaxInt;
end;

procedure TFDQueryHelper.SetPageSize(const Value: Integer);
var
  LOldPageNumber: Integer;
begin
  if Value <= 0 then
    raise Exception.Create('PageSize must be greater than 0');

  try
    // Store the current page before changing page size
    LOldPageNumber := PageNumber;

    // Set the new page size
    FetchOptions.RecsMax := Value;

    // Keep the same page number, but recalculate skip count for new page size
    FetchOptions.RecsSkip := (LOldPageNumber - 1) * Value;
  except
    on E: Exception do
      Logger.Error(Format('Error setting page size: %s', [E.Message]));
  end;
end;

function TFDQueryHelper.GetPageNumber: Integer;
begin
  if PageSize = MaxInt then
    Result := 1
  else
    Result := (GetActualSkipCount div PageSize) + 1;
end;

procedure TFDQueryHelper.SetPageNumber(const Value: Integer);
begin
  if Value <= 0 then
    raise Exception.Create('PageNumber must be greater than 0');

  try
    // Calculate skip count based on the new page number and current page size
    FetchOptions.RecsSkip := (Value - 1) * PageSize;
  except
    on E: Exception do
      Logger.Error(Format('Error setting page number: %s', [E.Message]));
  end;
end;

function TFDQueryHelper.GetTotalRecords: Integer;
var
  LCloneQuery: TFDQuery;
begin
  // Check cache first
  if (FCachedRecordCount > 0) and
     (SecondsBetween(Now, FLastCountTime) < CACHE_TIMEOUT) then
    Exit(FCachedRecordCount);

  try
    // Cache expired or not set, get fresh count
    LCloneQuery := TFDQuery.Create(nil);
    try
      LCloneQuery.Connection := Self.Connection;
      LCloneQuery.SQL.Text := Self.SQL.Text;

      if Params.Count > 0 then
        LCloneQuery.Params.AssignValues(Self.Params);

      // Let FireDAC optimize the count operation
      LCloneQuery.FetchOptions.RecordCountMode := cmTotal;

      LCloneQuery.Open;
      try
        if LCloneQuery.RecordCount > 0 then
          Result := LCloneQuery.RecordCount
        else
          Result := 0;

        // Update cache
        FCachedRecordCount := Result;
        FLastCountTime := Now;
      finally
        LCloneQuery.Close;
      end;
    finally
      LCloneQuery.Free;
    end;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error getting total records: %s', [E.Message]));
      Result := 0;
    end;
  end;
end;

function TFDQueryHelper.GetTotalPages: Integer;
var
  LTotalRecs: Integer;
begin
  if PageSize = MaxInt then
    Result := 1
  else
  begin
    LTotalRecs := GetTotalRecords;
    Result := (LTotalRecs + PageSize - 1) div PageSize;
    if Result = 0 then
      Result := 1;
  end;
end;

procedure TFDQueryHelper.ApplyPagination;
var
  LSavedSkip: Integer;
  LSavedMax: Integer;
begin
  if not Active then Exit;

  try
    // Save current fetch settings
    LSavedSkip := FetchOptions.RecsSkip;
    LSavedMax := FetchOptions.RecsMax;

    Disconnect;
    // Reapply fetch settings before opening
    FetchOptions.RecsSkip := LSavedSkip;
    FetchOptions.RecsMax := LSavedMax;
    Open;
  except
    on E: Exception do
      Logger.Error(Format('Error applying pagination: %s', [E.Message]));
  end;
end;

procedure TFDQueryHelper.CancelPagination;
begin
  try
    FetchOptions.RecsSkip := -1;
    FetchOptions.RecsMax := -1;
    if not Active then Exit;

    Disconnect;
    Open;
  except
    on E: Exception do
      Logger.Error(Format('Error canceling pagination: %s', [E.Message]));
  end;
end;

end.
