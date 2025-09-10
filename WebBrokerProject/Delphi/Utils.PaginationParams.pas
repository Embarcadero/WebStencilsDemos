unit Utils.PaginationParams;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  Web.HTTPApp,
  Utils.Search,
  Utils.Logger;

type

  TPaginationParams = class
  private
    FPageSize: Integer;
    FPageNumber: Integer;
    FTotalPages: Integer;
    FUri: string;
    procedure ParsePaginationParams(Request: TWebRequest);
    const
      DEFAULT_PAGE_SIZE = 10;
      DEFAULT_PAGE_NUMBER = 1;
      MAX_PAGE_SIZE = 100; // Prevent excessive page sizes
  public
    constructor Create(ARequest: TWebRequest; AUri: string);
    property PageSize: integer read FPageSize;
    property PageNumber: integer read FPageNumber;
    property TotalPages: integer read FTotalPages write FTotalPages;
    property Uri: string read FUri;
    function GetPageUrl(APage: Integer; ASearchParams: TSearchParams = nil): string;
  end;

implementation

{ TPaginationParams }

constructor TPaginationParams.Create(ARequest: TWebRequest; AUri: string);
begin
  FUri := AUri;
  ParsePaginationParams(ARequest);
end;

procedure TPaginationParams.ParsePaginationParams(Request: TWebRequest);
var
  PageSizeStr, PageNumberStr: string;
begin
  try
    // Initialize with default values
    FPageSize := DEFAULT_PAGE_SIZE;
    FPageNumber := DEFAULT_PAGE_NUMBER;

    // Try to get PageSize parameter
    PageSizeStr := Request.QueryFields.Values['pageSize'];
    if PageSizeStr <> '' then
    begin
      FPageSize := StrToIntDef(PageSizeStr, DEFAULT_PAGE_SIZE);
      // Validate PageSize
      if FPageSize <= 0 then
        FPageSize := DEFAULT_PAGE_SIZE
      else if FPageSize > MAX_PAGE_SIZE then
        FPageSize := MAX_PAGE_SIZE;
     end;

    // Try to get PageNumber parameter
    PageNumberStr := Request.QueryFields.Values['page'];
    if PageNumberStr <> '' then
    begin
      FPageNumber := StrToIntDef(PageNumberStr, DEFAULT_PAGE_NUMBER);
      // Validate PageNumber
      if FPageNumber <= 0 then
        FPageNumber := DEFAULT_PAGE_NUMBER;
    end;
  except
    on E: Exception do
      Logger.Error(Format('Error parsing pagination params: %s', [E.Message]));
  end;
end;

function TPaginationParams.GetPageUrl(APage: Integer; ASearchParams: TSearchParams = nil): string;
begin
  try
    Result := Format('%s?page=%d&pageSize=%d', [FUri, APage, FPageSize]);
    if Assigned(ASearchParams) and ASearchParams.HasSearch then
      Result := Result + '&search=' + ASearchParams.GetSearchTermForUrl;
  except
    on E: Exception do
    begin
      Logger.Error(Format('Error generating page URL: %s', [E.Message]));
      Result := FUri;
    end;
  end;
end;

end.
