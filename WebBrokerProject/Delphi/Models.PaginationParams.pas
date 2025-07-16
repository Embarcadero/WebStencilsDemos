unit Models.PaginationParams;

interface

uses
  System.SysUtils,
  System.NetEncoding,
  Web.HTTPApp;

type

  TPaginationParams = class
    FPageSize: Integer;
    FPageNumber: Integer;
    FTotalPages: Integer;
    FUri: string;
    FSearchTerm: string;
    FSearchFields: string;
  private
    procedure ParsePaginationParams(Request: TWebRequest);
    procedure ParseSearchParams(Request: TWebRequest);
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
    property SearchTerm: string read FSearchTerm write FSearchTerm;
    property SearchFields: string read FSearchFields write FSearchFields;
    function HasSearch: Boolean;
    function GetPageUrl(APage: Integer): string;
    function GetSearchSQL: string;
  end;

implementation

{ TPaginationParams }

constructor TPaginationParams.Create(ARequest: TWebRequest; AUri: string);
begin
  FUri := AUri;
  ParsePaginationParams(ARequest);
  ParseSearchParams(ARequest);
end;

procedure TPaginationParams.ParsePaginationParams(Request: TWebRequest);
var
  PageSizeStr, PageNumberStr: string;
begin
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
end;

procedure TPaginationParams.ParseSearchParams(Request: TWebRequest);
begin
  // Get search term from query parameters
  FSearchTerm := Trim(Request.QueryFields.Values['search']);
  
  // Default search fields for customers
  FSearchFields := 'FIRST_NAME,LAST_NAME,EMAIL,COMPANY,PHONE,CITY';
end;

function TPaginationParams.HasSearch: Boolean;
begin
  Result := FSearchTerm <> '';
end;

function TPaginationParams.GetPageUrl(APage: Integer): string;
begin
  Result := Format('%s?page=%d&pageSize=%d', [FUri, APage, FPageSize]);
  if HasSearch then
    Result := Result + '&search=' + TNetEncoding.URL.Encode(FSearchTerm);
end;

function TPaginationParams.GetSearchSQL: string;
begin
  if not HasSearch then
  begin
    Result := '';
    Exit;
  end;
  
  // Generate WHERE clause for customer search
  Result := 'WHERE (UPPER(FIRST_NAME) LIKE UPPER(:search) OR ' +
            'UPPER(LAST_NAME) LIKE UPPER(:search) OR ' +
            'UPPER(EMAIL) LIKE UPPER(:search) OR ' +
            'UPPER(COMPANY) LIKE UPPER(:search) OR ' +
            'UPPER(PHONE) LIKE UPPER(:search) OR ' +
            'UPPER(CITY) LIKE UPPER(:search))';
end;

end.
