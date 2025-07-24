unit Utils.Search;

interface

uses
  Web.HTTPApp,
  FireDAC.Comp.Client;

type
  // Base search class that can be used for any entity
  TBaseSearch = class
  private
    FSearchFields: TArray<string>;
  public
    constructor Create(const ASearchFields: TArray<string>);
    function GetSearchFields: TArray<string>;
    function GetSearchSQL(const ASearchTerm: string): string;
    function GetDefaultSearchFields: string;
  end;

  // Search parameters class
  TSearchParams = class
  private
    FSearchTerm: string;
    FBaseSearch: TBaseSearch;
  public
    constructor Create(ARequest: TWebRequest; ABaseSearch: TBaseSearch);
    property SearchTerm: string read FSearchTerm;
    property BaseSearch: TBaseSearch read FBaseSearch;
    function HasSearch: Boolean;
    function GetSearchSQL: string;
    function GetSearchTermForUrl: string;
  end;

implementation

uses
  System.SysUtils,
  System.NetEncoding;

{ TBaseSearch }

constructor TBaseSearch.Create(const ASearchFields: TArray<string>);
begin
  FSearchFields := ASearchFields;
end;

function TBaseSearch.GetSearchFields: TArray<string>;
begin
  Result := FSearchFields;
end;

function TBaseSearch.GetDefaultSearchFields: string;
var
  Field: string;
begin
  Result := '';
  for Field in FSearchFields do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + Field;
  end;
end;

function TBaseSearch.GetSearchSQL(const ASearchTerm: string): string;
var
  Field: string;
  WhereClause: string;
begin
  if ASearchTerm = '' then
  begin
    Result := '';
    Exit;
  end;

  WhereClause := '';
  
  for Field in FSearchFields do
  begin
    if WhereClause <> '' then
      WhereClause := WhereClause + ' OR ';
    WhereClause := WhereClause + Format('UPPER(%s) LIKE UPPER(:search)', [Field]);
  end;
  
  Result := 'WHERE (' + WhereClause + ')';
end;

{ TSearchParams }

constructor TSearchParams.Create(ARequest: TWebRequest; ABaseSearch: TBaseSearch);
begin
  FBaseSearch := ABaseSearch;
  FSearchTerm := Trim(ARequest.QueryFields.Values['search']);
end;

function TSearchParams.HasSearch: Boolean;
begin
  Result := FSearchTerm <> '';
end;

function TSearchParams.GetSearchSQL: string;
begin
  if not HasSearch or not Assigned(FBaseSearch) then
  begin
    Result := '';
    Exit;
  end;
  
  Result := FBaseSearch.GetSearchSQL(FSearchTerm);
end;

function TSearchParams.GetSearchTermForUrl: string;
begin
  if HasSearch then
    Result := TNetEncoding.URL.Encode(FSearchTerm)
  else
    Result := '';
end;

end. 