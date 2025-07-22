unit Interfaces.Search;

interface

uses
  Web.HTTPApp,
  FireDAC.Comp.Client;

type
  // Interface for searchable entities
  ISearchable = interface
    ['{B1E2F3D4-5A6B-7C8D-9E0F-1A2B3C4D5E6F}']
    function GetSearchFields: TArray<string>;
    function GetSearchSQL(const ASearchTerm: string): string;
    function GetDefaultSearchFields: string;
  end;

  // Search parameters class (separate from pagination)
  TSearchParams = class
  private
    FSearchTerm: string;
    FSearchable: ISearchable;
  public
    constructor Create(ARequest: TWebRequest; ASearchable: ISearchable);
    property SearchTerm: string read FSearchTerm;
    property Searchable: ISearchable read FSearchable;
    function HasSearch: Boolean;
    function GetSearchSQL: string;
    function GetSearchTermForUrl: string;
  end;

implementation

uses
  System.SysUtils,
  System.NetEncoding;

{ TSearchParams }

constructor TSearchParams.Create(ARequest: TWebRequest; ASearchable: ISearchable);
begin
  FSearchable := ASearchable;
  FSearchTerm := Trim(ARequest.QueryFields.Values['search']);
end;

function TSearchParams.HasSearch: Boolean;
begin
  Result := FSearchTerm <> '';
end;

function TSearchParams.GetSearchSQL: string;
begin
  if not HasSearch or not Assigned(FSearchable) then
  begin
    Result := '';
    Exit;
  end;
  
  Result := FSearchable.GetSearchSQL(FSearchTerm);
end;

function TSearchParams.GetSearchTermForUrl: string;
begin
  if HasSearch then
    Result := TNetEncoding.URL.Encode(FSearchTerm)
  else
    Result := '';
end;

end. 