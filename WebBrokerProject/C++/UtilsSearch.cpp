//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "UtilsSearch.h"
#include "UtilsLogger.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------
// TSearchParams Implementation
//---------------------------------------------------------------------------
__fastcall TSearchParams::TSearchParams()
    : System::Classes::TPersistent()
{
    FSearchTerm = "";
    FHasSearch = false;
}

//---------------------------------------------------------------------------
__fastcall TSearchParams::TSearchParams(const String& ASearchTerm, const std::vector<String>& ASearchFields)
    : System::Classes::TPersistent()
{
    FSearchTerm = ASearchTerm.Trim();
    FSearchFields = ASearchFields;
    UpdateHasSearch();
}

//---------------------------------------------------------------------------
__fastcall TSearchParams::~TSearchParams()
{
    FSearchFields.clear();
}

//---------------------------------------------------------------------------
void TSearchParams::UpdateHasSearch()
{
    FHasSearch = !FSearchTerm.IsEmpty() && !FSearchFields.empty();
}

//---------------------------------------------------------------------------
String TSearchParams::GetSearchSQL()
{
    if (!FHasSearch)
        return "";
        
    try
    {
        String SearchSQL = " WHERE (";
        
        for (size_t I = 0; I < FSearchFields.size(); I++)
        {
            if (I > 0)
                SearchSQL += " OR ";
                
            SearchSQL += "UPPER(" + FSearchFields[I] + ") LIKE UPPER(:search)";
        }
        
        SearchSQL += ")";
        
        return SearchSQL;
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error generating search SQL: ") + E.Message);
        return "";
    }
}

//---------------------------------------------------------------------------
void TSearchParams::SetSearchParameters(TFDQuery* AQuery)
{
    if (!AQuery || !FHasSearch)
        return;
        
    try
    {
        // Set the search parameter with wildcards
        String SearchValue = "%" + FSearchTerm + "%";
        AQuery->ParamByName("search")->AsString = SearchValue;
        
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error setting search parameters: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
// TBaseSearch Implementation
//---------------------------------------------------------------------------
__fastcall TBaseSearch::TBaseSearch(const std::vector<String>& ASearchableFields)
    : TObject()
{
    FSearchableFields = ASearchableFields;
    
}

//---------------------------------------------------------------------------
__fastcall TBaseSearch::~TBaseSearch()
{
    FSearchableFields.clear();
}

//---------------------------------------------------------------------------
std::unique_ptr<TSearchParams> TBaseSearch::CreateSearchParams(TWebRequest* ARequest)
{
    if (!ARequest)
        return std::make_unique<TSearchParams>();
        
    try
    {
        // Get search term from query parameters
        String SearchTerm = "";
        
        // Try different common parameter names
        if (ARequest->QueryFields->IndexOfName("search") >= 0)
            SearchTerm = ARequest->QueryFields->Values["search"];
        else if (ARequest->QueryFields->IndexOfName("q") >= 0)
            SearchTerm = ARequest->QueryFields->Values["q"];
        else if (ARequest->QueryFields->IndexOfName("term") >= 0)
            SearchTerm = ARequest->QueryFields->Values["term"];
            
        SearchTerm = SearchTerm.Trim();
        
        auto SearchParams = std::make_unique<TSearchParams>(SearchTerm, FSearchableFields);
        return SearchParams;
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error creating search params from request: ") + E.Message);
        return std::make_unique<TSearchParams>();
    }
}

//---------------------------------------------------------------------------
std::unique_ptr<TSearchParams> TBaseSearch::CreateSearchParams(const String& ASearchTerm)
{
    try
    {
        auto SearchParams = std::make_unique<TSearchParams>(ASearchTerm, FSearchableFields);
        return SearchParams;
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error creating search params: ") + E.Message);
        return std::make_unique<TSearchParams>();
    }
}