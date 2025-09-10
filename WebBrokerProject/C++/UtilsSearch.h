//---------------------------------------------------------------------------
#ifndef UtilsSearchH
#define UtilsSearchH
//---------------------------------------------------------------------------

#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Web.HTTPApp.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <memory>
#include <vector>

//---------------------------------------------------------------------------
class TSearchParams : public System::Classes::TPersistent
{
private:
    String FSearchTerm;
    std::vector<String> FSearchFields;
    bool FHasSearch;
    
    void UpdateHasSearch();

public:
    __fastcall TSearchParams();
    __fastcall TSearchParams(const String& ASearchTerm, const std::vector<String>& ASearchFields);
    __fastcall virtual ~TSearchParams();
    
    String GetSearchSQL();
    void SetSearchParameters(TFDQuery* AQuery);

__published: // Properties for WebStencils RTTI access
    __property String SearchTerm = {read=FSearchTerm, write=FSearchTerm};
    __property bool HasSearch = {read=FHasSearch};

public:
    // Utility accessor (not published)
    std::vector<String>* GetSearchFields() { return &FSearchFields; }
};

//---------------------------------------------------------------------------
class TBaseSearch : public TObject
{
private:
    std::vector<String> FSearchableFields;

public:
    __fastcall TBaseSearch(const std::vector<String>& ASearchableFields);
    __fastcall virtual ~TBaseSearch();
    
    std::unique_ptr<TSearchParams> CreateSearchParams(TWebRequest* ARequest);
    std::unique_ptr<TSearchParams> CreateSearchParams(const String& ASearchTerm);
    
    // Note: Property access to vector requires getter method
    std::vector<String>* GetSearchableFields() { return &FSearchableFields; }
    __property std::vector<String>* SearchableFields = {read=GetSearchableFields};
};

//---------------------------------------------------------------------------
#endif