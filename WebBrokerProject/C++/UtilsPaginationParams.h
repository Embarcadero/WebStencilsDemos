#ifndef UtilsPaginationParamsH
#define UtilsPaginationParamsH

#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <System.SysUtils.hpp>

class TPaginationParams : public TObject
{
private:
    int FPageSize;
    int FPageNumber;
    int FTotalPages;
    String FUri;
    void ParsePaginationParams(TWebRequest* Request);

    static const int DEFAULT_PAGE_SIZE = 10;
    static const int DEFAULT_PAGE_NUMBER = 1;
    static const int MAX_PAGE_SIZE = 100; // Prevent excessive page sizes

__published:
    __fastcall TPaginationParams(TWebRequest* ARequest, String AUri);
    __property int PageSize = {read = FPageSize};
    __property int PageNumber = {read = FPageNumber};
    __property int TotalPages = {read = FTotalPages, write = FTotalPages};
    __property String Uri = {read = FUri};
};

#endif
