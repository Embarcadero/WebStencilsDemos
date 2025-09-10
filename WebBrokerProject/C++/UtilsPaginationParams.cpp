#pragma hdrstop

#include "UtilsPaginationParams.h"
#include <System.SysUtils.hpp>

//-----------------------------------------------------------------------------
#pragma package(smart_init)
//-----------------------------------------------------------------------------

__fastcall TPaginationParams::TPaginationParams(TWebRequest* ARequest, String AUri)
    : TObject()
{
    FUri = AUri;
    ParsePaginationParams(ARequest);
}
//-----------------------------------------------------------------------------

void TPaginationParams::ParsePaginationParams(TWebRequest* Request)
{
    String PageSizeStr, PageNumberStr;

    // Initialize with default values
    FPageSize = DEFAULT_PAGE_SIZE;
    FPageNumber = DEFAULT_PAGE_NUMBER;

    // Try to get PageSize parameter
    PageSizeStr = Request->QueryFields->Values["pageSize"];
    if (!PageSizeStr.IsEmpty())
    {
        FPageSize = StrToIntDef(PageSizeStr, DEFAULT_PAGE_SIZE);
        // Validate PageSize
        if (FPageSize <= 0)
            FPageSize = DEFAULT_PAGE_SIZE;
        else if (FPageSize > MAX_PAGE_SIZE)
            FPageSize = MAX_PAGE_SIZE;
    }

    // Try to get PageNumber parameter
    PageNumberStr = Request->QueryFields->Values["page"];
    if (!PageNumberStr.IsEmpty())
    {
        FPageNumber = StrToIntDef(PageNumberStr, DEFAULT_PAGE_NUMBER);
        // Validate PageNumber
        if (FPageNumber <= 0)
            FPageNumber = DEFAULT_PAGE_NUMBER;
    }
}
//-----------------------------------------------------------------------------
