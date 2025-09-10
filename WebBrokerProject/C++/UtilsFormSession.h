//---------------------------------------------------------------------------
#ifndef UtilsFormSessionH
#define UtilsFormSessionH
//---------------------------------------------------------------------------

#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <Web.HTTPApp.hpp>
#include <memory>
#include <vector>

//---------------------------------------------------------------------------
typedef System::Generics::Collections::TDictionary__2<String, String> TStringDictionary;

//---------------------------------------------------------------------------
class TFormSessionManager : public TObject
{
private:
    static const String FORM_DATA_PREFIX;
    static const String FORM_ERRORS_PREFIX;
    static const String FORM_TIMESTAMP_SUFFIX;

public:
    static void StoreFormData(TWebSession* ASession, const String& AFormName, TWebRequest* ARequest);
    static void StoreValidationErrors(TWebSession* ASession, const String& AFormName, const std::vector<String>& AErrors);
    static std::unique_ptr<TStringDictionary> GetFormData(TWebSession* ASession, const String& AFormName);
    static std::vector<String> GetValidationErrors(TWebSession* ASession, const String& AFormName);
    static void ClearFormData(TWebSession* ASession, const String& AFormName);
    static bool HasFormData(TWebSession* ASession, const String& AFormName);
    // Field-level error storage/retrieval
    static void StoreFieldErrors(TWebSession* ASession, const String& AFormName, TStringDictionary* AErrors);
    static std::unique_ptr<TStringDictionary> GetFieldErrors(TWebSession* ASession, const String& AFormName);
    
private:
    static String GetFormDataKey(const String& AFormName, const String& AFieldName);
    static String GetFormErrorsKey(const String& AFormName);
    static String GetFormTimestampKey(const String& AFormName);
};

//---------------------------------------------------------------------------
#endif