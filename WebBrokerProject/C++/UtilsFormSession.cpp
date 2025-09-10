//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "UtilsFormSession.h"
#include "UtilsLogger.h"
#include <System.DateUtils.hpp>

//---------------------------------------------------------------------------
#pragma package(smart_init)

// Static member definitions
const String TFormSessionManager::FORM_DATA_PREFIX = "FormData_";
const String TFormSessionManager::FORM_ERRORS_PREFIX = "FormErrors_";
const String TFormSessionManager::FORM_TIMESTAMP_SUFFIX = "_timestamp";

//---------------------------------------------------------------------------
String TFormSessionManager::GetFormDataKey(const String& AFormName, const String& AFieldName)
{
    return FORM_DATA_PREFIX + AFormName + "_" + AFieldName;
}

//---------------------------------------------------------------------------
String TFormSessionManager::GetFormErrorsKey(const String& AFormName)
{
    return FORM_ERRORS_PREFIX + AFormName;
}

//---------------------------------------------------------------------------
String TFormSessionManager::GetFormTimestampKey(const String& AFormName)
{
    return FORM_DATA_PREFIX + AFormName + FORM_TIMESTAMP_SUFFIX;
}

//---------------------------------------------------------------------------
void TFormSessionManager::StoreFormData(TWebSession* ASession, const String& AFormName, TWebRequest* ARequest)
{
    if (!ASession || !ARequest || AFormName.IsEmpty())
    {
        Logger->Warning("StoreFormData: Invalid parameters");
        return;
    }
    
    try
    {
        // Store each form field in session
        for (int I = 0; I < ARequest->ContentFields->Count; I++)
        {
            String FieldName = ARequest->ContentFields->Names[I];
            String FieldValue = ARequest->ContentFields->ValueFromIndex[I];
            String SessionKey = GetFormDataKey(AFormName, FieldName);
            // Use AddPair to mimic Delphi behavior and ensure persistence
            ASession->DataVars->AddPair(SessionKey, FieldValue);
        }
        
        // Set timestamp for cleanup
        String TimestampKey = GetFormTimestampKey(AFormName);
        ASession->DataVars->AddPair(TimestampKey, FormatDateTime("yyyy-mm-dd hh:nn:ss", Now()));
        
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error storing form data: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
void TFormSessionManager::StoreValidationErrors(TWebSession* ASession, const String& AFormName, const std::vector<String>& AErrors)
{
    if (!ASession || AFormName.IsEmpty())
    {
        Logger->Warning("StoreValidationErrors: Invalid parameters");
        return;
    }
    
    try
    {
        String ErrorsKey = GetFormErrorsKey(AFormName);
        
        // Convert vector to delimited string
        String ErrorsString = "";
        for (size_t I = 0; I < AErrors.size(); I++)
        {
            if (I > 0)
                ErrorsString += "|";
            ErrorsString += AErrors[I];
        }
        
        ASession->DataVars->Values[ErrorsKey] = ErrorsString;
        
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error storing validation errors: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
std::unique_ptr<TStringDictionary> TFormSessionManager::GetFormData(TWebSession* ASession, const String& AFormName)
{
    auto FormData = std::make_unique<TStringDictionary>();
    
    if (!ASession || AFormName.IsEmpty())
    {
        Logger->Warning("GetFormData: Invalid parameters");
        return FormData;
    }
    
    try
    {
        String KeyPrefix = FORM_DATA_PREFIX + AFormName + "_";
        
        // Iterate through session data variables
        for (int I = 0; I < ASession->DataVars->Count; I++)
        {
            String Key = ASession->DataVars->Names[I];
            if (Key.Pos(KeyPrefix) == 1) // Key starts with prefix
            {
                String FieldName = Key.SubString(KeyPrefix.Length() + 1, Key.Length());
                // Skip timestamp field
                if (FieldName != "timestamp")
                {
                    // Use Values[] or ValueFromIndex as available; prefer Values[]
                    String Value = ASession->DataVars->Values[Key];
                    FormData->Add(FieldName, Value);
                }
            }
        }
        
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error getting form data: ") + E.Message);
        FormData->Clear();
    }
    
    return FormData;
}

//---------------------------------------------------------------------------
std::vector<String> TFormSessionManager::GetValidationErrors(TWebSession* ASession, const String& AFormName)
{
    std::vector<String> Errors;
    
    if (!ASession || AFormName.IsEmpty())
    {
        Logger->Warning("GetValidationErrors: Invalid parameters");
        return Errors;
    }
    
    try
    {
        String ErrorsKey = GetFormErrorsKey(AFormName);
        String ErrorsString = ASession->DataVars->Values[ErrorsKey];
        
        if (!ErrorsString.IsEmpty())
        {
            // Split delimited string into vector
            int Pos = 1;
            while (Pos <= ErrorsString.Length())
            {
                int NextPos = ErrorsString.Pos("|");
                if (NextPos == 0 || NextPos < Pos)
                    NextPos = ErrorsString.Length() + 1;
                    
                String Error = ErrorsString.SubString(Pos, NextPos - Pos);
                if (!Error.IsEmpty())
                    Errors.push_back(Error);
                    
                Pos = NextPos + 1;
            }
        }
        
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error getting validation errors: ") + E.Message);
        Errors.clear();
    }
    
    return Errors;
}

//---------------------------------------------------------------------------
void TFormSessionManager::ClearFormData(TWebSession* ASession, const String& AFormName)
{
    if (!ASession || AFormName.IsEmpty())
    {
        Logger->Warning("ClearFormData: Invalid parameters");
        return;
    }
    
    try
    {
        String KeyPrefix = FORM_DATA_PREFIX + AFormName + "_";
        String ErrorsPrefix = FORM_ERRORS_PREFIX + AFormName + "_";
        String ErrorsKey = GetFormErrorsKey(AFormName);
        
        // Remove form data keys and field-specific error keys
        for (int I = ASession->DataVars->Count - 1; I >= 0; I--)
        {
            String Key = ASession->DataVars->Names[I];
            if (Key.Pos(KeyPrefix) == 1 || Key.Pos(ErrorsPrefix) == 1 || Key == ErrorsKey)
            {
                ASession->DataVars->Delete(I);
            }
        }
        
        Logger->Debug(String("Cleared form data for form: ") + AFormName);
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error clearing form data: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
bool TFormSessionManager::HasFormData(TWebSession* ASession, const String& AFormName)
{
    if (!ASession || AFormName.IsEmpty())
    {
        return false;
    }
    
    try
    {
        String TimestampKey = GetFormTimestampKey(AFormName);
        String Timestamp = ASession->DataVars->Values[TimestampKey];
        return !Timestamp.IsEmpty();
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error checking form data: ") + E.Message);
        return false;
    }
}

//---------------------------------------------------------------------------
void TFormSessionManager::StoreFieldErrors(TWebSession* ASession, const String& AFormName, TStringDictionary* AErrors)
{
    if (!ASession || !AErrors)
        return;

    try
    {
        // Persist each field error under a unique key
        DynamicArray<String> Keys = AErrors->Keys->ToArray();
        const int Count = Keys.get_length();
        for (int i = 0; i < Count; ++i)
        {
            const String FieldName = Keys[i];
            const String ErrorMsg = AErrors->Items[FieldName];
            const String Key = FORM_ERRORS_PREFIX + AFormName + "_" + FieldName.UpperCase();
            ASession->DataVars->Values[Key] = ErrorMsg;
        }
        // Also store a count for cleanup
        ASession->DataVars->Values[FORM_ERRORS_PREFIX + AFormName + "_count"] = IntToStr(Count);
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error storing field errors: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
std::unique_ptr<TStringDictionary> TFormSessionManager::GetFieldErrors(TWebSession* ASession, const String& AFormName)
{
    auto Errors = std::make_unique<TStringDictionary>();
    if (!ASession)
        return std::move(Errors);

    try
    {
        const String Prefix = FORM_ERRORS_PREFIX + AFormName + "_";
        for (int i = 0; i < ASession->DataVars->Count; ++i)
        {
            const String Key = ASession->DataVars->Names[i];
            if (Key.Pos(Prefix) == 1)
            {
                // Extract tail after prefix and skip the special "count" key
                const String Tail = Key.SubString(Prefix.Length() + 1, Key.Length());
                if (!Tail.IsEmpty() && Tail != "count")
                {
                    const String FieldName = Tail;
                    const String Value = ASession->DataVars->ValueFromIndex[i];
                    Errors->AddOrSetValue(FieldName, Value);
                }
            }
        }
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error getting field errors: ") + E.Message);
        Errors->Clear();
    }

    return Errors;
}