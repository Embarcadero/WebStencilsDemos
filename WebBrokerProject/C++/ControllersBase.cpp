//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "ControllersBase.h"
#include "UtilsLogger.h"
#include <System.IOUtils.hpp>
#include <System.StrUtils.hpp>

//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------
// TFieldErrorManager Implementation
//---------------------------------------------------------------------------
__fastcall TFieldErrorManager::TFieldErrorManager()
    : TObject()
{
    FErrors = std::make_unique<TStringDictionary>();
}

//---------------------------------------------------------------------------
__fastcall TFieldErrorManager::~TFieldErrorManager()
{
    if (FErrors)
    {
        FErrors->Clear();
    }
}

//---------------------------------------------------------------------------
int TFieldErrorManager::GetErrorCount()
{
    return FErrors ? FErrors->Count : 0;
}

//---------------------------------------------------------------------------
void TFieldErrorManager::AddError(const String& AFieldName, const String& AMessage)
{
    if (FErrors && !AFieldName.IsEmpty() && !AMessage.IsEmpty())
    {
        String Key = AFieldName.UpperCase();
        FErrors->AddOrSetValue(Key, AMessage);
    }
}

//---------------------------------------------------------------------------
void TFieldErrorManager::Clear()
{
    if (FErrors)
    {
        FErrors->Clear();
    }
}

//---------------------------------------------------------------------------
String TFieldErrorManager::GetError(const String& AFieldName)
{
    String Key = AFieldName.UpperCase();
    if (FErrors && FErrors->ContainsKey(Key))
    {
        return FErrors->Items[Key];
    }
    return "";
}

//---------------------------------------------------------------------------
bool TFieldErrorManager::HasError(const String& AFieldName)
{
    String Key = AFieldName.UpperCase();
    return FErrors && FErrors->ContainsKey(Key);
}

//---------------------------------------------------------------------------
// TBaseController Implementation
//---------------------------------------------------------------------------
__fastcall TBaseController::TBaseController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, const String& AControllerName)
    : TObject()
{
    try
    {
        FWebStencilsEngine = AWebStencilsEngine;
        FControllerName = AControllerName;
        FFieldErrorManager = std::make_unique<TFieldErrorManager>();
        
        // Create processor
        FWebStencilsProcessor = new Web::Stencils::TWebStencilsProcessor(nullptr);
        FWebStencilsProcessor->Engine = FWebStencilsEngine;
        
        // Initialize validation errors in template processor (like Delphi)
        InitializeValidationErrors();
        
        Logger->Debug(String("Created base controller: ") + FControllerName);
    }
    catch (Exception &E)
    {
        Logger->Error(String("TBaseController constructor error: ") + E.Message);
        throw;
    }
}

//---------------------------------------------------------------------------
__fastcall TBaseController::~TBaseController()
{
    try
    {
        delete FWebStencilsProcessor;
        Logger->Debug(String("Destroyed base controller: ") + FControllerName);
    }
    catch (...)
    {
        // Ignore exceptions during cleanup
    }
}

//---------------------------------------------------------------------------
String TBaseController::RenderTemplate(const String& ATemplatePath, TWebRequest* ARequest)
{
    try
    {
        String RootDir = FWebStencilsEngine->RootDirectory;
        if (!RootDir.IsEmpty() && *RootDir.LastChar() != System::Sysutils::PathDelim)
        {
            RootDir += System::Sysutils::PathDelim;
        }
        
        FWebStencilsProcessor->InputFileName = RootDir + ATemplatePath;
        if (ARequest)
        {
            FWebStencilsProcessor->WebRequest = ARequest;
        }
        
        // Add flash messages for current session (matches Delphi behavior)
        if (ARequest && ARequest->Session)
        {
            auto Provider = TFlashMessageManager::GetMessages(ARequest->Session);
            if (Provider)
            {
                FWebStencilsProcessor->AddVar("messages", Provider, false);
            }
        }
        
        // Add form data from session if request is provided
        if (ARequest && ARequest->Session)
        {
            // Determine logical form name from template path (customer_add/customer_edit) or controller name
            String FormName = FControllerName;
            if (ATemplatePath.Pos("customers/add") > 0) FormName = "customer_add";
            else if (ATemplatePath.Pos("customers/edit") > 0) FormName = "customer_edit";

            auto FormData = TFormSessionManager::GetFormData(ARequest->Session, FormName);
            if (FormData && FormData->Count > 0)
            {
                FWebStencilsProcessor->AddVar("formData", FormData.get(), false);
            }

            // Merge field-level errors from session into the manager so methods are callable
            auto FieldErrDict = TFormSessionManager::GetFieldErrors(ARequest->Session, FormName);
            if (FieldErrDict && FieldErrDict->Count > 0)
            {
                // Clear current errors and repopulate
                FFieldErrorManager->Clear();
                DynamicArray<String> Keys = FieldErrDict->Keys->ToArray();
                const int Count = Keys.get_length();
                for (int i = 0; i < Count; ++i)
                {
                    const String Key = Keys[i];
                    const String Msg = FieldErrDict->Items[Key];
                    FFieldErrorManager->AddError(Key, Msg);
                }
            }
        }
        
        String Result = FWebStencilsProcessor->Content();
        
        // Clear validation errors after template processing (matches Delphi behavior)
        ClearValidationErrors();
        
        // Clean up temporary variables (fieldErrors is persistent, formData and messages are temporary)
        FWebStencilsProcessor->DataVars->Remove("formData");
        FWebStencilsProcessor->DataVars->Remove("messages");
        
        return Result;
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error rendering template ") + ATemplatePath + ": " + E.Message);
        return String("Error rendering template: ") + E.Message;
    }
}

//---------------------------------------------------------------------------
void TBaseController::Redirect(TWebResponse* AResponse, const String& ALocation)
{
    if (AResponse)
    {
        AResponse->SendRedirect(ALocation);
    }
}

//---------------------------------------------------------------------------
TWebSession* TBaseController::GetCurrentSession(TWebRequest* ARequest)
{
    return ARequest ? ARequest->Session : nullptr;
}

//---------------------------------------------------------------------------
void TBaseController::AddSuccessMessage(TWebRequest* ARequest, const String& AMessage)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        TFlashMessageManager::AddSuccessMessage(Session, AMessage);
    }
}

//---------------------------------------------------------------------------
void TBaseController::AddWarningMessage(TWebRequest* ARequest, const String& AMessage)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        TFlashMessageManager::AddWarningMessage(Session, AMessage);
    }
}

//---------------------------------------------------------------------------
void TBaseController::AddErrorMessage(TWebRequest* ARequest, const String& AMessage)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        TFlashMessageManager::AddErrorMessage(Session, AMessage);
    }
}

//---------------------------------------------------------------------------
void TBaseController::AddInfoMessage(TWebRequest* ARequest, const String& AMessage)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        TFlashMessageManager::AddInfoMessage(Session, AMessage);
    }
}

//---------------------------------------------------------------------------
void TBaseController::InitializeValidationErrors()
{
    // Always add field error manager to template processor (empty by default) - matches Delphi behavior
    FWebStencilsProcessor->AddVar("fieldErrors", FFieldErrorManager.get(), false);
}

//---------------------------------------------------------------------------
void TBaseController::ValidateRequired(const String& AFieldName, const String& AValue, const String& ADisplayName)
{
    if (AValue.Trim().IsEmpty())
    {
        String DisplayName = ADisplayName.IsEmpty() ? AFieldName : ADisplayName;
        FFieldErrorManager->AddError(AFieldName, DisplayName + " is required");
    }
}

//---------------------------------------------------------------------------
void TBaseController::ValidateEmail(const String& AFieldName, const String& AValue)
{
    if (!AValue.Trim().IsEmpty())
    {
        // Simple email validation
        if (AValue.Pos("@") == 0 || AValue.Pos(".") == 0)
        {
            FFieldErrorManager->AddError(AFieldName, "Please enter a valid email address");
        }
    }
}

//---------------------------------------------------------------------------
void TBaseController::ValidateLength(const String& AFieldName, const String& AValue, int AMinLength, int AMaxLength)
{
    int Length = AValue.Trim().Length();
    
    if (AMinLength > 0 && Length < AMinLength)
    {
        FFieldErrorManager->AddError(AFieldName, String("Minimum length is ") + IntToStr(AMinLength) + " characters");
    }
    
    if (AMaxLength > 0 && Length > AMaxLength)
    {
        FFieldErrorManager->AddError(AFieldName, String("Maximum length is ") + IntToStr(AMaxLength) + " characters");
    }
}

//---------------------------------------------------------------------------
bool TBaseController::HasValidationErrors()
{
    return FFieldErrorManager->ErrorCount > 0;
}

//---------------------------------------------------------------------------
void TBaseController::ClearValidationErrors()
{
    FFieldErrorManager->Clear();
}

//---------------------------------------------------------------------------
void TBaseController::StoreFormDataInSession(TWebRequest* ARequest, const String& AFormName)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session && ARequest)
    {
        TFormSessionManager::StoreFormData(Session, AFormName, ARequest);
        // Persist field-level errors alongside form data (use the FormName instead of ControllerName)
        TFormSessionManager::StoreFieldErrors(Session, AFormName, FFieldErrorManager->GetErrorsDict());
    }
}

//---------------------------------------------------------------------------
void TBaseController::AssignFieldValue(TField* Field, const String& Value)
{
    if (!Field)
        return;

    if (Value.IsEmpty())
    {
        Field->Clear();
        return;
    }

    try
    {
        switch (Field->DataType)
        {
            case ftBoolean:
                // For boolean fields, if the field is present in POST data, it's True
                // If not present, it's False (handled by the hidden field)
                Field->AsBoolean = SameText(Value, "True");
                break;
            case ftDate:
            case ftDateTime:
            case ftTime:
                // Try ISO8601 conversion first, fallback to string
                try
                {
                    TDateTime DateValue = ISO8601ToDate(Value, false);
                    Field->AsDateTime = DateValue;
                }
                catch (...)
                {
                    Field->AsString = Value; // Fallback to string
                }
                break;
            default:
                Field->AsString = Value;
                break;
        }
    }
    catch (Exception &E)
    {
        // On any conversion error, fallback to string assignment
        Field->AsString = Value;
        Logger->Warning(String("Field assignment warning for ") + Field->FieldName + ": " + E.Message);
    }
}

//---------------------------------------------------------------------------
void TBaseController::RestoreFormDataToDataset(TDataSet* DataSet, TWebRequest* ARequest, const String& FormName)
{
    if (!DataSet || !ARequest)
        return;

    auto FormData = TFormSessionManager::GetFormData(GetCurrentSession(ARequest), FormName);
    if (!FormData || FormData->Count == 0)
    {
        return;
    }

    // Enumerate keys and assign values (C++Builder TDictionary has Keys/Items but no STL iterators)
    DynamicArray<String> Keys = FormData->Keys->ToArray();
    const int Count = Keys.get_length();
    for (int i = 0; i < Count; ++i)
    {
        const String FieldName = Keys[i];
        const String FieldValue = FormData->Items[FieldName];
        // Try case-sensitive first, then case-insensitive lookup
        TField* Field = DataSet->FindField(FieldName);
        if (!Field)
        {
            // Try with uppercase (database fields are typically uppercase)
            Field = DataSet->FindField(FieldName.UpperCase());
        }
        
        if (Field)
        {
            AssignFieldValue(Field, FieldValue);
        }
    }
}

//---------------------------------------------------------------------------
void TBaseController::PopulateDatasetFromRequest(TDataSet* ADataset, TWebRequest* ARequest, const std::vector<String>& ASkipFields)
{
    if (!ADataset || !ARequest)
        return;

    for (int I = 0; I < ARequest->ContentFields->Count; I++)
    {
        String FieldName = ARequest->ContentFields->Names[I];
        String FieldValue = ARequest->ContentFields->ValueFromIndex[I];
        
        // Check if this field should be skipped
        bool SkipField = false;
        for (const String& SkipName : ASkipFields)
        {
            if (SameText(FieldName, SkipName))
            {
                SkipField = true;
                break;
            }
        }
        
        if (!SkipField)
        {
            // Try case-sensitive first, then case-insensitive lookup
            TField* Field = ADataset->FindField(FieldName);
            if (!Field)
            {
                // Try with uppercase (database fields are typically uppercase)
                Field = ADataset->FindField(FieldName.UpperCase());
            }
            
            if (Field)
            {
                AssignFieldValue(Field, FieldValue);
            }
        }
    }
}

//---------------------------------------------------------------------------
void TBaseController::ClearFormSessionAfterProcessing(TWebRequest* ARequest, const String& FormName)
{
    ClearFormSession(ARequest, FormName);
}

//---------------------------------------------------------------------------
std::unique_ptr<TStringDictionary> TBaseController::GetFormDataFromSession(TWebRequest* ARequest, const String& AFormName)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        return TFormSessionManager::GetFormData(Session, AFormName);
    }
    return std::make_unique<TStringDictionary>();
}

//---------------------------------------------------------------------------
void TBaseController::ClearFormSession(TWebRequest* ARequest, const String& AFormName)
{
    TWebSession* Session = GetCurrentSession(ARequest);
    if (Session)
    {
        TFormSessionManager::ClearFormData(Session, AFormName);
    }
}