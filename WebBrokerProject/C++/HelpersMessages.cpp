//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "HelpersMessages.h"
#include "UtilsLogger.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

// Static member definitions
const String TFlashMessageManager::SESSION_KEY = "messages";

//---------------------------------------------------------------------------
// TFlashMessage Implementation
//---------------------------------------------------------------------------
__fastcall TFlashMessage::TFlashMessage(TMessageType AMessageType, const String& AMessage)
    : TObject()
{
    FMessageType = AMessageType;
    FMessage = AMessage;
}

//---------------------------------------------------------------------------
__fastcall TFlashMessage::~TFlashMessage()
{
    // Base destructor
}

//---------------------------------------------------------------------------
String TFlashMessage::GetCssClass()
{
    switch (FMessageType)
    {
        case TMessageType::mtSuccess:
            return "alert-success";
        case TMessageType::mtWarning:
            return "alert-warning";
        case TMessageType::mtError:
            return "alert-danger";
        case TMessageType::mtInfo:
            return "alert-info";
        default:
            return "alert-secondary";
    }
}

//---------------------------------------------------------------------------
String TFlashMessage::GetIcon()
{
    switch (FMessageType)
    {
        case TMessageType::mtSuccess:
            return "bi-check-circle-fill";
        case TMessageType::mtWarning:
            return "bi-exclamation-triangle-fill";
        case TMessageType::mtError:
            return "bi-x-circle-fill";
        case TMessageType::mtInfo:
            return "bi-info-circle-fill";
        default:
            return "bi-info-circle";
    }
}

//---------------------------------------------------------------------------
// TMessagesList Implementation
//---------------------------------------------------------------------------
__fastcall TMessagesList::TMessagesList()
    : TObject()
{
    FList = new TList__1<TObject*>();
}

//---------------------------------------------------------------------------
__fastcall TMessagesList::~TMessagesList()
{
    if (FList)
    {
        // Do not delete contained objects here; owner manages lifetime
        delete FList;
        FList = nullptr;
    }
}

//---------------------------------------------------------------------------
void TMessagesList::Add(TFlashMessage* AItem)
{
    if (FList && AItem)
    {
        FList->Add(static_cast<TObject*>(AItem));
    }
}

//---------------------------------------------------------------------------
void TMessagesList::Clear()
{
    if (FList)
    {
        // Delete items and clear list to mimic ownership semantics
        for (int i = 0; i < FList->Count; ++i)
        {
            auto obj = static_cast<TFlashMessage*>(FList->Items[i]);
            delete obj;
        }
        FList->Clear();
    }
}

//---------------------------------------------------------------------------
int TMessagesList::Count()
{
    return FList ? FList->Count : 0;
}

//---------------------------------------------------------------------------
TFlashMessage* TMessagesList::Items(int AIndex)
{
    return FList ? static_cast<TFlashMessage*>(FList->Items[AIndex]) : nullptr;
}

//---------------------------------------------------------------------------
void TMessagesList::Remove(TFlashMessage* AItem)
{
    if (FList && AItem)
    {
        FList->Remove(static_cast<TObject*>(AItem));
    }
}

//---------------------------------------------------------------------------
void TMessagesList::Delete(int AIndex)
{
    if (FList && AIndex >= 0 && AIndex < FList->Count)
    {
        TFlashMessage* Item = static_cast<TFlashMessage*>(FList->Items[AIndex]);
        FList->Remove(static_cast<TObject*>(Item));
        delete Item;
    }
}

//---------------------------------------------------------------------------
// TMessageProvider Implementation
//---------------------------------------------------------------------------
__fastcall TMessageProvider::TMessageProvider()
    : TObject()
{
	FMessages = new TMessagesList();
    FHasMessages = false;
}

//---------------------------------------------------------------------------
__fastcall TMessageProvider::~TMessageProvider()
{
    if (FMessages)
    {
        delete FMessages;
        FMessages = nullptr;
    }
}

//---------------------------------------------------------------------------
bool TMessageProvider::GetHasMessages()
{
    return FMessages && (FMessages->Count() > 0);
}

//---------------------------------------------------------------------------
TMessagesList* TMessageProvider::GetMessages()
{
    return FMessages;
}

//---------------------------------------------------------------------------
TList__1<TObject*>* TMessageProvider::GetMessagesList()
{
    return FMessages ? FMessages->GetList() : nullptr;
}

//---------------------------------------------------------------------------
void TMessageProvider::Clear()
{
    if (FMessages)
    {
        FMessages->Clear();
        FHasMessages = false;
    }
}

//---------------------------------------------------------------------------
void TMessageProvider::AddMessage(TMessageType AMessageType, const String& AMessage)
{
    if (FMessages)
    {
        TFlashMessage* Message = new TFlashMessage(AMessageType, AMessage);
        FMessages->Add(Message);
        FHasMessages = true;
    }
}

//---------------------------------------------------------------------------
// TMessageManager Implementation
//---------------------------------------------------------------------------
TMessageProvider* TFlashMessageManager::GetMessageProvider(TWebSession* ASession)
{
    if (!ASession)
        return nullptr;
        
    TMessageProvider* Provider = nullptr;
    
    try
    {
        int Index = ASession->DataVars->IndexOf(SESSION_KEY);
        if (Index >= 0)
        {
            TObject* SessionObj = ASession->DataVars->Objects[Index];
            Provider = dynamic_cast<TMessageProvider*>(SessionObj);
        }
    }
    catch (Exception &E)
    {
        Logger->Warning(String("Error getting message provider: ") + E.Message);
        Provider = nullptr;
    }
    
    return Provider;
}

//---------------------------------------------------------------------------
void TFlashMessageManager::EnsureMessageProvider(TWebSession* ASession)
{
    if (!ASession)
        return;
        
    try
    {
        TMessageProvider* Provider = GetMessageProvider(ASession);
        if (!Provider)
        {
            Provider = new TMessageProvider();
            ASession->DataVars->AddObject(SESSION_KEY, Provider);
        }
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error ensuring message provider: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
void TFlashMessageManager::AddMessage(TWebSession* ASession, TMessageType AMessageType, const String& AMessage)
{
    if (!ASession)
        return;
        
    try
    {
        EnsureMessageProvider(ASession);
        TMessageProvider* Provider = GetMessageProvider(ASession);
        if (Provider)
        {
            Provider->AddMessage(AMessageType, AMessage);
        }
    }
    catch (Exception &E)
    {
        Logger->Error(String("Error adding message: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
void TFlashMessageManager::AddSuccessMessage(TWebSession* ASession, const String& AMessage)
{
    AddMessage(ASession, TMessageType::mtSuccess, AMessage);
}

//---------------------------------------------------------------------------
void TFlashMessageManager::AddWarningMessage(TWebSession* ASession, const String& AMessage)
{
    AddMessage(ASession, TMessageType::mtWarning, AMessage);
}

//---------------------------------------------------------------------------
void TFlashMessageManager::AddErrorMessage(TWebSession* ASession, const String& AMessage)
{
    AddMessage(ASession, TMessageType::mtError, AMessage);
}

//---------------------------------------------------------------------------
void TFlashMessageManager::AddInfoMessage(TWebSession* ASession, const String& AMessage)
{
    AddMessage(ASession, TMessageType::mtInfo, AMessage);
}

//---------------------------------------------------------------------------
void TFlashMessageManager::ClearMessages(TWebSession* ASession)
{
    if (!ASession)
        return;
        
    try
    {
        TMessageProvider* Provider = GetMessageProvider(ASession);
        if (Provider)
        {
            Provider->Clear();
        }
    }
    catch (Exception &E)
    {
        Logger->Warning(String("Error clearing messages: ") + E.Message);
    }
}

//---------------------------------------------------------------------------
TMessageProvider* TFlashMessageManager::GetMessages(TWebSession* ASession)
{
    return GetMessageProvider(ASession);
}