//---------------------------------------------------------------------------
#ifndef HelpersMessagesH
#define HelpersMessagesH
//---------------------------------------------------------------------------

#include <System.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <System.Generics.Collections.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <memory>
#include <vector>

//---------------------------------------------------------------------------
enum class TMessageType { mtSuccess, mtWarning, mtError, mtInfo };

//---------------------------------------------------------------------------
class TFlashMessage : public TObject
{
private:
    TMessageType FMessageType;
    String FMessage;
    
    String GetCssClass();
    String GetIcon();

public:
    __fastcall TFlashMessage(TMessageType AMessageType, const String& AMessage);
    __fastcall virtual ~TFlashMessage();
    
    __published:
    __property TMessageType MessageType = {read=FMessageType, write=FMessageType};
    __property String Message = {read=FMessage, write=FMessage};
    __property String CssClass = {read=GetCssClass};
    __property String Icon = {read=GetIcon};
};

//---------------------------------------------------------------------------
// Concrete wrapper class to avoid generic template instantiation issues
class TMessagesList : public TObject
{
private:
    TList__1<TObject*>* FList;
    
public:
    __fastcall TMessagesList();
    __fastcall virtual ~TMessagesList();
    
    void Add(TFlashMessage* AItem);
    void Clear();
    int Count();
    TFlashMessage* Items(int AIndex);
    void Remove(TFlashMessage* AItem);
    void Delete(int AIndex);
    
    // Property accessor
    TList__1<TObject*>* GetList() { return FList; }
};

//---------------------------------------------------------------------------
class TMessageProvider : public TObject
{
private:
    TMessagesList* FMessages;
    bool FHasMessages;
    
    bool GetHasMessages();
    TMessagesList* GetMessages();
    TList__1<TObject*>* GetMessagesList();

public:
    __fastcall TMessageProvider();
    __fastcall virtual ~TMessageProvider();
    
    void Clear();
    void AddMessage(TMessageType AMessageType, const String& AMessage);
    
__published: // Properties for WebStencils RTTI access
    __property bool HasMessages = {read=GetHasMessages};
    // Expose a raw list of TObject* so WebStencils can iterate it
    __property TList__1<TObject*>* Messages = {read=GetMessagesList};
    // Keep wrapper access if needed elsewhere
    __property TMessagesList* MessagesWrapper = {read=GetMessages};
};

//---------------------------------------------------------------------------
class TFlashMessageManager : public TObject
{
private:
    static const String SESSION_KEY;
    
    static TMessageProvider* GetMessageProvider(TWebSession* ASession);

public:
    static void EnsureMessageProvider(TWebSession* ASession);
    static void AddMessage(TWebSession* ASession, TMessageType AMessageType, const String& AMessage);
    static void AddSuccessMessage(TWebSession* ASession, const String& AMessage);
    static void AddWarningMessage(TWebSession* ASession, const String& AMessage);
    static void AddErrorMessage(TWebSession* ASession, const String& AMessage);
    static void AddInfoMessage(TWebSession* ASession, const String& AMessage);
    static void ClearMessages(TWebSession* ASession);
    static TMessageProvider* GetMessages(TWebSession* ASession);
};

//---------------------------------------------------------------------------
#endif