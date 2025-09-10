//---------------------------------------------------------------------------
#ifndef MainWebModuleH
#define MainWebModuleH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.Stan.StorageJSON.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.SQLite.hpp>
#include <FireDAC.Phys.SQLiteDef.hpp>
#include <FireDAC.Phys.SQLiteWrapper.Stat.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.ExprFuncs.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FireDAC.VCLUI.Wait.hpp>
#include <memory> // Required for std::unique_ptr
#include <System.Generics.Collections.hpp> // Required for TDictionary

// Include own units/controllers
#include "CodeExamplesU.h"
#include "ClassHelpers.h"
#include "ControllerTasks.h"
#include "ControllerCustomers.h"
#include "UtilsLogger.h"
#include "HelpersMessages.h"

// Other FireDAC includes (cleaned up duplicates)
#include <FireDAC.DApt.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.SQLite.hpp>
#include <FireDAC.Phys.SQLiteDef.hpp>
#include <FireDAC.Phys.SQLiteWrapper.Stat.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.ExprFuncs.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FireDAC.VCLUI.Wait.hpp>

//---------------------------------------------------------------------------

// Define the Environment Settings class
class TEnvironmentSettings : public System::Classes::TPersistent
{
private:
    System::UnicodeString FAppVersion;
    System::UnicodeString FAppName;
    System::UnicodeString FAppEdition;
    System::UnicodeString FCompanyName;
    System::UnicodeString FResource;
    bool FDebugMode;
    bool FIsRadServer;

public:
    __fastcall TEnvironmentSettings();

__published:
    // Match Delphi template names (snake_case)
    __property System::UnicodeString app_name = {read=FAppName};
    __property System::UnicodeString version = {read=FAppVersion};
    __property System::UnicodeString edition = {read=FAppEdition};
    __property System::UnicodeString company = {read=FCompanyName};
    __property System::UnicodeString resource = {read=FResource};
    __property bool is_rad_server = {read=FIsRadServer};
    __property bool debug = {read=FDebugMode};
};

//---------------------------------------------------------------------------
class TMainWebModule : public TWebModule
{
__published:    // IDE-managed Components
    TWebStencilsEngine *WebStencilsEngine;
    TWebFileDispatcher *WebFileDispatcher;
    TFDQuery *Customers;
    TFDConnection *Connection;
    TWebSessionManager *WebSessionManager;
    TWebFormsAuthenticator *WebFormsAuthenticator;
    TWebAuthorizer *WebAuthorizer;
    TFDAutoIncField *CustomersID;
    TStringField *CustomersCOMPANY;
    TStringField *CustomersFIRST_NAME;
    TStringField *CustomersLAST_NAME;
    TStringField *CustomersGENDER;
    TStringField *CustomersEMAIL;
    TStringField *CustomersPHONE;
    TStringField *CustomersADDRESS;
    TStringField *CustomersPOSTAL_CODE;
    TStringField *CustomersCITY;
    TStringField *CustomersCOUNTRY;
    TStringField *CustomersIP_ADDRESS;
    TFDQuery *Countries;
    TIntegerField *CustomersAGE;
    TDateField *CustomersACTIVATION_DATE;
    TBooleanField *CustomersACTIVE;
    TWideMemoField *CustomersCOMMENTS;
    TStringField *CountriesCOUNTRY;
    void __fastcall WebModuleCreate(TObject *Sender);
    void __fastcall WebModuleAfterDispatch(TObject *Sender, TWebRequest *Request, TWebResponse *Response,
          bool &Handled);
    void __fastcall WebSessionManagerCreated(TCustomWebSessionManager *Sender, TWebRequest *Request,
          TWebSession *Session);
    void __fastcall WebFormsAuthenticatorAuthenticate(TCustomWebAuthenticator *Sender,
          TWebRequest *Request, const UnicodeString UserName,
          const UnicodeString Password, UnicodeString &Roles,
          bool &Success);
    void __fastcall WebStencilsEngineValue(TObject *Sender, const UnicodeString AObjectName,
          const UnicodeString APropName, UnicodeString &AValue,
          bool &AHandled);
    void __fastcall WebModule1ActHealthAction(TObject *Sender, TWebRequest *Request,
            TWebResponse *Response, bool &Handled);

private:    // User declarations
    std::unique_ptr<TTasksController> FTasksController;
    std::unique_ptr<TCustomersController> FCustomersController;
    std::unique_ptr<TCodeExamples> FCodeExamples;
    std::unique_ptr<TEnvironmentSettings> FEnvironmentSettings; // Changed from TDictionary unique_ptr
    String FResourcesPath;

    void DefineRoutes();
    void InitRequiredData();
    void InitControllers();

public:        // User declarations
    __fastcall TMainWebModule(TComponent* Owner);
    __fastcall virtual ~TMainWebModule();
};
//---------------------------------------------------------------------------
extern PACKAGE TMainWebModule *MainWebModule;
//---------------------------------------------------------------------------
#endif


