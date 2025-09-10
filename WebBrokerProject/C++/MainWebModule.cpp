//---------------------------------------------------------------------------
#include "MainWebModule.h"
#include <IdHTTPWebBrokerBridge.hpp>
#include <IdContext.hpp>
#include <System.IOUtils.hpp>
#include <System.DateUtils.hpp>
#include <System.SysUtils.hpp>
#include <vector>
#include <functional>

//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "Vcl.Controls.TControl"
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(TMainWebModule);
//---------------------------------------------------------------------------

// Implementation for TEnvironmentSettings constructor
__fastcall TEnvironmentSettings::TEnvironmentSettings()
    : System::Classes::TPersistent()
{
    FAppVersion = "1.5.2";
    FAppName = "WebStencils demo";
    FAppEdition = "WebBroker C++";
    FCompanyName = "Embarcadero Inc.";
    FResource = ""; // Set empty for WebBroker - This is needed to share the same templates between WebBroker and RadServer
    FIsRadServer = false;
#ifdef _DEBUG
    FDebugMode = true;
#else
    FDebugMode = false;
#endif
}

//---------------------------------------------------------------------------

__fastcall TMainWebModule::TMainWebModule(TComponent* Owner)
    : TWebModule(Owner)
{
    Logger->Info("WebStencils demo module constructor called");
}
//---------------------------------------------------------------------------

__fastcall TMainWebModule::~TMainWebModule()
{
    Logger->Info("Shutting down WebStencils demo module...");
    // Ensure components are disconnected/inactive before controllers are destroyed
    if (Customers && Customers->Active)
    {
        Customers->Active = false;
    }
    if (Countries && Countries->Active)
    {
        Countries->Active = false;
    }
    // Controllers and FEnvironmentSettings are destroyed automatically by unique_ptr
    Logger->Info("WebStencils demo module shutdown complete");
}
//----------------------------------------------------------------------------

void TMainWebModule::InitControllers()
{
    Logger->Info("Initializing controllers...");
    FTasksController = std::make_unique<TTasksController>(WebStencilsEngine);
    FCustomersController = std::make_unique<TCustomersController>(WebStencilsEngine, Customers);
    FCodeExamples = std::make_unique<TCodeExamples>(WebStencilsEngine);
    Logger->Info("Controllers initialized successfully");
}

void TMainWebModule::InitRequiredData()
{
    try {
        // extract the binary location and point to the resources folder where the database and WebStencils templates are
        FResourcesPath = System::Ioutils::TPath::Combine(System::Ioutils::TPath::GetDirectoryName(GetModuleName(0)), "../../../resources");

        // Normalize the path to handle potential variations
        FResourcesPath = System::Ioutils::TPath::GetFullPath(FResourcesPath);

        WebStencilsEngine->RootDirectory = System::Ioutils::TPath::Combine(FResourcesPath, "html");
        WebFileDispatcher->RootDirectory = WebStencilsEngine->RootDirectory;

        String dbPath = System::Ioutils::TPath::Combine(FResourcesPath, "data/database.sqlite3");

        // Ensure the Connection component is assigned and configured in the DFM
        if (Connection)
        {
            Connection->Params->Database = dbPath;
        }
        else
        {
            printf("Warning: TFDConnection component 'Connection' is not assigned.\n");
        }

        // Initialize environment settings object
        FEnvironmentSettings = std::make_unique<TEnvironmentSettings>();
        // Add the settings object instance to the engine
        // Note: .get() passes the raw pointer, ownership remains with unique_ptr
        WebStencilsEngine->AddVar("env", FEnvironmentSettings.get());

        // Controllers will handle providing data to templates.
		WebStencilsEngine->AddVar("customers", Customers, false);
        WebStencilsEngine->AddVar("countries", Countries, false);
        
        // Initialize Countries query
        if (Countries)
        {
            Countries->Open();
        }
        // --- End of DB Path Setting ---

    }
    catch (Exception& E) {
        printf("Error in InitRequiredData: %s\n", AnsiString(E.Message).c_str());
        throw; // Re-throw the exception
    }
}

void TMainWebModule::DefineRoutes()
{
    Logger->Info("Defining application routes...");
    std::vector<TRoute> routes = {
        // Task routes (protected)
        TRoute(mtDelete, "/tasks", FTasksController->DeleteTask),
        TRoute(mtPost, "/tasks/add", FTasksController->CreateTask),
        TRoute(mtGet, "/tasks/edit", FTasksController->GetEditTask),
        TRoute(mtPut, "/tasks/toggleCompleted", FTasksController->ToggleCompletedTask),
        TRoute(mtPut, "/tasks", FTasksController->EditTask),
        // Customer routes (admin only)
        TRoute(mtGet, "/bigtable", FCustomersController->GetAllCustomers),
        TRoute(mtGet, "/customers", FCustomersController->GetCustomers),
        TRoute(mtGet, "/customers/add", FCustomersController->GetAddCustomer),
        TRoute(mtPost, "/customers/create", FCustomersController->CreateCustomer),
        TRoute(mtGet, "/customers/edit", FCustomersController->GetEditCustomer),
        TRoute(mtPost, "/customers/update", FCustomersController->UpdateCustomer),
        TRoute(mtPost, "/customers/delete", FCustomersController->DeleteCustomer),
        // System routes
        TRoute(mtGet, "/health", WebModule1ActHealthAction)
    };

    TWebModuleHelper::AddRoutes(this, routes);
    Logger->Info("Application routes defined successfully");
}

//---------------------------------------------------------------------------

void __fastcall TMainWebModule::WebModuleCreate(TObject *Sender)
{
    Logger->Info("Initializing WebStencils demo module in OnCreate event...");
    InitControllers();
    InitRequiredData();
    DefineRoutes();
    Logger->Info("WebStencils demo module initialized successfully");
}


//---------------------------------------------------------------------------

void __fastcall TMainWebModule::WebSessionManagerCreated(TCustomWebSessionManager *Sender,
          TWebRequest *Request, TWebSession *Session)
{
    Logger->Info(String("New session created: ") + Session->Id);
    Logger->Info(String("Request Path: ") + Request->PathInfo);
    Logger->Info(String("Request Method: ") + Request->Method);

    // Add session creation timestamp for demo purposes
    Session->DataVars->Values["created"] = FormatDateTime("yyyy-mm-dd hh:nn:ss", Now());
    TFlashMessageManager::EnsureMessageProvider(Session);

    if (Session->User)
        Logger->Info(String("Session created for authenticated user: ") + Session->User->UserName);
    else
        Logger->Info("Session created for anonymous user");
}


//---------------------------------------------------------------------------

void __fastcall TMainWebModule::WebFormsAuthenticatorAuthenticate(TCustomWebAuthenticator *Sender,
          TWebRequest *Request, const UnicodeString UserName,
          const UnicodeString Password, UnicodeString &Roles,
          bool &Success)
{
    Logger->Info(String("Authentication attempt for user: ") + UserName);

    // Demo hardcoded credentials
    Success = false;
    Roles = "";

    if (SameText(UserName, "demo") && SameText(Password, "demo123"))
    {
        Success = true;
        Roles = "user";
    }
    else if (SameText(UserName, "admin") && SameText(Password, "admin123"))
    {
        Success = true;
        Roles = "admin";
    }

    if (Success)
        Logger->Info(String("User ") + UserName + " authenticated successfully with role: " + Roles);
    else
        Logger->Info(String("Authentication failed for user: ") + UserName);
}

//---------------------------------------------------------------------------

void __fastcall TMainWebModule::WebModuleAfterDispatch(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
    // Message clearing - only when not redirecting
    bool IsRedirect = (Response->StatusCode >= 300) && (Response->StatusCode < 400);
    if (!IsRedirect && Request->Session)
        TFlashMessageManager::ClearMessages(Request->Session);

}

//---------------------------------------------------------------------------
void __fastcall TMainWebModule::WebModule1ActHealthAction(TObject *Sender, TWebRequest *Request,
    TWebResponse *Response, bool &Handled)
{
    Response->ContentType = "application/json";
    
    String HealthData = System::Sysutils::Format(
        "{\n"
        "  \"status\": \"healthy\",\n"
        "  \"timestamp\": \"%s\",\n"
        "  \"uptime\": \"%s\",\n"
        "  \"version\": \"%s\",\n"
        "  \"environment\": \"%s\",\n"
        "  \"container\": false,\n"
        "  \"resources_path\": \"%s\",\n"
        "  \"database_path\": \"%s\"\n"
        "}",
        ARRAYOFCONST((
            FormatDateTime("yyyy-mm-dd\"T\"hh:nn:ss.zzz\"Z\"", TTimeZone::Local->ToUniversalTime(Now())),
            TimeToStr(Now()),
#ifdef _WIN64
            "Windows x64",
#else
            "Windows x32", 
#endif
            "C++Builder WebBroker",
            FResourcesPath,
            Connection->Params->Database
        ))
    );
    
    Response->Content = HealthData;
    Handled = true;
}
//---------------------------------------------------------------------------

void __fastcall TMainWebModule::WebStencilsEngineValue(TObject *Sender, const UnicodeString AObjectName,
          const UnicodeString APropName, UnicodeString &AValue,
          bool &AHandled)
{
    // env object is now handled by RTTI via AddVar

    // Handle dynamic system information
    if (SameText(AObjectName, "system"))
    {
        AHandled = true; // Handle all system properties here
        if (SameText(APropName, "timestamp"))
        {
            AValue = FormatDateTime("yyyy-mm-dd hh:nn:ss", Now());
        }
        else if (SameText(APropName, "year"))
        {
            AValue = FormatDateTime("yyyy", Now());
        }
        else
        {
            AValue = System::Sysutils::Format("SYSTEM_%s_NOT_FOUND", ARRAYOFCONST((APropName.UpperCase())));
        }
    }
}
//---------------------------------------------------------------------------

