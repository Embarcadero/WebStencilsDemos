//---------------------------------------------------------------------------
#pragma hdrstop

#include "WebResource.h"
#include <memory>
#include <System.IOUtils.hpp>
#include <System.DateUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.JSON.hpp>
#include "CodeExamplesU.h"
#include "ControllerTasks.h"
#include "ControllerCustomers.h"
#include "ModelPaginationParams.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "System.Classes.TPersistent"
#pragma resource "*.dfm"
//---------------------------------------------------------------------------

TEnvironmentSettings::TEnvironmentSettings()
{
    FAppVersion = "1.0.0";
    FAppName = "WebStencils demo";
    FAppEdition = "RAD Server C++";
    FCompanyName = "Embarcadero Inc.";
    FResource = "/web";
    FIsRadServer = true;
#ifdef _DEBUG
    FDebugMode = true;
#else
    FDebugMode = false;
#endif
}

__fastcall TWebstencilsResource1::TWebstencilsResource1(TComponent* Owner)
    : TDataModule(Owner)
{
    //////////////////////
    // Replace the constant LProjectPath with the absolute path to the resources folder of the reporitory
    //////////////////////
    const String LProjectPath = "C:\\replace\\with\\your\\absolute\\path\\to\\the\\resources\\folder";

    FDConnection->Params->Database = TPath::Combine(LProjectPath, "data\\tasks.ib");
    html->PathTemplate = TPath::Combine(LProjectPath, "html\\{filename}");
    css->PathTemplate = TPath::Combine(LProjectPath, "html\\static\\css\\{filename}");
    js->PathTemplate = TPath::Combine(LProjectPath, "html\\static\\js\\{filename}");
    img->PathTemplate = TPath::Combine(LProjectPath, "html\\static\\img\\{filename}");
    WebStencilsProcessor->PathTemplate = TPath::Combine(LProjectPath, "html");
    WebStencilsEngine1->RootDirectory = TPath::Combine(LProjectPath, "html");
    WebStencilsEngine1->AddVar("customers", customers, False);
    FCodeExamples = new TCodeExamples(WebStencilsEngine1);
    FTasksController = new TTasksController(WebStencilsEngine1, FDConnection);
    FCustomersController = new TCustomersController(WebStencilsEngine1, customers);
    FEnvironmentSettings = new TEnvironmentSettings();
    WebStencilsEngine1->AddVar("env", FEnvironmentSettings);
    WebStencilsEngine1->OnValue = WebStencilsEngine1Value;
}

void __fastcall TWebstencilsResource1::WebStencilsEngine1Value(TObject* Sender, const String AObjectName,
            const String APropName, String &AReplaceText, bool &AHandled)
{
    if (SameText(AObjectName, "system"))
    {
        if (SameText(APropName, "timestamp"))
        {
            AReplaceText = FormatDateTime("yyyy-mm-dd hh:nn:ss", Now());
        }
        else if (SameText(APropName, "year"))
        {
            AReplaceText = FormatDateTime("yyyy", Now());
        }
        else
        {
            AReplaceText = System::Sysutils::Format("SYSTEM_%s_NOT_FOUND", ARRAYOFCONST((APropName.UpperCase())));
        }
        AHandled = true;
    }
}

void TWebstencilsResource1::GetHome(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    String LHTMLContent;
    WebStencilsProcessor->InputFileName = TPath::Combine(WebStencilsProcessor->PathTemplate, "home.html");
    LHTMLContent = WebStencilsProcessor->Content();
    AResponse->Body->SetString(LHTMLContent);
}

void TWebstencilsResource1::DeleteTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    FTasksController->DeleteTask(ARequest, AResponse);
}

void TWebstencilsResource1::PostTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    FTasksController->CreateTask(ARequest, AResponse);
}

void TWebstencilsResource1::GetTasksEdit(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    FTasksController->GetEditTask(ARequest, AResponse);
}

void TWebstencilsResource1::PutTaskToggleCompleted(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    FTasksController->ToggleCompletedTask(ARequest, AResponse);
}

void TWebstencilsResource1::PutTask(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    FTasksController->EditTask(ARequest, AResponse);
}

void TWebstencilsResource1::GetPaginatedCustomers(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    if (FCustomersController)
    {
        FCustomersController->GetCustomers(ARequest, AResponse);
    }
    else
    {
        AResponse->RaiseError(500, "Customers controller not initialized.", "Internal Server Error");
    }
}

void TWebstencilsResource1::GetAllCustomersEndpoint(TEndpointContext* AContext, TEndpointRequest* ARequest, TEndpointResponse* AResponse)
{
    if (FCustomersController)
    {
        FCustomersController->GetAllCustomers(ARequest, AResponse);
    }
    else
    {
        AResponse->RaiseError(500, "Customers controller not initialized.", "Internal Server Error");
    }
}

static void Register()
{
    std::unique_ptr<TEMSResourceAttributes> attributes(new TEMSResourceAttributes());
    attributes->ResourceName = "web";

    attributes->ResourceSuffix["html.Get"] = "/{filename}";
    attributes->ResourceSuffix["css.Get"] = "/static/css/{filename}";
    attributes->ResourceSuffix["js.Get"] = "/static/js/{filename}";
    attributes->ResourceSuffix["img.Get"] = "/static/img/{filename}";

    attributes->ResourceSuffix["GetHome"] = "./";

    attributes->ResourceSuffix["DeleteTask"] = "/tasks";
    attributes->ResourceSuffix["PostTask"] = "/tasks/add";
    attributes->ResourceSuffix["GetTasksEdit"] = "/tasks/edit";
    attributes->ResourceSuffix["PutTaskToggleCompleted"] = "/tasks/toggleCompleted";
    attributes->ResourceSuffix["PutTask"] = "/tasks";

    attributes->ResourceSuffix["GetPaginatedCustomers"] = "/pagination";
    attributes->ResourceSuffix["GetAllCustomersEndpoint"] = "/bigtable";


    RegisterResource(__typeinfo(TWebstencilsResource1), attributes.release());
}

#pragma startup Register 32
