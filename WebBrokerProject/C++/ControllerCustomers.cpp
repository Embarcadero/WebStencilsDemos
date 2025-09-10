//---------------------------------------------------------------------------
#pragma hdrstop

#include "ControllerCustomers.h"
#include <Web.Stencils.hpp>
#include <System.IOUtils.hpp>
#include <System.SysUtils.hpp>
#include <System.NetEncoding.hpp>
#include "UtilsLogger.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

TCustomersController::TCustomersController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, TFDQuery* ACustomers)
    : TBaseController(AWebStencilsEngine, "customers")
{
    FCustomers = ACustomers;
    // Initialize customer search with the desired fields
    std::vector<String> searchFields = {"FIRST_NAME", "LAST_NAME", "EMAIL", "COMPANY", "PHONE", "CITY"};
    FCustomerSearch = std::make_unique<TBaseSearch>(searchFields);
}

TCustomersController::~TCustomersController()
{
    // FCustomers is owned externally
}

String TCustomersController::RenderCustomerTemplate(const String& ATemplate, TWebRequest* ARequest,
                                                   TPaginationParams* APaginationParams,
                                                   TSearchParams* ASearchParams)
{
    if (APaginationParams != nullptr)
    {
        FWebStencilsProcessor->AddVar("customersPagination", APaginationParams, false);
    }
    if (ASearchParams != nullptr)
    {
        FWebStencilsProcessor->AddVar("customersSearchParams", ASearchParams, false);
    }

    String result = RenderTemplate(String("customers/") + ATemplate + ".html", ARequest);

    if (APaginationParams != nullptr)
    {
        FWebStencilsProcessor->DataVars->Remove("customersPagination");
    }
    if (ASearchParams != nullptr)
    {
        FWebStencilsProcessor->DataVars->Remove("customersSearchParams");
    }

    return result;
}

void TCustomersController::ResetQuery()
{
    if (FCustomers && FCustomers->Active)
    {
        FCustomers->Close();
    }
    FCustomers->SQL->Clear();
    FCustomers->SQL->Add("SELECT * FROM customers");
    FCustomers->Params->Clear();
}

void TCustomersController::ApplySearchToQuery(TSearchParams* ASearchParams)
{
    if (!ASearchParams || !ASearchParams->HasSearch)
    {
        return;
    }

    String WhereClause = ASearchParams->GetSearchSQL();
    if (!WhereClause.IsEmpty())
    {
        FCustomers->SQL->Add(WhereClause);
        // Bind parameters using helper for consistency
        ASearchParams->SetSearchParameters(FCustomers);
    }
}

void TCustomersController::ValidateCustomerForm(TWebRequest* ARequest)
{
    ClearValidationErrors();
    const String FirstName = ARequest->ContentFields->Values["first_name"];
    const String LastName = ARequest->ContentFields->Values["last_name"];
    const String Email = ARequest->ContentFields->Values["email"];

    ValidateRequired("first_name", FirstName, "First name");
    ValidateRequired("last_name", LastName, "Last name");
    ValidateRequired("email", Email, "Email");

    // Basic email validation
    ValidateEmail("email", Email);

    // Max lengths mirroring Delphi
    if (!FirstName.IsEmpty())
    {
        ValidateLength("first_name", FirstName, 0, 50);
    }
    if (!LastName.IsEmpty())
    {
        ValidateLength("last_name", LastName, 0, 50);
    }
    if (!Email.IsEmpty())
    {
        ValidateLength("email", Email, 0, 100);
    }
}

void TCustomersController::GetCustomers(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    std::unique_ptr<TPaginationParams> LPaginationParams(new TPaginationParams(Request, "customers"));
    std::unique_ptr<TSearchParams> LSearchParams = FCustomerSearch->CreateSearchParams(Request);

    try
    {
        ResetQuery();
        ApplySearchToQuery(LSearchParams.get());

        // Apply pagination
        FDQueryHelpers::SetPageSize(FCustomers, LPaginationParams->PageSize);
        FDQueryHelpers::SetPageNumber(FCustomers, LPaginationParams->PageNumber);
        FDQueryHelpers::ApplyPagination(FCustomers);

        LPaginationParams->TotalPages = FDQueryHelpers::GetTotalPages(FCustomers);

        Response->Content = RenderCustomerTemplate("index", Request, LPaginationParams.get(), LSearchParams.get());
        Handled = true;
    }
    catch (Exception &E)
    {
        Response->StatusCode = 500;
        Response->Content = String("Error retrieving customers: ") + E.Message;
        Handled = true;
    }
}

void TCustomersController::GetAllCustomers(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    try
    {
        ResetQuery();
        FDQueryHelpers::CancelPagination(FCustomers);
        Response->Content = RenderCustomerTemplate("bigtable", Request);
        Handled = true;
    }
    catch (Exception &E)
    {
        Response->StatusCode = 500;
        Response->Content = String("Error retrieving all customers: ") + E.Message;
        Handled = true;
    }
}

void TCustomersController::GetAddCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    __try
    {
        // Prepare dataset to serve as model metadata and blank record
        FDQueryHelpers::CancelPagination(FCustomers);
        FCustomers->Active = true;
        FCustomers->Append();

        // Restore previously saved form data (after validation errors)
        RestoreFormDataToDataset(FCustomers, Request, "customer_add");

        Response->Content = RenderCustomerTemplate("add", Request);

        // Clear the saved form data AFTER rendering so template can access it
        ClearFormSessionAfterProcessing(Request, "customer_add");
        Handled = true;
    }
    __finally
    {
        if (FCustomers->Active)
        {
            FCustomers->Cancel();
            FCustomers->Active = false;
        }
    }
}

void TCustomersController::CreateCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    // 1. Validate
    ValidateCustomerForm(Request);
    if (HasValidationErrors())
    {
        // Persist input and field-level errors for re-display
        StoreFormDataInSession(Request, "customer_add");
        AddErrorMessage(Request, "Please correct the errors below");
        Redirect(Response, "/customers/add");
        Handled = true;
        return;
    }

    // 2. Save
    FCustomers->Active = true;
    try
    {
        FDQueryHelpers::CancelPagination(FCustomers);
        FCustomers->Append();

        // Populate dataset from form data (skip ID field as it's auto-generated)
        std::vector<String> skipFields = {"id"};
        PopulateDatasetFromRequest(FCustomers, Request, skipFields);

        FCustomers->Post();
        FCustomers->Close();

        ClearFormSession(Request, "customer_add");
        AddSuccessMessage(Request, "Customer created successfully");
        Redirect(Response, "/customers");
    }
    catch (Exception &E)
    {
        FCustomers->Cancel();
        StoreFormDataInSession(Request, "customer_add");
        AddErrorMessage(Request, String("Error creating customer: ") + E.Message);
        Redirect(Response, "/customers/add");
    }

    Handled = true;
}

void TCustomersController::GetEditCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    String IdStr = Request->QueryFields->Values["id"];
    if (IdStr.IsEmpty())
    {
        AddErrorMessage(Request, "Customer ID is required");
        Redirect(Response, "/customers");
        Handled = true;
        return;
    }

    FCustomers->Active = true;
    try
    {
        FDQueryHelpers::CancelPagination(FCustomers);
        if (!FCustomers->Locate("ID", IdStr, TLocateOptions()))
        {
            AddErrorMessage(Request, "Customer not found");
            Redirect(Response, "/customers");
            Handled = true;
            return;
        }

        FCustomers->Edit();

        // Restore form data captured on previous validation errors
        RestoreFormDataToDataset(FCustomers, Request, "customer_edit");

        Response->Content = RenderCustomerTemplate("edit", Request);
        // Clear session data AFTER rendering
        ClearFormSessionAfterProcessing(Request, "customer_edit");
        Handled = true;
    }
    __finally
    {
        FCustomers->Cancel();
        FCustomers->Active = false;
    }
}

void TCustomersController::UpdateCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    String IdStr = Request->ContentFields->Values["id"];
    if (IdStr.IsEmpty())
    {
        AddErrorMessage(Request, "Customer ID is required");
        Redirect(Response, "/customers");
        Handled = true;
        return;
    }

    // Validate
    ValidateCustomerForm(Request);
    if (HasValidationErrors())
    {
        // Persist input and field-level errors for re-display
        StoreFormDataInSession(Request, "customer_edit");
        AddErrorMessage(Request, "Please correct the errors below");
        Redirect(Response, String("/customers/edit?id=") + IdStr);
        Handled = true;
        return;
    }

    // Save
    FCustomers->Active = true;
    try
    {
        FDQueryHelpers::CancelPagination(FCustomers);
        if (!FCustomers->Locate("ID", IdStr, TLocateOptions()))
        {
            AddErrorMessage(Request, "Customer not found");
            Redirect(Response, "/customers");
            Handled = true;
            return;
        }

        FCustomers->Edit();

        // Populate dataset from form data (skip ID field to prevent accidental modification)
        std::vector<String> skipFields = {"id"};
        PopulateDatasetFromRequest(FCustomers, Request, skipFields);

        // Post and check RowsAffected to ensure DB actually updated
        FCustomers->Post();
        const int Rows = FCustomers->RowsAffected;
        if (Rows <= 0)
        {
            throw Exception("Update failed or did not affect any rows");
        }
        FCustomers->Close();

        ClearFormSession(Request, "customer_edit");
        AddSuccessMessage(Request, "Customer updated successfully");

        String LRedirectUrl = Request->GetFieldByName("HTTP_REFERER");
        if (LRedirectUrl.IsEmpty())
            LRedirectUrl = "/customers";
        Redirect(Response, LRedirectUrl);
    }
    catch (Exception &E)
    {
        FCustomers->Cancel();
        StoreFormDataInSession(Request, "customer_edit");
        AddErrorMessage(Request, String("Error updating customer: ") + E.Message);
        Redirect(Response, String("/customers/edit?id=") + IdStr);
    }

    Handled = true;
}

void TCustomersController::DeleteCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled)
{
    String IdStr = Request->ContentFields->Values["id"];
    if (IdStr.IsEmpty())
    {
        AddErrorMessage(Request, "Customer ID is required");
        Redirect(Response, "/customers");
        Handled = true;
        return;
    }

    try
    {
        FCustomers->Active = true;
        FDQueryHelpers::CancelPagination(FCustomers);
        if (!FCustomers->Locate("ID", IdStr, TLocateOptions()))
        {
            AddErrorMessage(Request, "Customer not found");
            Redirect(Response, "/customers");
            Handled = true;
            return;
        }

        FCustomers->Delete();
        FCustomers->Close();

        AddSuccessMessage(Request, "Customer deleted successfully");
        Redirect(Response, "/customers");
    }
    catch (Exception &E)
    {
        AddErrorMessage(Request, String("Error deleting customer: ") + E.Message);
        Redirect(Response, "/customers");
    }

    Handled = true;
}