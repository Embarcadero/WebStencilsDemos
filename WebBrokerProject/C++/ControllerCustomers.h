#ifndef ControllerCustomersH
#define ControllerCustomersH

#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IOUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <Data.DB.hpp>
#include <memory>

#include "ControllersBase.h"
#include "UtilsPaginationParams.h"
#include "FDQueryHelpers.h"
#include "UtilsSearch.h"

class TCustomersController : public TBaseController
{
private:
    TFDQuery* FCustomers;
    std::unique_ptr<TBaseSearch> FCustomerSearch;

    String RenderCustomerTemplate(const String& ATemplate, TWebRequest* ARequest,
                                  TPaginationParams* APaginationParams = nullptr,
                                  TSearchParams* ASearchParams = nullptr);
    void ResetQuery();
    void ApplySearchToQuery(TSearchParams* ASearchParams);
    void ValidateCustomerForm(TWebRequest* ARequest);

public:
    void GetCustomers(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void GetAllCustomers(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);

    void GetAddCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void CreateCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void GetEditCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void UpdateCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);
    void DeleteCustomer(TObject* Sender, TWebRequest* Request, TWebResponse* Response, bool& Handled);

    TCustomersController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, TFDQuery* ACustomers);
    virtual ~TCustomersController();
};

#endif