//---------------------------------------------------------------------------
#ifndef ControllersBaseH
#define ControllersBaseH
//---------------------------------------------------------------------------

#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Generics.Collections.hpp>
#include <System.DateUtils.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <Data.DB.hpp>
#include <memory>
#include <vector>

#include "HelpersMessages.h"
#include "UtilsFormSession.h"

//---------------------------------------------------------------------------
typedef System::Generics::Collections::TDictionary__2<String, String> TStringDictionary;

//---------------------------------------------------------------------------
class TFieldErrorManager : public TObject
{
private:
    std::unique_ptr<TStringDictionary> FErrors;
    
    int GetErrorCount();

public:
    __fastcall TFieldErrorManager();
    __fastcall virtual ~TFieldErrorManager();
    
    void AddError(const String& AFieldName, const String& AMessage);
    void Clear();
    // Expose underlying dictionary for session persistence
    TStringDictionary* GetErrorsDict() { return FErrors.get(); }

__published:
    String GetError(const String& AFieldName);
    bool HasError(const String& AFieldName);
    __property int ErrorCount = {read=GetErrorCount};
};

//---------------------------------------------------------------------------
class TBaseController : public TObject
{
protected:
    Web::Stencils::TWebStencilsEngine* FWebStencilsEngine;
    Web::Stencils::TWebStencilsProcessor* FWebStencilsProcessor;
    String FControllerName;
    std::unique_ptr<TFieldErrorManager> FFieldErrorManager;
    
    String RenderTemplate(const String& ATemplatePath, TWebRequest* ARequest = nullptr);
    void Redirect(TWebResponse* AResponse, const String& ALocation);
    TWebSession* GetCurrentSession(TWebRequest* ARequest);
    
    // Global message methods
    void AddSuccessMessage(TWebRequest* ARequest, const String& AMessage);
    void AddWarningMessage(TWebRequest* ARequest, const String& AMessage);
    void AddErrorMessage(TWebRequest* ARequest, const String& AMessage);
    void AddInfoMessage(TWebRequest* ARequest, const String& AMessage);
    
    // Form validation helpers
    void InitializeValidationErrors();
    void ValidateRequired(const String& AFieldName, const String& AValue, const String& ADisplayName = "");
    void ValidateEmail(const String& AFieldName, const String& AValue);
    void ValidateLength(const String& AFieldName, const String& AValue, int AMinLength, int AMaxLength = 0);
    bool HasValidationErrors();
    void ClearValidationErrors();
    
    // Form session management
    void StoreFormDataInSession(TWebRequest* ARequest, const String& AFormName);
    std::unique_ptr<TStringDictionary> GetFormDataFromSession(TWebRequest* ARequest, const String& AFormName);
    void ClearFormSession(TWebRequest* ARequest, const String& AFormName);

    // Form data helpers (to mirror Delphi logic)
    void AssignFieldValue(TField* Field, const String& Value);
    void RestoreFormDataToDataset(TDataSet* DataSet, TWebRequest* ARequest, const String& FormName);
    void PopulateDatasetFromRequest(TDataSet* ADataset, TWebRequest* ARequest, const std::vector<String>& ASkipFields = {});
    void ClearFormSessionAfterProcessing(TWebRequest* ARequest, const String& FormName);

public:
    __fastcall TBaseController(Web::Stencils::TWebStencilsEngine* AWebStencilsEngine, const String& AControllerName);
    __fastcall virtual ~TBaseController();
    
    __property String ControllerName = {read=FControllerName};
    __property TFieldErrorManager* FieldErrors = {read=FFieldErrorManager};
};

//---------------------------------------------------------------------------
#endif