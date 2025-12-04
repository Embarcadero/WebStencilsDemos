
//---------------------------------------------------------------------------
#include "ModuleMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(TWebModuleMain);
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// TWebStencilsLookupFuncWrapper implementation
//---------------------------------------------------------------------------
bool __fastcall TWebStencilsLookupFuncWrapper::Invoke(TWebStencilsDataVar* AVar, const System::UnicodeString APropName, System::UnicodeString &AValue)
{
  return FFunc ? FFunc(AVar, APropName, AValue) : false;
}

//---------------------------------------------------------------------------
// TWebModuleMain implementation
//---------------------------------------------------------------------------
__fastcall TWebModuleMain::TWebModuleMain(TComponent* Owner)
  : TWebModule(Owner)
{
}
//---------------------------------------------------------------------------

__fastcall TWebModuleMain::~TWebModuleMain()
{
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  // Example 1: Using a lambda with an object (TDictionary)
  // This is equivalent to Delphi's env1 with anonymous method + TDictionary
  // Create dictionary for this request (WebStencils will own it when AOwned=true)
  auto* dict = new System::Generics::Collections::TDictionary__2<System::UnicodeString, System::UnicodeString>();
  dict->Add("APP_VERSION", "1.0.0");
  dict->Add("DEBUG_MODE", "True");
  
  // Create a lambda that accesses the dictionary
  // Capture dict by value in the lambda since WebStencils will own it
  auto lookupFunc1 = MakeLookupFunc(
    [dict](TWebStencilsDataVar* AVar, const System::UnicodeString& APropName, System::UnicodeString& AValue) -> bool
    {
      System::UnicodeString UpperPropName = APropName.UpperCase();
      return dict->TryGetValue(UpperPropName, AValue);
    }
  );
  
  WSProcessor->AddVar("env1", dict, true, lookupFunc1);
  
  // Example 2: Fully anonymous lambda without an associated object
  // This is equivalent to Delphi's env2 with fully anonymous method
  auto lookupFunc2 = MakeLookupFunc(
    [](TWebStencilsDataVar* AVar, const System::UnicodeString& APropName, System::UnicodeString& AValue) -> bool
    {
      System::UnicodeString UpperPropName = APropName.UpperCase();
      if (UpperPropName == "APP_VERSION")
      {
        AValue = "1.0.0";
        return true;
      }
      else if (UpperPropName == "DEBUG_MODE")
      {
        AValue = "True";
        return true;
      }
      return false;
    }
  );
  
  WSProcessor->AddVar("env2", nullptr, false, lookupFunc2);
  
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------
