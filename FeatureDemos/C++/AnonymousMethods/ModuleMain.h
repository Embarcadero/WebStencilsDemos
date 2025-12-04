
//---------------------------------------------------------------------------
#ifndef ModuleMainH
#define ModuleMainH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <System.Generics.Collections.hpp>
#include <memory>
#include <functional>

//---------------------------------------------------------------------------
// Helper class to wrap C++ lambdas for use with TWebStencilsLookupFunc interface
// This allows us to use C++ lambdas similar to Delphi anonymous methods
//---------------------------------------------------------------------------
class TWebStencilsLookupFuncWrapper : public TInterfacedObject, public TWebStencilsLookupFunc
{
private:
  std::function<bool(TWebStencilsDataVar*, const System::UnicodeString&, System::UnicodeString&)> FFunc;

public:
  // Constructor that accepts a lambda or function object
  template<typename Func>
  __fastcall TWebStencilsLookupFuncWrapper(Func&& func) : FFunc(std::forward<Func>(func)) {}
  
  // Implement IInterface methods explicitly to resolve multiple inheritance ambiguity
  // These delegate to TInterfacedObject's implementation
  HRESULT __stdcall QueryInterface(const GUID& riid, void** ppvObj) { return TInterfacedObject::QueryInterface(riid, ppvObj); }
  ULONG __stdcall AddRef() { return TInterfacedObject::_AddRef(); }
  ULONG __stdcall Release() { return TInterfacedObject::_Release(); }
  
  // Implement the interface method - exact signature from TWebStencilsLookupFunc interface
  bool __fastcall Invoke(TWebStencilsDataVar* AVar, const System::UnicodeString APropName, System::UnicodeString &AValue) override;
};

//---------------------------------------------------------------------------
// Helper function to create a TWebStencilsLookupFunc from a C++ lambda
// This allows us to use C++ lambdas similar to Delphi anonymous methods
//---------------------------------------------------------------------------
template<typename Func>
_di_TWebStencilsLookupFunc MakeLookupFunc(Func&& func)
{
  return _di_TWebStencilsLookupFunc(new TWebStencilsLookupFuncWrapper(std::forward<Func>(func)));
}

//---------------------------------------------------------------------------
class TWebModuleMain : public TWebModule
{
__published:
  TWebStencilsProcessor *WSProcessor;
  void __fastcall WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
      TWebResponse *Response, bool &Handled);
private:
public:
  __fastcall TWebModuleMain(TComponent* Owner);
  __fastcall virtual ~TWebModuleMain();
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModuleMain *WebModuleMain;
//---------------------------------------------------------------------------
#endif
