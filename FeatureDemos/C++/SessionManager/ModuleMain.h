
//---------------------------------------------------------------------------
#ifndef ModuleMainH
#define ModuleMainH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <System.IOUtils.hpp>

//---------------------------------------------------------------------------
class TWebModuleMain : public TWebModule
{
__published:  // IDE-managed Components
  TWebSessionManager *WebSessionManager;
  TWebFormsAuthenticator *WebFormsAuthenticator;
  TWebAuthorizer *WebAuthorizer;
  TWebStencilsEngine *WSEngine;
  TWebFileDispatcher *WebFileDispatcher;
  void __fastcall WebModuleCreate(TObject *Sender);
  void __fastcall WebFormsAuthenticatorAuthenticate(TCustomWebAuthenticator *Sender,
          TWebRequest *Request, const UnicodeString UserName,
          const UnicodeString Password, UnicodeString &Roles, bool &Success);
private:  // User declarations
  String FResourcesPath;
public:    // User declarations
  __fastcall TWebModuleMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModuleMain *WebModuleMain;
//---------------------------------------------------------------------------
#endif

