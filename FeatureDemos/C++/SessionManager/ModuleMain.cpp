
//---------------------------------------------------------------------------
#include "ModuleMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(TWebModuleMain);
//---------------------------------------------------------------------------

__fastcall TWebModuleMain::TWebModuleMain(TComponent* Owner)
  : TWebModule(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleCreate(TObject *Sender)
{
  String BinaryPath;
  String EnvResourcesPath;
  
  // Try to get paths from environment variables
  EnvResourcesPath = GetEnvironmentVariable("APP_RESOURCES_PATH");
  
  // Set the path for resources based on the platform and build configuration
  BinaryPath = System::Ioutils::TPath::GetDirectoryName(ParamStr(0));
#ifdef _WIN32
  if (EnvResourcesPath == "")
    FResourcesPath = System::Ioutils::TPath::Combine(BinaryPath, "..\\..");
  else
    FResourcesPath = EnvResourcesPath;
#else
  if (EnvResourcesPath == "")
    FResourcesPath = BinaryPath;
  else
    FResourcesPath = EnvResourcesPath;
#endif
  // TWebStencilsEngine provides template routing and rendering
  // It works with WebBroker authentication/authorization components
  WSEngine->RootDirectory = System::Ioutils::TPath::Combine(FResourcesPath, "templates");
  WebFileDispatcher->RootDirectory = WSEngine->RootDirectory;
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebFormsAuthenticatorAuthenticate(TCustomWebAuthenticator *Sender,
          TWebRequest *Request, const UnicodeString UserName,
          const UnicodeString Password, UnicodeString &Roles, bool &Success)
{
  // Demo hardcoded credentials
  // In production, authenticate against a database or identity provider
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
}
//---------------------------------------------------------------------------

