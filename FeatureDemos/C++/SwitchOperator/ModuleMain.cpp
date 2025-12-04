
//---------------------------------------------------------------------------
#include "ModuleMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"

TComponentClass WebModuleClass = __classid(TWebModuleMain);
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
// TStatusObject implementation
//---------------------------------------------------------------------------
System::UnicodeString TStatusObject::RandomStatus()
{
  const System::UnicodeString Statuses[5] = {"a", "i", "p", "s", "m"};
  return Statuses[Random(5)];
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
  // FStatus will be automatically destroyed by unique_ptr
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleCreate(TObject *Sender)
{
  // Add a status object that returns random status values
  // The template uses @switch to render different badges based on status.Name
  FStatus = std::make_unique<TStatusObject>();
  WSProcessor->AddVar("status", FStatus.get(), false);
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleDestroy(TObject *Sender)
{
  // Clean up - unique_ptr will handle destruction automatically
  // Remove the var from processor if needed
  if (WSProcessor && FStatus.get())
  {
    WSProcessor->DataVars->Remove("status");
  }
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------

