
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

void __fastcall TWebModuleMain::WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  // home.html uses MainLayout which uses BaseLayout
  // Demonstrates nested @ExtraHeader: BaseLayout -> MainLayout -> page content
  WSProcessor->InputFileName = "..\\..\\templates\\home.html";
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleMainWebActionItem1Action(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  // link1.html adds its own @ExtraHeader, creating a three-level nesting
  WSProcessor->InputFileName = "..\\..\\templates\\link1.html";
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleMainWebActionItem2Action(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  WSProcessor->InputFileName = "..\\..\\templates\\link2.html";
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------

