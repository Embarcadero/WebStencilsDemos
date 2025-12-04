
//---------------------------------------------------------------------------
#ifndef ModuleMainH
#define ModuleMainH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>

//---------------------------------------------------------------------------
class TWebModuleMain : public TWebModule
{
__published:  // IDE-managed Components
  TWebStencilsProcessor *WSProcessor;
  void __fastcall WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
  void __fastcall WebModuleMainWebActionItem1Action(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
  void __fastcall WebModuleMainWebActionItem2Action(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
private:  // User declarations
public:   // User declarations
  __fastcall TWebModuleMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModuleMain *WebModuleMain;
//---------------------------------------------------------------------------
#endif

