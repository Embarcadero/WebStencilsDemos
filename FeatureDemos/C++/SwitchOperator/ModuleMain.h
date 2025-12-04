
//---------------------------------------------------------------------------
#ifndef ModuleMainH
#define ModuleMainH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <memory>

//---------------------------------------------------------------------------
// TStatusObject class - returns random status values
//---------------------------------------------------------------------------
class TStatusObject : public System::Classes::TPersistent
{
private:
	System::UnicodeString RandomStatus();

public:
	__published:
		__property System::UnicodeString Name = {read=RandomStatus};
};

//---------------------------------------------------------------------------
class TWebModuleMain : public TWebModule
{
__published:	// IDE-managed Components
	TWebStencilsProcessor *WSProcessor;
	void __fastcall WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
	void __fastcall WebModuleCreate(TObject *Sender);
	void __fastcall WebModuleDestroy(TObject *Sender);
private:	// User declarations
	std::unique_ptr<TStatusObject> FStatus;
public:		// User declarations
	__fastcall TWebModuleMain(TComponent* Owner);
	__fastcall virtual ~TWebModuleMain();
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModuleMain *WebModuleMain;
//---------------------------------------------------------------------------
#endif

