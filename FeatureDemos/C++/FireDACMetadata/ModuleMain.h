
//---------------------------------------------------------------------------
#ifndef ModuleMainH
#define ModuleMainH
//---------------------------------------------------------------------------
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <Web.HTTPApp.hpp>
#include <Web.Stencils.hpp>
#include <Data.DB.hpp>
#include <FireDAC.Comp.Client.hpp>
#include <FireDAC.Comp.DataSet.hpp>
#include <FireDAC.DApt.Intf.hpp>
#include <FireDAC.DatS.hpp>
#include <FireDAC.Phys.Intf.hpp>
#include <FireDAC.Stan.Error.hpp>
#include <FireDAC.Stan.Intf.hpp>
#include <FireDAC.Stan.Option.hpp>
#include <FireDAC.Stan.Param.hpp>
#include <FireDAC.DApt.hpp>
#include <FireDAC.Phys.hpp>
#include <FireDAC.Phys.SQLite.hpp>
#include <FireDAC.Phys.SQLiteDef.hpp>
#include <FireDAC.Phys.SQLiteWrapper.Stat.hpp>
#include <FireDAC.Stan.Async.hpp>
#include <FireDAC.Stan.Def.hpp>
#include <FireDAC.Stan.ExprFuncs.hpp>
#include <FireDAC.Stan.Pool.hpp>
#include <FireDAC.UI.Intf.hpp>
#include <FireDAC.VCLUI.Wait.hpp>

//---------------------------------------------------------------------------
class TWebModuleMain : public TWebModule
{
__published:  // IDE-managed Components
  TWebStencilsProcessor *WSProcessor;
  TFDQuery *Customers;
  TFDConnection *Connection;
  TFDAutoIncField *CustomersID;
  TStringField *CustomersCOMPANY;
  TStringField *CustomersFIRST_NAME;
  TStringField *CustomersLAST_NAME;
  TStringField *CustomersGENDER;
  TIntegerField *CustomersAGE;
  TStringField *CustomersPOSTAL_CODE;
  TStringField *CustomersADDRESS;
  TStringField *CustomersCITY;
  TStringField *CustomersCOUNTRY;
  TStringField *CustomersPHONE;
  TStringField *CustomersEMAIL;
  TStringField *CustomersIP_ADDRESS;
  TDateField *CustomersACTIVATION_DATE;
  TBooleanField *CustomersACTIVE;
  TWideMemoField *CustomersCOMMENTS;
  void __fastcall WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled);
  void __fastcall WebModuleCreate(TObject *Sender);
private:  // User declarations
public:    // User declarations
  __fastcall TWebModuleMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TWebModuleMain *WebModuleMain;
//---------------------------------------------------------------------------
#endif

