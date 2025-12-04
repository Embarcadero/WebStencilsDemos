
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
  // Configure whitelist to expose FireDAC field metadata properties
  // This allows templates to access field properties like DisplayText, DataType, Required, etc.
  System::DynamicArray<System::UnicodeString> Props;
  Props.Length = 9;
  Props[0] = "DisplayText";
  Props[1] = "Value";
  Props[2] = "DisplayLabel";
  Props[3] = "FieldName";
  Props[4] = "Required";
  Props[5] = "Visible";
  Props[6] = "DataType";
  Props[7] = "Size";
  Props[8] = "IsNull";
  System::DynamicArray<System::UnicodeString> EmptyArray;
  TWebStencilsProcessor::Whitelist->Configure(__classid(TField), Props, EmptyArray, false);
  
  Connection->Params->Database = "..\\..\\database.sqlite3";
  Customers->Active = true;
  
  // Manually add the Customers query to WebStencils (C++ doesn't support attributes)
  WSProcessor->AddVar("customers", Customers, false);
}
//---------------------------------------------------------------------------

void __fastcall TWebModuleMain::WebModuleMainDefaultHandlerAction(TObject *Sender, TWebRequest *Request,
          TWebResponse *Response, bool &Handled)
{
  Customers->Locate("ID", Random(500), TLocateOptions());
  Response->Content = WSProcessor->Content();
}
//---------------------------------------------------------------------------

