#include <vcl.h>
#pragma hdrstop

#include "CodeExamplesU.h"
#pragma package(smart_init)

TCodeExample::TCodeExample(const String& AId, const String& ACode) {
    FId = AId;
    FCode = ACode;
}

TCodeExamples::TCodeExamples(TWebStencilsEngine* AWebStencilsEngine) {
    if (!AWebStencilsEngine) {
        throw Exception("WebStencilsEngine cannot be null");
    }
    
    try {
        FItems = new TStringList();
        FItems->OwnsObjects = true;
        FWebStencilsEngine = AWebStencilsEngine;
        
        // Initialize values first
        InitValues();
        
        // Then add them to WebStencils
        for (int i = 0; i < FItems->Count; i++) {
            TCodeExample* example = dynamic_cast<TCodeExample*>(FItems->Objects[i]);
            if (example) {
                try {
                    FWebStencilsEngine->AddVar(FItems->Strings[i], example);
                }
                catch (Exception& E) {
                    // Log the error but continue
                    printf("Error adding code example '%s': %s\n", 
                        AnsiString(FItems->Strings[i]).c_str(), 
                        AnsiString(E.Message).c_str());
                }
            }
        }
    }
    catch (...) {
        // Clean up if something goes wrong
        if (FItems) {
            delete FItems;
            FItems = nullptr;
        }
        throw;
    }
}

TCodeExamples::~TCodeExamples() {
    if (FItems) {
        delete FItems;
        FItems = nullptr;
    }
}

void TCodeExamples::Add(const String& AId, const String& ACode) {
    if (!FItems) {
        throw Exception("TStringList not initialized");
    }
    FItems->AddObject(AId, new TCodeExample(AId, ACode));
}

TCodeExample* TCodeExamples::Find(const String& AId) {
    if (!FItems) {
        return nullptr;
    }
    int index = FItems->IndexOf(AId);
    if (index >= 0) {
        return dynamic_cast<TCodeExample*>(FItems->Objects[index]);
    }
    return nullptr;
}

void TCodeExamples::InitValues() {
    if (!FItems) {
        throw Exception("TStringList not initialized");
    }
    
    Add("codeAtSymbol", "@object.value");
    Add("codeDotNotation",
        "<h2>User Profile</h2>\n"
        "<p>Name: @user.name</p>\n"
        "<p>Email: @user.email</p>");
    Add("codeComments",
        "@* This is a comment and will not appear in the output *@\n"
        "<p>This will appear in the output</p>");
    Add("codeIfAndElse",
        "@if user.isLoggedIn {\n"
        "    <p>Welcome, @user.name!</p>\n"
        "}\n"
        "@else {\n"
        "    <p>Please log in to continue.</p>\n"
        "}");
    Add("codeIfNot",
        "@if not cart.isEmpty {\n"
        "  <p>You have @cart.itemCount items in your cart.</p>\n"
        "}\n"
        "@else {\n"
        "  <p>Your cart is empty.</p>\n"
        "}");
    Add("codeForeach",
        "<ul>\n"
        "@ForEach (var product in productList) {\n"
        "    <li>@product.name - @product.price</li>\n"
        "}\n"
        "</ul>");
    Add("codeQuery", "<p>You searched for: @query.searchTerm</p>");
    Add("codePage", "<p>Current page is: @page.pagename</p>");
    Add("codeDataObject", "WebStencilsProcessor1->AddVar(\"user\", UserObject);");
    Add("codeDataMethod",
        "WebStencilsProcessor1.AddVar('products',\n"
        "  function: TObject\n"
        "  begin\n"
        "    Result := GetProductList;\n"
        "  end);");
    Add("codeDataAttribute",
        "type\n"
        "TMyDataModule = class(TDataModule)\n"
        "  [WebStencilsVar('customers', false)]\n"
        "  FDMemTable1: TFDMemTable;\n"
        "  [WebStencilsVar('users')]\n"
        "  FUsers: TUsers;\n"
        "end;\n"
        "\n"
        "WebStencilsProcessor1.AddModule(DataModule1);");

    Add("codeLayoutPage",
        "@LayoutPage BaseTemplate\n"
        "<h1>Welcome to My Page</h1>\n"
        "<p>This is the content of my page.</p>");

    Add("codeRenderBody",
        "<!DOCTYPE html>\n"
        "<html>\n"
        "<head>\n"
        "    <title>My Website</title>\n"
        "</head>\n"
        "<body>\n"
        "    <header>\n"
        "        <!-- Common header content -->\n"
        "    </header>\n"
        "    <main>\n"
        "        @RenderBody\n"
        "    </main>\n"
        "    <footer>\n"
        "        <!-- Common footer content -->\n"
        "    </footer>\n"
        "</body>\n"
        "</html>");

    Add("codeImport",
        "@Import Sidebar.html\n"
        "@* Same behaviour as the previous one *@\n"
        "@Import Sidebar\n"
        "@* Use nested templates for better organization *@\n"
        "@Import folder/Sidebar");

    Add("codeHeaderBodyFooter",
        "@Import Header.html\n"
        "<main>\n"
        "    <!-- Page-specific content here -->\n"
        "</main>\n"
        "@Import Footer.html");

    Add("codeReusableComponents",
        "<div class=\"product-list\">\n"
        "    @Import ProductList { @list = @ProductList }\n"
        "</div>\n"
        "<div class=\"tasks\">\n"
        "  @ForEach (var Task in Tasks.AllTasks) {\n"
        "        @Import partials/tasks/item { @Task }\n"
        "    }\n"
        "</div>");

    Add("codeSessionProperties",
        "@session.Id\n"
        "@session.Authenticated\n"
        "@session.UserName\n"
        "@session.UserRoles\n"
        "@session.CreatedTime\n"
        "@session.AccessedTime\n"
        "@session.AccessedCount\n"
        "@session.Timeout");

    Add("codeSessionInformation",
        "@session.LastURL\n"
        "@session.IP\n"
        "@session.LastReferer\n"
        "@session.LastUserAgent");
}
