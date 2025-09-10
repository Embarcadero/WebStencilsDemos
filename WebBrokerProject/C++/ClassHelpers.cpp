#include <vcl.h>
#pragma hdrstop

#include "ClassHelpers.h"

#pragma package(smart_init)

// Helper function to convert TMethodType to string
String MethodTypeToString(TMethodType AMethodType) {
    switch (AMethodType) {
        case TMethodType::mtGet: return "GET";
        case TMethodType::mtPost: return "POST";
        case TMethodType::mtPut: return "PUT";
        case TMethodType::mtDelete: return "DELETE";
        case TMethodType::mtHead: return "HEAD";
//        case TMethodType::mtOptions: return "OPTIONS";
        case TMethodType::mtPatch: return "PATCH";
        default: return "UNKNOWN";
    }
}

TWebModule* TWebModuleHelper::AddAction(TWebModule* WebModule, TMethodType AMethodType, const String& APathInfo,
    THTTPMethodEvent AOnAction, bool ADefault) {
    TWebActionItem* act = WebModule->Actions->Add();
    act->MethodType = AMethodType;
    // Convert the enum AMethodType to string to add it to the Action name and avoid collisions between endpoints
    String MethodTypeString = GetEnumName(__delphirtti(TMethodType), static_cast<int>(AMethodType));
    act->Name = APathInfo + MethodTypeString;
    act->PathInfo = APathInfo;
    act->OnAction = AOnAction;
    act->Default = ADefault;
    return WebModule;
}

void TWebModuleHelper::AddRoutes(TWebModule* WebModule, const std::vector<TRoute>& Routes) {
    for (const auto& Route : Routes) {
        AddAction(WebModule, Route.MethodType, Route.PathInfo, Route.OnAction, Route.Default);
    }
}

