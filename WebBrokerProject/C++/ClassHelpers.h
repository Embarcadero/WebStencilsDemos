#ifndef ClassHelpersH
#define ClassHelpersH

#include <System.SysUtils.hpp>
#include <Web.HTTPApp.hpp>
#include <vector>

// Route structure definition
struct TRoute {
    TMethodType MethodType;
    String PathInfo;
    THTTPMethodEvent OnAction;
    bool Default;

    TRoute(TMethodType AMethodType, const String& APathInfo,
           THTTPMethodEvent AOnAction, bool ADefault = false)
        : MethodType(AMethodType), PathInfo(APathInfo),
          OnAction(AOnAction), Default(ADefault) {}
};

// WebModule helper utility class
class TWebModuleHelper {
public:
    static void AddRoutes(TWebModule* WebModule, const std::vector<TRoute>& Routes);
    static TWebModule* AddAction(TWebModule* WebModule, TMethodType AMethodType, 
                                const String& APathInfo, THTTPMethodEvent AOnAction, 
                                bool ADefault = false);
};

#endif
