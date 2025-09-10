//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop

#include "UtilsLogger.h"
#include <iostream>
#include <fstream>

//---------------------------------------------------------------------------
#pragma package(smart_init)

// Static member definitions
std::unique_ptr<TLogger> TLogger::FInstance;
std::mutex TLogger::FMutex;

// Global Logger instance
TLogger* Logger = TLogger::Instance();

//---------------------------------------------------------------------------
__fastcall TLogger::TLogger()
    : TObject()
{
    try
    {
        // Get log path from environment variable or use default
        String EnvLogPath = GetEnvironmentVariable("APP_LOG_PATH");
        if (!EnvLogPath.IsEmpty())
        {
            FLogFileName = EnvLogPath;
            FLogPath = System::Sysutils::ExtractFilePath(FLogFileName);
        }
        else
        {
            // Default to logs subdirectory next to the binary
            String BinaryPath = System::Ioutils::TPath::GetDirectoryName(ParamStr(0));
            FLogPath = System::Ioutils::TPath::Combine(BinaryPath, "logs");
            FLogFileName = System::Ioutils::TPath::Combine(FLogPath, "app.log");
        }
        
        EnsureLogDirectory();
        // Avoid calling Info() here to prevent re-entrancy/deadlock during Instance()
    }
    catch (Exception &E)
    {
        std::wcerr << L"TLogger initialization error: " << E.Message.c_str() << std::endl;
        // Fallback: disable file logging
        FLogFileName = "";
        FLogPath = "";
    }
}

//---------------------------------------------------------------------------
__fastcall TLogger::~TLogger()
{
    // Avoid logging during destruction to prevent exceptions on shutdown
}

//---------------------------------------------------------------------------
TLogger* TLogger::Instance()
{
    std::lock_guard<std::mutex> lock(FMutex);
    
    if (!FInstance)
    {
        // Construct instance; constructor avoids using the mutex/logging
        FInstance = std::unique_ptr<TLogger>(new TLogger());
        // Set global instance
        Logger = FInstance.get();
    }
    
    return FInstance.get();
}

//---------------------------------------------------------------------------
void TLogger::EnsureLogDirectory()
{
    if (!FLogPath.IsEmpty() && !System::Sysutils::DirectoryExists(FLogPath))
    {
        try
        {
            System::Sysutils::ForceDirectories(FLogPath);
        }
        catch (Exception &E)
        {
            std::wcerr << L"Failed to create log directory: " << E.Message.c_str() << std::endl;
            // Fallback: disable file logging instead of throwing
            FLogFileName = "";
            FLogPath = "";
        }
    }
}

//---------------------------------------------------------------------------
String TLogger::FormatLogEntry(const String& ALevel, const String& AMessage)
{
    return FormatDateTime("yyyy-mm-dd hh:nn:ss.zzz", Now()) + " [" + ALevel + "] " + AMessage;
}

//---------------------------------------------------------------------------
void TLogger::WriteToFile(const String& ALevel, const String& AMessage)
{
    std::lock_guard<std::mutex> lock(FMutex);
    
    try
    {
        String LogEntry = FormatLogEntry(ALevel, AMessage);
        
        // Also output to console for debugging
#ifdef _DEBUG
        std::wcout << LogEntry.c_str() << std::endl;
#endif
        
        // Write to file if configured
        if (!FLogFileName.IsEmpty())
        {
            std::wofstream logFile;
            logFile.open(FLogFileName.c_str(), std::ios::app | std::ios::out);
            if (logFile.is_open())
            {
                logFile << LogEntry.c_str() << std::endl;
                logFile.close();
            }
        }
    }
    catch (Exception &E)
    {
        std::wcerr << L"Logger write error: " << E.Message.c_str() << std::endl;
    }
    catch (...)
    {
        std::wcerr << L"Logger write error: Unknown exception" << std::endl;
    }
}

//---------------------------------------------------------------------------
void TLogger::Info(const String& AMessage)
{
    WriteToFile("INFO", AMessage);
}

//---------------------------------------------------------------------------
void TLogger::Error(const String& AMessage)
{
    WriteToFile("ERROR", AMessage);
}

//---------------------------------------------------------------------------
void TLogger::Warning(const String& AMessage)
{
    WriteToFile("WARN", AMessage);
}

//---------------------------------------------------------------------------
void TLogger::Debug(const String& AMessage)
{
#ifdef _DEBUG
    WriteToFile("DEBUG", AMessage);
#endif
}