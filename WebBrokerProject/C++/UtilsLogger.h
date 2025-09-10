//---------------------------------------------------------------------------
#ifndef UtilsLoggerH
#define UtilsLoggerH
//---------------------------------------------------------------------------

#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.IOUtils.hpp>
#include <System.DateUtils.hpp>
#include <memory>
#include <mutex>

//---------------------------------------------------------------------------
class TLogger : public TObject
{
private:
    static std::unique_ptr<TLogger> FInstance;
    static std::mutex FMutex;
    String FLogFileName;
    String FLogPath;
    
    void WriteToFile(const String& ALevel, const String& AMessage);
    String FormatLogEntry(const String& ALevel, const String& AMessage);
    void EnsureLogDirectory();
    
    __fastcall TLogger();

public:
    __fastcall virtual ~TLogger();

    static TLogger* Instance();
    void Info(const String& AMessage);
    void Error(const String& AMessage);
    void Warning(const String& AMessage);
    void Debug(const String& AMessage);
    
    __property String LogFileName = {read=FLogFileName};
    __property String LogPath = {read=FLogPath};
};

// Global Logger instance access
extern TLogger* Logger;

//---------------------------------------------------------------------------
#endif