@echo off
REM Tailwind CSS Build Script
REM This script can be called from Delphi post-build events or run manually

REM Set paths (adjust based on your project structure)
set TAILWIND_CLI=tools\tailwindcss.exe
set INPUT_CSS=src\input.css
set OUTPUT_CSS=templates\static\css\output.css
set CONTENT_PATH=templates\**\*.html

REM Create output directory if it doesn't exist
if not exist "templates\css" mkdir "templates\css"

REM Run Tailwind CLI
"%TAILWIND_CLI%" -i "%INPUT_CSS%" -o "%OUTPUT_CSS%" --minify --content "%CONTENT_PATH%"

if %ERRORLEVEL% EQU 0 (
    echo Tailwind CSS generated successfully: %OUTPUT_CSS%
) else (
    echo ERROR: Tailwind CSS generation failed!
    exit /b %ERRORLEVEL%
)


