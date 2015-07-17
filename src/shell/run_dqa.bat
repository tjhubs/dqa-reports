REM Batch file to run app

set MAINFILELOCATION="%USERPROFILE%\hsuApps\dqa-reports\src\r\00-main.R"
R -f --slave --no-save --no-restore %MAINFILELOCATION%
PAUSE
