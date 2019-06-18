@ECHO OFF 
REM ---------------------------------------------------------
REM 
REM
REM      Author: Paul Koufalis
REM        Date: 2016/12/27
REM
REM     Purpose: Send a custom alert from a script
REM
REM
REM  Parameters: Friendly_Name
REM              -type = alertType (default alert)
REM              -m    = alertMetric (default sendAlert)
REM              -msg  = "Msg in quotes" (default "No message text")
REM
REM  MODIFICATION LOG
REM
REM  20130528 KOUP Creation
REM  20161227 KOUP Configure for ProTop
REM 
REM ---------------------------------------------------------

REM Delay variable expansion to runtime
Setlocal EnableDelayedExpansion

REM VARIABLES
CALL "C:\Users\Administrator.WIN-SIDPMPLNDIB\Downloads\protop\bin\protopenv.bat"

cd /d %PROTOP%

SET USAGE="sendalert <friendlyName> [-type <alertType=alert>] [-m <alertMetric=sendAlert>] [-msg <\"Alert msg in quotes\">]"
SET TYPE=alert
SET METRIC=sendAlert
SET MSG="No message text"

REM Process command line parameters

set P1=%1
SHIFT

if "%P1%" == "-h" (
        

        echo %USAGE%
        GOTO :END
)

:loop
IF NOT "%1"=="" (
    IF "%1"=="-type" (
        SET TYPE=%2
        SHIFT
    )
    IF "%1"=="-m" (
        SET METRIC=%2
        SHIFT
    )
    IF "%1"=="-msg" (
        REM the %~2 strips the double quotes from -msg "This is a msg"
        SET MSG=%~2
        SHIFT
    )

    SHIFT
    GOTO :loop
)

if exist %PROTOP%\etc\dblist.cfg (

        echo. > %TEMP%\pt3dblist.tmp
        grep "^%P1%|" %PROTOP%\etc\dblist.cfg | gawk -F "|" "{print $2}" > %TEMP%\pt3dblist.tmp
        set /p XDB=<%TEMP%\pt3dblist.tmp

)

if not exist %XDB%.db (

       echo "Friendly name %P1% does not exist."
       goto :END

)

SET PARAM="%P1%|%TYPE%|%METRIC%|%MSG%"

%DLC%\bin\_progres.exe -b -p util\sendalert.p -pf %PROTOP%\etc\protop.pf -T %TMPDIR% -basekey "INI" -ininame etc\protop.ini -param %PARAM% >> %LOGDIR%\sendalert.%P1%.e2 2>&1

:END
