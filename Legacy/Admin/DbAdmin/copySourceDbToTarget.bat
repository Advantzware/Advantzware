@ECHO OFF
REM This program allows a user to create a backup of a single database, 
REM or of all databases known to the admin server

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
IF "%drive%" == "" (
    ECHO advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

:NEXT 
CLS
ECHO Use this utility to copy one database ("source") to another ("target")
ECHO Note that neither source nor target databases will be available during the copy.
ECHO .
SET /P source="Enter the SOURCE database ("Prod","Test","Ship") :"
IF /I NOT "%source%" == "prod" (
    IF /I NOT "%source%" == "test" (
        IF /I NOT "%source%" == "ship" (
            ECHO You must choose one of 'Prod", 'Test' or 'Ship' for the source
            PAUSE
            GOTO :NEXT
            )
        )
    )
SET /P target="Enter the TARGET database ("Prod","Test") :"
IF /I NOT "%target%" == "prod" (
    IF /I NOT "%target%" == "test" (
        ECHO You must choose one of 'Prod" or 'Test' for the target
        PAUSE
        GOTO :NEXT
        )
    )
ECHO.
SET /P imsure="This action will completely replace the %targetDB% database. Are you sure? (Y/N)
IF /I NOT "%imsure%" == "Y" (
    GOTO :EXIT
    )

SET targetDbStFile=asi%target%.st
SET sourceDir=%source%
SET targetDir=%target%
SET sourceDBname=asi%source%
SET targetDBname=asi%target%

IF NOT EXIST %Drive%\%topDir%\%dbDir%\%sourceDir%\%sourceDbName%.db (
    ECHO The source database does not exist
    ECHO You cannot copy from a nonexistent database
    ECHO .
    GOTO :EXIT
    )
IF NOT EXIST %Drive%\%topDir%\%dbDir%\%targetDir%\%targetDbName%.db (
    ECHO The target database does not exist
    ECHO You should use the createOrigDatabase batch file to create it
    ECHO .
    GOTO :EXIT
    )

:STARTCREATEDB
SET startTime=%time%
ECHO Stopping both databases
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !sourceDBName! -stop > NUL 2> NUL
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !targetDBName! -stop > NUL 2> NUL

%Drive%
CD \%topDir%\%dbDir%\%targetDir%

ECHO Remove existing %target% DB
REN %targetDbStFile% backup.st
DEL asi%target%*.*
REN backup.st %targetDbStFile%

ECHO Truncate %source% database bi file...
CALL %DLC11%\bin\proutil %drive%\%topDir%\%dbDir%\%sourceDir%\%sourceDBName% -C truncate bi > NUL

ECHO Building empty %target% database...
CALL %DLC11%\bin\prostrct create %targetDbName% %targetDbStFile% > NUL

ECHO Copying %source% to %target%
CALL %DLC11%\bin\procopy %drive%\%topDir%\%dbDir%\%sourceDir%\%sourceDBName% %targetDbName% > NUL

ECHO Rebuilding indexes...
CALL %DLC11%\bin\proutil %drive%\%topDir%\%dbDir%\%targetDir%\%targetDbName% -C idxbuild all -B 1024 -TB 31 -TM 32 > NUL

ECHO %targetDbName% replaced by %sourceDBName%
ECHO.
ECHO Starting both databases
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !sourceDBName! -start > NUL
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !targetDBName! -start > NUL

:ENDCREATEDB

:EXIT
ECHO .
ECHO Start Time: %startTime%
ECHO End Time  : %time%
PAUSE
EXIT /b