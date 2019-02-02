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
ECHO Use this utility to copy the Production database OVER the Test database.
ECHO Note that neither source nor target databases will be available during the copy.
ECHO .
SET /P imsure="This action will completely replace the TEST database. Are you sure? (Y/N)
IF /I NOT "%imsure%" == "Y" (
    GOTO :EXIT
    )

SET sourceDir=%source%
SET targetDir=%target%
SET sourceDBname=%%source%DbName%
SET targetDBname=%%target%DbName%

IF NOT EXIST %Drive%\%topDir%\%dbDir%\%dbProdDir%\%prodDbName%.db (
    ECHO The source database does not exist
    ECHO You cannot copy from a nonexistent database
    ECHO .
    GOTO :EXIT
    )
IF NOT EXIST %Drive%\%topDir%\%dbDir%\%dbTestDir%\%testDbName%.db (
    ECHO The target database does not exist
    ECHO You should use the createOrigDatabase batch file to create it
    ECHO .
    GOTO :EXIT
    )

:STARTCREATEDB
SET DLC=%DLCDir%
SET startTime=%time%

ECHO Backing up %testDbName% to %mapDir%\%backupDir%\%dbBackup%\%testDbName%.bak
%Drive%
CD \%topDir%\%dbDir%\%dbTestDir%
CALL %DLC%\bin\probkup online %testDbName% %mapDir%\%backupDir%\%dbBackup%\%testDbName%.bak > NUL

ECHO Backing up %prodDbName% to %mapDir%\%backupDir%\%dbBackup%\%prodDbName%.bak
CD \%topDir%\%dbDir%\%dbProdDir%
CALL %DLC%\bin\probkup online %prodDbName% %mapDir%\%backupDir%\%dbBackup%\%prodtestDbName%.bak > NUL

ECHO Stopping TEST database
CALL !DLC!\bin\dbman -host %hostname% -port %adminport% -database !testDBName! -stop > NUL
ECHO Waiting for databases to complete stopping...
timeout 15 /nobreak

ECHO Restoring %prodDbName%.bak data files to %testDbName% 
%Drive%
CD \%topDir%\%dbDir%\%dbTestDir%
CALL %DLC%\bin\prorest %testDbName% %mapDir%\%backupDir%\%dbBackup%\%prodtestDbName%.bak > NUL
timeout 15 /nobreak

ECHO Restarting test database
ECHO Waiting for databases to start...
CALL !DLC!\bin\dbman -host %hostname% -port %adminport% -database !testDBName! -start  > NUL

:ENDCREATEDB

:EXIT
ECHO .
ECHO Copy complete.
ECHO Start Time: %startTime%
ECHO End Time  : %time%
PAUSE
EXIT /b