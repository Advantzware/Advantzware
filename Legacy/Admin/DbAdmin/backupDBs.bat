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

:SETWHICH
CLS
ECHO This program allows you to back up one or all databases.
ECHO Enter one of 'prod', 'test', 'ship' or 'all'
SET /P whichDB="Which database do you wish to back up: "
ECHO .
IF /I NOT !whichDB! == prod (
    IF /I NOT !whichDB! == test (
        IF /I NOT !whichDB! == ship (
            IF /I NOT !whichDB! == all (
	            ECHO You must enter one of 'prod', 'test', 'ship' or 'all'
        	    GOTO :SETWHICH
                )
            )
        )
    )
:ENDSETWHICH

:BACKUP
FOR /f "skip=1" %%x IN ('wmic os get localdatetime') DO IF NOT DEFINED MyDate SET MyDate=%%x
SET backdate=%MyDate:~3,1%%MyDate:~4,2%%MyDate:~6,2%
SET DLC=%DLC11%
!Drive!

IF /I !whichDB! == prod (
    CD \!topDir!\!DbDir!\!dbProdDir!
    CALL !DLC11!\bin\probkup online !prodDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.bak (
        ECHO PROD database backed up successfully
        )
    COPY !prodDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.st > NUL
    )
IF /I !whichDB! == test (
    CD \!topDir!\!DbDir!\!dbTestDir!
    CALL !DLC11!\bin\probkup online !testDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.bak (
        ECHO TEST database backed up successfully
        )
    COPY !testDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.st > NUL
    )
IF /I !whichDB! == ship (
    CD \!topDir!\!DbDir!\!dbShipDir!
    CALL !DLC11!\bin\probkup online !shipDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.bak (
        ECHO SHIP database backed up successfully
        )
    COPY !shipDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.st > NUL
    )
IF /I !whichDB! == all (
    CD \!topDir!\!DbDir!\!dbProdDir!
    CALL !DLC11!\bin\probkup online !prodDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.bak (
        ECHO PROD database backed up successfully
        )
    COPY !prodDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiProd.%backdate%.st > NUL

    CD \!topDir!\!DbDir!\!dbTestDir!
    CALL !DLC11!\bin\probkup online !testDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.bak (
        ECHO TEST database backed up successfully
        )
    COPY !testDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiTest.%backdate%.st > NUL

    CD \!topDir!\!DbDir!\!dbShipDir!
    CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !shipDBName! -start > NUL
    CALL !DLC11!\bin\probkup online !shipDbName! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.bak > NUL
    IF EXIST !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.bak (
        ECHO SHIP database backed up successfully
        )
    CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !shipDBName! -stop > NUL
    COPY !shipDbStFile! !Drive!\!topDir!\!backupDir!\!dbBackup!\asiShip.%backdate%.st > NUL
    )
GOTO :EXIT

:EXIT
pause