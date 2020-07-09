@ECHO OFF
SET configFile=advantzware.ini
SET chgMade=N

REM Walk up the directory structure until it's found
IF EXIST "%configFile%" (
    GOTO :NEXT
    ) ELSE (
        SET configFile=..\%configFile%
        IF EXIST "..\%configFile%" (
            GOTO :NEXT
            ) ELSE (
            SET configFile=..\..\%configFile%
            IF EXIST "..\..\%configFile%" (
                GOTO :NEXT
                ) ELSE (
                SET configFile=..\..\..\%configFile%
                IF EXIST "..\..\..\%configFile%" (
                    GOTO :NEXT
                    ) ELSE (
                    ECHO Unable to locate an advantzware.ini file
                    GOTO :EXITVARS
                )
            )
        )
    )

:NEXT
FOR /F "tokens=1,2 delims==" %%G IN (%configFile%) DO SET %%G=%%H

:ERRCHECK
SET isError=N
SET isWarn=N
GOTO :ENDERRCHECK

REM If initial run, populate empty variables
IF "%siteName%" == "" (
    SET isError=Y
    ECHO You must enter a sitename in the next screens
    GOTO :ENDERRCHECK
)
REM Check that all the directories exist
IF NOT EXIST "%drive%\%topDir%" (
    SET isError=Y
    ECHO Unable to locate required folder %drive%\%topDir%
    GOTO :ENDERRCHECK
)
IF NOT EXIST "%mapDir%" (
    SET isError=Y
    ECHO Drive %mapDir% is not properly mapped
    GOTO :ENDERRCHECK
)
IF NOT EXIST "%DLC11%" (
    SET isError=Y
    ECHO Unable to locate your DBMS environment %DLC11%
    GOTO :ENDERRCHECK
)
IF NOT EXIST "%mapDir%\%adminDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%adminDir%> NUL
    ECHO Building required folder %mapDir%\%adminDir%
)
IF NOT EXIST "%mapDir%\%backupDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%backupDir%> NUL
    ECHO Building required folder %mapDir%\%backupDir%
)
IF NOT EXIST "%mapDir%\%custDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%custDir%> NUL
    ECHO Building required folder %mapDir%\%custDir%
)
IF NOT EXIST "%mapDir%\%dbDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%> NUL
    ECHO Building required folder %mapDir%\%dbDir%
)
IF NOT EXIST "%mapDir%\%envDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%
)
IF NOT EXIST "%mapDir%\%updateDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%updateDir%> NUL
    ECHO Building required folder %mapDir%\%updateDir%
)
IF NOT EXIST "%mapDir%\%adminDir%\%dbAdmin%" (
    SET isWarn=Y
    MKDIR %mapDir%\%adminDir%\%dbAdmin%> NUL
    ECHO Building required folder %mapDir%\%adminDir%\%dbAdmin
)
IF NOT EXIST "%mapDir%\%adminDir%\%envAdmin%" (
    SET isWarn=Y
    MKDIR %mapDir%\%adminDir%\%envAdmin%> NUL
    ECHO Building required folder %mapDir%\%adminDir%\%envAdmin
)
IF NOT EXIST "%mapDir%\%backupDir%\%dbBackup%" (
    SET isWarn=Y
    MKDIR %mapDir%\%backupDir%\%dbBackup%> NUL
    ECHO Building required folder %mapDir%\%backupDir%\%dbBackup%
)
IF NOT EXIST "%mapDir%\%backupDir%\%pgmBackup%" (
    SET isWarn=Y
    MKDIR %mapDir%\%backupDir%\%pgmBackup%> NUL
    ECHO Building required folder %mapDir%\%backupDir%\%pgmBackup%
)
IF NOT EXIST "%mapDir%\%backupDir%\%resBackup%" (
    SET isWarn=Y
    MKDIR %mapDir%\%backupDir%\%resBackup%> NUL
    ECHO Building required folder %mapDir%\%backupDir%\%resBackup%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbDataDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%\%dbDataDir%> NUL
    ECHO Building required folder %mapDir%\%dbDir%\%dbDataDir%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbProdDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%\%dbProdDir%> NUL
    ECHO Building required folder %mapDir%\%dbDir%\%dbProdDir%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbShipDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%\%dbShipDir%> NUL
    ECHO Building required folder %mapDir%\%dbDir%\%dbShipDir%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbStructDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%\%dbStructDir%> NUL
    ECHO Building required folder %mapDir%\%dbDir%\%dbStructDir%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbTestDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%dbDir%\%dbTestDir%> NUL
    Building required folder %mapDir%\%dbDir%\%dbTestDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envCustDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envCustDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envCustDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envOverDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envOverDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envOverDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envPgmDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envPgmDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envPgmDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envResDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envResDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envResDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envSchDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envSchDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envSchDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envUmenuDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envUmenuDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envUmenuDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envProdDir%\%envUserDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envProdDir%\%envUserDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envProdDir%\%envUserDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envCustDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envCustDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envCustDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envOverDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envOverDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envOverDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envPgmDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envPgmDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envPgmDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envResDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envResDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envResDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envSchDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envSchDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envSchDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envUmenuDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envUmenuDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envUmenuDir%
)
IF NOT EXIST "%mapDir%\%envDir%\%envTestDir%\%envUserDir%" (
    SET isWarn=Y
    MKDIR %mapDir%\%envDir%\%envTestDir%\%envUserDir%> NUL
    ECHO Building required folder %mapDir%\%envDir%\%envTestDir%\%envUserDir%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbShipDir%\%shipDbName%.db" (
    SET isError=Y
    ECHO Unable to locate required database %mapDir%\%dbDir%\%dbShipDir%\%shipDbName%.db
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbStructDir%\%dfFileName%" (
    SET isError=Y
    ECHO Unable to locate required file %mapDir%\%dbDir%\%dbStructDir%\%dfFileName%
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbStructDir%\small.st" (
    SET isError=Y
    ECHO Unable to locate required file %mapDir%\%dbDir%\%dbStructDir%\small.st
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbStructDir%\medium.st" (
    SET isError=Y
    ECHO Unable to locate required file %mapDir%\%dbDir%\%dbStructDir%\medium.st
)
IF NOT EXIST "%mapDir%\%dbDir%\%dbStructDir%\large.st" (
    SET isError=Y
    ECHO Unable to locate required file %mapDir%\%dbDir%\%dbStructDir%\large.st
)

:ENDERRCHECK

IF %isWarn% == Y (
    PAUSE
    )

IF %isError% == Y (
    SET chgMade=Y
    ECHO There is a problem with the values in the advantzware.ini file
    ECHO Please edit them in the following screens
    PAUSE
    GOTO :GETINFO
    )

IF %chgMade% == Y (
    GOTO :WRITEFILE
    )

GOTO :EXITVARS

:GETINFO
CLS
ECHO Server/Host variables
SET /P sitename="Enter a unique name for this site (%sitename%): "
SET /P hostname="Enter this computer's network name (%hostname%): "
SET /P drive="Enter the drive where this install 'Prods' (%drive%): "
SET /P topDir="Enter the top level directory for this install (%topDir%): "
SET /P mapDir="Enter the drive letter this is mapped to (%mapDir%): "
ECHO Misc variables
SET /P adminport="Enter the port for DBMS Admin Server (%adminport%): "
SET /P makeBackup="Do you want to automatically back up files when upgrading (%makeBackup%): "
GOTO :ERRCHECK
:ENDGETINFO

:WRITEFILE
SET outFile=%drive%\%topDir%\advantzware.ini
ECHO # Server/Host variables> %outFile%
ECHO sitename=%sitename%>> %outFile%
ECHO hostname=%hostname%>> %outFile%
ECHO drive=%drive%>> %outFile%
ECHO topDir=%topDir%>> %outFile%
ECHO mapDir=%mapDir%>> %outFile%
ECHO # Directory Structure>> %outFile%
ECHO # Top Level>> %outFile%
ECHO adminDir=%adminDir%>> %outFile%
ECHO backupDir=%backupDir%>> %outFile%
ECHO custDir=%custDir%>> %outFile%
ECHO dbDir=%dbDir%>> %outFile%
ECHO envDir=%envDir%>> %outFile%
ECHO updateDir=%updateDir%>> %outFile%
ECHO # Admin Level>> %outFile%
ECHO dbAdmin=%dbAdmin%>> %outFile%
ECHO envAdmin=%envAdmin%>> %outFile%
ECHO # Backup Level>> %outFile%
ECHO dbBackup=%dbBackup%>> %outFile%
ECHO pgmBackup=%pgmBackup%>> %outFile%
ECHO resBackup=%resBackup%>> %outFile%
ECHO # Database Level>> %outFile%
ECHO dbDataDir=%dbDataDir%>> %outFile%
ECHO dbProdDir=%dbProdDir%>> %outFile%
ECHO dbShipDir=%dbShipDir%>> %outFile%
ECHO dbStructDir=%dbStructDir%>> %outFile%
ECHO dbTestDir=%dbTestDir%>> %outFile%
ECHO # Environments Level>> %outFile%
ECHO envProdDir=%envProdDir%>> %outFile%
ECHO envTestDir=%envTestDir%>> %outFile%
ECHO envCustDir=%envCustDir%>> %outFile%
ECHO envOverDir=%envOverDir%>> %outFile%
ECHO envPgmDir=%envPgmDir%>> %outFile%
ECHO envResDir=%envResDir%>> %outFile%
ECHO envSchDir=%envSchDir%>> %outFile%
ECHO envUMenuDir=%envUMenuDir%>> %outFile%
ECHO envUserDir=%envUserDir%>> %outFile%
ECHO # Updates Level>> %outFile%
ECHO zipDir=%zipDir%>> %outFile%
ECHO dataUpdDir=%dataUpdDir%>> %outFile%
ECHO strUpdDir=%strUpdDir%>> %outFile%
ECHO overDir=%overDir%>> %outFile%
ECHO programsDir=%programsDir%>> %outFile%
ECHO resourceDir=%resourceDir%>> %outFile%
ECHO # DBMS variables>> %outFile%
ECHO DLC11=%DLC11%>> %outFile%
ECHO proVersion=%proVersion%>> %outFile%
ECHO # Database variables>> %outFile%
ECHO prodDbName=%prodDbName%>> %outFile%
ECHO prodDbPort=%prodDbPort%>> %outFile%
ECHO prodDbStFile=%prodDbStFile%>> %outFile%
ECHO shipDBName=%shipDbName%>> %outFile%
ECHO shipDBPort=%shipDbPort%>> %outFile%
ECHO shipDbStFile=%shipDbStFile%>> %outFile%
ECHO testDBName=%testDbName%>> %outFile%
ECHO testDbPort=%testDbPort%>> %outFile%
ECHO testDbStFile=%testDbStFile%>> %outFile%
ECHO dfFilename=%dfFilename%>> %outFile%
ECHO # Misc variables>> %outFile%
ECHO deltaFilename=%deltaFilename%>> %outFile%
ECHO adminport=%adminport%>> %outFile%
ECHO makeBackup=%makeBackup%>> %outFile%
ECHO # Login variables>> %outFile%
ECHO envList=%envList%>> %outFile%
ECHO modeList=%modeList%>> %outFile%
:ENDWRITEFILE

:EXITVARS
