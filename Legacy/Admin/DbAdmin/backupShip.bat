@ECHO OFF
SETLOCAL ENABLEDELAYEDEXPANSION

SET configFile=advantzware.ini

REM Walk up the directory structure until it's found
IF EXIST "%configFile%" (
    GOTO :NEXT
    ) ELSE (
        SET configFile=..\!configFile!
        IF EXIST "!configFile!" (
            GOTO :NEXT
            ) ELSE (
            SET configFile=..\!configFile!
            IF EXIST "!configFile!" (
                GOTO :NEXT
                ) ELSE (
                SET configFile=..\!configFile!
                IF EXIST "!configFile!" (
                    GOTO :NEXT
                    ) ELSE (
                    ECHO Unable to locate an advantzware.ini file
                    GOTO :EXIT
                )
            )
        )
    )

:NEXT
FOR /F "tokens=1,2 delims==" %%G IN (!configFile!) DO SET %%G=%%H

:BACKUP
SET DLC=%DLC11%
!Drive!
CD \!topDir!\!DbDir!\!dbShipDir!
CALL !DLC11!\bin\probkup online !shipDbName! !Drive!\!topDir!\!dbBackupDir!\asiShip
COPY !shipDbStFile! !Drive!\!topDir!\!dbBackupDir!\

:EXIT
