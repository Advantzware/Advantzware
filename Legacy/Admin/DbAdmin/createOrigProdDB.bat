@ECHO OFF
REM This program will create a (combined) advantzware database using
REM a structure file (advantzware.dt).  Following creation, metadata
REM must be installed via procopy, and finally the .df must be
REM applied to finish creating the (empty) db.
REM A separate batch file is required to dump data from existing
REM databases, and a third file is required to load that data into
REM the new database.

SETLOCAL ENABLEDELAYEDEXPANSION

REM -----------------------------------------------------------------
REM Choose activities
REM -----------------------------------------------------------------
CLS
ECHO .
ECHO This program allows you to create a new database containing all the 
ECHO structure and data from an existing database.
ECHO .
SET /P createDB="Do you want to create a new PRODUCTION database? (Y/N) : "
IF "%createDB%" == "y" SET createDB=Y
IF NOT "%createDB%" == "Y" (
    GOTO :EXIT
    )
ECHO .
ECHO Enter the projected size of the database:
ECHO 'small' = less than 4 GB
ECHO 'medium' = less than 40 GB
ECHO 'large' = over 40 GB (must have Enterprise-level DB license)
ECHO .
SET /P dbSize="DB Size (small,medium,large) :"

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

:STARTCREATEDB
REM ONLY FOR CREATE PROD DB FROM SHIP DB
SET startTime=%time%
%Drive%
CD \%topDir%\%DbDir%\%dbProdDir%

CLS
ECHO Building production database...
CALL !DLC11!\bin\prostrct create !prodDbName! !drive!\!topDir!\!DbDir!\!dbStFileDir!\%dbSize%.st

ECHO Copying empty metadata
CALL !DLC11!\bin/procopy !drive!\!topDir!\!dbDir!\!dbShipDir!\!shipDBName!.db !prodDbName!

ECHO Rebuilding indexes...
CALL !DLC11!\bin\proutil !drive!\!topDir!\!dbDir!\!dbProdDir!\!prodDbName!.db -C idxbuild all -B 1024 -TB 31 -TM 32 > NUL
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !prodDBName! -start

COPY !drive!\!topDir!\!DbDir!\!dbStFileDir!\%dbSize%.st !drive!\!topDir!\!dbDir!\!dbProdDir!\!prodDbName!.st
:ENDCREATEDB

:STARTDB
CALL !DLC11!\bin\dbman -host asivm2 -database asiProd
:ENDSTARTDB

:EXIT
ECHO .
ECHO Start Time: !startTime!
ECHO End Time  : %time%
PAUSE
EXIT /b