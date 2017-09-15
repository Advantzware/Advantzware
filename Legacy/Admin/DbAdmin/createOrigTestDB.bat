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
SET /P createDB="Do you want to create a new TEST database? (Y/N) : "
IF "%createDB%" == "y" SET createDB=Y
IF NOT "%createDB%" == "Y" (
    GOTO :EXIT
    )

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
REM ONLY FOR CREATE TEST DB FROM SHIP DB
SET startTime=%time%
!Drive!
CD \!topDir!\!DbDir!\!dbTestDir!
CLS

ECHO Building Test database...
CALL !DLC11!\bin\prostrct create !testDbName! !drive!\!topDir!\!DbDir!\!dbStFileDir!\!testDbStFile!

ECHO Copying empty metadata
CALL !DLC11!\bin/procopy !drive!\!topDir!\!dbDir!\!dbShipDir!\!shipDBName!.db !testDbName!

ECHO Rebuilding indexes...
CALL !DLC11!\bin\proutil !drive!\!topDir!\!dbDir!\!dbTestDir!\!testDbName!.db -C idxbuild all -B 1024 -TB 31 -TM 32 > NUL
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !testDBName! -start

COPY !drive!\!topDir!\!DbDir!\!dbStFileDir!\!TestDbStFile! !drive!\!topDir!\!dbDir!\!dbTestDir!\!TestDbName!.st
:ENDCREATEDB

:EXIT
ECHO .
ECHO Start Time: !startTime!
ECHO End Time  : %time%
PAUSE
EXIT /b