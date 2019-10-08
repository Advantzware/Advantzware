@ECHO OFF
REM This program will create a (combined) advantzware database using
REM a structure file, and an existing database (asiShip.db).  The user
REM chooses the file size, and nothing else.  All database names and
REM locations are controlled by this program.
REM If the user needs to create additional databases, this must be done
REM outside the scope of the basic Advantzware installation

REM Check advantzware.ini for variable defaults
CALL ..\getSysVars.bat
IF "%drive%" == "" (
    ECHO advantzware.ini file not found...Exiting
    GOTO :EXIT
    )

REM Do this AFTER you get the system variables
SETLOCAL ENABLEDELAYEDEXPANSION

CLS
ECHO This program allows you to create a new database containing all the 
ECHO structure and data from an existing database.
ECHO .
SET /P createDB="Do you want to create a new database now? (Y/N) : "
IF /I NOT %createDB% == Y (
    GOTO :EXIT
    )

:SETWHICH
ECHO .
ECHO Will this be the LIVE (Prod) database, or the TEST database?
SET /P whichDB="(Prod/Test) : "
IF /I NOT !whichDB! == Prod (
    IF /I NOT !whichDB! == Test (
        ECHO You must enter one of "Prod" or "Test"
        GOTO :SETWHICH
        )
    )
SET thisDbName=asi!whichDb!
:ENDSETWHICH

:SETSIZE
ECHO .
ECHO Enter the projected size of the database:
ECHO 'small' = less than 4 GB
ECHO 'medium' = less than 40 GB
ECHO 'large' = over 40 GB (must have Enterprise-level DB license)
ECHO .
SET /P dbSize="DB Size (small,medium,large) :"
IF /I NOT !dbSize! == small (
    IF /I NOT !dbSize! == medium (
        IF /I NOT !dbSize! == large (
            ECHO You must enter one of 'small', 'medium' or 'large'
            GOTO :SETSIZE
            )
        )
    )
:ENDSETSIZE

:STARTCREATEDB
SET startTime=%time%
%Drive%
CD \%topDir%\%DbDir%\!whichDb!

IF EXIST !Drive!\!topDir!\!DbDir!\!whichDb!\!thisDbName!.db (
    CLS
    ECHO The !thisDBName! database already exists.
    ECHO You cannot use this utility to create a new one.
    GOTO :EXIT
    )

CLS
ECHO Building database (this may take some time) ...
CALL !DLC11!\bin\prostrct create !thisDbName! !drive!\!topDir!\!DbDir!\!dbStructDir!\%dbSize%.st

CLS
ECHO Database structure complete.
ECHO Copying SHIP database metadata and contents...
CALL !DLC11!\bin/procopy !drive!\!topDir!\!dbDir!\!dbShipDir!\!shipDBName!.db !thisDbName! > NUL

ECHO Rebuilding indexes...
CALL !DLC11!\bin\proutil !drive!\!topDir!\!dbDir!\!whichDb!\!thisDbName!.db -C idxbuild all -B 1024 -TB 31 -TM 32 > NUL

ECHO Creating archive structure file...
COPY !drive!\!topDir!\!DbDir!\!dbStructDir!\%dbSize%.st !drive!\!topDir!\!dbDir!\!whichDb!\!thisDbName!.st > NUL

ECHO Starting database server...
CALL !DLC11!\bin\dbman -host %hostname% -port %adminport% -database !thisDBName! -start > NUL
:ENDCREATEDB

:EXIT
ECHO .
ECHO Start Time: !startTime!
ECHO End Time  : %time%
PAUSE
EXIT /b