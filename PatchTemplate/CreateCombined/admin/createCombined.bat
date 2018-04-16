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
ECHO This program allows you to create a new (combined) database from 
ECHO your existing Advantzware databases.  This new database is required
ECHO for all Advantzware versions beginning with Release 16.5.0.
ECHO .
ECHO From this program, you can also create copies of the combined database
ECHO if you need these for testing and/or training purposes.
ECHO .
SET /P createDB="Are you creating a new (combined) database? (Y/N) : "
SET /P dumpData="Are you dumping data from an old database group? (Y/N) : "
SET /P loadData="Are you loading dumped data into the new database? (Y/N) : "
IF "!createDB!" == "y" SET createDB=Y
IF "!dumpData!" == "y" SET dumpData=Y
IF "!loadData!" == "y" SET loadData=Y
IF "!copyDB!" == "y" SET copyDB=Y

:STARTSETDEFAULTS
REM -----------------------------------------------------------------
REM Set Defaults
REM -----------------------------------------------------------------
IF !dumpData! == Y SET needData=Y
IF !loadData! == Y SET needData=Y
SET DLC11=C:\Progress\OE116
SET DLC10=C:\Progress\Openedge
SET DLC=%DLC11%
SET topDir=c:\asigui
SET dbDir=\db
SET adminDir=\admin
SET dataDir=\data
SET combDir=
SET stFileDir=
SET dfFileDir=
SET combDbName=advantzware
SET combStFile=advantzware.st
SET combDbDf=advantzware.df
SET splitDir=
SET dataDirASI=\asi
SET dataDirNOS=\nosweat
SET dataDirEMP=\emptrack
SET dataDirJOB=\jobs
SET dataDirRFQ=\rfq
SET dataDirHLP=\asihelp
SET dataDirASN=\asinos
SET dbConnASI=-H db1 -S 2800 -N tcp
SET dbConnNOS=-H db1 -S 2802 -N tcp
SET dbConnEMP=-H db1 -S 2803 -N tcp
SET dbConnJOB=-H db1 -S 2804 -N tcp
SET dbConnRFQ=-H db1 -S 2806 -N tcp
SET dbConnHLP=-H db1 -S 2801 -N tcp
SET dbConnASN=-H db1 -S 2805 -N tcp -ld asinos
SET startTime=%time%
:ENDSETDEFAULTS

GOTO :DISPVALUES

:ENTVALUES
REM -----------------------------------------------------------------
REM Let user set/confirm parameters
REM -----------------------------------------------------------------
CLS
ECHO If these values are correct, just press Enter
SET /P DLC11="Enter the Progress 11 directory (%DLC11%): "
SET /P DLC10="Enter the Progress 10 directory (%DLC10%): "
SET /P TopDir="Enter the top (root) level directory for Advantzware (%topDir%): " 

:STARTSETLOAD:
IF NOT "!loadData!" == "Y" (
    IF NOT "!copyData!" == "Y" (
        GOTO :ENDSETLOAD
    )
)
SET /P dbDir="Enter the directory where databases are located (%dbDir%): "
SET /P adminDir="Enter the db admin files directory (%adminDir%): "
SET /P combDir="Enter the subdirectory for the new combined database (%combDir%): "
SET /P stFileDir="Enter the admin subdirectory where .st files are located (%stFileDir%): "
SET /P dfFileDir="Enter the admin subdirectory where .df files are location (%dfFileDir%): "
SET /P combDbName="Enter the physical file name of the new combined database (%combDbName%): "
SET /P combStFile="Enter the filename of the combined db structure file (%combStFile%): "
SET /P combDbDf="Enter the filename of the combined db .df file (%combDbDf%):
:ENDSETLOAD

:STARTSETCOPY
SET /P copyDir="Enter the subdirectory for the new database copy (%copyDir%): "
SET /P copyDbName="Enter the physical file name of the new database copy (%copyDbName%): "
:ENDSETCOPY

:STARTSETDATA
IF NOT "needData" == "Y" GOTO :ENDSETDATA
SET /P dataDir="Enter the directory that will contain data dump files (%dataDir%): "
SET /P splitDir="Enter the subdirectory for the old ASI databases (%splitDir%): "t
SET /P dataDirASI="Enter the subdir that will contain data dumps from the asi DB (%dataDirASI%): "
SET /P dataDirNOS="Enter the subdir that will contain data dumps from the nosweat DB (%dataDirNOS%): "
SET /P dataDirEMP="Enter the subdir that will contain data dumps from the emptrack DB (%dataDirEMP%): "
SET /P dataDirJOB="Enter the subdir that will contain data dumps from the jobs DB (%dataDirJOB%): "
SET /P dataDirRFQ="Enter the subdir that will contain data dumps from the rfq DB (%dataDirRFQ%): "
SET /P dataDirHLP="Enter the subdir that will contain data dumps from the asihelp DB (%dataDirHLP%): "
SET /P dataDirASN="Enter the subdir that will contain data dumps from the addon/nosweat DB (%dataDirASN%): "
SET /P dbConnASI="Enter the connection parameter string for the asi database (%dbConnASI%): "
SET /P dbConnNOS="Enter the connection parameter string for the nosweat database (%dbConnNOS%): "
SET /P dbConnEMP="Enter the connection parameter string for the emptrack database (%dbConnEMP%): "
SET /P dbConnJOB="Enter the connection parameter string for the jobs database (%dbConnJOB%): "
SET /P dbConnRFQ="Enter the connection parameter string for the rfq database (%dbConnRFQ%): "
SET /P dbConnHLP="Enter the connection parameter string for the asihelp database (%dbConnHLP%): "
SET /P dbConnASN="Enter the connection parameter string for the addon/nosweat database (%dbConnASN%): "
:ENDSETDATA

REM -----------------------------------------------------------------
REM Display program options
REM -----------------------------------------------------------------
:DISPVALUES
CLS
ECHO These values will be used to create the new database:

:STARTDISPLOAD:
IF NOT "!createDB!" == "Y" (
    IF NOT "!copyDB!" == "Y" (
        GOTO :ENDDISPLOAD
    )
)
ECHO Progress Directory  : !DLC11!
ECHO Progress 10 Dir     : !DLC10!
ECHO Root (top) Directory: !topDir!
ECHO Database Directory  : !topDir!!dbDir!
ECHO DB Admin Directory  : !TopDir!!dbDir!!adminDir!
ECHO Comb DB Directory   : !TopDir!!dbDir!!combDir!
ECHO .st File Location   : !TopDir!!dbDir!!adminDir!!stFileDir!
ECHO .df File Location   : !TopDir!!dbDir!!adminDir!!dfFileDir!
ECHO Comb DB filename    : !TopDir!!dbDir!!combDir!\!combDbName!.db
ECHO .st filename        : !TopDir!!dbDir!!adminDir!!stFileDir!\!combStFile!
ECHO .df filename        : !TopDir!!dbDir!!adminDir!!dfFileDir!\!combDbDf!
:ENDDISPLOAD

:STARTDISPCOPY
IF NOT "!copyData!" == "Y" GOTO :ENDDISPCOPY
ECHO Copy DB Directory   : !TopDir!!dbDir!!copyDir!
ECHO Copy DB filename    : !TopDir!!dbDir!!copyDir!\!copyDbName!.db
:ENDDISPCOPY

:STARTDISPDATA
IF NOT "!needData!" == "Y" GOTO :ENDDISPDATA
ECHO .
ECHO Datadump Directory  : %TopDir%%dbDir%%adminDir%%dataDir%
ECHO Old DB Subdirectory : %TopDir%%dbDir%%splitDir%
ECHO asi Dump Directory  : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirASI%
ECHO nosweat Dump Dir    : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirNOS%
ECHO emptrack Dump Dir   : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirEMP%
ECHO jobs Dump Dir       : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirJOB%
ECHO rfq Dump Dir        : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirRFQ%
ECHO asihelp Dump Dir    : %TopDir%%dbDir%%adminDir%%dataDir%%dataDirHLP%
ECHO add/nosweat Dump Dir: %TopDir%%dbDir%%adminDir%%dataDir%%dataDirASN%
ECHO asi Conn Parms      : %dbConnASI%
ECHO nosweat Conn Parms  : %dbConnNOS%
ECHO emptrack Conn Parms : %dbConnEMP%
ECHO jobs Conn Parms     : %dbConnJOB%
ECHO rfq Conn Parms      : %dbConnRFQ%
ECHO asihelp Conn Parms  : %dbConnHLP%
ECHO add/nos Conn Parms  : %dbConnASN%
:ENDDISPDATA

ECHO.
SET /P isCorrect="Are these values correct? (Y/N): "
IF "!isCorrect!" == "y" SET isCorrect=Y
IF NOT "!isCorrect!" == "Y" GOTO :ENTVALUES

GOTO :ERRCHECK

:ERRCHECK
CLS
IF NOT EXIST "!DLC11!" (
    ECHO Progress 11 file not in location specified
    SET isError=Y
    )
IF NOT EXIST "!DLC10!" (
    ECHO Progress 10 file not in location specified
    SET isError=Y
    )
IF NOT EXIST "!topDir!!dbDir!" (
    ECHO Database directory does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!" (
    ECHO DBAdmin directory does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!" (
    ECHO Data dump directory does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!combDir!" (
    ECHO Combined DB directory does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!stFileDir!\!combStFile!" (
    ECHO Structure file not in location specified
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dfFileDir!\!combDbDf!" (
    ECHO .DF file not in location specified
    SET isError=Y
    )
IF EXIST "!TopDir!!dbDir!!combDir!!combDbName!.db" (
    ECHO Combined database name already exists
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!" (
    ECHO Old DB subdirectory does not exist
    SET isError=Y
    )

IF NOT "!needData!" == "Y" GOTO :ENDERRCHECK

IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirASI!" (
    ECHO Directory for asi data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirNOS!" (
    ECHO Directory for nosweat data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirEMP!" (
    ECHO Directory for emptrack data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirJOB!" (
    ECHO Directory for jobs data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirRFQ!" (
    ECHO Directory for rfq data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirHLP!" (
    ECHO Directory for asihelp data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!adminDir!!dataDir!!dataDirASN!" (
    ECHO Directory for addon/nosweat data dump does not exist
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\asi.db" (
    ECHO Database asi not in expected location
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\nosweat.db" (
    ECHO Database nosweat not in expected location
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\asihelp.db" (
    ECHO Database asihelp not in expected location
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\emptrack.db" (
    IF NOT EXIST "!TopDir!!dbDir!!splitDir!\addon\emptrack.db" (
        ECHO Database emptrack not in expected location
        SET isError=Y
        )
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\jobs.db" (
    IF NOT EXIST "!TopDir!!dbDir!!splitDir!\addon\jobs.db" (
        ECHO Database emptrack not in expected location
        SET isError=Y
        )
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\rfq.db" (
    IF NOT EXIST "!TopDir!!dbDir!!splitDir!\addon\rfq.db" (
        ECHO Database emptrack not in expected location
        SET isError=Y
        )
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\addon\nosweat.db" (
    ECHO Database emptrack not in expected location
    SET isError=Y
    )
IF NOT EXIST "!TopDir!!dbDir!!splitDir!\emptrack.db" (
    IF NOT EXIST "!TopDir!!dbDir!!splitDir!\addon\emptrack.db" (
        ECHO Database emptrack not in expected location
        SET isError=Y
        )
    )
:ENDERRCHECK
IF "!isError!" == "Y" (
    ECHO Error found; returning to value assignments
    PAUSE
    GOTO :ENTVALUES
    )

:STARTCREATEDB
REM -----------------------------------------------------------------
REM Create combined (empty)
REM -----------------------------------------------------------------
IF NOT "!createDB!" == "Y" (
    IF NOT "!copyDB!" == "Y" (
        GOTO :ENDCREATEDB
    )
)

%TopDir%
CD %TopDir%%DbDir%%CombDir%
SET pctcomp=0
CLS
ECHO Building combined database...
ECHO   Creating structure file
ECHO     Percent complete = %pctComp%%%

CALL !TopDir!!DbDir!!adminDir!!stFileDir!\buildStruct.bat

:LOOP1
PING localhost -n 6 > NUL
IF EXIST "!combDbName!.b1" SET pctComp=5
IF EXIST "!combDbName!.b2" SET pctComp=10
IF EXIST "!combDbName!.d1" SET pctComp=15
IF EXIST "!combDbName!.d2" SET pctComp=20
IF EXIST "!combDbName!.a2" SET pctComp=25
IF EXIST "!combDbName!.a2" SET pctComp=30
IF EXIST "!combDbName!_7.d1" SET pctComp=35
IF EXIST "!combDbName!_7.d2" SET pctComp=40
IF EXIST "!combDbName!_7.d3" SET pctComp=45
IF EXIST "!combDbName!_7.d4" SET pctComp=50
IF EXIST "!combDbName!_7.d5" SET pctComp=55
IF EXIST "!combDbName!_7.d6" SET pctComp=60
IF EXIST "!combDbName!_8.d1" SET pctComp=70
IF EXIST "!combDbName!_8.d2" SET pctComp=75
IF EXIST "!combDbName!_8.d3" SET pctComp=80
IF EXIST "!combDbName!_8.d4" SET pctComp=90
IF EXIST "!combDbName!_8.d5" SET pctComp=95
IF EXIST "!combDbName!_8.d6" SET pctComp=100
IF !pctComp! == 100 (
    CLS
    ECHO Building combined database...
    ECHO   Creating structure file
    ECHO     Percent complete = !pctComp!%%
    GOTO :METADATA
    )
CLS
ECHO Building combined database...
ECHO   Creating structure file
ECHO     Percent complete = !pctComp!%%
GOTO :LOOP1

:METADATA
ECHO   Copying empty metadata
CALL !TopDir!!DbDir!!adminDir!!stFileDir!\copyMetadata.bat

ECHO   Loading file structure
START /min "Load DF" /wait !DLC11!\bin\_progres -b -db %TopDir%%DbDir%%CombDir%\!combDbName! -1 -p !TopDir!!dbDir!!adminDir!!dfFileDir!\runloaddf.r -param "!TopDir!!dbDir!!adminDir!!dfFileDir!\!combDbDf!"

:ENDCREATEDB

:STARTDUMPLOAD
REM -----------------------------------------------------------------
REM Build dump and load script (.bat) files
REM -----------------------------------------------------------------
CLS
ECHO Creating batch files for dump/load
SET DLC=!DLC10!
!DLC10!\bin\_progres -b -rx -db asi !dbConnASI! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db nosweat !dbConnNOS! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db asihelp !dbConnHLP! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db emptrack !dbConnEMP! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db jobs !dbConnJOB! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db rfq !dbConnRFQ! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"
!DLC10!\bin\_progres -b -rx -db nosweat !dbConnASN! -p !TopDir!!dbDir!!adminDir!\buildDumpLoad.p -param "!DLC10!,!DLC11!,!TopDir!!dbDir!!combDir!\!combDbName!.db,!TopDir!!dbDir!!adminDir!!dataDir!"

REM -----------------------------------------------------------------
REM Dump data from split DBs
REM -----------------------------------------------------------------
IF NOT !dumpData! == Y GOTO :ENDDUMPDATA

CALL !TopDir!!dbDir!!adminDir!!dataDir!\asi\dumpasi.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\nosweat\dumpnosweat.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\asihelp\dumpasihelp.bat
CD addon
CALL !TopDir!!dbDir!!adminDir!!dataDir!\emptrack\dumpemptrack.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\jobs\dumpjobs.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\rfq\dumprfq.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\asinos\dumpnosweat.bat
CD ..
:ENDDUMPDATA

REM -----------------------------------------------------------------
REM Load data from dump files
REM -----------------------------------------------------------------
IF NOT !loadData! == Y GOTO :ENDLOADDATA
SET DLC=!DLC11!
CALL !TopDir!!dbDir!!adminDir!!dataDir!\asi\loadasi.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\nosweat\loadnosweat.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\asihelp\loadasihelp.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\emptrack\loademptrack.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\jobs\loadjobs.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\rfq\loadrfq.bat
CALL !TopDir!!dbDir!!adminDir!!dataDir!\asinos\loadnosweat.bat
ECHO Rebuilding indexes...
CALL !DLC11!\bin\proutil !TopDir!!dbDir!!combDir!\!combDbName!.db -C idxbuild all -B 1024 -TB 31 -TM 32 > NUL

:ENDLOADDATA

:ENDDUMPLOAD 

:EXIT
ECHO .
ECHO Start Time: !startTime!
ECHO End Time  : %time%
PAUSE
EXIT /b