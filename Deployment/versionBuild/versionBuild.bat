@ECHO OFF
SET DLC=C:\Progress\OE116_64
SET DLCBIN=%DLC%\bin
SET /p cCurVer="Enter CURRENT version number (xx.xx.xx) "
SET ver1=%cCurVer:~0,2%
SET ver2=%cCurVer:~3,2%
SET ver3=%cCurVer:~6,2%
SET iCurVer=%ver1%%ver2%%ver3%
SET /p cNewVer="Enter NEW version number (xx.xx.xx) "
SET ver1=%cNewVer:~0,2%
SET ver2=%cNewVer:~3,2%
SET ver3=%cNewVer:~6,2%
SET iNewVer=%ver1%%ver2%%ver3%
SET cCompVer=%ver1%_%ver2%_%ver3%
SET buildDir=\asigui\build
SET compDir=\asigui\databases\comp
SET testDbDir=\asigui\databases\test
SET patchDir=\asigui\Upgrades\PATCH%cNewVer%
SET envDir=\asigui\Environments\%cNewVer%
SET develDir=\asigui\Environments\Devel
SET repoDir=\asigui\Repositories\Advantzware
SET DUMP_INC_CODEPAGE=ISO8859-1
SET DUMP_INC_INDEXMODE=active
SET DUMP_INC_RENAMEFILE=.\rename.txt
SET DUMP_INC_DEBUG=0
SET blog=C:\tmp\VERBUILD-%iNewVer%-%date:~12,2%%date:~4,2%%date:~7,2%-%time:~0,2%%time:~3,2%%time:~6,2%-BASIC.log
SET vlog=C:\tmp\VERBUILD-%iNewVer%-%date:~12,2%%date:~4,2%%date:~7,2%-%time:~0,2%%time:~3,2%%time:~6,2%-VERBOSE.log
SET elog=C:\tmp\VERBUILD-%iNewVer%-%date:~12,2%%date:~4,2%%date:~7,2%-%time:~0,2%%time:~3,2%%time:~6,2%-ERRORS.log
C:
CD %buildDir%
ECHO %cNewVer% > newVer.txt
ECHO .
ECHO Starting at %time% on %date%
ECHO Starting at %time% on %date% > %blog%
ECHO Starting at %time% on %date% > %vlog%
ECHO Starting at %time% on %date% > %elog%
:: Testing - UNcomment to jump to specific section
::GOTO :UpdateRepo

::Section Names
::  DatabaseActivities
::    CreatePatchDir
::    DumpSchema
::    CreateCompDbs
::    MakeTestDbs
::    CreateDeltas
::  EnvironmentActivities
::    CreateNumberedEnv
::    UpdateFiles
::    CompilePrograms
::  PatchActivities
::    CompileAdmin
::    CreateDBDict
::    DumpData
::  FinalizePatch
::    ZipEnv
::    ZipPatch
::    UpdateRepo

:Database Activities
:CreatePatchDir
ECHO Creating New PATCH directories from repository
ECHO Creating New PATCH directories from repository >> %blog%
ECHO Creating New PATCH directories from repository >> %vlog%
:: Test for existence of patch
IF EXIST C:\asigui\upgrades\PATCH%cNewVer%\ (GOTO :PROMPT) ELSE (GOTO :NEXT)
:PROMPT
ECHO The version you are about to create already exists.
SET /p lProceed="Are you sure?: " 
IF /I %lProceed%==Y (GOTO :NEXT) ELSE (GOTO :END)
:NEXT
DEL /S /Q C:%patchDir%\* >> %vlog% 2>> %elog%
DEL /S /Q C:%patchDir%\patch.mft >> %vlog% 2>> %elog%
RMDIR /S /Q C:%patchDir% >> %vlog% 2>> %elog%
MKDIR C:%patchDir% >> %vlog%
XCOPY C:\asigui\Repositories\Advantzware\Deployment\Patch\* C:%patchDir% /E /S /H /C /I >> %vlog%
DEL /S /Q C:%patchDir%\Documentation\DBDict\* >> %vlog%
RMDIR /S /Q C:%patchDir%\Documentation\DBDict >> %vlog%
MKDIR C:%patchDir%\Documentation\DBDict >> %vlog%
DEL /S /Q C:%patchDir%\DataFiles\* >> %vlog%
DEL /S /Q C:%patchDir%\Structure\DFFiles\* >> %vlog%
XCOPY C:\Asigui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\audEmp* C:%patchDir%\Structure\DFFiles /E /S /H /C /I >> %vlog%
DEL /Q C:%patchDir%\patch.mft >> %vlog%
ECHO patchVer=%cNewVer% > C:%patchDir%\patch.mft
ECHO asiDbVer=%cNewVer% >> C:%patchDir%\patch.mft
ECHO audDbVer=%cNewVer% >> C:%patchDir%\patch.mft
IF NOT EXIST C:%patchDir%\Admin\EnvAdmin\prerun%iCurVer%.r (
    COPY /Y C:%patchDir%\prerun.r C:%patchDir%\Admin\EnvAdmin\prerun%iCurVer%.r
    )
ECHO   Directory build complete
ECHO   Directory build complete >> %blog%
ECHO   Directory build complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:DumpSchema
CD %buildDir%
ECHO Dumping schema definitions from DEVEL databases
ECHO Dumping schema definitions from DEVEL databases >> %blog%
ECHO Dumping schema definitions from DEVEL databases >> %vlog%
ECHO   (Press SPACEBAR when prompted - TWICE)
IF EXIST .\advantzware.df (DEL .\advantzware.df)
IF EXIST .\audit.df (DEL .\audit.df)
CALL %DLCBIN%\prowin.exe -basekey INI -ininame versionBuild.ini -pf versionBuildASIDB.pf -p versionBuildDumpASI.p
IF NOT EXIST C:%buildDir%\advantzware.df (
    ECHO advantzware.df did NOT create successfully - TERMINATING BUILD
    ECHO advantzware.df did NOT create successfully - TERMINATING BUILD >> %blog%
    ECHO advantzware.df did NOT create successfully - TERMINATING BUILD >> %vlog%
    GOTO :END
    )
CALL %DLCBIN%\prowin.exe -basekey INI -ininame versionBuild.ini -pf versionBuildAUDDB.pf -p versionBuildDumpAUDIT.p >> %vlog%
IF NOT EXIST C:%buildDir%\audit.df (
    ECHO audit.df did NOT create successfully - TERMINATING BUILD
    ECHO audit.df did NOT create successfully - TERMINATING BUILD >> %blog%
    ECHO audit.df did NOT create successfully - TERMINATING BUILD >> %vlog%
    GOTO :END
    )
ECHO   Schema dump complete
ECHO   Schema dump complete >> %blog%
ECHO   Schema dump complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:CreateCompDbs
ECHO Creating COMP databases
ECHO Creating COMP databases >> %blog%
ECHO Creating COMP databases >> %vlog%
CD %compDir%
IF EXIST COMP%iNewVer%d.lg (DEL COMP%iNewVer%*.*)
ECHO     prostrct create
ECHO     prostrct create >> %blog%
ECHO     prostrct create >> %vlog%
CALL %DLCBIN%\prostrct create COMP%iNewVer%d asi.st >> %vlog%
CALL %DLCBIN%\prostrct create COMP%iNewVer%a audit.st >> %vlog%
ECHO     procopy from empty
ECHO     procopy from empty >> %blog%
ECHO     procopy from empty >> %vlog%
CALL %DLCBIN%\procopy %DLC%\empty4 COMP%iNewVer%d >> %vlog%
CALL %DLCBIN%\procopy %DLC%\empty4 COMP%iNewVer%a >> %vlog%
IF NOT EXIST C:%compDir%\COMP%iNewVer%d.lg (
    ECHO COMP%iNewVer%d did NOT create successfully - TERMINATING BUILD
    ECHO COMP%iNewVer%d did NOT create successfully - TERMINATING BUILD >> %blog%
    ECHO COMP%iNewVer%d did NOT create successfully - TERMINATING BUILD >> %vlog%
    GOTO :END
    )
IF NOT EXIST C:%compDir%\COMP%iNewVer%a.lg (
    ECHO COMP%iNewVer%a did NOT create successfully - TERMINATING BUILD
    ECHO COMP%iNewVer%a did NOT create successfully - TERMINATING BUILD >> %blog%
    ECHO COMP%iNewVer%a did NOT create successfully - TERMINATING BUILD >> %vlog%
    GOTO :END
    )
ECHO     loading schema
ECHO     loading schema >> %blog%
ECHO     loading schema >> %vlog%
ECHO       (wait for next prompt - can take several minutes)
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey INI -ininame versionBuild.ini -db C:%compDir%\COMP%iNewVer%d -1 -p versionBuildLoadASI.p >> %vlog%
CALL %DLCBIN%\prowin.exe -basekey INI -ininame versionBuild.ini -db C:%compDir%\COMP%iNewVer%a -1 -p versionBuildLoadAUDIT.p >> %vlog%
ECHO   COMP DB creation complete
ECHO   COMP DB creation complete >> %blog%
ECHO   COMP DB creation complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:MakeTestDbs
ECHO Creating new TEST DBs
ECHO Creating new TEST DBs >> %blog%
ECHO Creating new TEST DBs >> %vlog%
CD %testDbDir%
IF EXIST TEST%iNewVer%d.lg (ECHO     Stopping/Deleting old copies)
IF EXIST TEST%iNewVer%d.lk (CALL %DLCBIN%\dbman -host localhost -port 20932 -database TEST%iNewVer%d -stop >> %vlog%)
IF EXIST TEST%iNewVer%a.lk (CALL %DLCBIN%\dbman -host localhost -port 20932 -database TEST%iNewVer%a -stop >> %vlog%)
PING localhost -n 20 >> %vlog%
DEL TEST%iNewVer%*.* >> %vlog% 2>> %elog%
ECHO     prostrct create
ECHO     prostrct create >> %blog%
ECHO     prostrct create >> %vlog%
CALL %DLCBIN%\prostrct create TEST%iNewVer%d asi.st >> %vlog%
CALL %DLCBIN%\prostrct create TEST%iNewVer%a audit.st >> %vlog%
ECHO     restoring from DEVEL backup
ECHO     restoring from DEVEL backup >> %blog%
ECHO     restoring from DEVEL backup >> %vlog%
CALL %DLCBIN%\prorest TEST%iNewVer%d C:\asigui\Backups\Databases\TESTDEVELd.bak >> %vlog%
CALL %DLCBIN%\prorest TEST%iNewVer%a C:\asigui\Backups\Databases\TESTDEVELa.bak >> %vlog%
IF NOT EXIST C:%testDbDir%\TEST%iNewVer%d.lg (ECHO TEST%iNewVer%d did NOT create successfully)
IF NOT EXIST C:%testDbDir%\TEST%iNewVer%a.lg (ECHO TEST%iNewVer%a did NOT create successfully)
ECHO   TEST%iNewVer% databases create complete
ECHO   TEST%iNewVer% databases create complete >> %blog%
ECHO   TEST%iNewVer% databases create complete >> %vlog%
ECHO     (Note: be sure to add to OE Explorer when ready)
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:CreateDeltas
CD %buildDir%
ECHO Creating delta .df files for current-old databases
ECHO Creating delta .df files for current-old databases >> %blog%
ECHO Creating delta .df files for current-old databases >> %vlog%
SETLOCAL EnableDelayedExpansion
SET alreadyThere=NO
COPY /Y .\advantzware.df C:!patchDir!\Structure\DFFiles >> %vlog%
COPY /Y .\advantzware.df C:!compDir!\Schema\COMP!iNewVer!d.df >> %vlog%
COPY /Y .\audit.df C:!patchDir!\Structure\DFFiles >> %vlog%
COPY /Y .\audit.df C:!compDir!\Schema\COMP!iNewVer!a.df >> %vlog%
DEL .\advantzware.df >> %vlog%
DEL .\audit.df >> %vlog%
FOR /F %%g IN (versionBuildDbverlist.txt) DO (
    IF %%g == !iNewVer! SET alreadyThere=YES
    )
IF NOT !alreadyThere!==YES ECHO !iNewVer! >> versionBuildDbverlist.txt
FOR /F %%f IN (versionBuildDbverlist.txt) DO (
    IF %%f == !iNewVer! (
        COPY /Y C:!compDir!\Schema\comp!iNewVer!d.df C:!patchDir!\Structure\DFFiles\asi!iNewVer!00.df >> %vlog%
        COPY /Y C:!compDir!\Schema\comp!iNewVer!a.df C:!patchDir!\Structure\DFFiles\aud!iNewVer!00.df >> %vlog%
    ) ELSE (
        SET DUMP_INC_DFFILE=C:!patchDir!\Structure\DFFiles\asi%%f00-!iNewVer!00.df
        !DLCBIN!\_progres -b -db C:!compDir!\COMP!iNewVer!d.db -1 -db C:!compDir!\COMP%%fd.db -1 -p prodict\dump_inc.p
        IF EXIST C:!compDir!\COMP%%fa.db (
            SET DUMP_INC_DFFILE=C:!patchDir!\Structure\DFFiles\aud%%f00-!iNewVer!00.df
            !DLCBIN!\_progres -b -db C:!compDir!\comp!iNewVer!a.db -1 -db C:!compDir!\Comp%%fa.db -1 -p prodict\dump_inc.p >> %vlog%
        )
    )
)
SETLOCAL DisableDelayedExpansion
ECHO   Delta file creation complete
ECHO   Delta file creation complete >> %blog%
ECHO   Delta file creation complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:EnvironmentActivities
:CreateNumberedEnv
ECHO Creating new Environment Directory C:%envDir%
ECHO Creating new Environment Directory C:%envDir% >> %blog%
ECHO Creating new Environment Directory C:%envDir% >> %vlog%
IF EXIST C:%envDir%. (
    ECHO   Deleting existing directory C:%envDir%
    ECHO   Deleting existing directory C:%envDir% >> %blog%
    ECHO   Deleting existing directory C:%envDir% >> %vlog%
    DEL /S /Q C:%envDir%\* >> %vlog% 2>> %elog%
    RMDIR /S /Q C:%envDir% >> %vlog% 2>> %elog%
    )
MKDIR C:%envDir% >> %vlog%
MKDIR C:%envDir%\Override >> %vlog%
MKDIR C:%envDir%\Oversource >> %vlog%
MKDIR C:%envDir%\Programs >> %vlog%
MKDIR C:%envDir%\Resources >> %vlog%
MKDIR C:%envDir%\Source >> %vlog%
ECHO   Copying files from DEVEL environment
ECHO   Copying files from DEVEL environment >> %blog%
ECHO   Copying files from DEVEL environment >> %vlog%
ECHO     CustFiles (may take several minutes)
ECHO     CustFiles (may take several minutes) >> %blog%
ECHO     CustFiles (may take several minutes) >> %vlog%
XCOPY C:%develDir%\Custfiles C:%envDir%\CustFiles /E /S /H /C /I >> %vlog%
ECHO     Schedule
ECHO     Schedule >> %blog%
ECHO     Schedule >> %vlog%
XCOPY C:%develDir%\Schedule C:%envDir%\Schedule /E /S /H /C /I >> %vlog%
ECHO     Users
ECHO     Users >> %blog%
ECHO     Users >> %vlog%
XCOPY C:%develDir%\Users C:%envDir%\Users /E /S /H /C /I >> %vlog%
ECHO   Copying files from REPOSITORY
ECHO   Copying files from REPOSITORY >> %blog%
ECHO   Copying files from REPOSITORY >> %vlog%
ECHO     Source
ECHO     Source >> %blog%
ECHO     Source >> %vlog%
XCOPY C:%repoDir%\Source C:%envDir%\Source /E /S /H /C /I >> %vlog%
ECHO     Resources
ECHO     Resources >> %blog%
ECHO     Resources >> %vlog%
XCOPY C:%repoDir%\Resources C:%envDir%\Resources /E /S /H /C /I >> %vlog%
ECHO   Environment creation complete
ECHO   Environment creation complete >> %blog%
ECHO   Environment creation complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:UpdateFiles
ECHO Updating needed files
ECHO Updating needed files >> %blog%
ECHO Updating needed files >> %vlog%
CALL %DLCBIN%\prowin.exe -basekey INI -ininame versionBuild.ini -pf versionBuildASIDB.pf -p versionBuildFileUpdate.p
ECHO   File update complete
ECHO   File update complete >> %blog%
ECHO   File update complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:CompilePrograms
ECHO Compiling source programs in ENV directory
ECHO Compiling source programs in ENV directory >> %blog%
ECHO Compiling source programs in ENV directory >> %vlog%
CD /asigui/build/fsinstaller
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame progress11.ini -zn -pf client2.pf -p fsInstallerAuto.w -param "C:\asigui\build\fsInstaller\AW%cCompVer%.cfg" 
ECHO   Source compile complete
ECHO   Source compile complete >> %blog%
ECHO   Source compile complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:PatchActivities
:CompileAdmin
ECHO Compiling ADMIN programs (asiLogin, etc.)
ECHO Compiling ADMIN programs (asiLogin, etc.) >> %blog%
ECHO Compiling ADMIN programs (asiLogin, etc.) >> %vlog%
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame versionBuild.ini -zn -pf versionBuildNODB.pf -p versionBuildCompileAdmin.p 
ECHO   Admin file compile complete
ECHO   Admin file compile complete >> %blog%
ECHO   Admin file compile complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:CreateDBDict
ECHO Building HTML data dictionary
ECHO Building HTML data dictionary >> %blog%
ECHO Building HTML data dictionary >> %vlog%
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame versionBuild.ini -pf versionBuildBOTHDB.pf -p HtmlDict.w -param "ShowHtml=false OutputDirectory=C:\asigui\Upgrades\PATCH%cNewVer%\Documentation\DBDict Autorun=true"
ECHO   HTML Dictionary generation complete
ECHO   HTML Dictionary generation complete >> %blog%
ECHO   HTML Dictionary generation complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:DumpData
ECHO Dumping data from selected DEVEL tables
ECHO Dumping data from selected DEVEL tables >> %blog%
ECHO Dumping data from selected DEVEL tables >> %vlog%
CD %buildDir%
CALL %DLCBIN%\prowin.exe -basekey "INI" -ininame versionBuild.ini -pf versionBuildBOTHDB.pf -p versionBuildDumpData.p
ECHO   Data dump complete
ECHO   Data dump complete >> %blog%
ECHO   Data dump complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:FinalizePatch
:ZipEnv
ECHO Compressing distribution files (programs, resources, override)
ECHO Compressing distribution files (programs, resources, override) >> %blog%
ECHO Compressing distribution files (programs, resources, override) >> %vlog%
CD %buildDir%
CALL C:\asigui\Admin\EnvAdmin\7z.exe a -r C:\asigui\upgrades\PATCH%cNewVer%\ProgramFiles\programs.7z C:\asigui\Environments\%cNewVer%\Programs\*.* >> %vlog%
CALL C:\asigui\Admin\EnvAdmin\7z.exe a -r C:\asigui\upgrades\PATCH%cNewVer%\ProgramFiles\resources.7z C:\asigui\Environments\%cNewVer%\Resources\*.* >> %vlog%
CALL C:\asigui\Admin\EnvAdmin\7z.exe a -r C:\asigui\upgrades\PATCH%cNewVer%\ProgramFiles\override.7z C:\asigui\Environments\%cNewVer%\Override\*.* >> %vlog%
ECHO   Compression complete
ECHO   Compression complete >> %blog%
ECHO   Compression complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:ZipPatch
ECHO Creating PATCH zip file
ECHO Creating PATCH zip file >> %blog%
ECHO Creating PATCH zip file >> %vlog%
CD %buildDir%
CALL C:\asigui\Admin\EnvAdmin\7z.exe a -r -tzip C:\asigui\upgrades\PATCH%cNewVer%\PATCH%cCompVer%.zip C:\asigui\upgrades\PATCH%cNewVer%\*.* >> %vlog%
ECHO   Zip file creation complete
ECHO   Zip file creation complete >> %blog%
ECHO   Zip file creation complete >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:UpdateRepo
ECHO Updating repository files
ECHO Updating repository files >> %blog%
ECHO Updating repository files >> %vlog%
MKDIR C:\asigui\Repositories\Advantzware\Deployment\SchemaChanges\%iCurVer%00 >> %vlog% 2>> %elog%
XCOPY C:\asigui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\* C:\asigui\Repositories\Advantzware\Deployment\SchemaChanges\%iCurVer%00 /E /S /H /C /I /Y >> %vlog%
DEL /S /Q C:\Asigui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\*.* >> %vlog%
XCOPY C:%patchDir%\Admin\* C:\asigui\Repositories\Advantzware\Deployment\Patch\Admin /E /S /H /C /I /Y >> %vlog%
XCOPY C:%patchDir%\DataFiles\* C:\asigui\Repositories\Advantzware\Deployment\Patch\DataFiles /E /S /H /C /I /Y >> %vlog%
XCOPY C:%patchDir%\Structure\* C:\asigui\Repositories\Advantzware\Deployment\Patch\Structure /E /S /H /C /I /Y >> %vlog%
COPY /Y C:%patchDir%\patch.mft C:\asigui\Repositories\Advantzware\Deployment\Patch\patch.mft
ECHO   Repository update complete
ECHO   Repository update complete >> %blog%
ECHO   Repository update complete >> %vlog%
ECHO     BE SURE TO STAGE AND PUSH CHANGES TO GITHUB!!!
ECHO     BE SURE TO STAGE AND PUSH CHANGES TO GITHUB!!! >> %blog%
ECHO     BE SURE TO STAGE AND PUSH CHANGES TO GITHUB!!! >> %vlog%
ECHO .
ECHO . >> %blog%
ECHO . >> %vlog%

:Cleanup
CD %buildDir%

:END

ECHO ------------------------------------------------------------------
ECHO BUILD COMPLETE!!!
ECHO   Completed at %time% on %date%
ECHO ------------------------------------------------------------------
ECHO ------------------------------------------------------------------ >> %blog%
ECHO BUILD COMPLETE!!! >> %blog%
ECHO   Completed at %time% on %date% >> %blog%
ECHO ------------------------------------------------------------------ >> %blog%
ECHO ------------------------------------------------------------------ >> %vlog%
ECHO BUILD COMPLETE!!! >> %vlog%
ECHO   Completed at %time% on %date% >> %vlog%
ECHO ------------------------------------------------------------------ >> %vlog%
ECHO .
ECHO --------------------- NEXT STEPS ---------------------------------
ECHO 1 - CREATE ENTRIES IN OE EXPLORER FOR ASI AND AUDIT DBS
ECHO       (use noted ports, or find in advantzware.ini)
ECHO 2 - WHEN PATCH APPROVED, CREATE RELEASE IN GITHUB AND PUBLISH
ECHO 3 - WHEN READY TO PUBLISH, COPY PATCH ZIP FILE TO H: DRIVE AND UPDATE ASIupdate.html
ECHO ------------------------------------------------------------------
ECHO .
 
PAUSE
