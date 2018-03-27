@ECHO OFF

REM ================ Get Information needed from user           ====================
REM ================ Patch created to be placed next to rcode   ====================
REM ================ containing resources_version.zip, programs_version.zip ========
REM ================ If necessary install ASI utilities         ====================
REM ================ and progress-pre.ini and progress-post.ini ====================
REM ================ Steps:                                     ====================
REM ================ Compress Programs, Resources, Utils        ====================
REM ================ and progress-pre.ini and progress-post.ini ====================

@SET patchTemplateDir=P:\ASI\Admin\Programs\patchBuild\template
@SET DLC10Dir=C:\Progress\OE102B
@SET DLC11Dir=C:\Progress\OE116_64
@SET envDir=P:\ASI\Environments
@SET envObj10=Develop\Object102
@SET envObj11=Develop\Object116
@SET patchTopDir=P:\ASI\Releases

@SET /P patchID="Enter the patch identifier (e.g. 16.4.4): "
@SET /P envParentDir="Enter the enviroment directory for this version: "
@SET /P compareVersion="Enter the patch version for comparison to this patch: "

@SET resourcesDir=%envDir%\%envParentDir%\Develop\Object102\Resources
@SET patchDir=%patchTopDir%\PATCH%patchID%
@SET rcode10Dir=%envDir%\%envParentDir%\%envObj10%\Legacy
@SET rcode11Dir=%envDir%\%envParentDir%\%envObj11%\Legacy

@ECHO .
@ECHO This function will create a patch in folder: %patchDir%
@ECHO using the patch template found in: %patchTemplateDir%
@ECHO with Progress 10 rCode from: %rCode10Dir%
@ECHO and Progress 11 rCode from: %rCode11Dir%
@ECHO and Resources files from: %resourcesDir%
@ECHO .
@ECHO Patch ID %patchID% will be compared with patch %compareVersion%
@ECHO .
@SET /P askUpgrade="Do you wish to continue to create this patch (Y/N)?"

IF NOT %askUpgrade% == Y GOTO :exit

ECHO .
ECHO Creating patch structure...
MKDIR %patchDir% > NUL
XCOPY %patchTemplateDir%\*.* %patchDir% /e /q > NUL
CD %patchDir% > NUL

IF NOT EXIST "%rCode10Dir%\addon\touch\copynote.r" (
  ECHO %rCode10Dir%\addon\touch\copynote.r was not found; aborting script
  pause
  GOTO :ERROR
)

ECHO Compressing files...
.\7-zip\7z a -r programs10.7z %rCode10Dir%\Legacy > NUL
.\7-zip\7z a -r programs11.7z %rCode11Dir%\Legacy > NUL
.\7-zip\7z a -r resources.7z %resourcesDir%\Resources > NUL

ECHO Creating list files...
CD %resourcesDir%
FORFILES /s /m *.* /c "cmd /c echo @relpath" > %patchDir%\resourceslist.txt
CD %rCode10Dir%
FORFILES /s /m *.* /c "cmd /c echo @relpath" > %patchDir%\programslist.txt
CD %patchDir%

ECHO Copying menu and structure files...
COPY %resourcesDir%\stdMenu\*.d . > NUL
COPY %resourcesDir%\stdMenu\addon\*.d .\addon > NUL
COPY /Y %resourcesDir%\stdMenu\menu.* . > NUL
COPY /Y %resourcesDir%\stdMenu\addon\menu.* .\addon > NUL
COPY /Y %resourcesDir%\stdMenu\deltas\*.df .\Deltas > NUL
COPY /Y %resourcesDir%\stdMenu\deltas\addon\*.df .\Deltas\addon > NUL
COPY /Y %resourcesDir%\stdMenu\dataUpdates\*.* .\dataUpdates > NUL

ECHO Comparing files...
"C:\Program Files\ExamDiff Pro\ExamDiff.exe" %patchDir%\programslist.txt %patchTopDir%\PATCH%compareVersion%\programslist.txt /i /w /t /o:%patchTopDir%\PATCH%compareVersion%\programslist.txt
"C:\Program Files\ExamDiff Pro\ExamDiff.exe" %patchDir%\programslist.txt %patchTopDir%\PATCH%compareVersion%\programslist.txt /i /w /t /o:%patchTopDir%\PATCH%compareVersion%\programslist.txt

FOR %%a IN (*.d) DO (
    "C:\Program Files\ExamDiff Pro\ExamDiff.exe" %patchDir%\%%a %patchTopDir%\PATCH%compareVersion%\%%a /i /w /t /o:%patchDir%\DIFF%%a
)

FOR %%a IN (menu.*) DO (
    "C:\Program Files\ExamDiff Pro\ExamDiff.exe" %patchDir%\%%a %patchTopDir%\PATCH%compareVersion%\%%a /i /w /t /o:%patchDir%\DIFF%%a
)

GOTO :NORMALCOMPLETION

:ERROR
CLS
ECHO An error occurred, aborting install...
PAUSE 
EXIT

:NORMALCOMPLETION
REM clean up and exit
ECHO Script completed normally
PAUSE
GOTO EXIT

:DONE
REM nothing to do

:EXIT
