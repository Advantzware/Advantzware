REM 
REM ================================================================================ 
REM ================ Get Information needed from user           ====================
REM ================ Patch created to be placed next to rcode   ====================
REM ================ containing resources_version.zip, programs_version.zip ========
REM ================ If necessary install ASI utilities         ====================
REM ================ and progress-pre.ini and progress-post.ini ====================
REM ================ Steps:                                     ====================
REM ================ Compress Programs, Resources, Utils        ====================
REM ================ and progress-pre.ini and progress-post.ini ====================
set sourceDir=%1
CLS
ECHO Install Version is %InstallVersion%
	
SET /P InstallVersion="Enter the version number for this patch: "
SET /P askUpgrade="Do you wish to continue to create patch for version  %InstallVersion%?"
SET /P compareVersion="Enter the version number for file comparison with this patch: "
IF NOT %askUpgrade% == Y DO GOTO :exit


mkdir ..\PATCH%InstallVersion%%sourceDir%
copy *.p ..\PATCH%InstallVersion%%sourceDir%
echo %InstallVersion% > asiversion.txt
copy *.txt ..\PATCH%InstallVersion%%sourceDir%
copy *.bat ..\PATCH%InstallVersion%%sourceDir%
xcopy /ad /s * ..\PATCH%InstallVersion%%sourceDir%
cd ..\PATCH%InstallVersion%%sourceDir%


del /s /q ./programs
del /s /q ./Resources

md .\programs
xcopy /s /E p:\asi10test\branch\master%sourceDir%\rcode\*.* .\programs

md .\Resources
xcopy /s /E p:\asi10test\branch\master\resources\*.* .\Resources

IF NOT EXIST .\programs\addon\touch\copynote.r (
  ECHO addon/touch/copynote.r not found, aborting script
  GOTO :ERROR
)

md .\Deltas
md .\Deltas\addon
md .\DataUpdates
md .\addon

del programs.7z
del resources.7z

.\7-zip\7z a -r programs.7z .\programs
.\7-zip\7z a -r resources.7z .\resources
.\7-zip\7z a -r programs.7z .\programs
.\7-zip\7z a -r resources.7z .\resources

REM Visually compare file list changes between versions
cd resources
forfiles /s /m *.* /c "cmd /c echo @relpath" > ..\resourceslist.txt
cd ..
cd programs
forfiles /s /m *.* /c "cmd /c echo @relpath" > ..\programslist.txt
cd ..

"C:\Program Files\WinMerge\WinMergeU.exe" programslist.txt ..\Patch%compareVersion%%sourceDir%\programslist.txt
"C:\Program Files\WinMerge\WinMergeU.exe" resourceslist.txt ..\Patch%compareVersion%%sourceDir%\resourceslist.txt

copy p:\asi10test\branch\develop\resources\stdMenu\*.d .
copy p:\asi10test\branch\develop\resources\stdMenu\addon\*.d .\addon

copy /Y p:\asi10test\branch\develop\resources\stdMenu\menu.* .
copy /Y p:\asi10test\branch\develop\resources\stdMenu\addon\menu.* .\addon

copy /Y p:\asi10test\branch\develop\resources\stdMenu\deltas\*.df .\Deltas
copy /Y p:\asi10test\branch\develop\resources\stdMenu\deltas\addon\*.df .\Deltas\addon

copy /Y p:\asi10test\branch\develop\resources\stdMenu\dataUpdates\*.* .\dataUpdates

REM Compare .d files with earlier release
for %%a IN (*.d) do (
  "C:\Program Files\WinMerge\WinMergeU.exe" %%a ..\Patch%compareVersion%%sourceDir%\%%a
)

REM Compare menu files with earlier release
for %%a IN (menu.*) do (
  "C:\Program Files\WinMerge\WinMergeU.exe" %%a ..\Patch%compareVersion%%sourceDir%\%%a
)

del /s /q .\programs
del /s /q .\resources


rem c:\progress\openedge\bin\prowin32 -b -pf p:\asi10test\rco1010\nosweatbuild.pf -p dumpmenufe.p






GOTO :NORMALCOMPLETION

:ERROR
CLS
ECHO An error occurred, aborting install...
PAUSE 
exit

:NORMALCOMPLETION
REM clean up and exit
ECHO Script completed normally, Press spacebar to exit
PAUSE

goto exit
:done
REM nothing to do
:exit
