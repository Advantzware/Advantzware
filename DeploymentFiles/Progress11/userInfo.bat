REM ================================================================================ 
REM ================ Get Information needed from user           ====================
CLS
REM ECHO Install Version is %InstallVersion%
REM 	

SET /P InstallVersion="Enter the version number for this patch: "
SET /P askUpgrade="Do you wish to continue to install the patch for version  %InstallVersion%?"
IF NOT %askUpgrade% == Y GOTO :exit

SET /P sitename="Enter the site name (no spaces): "
SET /P dbDir="Enter the location of the databases: "
SET /P progressDir="Enter the location of where progress is installed (e.g. c:\progress\openedge): "
SET /P backupDir="Enter the location of the backup folder: "
SET /P rootfolder="Enter the folder above the rcode folder (e.g. N: or e:\asigui)  : "
SET /P rcodefolder="Enter the name of the rcode folder (e.g. rcode) : "

IF NOT EXIST %dbDir% (
  ECHO Location of databases not found, please start script again
  ECHO Press spacebar to continue
  goto :END
)

CALL installPatchv2 %sitename% %dbDir% %progressDir% %backupDir% %rootfolder% %rcodefolder%

:END
:EXIT

