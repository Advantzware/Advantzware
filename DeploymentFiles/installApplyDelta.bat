REM applyDeltas.bat



SET sitename=%1
SET dbDir=%~f2
SET progressDir=%~f3
SET backupDir=%~f4
SET rootfolder=%5
SET rcodefolder=%6
SET isUpgrade=%7
SET deltaDb=%8
SET asiPF=%9
shift
SET nosweatPF=%9


%progressDir%/bin/_progres -b -p getDbConnection.p -param %asiPF% 
%progressDir%/bin/_progres -b -p getDbConnection.p -param %nosweatPF% 



   
if %deltaDB%==ASI (
   REM Apply delta.df to ASI database
   %progressDir%/bin/_progres -b -p load.p -zn -pf %asipf%
  )
  
if %deltaDB%==Nosweat (
   REM Apply delta.df to NOsweat database
   %progressDir%/bin/_progres -b -p load.p -zn -pf %nosweatpf%
  )	  

