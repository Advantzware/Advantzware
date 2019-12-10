:: Start prowin session, might be 32 or 64 bit version
::
if exist %dlc%\bin\prowin32.exe set prowin=%dlc%\bin\prowin32.exe
if exist %dlc%\bin\prowin.exe set prowin=%dlc%\bin\prowin.exe

:: Copy files to db folder
::
copy qb.st ..\db
copy qb.df ..\db

:: Create db 
::
if not exist ..\db md ..\db
cd ..\db
%PROWIN% -p ..\bin\create_db.p
cd ..\bin
