@ECHO OFF
:: Remove all programs from /Updates dir
cd ..\..\Updates
IF EXIST "..\..\DoNotDelete.txt" (
    GOTO :QUIT
    )

RD /S /Q . > NUL
DEL /S /Q *.* > NUL

:QUIT

EXIT
