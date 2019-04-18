@ECHO OFF
:: Remove all programs from /Updates dir
cd ..\..\Updates
RD /S /Q . > NUL
DEL /S /Q *.* > NUL

