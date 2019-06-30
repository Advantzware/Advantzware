@echo off

rem mailx.bat
rem
rem mailx person@domain "subject line"
rem

call #ENVPT3#\bin\protopenv.bat

rem	set these in %PROTOP%\bin\localenv.bat so that they do not get overwritten by updates
rem
rem	set FROM=protop@custid.com
rem	set RPLY=noreply@custid.com
rem	set MSRV=smtp.custid.com

REM for debugging
REM echo %PROTOP%\ubin\blat.exe - -to %1 -from %FROM% -f %RPLY% -server %MSRV% -subject %2
REM echo %PROTOP%\ubin\blat.exe - -to %1 -from %FROM% -f %RPLY% -server %MSRV% -subject %2 >> %TMPDIR%\mailx.log 2>&1
 
%PROTOP%\ubin\blat.exe - -q -to %1 -from %FROM% -f %RPLY% -server %MSRV% -subject %2
