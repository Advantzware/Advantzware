@echo off

rem	 bpro -p util/foreachdb.p -param echo > tmp/foreachdb.err 2>&1
rem

cd /d %PROTOP%

call proutil %1 -C dbanalys > dbanalys\%2.dba
