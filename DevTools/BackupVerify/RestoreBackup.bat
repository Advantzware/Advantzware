Path=C:\Progress\OE116_64\BIN;C:\Progress\OE116_64\oebpm\server\bin;C:\Progress\OE116_64\PERL\BIN;C:\Program Files\^
Microsoft MPI\Bin\;;C:\ProgramData\Oracle\Java\javapath;C:\Program Files (x86)\PHP\v5.5;C:\Program Files (x86)\Goog^
le\Chrome\Application;;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShel^
l\v1.0\;C:\Program Files\Amazon\cfn-bootstrap\;C:\Program Files\Git\cmd;C:\GnuWin32\bin;C:\Program Files\MySQL\MySQ^
L Server 5.1\bin;C:\pdrive\ASI\Repositories\Possenet\src\adebuild;C:\Program Files\PuTTY\;C:\Program Files (x86)\Mi^
crosoft SQL Server\140\Tools\Binn\;C:\Program Files\Microsoft SQL Server\140\Tools\Binn\;C:\Program Files (x86)\Mic^
rosoft SQL Server\140\DTS\Binn\;C:\Program Files\Microsoft SQL Server\140\DTS\Binn\;C:\Program Files\Microsoft SQL^
Server\Client SDK\ODBC\130\Tools\Binn\;C:\Program Files (x86)\Microsoft SQL ^Server\Client SDK\ODBC\130\Tools\Bin\;^
C:\Program Files (x86)\Microsoft SQL Server\140\Tools\Binn\ManagementStudio\

SET DLC=C:\Progress\OE116_64
SET LIB=C:\Progress\OE116_64\LIB;
SET OEM=C:\Progress\OE116_64oemgmt
SET OEMWRKDIR=C:\Progress\OE116_64wrk_oemgmt
SET OPENSSL_CONF=C:\Progress\OE116_64\keys\policy\pscpki.cnf
SET WRKDIR=C:\Progress\OE116_~1
REN asiProd.bak xAsiProd.bak
del /q c:\asigui\databases\customer\backupTest\asiprod*
REN xAsiProd.bak asiProd.bak

REM Rename to .gz and try to uncompress, if it fails, the rename to asiProd.bak should succeed
REN asiProd.bak asiProd.bak.gz
c:\temp\wade\gzip\bin\gzip -d asiprod.bak.gz
REN asiProd.bak.gz asiProd.bak > NUL 2> Null

copy /y c:\asigui\databases\customer\backupTest\SaveAsiProd.st c:\asigui\databases\customer\backupTest\asiProd.st
"%DLC%\bin\_dbutil" prostrct create c:\asigui\databases\customer\backupTest\asiProd c:\asigui\databases\customer\backupTest\asiProd.st 2> restoreErrs.txt > restore.txt
"%DLC%\bin\_proutil" c:\asigui\databases\customer\backupTest\asiProd -C enableLargeFiles 2>> restoreErrs.txt >> restore.txt
"%DLC%\bin\_dbutil" prorest c:\asigui\databases\customer\backupTest\asiProd c:\asigui\databases\customer\backupTest\asiProd.bak 2>> restoreErrs.txt >> restore.txt
if %ERRORLEVEL% == 0 GOTO continue
if NOT %ERRORLEVEL% == 0 GOTO error

:continue

  REM echo succeeded
  REM echo "Database restore succeeded." >> c:\asigui\databases\customer\backupTest\backupReport.txt
REM "%DLC%\bin\_proutil" c:\asigui\databases\customer\backupTest\asiProd -C dbanalys > c:\asigui\databases\customer\backupTest\dbanalysis.txt  2>> restoreErrs.txt >> restore.txt
"%DLC%\bin\prowin.exe" -b -db c:\asigui\databases\customer\backupTest\asiProd -1 -p firstOrder.p 2>> restoreErrs.txt >> restore.txt
  REM find /i "oe-ordl   " c:\asigui\databases\customer\backupTest\dbanalysis.txt >> c:\asigui\databases\customer\backupTest\backupReport.txt
  REM type firstOrderReport.txt >> c:\asigui\databases\customer\backupTest\backupReport.txt
  
  goto exit

:error
  rem  echo Failed
  echo "Database restore failed!!!" >> c:\asigui\databases\customer\backupTest\backupReport.txt

:exit
 