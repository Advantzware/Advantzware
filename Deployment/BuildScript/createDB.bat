CD %1%
CALL prostrct create %2% asi.st
CALL procopy c:\progress\oe116_64\empty4 %2%
CALL proutil %2% -C enablelargefiles
pause
CD ..
