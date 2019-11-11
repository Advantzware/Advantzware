SETLOCAL EnableDelayedExpansion
for /F %%f in (dbverlist.txt) do (
    CD comp
    CALL prostrct create COMP%%fd comp.st
    CALL procopy c:\progress\oe116_64\empty4 COMP%%fd
    CALL prowin -db c:\asigui\databases\comp\COMP%%fd -1 -p ..\loadcompdf.p 
    IF EXIST c:\asigui\databases\comp\schema\COMP%%fa.df (
        CALL prostrct create COMP%%fa audit.st
        CALL procopy c:\progress\oe116_64\empty4 COMP%%fa
        CALL prowin -db c:\asigui\databases\comp\COMP%%fa -1 -p ..\loadcompdf.p 
        )
    CD ..
    )

:end
