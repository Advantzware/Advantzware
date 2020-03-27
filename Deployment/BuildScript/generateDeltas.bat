@echo off

set DLC=c:\progress\oe116_64
set DUMP_INC_CODEPAGE=ISO8859-1
set DUMP_INC_INDEXMODE=active
set DUMP_INC_RENAMEFILE=.\rename.txt
set DUMP_INC_DEBUG=0
set cCompDir=c:\asigui\databases\comp
set cToDB=200100

SETLOCAL EnableDelayedExpansion

DEL .\dump_inc.log
for /F %%f in (dbverlist.txt) do (
    if %%f == !cToDb! (
        copy /Y !cCompDir!\Schema\comp!cToDb!d.df C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\asi!cToDb!00.df >> .\dump_inc.log
        copy /Y !cCompDir!\Schema\comp!cToDb!a.df C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\aud!cToDb!00.df >> .\dump_inc.log
    ) else (
        set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\asi%%f00-!cToDB!00.df
        echo Generating !DUMP_INC_DFFILE!...
        !DLC!/bin/_progres -b -db !cCompDir!\comp!cToDb!d.db -1 -db !cCompDir!\Comp%%fd.db -1 -p prodict/dump_inc.p >> .\dump_inc.log
        if exist !cCompDir!\Comp%%fa.db (
            set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\aud%%f00-!cToDB!00.df
            echo Generating !DUMP_INC_DFFILE!...
            !DLC!/bin/_progres -b -db !cCompDir!\comp!cToDb!a.db -1 -db !cCompDir!\Comp%%fa.db -1 -p prodict/dump_inc.p >> .\dump_inc.log
        )
    )
)
echo End delta build.
pause 

