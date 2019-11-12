@echo off

set DLC=c:\progress\oe116_64
set DUMP_INC_CODEPAGE=ISO8859-1
set DUMP_INC_INDEXMODE=active
set DUMP_INC_RENAMEFILE=.\rename.txt
set DUMP_INC_DEBUG=0
set cCompDir=c:\asigui\databases\compile

set cToDB=161400
set cToVer=161400

set cTYPE=asi

set cFromDb=161300
set cFromVer=161300
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\Comp%cFromDb%d.db -1 -p prodict/dump_inc.p > .\dump_inc.log. 

set cFromDb=161200
set cFromVer=161200
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\Comp%cFromDb%d.db -1 -p prodict/dump_inc.p > .\dump_inc.log. 

set cFromDb=161102
set cFromVer=161102
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\Comp%cFromDb%d.db -1 -p prodict/dump_inc.p > .\dump_inc.log. 

set cFromDb=161101
set cFromVer=161101
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\Comp%cFromDb%d.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1611
set cFromVer=161100
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1610
set cFromVer=161000
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1690
set cFromVer=160900
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1689
set cFromVer=160890
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1688
set cFromVer=160880
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1687
set cFromVer=160870
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1686
set cFromVer=160860
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1685
set cFromVer=160850
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1680
set cFromVer=160800
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1670
set cFromVer=160700
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1660
set cFromVer=160600
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%d.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cTYPE=aud

set cFromDb=161300
set cFromVer=161300
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\Comp%cFromDb%a.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=161200
set cFromVer=161200
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\Comp%cFromDb%a.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=161102
set cFromVer=161102
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\Comp%cFromDb%a.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=161101
set cFromVer=161101
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\Comp%cFromDb%a.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1611
set cFromVer=161100
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1610
set cFromVer=161000
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1690
set cFromVer=160900
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1689
set cFromVer=160890
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1688
set cFromVer=160880
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1687
set cFromVer=160870
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1686
set cFromVer=160860
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1685
set cFromVer=160850
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

set cFromDb=1680
set cFromVer=160800
  set DUMP_INC_DFFILE=C:\ASIgui\Repositories\Advantzware\Deployment\Patch\Structure\DFFiles\%cType%%cFromVer%00-%cToVer%00.df
  echo Generating %DUMP_INC_DFFILE%...
  %DLC%/bin/_progres -b -db %cCompDir%\comp%cToDB%a.db -1 -db %cCompDir%\%cType%Comp%cFromDb%.db -1 -p prodict/dump_inc.p >> .\dump_inc.log. 

echo End delta build.
pause 
