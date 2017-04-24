@echo off
echo ABHACK old version remover
echo.
echo This script will remove all old ABHack files
echo that were made obsolete in the 20-DEC-2006 update.
echo.
echo Press ctrl-c if you want to cancel, otherwise press enter.
echo.
pause

if exist ..\ABHackSleepUntilPrintable.p del ..\ABHackSleepUntilPrintable.p
if exist ..\abhackwin.gvarDump.txt      del ..\abhackwin.gvarDump.txt     
if exist ..\ABHackWin.readme.pdf        del ..\ABHackWin.readme.pdf       
if exist ..\ABHackWin.readme.txt        del ..\ABHackWin.readme.txt       
if exist ..\ABHackWin.w                 del ..\ABHackWin.w                
if exist ..\ABHackWin.wrx               del ..\ABHackWin.wrx              
if exist ..\ABHackWin.w~                del ..\ABHackWin.w~               
if exist ..\attrTabCompletion.txt       del ..\attrTabCompletion.txt      
if exist ..\attrTabCompletion10.txt     del ..\attrTabCompletion10.txt    
if exist ..\attrTabCompletion9.txt      del ..\attrTabCompletion9.txt     
if exist ..\cleanup.bat                 del ..\cleanup.bat                
if exist ..\closeButton.bmp             del ..\closeButton.bmp            
if exist ..\customAPITooltip.txt        del ..\customAPITooltip.txt       
if exist ..\customTabCompletion.txt     del ..\customTabCompletion.txt    
if exist ..\procEditorList.w            del ..\procEditorList.w           
if exist ..\procEditorList.wrx          del ..\procEditorList.wrx         
if exist ..\procEditorList.w~           del ..\procEditorList.w~          
if exist ..\procEditorTooltip.w         del ..\procEditorTooltip.w        
if exist ..\procEditorTooltip.wrx       del ..\procEditorTooltip.wrx      
if exist ..\procEditorTooltip.w~        del ..\procEditorTooltip.w~       
if exist ..\procEditorTriggers.p        del ..\procEditorTriggers.p       
if exist ..\refineProgressDoc.p         del ..\refineProgressDoc.p        
if exist ..\refineProgressDoc2.p        del ..\refineProgressDoc2.p       
if exist ..\tux.bmp                     del ..\tux.bmp                    
if exist ..\tux.ico                     del ..\tux.ico                    
if exist ..\tuxOn.bmp                   del ..\tuxOn.bmp                  

echo.
echo Old files ares cleaned up. You do not need to run this script again. 
echo.
pause
