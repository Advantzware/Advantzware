/*custom/bf-userWindow.i 
Purpose - Create the records of bf-userWindow on close of window trigger when user resize the window.
*/

DEFINE BUFFER bf-userWindow FOR userWindow.

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.

cFileName = ENTRY(1, THIS-PROCEDURE:FILE-NAME, ".").

FIND FIRST bf-userWindow EXCLUSIVE-LOCK 
     WHERE bf-userWindow.usrId       EQ USERID('ASI')
       AND bf-userWindow.programName EQ cFileName 
     NO-ERROR.      
IF AVAILABLE bf-userWindow THEN DO:           
    ASSIGN 
        bf-userWindow.winWidth      = {&WINDOW-NAME}:WIDTH 
        bf-userWindow.winHeight     = {&WINDOW-NAME}:HEIGHT  
        bf-userWindow.winXpos       = {&WINDOW-NAME}:X
        bf-userWindow.winYpos       = {&WINDOW-NAME}:Y
        bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
        bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
        .    
END.                            
ELSE DO:        
    CREATE bf-userWindow.
    ASSIGN 
        bf-userWindow.winWidth      = {&WINDOW-NAME}:WIDTH 
        bf-userWindow.winHeight     = {&WINDOW-NAME}:HEIGHT  
        bf-userWindow.winXpos       = {&WINDOW-NAME}:X
        bf-userWindow.winYpos       = {&WINDOW-NAME}:Y
        bf-userWindow.usrId         = USERID('ASI')
        bf-userWindow.programName   = cFileName
        bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
        bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
        .
END.      