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
IF AVAILABLE bf-userWindow THEN 
DO:



    IF VALID-HANDLE({&WINDOW-NAME}) THEN    
    DO:
       /*  userWindow.state = 1 => window is saved as maximized
           userWindow.state = 0 => window is saved as custom size
           userWindow.state = 3 => window is saved as normal restored */     
        ASSIGN 
            bf-userWindow.winWidth  = {&WINDOW-NAME}:WIDTH 
            bf-userWindow.winHeight = {&WINDOW-NAME}:HEIGHT  
            bf-userWindow.winXpos   = {&WINDOW-NAME}:X
            bf-userWindow.winYpos   = {&WINDOW-NAME}:Y           
            . 
        IF (deOrigWinWidth  NE {&WINDOW-NAME}:WIDTH OR
            deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT)  AND
            {&window-name}:WINDOW-STATE NE 1       THEN 
            bf-userWindow.state     = 0.
        ELSE IF {&window-name}:WINDOW-STATE EQ 2 THEN 
                ASSIGN
                    bf-userWindow.state     = 3
                    bf-userWindow.winWidth  = {&WINDOW-NAME}:WIDTH 
                    bf-userWindow.winHeight = {&WINDOW-NAME}:HEIGHT 
                    .
            ELSE 
                bf-userWindow.state     = {&window-name}:window-state.
      END.
    ASSIGN 
        bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
        bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
        . 
           
END.                            
ELSE 
DO:        
    CREATE bf-userWindow.
    ASSIGN 
        bf-userWindow.usrId         = USERID('ASI')
        bf-userWindow.programName   = cFileName
        bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
        bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
        .
    IF VALID-HANDLE({&WINDOW-NAME}) THEN  
    DO:
        IF (deOrigWinWidth  NE {&WINDOW-NAME}:WIDTH OR
            deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT)  AND
        {&window-name}:WINDOW-STATE NE 1  THEN 
           bf-userWindow.state     = 2.
       ELSE
            bf-userWindow.state     = {&window-name}:window-state.   
        ASSIGN 
            bf-userWindow.winWidth  = {&WINDOW-NAME}:WIDTH 
            bf-userWindow.winHeight = {&WINDOW-NAME}:HEIGHT  
            bf-userWindow.winXpos   = {&WINDOW-NAME}:X
            bf-userWindow.winYpos   = {&WINDOW-NAME}:Y  
            .
      END.
END.      