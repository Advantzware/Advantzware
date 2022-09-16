/*custom/bf-userWindow.i 
Purpose - Create the records of bf-userWindow on close of window trigger when user resize the window.
*/

DEFINE BUFFER bf-userWindow FOR userWindow.

DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
  
/* If the window is off scrren or 
      minimized  OR 
      there is NO change IN SIZE AND position
      then dont save the position of WINDOW */
      
IF  VALID-HANDLE({&WINDOW-NAME}) AND 
    {&WINDOW-NAME}:X < (SESSION:WORK-AREA-WIDTH-PIXELS - {&WINDOW-NAME}:width-PIXELS) AND 
    {&WINDOW-NAME}:Y < (SESSION:WORK-AREA-HEIGHT-PIXELS - {&WINDOW-NAME}:HEIGHT-PIXELS) AND 
    {&window-name}:WINDOW-STATE NE 2 AND
    (deOrigWinWidth  NE {&WINDOW-NAME}:WIDTH  OR
     deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT OR 
     deOrigX         NE {&WINDOW-NAME}:X      OR 
     deOrigY         NE {&WINDOW-NAME}:Y) 
    THEN
    DO:
        cFileName = ENTRY(1, THIS-PROCEDURE:FILE-NAME, ".").
        FIND FIRST bf-userWindow EXCLUSIVE-LOCK 
            WHERE bf-userWindow.usrId     EQ USERID('ASI')
            AND bf-userWindow.programName EQ cFileName 
            NO-ERROR.      
            IF AVAILABLE bf-userWindow 
                THEN    
            DO:                   
                /*  userWindow.state = 1 => window is saved as maximized
                    userWindow.state = 0 => window is saved as custom size
                    userWindow.state = 3 => window is saved as normal restored */  
                    ASSIGN 
                        bf-userWindow.winWidth  = {&WINDOW-NAME}:WIDTH 
                        bf-userWindow.winHeight = {&WINDOW-NAME}:HEIGHT  
                        bf-userWindow.winXpos   = {&WINDOW-NAME}:X
                        bf-userWindow.winYpos   = {&WINDOW-NAME}:Y           
                        bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
                        bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
                    .
                
                IF bf-userWindow.winXpos LT 1 THEN
                    bf-userWindow.winXpos = 1.
                
                IF bf-userWindow.winYpos LT 1 THEN
                    bf-userWindow.winYpos = 1.
            // If window size is changes and it is not maximized then dave the window state as custom             
                IF (deOrigWinWidth  NE {&WINDOW-NAME}:WIDTH OR
                    deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT)  AND
                    {&window-name}:WINDOW-STATE NE 1       THEN 
                    bf-userWindow.state     = 0.
                ELSE
                    bf-userWindow.state     = {&window-name}:window-state. 
            END.         
                            
        ELSE 
        DO:        
            CREATE bf-userWindow.
            ASSIGN 
                bf-userWindow.usrId         = USERID('ASI')
                bf-userWindow.programName   = cFileName
                bf-userWindow.sessionWidth  = SESSION:WIDTH-PIXELS
                bf-userWindow.sessionHeight = SESSION:HEIGHT-PIXELS
                bf-userWindow.winWidth      = {&WINDOW-NAME}:WIDTH 
                bf-userWindow.winHeight     = {&WINDOW-NAME}:HEIGHT  
                bf-userWindow.winXpos       = {&WINDOW-NAME}:X
                bf-userWindow.winYpos       = {&WINDOW-NAME}:Y  
                .

            IF bf-userWindow.winXpos LT 1 THEN
                bf-userWindow.winXpos = 1.
            
            IF bf-userWindow.winYpos LT 1 THEN
                bf-userWindow.winYpos = 1.                
       // If window size is changes and it is not maximized then dave the window state as custom         
                IF (deOrigWinWidth  NE {&WINDOW-NAME}:WIDTH OR
                    deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT)  AND
                    {&window-name}:WINDOW-STATE NE 1  THEN 
                    bf-userWindow.state     = 0.
                ELSE
                    bf-userWindow.state     = {&window-name}:window-state.   
               
        END.           
    END.

