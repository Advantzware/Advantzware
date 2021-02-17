/* custom/resizrs.i*/

ON 'WINDOW-RESTORED':U OF {&WINDOW-NAME}
DO:
    
    IF iWinState NE 2 THEN 
    ASSIGN 
        {&WINDOW-NAME}:HEIGHT = deOrigWinHeight 
        {&WINDOW-NAME}:WIDTH  = deOrigWinWidth
        .    
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}.
    iWinState = {&WINDOW-NAME}:WINDOW-STATE.
END.    
ON 'WINDOW-MAXIMIZED':U OF {&WINDOW-NAME}
DO:
    iWinState = {&WINDOW-NAME}:WINDOW-STATE.
    ASSIGN
        {&WINDOW-NAME}:MAX-HEIGHT   = ?
        {&WINDOW-NAME}:MAX-WIDTH    = ?
        {&WINDOW-NAME}:WINDOW-STATE = WINDOW-MAXIMIZED.
END.
ON 'WINDOW-MINIMIZED':U OF {&WINDOW-NAME}
    DO: 
        iWinState = {&WINDOW-NAME}:WINDOW-STATE.
    END.

/*
ON 'WINDOW-RESTORED':U OF {&WINDOW-NAME}
DO:
IF winstate = 3 THEN ASSIGN winstate = 0 NO-ERROR.
ELSE 
DO:
       IF winstate = 1 THEN DO:
         ASSIGN winstate = 0 NO-ERROR.
      END.
      ASSIGN hcol = FRAME {&FRAME-NAME}:HANDLE NO-ERROR.
       FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
       IF AVAIL tt_size THEN DO:
           ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos NO-ERROR.
       END.



       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       ASSIGN hcol = hcol:FIRST-CHILD NO-ERROR.
       DO WHILE VALID-HANDLE(hcol):
  
          FIND FIRST tt_size WHERE tt_size.wg_name = STRING(hcol) NO-ERROR.
          IF AVAIL tt_size THEN DO:

             ASSIGN hcol:HEIGHT-PIXELS = tt_size.wg_height
                  hcol:WIDTH-PIXELS = tt_size.wg_width
                  hcol:X = tt_size.wg_xpos
                  hcol:Y = tt_size.wg_ypos  NO-ERROR.
               
          END.
          ASSIGN hcol = hcol:NEXT-SIBLING NO-ERROR.
       END.

       /*RUN set-size IN h_folder ( 21.67 , 148.00 ) NO-ERROR. /*default tool kit*/
       RUN set-size IN h_folder ( 22.14 , 159.00 ) NO-ERROR. /*w-order.w*/
       RUN set-size IN h_folder ( 21.67 , 150.00 ) NO-ERROR. /* w-est.w*/
       */

       RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-target",OUTPUT charsiz-hdl).
       DO ii = 1 TO NUM-ENTRIES(charsiz-hdl):
          RUN restore-widget IN WIDGET-HANDLE(ENTRY(ii,charsiz-hdl)) NO-ERROR.
       END.
END.

END.
*/

ON WINDOW-RESIZED OF {&WINDOW-NAME}
    DO:
        DEFINE VARIABLE cSmartObjList AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iCnt          AS INTEGER   NO-UNDO.
        DEFINE VARIABLE hTempHand     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE deRowPos      AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deColPos      AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deWidth       AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE deHeight      AS DECIMAL   NO-UNDO.   
        
        &if defined(exclude-globaldef) <> 0 &then
        IF deOrigWinWidth > {&WINDOW-NAME}:WIDTH THEN
            {&WINDOW-NAME}:WIDTH = deOrigWinWidth.
        IF deOrigWinHeight > {&WINDOW-NAME}:HEIGHT THEN
            {&WINDOW-NAME}:HEIGHT = deOrigWinHeight.        
        &endif
        
        deDeltaWidth      =  {&WINDOW-NAME}:WIDTH  - FRAME {&FRAME-NAME}:WIDTH.      
        deDeltaHeight     =  {&WINDOW-NAME}:HEIGHT - FRAME {&FRAME-NAME}:HEIGHT.
        
        widresizedlise = "".
        
        /* if window height and width increased then first increase the size of frame and then containing widgets */
        IF (deDeltaHeight > 0 OR deDeltaHeight = 0) AND (deDeltaWidth> 0 OR deDeltaWidth= 0) THEN
        DO:          
            ASSIGN 
                FRAME {&FRAME-NAME}:WIDTH                 = {&WINDOW-NAME}:WIDTH
                FRAME {&FRAME-NAME}:HEIGHT                = {&WINDOW-NAME}:HEIGHT
                FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
                NO-ERROR.  
            RUN get-size IN h_folder ( OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder ( INPUT {&WINDOW-NAME}:HEIGHT , INPUT {&WINDOW-NAME}:WIDTH  ) NO-ERROR.
        END.
        /* if window height increased then first increase the height of frame and then containing widgets */
        IF (deDeltaHeight > 0 OR deDeltaHeight = 0) AND (deDeltaWidth< 0 OR deDeltaWidth= 0) THEN
        DO:  
            ASSIGN 
                FRAME {&FRAME-NAME}:HEIGHT                = {&WINDOW-NAME}:HEIGHT
                FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                NO-ERROR. 
            RUN get-size IN h_folder ( OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder ( INPUT {&WINDOW-NAME}:HEIGHT , INPUT deWidth  ) NO-ERROR.
        END.
        /* if window width increased then first increase the width of frame and then containing widgets */
        IF (deDeltaHeight < 0 OR deDeltaHeight = 0) AND (deDeltaWidth> 0 OR deDeltaWidth= 0) THEN
        DO:          
            ASSIGN 
                FRAME {&FRAME-NAME}:WIDTH                 = {&WINDOW-NAME}:WIDTH
                FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
                NO-ERROR.  
            RUN get-size IN h_folder ( OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder ( INPUT deHeight , INPUT {&WINDOW-NAME}:WIDTH  ) NO-ERROR.
        END.
        
        FOR EACH toreposition BY toreposition.resizepage:
            
            IF NOT VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
                NEXT.
                
            cPageVisited   = cPageVisited + toreposition.resizepage + ",".            
            widresizedlise = widresizedlise + toreposition.widhand + ",".
            
            IF toreposition.widtype = "Browse" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
                RUN winReSize IN  WIDGET-HANDLE(toreposition.widhand)(INPUT deDeltaHeight,INPUT deDeltaWidth) NO-ERROR.
            ELSE IF toreposition.widtype = "Option-frame" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand))THEN 
            DO:
                hTempHand = WIDGET-HANDLE(toreposition.widhand).
                ASSIGN
                    hTempHand:WIDTH                 = {&WINDOW-NAME}:WIDTH
                    hTempHand:VIRTUAL-HEIGHT-PIXELS = hTempHand:HEIGHT-PIXELS
                    hTempHand:VIRTUAL-WIDTH-PIXELS  = hTempHand:WIDTH-PIXELS 
                no-error.
            END.
            ELSE IF toreposition.widtype = "moveright" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
            DO:
                deRowPos = 0.
                deColPos = 0.  
                RUN get-position IN  WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos ,OUTPUT deColPos           ) NO-ERROR. 
                RUN set-position IN  WIDGET-HANDLE(toreposition.widhand)(INPUT  deRowPos ,INPUT  deColPos + deDeltaWidth)  NO-ERROR.
            END.  
            ELSE IF toreposition.widtype = "movedown" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
                DO:          
                    deRowPos = 0.
                    deColPos = 0. 
                    RUN get-position IN  WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos                ,OUTPUT deColPos ) NO-ERROR. 
                    RUN set-position IN  WIDGET-HANDLE(toreposition.widhand)(INPUT deRowPos + deDeltaHeight ,INPUT  deColPos ) NO-ERROR.
                END.               
            ELSE IF  VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
            DO:
                deRowPos = 0.
                deColPos = 0.
                RUN get-position IN  WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos , OUTPUT deColPos            ) NO-ERROR. 
                RUN set-position IN  WIDGET-HANDLE(toreposition.widhand)(INPUT  deRowPos  ,INPUT  deColPos + deDeltaWidth ) NO-ERROR. 
            END.
 
        END.   
        /* if window height and width decreased then first set the size of containing widgets and then decrease the size of frame */
        IF (deDeltaHeight < 0 OR deDeltaHeight = 0) AND (deDeltaWidth< 0 OR deDeltaWidth= 0) THEN
        DO:
            ASSIGN 
                FRAME {&FRAME-NAME}:WIDTH                 = {&WINDOW-NAME}:WIDTH
                FRAME {&FRAME-NAME}:HEIGHT                = {&WINDOW-NAME}:HEIGHT 
                FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
                deHeight = 0
                deWidth = 0
                NO-ERROR .
            RUN get-size IN h_folder (OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder (INPUT {&WINDOW-NAME}:HEIGHT , INPUT {&WINDOW-NAME}:WIDTH  ) NO-ERROR.
        END.
        /* if window  width decreased then first set the size of containing widgets and then decrease the width of frame */
        IF (deDeltaHeight > 0 OR deDeltaHeight = 0) AND (deDeltaWidth< 0 OR deDeltaWidth= 0) THEN 
        DO:          
            ASSIGN 
                FRAME {&FRAME-NAME}:WIDTH                 = {&WINDOW-NAME}:WIDTH
                FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = FRAME {&FRAME-NAME}:WIDTH-PIXELS
                NO-ERROR.  
            RUN get-size IN h_folder (OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder (INPUT {&WINDOW-NAME}:HEIGHT , INPUT {&WINDOW-NAME}:WIDTH  ) NO-ERROR.
        END. 
        /* if window height  decreased then first set the size of containing widgets and then decrease the height of frame */
        IF (deDeltaHeight < 0 OR deDeltaHeight = 0) AND (deDeltaWidth< 0 OR deDeltaWidth= 0) THEN
        DO:  
            ASSIGN 
                FRAME {&FRAME-NAME}:HEIGHT                = {&WINDOW-NAME}:HEIGHT
                FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                NO-ERROR. 
            RUN get-size IN h_folder (OUTPUT deHeight , OUTPUT deWidth ) NO-ERROR. 
            RUN set-size IN h_folder (INPUT {&WINDOW-NAME}:HEIGHT , INPUT {&WINDOW-NAME}:WIDTH  ) NO-ERROR.
        END. 
        
            
    END.
          

