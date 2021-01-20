PROCEDURE beforeinitialize:
    ASSIGN
        hTempWinmn    = ?
        deResizeVal   = 0
        deTempColPos  = 0
        cSmartObjList = ""
        deRowPos      = 0
        deColPos      = 0
        deWidth       = 0
        deHeight      = 0
        .

    IF VALID-HANDLE({&WINDOW-NAME}) THEN 
    DO:
        CREATE pagewinsize.
        ASSIGN 
            pagewinsize.pageNo    = "Default"
            pagewinsize.WinWidth  = {&WINDOW-NAME}:WIDTH
            pagewinsize.winHeight = {&WINDOW-NAME}:HEIGHT 
            .  
        // capture the original window size       
        deOrigWinWidth = {&WINDOW-NAME}:WIDTH.
        deOrigWinHeight = {&WINDOW-NAME}:HEIGHT.
         
        // set window position captured when it was closed 
        RUN setCapturedWindowPosition.
              
        // set the option frame with equal to window withd and set its color to blue
        hTempWinmn  = FRAME {&FRAME-NAME}:handle.

        IF VALID-HANDLE(hTempWinmn) THEN
            hTempWinmn  = hTempWinmn:FIRST-CHILD. 
              
        IF hTempWinmn:TYPE = "FIELD-GROUP" THEN  
            hTempWinmn  = hTempWinmn:FIRST-CHILD.
        
        REPEAT WHILE VALID-HANDLE(hTempWinmn):
            IF hTempWinmn:NAME = "OPTIONS-FRAME"  THEN
            DO:
                ASSIGN
                    hTempWinmn:ROW                   = 1
                    hTempWinmn:COLUMN                = 1
                    hTempWinmn:BGCOLOR               = 21
                    hTempWinmn:WIDTH                 = {&WINDOW-NAME}:width
                    hTempWinmn:VIRTUAL-HEIGHT-PIXELS = hTempWinmn:HEIGHT-PIXELS
                    hTempWinmn:VIRTUAL-WIDTH-PIXELS  = hTempWinmn:WIDTH-PIXELS
                    . 
                // get all the objects handle created before initialization to be resized or repositioned     
                FIND FIRST toreposition WHERE toreposition.widhand =  STRING(hTempWinmn) NO-ERROR.
                IF NOT AVAILABLE toreposition THEN
                    CREATE toreposition.
                ASSIGN 
                    toreposition.widhand   = STRING(hTempWinmn)
                    toreposition.colpos    = hTempWinmn:COLUMN
                    toreposition.rowpos    = hTempWinmn:ROW
                    toreposition.widwidth  = hTempWinmn:WIDTH
                    toreposition.widheight = hTempWinmn:height
                    toreposition.widtype   = "Option-frame"
                    .  
            END.
            hTempWinmn        = hTempWinmn:NEXT-SIBLING.

        END.
    END.

    hTempWinmn = ?.
     
 // get all the scoped define objects handle created before initialization to be moved down oe right
     &IF DEFINED(h_Object01) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object01}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object01})
        toreposition.resizepage = "1"       
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object01}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".         
        &endif         
    &endif
    
    &IF DEFINED(h_Object02) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object02}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object02})
        toreposition.resizepage = "1"      
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object02}',"{&moveRight}",",") > 0 THEN
        toreposition.widtype   = "moveright".
        &endif
    &endif
        
    &IF DEFINED(h_Object03) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object03}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object03})
        toreposition.resizepage = "1"      
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object03}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif   
    &endif
    
    &IF DEFINED(h_Object04) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object04}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object04})
        toreposition.resizepage = "1"       
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object04}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif
    &endif 
    
    &IF DEFINED(h_Object05) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object05}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object05})
        toreposition.resizepage = "1"  
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object05}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif  
    &endif
    
    &IF DEFINED(h_Object06) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object06}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object06})
        toreposition.resizepage = "1"    
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object06}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif 
    &endif
    
    &IF DEFINED(h_Object07) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object07}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object07})
        toreposition.resizepage = "1"
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object07}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif   
    &endif
    
    &IF DEFINED(h_Object08) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object08}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object08})
        toreposition.resizepage = "1"      
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object08}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif
    &endif 
    
    &IF DEFINED(h_Object09) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object09}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object09})
        toreposition.resizepage = "1"  
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object09}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif  
    &endif
    
    &IF DEFINED(h_Object10) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object10}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object10})
        toreposition.resizepage = "1"     
        toreposition.widtype    = "movedown".
        &IF DEFINED(moveRight) NE 0 &THEN 
    IF LOOKUP('{&h_Object10}',"{&moveRight}",",") > 0  THEN
        toreposition.widtype   = "moveright".
        &endif 
    &endif

END PROCEDURE.   


PROCEDURE afterinitialize:

    RUN getlinktable IN adm-broker-hdl(
        INPUT THIS-PROCEDURE:UNIQUE-ID, 
        OUTPUT cSmartObjList
        ).
   // get all the object handles that have bbe created after initialization
    DO iCntWidHand = 1 TO NUM-ENTRIES(cSmartObjList,","): 
                       
        hTempWinmn = HANDLE(ENTRY(iCntWidHand,cSmartObjList,",")).

        IF NOT VALID-HANDLE(hTempWinmn) THEN 
            NEXT.
    
        IF LOOKUP( "panel-identifier",      hTempWinmn:INTERNAL-ENTRIES, ",")  > 0 OR
            LOOKUP( "viewer-identifier",     hTempWinmn:INTERNAL-ENTRIES, ",")  > 0 OR
            LOOKUP( "count-buttons",         hTempWinmn:INTERNAL-ENTRIES, ",")  > 0 OR
            INDEX(hTempWinmn:INTERNAL-ENTRIES, "folder")                        > 0 OR
            THIS-PROCEDURE:FILE-NAME =  hTempWinmn:NAME                             OR
            hTempWinmn:NAME = "smartobj/smartmsg.w"
            THEN NEXT.  
                 
        deRowPos      = 0.
        deColPos      = 0.
        RUN get-position IN hTempWinmn (OUTPUT deRowPos ,OUTPUT deColPos ) NO-ERROR.
            
        deWidth       = 0.
        deHeight      = 0.
        RUN get-size IN hTempWinmn (OUTPUT deHeight ,OUTPUT deWidth) NO-ERROR. 
            
        FIND FIRST toreposition WHERE toreposition.widhand =  ENTRY(iCntWidHand,cSmartObjList,",") NO-ERROR.
        IF NOT AVAILABLE toreposition THEN
            CREATE toreposition.
        ASSIGN 
            toreposition.widhand    = ENTRY(iCntWidHand,cSmartObjList,",")
            toreposition.colpos     = deColPos
            toreposition.rowpos     = deRowPos
            toreposition.widwidth   = deWidth
            toreposition.widheight  = deHeight
            toreposition.resizepage = "1"
            toreposition.widtype    = hTempWinmn:NAME
            .
        IF LOOKUP( "nav-browse-identIFier", hTempWinmn:INTERNAL-ENTRIES, ",")  > 0 OR
            LOOKUP( "browse-identifier",     hTempWinmn:INTERNAL-ENTRIES, ",")  > 0 THEN
            toreposition.widtype   = "Browse".
    END.  
   
    IF VALID-HANDLE({&WINDOW-NAME}) THEN 
        deResizeVal = {&WINDOW-NAME}:WIDTH.
    // shift all the buttons in toolbar towards right   
    FOR EACH toreposition BY toreposition.colpos DESCENDING :

        IF toreposition.widtype = "Browse" THEN 
            NEXT.
        IF toreposition.widtype = "movedown" THEN 
            NEXT.
        IF toreposition.widtype = "moveright" THEN 
            NEXT.
        IF toreposition.widtype = "Option-frame" THEN 
            NEXT.
                
        IF deTempColPos NE toreposition.colpos THEN 
            deResizeVal = deResizeVal - (toreposition.widwidth + 2).
                
        RUN set-position IN WIDGET-HANDLE(toreposition.widhand)( INPUT toreposition.rowpos , INPUT deResizeVal) NO-ERROR.
             
        deTempColPos = toreposition.colpos.          
    END.
    
    // set the window size to last captured size
    RUN setCapturedWindowSize.
    // if window size have been changed the apply window resize trigger to repositioned or resize objects
    IF  deOrigWinWidth NE  {&WINDOW-NAME}:WIDTH OR
    deOrigWinHeight NE {&WINDOW-NAME}:HEIGHT
    THEN
    APPLY "WINDOW-RESIZED" TO {&WINDOW-NAME}. 
    
    lastBtnPos = deResizeVal.
END PROCEDURE.


&IF DEFINED(local-destroy) eq 0 &THEN 
PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
     // capture the window position and size when window is closed and save to DB
    {custom/userWindow.i}

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

END PROCEDURE.
&endif

PROCEDURE setCapturedWindowSize:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    cFileName = ENTRY(1, THIS-PROCEDURE:FILE-NAME, ".").
    
    FIND FIRST userWindow NO-LOCK 
        WHERE userWindow.usrId       EQ USERID('ASI')
        AND userwindow.programname EQ cFileName 
        NO-ERROR.
    IF AVAILABLE userWindow THEN 
    DO:                
        IF  userWindow.sessionWidth   EQ SESSION:WIDTH-PIXELS 
            AND userWindow.sessionHeight  EQ SESSION:HEIGHT-PIXELS THEN     
            ASSIGN                 
                {&WINDOW-NAME}:WIDTH  = userWindow.winWidth
                {&WINDOW-NAME}:HEIGHT = userwindow.winHeight
                   NO-ERROR .
         ELSE 
            ASSIGN 
                {&WINDOW-NAME}:WIDTH         = userWindow.winWidth
                {&WINDOW-NAME}:HEIGHT        = userWindow.winHeight
                {&WINDOW-NAME}:WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS - (userWindow.sessionWidth - SESSION:WIDTH-PIXELS)
                {&WINDOW-NAME}:HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS - (userWindow.sessionHeight - SESSION:HEIGHT-PIXELS)
                    NO-ERROR . 
           
        IF deOrigWinWidth > {&WINDOW-NAME}:WIDTH THEN
            {&WINDOW-NAME}:WIDTH = deOrigWinWidth.
        IF deOrigWinHeight > {&WINDOW-NAME}:HEIGHT THEN
            {&WINDOW-NAME}:HEIGHT = deOrigWinHeight. 
            
        ASSIGN                 
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS 
            {&window-name}:window-state          = userWindow.state
              NO-ERROR 
            . 
            
               
    END.

END PROCEDURE.

PROCEDURE setCapturedWindowPosition:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    cFileName = ENTRY(1, THIS-PROCEDURE:FILE-NAME, ".").
                
    FIND FIRST userWindow NO-LOCK 
        WHERE userWindow.usrId       EQ USERID('ASI')
        AND userwindow.programname EQ cFileName 
        NO-ERROR.
    IF AVAILABLE userWindow THEN  
        ASSIGN                 
            {&WINDOW-NAME}:X                     = userwindow.winxpos
            {&WINDOW-NAME}:Y                     = userwindow.winypos
              NO-ERROR 
            .                 

END PROCEDURE.