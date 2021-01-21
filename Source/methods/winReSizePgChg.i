/* winReSizePgChg.i */
// pgchg
ASSIGN 
    deResizeVal   = 0
    cSmartObjList = ""    
    iCntWidHand   = 0
    hTempObjHand  = ?
    deRowPos      = 0
    deColPos      = 0
    deWidth       = 0
    deHeight      = 0
    pgno          = ""
    deTempColPos  = 0.

// get the current page number
RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
ASSIGN 
    pgno = STRING(RETURN-VALUE).

// resize/shift the objects(browse,buttons etc.) that initizie with page based on delta 
// if page is previsoly visited the  (current_win_size - previous_visit_win_size) saved in "pagewinsize.pageno = pgno" record
// if page is not visited the calculte delta as (current_win_size - Orig_win_size) saved in "pagewinsize.pageno = "Default"" record
FIND FIRST pagewinsize WHERE pagewinsize.pageno = pgno NO-ERROR.
IF NOT AVAILABLE pagewinsize THEN 
    DO:
        FIND FIRST pagewinsize WHERE pagewinsize.pageno = "Default" NO-ERROR.
        IF  AVAILABLE pagewinsize THEN 
            ASSIGN  
                deDeltaWidthchange  = {&WINDOW-NAME}:WIDTH - pagewinsize.WinWidth
                deDeltaHeightchange = {&WINDOW-NAME}:HEIGHT - pagewinsize.winHeight
                .
    END.
 ELSE
    ASSIGN  
        deDeltaWidthchange  = {&WINDOW-NAME}:WIDTH - pagewinsize.WinWidth
        deDeltaHeightchange =  {&WINDOW-NAME}:HEIGHT - pagewinsize.winHeight
        .
 
 // get the handels of all objects initialized with page "scoped define" and needs to be shifted 
&IF DEFINED(h_Object01) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object01}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand = STRING({&h_Object01})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object01}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif  
&endif
&IF DEFINED(h_Object02) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object02}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand = STRING({&h_Object02})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
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
        toreposition.widhand = STRING({&h_Object03})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object03}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif 
&endif
&IF DEFINED(h_Object04) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object04}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand = STRING({&h_Object04})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object04}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif 
&endif
&IF DEFINED(h_Object05) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object05}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object05})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object05}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif  
&endif
&IF DEFINED(h_Object06) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object06}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object06})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object06}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif
&endif
&IF DEFINED(h_Object07) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object07}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object07})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object07}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif 
&endif
&IF DEFINED(h_Object08) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object08}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object08})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object08}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif 
&endif
&IF DEFINED(h_Object09) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object09}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object09})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object09}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif  
&endif
&IF DEFINED(h_Object10) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object10}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object10})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object10}',"{&moveRight}",",") > 0 THEN
            toreposition.widtype   = "moveright".
    &endif
&endif
&IF DEFINED(h_Object11) NE 0 &THEN
    FIND FIRST toreposition WHERE toreposition.widhand =  STRING({&h_Object11}) NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = STRING({&h_Object11})
        toreposition.resizepage = pgno.
        toreposition.widtype   = "movedown".
    &IF DEFINED(moveRight) NE 0 &THEN 
        IF LOOKUP('{&h_Object11}',"{&moveRight}",",") > 0 THEN
        toreposition.widtype   = "moveright".
    &endif
&endif

 // get the handels of all objects initialized with page "Objects linked with page(browse,buttons)" and needs to be shifted 
RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,INPUT "PAGE" + pgno + "-TARGET", OUTPUT cSmartObjList) NO-ERROR.

DO iCntWidHand = 1 TO NUM-ENTRIES(cSmartObjList,","): 

    IF  LOOKUP(pgno, cPageVisited, ",") > 0 AND
        dePrevDeltaWidth   = deDeltaWidthchange AND 
        dePrevDeltaHeight  = deDeltaHeightchange
        THEN NEXT.
   
    hTempObjHand = HANDLE(ENTRY(iCntWidHand,cSmartObjList,",")).

    IF VALID-HANDLE(hTempObjHand) AND
        (
        LOOKUP( "panel-identifier",      hTempObjHand:INTERNAL-ENTRIES, ",")  > 0 OR
        LOOKUP( "viewer-identifier",     hTempObjHand:INTERNAL-ENTRIES, ",")  > 0 OR
        LOOKUP( "count-buttons",         hTempObjHand:INTERNAL-ENTRIES, ",")  > 0 OR
        THIS-PROCEDURE:FILE-NAME =  hTempObjHand:NAME                             OR
        hTempObjHand:NAME = "smartobj/smartmsg.w")                                OR
        INDEX(hTempObjHand:INTERNAL-ENTRIES, "folder")  > 0 
        THEN NEXT.
           
    RUN get-position IN hTempObjHand (  OUTPUT deRowPos , OUTPUT deColPos ) NO-ERROR.   
    RUN get-size IN hTempObjHand (  OUTPUT deHeight ,  OUTPUT deWidth  ) NO-ERROR.
    
    FIND FIRST toreposition WHERE toreposition.widhand =  ENTRY(iCntWidHand,cSmartObjList,",") NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = ENTRY(iCntWidHand,cSmartObjList,",")
        toreposition.colpos     = deRowPos
        toreposition.rowpos     = deColPos
        toreposition.widwidth   = deWidth
        toreposition.widheight  = deHeight
        toreposition.resizepage = pgno
        .    
     
    IF VALID-HANDLE(hTempObjHand) AND
        (LOOKUP( "nav-browse-identifier", hTempObjHand:INTERNAL-ENTRIES, ",")  > 0 OR
         LOOKUP( "browse-identifier",     hTempObjHand:INTERNAL-ENTRIES, ",")  > 0 ) THEN 
        toreposition.widtype   = "Browse". 
    ELSE   
        toreposition.widtype   = hTempObjHand:NAME.         
END.

// resize/shift all objects except the icon in toolbar linked to that page
FOR EACH toreposition WHERE toreposition.resizepage = pgno: 

    IF NOT VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
        NEXT.
   
    IF LOOKUP(pgno, cPageVisited, ",") > 0        AND
        dePrevDeltaWidth = deDeltaWidthchange     AND 
        dePrevDeltaHeight   = deDeltaHeightchange
        THEN NEXT.    
   
    IF LOOKUP(toreposition.widhand, widresizedlise, ",") > 0 
        THEN NEXT.
 
    
    IF toreposition.widtype   = "Browse" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
        RUN winReSize IN  WIDGET-HANDLE(toreposition.widhand) (deDeltaHeightchange,deDeltaWidthchange) NO-ERROR.
    ELSE IF toreposition.widtype   = "moveright" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO:     
        deRowPos = 0. 
        deColPos = 0.     
        RUN get-position IN  WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos ,OUTPUT deColPos) NO-ERROR.
        RUN set-position IN WIDGET-HANDLE(toreposition.widhand) (INPUT deRowPos  ,INPUT deColPos + deDeltaWidthchange) NO-ERROR .
    END.  
    ELSE IF toreposition.widtype   = "movedown" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO: 
        deRowPos = 0. 
        deColPos = 0.            
        RUN get-position IN  WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos ,OUTPUT deColPos ) NO-ERROR. 
        RUN set-position IN WIDGET-HANDLE(toreposition.widhand) (INPUT deRowPos + deDeltaHeightchange ,INPUT deColPos ) NO-ERROR.
    END.               
    ELSE IF  VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO:
        deRowPos = 0. 
        deColPos = 0. 
        IF LOOKUP(STRING(toreposition.widhand),cSmartObjList, ",") > 0 THEN NEXT.
        RUN get-position IN  WIDGET-HANDLE(toreposition.widhand) (OUTPUT deRowPos , OUTPUT deColPos ) NO-ERROR. 
        RUN set-position IN  WIDGET-HANDLE(toreposition.widhand) (INPUT deRowPos ,  INPUT deColPos + deDeltaWidthchange ) NO-ERROR. 
    END.
    widresizedlise = widresizedlise + "," + toreposition.widhand.        
END. 

cPageVisited      = cPageVisited + pgno + ",".
dePrevDeltaWidth  = deDeltaWidthchange.
dePrevDeltaHeight = deDeltaHeightchange.

FIND FIRST pagewinsize WHERE pagewinsize.pageno = pgno NO-ERROR.
IF NOT AVAILABLE pagewinsize THEN 
    CREATE pagewinsize.
ASSIGN 
    pagewinsize.pageNo    = pgno
    pagewinsize.WinWidth  = {&WINDOW-NAME}:WIDTH
    pagewinsize.winHeight = {&WINDOW-NAME}:HEIGHT 
    .  
    
cSmartObjList = "".  
RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,INPUT "PAGE" + "1" + "-TARGET", OUTPUT cSmartObjList) NO-ERROR.
// get the last position of the icon after shifting
FOR EACH toreposition WHERE toreposition.resizepage = "1" 
                      AND   toreposition.widtype NE "movedown" 
                      AND   toreposition.widtype NE "moveright" 
                      AND   toreposition.widtype NE "Browse" 
                      AND   toreposition.widtype NE "Option-frame" 
                      BY    toreposition.colpos DESCENDING :

    IF LOOKUP(STRING(toreposition.widhand),cSmartObjList, ",") > 0 THEN 
        NEXT.            
    RUN get-position IN WIDGET-HANDLE(toreposition.widhand)( OUTPUT deTempColPos , OUTPUT lastBtnPos ) NO-ERROR.
        
END.
   
deResizeVal = lastBtnPos. 
deTempColPos = 0. 
 
// shift the icons linked with page starting from last position tracked earlier
FOR EACH toreposition WHERE toreposition.resizepage = pgno 
                      AND   toreposition.widtype NE "movedown" 
                      AND   toreposition.widtype NE "moveright" 
                      AND   toreposition.widtype NE "Browse" 
                      AND   toreposition.widtype NE "Option-frame" 
                      AND   toreposition.resizepage NE "1"
                      BY    toreposition.colpos DESCENDING :
                          
    deResizeVal = deResizeVal - (toreposition.widwidth + 2).
                
    RUN set-position IN WIDGET-HANDLE(toreposition.widhand)( INPUT toreposition.rowpos , INPUT deResizeVal) NO-ERROR.
             
    deTempColPos = toreposition.colpos.          
END.

/*

&IF DEFINED(winReSize) NE 0 &THEN
  &IF '{1}' EQ '' &THEN
    DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE objectWidget AS WIDGET-HANDLE NO-UNDO.
  &ENDIF
  
  /* scop-def h_ObjectXX in window container */
  {methods/moveObject.i 01}
  {methods/moveObject.i 02}
  {methods/moveObject.i 03}
  {methods/moveObject.i 04}
  {methods/moveObject.i 05}
  {methods/moveObject.i 06}
  {methods/moveObject.i 07}
  {methods/moveObject.i 08}
  {methods/moveObject.i 09}
  {methods/moveObject.i 10}
  {methods/moveObject.i 11}
  {methods/moveObject.i 12}
  {methods/moveObject.i 13}
  {methods/moveObject.i 14}
  {methods/moveObject.i 15}
  {methods/moveObject.i 16}
  {methods/moveObject.i 17}
  {methods/moveObject.i 18}
  {methods/moveObject.i 19}
  {methods/moveObject.i 20}
&ENDIF
 */
