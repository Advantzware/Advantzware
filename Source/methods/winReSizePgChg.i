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
    deTempColPos  = 0
    .
RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
pgno = STRING(RETURN-VALUE).

FIND FIRST pagewinsize
     WHERE pagewinsize.pageno EQ pgno
     NO-ERROR.
IF NOT AVAILABLE pagewinsize THEN 
FIND FIRST pagewinsize
     WHERE pagewinsize.pageno EQ "Default"
     NO-ERROR.
IF AVAILABLE pagewinsize THEN 
ASSIGN  
    deDeltaWidthchange  = {&WINDOW-NAME}:WIDTH  - pagewinsize.WinWidth
    deDeltaHeightchange = {&WINDOW-NAME}:HEIGHT - pagewinsize.winHeight
    .
  
/* scop-def h_ObjectXX in window container */
{methods/toReposition.i 01}
{methods/toReposition.i 02}
{methods/toReposition.i 03}
{methods/toReposition.i 04}
{methods/toReposition.i 05}
{methods/toReposition.i 06}
{methods/toReposition.i 07}
{methods/toReposition.i 08}
{methods/toReposition.i 09}
{methods/toReposition.i 10}
{methods/toReposition.i 11}
{methods/toReposition.i 12}
{methods/toReposition.i 13}
{methods/toReposition.i 14}
{methods/toReposition.i 15}
{methods/toReposition.i 16}
{methods/toReposition.i 17}
{methods/toReposition.i 18}
{methods/toReposition.i 19}
{methods/toReposition.i 20}

RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, "PAGE" + pgno + "-TARGET", OUTPUT cSmartObjList) NO-ERROR.

DO iCntWidHand = 1 TO NUM-ENTRIES(cSmartObjList,","): 

    IF LOOKUP(pgno, cPageVisited) GT 0 AND
        dePrevDeltaWidth  EQ deDeltaWidthchange AND 
        dePrevDeltaHeight EQ deDeltaHeightchange THEN NEXT.
   
    hTempObjHand = HANDLE(ENTRY(iCntWidHand,cSmartObjList)).

    IF VALID-HANDLE(hTempObjHand) AND
        (LOOKUP("panel-identifier",  hTempObjHand:INTERNAL-ENTRIES) GT 0 OR
         LOOKUP("viewer-identifier", hTempObjHand:INTERNAL-ENTRIES) GT 0 OR
         LOOKUP("count-buttons",     hTempObjHand:INTERNAL-ENTRIES) GT 0 OR
         THIS-PROCEDURE:FILE-NAME EQ hTempObjHand:NAME  OR
         hTempObjHand:NAME EQ "smartobj/smartmsg.w")    OR
         INDEX(hTempObjHand:INTERNAL-ENTRIES, "folder") GT 0 THEN NEXT.
           
    RUN get-position IN hTempObjHand (OUTPUT deRowPos, OUTPUT deColPos) NO-ERROR.   
    RUN get-size IN hTempObjHand (OUTPUT deHeight, OUTPUT deWidth) NO-ERROR.
    
    FIND FIRST toreposition
         WHERE toreposition.widhand EQ ENTRY(iCntWidHand,cSmartObjList)
         NO-ERROR.
    IF NOT AVAILABLE toreposition THEN
        CREATE toreposition.
    ASSIGN 
        toreposition.widhand    = ENTRY(iCntWidHand,cSmartObjList)
        toreposition.colpos     = deRowPos
        toreposition.rowpos     = deColPos
        toreposition.widwidth   = deWidth
        toreposition.widheight  = deHeight
        toreposition.resizepage = pgno
        .    
    IF VALID-HANDLE(hTempObjHand) AND
        (LOOKUP("nav-browse-identifier", hTempObjHand:INTERNAL-ENTRIES) GT 0  OR
         LOOKUP("browse-identifier",     hTempObjHand:INTERNAL-ENTRIES) GT 0) THEN 
        toreposition.widtype = "Browse". 
    ELSE   
        toreposition.widtype = hTempObjHand:NAME.         
END.

FOR EACH toreposition
    WHERE toreposition.resizepage EQ pgno
    :
    IF NOT VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN NEXT.
   
    IF LOOKUP(pgno, cPageVisited) GT 0  AND
        dePrevDeltaWidth  EQ deDeltaWidthchange  AND 
        dePrevDeltaHeight EQ deDeltaHeightchange THEN NEXT.    
   
    IF LOOKUP(toreposition.widhand, widresizedlise) GT 0 THEN NEXT.
 
    IF toreposition.widtype EQ "Browse" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
        RUN winReSize IN WIDGET-HANDLE(toreposition.widhand) (deDeltaHeightchange, deDeltaWidthchange) NO-ERROR.
    ELSE IF toreposition.widtype EQ "moveright" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO:
        ASSIGN
            deRowPos = 0
            deColPos = 0
            .     
        RUN get-position IN WIDGET-HANDLE(toreposition.widhand) (OUTPUT deRowPos, OUTPUT deColPos) NO-ERROR.
        RUN set-position IN WIDGET-HANDLE(toreposition.widhand) (deRowPos, deColPos + deDeltaWidthchange) NO-ERROR.
    END.  
    ELSE IF toreposition.widtype EQ "movedown" AND VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO: 
        ASSIGN
            deRowPos = 0 
            deColPos = 0
            .     
        RUN get-position IN WIDGET-HANDLE(toreposition.widhand)(OUTPUT deRowPos, OUTPUT deColPos) NO-ERROR.
        RUN set-position IN WIDGET-HANDLE(toreposition.widhand) (deRowPos + deDeltaHeightchange, deColPos) NO-ERROR.
    END.               
    ELSE IF  VALID-HANDLE(WIDGET-HANDLE(toreposition.widhand)) THEN
    DO:
        ASSIGN
            deRowPos = 0 
            deColPos = 0
            .     
        RUN get-position IN  WIDGET-HANDLE(toreposition.widhand) (OUTPUT deRowPos, OUTPUT deColPos) NO-ERROR.
        RUN set-position IN  WIDGET-HANDLE(toreposition.widhand) (deRowPos, deColPos + deDeltaWidthchange) NO-ERROR.
    END.
    widresizedlise = widresizedlise + "," + toreposition.widhand.        
END. 

ASSIGN
    cPageVisited      = cPageVisited + pgno + ","
    dePrevDeltaWidth  = deDeltaWidthchange
    dePrevDeltaHeight = deDeltaHeightchange
    .
FIND FIRST pagewinsize
     WHERE pagewinsize.pageno EQ pgno
     NO-ERROR.
IF NOT AVAILABLE pagewinsize THEN 
    CREATE pagewinsize.
ASSIGN 
    pagewinsize.pageNo    = pgno
    pagewinsize.WinWidth  = {&WINDOW-NAME}:WIDTH
    pagewinsize.winHeight = {&WINDOW-NAME}:HEIGHT 
    .   
