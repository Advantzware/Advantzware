/* pCreateINIObjects.i */

&global-define defaultWidth 38
&global-define defaultHeight 2.62
&global-define defaultFGColor ?
&global-define defaultBGColor 8
&global-define defaultCol 2
&global-define defaultRow 5.29
&global-define defaultColGap 39
&global-define defaultRowGap 2.81

PROCEDURE pCreateINIObjects:
    DEFINE INPUT PARAMETER cSections AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE tsINI    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dRow     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCol     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWidth   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dHeight  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iFGColor AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBGColor AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cImage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTooltip AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hWidget  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE poolName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cClick   AS CHARACTER NO-UNDO.
    
    ASSIGN
    &IF DEFINED(VDC) NE 0 &THEN
        jdx = 2
        poolName = "VDC" + ENTRY(1,cSections)
        FILE-INFO:FILE-NAME = SEARCH("VDC\VDC.ini")
    &ELSE
        jdx = 1
        poolName = "TouchPool{&PageNo}"
        hFrame = FRAME {&FRAME-NAME}:HANDLE
        FILE-INFO:FILE-NAME = SEARCH("touch\touchscr.ini")
    &ENDIF
        tsINI = FILE-INFO:FULL-PATHNAME
        .
    LOAD tsINI.
    USE tsINI.
    
    &IF DEFINED(VDC) NE 0 &THEN
    ASSIGN 
        dCol = DEC(fGetKeyValue(hFrame:NAME,"Col"))
        dRow = DEC(fGetKeyValue(hFrame:NAME,"Row"))
        dWidth = DEC(fGetKeyValue(hFrame:NAME,"Width"))
        dHeight = DEC(fGetKeyValue(hFrame:NAME,"Height"))
        hFrame:HIDDEN = YES
        .
    IF dCol EQ ? THEN dCol = 42.
    IF dRow EQ ? THEN dRow = 1.
    ASSIGN 
        hFrame:COL = dCol
        hFrame:ROW = dRow
        .
    IF dHeight EQ ? THEN
    dHeight = FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT - dRow + 1.24.
    ASSIGN
        hFrame:VIRTUAL-HEIGHT = dHeight
        hFrame:HEIGHT = dHeight
        .
    IF dWidth EQ ? THEN
    dWidth = FRAME {&FRAME-NAME}:WIDTH - dCol + 2.
    ASSIGN
        hFrame:VIRTUAL-WIDTH = dWidth
        hFrame:WIDTH = dWidth
        .
    &ENDIF

    DO idx = jdx TO NUM-ENTRIES(cSections):
        ASSIGN
            dCol     = DEC(fGetKeyValue(ENTRY(idx,cSections),"Col"))
            dRow     = DEC(fGetKeyValue(ENTRY(idx,cSections),"Row"))
            dWidth   = DEC(fGetKeyValue(ENTRY(idx,cSections),"Width"))
            dHeight  = DEC(fGetKeyValue(ENTRY(idx,cSections),"Height"))
            iFGColor = INT(fGetKeyValue(ENTRY(idx,cSections),"FGColor"))
            iBGColor = INT(fGetKeyValue(ENTRY(idx,cSections),"BGColor"))
            cImage   =     fGetKeyValue(ENTRY(idx,cSections),"Image")
            cText    =     fGetKeyValue(ENTRY(idx,cSections),"Text")
            cTooltip =     fGetKeyValue(ENTRY(idx,cSections),"Tooltip")
            cClick   = ENTRY(idx,cSections)
            &IF DEFINED(VDC) NE 0 &THEN
            cClick   = hFrame:NAME + "," + cClick
            &ENDIF
            .
        IF dCol EQ ? THEN NEXT.
        
        IF dWidth   EQ ? THEN dWidth   = {&defaultWidth}.
        IF dHeight  EQ ? THEN dHeight  = {&defaultHeight}.
        IF iFGColor EQ ? THEN iFGColor = {&defaultFGColor}.
        IF iBGColor EQ ? THEN iBGColor = {&defaultBGColor}.
        IF cTooltip EQ ? THEN cToolTip = cText.
        
        RUN pCreateObject (
            poolName,
            hFrame,
            ENTRY(idx,cSections),
            dCol,
            dRow,
            dWidth,
            dHeight,
            iFGColor,
            iBGColor,
            cText,
            cTooltip,
            cClick,
            cImage,
            NO
            ).
    END. /* do idx */
    
    UNLOAD tsINI.

END PROCEDURE.

PROCEDURE pCreateObject:
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcText     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplDynamic  AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE cDynamic AS CHARACTER NO-UNDO.
    
    IF iplDynamic THEN cDynamic = "dynamic".
    
    CREATE ttTriggers.
    ASSIGN
        ttTriggers.frameHandle   = iphFrame 
        ttTriggers.triggerName   = ipcName
        ttTriggers.dynamicObject = iplDynamic
        .
    RUN pCreateRectangle (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipiFGColor,
        ipiBGColor,
        ipcTooltip,
        ipcClick
        ).            
    IF ipcText NE ? THEN
    RUN pCreateEditor (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipiFGColor,
        ipiBGColor,
        ipcText,
        ipcTooltip,
        ipcClick
        ).
    IF ipcText  NE ?  THEN ipdWidth = ipdWidth - 28.
    IF ipdWidth LT 10 THEN ipdWidth = 10.
    RUN pCreateImage (
        ipcPoolName,
        iphFrame,
        ipcName,
        ipdCol,
        ipdRow,
        ipdWidth,
        ipdHeight,
        ipcTooltip,
        ipcClick,
        ipcImage
        ).

END PROCEDURE.

PROCEDURE pCreateEditor:
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcText     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

    CREATE EDITOR hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName + "Text"
            COL = ipdCol + 12
            ROW = ipdRow + .95
            SENSITIVE = YES
            WIDTH = ipdWidth - 13
            HEIGHT = ipdHeight - 1.19
            FGCOLOR = ipiFGColor
            BGCOLOR = ipiBGColor
            SCROLLBAR-HORIZONTAL = NO
            SCROLLBAR-VERTICAL = NO
            WORD-WRAP = YES
            READ-ONLY = YES
            BOX = NO
            PRIVATE-DATA = ipcText
            SCREEN-VALUE = ipcText
            TOOLTIP = ipcTooltip
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (ipcClick).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttTriggers.hEditor = hWidget.
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

PROCEDURE pCreateImage:
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImage    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
    CREATE IMAGE hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName + "Image"
            COL = ipdCol + 1
            ROW = ipdRow + .23
            SENSITIVE = YES
            WIDTH = ipdWidth
            HEIGHT = ipdHeight - .48
            TRANSPARENT = YES
            TOOLTIP = ipcTooltip
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (ipcClick).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttTriggers.hImage = hWidget.
        hWidget:LOAD-IMAGE(SEARCH(ipcImage)).
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

PROCEDURE pCreateRectangle:
    DEFINE INPUT PARAMETER ipcPoolName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame    AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcName     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow      AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth    AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdHeight   AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiFGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcToolTip  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcClick    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.

    CREATE RECTANGLE hWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName + "Trigger"
            COL = ipdCol
            ROW = ipdRow
            SENSITIVE = YES
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            FGCOLOR = ipiFGColor
            BGCOLOR = ipiBGColor
            TOOLTIP = ipcTooltip
            ROUNDED = YES
      TRIGGERS:
        ON MOUSE-SELECT-CLICK
          PERSISTENT RUN pClick IN THIS-PROCEDURE (ipcClick).
      END TRIGGERS.
    IF VALID-HANDLE(hWidget) THEN DO:
        ttTriggers.hRectangle = hWidget.
        hWidget:MOVE-TO-TOP().
    END.

END PROCEDURE.

PROCEDURE pSetSensitive:
    DEFINE INPUT PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    
    FIND FIRST ttTriggers
         WHERE ttTriggers.frameHandle EQ hFrame
           AND ttTriggers.triggerName EQ ipcName
         NO-ERROR.
    IF AVAILABLE ttTriggers THEN DO:
        IF VALID-HANDLE(ttTriggers.hRectangle) THEN
        ttTriggers.hRectangle:SENSITIVE = iplSensitive.
        IF VALID-HANDLE(ttTriggers.hEditor) THEN
        ttTriggers.hEditor:SENSITIVE    = iplSensitive.
        IF VALID-HANDLE(ttTriggers.hImage) THEN
        ttTriggers.hImage:SENSITIVE     = iplSensitive.
    END.
    
END PROCEDURE.
