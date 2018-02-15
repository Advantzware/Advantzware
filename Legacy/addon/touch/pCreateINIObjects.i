/* pCreateINIObjects.i */

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
    DEFINE VARIABLE hWidget  AS HANDLE    NO-UNDO.
    
    ASSIGN
        FILE-INFO:FILE-NAME = SEARCH("touch\touchscr.ini")
        tsINI = FILE-INFO:FULL-PATHNAME
        .
    LOAD tsINI.
    USE tsINI.
    
    DO idx = 1 TO NUM-ENTRIES(cSections):
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
            .
        IF dCol EQ ? THEN NEXT.
        
        IF dWidth   EQ ? THEN dWidth   = 38.
        IF dHeight  EQ ? THEN dHeight  = 2.62.
        IF iFGColor EQ ? THEN iFGColor = ?.
        IF iBGColor EQ ? THEN iBGColor = 8.
        IF cTooltip EQ ? THEN cToolTip = cText.
        
        CREATE ttTriggers.
        ttTriggers.triggerName = ENTRY(idx,cSections).
        
        CREATE RECTANGLE hWidget IN WIDGET-POOL "TouchPool{&PageNo}"
            ASSIGN
                FRAME = FRAME {&FRAME-NAME}:HANDLE
                NAME = ENTRY(idx,cSections) + "Trigger"
                COL = dCol
                ROW = dRow
                SENSITIVE = YES
                WIDTH = dWidth
                HEIGHT = dHeight
                FGCOLOR = iFGColor
                BGCOLOR = iBGColor
                TOOLTIP = cTooltip
          TRIGGERS:
            ON MOUSE-SELECT-CLICK
              PERSISTENT RUN pClick IN THIS-PROCEDURE (ENTRY(idx,cSections)).
          END TRIGGERS.
        IF VALID-HANDLE(hWidget) THEN DO:
            ttTriggers.hRectangle = hWidget.
            hWidget:MOVE-TO-TOP().
        END.
        IF cText NE ? THEN DO:
            CREATE EDITOR hWidget IN WIDGET-POOL "TouchPool{&PageNo}"
                ASSIGN
                    FRAME = FRAME {&FRAME-NAME}:HANDLE
                    NAME = ENTRY(idx,cSections) + "Text"
                    COL = dCol + 12
                    ROW = dRow + .95
                    SENSITIVE = YES
                    WIDTH = dWidth - 13
                    HEIGHT = dHeight - 1.19
                    FGCOLOR = iFGColor
                    BGCOLOR = iBGColor
                    SCROLLBAR-HORIZONTAL = NO
                    SCROLLBAR-VERTICAL = NO
                    WORD-WRAP = YES
                    READ-ONLY = YES
                    BOX = NO
                    PRIVATE-DATA = cText
                    SCREEN-VALUE = cText
                    TOOLTIP = cTooltip
              TRIGGERS:
                ON MOUSE-SELECT-CLICK
                  PERSISTENT RUN pClick IN THIS-PROCEDURE (ENTRY(idx,cSections)).
              END TRIGGERS.
            IF VALID-HANDLE(hWidget) THEN DO:
                ttTriggers.hEditor = hWidget.
                hWidget:MOVE-TO-TOP().
            END.
        END. /* ctext ne ? */
        IF cText NE ? THEN
        dWidth = dWidth - 28.
        IF dWidth LT 10 THEN
        dWidth = 10.
        CREATE IMAGE hWidget IN WIDGET-POOL "TouchPool{&PageNo}"
            ASSIGN
                FRAME = FRAME {&FRAME-NAME}:HANDLE
                NAME = ENTRY(idx,cSections) + "Image"
                COL = dCol + 1
                ROW = dRow + .23
                SENSITIVE = YES
                WIDTH = dWidth
                HEIGHT = dHeight - .48
                TRANSPARENT = YES
                TOOLTIP = cTooltip
          TRIGGERS:
            ON MOUSE-SELECT-CLICK
              PERSISTENT RUN pClick IN THIS-PROCEDURE (ENTRY(idx,cSections)).
          END TRIGGERS.
        IF VALID-HANDLE(hWidget) THEN DO:
            ttTriggers.hImage = hWidget.
            hWidget:LOAD-IMAGE(SEARCH(cImage)).
            hWidget:MOVE-TO-TOP().
        END.
    END. /* do idx */
    
    UNLOAD tsINI.

END PROCEDURE.

PROCEDURE pSetSensitive:
    DEFINE INPUT PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    
    FIND FIRST ttTriggers
         WHERE ttTriggers.triggerName EQ ipcName
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
