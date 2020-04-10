/* dynWidgets.i - rstark - 2.22.20109 */

DEFINE VARIABLE cAction         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInitialItems   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInitialValue   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamLabel     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE dParamHeight    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dParamWidth     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lIsVisible      AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lMovable        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lResizable      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSelectable     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSensitive      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lShowLabel      AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttParamOrder NO-UNDO
    FIELD paramRow      AS INTEGER
    FIELD paramCol      AS INTEGER
    FIELD paramName     AS CHARACTER
    FIELD paramLabel    AS CHARACTER
    FIELD paramValue    AS CHARACTER
    FIELD paramDataType AS CHARACTER
    FIELD paramFormat   AS CHARACTER
        INDEX ttParamOrder IS PRIMARY
            paramRow
            paramCol
            .

/* **********************  Internal Procedures  *********************** */

PROCEDURE pButtonCalendar:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iphWidget    AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCol       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.

    CREATE BUTTON ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = "btnCalendar-" + ipcName
            COL = ipdCol
            ROW = ipdRow
            WIDTH = 4.6
            HEIGHT = 1.05
            TOOLTIP = "Calendar Popup"
            SENSITIVE = iplSensitive
            HIDDEN = NO
        TRIGGERS:
          ON CHOOSE
            PERSISTENT RUN pCalendar IN THIS-PROCEDURE (iphWidget:HANDLE).
        END TRIGGERS.
    IF VALID-HANDLE(ophWidget) THEN DO:
        ophWidget:LOAD-IMAGE("Graphics\16x16\calendar.bmp").
        opdCol = ophWidget:COL + ophWidget:WIDTH + .4.
    END. /* if valid-handle */
END PROCEDURE.

PROCEDURE pButtonEmail:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iphWidget    AS HANDLE    NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.

    CREATE BUTTON ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = "btnRecipients"
            COL = ipdCol
            ROW = ipdRow
            WIDTH = 4.4
            HEIGHT = 1.05
            TOOLTIP = "Add Recipients"
            SENSITIVE = iplSensitive
            HIDDEN = NO
        TRIGGERS:
          ON CHOOSE
            PERSISTENT RUN pRecipients IN THIS-PROCEDURE (iphWidget:HANDLE).
        END TRIGGERS.
    IF VALID-HANDLE(ophWidget) THEN
    ophWidget:LOAD-IMAGE("AOA\images\navigate_plus.gif").
END PROCEDURE.

PROCEDURE pComboBox:
    DEFINE INPUT  PARAMETER ipcPoolName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame         AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplListItemPairs AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcListItems     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLines         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive     AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible     AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget        AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.

    IF ipcLabel NE "" AND lShowLabel THEN
    hLabel = fCreateLabel(ipcPoolName, iphFrame, ipcLabel, ipdRow, 1).
    CREATE COMBO-BOX ophWidget IN WIDGET-POOL ipcPoolName
      ASSIGN
        FRAME = iphFrame
        NAME = ipcName
        COL = ipdCol
        ROW = ipdRow
        WIDTH = ipdWidth
        FORMAT = ipcFormat
        INNER-LINES = ipiLines
        SIDE-LABEL-HANDLE = hLabel
        SENSITIVE = iplSensitive
        MOVABLE = lMovable
        RESIZABLE = lResizable
        SELECTABLE = lSelectable
    TRIGGERS:
      {AOA/includes/cursorTriggers.i "{2}"}
      ON START-MOVE
        PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
      ON START-RESIZE
        PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
      ON VALUE-CHANGED
        PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget:HANDLE).
    END TRIGGERS.
    IF VALID-HANDLE(ophWidget) THEN DO:
        IF ipcLabel NE "" AND lShowLabel THEN
        hLabel:COL = ophWidget:COL - hLabel:WIDTH.
        IF iplListItemPairs THEN
        ophWidget:LIST-ITEM-PAIRS = ipcListItems.
        ELSE
        ophWidget:LIST-ITEMS = ipcListItems.
        ophWidget:SCREEN-VALUE = ipcValue.
    END. /* if valid-handle */
END PROCEDURE.

PROCEDURE pCreateDynParameters :
    DEFINE INPUT PARAMETER iphFrame AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER iplLive  AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cInitItems  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCol        AS DECIMAL   NO-UNDO INITIAL 1.
    DEFINE VARIABLE dRow        AS DECIMAL   NO-UNDO INITIAL 1.
    DEFINE VARIABLE dRowMax     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSetCol     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSetRow     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hCalendar   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hFrame      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hLabel      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hPickList   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE lSensitive  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pdx         AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    cPoolName = "ParameterPool" + STRING(TIME).
    CREATE WIDGET-POOL cPoolName PERSISTENT.
    
    IF iplLive THEN DO:
        &IF "{1}" EQ "dyn" &THEN
        RUN pGetDynParamValue.
        &ELSE
        RUN pGetDynParamValue (dynSubject.subjectID, "{&defaultUser}", "{&program-id}", 0).
        &ENDIF
        IF NOT AVAILABLE dynParamValue OR
           NOT AVAILABLE dynSubject THEN RETURN.    
        hFrame  = iphFrame.
    END. /* if live */
    
    EMPTY TEMP-TABLE ttDynAction.
    FOR EACH {1}SubjectParamSet NO-LOCK
        WHERE {1}SubjectParamSet.subjectID EQ dynSubject.subjectID,
         EACH dynParamSet NO-LOCK
        WHERE dynParamSet.paramSetID EQ {1}SubjectParamSet.paramSetID,
         EACH dynParamSetDtl NO-LOCK
        WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID,
        FIRST dynParam NO-LOCK
        WHERE dynParam.paramID EQ dynParamSetDtl.paramID
        BREAK BY {1}SubjectParamSet.sortOrder
              BY {1}SubjectParamSet.paramSetID
        :
        IF FIRST-OF({1}SubjectParamSet.paramSetID) THEN DO:
            ASSIGN
                lIsVisible = IF AVAILABLE dynParamValue THEN dynParamValue.isVisible[{1}SubjectParamSet.sortOrder]
                             ELSE {1}SubjectParamSet.isVisible
                lShowLabel = iplLive EQ NO OR lIsVisible
                dSetCol    = IF iplLive THEN {1}SubjectParamSet.setCol ELSE 1
                dSetRow    = IF iplLive THEN {1}SubjectParamSet.setRow ELSE 1
                hFrame     = iphFrame
                .
            IF NOT iplLive THEN DO:
                /* create frame in subject parameter set builder */
                RUN pFrame (
                    cPoolName,
                    iphFrame,
                    STRING({1}SubjectParamSet.paramSetID),
                    {1}SubjectParamSet.setCol,
                    {1}SubjectParamSet.setRow,
                    dynParamSet.setWidth,
                    dynParamSet.setHeight - .74,
                    YES,
                    NO,
                    dynParamSet.setName,
                    lIsVisible,
                    OUTPUT hFrame
                    ).
            END. /* if not live */
        END. /* if first-of */
        /* get all the defaults values needed */
        ASSIGN
            lSensitive  = IF iplLive THEN dynParamSetDtl.paramPrompt ELSE NO
            cParamName  = IF dynParamSetDtl.paramName  NE "" THEN dynParamSetDtl.paramName
                          ELSE dynParam.paramName
            cParamLabel = dynParamSetDtl.paramLabel
            cParamValue = dynParamSetDtl.initialValue
            cInitItems  = dynParamSetDtl.initialItems
            .
&IF "{&program-id}" NE "dynBrowserParam." &THEN
        CASE svSetAlignment:
            WHEN "Custom" THEN
&ENDIF
            ASSIGN
                dCol = dynParamSetDtl.paramCol + dSetCol - 1
                dRow = dynParamSetDtl.paramRow + dSetRow - 1
                .
&IF "{&program-id}" NE "dynBrowserParam." &THEN
        END CASE.
&ENDIF
        /* set screen-value for parameters from dynparamvalue */
        IF iplLive AND lSensitive THEN
        DO pdx = 1 TO EXTENT(dynParamValue.paramName):
            IF dynParamValue.paramName[pdx] EQ "" THEN LEAVE.
            IF dynParamValue.paramName[pdx] NE cParamName THEN NEXT.
            cParamValue = dynParamValue.paramValue[pdx].
            LEAVE.
        END. /* do pdx */
        /* get any initialized values from custom procedures */
        IF dynParamSetDtl.initializeProc NE "" AND
           CAN-DO(hDynInitProc:INTERNAL-ENTRIES,dynParamSetDtl.initializeProc) THEN DO:
            RUN VALUE(dynParamSetDtl.initializeProc) IN hDynInitProc.
            cInitItems = RETURN-VALUE.
            IF INDEX(cInitItems,":DISABLE") NE 0 THEN
            ASSIGN
                lSensitive = NO
                cInitItems = REPLACE(cInitItems,":DISABLE","")
                .
            cParamValue = cInitItems.
        END. /* if initializeProc */
        IF FIRST-OF({1}SubjectParamSet.paramSetID) AND
           dynParamSet.setRectangle THEN DO:
            idx = idx + 1.
            RUN pRectangle (
                cPoolName,
                hFrame,
                STRING(idx),
                dSetCol + 1,
                dSetRow + .48,
                dynParamSet.setWidth - 2.2,
                dynParamSet.setHeight - 1.5,
                iplLive EQ NO OR lIsVisible,
                OUTPUT hWidget
                ).
            IF dynParamSet.setTitle NE "" THEN
            RUN pText (
                cPoolName,
                hFrame,
                hWidget:COL + 2,
                hWidget:ROW - .24,
                dynParamSet.setTitle,
                lIsVisible
                ).
        END. /* if rectangle */
        CASE dynParam.viewAs:
            WHEN "COMBO-BOX" THEN
            RUN pComboBox (
                cPoolName,
                hFrame,
                cParamLabel,
                cParamName,
                dCol,
                dRow,
                dynParamSetDtl.paramWidth,
                CAN-DO(dynParamSetDtl.action,"LIST-ITEM-PAIRS"),
                cInitItems,
                dynParam.paramFormat,
                cParamValue,
                dynParam.innerLines,
                lSensitive,
                lIsVisible,
                OUTPUT hWidget
                ).
            WHEN "EDITOR" THEN DO:
                RUN pEditor (
                    cPoolName,
                    hFrame,
                    cParamLabel,
                    dynParamSetDtl.paramName,
                    dCol,
                    dRow,
                    dynParamSetDtl.paramWidth,
                    dynParamSetDtl.paramHeight,
                    CAN-DO(dynParamSetDtl.action,"HORIZONTAL"),
                    CAN-DO(dynParamSetDtl.action,"VERTICAL"),
                    cParamValue,
                    lSensitive,
                    lIsVisible,
                    OUTPUT hWidget
                    ).
                IF CAN-DO(dynParamSetDtl.action,"EMAIL") THEN DO:
                    ASSIGN
                        dCol = dCol - 5
                        dRow = dRow + .95
                        .
                    RUN pButtonEmail (
                        cPoolName,
                        hFrame,
                        dCol,
                        dRow,
                        lSensitive,
                        hWidget,
                        OUTPUT hCalendar
                        ).
                END. /* if use a calendar */
            END. /* editor */
            WHEN "FILL-IN" THEN DO:
                IF NOT lSensitive THEN
                cParamValue = cInitItems.
                RUN pFillIn (
                    cPoolName,
                    hFrame,
                    cParamLabel,
                    cParamName,
                    dynParam.dataType,
                    dynParam.paramFormat,
                    dCol,
                    dRow,
                    dynParamSetDtl.paramWidth,
                    dynParamSetDtl.paramHeight,
                    cParamValue,
                    lSensitive,
                    lIsVisible,
                    OUTPUT dCol,
                    OUTPUT hWidget
                    ).
                hCalendar = ?.
                IF dynParam.dataType EQ "DATE" AND lIsVisible THEN DO:
                    IF CAN-DO(dynParamSetDtl.action,"CALENDAR") THEN DO:
                        jdx = jdx + 1.
                        RUN pButtonCalendar (
                            cPoolName,
                            hFrame,
                            STRING(jdx),
                            dCol,
                            dRow,
                            lSensitive,
                            lIsVisible,
                            hWidget,
                            OUTPUT dCol,
                            OUTPUT hCalendar
                            ).
                    END. /* if use a calendar */
                    IF CAN-DO(dynParamSetDtl.action,"DATEPICKLIST") THEN DO:
                        kdx = kdx + 1.
                        RUN pPickList (
                            cPoolName,
                            hFrame,
                            STRING(kdx),
                            dCol,
                            dRow,
                            lSensitive,
                            lIsVisible,
                            hWidget,
                            hCalendar
                            ).
                    END. /* if date pick list */
                END. /* if date type */
            END. /* fill-in */
            WHEN "RADIO-SET" THEN
            RUN pRadioSet (
                cPoolName,
                hFrame,
                cParamLabel,
                cParamName,
                cInitItems,
                CAN-DO(dynParamSetDtl.action,"HORIZONTAL"),
                dCol,
                dRow,
                dynParamSetDtl.paramWidth,
                dynParamSetDtl.paramHeight,
                cParamValue,
                lSensitive,
                lIsVisible,
                OUTPUT hWidget
                ).
            WHEN "SELECTION-LIST" THEN DO:
            RUN pSelectionList (
                cPoolName,
                hFrame,
                cParamLabel,
                cParamName,
                dCol,
                dRow,
                dynParamSetDtl.paramWidth,
                dynParamSetDtl.paramHeight,
                CAN-DO(dynParamSetDtl.action,"MULTISELECT"),
                cInitItems,
                cParamValue,
                lSensitive,
                lIsVisible,
                OUTPUT hWidget
                ).
            END. /* selection-list */
            WHEN "TOGGLE-BOX" THEN
            RUN pToggleBox (
                cPoolName,
                hFrame,
                cParamLabel,
                cParamName,
                dCol,
                dRow,
                dynParamSetDtl.paramWidth,
                dynParamSetDtl.paramHeight,
                cParamValue,
                lSensitive,
                lIsVisible,
                OUTPUT hWidget
                ).
        END CASE.
        /* build action table which dictact behavior of the parameter */
        IF VALID-HANDLE(hWidget) THEN DO:
            CREATE ttDynAction.
            ASSIGN
                ttDynAction.paramWidget     = hWidget:HANDLE
                ttDynAction.paramID         = dynParamSetDtl.paramID
                ttDynAction.paramName       = dynParamSetDtl.paramName
                ttDynAction.actionParamName = dynParamSetDtl.actionParamName
                ttDynAction.action          = dynParamSetDtl.action
                ttDynAction.initializeProc  = dynParamSetDtl.initializeProc
                ttDynAction.validateProc    = dynParamSetDtl.validateProc
                ttDynAction.descriptionProc = dynParamSetDtl.descriptionProc
                ttDynAction.initialValue    = dynParamSetDtl.initialValue
                .
            RUN spSetSessionParam (ttDynAction.paramName + "-Handle",STRING(hWidget:HANDLE)).
        END. /* if valid-handle */
        hWidget:HIDDEN = iplLive AND lIsVisible EQ NO.
        hWidget:MOVE-TO-TOP().
        IF LAST-OF({1}SubjectParamSet.paramSetID) THEN DO:
            hFrame:HIDDEN = NO.
            hFrame:MOVE-TO-TOP().
        END. /* if last-of */
    END. /* each {1}SubjectParamSet */
/*    OUTPUT TO c:\tmp\ttDynAction.txt.                                   */
/*    FOR EACH ttDynAction BY ttDynAction.paramName:                      */
/*        DISPLAY ttDynAction EXCEPT paramWidget WITH STREAM-IO WIDTH 500.*/
/*    END.                                                                */
/*    OUTPUT CLOSE.                                                       */
/*    OS-COMMAND NO-WAIT notepad.exe c:\tmp\ttDynAction.txt.              */
    /* get and set the output frame values */
    IF iplLive THEN DO:
&IF "{&program-id}" NE "dynBrowserParam." &THEN
        ASSIGN
            FRAME outputFrame:TITLE = dynSubject.subjectTitle
            hWidget = FRAME outputFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            DO pdx = 1 TO EXTENT(dynParamValue.paramName):
                IF dynParamValue.paramName[pdx] EQ "" THEN LEAVE.
                IF dynParamValue.paramName[pdx] NE hWidget:NAME THEN NEXT.
                hWidget:SCREEN-VALUE = dynParamValue.paramValue[pdx].
                LEAVE.
            END. /* do pdx */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
&ELSE
        /* shift objects up when in dynamic browser parameter */
        ASSIGN
            hWidget = FRAME {&FRAME-NAME}:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "FRAME" THEN 
            hWidget:ROW = hWidget:ROW - 5.
            IF dRowMax LT hWidget:ROW + hWidget:HEIGHT THEN
            dRowMax = hWidget:ROW + hWidget:HEIGHT.
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            dRowMax                            = dRowMax + FRAME FRAME-OK:HEIGHT
            {&WINDOW-NAME}:HEIGHT              = dRowMax
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME FRAME-OK:ROW                 = dRowMax - FRAME FRAME-OK:HEIGHT
            .
&ENDIF
        hFrame:HIDDEN = NO.
        RUN pInitDynParameters (hFrame).
    END. /* if live */
END PROCEDURE.

PROCEDURE pEditor:
    DEFINE INPUT  PARAMETER ipcPoolName   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame      AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol        AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow        AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplHorizontal AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplVertical   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive  AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget     AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.

    CREATE EDITOR ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            COL = ipdCol
            ROW = ipdRow
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            SCROLLBAR-HORIZONTAL = iplHorizontal
            SCROLLBAR-VERTICAL = iplVertical
            WORD-WRAP = NO
            BOX = YES
            SCREEN-VALUE = ipcValue
            SENSITIVE = iplSensitive
            READ-ONLY = iplSensitive EQ NO
            MOVABLE = lMovable
            RESIZABLE = lResizable
            SELECTABLE = lSelectable
        TRIGGERS:
          {AOA/includes/cursorTriggers.i "{2}"}
          ON START-MOVE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON START-RESIZE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON LEAVE
            PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget).
        END TRIGGERS.
    IF ipcLabel NE "" AND lShowLabel THEN
    ASSIGN
        hLabel = fCreateLabel(ipcPoolName, iphFrame, ipcLabel, ipdRow, ipdHeight)
        hLabel:COL = ipdCol - hLabel:WIDTH
        .
END PROCEDURE.

PROCEDURE pFillIn:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDataType  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFormat    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCol       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.

    IF ipcLabel NE "" AND lShowLabel THEN
    hLabel = fCreateLabel(ipcPoolName, iphFrame, ipcLabel, ipdRow, ipdHeight).
    CREATE FILL-IN ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            DATA-TYPE = ipcDataType
            FORMAT = ipcFormat
            COL = ipdCol
            ROW = ipdRow
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            SCREEN-VALUE = ipcValue
            SIDE-LABEL-HANDLE = hLabel
            SENSITIVE = iplSensitive
            READ-ONLY = iplSensitive EQ NO
            BGCOLOR = IF iplSensitive THEN ? ELSE 8
            MOVABLE = lMovable
            RESIZABLE = lResizable
            SELECTABLE = lSelectable
        TRIGGERS:
          {AOA/includes/cursorTriggers.i "{2}"}
          ON START-MOVE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON START-RESIZE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON LEAVE
            PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget).
        END TRIGGERS.
    IF ipcLabel NE "" AND lShowLabel THEN
    ASSIGN
        hLabel:COL = ophWidget:COL - hLabel:WIDTH
        opdCol = ophWidget:COL + ophWidget:WIDTH + .4
        .
END PROCEDURE.

PROCEDURE pFrame:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplMovable   AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplResizable AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTitle     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.

    CREATE FRAME ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = "SetFrame" + ipcName
            COL = ipdCol
            ROW = ipdRow
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            PRIVATE-DATA = ipcName
            OVERLAY = YES
            BOX-SELECTABLE = iplResizable
            MOVABLE = iplMovable
            RESIZABLE = iplResizable
            SELECTABLE = iplMovable OR iplResizable
            BGCOLOR = IF iplIsVisible THEN ? ELSE 0
            FGCOLOR = IF iplIsVisible THEN ? ELSE 15
            HIDDEN = YES
        TRIGGERS:
          &IF "{2}" EQ "SubjectBuilder" &THEN
          ON CURSOR-DOWN
            PERSISTENT RUN pFrameMove IN THIS-PROCEDURE (iphFrame, "Down").
          ON CURSOR-LEFT
            PERSISTENT RUN pFrameMove IN THIS-PROCEDURE (iphFrame, "Left").
          ON CURSOR-RIGHT
            PERSISTENT RUN pFrameMove IN THIS-PROCEDURE (iphFrame, "Right").
          ON CURSOR-UP
            PERSISTENT RUN pFrameMove IN THIS-PROCEDURE (iphFrame, "Up").
          &ENDIF
          &IF "{2}" EQ "ParamBuilder" &THEN
          ON MOUSE-SELECT-DBLCLICK
            PERSISTENT RUN pMouseSelectDBLClick IN THIS-PROCEDURE (ophWidget).
          &ENDIF
          ON END-RESIZE
            PERSISTENT RUN pFrameResize IN THIS-PROCEDURE (ophWidget).
          ON START-MOVE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON START-RESIZE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
        END TRIGGERS.
        IF iplResizable THEN
        ophWidget:TITLE = ipcTitle.
END PROCEDURE.

&IF "{2}" EQ "SubjectBuilder" &THEN
PROCEDURE pFrameMove:
    DEFINE INPUT PARAMETER iphFrame  AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcMove   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    ASSIGN
        hWidget = iphFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:SELECTED THEN
        CASE ipcMove:
            WHEN "Down"  THEN
                IF hWidget:ROW + hWidget:HEIGHT + iphFrame:GRID-UNIT-HEIGHT-CHARS - 1 LT iphFrame:HEIGHT THEN
                hWidget:ROW = hWidget:ROW + iphFrame:GRID-UNIT-HEIGHT-CHARS.
            WHEN "Left"  THEN
                IF hWidget:COL - iphFrame:GRID-UNIT-WIDTH-CHARS GE 1 THEN
                hWidget:COL = hWidget:COL - iphFrame:GRID-UNIT-WIDTH-CHARS.
            WHEN "Right" THEN
                IF hWidget:COL + hWidget:WIDTH + iphFrame:GRID-UNIT-WIDTH-CHARS - 1 LT iphFrame:WIDTH THEN
                hWidget:COL = hWidget:COL + iphFrame:GRID-UNIT-WIDTH-CHARS.
            WHEN "Up"    THEN
                IF hWidget:ROW - iphFrame:GRID-UNIT-HEIGHT-CHARS GE 1 THEN
                hWidget:ROW = hWidget:ROW - iphFrame:GRID-UNIT-HEIGHT-CHARS.
        END CASE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    RUN pSetSaveReset (YES).

END PROCEDURE.
&ENDIF

&IF "{2}" EQ "ParamBuilder" &THEN
PROCEDURE pMouseSelectDBLClick:
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE dHeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    
    ASSIGN
        hWidget = iphFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:TYPE NE "RECTANGLE" THEN DO:
            IF hWidget:ROW + hWidget:HEIGHT GT dHeight THEN
            dHeight = hWidget:ROW + hWidget:HEIGHT.
            IF hWidget:COL + hWidget:WIDTH  GT dWidth  THEN
            dWidth  = hWidget:COL + hWidget:WIDTH.
        END. /* if ne rectangle */
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    ASSIGN
        iphFrame:VIRTUAL-HEIGHT = dHeight + .38
        iphFrame:HEIGHT         = iphFrame:VIRTUAL-HEIGHT
        iphFrame:VIRTUAL-WIDTH  = dWidth  + 1
        iphFrame:WIDTH          = iphFrame:VIRTUAL-WIDTH
        .
    RUN pFrameResize (iphFrame).

END PROCEDURE.
&ENDIF

PROCEDURE pPickList:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iphWidget    AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER iphCalendar  AS HANDLE    NO-UNDO.

    DEFINE VARIABLE hPickList AS HANDLE NO-UNDO.

    CREATE COMBO-BOX hPickList IN WIDGET-POOL ipcPoolName
      ASSIGN
        FRAME = iphFrame
        NAME = "DatePickList-" + ipcName
        FORMAT = "x(256)"
        COL = ipdCol
        ROW = ipdRow
        WIDTH = 25
        TOOLTIP = "Date Pick List"
        PRIVATE-DATA = iphWidget:NAME
        SENSITIVE = iplSensitive
        HIDDEN = NO
    TRIGGERS:
      ON VALUE-CHANGED
        PERSISTENT RUN pDatePickList IN THIS-PROCEDURE (
            iphWidget:HANDLE,
            iphCalendar:HANDLE,
            hPickList:HANDLE
            ).
    END TRIGGERS.
    fDateOptions(hPickList).
    ASSIGN
        hPickList:SCREEN-VALUE = hPickList:ENTRY(1)
        iphWidget:PRIVATE-DATA = hPickList:SCREEN-VALUE
        .
END PROCEDURE.

PROCEDURE pRadioSet:
    DEFINE INPUT  PARAMETER ipcPoolName   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame      AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcButtons    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplHorizontal AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol        AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow        AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive  AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget     AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.
    
    CREATE RADIO-SET ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            RADIO-BUTTONS = ipcButtons
            HORIZONTAL = iplHorizontal
            COL = ipdCol
            ROW = ipdRow
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            SCREEN-VALUE = ipcValue
            SENSITIVE = iplSensitive
            MOVABLE = lMovable
            RESIZABLE = lResizable
            SELECTABLE = lSelectable
        TRIGGERS:
          {AOA/includes/cursorTriggers.i "{2}"}
          ON START-MOVE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON START-RESIZE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON VALUE-CHANGED
            PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget).
        END TRIGGERS.
    IF ipcLabel NE "" AND lShowLabel THEN
    ASSIGN
        hLabel = fCreateLabel(ipcPoolName, iphFrame, ipcLabel, ipdRow, ipdHeight)
        hLabel:COL = ipdCol - hLabel:WIDTH
        .
END PROCEDURE.

PROCEDURE pRectangle:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.
    
    CREATE RECTANGLE ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = "RECT-" + ipcName
            GRAPHIC-EDGE = YES
            ROUNDED = YES
            WIDTH-PIXELS = 1
            COL = ipdCol
            ROW = ipdRow
            SENSITIVE = NO
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            FILLED = NO
            HIDDEN = iplIsVisible EQ NO
            .
END PROCEDURE.

PROCEDURE pSelectionList:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplMultiple  AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcListItems AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.

    CREATE SELECTION-LIST ophWidget IN WIDGET-POOL ipcPoolName
      ASSIGN
        FRAME = iphFrame
        NAME = ipcName
        COL = ipdCol
        ROW = ipdRow
        WIDTH = ipdWidth
        HEIGHT = ipdHeight
        SCROLLBAR-VERTICAL = YES
        MULTIPLE = iplMultiple
        LIST-ITEMS = ipcListItems
        SCREEN-VALUE = ipcValue
        SENSITIVE = iplSensitive
        MOVABLE = lMovable
        RESIZABLE = lResizable
        SELECTABLE = lSelectable
    TRIGGERS:
      {AOA/includes/cursorTriggers.i "{2}"}
      ON START-MOVE
        PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
      ON START-RESIZE
        PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
      ON VALUE-CHANGED
        PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget).
    END TRIGGERS.
    IF ipcLabel NE "" AND lShowLabel THEN
    ASSIGN
        hLabel = fCreateLabel(ipcPoolName, iphFrame, ipcLabel, ipdRow, ipdHeight)
        hLabel:COL = ipdCol - hLabel:WIDTH
        .
END PROCEDURE.

PROCEDURE pText:
    DEFINE INPUT PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcText      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE hLabel AS HANDLE NO-UNDO.
    
    IF NOT iplIsVisible THEN RETURN.
    
    CREATE TEXT hLabel IN WIDGET-POOL ipcPoolName
      ASSIGN
        FRAME = iphFrame
        AUTO-RESIZE = YES
        HEIGHT = .62
        WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(" " + ipcText,iphFrame:FONT) + 1
        COL = ipdCol
        ROW = ipdRow
        FORMAT = "x(" + STRING(LENGTH(" " + ipcText)) + ")"
        SCREEN-VALUE = " " + ipcText
        SENSITIVE = YES
        HIDDEN = NO
        .
END PROCEDURE.

PROCEDURE pToggleBox:
    DEFINE INPUT  PARAMETER ipcPoolName  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iphFrame     AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLabel     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcName      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCol       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRow       AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdWidth     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdHeight    AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplSensitive AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsVisible AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER ophWidget    AS HANDLE    NO-UNDO.

    CREATE TOGGLE-BOX ophWidget IN WIDGET-POOL ipcPoolName
        ASSIGN
            FRAME = iphFrame
            NAME = ipcName
            LABEL = ipcLabel
            COL = ipdCol
            ROW = ipdRow
            WIDTH = ipdWidth
            HEIGHT = ipdHeight
            SCREEN-VALUE = ipcValue
            SENSITIVE = iplSensitive
            MOVABLE = lMovable
            RESIZABLE = lResizable
            SELECTABLE = lSelectable
        TRIGGERS:
          {AOA/includes/cursorTriggers.i "{2}"}
          ON START-MOVE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON START-RESIZE
            PERSISTENT RUN pSetSaveReset IN THIS-PROCEDURE (YES).
          ON VALUE-CHANGED
            PERSISTENT RUN pParamValidate IN THIS-PROCEDURE (ophWidget).
        END TRIGGERS.
END PROCEDURE.

&IF "{2}" EQ "ParamBuilder" &THEN
PROCEDURE pWidgetMove:
    DEFINE INPUT PARAMETER iphFrame AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcMove  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    ASSIGN
        hWidget = iphFrame:HANDLE
        hWidget = hWidget:FIRST-CHILD
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        IF hWidget:SELECTED THEN
        CASE ipcMove:
            WHEN "Down"  THEN
                IF hWidget:ROW + hWidget:HEIGHT + iphFrame:GRID-UNIT-HEIGHT-CHARS LT iphFrame:HEIGHT THEN
                hWidget:ROW = hWidget:ROW + iphFrame:GRID-UNIT-HEIGHT-CHARS.
            WHEN "Left"  THEN
                IF hWidget:COL - iphFrame:GRID-UNIT-WIDTH-CHARS GE 1 THEN
                hWidget:COL = hWidget:COL - iphFrame:GRID-UNIT-WIDTH-CHARS.
            WHEN "Right" THEN
                IF hWidget:COL + hWidget:WIDTH + iphFrame:GRID-UNIT-WIDTH-CHARS - 1 LT iphFrame:WIDTH THEN
                hWidget:COL = hWidget:COL + iphFrame:GRID-UNIT-WIDTH-CHARS.
            WHEN "Up"    THEN
                IF hWidget:ROW - iphFrame:GRID-UNIT-HEIGHT-CHARS GE 1 THEN
                hWidget:ROW = hWidget:ROW - iphFrame:GRID-UNIT-HEIGHT-CHARS.
        END CASE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */
    RUN pSetSaveReset (YES).

END PROCEDURE.
&ENDIF

&IF "{2}" EQ "ParamBuilder" &THEN
PROCEDURE pWidgetSize:
    DEFINE INPUT PARAMETER iphFrame AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcMove  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    IF iphFrame:SELECTED THEN DO:
        CASE ipcMove:
            WHEN "Down"  THEN
            ASSIGN
                iphFrame:VIRTUAL-HEIGHT = iphFrame:VIRTUAL-HEIGHT + iphFrame:GRID-UNIT-HEIGHT-CHARS
                iphFrame:HEIGHT         = iphFrame:VIRTUAL-HEIGHT
                .
            WHEN "Left"  THEN
            ASSIGN
                iphFrame:VIRTUAL-WIDTH  = iphFrame:VIRTUAL-WIDTH  - iphFrame:GRID-UNIT-WIDTH-CHARS
                iphFrame:WIDTH          = iphFrame:VIRTUAL-WIDTH
                .
            WHEN "Right" THEN
            ASSIGN
                iphFrame:VIRTUAL-WIDTH  = iphFrame:VIRTUAL-WIDTH  + iphFrame:GRID-UNIT-WIDTH-CHARS
                iphFrame:WIDTH          = iphFrame:VIRTUAL-WIDTH
                .
            WHEN "Up"    THEN
            ASSIGN
                iphFrame:VIRTUAL-HEIGHT = iphFrame:VIRTUAL-HEIGHT - iphFrame:GRID-UNIT-HEIGHT-CHARS
                iphFrame:HEIGHT         = iphFrame:VIRTUAL-HEIGHT
                .
        END CASE.
        RUN pFrameResize (iphFrame).
    END. /* if selected */
    ELSE DO:
        ASSIGN
            hWidget = iphFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:SELECTED THEN
            CASE ipcMove:
                WHEN "Down"  THEN
                    IF hWidget:ROW + hWidget:HEIGHT + iphFrame:GRID-UNIT-HEIGHT-CHARS LT iphFrame:HEIGHT THEN
                    hWidget:HEIGHT = hWidget:HEIGHT + iphFrame:GRID-UNIT-HEIGHT-CHARS.
                WHEN "Left"  THEN
                    IF hWidget:WIDTH - iphFrame:GRID-UNIT-WIDTH-CHARS GE 1 THEN
                    hWidget:WIDTH = hWidget:WIDTH - iphFrame:GRID-UNIT-WIDTH-CHARS.
                WHEN "Right" THEN
                    IF hWidget:COL + hWidget:WIDTH + iphFrame:GRID-UNIT-WIDTH-CHARS LT iphFrame:WIDTH THEN
                    hWidget:WIDTH = hWidget:WIDTH + iphFrame:GRID-UNIT-WIDTH-CHARS.
                WHEN "Up"    THEN
                    IF hWidget:HEIGHT - iphFrame:GRID-UNIT-HEIGHT-CHARS GE 1 THEN
                    hWidget:HEIGHT = hWidget:HEIGHT - iphFrame:GRID-UNIT-HEIGHT-CHARS.
            END CASE.
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
    END. /* else */
    RUN pSetSaveReset (YES).

END PROCEDURE.
&ENDIF
