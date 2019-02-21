/* pDynParamProcs.i - rstark - 2.18.2019 */

PROCEDURE pCalendar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
    
    RUN nosweat/popupcal.w (OUTPUT calendarDate).
    IF calendarDate NE '' THEN
    iphWidget:SCREEN-VALUE = calendarDate.
    RETURN NO-APPLY.

END PROCEDURE.

PROCEDURE pCreateDynParameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCol        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dRow        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hCalendar   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hLabel      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hPickList   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hWidget     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE kdx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE pdx         AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    cPoolName = "ParameterPool" + STRING(TIME).
    CREATE WIDGET-POOL cPoolName PERSISTENT.
    
    &IF "{1}" EQ "dyn" &THEN
    RUN pGetDynParamValue.
    &ELSE
    RUN pGetDynParamValue (dynSubject.subjectID, "{&defaultUser}", "{&program-id}", 0).
    &ENDIF
    IF NOT AVAILABLE dynParamValue OR
       NOT AVAILABLE dynSubject THEN RETURN.    
    
    EMPTY TEMP-TABLE ttAction.
    FOR EACH {1}SubjectParamSet
        WHERE {1}SubjectParamSet.subjectID EQ dynSubject.subjectID,
        EACH dynParamSet NO-LOCK
        WHERE dynParamSet.paramSetID EQ {1}SubjectParamSet.paramSetID,
        EACH dynParamSetDtl
        WHERE dynParamSetDtl.paramSetID EQ dynParamSet.paramSetID,
        FIRST dynParam NO-LOCK
        WHERE dynParam.paramID EQ dynParamSetDtl.paramID
        :
        ASSIGN
            cParamName  = IF dynParamSetDtl.paramName  NE "" THEN dynParamSetDtl.paramName
                          ELSE dynParam.paramName
            cParamLabel = IF dynParamSetDtl.paramLabel NE "" THEN dynParamSetDtl.paramLabel
                          ELSE dynParam.paramLabel
            cParamValue = dynParamSetDtl.initialValue
            dCol        = dynParamSetDtl.paramCol + {1}SubjectParamSet.setCol - 1
            dRow        = dynParamSetDtl.paramRow + {1}SubjectParamSet.setRow - 1
            .
        DO pdx = 1 TO EXTENT(dynParamValue.paramName):
            IF dynParamValue.paramName[pdx] EQ "" THEN LEAVE.
            IF dynParamValue.paramName[pdx] NE cParamName THEN NEXT.
            cParamValue = dynParamValue.paramValue[pdx].
            LEAVE.
        END. /* do pdx */
        IF dynParamSet.setRectangle THEN DO:
            idx = idx + 1.
            CREATE RECTANGLE hWidget IN WIDGET-POOL cPoolName
                ASSIGN
                    FRAME = iphFrame
                    NAME = "RECT-" + STRING(idx)
                    GRAPHIC-EDGE = YES
                    ROUNDED = YES
                    WIDTH-PIXELS = 1
                    COL = {1}SubjectParamSet.setCol + 1
                    ROW = {1}SubjectParamSet.setRow + .48
                    SENSITIVE = NO
                    WIDTH = dynParamSet.setWidth - 2.2
                    HEIGHT = dynParamSet.setHeight - .76
                    FILLED = NO
                    .
            IF dynParamSet.setTitle NE "" THEN
            CREATE TEXT hLabel IN WIDGET-POOL cPoolName
              ASSIGN
                FRAME = iphFrame
                AUTO-RESIZE = YES
                HEIGHT = .62
                WIDTH = FONT-TABLE:GET-TEXT-WIDTH-CHARS(" " + dynParamSet.setTitle,iphFrame:FONT) + 1
                COL = hWidget:COL + 2
                ROW = hWidget:ROW - .24
                FORMAT = "x(" + STRING(LENGTH(" " + dynParamSet.setTitle)) + ")"
                SCREEN-VALUE = " " + dynParamSet.setTitle
                SENSITIVE = YES
                .
        END. /* if rectangle */
        CASE dynParam.viewAs:
            WHEN "COMBO-BOX" THEN DO:
                hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow).
                CREATE COMBO-BOX hWidget IN WIDGET-POOL cPoolName
                  ASSIGN
                    FRAME = iphFrame
                    NAME = cParamName
                    COL = dCol + 2
                    ROW = dRow
                    WIDTH = dynParam.paramWidth
                    LIST-ITEMS = dynParamSetDtl.initialItems
                    FORMAT = dynParam.paramFormat
                    SCREEN-VALUE = cParamValue
                    INNER-LINES = dynParam.innerLines
                    SIDE-LABEL-HANDLE = hLabel
                    SENSITIVE = dynParamSetDtl.paramPrompt
                TRIGGERS:
                  ON VALUE-CHANGED
                    PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                END TRIGGERS.
                hLabel:COL = hWidget:COL - hLabel:WIDTH.
            END. /* combo-box */
            WHEN "EDITOR" THEN DO:
                CREATE EDITOR hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = dynParamSetDtl.paramName
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCROLLBAR-HORIZONTAL = CAN-DO(dynParamSetDtl.action,"HORIZONTAL")
                        SCROLLBAR-VERTICAL = CAN-DO(dynParamSetDtl.action,"VERTICAL")
                        WORD-WRAP = NO
                        BOX = YES
                        SCREEN-VALUE = cParamValue
                        SENSITIVE = YES
                        READ-ONLY = dynParamSetDtl.paramPrompt EQ NO
                    TRIGGERS:
                      ON LEAVE
                        PERSISTENT RUN pValidate IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
                IF CAN-DO(dynParamSetDtl.action,"EMAIL") THEN DO:
                    ASSIGN
                        dCol = dCol - 5
                        dRow = dRow + .95
                        .
                    CREATE BUTTON hCalendar IN WIDGET-POOL cPoolName
                        ASSIGN
                            FRAME = iphFrame
                            NAME = "btnRecipients"
                            COL = dCol
                            ROW = dRow
                            WIDTH = 4.4
                            HEIGHT = 1.05
                            TOOLTIP = "Add Recipients"
                            SENSITIVE = dynParamSetDtl.paramPrompt
                        TRIGGERS:
                          ON CHOOSE
                            PERSISTENT RUN pRecipients IN THIS-PROCEDURE (hWidget:HANDLE).
                        END TRIGGERS.
                    IF VALID-HANDLE(hCalendar) THEN
                    hCalendar:LOAD-IMAGE("AOA\images\navigate_plus.gif").
                END. /* if use a calendar */
            END. /* editor */
            WHEN "FILL-IN" THEN DO:
                hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow).
                CREATE FILL-IN hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        DATA-TYPE = dynParam.dataType
                        FORMAT = dynParam.paramFormat
                        COL = dCol + 2
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = cParamValue
                        SIDE-LABEL-HANDLE = hLabel
                        SENSITIVE = YES
                        READ-ONLY = dynParamSetDtl.paramPrompt EQ NO
                    TRIGGERS:
                      ON LEAVE
                        PERSISTENT RUN pValidate IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel:COL = hWidget:COL - hLabel:WIDTH
                    dCol = hWidget:COL + hWidget:WIDTH + .4
                    hCalendar = ?
                    .
                IF dynParam.dataType EQ "DATE" THEN DO:
                    IF CAN-DO(dynParamSetDtl.action,"CALENDAR") THEN DO:
                        jdx  = jdx + 1.
                        CREATE BUTTON hCalendar IN WIDGET-POOL cPoolName
                            ASSIGN
                                FRAME = iphFrame
                                NAME = "btnCalendar-" + STRING(jdx)
                                COL = dCol
                                ROW = dRow
                                WIDTH = 4.6
                                HEIGHT = 1.05
                                TOOLTIP = "Calendar Popup"
                                SENSITIVE = dynParamSetDtl.paramPrompt
                            TRIGGERS:
                              ON CHOOSE
                                PERSISTENT RUN pCalendar IN THIS-PROCEDURE (hWidget:HANDLE).
                            END TRIGGERS.
                        IF VALID-HANDLE(hCalendar) THEN DO:
                            hCalendar:LOAD-IMAGE("Graphics\16x16\calendar.bmp").
                            dCol = hCalendar:COL + hCalendar:WIDTH + .4.
                        END. /* if valid-handle */
                    END. /* if use a calendar */
                    IF CAN-DO(dynParamSetDtl.action,"DATEPICKLIST") THEN DO:
                        kdx = kdx + 1.
                        CREATE COMBO-BOX hPickList IN WIDGET-POOL cPoolName
                          ASSIGN
                            FRAME = iphFrame
                            NAME = "DatePickList-" + STRING(kdx)
                            FORMAT = "x(256)"
                            COL = dCol
                            ROW = dRow
                            WIDTH = 25
                            TOOLTIP = "Date Pick List"
                            PRIVATE-DATA = hWidget:NAME
                            SENSITIVE = dynParamSetDtl.paramPrompt
                        TRIGGERS:
                          ON VALUE-CHANGED
                            PERSISTENT RUN pDatePickList IN THIS-PROCEDURE (
                                hWidget:HANDLE,
                                hCalendar:HANDLE,
                                hPickList:HANDLE
                                ).
                        END TRIGGERS.
                        fDateOptions(hPickList).
                        ASSIGN
                            hPickList:SCREEN-VALUE = hPickList:ENTRY(1)
                            hWidget:PRIVATE-DATA = hPickList:SCREEN-VALUE
                            .
                    END. /* if date pick list */
                END. /* if date type */
            END. /* fill-in */
            WHEN "RADIO-SET" THEN DO:
                CREATE RADIO-SET hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        RADIO-BUTTONS = dynParamSetDtl.initialItems
                        HORIZONTAL = CAN-DO(dynParamSetDtl.action,"HORIZONTAL")
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = cParamValue
                        SENSITIVE = dynParamSetDtl.paramPrompt
                    TRIGGERS:
                      ON VALUE-CHANGED
                        PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
            END. /* radio-set */
            WHEN "SELECTION-LIST" THEN DO:
                CREATE SELECTION-LIST hWidget IN WIDGET-POOL cPoolName
                  ASSIGN
                    FRAME = iphFrame
                    NAME = cParamName
                    COL = dCol
                    ROW = dRow
                    WIDTH = dynParam.paramWidth
                    SCROLLBAR-VERTICAL = YES
                    MULTIPLE = CAN-DO(dynParamSetDtl.action,"MULTISELECT")
                    LIST-ITEMS = dynParamSetDtl.initialItems
                    SCREEN-VALUE = cParamValue
                    INNER-LINES = dynParam.innerLines
                    SENSITIVE = dynParamSetDtl.paramPrompt
                TRIGGERS:
                  ON VALUE-CHANGED
                    PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                END TRIGGERS.
                ASSIGN
                    hLabel = fCreateLabel(cPoolName, iphFrame, cParamLabel, dRow)
                    hLabel:COL = dCol - hLabel:WIDTH
                    .
            END. /* selection-list */
            WHEN "TOGGLE-BOX" THEN DO:
                CREATE TOGGLE-BOX hWidget IN WIDGET-POOL cPoolName
                    ASSIGN
                        FRAME = iphFrame
                        NAME = cParamName
                        LABEL = cParamLabel
                        COL = dCol
                        ROW = dRow
                        WIDTH = dynParam.paramWidth
                        HEIGHT = dynParam.paramHeight
                        SCREEN-VALUE = cParamValue
                        SENSITIVE = dynParamSetDtl.paramPrompt
                    TRIGGERS:
                      ON VALUE-CHANGED
                        PERSISTENT RUN pParamAction IN THIS-PROCEDURE (hWidget:HANDLE).
                    END TRIGGERS.
            END. /* toggle-box */
        END CASE.
        IF VALID-HANDLE(hWidget) THEN DO:
            CREATE ttAction.
            ASSIGN
                ttAction.paramWidget    = hWidget:HANDLE
                ttAction.paramID        = dynParamSetDtl.paramID
                ttAction.actionParamID  = dynParamSetDtl.actionParamID
                ttAction.action         = dynParamSetDtl.action
                ttAction.initializeProc = dynParam.initializeProc
                ttAction.validateProc   = dynParam.validateProc
                .
        END. /* if valid-handle */
    END. /* each {1}SubjectParamSet */
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
    iphFrame:HIDDEN = NO.
    RUN pInitialize.
    RUN pInitDynParameters (iphFrame).

END PROCEDURE.

PROCEDURE pDatePickList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget   AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphCalendar AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphPickList AS HANDLE NO-UNDO.
    
    ASSIGN
        iphWidget:SCREEN-VALUE = STRING(fDateOptionValue(
            iphPickList:SCREEN-VALUE,
            DATE(iphWidget:SCREEN-VALUE)
            ))
        iphWidget:READ-ONLY = iphPickList:SCREEN-VALUE NE "Fixed Date"
        iphWidget:PRIVATE-DATA = iphPickList:SCREEN-VALUE
        .
    IF VALID-HANDLE(iphCalendar) THEN
    iphCalendar:SENSITIVE = iphPickList:SCREEN-VALUE EQ "Fixed Date".

END PROCEDURE.

PROCEDURE pInitDynParameters :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    ASSIGN 
        hWidget = iphFrame:HANDLE 
        hWidget = hWidget:FIRST-CHILD 
        hWidget = hWidget:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hWidget):
        CASE hWidget:TYPE:
            WHEN "COMBO-BOX" OR
            WHEN "RADIO-SET" OR
            WHEN "SELECTION-LIST" OR
            WHEN "TOGGLE-BOX" THEN
                APPLY "VALUE-CHANGED":U TO hWidget.
            WHEN "EDITOR" OR
            WHEN "FILL-IN" THEN
                APPLY "LEAVE":U TO hWidget.
        END CASE.
        hWidget = hWidget:NEXT-SIBLING.
    END. /* do while */

END PROCEDURE.

PROCEDURE pInitialize:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
/*    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.*/
/*    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.*/
/*                                                 */
/*    IF dynParam.initializeProc EQ "" THEN RETURN.*/
/*    RUN VALUE(dynParam.initializeProc).          */
/*    cReturn = RETURN:VALUE.                      */
/*    IF cReturn EQ "" THEN RETURN.                */
/*    DO idx = 1 TO NUM-ENTRIES(cReturn):          */
/*    END. /* do idx */                            */

END PROCEDURE.

PROCEDURE pParamAction :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cAction  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParamID AS INTEGER   NO-UNDO.
    
    FIND FIRST ttAction
         WHERE ttACtion.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttAction THEN RETURN.
    iParamID = ttAction.paramID.
    FOR EACH ttAction
        WHERE ttAction.actionParamID EQ iParamID
          AND ttAction.action NE "",
        FIRST dynParam
        WHERE dynParam.paramID EQ ttAction.paramID
        :
        DO idx = 1 TO NUM-ENTRIES(ttAction.action):
            ASSIGN
                cValue  = ENTRY(1,ENTRY(idx,ttAction.action),":")
                cAction = ENTRY(2,ENTRY(idx,ttAction.action),":")
                .
            IF iphWidget:SCREEN-VALUE EQ cValue THEN
            CASE cAction:
                WHEN "DISABLE" THEN
                ttAction.paramWidget:READ-ONLY = YES.
                WHEN "ENABLE" THEN
                ttAction.paramWidget:READ-ONLY = NO.
                WHEN "HI" THEN
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = CHR(254).
                    WHEN "DECIMAL" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "99999999.99".
                    WHEN "DATE" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "12/31/2049".
                    WHEN "INTEGER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "99999999".
                END CASE.
                WHEN "LOW" THEN
                CASE dynParam.dataType:
                    WHEN "CHARACTER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = CHR(32).
                    WHEN "DECIMAL" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "-99999999.99".
                    WHEN "DATE" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "1/1/1950".
                    WHEN "INTEGER" THEN
                    ttAction.paramWidget:SCREEN-VALUE = "-99999999".
                END CASE.
            END CASE.
        END. /* do idx */
    END. /* each ttaction */

END PROCEDURE.

PROCEDURE pRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = iphWidget:SCREEN-VALUE.
    RUN AOA/Recipients.w (INPUT-OUTPUT cRecipients).
    iphWidget:SCREEN-VALUE = cRecipients.

END PROCEDURE.

PROCEDURE pSaveDynParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFormat AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
            dynParamValue.outputFormat  = ipcOutputFormat
            dynParamValue.paramName     = ""
            dynParamValue.paramLabel    = ""
            dynParamValue.paramValue    = ""
            dynParamValue.paramDataType = ""
            dynParamValue.paramFormat   = ""
            hWidget = FRAME paramFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF hWidget:TYPE NE "BUTTON"    AND
               hWidget:TYPE NE "FRAME"     AND
               hWidget:TYPE NE "RECTANGLE" AND
               hWidget:TYPE NE "TEXT" THEN DO:
                ASSIGN
                    idx = idx + 1
                    dynParamValue.paramName[idx]     = hWidget:NAME
                    dynParamValue.paramLabel[idx]    = hWidget:LABEL
                    dynParamValue.paramValue[idx]    = hWidget:SCREEN-VALUE
                    dynParamValue.paramDataType[idx] = hWidget:DATA-TYPE
                    .
                IF CAN-DO("COMBO-BOX,FILL-IN",hWidget:TYPE) THEN
                dynParamValue.paramFormat[idx] = hWidget:FORMAT.
            END. /* if type */
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        ASSIGN
            hWidget = FRAME outputFrame:HANDLE
            hWidget = hWidget:FIRST-CHILD
            hWidget = hWidget:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hWidget):
            IF CAN-DO("EDITOR,TOGGLE-BOX",hWidget:TYPE) THEN
            ASSIGN
                idx = idx + 1
                dynParamValue.paramName[idx]     = hWidget:NAME
                dynParamValue.paramLabel[idx]    = hWidget:LABEL
                dynParamValue.paramValue[idx]    = hWidget:SCREEN-VALUE
                dynParamValue.paramDataType[idx] = hWidget:DATA-TYPE
                .
            hWidget = hWidget:NEXT-SIBLING.
        END. /* do while */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */

END PROCEDURE.

PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    RUN pParamAction (iphWidget).

END PROCEDURE.
