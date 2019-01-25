&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : dynProcs.i
    Purpose     : shared procedures used in dynamic modules

    Syntax      : AOA/includes/dynProces.i

    Description : Dynamic Procedures

    Author(s)   : Ron Stark
    Created     : 1.25.2019
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalendar Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDatePickList Include 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDynParameters Include 
PROCEDURE pDynParameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cParamName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParamValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPoolName   AS CHARACTER NO-UNDO INITIAL "ParameterPool".
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
    
    DELETE WIDGET-POOL "parameterPool" NO-ERROR.
    CREATE WIDGET-POOL "parameterPool" PERSISTENT.
    
    RUN pGetDefault.    
    EMPTY TEMP-TABLE ttAction.    
    FOR EACH {1}SubjectParamSet
        WHERE {1}SubjectParamSet.subjectID EQ {1}Subject.subjectID,
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
    iphFrame:HIDDEN = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pParamAction Include 
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
    
    DEFINE BUFFER bttAction FOR ttAction.
    
    FIND FIRST ttAction
         WHERE ttACtion.paramWidget EQ iphWidget
         NO-ERROR.
    IF NOT AVAILABLE ttAction THEN RETURN.
    iParamID = ttAction.paramID.
    FOR EACH ttAction
        WHERE ttAction.actionParamID EQ iParamID
          AND ttAction.action NE ""
        :
        DO idx = 1 TO NUM-ENTRIES(ttAction.action):
            ASSIGN
                cValue  = ENTRY(1,ENTRY(idx,ttAction.action),":")
                cAction = ENTRY(2,ENTRY(idx,ttAction.action),":")
                .
            IF iphWidget:SCREEN-VALUE EQ cValue THEN
            FOR EACH bttAction
                WHERE bttAction.paramID EQ ttAction.paramID,
                FIRST dynParam
                WHERE dynParam.paramID EQ bttAction.paramID
                :
                CASE cAction:
                    WHEN "DISABLE" THEN
                    bttACtion.paramWidget:READ-ONLY = YES.
                    WHEN "ENABLE" THEN
                    bttACtion.paramWidget:READ-ONLY = NO.
                    WHEN "HI" THEN
                    CASE dynParam.dataType:
                        WHEN "CHARACTER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = CHR(254).
                        WHEN "DECIMAL" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "99999999.99".
                        WHEN "DATE" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "12/31/2049".
                        WHEN "INTEGER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "99999999".
                    END CASE.
                    WHEN "LOW" THEN
                    CASE dynParam.dataType:
                        WHEN "CHARACTER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = CHR(32).
                        WHEN "DECIMAL" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "-99999999.99".
                        WHEN "DATE" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "1/1/1950".
                        WHEN "INTEGER" THEN
                        bttACtion.paramWidget:SCREEN-VALUE = "-99999999".
                    END CASE.
                END CASE.
            END. /* each bttaction */
        END. /* do idx */
    END. /* each ttaction */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRecipients Include 
PROCEDURE pRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    cRecipients = iphWidget:SCREEN-VALUE.
    RUN AOA/aoaRecipients.w (INPUT-OUTPUT cRecipients).
    iphWidget:SCREEN-VALUE = cRecipients.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsBrowser Include 
PROCEDURE pResultsBrowser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE NO-UNDO.
    
    DEFINE BUFFER b{1}SubjectColumn FOR {1}SubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    
    iphQuery:QUERY-OPEN.
    CREATE BROWSE hQueryBrowse
        ASSIGN
            FRAME = FRAME resultsFrame:HANDLE
            TITLE = "Subject Query Results"
            SENSITIVE = TRUE
            SEPARATORS = TRUE
            ROW-MARKERS = FALSE
            COLUMN-RESIZABLE = TRUE
            COLUMN-MOVABLE = TRUE 
            ALLOW-COLUMN-SEARCHING = TRUE
            QUERY = iphQuery
            COL = 1
            ROW = 1
            HEIGHT = FRAME resultsFrame:HEIGHT - .1
            WIDTH = FRAME resultsFrame:WIDTH - .32
            VISIBLE = TRUE
            NO-VALIDATE = TRUE
            .
    FOR EACH b{1}SubjectColumn
        WHERE b{1}SubjectColumn.subjectID EQ {1}Subject.subjectID
           BY b{1}SubjectColumn.sortOrder
        :
        hColumn = hQueryBrowse:ADD-LIKE-COLUMN(b{1}SubjectColumn.fieldName).
        /*
        IF b{1}SubjectColumn.sortOrder MOD 2 EQ 0 THEN
        hColumn:COLUMN-BGCOLOR = 11.
        */
        IF b{1}SubjectColumn.columnSize NE 0 THEN
        hColumn:WIDTH-CHARS = b{1}SubjectColumn.columnSize.
    END. /* each {1}SubjectColumn */
    ASSIGN
        btnCloseResults:HIDDEN = NO
        btnSaveResults:HIDDEN  = NO
        .
    btnCloseResults:MOVE-TO-TOP().
    btnSaveResults:MOVE-TO-TOP().
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsJasper Include 
PROCEDURE pResultsJasper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER ipcType  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    
    DEFINE BUFFER b{1}SubjectTable  FOR {1}SubjectTable.
    DEFINE BUFFER b{1}SubjectColumn FOR {1}SubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    iphQuery:QUERY-OPEN.
    iphQuery:GET-FIRST().
    IF NOT iphQuery:QUERY-OFF-END THEN DO:
        RUN pEmpty{1}Column IN hJasper.
        FOR EACH b{1}SubjectColumn
            WHERE b{1}SubjectColumn.subjectID EQ {1}Subject.subjectID
            :
            ASSIGN
                hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(b{1}SubjectColumn.tableName)
                cFieldName   = ENTRY(2,b{1}SubjectColumn.fieldName,".")
                .
            RUN pCreate{1}Column IN hJasper (
                cFieldName,
                b{1}SubjectColumn.sortOrder,
                YES,
                b{1}SubjectColumn.fieldLabel,
                b{1}SubjectColumn.dataType,
                b{1}SubjectColumn.fieldFormat,
                hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,
                MAX(hQueryBuf:BUFFER-FIELD(cFieldName):WIDTH,LENGTH(hQueryBuf:BUFFER-FIELD(cFieldName):LABEL))
                ).
        END. /* do iColumn */
        OS-CREATE-DIR "users".
        OS-CREATE-DIR VALUE("users\" + USERID("ASI")).
        OS-CREATE-DIR VALUE("users\" + USERID("ASI") + "\Jasper").
        cJasperFile = "users\" + USERID("ASI") + "\"
                    + REPLACE({1}Subject.subjectName," ","")
                    + ".json"
                    .
        OUTPUT TO VALUE(cJasperFile).
        PUT UNFORMATTED
            "~{" SKIP
            FILL(" ",2)
            "~"" REPLACE({1}Subject.subjectName," ","_") "~": ~{" SKIP
            FILL(" ",4)
            "~"{1}" REPLACE({1}Subject.subjectName," ","") "~": [" SKIP
            .
        REPEAT:
            PUT UNFORMATTED
                FILL(" ",6) "~{" SKIP
                .
            FOR EACH b{1}SubjectColumn
                WHERE b{1}SubjectColumn.subjectID EQ {1}Subject.subjectID
                   BY b{1}SubjectColumn.sortOrder
                :
                ASSIGN
                    hQueryBuf    = iphQuery:GET-BUFFER-HANDLE(b{1}SubjectColumn.tableName)
                    cFieldName   = ENTRY(2,b{1}SubjectColumn.fieldName,".")
                    cBufferValue = fFormatValue(hQueryBuf, hQueryBuf:BUFFER-FIELD(cFieldName):NAME)
                    /* remove special characters with escape values */
                    cBufferValue = REPLACE(cBufferValue,"~&","~&amp;")
                    cBufferValue = REPLACE(cBufferValue,"~'","~&apos;")
                    cBufferValue = REPLACE(cBufferValue,"~"","~&quot;")
                    cBufferValue = REPLACE(cBufferValue,"<","~&lt;")
                    cBufferValue = REPLACE(cBufferValue,">","~&gt;")
                    cBufferValue = REPLACE(cBufferValue,"~\","~\~\")
                    .
                IF b{1}SubjectColumn.sortOrder GT 1 THEN
                PUT UNFORMATTED "," SKIP.
                PUT UNFORMATTED
                    FILL(" ",8)
                    "~"" cFieldName "~": ~""
                    IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "~""
                    .
            END. /* do iColumn */
            PUT UNFORMATTED SKIP FILL(" ",6) "}".
            iphQuery:GET-NEXT().
            IF iphQuery:QUERY-OFF-END THEN LEAVE.
            PUT UNFORMATTED "," SKIP.
        END. /* repeat */
        iphQuery:QUERY-CLOSE().
        PUT UNFORMATTED
            SKIP
            FILL(" ",4) "]" SKIP
            FILL(" ",2) "}" SKIP
            "}" SKIP
            .
        OUTPUT CLOSE.
        RUN pJasperCopy IN hJasper (cJasperFile).    
        RUN spJasperQuery IN hJasper (
            ipcType,
            {1}Subject.subjectName,
            USERID("ASI"),
            OUTPUT cJasperFile
            ).
    END. /* if get-first is valid */
    iphQuery:QUERY-CLOSE().
    
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunSubject Include 
PROCEDURE pRunSubject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDate     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cError    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cQueryStr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cParam    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE hBuffer   AS HANDLE    NO-UNDO EXTENT 1000.
    DEFINE VARIABLE hQuery    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectColumn   FOR {1}SubjectColumn.
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    DEFINE BUFFER {1}SubjectTable    FOR {1}SubjectTable.    

    RUN pSetDefault.
    RUN pSaveParamValues.
        
    cQueryStr = queryStr.
    IF INDEX(queryStr,"[[") NE 0 THEN
    DO idx = 1 TO EXTENT(dynParamValue.paramName):
        IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
        cParam = "[[" + dynParamValue.paramName[idx] + "]]".
        IF INDEX(cQueryStr,cParam) NE 0 THEN
        CASE dynParamValue.paramDataType[idx]:
            WHEN "Character" THEN
            cQueryStr = REPLACE(cQueryStr,cParam,"~"" + dynParamValue.paramValue[idx] + "~"").
            WHEN "Date" THEN DO:
                dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.
                ASSIGN
                    cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])
                    cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                    .
            END. /* date */
            WHEN "DateTime" THEN DO:
                dtDate = DATE(dynParamValue.paramValue[idx]) NO-ERROR.
                ASSIGN
                    cDate     = IF dtDate EQ ? THEN "~?" ELSE STRING(dtDate,dynParamValue.paramFormat[idx])
                    cQueryStr = REPLACE(cQueryStr,cParam,cDate)
                    cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx])
                    .
            END. /* date */
            WHEN "Decimal" OR WHEN "Integer" OR WHEN "Logical" THEN
            cQueryStr = REPLACE(cQueryStr,cParam,dynParamValue.paramValue[idx]).
        END CASE.
    END. /* if [[ (parameter used) */

    FOR EACH {1}SubjectColumn
        WHERE {1}SubjectColumn.subjectID EQ {1}Subject.subjectID
          AND {1}SubjectColumn.sortCol   GT 0
           BY {1}SubjectColumn.sortCol
        :
        cQueryStr = cQueryStr + " BY " + {1}SubjectColumn.fieldName.
    END. /* each {1}SubjectColumn */

    idx = 0.
    CREATE QUERY hQuery.
    FOR EACH {1}SubjectTable
        WHERE {1}SubjectTable.subjectID EQ {1}Subject.subjectID
           BY {1}SubjectTable.sortOrder
        :
        idx = idx + 1.
        CREATE BUFFER hBuffer[idx] FOR TABLE {1}SubjectTable.tableName.
        hQuery:ADD-BUFFER(hBuffer[idx]).
    END. /* each {1}SubjectTable */
    lOK = hQuery:QUERY-PREPARE(cQueryStr) NO-ERROR.
    IF lOK THEN DO:
        IF iplRun THEN DO:
            IF ipcType EQ "Results" THEN
            RUN pResultsBrowser (hQuery).
            ELSE
            RUN pResultsJasper (hQuery, ipcType).
        END. /* if run */
        ELSE
        MESSAGE
            "Query Syntax is Correct."        
        VIEW-AS ALERT-BOX TITLE "Query Syntax Check".
    END. /* if ok */
    ELSE DO:
        DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
            cError = cError + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
        END. /* do idx */
        MESSAGE cError VIEW-AS ALERT-BOX ERROR TITLE "Query Syntax Check".
    END. /* else */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveParamValues Include 
PROCEDURE pSaveParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hWidget AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
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
            IF hWidget:TYPE NE "BUTTON" AND
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
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pValidate Include 
PROCEDURE pValidate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget AS HANDLE NO-UNDO.
    
    RUN pParamAction (iphWidget).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

