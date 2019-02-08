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

DEFINE VARIABLE hDynamic AS HANDLE NO-UNDO.

RUN AOA/spDynamic.p PERSISTENT SET hDynamic.
SESSION:ADD-SUPER-PROCEDURE (hDynamic).

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

RUN spSetCompany (g_company).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDynParameters Include 
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
    
    cPoolName = "ParameterPool" + STRING(TIME).
    DELETE WIDGET-POOL cPoolName NO-ERROR.
    CREATE WIDGET-POOL cPoolName PERSISTENT.
    
    RUN pGetDynParamValue.
    IF NOT AVAILABLE dynParamValue OR
       NOT AVAILABLE {1}Subject THEN RETURN.    
    
    &IF "{1}" EQ "dyn" &THEN
    ASSIGN
        FRAME paramFrame:HEIGHT = {1}Subject.subjectHeight
        FRAME paramFrame:WIDTH  = {1}Subject.subjectWidth
        .
    RUN pWinReSize.
    &ENDIF
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
    ASSIGN
        FRAME outputFrame:TITLE = {1}Subject.subjectName
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
    RUN pInitialize.
    RUN pInitDynParameters (iphFrame).
    iphFrame:HIDDEN = NO.

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecipients Include
PROCEDURE pGetRecipients:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRecipients AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO idx = 1 TO EXTENT(dynParamValue.paramName):
        IF dynParamValue.paramName[idx]  EQ "svRecipients" AND
           dynParamValue.paramValue[idx] NE "" THEN DO:
            opcRecipients = dynParamValue.paramValue[idx].
            LEAVE.
        END. /* if */
    END. /* do idx */
    IF opcRecipients NE "" THEN DO:
        MESSAGE
            "Recipients:" opcRecipients SKIP(1)
            "Email Results?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE lUseEmail AS LOGICAL.
        IF lUseEmail EQ NO THEN
        opcRecipients = "".
    END. /* if */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitDynParameters Include 
PROCEDURE pInitDynParameters :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphFrame AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hWidget AS HANDLE NO-UNDO.
    
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInitialize Include
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
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    DEFINE BUFFER b{1}SubjectColumn FOR {1}SubjectColumn.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    IF VALID-HANDLE(hQueryBrowse) THEN
    DELETE OBJECT hQueryBrowse.
    
    iphQuery:QUERY-OPEN.
    CREATE BROWSE hQueryBrowse
        ASSIGN
            FRAME = FRAME resultsFrame:HANDLE
            TITLE = {1}Subject.subjectName + " Results"
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
        TRIGGERS:
            ON ROW-DISPLAY
                PERSISTENT RUN pRowDisplay IN THIS-PROCEDURE (iphQuery:HANDLE).
        END TRIGGERS.
    DO idx = 1 TO EXTENT(dynParamValue.colName):
        IF dynParamValue.colName[idx] EQ "" THEN LEAVE.
        IF dynParamValue.isCalcField[idx] THEN DO:
            hColumn = hQueryBrowse:ADD-CALC-COLUMN(
                dynParamValue.dataType[idx],
                dynParamValue.colFormat[idx],
                "",
                dynParamValue.colLabel[idx]
                ).
        END. /* if calc field */
        ELSE
        hColumn = hQueryBrowse:ADD-LIKE-COLUMN(dynParamValue.colName[idx]).
/*        IF idx MOD 2 EQ 0 THEN      */
/*        hColumn:COLUMN-BGCOLOR = 11.*/
        IF VALID-HANDLE(hColumn) AND dynParamValue.columnSize[idx] NE 0 THEN
        hColumn:WIDTH-CHARS = dynParamValue.columnSize[idx].
    END. /* do idx */
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
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    
    RUN spJasperQuery IN hJasper (
        ipcType,
        ROWID(dynParamValue),
        {1}Subject.subjectName,
        ipcUserID,
        hAppSrvBin,
        OUTPUT cJasperFile
        ).
    
    RUN LockWindowUpdate (0,OUTPUT i).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRowDisplay Include
PROCEDURE pRowDisplay:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery AS HANDLE NO-UNDO.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunNow Include
PROCEDURE pRunNow:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTaskFormat AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cRecipients AS CHARACTER NO-UNDO.
    
    RUN pGetRecipients (OUTPUT cRecipients).
    DO TRANSACTION:
        CREATE Task.
        ASSIGN
            Task.subjectID    = dynParamValue.subjectID
            Task.user-id      = dynParamValue.user-id
            Task.prgmName     = dynParamValue.prgmName
            Task.paramValueID = dynParamValue.paramValueID
            Task.taskName     = "Run Now Task"
            Task.taskFormat   = ipcTaskFormat
            Task.runNow       = YES
            Task.recipients   = cRecipients
            .
        RELEASE Task.
    END. /* do trans */
    MESSAGE
        "Task ~"" + {1}Subject.subjectName + "~" has been submitted."
    VIEW-AS ALERT-BOX TITLE "Run Now".

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunQuery Include
PROCEDURE pRunQuery:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplRun    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cError     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lOK        AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER {1}SubjectTable FOR {1}SubjectTable.    

    FOR EACH {1}SubjectTable
        WHERE {1}SubjectTable.subjectID EQ {1}Subject.subjectID
           BY {1}SubjectTable.sortOrder
        :
        cTableName = cTableName + {1}SubjectTable.tableName + ",".
    END. /* each {1}SubjectTable */
    cTableName = TRIM(cTableName,",").
    RUN AOA/hQuery.p (
        ROWID(dynParamValue),
        queryStr,
        cTableName,
        OUTPUT hQuery,
        OUTPUT lOK,
        OUTPUT cError
        ).
    IF lOK THEN DO:
        IF iplRun THEN DO:
            CASE ipcType:
                WHEN "Results" THEN
                RUN pResultsBrowser (hQuery).
                WHEN "Print -d" OR WHEN "View" THEN
                RUN pResultsJasper (ipcType, ipcUserID).
                OTHERWISE
                IF dynParamValue.prgmName NE "" THEN
                RUN pRunNow (ipcType).
                ELSE
                RUN pResultsJasper (ipcType, ipcUserID).
            END CASE.
        END. /* if run */
        ELSE
        MESSAGE
            "Query Syntax is Correct."        
        VIEW-AS ALERT-BOX TITLE "Query Syntax Check".
    END. /* if ok */
    ELSE MESSAGE cError VIEW-AS ALERT-BOX ERROR TITLE "Query Syntax Check".

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
    DEFINE INPUT PARAMETER iplRun      AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcType     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.

    RUN pSetDynParamValue (ipcUserID, ipcPrgmName).
    IF iplRun THEN
    RUN pSaveDynParamValues.
    RUN pRunQuery (iplRun, ipcType, ipcUserID).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveDynParamValues Include 
PROCEDURE pSaveDynParamValues :
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDynParamValue Include
PROCEDURE pSetDynParamValue:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER {1}SubjectParamSet FOR {1}SubjectParamSet.
    DEFINE BUFFER {1}SubjectColumn   FOR {1}SubjectColumn.
    
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ {1}Subject.subjectID
           AND dynParamValue.user-id      EQ ipcUserID
           AND dynParamValue.prgmName     EQ ipcPrgmName
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN DO TRANSACTION:
        CREATE dynParamValue.
        ASSIGN
            dynParamValue.subjectID        = {1}Subject.subjectID
            dynParamValue.user-id          = ipcUserID
            dynParamValue.prgmName         = ipcPrgmName
            dynParamValue.paramDescription = IF ipcUserID EQ "{&defaultUser}" THEN "System Default"
                                             ELSE "User Default"
            .
        FOR EACH {1}SubjectParamSet
            WHERE {1}SubjectParamSet.subjectID EQ {1}Subject.subjectID,
            EACH dynParamSetDtl NO-LOCK
            WHERE dynParamSetDtl.paramSetID EQ {1}SubjectParamSet.paramSetID,
            FIRST dynParam NO-LOCK
            WHERE dynParam.paramID EQ dynParamSetDtl.paramID
            :
            ASSIGN
                idx                              = idx + 1
                dynParamValue.paramName[idx]     = dynParamSetDtl.paramName
                dynParamValue.paramLabel[idx]    = dynParamSetDtl.paramLabel
                dynParamValue.paramValue[idx]    = dynParamSetDtl.initialValue
                dynParamValue.paramDataType[idx] = dynParam.dataType
                dynParamValue.paramFormat[idx]   = dynParam.paramFormat
                .
        END. /* each dynsubjectparamset */
        idx = 0.
        FOR EACH {1}SubjectColumn NO-LOCK
            WHERE {1}SubjectColumn.subjectID EQ {1}Subject.subjectID
               BY {1}SubjectColumn.sortOrder
            :
            ASSIGN
                idx = idx + 1
                dynParamValue.colName[idx]     = {1}SubjectColumn.fieldName
                dynParamValue.colLabel[idx]    = {1}SubjectColumn.fieldLabel
                dynParamValue.colFormat[idx]   = {1}SubjectColumn.fieldFormat
                dynParamValue.columnSize[idx]  = {1}SubjectColumn.columnSize
                dynParamValue.dataType[idx]    = {1}SubjectColumn.dataType
                dynParamValue.sortCol[idx]     = {1}SubjectColumn.sortCol
                dynParamValue.isGroup[idx]     = {1}SubjectColumn.isGroup
                dynParamValue.groupLabel[idx]  = {1}SubjectColumn.groupLabel
                dynParamValue.groupCalc[idx]   = {1}SubjectColumn.groupCalc
                dynParamValue.isCalcField[idx] = {1}SubjectColumn.isCalcField
                dynParamValue.calcProc[idx]    = {1}SubjectColumn.calcProc
                dynParamValue.calcParam[idx]   = {1}SubjectColumn.calcParam
                .
        END. /* each {1}SubjectColumn */
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* not avail */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spGetCompanyList Include
PROCEDURE spGetCompanyList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompanyList AS CHARACTER NO-UNDO.
    
    FOR EACH company NO-LOCK:
        cCompanyList = cCompanyList + company.company + ",".
    END. /* each company */
    cCompanyList = TRIM(cCompanyList).
    
    RETURN cCompanyList.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
