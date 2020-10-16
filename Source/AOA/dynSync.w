&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/dynSync.w

  Description: Dynamic Subject Sync

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 1.10.2020

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&Scoped-define program-id dynSync.
&Scoped-define defaultUser _default

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE VARIABLE cPrgmName AS CHARACTER NO-UNDO INITIAL "{&program-id}".

DEFINE TEMP-TABLE ttDynSync NO-UNDO
    FIELD subjectID        LIKE dynSubject.subjectID
    FIELD subjectTitle     LIKE dynSubject.subjectTitle
    FIELD paramDescription LIKE dynParamValue.paramDescription
    FIELD valueType        AS CHARACTER FORMAT "x(7)"  LABEL "Type"
    FIELD valueAction      AS CHARACTER FORMAT "x(7)"  LABEL "Action"
    FIELD defaultValue     AS CHARACTER FORMAT "x(30)" LABEL "Default Value"
    FIELD currentValue     AS CHARACTER FORMAT "x(30)" LABEL "Current Value"
    FIELD colName          AS CHARACTER FORMAT "x(20)" LABEL "Column"
    FIELD sync             AS LOGICAL                  LABEL ""
    FIELD user-id          LIKE dynParamValue.user-id
    FIELD prgmName         LIKE dynParamValue.prgmName
    FIELD paramValueID     LIKE dynParamValue.paramValueID
    FIELD sortOrder        AS INTEGER   FORMAT ">>9"   LABEL "Order"
    FIELD rRowID           AS ROWID
        INDEX ttDynSync IS PRIMARY
            subjectID
            paramValueID
            sortOrder
            valueAction DESCENDING
            colName
            .
DEFINE TEMP-TABLE ttDefaultValue NO-UNDO
    FIELD colName       AS CHARACTER
    FIELD colLabel      AS CHARACTER
    FIELD colFormat     AS CHARACTER
    FIELD isReturnValue AS CHARACTER
    FIELD isSearchable  AS CHARACTER
    FIELD isSortable    AS CHARACTER
    FIELD sortOrder     AS INTEGER
    FIELD found         AS LOGICAL
    .
{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME dynSync

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDynSync

/* Definitions for BROWSE dynSync                                       */
&Scoped-define FIELDS-IN-QUERY-dynSync ttDynSync.subjectID ttDynSync.subjectTitle ttDynSync.paramDescription ttDynSync.colName ttDynSync.sync ttDynSync.valueAction ttDynSync.valueType ttDynSync.defaultValue ttDynSync.currentValue ttDynSync.user-id ttDynSync.paramValueID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dynSync ttDynSync.sync   
&Scoped-define ENABLED-TABLES-IN-QUERY-dynSync ttDynSync
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-dynSync ttDynSync
&Scoped-define SELF-NAME dynSync
&Scoped-define QUERY-STRING-dynSync FOR EACH ttDynSync ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-dynSync OPEN QUERY {&SELF-NAME} FOR EACH ttDynSync ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-dynSync ttDynSync
&Scoped-define FIRST-TABLE-IN-QUERY-dynSync ttDynSync


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-dynSync}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS dynSync setSync 
&Scoped-Define DISPLAYED-OBJECTS setSync 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetSaveButton C-Win 
FUNCTION fSetSaveButton RETURNS LOGICAL
  ( iplSave AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save".

DEFINE VARIABLE setSync AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.6 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dynSync FOR 
      ttDynSync SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dynSync
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dynSync C-Win _FREEFORM
  QUERY dynSync DISPLAY
      ttDynSync.subjectID LABEL-BGCOLOR 14
ttDynSync.subjectTitle LABEL-BGCOLOR 14
ttDynSync.paramDescription LABEL-BGCOLOR 14
ttDynSync.colName LABEL-BGCOLOR 14
ttDynSync.sync LABEL-BGCOLOR 14 VIEW-AS TOGGLE-BOX
ttDynSync.valueAction LABEL-BGCOLOR 14
ttDynSync.valueType LABEL-BGCOLOR 14
ttDynSync.defaultValue LABEL-BGCOLOR 14
ttDynSync.currentValue LABEL-BGCOLOR 14
ttDynSync.user-id LABEL-BGCOLOR 14
ttDynSync.paramValueID
ttDynSync.sortOrder
ENABLE
ttDynSync.sync
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 28.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     dynSync AT ROW 1 COL 1 WIDGET-ID 200
     btnSave AT ROW 1.19 COL 95.8 HELP
          "Update/Save" WIDGET-ID 248
     setSync AT ROW 1.1 COL 105.2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Dynamic Subject Sync"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB dynSync 1 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnSave IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnSave:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       dynSync:NUM-LOCKED-COLUMNS IN FRAME DEFAULT-FRAME     = 2.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dynSync
/* Query rebuild information for BROWSE dynSync
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynSync ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dynSync */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Subject Sync */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Subject Sync */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Dynamic Subject Sync */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Save */
DO:
    fSetSaveButton (NO).
    ASSIGN
        BROWSE {&BROWSE-NAME}:MODIFIED = NO
        setSync:SCREEN-VALUE = "NO"
        setSync
        .
    RUN pSync.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dynSync
&Scoped-define SELF-NAME dynSync
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynSync C-Win
ON ROW-LEAVE OF dynSync IN FRAME DEFAULT-FRAME
DO:
    IF BROWSE {&SELF-NAME}:MODIFIED THEN
    fSetSaveButton (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dynSync C-Win
ON START-SEARCH OF dynSync IN FRAME DEFAULT-FRAME
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = SELF:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    APPLY "VALUE-CHANGED":U TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME setSync
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL setSync C-Win
ON VALUE-CHANGED OF setSync IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetSync ({&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
{methods/template/brwcustom.i}
/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetSettings.
  RUN pGetSync.
  RUN enable_UI.
  APPLY "VALUE-CHANGED":U TO dynSync.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pBySubjectID" "ttDynSync.subjectID"}
{methods/sortByProc.i "pBySubjectTitle" "ttDynSync.subjectTitle"}
{methods/sortByProc.i "pByParamDescription" "ttDynSync.paramDescription"}
{methods/sortByProc.i "pByValueAction" "ttDynSync.valueAction"}
{methods/sortByProc.i "pByValueType" "ttDynSync.valueType"}
{methods/sortByProc.i "pByDefaultValue" "ttDynSync.defaultValue"}
{methods/sortByProc.i "pByCurrentValue" "ttDynSync.currentValue"}
{methods/sortByProc.i "pByUserID" "ttDynSync.user-id"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY setSync 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE dynSync setSync 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTTDynSync C-Win 
PROCEDURE pCreateTTDynSync :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSubjectID        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcSubjectTitle     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcParamDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrgmName         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiParamValueID     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcValueType        AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValueAction      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDefaultValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCurrentValue     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcColName          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSortOrder        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID            AS ROWID     NO-UNDO.

    CREATE ttDynSync.
    ASSIGN
        ttDynSync.subjectID        = ipiSubjectID
        ttDynSync.subjectTitle     = ipcSubjectTitle
        ttDynSync.paramDescription = ipcParamDescription
        ttDynSync.user-id          = ipcUserID
        ttDynSync.prgmName         = ipcPrgmName
        ttDynSync.paramValueID     = ipiParamValueID
        ttDynSync.valueType        = ipcValueType
        ttDynSync.valueAction      = ipcValueAction
        ttDynSync.defaultValue     = ipcDefaultValue
        ttDynSync.currentValue     = ipcCurrentValue
        ttDynSync.colName          = ipcColName
        ttDynSync.sortOrder        = ipiSortOrder
        ttDynSync.rRowID           = iprRowID
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ cPrgmName
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowColumn" THEN
                {&WINDOW-NAME}:COLUMN = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowRow" THEN
                {&WINDOW-NAME}:ROW = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                ASSIGN
                    {&WINDOW-NAME}:WIDTH = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-WIDTH = {&WINDOW-NAME}:WIDTH
                    .
                WHEN "WindowHeight" THEN
                ASSIGN
                    {&WINDOW-NAME}:HEIGHT = DECIMAL(user-print.field-value[idx])
                    FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
                    .
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSync C-Win 
PROCEDURE pGetSync :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCurrentValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDefaultValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValueAction  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValueType    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.

    DEFINE BUFFER bDynParamValue FOR dynParamValue.

    EMPTY TEMP-TABLE ttDynSync.
    FOR EACH dynParamValue NO-LOCK
        WHERE dynParamValue.user-id EQ "{&defaultUser}",
        FIRST dynSubject OF dynParamValue NO-LOCK
        :
        IF CAN-FIND(FIRST bDynParamValue
                    WHERE bDynParamValue.subjectID EQ dynParamValue.subjectID
                      AND bDynParamValue.user-id   NE dynParamValue.user-id) THEN DO:
            EMPTY TEMP-TABLE ttDefaultValue.
            FOR EACH dynValueColumn NO-LOCK
                WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
                  AND dynValueColumn.user-id      EQ dynParamValue.user-id
                  AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
                  AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
                   BY dynValueColumn.sortOrder
                :
                CREATE ttDefaultValue.
                ASSIGN
                    ttDefaultValue.colName       = dynValueColumn.colName
                    ttDefaultValue.colLabel      = dynValueColumn.colLabel
                    ttDefaultValue.colFormat     = dynValueColumn.colFormat
                    ttDefaultValue.isReturnValue = STRING(dynValueColumn.isReturnValue)
                    ttDefaultValue.isSearchable  = STRING(dynValueColumn.isSearchable)
                    ttDefaultValue.isSortable    = STRING(dynValueColumn.isSortable)
                    ttDefaultValue.sortOrder     = dynValueColumn.sortOrder
                    .
            END. /* each dynValueColumn */
            FOR EACH bDynParamValue NO-LOCK
                WHERE bDynParamValue.subjectID EQ dynParamValue.subjectID
                  AND bDynParamValue.user-id   NE dynParamValue.user-id
                :
                FOR EACH dynValueColumn NO-LOCK
                    WHERE dynValueColumn.subjectID    EQ bDynParamValue.subjectID
                      AND dynValueColumn.user-id      EQ bDynParamValue.user-id
                      AND dynValueColumn.prgmName     EQ bDynParamValue.prgmName
                      AND dynValueColumn.paramValueID EQ bDynParamValue.paramValueID
                       BY dynValueColumn.sortOrder
                    :
                    ASSIGN
                        cValueType    = ""
                        cValueAction  = ""
                        cDefaultValue = ""
                        cCurrentValue = ""
                        .
                    FIND FIRST ttDefaultValue
                         WHERE ttDefaultValue.colName EQ dynValueColumn.colName
                         NO-ERROR.
                    IF AVAILABLE ttDefaultValue THEN
                    ttDefaultValue.found = YES.
                    DO idx = 1 TO 6:
                        CASE idx:
                            WHEN 1 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.colName.
                                ASSIGN
                                    cCurrentValue = dynValueColumn.colName
                                    cValueType    = ""
                                    .
                            END.
                            WHEN 2 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.colLabel.
                                ASSIGN
                                    cCurrentValue = dynValueColumn.colLabel
                                    cValueType    = "Label"
                                    .
                            END.
                            WHEN 3 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.colFormat.
                                ASSIGN
                                    cCurrentValue = dynValueColumn.colFormat
                                    cValueType    = "Format"
                                    .
                            END.
                            WHEN 4 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.isReturnValue.
                                ASSIGN
                                    cCurrentValue = STRING(dynValueColumn.isReturnValue)
                                    cValueType    = "ReturnValue"
                                    .
                            END.
                            WHEN 5 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.isSearchable.
                                ASSIGN
                                    cCurrentValue = STRING(dynValueColumn.isSearchable)
                                    cValueType    = "Searchable"
                                    .
                            END.
                            WHEN 6 THEN DO:
                                IF AVAILABLE ttDefaultValue THEN
                                cDefaultValue = ttDefaultValue.isSortable.
                                ASSIGN
                                    cCurrentValue = STRING(dynValueColumn.isSortable)
                                    cValueType    = "Sortable"
                                    .
                            END.
                        END CASE.
                        cValueAction = IF NOT AVAILABLE ttDefaultValue THEN "Delete"
                                  ELSE IF cCurrentValue EQ "" THEN "Add"
                                  ELSE IF cDefaultValue NE cCurrentValue THEN "Update"
                                  ELSE "".
                        IF cValueAction EQ "" OR
                          (cValueAction EQ "Delete" AND idx GT 1) THEN
                        NEXT.
                        RUN pCreateTTDynSync (
                            dynParamValue.subjectID,
                            dynSubject.subjectTitle,
                            bDynParamValue.paramDescription,
                            bDynParamValue.user-id,
                            bDynParamValue.prgmName,
                            bDynParamValue.paramValueID,
                            cValueType,
                            cValueAction,
                            cDefaultValue,
                            cCurrentValue,
                            dynValueColumn.colName,
                            dynValueColumn.sortOrder,
                            ROWID(dynValueColumn)
                            ).
                    END. /* do idx */
                END. /* each dynValueColumn */
                FOR EACH ttDefaultValue:
                    IF ttDefaultValue.found EQ NO THEN
                    RUN pCreateTTDynSync (
                        dynParamValue.subjectID,
                        dynSubject.subjectTitle,
                        bDynParamValue.paramDescription,
                        bDynParamValue.user-id,
                        bDynParamValue.prgmName,
                        bDynParamValue.paramValueID,
                        "",
                        "Add",
                        ttDefaultValue.colName,
                        "",
                        ttDefaultValue.colName,
                        ttDefaultValue.sortOrder,
                        ?
                        ).
                    ttDefaultValue.found = NO.
                END. /* each ttdefaultvalue */
            END. /* each bdynparamvalue */
        END. /* if can-find */
    END. /* each dynparamvalue */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse C-Win 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "subjectID" THEN
        RUN pBySubjectID.
        WHEN "subjectTitle" THEN
        RUN pBySubjectTitle.
        WHEN "paramDescription" THEN
        RUN pByParamDescription.
        WHEN "valueAction" THEN
        RUN pByValueAction.
        WHEN "valueType" THEN
        RUN pByValueType.
        WHEN "defaultValue" THEN
        RUN pByDefaultValue.
        WHEN "currentValue" THEN
        RUN pByCurrentValue.
        WHEN "user-id" THEN
        RUN pByUserID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings C-Win 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.program-id EQ cPrgmName
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = cPrgmName
            user-print.user-id    = USERID("ASI")
            .
    END. /* not avail */
    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "WindowColumn"
        user-print.field-label[idx] = "WindowColumn"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:COLUMN)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowRow"
        user-print.field-label[idx] = "WindowRow"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:ROW)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowWidth"
        user-print.field-label[idx] = "WindowWidth"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:WIDTH)
        idx = idx + 1
        user-print.field-name[idx]  = "WindowHeight"
        user-print.field-label[idx] = "WindowHeight"
        user-print.field-value[idx] = STRING({&WINDOW-NAME}:HEIGHT)
        .
    FIND CURRENT user-print NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetSync C-Win 
PROCEDURE pSetSync :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplSync AS LOGICAL NO-UNDO.

    DEFINE BUFFER bTTDynSync FOR ttDynSync.
    
    FOR EACH bTTDynSync:
        bTTDynSync.sync = iplSync.
    END. /* each bttdynsync */
    fSetSaveButton (iplSync).
    IF CAN-FIND(FIRST ttDynSync) THEN
    BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSync C-Win 
PROCEDURE pSync :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSortOrder AS INTEGER NO-UNDO.

    DEFINE BUFFER bDynParamValue  FOR dynParamValue.
    DEFINE BUFFER bDynValueColumn FOR dynValueColumn.

    FOR EACH ttDynSync
        WHERE ttDynSync.sync EQ YES
        :
        CASE ttDynSync.valueAction:
            WHEN "Add" THEN DO TRANSACTION:
                FIND LAST dynValueColumn NO-LOCK
                     WHERE dynValueColumn.subjectID    EQ ttDynSync.subjectID
                       AND dynValueColumn.user-id      EQ ttDynSync.user-id
                       AND dynValueColumn.prgmName     EQ ttDynSync.prgmName
                       AND dynValueColumn.paramValueID EQ ttDynSync.paramValueID
                     NO-ERROR.
                iSortOrder = IF AVAILABLE dynValueColumn THEN dynValueColumn.sortOrder
                             ELSE 0.
                FIND FIRST bDynValueColumn NO-LOCK
                     WHERE bDynValueColumn.subjectID    EQ ttDynSync.subjectID
                       AND bDynValueColumn.user-id      EQ "{&defaultUser}"
                       AND bDynValueColumn.prgmName     EQ ttDynSync.prgmName
                       AND bDynValueColumn.paramValueID EQ 0
                       AND bDynValueColumn.colName      EQ ttDynSync.colName
                       AND bDynValueColumn.sortOrder    EQ ttDynSync.sortOrder
                     NO-ERROR.
                IF NOT AVAILABLE bDynValueColumn THEN NEXT.
                CREATE dynValueColumn.
                BUFFER-COPY bDynValueColumn
                     EXCEPT user-id sortOrder isActive sortCol sortDescending
                         TO dynValueColumn
                     ASSIGN
                         dynValueColumn.user-id        = ttDynSync.user-id
                         dynValueColumn.sortOrder      = iSortOrder + 1
                         dynValueColumn.isActive       = NO
                         dynValueColumn.sortCol        = 0
                         dynValueColumn.sortDescending = NO
                         .
            END. /* add */
            WHEN "Delete" THEN DO TRANSACTION:
                FIND FIRST dynValueColumn EXCLUSIVE-LOCK
                     WHERE dynValueColumn.subjectID    EQ ttDynSync.subjectID
                       AND dynValueColumn.user-id      EQ ttDynSync.user-id
                       AND dynValueColumn.prgmName     EQ ttDynSync.prgmName
                       AND dynValueColumn.paramValueID EQ ttDynSync.paramValueID
                       AND dynValueColumn.colName      EQ ttDynSync.colName
                       AND dynValueColumn.sortOrder    EQ ttDynSync.sortOrder
                     NO-ERROR.
                IF NOT AVAILABLE dynValueColumn THEN NEXT.
                DELETE dynValueColumn.
            END. /* delete */
            WHEN "Update" THEN DO TRANSACTION:
                FIND FIRST dynValueColumn EXCLUSIVE-LOCK
                     WHERE dynValueColumn.subjectID    EQ ttDynSync.subjectID
                       AND dynValueColumn.user-id      EQ ttDynSync.user-id
                       AND dynValueColumn.prgmName     EQ ttDynSync.prgmName
                       AND dynValueColumn.paramValueID EQ ttDynSync.paramValueID
                       AND dynValueColumn.colName      EQ ttDynSync.colName
                       AND dynValueColumn.sortOrder    EQ ttDynSync.sortOrder
                     NO-ERROR.
                IF NOT AVAILABLE dynValueColumn THEN NEXT.
                CASE ttDynSync.valueType:
                    WHEN "Format" THEN
                    dynValueColumn.colFormat     = ttDynSync.defaultValue.
                    WHEN "Label" THEN
                    dynValueColumn.colLabel      = ttDynSync.defaultValue.
                    WHEN "ReturnValue" THEN
                    dynValueColumn.isReturnValue = ttDynSync.defaultValue EQ "YES".
                    WHEN "Searchable" THEN
                    dynValueColumn.isSearchable  = ttDynSync.defaultValue EQ "YES".
                    WHEN "Sortable" THEN
                    dynValueColumn.isSortable    = ttDynSync.defaultValue EQ "YES".
                END CASE.
            END. /* update */
        END CASE.
    END. /* each ttdynsync */
    RUN pGetSync.
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinResize C-Win 
PROCEDURE pWinResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            BROWSE {&BROWSE-NAME}:HEIGHT       = FRAME {&FRAME-NAME}:HEIGHT
            BROWSE {&BROWSE-NAME}:WIDTH        = FRAME {&FRAME-NAME}:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        setSync:MOVE-TO-TOP().
        btnSave:MOVE-TO-TOP().
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetSaveButton C-Win 
FUNCTION fSetSaveButton RETURNS LOGICAL
  ( iplSave AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            btnSave:HIDDEN    = NOT iplSave
            btnSave:SENSITIVE = iplSave
            .
    END. /* with frame */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

