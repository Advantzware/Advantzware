&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: setaudit.w

  Description: Set Audit Table Selections

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 07/29/98

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

DEFINE VARIABLE hFieldColumn AS HANDLE  NO-UNDO EXTENT 20.
DEFINE VARIABLE hTableColumn AS HANDLE  NO-UNDO EXTENT 20.
DEFINE VARIABLE lContinue    AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD auditTable   AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD description  AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD audit        AS LOGICAL   EXTENT 4
    FIELD auditDefault AS LOGICAL   EXTENT 4
        INDEX ttTable IS PRIMARY auditTable
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD auditTable   AS CHARACTER
    FIELD auditField   AS CHARACTER FORMAT "x(30)" LABEL "Field"
    FIELD description  AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD audit        AS LOGICAL                  LABEL "Audit"
    FIELD auditDefault AS LOGICAL
        INDEX ttField IS PRIMARY 
            auditTable
            auditField
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME dbFields

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttField ttTable

/* Definitions for BROWSE dbFields                                      */
&Scoped-define FIELDS-IN-QUERY-dbFields ttField.auditField ttField.description ttField.audit   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dbFields ttField.audit   
&Scoped-define ENABLED-TABLES-IN-QUERY-dbFields ttField
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-dbFields ttField
&Scoped-define SELF-NAME dbFields
&Scoped-define QUERY-STRING-dbFields FOR EACH ttField WHERE ttField.auditTable EQ ttTable.auditTable
&Scoped-define OPEN-QUERY-dbFields OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE ttField.auditTable EQ ttTable.auditTable.
&Scoped-define TABLES-IN-QUERY-dbFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-dbFields ttField


/* Definitions for BROWSE dbTables                                      */
&Scoped-define FIELDS-IN-QUERY-dbTables ttTable.auditTable ttTable.description ttTable.audit[1] ttTable.audit[2] ttTable.audit[3] ttTable.audit[4]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dbTables ttTable.audit[1] ttTable.audit[2] ttTable.audit[3] ttTable.audit[4]   
&Scoped-define ENABLED-TABLES-IN-QUERY-dbTables ttTable
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-dbTables ttTable
&Scoped-define SELF-NAME dbTables
&Scoped-define QUERY-STRING-dbTables FOR EACH ttTable WHERE ttTable.auditTable MATCHES "*" + svFilter + "*"
&Scoped-define OPEN-QUERY-dbTables OPEN QUERY {&SELF-NAME} FOR EACH ttTable WHERE ttTable.auditTable MATCHES "*" + svFilter + "*".
&Scoped-define TABLES-IN-QUERY-dbTables ttTable
&Scoped-define FIRST-TABLE-IN-QUERY-dbTables ttTable


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-dbFields}~
    ~{&OPEN-QUERY-dbTables}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnBeforeValueFilterClear svFilter ~
ResetFromDefault btnExit SaveAsDefault svToggleAuditCreate ~
svToggleAuditDelete svToggleAuditUpdate svStackTrace dbTables dbFields ~
btnOK toggleFields 
&Scoped-Define DISPLAYED-OBJECTS svFilter svToggleAuditCreate ~
svToggleAuditDelete svToggleAuditUpdate svStackTrace toggleFields 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnBeforeValueFilterClear 
     IMAGE-UP FILE "Graphics/16x16/navigate_cross.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Clear Filter Value".

DEFINE BUTTON btnExit DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     BGCOLOR 8 FONT 4.

DEFINE BUTTON btnOK AUTO-GO DEFAULT 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 FONT 4.

DEFINE BUTTON ResetFromDefault 
     LABEL "Reset From Defaults" 
     SIZE 22 BY 1.14.

DEFINE BUTTON SaveAsDefault 
     LABEL "Save As Defaults" 
     SIZE 19 BY 1.14 TOOLTIP "Save As Defaults".

DEFINE VARIABLE svFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 107 BY 2.38.

DEFINE VARIABLE svStackTrace AS LOGICAL INITIAL no 
     LABEL "Stack" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditCreate AS LOGICAL INITIAL no 
     LABEL "Create" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditDelete AS LOGICAL INITIAL no 
     LABEL "Delete" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditUpdate AS LOGICAL INITIAL no 
     LABEL "Update" 
     VIEW-AS TOGGLE-BOX
     SIZE 10 BY .81 NO-UNDO.

DEFINE VARIABLE toggleFields AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 TOOLTIP "Toggle ON/OFF" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY dbFields FOR 
      ttField SCROLLING.

DEFINE QUERY dbTables FOR 
      ttTable SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE dbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dbFields C-Win _FREEFORM
  QUERY dbFields DISPLAY
      ttField.auditField
ttField.description
ttField.audit VIEW-AS TOGGLE-BOX
ENABLE
ttField.audit
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 72 BY 30.48
         TITLE "Fields".

DEFINE BROWSE dbTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dbTables C-Win _FREEFORM
  QUERY dbTables DISPLAY
      ttTable.auditTable
ttTable.description
ttTable.audit[1] COLUMN-LABEL "Audit Create" VIEW-AS TOGGLE-BOX
ttTable.audit[2] COLUMN-LABEL "Audit Delete" VIEW-AS TOGGLE-BOX
ttTable.audit[3] COLUMN-LABEL "Audit Update" VIEW-AS TOGGLE-BOX
ttTable.audit[4] COLUMN-LABEL "Stack Trace"  VIEW-AS TOGGLE-BOX
ENABLE
ttTable.audit[1]
ttTable.audit[2]
ttTable.audit[3]
ttTable.audit[4]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 107 BY 30.48
         TITLE "Database Tables".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnBeforeValueFilterClear AT ROW 1.48 COL 67 HELP
          "Click to Clear Value Filter" WIDGET-ID 40
     svFilter AT ROW 1.48 COL 33 COLON-ALIGNED HELP
          "Enter Filter Value" WIDGET-ID 6
     ResetFromDefault AT ROW 1.95 COL 110 HELP
          "Reset From Defaults" WIDGET-ID 48
     btnExit AT ROW 1.48 COL 100 HELP
          "Use this function to CANCEL field selecition"
     SaveAsDefault AT ROW 1.95 COL 163 HELP
          "Save As Defaults" WIDGET-ID 46
     svToggleAuditCreate AT ROW 2.67 COL 35 HELP
          "Select to Toggle Audit Create" WIDGET-ID 4
     svToggleAuditDelete AT ROW 2.67 COL 46 HELP
          "Select to Toggle Audit Delete" WIDGET-ID 8
     svToggleAuditUpdate AT ROW 2.67 COL 57 HELP
          "Select to Toggle Audit Update" WIDGET-ID 10
     svStackTrace AT ROW 2.67 COL 68 HELP
          "Select to Toggle Audit Stack Trace" WIDGET-ID 42
     dbTables AT ROW 3.86 COL 2 WIDGET-ID 100
     dbFields AT ROW 3.86 COL 110 WIDGET-ID 200
     btnOK AT ROW 1.48 COL 92 HELP
          "Use this function to ACCEPT selected field"
     toggleFields AT ROW 3.91 COL 173 HELP
          "Toggle ON/OFF" WIDGET-ID 44
     " GREEN : Default Value is ON" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 2.67 COL 133 WIDGET-ID 52
          BGCOLOR 10 
     "Toggle Audit" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 2.67 COL 22 WIDGET-ID 12
     " RED : Default Value is OFF" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1.95 COL 133 WIDGET-ID 50
          BGCOLOR 12 FGCOLOR 15 
     RECT-11 AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 182 BY 33.33.


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
         TITLE              = "Audit Table Selections"
         HEIGHT             = 33.33
         WIDTH              = 182
         MAX-HEIGHT         = 33.33
         MAX-WIDTH          = 182
         VIRTUAL-HEIGHT     = 33.33
         VIRTUAL-WIDTH      = 182
         MAX-BUTTON         = no
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
/* BROWSE-TAB dbTables svStackTrace DEFAULT-FRAME */
/* BROWSE-TAB dbFields dbTables DEFAULT-FRAME */
/* SETTINGS FOR RECTANGLE RECT-11 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dbFields
/* Query rebuild information for BROWSE dbFields
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttField
WHERE ttField.auditTable EQ ttTable.auditTable.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dbFields */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE dbTables
/* Query rebuild information for BROWSE dbTables
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttTable
WHERE ttTable.auditTable MATCHES "*" + svFilter + "*".
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE dbTables */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Audit Table Selections */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Audit Table Selections */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnBeforeValueFilterClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBeforeValueFilterClear C-Win
ON CHOOSE OF btnBeforeValueFilterClear IN FRAME DEFAULT-FRAME
DO:
    svFilter:SCREEN-VALUE = "".
    APPLY "VALUE-CHANGED":U TO svFilter.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExit C-Win
ON CHOOSE OF btnExit IN FRAME DEFAULT-FRAME
DO:
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK C-Win
ON CHOOSE OF btnOK IN FRAME DEFAULT-FRAME
DO:
    APPLY "ROW-LEAVE":U TO BROWSE {&BROWSE-NAME}.
    RUN pSave.
    MESSAGE
        "Audit Selections Saved"
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dbFields
&Scoped-define SELF-NAME dbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbFields C-Win
ON ROW-DISPLAY OF dbFields IN FRAME DEFAULT-FRAME /* Fields */
DO:
    hFieldColumn[3]:BGCOLOR = IF ttField.auditDefault THEN 10 ELSE 12.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dbTables
&Scoped-define SELF-NAME dbTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables C-Win
ON ROW-DISPLAY OF dbTables IN FRAME DEFAULT-FRAME /* Database Tables */
DO:
    ASSIGN
        hTableColumn[3]:BGCOLOR = IF ttTable.auditDefault[1] THEN 10 ELSE 12
        hTableColumn[4]:BGCOLOR = IF ttTable.auditDefault[2] THEN 10 ELSE 12
        hTableColumn[5]:BGCOLOR = IF ttTable.auditDefault[3] THEN 10 ELSE 12
        hTableColumn[6]:BGCOLOR = IF ttTable.auditDefault[4] THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables C-Win
ON VALUE-CHANGED OF dbTables IN FRAME DEFAULT-FRAME /* Database Tables */
DO:
    {&OPEN-QUERY-dbFields}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ResetFromDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ResetFromDefault C-Win
ON CHOOSE OF ResetFromDefault IN FRAME DEFAULT-FRAME /* Reset From Defaults */
DO:
    RUN pResetFromDefault.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SaveAsDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaveAsDefault C-Win
ON CHOOSE OF SaveAsDefault IN FRAME DEFAULT-FRAME /* Save As Defaults */
DO:
    RUN pSaveAsDefault.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SaveAsDefault C-Win
ON RIGHT-MOUSE-CLICK OF SaveAsDefault IN FRAME DEFAULT-FRAME /* Save As Defaults */
DO:
    RUN pUpdateAuditTables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svFilter C-Win
ON VALUE-CHANGED OF svFilter IN FRAME DEFAULT-FRAME /* Filter */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svStackTrace
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svStackTrace C-Win
ON VALUE-CHANGED OF svStackTrace IN FRAME DEFAULT-FRAME /* Stack */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetAudit (4,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svToggleAuditCreate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svToggleAuditCreate C-Win
ON VALUE-CHANGED OF svToggleAuditCreate IN FRAME DEFAULT-FRAME /* Create */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetAudit (1,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svToggleAuditDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svToggleAuditDelete C-Win
ON VALUE-CHANGED OF svToggleAuditDelete IN FRAME DEFAULT-FRAME /* Delete */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetAudit (2,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svToggleAuditUpdate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svToggleAuditUpdate C-Win
ON VALUE-CHANGED OF svToggleAuditUpdate IN FRAME DEFAULT-FRAME /* Update */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetAudit (3,{&SELF-NAME}).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME toggleFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL toggleFields C-Win
ON VALUE-CHANGED OF toggleFields IN FRAME DEFAULT-FRAME
DO:
    ASSIGN {&SELF-NAME}.
    RUN pSetAuditField ({&SELF-NAME}).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dbFields
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","Audit", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
      RUN enable_UI.
      RUN pGetColumns.
      RUN pGetTables.
      toggleFields:MOVE-TO-TOP().
      SaveAsDefault:HIDDEN = NOT DYNAMIC-FUNCTION("sfIsUserAdmin").
  END. /* if lcontinue */
  {methods/nowait.i}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
  IF NOT lContinue THEN
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

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
  DISPLAY svFilter svToggleAuditCreate svToggleAuditDelete svToggleAuditUpdate 
          svStackTrace toggleFields 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnBeforeValueFilterClear svFilter ResetFromDefault btnExit 
         SaveAsDefault svToggleAuditCreate svToggleAuditDelete 
         svToggleAuditUpdate svStackTrace dbTables dbFields btnOK toggleFields 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetColumns C-Win 
PROCEDURE pGetColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DO idx = 1 TO BROWSE dbTables:NUM-COLUMNS:
        hTableColumn[idx] = BROWSE dbTables:GET-BROWSE-COLUMN(idx).
    END. /* do idx */
    DO idx = 1 TO BROWSE dbFields:NUM-COLUMNS:
        hFieldColumn[idx] = BROWSE dbFields:GET-BROWSE-COLUMN(idx).
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetTables C-Win 
PROCEDURE pGetTables :
/* -----------------------------------------------------------
  Purpose: Populate table-names selection list with table names
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
    FOR EACH AuditTbl NO-LOCK:
        CREATE ttTable.
        ASSIGN
            ttTable.auditTable      = AuditTbl.AuditTable
            ttTable.audit[1]        = AuditTbl.AuditCreate
            ttTable.audit[2]        = AuditTbl.AuditDelete
            ttTable.audit[3]        = AuditTbl.AuditUpdate
            ttTable.audit[4]        = AuditTbl.AuditStack
            ttTable.auditDefault[1] = AuditTbl.AuditCreateDefault
            ttTable.auditDefault[2] = AuditTbl.AuditDeleteDefault
            ttTable.auditDefault[3] = AuditTbl.AuditUpdateDefault
            ttTable.auditDefault[4] = AuditTbl.AuditStackDefault
            .
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name EQ ttTable.auditTable
             NO-ERROR.
        IF AVAILABLE ASI._file THEN DO:
            ttTable.description = ASI._file._Desc.
            FOR EACH AuditFld OF AuditTbl NO-LOCK:
                FIND FIRST ASI._field OF ASI._file NO-LOCK
                     WHERE ASI._field._field-name EQ AuditFld.AuditField
                     NO-ERROR.
                IF AVAILABLE ASI._field THEN DO:
                    CREATE ttField.
                    ASSIGN
                        ttField.auditTable   = ASI._file._file-name
                        ttField.auditField   = ASI._field._field-name
                        ttField.description  = IF ASI._field._label NE ? THEN ASI._field._label
                                               ELSE ASI._field._field-name
                        ttField.audit        = AuditFld.Audit
                        ttField.auditDefault = AuditFld.AuditDefault
                        .
                END. /* if avail */
                ELSE /* field no longer exists in schema, turn off audit */
                ASSIGN
                    ttField.audit        = NO
                    ttField.auditDefault = NO
                    .
            END. /* each auditfld */
        END. /* if avail */
        ELSE /* table no longer exists in schema, turn off audit */
        ASSIGN
            ttTable.audit        = NO
            ttTable.auditDefault = NO.
    END. /* each audittbl */
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetFromDefault C-Win 
PROCEDURE pResetFromDefault :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    FOR EACH ttTable:
        DO idx = 1 TO EXTENT(ttTable.audit):
            ttTable.audit[idx] = ttTable.auditDefault[idx].
        END. /* do idx */
    END. /* each tttable */
    FOR EACH ttField:
        ttField.audit = ttField.auditDefault.
    END. /* each tttable */
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave C-Win 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTables AS CHARACTER NO-UNDO EXTENT 3.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttTable FOR ttTable.
    DEFINE BUFFER bttField FOR ttField.
    
    FOR EACH bttTable:
        FIND FIRST AuditTbl EXCLUSIVE-LOCK
             WHERE AuditTbl.AuditTable EQ bttTable.auditTable
             NO-ERROR.
        IF NOT AVAILABLE AuditTbl THEN NEXT.
        ASSIGN
            AuditTbl.AuditCreate = bttTable.audit[1]
            AuditTbl.AuditDelete = bttTable.audit[2]
            AuditTbl.AuditUpdate = bttTable.audit[3]
            AuditTbl.AuditStack  = bttTable.audit[4]
            .
        FIND CURRENT AuditTbl NO-LOCK.
    END. /* each btttable */
    FOR EACH bttField:
        FIND FIRST AuditFld EXCLUSIVE-LOCK
             WHERE AuditFld.AuditTable EQ bttField.AuditTable
               AND AuditFld.AuditField EQ bttField.AuditField
               AND AuditFld.Audit      NE bttField.audit
             NO-ERROR.
        IF NOT AVAILABLE AuditFld THEN NEXT.
        AuditFld.Audit = bttField.audit.
        FIND CURRENT AuditFld NO-LOCK.
    END. /* each bttfield */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveAsDefault C-Win 
PROCEDURE pSaveAsDefault :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    FOR EACH ttTable,
        FIRST AuditTbl EXCLUSIVE-LOCK
        WHERE AuditTbl.AuditTable EQ ttTable.auditTable
        :
        DO idx = 1 TO EXTENT(ttTable.auditDefault):
            ttTable.auditDefault[idx] = ttTable.audit[idx].
            CASE idx:
                WHEN 1 THEN
                AuditTbl.AuditCreateDefault = ttTable.auditDefault[idx].
                WHEN 2 THEN
                AuditTbl.AuditDeleteDefault = ttTable.auditDefault[idx].
                WHEN 3 THEN
                AuditTbl.AuditUpdateDefault = ttTable.auditDefault[idx].
                WHEN 4 THEN
                AuditTbl.AuditStackDefault  = ttTable.auditDefault[idx].
            END CASE.
        END. /* do idx */
    END. /* each tttable */
    FOR EACH ttField,
        FIRST AuditFld EXCLUSIVE-LOCK
        WHERE AuditFld.AuditTable EQ ttField.auditTable
          AND AuditFld.AuditField EQ ttField.auditField
        :
        ASSIGN
            ttField.auditDefault  = ttField.audit
            AuditFld.AuditDefault = ttField.auditDefault
            .
    END. /* each tttable */
    MESSAGE 
        "Audit Table and Field Defaults Saved"
    VIEW-AS ALERT-BOX.
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetAudit C-Win 
PROCEDURE pSetAudit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiIdx   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplAudit AS LOGICAL NO-UNDO.
    
    FOR EACH ttTable
        WHERE ttTable.auditTable MATCHES "*" + svFilter + "*"
        :
        ttTable.audit[ipiIdx] = iplAudit.
    END. /* each tttable */
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetAuditField C-Win 
PROCEDURE pSetAuditField :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplAudit AS LOGICAL NO-UNDO.
    
    FOR EACH ttField
        WHERE ttField.auditTable EQ ttTable.auditTable
        :
        ttField.audit = iplAudit.
    END. /* each ttField */
    BROWSE dbFields:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateAuditTables C-Win 
PROCEDURE pUpdateAuditTables :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUpdate AS LOGICAL NO-UNDO.
    
    MESSAGE 
        "Auto Update AuditTbl and AuditFld Tables?"
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE lUpdate.
    IF lUpdate THEN DO:
        FOR EACH AuditTbl EXCLUSIVE-LOCK:
            IF CAN-FIND(FIRST ASI._file
                        WHERE ASI._file._file-name EQ AuditTbl.AuditTable) THEN
            NEXT.
            FOR EACH AuditFld OF AuditTbl EXCLUSIVE-LOCK:
                DELETE AuditFld.
            END. /* each auditfld */
            DELETE AuditTbl.
        END. /* each audittbl */

        FOR EACH AuditTbl NO-LOCK,
            FIRST ASI._file NO-LOCK
            WHERE ASI._file._file-name EQ AuditTbl.AuditTable
            :
            FOR EACH ASI._field OF ASI._file NO-LOCK:
                IF CAN-FIND(FIRST AuditFld OF AuditTbl
                            WHERE AuditFld.AuditField EQ ASI._field._field-name) THEN
                NEXT.
                CREATE AuditFld.
                ASSIGN
                    AuditFld.AuditTable = ASI._file._file-name
                    AuditFld.AuditField = ASI._field._field-name
                    .
            END. /* each _field */
            FOR EACH AuditFld OF AuditTbl EXCLUSIVE-LOCK:
                IF CAN-FIND(FIRST ASI._field OF ASI._file
                            WHERE ASI._field._field-name EQ AuditFld.AuditField) THEN
                NEXT.
                DELETE AuditFld.
            END. /* each auditfld */
        END. /* each audittbl */
        MESSAGE 
            "Update Complete"
        VIEW-AS ALERT-BOX.
    END. /* if update */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Set-Focus C-Win 
PROCEDURE Set-Focus :
/*------------------------------------------------------------------------------
  Purpose:     Set Focus
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/setfocus.i svFilter}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

