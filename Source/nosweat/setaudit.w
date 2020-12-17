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
{methods/template/brwcustomdef.i}

DEFINE VARIABLE hFieldColumn AS HANDLE  NO-UNDO EXTENT 20.
DEFINE VARIABLE hTableColumn AS HANDLE  NO-UNDO EXTENT 20.
DEFINE VARIABLE lContinue    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSave        AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSuperAdmin  AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttTable NO-UNDO
    FIELD auditTable        AS CHARACTER FORMAT "x(20)" LABEL "Table"
    FIELD description       AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD saveHolder        AS CHARACTER FORMAT "x(2)"  LABEL "Save"
    FIELD resetHolder       AS CHARACTER FORMAT "x(2)"  LABEL "Reset"
    FIELD expireDays        AS INTEGER   FORMAT ">>>9"  LABEL "Expire"
    FIELD expireDaysDefault AS INTEGER   FORMAT ">>>9"  LABEL "Default"
    FIELD audit             AS LOGICAL   EXTENT 4
    FIELD auditDefault      AS LOGICAL   EXTENT 4
        INDEX ttTable IS PRIMARY auditTable
        .
DEFINE TEMP-TABLE ttField NO-UNDO
    FIELD auditTable   AS CHARACTER
    FIELD auditField   AS CHARACTER FORMAT "x(30)" LABEL "Field"
    FIELD description  AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD saveHolder   AS CHARACTER FORMAT "x(2)"  LABEL "Save"
    FIELD resetHolder  AS CHARACTER FORMAT "x(2)"  LABEL "Reset"
    FIELD audit        AS LOGICAL                  LABEL "Update"
    FIELD auditDefault AS LOGICAL                  LABEL "Default"
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
&Scoped-define FIELDS-IN-QUERY-dbFields ttField.saveHolder ttField.resetHolder ttField.auditField ttField.description ttField.audit ttField.auditDefault   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dbFields ttField.audit   
&Scoped-define ENABLED-TABLES-IN-QUERY-dbFields ttField
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-dbFields ttField
&Scoped-define SELF-NAME dbFields
&Scoped-define QUERY-STRING-dbFields FOR EACH ttField WHERE ttField.auditTable EQ ttTable.auditTable
&Scoped-define OPEN-QUERY-dbFields OPEN QUERY {&SELF-NAME} FOR EACH ttField WHERE ttField.auditTable EQ ttTable.auditTable.
&Scoped-define TABLES-IN-QUERY-dbFields ttField
&Scoped-define FIRST-TABLE-IN-QUERY-dbFields ttField


/* Definitions for BROWSE dbTables                                      */
&Scoped-define FIELDS-IN-QUERY-dbTables ttTable.saveHolder ttTable.resetHolder ttTable.auditTable ttTable.description ttTable.expireDays ttTable.expireDaysDefault ttTable.audit[1] ttTable.auditDefault[1] ttTable.audit[2] ttTable.auditDefault[2] ttTable.audit[3] ttTable.auditDefault[3] ttTable.audit[4] ttTable.auditDefault[4]   
&Scoped-define ENABLED-FIELDS-IN-QUERY-dbTables ttTable.expireDays ttTable.audit[1] ttTable.audit[2] ttTable.audit[3] ttTable.audit[4]   
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
&Scoped-Define ENABLED-OBJECTS btnResetField btnResetTable btnSaveField ~
btnSaveTable svFilter ResetFromDefault dbTables dbFields ~
svToggleAuditCreate svToggleAuditDelete btnBeforeValueFilterClear ~
svToggleAuditUpdate svStackTrace toggleFields btnExit 
&Scoped-Define DISPLAYED-OBJECTS svFilter svToggleAuditCreate ~
svToggleAuditDelete svToggleAuditUpdate svStackTrace toggleFields 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetSaveButton C-Win
FUNCTION fSetSaveButton RETURNS LOGICAL 
  (iplSave AS LOGICAL) FORWARD.

/* _UIB-CODE-BLOCK-END */
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

DEFINE BUTTON btnResetField 
     IMAGE-UP FILE "Graphics/16x16/undo.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Reset From Default".

DEFINE BUTTON btnResetTable 
     IMAGE-UP FILE "Graphics/16x16/undo.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Reset From Default".

DEFINE BUTTON btnSaveAsDefault 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save As Defaults" 
     SIZE 8 BY 1.91 TOOLTIP "Save As Defaults".

DEFINE BUTTON btnSaveField 
     IMAGE-UP FILE "Graphics/16x16/floppy_disk.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Save To Default".

DEFINE BUTTON btnSaveTable 
     IMAGE-UP FILE "Graphics/16x16/floppy_disk.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1.05 TOOLTIP "Save To Default".

DEFINE BUTTON ResetFromDefault 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset ALL From Defaults" 
     SIZE 8 BY 1.91.

DEFINE VARIABLE svFilter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filter" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 236 BY 2.38.

DEFINE VARIABLE svStackTrace AS LOGICAL INITIAL no 
     LABEL "Stack" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditCreate AS LOGICAL INITIAL no 
     LABEL "Create" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditDelete AS LOGICAL INITIAL no 
     LABEL "Delete" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE svToggleAuditUpdate AS LOGICAL INITIAL no 
     LABEL "Update" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

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
      ttField.saveHolder
ttField.resetHolder
ttField.auditField
ttField.description
ttField.audit VIEW-AS TOGGLE-BOX
ttField.auditDefault VIEW-AS TOGGLE-BOX
ENABLE
ttField.audit
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 30.48
         TITLE "Fields".

DEFINE BROWSE dbTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS dbTables C-Win _FREEFORM
  QUERY dbTables DISPLAY
      ttTable.saveHolder
ttTable.resetHolder
ttTable.auditTable
ttTable.description
ttTable.expireDays
ttTable.expireDaysDefault
ttTable.audit[1] COLUMN-LABEL "Create" VIEW-AS TOGGLE-BOX
ttTable.auditDefault[1] COLUMN-LABEL "Default" VIEW-AS TOGGLE-BOX
ttTable.audit[2] COLUMN-LABEL "Delete" VIEW-AS TOGGLE-BOX
ttTable.auditDefault[2] COLUMN-LABEL "Default" VIEW-AS TOGGLE-BOX
ttTable.audit[3] COLUMN-LABEL "Update" VIEW-AS TOGGLE-BOX
ttTable.auditDefault[3] COLUMN-LABEL "Default" VIEW-AS TOGGLE-BOX
ttTable.audit[4] COLUMN-LABEL "Stack"  VIEW-AS TOGGLE-BOX
ttTable.auditDefault[4] COLUMN-LABEL "Default" VIEW-AS TOGGLE-BOX
ENABLE
ttTable.expireDays
ttTable.audit[1]
ttTable.audit[2]
ttTable.audit[3]
ttTable.audit[4]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 142 BY 30.48
         TITLE "Database Tables".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnOK AT ROW 1.48 COL 221 HELP
          "Use this function to ACCEPT selected field"
     btnSaveAsDefault AT ROW 1.48 COL 11 HELP
          "Save As Defaults" WIDGET-ID 46
     btnResetField AT ROW 1.48 COL 151 WIDGET-ID 62
     btnResetTable AT ROW 2.91 COL 8 WIDGET-ID 60
     btnSaveField AT ROW 1.48 COL 146 WIDGET-ID 64
     btnSaveTable AT ROW 2.91 COL 3 WIDGET-ID 58
     svFilter AT ROW 2.43 COL 32 COLON-ALIGNED HELP
          "Enter Filter Value" WIDGET-ID 6
     ResetFromDefault AT ROW 1.48 COL 3 HELP
          "Reset From Defaults" WIDGET-ID 48
     dbTables AT ROW 3.86 COL 2 WIDGET-ID 100
     dbFields AT ROW 3.86 COL 145 WIDGET-ID 200
     svToggleAuditCreate AT ROW 3.91 COL 83 HELP
          "Select to Toggle Audit Create" WIDGET-ID 4
     svToggleAuditDelete AT ROW 3.91 COL 98 HELP
          "Select to Toggle Audit Delete" WIDGET-ID 8
     btnBeforeValueFilterClear AT ROW 2.43 COL 66 HELP
          "Click to Clear Value Filter" WIDGET-ID 40
     svToggleAuditUpdate AT ROW 3.91 COL 113 HELP
          "Select to Toggle Audit Update" WIDGET-ID 10
     svStackTrace AT ROW 3.91 COL 128 HELP
          "Select to Toggle Audit Stack Trace" WIDGET-ID 42
     toggleFields AT ROW 3.91 COL 221.6 HELP
          "Toggle ON/OFF" WIDGET-ID 44
     btnExit AT ROW 1.48 COL 229 HELP
          "Use this function to CANCEL field selecition"
     " CYAN : Field Audit Overrides Exist" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 2.91 COL 175 WIDGET-ID 56
          BGCOLOR 11 
     " GREEN : Default Value is ON" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 2.91 COL 145 WIDGET-ID 52
          BGCOLOR 10 
     " RED : Default Value is OFF" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 2.91 COL 86 WIDGET-ID 50
          BGCOLOR 12 FGCOLOR 15 
     " YELLOW : Default Overridden" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 2.91 COL 114 WIDGET-ID 54
          BGCOLOR 14 
     RECT-11 AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 238 BY 33.33.


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
         WIDTH              = 238
         MAX-HEIGHT         = 33.33
         MAX-WIDTH          = 238
         VIRTUAL-HEIGHT     = 33.33
         VIRTUAL-WIDTH      = 238
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
/* BROWSE-TAB dbTables ResetFromDefault DEFAULT-FRAME */
/* BROWSE-TAB dbFields dbTables DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnOK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnOK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON btnSaveAsDefault IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       btnSaveAsDefault:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

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


&Scoped-define SELF-NAME btnResetField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetField C-Win
ON CHOOSE OF btnResetField IN FRAME DEFAULT-FRAME
DO:
    RUN pResetDefault2Field.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnResetTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnResetTable C-Win
ON CHOOSE OF btnResetTable IN FRAME DEFAULT-FRAME
DO:
    RUN pResetDefault2Table.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveAsDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveAsDefault C-Win
ON CHOOSE OF btnSaveAsDefault IN FRAME DEFAULT-FRAME /* Save As Defaults */
DO:
    RUN pSaveAsDefault.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveAsDefault C-Win
ON RIGHT-MOUSE-CLICK OF btnSaveAsDefault IN FRAME DEFAULT-FRAME /* Save As Defaults */
DO:
    RUN pUpdateAuditTables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveField C-Win
ON CHOOSE OF btnSaveField IN FRAME DEFAULT-FRAME
DO:
    RUN pSaveField2Default.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveTable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveTable C-Win
ON CHOOSE OF btnSaveTable IN FRAME DEFAULT-FRAME
DO:
    RUN pSaveTable2Default.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dbFields
&Scoped-define SELF-NAME dbFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbFields C-Win
ON ROW-DISPLAY OF dbFields IN FRAME DEFAULT-FRAME /* Fields */
DO:
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}    
    ASSIGN
        hFieldColumn[5]:BGCOLOR = IF ttField.audit NE ttField.auditDefault THEN 14 ELSE ?
        hFieldColumn[6]:BGCOLOR = IF ttField.auditDefault THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbFields C-Win
ON VALUE-CHANGED OF dbFields IN FRAME DEFAULT-FRAME /* Fields */
DO:
    ASSIGN
        btnSaveField:ROW  = SELF:ROW + .86 + SELF:FOCUSED-ROW * .81
        btnResetField:ROW = SELF:ROW + .86 + SELF:FOCUSED-ROW * .81
        .
    btnSaveField:MOVE-TO-TOP().
    btnResetField:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME dbTables
&Scoped-define SELF-NAME dbTables
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables C-Win
ON ROW-DISPLAY OF dbTables IN FRAME DEFAULT-FRAME /* Database Tables */
DO:
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}    
    ASSIGN
        hTableColumn[5]:BGCOLOR  = IF ttTable.expireDays NE ttTable.expireDaysDefault THEN 14 ELSE ?
        /* create */
        hTableColumn[7]:BGCOLOR  = IF ttTable.audit[1] NE ttTable.auditDefault[1] THEN 14 ELSE ?
        hTableColumn[8]:BGCOLOR  = IF ttTable.auditDefault[1] THEN 10 ELSE 12
        /* delete */
        hTableColumn[9]:BGCOLOR  = IF ttTable.audit[2] NE ttTable.auditDefault[2] THEN 14 ELSE ?
        hTableColumn[10]:BGCOLOR = IF ttTable.auditDefault[2] THEN 10 ELSE 12
        /* update */
        hTableColumn[11]:BGCOLOR = IF ttTable.audit[3] NE ttTable.auditDefault[3] THEN 14 ELSE ?
        hTableColumn[12]:BGCOLOR = IF ttTable.auditDefault[3] THEN 10 ELSE 12
        hTableColumn[11]:BGCOLOR = IF CAN-FIND(FIRST ttField
                                               WHERE ttField.AuditTable EQ ttTable.AuditTable
                                                 AND ttField.Audit NE ttField.AuditDefault) THEN 11
                                   ELSE IF ttTable.audit[3] NE ttTable.auditDefault[3] THEN 14
                                   ELSE ?
        /* stack trace */
        hTableColumn[13]:BGCOLOR = IF ttTable.audit[4] NE ttTable.auditDefault[4] THEN 14 ELSE ?
        hTableColumn[14]:BGCOLOR = IF ttTable.auditDefault[4] THEN 10 ELSE 12
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables C-Win
ON ROW-LEAVE OF dbTables IN FRAME DEFAULT-FRAME /* Database Tables */
DO:
    IF BROWSE {&SELF-NAME}:MODIFIED THEN
    fSetSaveButton (YES).
/*    fSetSaveButton (YES).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL dbTables C-Win
ON VALUE-CHANGED OF dbTables IN FRAME DEFAULT-FRAME /* Database Tables */
DO:
    {&OPEN-QUERY-dbFields}
    ASSIGN
        btnSaveTable:ROW  = SELF:ROW + .86 + SELF:FOCUSED-ROW * .81
        btnResetTable:ROW = SELF:ROW + .86 + SELF:FOCUSED-ROW * .81
        .
    btnSaveTable:MOVE-TO-TOP().
    btnResetTable:MOVE-TO-TOP().
    APPLY "VALUE-CHANGED":U TO BROWSE dbFields.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ResetFromDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ResetFromDefault C-Win
ON CHOOSE OF ResetFromDefault IN FRAME DEFAULT-FRAME /* Reset ALL From Defaults */
DO:
    RUN pResetFromDefault.  
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
    APPLY "ENTRY":U TO SELF.
    APPLY "END":U TO SELF.
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

ON "VALUE-CHANGED":U OF ttTable.Audit[3]
DO:
    RUN pSetFieldAudit (BROWSE dbTables ttTable.Audit[3]).
END.

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
RUN util/CheckModule.p ("ASI","Audit", YES, OUTPUT lContinue).
&ELSE
lContinue = YES.
&ENDIF
{methods/template/brwcustom.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  IF lContinue THEN DO:
      RUN enable_UI.
      RUN pGetColumns.
      RUN pGetTables.
      svToggleAuditCreate:MOVE-TO-TOP().
      svToggleAuditDelete:MOVE-TO-TOP().
      svToggleAuditUpdate:MOVE-TO-TOP().
      svStackTrace:MOVE-TO-TOP().
      toggleFields:MOVE-TO-TOP().
      ASSIGN
        lSuperAdmin              = DYNAMIC-FUNCTION("sfIsUserSuperAdmin")
/*        btnSaveAsDefault:HIDDEN  = NOT lSuperAdmin*/
        btnSaveTable:HIDDEN      = NOT lSuperAdmin
        btnSaveField:HIDDEN      = NOT lSuperAdmin
        BROWSE dbtables:MODIFIED = NO
        .
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
  ENABLE btnResetField btnResetTable btnSaveField btnSaveTable svFilter 
         ResetFromDefault dbTables dbFields svToggleAuditCreate 
         svToggleAuditDelete btnBeforeValueFilterClear svToggleAuditUpdate 
         svStackTrace toggleFields btnExit 
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

    SESSION:SET-WAIT-STATE("General").
    DO idx = 1 TO BROWSE dbTables:NUM-COLUMNS:
        hTableColumn[idx] = BROWSE dbTables:GET-BROWSE-COLUMN(idx).
    END. /* do idx */
    DO idx = 1 TO BROWSE dbFields:NUM-COLUMNS:
        hFieldColumn[idx] = BROWSE dbFields:GET-BROWSE-COLUMN(idx).
    END. /* do idx */
    SESSION:SET-WAIT-STATE("").

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
    SESSION:SET-WAIT-STATE("General").
    FOR EACH AuditTbl NO-LOCK:
        CREATE ttTable.
        ASSIGN
            ttTable.auditTable        = AuditTbl.AuditTable
            ttTable.audit[1]          = AuditTbl.AuditCreate
            ttTable.audit[2]          = AuditTbl.AuditDelete
            ttTable.audit[3]          = AuditTbl.AuditUpdate
            ttTable.audit[4]          = AuditTbl.AuditStack
            ttTable.auditDefault[1]   = AuditTbl.AuditCreateDefault
            ttTable.auditDefault[2]   = AuditTbl.AuditDeleteDefault
            ttTable.auditDefault[3]   = AuditTbl.AuditUpdateDefault
            ttTable.auditDefault[4]   = AuditTbl.AuditStackDefault
            ttTable.expireDays        = AuditTbl.expireDays
            ttTable.expireDaysDefault = AuditTbl.expireDaysDefault
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
    SESSION:SET-WAIT-STATE("").
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetDefault2Field C-Win 
PROCEDURE pResetDefault2Field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        ttField.Audit:SCREEN-VALUE IN BROWSE dbFields = STRING(ttField.AuditDefault)
        ttField.Audit
        .
    BROWSE dbFields:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResetDefault2Table C-Win 
PROCEDURE pResetDefault2Table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        ttTable.expireDays:SCREEN-VALUE IN BROWSE dbTables = STRING(ttTable.expireDaysDefault)
        ttTable.Audit[1]:SCREEN-VALUE = STRING(ttTable.AuditDefault[1])
        ttTable.Audit[2]:SCREEN-VALUE = STRING(ttTable.AuditDefault[2])
        ttTable.Audit[3]:SCREEN-VALUE = STRING(ttTable.AuditDefault[3])
        ttTable.Audit[4]:SCREEN-VALUE = STRING(ttTable.AuditDefault[4])
        ttTable.expireDays
        ttTable.Audit[1]
        ttTable.Audit[2]
        ttTable.Audit[3]
        ttTable.Audit[4]
        .
    BROWSE dbTables:REFRESH().

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

    SESSION:SET-WAIT-STATE("General").
    FOR EACH ttTable:
        ttTable.expireDays = ttTable.expireDaysDefault.
        DO idx = 1 TO EXTENT(ttTable.audit):
            ttTable.audit[idx] = ttTable.auditDefault[idx].
        END. /* do idx */
    END. /* each tttable */
    FOR EACH ttField:
        ttField.audit = ttField.auditDefault.
    END. /* each tttable */
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.
    SESSION:SET-WAIT-STATE("").

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
    
    SESSION:SET-WAIT-STATE("General").
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
            AuditTbl.expireDays  = bttTable.expireDays
            .
        FIND CURRENT AuditTbl NO-LOCK.
    END. /* each btttable */
    BROWSE dbTables:REFRESH().
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
    BROWSE dbFields:REFRESH().
    RELEASE AuditTbl.
    RELEASE AuditFld.
    fSetSaveButton (NO).
    BROWSE dbTables:MODIFIED = NO.
    SESSION:SET-WAIT-STATE("").

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

    SESSION:SET-WAIT-STATE("General").
    FOR EACH ttTable,
        FIRST AuditTbl EXCLUSIVE-LOCK
        WHERE AuditTbl.AuditTable EQ ttTable.auditTable
        :
        ASSIGN
            ttTable.expireDaysDefault  = ttTable.expireDays
            AuditTbl.expireDaysDefault = ttTable.expireDays
            .
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
    RELEASE AuditTbl.
    RELEASE AuditFld.
    {&OPEN-QUERY-dbTables}
    fSetSaveButton (NO).
    BROWSE dbTables:MODIFIED = NO.
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.
    SESSION:SET-WAIT-STATE("").
    MESSAGE 
        "Audit Table and Field Defaults Saved"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveField2Default C-Win 
PROCEDURE pSaveField2Default :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION:
        ASSIGN
            BROWSE dbFields
            ttField.Audit
            .
        FIND FIRST AuditFld EXCLUSIVE-LOCK
             WHERE AuditFld.AuditTable EQ ttField.AuditTable
               AND AuditFld.AuditField EQ ttField.AuditField
             NO-ERROR.
        IF AVAILABLE AuditFld THEN
        AuditFld.AuditDefault = ttField.Audit.
        ASSIGN
            ttField.AuditDefault:SCREEN-VALUE IN BROWSE dbFields = STRING(ttField.Audit)
            ttField.AuditDefault
            .
    END. /* do trans */
    RELEASE AuditFld.
    BROWSE dbFields:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveTable2Default C-Win 
PROCEDURE pSaveTable2Default :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO TRANSACTION:
        ASSIGN
            BROWSE dbTables
            ttTable.Audit[1]
            ttTable.Audit[2]
            ttTable.Audit[3]
            ttTable.Audit[4]
            ttTable.expireDays
            .
        FIND FIRST AuditTbl EXCLUSIVE-LOCK
             WHERE AuditTbl.AuditTable EQ ttTable.AuditTable
             NO-ERROR.
        IF AVAILABLE AuditTbl THEN
        ASSIGN
            AuditTbl.AuditCreateDefault = ttTable.Audit[1]
            AuditTbl.AuditDeleteDefault = ttTable.Audit[2]
            AuditTbl.AuditUpdateDefault = ttTable.Audit[3]
            AuditTbl.AuditStackDefault  = ttTable.Audit[4]
            AuditTbl.expireDaysDefault  = ttTable.expireDays
            .
        ASSIGN
            ttTable.expireDaysDefault:SCREEN-VALUE IN BROWSE dbTables = STRING(ttTable.expireDays)
            ttTable.AuditDefault[1]:SCREEN-VALUE = STRING(ttTable.Audit[1])
            ttTable.AuditDefault[2]:SCREEN-VALUE = STRING(ttTable.Audit[2])
            ttTable.AuditDefault[3]:SCREEN-VALUE = STRING(ttTable.Audit[3])
            ttTable.AuditDefault[4]:SCREEN-VALUE = STRING(ttTable.Audit[4])
            ttTable.AuditDefault[1]
            ttTable.AuditDefault[2]
            ttTable.AuditDefault[3]
            ttTable.AuditDefault[4]
            ttTable.expireDaysDefault
            .
    END. /* do trans */
    RELEASE AuditTbl.
    BROWSE dbTables:REFRESH().

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
    
    SESSION:SET-WAIT-STATE("General").
    FOR EACH ttTable
        WHERE ttTable.auditTable MATCHES "*" + svFilter + "*"
        :
        ttTable.audit[ipiIdx] = iplAudit.
        /* update */
        IF ipiIdx EQ 3 THEN
        FOR EACH ttField
            WHERE ttField.AuditTable EQ ttTable.AuditTable
            :
            ttField.audit = iplAudit.
        END. /* each tttable */
    END. /* each tttable */
    {&OPEN-QUERY-dbTables}
    APPLY "VALUE-CHANGED":U TO BROWSE dbTables.
    SESSION:SET-WAIT-STATE("").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFieldAudit C-Win 
PROCEDURE pSetFieldAudit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplAudit AS LOGICAL NO-UNDO.

    FOR EACH ttField
        WHERE ttField.AuditTable EQ ttTable.AuditTable
        :
        ttField.Audit = iplAudit.
    END. /* each ttfield */
    {&OPEN-QUERY-dbFields}

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
        RELEASE AuditTbl.
        RELEASE AuditFld.
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


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetSaveButton C-Win
FUNCTION fSetSaveButton RETURNS LOGICAL 
  (iplSave AS LOGICAL):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            lSave                      = iplSave
            btnOK:HIDDEN               = NOT lSave
            btnOK:SENSITIVE            = lSave
            .
        IF lSuperAdmin THEN
        ASSIGN
            btnSaveAsDefault:HIDDEN    = btnOK:HIDDEN
            btnSaveAsDefault:SENSITIVE = btnOK:SENSITIVE
            .
    END. /* with frame */
    RETURN lSave.

END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
