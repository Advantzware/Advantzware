&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS s-object 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: userTasks.w

  Description: from SMART.W - Template for basic SmartObject

  Author: Ron Stark
  Created: 1.14.2019

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

&Scoped-define prgmName userTasks.
&Scoped-define defaultUser _default

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExternalForm AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMnemonic     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModule       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOutputFormat AS CHARACTER NO-UNDO INITIAL
    "Grid,CSV,XLS,DOCX,PDF,HTML,View".
DEFINE VARIABLE cOutputImage  AS CHARACTER NO-UNDO INITIAL
    "table.ico,CSV.jpg,XLS.jpg,DOCX.jpg,PDF.jpg,html_tag.ico,table.ico".
DEFINE VARIABLE cPrgmName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamDescrip AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID       AS CHARACTER NO-UNDO.
DEFINE VARIABLE iRecordLimit  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOK           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lSortMove     AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE pHandle       AS HANDLE    NO-UNDO.

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttDynParamValue NO-UNDO
    FIELD subjectID        LIKE dynParamValue.subjectID
    FIELD mnemonic         LIKE prgrms.mnemonic LABEL "Hotkey"
    FIELD paramDescription LIKE dynParamValue.paramDescription LABEL "Description"
    FIELD paramTitle       LIKE dynParamValue.paramTitle
    FIELD module           LIKE dynParamValue.module
    FIELD user-id          LIKE dynParamValue.user-id
    FIELD paramValueID     LIKE dynParamValue.paramValueID
    FIELD prgmName         LIKE dynParamValue.prgmName
    FIELD outputFormat     LIKE dynParamValue.outputFormat
    FIELD securityLevel    LIKE dynParamValue.securityLevel
    FIELD externalForm     LIKE dynParamValue.externalForm
    FIELD recordLimit      LIKE dynParamValue.recordLimit
    FIELD paramValueRowID    AS ROWID
    FIELD allData            AS CHARACTER
        INDEX mnemonic IS PRIMARY mnemonic paramTitle paramValueID user-id
        INDEX paramValueRowID paramValueRowID
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseParamValue

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDynParamValue

/* Definitions for BROWSE browseParamValue                              */
&Scoped-define FIELDS-IN-QUERY-browseParamValue ttDynParamValue.paramTitle ttDynParamValue.paramDescription ttDynParamValue.module ttDynParamValue.user-id ttDynParamValue.paramValueID ttDynParamValue.outputFormat ttDynParamValue.prgmName ttDynParamValue.securityLevel ttDynParamValue.mnemonic ttDynParamValue.externalForm   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseParamValue   
&Scoped-define SELF-NAME browseParamValue
&Scoped-define QUERY-STRING-browseParamValue FOR EACH ttDynParamValue WHERE ttDynParamValue.prgmName BEGINS cPrgmName   AND ttDynParamValue.paramDescription BEGINS cParamDescrip   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-browseParamValue OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue WHERE ttDynParamValue.prgmName BEGINS cPrgmName   AND ttDynParamValue.paramDescription BEGINS cParamDescrip   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-browseParamValue ttDynParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseParamValue ttDynParamValue


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseParamValue}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS searchBar btnSubjctAttr browseParamValue ~
btnOutputFormat btnScheduleTask btnRunTask btnCopyTask btnDeleteTask ~
btnRestoreDefaults btnSortMove 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* taskObjects,List-2,List-3,List-4,List-5,List-6                       */
&Scoped-define taskObjects btnSubjctAttr btnOutputFormat btnScheduleTask ~
btnRunTask btnCopyTask btnDeleteTask 
&Scoped-define List-3 browseParamValue 
&Scoped-define List-4 browseParamValue 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCopyTask 
     IMAGE-UP FILE "Graphics/32x32/element_copy.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Copy Task" 
     SIZE 8 BY 1.91 TOOLTIP "Copy Task".

DEFINE BUTTON btnDeleteTask 
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete Task" 
     SIZE 8 BY 1.91 TOOLTIP "Delete Task".

DEFINE BUTTON btnOutputFormat 
     IMAGE-UP FILE "Graphics/32x32/table.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     CONTEXT-HELP-ID 0
     SIZE 8 BY 1.91 TOOLTIP "Run Now".

DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnRunTask 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Run Task" 
     SIZE 8 BY 1.91 TOOLTIP "Run Task".

DEFINE BUTTON btnScheduleTask 
     IMAGE-UP FILE "Graphics/32x32/calendar_clock.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Schedule Task" 
     SIZE 8 BY 1.91 TOOLTIP "Schedule Task".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE BUTTON btnSubjctAttr 
     IMAGE-UP FILE "Graphics/32x32/window_dialog.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Subject Attributes" 
     SIZE 8 BY 1.91 TOOLTIP "Set External Form".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 105 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 10 BY 11.91.

DEFINE VARIABLE filterModule AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Module" NO-UNDO.

DEFINE VARIABLE filterPrgmName AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Module" NO-UNDO.

DEFINE VARIABLE filterDescrip AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Report" NO-UNDO.

DEFINE VARIABLE filterUser AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select User" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseParamValue FOR 
      ttDynParamValue SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseParamValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseParamValue s-object _FREEFORM
  QUERY browseParamValue DISPLAY
      ttDynParamValue.paramTitle LABEL-BGCOLOR 14
ttDynParamValue.paramDescription LABEL-BGCOLOR 14
ttDynParamValue.module LABEL-BGCOLOR 14
ttDynParamValue.user-id LABEL-BGCOLOR 14
ttDynParamValue.paramValueID LABEL-BGCOLOR 14
ttDynParamValue.outputFormat
ttDynParamValue.prgmName LABEL-BGCOLOR 14
ttDynParamValue.securityLevel
ttDynParamValue.mnemonic LABEL-BGCOLOR 14
ttDynParamValue.externalForm
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 25.95
         TITLE "Tasks".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     searchBar AT ROW 1 COL 52 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     btnSubjctAttr AT ROW 21.48 COL 27 HELP
          "Set Subject Attributes" WIDGET-ID 286
     browseParamValue AT ROW 1.95 COL 37 WIDGET-ID 500
     btnOutputFormat AT ROW 19.57 COL 27 HELP
          "Run Now" WIDGET-ID 256
     btnScheduleTask AT ROW 23.38 COL 27 HELP
          "Schedule Task" WIDGET-ID 252
     btnRunTask AT ROW 13.86 COL 27 HELP
          "Run Task" WIDGET-ID 250
     btnCopyTask AT ROW 15.76 COL 27 HELP
          "Copy Task" WIDGET-ID 258
     btnDeleteTask AT ROW 17.67 COL 27 HELP
          "Delete Task" WIDGET-ID 260
     btnRestoreDefaults AT ROW 1 COL 37 HELP
          "Restore Defaults" WIDGET-ID 42
     btnSortMove AT ROW 1 COL 41 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 48
     RECT-1 AT ROW 13.62 COL 26 WIDGET-ID 254
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME filterFrame
     filterDescrip AT ROW 1.95 COL 2 HELP
          "Select Report" NO-LABEL WIDGET-ID 50
     filterModule AT ROW 4.1 COL 2 HELP
          "Select Module" NO-LABEL WIDGET-ID 54
     filterUser AT ROW 6.24 COL 2 HELP
          "Select User" NO-LABEL WIDGET-ID 58
     filterPrgmName AT ROW 8.38 COL 2 HELP
          "Select Module" NO-LABEL WIDGET-ID 64
     "Program:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 7.67 COL 2 WIDGET-ID 62
     "Module:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.38 COL 2 WIDGET-ID 56
     "User:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 5.52 COL 2 WIDGET-ID 60
     "Description:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 52
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.95
         SIZE 34 BY 9.76
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "Filters" WIDGET-ID 600.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartObject
   Allow: Basic
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW s-object ASSIGN
         HEIGHT             = 26.95
         WIDTH              = 158.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB s-object 
/* ************************* Included-Libraries *********************** */

{src/adm/method/smart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW s-object
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME filterFrame:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB browseParamValue btnSubjctAttr F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 26.95
       FRAME F-Main:WIDTH            = 158.

/* SETTINGS FOR BROWSE browseParamValue IN FRAME F-Main
   3 4                                                                  */
ASSIGN 
       browseParamValue:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1
       browseParamValue:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       browseParamValue:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR BUTTON btnCopyTask IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnDeleteTask IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnOutputFormat IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnRunTask IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnScheduleTask IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR BUTTON btnSubjctAttr IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME filterFrame
                                                                        */
/* SETTINGS FOR COMBO-BOX filterModule IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterPrgmName IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterDescrip IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterUser IN FRAME filterFrame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseParamValue
/* Query rebuild information for BROWSE browseParamValue
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue
WHERE ttDynParamValue.prgmName BEGINS cPrgmName
  AND ttDynParamValue.paramDescription BEGINS cParamDescrip
  AND ttDynParamValue.module BEGINS cModule
  AND ttDynParamValue.user-id BEGINS cUserID
  AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseParamValue */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME filterFrame
/* Query rebuild information for FRAME filterFrame
     _Query            is NOT OPENED
*/  /* FRAME filterFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME browseParamValue
&Scoped-define SELF-NAME browseParamValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseParamValue s-object
ON DEFAULT-ACTION OF browseParamValue IN FRAME F-Main /* Tasks */
DO:
    APPLY "CHOOSE":U TO btnRunTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseParamValue s-object
ON DELETE-CHARACTER OF browseParamValue IN FRAME F-Main /* Tasks */
DO:
    IF NOT btnDeleteTask:HIDDEN THEN
    APPLY "CHOOSE":U TO btnDeleteTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseParamValue s-object
ON START-SEARCH OF browseParamValue IN FRAME F-Main /* Tasks */
DO:
    IF {&BROWSE-NAME}:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseParamValue s-object
ON VALUE-CHANGED OF browseParamValue IN FRAME F-Main /* Tasks */
DO:
    IF AVAILABLE ttDynParamValue THEN DO:
        btnOutputFormat:LOAD-IMAGE("Graphics/32x32/" +
            ENTRY(LOOKUP(ttDynParamValue.outputFormat,cOutputFormat),cOutputImage)).
        ASSIGN
            btnOutputFormat:HIDDEN = ttDynParamValue.user-id EQ "{&defaultUser}"
            btnDeleteTask:HIDDEN   = btnOutputFormat:HIDDEN
            btnSubjctAttr:HIDDEN   = btnOutputFormat:HIDDEN
            btnScheduleTask:HIDDEN = ttDynParamValue.paramValueID EQ 0
            .
    END. /* if avail */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCopyTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCopyTask s-object
ON CHOOSE OF btnCopyTask IN FRAME F-Main /* Copy Task */
DO:
    IF AVAILABLE ttDynParamValue THEN
    RUN pCopyTask.
    APPLY "VALUE-CHANGED":U TO browseParamValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDeleteTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDeleteTask s-object
ON CHOOSE OF btnDeleteTask IN FRAME F-Main /* Delete Task */
DO:
    IF AVAILABLE ttDynParamValue THEN
    RUN pDeleteTask.
    APPLY "VALUE-CHANGED":U TO browseParamValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOutputFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOutputFormat s-object
ON CHOOSE OF btnOutputFormat IN FRAME F-Main
DO:
    IF AVAILABLE ttDynParamValue THEN
    RUN pRunTask (NO).
    APPLY "VALUE-CHANGED":U TO browseParamValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults s-object
ON CHOOSE OF btnRestoreDefaults IN FRAME F-Main /* Defaults */
DO:
    RUN pGetSettings ("{&defaultUser}").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunTask s-object
ON CHOOSE OF btnRunTask IN FRAME F-Main /* Run Task */
DO:
    IF AVAILABLE ttDynParamValue THEN
    RUN pRunTask (YES).
    APPLY "VALUE-CHANGED":U TO browseParamValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScheduleTask
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScheduleTask s-object
ON CHOOSE OF btnScheduleTask IN FRAME F-Main /* Schedule Task */
DO:
    IF AVAILABLE ttDynParamValue THEN
    RUN pScheduleTask.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSortMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMove s-object
ON CHOOSE OF btnSortMove IN FRAME F-Main /* Sort/Move */
DO:
    ASSIGN
        BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = lSortMove
        lSortMove = NOT lSortMove
        .
    SELF:LOAD-IMAGE("Graphics/16x16/"
        + IF lSortMove THEN "sort_up_down2.gif"
          ELSE "left_right_arrows.gif")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSubjctAttr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSubjctAttr s-object
ON CHOOSE OF btnSubjctAttr IN FRAME F-Main /* Subject Attributes */
DO:
    ASSIGN
        cExternalForm = ttDynParamValue.externalForm
        iRecordLimit  = ttDynParamValue.recordLimit
        .
    RUN AOA/dynSubjctAttr.w (
        INPUT-OUTPUT cExternalForm,
        INPUT-OUTPUT iRecordLimit,
        OUTPUT lOK
        ).
    IF lOK THEN DO TRANSACTION:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
        ASSIGN
            dynParamValue.externalForm   = cExternalForm
            dynParamValue.recordLimit    = iRecordLimit
            ttDynParamValue.externalForm = cExternalForm
            ttDynParamValue.recordLimit  = iRecordLimit
            .
        RELEASE dynParamValue.
        browseParamValue:REFRESH().
    END. /* if ok */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME filterFrame
&Scoped-define SELF-NAME filterModule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterModule s-object
ON VALUE-CHANGED OF filterModule IN FRAME filterFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cModule = {&SELF-NAME}
        .
    IF cModule EQ "<All>" THEN
    cModule = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterPrgmName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterPrgmName s-object
ON VALUE-CHANGED OF filterPrgmName IN FRAME filterFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cPrgmName = {&SELF-NAME}
        .
    IF cPrgmName EQ "<All>" THEN
    cPrgmName = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterDescrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterDescrip s-object
ON VALUE-CHANGED OF filterDescrip IN FRAME filterFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cParamDescrip = {&SELF-NAME}
        .
    IF cParamDescrip EQ "<All>" THEN
    cParamDescrip = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterUser s-object
ON VALUE-CHANGED OF filterUser IN FRAME filterFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cUserID = {&SELF-NAME}
        .
    IF cUserID EQ "<All>" THEN
    cUserID = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar s-object
ON VALUE-CHANGED OF searchBar IN FRAME F-Main /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{AOA/includes/pRunNow.i "tt"}

{methods/sortByProc.i "pBymnemonic" "ttDynParamValue.mnemonic"}
{methods/sortByProc.i "pByParamTitle" "ttDynParamValue.paramTitle"}
{methods/sortByProc.i "pByPrgmName" "ttDynParamValue.prgmName"}
{methods/sortByProc.i "pByProgTitle" "ttDynParamValue.paramDescription"}
{methods/sortByProc.i "pByParamDescription" "ttDynParamValue.ParamDescription"}
{methods/sortByProc.i "pByUserID" "ttDynParamValue.user-id"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI s-object  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  HIDE FRAME filterFrame.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize s-object 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME filterFrame:
    ASSIGN
        filterDescrip:SENSITIVE     = YES
        filterModule:SENSITIVE      = YES
        filterUser:SENSITIVE        = YES
        filterPrgmName:SENSITIVE    = YES
        filterDescrip:SCREEN-VALUE  = "<All>"
        filterModule:SCREEN-VALUE   = "<All>"
        filterUser:SCREEN-VALUE     = "<All>"
        filterPrgmName:SCREEN-VALUE = "<All>"
        .
  END. /* with frame */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  RUN pGetParamValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCopyTask s-object 
PROCEDURE pCopyTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDescription  LIKE dynParamValue.paramDescription NO-UNDO.
    DEFINE VARIABLE iParamValueID AS INTEGER NO-UNDO INITIAL 1.
    DEFINE VARIABLE rRowID        AS ROWID   NO-UNDO.

    DEFINE BUFFER bDynParamValue FOR dynParamValue.

    DO TRANSACTION:
        FIND LAST dynParamValue NO-LOCK USE-INDEX si-paramValueID.
        IF AVAILABLE dynParamValue THEN
        iParamValueID = dynParamValue.paramValueID + 1.
        FIND FIRST dynParamValue NO-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
        CREATE bDynParamValue.
        BUFFER-COPY dynParamValue EXCEPT paramValueID user-id rec_key TO bDynParamValue
            ASSIGN
                bDynParamValue.paramValueID     = iParamValueID
                bDynParamValue.user-id          = USERID("ASI")
                bDynParamValue.paramDescription = "New Task ID " + STRING(iParamValueID)
                rRowID                          = ROWID(bDynParamValue)
                .
        UPDATE
            SKIP(1)
            bDynParamValue.paramDescription AT 5
            SKIP(1)
                WITH FRAME fDescription WIDTH 50
                     CENTERED ROW 10 NO-LABELS
                     VIEW-AS DIALOG-BOX
                     TITLE "Enter Task Description".
    END. /* do trans */
    RUN pGetParamValue.
    FIND FIRST ttDynParamValue
         WHERE ttDynParamValue.paramValueRowID EQ rRowID.
    rRowID = ROWID(ttDynParamValue).
    REPOSITION browseParamValue TO ROWID rRowID.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteTask s-object 
PROCEDURE pDeleteTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMsg AS CHARACTER NO-UNDO.

    cMsg = "Deleted Selected Task?".
    FIND FIRST Task NO-LOCK
         WHERE Task.subjectID    EQ ttDynParamValue.subjectID
           AND Task.user-id      EQ ttDynParamValue.user-id
           AND Task.prgmName     EQ ttDynParamValue.prgmName
           AND Task.paramValueID EQ ttDynParamValue.paramValueID
         NO-ERROR.
    IF AVAILABLE Task THEN
    cMsg = cMsg + CHR(10) + CHR(10)
         + "Scheduled Task Exists and will be DELETED also"
         .
    MESSAGE cMsg
    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL
    UPDATE lDelete AS LOGICAL.
    IF lDelete THEN DO TRANSACTION:
        IF AVAILABLE Task THEN DO:
            FIND CURRENT Task EXCLUSIVE-LOCK.
            DELETE Task.
        END. /* if avail task */
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
        DELETE dynParamValue.
        DELETE ttDynParamValue.
    END. /* if delete */
    IF lDelete THEN
    RUN pReopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValue s-object 
PROCEDURE pGetParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttDynParamValue.
    RUN spGetTaskFilter (OUTPUT cMnemonic, OUTPUT cPrgmName, OUTPUT cUserID).
    FOR EACH dynParamValue NO-LOCK
        WHERE dynParamValue.module BEGINS cMnemonic
          AND dynParamValue.securityLevel LE DYNAMIC-FUNCTION("sfUserSecurityLevel")
        WITH FRAME filterFrame:
        CREATE ttDynParamValue.
        ASSIGN
            ttDynParamValue.subjectID        = dynParamValue.subjectID
            ttDynParamValue.mnemonic         = dynParamValue.module
            ttDynParamValue.paramDescription = dynParamValue.paramDescription
            ttDynParamValue.prgmName         = dynParamValue.prgmName
            ttDynParamValue.paramTitle       = dynParamValue.paramTitle
            ttDynParamValue.module           = dynParamValue.module
            ttDynParamValue.user-id          = dynParamValue.user-id
            ttDynParamValue.paramValueID     = dynParamValue.paramValueID
            ttDynParamValue.outputFormat     = dynParamValue.outputFormat
            ttDynParamValue.securityLevel    = dynParamValue.securityLevel
            ttDynParamValue.externalForm     = dynParamValue.externalForm
            ttDynParamValue.paramValueRowID  = ROWID(dynParamValue)
            ttDynParamValue.allData          = ttDynParamValue.mnemonic + "|"
                                             + ttDynParamValue.paramDescription + "|"
                                             + ttDynParamValue.prgmName + "|"
                                             + ttDynParamValue.paramTitle + "|"
                                             + ttDynParamValue.module + "|"
                                             + ttDynParamValue.user-id + "|"
                                             + ttDynParamValue.outputFormat + "|"
                                             + ttDynParamValue.externalForm + "|"
                                             + STRING(ttDynParamValue.recordLimit) + "|"
                                             + STRING(ttDynParamValue.paramValueID)
                                             .
        IF NOT CAN-DO(filterDescrip:LIST-ITEMS,ttDynParamValue.paramDescription) THEN
        filterDescrip:ADD-LAST(ttDynParamValue.paramDescription).
        IF NOT CAN-DO(filterModule:LIST-ITEMS,ttDynParamValue.module) THEN
        filterModule:ADD-LAST(ttDynParamValue.module).
        IF NOT CAN-DO(filterUser:LIST-ITEMS,ttDynParamValue.user-id) THEN
        filterUser:ADD-LAST(ttDynParamValue.user-id).
        IF NOT CAN-DO(filterPrgmName:LIST-ITEMS,ttDynParamValue.prgmName) THEN
        filterPrgmName:ADD-LAST(ttDynParamValue.PrgmName).
    END. /* each dynParamValue */
    RUN pReopenBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValueRowID s-object 
PROCEDURE pGetParamValueRowID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oprRowID AS ROWID NO-UNDO.
    
    IF AVAILABLE ttDynParamValue THEN
    oprRowID = ttDynParamValue.paramValueRowID.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings s-object 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ cCompany
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ "{&defaultUser}") THEN
    RUN pSaveSettings ("{&defaultUser}").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE browseParamValue:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE browseParamValue:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        hColumn = BROWSE browseParamValue:GET-BROWSE-COLUMN(kdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE browseParamValue:MOVE-COLUMN(kdx,idx).
                END. /* if name */
            END. /* do kdx */
        END. /* do idx */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse s-object 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CASE cColumnLabel:
        WHEN "mnemonic" THEN
        RUN pBymnemonic.
        WHEN "paramDescription" THEN
        RUN pByParamDescription.
        WHEN "prgmName" THEN
        RUN pByPrgmName.
        WHEN "paramTitle" THEN
        RUN pByParamTitle.
        WHEN "taskID" THEN
        RUN pByTaskID.
        WHEN "user-id" THEN
        RUN pByUserID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    IF AVAILABLE ttDynParamValue THEN
    APPLY "VALUE-CHANGED":U TO browseParamValue IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunTask s-object 
PROCEDURE pRunTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplParameters AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
    
    IF iplParameters EQ YES OR
      (iplParameters EQ NO AND CAN-DO("Grid,View",ttDynParamValue.outputFormat)) THEN DO:
        RUN AOA/Jasper.p (
            ttDynParamValue.subjectID,
            ttDynParamValue.user-id,
            ttDynParamValue.prgmName,
            ttDynParamValue.paramValueID,
            iplParameters
            ).
        rRowID = ttDynParamValue.paramValueRowID.
        RUN pGetParamValue.
        FIND FIRST ttDynParamValue
             WHERE ttDynParamValue.paramValueRowID EQ rRowID
             NO-ERROR.
        IF AVAILABLE ttDynParamValue THEN DO:
            rRowID = ROWID(ttDynParamValue).
            REPOSITION browseParamValue TO ROWID rRowID.
        END. /* if avail */
    END. /* if */
    ELSE
    RUN pRunNow (
        ttDynParamValue.outputFormat,
        ttDynParamValue.paramTitle,
        NO
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings s-object 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = cCompany
            user-print.program-id = "{&program-id}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date   = TODAY
        user-print.next-time   = TIME
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE browseParamValue:NUM-COLUMNS:
        ASSIGN
            idx = idx + 1
            hColumn = BROWSE browseParamValue:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE sysCtrlBrowse:MIN-COLUMN-WIDTH-CHARS*/ ))                                              
            .
    END. /* do jdx */
    FIND CURRENT user-print NO-LOCK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pScheduleTask s-object 
PROCEDURE pScheduleTask :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN spSetSessionParam ("ParamValueID", STRING(ttDynParamValue.paramValueID)).
    RUN AOA/dynSched.w.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize s-object 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdHeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth  AS DECIMAL NO-UNDO.
    
    HIDE FRAME {&FRAME-NAME}.
    ASSIGN
        FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = ipdHeight
        FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = ipdWidth
        FRAME {&FRAME-NAME}:HEIGHT   = ipdHeight
        FRAME {&FRAME-NAME}:WIDTH    = ipdWidth
        BROWSE {&BROWSE-NAME}:HEIGHT = ipdHeight - BROWSE {&BROWSE-NAME}:ROW + 1
        BROWSE {&BROWSE-NAME}:WIDTH  = ipdWidth  - BROWSE {&BROWSE-NAME}:COL + 1
        searchBar:WIDTH              = ipdWidth  - searchBar:COL + 1
        .
    VIEW FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed s-object 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     Receive and process 'state-changed' methods
               (issued by 'new-state' event).
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

