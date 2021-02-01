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

&Scoped-define defaultUser _default
&Scoped-define prgmName userTasks.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDescrip       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExternalForm  AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cHotkey        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGroup         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPassedModule  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModule        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cParamValueID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScheduled     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubjectID     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSubjectTitle  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTaskDescrip   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserGroups    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEmail         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFavorite      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hContainer     AS HANDLE    NO-UNDO.
DEFINE VARIABLE pHandle        AS HANDLE    NO-UNDO.
DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRecordLimit   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iSecurityLevel AS INTEGER   NO-UNDO.
DEFINE VARIABLE lOK            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMoveColumn    AS LOGICAL   NO-UNDO.

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttDynParamValue NO-UNDO
    FIELD email              AS LOGICAL LABEL "Email"
    FIELD externalForm     LIKE dynParamValue.externalForm
    FIELD favorite         LIKE dynParamValue.favorite
    FIELD lastRunDateTime  LIKE dynParamValue.lastRunDateTime
    FIELD mnemonic         LIKE prgrms.mnemonic LABEL "Hotkey"
    FIELD module           LIKE dynParamValue.module
    FIELD outputFormat     LIKE dynParamValue.outputFormat
    FIELD paramDescription LIKE dynParamValue.paramDescription LABEL "Description"
    FIELD paramTitle       LIKE dynParamValue.paramTitle
    FIELD paramValueID     LIKE dynParamValue.paramValueID
    FIELD prgmName         LIKE dynParamValue.prgmName
    FIELD recordLimit      LIKE dynParamValue.recordLimit
    FIELD runSync          LIKE dynParamValue.runSync
    FIELD autoClose        LIKE dynParamValue.autoClose
    FIELD saveLastRun      LIKE dynParamValue.saveLastRun
    FIELD scheduled          AS LOGICAL LABEL "Sched"
    FIELD securityLevel    LIKE dynParamValue.securityLevel
    FIELD subjectID        LIKE dynParamValue.subjectID
    FIELD subjectType      LIKE dynSubject.subjectType
    FIELD subjectGroup     LIKE dynParamValue.subjectGroup
    FIELD titleDescription   AS CHARACTER FORMAT "x(80)" LABEL "Title / Description"
    FIELD user-id          LIKE dynParamValue.user-id
    FIELD paramValueRowID    AS ROWID
    FIELD allData            AS CHARACTER
        INDEX ttDynParamValue IS PRIMARY paramTitle user-id paramValueID
        INDEX paramValueRowID paramValueRowID
        .
iSecurityLevel = DYNAMIC-FUNCTION("sfUserSecurityLevel").

{methods/lockWindowUpdate.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseTasks

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDynParamValue

/* Definitions for BROWSE browseTasks                                   */
&Scoped-define FIELDS-IN-QUERY-browseTasks ttDynParamValue.favorite ttDynParamValue.titleDescription ttDynParamValue.module fUserID(ttDynParamValue.user-id) @ ttDynParamValue.user-id ttDynParamValue.mnemonic ttDynParamValue.subjectGroup ttDynParamValue.scheduled fParamValueID(ttDynParamValue.paramValueID) @ cParamValueID fSubjectID(ttDynParamValue.subjectID) @ cSubjectID ttDynParamValue.email ttDynParamValue.outputFormat ttDynParamValue.lastRunDateTime ttDynParamValue.securityLevel ttDynParamValue.runSync ttDynParamValue.autoClose ttDynParamValue.subjectType ttDynParamValue.externalForm   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseTasks   
&Scoped-define SELF-NAME browseTasks
&Scoped-define QUERY-STRING-browseTasks FOR EACH ttDynParamValue WHERE ttDynParamValue.securityLevel LE iSecurityLevel   AND ttDynParamValue.paramDescription BEGINS cDescrip   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.mnemonic BEGINS cHotkey   AND ttDynParamValue.subjectGroup BEGINS cGroup   AND (ttDynParamValue.scheduled EQ (cScheduled EQ "Yes")    OR cScheduled EQ "<All>")   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-browseTasks OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue WHERE ttDynParamValue.securityLevel LE iSecurityLevel   AND ttDynParamValue.paramDescription BEGINS cDescrip   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.mnemonic BEGINS cHotkey   AND ttDynParamValue.subjectGroup BEGINS cGroup   AND (ttDynParamValue.scheduled EQ (cScheduled EQ "Yes")    OR cScheduled EQ "<All>")   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-browseTasks ttDynParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseTasks ttDynParamValue


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseTasks}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnMoveColumn btnSearch searchBar ~
filterTasks filterDescrip filterModule filterUser filterHotkey filterGroup ~
filterScheduled browseTasks 
&Scoped-Define DISPLAYED-OBJECTS searchBar filterTasks filterDescrip ~
filterModule filterUser filterHotkey filterGroup filterScheduled 

/* Custom List Definitions                                              */
/* taskObjects,List-2,List-3,List-4,List-5,List-6                       */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fEmail s-object 
FUNCTION fEmail RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsScheduled s-object 
FUNCTION fIsScheduled RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fParamValueID s-object 
FUNCTION fParamValueID RETURNS CHARACTER
  ( ipiParamValueID AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSubjectID s-object 
FUNCTION fSubjectID RETURNS CHARACTER
  ( ipiSubjectID AS INTEGER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUSerID s-object 
FUNCTION fUSerID RETURNS CHARACTER
  ( ipcUserID AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnMoveColumn 
     IMAGE-UP FILE "Graphics/32x32/spreadsheet.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Column" 
     SIZE 8 BY 1.91 TOOLTIP "Move Column".

DEFINE BUTTON btnSearch 
     IMAGE-UP FILE "Graphics/16x16/search.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Search" 
     SIZE 5 BY 1.19 TOOLTIP "Search".

DEFINE BUTTON btnSort 
     IMAGE-UP FILE "Graphics/32x32/sort_az_descending.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/sort_az_descending_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort" 
     SIZE 8 BY 1.91 TOOLTIP "Sort".

DEFINE VARIABLE filterDescrip AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 62 BY 1 TOOLTIP "Select Report"
     FONT 1 NO-UNDO.

DEFINE VARIABLE filterGroup AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 34 BY 1 TOOLTIP "Select User"
     FONT 1 NO-UNDO.

DEFINE VARIABLE filterHotkey AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Select Module"
     FONT 1 NO-UNDO.

DEFINE VARIABLE filterModule AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 TOOLTIP "Select Module"
     FONT 1 NO-UNDO.

DEFINE VARIABLE filterScheduled AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEMS "<All>","No","Yes" 
     DROP-DOWN-LIST
     SIZE 11.4 BY 1 TOOLTIP "Select Module"
     FONT 1 NO-UNDO.

DEFINE VARIABLE filterUser AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 50
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 15 BY 1 TOOLTIP "Select User"
     FONT 1 NO-UNDO.

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 48.6 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE VARIABLE filterTasks AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "&Favorites", 1,
"My &Tasks", 2,
"My &Groups", 3,
"&Default Tasks", 4,
"&All Tasks", 5
     SIZE 84 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 158 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseTasks FOR 
      ttDynParamValue SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseTasks s-object _FREEFORM
  QUERY browseTasks DISPLAY
      ttDynParamValue.favorite COLUMN-LABEL "Fav" VIEW-AS TOGGLE-BOX
ttDynParamValue.titleDescription LABEL-BGCOLOR 14
ttDynParamValue.module LABEL-BGCOLOR 14
fUserID(ttDynParamValue.user-id) @ ttDynParamValue.user-id LABEL-BGCOLOR 14
ttDynParamValue.mnemonic LABEL-BGCOLOR 14
ttDynParamValue.subjectGroup LABEL-BGCOLOR 14
ttDynParamValue.scheduled VIEW-AS TOGGLE-BOX
fParamValueID(ttDynParamValue.paramValueID) @ cParamValueID COLUMN-LABEL "Sched ID" LABEL-BGCOLOR 14
fSubjectID(ttDynParamValue.subjectID) @ cSubjectID COLUMN-LABEL "Subject" LABEL-BGCOLOR 14
ttDynParamValue.email VIEW-AS TOGGLE-BOX
ttDynParamValue.outputFormat LABEL-BGCOLOR 14
ttDynParamValue.lastRunDateTime LABEL-BGCOLOR 14
ttDynParamValue.securityLevel
ttDynParamValue.runSync VIEW-AS TOGGLE-BOX
ttDynParamValue.autoClose VIEW-AS TOGGLE-BOX
ttDynParamValue.subjectType
ttDynParamValue.externalForm
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-BOX NO-ROW-MARKERS SEPARATORS SIZE 159 BY 19.43
         BGCOLOR 25 FONT 22 ROW-HEIGHT-CHARS .86.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnSort AT ROW 3.62 COL 151 HELP
          "Sort" WIDGET-ID 48
     btnMoveColumn AT ROW 1.71 COL 151 HELP
          "Move Column" WIDGET-ID 42
     btnSearch AT ROW 1.62 COL 61 HELP
          "Search" WIDGET-ID 288
     searchBar AT ROW 1.71 COL 10.4 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     filterTasks AT ROW 1.71 COL 67 NO-LABEL WIDGET-ID 66
     filterDescrip AT ROW 4.33 COL 3 HELP
          "Select Report" NO-LABEL WIDGET-ID 50
     filterModule AT ROW 4.33 COL 66 HELP
          "Select Module" NO-LABEL WIDGET-ID 54
     filterUser AT ROW 4.33 COL 77 HELP
          "Select User" NO-LABEL WIDGET-ID 58
     filterHotkey AT ROW 4.33 COL 93 HELP
          "Select Hotkey" NO-LABEL WIDGET-ID 290
     filterGroup AT ROW 4.33 COL 104 HELP
          "Select Group" NO-LABEL WIDGET-ID 294
     filterScheduled AT ROW 4.33 COL 138.6 HELP
          "Select Hotkey" NO-LABEL WIDGET-ID 298
     browseTasks AT ROW 6.24 COL 2 WIDGET-ID 500
     "User:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.62 COL 77 WIDGET-ID 60
     "Hotkey:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.62 COL 93 WIDGET-ID 292
     "Module:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.62 COL 66 WIDGET-ID 56
     "Description:" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 3.62 COL 3 WIDGET-ID 52
     "Group:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 3.62 COL 104 WIDGET-ID 296
     "Scheduled:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 3.62 COL 138 WIDGET-ID 300
     RECT-1 AT ROW 1.24 COL 2 WIDGET-ID 302
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 FONT 6 WIDGET-ID 100.


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
         HEIGHT             = 24.67
         WIDTH              = 160.4.
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
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB browseTasks filterScheduled F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 24.67
       FRAME F-Main:WIDTH            = 160.

ASSIGN 
       browseTasks:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       browseTasks:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR BUTTON btnSort IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX filterDescrip IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterGroup IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterHotkey IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterModule IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterScheduled IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterUser IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseTasks
/* Query rebuild information for BROWSE browseTasks
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue
WHERE ttDynParamValue.securityLevel LE iSecurityLevel
  AND ttDynParamValue.paramDescription BEGINS cDescrip
  AND ttDynParamValue.module BEGINS cModule
  AND ttDynParamValue.user-id BEGINS cUserID
  AND ttDynParamValue.mnemonic BEGINS cHotkey
  AND ttDynParamValue.subjectGroup BEGINS cGroup
  AND (ttDynParamValue.scheduled EQ (cScheduled EQ "Yes")
   OR cScheduled EQ "<All>")
  AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseTasks */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME browseTasks
&Scoped-define SELF-NAME browseTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseTasks s-object
ON DEFAULT-ACTION OF browseTasks IN FRAME F-Main
DO:
    RUN pAttribute.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseTasks s-object
ON DELETE-CHARACTER OF browseTasks IN FRAME F-Main
DO:
    RUN pDelete.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseTasks s-object
ON START-SEARCH OF browseTasks IN FRAME F-Main
DO:
    &Scoped-define sortButton
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseTasks s-object
ON VALUE-CHANGED OF browseTasks IN FRAME F-Main
DO:
    RUN pFavorite.
    RUN pSetButtons.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveColumn s-object
ON CHOOSE OF btnMoveColumn IN FRAME F-Main /* Move Column */
DO:
    ASSIGN
        btnSort:SENSITIVE = lMoveColumn AND VALID-HANDLE(hColumnLabel)
        lMoveColumn = NOT lMoveColumn
        BROWSE {&BROWSE-NAME}:COLUMN-MOVABLE = lMoveColumn
        .
    SELF:LOAD-IMAGE("Graphics/32x32/"
        + IF lMoveColumn THEN "spreadsheet_column.png"
          ELSE "spreadsheet.png")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSearch s-object
ON CHOOSE OF btnSearch IN FRAME F-Main /* Search */
DO:
    ASSIGN searchBar.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSort s-object
ON CHOOSE OF btnSort IN FRAME F-Main /* Sort */
DO:
    lAscending = NOT lAscending.
    RUN pReopenBrowse.
    btnSort:LOAD-IMAGE("Graphics/32x32/"
        + IF lAscending THEN "sort_az_descending.png"
          ELSE "sort_az_descending2.png")
        .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterDescrip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterDescrip s-object
ON VALUE-CHANGED OF filterDescrip IN FRAME F-Main
DO:
    ASSIGN
        {&SELF-NAME}
        cDescrip = {&SELF-NAME}
        .
    IF cDescrip EQ "<All>" THEN
    cDescrip = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterGroup
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterGroup s-object
ON VALUE-CHANGED OF filterGroup IN FRAME F-Main
DO:
    ASSIGN
        {&SELF-NAME}
        cGroup = {&SELF-NAME}
        .
    IF cGroup EQ "<All>" THEN
    cGroup = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterHotkey
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterHotkey s-object
ON VALUE-CHANGED OF filterHotkey IN FRAME F-Main
DO:
    ASSIGN
        {&SELF-NAME}
        cHotkey = {&SELF-NAME}
        .
    IF cHotkey EQ "<All>" THEN
    cHotkey = "".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterModule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterModule s-object
ON VALUE-CHANGED OF filterModule IN FRAME F-Main
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


&Scoped-define SELF-NAME filterScheduled
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterScheduled s-object
ON VALUE-CHANGED OF filterScheduled IN FRAME F-Main
DO:
    ASSIGN
        {&SELF-NAME}
        cScheduled = {&SELF-NAME}
        .
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterTasks
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterTasks s-object
ON VALUE-CHANGED OF filterTasks IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    RUN pGetParamValue.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME filterUser
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterUser s-object
ON VALUE-CHANGED OF filterUser IN FRAME F-Main
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


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar s-object
ON HELP OF searchBar IN FRAME F-Main /* Search */
DO:
    RUN AOA/dynSubTableField.w (OUTPUT cSubjectTitle).
    IF cSubjectTitle NE "" THEN
    SELF:SCREEN-VALUE = cSubjectTitle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar s-object
ON RETURN OF searchBar IN FRAME F-Main /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK s-object 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom2.i}

/* If testing in the UIB, initialize the SmartObject. */  
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{AOA/includes/pRunNow.i "tt"}

{methods/sortByProc.i "pByHotkey" "ttDynParamValue.mnemonic"}
{methods/sortByProc.i "pByLastRunDateTime" "ttDynParamValue.lastRunDateTime"}
{methods/sortByProc.i "pByModule" "ttDynParamValue.module"}
{methods/sortByProc.i "pByOutputFormat" "ttDynParamValue.outputFormat"}
{methods/sortByProc.i "pByParamTitle" "ttDynParamValue.paramTitle"}
{methods/sortByProc.i "pByParamDescription" "ttDynParamValue.ParamDescription"}
{methods/sortByProc.i "pByParamValueID" "ttDynParamValue.paramValueID"}
{methods/sortByProc.i "pBySubjectID" "ttDynParamValue.subjectID"}
{methods/sortByProc.i "pBySubjectGroup" "ttDynParamValue.subjectGroup"}
{methods/sortByProc.i "pByTitleDescription" "ttDynParamValue.titleDescription"}
{methods/sortByProc.i "pByUserID" "ttDynParamValue.user-id"}

{AOA/includes/pGetDynParamValue.i}
{AOA/includes/pSetDynParamValue.i "dyn"}
{AOA/includes/pRunBusinessLogic.i}

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
  DEFINE VARIABLE cUser AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH usergrps NO-LOCK:
      IF CAN-DO(usergrps.users,USERID("ASI")) THEN
      cUserGroups = cUserGroups + usergrps.usergrps + ",".
  END.
  cUserGroups = TRIM(cUserGroups,",").

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
        filterTasks:SENSITIVE     = YES
        filterDescrip:SENSITIVE   = YES
        filterModule:SENSITIVE    = YES
        filterUser:SENSITIVE      = YES
        filterHotkey:SENSITIVE    = YES
        filterGroup:SENSITIVE     = YES
        filterScheduled:SENSITIVE = YES
        .
  END. /* with frame */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  ASSIGN
    hContainer = pHandle
    cScheduled = "<All>"
    .
  RUN pGetSettings (USERID("ASI")).
  filterTasks:SCREEN-VALUE = STRING(filterTasks).
  RUN spGetTaskFilter (OUTPUT cPassedModule, OUTPUT cPrgmName, OUTPUT cUser).
  IF cPassedModule NE "" THEN
  ASSIGN
      filterTasks:SCREEN-VALUE = "5"
      filterTasks
      .
  RUN pGetParamValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAttribute s-object 
PROCEDURE pAttribute :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF NOT AVAILABLE ttDynParamValue OR
       ttDynParamValue.user-id NE USERID("ASI") THEN RETURN.

    ASSIGN
        cTaskDescrip  = ttDynParamValue.paramDescription
        cExternalForm = ttDynParamValue.externalForm
        iRecordLimit  = ttDynParamValue.recordLimit
        lFavorite     = ttDynParamValue.favorite
        .
    RUN AOA/dynSubjctAttr.w (
        INPUT-OUTPUT cTaskDescrip,
        INPUT-OUTPUT cExternalForm,
        INPUT-OUTPUT iRecordLimit,
        INPUT-OUTPUT lFavorite,
        OUTPUT lOK
        ).
    IF lOK THEN DO TRANSACTION WITH FRAME {&FRAME-NAME}:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
        ASSIGN
            filterDescrip:LIST-ITEMS         = REPLACE(filterDescrip:LIST-ITEMS,
                                                       ttDynParamValue.paramTitle + " - " + ttDynParamValue.paramDescription,
                                                       ttDynParamValue.paramTitle + " - " + cTaskDescrip
                                                       )
            filterDescrip:SCREEN-VALUE       = "<All>"
            dynParamValue.paramDescription   = cTaskDescrip
            dynParamValue.externalForm       = cExternalForm
            dynParamValue.recordLimit        = iRecordLimit
            dynParamValue.favorite           = lFavorite
            ttDynParamValue.titleDescription = ttDynParamValue.paramTitle + " - "
                                             + cTaskDescrip
            ttDynParamValue.paramDescription = cTaskDescrip
            ttDynParamValue.externalForm     = cExternalForm
            ttDynParamValue.recordLimit      = iRecordLimit
            ttDynParamValue.favorite         = lFavorite
            .
        RELEASE dynParamValue.
        BROWSE {&BROWSE-NAME}:REFRESH().
        RUN pFavorite.
    END. /* if ok */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCallAudit s-object 
PROCEDURE pCallAudit :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable AS HANDLE NO-UNDO.

    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN RETURN.
    hTable = BUFFER dynParamValue:HANDLE.
    IF VALID-HANDLE(hTable) THEN
    RUN system/CallAudit.p ("dynParamValue", hTable, "Browser", PROGRAM-NAME(1)).

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

    DEFINE BUFFER bDynParamValue    FOR dynParamValue.
    DEFINE BUFFER bDynValueColumn   FOR dynValueColumn.
    DEFINE BUFFER bDynValueParam    FOR dynValueParam.
    DEFINE BUFFER bDynValueParamSet FOR dynValueParamSet.
    
    IF NOT AVAILABLE ttDynParamValue THEN RETURN.

    FIND LAST dynParamValue NO-LOCK USE-INDEX si-paramValueID.
    IF AVAILABLE dynParamValue THEN
    iParamValueID = dynParamValue.paramValueID + 1.
    ASSIGN
        cTaskDescrip = ttDynParamValue.paramTitle + " " + STRING(iParamValueID)
        lFavorite    = NO
        .
    RUN AOA/dynTaskDescrip.w (
        INPUT-OUTPUT cTaskDescrip,
        INPUT-OUTPUT lFavorite,
        OUTPUT lOK
        ).
    IF NOT lOK THEN RETURN.

    DO TRANSACTION:
        FIND FIRST dynParamValue NO-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
        CREATE bDynParamValue.
        BUFFER-COPY dynParamValue
             EXCEPT favorite paramDescription paramValueID user-id rec_key
                 TO bDynParamValue
             ASSIGN
                 bDynParamValue.paramValueID     = iParamValueID
                 bDynParamValue.user-id          = USERID("ASI")
                 bDynParamValue.paramDescription = cTaskDescrip
                 bDynParamValue.favorite         = lFavorite
                 rRowID                          = ROWID(bDynParamValue)
                 .
        FOR EACH dynValueColumn NO-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
            :
            CREATE bDynValueColumn.
            BUFFER-COPY dynValueColumn
                 EXCEPT paramValueID user-id rec_key
                     TO bDynValueColumn
                 ASSIGN
                     bDynValueColumn.paramValueID = iParamValueID
                     bDynValueColumn.user-id      = USERID("ASI")
                     .
        END. /* each dynvaluecolumn */
        FOR EACH dynValueParam NO-LOCK
            WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
              AND dynValueParam.user-id      EQ dynParamValue.user-id
              AND dynValueParam.prgmName     EQ dynParamValue.prgmName
              AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
            :
            CREATE bDynValueParam.
            BUFFER-COPY dynValueParam
                 EXCEPT paramValueID user-id rec_key
                     TO bDynValueParam
                 ASSIGN
                     bDynValueParam.paramValueID = iParamValueID
                     bDynValueParam.user-id      = USERID("ASI")
                     .
        END. /* each dynvalueparam */
        FOR EACH dynValueParamSet NO-LOCK
            WHERE dynValueParamSet.subjectID    EQ dynParamValue.subjectID
              AND dynValueParamSet.user-id      EQ dynParamValue.user-id
              AND dynValueParamSet.prgmName     EQ dynParamValue.prgmName
              AND dynValueParamSet.paramValueID EQ dynParamValue.paramValueID
            :
            CREATE bDynValueParamSet.
            BUFFER-COPY dynValueParamSet
                 EXCEPT paramValueID user-id rec_key
                     TO bDynValueParamSet
                 ASSIGN
                     bDynValueParamSet.paramValueID = iParamValueID
                     bDynValueParamSet.user-id      = USERID("ASI")
                     .
        END. /* each dynvalueparamset */
    END. /* do trans */
    ASSIGN
        filterTasks:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2"
        filterTasks
        .
    RUN pGetParamValue.
    FIND FIRST ttDynParamValue
         WHERE ttDynParamValue.paramValueRowID EQ rRowID.
    ASSIGN
        rRowID = ROWID(ttDynParamValue)
        ttDynParamValue.favorite = lFavorite
        .
    IF ttDynParamValue.paramValueID NE 0 THEN
    REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
    
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
    
    IF NOT AVAILABLE ttDynParamValue THEN RETURN.

    cMsg = "Delete Selected Task?".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFavorite s-object 
PROCEDURE pFavorite :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pSetFavorite IN hContainer (ttDynParamValue.favorite).

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
    DO WITH FRAME {&FRAME-NAME}:
        EMPTY TEMP-TABLE ttDynParamValue.
        ASSIGN
            filterDescrip:LIST-ITEMS   = "<All>"
            filterModule:LIST-ITEMS    = "<All>"
            filterUser:LIST-ITEMS      = "<All>"
            filterHotkey:LIST-ITEMS    = "<All>"
            filterGroup:LIST-ITEMS     = "<All>"
            filterScheduled:LIST-ITEMS = "<All>,No,Yes"
            .
        FOR EACH dynParamValue NO-LOCK
            WHERE dynParamValue.module        BEGINS cPassedModule
              AND dynParamValue.securityLevel LE DYNAMIC-FUNCTION("sfUserSecurityLevel")
              AND dynParamValue.isLookup      EQ NO
              AND dynParamValue.prgmName      LT "[",
            FIRST dynSubject NO-LOCK
            WHERE dynSubject.subjectID EQ dynParamValue.subjectID
            WITH FRAME filterFrame:
            IF dynParamValue.paramValueID EQ 0 AND
               dynParamValue.user-id      NE "{&defaultUser}" THEN NEXT.
            CASE filterTasks:
                /* favorites */
                WHEN 1 THEN
                IF dynParamValue.user-id NE USERID("ASI") OR
                   dynParamValue.favorite EQ NO OR
                   dynParamValue.paramValueID EQ 0 THEN NEXT.
                /* my tasks */
                WHEN 2 THEN
                IF dynParamValue.user-id NE USERID("ASI") OR
                   dynParamValue.paramValueID EQ 0 THEN NEXT.
                /* my groups */
                WHEN 3 THEN
                IF NOT CAN-DO(cUserGroups,dynParamValue.subjectGroup) THEN NEXT.
                /* default tasks */
                WHEN 4 THEN
                IF dynParamValue.user-id NE "{&defaultUser}" THEN NEXT.
                WHEN 5 THEN
                IF dynParamValue.user-id NE "{&defaultUser}" AND
                   dynParamValue.paramValueID EQ 0 THEN NEXT. 
            END CASE.
            CREATE ttDynParamValue.
            ASSIGN
                ttDynParamValue.autoClose        = dynParamValue.autoClose
                ttDynParamValue.email            = fEmail()
                ttDynParamValue.externalForm     = dynParamValue.externalForm
                ttDynParamValue.favorite         = dynParamValue.favorite
                ttDynParamValue.lastRunDateTime  = dynParamValue.lastRunDateTime
                ttDynParamValue.mnemonic         = dynParamValue.mnemonic
                ttDynParamValue.module           = dynParamValue.module
                ttDynParamValue.outputFormat     = dynParamValue.outputFormat
                ttDynParamValue.paramDescription = dynParamValue.paramDescription
                ttDynParamValue.paramTitle       = dynParamValue.paramTitle
                ttDynParamValue.paramValueID     = dynParamValue.paramValueID
                ttDynParamValue.prgmName         = dynParamValue.prgmName
                ttDynParamValue.runSync          = dynParamValue.runSync
                ttDynParamValue.saveLastRun      = dynParamValue.saveLastRun
                ttDynParamValue.scheduled        = fIsScheduled()
                ttDynParamValue.securityLevel    = dynParamValue.securityLevel
                ttDynParamValue.subjectGroup     = dynParamValue.subjectGroup
                ttDynParamValue.subjectID        = dynParamValue.subjectID
                ttDynParamValue.subjectType      = dynSubject.subjectType
                ttDynParamValue.titleDescription = ttDynParamValue.paramTitle + " - "
                                                 + REPLACE(ttDynParamValue.paramDescription,"System Default","Default")
                ttDynParamValue.user-id          = dynParamValue.user-id
                ttDynParamValue.paramValueRowID  = ROWID(dynParamValue)
                ttDynParamValue.allData          = ttDynParamValue.mnemonic + "|"
                                                 + ttDynParamValue.prgmName + "|"
                                                 + ttDynParamValue.module + "|"
                                                 + ttDynParamValue.user-id + "|"
                                                 + ttDynParamValue.outputFormat + "|"
                                                 + ttDynParamValue.subjectType + "|"
                                                 + ttDynParamValue.subjectGroup + "|"
                                                 + ttDynParamValue.titleDescription + "|"
                                                 + ttDynParamValue.externalForm + "|"
                                                 + STRING(ttDynParamValue.recordLimit) + "|"
                                                 + STRING(ttDynParamValue.paramValueID)
                                                 .
            IF ttDynParamValue.outputFormat EQ "" THEN
            ttDynParamValue.outputFormat = "Grid".
            IF NOT CAN-DO(filterDescrip:LIST-ITEMS,ttDynParamValue.titleDescription) THEN
            filterDescrip:ADD-LAST(ttDynParamValue.titleDescription).
            IF NOT CAN-DO(filterModule:LIST-ITEMS,ttDynParamValue.module) THEN
            filterModule:ADD-LAST(ttDynParamValue.module).
            IF NOT CAN-DO(filterUser:LIST-ITEMS,ttDynParamValue.user-id) THEN
            filterUser:ADD-LAST(ttDynParamValue.user-id).
            IF NOT CAN-DO(filterHotkey:LIST-ITEMS,ttDynParamValue.mnemonic) THEN
            filterHotkey:ADD-LAST(ttDynParamValue.mnemonic).
            IF NOT CAN-DO(filterGroup:LIST-ITEMS,ttDynParamValue.subjectGroup) THEN
            filterGroup:ADD-LAST(ttDynParamValue.subjectGroup).
        END. /* each dynParamValue */
    END. /* do with frame */
    ASSIGN
        cDescrip = IF CAN-DO(filterDescrip:LIST-ITEMS,cDescrip) AND cDescrip NE "" THEN cDescrip ELSE ""
        cModule  = IF CAN-DO(filterModule:LIST-ITEMS, cModule)  AND cModule  NE "" THEN cModule  ELSE ""
        cUserID  = IF CAN-DO(filterUser:LIST-ITEMS,   cUserID)  AND cUserID  NE "" THEN cUserID  ELSE ""
        cHotKey  = IF CAN-DO(filterHotkey:LIST-ITEMS, cHotKey)  AND cHotKey  NE "" THEN cHotKey  ELSE ""
        cGroup   = IF CAN-DO(filterGroup:LIST-ITEMS,  cGroup)   AND cGroup   NE "" THEN cGroup   ELSE ""
        filterDescrip:SCREEN-VALUE   = IF cDescrip NE "" THEN cDescrip ELSE "<All>"
        filterModule:SCREEN-VALUE    = IF cModule  NE "" THEN cModule  ELSE "<All>"
        filterUser:SCREEN-VALUE      = IF cUserID  NE "" THEN cUserID  ELSE "<All>"
        filterHotkey:SCREEN-VALUE    = IF cHotKey  NE "" THEN cHotKey  ELSE "<All>"
        filterGroup:SCREEN-VALUE     = IF cGroup   NE "" THEN cGroup   ELSE "<All>"
        filterScheduled:SCREEN-VALUE = cScheduled
        .
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
                      AND user-print.program-id EQ "{&prgmName}"
                      AND user-print.user-id    EQ "{&defaultUser}") THEN
    RUN pSaveSettings ("{&defaultUser}").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ cCompany
           AND user-print.program-id EQ "{&prgmName}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        hColumn = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(kdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE {&BROWSE-NAME}:MOVE-COLUMN(kdx,idx).
                END. /* if name */
            END. /* do kdx */
            IF user-print.field-name[idx] EQ "filterTasks" THEN
            filterTasks = INTEGER(user-print.field-value[idx]).
        END. /* do idx */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPageFormat s-object 
PROCEDURE pPageFormat :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPageOrientation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPageFormat      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageHeight      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPageWidth       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lContinue        AS LOGICAL   NO-UNDO.

    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ ttdynParamValue.paramValueRowID
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN RETURN.
    ASSIGN
        iPageFormat      = dynParamValue.pageFormat
        cPageOrientation = dynParamValue.pageOrientation
        iPageWidth       = dynParamValue.pageWidth
        iPageHeight      = dynParamValue.pageHeight
        .
    RUN AOA/dynPageFormat.w (
        INPUT-OUTPUT iPageFormat,
        INPUT-OUTPUT cPageOrientation,
        INPUT-OUTPUT iPageWidth,
        INPUT-OUTPUT iPageHeight,
        OUTPUT lContinue
        ).
    IF lContinue THEN
    DO TRANSACTION:
        RUN pSetDynParamValue (
            dynParamValue.subjectID,
            dynParamValue.user-id,
            dynParamValue.prgmName,
            dynParamValue.paramValueID).
        FIND CURRENT dynParamValue EXCLUSIVE-LOCK.
        ASSIGN
            dynParamValue.pageFormat      = iPageFormat
            dynParamValue.pageOrientation = cPageOrientation
            dynParamValue.pageWidth       = iPageWidth
            dynParamValue.pageHeight      = iPageHeight
            .
        FIND CURRENT dynParamValue NO-LOCK.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRefreshBrowse s-object 
PROCEDURE pRefreshBrowse :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDefaultOutputFormat AS CHARACTER NO-UNDO.

    RUN spGetSessionParam ("DefaultOutputFormat", OUTPUT cDefaultOutputFormat).
    ttDynParamValue.outputFormat = cDefaultOutputFormat.
    BROWSE {&BROWSE-NAME}:REFRESH().
    APPLY "VALUE-CHANGED":U TO BROWSE browseTasks.

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
        WHEN "hotkey" THEN
        RUN pByHotkey.
        WHEN "lastRunDateTime" THEN
        RUN pByLastRunDateTime.
        WHEN "module" THEN
        RUN pByModule.
        WHEN "outputFormat" THEN
        RUN pByOutputFormat.
        WHEN "paramDescription" THEN
        RUN pByParamDescription.
        WHEN "paramTitle" THEN
        RUN pByParamTitle.
        WHEN "cParamValueID" THEN
        RUN pByParamValueID.
        WHEN "cSubjectID" THEN
        RUN pBySubjectID.
        WHEN "subjectGroup" THEN
        RUN pBySubjectGroup.
        WHEN "taskID" THEN
        RUN pByTaskID.
        WHEN "titleDescription" THEN
        RUN pByTitleDescription.
        WHEN "user-id" THEN
        RUN pByUserID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.
    IF AVAILABLE ttDynParamValue THEN
    APPLY "VALUE-CHANGED":U TO BROWSE {&BROWSE-NAME}.
    ELSE
    RUN pSetButtons IN hContainer (NO, "", NO).
    {AOA/includes/pReopenBrowse.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pResultsJasper s-object 
PROCEDURE pResultsJasper :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTaskRecKey AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hAppSrvBin  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hJasper     AS HANDLE    NO-UNDO.
    
    FIND FIRST dynSubject NO-LOCK
         WHERE dynSubject.subjectID EQ ttDynParamValue.subjectID
         NO-ERROR.
    IF NOT AVAILABLE dynSubject THEN RETURN.
    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    RUN AOA/spJasper.p PERSISTENT SET hJasper.
    RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
    RUN pGetDynParamValue (
        ttDynParamValue.subjectID,
        USERID("ASI"),
        ttDynParamValue.prgmName,
        ttDynParamValue.paramValueID
        ).
    RUN spJasperQuery IN hJasper (
        ipcType,
        ttDynParamValue.paramValueRowID,
        dynSubject.subjectTitle,
        ipcUserID,
        hAppSrvBin,
        ipcTaskRecKey,
        OUTPUT cJasperFile
        ).
    DELETE PROCEDURE hJasper.
    DELETE PROCEDURE hAppSrvBin.
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

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
    DEFINE INPUT PARAMETER ipcOutputFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplParameters   AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE rRowID AS ROWID NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST dynParamValue EXCLUSIVE-LOCK
             WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID
             NO-ERROR.
        IF NOT AVAILABLE dynParamValue THEN RETURN.
        ASSIGN
            ttDynParamValue.outputFormat    = ipcOutputFormat
            ttDynParamValue.lastRunDateTime = NOW
            dynParamValue.outputFormat      = ipcOutputFormat
            dynParamValue.lastRunDateTime   = NOW
            .
        RELEASE dynParamValue.
    END. /* do trans */

    IF iplParameters EQ YES OR
      (iplParameters EQ NO AND
       CAN-DO("Grid,LocalCSV,View",ttDynParamValue.outputFormat)) THEN DO:
        CASE ipcOutputFormat:
            WHEN "Grid" THEN
            RUN AOA/Jasper.p (
                ttDynParamValue.subjectID,
                ttDynParamValue.user-id,
                ttDynParamValue.prgmName,
                ttDynParamValue.paramValueID,
                iplParameters
                ).
            WHEN "LocalCSV" OR WHEN "Print -d" OR WHEN "View" THEN
            RUN pResultsJasper (ipcOutputFormat, ttDynParamValue.user-id, "NoTask").
        END CASE.
        rRowID = ttDynParamValue.paramValueRowID.
        RUN pGetParamValue.
        FIND FIRST ttDynParamValue
             WHERE ttDynParamValue.paramValueRowID EQ rRowID
             NO-ERROR.
        IF AVAILABLE ttDynParamValue THEN DO:
            rRowID = ROWID(ttDynParamValue).
            REPOSITION {&BROWSE-NAME} TO ROWID rRowID.
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
           AND user-print.program-id EQ "{&prgmName}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = cCompany
            user-print.program-id = "{&prgmName}"
            user-print.user-id    = ipcUserID
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* not avail */
    ASSIGN
        user-print.next-date        = TODAY
        user-print.next-time        = TIME
        user-print.field-name       = ""
        user-print.field-value      = ""
        user-print.field-label      = ""
        .
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
        ASSIGN
            idx = idx + 1
            hColumn = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE sysCtrlBrowse:MIN-COLUMN-WIDTH-CHARS*/ ))                                              
            .
    END. /* do jdx */
    ASSIGN
        idx                         = idx + 1
        user-print.field-label[idx] = "filterTasks"
        user-print.field-name[idx]  = "filterTasks"
        user-print.field-value[idx] = STRING(filterTasks)                                              
        .
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
    IF NOT AVAILABLE ttDynParamValue THEN RETURN.

    IF ttDynParamValue.paramValueID EQ 0 THEN
    RUN pCopyTask.
    RUN spSetSessionParam ("ParamValueID", STRING(ttDynParamValue.paramValueID)).
    RUN AOA/dynSched.w.
    ttDynParamValue.scheduled = fIsScheduled().
    BROWSE {&BROWSE-NAME}:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetButtons s-object 
PROCEDURE pSetButtons :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pSetButtons IN hContainer (
        AVAILABLE ttDynParamValue,
        (IF AVAILABLE ttDynParamValue THEN ttDynParamValue.user-id ELSE "{&defaultUser}"),
        (AVAILABLE ttDynParamValue AND ttDynParamValue.favorite AND ttDynParamValue.user-id EQ USERID("ASI"))
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetFavorite s-object 
PROCEDURE pSetFavorite :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF ttDynParamValue.user-id EQ USERID("ASI") THEN DO:
        DO TRANSACTION:
            FIND FIRST dynParamValue EXCLUSIVE-LOCK
                 WHERE ROWID(dynParamValue) EQ ttDynParamValue.paramValueRowID.
            IF AVAILABLE dynParamValue THEN DO:
                ASSIGN
                    ttDynParamValue.favorite = NOT ttDynParamValue.favorite
                    dynParamValue.favorite   = NOT dynParamValue.favorite
                    .
            END. /* if avail */
            FIND CURRENT dynParamValue NO-LOCK.
        END. /* do trans */
        IF filterTasks EQ 1 THEN
        RUN pGetParamValue.
        ELSE DO:
            BROWSE {&BROWSE-NAME}:REFRESH().
            RUN pFavorite.
        END. /* else */
    END. /* if userid */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetParamValueDefault s-object 
PROCEDURE pSetParamValueDefault :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cUserID AS CHARACTER NO-UNDO.

    DEFINE BUFFER bDynParamValue    FOR dynParamValue.
    DEFINE BUFFER bDynValueColumn   FOR dynValueColumn.
    DEFINE BUFFER bDynValueParam    FOR dynValueParam.
    DEFINE BUFFER bDynValueParamSet FOR dynValueParamSet.

    cUserID = dynParamValue.user-id.
    DO TRANSACTION:
        FIND FIRST bDynParamValue EXCLUSIVE-LOCK
             WHERE bDynParamValue.subjectID    EQ dynParamValue.subjectID
               AND bDynParamValue.user-id      EQ dynParamValue.user-id
               AND bDynParamValue.prgmName     EQ dynParamValue.prgmName
               AND bDynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF NOT AVAILABLE bDynParamValue THEN DO:
            CREATE bDynParamValue.
            bDynParamValue.paramDescription = "User Default".
        END. /* if not avail */
        BUFFER-COPY dynParamValue
             EXCEPT paramValueID paramDescription
                 TO bDynParamValue.

        FOR EACH dynValueColumn NO-LOCK
            WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
              AND dynValueColumn.user-id      EQ dynParamValue.user-id
              AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
              AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
               BY dynValueColumn.sortOrder
            :
            FIND FIRST bDynValueColumn EXCLUSIVE-LOCK
                 WHERE bDynValueColumn.subjectID    EQ dynValueColumn.subjectID
                   AND bDynValueColumn.user-id      EQ dynValueColumn.user-id
                   AND bDynValueColumn.prgmName     EQ dynValueColumn.prgmName
                   AND bDynValueColumn.paramValueID EQ 0
                   AND bDynValueColumn.sortOrder    EQ dynValueColumn.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueColumn THEN
            CREATE bDynValueColumn.
            BUFFER-COPY dynValueColumn
                 EXCEPT paramValueID
                     TO bDynValueColumn.
        END. /* each dynvaluecolumn */
        FOR EACH dynValueParam NO-LOCK
            WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
              AND dynValueParam.user-id      EQ dynParamValue.user-id
              AND dynValueParam.prgmName     EQ dynParamValue.prgmName
              AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
               BY dynValueParam.sortOrder
            :
            FIND FIRST bDynValueParam EXCLUSIVE-LOCK
                 WHERE bDynValueParam.subjectID    EQ dynValueParam.subjectID
                   AND bDynValueParam.user-id      EQ dynValueParam.user-id
                   AND bDynValueParam.prgmName     EQ dynValueParam.prgmName
                   AND bDynValueParam.paramValueID EQ 0
                   AND bDynValueParam.sortOrder    EQ dynValueParam.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueParam THEN
            CREATE bDynValueParam.
            BUFFER-COPY dynValueParam
                 EXCEPT paramValueID
                     TO bDynValueParam.
        END. /* each dynValueParam */
        FOR EACH dynValueParamSet NO-LOCK
            WHERE dynValueParamSet.subjectID    EQ dynParamValue.subjectID
              AND dynValueParamSet.user-id      EQ dynParamValue.user-id
              AND dynValueParamSet.prgmName     EQ dynParamValue.prgmName
              AND dynValueParamSet.paramValueID EQ dynParamValue.paramValueID
               BY dynValueParamSet.sortOrder
            :
            FIND FIRST bDynValueParamSet EXCLUSIVE-LOCK
                 WHERE bDynValueParamSet.subjectID    EQ dynValueParamSet.subjectID
                   AND bDynValueParamSet.user-id      EQ dynValueParamSet.user-id
                   AND bDynValueParamSet.prgmName     EQ dynValueParamSet.prgmName
                   AND bDynValueParamSet.paramValueID EQ 0
                   AND bDynValueParamSet.sortOrder    EQ dynValueParamSet.sortOrder
                 NO-ERROR.
            IF NOT AVAILABLE bDynValueParamSet THEN
            CREATE bDynValueParamSet.
            BUFFER-COPY dynValueParamSet
                 EXCEPT paramValueID
                    TO bDynValueParamSet.
        END. /* each dynValueParamSet */
        RELEASE bDynParamValue.
        RELEASE bDynValueColumn.
        RELEASE bDynValueParam.
        RELEASE bDynValueParamSet.
    END. /* do trans */

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fEmail s-object 
FUNCTION fEmail RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN CAN-FIND(FIRST dynValueParam
                    WHERE dynValueParam.subjectID    EQ dynParamValue.subjectID
                      AND dynValueParam.user-id      EQ dynParamValue.user-id
                      AND dynValueParam.prgmName     EQ dynParamValue.prgmName
                      AND dynValueParam.paramValueID EQ dynParamValue.paramValueID
                      AND dynValueParam.paramName    EQ "svRecipients"
                      AND dynValueParam.paramValue   NE ""
                    ).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIsScheduled s-object 
FUNCTION fIsScheduled RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN CAN-FIND(FIRST Task
                    WHERE Task.subjectID    EQ ttDynParamValue.subjectID
                      AND Task.user-id      EQ ttDynParamValue.user-id
                      AND Task.prgmName     EQ ttDynParamValue.prgmName
                      AND Task.paramValueID EQ ttDynParamValue.paramValueID
                      AND Task.scheduled    EQ YES).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fParamValueID s-object 
FUNCTION fParamValueID RETURNS CHARACTER
  ( ipiParamValueID AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN IF ipiParamValueID EQ 0 THEN ""
           ELSE STRING(ipiParamValueID).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSubjectID s-object 
FUNCTION fSubjectID RETURNS CHARACTER
  ( ipiSubjectID AS INTEGER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN STRING(ipiSubjectID).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUSerID s-object 
FUNCTION fUSerID RETURNS CHARACTER
  ( ipcUserID AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN IF ipcUserID EQ "{&defaultUser}" THEN "" ELSE ipcUserID.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

