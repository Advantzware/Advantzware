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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cCompany   AS CHARACTER NO-UNDO.
DEFINE VARIABLE char-hdl   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMnemonic  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cModule    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmTitle AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSortMove  AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE pHandle    AS HANDLE    NO-UNDO.

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttDynParamValue NO-UNDO
    FIELD mnemonic     AS CHARACTER FORMAT "x(3)"  LABEL "HotKey"
    FIELD paramDescription AS CHARACTER FORMAT "x(30)" LABEL "Description"
    FIELD prgtitle     AS CHARACTER FORMAT "x(30)" LABEL "Report"
    FIELD module       AS CHARACTER FORMAT "x(8)"  LABEL "Module"
    FIELD user-id      AS CHARACTER FORMAT "x(20)" LABEL "User ID"
    FIELD paramValueID AS INTEGER   FORMAT ">>>>9" LABEL "Task ID"
    FIELD prgmName     AS CHARACTER FORMAT "x(10)" LABEL "Program"
    FIELD allData      AS CHARACTER
        INDEX mnemonic IS PRIMARY mnemonic
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartObject
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browseUserPrint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttDynParamValue

/* Definitions for BROWSE browseUserPrint                               */
&Scoped-define FIELDS-IN-QUERY-browseUserPrint ttDynParamValue.mnemonic ttDynParamValue.prgTitle ttDynParamValue.paramDescription ttDynParamValue.module ttDynParamValue.user-id ttDynParamValue.paramValueID ttDynParamValue.prgmName   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseUserPrint   
&Scoped-define SELF-NAME browseUserPrint
&Scoped-define QUERY-STRING-browseUserPrint FOR EACH ttDynParamValue WHERE ttDynParamValue.prgmName BEGINS cPrgmName   AND ttDynParamValue.prgTitle BEGINS cPrgmTitle   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-browseUserPrint OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue WHERE ttDynParamValue.prgmName BEGINS cPrgmName   AND ttDynParamValue.prgTitle BEGINS cPrgmTitle   AND ttDynParamValue.module BEGINS cModule   AND ttDynParamValue.user-id BEGINS cUserID   AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-browseUserPrint ttDynParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseUserPrint ttDynParamValue


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browseUserPrint}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRestoreDefaults searchBar browseUserPrint ~
btnSortMove 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-3 browseUserPrint 
&Scoped-define List-4 browseUserPrint 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 105 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE VARIABLE filterModule AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Module" NO-UNDO.

DEFINE VARIABLE filterPrgmName AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Module" NO-UNDO.

DEFINE VARIABLE filterReport AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select Report" NO-UNDO.

DEFINE VARIABLE filterUser AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 TOOLTIP "Select User" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseUserPrint FOR 
      ttDynParamValue SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseUserPrint s-object _FREEFORM
  QUERY browseUserPrint DISPLAY
      ttDynParamValue.mnemonic LABEL-BGCOLOR 14
ttDynParamValue.prgTitle LABEL-BGCOLOR 14
ttDynParamValue.paramDescription LABEL-BGCOLOR 14
ttDynParamValue.module LABEL-BGCOLOR 14
ttDynParamValue.user-id LABEL-BGCOLOR 14
ttDynParamValue.paramValueID LABEL-BGCOLOR 14
ttDynParamValue.prgmName LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 122 BY 25.95
         TITLE "Tasks".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnRestoreDefaults AT ROW 1 COL 37 HELP
          "Restore Defaults" WIDGET-ID 42
     searchBar AT ROW 1 COL 52 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     browseUserPrint AT ROW 1.95 COL 37 WIDGET-ID 500
     btnSortMove AT ROW 1 COL 41 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 48
     SPACE(0.00) SKIP(9.76)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME filterFrame
     filterReport AT ROW 1.95 COL 2 HELP
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
     "Report:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 52
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 1.95
         SIZE 34 BY 9.76
         TITLE "Filters" WIDGET-ID 600.


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

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME filterFrame:MOVE-AFTER-TAB-ITEM (searchBar:HANDLE IN FRAME F-Main)
       XXTABVALXX = FRAME filterFrame:MOVE-BEFORE-TAB-ITEM (browseUserPrint:HANDLE IN FRAME F-Main)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB browseUserPrint filterFrame F-Main */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE
       FRAME F-Main:HEIGHT           = 26.95
       FRAME F-Main:WIDTH            = 158.

/* SETTINGS FOR BROWSE browseUserPrint IN FRAME F-Main
   3 4                                                                  */
ASSIGN 
       browseUserPrint:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1
       browseUserPrint:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
       browseUserPrint:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FRAME filterFrame
                                                                        */
/* SETTINGS FOR COMBO-BOX filterModule IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterPrgmName IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterReport IN FRAME filterFrame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX filterUser IN FRAME filterFrame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseUserPrint
/* Query rebuild information for BROWSE browseUserPrint
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttDynParamValue
WHERE ttDynParamValue.prgmName BEGINS cPrgmName
  AND ttDynParamValue.prgTitle BEGINS cPrgmTitle
  AND ttDynParamValue.module BEGINS cModule
  AND ttDynParamValue.user-id BEGINS cUserID
  AND ttDynParamValue.allData MATCHES "*" + searchBar + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseUserPrint */
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

&Scoped-define BROWSE-NAME browseUserPrint
&Scoped-define SELF-NAME browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseUserPrint s-object
ON START-SEARCH OF browseUserPrint IN FRAME F-Main /* Tasks */
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseUserPrint s-object
ON VALUE-CHANGED OF browseUserPrint IN FRAME F-Main /* Tasks */
DO:
    {&OPEN-QUERY-browseParamValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults s-object
ON CHOOSE OF btnRestoreDefaults IN FRAME F-Main /* Defaults */
DO:
    RUN pGetSettings ("_default").
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


&Scoped-define SELF-NAME filterReport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL filterReport s-object
ON VALUE-CHANGED OF filterReport IN FRAME filterFrame
DO:
    ASSIGN
        {&SELF-NAME}
        cPrgmTitle = {&SELF-NAME}
        .
    IF cPrgmTitle EQ "<All>" THEN
    cPrgmTitle = "".
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

{methods/sortByProc.i "pBymnemonic" "ttDynParamValue.mnemonic"}
{methods/sortByProc.i "pByPrgTitle" "ttDynParamValue.prgtitle"}
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
  RUN spGetTaskFilter (OUTPUT cMnemonic, OUTPUT cPrgmName, OUTPUT cUserID).
  DO WITH FRAME filterFrame:
    ASSIGN
      filterReport:SENSITIVE   = YES
      filterModule:SENSITIVE   = YES
      filterUser:SENSITIVE     = YES
      filterPrgmName:SENSITIVE = YES
      .
  END. /* with frame */
  {methods/run_link.i "CONTAINER" "pGetCompany" "(OUTPUT cCompany)"}
  RUN pGetParamValue.
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
    DEFINE BUFFER bDynParamValue FOR dynParamValue.
    
    FOR EACH prgrms NO-LOCK
        WHERE prgrms.mnemonic BEGINS cMnemonic
           OR cMnemonic EQ "ALL",
        EACH dynParamValue NO-LOCK
        WHERE dynParamValue.prgmName EQ prgrms.prgmname
        WITH FRAME filterFrame:
        CREATE ttDynParamValue.
        ASSIGN
            ttDynParamValue.mnemonic     = prgrms.mnemonic
            ttDynParamValue.paramDescription = dynParamValue.paramDescription
            ttDynParamValue.prgmName     = dynParamValue.prgmName
            ttDynParamValue.prgTitle     = prgrms.prgtitle
            ttDynParamValue.module       = prgrms.module
            ttDynParamValue.user-id      = dynParamValue.user-id
            ttDynParamValue.paramValueID = dynParamValue.paramValueID
            ttDynParamValue.allData      = ttDynParamValue.mnemonic + "|"
                                         + ttDynParamValue.paramDescription + "|"
                                         + ttDynParamValue.prgmName + "|"
                                         + ttDynParamValue.prgtitle + "|"
                                         + ttDynParamValue.module + "|"
                                         + ttDynParamValue.user-id + "|"
                                         + STRING(ttDynParamValue.paramValueID)
                                         .
        IF NOT CAN-DO(filterReport:LIST-ITEMS,ttDynParamValue.prgTitle) THEN
        filterReport:ADD-LAST(ttDynParamValue.prgTitle).
        IF NOT CAN-DO(filterModule:LIST-ITEMS,ttDynParamValue.module) THEN
        filterModule:ADD-LAST(ttDynParamValue.module).
        IF NOT CAN-DO(filterUser:LIST-ITEMS,ttDynParamValue.user-id) THEN
        filterUser:ADD-LAST(ttDynParamValue.user-id).
        IF NOT CAN-DO(filterPrgmName:LIST-ITEMS,ttDynParamValue.prgmName) THEN
        filterPrgmName:ADD-LAST(ttDynParamValue.PrgmName).
    END. /* each dynParamValue */
    ASSIGN
        filterReport:SCREEN-VALUE   = "<All>"
        filterModule:SCREEN-VALUE   = "<All>"
        filterUser:SCREEN-VALUE     = "<All>"
        filterPrgmName:SCREEN-VALUE = "<All>"
        .

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
        WHEN "prgtitle" THEN
        RUN pByPrgTitle.
        WHEN "taskID" THEN
        RUN pByTaskID.
        WHEN "user-id" THEN
        RUN pByUserID.
        OTHERWISE
        {&OPEN-QUERY-{&BROWSE-NAME}}
    END CASE.

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

