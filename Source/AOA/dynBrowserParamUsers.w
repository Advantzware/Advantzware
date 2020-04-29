&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: dynBrowserParamUsers.w

  Description: Dynamic Browser Parameter Users

  Input Parameters: Recipient List

  Output Parameters: Recipient List

  Author: Ron Stark

  Created: 4.8.2020
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipiSubjectID AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ipcPrgmName  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID     AS ROWID     NO-UNDO.
&ELSE
DEFINE VARIABLE ipiSubjectID AS INTEGER   NO-UNDO INITIAL 59.
DEFINE VARIABLE ipcPrgmName  AS CHARACTER NO-UNDO INITIAL "[mach]".
DEFINE VARIABLE iprRowID     AS ROWID     NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE rDynParamValueRowID AS ROWID NO-UNDO.

{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttUser NO-UNDO
    FIELD isSelected      AS LOGICAL LABEL "Sel"
    FIELD cUserID2      LIKE users.user_id
    FIELD cUserName2    LIKE users.user_name
    FIELD lShowParameters AS LOGICAL LABEL "Show"
    FIELD allData         AS CHARACTER
        INDEX ttUser IS PRIMARY
            cUserID2
            .
DEFINE TEMP-TABLE ttParamUser NO-UNDO
    FIELD cUserID1      LIKE users.user_id
    FIELD cUserName1    LIKE users.user_name
    FIELD lShowParameters AS LOGICAL LABEL "Show"
    FIELD lDelete         AS LOGICAL LABEL "Delete"
    FIELD cPrgmName       AS CHARACTER
    FIELD rRowID          AS ROWID
    FIELD allData         AS CHARACTER
        INDEX ttParamUser IS PRIMARY
            cUserID1
            .
DEFINE TEMP-TABLE ttParamValues NO-UNDO
    FIELD sortOrder  AS INTEGER
    FIELD paramName  AS CHARACTER FORMAT "x(30)" LABEL "Parameter Name"
    FIELD paramLabel AS CHARACTER FORMAT "x(30)" LABEL "Parameter Label"
    FIELD paramValue AS CHARACTER FORMAT "x(30)" LABEL "Parameter Value"
        INDEX ttParamValues IS PRIMARY
            sortOrder
            .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME paramUserBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttParamUser ttParamValues ttUser

/* Definitions for BROWSE paramUserBrowse                               */
&Scoped-define FIELDS-IN-QUERY-paramUserBrowse ttParamUser.cUserID1 ttParamUser.cUserName1 ttParamUser.lShowParameters ttParamUser.lDelete   
&Scoped-define ENABLED-FIELDS-IN-QUERY-paramUserBrowse ttParamUser.lShowParameters ttParamUser.lDelete   
&Scoped-define ENABLED-TABLES-IN-QUERY-paramUserBrowse ttParamUser
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-paramUserBrowse ttParamUser
&Scoped-define SELF-NAME paramUserBrowse
&Scoped-define QUERY-STRING-paramUserBrowse FOR EACH ttParamUser WHERE ttParamUser.allData MATCHES "*" + searchBar1 + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-paramUserBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttParamUser WHERE ttParamUser.allData MATCHES "*" + searchBar1 + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-paramUserBrowse ttParamUser
&Scoped-define FIRST-TABLE-IN-QUERY-paramUserBrowse ttParamUser


/* Definitions for BROWSE paramValueBrowse                              */
&Scoped-define FIELDS-IN-QUERY-paramValueBrowse ttParamValues.paramName ttParamValues.paramLabel ttParamValues.paramValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-paramValueBrowse   
&Scoped-define SELF-NAME paramValueBrowse
&Scoped-define QUERY-STRING-paramValueBrowse FOR EACH ttParamValues
&Scoped-define OPEN-QUERY-paramValueBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttParamValues.
&Scoped-define TABLES-IN-QUERY-paramValueBrowse ttParamValues
&Scoped-define FIRST-TABLE-IN-QUERY-paramValueBrowse ttParamValues


/* Definitions for BROWSE userBrowse                                    */
&Scoped-define FIELDS-IN-QUERY-userBrowse ttUser.isSelected ttUser.cUserID2 ttUser.cUserName2 ttUser.lShowParameters   
&Scoped-define ENABLED-FIELDS-IN-QUERY-userBrowse ttUser.isSelected ttUser.lShowParameters   
&Scoped-define ENABLED-TABLES-IN-QUERY-userBrowse ttUser
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-userBrowse ttUser
&Scoped-define SELF-NAME userBrowse
&Scoped-define QUERY-STRING-userBrowse FOR EACH ttUser WHERE ttUser.allData MATCHES "*" + searchBar2 + "*"  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-userBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttUser WHERE ttUser.allData MATCHES "*" + searchBar2 + "*"  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-userBrowse ttUser
&Scoped-define FIRST-TABLE-IN-QUERY-userBrowse ttUser


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-paramUserBrowse}~
    ~{&OPEN-QUERY-paramValueBrowse}~
    ~{&OPEN-QUERY-userBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClose searchBar2 btnSave searchBar1 ~
userBrowse paramValueBrowse paramUserBrowse lIsSelectedAll lShowParamAll2 ~
lShowParamAll1 lDeleteAll 
&Scoped-Define DISPLAYED-OBJECTS searchBar2 searchBar1 lIsSelectedAll ~
lShowParamAll2 lShowParamAll1 lDeleteAll 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnClose 
&Scoped-define List-3 btnClose 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Close" 
     SIZE 8 BY 1.91 TOOLTIP "Close".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     BGCOLOR 8 .

DEFINE VARIABLE searchBar1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE VARIABLE searchBar2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19 BY 2.38.

DEFINE VARIABLE lDeleteAll AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE lIsSelectedAll AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE lShowParamAll1 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

DEFINE VARIABLE lShowParamAll2 AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 3 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY paramUserBrowse FOR 
      ttParamUser SCROLLING.

DEFINE QUERY paramValueBrowse FOR 
      ttParamValues SCROLLING.

DEFINE QUERY userBrowse FOR 
      ttUser SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE paramUserBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS paramUserBrowse Dialog-Frame _FREEFORM
  QUERY paramUserBrowse DISPLAY
      ttParamUser.cUserID1 LABEL-BGCOLOR 14 LABEL "User ID"
ttParamUser.cUserName1 LABEL-BGCOLOR 14
ttParamUser.lShowParameters VIEW-AS TOGGLE-BOX
ttParamUser.lDelete VIEW-AS TOGGLE-BOX
ENABLE
ttParamUser.lShowParameters
ttParamUser.lDelete
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-SCROLLBAR-VERTICAL SIZE 53 BY 27.86
         TITLE "Users with Parameters".

DEFINE BROWSE paramValueBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS paramValueBrowse Dialog-Frame _FREEFORM
  QUERY paramValueBrowse DISPLAY
      ttParamValues.paramName
      ttParamValues.paramLabel
      ttParamValues.paramValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 97 BY 25.24
         TITLE "Parameter Values".

DEFINE BROWSE userBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS userBrowse Dialog-Frame _FREEFORM
  QUERY userBrowse DISPLAY
      ttUser.isSelected VIEW-AS TOGGLE-BOX
ttUser.cUserID2 LABEL-BGCOLOR 14 LABEL "User ID"
ttUser.cUserName2 LABEL-BGCOLOR 14
ttUser.lShowParameters VIEW-AS TOGGLE-BOX
ENABLE
ttUser.isSelected
ttUser.lShowParameters
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 27.86
         TITLE "Users".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnClose AT ROW 27.67 COL 142 HELP
          "Close" WIDGET-ID 28
     searchBar2 AT ROW 1 COL 8 COLON-ALIGNED HELP
          "Search" WIDGET-ID 30
     btnSave AT ROW 27.67 COL 133 HELP
          "Save"
     searchBar1 AT ROW 1 COL 158 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     userBrowse AT ROW 1.95 COL 1 WIDGET-ID 400
     paramValueBrowse AT ROW 1.95 COL 55 WIDGET-ID 300
     paramUserBrowse AT ROW 1.95 COL 152 WIDGET-ID 200
     lIsSelectedAll AT ROW 2 COL 2 WIDGET-ID 32
     lShowParamAll2 AT ROW 2 COL 47 WIDGET-ID 34
     lShowParamAll1 AT ROW 2 COL 194 WIDGET-ID 36
     lDeleteAll AT ROW 2 COL 199.6 WIDGET-ID 38
     RECT-1 AT ROW 27.43 COL 132 WIDGET-ID 2
     SPACE(54.00) SKIP(0.00)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Browser Parameter Users"
         CANCEL-BUTTON btnClose WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB userBrowse RECT-1 Dialog-Frame */
/* BROWSE-TAB paramValueBrowse userBrowse Dialog-Frame */
/* BROWSE-TAB paramUserBrowse paramValueBrowse Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnClose IN FRAME Dialog-Frame
   1 3                                                                  */
ASSIGN 
       paramUserBrowse:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       userBrowse:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE paramUserBrowse
/* Query rebuild information for BROWSE paramUserBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamUser
WHERE ttParamUser.allData MATCHES "*" + searchBar1 + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE paramUserBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE paramValueBrowse
/* Query rebuild information for BROWSE paramValueBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamValues.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE paramValueBrowse */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE userBrowse
/* Query rebuild information for BROWSE userBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUser
WHERE ttUser.allData MATCHES "*" + searchBar2 + "*"
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE userBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Browser Parameter Users */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    RUN pSave.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lDeleteAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lDeleteAll Dialog-Frame
ON VALUE-CHANGED OF lDeleteAll IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttParamUser
        WHERE ttParamUser.allData MATCHES "*" + searchBar1 + "*"
        :
        ttParamUser.lDelete = {&SELF-NAME}.
    END. /* each ttusers */
    BROWSE paramUserBrowse:REFRESH().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lIsSelectedAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lIsSelectedAll Dialog-Frame
ON VALUE-CHANGED OF lIsSelectedAll IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttUser
        WHERE ttUser.allData MATCHES "*" + searchBar2 + "*"
        :
        ttUser.isSelected = {&SELF-NAME}.
    END. /* each ttusers */
    BROWSE userBrowse:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lShowParamAll1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lShowParamAll1 Dialog-Frame
ON VALUE-CHANGED OF lShowParamAll1 IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttParamUser
        WHERE ttParamUser.allData MATCHES "*" + searchBar1 + "*"
        :
        ttParamUser.lShowParameters = {&SELF-NAME}.
    END. /* each ttusers */
    BROWSE paramUserBrowse:REFRESH().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lShowParamAll2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lShowParamAll2 Dialog-Frame
ON VALUE-CHANGED OF lShowParamAll2 IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    FOR EACH ttUser
        WHERE ttUser.allData MATCHES "*" + searchBar2 + "*"
        :
        ttUser.lShowParameters = {&SELF-NAME}.
    END. /* each ttusers */
    BROWSE userBrowse:REFRESH().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME paramUserBrowse
&Scoped-define SELF-NAME paramUserBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramUserBrowse Dialog-Frame
ON START-SEARCH OF paramUserBrowse IN FRAME Dialog-Frame /* Users with Parameters */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE paramUserBrowse:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL paramUserBrowse Dialog-Frame
ON VALUE-CHANGED OF paramUserBrowse IN FRAME Dialog-Frame /* Users with Parameters */
DO:
    RUN pGetParamValues.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar1 Dialog-Frame
ON VALUE-CHANGED OF searchBar1 IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-paramUserBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar2 Dialog-Frame
ON VALUE-CHANGED OF searchBar2 IN FRAME Dialog-Frame /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-userBrowse}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME userBrowse
&Scoped-define SELF-NAME userBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL userBrowse Dialog-Frame
ON START-SEARCH OF userBrowse IN FRAME Dialog-Frame /* Users */
DO:
    IF SELF:CURRENT-COLUMN:NAME NE ? THEN DO:
        cColumnLabel = BROWSE userBrowse:CURRENT-COLUMN:NAME.
        IF cColumnLabel EQ cSaveLabel THEN
        lAscending = NOT lAscending.
        cSaveLabel = cColumnLabel.
        RUN pReopenBrowse.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME paramUserBrowse
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetUsers.
  RUN enable_UI.
  lIsSelectedAll:MOVE-TO-TOP().
  lShowParamAll1:MOVE-TO-TOP().
  lShowParamAll2:MOVE-TO-TOP().
  lDeleteAll:MOVE-TO-TOP().
  APPLY "VALUE-CHANGED":U TO paramUserBrowse.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&Scoped-define sdBrowseName paramUserBrowse
{methods/sortByProc.i "pByUserID1" "ttParamUser.cUserID1"}
{methods/sortByProc.i "pByUserName1" "ttParamUser.cUserName1"}

&Scoped-define sdBrowseName userBrowse
{methods/sortByProc.i "pByUserID2" "ttUser.cUserID2"}
{methods/sortByProc.i "pByUserName2" "ttUser.cUserName2"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY searchBar2 searchBar1 lIsSelectedAll lShowParamAll2 lShowParamAll1 
          lDeleteAll 
      WITH FRAME Dialog-Frame.
  ENABLE btnClose searchBar2 btnSave searchBar1 userBrowse paramValueBrowse 
         paramUserBrowse lIsSelectedAll lShowParamAll2 lShowParamAll1 
         lDeleteAll 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValues Dialog-Frame 
PROCEDURE pGetParamValues :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttParamValues.

    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ipiSubjectID
           AND dynParamValue.user-id      EQ ttParamUser.cUserID1
           AND dynParamValue.prgmName     EQ ipcPrgmName
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF NOT AVAILABLE dynParamValue THEN
    FIND FIRST dynParamValue NO-LOCK
         WHERE dynParamValue.subjectID    EQ ipiSubjectID
           AND dynParamValue.user-id      EQ "_default"
           AND dynParamValue.prgmName     EQ "dynSubjct."
           AND dynParamValue.paramValueID EQ 0
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN DO:
        rDynParamValueRowID = ROWID(dynParamValue).
        DO idx = 1 TO EXTENT(dynParamValue.paramName):
            IF dynParamValue.paramName[idx] EQ "" THEN LEAVE.
            IF dynParamValue.paramName[idx] BEGINS "sv" THEN NEXT.
            CREATE ttParamValues.
            ASSIGN
                ttParamValues.sortOrder  = idx
                ttParamValues.paramName  = dynParamValue.paramName[idx]
                ttParamValues.paramLabel = dynParamValue.paramLabel[idx]
                ttParamValues.paramValue = dynParamValue.paramValue[idx]
                .
        END. /* do idx */
    END. /* if avail */
    {&OPEN-QUERY-paramValueBrowse}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUsers Dialog-Frame 
PROCEDURE pGetUsers :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttParamUser.
    
    CREATE ttParamUser.
    ASSIGN
        ttParamUser.cUserID         = "_default"
        ttParamUser.cUserName       = "System Default"
        ttParamUser.lShowParameters = YES
        ttParamUser.cPrgmName       = "dynSubjct."
        ttParamUser.allData         = ttParamUser.cUserID1  + "|"
                                    + ttParamUser.cUserName1
                                    .
    FOR EACH users NO-LOCK
        :
        FIND FIRST dynParamValue NO-LOCK
             WHERE dynParamValue.subjectID    EQ ipiSubjectID
               AND dynParamValue.user-id      EQ users.user_id
               AND dynParamValue.prgmName     EQ ipcPrgmName
               AND dynParamValue.paramValueID EQ 0
             NO-ERROR.
        IF AVAILABLE dynParamValue THEN DO:
            CREATE ttParamUser.
            ASSIGN
                ttParamUser.cUserID1        = users.user_id
                ttParamUser.cUserName1      = users.user_name
                ttParamUser.lShowParameters = dynParamValue.showParameters
                ttParamUser.cPrgmName       = ipcPrgmName
                ttParamUser.rRowID          = ROWID(dynParamValue)
                ttParamUser.allData         = ttParamUser.cUserID1  + "|"
                                            + ttParamUser.cUserName1
                                            .
        END. /* if avail */
        CREATE ttUser.
        ASSIGN
            ttUser.cUserID2   = users.user_id
            ttUser.cUserName2 = users.user_name
            ttUser.allData    = ttUser.cUserID  + "|"
                              + ttUser.cUserName
                              .
    END. /* each users */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pReopenBrowse Dialog-Frame 
PROCEDURE pReopenBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    CASE cColumnLabel:
        WHEN "cUserID1" THEN
        RUN pByUserID1.
        WHEN "cUserName1" THEN
        RUN pByUserName1.
        WHEN "cUserID2" THEN
        RUN pByUserID2.
        WHEN "cUserName2" THEN
        RUN pByUserName2.
    END CASE.
    APPLY "VALUE-CHANGED":U TO BROWSE paramUserBrowse.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSave Dialog-Frame 
PROCEDURE pSave :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNewParamUser AS LOGICAL NO-UNDO. 

    DEFINE BUFFER bDynParamValue FOR dynParamValue.

    SESSION:SET-WAIT-STATE("General").
    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ rDynParamValueRowID
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN
    FOR EACH ttUser
        WHERE ttUser.isSelected EQ YES
          AND ttUser.cUserID2   NE dynParamValue.user-id
        :
        lNewParamUser = NO.
        FIND FIRST bDynParamValue EXCLUSIVE-LOCK
             WHERE bDynParamValue.subjectID    EQ dynParamValue.subjectID
               AND bDynParamValue.user-id      EQ ttUser.cUserID2
               AND bDynParamValue.prgmName     EQ dynParamValue.prgmName
               AND bDynParamValue.paramValueID EQ dynParamValue.paramValueID
             NO-ERROR.
        IF NOT AVAILABLE bDynParamValue THEN DO:
            lNewParamUser = YES.
            CREATE bDynParamValue.
        END. /* if not avail */
        BUFFER-COPY dynParamValue EXCEPT user-id rec_key TO bDynParamValue
            ASSIGN
                bDynParamValue.user-id        = ttUser.cUserID2
                bDynParamValue.showParameters = ttUser.lShowParameters
                .
        ASSIGN
            ttUser.isSelected      = NO
            ttUser.lShowParameters = NO
            .
        IF lNewParamUser THEN DO:
            CREATE ttParamUser.
            ASSIGN
                ttParamUser.cUserID1        = ttUser.cUserID2
                ttParamUser.cUserName1      = ttUser.cUserName2
                ttParamUser.lShowParameters = bDynParamValue.showParameters
                ttParamUser.cPrgmName       = bDynParamValue.prgmName
                ttParamUser.rRowID          = ROWID(bDynParamValue)
                ttParamUser.allData         = ttParamUser.cUserID1  + "|"
                                            + ttParamUser.cUserName1
                                            .
        END. /* if lNewParamUser */
    END. /* each ttuser */

    FOR EACH ttParamUser:
        /* can't delete default or current parameters */
        IF ttParamUser.rRowID EQ ? OR
           ttParamUser.rRowID EQ iprRowID THEN
        ASSIGN
            ttParamUser.lDelete         = NO
            ttParamUser.lShowParameters = YES
            .
        DO TRANSACTION:
            FIND FIRST dynParamValue EXCLUSIVE-LOCK
                 WHERE ROWID(dynParamValue) EQ ttParamUser.rRowID
                 NO-ERROR.
            IF NOT AVAILABLE dynParamValue THEN NEXT.
            IF ttParamUser.lDelete THEN DO:
                DELETE dynParamValue.
                DELETE ttParamUser.
            END. /* if ldelete */
            ELSE
            dynParamValue.showParameters = ttParamUser.lShowParameters.
        END. /* do trans */
    END. /* each ttparamuser */
    BROWSE userBrowse:REFRESH ().
    {&OPEN-QUERY-paramUserBrowse}
    APPLY "VALUE-CHANGED":U TO BROWSE paramUserBrowse.
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

