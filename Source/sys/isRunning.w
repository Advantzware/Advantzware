&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: isRunning.w

  Description: Access Currently Running Procedures/Functions

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 9.9.2021

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cProcs       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgressBar AS CHARACTER NO-UNDO INITIAL "Procedures".
DEFINE VARIABLE lProgressBar AS LOGICAL   NO-UNDO INITIAL YES.

{methods/defines/sortByDefs.i}
DEFINE TEMP-TABLE ttRunning NO-UNDO
    FIELD procHandle AS CHARACTER FORMAT "x(10)" LABEL "Handle"
    FIELD procName   AS CHARACTER FORMAT "x(60)" LABEL "Name"
    FIELD calledFrom AS CHARACTER FORMAT "x(60)" LABEL "Called From"
    FIELD procType   AS CHARACTER FORMAT "x(8)"  LABEL "Type"
        INDEX ttRunning IS PRIMARY procHandle procName
        .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME procsBrowse

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttRunning

/* Definitions for BROWSE procsBrowse                                   */
&Scoped-define FIELDS-IN-QUERY-procsBrowse ttRunning.procHandle ttRunning.procName ttRunning.calledFrom ttRunning.procType   
&Scoped-define ENABLED-FIELDS-IN-QUERY-procsBrowse   
&Scoped-define SELF-NAME procsBrowse
&Scoped-define QUERY-STRING-procsBrowse FOR EACH ttRunning WHERE (procedureFile EQ "<All>" OR ttRunning.procName EQ procedureFile) AND ((matchesValue EQ NO AND (ttRunning.procName BEGINS searchValue OR ttRunning.calledFrom BEGINS searchValue)) OR (matchesValue EQ YES AND (ttRunning.procName MATCHES "*" + searchValue + "*" OR ttRunning.calledFrom MATCHES "*" + searchValue + "*")))  ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-procsBrowse OPEN QUERY {&SELF-NAME} FOR EACH ttRunning WHERE (procedureFile EQ "<All>" OR ttRunning.procName EQ procedureFile) AND ((matchesValue EQ NO AND (ttRunning.procName BEGINS searchValue OR ttRunning.calledFrom BEGINS searchValue)) OR (matchesValue EQ YES AND (ttRunning.procName MATCHES "*" + searchValue + "*" OR ttRunning.calledFrom MATCHES "*" + searchValue + "*")))  ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-procsBrowse ttRunning
&Scoped-define FIRST-TABLE-IN-QUERY-procsBrowse ttRunning


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-procsBrowse}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnDelete btnRefresh procedureFile ~
searchValue matchesValue procsBrowse 
&Scoped-Define DISPLAYED-OBJECTS procedureFile searchValue matchesValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RECT-TABLE searchValue matchesValue 
&Scoped-define List-2 RECT-TABLE searchValue matchesValue 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "Graphics/16x16/delete.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Delete Orphans" 
     SIZE 4 BY .95 TOOLTIP "Delete Orphans".

DEFINE BUTTON btnRefresh 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Refresh" 
     SIZE 4 BY .95 TOOLTIP "Refresh".

DEFINE VARIABLE procedureFile AS CHARACTER FORMAT "X(256)":U INITIAL "<All>" 
     LABEL "Procedure File" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS "<All>" 
     DROP-DOWN-LIST
     SIZE 54 BY 1 NO-UNDO.

DEFINE VARIABLE searchValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 58 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-TABLE
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 160 BY 1.43.

DEFINE VARIABLE matchesValue AS LOGICAL INITIAL yes 
     LABEL "Matches" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY procsBrowse FOR 
      ttRunning SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE procsBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS procsBrowse C-Win _FREEFORM
  QUERY procsBrowse DISPLAY
      ttRunning.procHandle LABEL-BGCOLOR 14
ttRunning.procName LABEL-BGCOLOR 14
ttRunning.calledFrom LABEL-BGCOLOR 14
ttRunning.procType LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 160 BY 27.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnDelete AT ROW 1.24 COL 77 WIDGET-ID 68
     btnRefresh AT ROW 1.24 COL 72 WIDGET-ID 66
     procedureFile AT ROW 1.24 COL 15 COLON-ALIGNED WIDGET-ID 64
     searchValue AT ROW 1.24 COL 82 WIDGET-ID 62
     matchesValue AT ROW 1.24 COL 149 HELP
          "Select for Table Search Matches" WIDGET-ID 40
     procsBrowse AT ROW 2.43 COL 1 WIDGET-ID 200
     RECT-TABLE AT ROW 1 COL 1 WIDGET-ID 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.62
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 100.


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
         TITLE              = "Running Super/Session Procedures"
         HEIGHT             = 28.57
         WIDTH              = 160
         MAX-HEIGHT         = 320
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 320
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* BROWSE-TAB procsBrowse matchesValue DEFAULT-FRAME */
/* SETTINGS FOR TOGGLE-BOX matchesValue IN FRAME DEFAULT-FRAME
   1 2                                                                  */
ASSIGN 
       matchesValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       procsBrowse:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR RECTANGLE RECT-TABLE IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2                                                        */
ASSIGN 
       RECT-TABLE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN searchValue IN FRAME DEFAULT-FRAME
   ALIGN-L 1 2                                                          */
ASSIGN 
       searchValue:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE procsBrowse
/* Query rebuild information for BROWSE procsBrowse
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttRunning
WHERE (procedureFile EQ "<All>"
OR ttRunning.procName EQ procedureFile)
AND ((matchesValue EQ NO
AND (ttRunning.procName BEGINS searchValue
OR ttRunning.calledFrom BEGINS searchValue))
OR (matchesValue EQ YES
AND (ttRunning.procName MATCHES "*" + searchValue + "*"
OR ttRunning.calledFrom MATCHES "*" + searchValue + "*")))
 ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE procsBrowse */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Running Super/Session Procedures */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Running Super/Session Procedures */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Running Super/Session Procedures */
DO:
    RUN pWinResize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete C-Win
ON CHOOSE OF btnDelete IN FRAME DEFAULT-FRAME /* Delete Orphans */
DO:
    RUN pDeleteOrphans.
    RUN pRunning.
    cColumnLabel = "procHandle".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRefresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRefresh C-Win
ON CHOOSE OF btnRefresh IN FRAME DEFAULT-FRAME /* Refresh */
DO:
    RUN pRunning.
    cColumnLabel = "procHandle".
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME matchesValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL matchesValue C-Win
ON VALUE-CHANGED OF matchesValue IN FRAME DEFAULT-FRAME /* Matches */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME procedureFile
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL procedureFile C-Win
ON VALUE-CHANGED OF procedureFile IN FRAME DEFAULT-FRAME /* Procedure File */
DO:
    ASSIGN {&SELF-NAME}.
    RUN pReopenBrowse.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME procsBrowse
&Scoped-define SELF-NAME procsBrowse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL procsBrowse C-Win
ON START-SEARCH OF procsBrowse IN FRAME DEFAULT-FRAME
DO:
    {AOA/includes/startSearch.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchValue C-Win
ON VALUE-CHANGED OF searchValue IN FRAME DEFAULT-FRAME /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

{methods/template/brwcustom2.i}

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

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
  RUN pRunning.
  RUN pWinResize.
  RUN enable_UI.
  cColumnLabel = "procHandle".
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pBycalledFrom" "ttRunning.calledFrom"}
{methods/sortByProc.i "pByProcHandle" "ttRunning.procHandle"}
{methods/sortByProc.i "pByProcName" "ttRunning.procName"}
{methods/sortByProc.i "pByProcType" "ttRunning.procType"}

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
  DISPLAY procedureFile searchValue matchesValue 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnDelete btnRefresh procedureFile searchValue matchesValue 
         procsBrowse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteOrphans C-Win 
PROCEDURE pDeleteOrphans :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.

    MESSAGE 
        "Delete Orphaned Procedures?"
    VIEW-AS ALERT-BOX QUESTION
    UPDATE lDelete AS LOGICAL.
    IF lDelete THEN
    FOR EACH ttRunning:
        IF ttRunning.calledFrom NE "?" THEN
        NEXT.
        pHandle = WIDGET-HANDLE(ttRunning.procHandle).
        IF VALID-HANDLE(pHandle) THEN
        DELETE PROCEDURE pHandle.
    END. /* each ttrunning */

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
        WHEN "calledFrom" THEN
        RUN pByCalledFrom.
        WHEN "procHandle" THEN
        RUN pByProcHandle.
        WHEN "procName" THEN
        RUN pByProcName.
        WHEN "procType" THEN
        RUN pByProcType.
    END CASE.
    {AOA/includes/pReopenBrowse.i}
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunning C-Win 
PROCEDURE pRunning :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cProcsValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hCalledFrom     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hSuperProcedure AS HANDLE    NO-UNDO.
    DEFINE VARIABLE pHandle         AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx             AS INTEGER   NO-UNDO.
    
    cProcs = "".
    EMPTY TEMP-TABLE ttRunning.
    DO idx = 1 TO NUM-ENTRIES(SESSION:SUPER-PROCEDURES):
        CREATE ttRunning.
        ASSIGN
            hSuperProcedure      = HANDLE(ENTRY(idx,SESSION:SUPER-PROCEDURES))
            hCalledFrom          = hSuperProcedure:INSTANTIATING-PROCEDURE
            ttRunning.procHandle = STRING(INTEGER(ENTRY(idx,SESSION:SUPER-PROCEDURES)),">>>>>>>9")
            ttRunning.procName   = hSuperProcedure:NAME
            ttRunning.calledFrom = IF VALID-HANDLE(hCalledFrom) THEN hCalledFrom:NAME
                                   ELSE "?"
            ttRunning.procType   = "Super"
            .
        IF LOOKUP(ttRunning.procName,cProcs) EQ 0 THEN
        cProcs = cProcs + ttRunning.procName + ",".
    END. /* do idx */
    pHandle = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(pHandle):
        IF NOT CAN-FIND(FIRST ttRunning
                        WHERE ttRunning.procHandle EQ STRING(pHandle,">>>>>>>9")) THEN DO:
            CREATE ttRunning.
            ASSIGN
                hCalledFrom          = pHandle:INSTANTIATING-PROCEDURE
                ttRunning.procHandle = STRING(pHandle,">>>>>>>9")
                ttRunning.procName   = pHandle:NAME
                ttRunning.calledFrom = IF VALID-HANDLE(hCalledFrom) THEN hCalledFrom:NAME
                                       ELSE "?"
                ttRunning.procType   = "Session"
                .
            IF LOOKUP(ttRunning.procName,cProcs) EQ 0 THEN
            cProcs = cProcs + ttRunning.procName + ",".
        END. /* if not */
        pHandle = pHandle:NEXT-SIBLING.
    END. /* do while */
    ASSIGN
        cProcsValue                = procedureFile:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        cProcs                     = TRIM(cProcs,",")
        procedureFile:LIST-ITEMS   = "<All>," + cProcs
        procedureFile:INNER-LINES  = procedureFile:NUM-ITEMS
        procedureFile:SCREEN-VALUE = IF cProcsValue NE "" AND LOOKUP(cProcsValue,procedureFile:LIST-ITEMS) NE 0 THEN cProcsValue
                                     ELSE procedureFile:ENTRY(1)
        procedureFile
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinResize C-Win 
PROCEDURE pWinResize :
/*------------------------------------------------------------------------------
 Purpose:
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
                                               - BROWSE {&BROWSE-NAME}:ROW
            BROWSE {&BROWSE-NAME}:WIDTH        = FRAME {&FRAME-NAME}:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

