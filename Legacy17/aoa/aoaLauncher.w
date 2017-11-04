&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: aoa/aoaLauncher.w

  Description: AOA Launcher

  Input Parameters: Launch Type (Dashboard/Report)

  Output Parameters: <none>

  Author: Ron Stark

  Created: 5.16.2016

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

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipcLaunchType AS CHARACTER NO-UNDO.
&ELSE
DEFINE VARIABLE ipcLaunchType AS CHARACTER NO-UNDO INITIAL "Report".
&ENDIF

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cColumnLabel       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSaveLabel         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAscending         AS LOGICAL   NO-UNDO INITIAL YES.

DEFINE TEMP-TABLE ttModule NO-UNDO
    FIELD module   AS CHARACTER LABEL "Module"      FORMAT "x(5)"
    FIELD modDescr AS CHARACTER LABEL "Description" FORMAT "x(25)"
        INDEX module IS PRIMARY UNIQUE module.

DEFINE TEMP-TABLE ttAOA NO-UNDO
    FIELD module  AS CHARACTER LABEL "Module"   FORMAT "x(5)"
    FIELD aoaFile AS CHARACTER LABEL "AOA File" FORMAT "x(40)"
    FIELD progID  AS CHARACTER LABEL "Prog ID"  FORMAT "x(10)"
    FIELD menuID  AS CHARACTER LABEL "Menu ID"  FORMAT "x(4)"
        INDEX aoa IS PRIMARY UNIQUE aoaFile.

RUN util/chk-mod.p ("ASI",
                    IF ipcLaunchType EQ "Report" THEN "aoaReport"
                    ELSE "aoaDashboard") NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME browseAOA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAOA ttModule

/* Definitions for BROWSE browseAOA                                     */
&Scoped-define FIELDS-IN-QUERY-browseAOA ttAOA.aoaFile ttAOA.progID ttAOA.menuID ttAOA.module   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseAOA   
&Scoped-define SELF-NAME browseAOA
&Scoped-define QUERY-STRING-browseAOA FOR EACH ttAOA WHERE ttModule.module EQ "*" OR ttAOA.module EQ ttModule.module ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-browseAOA OPEN QUERY {&SELF-NAME} FOR EACH ttAOA WHERE ttModule.module EQ "*" OR ttAOA.module EQ ttModule.module ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-browseAOA ttAOA
&Scoped-define FIRST-TABLE-IN-QUERY-browseAOA ttAOA


/* Definitions for BROWSE browseModule                                  */
&Scoped-define FIELDS-IN-QUERY-browseModule ttModule.module ttModule.modDescr   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseModule   
&Scoped-define SELF-NAME browseModule
&Scoped-define QUERY-STRING-browseModule FOR EACH ttModule
&Scoped-define OPEN-QUERY-browseModule OPEN QUERY {&SELF-NAME} FOR EACH ttModule.
&Scoped-define TABLES-IN-QUERY-browseModule ttModule
&Scoped-define FIRST-TABLE-IN-QUERY-browseModule ttModule


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-browseAOA}~
    ~{&OPEN-QUERY-browseModule}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browseModule browseAOA btnClose btnLaunch 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClose 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Close" 
     SIZE 8 BY 1.67.

DEFINE BUTTON btnLaunch 
     IMAGE-UP FILE "Graphics/32x32/rocket.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Launch" 
     SIZE 8 BY 1.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseAOA FOR 
      ttAOA SCROLLING.

DEFINE QUERY browseModule FOR 
      ttModule SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseAOA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseAOA C-Win _FREEFORM
  QUERY browseAOA DISPLAY
      ttAOA.aoaFile LABEL-BGCOLOR 14
    ttAOA.progID LABEL-BGCOLOR 14
    ttAOA.menuID LABEL-BGCOLOR 14
    ttAOA.module LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 73 BY 25.24
         TITLE "AOA Files".

DEFINE BROWSE browseModule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseModule C-Win _FREEFORM
  QUERY browseModule DISPLAY
      ttModule.module
    ttModule.modDescr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 38 BY 25.24
         TITLE "Modules".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     browseModule AT ROW 1.24 COL 2 WIDGET-ID 200
     browseAOA AT ROW 1.24 COL 41 WIDGET-ID 300
     btnClose AT ROW 26.71 COL 106 WIDGET-ID 2
     btnLaunch AT ROW 26.71 COL 41 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 27.57 WIDGET-ID 100.


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
         TITLE              = "AOA Launcher"
         HEIGHT             = 27.57
         WIDTH              = 114
         MAX-HEIGHT         = 27.57
         MAX-WIDTH          = 114
         VIRTUAL-HEIGHT     = 27.57
         VIRTUAL-WIDTH      = 114
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB browseModule 1 DEFAULT-FRAME */
/* BROWSE-TAB browseAOA browseModule DEFAULT-FRAME */
ASSIGN 
       browseAOA:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       btnClose:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "WinKitRibbon".

ASSIGN 
       btnLaunch:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "WinKitRibbon".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseAOA
/* Query rebuild information for BROWSE browseAOA
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAOA WHERE ttModule.module EQ "*" OR ttAOA.module EQ ttModule.module ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseAOA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseModule
/* Query rebuild information for BROWSE browseModule
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttModule.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseModule */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* AOA Launcher */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* AOA Launcher */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseAOA
&Scoped-define SELF-NAME browseAOA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseAOA C-Win
ON DEFAULT-ACTION OF browseAOA IN FRAME DEFAULT-FRAME /* AOA Files */
DO:
    APPLY "CHOOSE":U TO btnLaunch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseAOA C-Win
ON START-SEARCH OF browseAOA IN FRAME DEFAULT-FRAME /* AOA Files */
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


&Scoped-define BROWSE-NAME browseModule
&Scoped-define SELF-NAME browseModule
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseModule C-Win
ON VALUE-CHANGED OF browseModule IN FRAME DEFAULT-FRAME /* Modules */
DO:
  {&OPEN-QUERY-browseAOA}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClose
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClose C-Win
ON CHOOSE OF btnClose IN FRAME DEFAULT-FRAME /* Close */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLaunch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLaunch C-Win
ON CHOOSE OF btnLaunch IN FRAME DEFAULT-FRAME /* Launch */
DO:
  RUN VALUE("aoa/" + ttAOA.progID + "p").
  RETURN NO-APPLY.
    {Advantzware/WinKit/winkit-panel-triggerend.i} /* added by script _nonAdm1.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseAOA
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i} /* added by script _nonAdm1.p */
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {&WINDOW-NAME}:TITLE = "AOA " + ipcLaunchType + " Launcher".
  RUN enable_UI.
  RUN pGetAOAFiles (ipcLaunchType).
    {Advantzware/WinKit/embedfinalize-nonadm.i} /* added by script _nonAdm1.p */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
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
  ENABLE browseModule browseAOA btnClose btnLaunch 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByFile C-Win 
PROCEDURE pByFile :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.aoaFile
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.aoaFile DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByMenuID C-Win 
PROCEDURE pByMenuID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.menuID
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.menuID DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByModule C-Win 
PROCEDURE pByModule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.module
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.module DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pByProgID C-Win 
PROCEDURE pByProgID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF lAscending THEN
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.progID
    {&OPEN-QUERY-{&BROWSE-NAME}}
    ELSE
    &SCOPED-DEFINE SORTBY-PHRASE BY ttAOA.progID DESCENDING
    {&OPEN-QUERY-{&BROWSE-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetAOAFiles C-Win 
PROCEDURE pGetAOAFiles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcLaunchDat AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cModule   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cModDescr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAOAFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProgID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMenuID   AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = "aoa/datFiles/" + ipcLaunchDat + ".dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT cModule cAOAFile cProgID cMenuID.
        CREATE ttAOA.
        ASSIGN
            ttAOA.module  = cModule
            ttAOA.aoaFile = cAOAFile
            ttAOA.progID  = cProgID
            ttAOA.menuID  = cMenuID
            .
    END. /* repeat */
    INPUT CLOSE.

    FILE-INFO:FILE-NAME = "aoa/datFiles/Module.dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT cModule cModDescr.
        IF NOT CAN-FIND(FIRST ttAOA WHERE ttAOA.module EQ cModule)
           AND cModule NE "*" THEN NEXT.
        CREATE ttModule.
        ASSIGN
            ttModule.module   = cModule
            ttModule.modDescr = cModDescr
            .
    END. /* repeat */
    INPUT CLOSE.

    {&OPEN-QUERY-browseModule}
    APPLY "VALUE-CHANGED":U TO BROWSE browseModule.
    {&OPEN-QUERY-browseALL}

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
    CASE cColumnLabel:
        WHEN "aoaFile" THEN
        RUN pByFile.
        WHEN "progID" THEN
        RUN pByProgID.
        WHEN "menuID" THEN
        RUN pByMenuID.
        WHEN "module" THEN
        RUN pByModule.
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

