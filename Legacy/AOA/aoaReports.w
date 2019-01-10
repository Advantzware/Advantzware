&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/aoaReports.w

  Description: AOA Report Launcher

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 1.9.2019

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

&Scoped-define program-id aoaReports.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lSortMove AS LOGICAL NO-UNDO INITIAL YES.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{methods/defines/sortByDefs.i}

DEFINE TEMP-TABLE ttUserPrint NO-UNDO
    FIELD user-id    AS CHARACTER FORMAT "x(20)" LABEL "User ID"
    FIELD prog-title AS CHARACTER FORMAT "x(30)" LABEL "Parameter Name"
    FIELD program-id AS CHARACTER FORMAT "x(10)" LABEL "Program"
    FIELD prgtitle   AS CHARACTER FORMAT "x(30)" LABEL "Title"
    FIELD mnemonic   AS CHARACTER FORMAT "x(3)"  LABEL "HotKey"
    FIELD taskID     AS INTEGER   FORMAT ">>>>9" LABEL "Task ID"
    FIELD allData    AS CHARACTER
        INDEX mnemonic IS PRIMARY mnemonic
        .

{methods/lockWindowUpdate.i}

&IF DEFINED(module) EQ 0 &THEN
&Scoped-define module ALL
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME ttUserPrint

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttUserPrint

/* Definitions for BROWSE ttUserPrint                                   */
&Scoped-define FIELDS-IN-QUERY-ttUserPrint ttUserPrint.mnemonic ttUserPrint.prog-title ttUserPrint.program-id ttUserPrint.prgtitle ttUserPrint.user-id ttUserPrint.taskID   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttUserPrint   
&Scoped-define SELF-NAME ttUserPrint
&Scoped-define QUERY-STRING-ttUserPrint FOR EACH ttUserPrint WHERE ttUserPrint.allData MATCHES "*" + searchBar + "*" ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-ttUserPrint OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint WHERE ttUserPrint.allData MATCHES "*" + searchBar + "*" ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-ttUserPrint ttUserPrint
&Scoped-define FIRST-TABLE-IN-QUERY-ttUserPrint ttUserPrint


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-ttUserPrint}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnHTML searchBar ttUserPrint btnSortMove ~
btnView btnPDF btnDOCX btnCSV btnXLS btnRun btnRestoreDefaults 
&Scoped-Define DISPLAYED-OBJECTS searchBar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define List-2 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define List-3 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define List-4 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define List-5 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 
&Scoped-define List-6 RECT-JASPER btnHTML btnView btnPDF btnDOCX btnCSV ~
btnXLS 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "AOA/images/aoaexcelcsv.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 4.4 BY 1 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "AOA/images/aoaword.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Word DOCX".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "AOA/images/html_tag.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "HTML".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "AOA/images/aoapdf.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "PDF".

DEFINE BUTTON btnRestoreDefaults 
     IMAGE-UP FILE "Graphics/16x16/rename.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Defaults" 
     SIZE 4 BY .95 TOOLTIP "Restore Defaults".

DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/16x16/media_play.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Run" 
     SIZE 4 BY .95 TOOLTIP "Run Now".

DEFINE BUTTON btnSortMove 
     IMAGE-UP FILE "Graphics/16x16/sort_up_down2.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Sort/Move" 
     SIZE 4 BY .95 TOOLTIP "Toggle Sort/Move Columns".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "AOA/images/jrxml_icon.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "AOA/images/aoaexcel.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Excel XLS".

DEFINE VARIABLE searchBar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 98 BY 1 TOOLTIP "Search Bar" NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 1.

DEFINE RECTANGLE RECT-JASPER
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 31 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY ttUserPrint FOR 
      ttUserPrint SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE ttUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttUserPrint C-Win _FREEFORM
  QUERY ttUserPrint DISPLAY
      ttUserPrint.mnemonic LABEL-BGCOLOR 14
ttUserPrint.prog-title LABEL-BGCOLOR 14
ttUserPrint.program-id LABEL-BGCOLOR 14
ttUserPrint.prgtitle LABEL-BGCOLOR 14
ttUserPrint.user-id LABEL-BGCOLOR 14
ttUserPrint.taskID LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 140 BY 26.91
         TITLE "User Reports".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnHTML AT ROW 1.24 COL 150 HELP
          "HTML" WIDGET-ID 144
     searchBar AT ROW 1.24 COL 28 COLON-ALIGNED HELP
          "Search" WIDGET-ID 6
     ttUserPrint AT ROW 2.67 COL 21 WIDGET-ID 200
     btnSortMove AT ROW 1 COL 9 HELP
          "Toggle Sort/Move Columns" WIDGET-ID 48
     btnView AT ROW 1.24 COL 155 HELP
          "Jasper Viewer" WIDGET-ID 148
     btnPDF AT ROW 1.24 COL 145 HELP
          "PDF" WIDGET-ID 146
     btnDOCX AT ROW 1.24 COL 140 HELP
          "Word DOCX" WIDGET-ID 142
     btnCSV AT ROW 1.24 COL 130 HELP
          "Excel CSV" WIDGET-ID 140
     btnXLS AT ROW 1.24 COL 135 HELP
          "Excel XLS" WIDGET-ID 150
     btnRun AT ROW 1 COL 14 HELP
          "Run Now" WIDGET-ID 44
     btnRestoreDefaults AT ROW 1 COL 4 HELP
          "Restore Defaults" WIDGET-ID 42
     "Filters" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 2.43 COL 8 WIDGET-ID 70
     RECT-8 AT ROW 2.19 COL 2 WIDGET-ID 72
     RECT-JASPER AT ROW 1 COL 129 WIDGET-ID 152
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
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
         TITLE              = "User Reports"
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
/* BROWSE-TAB ttUserPrint searchBar DEFAULT-FRAME */
/* SETTINGS FOR BUTTON btnCSV IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnDOCX IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnHTML IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnPDF IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnView IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR BUTTON btnXLS IN FRAME DEFAULT-FRAME
   1 2 3 4 5 6                                                          */
/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-JASPER IN FRAME DEFAULT-FRAME
   NO-ENABLE 1 2 3 4 5 6                                                */
ASSIGN 
       ttUserPrint:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttUserPrint
/* Query rebuild information for BROWSE ttUserPrint
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint
WHERE ttUserPrint.allData MATCHES "*" + searchBar + "*" ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttUserPrint */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* User Reports */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* User Reports */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings (USERID("ASI")).
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* User Reports */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV C-Win
ON CHOOSE OF btnCSV IN FRAME DEFAULT-FRAME /* csv */
DO:
    RUN pRunNow ("CSV").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX C-Win
ON CHOOSE OF btnDOCX IN FRAME DEFAULT-FRAME
DO:
    RUN pRunNow ("DOCX").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME DEFAULT-FRAME
DO:
    RUN pRunNow ("HTML").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF C-Win
ON CHOOSE OF btnPDF IN FRAME DEFAULT-FRAME
DO:
    RUN pRunNow ("PDF").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestoreDefaults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestoreDefaults C-Win
ON CHOOSE OF btnRestoreDefaults IN FRAME DEFAULT-FRAME /* Defaults */
DO:
    RUN pGetSettings ("_default").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun C-Win
ON CHOOSE OF btnRun IN FRAME DEFAULT-FRAME /* Run */
DO:
    RUN pRunNow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSortMove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSortMove C-Win
ON CHOOSE OF btnSortMove IN FRAME DEFAULT-FRAME /* Sort/Move */
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


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME DEFAULT-FRAME
DO:
    RUN pRunNow ("View").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS C-Win
ON CHOOSE OF btnXLS IN FRAME DEFAULT-FRAME
DO:
    RUN pRunNow ("XLS").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME searchBar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL searchBar C-Win
ON VALUE-CHANGED OF searchBar IN FRAME DEFAULT-FRAME /* Search */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttUserPrint
&Scoped-define SELF-NAME ttUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttUserPrint C-Win
ON START-SEARCH OF ttUserPrint IN FRAME DEFAULT-FRAME /* User Reports */
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

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN pGetUserPrint.
  RUN pGetSettings (USERID("ASI")).
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{methods/sortByProc.i "pBymnemonic" "ttUserPrint.mnemonic"}
{methods/sortByProc.i "pByPrgTitle" "ttUserPrint.prgtitle"}
{methods/sortByProc.i "pByProgramID" "ttUserPrint.program-id"}
{methods/sortByProc.i "pByProgTitle" "ttUserPrint.prog-title"}
{methods/sortByProc.i "pByTaskID" "ttUserPrint.taskID"}
{methods/sortByProc.i "pByUserID" "ttUserPrint.user-id"}

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
  DISPLAY searchBar 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnHTML searchBar ttUserPrint btnSortMove btnView btnPDF btnDOCX 
         btnCSV btnXLS btnRun btnRestoreDefaults 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetRecipients C-Win 
PROCEDURE pGetRecipients :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcRecipients AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx]  EQ "svRecipients" AND
           user-print.field-value[idx] NE "" THEN DO:
            opcRecipients = user-print.field-value[idx].
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings C-Win 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE kdx     AS INTEGER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST user-print
                    WHERE user-print.company    EQ g_company
                      AND user-print.program-id EQ "{&programID}"
                      AND user-print.user-id    EQ "_default") THEN
    RUN pSaveSettings ("_default").
    FIND FIRST user-print NO-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
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
        DO idx = 5 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            /* set browse column width, hidden & order */
            DO kdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
                IF user-print.field-name[idx] EQ BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(kdx):NAME THEN DO:
                    ASSIGN
                        jdx           = idx - 4
                        hColumn       = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
                        hColumn:WIDTH = DECIMAL(user-print.field-value[idx])
                        .
                    BROWSE {&BROWSE-NAME}:MOVE-COLUMN(kdx,jdx).
                END. /* if name */
            END. /* do kdx */
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrint C-Win 
PROCEDURE pGetUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bUserPrint FOR user-print.
    
    FOR EACH prgrms NO-LOCK
        WHERE prgrms.mnemonic BEGINS "{&module}"
           OR "{&module}" EQ "ALL",
        EACH user-print EXCLUSIVE-LOCK
        WHERE user-print.company    EQ g_company
          AND user-print.program-id EQ prgrms.prgmname
          AND user-print.prgmname   EQ ""
        :
        IF CAN-FIND(
        FIRST bUserPrint NO-LOCK
        WHERE bUserPrint.company    EQ user-print.company
          AND bUserPrint.program-id EQ user-print.program-id
          AND bUserPrint.user-id    EQ user-print.user-id
          AND bUserPrint.prgmname   EQ "Jasper") THEN DO:
        CREATE ttUserPrint.
        ASSIGN
            ttUserPrint.mnemonic   = prgrms.mnemonic
            ttUserPrint.prog-title = user-print.prog-title
            ttUserPrint.program-id = user-print.program-id
            ttUserPrint.prgtitle   = prgrms.prgtitle
            ttUserPrint.user-id    = user-print.user-id
            ttUserPrint.taskID     = user-print.batch-seq
            ttUserPrint.allData    = ttUserPrint.mnemonic + "|"
                                   + ttUserPrint.prog-title + "|"
                                   + ttUserPrint.program-id + "|"
                                   + ttUserPrint.prgtitle + "|"
                                   + ttUserPrint.user-id + "|"
                                   + STRING(ttUserPrint.taskID)
                                   .
        IF user-print.batch-seq eq 0 then
        user-print.prog-title = "User Default".
        END.
    END. /* each user-print */

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
        WHEN "mnemonic" THEN
        RUN pBymnemonic.
        WHEN "prog-title" THEN
        RUN pByProgTitle.
        WHEN "program-id" THEN
        RUN pByProgramID.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRunNow C-Win 
PROCEDURE pRunNow :
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
            Task.company    = g_company
            Task.user-id    = USERID("ASI")
            Task.programID  = ?
            Task.taskName   = "Run Now Task"
            Task.taskFormat = ipcTaskFormat
            Task.runNow     = YES
            Task.recipients = cRecipients
            .
        RELEASE Task.
    END. /* do trans */
    MESSAGE
        "Task ~"" + aoaTitle + "~" has been submitted."
    VIEW-AS ALERT-BOX TITLE "Run Now".

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
    DEFINE INPUT PARAMETER ipcUserID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hColumn AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK
         WHERE user-print.company    EQ g_company
           AND user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ ipcUserID
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = g_company
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
    /* save browse column order and width */
    DO jdx = 1 TO BROWSE {&BROWSE-NAME}:NUM-COLUMNS:
        ASSIGN
            idx                         = idx + 1
            hColumn                     = BROWSE {&BROWSE-NAME}:GET-BROWSE-COLUMN(jdx)
            user-print.field-label[idx] = "BrowseColumn"
            user-print.field-name[idx]  = hColumn:NAME
            user-print.field-value[idx] = STRING(MAX(hColumn:WIDTH, .2 /*BROWSE {&BROWSE-NAME}:MIN-COLUMN-WIDTH-CHARS*/ ))
            .
    END. /* do jdx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    SESSION:SET-WAIT-STATE("General").
    DO WITH FRAME {&FRAME-NAME}:
        HIDE BROWSE {&BROWSE-NAME}.
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
                                               - BROWSE {&BROWSE-NAME}:ROW + 1
            BROWSE {&BROWSE-NAME}:WIDTH        = FRAME {&FRAME-NAME}:WIDTH
                                               - BROWSE {&BROWSE-NAME}:COL + 1
            .
        VIEW FRAME {&FRAME-NAME}.
        VIEW BROWSE {&BROWSE-NAME}.
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

