&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: AOA/dynRun.w

  Description: Run/Execute Dynamic Subject/Query

  Input Parameters: <none>

  Output Parameters: <none>

  Author: Ron Stark

  Created: 1.18.2019

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

&Scoped-define program-id dynRun.
&Scoped-define defaultUser _default

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcPrgmName   AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iprRowID      AS ROWID     NO-UNDO.
DEFINE INPUT PARAMETER iplParameters AS LOGICAL   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cPoolName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrgmName    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTitle       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hAppSrvBin   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hJasper      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQueryBrowse AS HANDLE    NO-UNDO.
DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
DEFINE VARIABLE queryStr     AS CHARACTER NO-UNDO.

cPrgmName = ipcPrgmName.

{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{AOA/tempTable/ttDynAction.i}

RUN AOA/appServer/aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).
RUN AOA/spJasper.p PERSISTENT SET hJasper.
SESSION:ADD-SUPER-PROCEDURE (hJasper).

{methods/lockWindowUpdate.i}

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

{AOA/includes/dynFuncs.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame

/* Custom List Definitions                                              */
/* List-1,showFields,List-3,List-4,List-5,List-6                        */
&Scoped-define showFields svShowAll svShowReportHeader svShowReportFooter ~
svShowPageHeader svShowPageFooter svShowGroupHeader svShowGroupFooter ~
svShowParameters 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetShowAll C-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAddEmail 
     IMAGE-UP FILE "AOA/images/navigate_plus.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Email" 
     SIZE 4.4 BY 1.05 TOOLTIP "Add Recipents".

DEFINE BUTTON btnCSV 
     IMAGE-UP FILE "Graphics/32x32/spreadsheet_sum.png":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 8 BY 1.91 TOOLTIP "Excel CSV".

DEFINE BUTTON btnDOCX 
     IMAGE-UP FILE "Graphics/32x32/docx.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Word DOCX".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "Graphics/32x32/html_tag.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "HTML".

DEFINE BUTTON btnLocalCSV 
     IMAGE-UP FILE "Graphics/32x32/csv.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Local Excel CSV".

DEFINE BUTTON btnPageFormat 
     IMAGE-UP FILE "Graphics/32x32/document_gear.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Page Format".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "Graphics/32x32/pdf.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "PDF".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "Graphics/32x32/print_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Printer".

DEFINE BUTTON btnRunResults 
     IMAGE-UP FILE "Graphics/32x32/table.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Results Grid" 
     SIZE 8 BY 1.91 TOOLTIP "Results Grid".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "Graphics/32x32/jss_icon.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnXLS 
     IMAGE-UP FILE "Graphics/32x32/xls.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Excel XLS".

DEFINE VARIABLE svRecipients AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 69 BY 1.67
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-PANEL
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 82 BY 2.38.

DEFINE RECTANGLE RECT-SHOW
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 158 BY 1.19.

DEFINE VARIABLE svRunSync AS LOGICAL INITIAL no 
     LABEL "Run Synchronous" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL yes 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL yes 
     LABEL "Group Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL yes 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL yes 
     LABEL "Page Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL yes 
     LABEL "Page Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL yes 
     LABEL "Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL yes 
     LABEL "Report Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL yes 
     LABEL "Report Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE BUTTON btnCloseResults 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Close Results" 
     SIZE 4.4 BY 1 TOOLTIP "Close Results".

DEFINE BUTTON btnSaveResults 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Save Results" 
     SIZE 4.4 BY 1 TOOLTIP "Save Results".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 28.57
         FGCOLOR 1  WIDGET-ID 100.

DEFINE FRAME resultsFrame
     btnSaveResults AT ROW 1 COL 2 HELP
          "Jasper Viewer" WIDGET-ID 254
     btnCloseResults AT ROW 1 COL 6 HELP
          "Jasper Viewer" WIDGET-ID 252
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 6.48
         SIZE 10 BY 2.38
         BGCOLOR 15 FGCOLOR 1  WIDGET-ID 1200.

DEFINE FRAME outputFrame
     btnCSV AT ROW 1.48 COL 95 HELP
          "Excel CSV" WIDGET-ID 140
     btnLocalCSV AT ROW 1.48 COL 87 HELP
          "Local Excel CSV" WIDGET-ID 656
     svRecipients AT ROW 1.24 COL 8 NO-LABEL WIDGET-ID 600
     svRunSync AT ROW 2.91 COL 8 HELP
          "Toggle to Run Synchronous" WIDGET-ID 654
     btnPageFormat AT ROW 1.48 COL 143 HELP
          "Page Format" WIDGET-ID 652
     svShowAll AT ROW 4.1 COL 8 WIDGET-ID 18
     svShowReportHeader AT ROW 4.1 COL 24 WIDGET-ID 2
     svShowReportFooter AT ROW 4.1 COL 45 WIDGET-ID 4
     btnRunResults AT ROW 1.48 COL 79 HELP
          "Results Grid" WIDGET-ID 254
     svShowPageHeader AT ROW 4.1 COL 66 WIDGET-ID 6
     svShowPageFooter AT ROW 4.1 COL 85 WIDGET-ID 8
     svShowGroupHeader AT ROW 4.1 COL 104 WIDGET-ID 10
     svShowGroupFooter AT ROW 4.1 COL 124 WIDGET-ID 12
     svShowParameters AT ROW 4.1 COL 143 WIDGET-ID 16
     btnHTML AT ROW 1.48 COL 127 HELP
          "HTML" WIDGET-ID 144
     btnView AT ROW 1.48 COL 151 HELP
          "Jasper Viewer" WIDGET-ID 148
     btnAddEmail AT ROW 1.95 COL 3 HELP
          "Add Recipents" WIDGET-ID 636
     btnPrint AT ROW 1.48 COL 135 HELP
          "Printer" WIDGET-ID 644
     btnDOCX AT ROW 1.48 COL 111 HELP
          "Word DOCX" WIDGET-ID 142
     btnPDF AT ROW 1.48 COL 119 HELP
          "PDF" WIDGET-ID 146
     btnXLS AT ROW 1.48 COL 103 HELP
          "Excel XLS" WIDGET-ID 150
     "Email:" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.24 COL 2 WIDGET-ID 640
     RECT-PANEL AT ROW 1.24 COL 78 WIDGET-ID 256
     RECT-SHOW AT ROW 3.86 COL 2 WIDGET-ID 642
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 160 BY 5.24
         BGCOLOR 21 FGCOLOR 15 
         TITLE BGCOLOR 15 "Parameters" WIDGET-ID 1300.

DEFINE FRAME blankFrame
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 6.48
         SIZE 19 BY 2.38
         BGCOLOR 15 FGCOLOR 1 
         TITLE "Building Grid ..." WIDGET-ID 1400.


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
         TITLE              = "Dynamic Run Subject/Query"
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics/32x32/jss_icon_32.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics/32x32/jss_icon_32.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME blankFrame:FRAME = FRAME paramFrame:HANDLE
       FRAME outputFrame:FRAME = FRAME paramFrame:HANDLE
       FRAME resultsFrame:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME blankFrame
                                                                        */
ASSIGN 
       FRAME blankFrame:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME outputFrame
                                                                        */
ASSIGN 
       btnPageFormat:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

ASSIGN 
       btnPrint:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

ASSIGN 
       btnView:AUTO-RESIZE IN FRAME outputFrame      = TRUE.

/* SETTINGS FOR RECTANGLE RECT-PANEL IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-SHOW IN FRAME outputFrame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX svShowAll IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupFooter IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupHeader IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageFooter IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageHeader IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowParameters IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportFooter IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportHeader IN FRAME outputFrame
   2                                                                    */
/* SETTINGS FOR FRAME paramFrame
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME resultsFrame:MOVE-BEFORE-TAB-ITEM (FRAME blankFrame:HANDLE)
       XXTABVALXX = FRAME outputFrame:MOVE-BEFORE-TAB-ITEM (FRAME resultsFrame:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME resultsFrame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME resultsFrame:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME blankFrame
/* Query rebuild information for FRAME blankFrame
     _Query            is NOT OPENED
*/  /* FRAME blankFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME resultsFrame
/* Query rebuild information for FRAME resultsFrame
     _Query            is NOT OPENED
*/  /* FRAME resultsFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Dynamic Run Subject/Query */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Dynamic Run Subject/Query */
DO:
  /* This event will close the window and terminate the procedure.  */
  RUN pSaveSettings.
  RUN pDeleteProcedure.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Dynamic Run Subject/Query */
DO:
    RUN pWinReSize.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnAddEmail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAddEmail C-Win
ON CHOOSE OF btnAddEmail IN FRAME outputFrame /* Email */
DO:
    RUN pRecipients (svRecipients:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME resultsFrame
&Scoped-define SELF-NAME btnCloseResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCloseResults C-Win
ON CHOOSE OF btnCloseResults IN FRAME resultsFrame /* Close Results */
DO:
    IF iplParameters THEN
    FRAME resultsFrame:HIDDEN = YES.
    ELSE
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCSV C-Win
ON CHOOSE OF btnCSV IN FRAME outputFrame /* csv */
DO:
    RUN pRunSubject (YES, "CSV", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDOCX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDOCX C-Win
ON CHOOSE OF btnDOCX IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "DOCX", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML C-Win
ON CHOOSE OF btnHTML IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "HTML", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLocalCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLocalCSV C-Win
ON CHOOSE OF btnLocalCSV IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "LocalCSV", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPageFormat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPageFormat C-Win
ON CHOOSE OF btnPageFormat IN FRAME outputFrame
DO:
    RUN pPageFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF C-Win
ON CHOOSE OF btnPDF IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "PDF", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint C-Win
ON CHOOSE OF btnPrint IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "Print -d", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRunResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRunResults C-Win
ON CHOOSE OF btnRunResults IN FRAME outputFrame /* Results Grid */
DO:
    RUN pRunSubject (YES, "Grid", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME resultsFrame
&Scoped-define SELF-NAME btnSaveResults
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveResults C-Win
ON CHOOSE OF btnSaveResults IN FRAME resultsFrame /* Save Results */
DO:
    RUN pSaveResults.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME outputFrame
&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView C-Win
ON CHOOSE OF btnView IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "View", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnXLS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnXLS C-Win
ON CHOOSE OF btnXLS IN FRAME outputFrame
DO:
    RUN pRunSubject (YES, "XLS", USERID("ASI"), cPrgmName).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll C-Win
ON VALUE-CHANGED OF svShowAll IN FRAME outputFrame /* Show ALL */
DO:
  ASSIGN {&SELF-NAME}
      svShowReportHeader = {&SELF-NAME}
      svShowParameters   = {&SELF-NAME}
      svShowPageHeader   = {&SELF-NAME}
      svShowGroupHeader  = {&SELF-NAME}
      svShowGroupFooter  = {&SELF-NAME}
      svShowPageFooter   = {&SELF-NAME}
      svShowReportFooter = {&SELF-NAME}
      .
  DISPLAY {&showFields} WITH FRAME outputFrame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter C-Win
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME outputFrame /* Group Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader C-Win
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME outputFrame /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter C-Win
ON VALUE-CHANGED OF svShowPageFooter IN FRAME outputFrame /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader C-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME outputFrame /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters C-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME outputFrame /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter C-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME outputFrame /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader C-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME outputFrame /* Report Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
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

{AOA/includes/dynProcs.i "dyn"}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN spGetLookupTitle (OUTPUT cTitle).
  IF cTitle NE ? THEN
  {&WINDOW-NAME}:TITLE = cTitle.
  RUN pGetSettings.
  RUN enable_UI.
  RUN pCreateDynParameters (FRAME paramFrame:HANDLE, YES).
  IF iplParameters EQ NO THEN DO:
/*    IF dynParamValue.user-id NE "{&defaultUser}" AND*/
/*       dynParamValue.paramValueID NE 0 THEN DO:     */
        FRAME blankFrame:HIDDEN = NO.
        FRAME blankFrame:MOVE-TO-TOP().
/*    END. /* if user-id */*/
    IF dynParamValue.outputFormat EQ "LocalCSV" THEN
    APPLY "CHOOSE":U TO btnLocalCSV.
    ELSE
    APPLY "CHOOSE":U TO btnRunResults.
  END. /* if no */
  ELSE
  FRAME blankFrame:HIDDEN = YES.
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
  DISPLAY svRecipients svRunSync svShowAll svShowReportHeader svShowReportFooter 
          svShowPageHeader svShowPageFooter svShowGroupHeader svShowGroupFooter 
          svShowParameters 
      WITH FRAME outputFrame IN WINDOW C-Win.
  ENABLE btnCSV btnLocalCSV svRecipients svRunSync btnPageFormat svShowAll 
         svShowReportHeader svShowReportFooter btnRunResults svShowPageHeader 
         svShowPageFooter svShowGroupHeader svShowGroupFooter svShowParameters 
         btnHTML btnView btnAddEmail btnPrint btnDOCX btnPDF btnXLS 
      WITH FRAME outputFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-outputFrame}
  VIEW FRAME paramFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  ENABLE btnSaveResults btnCloseResults 
      WITH FRAME resultsFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-resultsFrame}
  VIEW FRAME blankFrame IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-blankFrame}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteProcedure C-Win 
PROCEDURE pDeleteProcedure :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(hAppSrvBin) THEN
   DELETE PROCEDURE hAppSrvBin.
   IF VALID-HANDLE(hJasper) THEN
   DELETE PROCEDURE hJasper.
   IF VALID-HANDLE(hDynCalcField) THEN
   DELETE PROCEDURE hDynCalcField.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetDynParamValue C-Win 
PROCEDURE pGetDynParamValue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND FIRST dynParamValue NO-LOCK
         WHERE ROWID(dynParamValue) EQ iprRowID
         NO-ERROR.
    IF AVAILABLE dynParamValue THEN DO:
        FIND FIRST dynSubject NO-LOCK
             WHERE dynSubject.subjectID EQ dynParamValue.subjectID
             NO-ERROR.
        IF AVAILABLE dynSubject THEN
        queryStr = dynSubject.queryStr.        
    END. /* if avail */

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
         WHERE user-print.program-id EQ "{&program-id}"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPageFormat C-Win 
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
        RUN pSetDynParamValue (dynSubject.subjectID, USERID("ASI"), cPrgmName, 0).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveResults C-Win 
PROCEDURE pSaveResults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDBName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dSize      AS DECIMAL   NO-UNDO.
    
    DO TRANSACTION:
        DO idx = 1 TO hQueryBrowse:NUM-COLUMNS:
            ASSIGN
                cDBName    = hQueryBrowse:GET-BROWSE-COLUMN(idx):DBNAME
                cTableName = hQueryBrowse:GET-BROWSE-COLUMN(idx):TABLE
                cFieldName = cTableName + "."
                           + hQueryBrowse:GET-BROWSE-COLUMN(idx):NAME
                dSize      = hQueryBrowse:GET-BROWSE-COLUMN(idx):WIDTH-CHARS
                .
            IF cDBName EQ "PROGRESST" THEN
            cDBName = "ASI".
            IF hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX NE 0 THEN
            cFieldName = cFieldName
                       + "["
                       + STRING(hQueryBrowse:GET-BROWSE-COLUMN(idx):INDEX)
                       + "]"
                       .
            FIND FIRST dynValueColumn EXCLUSIVE-LOCK
                 WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
                   AND dynValueColumn.user-id      EQ dynParamValue.user-id
                   AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
                   AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
                   AND dynValueColumn.colName      EQ cFieldName
                 NO-ERROR.
            IF AVAILABLE dynValueColumn THEN
            ASSIGN
                dynValueColumn.sortOrder  = idx
                dynValueColumn.columnSize = dSize
                .
            RELEASE dynValueColumn.
        END. /* do idx */
    END. /* trans */

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
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF NOT AVAILABLE user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id = "{&program-id}"
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize C-Win 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lResultsFrameHidden AS LOGICAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    RUN LockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
    lResultsFrameHidden = FRAME resultsFrame:HIDDEN.
    DO WITH FRAME {&FRAME-NAME}:
        hQueryBrowse:HIDDEN = YES.
        HIDE FRAME blankFrame.
        HIDE FRAME resultsFrame.
        HIDE FRAME {&FRAME-NAME}.
        IF {&WINDOW-NAME}:HEIGHT LT 28.57 THEN
        {&WINDOW-NAME}:HEIGHT = 28.57.
        IF {&WINDOW-NAME}:WIDTH  LT 160   THEN
        {&WINDOW-NAME}:WIDTH  = 160.
        ASSIGN
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = {&WINDOW-NAME}:WIDTH
            FRAME resultsFrame:VIRTUAL-HEIGHT  = FRAME {&FRAME-NAME}:HEIGHT
            FRAME resultsFrame:VIRTUAL-WIDTH   = FRAME {&FRAME-NAME}:WIDTH
            FRAME blankFrame:VIRTUAL-HEIGHT    = FRAME {&FRAME-NAME}:HEIGHT
            FRAME blankFrame:VIRTUAL-WIDTH     = FRAME {&FRAME-NAME}:WIDTH
            FRAME {&FRAME-NAME}:HEIGHT         = {&WINDOW-NAME}:HEIGHT
            FRAME {&FRAME-NAME}:WIDTH          = {&WINDOW-NAME}:WIDTH
            FRAME resultsFrame:HEIGHT          = FRAME {&FRAME-NAME}:HEIGHT
            FRAME resultsFrame:WIDTH           = FRAME {&FRAME-NAME}:WIDTH
            FRAME blankFrame:HEIGHT            = FRAME {&FRAME-NAME}:HEIGHT
            FRAME blankFrame:WIDTH             = FRAME {&FRAME-NAME}:WIDTH
            FRAME resultsFrame:COL             = 1
            FRAME resultsFrame:ROW             = 1
            FRAME blankFrame:COL               = 1
            FRAME blankFrame:ROW               = 1
            .
        IF iplParameters THEN
        VIEW FRAME {&FRAME-NAME}.
        ELSE DO:
            FRAME blankFrame:HIDDEN = NO.
            FRAME blankFrame:MOVE-TO-TOP().
        END.
    END. /* do with */
    DO WITH FRAME resultsFrame:
        ASSIGN
            btnCloseResults:COL = FRAME resultsFrame:WIDTH - btnCloseResults:WIDTH
            btnSaveResults:COL  = btnCloseResults:COL - btnSaveResults:WIDTH
            .
        IF VALID-HANDLE(hQueryBrowse) THEN DO:
            ASSIGN
                hQueryBrowse:HEIGHT = FRAME resultsFrame:HEIGHT - .1
                hQueryBrowse:WIDTH  = FRAME resultsFrame:WIDTH - .32
                hQueryBrowse:HIDDEN = NO
                .
        END. /* if valid-handle */
    END. /* do with */
    IF NOT lResultsFrameHidden THEN DO:
        VIEW FRAME resultsFrame.
        FRAME resultsFrame:MOVE-TO-TOP().
    END.
    RUN LockWindowUpdate (0,OUTPUT i).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetShowAll C-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME outputFrame:
        svShowAll = svShowReportHeader AND
                    svShowParameters   AND
                    svShowPageHeader   AND
                    svShowGroupHeader  AND
                    svShowGroupFooter  AND
                    svShowPageFooter   AND
                    svShowReportFooter
                    .
        DISPLAY {&showFields}.
    END. /* do with */
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

