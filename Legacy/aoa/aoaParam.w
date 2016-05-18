&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: aoaParam.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:  <none>

  Output Parameters: <none>

  History: Ron Stark - 3.7.2016
          
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

DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcParamStr AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{aoa/aoaParamDefs.i}

DEFINE VARIABLE hAppSrv       AS HANDLE    NO-UNDO.

DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPrinterCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPrtCmd          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrtPort         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttUserPrint NO-UNDO LIKE user-print
    FIELD UserPrintRowID AS ROWID.

DEFINE TEMP-TABLE ttParamValue NO-UNDO
    FIELD paramOrder AS INTEGER
    FIELD batch-seq  AS INTEGER
    FIELD paramLabel AS CHARACTER LABEL "Param Label" FORMAT "x(30)"
    FIELD paramValue AS CHARACTER LABEL "Param Value" FORMAT "x(30)"
        INDEX paramOrder IS PRIMARY paramOrder
    .

IF aoaColumns THEN
RUN VALUE("aoaAppSrv/" + ENTRY(1,aoaParam,"/") + ".p") PERSISTENT SET hAppSrv.

DEFINE BUFFER bUserPrint FOR user-print.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME paramFrame
&Scoped-define BROWSE-NAME browseParamValue

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttParamValue ttUserPrint

/* Definitions for BROWSE browseParamValue                              */
&Scoped-define FIELDS-IN-QUERY-browseParamValue ttParamValue.paramLabel ttParamValue.paramValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseParamValue   
&Scoped-define SELF-NAME browseParamValue
&Scoped-define QUERY-STRING-browseParamValue FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq
&Scoped-define OPEN-QUERY-browseParamValue OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq.
&Scoped-define TABLES-IN-QUERY-browseParamValue ttParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseParamValue ttParamValue


/* Definitions for BROWSE browseUserPrint                               */
&Scoped-define FIELDS-IN-QUERY-browseUserPrint ttUserPrint.batch-seq ttUserPrint.last-date STRING(ttUserPrint.last-time,"hh:mm:ss am")   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseUserPrint   
&Scoped-define SELF-NAME browseUserPrint
&Scoped-define QUERY-STRING-browseUserPrint FOR EACH ttUserPrint
&Scoped-define OPEN-QUERY-browseUserPrint OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint.
&Scoped-define TABLES-IN-QUERY-browseUserPrint ttUserPrint
&Scoped-define FIRST-TABLE-IN-QUERY-browseUserPrint ttUserPrint


/* Definitions for FRAME paramFrame                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-paramFrame ~
    ~{&OPEN-QUERY-browseParamValue}~
    ~{&OPEN-QUERY-browseUserPrint}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS browseParamValue btnCancel btnView ~
browseUserPrint btnDelete btnApply btnSave 

/* Custom List Definitions                                              */
/* ScheduleFields,showFields,List-3,List-4,List-5,List-6                */
&Scoped-define showFields svShowAll svShowReportHeader svShowParameters ~
svShowPageHeader svShowGroupHeader svShowGroupFooter svShowPageFooter ~
svShowReportFooter 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptions W-Win 
FUNCTION fDateOptions RETURNS LOGICAL (ipDateOption AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGenerateInclude W-Win 
FUNCTION fGenerateInclude RETURNS LOGICAL
  ( iphFrame AS HANDLE, ipcType AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCompany W-Win 
FUNCTION fGetCompany RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetShowAll W-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_aoaParam AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAdd 
     LABEL "&Add" 
     SIZE 8 BY 1.

DEFINE BUTTON btnDefault 
     LABEL "&Default" 
     SIZE 8 BY 1.

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "images/down.bmp":U
     LABEL "Move Down" 
     SIZE 5 BY 1.

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "images/up.bmp":U
     LABEL "Move Up" 
     SIZE 5 BY 1.

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "images/cancel.bmp":U
     LABEL "<< &Remove" 
     SIZE 5 BY 1.

DEFINE VARIABLE svAvailableColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 34 BY 9.52 NO-UNDO.

DEFINE VARIABLE svSelectedColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 34 BY 9.52 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL no 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL no 
     LABEL "Show Group Footer (SubTotals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL no 
     LABEL "Show Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL no 
     LABEL "Show Page Footer (Date / Page No.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL no 
     LABEL "Show Page Header (Column Headers)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL no 
     LABEL "Show Parameters (Report Header)" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL no 
     LABEL "Show Report Footer (Grand Totals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL no 
     LABEL "Show Report Header (Report Title)" 
     VIEW-AS TOGGLE-BOX
     SIZE 40 BY .81 NO-UNDO.

DEFINE BUTTON btnApply 
     LABEL "&Apply" 
     SIZE 12.6 BY 1.1.

DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnDelete 
     LABEL "&Delete" 
     SIZE 12.6 BY 1.1.

DEFINE BUTTON btnSave 
     LABEL "&Save" 
     SIZE 12.6 BY 1.1.

DEFINE BUTTON btnScheduler 
     LABEL "Assign &Scheduler Batch ID" 
     SIZE 31 BY 1.14.

DEFINE BUTTON btnView 
     LABEL "&View" 
     SIZE 15 BY 1.14.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseParamValue FOR 
      ttParamValue SCROLLING.

DEFINE QUERY browseUserPrint FOR 
      ttUserPrint SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseParamValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseParamValue W-Win _FREEFORM
  QUERY browseParamValue DISPLAY
      ttParamValue.paramLabel
    ttParamValue.paramValue
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66 BY 11.43
         TITLE "Batch Parameter Values".

DEFINE BROWSE browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseUserPrint W-Win _FREEFORM
  QUERY browseUserPrint DISPLAY
      ttUserPrint.batch-seq LABEL "Batch Seq"
      ttUserPrint.last-date LABEL "Date"
      STRING(ttUserPrint.last-time,"hh:mm:ss am") LABEL "Time" FORMAT "x(12)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 7.38
         TITLE "Batch".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     browseParamValue AT ROW 1 COL 173 WIDGET-ID 600
     btnCancel AT ROW 2.43 COL 1 WIDGET-ID 12
     btnView AT ROW 2.43 COL 17 WIDGET-ID 14
     btnScheduler AT ROW 3.62 COL 1 WIDGET-ID 10
     browseUserPrint AT ROW 5.05 COL 1 WIDGET-ID 500
     btnDelete AT ROW 11.24 COL 127 WIDGET-ID 4
     btnApply AT ROW 11.24 COL 141 WIDGET-ID 16
     btnSave AT ROW 11.24 COL 155 WIDGET-ID 18
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 239 BY 11.52.

DEFINE FRAME frameColumns
     svAvailableColumns AT ROW 1.76 COL 1 NO-LABEL WIDGET-ID 68
     svSelectedColumns AT ROW 1.76 COL 45 NO-LABEL WIDGET-ID 70
     btnDefault AT ROW 2.91 COL 36 HELP
          "Add Selected Table to Display" WIDGET-ID 76
     btnMoveUp AT ROW 2.91 COL 80 WIDGET-ID 66
     btnMoveDown AT ROW 4 COL 80 WIDGET-ID 62
     btnRemove AT ROW 5.14 COL 80 HELP
          "Remove Selected Table from Tables to Audit" WIDGET-ID 64
     btnAdd AT ROW 5.29 COL 36 HELP
          "Add Selected Table to Display" WIDGET-ID 58
     "Selected Columns (In Display Order)" VIEW-AS TEXT
          SIZE 34 BY .62 AT ROW 1 COL 45 WIDGET-ID 72
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1 COL 2 WIDGET-ID 74
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 1
         SIZE 85 BY 11.43
         TITLE "Report Columns" WIDGET-ID 200.

DEFINE FRAME frameShow
     svShowAll AT ROW 1.24 COL 2 WIDGET-ID 18
     svShowReportHeader AT ROW 2.19 COL 5 WIDGET-ID 2
     svShowParameters AT ROW 3.14 COL 8 WIDGET-ID 16
     svShowPageHeader AT ROW 4.1 COL 5 WIDGET-ID 6
     svShowGroupHeader AT ROW 5.05 COL 5 WIDGET-ID 10
     svShowGroupFooter AT ROW 6 COL 5 WIDGET-ID 12
     svShowPageFooter AT ROW 6.95 COL 5 WIDGET-ID 8
     svShowReportFooter AT ROW 7.91 COL 5 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 127 ROW 1
         SIZE 45 BY 8.81
         TITLE "Show/Hide Sections" WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "AdvantzwareOA"
         HEIGHT             = 11.52
         WIDTH              = 239
         MAX-HEIGHT         = 11.52
         MAX-WIDTH          = 239
         VIRTUAL-HEIGHT     = 11.52
         VIRTUAL-WIDTH      = 239
         SHOW-IN-TASKBAR    = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Win:LOAD-ICON("schedule/images/scheduler.ico":U) THEN
    MESSAGE "Unable to load icon: schedule/images/scheduler.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME frameColumns:FRAME = FRAME paramFrame:HANDLE
       FRAME frameShow:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME frameColumns
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frameColumns:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME frameShow
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frameShow:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX svShowAll IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowGroupHeader IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowPageHeader IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowParameters IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportFooter IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX svShowReportHeader IN FRAME frameShow
   2                                                                    */
/* SETTINGS FOR FRAME paramFrame
   FRAME-NAME                                                           */
/* BROWSE-TAB browseParamValue frameShow paramFrame */
/* BROWSE-TAB browseUserPrint btnScheduler paramFrame */
ASSIGN 
       browseParamValue:HIDDEN  IN FRAME paramFrame                = TRUE.

ASSIGN 
       btnApply:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnCancel:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

ASSIGN 
       btnDelete:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnSave:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnScheduler IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnScheduler:HIDDEN IN FRAME paramFrame           = TRUE
       btnScheduler:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

ASSIGN 
       btnView:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseParamValue
/* Query rebuild information for BROWSE browseParamValue
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue
     WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseParamValue */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseUserPrint
/* Query rebuild information for BROWSE browseUserPrint
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseUserPrint */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frameColumns
/* Query rebuild information for FRAME frameColumns
     _Query            is NOT OPENED
*/  /* FRAME frameColumns */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frameShow
/* Query rebuild information for FRAME frameShow
     _Query            is NOT OPENED
*/  /* FRAME frameShow */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME paramFrame
/* Query rebuild information for FRAME paramFrame
     _Query            is NOT OPENED
*/  /* FRAME paramFrame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* AdvantzwareOA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* AdvantzwareOA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  
  IF VALID-HANDLE(hAppSrv) THEN DO:
      DELETE OBJECT hAppSrv.
  END. /* valid happsrv */

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseUserPrint
&Scoped-define SELF-NAME browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseUserPrint W-Win
ON VALUE-CHANGED OF browseUserPrint IN FRAME paramFrame /* Batch */
DO:
    {&OPEN-QUERY-browseParamValue}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameColumns
&Scoped-define SELF-NAME btnAdd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAdd W-Win
ON CHOOSE OF btnAdd IN FRAME frameColumns /* Add */
DO:
  RUN pAddColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply W-Win
ON CHOOSE OF btnApply IN FRAME paramFrame /* Apply */
DO:
    IF AVAILABLE ttUserPrint THEN DO:
        RUN pGetParamValues (ttUserPrint.UserPrintRowID).
        IF aoaColumns THEN RUN pGetColumns.
        RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel W-Win
ON CHOOSE OF btnCancel IN FRAME paramFrame /* Cancel */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel W-Win
ON RIGHT-MOUSE-CLICK OF btnCancel IN FRAME paramFrame /* Cancel */
DO:
  RUN pGenerateInclude.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameColumns
&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault W-Win
ON CHOOSE OF btnDefault IN FRAME frameColumns /* Default */
DO:
  svSelectedColumns:LIST-ITEM-PAIRS = ?.
  RUN pGetColumns.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnDelete
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDelete W-Win
ON CHOOSE OF btnDelete IN FRAME paramFrame /* Delete */
DO:
    IF AVAILABLE ttUserPrint THEN DO:
        MESSAGE "Delete Batch" ttUserPrint.batch-seq "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            TITLE "Delete Batch Record"
            UPDATE deleteBatch AS LOGICAL
            .
        IF deleteBatch THEN DO:
            FIND bUserPrint EXCLUSIVE-LOCK WHERE ROWID(bUserPrint) EQ ttUserPrint.UserPrintRowID.
            DELETE bUserPrint.
            RUN pGetUserPrint.
        END. /* delete batch */
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameColumns
&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown W-Win
ON CHOOSE OF btnMoveDown IN FRAME frameColumns /* Move Down */
DO:
  RUN pMoveColumn("Down").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp W-Win
ON CHOOSE OF btnMoveUp IN FRAME frameColumns /* Move Up */
DO:
  RUN pMoveColumn("Up").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRemove
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemove W-Win
ON CHOOSE OF btnRemove IN FRAME frameColumns /* << Remove */
DO:
  RUN pDeleteColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave W-Win
ON CHOOSE OF btnSave IN FRAME paramFrame /* Save */
DO:
    IF AVAILABLE ttUserPrint THEN DO:
        FIND user-print WHERE ROWID(user-print) EQ ttUserPrint.UserPrintRowID.
        RUN pSaveParamValues (?, BUFFER user-print).
        MESSAGE "Batch" ttUserPrint.batch-seq "Saved"
            VIEW-AS ALERT-BOX
            TITLE "Batch Record Saved"
            .
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScheduler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScheduler W-Win
ON CHOOSE OF btnScheduler IN FRAME paramFrame /* Assign Scheduler Batch ID */
DO:
    RUN pSchedule.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnView
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnView W-Win
ON CHOOSE OF btnView IN FRAME paramFrame /* View */
DO:
    RUN pSaveParamValues (NO, BUFFER user-print).
    {aoa/aoaURL.i}
    APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameColumns
&Scoped-define SELF-NAME svAvailableColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svAvailableColumns W-Win
ON DEFAULT-ACTION OF svAvailableColumns IN FRAME frameColumns
DO:
  RUN pAddColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svSelectedColumns
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSelectedColumns W-Win
ON DEFAULT-ACTION OF svSelectedColumns IN FRAME frameColumns
DO:
    RUN pDeleteColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameShow
&Scoped-define SELF-NAME svShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowAll W-Win
ON VALUE-CHANGED OF svShowAll IN FRAME frameShow /* Show ALL */
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
  DISPLAY {&showFields} WITH FRAME frameShow.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupFooter W-Win
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME frameShow /* Show Group Footer (SubTotals) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader W-Win
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME frameShow /* Show Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter W-Win
ON VALUE-CHANGED OF svShowPageFooter IN FRAME frameShow /* Show Page Footer (Date / Page No.) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader W-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME frameShow /* Show Page Header (Column Headers) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters W-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME frameShow /* Show Parameters (Report Header) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter W-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME frameShow /* Show Report Footer (Grand Totals) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader W-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME frameShow /* Show Report Header (Report Title) */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} EQ FALSE THEN
    svShowParameters = {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define BROWSE-NAME browseParamValue
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

{&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " " + aoaType + " - " + aoaTitle.

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
           &IF DEFINED(UIB_is_Running) ne 0 &THEN
             INPUT  'aoa/aoaParamHolder.w':U ,
           &ELSE
             INPUT aoaParam ,
           &ENDIF
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_aoaParam ).
       RUN set-position IN h_aoaParam ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.29 , 39.40 ) */

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_aoaParam ,
             FRAME frameColumns:HANDLE , 'BEFORE':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE browseParamValue btnCancel btnView browseUserPrint btnDelete btnApply 
         btnSave 
      WITH FRAME paramFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  DISPLAY svAvailableColumns svSelectedColumns 
      WITH FRAME frameColumns IN WINDOW W-Win.
  ENABLE svAvailableColumns svSelectedColumns btnDefault btnMoveUp btnMoveDown 
         btnRemove btnAdd 
      WITH FRAME frameColumns IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameColumns}
  DISPLAY svShowAll svShowReportHeader svShowParameters svShowPageHeader 
          svShowGroupHeader svShowGroupFooter svShowPageFooter 
          svShowReportFooter 
      WITH FRAME frameShow IN WINDOW W-Win.
  ENABLE svShowAll svShowReportHeader svShowParameters svShowPageHeader 
         svShowGroupHeader svShowGroupFooter svShowPageFooter 
         svShowReportFooter 
      WITH FRAME frameShow IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameShow}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-objects W-Win 
PROCEDURE local-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-objects':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN pSetWinSize.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable W-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN pPopulateOptions IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.

  RUN pGetParamValues (?).
  
  IF aoaColumns THEN RUN pGetColumns.

  RUN pParamValuesOverride IN h_aoaParam NO-ERROR.

  RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.

  IF aoaType EQ "report" THEN DO:
      ENABLE btnScheduler WITH FRAME {&FRAME-NAME}.
      RUN pGetUserPrint.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddColumn W-Win 
PROCEDURE pAddColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF svAvailableColumns:SCREEN-VALUE IN FRAME frameColumns EQ ? THEN RETURN.

    svSelectedColumns:ADD-LAST(ENTRY((svAvailableColumns:LOOKUP(svAvailableColumns:SCREEN-VALUE) * 2) - 1,
                                      svAvailableColumns:LIST-ITEM-PAIRS),
                                      svAvailableColumns:SCREEN-VALUE).
    svAvailableColumns:DELETE(svAvailableColumns:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDeleteColumn W-Win 
PROCEDURE pDeleteColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF svSelectedColumns:SCREEN-VALUE IN FRAME frameColumns EQ ? THEN RETURN.

    svAvailableColumns:ADD-LAST(ENTRY((svSelectedColumns:LOOKUP(svSelectedColumns:SCREEN-VALUE) * 2) - 1,
                                       svSelectedColumns:LIST-ITEM-PAIRS),
                                       svSelectedColumns:SCREEN-VALUE).
    svSelectedColumns:DELETE(svSelectedColumns:SCREEN-VALUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGenerateInclude W-Win 
PROCEDURE pGenerateInclude :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hFrame    AS HANDLE  NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF NOT CAN-DO("ASI,NoSweat",USERID("NoSweat")) THEN RETURN.

    OUTPUT TO VALUE("aoaAppSrv/p" + REPLACE(aoaTitle," ","") + ".i").
    PUT UNFORMATTED
        "/* p" REPLACE(aoaTitle," ","") ".i - auto generated "
        STRING(TODAY,"99.99.9999") " @ " STRING(TIME,"hh:mm:ss am")
        " from aoa/aoaParam.w */"
        SKIP(1)
        "    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO." SKIP
        "    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO." SKIP
        "    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO." SKIP(1)
        "    /* parameter values loaded into these variables */" SKIP
        .
    
    fGenerateInclude(hFrame,"DefVar").
    
    PUT UNFORMATTED
        "    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO." SKIP
        "    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO." SKIP(1)
        "    /* locate parameter values record */" SKIP
        "    RUN pGetParamValues (ipcCompany, ~"" aoaProgramID "~", ipcUserID, ipiBatch)." SKIP(1)
        "    /* load parameter values from above record into variables */" SKIP
        "    ASSIGN" SKIP
        .
    
    fGenerateInclude(hFrame,"DynFunc").

    PUT UNFORMATTED
        "        cAvailableColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svAvailableColumns~")" SKIP
        "        cSelectedColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svSelectedColumns~")" SKIP
        "        ." SKIP(1)
        "    RUN pGetColumns (TEMP-TABLE tt" REPLACE(aoaTitle," ","") ":HANDLE," SKIP
        "                     cAvailableColumns," SKIP
        "                     cSelectedColumns" SKIP
        "                     )." SKIP(1)
        .

    fGenerateInclude(hFrame,"svAllGen").

    OUTPUT CLOSE.
    MESSAGE "aoaAppSrv/p" + REPLACE(aoaTitle," ","") + ".i" SKIP(1)
        "View Generated Code?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        TITLE "Auto Generated"
        UPDATE viewCode AS LOGICAL
        .
    IF viewCode THEN
    OS-COMMAND NO-WAIT notepad.exe VALUE("aoaAppSrv/p" + REPLACE(aoaTitle," ","") + ".i").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetColumns W-Win 
PROCEDURE pGetColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hTable AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.
    
    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        ASSIGN
            hTable = hTable:DEFAULT-BUFFER-HANDLE
            svAvailableColumns:LIST-ITEM-PAIRS = ?
            svSelectedColumns:LIST-ITEM-PAIRS = ?
            .
        DO idx = 1 TO hTable:NUM-FIELDS:
            IF CAN-DO("RECID,ROWID",hTable:BUFFER-FIELD(idx):DATA-TYPE) THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME BEGINS "xx" THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "rowType" THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "parameters" THEN NEXT.
            cRowType = cRowType + "|" + hTable:BUFFER-FIELD(idx):NAME.
            svAvailableColumns:ADD-LAST(hTable:BUFFER-FIELD(idx):LABEL,
                                        hTable:BUFFER-FIELD(idx):NAME).
        END.
        DO idx = 1 TO NUM-ENTRIES(cSelectedColumns):
            svAvailableColumns:SCREEN-VALUE = ENTRY(idx,cSelectedColumns) NO-ERROR.
            APPLY "CHOOSE":U TO btnAdd.
        END. /* do idx */
    END. /* valid happsrv */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetParamValues W-Win 
PROCEDURE pGetParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.

    DEFINE VARIABLE hFrame AS HANDLE  NO-UNDO.
    DEFINE VARIABLE hChild AS HANDLE  NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF iprRowID NE ? THEN
    FIND user-print NO-LOCK WHERE ROWID(user-print) EQ iprRowID.
    ELSE DO:
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ aoaUserID
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
    END. /* else */
    IF NOT AVAILABLE user-print THEN RETURN.

    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "" THEN LEAVE.
        IF user-print.field-name[idx] EQ "svSelectedColumns" THEN
        cSelectedColumns = user-print.field-value[idx].
    END. /* do idx */

    ASSIGN
        hChild = hFrame:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND hChild:SENSITIVE THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF TRIM(user-print.field-name[idx]) EQ hChild:NAME THEN DO:
                    hChild:SCREEN-VALUE = user-print.field-value[idx].
                    LEAVE.
                END. /* found screen object */
            END. /* do idx */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    
    ASSIGN
        hChild = FRAME frameShow:HANDLE
        hChild = hChild:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND hChild:SENSITIVE THEN DO:
            DO idx = 1 TO EXTENT(user-print.field-name):
                IF TRIM(user-print.field-name[idx]) EQ hChild:NAME THEN DO:
                    hChild:SCREEN-VALUE = user-print.field-value[idx].
                    LEAVE.
                END. /* found screen object */
            END. /* do idx */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    ASSIGN {&showFields}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrint W-Win 
PROCEDURE pGetUserPrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttUserPrint.
    EMPTY TEMP-TABLE ttParamValue.

    FOR EACH bUserPrint NO-LOCK
        WHERE bUserPrint.company    EQ aoaCompany
          AND bUserPrint.batch      EQ "Batch"
          AND bUserPrint.batch-seq  GT 0
          AND bUserPrint.program-id EQ aoaProgramID
          AND bUserPrint.user-id    EQ USERID("NoSweat")
        :
        CREATE ttUserPrint.
        BUFFER-COPY bUserPrint TO ttUserPrint.
        ttUserPrint.UserPrintRowID = ROWID(bUserPrint).
        DO idx = 1 TO EXTENT(bUserPrint.field-name):
            IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
            IF bUserPrint.field-name[idx] EQ "svTitle" THEN LEAVE.
            CREATE ttParamValue.
            ASSIGN
                ttParamValue.paramOrder = idx
                ttParamValue.batch-seq  = bUserPrint.batch-seq
                ttParamValue.paramLabel = bUserPrint.field-label[idx]
                ttParamValue.paramValue = bUserPrint.field-value[idx]
                .
        END. /* do idx */
    END. /* each buserprint */

    {&OPEN-QUERY-browseUserPrint}
    
    APPLY "VALUE-CHANGED":U TO BROWSE browseUserPrint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMoveColumn W-Win 
PROCEDURE pMoveColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcMove AS CHARACTER NO-UNDO.

    DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.
    DEFINE VARIABLE pos    AS INTEGER NO-UNDO.
    DEFINE VARIABLE idx    AS INTEGER NO-UNDO.

    IF svSelectedColumns:SCREEN-VALUE IN FRAME frameColumns EQ ? THEN RETURN.

    ASSIGN
        idx    = svSelectedColumns:LOOKUP(svSelectedColumns:SCREEN-VALUE)
        pos    = IF ipcMove EQ "Down" THEN idx + 1 ELSE idx - 1
        ldummy = IF ipcMove EQ "Down" THEN svSelectedColumns:INSERT(
            ENTRY((svSelectedColumns:LOOKUP(svSelectedColumns:SCREEN-VALUE) * 2) - 1,
                   svSelectedColumns:LIST-ITEM-PAIRS),
                   svSelectedColumns:SCREEN-VALUE,pos + 1)
                                      ELSE svSelectedColumns:INSERT(
            ENTRY((svSelectedColumns:LOOKUP(svSelectedColumns:SCREEN-VALUE) * 2) - 1,
                   svSelectedColumns:LIST-ITEM-PAIRS),
                   svSelectedColumns:SCREEN-VALUE,pos)
        ldummy = IF ipcMove EQ "Down" THEN svSelectedColumns:DELETE(idx)
                                      ELSE svSelectedColumns:DELETE(idx + 1)
        svSelectedColumns:SCREEN-VALUE = svSelectedColumns:ENTRY(pos)
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveParamValues W-Win 
PROCEDURE pSaveParamValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iplBatch   AS LOGICAL NO-UNDO.
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hFrame   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hChild   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cnt      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cColumns AS CHARACTER NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF iplBatch THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company    = aoaCompany
            user-print.program-id = aoaProgramID
            user-print.user-id    = aoaUserID
            user-print.batch      = "Batch"
            .
    END. /* not avail user-print */
    ELSE IF NOT iplBatch THEN DO:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ aoaUserID
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.company    = aoaCompany
                user-print.program-id = aoaProgramID
                user-print.user-id    = aoaUserID
                .
        END. /* not avail */
    END. /* not batch, view now request */

    ASSIGN
        user-print.field-name  = ""
        user-print.field-value = ""
        user-print.field-label = ""
        hChild = hFrame:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND
           hChild:SENSITIVE AND
           hChild:TYPE NE "Button" THEN
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = hChild:NAME
            user-print.field-label[idx] = hChild:LABEL
            user-print.field-value[idx] = hChild:SCREEN-VALUE
            .
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */

    ASSIGN
        idx = idx + 1
        user-print.field-name[idx]  = "svTitle"
        user-print.field-label[idx] = "Title"
        user-print.field-value[idx] = aoaTitle
        .
    
    IF aoaColumns THEN DO WITH FRAME frameColumns:
        DO cnt = 1 TO svAvailableColumns:NUM-ITEMS:
            cColumns = cColumns + svAvailableColumns:ENTRY(cnt) + ",".
        END. /* do cnt */
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = svAvailableColumns:NAME
            user-print.field-label[idx] = svAvailableColumns:LABEL
            user-print.field-value[idx] = TRIM(cColumns,",")
            cColumns = ""
            .
        DO cnt = 1 TO svSelectedColumns:NUM-ITEMS:
            cColumns = cColumns + svSelectedColumns:ENTRY(cnt) + ",".
        END. /* do cnt */
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = svSelectedColumns:NAME
            user-print.field-label[idx] = svSelectedColumns:LABEL
            user-print.field-value[idx] = TRIM(cColumns,",")
            .
    END. /* aoacolumns */
    
    ASSIGN
        hChild = FRAME frameShow:HANDLE
        hChild = hChild:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = hChild:NAME
            user-print.field-label[idx] = hChild:LABEL
            user-print.field-value[idx] = hChild:SCREEN-VALUE
            .
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSchedule W-Win 
PROCEDURE pSchedule :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBatchSeq AS INTEGER NO-UNDO.
    
    FOR EACH user-print NO-LOCK
        WHERE user-print.company EQ aoaCompany
           BY user-print.batch-seq DESCENDING :
        iBatchSeq = user-print.batch-seq.
        LEAVE.
    END. /* each user-print */

    RUN pSaveParamValues (YES, BUFFER user-print).

    ASSIGN
        user-print.batch-seq    = iBatchSeq + 1
        user-print.prog-title   = aoaTitle
        user-print.frequency    = ""
        user-print.next-date    = ?
        user-print.next-time    = 0
        user-print.last-date    = TODAY
        user-print.last-time    = TIME
        .

    FIND FIRST reftable
         WHERE reftable.reftable EQ "aoaReport"
           AND reftable.code     EQ cProgramID
         NO-ERROR.
    IF NOT AVAILABLE reftable THEN DO:
         CREATE reftable.
         ASSIGN
             reftable.reftable = "aoaReport"
             reftable.code     = cProgramID
             reftable.code2    = aoaProgramID
             .
    END. /* not avail */
    ASSIGN reftable.dscr = aoaID.

    MESSAGE
        "Parameters created for ..." SKIP(1)
        "Company:" aoaCompany "- Batch ID:" user-print.batch-seq
        VIEW-AS ALERT-BOX TITLE "Advantzware OA Scheduler".

    RUN pGetUserPrint.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetWinSize W-Win 
PROCEDURE pSetWinSize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hParamFrame    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iColumnsHeight AS INTEGER NO-UNDO.
    DEFINE VARIABLE iColumnsWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iDiff          AS INTEGER NO-UNDO.
    DEFINE VARIABLE lReport        AS LOGICAL NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    ASSIGN
        hParamFrame = WIDGET-HANDLE(RETURN-VALUE)
        lReport     = aoaType EQ "report"
        lReport     = NO /* disable schedule frame */
        .

    IF NOT VALID-HANDLE(hParamFrame) THEN RETURN.

    DO WITH FRAME {&FRAME-NAME}:
        IF aoaColumns THEN
        ASSIGN
            iColumnsHeight   = FRAME frameColumns:HEIGHT-PIXELS + 5
            iColumnsWidth    = FRAME frameColumns:WIDTH-PIXELS + 5
                             + FRAME frameShow:WIDTH-PIXELS + 5
                             + BROWSE browseParamValue:WIDTH-PIXELS + 5
            .

        ASSIGN
            {&WINDOW-NAME}:HEIGHT-PIXELS              = MAXIMUM(iColumnsHeight,
                                                                hParamFrame:HEIGHT-PIXELS + 5 + btnView:HEIGHT-PIXELS + 5)
            {&WINDOW-NAME}:WIDTH-PIXELS               = hParamFrame:WIDTH-PIXELS + 5 + iColumnsWidth
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS      = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS       = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:WIDTH-PIXELS          = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS         = {&WINDOW-NAME}:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            btnScheduler:Y                            = hParamFrame:HEIGHT-PIXELS + 5
            btnCancel:Y                               = hParamFrame:HEIGHT-PIXELS + 5
            btnView:Y                                 = hParamFrame:HEIGHT-PIXELS + 5
            iDiff                                     = hParamFrame:WIDTH-PIXELS - btnView:WIDTH-PIXELS * 2 - 5
            btnView:X                                 = btnView:X + iDiff
            btnCancel:X                               = btnCancel:X + iDiff
            .
    END. /* with frame  */

    IF aoaColumns THEN
    ASSIGN
        FRAME frameColumns:HEIGHT-PIXELS      = MAXIMUM(hParamFrame:HEIGHT-PIXELS,FRAME frameColumns:HEIGHT-PIXELS) + btnView:HEIGHT-PIXELS + 5
        svAvailableColumns:HEIGHT-PIXELS      = FRAME frameColumns:HEIGHT-PIXELS - 40
        svSelectedColumns:HEIGHT-PIXELS       = svAvailableColumns:HEIGHT-PIXELS
        FRAME frameColumns:X                  = hParamFrame:WIDTH-PIXELS + 5
        FRAME frameColumns:Y                  = hParamFrame:Y
        FRAME frameColumns:HIDDEN             = FALSE
        FRAME frameShow:X                     = hParamFrame:WIDTH-PIXELS + 5
                                              + FRAME frameColumns:WIDTH-PIXELS + 5
        FRAME frameShow:Y                     = hParamFrame:Y
        FRAME frameShow:HIDDEN                = FALSE
        BROWSE browseUserPrint:X              = FRAME frameShow:X
        BROWSE browseUserPrint:Y              = FRAME frameShow:HEIGHT-PIXELS + 5
        BROWSE browseUserPrint:HEIGHT-PIXELS  = FRAME frameColumns:HEIGHT-PIXELS - FRAME frameShow:HEIGHT-PIXELS - 33
        BROWSE browseUserPrint:HIDDEN         = FALSE
        BROWSE browseParamValue:X             = FRAME frameShow:X + FRAME frameShow:WIDTH-PIXELS + 5
        BROWSE browseParamValue:HEIGHT-PIXELS = FRAME frameColumns:HEIGHT-PIXELS
        BROWSE browseParamValue:HIDDEN        = FALSE
        btnDelete:X                           = BROWSE browseUserPrint:X
        btnDelete:Y                           = btnView:Y
        btnApply:X                            = btnDelete:X + btnDelete:WIDTH-PIXELS + 5
        btnApply:Y                            = btnView:Y
        btnSave:X                             = btnApply:X + btnApply:WIDTH-PIXELS + 5
        btnSave:Y                             = btnView:Y
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pURL W-Win 
PROCEDURE pURL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE fieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE testDate   AS DATE      NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.

    aoaURL = "http://" + aoaHost + ":80/AdvantzwareOA/"
           + aoaType + ".html?ID=" + aoaID
           + "^&svCompany=" + aoaCompany
           + "^&svBatch=0"
           + "^&svUserID=" + aoaUserID
           .
    /* used to extract each parameter value & add to URL 
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] NE "" THEN DO:
            fieldValue = user-print.field-value[idx].
            /* check if a date, pass DATE function in URL */
            IF INDEX(fieldValue,"/") NE 0 THEN DO:
                testDate = DATE(fieldValue) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN
                fieldValue = "DATE("
                           + REPLACE(STRING(DATE(fieldValue),"99/99/9999"),"/",",")
                           + ")"
                           .
            END. /* if / exists */
            aoaURL = aoaURL + "^&"
                   + user-print.field-name[idx] + "="
                   + fieldValue
                   .
        END. /* field-name ne '' */
    END. /* do idx */
    */
    
    IF aoaType EQ "Report" THEN
    ASSIGN aoaURL = aoaURL + "^&refresh=true^&connection=AdvantzwareOA".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateOptions W-Win 
FUNCTION fDateOptions RETURNS LOGICAL (ipDateOption AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dateOptions AS CHARACTER NO-UNDO INITIAL
"Fixed date~
,Current date~
,Start of this month~
,End of this month~
,First day of last month~
,Last day of last month~
,Start of this year~
,End of this year~
,First day of last year~
,Last day of last year~
,Last Sunday~
,Last Monday~
,Last Tuesday~
,Last Wednesday~
,Last Thursday~
,Last Friday~
,Last Saturday~
,Next Sunday~
,Next Monday~
,Next Tuesday~
,Next Wednesday~
,Next Thursday~
,Next Friday~
,Next Saturday~
".
    ASSIGN
        ipDateOption:LIST-ITEMS   = dateOptions
        ipDateOption:INNER-LINES  = NUM-ENTRIES(dateOptions)
        ipDateOption:SCREEN-VALUE = ipDateOption:ENTRY(1)
        .

  RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGenerateInclude W-Win 
FUNCTION fGenerateInclude RETURNS LOGICAL
  ( iphFrame AS HANDLE, ipcType AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hChild      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hRange      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAllRange   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartRange AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndRange   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPreFix     AS CHARACTER NO-UNDO INITIAL "c,d,dt,i,l".
    DEFINE VARIABLE cTypes      AS CHARACTER NO-UNDO INITIAL "CHARACTER,DECIMAL,DATE,INTEGER,LOGICAL".
    DEFINE VARIABLE cStartList  AS CHARACTER NO-UNDO INITIAL "CHR(32),0,1/1/1950,0".
    DEFINE VARIABLE cEndList    AS CHARACTER NO-UNDO INITIAL "CHR(255),99999999.99,12/31/2049,99999999".
    DEFINE VARIABLE cStartValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList   AS LOGICAL   NO-UNDO.

    ASSIGN
        hChild = iphFrame:FIRST-CHILD
        hChild = hChild:FIRST-CHILD
        .
    DO WHILE VALID-HANDLE(hChild):
        IF hChild:NAME NE ? AND hChild:SENSITIVE THEN DO:
            IF hChild:TYPE NE "Button" THEN DO:
                IF hChild:NAME NE "svCompany" THEN DO:
                    idx = LOOKUP(hChild:DATA-TYPE,cTypes).
                    CASE ipcType:
                        WHEN "DefVar" THEN
                        PUT UNFORMATTED
                            "    DEFINE VARIABLE " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix))
                            " AS " hChild:DATA-TYPE " NO-UNDO."
                            SKIP
                            .
                        WHEN "DynFunc" THEN DO:
                            PUT UNFORMATTED
                                "        " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix))
                                " = "
                                IF ENTRY(idx,cPreFix) EQ "dt" THEN "DATE(" ELSE ""
                                "DYNAMIC-FUNCTION(~"fGetParamValue~",~"" hChild:NAME "~")"
                                IF ENTRY(idx,cPreFix) EQ "dt" THEN ")" ELSE ""
                                IF ENTRY(idx,cPreFix) EQ "l" THEN " EQ ~"yes~"" ELSE ""
                                SKIP
                                .
                            IF INDEX(hChild:NAME,"DateOption") NE 0 THEN
                            PUT UNFORMATTED
                                "        "
                                REPLACE(REPLACE(hChild:NAME,"sv","dt"),"Option","")
                                " = DYNAMIC-FUNCTION(~"fDateOptionDate~","
                                REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix)) ","
                                REPLACE(REPLACE(hChild:NAME,"sv","dt"),"Option","") ")"
                                SKIP
                                .
                        END. /* dynfunc */
                        WHEN "svAllGen" THEN DO:
                            IF hChild:NAME BEGINS "svAll" THEN DO:
                                ASSIGN
                                    cAllRange = REPLACE(hChild:NAME,"svAll","")
                                    hRange    = iphFrame:FIRST-CHILD
                                    hRange    = hRange:FIRST-CHILD
                                    .
                                DO WHILE VALID-HANDLE(hRange):
                                    IF hRange:NAME NE ? AND hRange:SENSITIVE THEN DO:
                                        IF hRange:TYPE NE "Button" THEN DO:
                                            IF hRange:NAME NE "svCompany" THEN DO:
                                                IF hRange:NAME EQ "svStart" + cAllRange THEN
                                                ASSIGN
                                                    idx = LOOKUP(hRange:DATA-TYPE,cTypes)
                                                    cStartRange = REPLACE(hRange:NAME,"sv",ENTRY(idx,cPreFix))
                                                    cStartValue = ENTRY(idx,cStartList)
                                                    .
                                                IF hRange:NAME EQ "svEnd" + cAllRange THEN
                                                ASSIGN
                                                    idx = LOOKUP(hRange:DATA-TYPE,cTypes)
                                                    cEndRange = REPLACE(hRange:NAME,"sv",ENTRY(idx,cPreFix))
                                                    cEndValue = ENTRY(idx,cEndList)
                                                    .
                                            END. /* not svcompany */
                                        END. /* not button */
                                    END. /* name <> ? */
                                    hRange = hRange:NEXT-SIBLING.
                                END. /* do while */
                                idx = LOOKUP(hChild:DATA-TYPE,cTypes).
                                PUT UNFORMATTED
                                    "    IF " REPLACE(hChild:NAME,"sv",ENTRY(idx,cPreFix)) " THEN" SKIP
                                    "    ASSIGN" SKIP
                                    "        " cStartRange " = " cStartValue SKIP
                                    "        " cEndRange "   = " cEndValue   SKIP
                                    "        ." SKIP(1)
                                    .
                            END. /* if svall */
                            IF hChild:NAME EQ "svCustList" THEN
                            lCustList = YES.
                        END.
                    END CASE.
                END. /* not svcompany */
            END. /* not button */
        END. /* name <> ? */
        hChild = hChild:NEXT-SIBLING.
    END. /* do while */
    IF lCustList THEN
    PUT UNFORMATTED
        "    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, ~"" aoaCustListForm "~")." SKIP
        .
    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCompany W-Win 
FUNCTION fGetCompany RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN aoaCompany.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetShowAll W-Win 
FUNCTION fSetShowAll RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DO WITH FRAME frameShow:
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

