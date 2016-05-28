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

DEFINE VARIABLE hAppSrv          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterList     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinterName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPrinterCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPrtCmd          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrtPort         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRowType         AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.
DEFINE VARIABLE lShowBatchObjs   AS LOGICAL   NO-UNDO INITIAL YES.

DEFINE TEMP-TABLE ttUserPrint NO-UNDO LIKE user-print
    FIELD UserPrintRowID AS ROWID.

DEFINE TEMP-TABLE ttParamValue NO-UNDO
    FIELD paramOrder AS INTEGER
    FIELD batch-seq  AS INTEGER
    FIELD paramLabel AS CHARACTER LABEL "Param Label" FORMAT "x(31)"
    FIELD paramValue AS CHARACTER LABEL "Param Value" FORMAT "x(30)"
        INDEX paramOrder IS PRIMARY paramOrder
    .

IF aoaColumns THEN DO:
    RUN aoaAppSrv\aoaBin.p PERSISTENT SET hAppSrv.
    SESSION:ADD-SUPER-PROCEDURE (hAppSrv).
    RUN VALUE("aoaAppSrv/" + ENTRY(1,aoaParam,"/") + ".p") PERSISTENT SET hAppSrv.
END.

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
&Scoped-Define ENABLED-OBJECTS btnCancel btnView 

/* Custom List Definitions                                              */
/* ScheduleFields,showFields,batchObjects,batchShowHide,List-5,List-6   */
&Scoped-define showFields svShowAll svShowReportHeader svShowParameters ~
svShowPageHeader svShowGroupHeader svShowGroupFooter svShowPageFooter ~
svShowReportFooter 
&Scoped-define batchObjects btnShowBatch btnExcel btnDelete btnApply ~
btnSave browseUserPrint browseParamValue 
&Scoped-define batchShowHide btnDelete btnApply btnSave browseUserPrint ~
browseParamValue 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fSetDescription W-Win 
FUNCTION fSetDescription RETURNS CHARACTER
  ( ipObject AS HANDLE )  FORWARD.

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
     IMAGE-UP FILE "aoa/images/aoaadd.jpg":U
     LABEL "&Add" 
     SIZE 4.4 BY 1 TOOLTIP "Add Available Column to Selected Columns".

DEFINE BUTTON btnDefault 
     IMAGE-UP FILE "aoa/images/aoadefault.jpg":U
     LABEL "&Default" 
     SIZE 4.4 BY 1 TOOLTIP "Reset Selected Columns to Default".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "aoa/images/aoadown.jpg":U
     LABEL "Move Down" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "aoa/images/aoaup.jpg":U
     LABEL "Move Up" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Up".

DEFINE BUTTON btnRemove 
     IMAGE-UP FILE "aoa/images/aoacancel.jpg":U
     LABEL "<< &Remove" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Selected Column".

DEFINE VARIABLE svAvailableColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 30 BY 6.91 NO-UNDO.

DEFINE VARIABLE svSelectedColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 30 BY 6.91 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL no 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL no 
     LABEL "Group Footer (SubTotals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL no 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL no 
     LABEL "Page Footer (Date / Page No.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL no 
     LABEL "Page Header (Column Headers)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL no 
     LABEL "Parameters (Report Header)" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL no 
     LABEL "Report Footer (Grand Totals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL no 
     LABEL "Report Header (Report Title)" 
     VIEW-AS TOGGLE-BOX
     SIZE 34 BY .81 NO-UNDO.

DEFINE BUTTON btnApply 
     IMAGE-UP FILE "aoa/images/aoaapply.jpg":U
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Batch Values to Parameter Values".

DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "aoa/images/aoaclose.jpg":U
     LABEL "&Cancel" 
     SIZE 4.4 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "aoa/images/aoacancel.jpg":U
     LABEL "&Delete" 
     SIZE 4.4 BY 1 TOOLTIP "Delete Batch ID".

DEFINE BUTTON btnExcel 
     IMAGE-UP FILE "aoa/images/aoaexcel.jpg":U
     LABEL "&Excel" 
     SIZE 4.4 BY 1 TOOLTIP "Export to Excel".

DEFINE BUTTON btnSave 
     IMAGE-UP FILE "aoa/images/aoasave.jpg":U
     LABEL "&Save" 
     SIZE 4.4 BY 1 TOOLTIP "Save Parameter Values to Batch ID".

DEFINE BUTTON btnScheduler 
     LABEL "Assign &Scheduler Batch ID" 
     SIZE 27 BY 1 TOOLTIP "Assign Batch ID to Parameter Values".

DEFINE BUTTON btnShowBatch 
     IMAGE-UP FILE "aoa/images/aoashowbatch.jpg":U
     LABEL "&ShowBatch" 
     SIZE 4.4 BY 1 TOOLTIP "Show Batch Parameter Values".

DEFINE BUTTON btnView 
     IMAGE-UP FILE "aoa/images/aoaview.jpg":U
     LABEL "&View" 
     SIZE 4.4 BY 1 TOOLTIP "View".

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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 6.19
         TITLE "Batch Parameter Values".

DEFINE BROWSE browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseUserPrint W-Win _FREEFORM
  QUERY browseUserPrint DISPLAY
      ttUserPrint.batch-seq LABEL "Batch Seq"
      ttUserPrint.last-date LABEL "Date"
      STRING(ttUserPrint.last-time,"hh:mm:ss am") LABEL "Time" FORMAT "x(12)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 6.19
         TITLE "Batch Parameter".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     btnScheduler AT ROW 2.43 COL 2 HELP
          "Assign Batch ID to Parameter Values" WIDGET-ID 10
     btnShowBatch AT ROW 2.43 COL 30 HELP
          "Show Batch Parameter Values" WIDGET-ID 20
     btnExcel AT ROW 2.43 COL 35 HELP
          "Export to Excel" WIDGET-ID 22
     btnCancel AT ROW 3.62 COL 2 HELP
          "Close" WIDGET-ID 12
     btnView AT ROW 3.62 COL 7 HELP
          "View" WIDGET-ID 14
     btnDelete AT ROW 3.62 COL 25 HELP
          "Delete Batch ID" WIDGET-ID 4
     btnApply AT ROW 3.62 COL 30 HELP
          "Apply Batch Values to Parameter Values" WIDGET-ID 16
     btnSave AT ROW 3.62 COL 35 HELP
          "Save Parameter Values to Batch ID" WIDGET-ID 18
     browseUserPrint AT ROW 10.05 COL 41 WIDGET-ID 500
     browseParamValue AT ROW 10.05 COL 82 WIDGET-ID 600
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149 BY 15.5.

DEFINE FRAME frameColumns
     svAvailableColumns AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 68
     btnDefault AT ROW 1.71 COL 32 HELP
          "Reset Selected Columns to Default" WIDGET-ID 76
     svSelectedColumns AT ROW 1.71 COL 37 NO-LABEL WIDGET-ID 70
     btnAdd AT ROW 2.91 COL 32 HELP
          "Add Available Column to Selected Columns" WIDGET-ID 58
     btnMoveUp AT ROW 5.29 COL 32 HELP
          "Move Selected Column Up" WIDGET-ID 66
     btnRemove AT ROW 6.48 COL 32 HELP
          "Remove Selected Column" WIDGET-ID 64
     btnMoveDown AT ROW 7.67 COL 32 HELP
          "Move Selected Column Down" WIDGET-ID 62
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1 COL 2 WIDGET-ID 74
     "Selected Columns (In Order)" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1 COL 37 WIDGET-ID 72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 82 ROW 1
         SIZE 67 BY 8.81
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
         AT COL 41 ROW 1
         SIZE 40 BY 8.81
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
         HEIGHT             = 15.52
         WIDTH              = 149
         MAX-HEIGHT         = 15.52
         MAX-WIDTH          = 149
         VIRTUAL-HEIGHT     = 15.52
         VIRTUAL-WIDTH      = 149
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
/* BROWSE-TAB browseUserPrint btnSave paramFrame */
/* BROWSE-TAB browseParamValue browseUserPrint paramFrame */
/* SETTINGS FOR BROWSE browseParamValue IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       browseParamValue:HIDDEN  IN FRAME paramFrame                = TRUE.

/* SETTINGS FOR BROWSE browseUserPrint IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       browseUserPrint:HIDDEN  IN FRAME paramFrame                = TRUE.

/* SETTINGS FOR BUTTON btnApply IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnApply:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnCancel:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnDelete IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnDelete:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnExcel IN FRAME paramFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       btnExcel:HIDDEN IN FRAME paramFrame           = TRUE
       btnExcel:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnSave IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnSave:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnScheduler IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnScheduler:HIDDEN IN FRAME paramFrame           = TRUE
       btnScheduler:PRIVATE-DATA IN FRAME paramFrame     = 
                "WinKitRibbon".

/* SETTINGS FOR BUTTON btnShowBatch IN FRAME paramFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       btnShowBatch:HIDDEN IN FRAME paramFrame           = TRUE
       btnShowBatch:PRIVATE-DATA IN FRAME paramFrame     = 
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
ON VALUE-CHANGED OF browseUserPrint IN FRAME paramFrame /* Batch Parameter */
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


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel W-Win
ON CHOOSE OF btnExcel IN FRAME paramFrame /* Excel */
DO:
    RUN pExcel (BUFFER user-print).
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


&Scoped-define SELF-NAME btnShowBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnShowBatch W-Win
ON CHOOSE OF btnShowBatch IN FRAME paramFrame /* ShowBatch */
DO:
    RUN pShowBatchObjs.
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
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME frameShow /* Group Footer (SubTotals) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowGroupHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowGroupHeader W-Win
ON VALUE-CHANGED OF svShowGroupHeader IN FRAME frameShow /* Group Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageFooter W-Win
ON VALUE-CHANGED OF svShowPageFooter IN FRAME frameShow /* Page Footer (Date / Page No.) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader W-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME frameShow /* Page Header (Column Headers) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters W-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME frameShow /* Parameters (Report Header) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter W-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME frameShow /* Report Footer (Grand Totals) */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader W-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME frameShow /* Report Header (Report Title) */
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
             FRAME frameShow:HANDLE , 'BEFORE':U ).
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
  ENABLE btnCancel btnView 
      WITH FRAME paramFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  DISPLAY svShowAll svShowReportHeader svShowParameters svShowPageHeader 
          svShowGroupHeader svShowGroupFooter svShowPageFooter 
          svShowReportFooter 
      WITH FRAME frameShow IN WINDOW W-Win.
  ENABLE svShowAll svShowReportHeader svShowParameters svShowPageHeader 
         svShowGroupHeader svShowGroupFooter svShowPageFooter 
         svShowReportFooter 
      WITH FRAME frameShow IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameShow}
  DISPLAY svAvailableColumns svSelectedColumns 
      WITH FRAME frameColumns IN WINDOW W-Win.
  ENABLE svAvailableColumns btnDefault svSelectedColumns btnAdd btnMoveUp 
         btnRemove btnMoveDown 
      WITH FRAME frameColumns IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameColumns}
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
  RUN pShowBatchObjs.
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcel W-Win 
PROCEDURE pExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColumns    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE fieldName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iColumn     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iRow        AS INTEGER    NO-UNDO.
    DEFINE VARIABLE hQuery      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE hQueryBuf   AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cDynFunc    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cExcelFile  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cDataType   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFormat     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE chExcel     AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorksheet AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chWorkBook  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chRangeRow  AS COM-HANDLE NO-UNDO.
    DEFINE VARIABLE chRangeCol  AS COM-HANDLE NO-UNDO.
    
    RUN pSaveParamValues (NO, BUFFER user-print).

    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        ASSIGN
            cExcelFile = "aoaExcel\.keep"
            FILE-INFO:FILE-NAME = cExcelFile
            cExcelFile = FILE-INFO:FULL-PATHNAME
            cExcelFile = REPLACE(cExcelFile,".keep",aoaTitle + " (")
                       + USERID("NoSweat") + ").xls"
            .

        IF SEARCH(cExcelFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(cExcelFile)).

        /* Connect to the running Excel session. */
        CREATE "Excel.Application" chExcel CONNECT NO-ERROR.
        /* Start a new session of Excel. */
        IF NOT VALID-HANDLE(chExcel) THEN
        CREATE "Excel.Application" chExcel NO-ERROR.
        /* Check if Excel got initialized. */
        IF NOT VALID-HANDLE(chExcel) THEN DO:
            MESSAGE "Unable to Start Excel" VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
        /* Open our Excel Template. */
        chWorkbook = chExcel:Workbooks:Open(cExcelFile) NO-ERROR.
        chExcel:Visible = TRUE.
        /* Do not display Excel error messages. */
        chExcel:DisplayAlerts = FALSE NO-ERROR.
        ASSIGN
            chExcel:SheetsInNewWorkbook = 1
            chWorkbook  = chExcel:Workbooks:Add()
            chWorksheet = chWorkbook:Worksheets(1)
            .
        chWorkbook:Worksheets:Add(,chWorksheet).
        RELEASE OBJECT chWorksheet.

        /* Select a worksheet */
        chWorkbook:Worksheets(1):Activate.
        ASSIGN
            chWorksheet = chWorkbook:Worksheets(1)
            /* Rename the worksheet */
            chWorkSheet:Name = aoaTitle
            /* Disable screen updating so it will go faster */
            chExcel:ScreenUpdating = TRUE
            chWorkSheet:Cells(3,2):Value = "Running Query..."
            .
        /* remove spare worksheet */
        chWorkbook:WorkSheets(2):DELETE NO-ERROR.
        
        ASSIGN
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany,0,USERID("NoSweat")).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        ASSIGN
            hTable = hTable:DEFAULT-BUFFER-HANDLE
            iRow   = 1.

        /* build header row column labels */
        DO iColumn = 1 TO svSelectedColumns:NUM-ITEMS:
            ASSIGN
                chWorkSheet:Cells(3,2):Value = "Running Query...Done"
                chWorkSheet:Cells(5,2):Value = "Formatting Cells..."
                fieldName = svSelectedColumns:ENTRY(iColumn)
                cDataType = hTable:BUFFER-FIELD(fieldName):DATA-TYPE
                /* align left (-4131) or right (-4152) */
                chWorkSheet:Cells(iRow,iColumn):HorizontalAlignment = IF cDataType EQ "Character" THEN -4131 ELSE -4152
                /* column label */
                chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):LABEL
                chRangeRow = chWorkSheet:Cells(2,iColumn)
                chRangeCol = chWorkSheet:Cells(65536,iColumn)
                .
            /* apply column format based on data type */
            CASE cDataType:
                WHEN "Character" THEN
                ASSIGN
                    chWorkSheet:Range(chRangeRow,chRangeCol):HorizontalAlignment = -4131
                    chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "General"
                    .
                WHEN "Date" THEN
                chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = "mm/dd/yyyy".
                WHEN "Integer" OR WHEN "Decimal" THEN DO:
                    ASSIGN
                        cFormat = hTable:BUFFER-FIELD(fieldName):FORMAT
                        cFormat = REPLACE(cFormat,">","#")
                        cFormat = REPLACE(cFormat,"9","0")
                        .
                    IF INDEX(cFormat,"-") NE 0 THEN
                    ASSIGN
                        cFormat = REPLACE(cFormat,"-","")
                        cFormat = cFormat + "_);[Red](" + cFormat + ")"
                        .
                    chWorkSheet:Range(chRangeRow,chRangeCol):NumberFormat = cFormat.
                END. /* integer/decimal */
            END CASE.
        END. /* do iColumn */
        
        /* bold and underline header row */
        ASSIGN
            chRangeRow = chWorkSheet:Cells(iRow,1)
            chRangeCol = chWorkSheet:Cells(iRow,svSelectedColumns:NUM-ITEMS)
            chWorkSheet:Range(chRangeRow,chRangeCol):Font:Bold = TRUE
            chWorkSheet:Range(chRangeRow,chRangeCol):Font:Underline = TRUE
            chWorkSheet:Cells(5,2):Value = "Formatting Cells...Done"
            chWorkSheet:Cells(7,2):Value = "Building Wooksheet..."
            .

        /* pause to let excel display catch up */
        PAUSE 1 NO-MESSAGE.

        /* turn off display to run faster and clear status messages */
        ASSIGN
            chExcel:ScreenUpdating = FALSE
            chWorkSheet:Cells(3,2):Value = ""
            chWorkSheet:Cells(5,2):Value = ""
            chWorkSheet:Cells(7,2):Value = ""
            .
        
        /* scroll returned temp-table records */
        CREATE QUERY hQuery.
        hQuery:SET-BUFFERS(hTable:HANDLE).
        hQuery:QUERY-PREPARE("FOR EACH " + hTable:NAME).
        hQuery:QUERY-OPEN.
        hQueryBuf = hQuery:GET-BUFFER-HANDLE(hTable:NAME).
        REPEAT:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            IF hQueryBuf:BUFFER-FIELD("RowType"):BUFFER-VALUE() NE "Data" THEN NEXT.
            iRow = iRow + 1.
            DO iColumn = 1 TO svSelectedColumns:NUM-ITEMS:
                ASSIGN
                    fieldName = svSelectedColumns:ENTRY(iColumn)
                    chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE()
                    .
            END. /* do iColumn */
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        
        /* select everything */
        chWorksheet:Columns("A:ZZ"):Select.
        /* auto size the columns */
        chExcel:Selection:Columns:AutoFit.
        /* select first none header cell */
        chWorksheet:Range("A2"):Select.
        /* enable screen updating */
        chExcel:ScreenUpdating = TRUE.
        /* auto save excel file */
        chExcel:ActiveSheet:SaveAs(cExcelFile).
        
        /* Release created objects. */
        RELEASE OBJECT chWorkbook  NO-ERROR.
        RELEASE OBJECT chWorkSheet NO-ERROR.
        RELEASE OBJECT chExcel     NO-ERROR.
    END. /* valid happsrv */

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
            CREATE ttParamValue.
            ASSIGN
                ttParamValue.paramOrder = idx
                ttParamValue.batch-seq  = bUserPrint.batch-seq
                ttParamValue.paramLabel = IF bUserPrint.field-label[idx] NE ? THEN bUserPrint.field-label[idx]
                                          ELSE "[ " + bUserPrint.field-name[idx] + " ]"
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
    DEFINE VARIABLE hParamFrame AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iWidth      AS INTEGER NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hParamFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hParamFrame) THEN RETURN.

    IF aoaColumns THEN
    iWidth = FRAME frameShow:WIDTH-PIXELS + 5
           + FRAME frameColumns:WIDTH-PIXELS + 5.
    
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            {&WINDOW-NAME}:HEIGHT-PIXELS              = hParamFrame:HEIGHT-PIXELS + 5
                                                      + btnView:HEIGHT-PIXELS + 5
            {&WINDOW-NAME}:WIDTH-PIXELS               = hParamFrame:WIDTH-PIXELS + 5 + iWidth
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS      = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS       = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:WIDTH-PIXELS          = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS         = {&WINDOW-NAME}:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            btnScheduler:Y                            = hParamFrame:HEIGHT-PIXELS + 5
            btnShowBatch:Y                            = hParamFrame:HEIGHT-PIXELS + 5
            btnExcel:Y                                = hParamFrame:HEIGHT-PIXELS + 5
            btnCancel:Y                               = hParamFrame:HEIGHT-PIXELS + 5
            btnView:Y                                 = hParamFrame:HEIGHT-PIXELS + 5
            btnShowBatch:X                            = btnScheduler:X + btnScheduler:WIDTH-PIXELS + 2
            btnView:X                                 = hParamFrame:WIDTH-PIXELS - btnView:WIDTH-PIXELS
            btnCancel:X                               = btnView:X - btnCancel:WIDTH-PIXELS - 2
            btnExcel:X                                = btnCancel:X - ((btnCancel:X
                                                      - (btnShowBatch:X + btnShowBatch:WIDTH-PIXELS)) / 2)
                                                      - btnExcel:WIDTH-PIXELS / 2
            .

        IF aoaColumns THEN DO:
            ASSIGN
                FRAME frameColumns:HEIGHT-PIXELS         = MAXIMUM(FRAME frameShow:HEIGHT-PIXELS,
                                                                   hParamFrame:HEIGHT-PIXELS / 2)
                FRAME frameColumns:VIRTUAL-HEIGHT-PIXELS = FRAME frameColumns:HEIGHT-PIXELS
                svAvailableColumns:HEIGHT-PIXELS         = FRAME frameColumns:HEIGHT-PIXELS - 40
                svSelectedColumns:HEIGHT-PIXELS          = svAvailableColumns:HEIGHT-PIXELS
                FRAME frameShow:X                        = hParamFrame:WIDTH-PIXELS + 5
                FRAME frameShow:Y                        = hParamFrame:Y
                FRAME frameShow:HIDDEN                   = FALSE
                FRAME frameColumns:X                     = hParamFrame:WIDTH-PIXELS + 5
                                                         + FRAME frameShow:WIDTH-PIXELS + 5
                FRAME frameColumns:Y                     = hParamFrame:Y
                FRAME frameColumns:HIDDEN                = FALSE
                BROWSE browseParamValue:X                = FRAME frameColumns:X
                BROWSE browseParamValue:Y                = FRAME frameColumns:HEIGHT-PIXELS + 5
                BROWSE browseParamValue:HEIGHT-PIXELS    = hParamFrame:HEIGHT-PIXELS
                                                         - FRAME frameColumns:HEIGHT-PIXELS
                                                         + btnSave:HEIGHT-PIXELS
                BROWSE browseParamValue:HIDDEN           = FALSE
                BROWSE browseUserPrint:X                 = FRAME frameShow:X
                BROWSE browseUserPrint:Y                 = BROWSE browseParamValue:Y
                BROWSE browseUserPrint:HEIGHT-PIXELS     = BROWSE browseParamValue:HEIGHT-PIXELS
                                                         - btnSave:HEIGHT-PIXELS - 5
                BROWSE browseUserPrint:HIDDEN            = FALSE
                btnSave:Y                                = btnView:Y
                btnApply:Y                               = btnView:Y
                btnDelete:Y                              = btnView:Y
                btnSave:X                                = FRAME frameShow:X
                                                         + FRAME frameShow:WIDTH-PIXELS
                                                         - btnDelete:WIDTH-PIXELS - 15
                btnApply:X                               = btnSave:X - btnSave:WIDTH-PIXELS - 2
                btnDelete:X                              = btnApply:X - btnApply:WIDTH-PIXELS - 2
                .
            ENABLE {&batchObjects}.
        END. /* if aoacolumns */
    END. /* with frame  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pShowBatchObjs W-Win 
PROCEDURE pShowBatchObjs :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        lShowBatchObjs = NOT lShowBatchObjs.
        IF lShowBatchObjs THEN DO:
            FRAME frameColumns:HIDDEN = TRUE.
            RUN pSetWinSize.
        END. /* if showbatchobjs */
        ELSE DO:
            HIDE {&batchShowHide}.
            ASSIGN
                FRAME frameColumns:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 5
                svAvailableColumns:HEIGHT-PIXELS = FRAME frameColumns:HEIGHT-PIXELS - 40
                svSelectedColumns:HEIGHT-PIXELS  = svAvailableColumns:HEIGHT-PIXELS
                .
        END. /* else */
    END. /* do with frame */

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
"Fixed Date~
,Current Date~
,Current Date -1~
,Current Date +1~
,Current Date -2~
,Current Date +2~
,Current Date -3~
,Current Date +3~
,Current Date -4~
,Current Date +4~
,Current Date -5~
,Current Date +5~
,Current Date -6~
,Current Date +6~
,Start of this Month~
,End of this Month~
,First Day of Last Month~
,Last Day of Last Month~
,Start of this Year~
,End of this Year~
,First Day of Last Year~
,Last Day of Last Year~
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetDescription W-Win 
FUNCTION fSetDescription RETURNS CHARACTER
  ( ipObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRange       AS CHARACTER NO-UNDO.

    cRange = REPLACE(ipObject:NAME,"sv","").
    
    CASE ipObject:NAME:
        WHEN "svStartCustNo" OR WHEN "svEndCustNo" THEN DO:
            cRange = REPLACE(cRange,"CustNo","").
            FIND FIRST cust NO-LOCK
                 WHERE cust.company EQ aoaCompany
                   AND cust.cust-no EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE cust THEN cDescription = cust.name.
        END.
        WHEN "svStartDept" OR WHEN "svEndDept" THEN DO:
            cRange = REPLACE(cRange,"Dept","").
            FIND FIRST dept NO-LOCK
                 WHERE dept.company EQ aoaCompany
                   AND dept.code EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE dept THEN cDescription = dept.dscr.
        END.
        WHEN "svStartItemNo" OR WHEN "svEndItemNo" THEN DO:
            cRange = REPLACE(cRange,"ItemNo","").
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ aoaCompany
                   AND itemfg.i-no    EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE itemfg THEN cDescription = itemfg.i-dscr.
        END.
        WHEN "svStartMachine" OR WHEN "svEndMachine" THEN DO:
            cRange = REPLACE(cRange,"Machine","").
            FIND FIRST mach NO-LOCK
                 WHERE mach.company EQ aoaCompany
                   AND mach.m-code  EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE mach THEN cDescription = mach.m-dscr.
        END.
        WHEN "svStartProdCategory" OR WHEN "svEndProdCategory" THEN DO:
            cRange = REPLACE(cRange,"ProdCategory","").
            FIND FIRST procat NO-LOCK
                 WHERE procat.company EQ aoaCompany
                   AND procat.procat  EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE procat THEN cDescription = procat.dscr.
        END.
        WHEN "svStartSalesRep" OR WHEN "svEndSalesRep" THEN DO:
            cRange = REPLACE(cRange,"SalesRep","").
            FIND FIRST sman NO-LOCK
                 WHERE sman.company EQ aoaCompany
                   AND sman.sman EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE sman THEN cDescription = sman.sname.
        END.
        WHEN "svStartShift" OR WHEN "svEndShift" THEN DO:
            cRange = REPLACE(cRange,"Shift","").
            FIND FIRST shift NO-LOCK
                 WHERE shift.company EQ aoaCompany
                   AND shift.shift   EQ INTEGER(ipObject:SCREEN-VALUE)
                 NO-ERROR.
            IF AVAILABLE shift THEN cDescription = shift.descr.
        END.
        WHEN "svStartUserID" OR WHEN "svEndUserID" THEN DO:
            cRange = REPLACE(cRange,"UserID","").
            FIND FIRST users NO-LOCK
                 WHERE cust.company EQ aoaCompany
                   AND users.user_id EQ ipObject:SCREEN-VALUE
                 NO-ERROR.
            IF AVAILABLE users THEN cDescription = users.user_name.
        END.
    END CASE.

    IF cDescription EQ "" THEN
    cDescription = "<" + cRange + " Range Value>".

    RETURN cDescription.

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

