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
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcParamStr AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{aoa/includes/aoaParamDefs.i}

DEFINE VARIABLE hParamFrame      AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrv          AS HANDLE    NO-UNDO.
DEFINE VARIABLE hAppSrvBin       AS HANDLE    NO-UNDO.
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
DEFINE VARIABLE lSecure          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iSaveHeight      AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE ttUserPrint NO-UNDO LIKE user-print
    FIELD UserPrintRowID AS ROWID.

DEFINE TEMP-TABLE ttParamValue NO-UNDO
    FIELD paramOrder AS INTEGER
    FIELD batch-seq  AS INTEGER
    FIELD paramLabel AS CHARACTER LABEL "Param Label" FORMAT "x(31)"
    FIELD paramValue AS CHARACTER LABEL "Param Value" FORMAT "x(30)"
        INDEX paramOrder IS PRIMARY paramOrder
    .

RUN aoa\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).

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
&Scoped-Define ENABLED-OBJECTS btnExcel btnSaveParams btnCancel btnView 

/* Custom List Definitions                                              */
/* ScheduleFields,showFields,batchObjects,batchShowHide,columnObjects,List-6 */
&Scoped-define showFields svShowAll svShowReportHeader svShowParameters ~
svShowPageHeader svShowGroupHeader svShowGroupFooter svShowPageFooter ~
svShowReportFooter 
&Scoped-define batchObjects btnShowBatch btnDelete btnApply btnSave ~
browseUserPrint browseParamValue 
&Scoped-define batchShowHide btnDelete btnApply btnSave browseUserPrint ~
browseParamValue 
&Scoped-define columnObjects btnDefault btnAdd btnRemoveColumn btnMoveUp ~
btnMoveDown 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptions W-Win 
FUNCTION fDateOptions RETURNS LOGICAL (ipDateOption AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fDateOptionValue W-Win 
FUNCTION fDateOptionValue RETURNS DATE
  (ipcDateOption AS CHARACTER, ipdtDate AS DATE)  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLocation W-Win 
FUNCTION fGetLocation RETURNS CHARACTER FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetModule W-Win 
FUNCTION fGetModule RETURNS CHARACTER
  ( ipProgramID AS CHARACTER )  FORWARD.

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
     SIZE 4.4 BY 1 TOOLTIP "Add Available Column(s) to Selected Columns".

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

DEFINE BUTTON btnRemoveColumn 
     IMAGE-UP FILE "aoa/images/aoaremove.jpg":U
     LABEL "&Remove" 
     SIZE 4.4 BY 1 TOOLTIP "Remove Selected Column(s)".

DEFINE VARIABLE svAvailableColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 30 BY 8.57 NO-UNDO.

DEFINE VARIABLE svSelectedColumns AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "empty","empty" 
     SIZE 30 BY 8.57 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE    
     SIZE 38 BY 1.19
     FGCOLOR 2 .

DEFINE VARIABLE svExcelTable AS LOGICAL INITIAL no 
     LABEL "Format Data as Table (Excel Only)" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .81 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL no 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL no 
     LABEL "Group Footer (SubTotals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL no 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL no 
     LABEL "Page Footer (Date / Page No.)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL no 
     LABEL "Page Header (Column Headers)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL no 
     LABEL "Parameters (Report Header)" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL no 
     LABEL "Report Footer (Grand Totals)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL no 
     LABEL "Report Header (Report Title)" 
     VIEW-AS TOGGLE-BOX
     SIZE 35 BY .81 NO-UNDO.

DEFINE BUTTON btnApply 
     IMAGE-UP FILE "aoa/images/aoaapply.jpg":U
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Batch Values to Parameter Values".

DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "aoa/images/aoaclose.jpg":U
     LABEL "&Cancel" 
     SIZE 4.4 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "aoa/images/aoadelete.jpg":U
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

DEFINE BUTTON btnSaveParams 
     IMAGE-UP FILE "aoa/images/aoasave.jpg":U
     LABEL "Save Params" 
     SIZE 4.4 BY 1 TOOLTIP "Save Parameters".

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
    WITH NO-ROW-MARKERS SEPARATORS SIZE 67 BY 7.14
         TITLE "Batch Parameter Values".

DEFINE BROWSE browseUserPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseUserPrint W-Win _FREEFORM
  QUERY browseUserPrint DISPLAY
      ttUserPrint.batch-seq LABEL "Batch Seq"
      ttUserPrint.last-date LABEL "Date"
      STRING(ttUserPrint.last-time,"hh:mm:ss am") LABEL "Time" FORMAT "x(12)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 40 BY 7.14
         TITLE "Batch Parameter".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     btnScheduler AT ROW 2.43 COL 2 HELP
          "Assign Batch ID to Parameter Values" WIDGET-ID 10
     btnShowBatch AT ROW 2.43 COL 30 HELP
          "Show Batch Parameter Values" WIDGET-ID 20
     btnExcel AT ROW 2.43 COL 35 HELP
          "Export to Excel" WIDGET-ID 22
     btnSaveParams AT ROW 3.62 COL 2 HELP
          "Save Parameters" WIDGET-ID 24
     btnCancel AT ROW 3.62 COL 7 HELP
          "Close" WIDGET-ID 12
     btnView AT ROW 3.62 COL 12 HELP
          "View" WIDGET-ID 14
     btnDelete AT ROW 3.62 COL 25 HELP
          "Delete Batch ID" WIDGET-ID 4
     btnApply AT ROW 3.62 COL 30 HELP
          "Apply Batch Values to Parameter Values" WIDGET-ID 16
     btnSave AT ROW 3.62 COL 35 HELP
          "Save Parameter Values to Batch ID" WIDGET-ID 18
     browseUserPrint AT ROW 11.71 COL 41 WIDGET-ID 500
     browseParamValue AT ROW 11.71 COL 82 WIDGET-ID 600
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 149 BY 18.05.

DEFINE FRAME frameColumns
     svAvailableColumns AT ROW 1.71 COL 1 NO-LABEL WIDGET-ID 68
     btnDefault AT ROW 1.71 COL 32 HELP
          "Reset Selected Columns to Default" WIDGET-ID 76
     svSelectedColumns AT ROW 1.71 COL 37 NO-LABEL WIDGET-ID 70
     btnAdd AT ROW 4.33 COL 32 HELP
          "Add Available Column(s) to Selected Columns" WIDGET-ID 58
     btnRemoveColumn AT ROW 5.52 COL 32 HELP
          "Remove Selected Column(s)" WIDGET-ID 78
     btnMoveUp AT ROW 8.14 COL 32 HELP
          "Move Selected Column Up" WIDGET-ID 66
     btnMoveDown AT ROW 9.33 COL 32 HELP
          "Move Selected Column Down" WIDGET-ID 62
     "Selected Columns (In Order)" VIEW-AS TEXT
          SIZE 28 BY .62 AT ROW 1 COL 37 WIDGET-ID 72
     "Available Columns" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1 COL 2 WIDGET-ID 74
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 82 ROW 1
         SIZE 67 BY 10.48
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
     svExcelTable AT ROW 9.33 COL 3 WIDGET-ID 20
     RECT-1 AT ROW 9.1 COL 2 WIDGET-ID 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 1
         SIZE 40 BY 10.48
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
         TITLE              = "AOA"
         HEIGHT             = 18.05
         WIDTH              = 149
         MAX-HEIGHT         = 18.05
         MAX-WIDTH          = 149
         VIRTUAL-HEIGHT     = 18.05
         VIRTUAL-WIDTH      = 149
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{Advantzware/WinKit/embedwindow.i}
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

/* SETTINGS FOR BUTTON btnAdd IN FRAME frameColumns
   5                                                                    */
/* SETTINGS FOR BUTTON btnDefault IN FRAME frameColumns
   5                                                                    */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME frameColumns
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME frameColumns
   NO-ENABLE 5                                                          */
/* SETTINGS FOR BUTTON btnRemoveColumn IN FRAME frameColumns
   5                                                                    */
/* SETTINGS FOR FRAME frameShow
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME frameShow:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME frameShow
   NO-ENABLE                                                            */
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

/* SETTINGS FOR BUTTON btnDelete IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnDelete:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnExcel:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnSave IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnSave:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnScheduler IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnScheduler:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnShowBatch IN FRAME paramFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       btnShowBatch:HIDDEN IN FRAME paramFrame           = TRUE.

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
ON END-ERROR OF W-Win /* AOA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* AOA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
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
    IF VALID-HANDLE(hAppSrvBin) THEN DELETE OBJECT hAppSrvBin.
    IF VALID-HANDLE(hAppSrv)    THEN DELETE OBJECT hAppSrv.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
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
    ASSIGN
        cSelectedColumns = ""
        svSelectedColumns:LIST-ITEM-PAIRS = ?
        .
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
        IF deleteBatch THEN DO TRANSACTION:
            FIND bUserPrint EXCLUSIVE-LOCK WHERE ROWID(bUserPrint) EQ ttUserPrint.UserPrintRowID.
            DELETE bUserPrint.
        END. /* delete batch */
        IF deleteBatch THEN
        RUN pGetUserPrint.
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel W-Win
ON CHOOSE OF btnExcel IN FRAME paramFrame /* Excel */
DO:
    DO WITH FRAME frameShow:
        ASSIGN svExcelTable.
    END.
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


&Scoped-define SELF-NAME btnRemoveColumn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRemoveColumn W-Win
ON CHOOSE OF btnRemoveColumn IN FRAME frameColumns /* Remove */
DO:
  RUN pRemoveColumn.
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
        RUN pUpdateBatchValues.
        MESSAGE "Batch" ttUserPrint.batch-seq "Saved"
            VIEW-AS ALERT-BOX
            TITLE "Batch Record Saved"
            .
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveParams
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveParams W-Win
ON CHOOSE OF btnSaveParams IN FRAME paramFrame /* Save Params */
DO:
    RUN pSaveParamValues (NO, BUFFER user-print).
    MESSAGE "Parameter Values Saved"
    VIEW-AS ALERT-BOX.
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
    {aoa/includes/aoaURL.i}
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
    RUN pRemoveColumn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svSelectedColumns W-Win
ON VALUE-CHANGED OF svSelectedColumns IN FRAME frameColumns
DO:
  IF NUM-ENTRIES({&SELF-NAME}:SCREEN-VALUE) EQ 1 THEN
  ENABLE btnMoveUp btnMoveDown WITH FRAME frameColumns.
  ELSE
  DISABLE btnMoveUp btnMoveDown WITH FRAME frameColumns.
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
RUN VALUE("aoa/appServer/aoa" + fGetModule(aoaProgramID) + ".p") PERSISTENT SET hAppSrv.

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
  ENABLE btnExcel btnSaveParams btnCancel btnView 
      WITH FRAME paramFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  DISPLAY svShowAll svShowReportHeader svShowParameters svShowPageHeader 
          svShowGroupHeader svShowGroupFooter svShowPageFooter 
          svShowReportFooter svExcelTable 
      WITH FRAME frameShow IN WINDOW W-Win.
  ENABLE svShowAll svShowReportHeader svShowParameters svShowPageHeader 
         svShowGroupHeader svShowGroupFooter svShowPageFooter 
         svShowReportFooter svExcelTable 
      WITH FRAME frameShow IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-frameShow}
  DISPLAY svAvailableColumns svSelectedColumns 
      WITH FRAME frameColumns IN WINDOW W-Win.
  ENABLE svAvailableColumns btnDefault svSelectedColumns btnAdd btnRemoveColumn 
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

  RUN pGetColumns.

  RUN pParamValuesOverride IN h_aoaParam NO-ERROR.

  RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.

  IF aoaType EQ "Report" THEN DO:
      ENABLE btnScheduler WITH FRAME {&FRAME-NAME}.
      RUN pGetUserPrint.
  END.

  RUN pShowBatchObjs.
/*  IF VALID-OBJECT (oFormControl) THEN DO:*/
/*      RUN pShowBatchObjs.                */
/*      RUN pShowBatchObjs.                */
/*  END.                                   */
/**/
  IF NOT aoaColumns THEN DO WITH FRAME frameColumns:
      DISABLE svAvailableColumns svSelectedColumns.
      HIDE {&columnObjects}.
  END.

  IF aoaExcelOnly THEN
  ASSIGN
      btnScheduler:HIDDEN = YES
      btnShowBatch:HIDDEN = YES
      btnView:HIDDEN      = YES
      .

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
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    IF svAvailableColumns:SCREEN-VALUE IN FRAME frameColumns EQ ? THEN RETURN.

    DO idx = 1 TO svAvailableColumns:NUM-ITEMS:
        IF svAvailableColumns:IS-SELECTED(idx) THEN
        svSelectedColumns:ADD-LAST(ENTRY((svAvailableColumns:LOOKUP(svAvailableColumns:ENTRY(idx)) * 2) - 1,
                                          svAvailableColumns:LIST-ITEM-PAIRS),
                                          svAvailableColumns:ENTRY(idx)).
    END. /* do idx */
    DO idx = svAvailableColumns:NUM-ITEMS TO 1 BY -1:
        IF svAvailableColumns:IS-SELECTED(idx) THEN
        svAvailableColumns:DELETE(idx).
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcel W-Win 
PROCEDURE pExcel :
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to Excel
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hTable      AS HANDLE     NO-UNDO.
    DEFINE VARIABLE cColumns    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE fieldName   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iColumn     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iRow        AS INTEGER    NO-UNDO INITIAL 1.
    DEFINE VARIABLE iStatusRow  AS INTEGER    NO-UNDO INITIAL 3.
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
    DEFINE VARIABLE idx         AS INTEGER    NO-UNDO.
    DEFINE VARIABLE errorMsg    AS CHARACTER  NO-UNDO.

    RUN pSaveParamValues (NO, BUFFER user-print).

    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        ASSIGN
            cExcelFile = "aoa\excel\.keep"
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
        /* chWorkbook = chExcel:Workbooks:Open(cExcelFile) NO-ERROR. */
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

        IF svShowParameters THEN DO:
            chWorkbook:Worksheets(2):Activate.
            ASSIGN
                chWorksheet = chWorkbook:Worksheets(2)
                /* Rename the worksheet */
                chWorkSheet:Name = "Parameters"
                /* Disable screen updating so it will go faster */
                chExcel:ScreenUpdating = TRUE
                .
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chWorkSheet:Cells(iRow,1):Value = "Parameter:"
                chWorkSheet:Cells(iRow,1):Font:Bold = TRUE
                chWorkSheet:Cells(iRow,1):Font:Underline = TRUE
                chWorkSheet:Cells(iRow,1):HorizontalAlignment = -4152
                chWorkSheet:Cells(iRow,2):Value = "Value"
                chWorkSheet:Cells(iRow,2):Font:Bold = TRUE
                chWorkSheet:Cells(iRow,2):Font:Underline = TRUE
                iRow = iRow + 1
                .
            DO iColumn = 1 TO EXTENT(user-print.field-name):
                IF user-print.field-name[iColumn] EQ "" THEN LEAVE.
                IF user-print.field-name[iColumn] EQ "svTitle" THEN LEAVE.
                IF INDEX(user-print.field-name[iColumn],"DateOption") EQ 0 THEN DO:
                    /* align left (-4131) or right (-4152) */
                    ASSIGN
                        chWorkSheet:Cells(iRow,1):Value = (IF user-print.field-label[iColumn] NE ? THEN
                                                              user-print.field-label[iColumn] ELSE
                                                              user-print.field-name[iColumn]) + ":"
                        chWorkSheet:Cells(iRow,1):HorizontalAlignment = -4152
                        chWorkSheet:Cells(iRow,2):Value = user-print.field-value[iColumn]
                        chWorkSheet:Cells(iRow,2):HorizontalAlignment = -4131
                        iRow = iRow + 1
                        .
                END. /* not a date option parameter */
                ELSE
                chWorkSheet:Cells(iRow - 1,1):Value = chWorkSheet:Cells(iRow - 1,1):Value
                                                    + " (" + user-print.field-value[iColumn] + ")".
            END. /* do icolumn */
            ASSIGN
                chRangeRow = chWorkSheet:Cells(1,1)
                chRangeCol = chWorkSheet:Cells(iRow,2)
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            /* auto size the columns */
            chExcel:Selection:Columns:AutoFit.
            chWorksheet:Cells(iRow,1):Select.
        END. /* show parameters */
        ELSE /* remove spare worksheet */
        chWorkbook:WorkSheets(2):DELETE NO-ERROR.

        /* Select a worksheet */
        chWorkbook:Worksheets(1):Activate.
        ASSIGN
            chWorksheet = chWorkbook:Worksheets(1)
            /* Rename the worksheet */
            chWorkSheet:Name = aoaTitle
            /* Disable screen updating so it will go faster */
            chExcel:ScreenUpdating = TRUE
            iRow = 1
            .
        IF svShowReportHeader THEN DO:
            ASSIGN
                chRangeRow = chWorkSheet:Cells(1,1)
                chRangeCol = chWorkSheet:Cells(1,svSelectedColumns:NUM-ITEMS)
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chRangeRow = chWorkSheet:Cells(2,1)
                chRangeCol = chWorkSheet:Cells(2,svSelectedColumns:NUM-ITEMS)
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chWorkSheet:Range("A1"):Value = aoaTitle
                chWorkSheet:Range("A1"):Font:Bold = TRUE
                chWorkSheet:Range("A2"):Value = "Created " + STRING(TODAY,"99/99/9999")
                                              + " @ " + STRING(TIME,"hh:mm:ss am")
                iRow = iRow + 2
                iStatusRow = iStatusRow + 2
                .
        END. /* show report title */

        /* run dynamic function (business subject) */
        ASSIGN
            chWorkSheet:Cells(iStatusRow,2):Value = "Running Query..."
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("NoSweat"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

        /* build header row column labels */
        DO iColumn = 1 TO svSelectedColumns:NUM-ITEMS:
            ASSIGN
                chWorkSheet:Cells(iStatusRow,2):Value = "Running Query...Done"
                chWorkSheet:Cells(iStatusRow + 2,2):Value = "Formatting Cells..."
                fieldName = svSelectedColumns:ENTRY(iColumn)
                cDataType = hTable:BUFFER-FIELD(fieldName):DATA-TYPE
                /* align left (-4131) or right (-4152) */
                chWorkSheet:Cells(iRow,iColumn):HorizontalAlignment = IF cDataType EQ "Character" THEN -4131
                                                                                                  ELSE -4152
                chRangeRow = chWorkSheet:Cells(iRow,iColumn)
                chRangeCol = chWorkSheet:Cells(65536,iColumn)
                .
            /* column label */
            IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
            chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):LABEL.
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
                        cFormat = REPLACE(cFormat,"<","#")
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

        IF svShowPageHeader OR aoaType EQ "Dashboard" THEN DO:
            /* bold and underline header row */
            ASSIGN
                chRangeRow = chWorkSheet:Cells(iRow,1)
                chRangeCol = chWorkSheet:Cells(iRow,svSelectedColumns:NUM-ITEMS)
                chWorkSheet:Range(chRangeRow,chRangeCol):Font:Bold = TRUE
                chWorkSheet:Range(chRangeRow,chRangeCol):Font:Underline = TRUE
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            /* auto size the columns */
            chExcel:Selection:Columns:AutoFit.
            chWorksheet:Cells(iRow + 1,1):Select.
        END.
        ELSE
        iRow = iRow - 1.

        ASSIGN
            chWorkSheet:Cells(iStatusRow + 2,2):Value = "Formatting Cells...Done"
            chWorkSheet:Cells(iStatusRow + 4,2):Value = "Building Wooksheet..."
            .
        /* pause to let excel display catch up */
        PAUSE 1 NO-MESSAGE.

        /* turn off display to run faster and clear status messages */
        ASSIGN
            chExcel:ScreenUpdating = FALSE
            chWorkSheet:Cells(iStatusRow,2):Value     = ""
            chWorkSheet:Cells(iStatusRow + 2,2):Value = ""
            chWorkSheet:Cells(iStatusRow + 4,2):Value = ""
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
                fieldName = svSelectedColumns:ENTRY(iColumn).
                chWorkSheet:Cells(iRow,iColumn):Value = hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE() NO-ERROR.
                IF ERROR-STATUS:ERROR THEN DO:
                    errorMsg = "".
                    DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                        errorMsg = errorMsg + ERROR-STATUS:GET-MESSAGE(idx) + CHR(10).
                    END. /* do idx */
                    MESSAGE "Row:" iRow "Column:" iColumn SKIP(1)
                        "Field:" fieldName SKIP
                        "Label:" hTable:BUFFER-FIELD(fieldName):LABEL SKIP
                        "Value:" hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE() SKIP(1)
                        "Error:" errorMsg
                            VIEW-AS ALERT-BOX ERROR TITLE "CTRL-BREAK to End".
                END. /* if error-status:error */
            END. /* do iColumn */
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.

        /* calc header and data */
        IF iRow GT 0 THEN
        ASSIGN
            chRangeRow = chWorkSheet:Cells(iStatusRow - 2,1)
            chRangeCol = chWorkSheet:Cells(iRow,svSelectedColumns:NUM-ITEMS)
            .
        /* put data into a table */
        IF svExcelTable THEN
        ASSIGN
            chWorkSheet:ListObjects:Add(,chWorkSheet:Range(chRangeRow,chRangeCol),,NOT svShowPageHeader):Name = "TableAOA"
            chWorkSheet:ListObjects("TableAOA"):ShowTotals = TRUE
            .
        /* select header and data */
        chWorkSheet:Range(chRangeRow,chRangeCol):Select.
        /* auto size the columns */
        chExcel:Selection:Columns:AutoFit.
        /* select first none header cell */
        chWorksheet:Cells(iStatusRow - 1,1):Select.
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

    OUTPUT TO VALUE("aoa/includes/p" + REPLACE(aoaTitle," ","") + ".i") NO-ECHO.
    PUT UNFORMATTED
        "/* p" REPLACE(aoaTitle," ","") ".i - auto generated "
        STRING(TODAY,"99.99.9999") " @ " STRING(TIME,"hh:mm:ss am")
        " from aoa/aoaParam.w */"
        SKIP(1)
        "    ~{aoa/includes/aoaInputDefParams.i}" SKIP(1)
        "    /* parameter values loaded into these variables */" SKIP
        .

    fGenerateInclude(hFrame,"DefVar").

    PUT UNFORMATTED
        "    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO." SKIP
        "    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO." SKIP
        "    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO." SKIP(1)
        "    /* locate parameter values record */" SKIP
        "    RUN pGetParamValues (ipcCompany, ~"" aoaProgramID "~", ipcUserID, ipiBatch)." SKIP(1)
        "    /* load parameter values from above record into variables */" SKIP
        "    ASSIGN" SKIP
        .

    fGenerateInclude(hFrame,"DynFunc").

    PUT UNFORMATTED
        "        lSecure = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svSecure~") EQ ~"yes~"" SKIP
        "        cAvailableColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svAvailableColumns~")" SKIP
        "        cSelectedColumns = DYNAMIC-FUNCTION(~"fGetParamValue~",~"svSelectedColumns~")" SKIP
        "        ." SKIP(1)
        "    RUN pGetColumns (TEMP-TABLE tt" REPLACE(aoaTitle," ","") ":HANDLE, "
        "cAvailableColumns, "
        "cSelectedColumns"
        ")." SKIP(1)
        .

    fGenerateInclude(hFrame,"svAllGen").

    OUTPUT CLOSE.
    MESSAGE "aoa/includes/p" + REPLACE(aoaTitle," ","") + ".i" SKIP(1)
        "View Generated Code?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        TITLE "Auto Generated"
        UPDATE viewCode AS LOGICAL
        .
    IF viewCode THEN
    OS-COMMAND NO-WAIT notepad.exe VALUE("aoa/includes/p" + REPLACE(aoaTitle," ","") + ".i").

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
            IF hTable:BUFFER-FIELD(idx):NAME BEGINS "xx"      THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "rowType"     THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "parameters"  THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "recDataType" THEN NEXT.
            cRowType = cRowType + "|" + hTable:BUFFER-FIELD(idx):NAME.
            svAvailableColumns:ADD-LAST(hTable:BUFFER-FIELD(idx):LABEL,
                                        hTable:BUFFER-FIELD(idx):NAME).
        END.
        IF cSelectedColumns EQ "" OR NOT aoaColumns THEN
        ASSIGN
            svSelectedColumns:LIST-ITEM-PAIRS  = svAvailableColumns:LIST-ITEM-PAIRS
            svAvailableColumns:LIST-ITEM-PAIRS = ?
            .
        ELSE
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
        IF hChild:NAME NE ? AND (hChild:SENSITIVE OR hChild:TYPE EQ "COMBO-BOX") THEN DO:
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

    idx = svSelectedColumns:LOOKUP(svSelectedColumns:SCREEN-VALUE).
    IF ipcMove EQ "Down" AND idx EQ svSelectedColumns:NUM-ITEMS THEN RETURN.
    IF ipcMove EQ "Up"   AND idx EQ 1 THEN RETURN.

    ASSIGN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPassword W-Win 
PROCEDURE pPassword :
/*------------------------------------------------------------------------------
  Purpose:     prompt password for secured columns
  Parameters:  <none>
  Notes:       add additional secure programs/columns to pPassword.p
------------------------------------------------------------------------------*/
    RUN aoa/param/pPassword.p (
        aoaProgramID,
        svSelectedColumns:LIST-ITEM-PAIRS IN FRAME frameColumns,
        OUTPUT lSecure
        ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRemoveColumn W-Win 
PROCEDURE pRemoveColumn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    IF svSelectedColumns:SCREEN-VALUE IN FRAME frameColumns EQ ? THEN RETURN.

    DO idx = 1 TO svSelectedColumns:NUM-ITEMS:
        IF svSelectedColumns:IS-SELECTED(idx) THEN
        svAvailableColumns:ADD-LAST(ENTRY((svSelectedColumns:LOOKUP(svSelectedColumns:ENTRY(idx)) * 2) - 1,
                                           svSelectedColumns:LIST-ITEM-PAIRS),
                                           svSelectedColumns:ENTRY(idx)).
    END. /* do idx */
    DO idx = svSelectedColumns:NUM-ITEMS TO 1 BY -1:
        IF svSelectedColumns:IS-SELECTED(idx) THEN
        svSelectedColumns:DELETE(idx).
    END. /* do idx */

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
    /* number of reserved parameter fields needed */
    &SCOPED-DEFINE reserved 13

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

    RUN pPassword.

    DO TRANSACTION:
        IF iplBatch THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.company    = aoaCompany
                user-print.program-id = aoaProgramID
                user-print.user-id    = aoaUserID
                user-print.batch      = "Batch"
                .
        END. /* if batch */
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
        END. /* not batch, must be view now request */

        /* parameter values, currently up to 87 */
        ASSIGN
            user-print.field-name  = ""
            user-print.field-value = ""
            user-print.field-label = ""
            hChild = hFrame:FIRST-CHILD
            hChild = hChild:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:NAME NE ? AND
              (hChild:SENSITIVE OR
               hChild:TYPE EQ "COMBO-BOX") AND
               hChild:TYPE NE "Button" THEN
            ASSIGN
                idx = idx + 1
                user-print.field-name[idx]  = hChild:NAME
                user-print.field-label[idx] = hChild:LABEL
                user-print.field-value[idx] = hChild:SCREEN-VALUE
                .
            hChild = hChild:NEXT-SIBLING.
            IF idx EQ EXTENT(user-print.field-name) - {&reserved} THEN LEAVE.
        END. /* do while */

        /* reserve 2, 1 for security, another for title */
        IF idx LE EXTENT(user-print.field-name) - {&reserved} THEN
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = "svSecure"
            user-print.field-label[idx] = "Secure"
            user-print.field-value[idx] = STRING(lSecure)
            idx = idx + 1
            user-print.field-name[idx]  = "svTitle"
            user-print.field-label[idx] = "Title"
            user-print.field-value[idx] = aoaTitle
            .

        /* reserve 2 for avail columns and selected columns */
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

        /* reserve 9 for show/hide section parameters */
        ASSIGN
            hChild = FRAME frameShow:HANDLE
            hChild = hChild:FIRST-CHILD
            hChild = hChild:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:TYPE NE "Rectangle" THEN
            ASSIGN
                idx = idx + 1
                user-print.field-name[idx]  = hChild:NAME
                user-print.field-label[idx] = hChild:LABEL
                user-print.field-value[idx] = hChild:SCREEN-VALUE
                .
            hChild = hChild:NEXT-SIBLING.
        END. /* do while */
    END. /* do trans */

    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.

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

    DO TRANSACTION:
        FIND CURRENT user-print EXCLUSIVE-LOCK.
        ASSIGN
            user-print.batch-seq    = iBatchSeq + 1
            user-print.prog-title   = aoaTitle
            user-print.frequency    = ""
            user-print.next-date    = ?
            user-print.next-time    = 0
            user-print.last-date    = TODAY
            user-print.last-time    = TIME
            .
        FIND FIRST reftable EXCLUSIVE-LOCK
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
    END. /* do transaction */
    RELEASE reftable.

    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.

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
    DEFINE VARIABLE iHeight AS INTEGER NO-UNDO.
    DEFINE VARIABLE iWidth  AS INTEGER NO-UNDO.
    DEFINE VARIABLE hWinKitFrame       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE oWinKitControl     AS System.Windows.Forms.Control NO-UNDO.
    
    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hParamFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hParamFrame) THEN RETURN.    
                   
    DO WITH FRAME {&FRAME-NAME}:
        IF aoaType EQ "Report" THEN
        iWidth = FRAME frameShow:WIDTH-PIXELS    + 5
               + FRAME frameColumns:WIDTH-PIXELS + 5.

        ASSIGN
            hWinKitFrame                              = IF VALID-OBJECT (oFormControl) THEN
                                                        oFormControl:GetTabPageFrame (0)
                                                        ELSE {&WINDOW-NAME}:HANDLE 
            hWinKitFrame:WIDTH-PIXELS                 = hParamFrame:WIDTH-PIXELS + 5 + iWidth
            hWinKitFrame:HEIGHT-PIXELS                = hParamFrame:HEIGHT-PIXELS + 5
                                                      + btnView:HEIGHT-PIXELS + 5
            hWinKitFrame:VIRTUAL-WIDTH-PIXELS         = hWinKitFrame:WIDTH-PIXELS
            hWinKitFrame:VIRTUAL-HEIGHT-PIXELS        = hWinKitFrame:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:WIDTH-PIXELS          = hWinKitFrame:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS         = hWinKitFrame:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = hWinKitFrame:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = hWinKitFrame:HEIGHT-PIXELS
            iSaveHeight                               = hWinKitFrame:HEIGHT-PIXELS
            btnView:Y                                 = hParamFrame:HEIGHT-PIXELS + 5
            btnCancel:Y                               = hParamFrame:HEIGHT-PIXELS + 5
            btnSaveParams:Y                           = hParamFrame:HEIGHT-PIXELS + 5
            btnExcel:Y                                = hParamFrame:HEIGHT-PIXELS + 5
            btnView:X                                 = hParamFrame:WIDTH-PIXELS - btnView:WIDTH-PIXELS
            btnCancel:X                               = btnView:X - btnCancel:WIDTH-PIXELS - 2
            btnSaveParams:X                           = btnCancel:X - btnSaveParams:WIDTH-PIXELS - 2
            btnExcel:X                                = btnSaveParams:X - ((btnSaveParams:X
                                                      - (btnShowBatch:X + btnShowBatch:WIDTH-PIXELS)) / 2)
                                                      - btnExcel:WIDTH-PIXELS / 2
            .

        IF aoaType EQ "Report" THEN DO:
            ASSIGN
                FRAME frameShow:X                        = hParamFrame:WIDTH-PIXELS + 5
                FRAME frameShow:Y                        = hParamFrame:Y
                FRAME frameShow:HIDDEN                   = FALSE
                btnScheduler:Y                           = hParamFrame:HEIGHT-PIXELS + 5
                btnShowBatch:Y                           = hParamFrame:HEIGHT-PIXELS + 5
                btnShowBatch:X                           = btnScheduler:X + btnScheduler:WIDTH-PIXELS + 2
                iHeight                                  = FRAME frameShow:HEIGHT-PIXELS
                iHeight                                  = MAXIMUM(iHeight, hParamFrame:HEIGHT-PIXELS / 2)
                FRAME frameColumns:HEIGHT-PIXELS         = iHeight
                FRAME frameColumns:VIRTUAL-HEIGHT-PIXELS = FRAME frameColumns:HEIGHT-PIXELS
                svAvailableColumns:HEIGHT-PIXELS         = FRAME frameColumns:HEIGHT-PIXELS - 40
                svSelectedColumns:HEIGHT-PIXELS          = svAvailableColumns:HEIGHT-PIXELS
                FRAME frameColumns:X                     = hParamFrame:WIDTH-PIXELS + 5
                                                         + FRAME frameShow:WIDTH-PIXELS + 5
                FRAME frameColumns:Y                     = hParamFrame:Y
                FRAME frameColumns:HIDDEN                = FALSE
                BROWSE browseUserPrint:X                 = FRAME frameShow:X
                BROWSE browseUserPrint:Y                 = FRAME frameShow:Y + iHeight + 5
                BROWSE browseUserPrint:HEIGHT-PIXELS     = hParamFrame:HEIGHT-PIXELS
                                                         - iHeight - 5
                BROWSE browseUserPrint:HIDDEN            = FALSE
                BROWSE browseParamValue:X                = BROWSE browseUserPrint:X
                                                         + BROWSE browseUserPrint:WIDTH-PIXELS + 5
                BROWSE browseParamValue:Y                = BROWSE browseUserPrint:Y
                BROWSE browseParamValue:HEIGHT-PIXELS    = hParamFrame:HEIGHT-PIXELS
                                                         - iHeight
                                                         + btnSave:HEIGHT-PIXELS
                BROWSE browseParamValue:HIDDEN           = FALSE
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
        END. /* report */
        ELSE btnExcel:X = btnScheduler:X.
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
/*                FRAME frameColumns:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 5*/
                FRAME frameColumns:HEIGHT-PIXELS = iSaveHeight - 5
                svAvailableColumns:HEIGHT-PIXELS = FRAME frameColumns:HEIGHT-PIXELS - 40
                svSelectedColumns:HEIGHT-PIXELS  = svAvailableColumns:HEIGHT-PIXELS
                .
        END. /* else */
    END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pUpdateBatchValues W-Win 
PROCEDURE pUpdateBatchValues :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    FIND FIRST bUserPrint WHERE ROWID(bUserPrint) EQ ttUserPrint.UserPrintRowID.
    DO idx = 1 TO EXTENT(bUserPrint.field-name):
        IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
        FIND FIRST ttParamValue
             WHERE ttParamValue.paramOrder EQ idx
               AND ttParamValue.batch-seq  EQ bUserPrint.batch-seq.
        ttParamValue.paramValue = bUserPrint.field-value[idx].
    END. /* do idx */
    BROWSE browseParamValue:REFRESH().

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE winReSize W-Win 
PROCEDURE winReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

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
,Current Date -7~
,Current Date +7~
,Current Date -8~
,Current Date +8~
,Current Date -9~
,Current Date +9~
,Current Date -10~
,Current Date +10~
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fDateOptionValue W-Win 
FUNCTION fDateOptionValue RETURNS DATE
  (ipcDateOption AS CHARACTER, ipdtDate AS DATE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin,ipcDateOption,ipdtDate).

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
    DEFINE VARIABLE cEndList    AS CHARACTER NO-UNDO INITIAL "CHR(254),99999999.99,12/31/2049,99999999".
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
        "    IF lCustList THEN" SKIP
        "    RUN pBuildCustList (ipcCompany, "
        "~"" aoaCustListForm "~", "
        "OUTPUT cStartCustNo, "
        "OUTPUT cEndCustNo, "
        "OUTPUT lCustList"
        ")." SKIP
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLocation W-Win 
FUNCTION fGetLocation RETURNS CHARACTER:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN aoaLocation.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetModule W-Win 
FUNCTION fGetModule RETURNS CHARACTER
  ( ipProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cModule    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProgramID AS CHARACTER NO-UNDO.

    FILE-INFO:FILE-NAME = "aoa/datFiles/" + aoaType + ".dat".
    INPUT FROM VALUE(FILE-INFO:FULL-PATHNAME) NO-ECHO.
    REPEAT:
        IMPORT cModule ^ cProgramID.
        IF cProgramID EQ ipProgramID THEN LEAVE.
        cModule = "XX".
    END. /* repeat */
    INPUT CLOSE.

    RETURN cModule.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fSetDescription W-Win 
FUNCTION fSetDescription RETURNS CHARACTER
  ( ipObject AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  return descriptions for entered parameter values
    Notes:  add additional parameter fields to fSetDescription.p
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.

    RUN aoa/param/fSetDescription.p (ipObject:HANDLE, aoaCompany, OUTPUT cDescription).

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

