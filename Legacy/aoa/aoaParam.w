&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*------------------------------------------------------------------------

  File: aoaParam.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:  company, location and parameter string

  Output Parameters: <none>

  History: Ron Stark - 3.7.2016
  Updated: Ron Stark - 11.1.2018 (Jasper Enabled)
          
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

&SCOPED-DEFINE aoaJasper 7
&SCOPED-DEFINE aoaJasperGap 5

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocation AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcParamStr AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */

{AOA/includes/aoaParamDefs.i}

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
DEFINE VARIABLE lUseDefault      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lJasperStarter   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hScheduler       AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttSubject NO-UNDO
    FIELD ttOrder        AS INTEGER   LABEL "Order"             FORMAT ">>9"
    FIELD ttField        AS CHARACTER LABEL "Field"             FORMAT "x(20)"
    FIELD ttLabel        AS CHARACTER LABEL "Column"            FORMAT "x(30)"
    FIELD ttType         AS CHARACTER LABEL "Type"              FORMAT "x(10)"
    FIELD ttFormat       AS CHARACTER LABEL "Format"            FORMAT "x(20)"
    FIELD ttWidth        AS INTEGER   LABEL "Width"             FORMAT ">>>9"
    FIELD ttSize         AS INTEGER   LABEL "Size"              FORMAT ">>>>9"
    FIELD ttJasperSize   AS INTEGER   LABEL "Size"              FORMAT ">>>>9"
    FIELD ttJasperColumn AS INTEGER   LABEL "Column"            FORMAT ">>>>9"
    FIELD isActive       AS LOGICAL   LABEL "Active"
    FIELD isGroup        AS LOGICAL   LABEL "Group"
    FIELD ttGroupLabel   AS CHARACTER LABEL "Group Label"       FORMAT "x(20)"
    FIELD ttGroupCalc    AS CHARACTER LABEL "Group:Calculation" FORMAT "x(200)"
        INDEX ttSubject IS PRIMARY
            ttField
        INDEX ttOrder
            ttOrder
            .
DEFINE TEMP-TABLE ttGroupCalc NO-UNDO 
    FIELD ttField    AS CHARACTER
    FIELD ttGroup    AS CHARACTER 
    FIELD ttCalcType AS CHARACTER
        INDEX ttCalcGroup IS PRIMARY
            ttField
            ttGroup
            ttCalcType
            . 
DEFINE TEMP-TABLE ttUserPrint NO-UNDO LIKE user-print
    FIELD userPrintRowID       AS ROWID
    .
DEFINE TEMP-TABLE ttParamValue NO-UNDO
    FIELD paramOrder AS INTEGER
    FIELD batch-seq  AS INTEGER
    FIELD prgmName   AS CHARACTER 
    FIELD paramLabel AS CHARACTER LABEL "Param Label" FORMAT "x(31)"
    FIELD paramValue AS CHARACTER LABEL "Param Value" FORMAT "x(30)"
        INDEX paramOrder IS PRIMARY
            paramOrder
            .
RUN AOA\appServer\aoaBin.p PERSISTENT SET hAppSrvBin.
SESSION:ADD-SUPER-PROCEDURE (hAppSrvBin).

DEFINE BUFFER bUserPrint      FOR user-print.
DEFINE BUFFER jasperUserPrint FOR user-print.

/* function fDateOptions */
{AOA/includes/fDateOptions.i}
/* function fDateOptionValue */
{AOA/includes/fDateOptionValue.i}

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
&Scoped-define INTERNAL-TABLES ttParamValue ttUserPrint ttSubject

/* Definitions for BROWSE browseParamValue                              */
&Scoped-define FIELDS-IN-QUERY-browseParamValue ttParamValue.paramLabel ttParamValue.paramValue   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseParamValue   
&Scoped-define SELF-NAME browseParamValue
&Scoped-define QUERY-STRING-browseParamValue FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq      AND ttParamValue.prgmName EQ ttUserPrint.prgmName
&Scoped-define OPEN-QUERY-browseParamValue OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue      WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq      AND ttParamValue.prgmName EQ ttUserPrint.prgmName.
&Scoped-define TABLES-IN-QUERY-browseParamValue ttParamValue
&Scoped-define FIRST-TABLE-IN-QUERY-browseParamValue ttParamValue


/* Definitions for BROWSE browseUserPrint                               */
&Scoped-define FIELDS-IN-QUERY-browseUserPrint ttUserPrint.batch-seq ttUserPrint.prog-title ttUserPrint.last-date STRING(ttUserPrint.last-time,"hh:mm:ss am")   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseUserPrint   
&Scoped-define SELF-NAME browseUserPrint
&Scoped-define QUERY-STRING-browseUserPrint FOR EACH ttUserPrint
&Scoped-define OPEN-QUERY-browseUserPrint OPEN QUERY {&SELF-NAME} FOR EACH ttUserPrint.
&Scoped-define TABLES-IN-QUERY-browseUserPrint ttUserPrint
&Scoped-define FIRST-TABLE-IN-QUERY-browseUserPrint ttUserPrint


/* Definitions for BROWSE ttSubject                                     */
&Scoped-define FIELDS-IN-QUERY-ttSubject ttSubject.ttOrder ttSubject.isActive ttSubject.ttLabel ttSubject.isGroup ttSubject.ttGroupLabel ttSubject.ttGroupCalc   
&Scoped-define ENABLED-FIELDS-IN-QUERY-ttSubject ttSubject.isActive   ttSubject.isGroup   ttSubject.ttGroupLabel   
&Scoped-define ENABLED-TABLES-IN-QUERY-ttSubject ttSubject
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-ttSubject ttSubject
&Scoped-define SELF-NAME ttSubject
&Scoped-define QUERY-STRING-ttSubject FOR EACH ttSubject     USE-INDEX ttOrder
&Scoped-define OPEN-QUERY-ttSubject OPEN QUERY {&SELF-NAME} FOR EACH ttSubject     USE-INDEX ttOrder.
&Scoped-define TABLES-IN-QUERY-ttSubject ttSubject
&Scoped-define FIRST-TABLE-IN-QUERY-ttSubject ttSubject


/* Definitions for FRAME paramFrame                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-paramFrame ~
    ~{&OPEN-QUERY-browseParamValue}~
    ~{&OPEN-QUERY-browseUserPrint}~
    ~{&OPEN-QUERY-ttSubject}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnHTML btnJasper btnPDF btnPrint btnWord ~
btnDataPA btnCancel btnExcelCSV btnExcel btnSaveParams 

/* Custom List Definitions                                              */
/* ScheduleFields,showFields,batchObjects,batchShowHide,columnObjects,jasperOptions */
&Scoped-define showFields svShowAll svShowReportHeader svShowPageHeader ~
svShowGroupHeader svShowParameters svShowReportFooter svShowPageFooter ~
svShowGroupFooter 
&Scoped-define batchObjects btnScheduler browseUserPrint browseParamValue ~
btnShowBatch btnDelete btnSaveBatch btnApply 
&Scoped-define batchShowHide btnScheduler browseUserPrint browseParamValue ~
btnDelete btnSaveBatch btnApply 
&Scoped-define columnObjects btnJasperGroupCalc btnDefault btnMoveDown ~
btnMoveUp 
&Scoped-define jasperOptions btnHTML btnJasper btnPDF btnPrint btnWord 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFormatValue W-Win 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER) FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperCalcPattern W-Win 
FUNCTION fJasperCalcPattern RETURNS CHARACTER
  (ipcDataType AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperFields W-Win 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroupCalc W-Win 
FUNCTION fJasperGroupCalc RETURNS CHARACTER
  (ipcField AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperGroups W-Win 
FUNCTION fJasperGroups RETURNS CHARACTER
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperPattern W-Win 
FUNCTION fJasperPattern RETURNS CHARACTER
  (ipcFormat AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperReportSize W-Win 
FUNCTION fJasperReportSize RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fJasperVariables W-Win 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ) FORWARD.

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
DEFINE BUTTON btnDefault 
     IMAGE-UP FILE "AOA/images/aoaapply.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&Default" 
     SIZE 4.4 BY 1 TOOLTIP "Reset Selected Columns to Default".

DEFINE BUTTON btnJasperGroupCalc 
     IMAGE-UP FILE "AOA/images/window_dialog.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "Group:Calculatioins" 
     SIZE 4.4 BY 1 TOOLTIP "Access Group:Calculatioins".

DEFINE BUTTON btnMoveDown 
     IMAGE-UP FILE "AOA/images/aoadown.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Down" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Down".

DEFINE BUTTON btnMoveUp 
     IMAGE-UP FILE "AOA/images/aoaup.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "Move Up" 
     SIZE 4.4 BY 1 TOOLTIP "Move Selected Column Up".

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 1 GRAPHIC-EDGE    ROUNDED 
     SIZE 19.4 BY 1.38
     BGCOLOR 15 .

DEFINE VARIABLE svExcelTable AS LOGICAL INITIAL no 
     LABEL "Excel Table" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowAll AS LOGICAL INITIAL no 
     LABEL "Show ALL" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupFooter AS LOGICAL INITIAL no 
     LABEL "Group Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowGroupHeader AS LOGICAL INITIAL no 
     LABEL "Group Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageFooter AS LOGICAL INITIAL no 
     LABEL "Page Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowPageHeader AS LOGICAL INITIAL no 
     LABEL "Page Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

DEFINE VARIABLE svShowParameters AS LOGICAL INITIAL no 
     LABEL "Parameters" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportFooter AS LOGICAL INITIAL no 
     LABEL "Report Footer" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE svShowReportHeader AS LOGICAL INITIAL no 
     LABEL "Report Header" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .81 NO-UNDO.

DEFINE BUTTON btnApply 
     IMAGE-UP FILE "AOA/images/aoaapply.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "&Apply" 
     SIZE 4.4 BY 1 TOOLTIP "Apply Batch Values to Parameter Values".

DEFINE BUTTON btnAssignBatch  NO-FOCUS
     LABEL "Assign Batch ID" 
     SIZE 18 BY 1 TOOLTIP "Assign Batch ID to Parameter Values".

DEFINE BUTTON btnCancel AUTO-END-KEY 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Close".

DEFINE BUTTON btnDataPA 
     IMAGE-UP FILE "AOA/images/aoaview.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Browser".

DEFINE BUTTON btnDelete 
     IMAGE-UP FILE "AOA/images/navigate_cross.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Delete" 
     SIZE 4.4 BY 1 TOOLTIP "Delete Batch ID".

DEFINE BUTTON btnExcel 
     IMAGE-UP FILE "AOA/images/aoaexcel.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Excel XLS".

DEFINE BUTTON btnExcelCSV 
     IMAGE-UP FILE "AOA/images/aoaexcelcsv.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "csv" 
     SIZE 4.4 BY 1 TOOLTIP "Excel CSV".

DEFINE BUTTON btnHTML 
     IMAGE-UP FILE "AOA/images/html_tag.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "HTML".

DEFINE BUTTON btnJasper 
     IMAGE-UP FILE "AOA/images/jrxml_icon.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Jasper Viewer".

DEFINE BUTTON btnPDF 
     IMAGE-UP FILE "AOA/images/aoapdf.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "PDF".

DEFINE BUTTON btnPrint 
     IMAGE-UP FILE "AOA/images/printer.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Print".

DEFINE BUTTON btnSaveBatch 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "&Save" 
     SIZE 4.4 BY 1 TOOLTIP "Save Parameter Values to Batch ID".

DEFINE BUTTON btnSaveParams 
     IMAGE-UP FILE "AOA/images/navigate_check.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Save".

DEFINE BUTTON btnScheduler 
     IMAGE-UP FILE "AOA/images/aoascheduler.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "AOA Scheduler".

DEFINE BUTTON btnShowBatch 
     IMAGE-UP FILE "AOA/images/table_16xlg.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&ShowBatch" 
     SIZE 4.4 BY 1 TOOLTIP "Show Batch Parameter Values".

DEFINE BUTTON btnWord 
     IMAGE-UP FILE "AOA/images/aoaword.jpg":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.4 BY 1 TOOLTIP "Word DOCX".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseParamValue FOR 
      ttParamValue SCROLLING.

DEFINE QUERY browseUserPrint FOR 
      ttUserPrint SCROLLING.

DEFINE QUERY ttSubject FOR 
      ttSubject SCROLLING.
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
ttUserPrint.prog-title
ttUserPrint.last-date LABEL "Date"
STRING(ttUserPrint.last-time,"hh:mm:ss am") LABEL "Time" FORMAT "x(12)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 41 BY 5.95
         TITLE "Batch Parameter" ROW-HEIGHT-CHARS .62.

DEFINE BROWSE ttSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS ttSubject W-Win _FREEFORM
  QUERY ttSubject DISPLAY
      ttSubject.ttOrder
    ttSubject.isActive VIEW-AS TOGGLE-BOX
    ttSubject.ttLabel
    ttSubject.isGroup  VIEW-AS TOGGLE-BOX
    ttSubject.ttGroupLabel
    ttSubject.ttGroupCalc
ENABLE
    ttSubject.isActive
    ttSubject.isGroup
    ttSubject.ttGroupLabel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 5.95
         TITLE "Report Columns".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME paramFrame
     ttSubject AT ROW 4.33 COL 76 WIDGET-ID 700
     btnScheduler AT ROW 16.48 COL 87 HELP
          "AOA Scheduler" WIDGET-ID 42
     browseUserPrint AT ROW 10.29 COL 76 WIDGET-ID 500
     browseParamValue AT ROW 10.29 COL 117 WIDGET-ID 600
     btnHTML AT ROW 16.48 COL 51 HELP
          "HTML" WIDGET-ID 38
     btnJasper AT ROW 16.48 COL 56 HELP
          "Jasper Viewer" WIDGET-ID 32
     btnPDF AT ROW 16.48 COL 46 HELP
          "PDF" WIDGET-ID 36
     btnPrint AT ROW 16.48 COL 26 HELP
          "Print" WIDGET-ID 40
     btnWord AT ROW 16.48 COL 41 HELP
          "Word DOCX" WIDGET-ID 34
     btnDataPA AT ROW 16.48 COL 71 HELP
          "Browser" WIDGET-ID 28
     btnShowBatch AT ROW 16.48 COL 2 HELP
          "Show Batch Parameter Values" WIDGET-ID 20
     btnCancel AT ROW 16.48 COL 66 HELP
          "Close" WIDGET-ID 26
     btnExcelCSV AT ROW 16.48 COL 31 HELP
          "Excel CSV" WIDGET-ID 30
     btnExcel AT ROW 16.48 COL 36 HELP
          "Excel XLS" WIDGET-ID 22
     btnSaveParams AT ROW 16.48 COL 61 HELP
          "Save" WIDGET-ID 24
     btnDelete AT ROW 16.48 COL 104 HELP
          "Delete Batch ID" WIDGET-ID 4
     btnSaveBatch AT ROW 16.48 COL 99 HELP
          "Save Parameter Values to Batch ID" WIDGET-ID 18
     btnApply AT ROW 16.48 COL 109 HELP
          "Apply Batch Values to Parameter Values" WIDGET-ID 16
     btnAssignBatch AT ROW 16.48 COL 7 HELP
          "Assign Batch ID to Parameter Values" WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 183 BY 16.62
         BGCOLOR 15 FGCOLOR 1 .

DEFINE FRAME frameShow
     btnJasperGroupCalc AT ROW 1.57 COL 16.2 HELP
          "Click to Access Group:Calculatioins" WIDGET-ID 80
     svShowAll AT ROW 1.24 COL 24 WIDGET-ID 18
     svShowReportHeader AT ROW 1.24 COL 38 WIDGET-ID 2
     svShowPageHeader AT ROW 1.24 COL 57 WIDGET-ID 6
     svShowGroupHeader AT ROW 1.24 COL 74 WIDGET-ID 10
     svShowParameters AT ROW 1.24 COL 92 WIDGET-ID 16
     svShowReportFooter AT ROW 2.19 COL 38 WIDGET-ID 4
     svShowPageFooter AT ROW 2.19 COL 57 WIDGET-ID 8
     svShowGroupFooter AT ROW 2.19 COL 74 WIDGET-ID 12
     svExcelTable AT ROW 2.19 COL 92 WIDGET-ID 20
     btnDefault AT ROW 1.57 COL 11.8 HELP
          "Reset Selected Columns to Default" WIDGET-ID 76
     btnMoveDown AT ROW 1.57 COL 7.4 HELP
          "Move Selected Column Down" WIDGET-ID 62
     btnMoveUp AT ROW 1.57 COL 3 HELP
          "Move Selected Column Up" WIDGET-ID 66
     RECT-2 AT ROW 1.38 COL 2 WIDGET-ID 78
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 76 ROW 1
         SIZE 108 BY 3.1
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "Show/Hide Sections" WIDGET-ID 300.


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
         HEIGHT             = 16.62
         WIDTH              = 183
         MAX-HEIGHT         = 16.62
         MAX-WIDTH          = 183
         VIRTUAL-HEIGHT     = 16.62
         VIRTUAL-WIDTH      = 183
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
ASSIGN FRAME frameShow:FRAME = FRAME paramFrame:HANDLE.

/* SETTINGS FOR FRAME frameShow
   NOT-VISIBLE                                                          */
/* SETTINGS FOR BUTTON btnDefault IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnJasperGroupCalc IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnMoveDown IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR BUTTON btnMoveUp IN FRAME frameShow
   5                                                                    */
/* SETTINGS FOR RECTANGLE RECT-2 IN FRAME frameShow
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
/* BROWSE-TAB ttSubject frameShow paramFrame */
/* BROWSE-TAB browseUserPrint btnScheduler paramFrame */
/* BROWSE-TAB browseParamValue browseUserPrint paramFrame */
/* SETTINGS FOR BROWSE browseParamValue IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       browseParamValue:HIDDEN  IN FRAME paramFrame                = TRUE.

/* SETTINGS FOR BROWSE browseUserPrint IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       browseUserPrint:HIDDEN  IN FRAME paramFrame                = TRUE
       browseUserPrint:NUM-LOCKED-COLUMNS IN FRAME paramFrame     = 1.

/* SETTINGS FOR BUTTON btnApply IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnApply:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnAssignBatch IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       btnAssignBatch:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnDelete IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnDelete:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnExcel:HIDDEN IN FRAME paramFrame           = TRUE.

ASSIGN 
       btnExcelCSV:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnHTML IN FRAME paramFrame
   6                                                                    */
ASSIGN 
       btnHTML:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnJasper IN FRAME paramFrame
   6                                                                    */
ASSIGN 
       btnJasper:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnPDF IN FRAME paramFrame
   6                                                                    */
ASSIGN 
       btnPDF:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnPrint IN FRAME paramFrame
   6                                                                    */
ASSIGN 
       btnPrint:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnSaveBatch IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnSaveBatch:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnScheduler IN FRAME paramFrame
   NO-ENABLE 3 4                                                        */
ASSIGN 
       btnScheduler:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnShowBatch IN FRAME paramFrame
   NO-ENABLE 3                                                          */
ASSIGN 
       btnShowBatch:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BUTTON btnWord IN FRAME paramFrame
   6                                                                    */
ASSIGN 
       btnWord:HIDDEN IN FRAME paramFrame           = TRUE.

/* SETTINGS FOR BROWSE ttSubject IN FRAME paramFrame
   NO-ENABLE                                                            */
ASSIGN 
       ttSubject:HIDDEN  IN FRAME paramFrame                = TRUE
       ttSubject:NUM-LOCKED-COLUMNS IN FRAME paramFrame     = 3.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseParamValue
/* Query rebuild information for BROWSE browseParamValue
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttParamValue
     WHERE ttParamValue.batch-seq EQ ttUserPrint.batch-seq
     AND ttParamValue.prgmName EQ ttUserPrint.prgmName.
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE ttSubject
/* Query rebuild information for BROWSE ttSubject
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttSubject
    USE-INDEX ttOrder.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE ttSubject */
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
  IF VALID-HANDLE(hScheduler) THEN
  RUN disable_UI IN hScheduler.
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


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply W-Win
ON CHOOSE OF btnApply IN FRAME paramFrame /* Apply */
DO:
    IF AVAILABLE ttUserPrint THEN DO:
        RUN pGetParamValues (ttUserPrint.userPrintRowID).
        IF aoaColumns THEN RUN pGetColumns.
        RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAssignBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAssignBatch W-Win
ON CHOOSE OF btnAssignBatch IN FRAME paramFrame /* Assign Batch ID */
DO:
    RUN pAssignBatch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel W-Win
ON CHOOSE OF btnCancel IN FRAME paramFrame
DO:
    IF VALID-HANDLE(hAppSrvBin) THEN DELETE OBJECT hAppSrvBin.
    IF VALID-HANDLE(hAppSrv)    THEN DELETE OBJECT hAppSrv.
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel W-Win
ON RIGHT-MOUSE-CLICK OF btnCancel IN FRAME paramFrame
DO:
  RUN pGenerateInclude.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnDataPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDataPA W-Win
ON CHOOSE OF btnDataPA IN FRAME paramFrame
DO:
    RUN pSaveParamValues (NO, BUFFER user-print).
    RUN pJasperSaveUserPrint (NO, BUFFER jasperUserPrint).
    {AOA/includes/aoaURL.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameShow
&Scoped-define SELF-NAME btnDefault
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnDefault W-Win
ON CHOOSE OF btnDefault IN FRAME frameShow /* Default */
DO:
    ASSIGN
        cSelectedColumns = ""
        lUseDefault      = YES
        .
    RUN pGetColumns.
    lUseDefault = NO.
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
            FOR EACH bUserPrint EXCLUSIVE-LOCK
                WHERE bUserPrint.program-id EQ ttUserPrint.program-id
                  AND bUserPrint.user-id    EQ ttUserPrint.user-id
                  AND bUserPrint.batch-seq  EQ ttUserPrint.batch-seq
                  AND bUserPrint.batch      EQ ttUserPrint.batch
                :
                DELETE bUserPrint.
            END. /* each buserprint */
        END. /* delete batch */
        IF deleteBatch THEN
        RUN pGetUserPrintBatch.
    END. /* avail ttuserprint */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcel W-Win
ON CHOOSE OF btnExcel IN FRAME paramFrame
DO:
    IF lJasperStarter THEN
    RUN pJasper ("xls").
    ELSE
    DO WITH FRAME frameShow:
        ASSIGN svExcelTable.
        RUN pExcel (BUFFER user-print).
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExcelCSV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExcelCSV W-Win
ON CHOOSE OF btnExcelCSV IN FRAME paramFrame /* csv */
DO:
    IF lJasperStarter THEN
    RUN pJasper ("csv").
    ELSE
    DO WITH FRAME frameShow:
        ASSIGN svExcelTable.
        RUN pExcelCSV (BUFFER user-print).
    END. /* else */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnHTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnHTML W-Win
ON CHOOSE OF btnHTML IN FRAME paramFrame
DO:
    RUN pJasper ("html").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJasper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJasper W-Win
ON CHOOSE OF btnJasper IN FRAME paramFrame
DO:
    RUN pJasper ("view").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME frameShow
&Scoped-define SELF-NAME btnJasperGroupCalc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJasperGroupCalc W-Win
ON CHOOSE OF btnJasperGroupCalc IN FRAME frameShow /* Group:Calculatioins */
DO:
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveDown W-Win
ON CHOOSE OF btnMoveDown IN FRAME frameShow /* Move Down */
DO:
  RUN pMoveColumn (1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMoveUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMoveUp W-Win
ON CHOOSE OF btnMoveUp IN FRAME frameShow /* Move Up */
DO:
  RUN pMoveColumn (-1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME btnPDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPDF W-Win
ON CHOOSE OF btnPDF IN FRAME paramFrame
DO:
    RUN pJasper ("pdf").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPrint
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPrint W-Win
ON CHOOSE OF btnPrint IN FRAME paramFrame
DO:
    RUN pJasper ("print -d").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSaveBatch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSaveBatch W-Win
ON CHOOSE OF btnSaveBatch IN FRAME paramFrame /* Save */
DO:
    IF AVAILABLE ttUserPrint THEN DO:
        FIND FIRST user-print
             WHERE ROWID(user-print) EQ ttUserPrint.userPrintRowID.
        RUN pSaveParamValues (?, BUFFER user-print).
/*        RUN pJasperSaveUserPrint (?, BUFFER jasperUserPrint).*/
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
ON CHOOSE OF btnSaveParams IN FRAME paramFrame
DO:
    RUN pSaveParamValues (NO, BUFFER user-print).
    RUN pJasperSaveUserPrint (NO, BUFFER jasperUserPrint).
    MESSAGE
        "Parameter Values Saved"
    VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnScheduler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnScheduler W-Win
ON CHOOSE OF btnScheduler IN FRAME paramFrame
DO:
    RUN AOA/aoaSched.w PERSISTENT SET hScheduler.
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


&Scoped-define SELF-NAME btnWord
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnWord W-Win
ON CHOOSE OF btnWord IN FRAME paramFrame
DO:
    RUN pJasper ("docx").
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
ON VALUE-CHANGED OF svShowGroupFooter IN FRAME frameShow /* Group Footer */
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
ON VALUE-CHANGED OF svShowPageFooter IN FRAME frameShow /* Page Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowPageHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowPageHeader W-Win
ON VALUE-CHANGED OF svShowPageHeader IN FRAME frameShow /* Page Header */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowParameters
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowParameters W-Win
ON VALUE-CHANGED OF svShowParameters IN FRAME frameShow /* Parameters */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportFooter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportFooter W-Win
ON VALUE-CHANGED OF svShowReportFooter IN FRAME frameShow /* Report Footer */
DO:
    ASSIGN {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME svShowReportHeader
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL svShowReportHeader W-Win
ON VALUE-CHANGED OF svShowReportHeader IN FRAME frameShow /* Report Header */
DO:
    ASSIGN {&SELF-NAME}.
    IF {&SELF-NAME} EQ FALSE THEN
    svShowParameters = {&SELF-NAME}.
    fSetShowAll().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME ttSubject
&Scoped-define FRAME-NAME paramFrame
&Scoped-define SELF-NAME ttSubject
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ttSubject W-Win
ON DEFAULT-ACTION OF ttSubject IN FRAME paramFrame /* Report Columns */
DO:
    RUN pJasperGroupCalc.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseParamValue
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

ON "ENTRY":U OF ttSubject.isActive
DO:
    IF ttSubject.ttField BEGINS "xx" THEN DO:
        APPLY "TAB":U TO ttSubject.isActive IN BROWSE ttSubject.
        RETURN NO-APPLY.
    END. /* if xx */
END.

ON "ENTRY":U OF ttSubject.ttGroupLabel
DO:
    IF ttSubject.isGroup EQ NO THEN DO:
        APPLY "TAB":U TO ttSubject.ttGroupLabel IN BROWSE ttSubject.
        RETURN NO-APPLY.
    END. /* if not a group */
END.

ON "VALUE-CHANGED":U OF ttSubject.isActive
DO:
    ttSubject.isActive = NOT ttSubject.isActive.
    RUN pSetColumnOrder.
END.

ON "VALUE-CHANGED":U OF ttSubject.isGroup
DO:
    ttSubject.isGroup = NOT ttSubject.isGroup.
    RUN pSetGroupListItems.
END.

{&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " " + aoaType + " - " + aoaTitle.
RUN VALUE("AOA/appServer/aoa" + fGetModule(aoaProgramID) + ".p") PERSISTENT SET hAppSrv.

lJasperStarter = INDEX(OS-GETENV("Path"),"jasperstarter") NE 0.

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
             INPUT  'AOA/aoaParamHolder.w':U ,
           &ELSE
             INPUT aoaParam ,
           &ENDIF
             INPUT  FRAME paramFrame:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_aoaParam ).
       RUN set-position IN h_aoaParam ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 15.33 , 74.00 ) */

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
  ENABLE btnHTML btnJasper btnPDF btnPrint btnWord btnDataPA btnCancel 
         btnExcelCSV btnExcel btnSaveParams 
      WITH FRAME paramFrame IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-paramFrame}
  DISPLAY svShowAll svShowReportHeader svShowPageHeader svShowGroupHeader 
          svShowParameters svShowReportFooter svShowPageFooter svShowGroupFooter 
          svExcelTable 
      WITH FRAME frameShow IN WINDOW W-Win.
  ENABLE btnJasperGroupCalc svShowAll svShowReportHeader svShowPageHeader 
         svShowGroupHeader svShowParameters svShowReportFooter svShowPageFooter 
         svShowGroupFooter svExcelTable btnDefault btnMoveDown btnMoveUp 
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
  RUN pGetColumns.
  RUN pParamValuesOverride IN h_aoaParam NO-ERROR.
  RUN pInitialize IN h_aoaParam (THIS-PROCEDURE) NO-ERROR.
  IF aoaType EQ "Report" THEN DO:
      ENABLE btnAssignBatch WITH FRAME {&FRAME-NAME}.
      RUN pGetUserPrintBatch.
  END.
  RUN pShowBatchObjs.
  IF NOT aoaColumns THEN DO WITH FRAME frameShow:
      HIDE {&columnObjects}.
      BROWSE ttSubject:SENSITIVE = NO.
  END.
  IF aoaExcelOnly THEN
  ASSIGN
      btnAssignBatch:HIDDEN = YES
      btnShowBatch:HIDDEN   = YES
      btnPrint:HIDDEN       = YES
      btnExcel:HIDDEN       = YES
      btnWord:HIDDEN        = YES
      btnPDF:HIDDEN         = YES
      btnHTML:HIDDEN        = YES
      btnJasper:HIDDEN      = YES
      btnDataPA:HIDDEN      = YES
      .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignBatch W-Win 
PROCEDURE pAssignBatch :
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
            user-print.batch-seq  = iBatchSeq + 1
            user-print.prog-title = aoaTitle
            user-print.frequency  = ""
            user-print.next-date  = ?
            user-print.next-time  = 0
            user-print.last-date  = TODAY
            user-print.last-time  = TIME
            .
    END. /* do transaction */
    IF AVAILABLE user-print THEN
    FIND CURRENT user-print NO-LOCK.
    
    RUN pJasperSaveUserPrint (YES, BUFFER jasperUserPrint).    
    DO TRANSACTION:
        FIND CURRENT jasperUserPrint EXCLUSIVE-LOCK.
        ASSIGN
            jasperUserPrint.batch-seq  = user-print.batch-seq
            jasperUserPrint.prog-title = user-print.prog-title
            jasperUserPrint.frequency  = user-print.frequency
            jasperUserPrint.next-date  = user-print.next-date
            jasperUserPrint.next-time  = user-print.next-time
            jasperUserPrint.last-date  = user-print.last-date
            jasperUserPrint.last-time  = user-print.last-time
            .
    END. /* do transaction */
    IF AVAILABLE jasperUserPrint THEN
    FIND CURRENT jasperUserPrint NO-LOCK.

    MESSAGE
        "Parameters created for ..." SKIP(1)
        "Company:" aoaCompany "- Batch ID:" user-print.batch-seq
        VIEW-AS ALERT-BOX TITLE "Advantzware OA Scheduler".

    RUN pGetUserPrintBatch.

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
    RUN pJasperSaveUserPrint (NO, BUFFER jasperUserPrint).

    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        ASSIGN
            cExcelFile = "AOA\excel\.keep"
            FILE-INFO:FILE-NAME = cExcelFile
            cExcelFile = FILE-INFO:FULL-PATHNAME
            cExcelFile = REPLACE(cExcelFile,".keep",aoaTitle + " (")
                       + USERID("ASI") + ").xls"
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
                chRangeCol = chWorkSheet:Cells(1,NUM-ENTRIES(cSelectedColumns))
                .
            chWorkSheet:Range(chRangeRow,chRangeCol):Select.
            chExcel:Selection:Columns:MergeCells = TRUE.
            ASSIGN
                chRangeRow = chWorkSheet:Cells(2,1)
                chRangeCol = chWorkSheet:Cells(2,NUM-ENTRIES(cSelectedColumns))
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
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("ASI"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

        /* build header row column labels */
        DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
            ASSIGN
                chWorkSheet:Cells(iStatusRow,2):Value = "Running Query...Done"
                chWorkSheet:Cells(iStatusRow + 2,2):Value = "Formatting Cells..."
                fieldName = ENTRY(iColumn,cSelectedColumns)
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
                chRangeCol = chWorkSheet:Cells(iRow,NUM-ENTRIES(cSelectedColumns))
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
            chWorkSheet:Cells(iStatusRow + 4,2):Value = "Building Worksheet..."
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
            DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
                fieldName = ENTRY(iColumn,cSelectedColumns).
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
            chRangeCol = chWorkSheet:Cells(iRow,NUM-ENTRIES(cSelectedColumns))
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pExcelCSV W-Win 
PROCEDURE pExcelCSV :
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to Excel CSV Format
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cDynFunc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcelFile   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    
    RUN pSaveParamValues (NO, BUFFER user-print).
    RUN pJasperSaveUserPrint (NO, BUFFER jasperUserPrint).

    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        ASSIGN
            cExcelFile = "AOA\excel\.keep"
            FILE-INFO:FILE-NAME = cExcelFile
            cExcelFile = FILE-INFO:FULL-PATHNAME
            cExcelFile = REPLACE(cExcelFile,".keep",aoaTitle + " (")
                       + USERID("ASI") + ").csv"
                       .
        IF SEARCH(cExcelFile) NE ? THEN
        OS-DELETE VALUE(SEARCH(cExcelFile)).        
        OUTPUT TO VALUE(cExcelFile).
        
        /* run dynamic function (business subject) */
        ASSIGN
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("ASI"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

        /* build header row column labels */
        DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
            fieldName = ENTRY(iColumn,cSelectedColumns).
            /* column label */
            IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
            PUT UNFORMATTED hTable:BUFFER-FIELD(fieldName):LABEL + ",".
        END. /* do iColumn */
        IF svShowPageHeader OR aoaType EQ "Dashboard" THEN
        PUT UNFORMATTED SKIP.

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
            DO iColumn = 1 TO NUM-ENTRIES(cSelectedColumns):
                ASSIGN
                    fieldName    = ENTRY(iColumn,cSelectedColumns)
                    cBufferValue = hTable:BUFFER-FIELD(fieldName):BUFFER-VALUE()
                    cBufferValue = REPLACE(cBufferValue,",","")
                    cBufferValue = REPLACE(cBufferValue,CHR(10)," ")
                    .
                PUT UNFORMATTED cBufferValue + ",".
            END. /* do iColumn */
            PUT UNFORMATTED SKIP.
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        OUTPUT CLOSE.
        OS-COMMAND NO-WAIT START excel.exe VALUE("~"" + cExcelFile + "~"").
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
    DEFINE VARIABLE hFrame AS HANDLE NO-UNDO.
    
    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hFrame) THEN RETURN.

    IF NOT CAN-DO("ASI,NoSweat",USERID("ASI")) THEN RETURN.

    OUTPUT TO VALUE("AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i") NO-ECHO.
    PUT UNFORMATTED
        "/* p" REPLACE(aoaTitle," ","") ".i - auto generated "
        STRING(TODAY,"99.99.9999") " @ " STRING(TIME,"hh:mm:ss am")
        " from AOA/aoaParam.w */"
        SKIP(1)
        "    ~{AOA/includes/aoaInputDefParams.i}" SKIP(1)
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
    MESSAGE "AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i" SKIP(1)
        "View Generated Code?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        TITLE "Auto Generated"
        UPDATE viewCode AS LOGICAL
        .
    IF viewCode THEN
&IF DEFINED(FWD-VERSION) > 0 &THEN
    open-mime-resource "text/plain" string("file:///aoa/includes/p" + REPLACE(aoaTitle," ","") + ".i") false.
&ELSE
    OS-COMMAND NO-WAIT notepad.exe VALUE("AOA/includes/p" + REPLACE(aoaTitle," ","") + ".i").
&ENDIF

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
    
    IF VALID-HANDLE(hAppSrv) THEN
    DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        EMPTY TEMP-TABLE ttSubject.
    
        hTable = hTable:DEFAULT-BUFFER-HANDLE.
        DO idx = 1 TO hTable:NUM-FIELDS:
            IF CAN-DO("RECID,ROWID",hTable:BUFFER-FIELD(idx):DATA-TYPE) THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "rowType"     THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "parameters"  THEN NEXT.
            IF hTable:BUFFER-FIELD(idx):NAME EQ "recDataType" THEN NEXT.
            CREATE ttSubject.
            ASSIGN
                ttSubject.ttField  = hTable:BUFFER-FIELD(idx):NAME
                ttSubject.ttOrder  = IF cSelectedColumns EQ "" THEN idx
                                     ELSE LOOKUP(ttSubject.ttField,cSelectedColumns)
                ttSubject.isActive = CAN-DO(cSelectedColumns,ttSubject.ttField) OR
                                    (cSelectedColumns EQ "" AND NOT ttSubject.ttField BEGINS "xx")
                ttSubject.ttLabel  = hTable:BUFFER-FIELD(idx):LABEL
                ttSubject.ttType   = hTable:BUFFER-FIELD(idx):DATA-TYPE
                ttSubject.ttFormat = hTable:BUFFER-FIELD(idx):FORMAT
                ttSubject.ttWidth  = hTable:BUFFER-FIELD(idx):WIDTH
                ttSubject.ttSize   = MAX(hTable:BUFFER-FIELD(idx):WIDTH,
                                  LENGTH(hTable:BUFFER-FIELD(idx):LABEL))
                .
            IF ttSubject.ttOrder EQ 0 THEN
            ASSIGN
                ttSubject.ttOrder  = 999
                ttSubject.isActive = NO
                .
        END. /* do idx */
        RUN pJasperGetUserPrint.
        RUN pSetGroupListItems.
        RUN pSetColumnOrder.
        {&OPEN-QUERY-ttSubject}
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
    DEFINE VARIABLE jdx    AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.

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
               AND user-print.prgmName   EQ ""
               AND user-print.batch      EQ ""
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN
        FIND FIRST user-print NO-LOCK
             WHERE user-print.company    EQ aoaCompany
               AND user-print.program-id EQ aoaProgramID
               AND user-print.user-id    EQ ""
               AND user-print.prgmName   EQ ""
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
        IF hChild:NAME NE ? AND hChild:SENSITIVE AND
           hChild:TYPE NE "BUTTON" THEN DO:
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
    
    FIND FIRST jasperUserPrint NO-LOCK
         WHERE jasperUserPrint.company    EQ user-print.company
           AND jasperUserPrint.program-id EQ user-print.program-id
           AND jasperUserPrint.user-id    EQ user-print.user-id
           AND jasperUserPrint.batch      EQ user-print.batch
           AND jasperUserPrint.batch-seq  EQ user-print.batch-seq
           AND jasperUserPrint.prgmName   EQ "Jasper"
         NO-ERROR.
    IF NOT AVAILABLE jasperUserPrint THEN RETURN.
    DO idx = 1 TO EXTENT(jasperUserPrint.field-name):
        IF jasperUserPrint.field-name[idx] EQ "" THEN LEAVE.
        FIND FIRST ttSubject
             WHERE ttsubject.ttField EQ jasperUserPrint.field-name[idx]
             NO-ERROR.
        IF NOT AVAILABLE ttsubject THEN NEXT.
        ttSubject.isGroup = jasperUserPrint.field-label[idx] EQ "yes".
        IF jasperUserPrint.field-value[idx] NE "" THEN DO:
            DO jdx = 1 TO NUM-ENTRIES(jasperUserPrint.field-value[idx]) BY 2:
                IF ENTRY(jdx,jasperUserPrint.field-value[idx]) EQ "Label" THEN DO:
                    ttSubject.ttGroupLabel = ENTRY(jdx + 1,jasperUserPrint.field-value[idx]).
                    NEXT.
                END. /* if label */
                CREATE ttGroupCalc.
                ASSIGN 
                    ttGroupCalc.ttField    = jasperUserPrint.field-name[idx]
                    ttGroupCalc.ttGroup    = ENTRY(jdx,jasperUserPrint.field-value[idx])
                    ttGroupCalc.ttCalcType = ENTRY(jdx + 1,jasperUserPrint.field-value[idx])
                    .
            END. /* do jdx */
            ttSubject.ttGroupCalc = fJasperGroupCalc(ttSubject.ttField).
        END. /* if field-value */
    END. /* do idx */
    IF CAN-FIND(FIRST ttSubject) THEN
    BROWSE ttSubject:REFRESH().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSelectedColumns W-Win 
PROCEDURE pGetSelectedColumns :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iColumn AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    cSelectedColumns = "".
    FOR EACH ttSubject
        WHERE ttSubject.isActive EQ YES
           BY ttSubject.ttOrder
        :
        ASSIGN
            cSelectedColumns         = cSelectedColumns + ttSubject.ttField + ","
            ttSubject.ttJasperSize   = INTEGER(ttSubject.ttSize * {&aoaJasper})
            ttSubject.ttJasperColumn = iColumn
            iColumn = iColumn + ttSubject.ttJasperSize + {&aoaJasperGap}
            .
    END. /* each ttsubject */
    cSelectedColumns = TRIM(cSelectedColumns,",").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetUserPrintBatch W-Win 
PROCEDURE pGetUserPrintBatch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bJasperUserPrint FOR user-print.

    EMPTY TEMP-TABLE ttUserPrint.
    EMPTY TEMP-TABLE ttParamValue.

    FOR EACH bUserPrint NO-LOCK
        WHERE bUserPrint.company    EQ aoaCompany
          AND bUserPrint.program-id EQ aoaProgramID
          AND bUserPrint.user-id    EQ USERID("ASI")
          AND bUserPrint.batch      EQ "Batch"
          AND bUserPrint.batch-seq  GT 0
          AND bUserPrint.prgmName   NE "Jasper"
        :
        CREATE ttUserPrint.
        BUFFER-COPY bUserPrint TO ttUserPrint.
        ttUserPrint.userPrintRowID = ROWID(bUserPrint).
        DO idx = 1 TO EXTENT(bUserPrint.field-name):
            IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
            CREATE ttParamValue.
            ASSIGN
                ttParamValue.paramOrder = idx
                ttParamValue.batch-seq  = bUserPrint.batch-seq
                ttParamValue.prgmName   = bUserPrint.prgmName
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasper W-Win 
PROCEDURE pJasper :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iSize       AS INTEGER   NO-UNDO.

    IF lJasperStarter EQ NO THEN DO WITH FRAME {&FRAME-NAME}:
        MESSAGE 
          "Jasper Starter is NOT installed, please contact" SKIP
          "your System Administrator for assistance."
        VIEW-AS ALERT-BOX WARNING.
        HIDE {&jasperOptions}.
        RETURN.
    END. /* if excel only */
    SESSION:SET-WAIT-STATE("General").
    /* set columns for selected report columns */
    RUN pGetSelectedColumns.
    /* calculate width of jasper report */
    iSize = fJasperReportSize().
    /* if no active columns, done */
    IF iSize EQ ? THEN RETURN.    
    /* create jasper files in local user folder */
    /* create xml data file */
    RUN pJasperXML (BUFFER user-print).
    /* create xml adapter file (used in jasper studio) */
    RUN pJasperXMLAdapter.
    /* create jasper jrxml file */
    cJasperFile = "users\" + USERID("ASI") + "\" + REPLACE(aoaTitle," ","") + ".jrxml".    
    OUTPUT TO VALUE(cJasperFile).    
    RUN pJasperReport ("Open", ipcType, iSize).
    RUN pJasperStyles.
    RUN pJasperQueryString.
    RUN pJasperFieldDeclarations.
    RUN pJasperVariableDeclarations.
    IF svShowGroupHeader OR svShowGroupFooter THEN
    RUN pJasperGroupDeclarations.
    RUN pJasperBackgroundBand.    
    IF svShowReportHeader THEN
    RUN pJasterTitleBand.    
    IF svShowPageHeader THEN DO:
        RUN pJasperPageHeaderBand.    
        RUN pJasperColumnHeaderBand.
    END. /* show page header */
    /*IF svShowGroupHeader THEN*/    
    RUN pJasperDetailBand (iSize).    
    IF svShowGroupFooter THEN
    RUN pJasperColumnFooterBand.    
    IF svShowPageFooter THEN
    RUN pJasperPageFooterBand.    
    IF svShowParameters THEN
    RUN pJasperLastPageFooter.    
    IF svShowReportFooter THEN 
    RUN pJasperSummaryBand.    
    RUN pJasperReport ("Close", ipcType, iSize).    
    OUTPUT CLOSE.    
    /* copy local jasper files to jasper studio workspace */
    RUN pJasperCopy (cJasperFile).
    /* command line call to jasperstarter script */
    RUN pJasperStarter (ipcType).
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperBackgroundBand W-Win 
PROCEDURE pJasperBackgroundBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* background band */
    PUT UNFORMATTED
        "    <background>" SKIP
        "        <band splitType=~"Stretch~"/>" SKIP
        "    </background>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperColumnFooterBand W-Win 
PROCEDURE pJasperColumnFooterBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* column footer band */
    PUT UNFORMATTED
        "    <columnFooter>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Column").
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </columnFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperColumnHeaderBand W-Win 
PROCEDURE pJasperColumnHeaderBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    /* column header band */
    PUT UNFORMATTED
        "    <columnHeader>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    FOR EACH ttSubject
        WHERE ttSubject.isActive EQ YES
           BY ttSubject.ttOrder
        :
        PUT UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" ttSubject.ttJasperColumn "~" "
            "y=~"" 0 "~" "
            "width=~"" ttSubject.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                    <textElement"
            .
        IF CAN-DO("Decimal,Integer",ttSubject.ttType) THEN
        PUT UNFORMATTED
            " textAlignment=~"Right~""
            .
        PUT UNFORMATTED
            ">" SKIP
            "                        <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
            "                    </textElement>" SKIP
            "                <text><![CDATA[" ttSubject.ttLabel "]]></text>" SKIP
            "            </staticText>" SKIP
            .
        END. /* each ttsubject */
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </columnHeader>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperCopy W-Win 
PROCEDURE pJasperCopy :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcJasperFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.
    
    IF USERID("ASI") NE "NoSweat" THEN RETURN.
    
    cJasperFile = REPLACE(
        ipcJasperFile,
        "users\" + USERID("ASI"),
        "C:\Users\RStark\JaspersoftWorkspace\MyReports"
        ). 
    OS-COPY VALUE(ipcJasperFile) VALUE(cJasperFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperDetailBand W-Win 
PROCEDURE pJasperDetailBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiSize AS INTEGER NO-UNDO.

    DEFINE BUFFER ttSubject FOR ttSubject.
    
    /* detail band */
    PUT UNFORMATTED
        "    <detail>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        "            <rectangle radius=~"" 0 "~">" SKIP
        "                <reportElement style=~"Zebra~" mode=~"Opaque~" "
        "x=~"" 0 "~" "
        "y=~"" 0 "~" "
        "width=~"" ipiSize - 40 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <graphicElement>" SKIP
        "                    <pen lineWidth=~"0.0~"/>" SKIP
        "                </graphicElement>" SKIP
        "            </rectangle>" SKIP
        .
    FOR EACH ttSubject
        WHERE ttSubject.isActive EQ YES
           BY ttSubject.ttOrder
        :
        PUT UNFORMATTED
            "            <textField isBlankWhenNull=~"true~""
            .
        IF CAN-DO("Decimal,Integer",ttSubject.ttType) THEN
        PUT UNFORMATTED
            " pattern=~"" fJasperPattern(ttSubject.ttFormat) "~""
            .
        PUT UNFORMATTED
            ">" SKIP
            "                <reportElement "
            "x=~"" ttSubject.ttJasperColumn "~" "
            "y=~"" 0 "~" "
            "width=~"" ttSubject.ttJasperSize "~" "
            "height=~"" 14 "~">" SKIP
            "                    <property name=~"com.jaspersoft.studio.spreadsheet.connectionID~"/>" SKIP
            "                </reportElement>" SKIP
            .
        IF CAN-DO("Decimal,Integer",ttSubject.ttType) THEN
        PUT UNFORMATTED
            "                <textElement textAlignment=~"Right~"/>" SKIP
            .
        PUT UNFORMATTED
            "                <textFieldExpression><![CDATA[$F~{" ttSubject.ttField
            "}]]></textFieldExpression>" SKIP
            "            </textField>" SKIP
            .
    END. /* each ttsubject */
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </detail>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperFieldDeclarations W-Win 
PROCEDURE pJasperFieldDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType AS CHARACTER NO-UNDO.

    DEFINE BUFFER ttSubject FOR ttSubject.
    
    /* field declarations */
    FOR EACH ttSubject
        WHERE ttSubject.isActive    EQ YES
           OR ttSubject.isGroup     EQ YES
           OR ttSubject.ttGroupCalc NE ""
           BY ttSubject.ttOrder
        :
        CASE ttSubject.ttType:
            WHEN "Character" THEN
            cDataType = "String".
            WHEN "Decimal" THEN
            cDataType = "Double".
            WHEN "Integer" THEN
            cDataType = "Integer".
            OTHERWISE
            cDataType = "String".
        END CASE.
        PUT UNFORMATTED
            "    <field name=~"" ttSubject.ttField "~" class=~"java.lang." cDataType "~">" SKIP
            "        <property name=~"net.sf.jasperreports.xpath.field.expression~" value=~"" ttSubject.ttField "~"/>" SKIP
            "        <fieldDescription><![CDATA[" ttSubject.ttField "]]></fieldDescription>" SKIP
            "    </field>" SKIP
            .
    END. /* each ttsubject */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGetUserPrint W-Win 
PROCEDURE pJasperGetUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.
                    
    EMPTY TEMP-TABLE ttGroupCalc.
    
    RELEASE jasperUserPrint.
    /* get isgroup, ttgroup and ttcalctype values for user */
    IF lUseDefault EQ NO THEN
    FIND FIRST jasperUserPrint NO-LOCK
         WHERE jasperUserPrint.company    EQ user-print.company
           AND jasperUserPrint.program-id EQ user-print.program-id
           AND jasperUserPrint.user-id    EQ user-print.user-id
           AND jasperUserPrint.batch      EQ user-print.batch
           AND jasperUserPrint.batch-seq  EQ user-print.batch-seq
           AND jasperUserPrint.prgmName   EQ "Jasper"
         NO-ERROR.
    /* if no user found, get default values */
    IF NOT AVAILABLE jasperUserPrint THEN
    FIND FIRST jasperUserPrint NO-LOCK
         WHERE jasperUserPrint.company    EQ user-print.company
           AND jasperUserPrint.program-id EQ user-print.program-id
           AND jasperUserPrint.user-id    EQ ""
           AND jasperUserPrint.batch      EQ ""
           AND jasperUserPrint.prgmName   EQ "Jasper"
         NO-ERROR.
    IF AVAILABLE jasperUserPrint THEN    
    DO idx = 1 TO EXTENT(jasperUserPrint.field-name):
        IF jasperUserPrint.field-name[idx] EQ "" THEN LEAVE.
        FIND FIRST ttSubject
             WHERE ttsubject.ttField EQ jasperUserPrint.field-name[idx]
             NO-ERROR.
        IF NOT AVAILABLE ttsubject THEN NEXT.
        ttSubject.isGroup = jasperUserPrint.field-label[idx] EQ "yes".
        IF jasperUserPrint.field-value[idx] NE "" THEN DO:
            DO jdx = 1 TO NUM-ENTRIES(jasperUserPrint.field-value[idx]) BY 2:
                IF ENTRY(jdx,jasperUserPrint.field-value[idx]) EQ "Label" THEN DO:
                    ttSubject.ttGroupLabel = ENTRY(jdx + 1,jasperUserPrint.field-value[idx]).
                    NEXT.
                END. /* if label */
                CREATE ttGroupCalc.
                ASSIGN 
                    ttGroupCalc.ttField    = jasperUserPrint.field-name[idx]
                    ttGroupCalc.ttGroup    = ENTRY(jdx,jasperUserPrint.field-value[idx])
                    ttGroupCalc.ttCalcType = ENTRY(jdx + 1,jasperUserPrint.field-value[idx])
                    .
            END. /* do jdx */
            ttSubject.ttGroupCalc = fJasperGroupCalc(ttSubject.ttField).
        END. /* if field-value */
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupCalc W-Win 
PROCEDURE pJasperGroupCalc :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSave      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bttSubject FOR ttSubject.
    
    IF ttSubject.isActive EQ NO AND
       NOT ttSubject.ttField BEGINS "xx" THEN RETURN.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttSubject.ttField
        :
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.ttGroup + ","
                   + ttGroupCalc.ttCalcType + ","
                   .
    END. /* each ttgroupcalc */
    cGroupCalc = TRIM(cGroupCalc,",").
    RUN AOA/jasperGroupCalc.w (
        ttSubject.ttLabel,
        ttSubject.ttField,
        fJasperGroups(),
        fJasperFields(),
        fJasperVariables(),
        INPUT-OUTPUT cGroupCalc,
        OUTPUT lSave
        ).
    IF lSave THEN DO:
        FOR EACH ttGroupCalc
            WHERE ttGroupCalc.ttField EQ ttSubject.ttField
            :
            DELETE ttGroupCalc.
        END. /* each ttgroupcalc */
        IF cGroupCalc NE "" THEN
        DO idx = 1 TO NUM-ENTRIES(cGroupCalc) BY 2:
            CREATE ttGroupCalc.
            ASSIGN
                ttGroupCalc.ttField    = ttSubject.ttField
                ttGroupCalc.ttGroup    = ENTRY(idx,cGroupCalc)
                ttGroupCalc.ttCalcType = ENTRY(idx + 1,cGroupCalc)
                .
        END. /* do idx */
        ttSubject.ttGroupCalc = fJasperGroupCalc(ttSubject.ttField).
        BROWSE ttSubject:REFRESH() NO-ERROR.
    END. /* if lsave */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupDeclarations W-Win 
PROCEDURE pJasperGroupDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    /* groups declarations */
    FOR EACH ttSubject
        WHERE ttSubject.isGroup EQ YES
           BY ttSubject.ttOrder
        :
        PUT UNFORMATTED
            "    <group name=~"" REPLACE(ttSubject.ttLabel," ","_") + "_Group~">" SKIP
            "        <groupExpression><![CDATA[$F~{" ttSubject.ttField "}]]></groupExpression>" SKIP
            .
        IF svShowGroupHeader THEN
        RUN pJasperGroupHeader (ROWID(ttSubject)).
        IF svShowGroupFooter THEN
        RUN pJasperGroupFooter (ROWID(ttSubject)).
        PUT UNFORMATTED 
            "    </group>" SKIP
            .
    END. /* each ttsubject */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupFooter W-Win 
PROCEDURE pJasperGroupFooter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    DEFINE VARIABLE cGroupLabel AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPattern    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    DEFINE BUFFER bttSubject FOR ttSubject.    

    FIND FIRST ttSubject WHERE ROWID(ttSubject) EQ iprRowID.
    cGroupLabel = IF ttSubject.ttGroupLabel NE "" THEN ttSubject.ttGroupLabel
                  ELSE "** " + ttSubject.ttLabel + " **"
                  .
    PUT UNFORMATTED
        "        <groupFooter>" SKIP
        "            <band height=~"" 20 "~" splitType=~"Stretch~">" SKIP
        "                <staticText>" SKIP
        "                    <reportElement "
        "x=~"" 0 "~" "
        "y=~"" 0 "~" "
        "width=~"" (LENGTH(ttSubject.ttLabel) + 6) * {&aoaJasper} "~" "
        "height=~"" 14 "~"/>" SKIP
        "                    <textElement>" SKIP
        "                        <font isBold=~"true~"/>" SKIP
        "                    </textElement>" SKIP
        "                    <text><![CDATA[" cGroupLabel "]]></text>" SKIP
        "                </staticText>" SKIP
        .
    FOR EACH bttSubject
        WHERE bttSubject.isActive    EQ YES
           OR bttSubject.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ bttSubject.ttField
          AND ttGroupCalc.ttGroup BEGINS "[Group]"
          AND REPLACE(ttGroupCalc.ttGroup,"[Group] ","") EQ ttSubject.ttLabel
           BY bttSubject.ttOrder
        :
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        cPattern = fJasperCalcPattern(ENTRY(3,ttGroupCalc.ttCalcType,"|")).
        ELSE
        cPAttern = fJasperPattern(bttSubject.ttFormat).
        PUT UNFORMATTED
            "                <textField isBlankWhenNull=~"true~" pattern=~"" cPattern "~">" SKIP
            "                    <reportElement "
            "x=~"" IF bttSubject.ttJasperColumn GE 110 THEN bttSubject.ttJasperColumn ELSE 110 "~" "
            "y=~"" 0 "~" "
            "width=~"" bttSubject.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                    <box>" SKIP
            "                        <topPen lineWidth=~"1.0~"/>" SKIP
            "                    </box>" SKIP
            "                    <textElement textAlignment=~"Right~">" SKIP
            "                        <font isBold=~"true~"/>" SKIP
            "                    </textElement>" SKIP
            "                    <textFieldExpression><![CDATA[$V~{"
            bttSubject.ttField "_" REPLACE(ttSubject.ttLabel," ","_") "_Group"
            "}]]></textFieldExpression>" SKIP
            "                </textField>" SKIP
            .
    END. /* each bttsubject */
    PUT UNFORMATTED
        "            </band>" SKIP
        "        </groupFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupHeader W-Win 
PROCEDURE pJasperGroupHeader :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    
    PUT UNFORMATTED
        "        <groupHeader>" SKIP
        "            <band height=~"" 0 "~" splitType=~"Stretch~"/>" SKIP
        "        </groupHeader>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperGroupType W-Win 
PROCEDURE pJasperGroupType :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcGroupType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
    
    FOR EACH ttSubject
        WHERE ttSubject.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttSubject.ttField
          AND ttGroupCalc.ttGroup EQ ipcGroupType
        BREAK BY ttGroupCalc.ttGroup
              BY ttSubject.ttOrder
        :
        IF FIRST-OF(ttGroupCalc.ttGroup) THEN
        PUT UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" 0 "~" "
            "y=~"" 0 "~" "
            "width=~"" 110 "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <textElement>" SKIP
            "                    <font isBold=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <text><![CDATA[** " ttGroupCalc.ttGroup " **]]></text>" SKIP
            "            </staticText>" SKIP
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        cPattern = fJasperCalcPattern(ENTRY(3,ttGroupCalc.ttCalcType,"|")).
        ELSE
        cPattern = fJasperPattern(ttSubject.ttFormat).
        PUT UNFORMATTED
            "            <textField isBlankWhenNull=~"true~" pattern=~"" cPattern "~">" SKIP
            "                <reportElement "
            "x=~"" IF ttSubject.ttJasperColumn GE 110 THEN ttSubject.ttJasperColumn ELSE 110 "~" "
            "y=~"" 0 "~" "
            "width=~"" ttSubject.ttJasperSize "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <box>" SKIP
            "                    <topPen lineWidth=~"1.0~"/>" SKIP
            .
        IF ttGroupCalc.ttGroup EQ "Report" THEN
        PUT UNFORMATTED
            "                    <bottomPen lineWidth=~"1.0~"/>" SKIP
            .
        PUT UNFORMATTED
            "                </box>" SKIP
            "                <textElement textAlignment=~"Right~">" SKIP
            "                    <font isBold=~"true~"/>" SKIP
            "                </textElement>" SKIP
            "                <textFieldExpression><![CDATA[$V~{" ttSubject.ttField
            "_" ttGroupCalc.ttGroup "Footer}]]></textFieldExpression>" SKIP
            "            </textField>" SKIP
            .
    END. /* each ttsubject */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperLastPageFooter W-Win 
PROCEDURE pJasperLastPageFooter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cParameter     AS CHARACTER NO-UNDO EXTENT 100.
    DEFINE VARIABLE cValue         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iParameterRow  AS INTEGER   NO-UNDO INITIAL 1.
    
    IF AVAILABLE user-print THEN
    DO idx = 1 TO EXTENT(user-print.field-name):
        IF user-print.field-name[idx] EQ "svSecure" THEN LEAVE.
        ASSIGN
            cParameter[iParameterRow] = IF INDEX(user-print.field-name[idx],"Sort") NE 0 THEN "Sort By"
                ELSE IF user-print.field-label[idx] EQ ? THEN REPLACE(user-print.field-name[idx],"sv","")
                ELSE user-print.field-label[idx]
            cParameter[iParameterRow] = cParameter[iParameterRow] + ": @@@"
            cValue = IF user-print.field-value[idx] NE ? THEN user-print.field-value[idx] ELSE ""
            .
        IF user-print.field-name[idx] BEGINS "svAll" THEN
        ASSIGN
            cParameter[iParameterRow] = cParameter[iParameterRow] + ", "
                                      + user-print.field-label[idx + 1] + ": "
                                      + user-print.field-value[idx + 1] + "; "
                                      + user-print.field-label[idx + 2] + ": "
                                      + user-print.field-value[idx + 2]
            idx = idx + 2
            .
        ELSE IF user-print.field-label[idx + 1] EQ ? AND
           INDEX(user-print.field-name[idx + 1],"DateOption") NE 0 THEN
        ASSIGN
            cParameter[iParameterRow] = cParameter[iParameterRow] + " (" + user-print.field-value[idx + 1] + ")"
            cValue = STRING(DYNAMIC-FUNCTION("fDateOptionDate" IN hAppSrvBin, user-print.field-value[idx + 1], user-print.field-value[idx]),"99/99/9999")
            idx = idx + 1
            .
        ELSE IF INDEX(user-print.field-name[idx + 1],"AMPM") NE 0 THEN
        ASSIGN
            cParameter[iParameterRow] = cParameter[iParameterRow] + " " + user-print.field-value[idx + 1]
            idx = idx + 1
            .
        ASSIGN
            cParameter[iParameterRow] = REPLACE(cParameter[iParameterRow],"@@@",cValue)
            iParameterRow = iParameterRow + 1
            .
    END. /* do idx */
    
    /* last page footer band */
    PUT UNFORMATTED
        "    <lastPageFooter>" SKIP
        "        <band height=~"" (iParameterRow + 3) * 14 "~" splitType=~"Stretch~">" SKIP
        .
    IF svShowPageFooter THEN
    RUN pJasperGroupType ("Page").
    PUT UNFORMATTED
        "            <rectangle>" SKIP
        "                <reportElement mode=~"Transparent~" "
        "x=~"" 0 "~" "
        "y=~"" 14 "~" "
        "width=~"" 560 "~" "
        "height=~"" (iParameterRow - 1) * 14 "~"/>" SKIP
        "            </rectangle>" SKIP
        "            <staticText>" SKIP
        "                <reportElement "
        "x=~"" 0 "~" "
        "y=~"" 14 "~" "
        "width=~"" 56 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <textElement>" SKIP
        "                    <font isBold=~"true~" isUnderline=~"true~"/>" SKIP
        "                </textElement>" SKIP
        "                <text><![CDATA[Parameters:]]></text>" SKIP
        "            </staticText>" SKIP
        .
    DO idx = 1 TO iParameterRow:
        IF cParameter[idx] NE "" THEN
        PUT UNFORMATTED
            "            <staticText>" SKIP
            "                <reportElement "
            "x=~"" 60 "~" "
            "y=~"" (idx) * 14 "~" "
            "width=~"" 500 "~" "
            "height=~"" 14 "~"/>" SKIP
            "                <text><![CDATA[" cParameter[idx] "]]></text>" SKIP
            "            </staticText>" SKIP
            .
    END. /* do idx */
    RUN pJasperPageBottom (iParameterRow * 14).
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </lastPageFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageBottom W-Win 
PROCEDURE pJasperPageBottom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRow AS INTEGER NO-UNDO.

    IF CAN-FIND(FIRST ttGroupCalc
                WHERE ttGroupCalc.ttGroup EQ "Page") THEN
    ipiRow = ipiRow + 14.
    PUT UNFORMATTED
        "            <textField pattern=~"MMM d, yyyy h:mm:ss a~">" SKIP
        "                <reportElement "
        "x=~"" 0 "~" "
        "y=~"" ipiRow "~" "
        "width=~"" 180 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <textFieldExpression><![CDATA[new java.util.Date()]]></textFieldExpression>" SKIP
        "            </textField>" SKIP
        "            <staticText>" SKIP
        "                <reportElement "
        "x=~"" 0 "~" "
        "y=~"" ipiRow + 14 "~" "
        "width=~"" 26 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <text><![CDATA[Page:]]></text>" SKIP
        "            </staticText>" SKIP
        "            <textField>" SKIP
        "                <reportElement "
        "x=~"" 30 "~" "
        "y=~"" ipiRow + 14 "~" "
        "width=~"" 100 "~" "
        "height=~"" 14 "~"/>" SKIP
        "                <textFieldExpression><![CDATA[$V~{PAGE_NUMBER}]]></textFieldExpression>" SKIP
        "            </textField>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageFooterBand W-Win 
PROCEDURE pJasperPageFooterBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* page footer band */
    PUT UNFORMATTED
        "    <pageFooter>" SKIP
        "        <band height=~"" 44 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Page").    
    RUN pJasperPageBottom (0).
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </pageFooter>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperPageHeaderBand W-Win 
PROCEDURE pJasperPageHeaderBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* page header band */
    PUT UNFORMATTED
        "    <pageHeader>" SKIP
        "        <band height=~"" 0 "~" splitType=~"Stretch~"/>" SKIP
        "    </pageHeader>" SKIP
        .    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperQueryString W-Win 
PROCEDURE pJasperQueryString :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    PUT UNFORMATTED
        "    <queryString language=~"xPath~">" SKIP
        "        <![CDATA[/" REPLACE(aoaTitle," ","_")
        "/tt" REPLACE(aoaTitle," ","") "]]>" SKIP
        "    </queryString>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperReport W-Win 
PROCEDURE pJasperReport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcReport AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSize   AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE iMargin AS INTEGER NO-UNDO.
    
    iMargin = IF CAN-DO("pdf,view,docx",ipcType) THEN 20 ELSE 0.

    CASE ipcReport:
        WHEN "Open" THEN
        PUT UNFORMATTED
            "<?xml version=~"1.0~" encoding=~"UTF-8~"?>" SKIP
            "<!-- Created with Jaspersoft Studio version 6.6.0.final using JasperReports Library version 6.6.0  -->" SKIP
            "<jasperReport xmlns=~"http://jasperreports.sourceforge.net/jasperreports~" "
            "xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~" "
            "xsi:schemaLocation=~"http://jasperreports.sourceforge.net/jasperreports "
            "http://jasperreports.sourceforge.net/xsd/jasperreport.xsd~" "
            "name=~"" REPLACE(aoaTitle," ","") "~" "
            "pageWidth=~"" ipiSize "~" "
/*            "pageHeight=~"" 612 "~" "*/
            "orientation=~"Landscape~" "
            "columnWidth=~"" ipiSize - 40 "~" "
            "leftMargin=~"" iMargin "~" "
            "rightMargin=~"" iMargin "~" "
            "topMargin=~"" iMargin "~" "
            "bottomMargin=~"" iMargin "~">" SKIP
            "    <property name=~"com.jaspersoft.studio.data.defaultdataadapter~" "
            "value=~"" REPLACE(aoaTitle," ","") "XMLAdapter.xml~"/>" SKIP
            .
        WHEN "Close" THEN
        PUT UNFORMATTED
            "</jasperReport>" SKIP
            .
    END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperSaveUserPrint W-Win 
PROCEDURE pJasperSaveUserPrint :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplBatch AS LOGICAL NO-UNDO.
    DEFINE PARAMETER BUFFER jasperUserPrint FOR user-print.
    
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.    
    
    /* updating batch values, no need to update jasper values */
    IF iplBatch EQ ? THEN RETURN.
    
    DO TRANSACTION:
        IF iplBatch THEN DO:
            CREATE jasperUserPrint.
            ASSIGN
                jasperUserPrint.company    = aoaCompany
                jasperUserPrint.program-id = aoaProgramID
                jasperUserPrint.user-id    = aoaUserID
                jasperUserPrint.batch      = "Batch"
                jasperUserPrint.prgmName   = "Jasper"
                .
        END. /* if batch */
        ELSE IF NOT iplBatch THEN DO:
            FIND FIRST jasperUserPrint EXCLUSIVE-LOCK
                 WHERE jasperUserPrint.company    EQ aoaCompany
                   AND jasperUserPrint.program-id EQ aoaProgramID
                   AND jasperUserPrint.user-id    EQ aoaUserID
                   AND jasperUserPrint.batch      EQ ""
                   AND jasperUserPrint.prgmName   EQ "Jasper"
                 NO-ERROR.
            IF NOT AVAILABLE jasperUserPrint THEN DO:
                CREATE jasperUserPrint.
                ASSIGN
                    jasperUserPrint.company    = aoaCompany
                    jasperUserPrint.program-id = aoaProgramID
                    jasperUserPrint.user-id    = aoaUserID
                    jasperUserPrint.prgmName   = "Jasper"
                    .
            END. /* not avail */
        END. /* not batch, must be view now request */
        ASSIGN
            jasperUserPrint.field-name  = ""
            jasperUserPrint.field-value = ""
            jasperUserPrint.field-label = ""
            .
        FOR EACH ttSubject
            WHERE (ttSubject.ttGroupCalc NE ""
               OR ttSubject.isGroup EQ YES)
               BY ttSubject.ttOrder
            :
            ASSIGN
                idx = idx + 1
                jasperUserPrint.field-name[idx]  = ttSubject.ttField
                jasperUserPrint.field-label[idx] = STRING(ttSubject.isGroup)
                jasperUserPrint.field-value[idx] = "Label," + ttSubject.ttGroupLabel
                .
            IF ttSubject.ttGroupCalc NE "" THEN
            jasperUserPrint.field-value[idx] = jasperUserPrint.field-value[idx]
                                             + "," + ttSubject.ttGroupCalc
                                             .
        END. /* each ttsubject */
    END. /* do tran */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperStarter W-Win 
PROCEDURE pJasperStarter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cJasperStarter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile    AS CHARACTER NO-UNDO EXTENT 3.
    DEFINE VARIABLE cUserFolder    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    
    ASSIGN 
        cUserFolder    = "users/" + USERID("ASI") + "/"
        cJasperFile[1] = SEARCH(cUserFolder + REPLACE(aoaTitle," ","") + ".jrxml")
        cJasperFile[2] = SEARCH(cUserFolder + REPLACE(aoaTitle," ","") + ".xml")
        cJasperFile[3] = REPLACE(cJasperFile[1],"jrxml",ipcType)
        cJasperFile[3] = REPLACE(cJasperFile[3]," -d","")
        cJasperStarter = "jasperstarter process "
                       + "-f " + ipcType + " "
                       + "-t xml "
                       + "--data-file "
                       + cJasperFile[2] + " "
                       + "--xml-xpath "
                       + "/" + REPLACE(aoaTitle," ","_")
                       + "/tt" + REPLACE(aoaTitle," ","")
                       +  " " + cJasperFile[1]
                       .
    DO idx = 1 TO EXTENT(cJasperFile) - 1:
        IF cJasperFile[idx] EQ ? THEN DO:
            MESSAGE 
                "Unable to run" aoaTitle "Jasper Report" SKIP 
                "Jasper Files .jrxml and/or .xml not found!"
            VIEW-AS ALERT-BOX ERROR.
            RETURN.
        END. /* if ? */
    END. /* do idx */
    
    SESSION:SET-WAIT-STATE("General").
    OS-DELETE VALUE(cJasperFile[3]).    
    OS-COMMAND SILENT start VALUE(cJasperStarter).
    IF ipcType NE "view" AND ipcType NE "print -d" THEN DO:
        idx = 0.
        /* bail after 1 minute of waiting */        
        DO WHILE idx LE 30:
            /* have to pause while jasper creates the file */
            PAUSE 2 NO-MESSAGE.
            idx = idx + 1.
            /* check if jasper done creating file */
            IF SEARCH(cJasperFile[3]) EQ ? THEN NEXT.
            /* additional pause to ensure file exists */
            PAUSE 2 NO-MESSAGE.
            /* found it, now show it */
            OS-COMMAND NO-WAIT start VALUE(cJasperFile[3]).
            LEAVE.
        END. /* do while */
    END. /* else */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperStyles W-Win 
PROCEDURE pJasperStyles :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cColor AS CHARACTER NO-UNDO.
    
    cColor = "#FFF1D1".
    PUT UNFORMATTED
        "    <style name=~"Zebra~" mode=~"Transparent~">" SKIP
        "        <conditionalStyle>" SKIP
        "            <conditionExpression><![CDATA[$V~{REPORT_COUNT}%2 == 1]]></conditionExpression>" SKIP
        "            <style backcolor=~"" cColor "~"/>" SKIP
        "        </conditionalStyle>" SKIP
        "    </style>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperSummaryBand W-Win 
PROCEDURE pJasperSummaryBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* summary band */
    PUT UNFORMATTED
        "    <summary>" SKIP
        "        <band height=~"" 14 "~" splitType=~"Stretch~">" SKIP
        .
    RUN pJasperGroupType ("Report").
    PUT UNFORMATTED
        "        </band>" SKIP
        "    </summary>" SKIP
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperVariableDeclarations W-Win 
PROCEDURE pJasperVariableDeclarations :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cDataType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.

    /* variable declarations */
    FOR EACH ttSubject
        WHERE ttSubject.isGroup     EQ YES
           OR ttSubject.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ttSubject.ttField
           BY ttSubject.ttOrder
        :
        IF NOT svShowGroupFooter  AND ttGroupCalc.ttGroup BEGINS "[Group] " THEN NEXT.
        IF NOT svShowGroupFooter  AND ttGroupCalc.ttGroup EQ "Column"       THEN NEXT.
        IF NOT svShowPageFooter   AND ttGroupCalc.ttGroup EQ "Page"         THEN NEXT.
        IF NOT svShowReportFooter AND ttGroupCalc.ttGroup EQ "Report"       THEN NEXT.
        CASE ttSubject.ttType:
            WHEN "Character" THEN
            cDataType = "String".
            WHEN "Decimal" THEN
            cDataType = "Double".
            WHEN "Integer" THEN
            cDataType = "Integer".
            OTHERWISE
            cDataType = "String".
        END CASE.
        cDataType = IF ENTRY(1,ttGroupCalc.ttGroup,"|") NE "Calculated" THEN cDataType
                    ELSE ENTRY(3,ttGroupCalc.ttGroup,"|").
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.ttGroup,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.ttField + "_"
                        + IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.ttGroup + "Footer"
                        .
        PUT UNFORMATTED
            "    <variable name=~"" cName "~" class=~"java.lang." cDataType
            .
        IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN
        PUT UNFORMATTED
            "~" resetType=~"Group~" resetGroup=~"" cResetGroup
            .
        ELSE IF ttGroupCalc.ttGroup NE "Report" THEN
        PUT UNFORMATTED
            "~" resetType=~"" ttGroupCalc.ttGroup
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") NE "Calculated" THEN
        PUT UNFORMATTED
            "~" calculation=~"" ttGroupCalc.ttCalcType
            .
        PUT UNFORMATTED 
             "~">" SKIP
            "        <variableExpression><![CDATA["
            .
        IF ENTRY(1,ttGroupCalc.ttCalcType,"|") EQ "Calculated" THEN
        PUT UNFORMATTED
            ENTRY(2,ttGroupCalc.ttCalcType,"|")
            .
        ELSE
        PUT UNFORMATTED
            "$F~{" ttSubject.ttField "}"
            .
        PUT UNFORMATTED
            "]]></variableExpression>" SKIP
            "    </variable>" SKIP
            .
    END. /* each ttsubject */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperXML W-Win 
PROCEDURE pJasperXML :
/*------------------------------------------------------------------------------
  Purpose:     Export temp-table contents to XML Format
  Parameters:  user-print buffer
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hTable       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cColumns     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fieldName    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE hQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQueryBuf    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cDynFunc     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJasperFile  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirstRow    AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    RUN pSaveParamValues (NO, BUFFER user-print).
    RUN pJasperSaveUserPrint (NO, BUFFER jasperUserPrint).
    
    IF VALID-HANDLE(hAppSrv) THEN DO WITH FRAME frameColumns:
        hTable = DYNAMIC-FUNCTION('fGetTableHandle' IN hAppSrv, aoaProgramID).
        IF NOT VALID-HANDLE(hTable) THEN RETURN.
        
        OS-CREATE-DIR "users".
        OS-CREATE-DIR VALUE("users\" + USERID("ASI")).
        cJasperFile = "users\" + USERID("ASI") + "\" + REPLACE(aoaTitle," ","") + ".xml".
        OUTPUT TO VALUE(cJasperFile).
        PUT UNFORMATTED
            "<?xml version=~"1.0~" encoding=~"UTF-8~"?>" SKIP
            "<" REPLACE(aoaTitle," ","_") ">" SKIP
            SKIP.
        /* run dynamic function (business subject) */
        ASSIGN
            cDynFunc = "f" + REPLACE(aoaTitle," ","")
            hTable = DYNAMIC-FUNCTION(cDynFunc IN hAppSrv, aoaCompany, 0, USERID("ASI"))
            .
        IF NOT VALID-HANDLE(hTable) THEN RETURN.

        hTable = hTable:DEFAULT-BUFFER-HANDLE.

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
            PUT UNFORMATTED
                FILL(" ",4)
                "<" hTable:NAME ">"
                SKIP.
            FOR EACH ttSubject
                WHERE ttSubject.isActive    EQ YES
                   OR ttSubject.isGroup     EQ YES
                   OR ttSubject.ttGroupCalc NE ""
                :
                ASSIGN 
                    fieldName    = ttSubject.ttField
                    cBufferValue = fFormatValue(hTable, hTable:BUFFER-FIELD(fieldName):NAME)
                    /* remove special characters with escape values */
                    cBufferValue = REPLACE(cBufferValue,"~&","~&amp;")
                    cBufferValue = REPLACE(cBufferValue,"~'","~&apos;")
                    cBufferValue = REPLACE(cBufferValue,"~"","~&quot;")
                    cBufferValue = REPLACE(cBufferValue,"<","~&lt;")
                    cBufferValue = REPLACE(cBufferValue,">","~&gt;")
                    .
                PUT UNFORMATTED
                    FILL(" ",8)
                    "<" fieldName ">"
                    IF cBufferValue NE "" THEN cBufferValue ELSE " "
                    "</" fieldName ">"
                    SKIP.
            END. /* do iColumn */
            PUT UNFORMATTED
                FILL(" ",4)
                "</" hTable:NAME ">"
                SKIP.
        END. /* repeat */
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
        PUT UNFORMATTED "</" REPLACE(aoaTitle," ","_") ">" SKIP.
        OUTPUT CLOSE.
        RUN pJasperCopy (cJasperFile).
    END. /* valid happsrv */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasperXMLAdapter W-Win 
PROCEDURE pJasperXMLAdapter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJasperFile AS CHARACTER NO-UNDO.

    /* create xml adapter used in Jasper Studio */
    cJasperFile = "users\" + USERID("ASI") + "\" + REPLACE(aoaTitle," ","") + "XMLAdapter.xml".
    OUTPUT TO VALUE(cJasperFile).
    PUT UNFORMATTED
        "<?xml version=~"1.0~" encoding=~"ISO-8859-1~"?>" SKIP
        "<xmlDataAdapter class=~"net.sf.jasperreports.data.xml.XmlDataAdapterImpl~">" SKIP
        "    <name>" aoaTitle " XML Adapter</name>" SKIP
        "    <dataFile xsi:type=~"repositoryDataLocation~" xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~">" SKIP
        "        <location>" REPLACE(aoaTitle," ","") ".xml</location>" SKIP
        "    </dataFile>" SKIP
        "    <useConnection>true</useConnection>" SKIP
        "    <namespaceAware>false</namespaceAware>" SKIP
        "    <selectExpression/>" SKIP
        "    <locale xsi:type=~"java:java.lang.String~" xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~" xmlns:java=~"http://java.sun.com~">en_US</locale>" SKIP
        "    <timeZone xsi:type=~"java:java.lang.String~" xmlns:xsi=~"http://www.w3.org/2001/XMLSchema-instance~" xmlns:java=~"http://java.sun.com~">America/New_York</timeZone>" SKIP
        "</xmlDataAdapter>" SKIP
        .
    OUTPUT CLOSE.
    RUN pJasperCopy (cJasperFile).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pJasterTitleBand W-Win 
PROCEDURE pJasterTitleBand :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    /* title band */
    PUT UNFORMATTED
        "    <title>" SKIP
        "        <band height=~"" 40 "~" splitType=~"Stretch~">" SKIP
        "            <staticText>" SKIP
        "                <reportElement x=~"" 0 "~" y=~"" 0 "~" width=~"" 380 "~" height=~"" 40 "~"/>" SKIP
        "                <textElement>" SKIP
        "                    <font size=~"" 26 "~"/>" SKIP
        "                </textElement>" SKIP
        "                <text><![CDATA[" aoaTitle "]]></text>" SKIP
        "            </staticText>" SKIP
        "        </band>" SKIP
        "    </title>" SKIP
        .

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
    DEFINE INPUT PARAMETER ipiChangeOrder AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iCurrent AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMoveTo  AS INTEGER NO-UNDO.
    DEFINE VARIABLE rRowID   AS ROWID   NO-UNDO.
    
    DEFINE BUFFER bttSubject FOR ttSubject.
    
    /* can only move active column */
    IF ttsubject.isActive EQ NO THEN RETURN.
    /* first column, can't move up */
    IF ttSubject.ttOrder EQ 1 AND ipiChangeOrder EQ -1 THEN RETURN.
    /* check if at bottom, can't move down */
    FIND LAST bttSubject USE-INDEX ttOrder
         WHERE bttSubject.isActive EQ YES
         NO-ERROR.
    IF AVAILABLE bttSubject THEN DO:
        /* check if at bottom, can't move down */
        IF bttSubject.ttOrder EQ ttSubject.ttOrder AND ipiChangeOrder EQ 1 THEN
        RETURN.
    END. /* if avail */
    ELSE RETURN.
    ASSIGN
        iCurrent = ttSubject.ttOrder
        iMoveTo  = ttSubject.ttOrder + ipiChangeOrder
        .
    FIND FIRST bttSubject
         WHERE bttSubject.isActive EQ YES
           AND bttSubject.ttOrder  EQ iMoveTo
         NO-ERROR.
    IF AVAILABLE bttSubject THEN DO:
        ASSIGN
            ttSubject.ttOrder  = 0
            bttSubject.ttOrder = iCurrent
            ttSubject.ttOrder  = iMoveTo
            .
    END. /* if avail */
    cSelectedColumns = "".
    FOR EACH bttSubject
        WHERE bttSubject.isActive EQ YES
           BY bttSubject.ttOrder
        :
        cSelectedColumns = cSelectedColumns + bttSubject.ttField + ",".
    END. /* each bttsubject */
    ASSIGN
        cSelectedColumns = TRIM(cSelectedColumns,",")
        rRowID = ROWID(ttSubject)
        .
    {&OPEN-QUERY-ttSubject}
    REPOSITION ttSubject TO ROWID rRowID.

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
    RUN AOA/param/pPassword.p (
        aoaProgramID,
        cSelectedColumns,
        OUTPUT lSecure
        ).

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
    
    DEFINE INPUT PARAMETER iplBatch AS LOGICAL NO-UNDO.
    DEFINE PARAMETER BUFFER user-print FOR user-print.

    DEFINE VARIABLE hFrame   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hChild   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cnt      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cColumns AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.

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
                   AND user-print.prgmName   EQ ""
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
               hChild:TYPE NE "BUTTON" THEN DO:
                ASSIGN
                    idx = idx + 1
                    user-print.field-name[idx]  = hChild:NAME
                    user-print.field-label[idx] = hChild:LABEL
                    user-print.field-value[idx] = hChild:SCREEN-VALUE
                    .
                /* if a date field and not fixed date, clear value so doesn't show wrong dates */
                /* values when showing parameter values in report header, especially batch run */
                IF hChild:PRIVATE-DATA NE ? AND hChild:PRIVATE-DATA NE "Fixed Date" THEN
                user-print.field-value[idx] = "".
            END. /* enabled field */
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
            FOR EACH ttSubject
                WHERE ttSubject.isActive EQ NO
                   BY ttSubject.ttOrder
                :
                cColumns = cColumns + ttSubject.ttField + ",".
            END. /* each ttsubject */
            ASSIGN
                idx = idx + 1
                user-print.field-name[idx]  = "svAvailableColumns"
                user-print.field-label[idx] = ?
                user-print.field-value[idx] = TRIM(cColumns,",")
                cSelectedColumns = ""
                .
            FOR EACH ttSubject
                WHERE ttSubject.isActive EQ YES
                   BY ttSubject.ttOrder
                :
                cSelectedColumns = cSelectedColumns + ttSubject.ttField + ",".
            END. /* each ttsubject */
            ASSIGN
                cSelectedColumns            = TRIM(cSelectedColumns,",")
                idx = idx + 1
                user-print.field-name[idx]  = "svSelectedColumns"
                user-print.field-label[idx] = ?
                user-print.field-value[idx] = TRIM(cSelectedColumns,",")
                .
        END. /* aoacolumns */
        
        /* reserve 9 for show/hide section parameters */
        ASSIGN
            hChild = FRAME frameShow:HANDLE
            hChild = hChild:FIRST-CHILD
            hChild = hChild:FIRST-CHILD
            .
        DO WHILE VALID-HANDLE(hChild):
            IF hChild:TYPE NE "RECTANGLE" AND
               hChild:TYPE NE "BUTTON" THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetColumnOrder W-Win 
PROCEDURE pSetColumnOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iOrder AS INTEGER NO-UNDO.
    
    cSelectedColumns = "".
    FOR EACH ttSubject
        WHERE ttSubject.isActive EQ YES
           BY ttSubject.ttOrder
        :
        ASSIGN
            iOrder = iOrder + 1
            ttSubject.ttOrder = iOrder
            cSelectedColumns = cSelectedColumns + ttSubject.ttField + ",".
            .
    END. /* each ttSubject */
    cSelectedColumns = TRIM(cSelectedColumns,",").
    FOR EACH ttSubject
        WHERE ttSubject.isActive EQ NO
        :
        ASSIGN
            iOrder               = iOrder + 1
            ttSubject.ttOrder    = iOrder
            .
        IF NOT ttSubject.ttField BEGINS "xx" THEN DO:
            ttSubject.isGroup = NO.
            FOR EACH ttGroupCalc
                WHERE ttGroupCalc.ttField EQ ttSubject.ttField
                :
                DELETE ttGroupCalc.
            END. /* each ttgroupcalc */
        END. /* if not xx */
    END. /* each ttSubject */
    RUN pSetGroupListItems.
    {&OPEN-QUERY-ttSubject}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetGroupListItems W-Win 
PROCEDURE pSetGroupListItems :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    cGroups = fJasperGroups().
    /* check for invalid groups */
    FOR EACH ttGroupCalc
        WHERE LOOKUP(ttGroupCalc.ttGroup,cGroups) EQ 0
        :
        DELETE ttGroupCalc.
    END. /* each ttgroupcalc */
    FOR EACH ttSubject
        :
        ttSubject.ttGroupCalc = fJasperGroupCalc(ttSubject.ttField).
    END. /* each bttSubject*/
    BROWSE ttSubject:REFRESH() NO-ERROR.

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
    DEFINE VARIABLE iWidth AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRow   AS INTEGER NO-UNDO.

    RUN get-attribute IN h_aoaParam ('adm-object-handle':U).
    hParamFrame = WIDGET-HANDLE(RETURN-VALUE).

    IF NOT VALID-HANDLE(hParamFrame) THEN RETURN.

    DO WITH FRAME {&FRAME-NAME}:
        IF aoaType EQ "Report" THEN
        iWidth = FRAME frameShow:WIDTH-PIXELS + 5.
        ASSIGN
            iRow                                      = hParamFrame:HEIGHT-PIXELS + 5
            {&WINDOW-NAME}:WIDTH-PIXELS               = hParamFrame:WIDTH-PIXELS + 5 + iWidth
            {&WINDOW-NAME}:HEIGHT-PIXELS              = iRow + btnDataPA:HEIGHT-PIXELS + 5
            {&WINDOW-NAME}:VIRTUAL-HEIGHT-PIXELS      = {&WINDOW-NAME}:HEIGHT-PIXELS
            {&WINDOW-NAME}:VIRTUAL-WIDTH-PIXELS       = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:WIDTH-PIXELS          = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:HEIGHT-PIXELS         = {&WINDOW-NAME}:HEIGHT-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-WIDTH-PIXELS  = {&WINDOW-NAME}:WIDTH-PIXELS
            FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT-PIXELS = {&WINDOW-NAME}:HEIGHT-PIXELS
            btnDataPA:Y                               = iRow
            btnCancel:Y                               = iRow
            btnSaveParams:Y                           = iRow
            btnPrint:Y                                = iRow            
            btnExcelCSV:Y                             = iRow
            btnExcel:Y                                = iRow
            btnWord:Y                                 = iRow
            btnPDF:Y                                  = iRow
            btnHTML:Y                                 = iRow
            btnJasper:Y                               = iRow            
            btnDataPA:X                               = hParamFrame:WIDTH-PIXELS - btnDataPA:WIDTH-PIXELS
            btnCancel:X                               = btnDataPA:X - btnCancel:WIDTH-PIXELS - 1
            btnSaveParams:X                           = btnCancel:X - btnSaveParams:WIDTH-PIXELS - 1
            .
        IF aoaType EQ "Report" THEN DO:
            ASSIGN
                btnShowBatch:Y                        = iRow
                btnAssignBatch:Y                      = iRow
                FRAME frameShow:X                     = hParamFrame:WIDTH-PIXELS + 5
                BROWSE ttSubject:X                    = hParamFrame:WIDTH-PIXELS + 5
                BROWSE ttSubject:HEIGHT-PIXELS        = hParamFrame:HEIGHT-PIXELS - BROWSE ttSubject:Y
                BROWSE ttSubject:SENSITIVE            = TRUE
                BROWSE ttSubject:HIDDEN               = FALSE                
                BROWSE browseUserPrint:X              = BROWSE ttSubject:X
                BROWSE browseUserPrint:Y              = 1
                BROWSE browseUserPrint:HEIGHT-PIXELS  = hParamFrame:HEIGHT-PIXELS
                BROWSE browseUserPrint:HIDDEN         = FALSE                
                BROWSE browseParamValue:X             = BROWSE browseUserPrint:X
                                                      + BROWSE browseUserPrint:WIDTH-PIXELS
                BROWSE browseParamValue:Y             = 1
                BROWSE browseParamValue:HEIGHT-PIXELS = BROWSE browseUserPrint:HEIGHT-PIXELS
                                                      + btnSaveBatch:HEIGHT-PIXELS
                BROWSE browseParamValue:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS - 5
                BROWSE browseParamValue:HIDDEN        = FALSE                
                btnSaveBatch:Y                        = btnDataPA:Y
                btnApply:Y                            = btnDataPA:Y
                btnDelete:Y                           = btnDataPA:Y
                btnScheduler:Y                        = btnDataPA:Y
                btnApply:X                            = BROWSE browseUserPrint:X
                                                      + BROWSE browseUserPrint:WIDTH-PIXELS
                                                      - btnApply:WIDTH-PIXELS - 20
                btnDelete:X                           = btnApply:X - btnApply:WIDTH-PIXELS - 1
                btnSaveBatch:X                        = btnDelete:X - btnDelete:WIDTH-PIXELS - 1
                btnScheduler:X                        = btnSaveBatch:X - btnSaveBatch:WIDTH-PIXELS - 45 
                .
            ENABLE {&batchObjects}.
        END. /* report */
        ELSE
        ASSIGN
            btnPrint:X    = btnShowBatch:X
            btnExcelCSV:X = btnShowBatch:X
            btnExcel:X    = btnShowBatch:X
            btnWord:X     = btnShowBatch:X
            btnPDF:X      = btnShowBatch:X
            btnHTML:X     = btnShowBatch:X
            btnJasper:X   = btnShowBatch:X
            .
    END. /* with frame  */
    IF 1024 - {&WINDOW-NAME}:WIDTH-PIXELS  LT 0 OR
        768 - {&WINDOW-NAME}:HEIGHT-PIXELS LT 0 THEN
    MESSAGE
        "Width:" {&WINDOW-NAME}:WIDTH-PIXELS
        "(" 1024 - {&WINDOW-NAME}:WIDTH-PIXELS ")"
        SKIP 
        "Height:" {&WINDOW-NAME}:HEIGHT-PIXELS
        "(" 768 - {&WINDOW-NAME}:HEIGHT-PIXELS ")"    
    VIEW-AS ALERT-BOX.

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
            ASSIGN
                FRAME frameShow:HIDDEN  = TRUE
                BROWSE ttSubject:HIDDEN = TRUE
                .
            RUN pSetWinSize.
        END. /* if showbatchobjs */
        ELSE DO:
            HIDE {&batchShowHide}.
            ASSIGN
                FRAME frameShow:HIDDEN         = FALSE
                BROWSE ttSubject:HEIGHT-PIXELS = FRAME {&FRAME-NAME}:HEIGHT-PIXELS
                                               - BROWSE ttSubject:Y - 5
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

    FIND FIRST bUserPrint WHERE ROWID(bUserPrint) EQ ttUserPrint.userPrintRowID.
    DO idx = 1 TO EXTENT(bUserPrint.field-name):
        IF bUserPrint.field-name[idx] EQ "" THEN LEAVE.
        FIND FIRST ttParamValue
             WHERE ttParamValue.paramOrder EQ idx
               AND ttParamValue.batch-seq  EQ bUserPrint.batch-seq.
        ttParamValue.paramValue = bUserPrint.field-value[idx].
    END. /* do idx */
    BROWSE browseParamValue:REFRESH() NO-ERROR.

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
    IF aoaType EQ "Report" THEN
    ASSIGN aoaURL = aoaURL + "^&refresh=true^&connection=AdvantzwareOA".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFormatValue W-Win 
FUNCTION fFormatValue RETURNS CHARACTER
  (iphTable AS HANDLE, ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: format field value
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cStr AS CHARACTER NO-UNDO.

    cStr = STRING(iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE(),
                  iphTable:BUFFER-FIELD(ipcField):FORMAT) NO-ERROR.
    /* error raised if invalid format for field value */
    IF ERROR-STATUS:NUM-MESSAGES NE 0 OR
       iphTable:BUFFER-FIELD(ipcField):DATA-TYPE EQ "CHARACTER" THEN 
    cStr = iphTable:BUFFER-FIELD(ipcField):BUFFER-VALUE().
    
    RETURN LEFT-TRIM(TRIM(cStr)).

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

    FILE-INFO:FILE-NAME = "AOA/datFiles/" + aoaType + ".dat".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperCalcPattern W-Win 
FUNCTION fJasperCalcPattern RETURNS CHARACTER
  (ipcDataType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPattern AS CHARACTER NO-UNDO.
    
    CASE ipcDataType:
        WHEN "Integer" THEN
        cPattern = "#,##0".            
        WHEN "Double" THEN
        cPattern = "#,##0.#####".
        WHEN "String" THEN
        cPattern = "X(0)". 
    END CASE.
    
    RETURN cPattern.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperFields W-Win 
FUNCTION fJasperFields RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bttSubject FOR ttSubject.
        
    FOR EACH bttSubject
        :
        cFields = cFields + "$F~{" + bttSubject.ttField + "},".
    END. /* each bttsubject */
    RETURN TRIM(cFields,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroupCalc W-Win 
FUNCTION fJasperGroupCalc RETURNS CHARACTER
  (ipcField AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroupCalc AS CHARACTER NO-UNDO.
    
    FOR EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ ipcField
        :
        IF ttGroupCalc.ttGroup NE "" THEN 
        cGroupCalc = cGroupCalc
                   + ttGroupCalc.ttGroup + ","
                   + ttGroupCalc.ttCalcType + ","
                   .
    END. /* each ttgroupcalc */
    RETURN TRIM(cGroupCalc,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperGroups W-Win 
FUNCTION fJasperGroups RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cGroups AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER ttSubject FOR ttSubject.
    
    /* create list of groups */
    FOR EACH ttSubject
        WHERE ttSubject.isGroup  EQ YES
        :
        cGroups = cGroups + "[Group] " + ttSubject.ttLabel + ",".
    END. /* each bttSubject*/
    RETURN "Column," + cGroups + "Page,Report".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperPattern W-Win 
FUNCTION fJasperPattern RETURNS CHARACTER
  (ipcFormat AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    RETURN REPLACE(REPLACE(REPLACE(REPLACE(ipcFormat,">","#"),"9","0"),"-",""),"<","#").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperReportSize W-Win 
FUNCTION fJasperReportSize RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    FIND LAST ttSubject USE-INDEX ttOrder
         WHERE ttSubject.isActive       EQ YES
           AND ttSubject.ttJasperSize   NE 0
           AND ttSubject.ttJasperColumn NE 0
         NO-ERROR.
    IF NOT AVAILABLE ttSubject THEN RETURN ?.
    RETURN ttSubject.ttJasperColumn + ttSubject.ttJasperSize + 100.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fJasperVariables W-Win 
FUNCTION fJasperVariables RETURNS CHARACTER
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
        DEFINE VARIABLE cVariables  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cResetGroup AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cName       AS CHARACTER NO-UNDO.
        
        DEFINE BUFFER bttSubject FOR ttSubject.
        
    FOR EACH bttSubject
        WHERE bttSubject.isActive    EQ YES
           OR bttSubject.ttGroupCalc NE "",
        EACH ttGroupCalc
        WHERE ttGroupCalc.ttField EQ bttSubject.ttField
        :
        ASSIGN
            cResetGroup = REPLACE(REPLACE(ttGroupCalc.ttGroup,"[Group] ","")," ","_") + "_Group"
            cName       = ttGroupCalc.ttField + "_"
                        + IF ttGroupCalc.ttGroup BEGINS "[Group] " THEN cResetGroup
                          ELSE ttGroupCalc.ttGroup + "Footer" 
            cVariables  = cVariables + "$V~{" + cName + "},".
                        .
    END. /* each bttsubject */
    RETURN TRIM (cVariables,",").

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
    
    RUN AOA/param/fSetDescription.p (ipObject:HANDLE, aoaCompany, OUTPUT cDescription).

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

