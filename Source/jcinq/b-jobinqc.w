&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
/* Procedure Description
"ASI SmartNavBrowser Object Template with Wizard.

Use this template to create a new SmartNavBrowser object with the assistance of the SmartBrowser Wizard. When completed, this object can then be drawn onto any 'smart' container such as a SmartWindow, SmartDialog or SmartFrame."
*/
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  jcinq\b-jobinq.w

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

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE VARIABLE custPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE orderQty AS INTEGER NO-UNDO.
DEFINE VARIABLE producedQty AS INTEGER NO-UNDO.
DEFINE VARIABLE onHandQty AS INTEGER NO-UNDO.
DEFINE VARIABLE qtyOnHand AS INTEGER NO-UNDO.
DEFINE VARIABLE shipQty AS INTEGER NO-UNDO.
DEFINE VARIABLE invoiceQty AS INTEGER NO-UNDO.
DEFINE VARIABLE wipQty AS INTEGER NO-UNDO.
DEFINE VARIABLE overUnderPct AS INTEGER NO-UNDO.
DEFINE VARIABLE fgItemNo AS CHARACTER NO-UNDO.
DEF VAR v-job-rec-key AS CHAR NO-UNDO.
DEF VAR v-job-header AS CHAR NO-UNDO.

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR ll-initial AS LOG INIT YES NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-frst-rowid2 AS ROWID NO-UNDO.
DEF VAR lv-last-rowid2 AS ROWID NO-UNDO.
DEF VAR char-hdl AS cha NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.

DEF VAR lv-sort-by AS CHAR INIT "job-no" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Job#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-first-show-job-no AS cha NO-UNDO.
DEF VAR lv-last-show-job-no AS cha NO-UNDO.
DEF SHARED TEMP-TABLE tt-job-hdr LIKE job-hdr
    INDEX j-no j-no
    INDEX job company job
    INDEX job-no company job-no job-no2 frm blank-no
    INDEX ord-no company ord-no job-no job-no2
    INDEX est-no company est-no job-no job-no2
    INDEX i-no company i-no opened
    INDEX cust-idx company cust-no lock i-no.

&SCOPED-DEFINE key-phrase tt-job-hdr.company EQ cocode

&SCOPED-DEFINE for-each1                            ~
    FOR EACH tt-job-hdr                                ~
        WHERE {&key-phrase}                         ~
          AND ((tt-job-hdr.opened EQ YES AND tb_open) OR (tt-job-hdr.opened EQ NO AND tb_closed)) ~
          AND tt-job-hdr.cust-no   BEGINS fi_cust-no   ~
          AND tt-job-hdr.i-no      BEGINS fi_i-no      ~
          AND tt-job-hdr.est-no    BEGINS fi_est-no    ~
          AND tt-job-hdr.job-no    BEGINS fi_job-no    ~
          AND (tt-job-hdr.job-no2  EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each1l                            ~
    FOR EACH tt-job-hdr                                ~
        WHERE {&key-phrase}                         ~
          AND tt-job-hdr.opened EQ YES

&SCOPED-DEFINE for-each2                     ~
    FIRST job                                ~
        WHERE job.company EQ tt-job-hdr.company ~
          AND job.job     EQ tt-job-hdr.job     ~
          AND job.job-no  EQ tt-job-hdr.job-no  ~
          AND job.job-no2 EQ tt-job-hdr.job-no2 ~
          AND ((job.opened EQ YES AND tb_open) OR (job.opened EQ NO AND tb_closed)) ~
        NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                 ~
    IF lv-sort-by EQ "ord-no"     THEN STRING(tt-job-hdr.ord-no,"9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "stat"       THEN job.stat                                                                                                         ELSE ~
    IF lv-sort-by EQ "cust-no"    THEN tt-job-hdr.cust-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "i-no"       THEN tt-job-hdr.i-no                                                                                                     ELSE ~
    IF lv-sort-by EQ "est-no"     THEN tt-job-hdr.est-no                                                                                                   ELSE ~
    IF lv-sort-by EQ "job-no"     THEN STRING(tt-job-hdr.job-no,"x(9)") + STRING(tt-job-hdr.job-no2,"999")                                                     ELSE ~
    IF lv-sort-by EQ "close-date" THEN STRING(YEAR(job.close-date),"9999") + STRING(MONTH(job.close-date),"99") + STRING(DAY(job.close-date),"99")      ELSE ~
                                       STRING(YEAR(job.start-date),"9999") + STRING(MONTH(job.start-date),"99") + STRING(DAY(job.start-date),"99")

&SCOPED-DEFINE sortby BY tt-job-hdr.job-no BY tt-job-hdr.job-no2

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~

DO TRANSACTION:
  {sys/inc/browser.i "JCBROWSE"}
END.

ll-initial = browser-log.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-job-hdr job

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table tt-job-hdr.job-no tt-job-hdr.job-no2 ~
tt-job-hdr.i-no tt-job-hdr.est-no tt-job-hdr.ord-no tt-job-hdr.cust-no job.start-date ~
job.close-date job.stat custPart() @ custPart tt-job-hdr.qty ~
orderQty() @ orderQty producedQty(onHandQty) @ producedQty ~
onHandQty(qtyOnHand) @ onHandQty shipQty() @ shipQty ~
invoiceQty() @ invoiceQty wipQty() @ wipQty ~
overUnderPct(onHandQty) @ overUnderPct fgItemNo() @ fgItemNo 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table tt-job-hdr.job-no ~
tt-job-hdr.job-no2 tt-job-hdr.i-no tt-job-hdr.est-no tt-job-hdr.ord-no tt-job-hdr.cust-no ~
job.start-date job.close-date job.stat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table tt-job-hdr job
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table tt-job-hdr
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table job
&Scoped-define QUERY-STRING-Browser-Table FOR EACH tt-job-hdr WHERE ~{&KEY-PHRASE} ~
      AND tt-job-hdr.job = 0 NO-LOCK, ~
      EACH job OF tt-job-hdr NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH tt-job-hdr WHERE ~{&KEY-PHRASE} ~
      AND tt-job-hdr.job = 0 NO-LOCK, ~
      EACH job OF tt-job-hdr NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table tt-job-hdr job
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table tt-job-hdr
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table job


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_job-no fi_job-no2 fi_i-no fi_cust-no ~
fi_est-no fi_ord-no tb_open tb_closed btn_go btn_prev Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_job-no fi_job-no2 fi_i-no fi_cust-no ~
fi_est-no fi_ord-no tb_open tb_closed fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD custPart B-table-Win 
FUNCTION custPart RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fgItemNo B-table-Win 
FUNCTION fgItemNo RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD invoiceQty B-table-Win 
FUNCTION invoiceQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD onHandQty B-table-Win 
FUNCTION onHandQty RETURNS INTEGER
  (OUTPUT opQtyOnHand AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD orderQty B-table-Win 
FUNCTION orderQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD overUnderPct B-table-Win 
FUNCTION overUnderPct RETURNS INTEGER
  (ipBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD producedQty B-table-Win 
FUNCTION producedQty RETURNS INTEGER
  (OUTPUT opBalance AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD shipQty B-table-Win 
FUNCTION shipQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD wipQty B-table-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 16 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(9)":U 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.05.

DEFINE VARIABLE tb_closed AS LOGICAL INITIAL yes 
     LABEL "Closed Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL yes 
     LABEL "Open Jobs" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      tt-job-hdr, 
      job SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      tt-job-hdr.job-no COLUMN-LABEL "Job#" FORMAT "x(9)":U COLUMN-BGCOLOR 8
            LABEL-BGCOLOR 14
      tt-job-hdr.job-no2 COLUMN-LABEL "" FORMAT ">>9":U LABEL-BGCOLOR 14
      tt-job-hdr.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      tt-job-hdr.est-no COLUMN-LABEL "Estimate#" FORMAT "x(8)":U WIDTH 14
            LABEL-BGCOLOR 14
      tt-job-hdr.ord-no FORMAT ">>>>>>>9":U LABEL-BGCOLOR 14
      tt-job-hdr.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      job.start-date FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      job.close-date COLUMN-LABEL "Close Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      job.stat COLUMN-LABEL "Status" FORMAT "x":U LABEL-BGCOLOR 14
      custPart() @ custPart COLUMN-LABEL "Customer Part" FORMAT "X(15)":U
      tt-job-hdr.qty COLUMN-LABEL "Job Qty" FORMAT "->>>,>>>,>>9":U
      orderQty() @ orderQty COLUMN-LABEL "Ordered Qty" FORMAT "->>,>>>,>>>":U
      producedQty(onHandQty) @ producedQty COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
      onHandQty(qtyOnHand) @ onHandQty COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
      shipQty() @ shipQty COLUMN-LABEL "Shipped Qty" FORMAT "->>,>>>,>>>":U
      invoiceQty() @ invoiceQty COLUMN-LABEL "Invoice Qty" FORMAT "->>,>>>,>>>":U
      wipQty() @ wipQty COLUMN-LABEL "WIP Qty" FORMAT "->>,>>>,>>>":U
      overUnderPct(onHandQty) @ overUnderPct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
      fgItemNo() @ fgItemNo COLUMN-LABEL "FG Item" FORMAT "X(15)":U
  ENABLE
      tt-job-hdr.job-no
      tt-job-hdr.job-no2
      tt-job-hdr.i-no
      tt-job-hdr.est-no
      tt-job-hdr.ord-no
      tt-job-hdr.cust-no
      job.start-date
      job.close-date
      job.stat
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15.71
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_job-no AT ROW 2.19 COL 6 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 2.19 COL 21 COLON-ALIGNED
     fi_i-no AT ROW 2.19 COL 30 COLON-ALIGNED NO-LABEL
     fi_cust-no AT ROW 2.19 COL 59 COLON-ALIGNED NO-LABEL
     fi_est-no AT ROW 2.19 COL 82 COLON-ALIGNED NO-LABEL
     fi_ord-no AT ROW 2.19 COL 105 COLON-ALIGNED NO-LABEL
     tb_open AT ROW 1.24 COL 129
     tb_closed AT ROW 2.43 COL 129
     btn_go AT ROW 3.62 COL 3
     btn_prev AT ROW 3.62 COL 17
     btn_next AT ROW 3.62 COL 38
     fi_sort-by AT ROW 3.62 COL 66 COLON-ALIGNED
     Browser-Table AT ROW 5.05 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 12
          FGCOLOR 9 FONT 6
     "FG Item#" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 1.24 COL 38
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 63
          FGCOLOR 9 FONT 6
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 86
          FGCOLOR 9 FONT 6
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 110
          FGCOLOR 9 FONT 6
    /* "Click on Yellow Field Sorts From 1st to Last" VIEW-AS TEXT
          SIZE 52 BY 1.19 AT ROW 3.38 COL 96  */
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 FONT 6
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.81
         WIDTH              = 148.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.tt-job-hdr,ASI.job OF ASI.tt-job-hdr"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ","
     _Where[1]         = "ASI.tt-job-hdr.job = 0"
     _FldNameList[1]   > ASI.tt-job-hdr.job-no
"tt-job-hdr.job-no" "Job#" "x(9)" "character" 8 ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.tt-job-hdr.job-no2
"tt-job-hdr.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.tt-job-hdr.i-no
"tt-job-hdr.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.tt-job-hdr.est-no
"tt-job-hdr.est-no" "Estimate#" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.tt-job-hdr.ord-no
"tt-job-hdr.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.tt-job-hdr.cust-no
"tt-job-hdr.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.job.start-date
"job.start-date" ? ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.job.close-date
"job.close-date" "Close Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.job.stat
"job.stat" "Status" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"custPart() @ custPart" "Customer Part" "X(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.tt-job-hdr.qty
"tt-job-hdr.qty" "Job Qty" "->>>,>>>,>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"orderQty() @ orderQty" "Ordered Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > "_<CALC>"
"producedQty(onHandQty) @ producedQty" "Prod. Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > "_<CALC>"
"onHandQty(qtyOnHand) @ onHandQty" "On Hand Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > "_<CALC>"
"shipQty() @ shipQty" "Shipped Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > "_<CALC>"
"invoiceQty() @ invoiceQty" "Invoice Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"wipQty() @ wipQty" "WIP Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"overUnderPct(onHandQty) @ overUnderPct" "O/U%" "->>>>>%" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"fgItemNo() @ fgItemNo" "FG Item" "X(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  DEF VAR phandle AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.

  {methods/run_link.i "container-source" "select-page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN DO:
    APPLY 'ENTRY' TO fi_job-no IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
DO:
  {methods/template/sortindicator.i} 
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.


  ASSIGN
   lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
   lv-column-nam = lh-column:NAME
   lv-column-lab = lh-column:LABEL.

  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  APPLY "choose" TO btn_go.
  {methods/template/sortindicatorend.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  IF AVAIL job THEN DO:
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(job.rec_key,{methods/headers/job.i})"}
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = job.rec_key))"}

    IF AVAIL tt-job-hdr THEN
    DO:
       ASSIGN
          v-job-rec-key = tt-job-hdr.company + "|jh" + STRING(tt-job-hdr.j-no)
          v-job-header = " Job: " + tt-job-hdr.job-no + "-" + STRING(tt-job-hdr.job-no2) + " Item#: " + tt-job-hdr.i-no.

       {methods/run_link.i "CONTAINER-SOURCE" "Set-Misc-Rec-Key_Header"
        "(v-job-rec-key,v-job-header)"}

       {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
       "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = job.rec_key))"}
     /*"(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = v-job-rec-key))"}*/
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open
     tb_closed
     fi_cust-no
     fi_i-no
     fi_ord-no
     fi_est-no
     fi_job-no
     fi_job-no2.

    RUN dispatch ("open-query").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Show Next */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
   /* RUN set-defaults.*/
   /* tb_closed:SCREEN-VALUE  = "yes".*/

    lv-show-next = YES.

    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_prev B-table-Win
ON CHOOSE OF btn_prev IN FRAME F-Main /* Show Previous */
DO:
  SESSION:SET-WAIT-STATE("general").

  DO WITH FRAME {&FRAME-NAME}:
    /*RUN set-defaults.*/
    /* tb_closed:SCREEN-VALUE  = "yes".*/

    lv-show-prev = YES.

    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON LEAVE OF fi_cust-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON LEAVE OF fi_est-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no2 B-table-Win
ON LEAVE OF fi_job-no2 IN FRAME F-Main /* - */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-no B-table-Win
ON LEAVE OF fi_ord-no IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_closed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_closed B-table-Win
ON VALUE-CHANGED OF tb_closed IN FRAME F-Main /* Closed Jobs */
DO:
  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_open
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_open B-table-Win
ON VALUE-CHANGED OF tb_open IN FRAME F-Main /* Open Jobs */
DO:
  APPLY "choose" TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SESSION:DATA-ENTRY-RETURN = YES.
&SCOPED-DEFINE key-phrase tt-job-hdr.company EQ cocode

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sort-by:HIDDEN  = TRUE.
fi_sort-by:VISIBLE = FALSE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browse-rowid B-table-Win 
PROCEDURE browse-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-rowid AS ROWID NO-UNDO.


  IF AVAIL {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    op-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Disable-Navigation B-table-Win 
PROCEDURE Disable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Enable-Navigation B-table-Win 
PROCEDURE Enable-Navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-job-no LIKE tt-job-hdr.job-no INIT "" NO-UNDO.

  &SCOPED-DEFINE where-first1                         ~
        WHERE {&key-phrase}                           ~
          AND tt-job-hdr.opened EQ YES

  RUN set-defaults.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open
     tb_closed
     fi_cust-no
     fi_i-no
     fi_cust-no
     fi_ord-no
     fi_est-no
     fi_job-no
     fi_job-no2.
  END.
  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "JCBROWSE"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "JCBROWSE"
            sys-ctrl.descrip = "# of Records to be displayed in oe browser"
            sys-ctrl.log-fld = YES
            sys-ctrl.char-fld = "JC"
            sys-ctrl.int-fld = 30.
  end.

  adm-query-opened = YES.

  IF ll-initial THEN DO:
    RELEASE tt-job-hdr.
    FIND LAST tt-job-hdr {&where-first1} USE-INDEX job-no NO-LOCK NO-ERROR.
    DO WHILE AVAIL tt-job-hdr:
      IF tt-job-hdr.job-no NE lv-job-no THEN li = li + 1.
      lv-job-no = tt-job-hdr.job-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.

      FIND PREV tt-job-hdr {&where-first1} USE-INDEX job-no NO-LOCK NO-ERROR.
    END.

    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}                 ~
        FOR EACH tt-job-hdr                        ~
            {&where-first1}                     ~
                AND tt-job-hdr.job-no GE lv-job-no ~
            USE-INDEX job-no NO-LOCK,           ~
            {&for-each2}

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
  END.

  ELSE ll-first = NO.

  ll-initial = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-job-header B-table-Win 
PROCEDURE get-job-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM opHeader_key AS cha NO-UNDO.

  opHeader_key = STRING(job.job) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                              TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-open AS LOG INIT ? NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN 
   tt-job-hdr.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   tt-job-hdr.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   tt-job-hdr.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   tt-job-hdr.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
   tt-job-hdr.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
   tt-job-hdr.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES 
   job.start-date:READ-ONLY IN BROWSE {&browse-name} = YES
   job.close-date:READ-ONLY IN BROWSE {&browse-name} = YES
   job.stat:READ-ONLY IN BROWSE {&browse-name} = YES
   .
  APPLY 'ENTRY':U TO fi_job-no IN FRAME {&FRAME-NAME}.

  RUN set-focus.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"inquiry-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
    RUN get-ip-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid, OUTPUT ll-open).

    FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      ASSIGN
       ll-first               = NO
       fi_i-no:SCREEN-VALUE   = itemfg.i-no
       tb_open:SCREEN-VALUE   = STRING(ll-open EQ ? OR ll-open)
       tb_closed:SCREEN-VALUE = STRING(ll-open EQ ? OR NOT ll-open).

      APPLY "choose" TO btn_go.
      APPLY "entry" TO fi_i-no.
    END.
  END.
  ELSE tb_closed:SCREEN-VALUE = "No".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
SESSION:SET-WAIT-STATE("general").
  /* Dispatch standard ADM method.                             */
  /*RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .*/

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN RUN first-query.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-all.
  ELSE DO:
    {jcinq/j-jobinq.i}
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    /*RUN dispatch ('get-last':U).*/
    GET LAST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-rowid2 = ROWID({&second-table-in-query-{&browse-name}})
             lv-last-show-job-no = tt-job-hdr.job-no.

    /*RUN dispatch ('get-first':U).*/
    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
       ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
              lv-frst-rowid2 = ROWID({&second-table-in-query-{&browse-name}})
              lv-first-show-job-no = tt-job-hdr.job-no.
    ll-first = NO.
  END.

/*IF AVAIL oe-ord THEN*/ APPLY "value-changed" TO BROWSE {&browse-name}.
ASSIGN
lv-show-prev = NO
lv-show-next = NO.

SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.


  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN RUN dispatch ('get-next':U).
    WHEN "P" THEN RUN dispatch ('get-prev':U).
  END CASE.

  IF ROWID(tt-job-hdr) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID(tt-job-hdr) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser2 B-table-Win 
PROCEDURE navigate-browser2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT  PARAMETER ip-nav-type AS CHAR.
  DEF OUTPUT PARAMETER op-nav-type AS CHAR.

  DEF VAR hld-rowid AS ROWID NO-UNDO.


  hld-rowid = ROWID(job).

  IF ip-nav-type NE "" THEN
  CASE ip-nav-type:
    WHEN "F" THEN RUN dispatch ('get-first':U).
    WHEN "L" THEN RUN dispatch ('get-last':U).
    WHEN "N" THEN DO WHILE ROWID(job) EQ hld-rowid:
                    RUN dispatch ('get-next':U).
                  END. 
    WHEN "P" THEN DO WHILE ROWID(job) EQ hld-rowid:
                    RUN dispatch ('get-prev':U).
    END.
  END CASE.

  IF ROWID(job) EQ lv-last-rowid2 THEN
    op-nav-type = "L".

  IF ROWID(job) EQ lv-frst-rowid2 THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE record-added B-table-Win 
PROCEDURE record-added :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ll-initial = browser-log.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-tt-job-hdr FOR tt-job-hdr.
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  FIND bf-tt-job-hdr WHERE ROWID(bf-tt-job-hdr) = ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL bf-tt-job-hdr THEN DO WITH FRAME F-Main:
      ASSIGN fi_cust-no:SCREEN-VALUE = ""
             fi_est-no:SCREEN-VALUE  = ""
             fi_i-no:SCREEN-VALUE    = ""
             fi_job-no:SCREEN-VALUE  = bf-tt-job-hdr.job-no
             fi_job-no2:SCREEN-VALUE = STRING(bf-tt-job-hdr.job-no2, "999")
             fi_ord-no:SCREEN-VALUE  = ""
             fi_sort-by:SCREEN-VALUE = "".
    ASSIGN
     tb_open = YES
     tb_closed
     fi_cust-no
     fi_i-no
     fi_ord-no
     fi_est-no
     fi_job-no
     fi_job-no2.
  END.

  RUN dispatch ('open-query').
  DO WITH FRAME {&frame-name}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.
  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  RUN dispatch ('open-query').

  DO WITH FRAME {&frame-name}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
  END.

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "tt-job-hdr"}
  {src/adm/template/snd-list.i "job"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open:SCREEN-VALUE    = "yes"
     tb_closed:SCREEN-VALUE  = "no"
     fi_cust-no:SCREEN-VALUE = ""
     fi_i-no:SCREEN-VALUE    = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_job-no:SCREEN-VALUE  = ""
     fi_job-no2:SCREEN-VALUE = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*{methods/setfocus.i {&BROWSE-NAME}}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO fi_job-no.
  END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-all B-table-Win 
PROCEDURE show-all :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR li AS INT NO-UNDO.
DEF VAR lv-job-no AS cha NO-UNDO.

&SCOPED-DEFINE for-each-f1                            ~
    FOR EACH tt-job-hdr                                  ~
        WHERE {&key-phrase}                           ~
          AND tt-job-hdr.opened                      

&SCOPED-DEFINE for-each-f2       ~
    FIRST job OF tt-job-hdr NO-LOCK

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "JCBROWSE"
                        no-lock no-error.
if not avail sys-ctrl then do transaction:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name    = "JCBROWSE"
          sys-ctrl.descrip = "# of Records to be displayed in OE browser"
          sys-ctrl.log-fld = YES
          sys-ctrl.char-fld = "CE"
          sys-ctrl.int-fld = 30.
end.

RUN set-defaults. 

IF lv-show-prev THEN DO:


        {&for-each-f1}                    ~
                AND tt-job-hdr.job-no lE lv-last-show-job-no  ~
            USE-INDEX job-no  NO-LOCK,   ~
            {&for-each-f2} BREAK BY tt-job-hdr.job-no DESC:


     IF FIRST-OF(tt-job-hdr.job-no) THEN li = li + 1.
     lv-job-no = tt-job-hdr.job-no.
     IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each-f1}                    ~
                AND tt-job-hdr.job-no gE lv-job-no  ~
                AND tt-job-hdr.job-no LE lv-last-show-job-no ~
            USE-INDEX job-no  NO-LOCK,   ~
            {&for-each-f2}

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:

    {&for-each-f1}                    ~
          AND tt-job-hdr.job-no gE lv-first-show-job-no  ~
          USE-INDEX job-no  NO-LOCK,   ~
       {&for-each-f2} BREAK BY tt-job-hdr.job-no  :

       IF FIRST-OF(tt-job-hdr.job-no) THEN li = li + 1.
       lv-job-no = tt-job-hdr.job-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
            {&for-each-f1}                    ~
                AND tt-job-hdr.job-no lE lv-job-no  ~
                AND tt-job-hdr.job-no gE lv-first-show-job-no ~
            USE-INDEX job-no  NO-LOCK,   ~
            {&for-each-f2}


    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                           ELSE {&open-query} {&sortby-phrase-desc}. 

END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION custPart B-table-Win 
FUNCTION custPart RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RELEASE itemfg.

  IF AVAIL tt-job-hdr THEN
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ tt-job-hdr.company
        AND itemfg.i-no    EQ tt-job-hdr.i-no
      NO-ERROR.

  RETURN IF AVAIL itemfg THEN itemfg.part-no ELSE "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fgItemNo B-table-Win 
FUNCTION fgItemNo RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF AVAIL tt-job-hdr THEN tt-job-hdr.i-no ELSE ''.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION invoiceQty B-table-Win 
FUNCTION invoiceQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.inv-qty.
  END. /* avail tt-job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION onHandQty B-table-Win 
FUNCTION onHandQty RETURNS INTEGER
  (OUTPUT opQtyOnHand AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
    FOR EACH fg-bin fields(qty) NO-LOCK
        WHERE fg-bin.company EQ oe-ordl.company
          AND fg-bin.job-no EQ oe-ordl.job-no
          AND fg-bin.job-no2 EQ oe-ordl.job-no2
          AND fg-bin.i-no EQ oe-ordl.i-no:
      rtnValue = rtnValue + fg-bin.qty.
    END. /* each fg-bin */
  END. /* avail tt-job-hdr */
  opQtyOnHand = rtnValue.
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION orderQty B-table-Win 
FUNCTION orderQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    rtnValue = oe-ordl.qty.
  END. /* avail tt-job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION overUnderPct B-table-Win 
FUNCTION overUnderPct RETURNS INTEGER
  (ipBalance AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:
    rtnValue = ((ipBalance / oe-ordl.qty) - 1) * 100.
    IF rtnValue EQ 0 THEN rtnValue = 100.
    IF rtnValue EQ -100 THEN rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail tt-job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION producedQty B-table-Win 
FUNCTION producedQty RETURNS INTEGER
  (OUTPUT opBalance AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN
    DO:
       IF oe-ordl.job-no NE '' THEN
          FOR EACH fg-rcpth fields(r-no rita-code) NO-LOCK
             WHERE fg-rcpth.company EQ oe-ordl.company
               AND fg-rcpth.job-no EQ oe-ordl.job-no
               AND fg-rcpth.job-no2 EQ oe-ordl.job-no2
               AND fg-rcpth.i-no EQ oe-ordl.i-no
               AND fg-rcpth.rita-code EQ 'R' USE-INDEX job,
              EACH fg-rdtlh FIELDS(qty) NO-LOCK
             WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
               AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
              rtnValue = rtnValue + fg-rdtlh.qty.
       END.
      ELSE
         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
             WHERE fg-rcpth.company   EQ cocode
               AND fg-rcpth.job-no    EQ tt-job-hdr.job-no
               AND fg-rcpth.job-no2   EQ tt-job-hdr.job-no2
               AND fg-rcpth.i-no      EQ oe-ordl.i-no
               AND fg-rcpth.rita-code EQ "R"
               USE-INDEX job,
             EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                  fg-rdtlh.r-no      EQ fg-rcpth.r-no AND
                  fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                  rtnValue = rtnValue + fg-rdtlh.qty.
         END.
    END. /* avail oe-ordl */
    ELSE DO:

        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
            WHERE fg-rcpth.company   EQ cocode
              AND fg-rcpth.job-no    EQ tt-job-hdr.job-no
              AND fg-rcpth.job-no2   EQ tt-job-hdr.job-no2
              AND fg-rcpth.i-no      EQ tt-job-hdr.i-no
              AND fg-rcpth.rita-code EQ "R"
              USE-INDEX job,
            EACH fg-rdtlh FIELDS(qty) NO-LOCK WHERE
                 fg-rdtlh.r-no      EQ fg-rcpth.r-no AND
                 fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                 rtnValue = rtnValue + fg-rdtlh.qty.
        END.

    END.
  END. /* avail tt-job-hdr */
  opBalance = rtnValue.
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION shipQty B-table-Win 
FUNCTION shipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.
  DEF VAR li-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
  DEF VAR li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      RUN oe/ordlsqty.p (ROWID(oe-ordl),
                         OUTPUT li-inv-qty, OUTPUT li-ship-qty).

      rtnValue = li-ship-qty.
    END.
  END. /* avail tt-job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION wipQty B-table-Win 
FUNCTION wipQty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE tt-job-hdr THEN DO:
    FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ tt-job-hdr.company
                                 AND oe-ordl.i-no EQ tt-job-hdr.i-no
                                 AND oe-ordl.ord-no EQ tt-job-hdr.ord-no NO-ERROR.
    IF AVAILABLE oe-ordl THEN DO:
      FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
      rtnValue = oe-ordl.qty - (qtyOnHand + oe-ordl.ship-qty).
      IF rtnValue LT 0 OR
         rtnValue LT oe-ordl.qty *
                     (IF AVAIL oe-ord THEN oe-ordl.under-pct ELSE 100) / 100 THEN
        rtnValue = 0.
    END. /* avail oe-ordl */
  END. /* avail tt-job-hdr */
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

