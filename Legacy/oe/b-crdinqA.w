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

  File:  oe\b-crdinq.w

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

{sys/inc/oeinq.i}

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "ord-no" /*"req-date" */ NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Order#" /*"Due Date"*/  NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.

ll-sort-asc = NO /*oeinq*/  .

&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode

&SCOPED-DEFINE for-each1                            ~
    FOR EACH oe-ordl                                ~
        WHERE oe-ordl.company   EQ g_company        ~
          AND oe-ordl.cust-no   BEGINS fi_cust-no   ~
          AND oe-ordl.i-no      BEGINS fi_i-no      ~
          AND oe-ordl.part-no   BEGINS fi_part-no   ~
          AND oe-ordl.po-no     BEGINS fi_po-no     ~
          AND oe-ordl.est-no    BEGINS fi_est-no    ~
          AND oe-ordl.job-no    BEGINS fi_job-no    ~
          AND (oe-ordl.job-no2  EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "") ~
          AND oe-ordl.opened    EQ YES

&SCOPED-DEFINE for-each2                                   ~
    FIRST oe-ord OF oe-ordl                                ~
    WHERE ((INDEX("H",oe-ord.stat) EQ 0 AND tb_appr)   OR  ~
           (INDEX("H",oe-ord.stat) GT 0 AND tb_held))      ~
    USE-INDEX ord-no NO-LOCK

&SCOPED-DEFINE for-each11                           ~
    FOR EACH oe-ordl                                ~
        WHERE oe-ordl.company   EQ g_company        ~
          AND oe-ordl.opened    EQ YES

&SCOPED-DEFINE for-each21                                  ~
    FIRST oe-ord OF oe-ordl                                ~
    WHERE INDEX("H",oe-ord.stat) EQ 0                      ~
    USE-INDEX ord-no NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "ord-no"    THEN STRING(oe-ordl.ord-no,"9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "stat"      THEN oe-ord.stat                                                                                                      ELSE ~
    IF lv-sort-by EQ "ord-date"  THEN STRING(YEAR(oe-ord.ord-date),"9999") + STRING(MONTH(oe-ord.ord-date),"99") + STRING(DAY(oe-ord.ord-date),"99")   ELSE ~
    IF lv-sort-by EQ "cust-no"   THEN oe-ordl.cust-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "cust-name" THEN oe-ordl.cust-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "i-no"      THEN oe-ordl.i-no                                                                                                     ELSE ~
    IF lv-sort-by EQ "part-no"   THEN oe-ordl.part-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "po-no"     THEN oe-ordl.po-no                                                                                                    ELSE ~
    IF lv-sort-by EQ "est-no"    THEN oe-ordl.est-no                                                                                                   ELSE ~
    IF lv-sort-by EQ "job-no"    THEN STRING(oe-ordl.job-no,"x(6)") + STRING(oe-ordl.job-no2,"99")                                                     ELSE ~
    IF lv-sort-by EQ "spare-char-2"    THEN oe-ord.spare-char-2                                                                                                   ELSE ~
    IF lv-sort-by EQ "approved-date"    THEN STRING(YEAR(oe-ord.approved-date),"9999") + STRING(MONTH(oe-ord.approved-date),"99") + STRING(DAY(oe-ord.approved-date),"99")                                                     ELSE ~
                                      STRING(YEAR(oe-ordl.req-date),"9999") + STRING(MONTH(oe-ordl.req-date),"99") + STRING(DAY(oe-ordl.req-date),"99")

&SCOPED-DEFINE sortby BY oe-ordl.ord-no BY oe-ordl.i-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

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
&Scoped-define INTERNAL-TABLES oe-ordl oe-ord

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no oe-ord.stat ~
oe-ord.ord-date oe-ordl.req-date oe-ordl.cust-no oe-ord.cust-name ~
oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no oe-ordl.job-no ~
oe-ordl.job-no2 oe-ord.spare-char-2 oe-ord.approved-date
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no ~
oe-ord.stat oe-ord.ord-date oe-ordl.req-date oe-ordl.cust-no ~
oe-ord.cust-name oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no ~
oe-ordl.job-no oe-ordl.job-no2 oe-ord.spare-char-2 oe-ord.approved-date
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ord
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.ord-no gt 9999999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.ord-no gt 9999999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table oe-ord


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_ord-no fi_cust-no fi_i-no fi_part-no ~
fi_po-no fi_est-no fi_job-no fi_job-no2 tb_appr tb_held btn_go btn_show ~
Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_ord-no fi_cust-no fi_i-no fi_part-no ~
fi_po-no fi_est-no fi_job-no fi_job-no2 fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 4.05.

DEFINE VARIABLE tb_appr AS LOGICAL INITIAL yes 
     LABEL "Approved" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE tb_held AS LOGICAL INITIAL no 
     LABEL "On Hold" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-ordl
    FIELDS(oe-ordl.ord-no
      oe-ordl.req-date
      oe-ordl.cust-no
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2), 
      oe-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-ordl.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      oe-ord.stat COLUMN-LABEL "Status" FORMAT "x":U LABEL-BGCOLOR 14
      oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      oe-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      oe-ord.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      oe-ordl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      oe-ordl.po-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U LABEL-BGCOLOR 14
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      oe-ord.spare-char-2 COLUMN-LABEL "Hold Reason code"     
            LABEL-BGCOLOR 14
      oe-ord.approved-date COLUMN-LABEL "Hold/Approved Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
  ENABLE
      oe-ordl.ord-no
      oe-ord.stat
      oe-ord.ord-date
      oe-ordl.req-date
      oe-ordl.cust-no
      oe-ord.cust-name
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2
      oe-ord.spare-char-2
      oe-ord.approved-date
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 15.48
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_ord-no AT ROW 2.19 COL 2 NO-LABEL
     fi_cust-no AT ROW 2.19 COL 15 COLON-ALIGNED NO-LABEL
     fi_i-no AT ROW 2.19 COL 30 COLON-ALIGNED NO-LABEL
     fi_part-no AT ROW 2.19 COL 51 COLON-ALIGNED NO-LABEL
     fi_po-no AT ROW 2.19 COL 72 COLON-ALIGNED NO-LABEL
     fi_est-no AT ROW 2.19 COL 93 COLON-ALIGNED NO-LABEL
     fi_job-no AT ROW 2.19 COL 110 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 2.19 COL 121 COLON-ALIGNED
     tb_appr AT ROW 1.24 COL 130
     tb_held AT ROW 2.43 COL 130
     btn_go AT ROW 3.62 COL 6
     btn_show AT ROW 3.62 COL 22
     fi_sort-by AT ROW 3.62 COL 49 COLON-ALIGNED NO-LABEL
     Browser-Table AT ROW 5.05 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 116
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 39
          FONT 6
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 4
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 18
          FGCOLOR 9 FONT 6
     "FG Item#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 36
          FGCOLOR 9 FONT 6
     "Cust Part#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 56
          FGCOLOR 9 FONT 6
     "Customer PO#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 75
          FGCOLOR 9 FONT 6
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 96
          FGCOLOR 9 FONT 6
     "Click on Yellow Field, Sorts From 1st to Last" VIEW-AS TEXT
          SIZE 43 BY .95 AT ROW 3.62 COL 87
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
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
         HEIGHT             = 19.76
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

/* SETTINGS FOR FILL-IN fi_ord-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tb_appr IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       tb_appr:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tb_held IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       tb_held:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _Where[1]         = "oe-ordl.ord-no gt 9999999999"
     _FldNameList[1]   > ASI.oe-ordl.ord-no
"oe-ordl.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ord.stat
"oe-ord.stat" "Status" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ord.ord-date
"oe-ord.ord-date" "Order Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-ordl.req-date
"oe-ordl.req-date" "Due Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-ordl.cust-no
"oe-ordl.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-ord.cust-name
"oe-ord.cust-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" "Cust Part#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" "Est#" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
_FldNameList[13]   > ASI.oe-ord.spare-char-2
"oe-ord.spare-char-2" "Hold Reason code" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
_FldNameList[14]   > ASI.oe-ord.approved-date
"oe-ord.approved-date" "Hold/Approved Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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

  RUN dispatch ("open-query").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:

  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.
  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(oe-ord.rec_key,{methods/headers/oe-ord.i})"}
  {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
     "(CAN-FIND(FIRST notes WHERE notes.rec_key = oe-ord.rec_key))"}
  {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
     "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = oe-ord.rec_key))"}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_appr
     tb_held
     fi_cust-no
     fi_i-no
     fi_part-no
     fi_cust-no
     fi_ord-no
     fi_po-no
     fi_est-no
     fi_job-no
     fi_job-no2.

    ll-first = NO.

    RUN dispatch ("open-query").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    RUN set-defaults.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON VALUE-CHANGED OF fi_po-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 1 + IF LASTKEY EQ 32 THEN 1 ELSE 0. /* added by script _caps.p */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SESSION:DATA-ENTRY-RETURN = YES.

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apply-arrow B-table-Win 
PROCEDURE apply-arrow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
APPLY 'cursor-down' TO BROWSE {&browse-name}.
APPLY 'cursor-up' TO BROWSE {&browse-name}.
APPLY 'value-changed' TO BROWSE {&browse-name}.
BROWSE {&browse-name}:SELECT-FOCUSED-ROW ( ).
BROWSE {&browse-name}:SCROLL-TO-CURRENT-ROW ( ).
/*                                                  */
/* QUERY {&browse-name}:REPOSITION-TO-ROW (1 ).     */
RUN notify ('row-available':U).
RUN adm-row-changed.
  RUN adm-row-available.

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

  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('disable':U) NO-ERROR"}

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

  {methods/run_link.i "NAVIGATION-SOURCE" "dispatch" "('enable':U) NO-ERROR"}

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
  DEF BUFFER b-oe-ord FOR oe-ord.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ordl.ord-no NO-UNDO.

  RUN set-defaults.

  tb_appr = YES.
  tb_held = NO.

  DISPLAY tb_appr tb_held WITH FRAME {&FRAME-NAME}.

  FOR EACH b-oe-ord           
      WHERE b-oe-ord.company EQ g_company
        AND b-oe-ord.opened  EQ YES
        AND ((b-oe-ord.stat NE "H" AND tb_appr) OR
             (b-oe-ord.stat EQ "H" AND tb_held))
      USE-INDEX opened NO-LOCK
      BY b-oe-ord.ord-no DESC:
    ASSIGN
     li        = li + 1
     lv-ord-no = b-oe-ord.ord-no.
    IF li GE 30 THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each11}                         ~
              AND oe-ordl.ord-no GE lv-ord-no ~
            USE-INDEX opened NO-LOCK,         ~
            {&for-each21}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-line-est B-table-Win 
PROCEDURE get-line-est :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output parameter op-est-no as cha no-undo.


  op-est-no = if available oe-ordl then oe-ordl.est-no else oe-ord.est-no.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN oe-ordl.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.stat:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.ord-date:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.req-date:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.cust-name:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.part-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.spare-char-2:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.approved-date:READ-ONLY IN BROWSE {&browse-name} = YES
      .

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

  tb_appr = YES.
  tb_held = NO.
  /* Dispatch standard ADM method.                             */
  IF NOT ll-first THEN
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .    /* Task 11061208 */

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-first THEN RUN first-query.

  ELSE DO:
    {oe/j-crdinqA.i}
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    GET LAST {&browse-name} NO-LOCK.

    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}}).

    GET FIRST {&browse-name} NO-LOCK.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}}).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-view B-table-Win 
PROCEDURE local-view :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'view':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/setvalue.i}

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

  IF ROWID(oe-ordl) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID(oe-ordl) EQ lv-frst-rowid THEN
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

  ll-first = YES.

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
 DEF VAR lv-tmp-rowid AS ROWID NO-UNDO.
 lv-tmp-rowid = ROWID(oe-ordl).

 RUN reopen-query1 (lv-tmp-rowid).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query1 B-table-Win 
PROCEDURE reopen-query1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ord  FOR oe-ord.


  FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL b-oe-ord THEN DO:
    FIND FIRST b-oe-ordl OF b-oe-ord NO-LOCK.
    ip-rowid = ROWID(b-oe-ordl).
  END.

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select-his B-table-Win 
PROCEDURE select-his :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  find first cust {sys/ref/custW.i} and
                  cust.cust-no eq oe-ord.cust-no
                  use-index cust no-lock no-error.

  def var char-hdl as cha no-undo.
  run get-link-handle in adm-broker-hdl (this-procedure,"container-source",output char-hdl).
  run init-history in widget-handle(char-hdl) (this-procedure).

  /*run dispatch ('open-query'). ?? */

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
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "oe-ord"}

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
     tb_appr:SCREEN-VALUE    = "yes"
     tb_held:SCREEN-VALUE    = "yes"
     fi_cust-no:SCREEN-VALUE = ""
     fi_i-no:SCREEN-VALUE    = ""
     fi_part-no:SCREEN-VALUE = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_po-no:SCREEN-VALUE   = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_job-no:SCREEN-VALUE  = ""
     fi_job-no2:SCREEN-VALUE = ""
     tb_appr
     tb_held
     fi_cust-no
     fi_i-no
     fi_part-no
     fi_ord-no
     fi_po-no
     fi_est-no
     fi_job-no
     fi_job-no2.
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

  {methods/setfocus.i {&BROWSE-NAME}}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&BROWSE-NAME}.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR first-cust AS CHAR NO-UNDO.

/*
GET FIRST Browser-Table .
ASSIGN first-cust = cust.cust-no .
GET LAST Browser-Table .
ASSIGN last-cust = cust.cust-no . */

DO WITH FRAME {&FRAME-NAME}:

    IF tb_appr:SCREEN-VALUE EQ "Yes" THEN
        ASSIGN first-cust = "Yes".
    ELSE
        ASSIGN first-cust = "No". 

    RUN oerep/crap-ord.w (first-cust).
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

