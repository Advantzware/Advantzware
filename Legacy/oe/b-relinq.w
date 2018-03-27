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

  File:  oe\b-relinq.w

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

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "release#" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Release#" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-rel-no AS int NO-UNDO.
DEF VAR lv-first-show-rel-no AS int NO-UNDO.
DEF VAR lActive AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-shipto-zone AS CHARACTER NO-UNDO.
DEFINE VARIABLE begin_rno LIKE oe-rell.r-no NO-UNDO.
DEFINE VARIABLE ending_rno LIKE oe-rell.r-no NO-UNDO.
DEFINE VARIABLE ld-qty-oh LIKE itemfg.q-onh NO-UNDO.
DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""OT1"" }
END.

&SCOPED-DEFINE key-phrase oe-relh.company EQ cocode AND oe-relh.stat NE "W"

&SCOPED-DEFINE where1 oe-relh                    ~
        WHERE {&key-phrase}       ~
          AND oe-relh.deleted EQ NO              ~
          AND (oe-relh.posted  EQ NO  OR tb_posted = TRUE)    ~
          AND ( (lookup(oe-relh.cust-no,custcount) <> 0 AND oe-relh.cust-no <> "")  OR custcount = "")             ~
          AND oe-relh.cust-no BEGINS fi_cust-no

&SCOPED-DEFINE for-each1                         ~
    FOR EACH {&where1}

&SCOPED-DEFINE for-each2                            ~
        EACH oe-rell USE-INDEX r-no NO-LOCK         ~
        WHERE oe-rell.company   EQ oe-relh.company  ~
          AND oe-rell.r-no      EQ oe-relh.r-no     ~
          AND oe-rell.i-no      BEGINS fi_i-no      ~
          AND (oe-rell.ord-no   EQ fi_ord-no OR fi_ord-no EQ 0) ~
          AND oe-rell.po-no     BEGINS fi_po-no     ~
          AND oe-rell.job-no    BEGINS fi_job-no    ~
          AND (oe-rell.job-no2  EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ ""), ~
       FIRST itemfg OF oe-rell NO-LOCK

&SCOPED-DEFINE where1blank oe-relh               ~
        WHERE {&key-phrase}       ~
          AND ( (lookup(oe-relh.cust-no,custcount) <> 0 AND oe-relh.cust-no <> "")  OR custcount = "")             ~
          AND oe-relh.deleted EQ NO              ~
          AND oe-relh.posted LE tb_posted 

&SCOPED-DEFINE for-each1blank                       ~
    FOR EACH {&where1blank}

&SCOPED-DEFINE for-each2blank                       ~
        EACH oe-rell USE-INDEX r-no NO-LOCK         ~
        WHERE oe-rell.company   EQ oe-relh.company  ~
          AND oe-rell.r-no      EQ oe-relh.r-no,    ~
       FIRST itemfg OF oe-rell NO-LOCK

&SCOPED-DEFINE sortby-log                                                                                                                                    ~
    IF lv-sort-by EQ "ord-no"    THEN STRING(oe-rell.ord-no,"9999999999")                                                                               ELSE ~
    IF lv-sort-by EQ "release#"  THEN STRING(oe-relh.release#,"9999999999")                                                                             ELSE ~
    IF lv-sort-by EQ "cust-no"   THEN oe-relh.cust-no                                                                                                   ELSE ~
    IF lv-sort-by EQ "part-no"   THEN itemfg.part-no                                                                                                    ELSE ~
    IF lv-sort-by EQ "ship-id"   THEN oe-relh.ship-id                                                                                                   ELSE ~
    IF lv-sort-by EQ "ord-date"  THEN STRING(YEAR(oe-relh.rel-date),"9999") + STRING(MONTH(oe-relh.rel-date),"99") + STRING(DAY(oe-relh.rel-date),"99") ELSE ~
    IF lv-sort-by EQ "i-no"      THEN oe-rell.i-no                                                                                                      ELSE ~
    IF lv-sort-by EQ "po-no"     THEN oe-rell.po-no                                                                                                     ELSE ~
    IF lv-sort-by EQ "rel-date"  THEN STRING(YEAR(oe-relh.rel-date),"9999") + STRING(MONTH(oe-relh.rel-date),"99") + STRING(DAY(oe-relh.rel-date),"99") ELSE ~
    IF lv-sort-by EQ "job-no"    THEN STRING(oe-rell.job-no,"x(6)") + STRING(oe-rell.job-no2,"99")                                                      ELSE ~
    IF lv-sort-by EQ "qty"       THEN STRING(oe-rell.qty,"9999999999")                                                                                  ELSE ~
    IF lv-sort-by EQ "q-onh"       THEN STRING(itemfg.q-onh,"9999999999")                                                                               ELSE ~
    IF lv-sort-by EQ "v-shipto-zone"  THEN get-shipto-zone()                                                                                       ELSE ~
                                      STRING(oe-relh.printed, "Y/N")                                                
    IF lv-sort-by EQ "ld-qty-oh"       THEN string(fQty-oh(),"->>,>>>,>>9")                                                                                                   ELSE ~
                                      STRING(oe-relh.printed, "Y/N")

&SCOPED-DEFINE sortby BY oe-relh.release# BY oe-rell.i-no

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
&Scoped-define INTERNAL-TABLES oe-relh oe-rell itemfg

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-relh.release# ~
oe-rell.ord-no oe-rell.po-no oe-relh.cust-no itemfg.part-no oe-relh.ship-id ~
oe-rell.i-no oe-relh.rel-date oe-rell.job-no oe-rell.job-no2 ~
oe-relh.printed oe-rell.qty fQty-oh () @ ld-qty-oh itemfg.q-onh get-shipto-zone() @ v-shipto-zone 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-relh.release# ~
oe-rell.ord-no oe-rell.po-no oe-relh.cust-no oe-relh.ship-id oe-rell.i-no ~
oe-relh.rel-date oe-rell.job-no oe-rell.job-no2 oe-relh.printed 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-relh oe-rell
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-relh
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table oe-rell
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-relh WHERE ~{&KEY-PHRASE} ~
      AND oe-relh.company = cocode and  ~
oe-relh.release# eq 999999999 ~
use-index release# NO-LOCK, ~
      EACH oe-rell WHERE oe-rell.company = oe-relh.company ~
  AND oe-rell.r-no = oe-relh.r-no ~
use-index r-no NO-LOCK, ~
      EACH itemfg OF oe-rell NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-relh WHERE ~{&KEY-PHRASE} ~
      AND oe-relh.company = cocode and  ~
oe-relh.release# eq 999999999 ~
use-index release# NO-LOCK, ~
      EACH oe-rell WHERE oe-rell.company = oe-relh.company ~
  AND oe-rell.r-no = oe-relh.r-no ~
use-index r-no NO-LOCK, ~
      EACH itemfg OF oe-rell NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-relh oe-rell itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-relh
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table oe-rell
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table itemfg


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_posted fi_rel-no fi_ord-no fi_cust-no ~
fi_i-no fi_po-no fi_job-no fi_job-no2 btn_go btn_prev Browser-Table RECT-1 
&Scoped-Define DISPLAYED-OBJECTS tb_posted fi_rel-no fi_ord-no fi_cust-no ~
fi_i-no fi_po-no fi_job-no fi_job-no2 fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-shipto-zone B-table-Win 
FUNCTION get-shipto-zone RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fQty-oh B-table-Win 
FUNCTION fQty-oh RETURNS INTEGER
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
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
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

DEFINE VARIABLE fi_po-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rel-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 4.05.

DEFINE VARIABLE tb_posted AS LOGICAL INITIAL no 
     LABEL "Posted?" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-relh
    FIELDS(oe-relh.release#
      oe-relh.cust-no
      oe-relh.ship-id
      oe-relh.rel-date
      oe-relh.printed), 
      oe-rell, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-relh.release# FORMAT ">>>>>>>>>>":U WIDTH 15 LABEL-BGCOLOR 14
      oe-rell.ord-no FORMAT ">>>>>9":U WIDTH 9 LABEL-BGCOLOR 14
      oe-rell.po-no FORMAT "x(15)":U WIDTH 22 LABEL-BGCOLOR 14
      oe-relh.cust-no FORMAT "x(8)":U WIDTH 12 LABEL-BGCOLOR 14
      itemfg.part-no FORMAT "x(15)":U WIDTH 22 LABEL-BGCOLOR 14
      oe-relh.ship-id COLUMN-LABEL "Ship To" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-rell.i-no COLUMN-LABEL "FG Item Number" FORMAT "x(15)":U
            WIDTH 22 LABEL-BGCOLOR 14
      oe-relh.rel-date COLUMN-LABEL "Release Date" FORMAT "99/99/9999":U
            WIDTH 15 LABEL-BGCOLOR 14
      oe-rell.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U WIDTH 9
            LABEL-BGCOLOR 14
      oe-rell.job-no2 COLUMN-LABEL "" FORMAT "99":U LABEL-BGCOLOR 14
      oe-relh.printed FORMAT "Y/N":U LABEL-BGCOLOR 14
      oe-rell.qty COLUMN-LABEL "Release Qty" FORMAT "->>,>>>,>>9":U
            LABEL-BGCOLOR 14
      fQty-oh () @ ld-qty-oh COLUMN-LABEL "Qty On Hand" FORMAT "->>,>>>,>>9":U
      itemfg.q-onh COLUMN-LABEL "Qty On Hand" FORMAT "->,>>>,>>9":U
            LABEL-BGCOLOR 14
      get-shipto-zone() @ v-shipto-zone COLUMN-LABEL "Ship To Zone" FORMAT "x(5)":U
            LABEL-BGCOLOR 14
  ENABLE
      oe-relh.release#
      oe-rell.ord-no
      oe-rell.po-no
      oe-relh.cust-no
      oe-relh.ship-id
      oe-rell.i-no
      oe-relh.rel-date
      oe-rell.job-no
      oe-rell.job-no2
      oe-relh.printed
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 141 BY 16.60
    FONT 2. 


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_posted AT ROW 2.19 COL 118 WIDGET-ID 2
     fi_rel-no AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABEL
     fi_ord-no AT ROW 2.19 COL 20 NO-LABEL
     fi_cust-no AT ROW 2.19 COL 34 COLON-ALIGNED NO-LABEL
     fi_i-no AT ROW 2.19 COL 50 COLON-ALIGNED NO-LABEL
     fi_po-no AT ROW 2.19 COL 72 COLON-ALIGNED NO-LABEL
     fi_job-no AT ROW 2.19 COL 94 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 2.19 COL 106 COLON-ALIGNED
     btn_go AT ROW 3.62 COL 2
     fi_sort-by AT ROW 3.62 COL 70 COLON-ALIGNED NO-LABEL
     btn_prev AT ROW 3.62 COL 17
     Browser-Table AT ROW 5.05 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btn_next AT ROW 3.62 COL 38
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 99
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 3.62 COL 59
          FONT 6
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 23
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 37
          FGCOLOR 9 FONT 6
     "FG Item#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 55
          FGCOLOR 9 FONT 6
     "Customer PO#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 75
          FGCOLOR 9 FONT 6
     "Release#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 4
          FGCOLOR 9 FONT 6
     "Click on Yellow Field, Sorts From 1st to Last" VIEW-AS TEXT
          SIZE 42 BY .95 AT ROW 3.62 COL 102
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 
         DEFAULT-BUTTON btn_go.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser Template
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 20.1
         WIDTH              = 144.4.
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
/* BROWSE-TAB Browser-Table btn_prev F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_ord-no IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.oe-relh,ASI.oe-rell WHERE ASI.oe-relh  ...,ASI.itemfg OF ASI.oe-rell"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,,"
     _Where[1]         = "oe-relh.company = cocode and 
oe-relh.release# eq 999999999
use-index release#"
     _JoinCode[2]      = "ASI.oe-rell.company = ASI.oe-relh.company
  AND ASI.oe-rell.r-no = ASI.oe-relh.r-no
use-index r-no"
     _FldNameList[1]   > ASI.oe-relh.release#
"oe-relh.release#" ? ">>>>>>>>>>" "integer" ? ? ? 14 ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-rell.ord-no
"oe-rell.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-rell.po-no
"oe-rell.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-relh.cust-no
"oe-relh.cust-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.itemfg.part-no
"itemfg.part-no" ? "x(15)" "character" ? ? ? 14 ? ? no ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-relh.ship-id
"oe-relh.ship-id" "Ship To" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-rell.i-no
"oe-rell.i-no" "FG Item Number" ? "character" ? ? ? 14 ? ? yes ? no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-relh.rel-date
"oe-relh.rel-date" "Release Date" ? "date" ? ? ? 14 ? ? yes ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-rell.job-no
"oe-rell.job-no" "Job #" ? "character" ? ? ? 14 ? ? yes ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-rell.job-no2
"oe-rell.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-relh.printed
"oe-relh.printed" ? ? "logical" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-rell.qty
"oe-rell.qty" "Release Qty" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[13]   > "_<CALC>"
"fqty-oh () @ ld-qty-oh" "Qty On Hand" ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[14]   > ASI.itemfg.q-onh
"itemfg.q-onh" "Qty On Hand" "->,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"     
"get-shipto-zone() @ v-shipto-zone" "Ship To Zone" "x(8)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "SmartBrowserCues" B-table-Win _INLINE
/* Actions: adecomm/_so-cue.w ? adecomm/_so-cued.p ? adecomm/_so-cuew.p */
/* SmartBrowser,uib,49266
A SmartBrowser is a procedure object that retrieves and visualizes data using a browse.

CREATING A MASTER

Step 1
If necessary, specify an external table (steps 1a-1c). 

An external table completes join criteria for a query. For example, a query on 'Order OF Customer' requires the external table Customer. The external table is supplied by another procedure object--typically a SmartBrowser or SmartViewer.

Step 1a
In the UIB main window, Choose the Procedure button.

Step 1b
In the Procedure Settings dialog, choose Add.

Step 1c
From the Table Selector dialog, select the external table.

Step 2 
Double-click the browse to invoke the Query Builder.

Step 3
Using the Query Builder, specify the tables and fields for the browse.

Step 4 [Optional]
In the Code Section Editor, change the Foreign Keys and/or Sort Options for the browse query. Use the "List..." button to access these sections.

Step 5
Save and close the SmartBrowser master.

INSERTING AN INSTANCE

Step 1
Open or create a SmartContainer, such as a SmartWindow.

Step 2 
Choose the SmartBrowser master from the Object Palette.

Step 3
Draw the SmartBrowser instance into the SmartContainer.

Step 4
Add all necessary SmartLinks between the SmartBrowser and other SmartObjects. 

During assembly, the PROGRESS Advisor suggests links and creates them for you. However, you can also add and remove SmartLinks with the SmartLinks dialog box. To access this dialog box, choose the Procedure button from the UIB main window. Then choose the SmartLinks button from the Procedure Settings dialog box.
*/
/* _UIB-CODE-BLOCK-END */
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
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN DO:
    APPLY 'ENTRY' TO fi_rel-no IN FRAME {&FRAME-NAME}.                  /*Task# 02121406*/
    RETURN NO-APPLY.
  END.

  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
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
     lv-column-lab = "Job #".

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
    ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  /*RUN dispatch ("open-query").*/
  RUN query-proc.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  IF AVAIL oe-relh THEN FIND CURRENT oe-relh NO-LOCK NO-ERROR.

  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.
  IF AVAIL oe-relh THEN DO:


  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(oe-relh.rec_key,{methods/headers/oe-relh.i})"}
  {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
     "(CAN-FIND(FIRST notes WHERE notes.rec_key = oe-relh.rec_key))"}
  {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
     "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = oe-relh.rec_key))"}

       RUN dept-pan-image-proc.
       RUN spec-book-image-proc .

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DEFINE VARIABLE cCustno AS CHARACTER NO-UNDO .
  DEFINE BUFFER bf-oe-rell FOR oe-rell .
  DEFINE BUFFER bf-oe-relh FOR oe-relh .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_posted
     fi_cust-no
     fi_i-no
     fi_ord-no
     fi_po-no
     fi_rel-no
     fi_job-no
     fi_job-no2.

    ll-first = NO.

    /*RUN dispatch ("open-query").*/

   RUN query-proc.

   GET FIRST Browser-Table .

   IF NOT AVAIL oe-relh THEN DO:
       IF fi_cust-no <> "" THEN DO:
           cCustno = fi_cust-no  .
       END.
       ELSE DO:
           FIND FIRST  bf-oe-rell NO-LOCK  
               WHERE bf-oe-rell.company   EQ cocode  
                 AND bf-oe-rell.i-no      BEGINS fi_i-no      
                 AND (bf-oe-rell.ord-no   EQ fi_ord-no OR fi_ord-no EQ 0) 
                 AND bf-oe-rell.po-no     BEGINS fi_po-no    
                 AND bf-oe-rell.job-no    BEGINS fi_job-no   
                 AND (bf-oe-rell.job-no2  EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")
               NO-ERROR.
           IF AVAIL bf-oe-rell THEN
               FIND FIRST  bf-oe-relh USE-INDEX r-no NO-LOCK        
                     WHERE bf-oe-relh.company   EQ cocode
                       AND bf-oe-relh.r-no      EQ oe-relh.r-no 
                    NO-ERROR .

           IF AVAIL bf-oe-relh THEN
                 cCustno = bf-oe-relh.cust-no .
             ELSE cCustno = "".
       END.

       FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = cCustno NO-LOCK NO-ERROR.
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
         ELSE
         MESSAGE "No Order Release Found, please update your Search Criteria."
                VIEW-AS ALERT-BOX ERROR.

   END. /* not avail oe-relh */

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
    RUN set-defaults.    
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
    RUN set-defaults.    
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
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-cust2.w (INPUT g_company, INPUT FOCUS:SCREEN-VALUE,"", OUTPUT char-val).
          IF char-val <> "" THEN
          DO:
            APPLY 'entry' TO fi_cust-no IN FRAME F-MAIN.
            FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
          END.
          /* return no-apply. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  /* This does not allow for spaces, so delay it until leave of */
  /* {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE). */
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON VALUE-CHANGED OF fi_po-no IN FRAME F-Main
DO:
  /*{&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).*/
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
    IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
SESSION:DATA-ENTRY-RETURN = YES.

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OT1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""OT1""}


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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-rel-no AS INT NO-UNDO.
DEF VAR v-rel-cust AS CHAR NO-UNDO.
FIND CURRENT oe-relh NO-LOCK NO-ERROR .

IF AVAIL oe-relh THEN ASSIGN
    v-rel-no = oe-relh.release#
    v-rel-cust = oe-relh.cust-no .
RUN oeinq/rel-expi.w (INPUT v-rel-no,
                        INPUT v-rel-cust,
                        INPUT "").

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
  DEF BUFFER b-oe-relh FOR oe-relh.

  DEF VAR li AS INT NO-UNDO.
  DEF VAR lj AS INT NO-UNDO.
  DEF VAR lv-rel-no LIKE oe-relh.release# NO-UNDO.

  RUN set-defaults.

  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "REBROWSE"
                      no-lock no-error.
  if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign sys-ctrl.company = cocode
            sys-ctrl.name    = "REBROWSE"
            sys-ctrl.descrip = "# of Records to be displayed in oe browser"
            sys-ctrl.log-fld = YES
            sys-ctrl.char-fld = "RE"
            sys-ctrl.int-fld = 30.
  end.

  {&for-each1blank} USE-INDEX delpost NO-LOCK BREAK BY oe-relh.release# DESC:
    IF FIRST-OF(oe-relh.release#) THEN li = li + 1.
    lv-rel-no = oe-relh.release#.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query                     ~
      OPEN QUERY {&browse-name}                 ~
        {&for-each1blank}                       ~
              AND oe-relh.release# GE lv-rel-no ~
            USE-INDEX release# NO-LOCK,         ~
            {&for-each2blank}                   ~
            OUTER-JOIN

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

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
  ASSIGN
   oe-relh.release#:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-rell.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-rell.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-relh.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-rell.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-relh.rel-date:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-rell.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-rell.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-relh.printed:READ-ONLY IN BROWSE {&browse-name} = YES
   oe-relh.ship-id:READ-ONLY IN BROWSE {&browse-name} = YES.

  IF AVAIL oe-relh THEN
  DO:
    DEF VAR char-hdl AS cha NO-UNDO.
    DEF VAR phandle AS HANDLE NO-UNDO.
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(oe-relh.rec_key,{methods/headers/oe-relh.i})"}
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = oe-relh.rec_key))"}
    {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
       "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = oe-relh.rec_key))"}
  END.

    APPLY 'ENTRY':U TO fi_rel-no IN FRAME {&FRAME-NAME}.   /*Task# 02121406*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-oe-rell FOR oe-rell.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF ll-first THEN RUN first-query.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-all.
  ELSE DO:
    {oe/j-relinq.i}
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

   GET LAST {&browse-name}.
   IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-rel-no = oe-relh.release#.

    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-first-show-rel-no = oe-relh.release#.
  END.
   APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
  ASSIGN
     lv-show-prev = NO
     lv-show-next = NO.
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
  APPLY 'ENTRY':U TO fi_rel-no IN FRAME {&FRAME-NAME}.    /*Task# 02121406*/

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

  IF ROWID(oe-rell) EQ lv-last-rowid THEN
    op-nav-type = "L".

  IF ROWID(oe-rell) EQ lv-frst-rowid THEN
    op-nav-type = IF op-nav-type EQ "L" THEN "B" ELSE "F".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-proc B-table-Win 
PROCEDURE query-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-oe-rell FOR oe-rell.
 browse {&browse-name}:sensitive = false.
  IF ll-first THEN RUN first-query.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-all.
  ELSE DO:
    {oe/j-relinq.i}
  END.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

   GET LAST {&browse-name}.
   IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-rel-no = oe-relh.release#.

    GET FIRST {&browse-name}.
    IF AVAIL {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-first-show-rel-no = oe-relh.release#.
  END.
  browse {&browse-name}:sensitive = true.
  APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
  ASSIGN
     lv-show-prev = NO
     lv-show-next = NO.
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
  RUN set-defaults.
  ASSIGN FRAME {&FRAME-NAME}
     fi_cust-no
     fi_i-no
     fi_ord-no
     fi_po-no
     fi_rel-no
     fi_job-no
     fi_job-no2.


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
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEF BUFFER b-oe-rell FOR oe-rell.
  DEF BUFFER b-oe-relh  FOR oe-relh.


  FIND b-oe-rell WHERE ROWID(b-oe-rell) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAIL b-oe-rell THEN DO:
    FIND FIRST b-oe-relh OF b-oe-rell NO-LOCK.
    ip-rowid = ROWID(b-oe-relh).
  END.

  DO WITH FRAME {&FRAME-NAME}:
/*     RUN dispatch ("open-query"). */
    RUN query-proc.
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    RUN dispatch ("row-changed").
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query2 B-table-Win 
PROCEDURE reopen-query2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lh-column AS HANDLE NO-UNDO.
  DEF VAR lv-column-nam AS CHAR NO-UNDO.
  DEF VAR lv-column-lab AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
     lh-column = {&BROWSE-NAME}:CURRENT-COLUMN.
     IF lh-column:LABEL-BGCOLOR NE 14 THEN RETURN NO-APPLY.

     ASSIGN
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
  {src/adm/template/snd-list.i "oe-relh"}
  {src/adm/template/snd-list.i "oe-rell"}
  {src/adm/template/snd-list.i "itemfg"}

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
     fi_cust-no:SCREEN-VALUE = ""
     fi_i-no:SCREEN-VALUE    = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_po-no:SCREEN-VALUE   = ""
     fi_job-no:SCREEN-VALUE  = ""
     fi_rel-no:SCREEN-VALUE  = ""
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

 /* {methods/setfocus.i {&BROWSE-NAME}}                     

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&BROWSE-NAME}.
  END. */

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
  DEF VAR lv-rel-no LIKE oe-relh.release# NO-UNDO.

  RUN set-defaults.

  find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "REBROWSE"
                      no-lock no-error.
  if not avail sys-ctrl then do transaction:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
             sys-ctrl.name    = "REBROWSE"
             sys-ctrl.descrip = "# of Records to be displayed in oe browser"
             sys-ctrl.log-fld = YES
             sys-ctrl.char-fld = "RE"
             sys-ctrl.int-fld = 30.
  end.

IF lv-show-prev THEN DO:

  {&for-each1blank} AND oe-relh.release# <= lv-last-show-rel-no  
       USE-INDEX delpost NO-LOCK BREAK BY oe-relh.release# DESC:
    IF FIRST-OF(oe-relh.release#) THEN li = li + 1.
    lv-rel-no = oe-relh.release#.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query                     ~
      OPEN QUERY {&browse-name}                 ~
        {&for-each1blank}                       ~
              AND oe-relh.release# GE lv-rel-no ~
              AND oe-relh.release# LE lv-last-show-rel-no ~
            NO-LOCK,                            ~
            {&for-each2blank}                   ~
            OUTER-JOIN

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.

END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:

  li = 0.

  {&for-each1blank} AND oe-relh.release# >= lv-first-show-rel-no  
       USE-INDEX delpost NO-LOCK BREAK BY oe-relh.release# :
    IF FIRST-OF(oe-relh.release#) THEN li = li + 1.
    lv-rel-no = oe-relh.release#.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
  END.

  &SCOPED-DEFINE open-query                     ~
      OPEN QUERY {&browse-name}                 ~
        {&for-each1blank}                       ~
              AND oe-relh.release# LE lv-rel-no ~
              AND oe-relh.release# GE lv-first-show-rel-no ~
            NO-LOCK,                            ~
            {&for-each2blank}                   ~
            OUTER-JOIN

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dept-pan-image-proc B-table-Win 
PROCEDURE dept-pan-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = oe-relh.rec_key
       NO-LOCK NO-ERROR.

   IF AVAIL notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN do:
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spec-book-image-proc B-table-Win 
PROCEDURE spec-book-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-spec AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   DEF BUFFER bf2-itemfg FOR itemfg.

   FIND FIRST bf2-itemfg WHERE
        bf2-itemfg.company = oe-rell.company AND
        bf2-itemfg.i-no EQ oe-rell.i-no
        NO-LOCK NO-ERROR.

   IF AVAIL bf2-itemfg THEN
      v-spec = CAN-FIND(FIRST notes WHERE
               notes.rec_key = bf2-itemfg.rec_key AND
               notes.note_type = "S").

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-shipto-zone B-table-Win 
FUNCTION get-shipto-zone RETURNS CHARACTER
  ( /* parameter-definitions */ ) :

  FOR EACH shipto NO-LOCK
      WHERE shipto.company EQ cocode
       AND shipto.cust-no EQ oe-relh.cust-no
      AND shipto.ship-id EQ oe-relh.ship-id :

       IF AVAILABLE shipto THEN
       DO:
           RETURN shipto.dest-code.
       END.
  END.

  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fQty-oh B-table-Win 
FUNCTION fQty-oh RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR iQtyOh AS INTEGER NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      FIND itemfg NO-LOCK WHERE itemfg.company = oe-rell.company
          AND itemfg.i-no = oe-rell.i-no  NO-ERROR.
      IF AVAIL itemfg  THEN
          ASSIGN iQtyOh = itemfg.q-onh.
  END.

  RETURN iQtyOh.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
