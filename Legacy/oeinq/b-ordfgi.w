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

  File:  oeinq\b-ordfgi.w

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

DEF VAR lv-sort-by AS CHAR INIT "trans-date" NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "TR Date" NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR li-pallets AS INT NO-UNDO.
DEF VAR li-qty-pal AS INT NO-UNDO.
DEF VAR trans-time AS CHAR NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEF VAR v-upd-perms AS LOG NO-UNDO.
DEF VAR v-access-close AS LOG NO-UNDO.
DEF VAR v-access-list AS CHAR NO-UNDO.
DEF VAR bol-no AS INT NO-UNDO.
DEF VAR bol-ship AS CHAR NO-UNDO.
DEF VAR vend-name AS CHAR NO-UNDO.
DEF VAR vend-no AS CHAR NO-UNDO.
DEFINE VARIABLE iBinQty AS INTEGER NO-UNDO.
DEFINE VARIABLE iBinQtyBef AS INTEGER NO-UNDO.
DEFINE VARIABLE iBinQtyAft AS INTEGER NO-UNDO.
DEFINE VARIABLE hPgmReason AS HANDLE NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "FGHstUpd",
     INPUT "ALL", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT v-upd-perms, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */

{sys/inc/oeinq.i}
ll-sort-asc = NOT oeinq.

&SCOPED-DEFINE key-phrase fg-rcpth.company EQ cocode

&SCOPED-DEFINE for-each1                                        ~
    FOR EACH fg-rcpth OF itemfg                                 ~
        WHERE {&key-phrase}                                     ~
          AND fg-rcpth.trans-date GE fi_date                    ~
          AND fg-rcpth.i-no       BEGINS fi_i-no                ~
          AND fg-rcpth.rita-code  BEGINS fi_rita-code           ~
          AND (fg-rcpth.po-no     EQ TRIM(STRING(fi_po-no,">>>>>>>>")) OR fi_po-no EQ 0) ~
          AND fg-rcpth.job-no     BEGINS fi_job-no              ~
          AND (fg-rcpth.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                           ~
    EACH fg-rdtlh NO-LOCK                          ~
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no      ~
      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code ~
      AND fg-rdtlh.tag MATCHES fi_tag#

&SCOPED-DEFINE sortby-log                                                                                   ~
    IF lv-sort-by EQ "i-no"        THEN fg-rcpth.i-no                                                  ELSE ~
    IF lv-sort-by EQ "trans-date"  THEN STRING(INT(fg-rcpth.trans-date),"9999999999") + STRING(fg-rdtlh.trans-time) ELSE ~
    IF lv-sort-by EQ "post-date"  THEN STRING(INT(fg-rcpth.post-date),"9999999999")                    ELSE ~
    IF lv-sort-by EQ "rita-code"   THEN fg-rcpth.rita-code                                             ELSE ~
    IF lv-sort-by EQ "cust-no"     THEN fg-rdtlh.cust-no                                               ELSE ~
    IF lv-sort-by EQ "loc"         THEN fg-rdtlh.loc                                                   ELSE ~
    IF lv-sort-by EQ "bol-no"      THEN string(display-bol())                                          ELSE ~
    IF lv-sort-by EQ "loc-bin"     THEN fg-rdtlh.loc-bin                                               ELSE ~
    IF lv-sort-by EQ "tag"         THEN fg-rdtlh.tag                                                   ELSE ~
    IF lv-sort-by EQ "qty"         THEN STRING(9999999999.99 + fg-rdtlh.qty,"-9999999999.99")          ELSE ~
    IF lv-sort-by EQ "qty-case"    THEN STRING(fg-rdtlh.qty-case,"-9999999999.99999")                  ELSE ~
    IF lv-sort-by EQ "cases"       THEN STRING(fg-rdtlh.cases,"-9999999999.99999")                     ELSE ~
    IF lv-sort-by EQ "cost"        THEN STRING(fg-rdtlh.cost,"-9999999999.99999")                      ELSE ~
    IF lv-sort-by EQ "partial"     THEN STRING(fg-rdtlh.partial,"-9999999999.99999")                   ELSE ~
    IF lv-sort-by EQ "stacks-unit" THEN STRING(fg-rdtlh.stacks-unit,"-9999999999.99999")               ELSE ~
    IF lv-sort-by EQ "job-no"      THEN STRING(fg-rcpth.job-no,"x(6)") + STRING(fg-rcpth.job-no2,"99") ELSE ~
    IF lv-sort-by EQ "po-no"       THEN STRING(INT(fg-rcpth.po-no),"9999999999")                       ELSE ~
    IF lv-sort-by EQ "po-line"     THEN STRING(INT(fg-rcpth.po-line),"999")                            ELSE ~
    IF lv-sort-by EQ "stack-code"  THEN fg-rdtlh.stack-code                                            ELSE ~
    IF lv-sort-by EQ "tot-wt"      THEN string(fg-rdtlh.tot-wt)                                        ELSE ~
                                        STRING(INT(fg-rcpth.trans-date),"9999999999") + STRING(fg-rdtlh.trans-time) + fg-rdtlh.rec_key + STRING(fg-rcpth.r-no,"9999999999")

&SCOPED-DEFINE sortby BY fg-rcpth.i-no BY fg-rcpth.job-no BY fg-rcpth.job-no2

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC       ~
    {&sortby}

DO TRANSACTION:
   {sys\inc\fgsecur.i}
END.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES itemfg
&Scoped-define FIRST-EXTERNAL-TABLE itemfg


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR itemfg.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fg-rcpth fg-rdtlh

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table fg-rcpth.i-no fg-rcpth.po-no ~
fg-rcpth.job-no fg-rcpth.job-no2 display-bol() @ bol-no fg-rcpth.trans-date ~
STRING(fg-rdtlh.trans-time,'HH:MM') @ trans-time fg-rcpth.rita-code ~
fg-rdtlh.cust-no fg-rdtlh.loc fg-rdtlh.loc-bin fg-rdtlh.qty fg-rdtlh.tag ~
fg-rdtlh.cost fg-rdtlh.cases fg-rdtlh.qty-case ~
get-pallet-info (output li-qty-pal) @ li-pallets fg-rdtlh.stacks-unit ~
fg-rdtlh.partial li-qty-pal @ li-qty-pal fg-rdtlh.stack-code ~
fg-rdtlh.tot-wt fg-rdtlh.user-id fg-rcpth.b-no fg-rcpth.pur-uom ~
display-ship() @ bol-ship fg-rcpth.post-date get-vend-no () @ vend-no ~
get-vend-info () @ vend-name get-fg-qty (1) @ iBinQtyBef ~
get-fg-qty (2) @ iBinQty fg-rdtlh.reject-code[1] fg-rdtlh.enteredBy ~
fg-rdtlh.enteredDT fg-rcpth.po-line
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table fg-rcpth.i-no ~
fg-rcpth.po-no fg-rcpth.job-no fg-rcpth.job-no2 fg-rcpth.trans-date ~
fg-rcpth.rita-code fg-rdtlh.cust-no fg-rdtlh.loc fg-rdtlh.loc-bin ~
fg-rdtlh.qty fg-rdtlh.tag fg-rdtlh.cost fg-rdtlh.cases fg-rdtlh.qty-case ~
fg-rdtlh.stacks-unit fg-rdtlh.partial fg-rdtlh.stack-code fg-rdtlh.tot-wt ~
fg-rcpth.pur-uom fg-rcpth.post-date fg-rdtlh.reject-code[1] fg-rcpth.po-line 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table fg-rcpth fg-rdtlh
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rcpth
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table fg-rdtlh
&Scoped-define QUERY-STRING-Browser-Table FOR EACH fg-rcpth OF itemfg  WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no ~
AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH fg-rcpth OF itemfg  WHERE ~{&KEY-PHRASE} NO-LOCK, ~
      EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no ~
AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table fg-rcpth fg-rdtlh
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table fg-rcpth
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table fg-rdtlh


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table btCompress BTN-updt-trans-time ~
fi_job-no fi_job-no2 fi_rita-code fi_date btn_del fi_tag# btnPreFix ~
fi_po-no btn_copy btn_go btn_show RECT-1 
&Scoped-Define DISPLAYED-OBJECTS fi_i-no fi_job-no fi_job-no2 fi_rita-code ~
fi_date fi_tag# fi_po-no fi_name fi_q-onh fi_q-avail fi_sort-by FI_moveCol 

/* Custom List Definitions                                              */
/* goFields,List-2,List-3,List-4,List-5,List-6                          */
&Scoped-define goFields fi_i-no fi_job-no fi_job-no2 fi_rita-code fi_date ~
fi_tag# fi_po-no 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-bol B-table-Win 
FUNCTION display-bol RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-ship B-table-Win 
FUNCTION display-ship RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-qty-pal AS INT) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vend-info B-table-Win 
FUNCTION get-vend-info RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-vend-no B-table-Win 
FUNCTION get-vend-no RETURNS CHAR
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fg-qty B-table-Win 
FUNCTION get-fg-qty RETURNS INT
  ( /* parameter-definitions */ INPUT ip-int AS INT  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCompress 
     LABEL "Compress Trns" 
     SIZE 17 BY .95.

DEFINE BUTTON BTN-updt-trans-time 
     LABEL "Update Trans Time" 
     SIZE 21 BY 1.14.

DEFINE BUTTON btnPreFix 
     LABEL "<== Prefix FG Item#" 
     SIZE 21 BY 1.

DEFINE BUTTON btn_copy 
     LABEL "Copy" 
     SIZE 15 BY 1.

DEFINE BUTTON btn_del 
     LABEL "Delete" 
     SIZE 15 BY 1.

DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1.

DEFINE VARIABLE fi_date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     LABEL "FG Item#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "Vendor PO#" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-avail AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "QtyAvail" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_q-onh AS INTEGER FORMAT "->>>,>>>,>>>" INITIAL 0 
     LABEL "QtyOnHand" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_rita-code AS CHARACTER FORMAT "X":U 
     LABEL "Trans Code" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sorted By" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE fi_tag# AS CHARACTER FORMAT "X(20)":U 
     LABEL "Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 156 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      fg-rcpth
    FIELDS(fg-rcpth.i-no
      fg-rcpth.po-no
      fg-rcpth.po-line
      fg-rcpth.job-no
      fg-rcpth.job-no2
      fg-rcpth.trans-date
      fg-rcpth.rita-code
      fg-rcpth.b-no
      fg-rcpth.pur-uom
      fg-rcpth.post-date
      vend-no), 
      fg-rdtlh SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      fg-rcpth.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      fg-rcpth.po-no COLUMN-LABEL "Vendor PO#" FORMAT "x(9)":U
            LABEL-BGCOLOR 14
      fg-rcpth.po-line COLUMN-LABEL "PO Ln" FORMAT ">99":U
            LABEL-BGCOLOR 14
      fg-rcpth.job-no FORMAT "x(6)":U WIDTH 8 LABEL-BGCOLOR 14
      fg-rcpth.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      display-bol() @ bol-no COLUMN-LABEL "BOL #" FORMAT ">>>>>>>9":U
      fg-rcpth.trans-date COLUMN-LABEL "TR Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      STRING(fg-rdtlh.trans-time,'HH:MM') @ trans-time COLUMN-LABEL "Tr Time"
            WIDTH 10.2
      fg-rcpth.rita-code COLUMN-LABEL "TR!Code" FORMAT "x(1)":U
            WIDTH 6.2 LABEL-BGCOLOR 14
      fg-rdtlh.cust-no COLUMN-LABEL "Cust#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-rdtlh.loc COLUMN-LABEL "Ware-!house" FORMAT "x(5)":U LABEL-BGCOLOR 14
      fg-rdtlh.loc-bin COLUMN-LABEL "Bin" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      fg-rdtlh.qty COLUMN-LABEL "Quantity" FORMAT "->>>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rdtlh.tag COLUMN-LABEL "Tag#" FORMAT "x(25)":U WIDTH 30
            LABEL-BGCOLOR 14
      fg-rdtlh.cost COLUMN-LABEL "Cost" FORMAT "->>>,>>9.99<<":U
            LABEL-BGCOLOR 14
      fg-rdtlh.cases COLUMN-LABEL "Units" FORMAT "->,>>>,>>9":U
            LABEL-BGCOLOR 14
      fg-rdtlh.qty-case COLUMN-LABEL "Qty/Unit" FORMAT "->>>,>>9":U
            LABEL-BGCOLOR 14
      get-pallet-info (output li-qty-pal) @ li-pallets COLUMN-LABEL "Pallets" FORMAT "->>>>>>":U
            WIDTH 9.4 LABEL-BGCOLOR 14
      fg-rdtlh.stacks-unit COLUMN-LABEL "Units/Pallet" FORMAT ">,>>9":U
            LABEL-BGCOLOR 14
      fg-rdtlh.partial COLUMN-LABEL "Partial" FORMAT "->>>,>>9":U
            LABEL-BGCOLOR 14
      li-qty-pal @ li-qty-pal COLUMN-LABEL "Qty/Pallet" FORMAT "->>>>>>":U
            LABEL-BGCOLOR 14
      fg-rdtlh.stack-code COLUMN-LABEL "FG Lot#" FORMAT "X(20)":U
            LABEL-BGCOLOR 14
      fg-rdtlh.tot-wt COLUMN-LABEL "Lbs / 100" FORMAT ">>,>>9.99":U
            WIDTH 16
      fg-rdtlh.user-id COLUMN-LABEL "Posted By" FORMAT "x(8)":U
            WIDTH 13
      fg-rcpth.b-no FORMAT ">>>>>9":U
      fg-rcpth.pur-uom COLUMN-LABEL "UOM for Cost" FORMAT "x(3)":U
      display-ship() @ bol-ship COLUMN-LABEL "BOL Cust" FORMAT "x(14)":U
            WIDTH 14
      fg-rcpth.post-date COLUMN-LABEL "Posted" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      get-vend-no () @ vend-no COLUMN-LABEL "Vendor" FORMAT "x(10)":U
            WIDTH 9.4 LABEL-BGCOLOR 14
      get-vend-info () @ vend-name COLUMN-LABEL "Name" FORMAT "x(20)":U
            WIDTH 19.4 LABEL-BGCOLOR 14
      get-fg-qty (1) @ iBinQtyBef COLUMN-LABEL "Before Qty" FORMAT "->>>>>>9":U
            WIDTH 9.4 LABEL-BGCOLOR 14
      get-fg-qty (2) @ iBinQty COLUMN-LABEL "Bin Change" FORMAT "->>>>>>9":U
            WIDTH 9.4 LABEL-BGCOLOR 14
      fg-rdtlh.reject-code[1] COLUMN-LABEL "Adjustment Reason:" FORMAT "x(2)":U
            VIEW-AS COMBO-BOX SORT INNER-LINES 5
                      LIST-ITEM-PAIRS "Item 1"," Item 1"
                      DROP-DOWN-LIST 
      fg-rdtlh.enteredBy COLUMN-LABEL "Scanned By" FORMAT "x(12)":U
      fg-rdtlh.enteredDT COLUMN-LABEL "Scan Date/Time" FORMAT "99/99/9999 HH:MM:SS.SSS":U
  ENABLE
      fg-rcpth.i-no
      fg-rcpth.po-no
      fg-rcpth.po-line
      fg-rcpth.job-no
      fg-rcpth.job-no2
      fg-rcpth.trans-date
      fg-rcpth.rita-code
      fg-rdtlh.cust-no
      fg-rdtlh.loc
      fg-rdtlh.loc-bin
      fg-rdtlh.qty
      fg-rdtlh.tag
      fg-rdtlh.cost
      fg-rdtlh.cases
      fg-rdtlh.qty-case
      fg-rdtlh.stacks-unit
      fg-rdtlh.partial
      fg-rdtlh.stack-code
      fg-rdtlh.tot-wt
      fg-rcpth.pur-uom
      fg-rcpth.post-date
      fg-rdtlh.reject-code[1]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 156 BY 15
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btCompress AT ROW 4.81 COL 139.4 WIDGET-ID 12
     BTN-updt-trans-time AT ROW 3.57 COL 135.4 WIDGET-ID 8
     fi_i-no AT ROW 1.24 COL 12 COLON-ALIGNED
     fi_job-no AT ROW 1.24 COL 62 COLON-ALIGNED
     fi_job-no2 AT ROW 1.24 COL 75 COLON-ALIGNED
     fi_rita-code AT ROW 1.24 COL 94 COLON-ALIGNED
     fi_date AT ROW 1.24 COL 116 COLON-ALIGNED
     btn_del AT ROW 1.24 COL 141.4 WIDGET-ID 4
     fi_tag# AT ROW 2.43 COL 12 COLON-ALIGNED
     btnPreFix AT ROW 2.43 COL 54
     fi_po-no AT ROW 2.43 COL 116 COLON-ALIGNED WIDGET-ID 2
     btn_copy AT ROW 2.43 COL 141.4 WIDGET-ID 6
     fi_name AT ROW 3.62 COL 12 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches."
     fi_q-onh AT ROW 3.62 COL 83 COLON-ALIGNED
     fi_q-avail AT ROW 3.62 COL 115 COLON-ALIGNED
     btn_go AT ROW 4.81 COL 3
     btn_show AT ROW 4.81 COL 18
     fi_sort-by AT ROW 4.81 COL 39.2 COLON-ALIGNED
     FI_moveCol AT ROW 4.81 COL 124 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     "Click on Yellow Field, Sorts From 1st to Last" VIEW-AS TEXT
          SIZE 43 BY 1 AT ROW 4.81 COL 81.2
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
   External Tables: ASI.itemfg
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
         HEIGHT             = 20.48
         WIDTH              = 156.
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
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       fg-rcpth.b-no:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR FILL-IN fi_date IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_i-no IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN fi_job-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_job-no2 IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_po-no IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_q-avail IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_q-onh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_rita-code IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_tag# IN FRAME F-Main
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.fg-rcpth OF ASI.itemfg ,ASI.fg-rdtlh WHERE ASI.fg-rcpth ..."
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,, FIRST OUTER"
     _JoinCode[2]      = "fg-rdtlh.r-no EQ fg-rcpth.r-no
AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code"
     _FldNameList[1]   > ASI.fg-rcpth.i-no
"fg-rcpth.i-no" "FG Item#" "x(15)" "character" ? ? ? 14 ? ? yes "" no no ? yes no no "U" "" "" "FILL-IN" "?" ? ? 5 no 0 no no
     _FldNameList[2]   > ASI.fg-rcpth.po-no
"fg-rcpth.po-no" "Vendor PO#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.fg-rcpth.job-no
"fg-rcpth.job-no" ? ? "character" ? ? ? 14 ? ? yes ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.fg-rcpth.job-no2
"fg-rcpth.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"display-bol() @ bol-no" "BOL #" ">>>>>>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.fg-rcpth.trans-date
"fg-rcpth.trans-date" "TR Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"STRING(fg-rdtlh.trans-time,'HH:MM') @ trans-time" "Tr Time" ? ? ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.fg-rcpth.rita-code
"fg-rcpth.rita-code" "TR!Code" ? "character" ? ? ? 14 ? ? yes ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.fg-rdtlh.cust-no
"fg-rdtlh.cust-no" "Cust#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.fg-rdtlh.loc
"fg-rdtlh.loc" "Ware-!house" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.fg-rdtlh.loc-bin
"fg-rdtlh.loc-bin" "Bin" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.fg-rdtlh.qty
"fg-rdtlh.qty" "Quantity" "->>>>,>>9" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.fg-rdtlh.tag
"fg-rdtlh.tag" "Tag#" "x(25)" "character" ? ? ? 14 ? ? yes ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.fg-rdtlh.cost
"fg-rdtlh.cost" "Cost" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.fg-rdtlh.cases
"fg-rdtlh.cases" "Units" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.fg-rdtlh.qty-case
"fg-rdtlh.qty-case" "Qty/Unit" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > "_<CALC>"
"get-pallet-info (output li-qty-pal) @ li-pallets" "Pallets" "->>>>>>" ? ? ? ? 14 ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.fg-rdtlh.stacks-unit
"fg-rdtlh.stacks-unit" "Units/Pallet" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.fg-rdtlh.partial
"fg-rdtlh.partial" "Partial" "->>>,>>9" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"li-qty-pal @ li-qty-pal" "Qty/Pallet" "->>>>>>" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.fg-rdtlh.stack-code
"fg-rdtlh.stack-code" "FG Lot#" "X(20)" "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.fg-rdtlh.tot-wt
"fg-rdtlh.tot-wt" "Lbs / 100" ? "decimal" ? ? ? ? ? ? yes ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.fg-rdtlh.user-id
"fg-rdtlh.user-id" "Posted By" ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.fg-rcpth.b-no
"fg-rcpth.b-no" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.fg-rcpth.pur-uom
"fg-rcpth.pur-uom" "UOM for Cost" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"display-ship() @ bol-ship" "BOL Cust" "x(14)" ? ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ASI.fg-rcpth.post-date
"fg-rcpth.post-date" "Posted" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"get-vend-no () @ vend-no" "Vendor" "x(10)" ? ? ? ? 14 ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > "_<CALC>"
"get-vend-info () @ vend-name" "Name" "x(20)" ? ? ? ? 14 ? ? no ? no no "19.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"get-fg-qty (1) @ iBinQtyBef" "Before Qty" "->>>>>>9" ? ? ? ? 14 ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"get-fg-qty (2) @ iBinQty" "Bin Change" "->>>>>>9" ? ? ? ? 14 ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > ASI.fg-rdtlh.reject-code[1]
"fg-rdtlh.reject-code[1]" "Adjustment Reason:" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "DROP-DOWN-LIST" "," ? "Item 1, Item 1" 5 yes 0 no no
     _FldNameList[33]   > ASI.fg-rdtlh.enteredBy
"fg-rdtlh.enteredBy" "Scanned By" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > ASI.fg-rdtlh.enteredDT
"fg-rdtlh.enteredDT" "Scan Date/Time" ? "datetime" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[35]   > ASI.fg-rcpth.po-line
"fg-rcpth.po-line" "Po Ln" ">99" "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON MOUSE-SELECT-DBLCLICK OF Browser-Table IN FRAME F-Main
DO:
  IF v-upd-perms THEN DO:
    RUN set-read-only (NO).

    APPLY "entry" TO fg-rcpth.i-no IN BROWSE {&browse-name}.
  END.
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

  IF lv-column-nam BEGINS "li-" THEN DO:
    APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
    RETURN NO-APPLY.
  END.

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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  IF AVAIL fg-rcpth THEN RUN display-itemfg (ROWID(fg-rcpth)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rcpth.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-i-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.i-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.i-no IN BROWSE Browser-Table /* FG Item# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.po-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.po-no IN BROWSE Browser-Table /* Vendor PO# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rcpth.job-no IN BROWSE Browser-Table /* Job# */
DO:
  /*IF fg-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name} NE "S" THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rcpth.job-no IN BROWSE Browser-Table /* Job# */
DO:
  DEF VAR lv-job-no AS CHAR NO-UNDO.


  ASSIGN
   lv-job-no = TRIM({&self-name}:SCREEN-VALUE IN BROWSE {&browse-name})
   lv-job-no = FILL(" ",6 - LENGTH(lv-job-no)) + lv-job-no
   {&self-name}:SCREEN-VALUE IN BROWSE {&browse-name} = lv-job-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.job-no IN BROWSE Browser-Table /* Job# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF fg-rcpth.job-no2 IN BROWSE Browser-Table
DO:
  /*IF fg-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name} NE "S" THEN DO:
    APPLY "tab" TO {&self-name}.
    RETURN NO-APPLY.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.job-no2 Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.job-no2 IN BROWSE Browser-Table
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.trans-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.trans-date Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.trans-date IN BROWSE Browser-Table /* TR Date */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fg-rcpth.post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.post-date Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.post-date IN BROWSE Browser-Table /* TR Date */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.rita-code Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.rita-code IN BROWSE Browser-Table /* TR!Code */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.cust-no IN BROWSE Browser-Table /* Cust# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cust-no Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.cust-no IN BROWSE Browser-Table /* Cust# */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.loc IN BROWSE Browser-Table /* Ware-!house */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.loc IN BROWSE Browser-Table /* Ware-!house */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.loc-bin Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.loc-bin IN BROWSE Browser-Table /* Bin */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.qty IN BROWSE Browser-Table /* Quantity */
DO:
/*   fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                        */
/*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) /      */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */
/*                                                                                  */
RUN reCalcUnits.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.qty IN BROWSE Browser-Table /* Quantity */
DO:

/*   fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                        */
/*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) /      */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */
  RUN reCalcUnits.
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.tag IN BROWSE Browser-Table /* Tag# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tag NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-tag-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.tag Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.tag IN BROWSE Browser-Table /* Tag# */
DO:
    RUN valid-tag-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.cost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.cost IN BROWSE Browser-Table /* Cost */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cost Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.cost IN BROWSE Browser-Table /* Cost */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.cases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN reCalcQty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.cases Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.cases IN BROWSE Browser-Table /* Units */
DO:
  RUN reCalcQty.
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.qty-case
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:
/*     fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                      */
/*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) /      */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */
    RUN reCalcQty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.qty-case Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.qty-case IN BROWSE Browser-Table /* Qty/Unit */
DO:

/*   fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                        */
/*       string((INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) -     */
/*              INTEGER(fg-rdtlh.partial:SCREEN-VALUE IN BROWSE {&browe-name})) /   */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */
    RUN reCalcQty.
    RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.stacks-unit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.stacks-unit Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.stacks-unit IN BROWSE Browser-Table /* Units/Pallet */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rdtlh.partial IN BROWSE Browser-Table /* Partial */
DO:
  RUN reCalcQty.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.partial Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.partial IN BROWSE Browser-Table /* Partial */
DO:
    RUN reCalcQty.
    RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rdtlh.tot-wt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rdtlh.tot-wt Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rdtlh.tot-wt IN BROWSE Browser-Table /* Lbs / 100 */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.pur-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF fg-rcpth.pur-uom IN BROWSE Browser-Table /* UOM for Cost */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.pur-uom Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.pur-uom IN BROWSE Browser-Table /* UOM for Cost */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fg-rcpth.post-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fg-rcpth.post-date Browser-Table _BROWSE-COLUMN B-table-Win
ON RETURN OF fg-rcpth.post-date IN BROWSE Browser-Table /* Posted */
DO:
  RUN update-record.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCompress
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCompress B-table-Win
ON CHOOSE OF btCompress IN FRAME F-Main /* Compress Trns */
DO:
  RUN util/consolidatexfer.w.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-updt-trans-time
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-updt-trans-time B-table-Win
ON CHOOSE OF BTN-updt-trans-time IN FRAME F-Main /* Update Trans Time */
DO:
   RUN fg/d-trtime.w(INPUT ROWID(fg-rdtlh),
                     INPUT fg-rdtlh.trans-time). 
   Browser-Table:REFRESH().
   RUN dispatch ("open-query").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPreFix
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPreFix B-table-Win
ON CHOOSE OF btnPreFix IN FRAME F-Main /* <== Prefix FG Item# */
DO:
  fi_tag#:SCREEN-VALUE = itemfg.i-no + FILL(' ',15 - LENGTH(itemfg.i-no)) + '0'.
  APPLY 'ENTRY':U TO fi_tag#.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_copy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_copy B-table-Win
ON CHOOSE OF btn_copy IN FRAME F-Main /* Copy */
DO:
   DEF VAR confirm AS LOG NO-UNDO.
   DEF BUFFER b-fg-rcpth FOR fg-rcpth.
   DEF VAR v-rcpth-no AS INT NO-UNDO.
   DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.
   DEF BUFFER b-reftable FOR reftable.
   DEF VAR op-all AS LOG NO-UNDO .
   DEF VAR op-whbn AS LOG NO-UNDO .

   DEF BUFFER b-fg-rcpth2 FOR fg-rcpth.
   DEF BUFFER b-fg-rdtlh2 FOR fg-rdtlh.

   IF AVAIL fg-rcpth AND v-upd-perms THEN
   DO:
     /* MESSAGE "Do you Wish To Copy This Transaction Record?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm. */

       RUN oeinq/d-cpyopn.w (OUTPUT op-all, OUTPUT op-whbn) .

    IF op-all THEN DO:
         FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
         IF AVAIL fg-rctd AND fg-rctd.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rctd.r-no.
         FIND LAST b-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL b-fg-rcpth AND b-fg-rcpth.r-no GT v-rcpth-no THEN v-rcpth-no = b-fg-rcpth.r-no.

         DO WHILE TRUE:
            v-rcpth-no = v-rcpth-no + 1.
            FIND FIRST b-fg-rcpth WHERE b-fg-rcpth.r-no EQ v-rcpth-no USE-INDEX r-no NO-LOCK NO-ERROR.
            IF AVAIL b-fg-rcpth THEN NEXT.
            FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ v-rcpth-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
            IF AVAIL fg-rctd THEN NEXT.
            LEAVE.
         END.

         CREATE b-fg-rcpth.
         FIND CURRENT fg-rcpth EXCLUSIVE-LOCK NO-ERROR.
         BUFFER-COPY fg-rcpth EXCEPT r-no TO b-fg-rcpth 
             ASSIGN b-fg-rcpth.r-no = v-rcpth-no.
         RELEASE b-fg-rcpth.
         FIND CURRENT fg-rcpth NO-LOCK NO-ERROR.

         IF AVAILABLE fg-rdtlh THEN
         DO:
            FIND CURRENT fg-rdtlh EXCLUSIVE-LOCK NO-ERROR.
            CREATE b-fg-rdtlh.
            BUFFER-COPY fg-rdtlh EXCEPT r-no TO b-fg-rdtlh
                ASSIGN b-fg-rdtlh.r-no = v-rcpth-no.
            RELEASE b-fg-rdtlh.
            FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.
         END.

         IF AVAIL reftable THEN
         DO:
            FIND CURRENT reftable EXCLUSIVE-LOCK NO-ERROR.
            CREATE b-reftable.
            BUFFER-COPY reftable EXCEPT loc TO b-reftable
               ASSIGN b-reftable.loc = STRING(v-rcpth-no,"9999999999").
            RELEASE b-reftable.
            FIND CURRENT reftable NO-LOCK NO-ERROR.
         END.

         RUN local-open-query.

    END. /* if op-all */

    IF op-whbn THEN DO:
        FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.

        FOR EACH b-fg-rcpth2 OF itemfg 
            WHERE b-fg-rcpth2.trans-date GE fi_date 
            AND b-fg-rcpth2.i-no       BEGINS fi_i-no
            AND b-fg-rcpth2.rita-code  BEGINS fi_rita-code
            AND (b-fg-rcpth2.po-no     EQ TRIM(STRING(fi_po-no,">>>>>>>>")) OR fi_po-no EQ 0)
            AND b-fg-rcpth2.job-no     BEGINS fi_job-no           
            AND (b-fg-rcpth2.job-no2   EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ ""),
            EACH b-fg-rdtlh2 EXCLUSIVE-LOCK   
            WHERE b-fg-rdtlh2.r-no      EQ b-fg-rcpth2.r-no     
            AND b-fg-rdtlh2.rita-code EQ b-fg-rcpth2.rita-code 
            AND b-fg-rdtlh2.tag MATCHES fi_tag# :
            ASSIGN
                b-fg-rdtlh2.loc     =  fg-rdtlh.loc
                b-fg-rdtlh2.loc-bin =  fg-rdtlh.loc-bin .

        END.
        RUN local-open-query.

    END. /* op whe */


   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_del B-table-Win
ON CHOOSE OF btn_del IN FRAME F-Main /* Delete */
DO:
   DEF VAR confirm AS LOG NO-UNDO.
   DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
   IF v-upd-perms THEN DO:
     IF AVAIL fg-rcpth THEN
     DO:
        MESSAGE "Do you Wish To Delete This Transaction Record?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE confirm.

        IF confirm THEN
        DO:
           FIND CURRENT fg-rcpth EXCLUSIVE-LOCK NO-ERROR.

           IF AVAILABLE fg-rcpth THEN DO:


              FOR EACH bf-fg-rdtlh EXCLUSIVE-LOCK 
                          WHERE bf-fg-rdtlh.r-no EQ fg-rcpth.r-no:
                 DELETE bf-fg-rdtlh.
              END.

              DELETE fg-rcpth.

          END.

          RUN local-open-query.
        END.
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
    ASSIGN {&goFields}
      fi_tag# = fi_tag# + '*'.
    {fginq/j-fgiinq.i}
    fi_tag# = SUBSTR(fi_tag#,1,LENGTH(fi_tag#) - 1).
    RUN dispatch ("row-changed").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     /* fi_i-no:SCREEN-VALUE      = "" */
     fi_job-no:SCREEN-VALUE    = ""
     fi_job-no2:SCREEN-VALUE   = ""
     fi_rita-code:SCREEN-VALUE = ""
     fi_date:SCREEN-VALUE      = "01/01/0001"
     fi_tag#:SCREEN-VALUE      = ""
     fi_po-no:SCREEN-VALUE     = "".


    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_date B-table-Win
ON LEAVE OF fi_date IN FRAME F-Main /* From Date */
DO:

   IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON LEAVE OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main /* FG Item# */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON LEAVE OF fi_job-no IN FRAME F-Main /* Job# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON VALUE-CHANGED OF fi_job-no IN FRAME F-Main /* Job# */
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
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no B-table-Win
ON LEAVE OF fi_po-no IN FRAME F-Main /* Vendor PO# */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_rita-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON HELP OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   RUN windows/l-tranCd.w (fi_rita-code:SCREEN-VALUE, OUTPUT char-val).
              IF char-val <> "" THEN 
                 fi_rita-code:SCREEN-VALUE = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON LEAVE OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_rita-code B-table-Win
ON VALUE-CHANGED OF fi_rita-code IN FRAME F-Main /* Trans Code */
DO:
  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_tag#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag# B-table-Win
ON LEAVE OF fi_tag# IN FRAME F-Main /* Tag# */
DO:
  IF LASTKEY NE -1 THEN
  APPLY 'CHOOSE':U TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_tag# B-table-Win
ON RETURN OF fi_tag# IN FRAME F-Main /* Tag# */
DO:
  APPLY 'CHOOSE':U TO btn_go.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

ON RETURN OF fg-rdtlh.stack-code IN BROWSE Browser-Table /* Stack Code */
DO:
  RUN update-record.
END.

&SCOPED-DEFINE cellColumnDat b-ordfgi

{methods/browsers/setCellColumns.i}

{sys/inc/f3help.i}  /* asi field contents help */
SESSION:DATA-ENTRY-RETURN = YES.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND
     sys-ctrl.NAME = "FGHistoryDate" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = g_company
         sys-ctrl.name     = "FGHistoryDate"
         sys-ctrl.descrip  = "Default date on Finished Goods History"
         sys-ctrl.log-fld = YES
         sys-ctrl.date-fld = 01/01/2011.
   END.
   IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN
        fi_date = sys-ctrl.date-fld.
   ELSE
      fi_date = DATE("01/01/" + SUBSTRING(STRING(TODAY),7,11)).

IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      fg-rdtlh.cost:VISIBLE IN BROWSE {&browse-name} = NO.
END.

IF v-upd-perms THEN
   BTN-updt-trans-time:HIDDEN = FALSE.
ELSE 
   BTN-updt-trans-time:HIDDEN = TRUE.

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "itemfg"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "itemfg"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-type-list B-table-Win 
PROCEDURE build-type-list :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    &IF DEFINED(FWD-VERSION) EQ 0 &THEN
    DEFINE VARIABLE cComboList AS CHARACTER NO-UNDO .
     
    RUN "fg/ReasonCode.p" PERSISTENT SET hPgmReason.
    RUN pBuildReasonCode IN hPgmReason ("ADJ",OUTPUT cComboList).
    DELETE OBJECT hPgmReason.
  
    DO WITH FRAME {&FRAME-NAME}:
        fg-rdtlh.reject-code[1]:LIST-ITEM-PAIRS IN BROWSE {&browse-name} = cComboList .
    END.
    &ENDIF
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-itemfg B-table-Win 
PROCEDURE display-itemfg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
  DEF BUFFER b-itemfg FOR itemfg.

 /* FIND fg-rcpth WHERE ROWID(fg-rcpth) EQ ip-rowid NO-LOCK NO-ERROR. */

  IF AVAIL fg-rcpth THEN
  DO WITH FRAME {&FRAME-NAME}:
    /*ASSIGN fi_job-no:SCREEN-VALUE = fg-rcpth.job-no
           fi_job-no2:SCREEN-VALUE = STRING(fg-rcpth.job-no2,"99"). */
    FIND FIRST b-itemfg
        WHERE b-itemfg.company EQ cocode
          AND b-itemfg.i-no    EQ fg-rcpth.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL b-itemfg THEN
      ASSIGN
       fi_name:SCREEN-VALUE    = b-itemfg.i-name
       fi_q-onh:SCREEN-VALUE   = STRING(b-itemfg.q-onh)
       fi_q-avail:SCREEN-VALUE = STRING(b-itemfg.q-avail).
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-current-item B-table-Win 
PROCEDURE get-current-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER op-i-no AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    op-i-no = CAPS(fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
              NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR hPgmSecurity AS HANDLE NO-UNDO.
    DEF VAR lResult AS LOG NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    IF AVAIL fg-rcpth THEN 
        RUN display-itemfg (ROWID(fg-rcpth)).

    DO WITH FRAME {&FRAME-NAME}:
        fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
                                  TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".

        RUN "system/PgmMstrSecur.p" PERSISTENT SET hPgmSecurity.
        RUN epCanAccess IN hPgmSecurity ("oeinq/b-ordfgi.w", "", OUTPUT lResult).
        DELETE OBJECT hPgmSecurity.

        IF NOT lResult THEN ASSIGN btn_del:HIDDEN = YES
            btn_del:SENSITIVE = NO
            btn_copy:HIDDEN = YES
            btn_copy:SENSITIVE = NO
            btCompress:HIDDEN = YES
            btCompress:SENSITIVE = NO.
        ELSE ASSIGN 
            btn_del:HIDDEN = NO
            btn_del:SENSITIVE = YES
            btn_copy:HIDDEN = NO
            btn_copy:SENSITIVE = YES
            btCompress:HIDDEN = NO
            btCompress:SENSITIVE = YES.

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
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  RUN setCellColumns.

  /* Code placed here will execute AFTER standard behavior.    */
  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  {methods/winReSizeLocInit.i}

    RUN set-read-only (YES).
/*   ASSIGN                                                         */
/*    fg-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name} = YES        */
/*    fg-rcpth.po-no:READ-ONLY IN BROWSE {&browse-name} = YES       */
/*    fg-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name} = YES      */
/*    fg-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES     */
/*    fg-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name} = YES  */
/*    fg-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name} = YES   */
/*    fg-rdtlh.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES     */
/*    fg-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name} = YES         */
/*    fg-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name} = YES     */
/*    fg-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name} = YES         */
/*    fg-rdtlh.qty:READ-ONLY IN BROWSE {&browse-name} = YES         */
/*    fg-rdtlh.cost:READ-ONLY IN BROWSE {&browse-name} = YES        */
/*    fg-rdtlh.qty-case:READ-ONLY IN BROWSE {&browse-name} = YES    */
/*    fg-rdtlh.cases:READ-ONLY IN BROWSE {&browse-name} = YES       */
/*    fg-rdtlh.partial:READ-ONLY IN BROWSE {&browse-name} = YES     */
/*    fg-rdtlh.stacks-unit:READ-ONLY IN BROWSE {&browse-name} = YES */
/*    fg-rdtlh.stack-code:READ-ONLY IN BROWSE {&browse-name} = YES  */
/*    reftable.code:READ-ONLY IN BROWSE {&browse-name} = YES        */
   FI_moveCol = "Sort".

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
  IF NOT v-upd-perms THEN
      ASSIGN btn_copy:VISIBLE = NO
             btn_del:VISIBLE = NO
             btCompress:VISIBLE = NO.              
  RUN set-focus.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL itemfg THEN fi_i-no = itemfg.i-no.

  fi_tag# = fi_tag# + '*'.
  {fginq/j-fgiinq.i}
  fi_tag# = SUBSTR(fi_tag#,1,LENGTH(fi_tag#) - 1).

  RUN build-type-list .
  
  RUN dispatch ("display-fields").

  RUN dispatch ("row-changed").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
         Browser-Table:COLUMN-MOVABLE = v-col-move
         Browser-Table:COLUMN-RESIZABLE = v-col-move
         v-col-move = NOT v-col-move
         FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
      DISPLAY FI_moveCol.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reCalcQty B-table-Win 
PROCEDURE reCalcQty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE liQty AS INTEGER.
    DEFINE VARIABLE liQtyPerCase AS INTEGER.
    DEFINE VARIABLE liPartial AS INTEGER.
    DEFINE VARIABLE liCases AS INTEGER.

    ASSIGN 
        liCases         =   INTEGER(fg-rdtlh.cases:SCREEN-VALUE IN BROWSE {&browse-name})
        liQtyPerCase    =   INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})
        liPartial       =   INTEGER(fg-rdtlh.partial:SCREEN-VALUE IN BROWSE {&browse-name}).

    liQty = (liQtyPerCase * liCases) + liPartial.
    fg-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(liQty).
/*   fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                        */
/*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) /      */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reCalcUnits B-table-Win 
PROCEDURE reCalcUnits :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE liQty AS INTEGER.
    DEFINE VARIABLE liQtyPerCase AS INTEGER.
    DEFINE VARIABLE liPartial AS INTEGER.
    DEFINE VARIABLE liCases AS INTEGER.

    ASSIGN 
        liQty           =   INTEGER(fg-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name})
        liQtyPerCase    =   INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})
        liPartial       =   INTEGER(fg-rdtlh.partial:SCREEN-VALUE IN BROWSE {&browse-name}).

    liCases = TRUNC((liQty - liPartial )/ liQtyPerCase, 0).
    liPartial = liQty - (liCases * liQtyPerCase).

    ASSIGN 
        fg-rdtlh.cases:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(liCases)
        fg-rdtlh.partial:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(liPartial).

/*   fg-rdtlh.cases:SCREEN-VALUE  IN BROWSE {&browse-name} =                        */
/*       string(INTEGER(fg-rdtlh.qty:SCREEN-VALUE  IN BROWSE {&browse-name}) /      */
/*              INTEGER(fg-rdtlh.qty-case:SCREEN-VALUE  IN BROWSE {&browse-name})). */

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
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
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
  {src/adm/template/snd-list.i "itemfg"}
  {src/adm/template/snd-list.i "fg-rcpth"}
  {src/adm/template/snd-list.i "fg-rdtlh"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-read-only B-table-Win 
PROCEDURE set-read-only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-log AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     fg-rcpth.i-no:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     fg-rcpth.po-no:READ-ONLY IN BROWSE {&browse-name}       = ip-log
     fg-rcpth.po-line:READ-ONLY IN BROWSE {&browse-name}     = ip-log
     fg-rcpth.job-no:READ-ONLY IN BROWSE {&browse-name}      = ip-log
     fg-rcpth.job-no2:READ-ONLY IN BROWSE {&browse-name}     = ip-log
     fg-rcpth.rita-code:READ-ONLY IN BROWSE {&browse-name}   = ip-log
     fg-rcpth.trans-date:READ-ONLY IN BROWSE {&browse-name}  = ip-log  
     fg-rdtlh.cust-no:READ-ONLY IN BROWSE {&browse-name}     = ip-log
     fg-rdtlh.loc:READ-ONLY IN BROWSE {&browse-name}         = ip-log
     fg-rdtlh.loc-bin:READ-ONLY IN BROWSE {&browse-name}     = ip-log
     fg-rdtlh.qty:READ-ONLY IN BROWSE {&browse-name}         = ip-log
     fg-rdtlh.tag:READ-ONLY IN BROWSE {&browse-name}         = ip-log
     fg-rdtlh.cost:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     fg-rdtlh.cases:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     fg-rdtlh.partial:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     fg-rdtlh.qty-case:READ-ONLY IN BROWSE {&browse-name}    = ip-log
     fg-rdtlh.stacks-unit:READ-ONLY IN BROWSE {&browse-name} = ip-log
     fg-rdtlh.stack-code:READ-ONLY IN BROWSE {&browse-name} = ip-log
     fg-rdtlh.tot-wt:READ-ONLY IN BROWSE {&browse-name} = ip-log
     fg-rcpth.pur-uom:READ-ONLY IN BROWSE {&browse-name}        = ip-log
     fg-rcpth.post-date:READ-ONLY IN BROWSE {&browse-name}  = ip-log
     fg-rdtlh.reject-code[1]:READ-ONLY IN BROWSE {&browse-name}  = ip-log
     /*fg-rcpth.vend-no:READ-ONLY IN BROWSE {&browse-name}  = ip-log*/
      .

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-record B-table-Win 
PROCEDURE update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>                          
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-rcpth FOR fg-rcpth.
  DEF BUFFER b-rdtlh FOR fg-rdtlh.

  DISABLE TRIGGERS FOR LOAD OF fg-rcpth.
  DISABLE TRIGGERS FOR LOAD OF fg-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(fg-rcpth).
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(fg-rdtlh).


    ASSIGN
     b-rcpth.i-no        = fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.po-no       = fg-rcpth.po-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.po-line     = INTEGER(fg-rcpth.po-line:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.job-no      = fg-rcpth.job-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.job-no2     = INT(fg-rcpth.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.trans-date  = DATE(fg-rcpth.trans-date:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rcpth.rita-code   = fg-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.update-by   = USERID("NOSWEAT")
     b-rdtlh.cust-no     = fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.loc         = fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.loc-bin     = fg-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.qty         = DEC(fg-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.tot-wt      = DEC(fg-rdtlh.tot-wt:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.tag         = fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.cost        = DEC(fg-rdtlh.cost:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.qty-case    = INT(fg-rdtlh.qty-case:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.partial     = INT(fg-rdtlh.partial:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.cases       = INT(fg-rdtlh.cases:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.stacks-unit = INT(fg-rdtlh.stacks-unit:SCREEN-VALUE IN BROWSE {&browse-name})
     b-rdtlh.stack-code  = fg-rdtlh.stack-code:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rcpth.pur-uom     = fg-rcpth.pur-uom:SCREEN-VALUE IN BROWSE {&browse-name}
     b-rdtlh.rita-code   = b-rcpth.rita-code 
     b-rcpth.post-date  = DATE(fg-rcpth.post-date:SCREEN-VALUE IN BROWSE {&browse-name})
     .

    FIND b-rcpth WHERE ROWID(b-rcpth) EQ ROWID(fg-rcpth) NO-LOCK NO-ERROR.
    FIND b-rdtlh WHERE ROWID(b-rdtlh) EQ ROWID(fg-rdtlh) NO-LOCK NO-ERROR.


    RUN set-read-only (YES).

    RUN repo-query (ROWID(fg-rcpth)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no B-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ itemfg.company
                      AND cust.cust-no EQ fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Customer#..." VIEW-AS ALERT-BOX ERROR.
      fg-rdtlh.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rdtlh.cust-no.
      APPLY "entry" TO fg-rdtlh.cust-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no B-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-itemfg FOR itemfg.


  DO WITH FRAME {&FRAME-NAME}:
    fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST b-itemfg
                    WHERE b-itemfg.company EQ itemfg.company
                      AND b-itemfg.i-no    EQ fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid FG Item#..." VIEW-AS ALERT-BOX ERROR.
      fg-rcpth.i-no:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rcpth.i-no.
      APPLY "entry" TO fg-rcpth.i-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc B-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ itemfg.company
                      AND loc.loc     EQ fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Warehouse..." VIEW-AS ALERT-BOX ERROR.
      fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rdtlh.loc.
      APPLY "entry" TO fg-rdtlh.loc IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin B-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    fg-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} =
        CAPS(fg-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name}).

    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ itemfg.company
                      AND fg-bin.i-no    EQ ""
                      AND fg-bin.loc     EQ fg-rdtlh.loc:SCREEN-VALUE IN BROWSE {&browse-name}
                      AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name})
    THEN DO:
      MESSAGE "Invalid Bin..." VIEW-AS ALERT-BOX ERROR.
      fg-rdtlh.loc-bin:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rdtlh.loc-bin.
      APPLY "entry" TO fg-rdtlh.loc-bin IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag B-table-Win 
PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

  DEF VAR lv-tag LIKE fg-rdtlh.tag NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    lv-tag = fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}.

    IF lv-tag NE fg-rdtlh.tag AND
       (lv-tag EQ ""       OR
        (NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company   EQ itemfg.company
                        AND loadtag.item-type EQ NO
                        AND loadtag.i-no      EQ itemfg.i-no
                        AND loadtag.tag-no    EQ lv-tag)             AND
         NOT CAN-FIND(FIRST b-fg-rdtlh
                      WHERE b-fg-rdtlh.company EQ itemfg.company
                        AND b-fg-rdtlh.tag     EQ lv-tag
                        AND ROWID(b-fg-rdtlh)  NE ROWID(fg-rdtlh)))) THEN DO:
      MESSAGE "Invalid Tag# Change..." VIEW-AS ALERT-BOX ERROR.
      fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rdtlh.tag.
      APPLY "entry" TO fg-rdtlh.tag IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.



  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tag-no B-table-Win 
PROCEDURE valid-tag-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tag LIKE fg-rdtlh.tag NO-UNDO.
  DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

  DO WITH FRAME {&FRAME-NAME}:
      lv-tag = fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name}.

   IF fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} NE "" AND
       fg-rcpth.rita-code:SCREEN-VALUE IN BROWSE {&browse-name} EQ "R" AND  
       int(fg-rdtlh.qty:SCREEN-VALUE IN BROWSE {&browse-name}) GT 0 THEN do:

       FIND FIRST  b-fg-rdtlh NO-LOCK
           WHERE b-fg-rdtlh.company EQ cocode
           /*AND b-fg-rcpth.i-no EQ fi_rm-i-no*/
           AND b-fg-rdtlh.tag     EQ lv-tag 
           AND ROWID(b-fg-rdtlh)  NE ROWID(fg-rdtlh) NO-ERROR .

       IF AVAIL b-fg-rdtlh THEN DO:
           MESSAGE "This Tag Number has already been used..." VIEW-AS ALERT-BOX INFO.
           fg-rdtlh.tag:SCREEN-VALUE IN BROWSE {&browse-name} = fg-rdtlh.tag.
           APPLY "entry" TO fg-rdtlh.tag IN BROWSE {&browse-name}.
           RETURN ERROR.
       END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-bol B-table-Win 
FUNCTION display-bol RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST oe-bolh
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.


  IF AVAIL oe-bolh THEN
      RETURN INT(oe-bolh.bol-no)    .
  ELSE
      RETURN 0.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-ship B-table-Win 
FUNCTION display-ship RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST oe-bolh
          WHERE oe-bolh.company EQ cocode
            AND oe-bolh.b-no    EQ fg-rcpth.b-no NO-LOCK NO-ERROR.

  IF AVAIL oe-bolh THEN DO:
     RETURN STRING(oe-bolh.cust-no)    .
  END.
  ELSE
      RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-fg-qty B-table-Win 
FUNCTION get-fg-qty RETURNS INT
  ( /* parameter-definitions */ INPUT ip-int AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iReturnVal AS INTEGER NO-UNDO.
DEFINE VARIABLE iBinQtyb AS INTEGER NO-UNDO.
DEFINE VARIABLE iBinQtya AS INTEGER NO-UNDO.
DEFINE VARIABLE iBinQtydeff AS INTEGER NO-UNDO.
DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth .
DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh .

  iBinQtyb = 0 .  

 IF fg-rcpth.rita-code = "C" THEN DO:
  FOR EACH bf-fg-rcpth NO-LOCK
      WHERE bf-fg-rcpth.company EQ cocode 
        AND bf-fg-rcpth.i-no EQ fg-rcpth.i-no
        AND bf-fg-rcpth.job-no EQ fg-rcpth.job-no
        AND bf-fg-rcpth.job-no2 EQ fg-rcpth.job-no2
        AND bf-fg-rcpth.po-no EQ fg-rcpth.po-no
        AND bf-fg-rcpth.rita-code NE "C" ,
       EACH bf-fg-rdtlh NO-LOCK WHERE
            bf-fg-rdtlh.r-no EQ bf-fg-rcpth.r-no
        AND bf-fg-rdtlh.loc EQ fg-rdtlh.loc
        AND bf-fg-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
        AND bf-fg-rdtlh.tag EQ fg-rdtlh.tag
        AND bf-fg-rdtlh.cust-no EQ fg-rdtlh.cust-no 
        AND bf-fg-rdtlh.bol-no EQ fg-rdtlh.bol-no
        AND bf-fg-rdtlh.inv-no EQ fg-rdtlh.inv-no BY fg-rcpth.trans-date DESC :
      iBinQtyb = iBinQtyb +  bf-fg-rdtlh.qty  .
      LEAVE .
  END.

  ASSIGN iBinQtya = fg-rdtlh.qty  
      iBinQtydeff = iBinQtya - iBinQtyb .

  IF ip-int = 1 THEN
       iReturnVal = iBinQtyb .
  ELSE 
       iReturnVal = iBinQtydeff.

 END. /* fg-rcpth.rita-code = "C" */
 ELSE DO:
     IF ip-int = 1 THEN
       iReturnVal = 0 .
   ELSE 
       iReturnVal = fg-rdtlh.qty .

 END.

 RETURN iReturnVal .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pallet-info B-table-Win 
FUNCTION get-pallet-info RETURNS INTEGER
  (OUTPUT op-qty-pal AS INT):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND FIRST fg-bin
    WHERE fg-bin.company EQ cocode
      AND fg-bin.i-no    EQ fg-rcpth.i-no
      AND fg-bin.job-no  EQ fg-rcpth.job-no
      AND fg-bin.job-no2 EQ fg-rcpth.job-no2
      AND fg-bin.loc     EQ fg-rdtlh.loc
      AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
      AND fg-bin.tag     EQ fg-rdtlh.tag
      AND fg-bin.cust-no EQ fg-rdtlh.cust-no
    NO-LOCK NO-ERROR.  

/*if avail fg-bin then
  assign
   op-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
   /*li-pallets = fg-rdtlh.qty / op-qty-pal*/.

else*/
  op-qty-pal = (IF fg-rdtlh.qty-case     NE 0 THEN fg-rdtlh.qty-case     ELSE
                IF AVAIL fg-bin AND
                   fg-bin.case-count     NE 0 THEN fg-bin.case-count     ELSE 1) *
               (IF fg-rdtlh.stacks-unit  NE 0 THEN fg-rdtlh.stacks-unit  ELSE
                IF AVAIL fg-bin AND
                   fg-bin.cases-unit     NE 0 THEN fg-bin.cases-unit     ELSE 1) *
               (IF fg-rdtlh.units-pallet NE 0 THEN fg-rdtlh.units-pallet ELSE
                IF AVAIL fg-bin AND
                   fg-bin.units-pallet   NE 0 THEN fg-bin.units-pallet   ELSE 1).
  /*assign
   li-pallets = 1
   op-qty-pal = fg-rdtlh.qty.*/

li-pallets = fg-rdtlh.qty / op-qty-pal.

{sys/inc/roundup.i li-pallets}

IF op-qty-pal LT 0 THEN
  ASSIGN
   op-qty-pal = op-qty-pal * -1
   li-pallets = li-pallets * -1.

RETURN li-pallets.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vend-info B-table-Win 
FUNCTION get-vend-info RETURNS CHAR
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND FIRST po-ord
    WHERE po-ord.company EQ cocode
      AND po-ord.po-no    EQ int(fg-rcpth.po-no)
    NO-LOCK NO-ERROR.  

IF AVAIL po-ord THEN DO:
    FIND FIRST vend
        WHERE vend.company EQ cocode
        AND vend.vend-no    EQ po-ord.vend-no
        NO-LOCK NO-ERROR.  

    IF AVAIL vend THEN
        RETURN STRING(vend.NAME) .
    ELSE
        RETURN "" .
END.
ELSE RETURN "" .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-vend-no B-table-Win 
FUNCTION get-vend-no RETURNS CHAR
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

FIND FIRST po-ord
    WHERE po-ord.company EQ cocode
      AND po-ord.po-no    EQ int(fg-rcpth.po-no)
    NO-LOCK NO-ERROR.  

IF AVAIL po-ord THEN
    RETURN STRING(po-ord.vend-no) .
ELSE
    RETURN "" .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

