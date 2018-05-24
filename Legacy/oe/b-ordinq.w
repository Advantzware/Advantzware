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

  File:  oe\b-ordinq.w

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
&SCOPED-DEFINE repositionBrowse
&SCOPED-DEFINE xlocal-destroy xlocal-destroy
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/oeinq.i}
{methods\defines\hndldefs.i}
DEFINE BUFFER cust-fg FOR itemfg.
DEFINE BUFFER b-itemfg FOR itemfg.
DEFINE BUFFER b-oe-rel FOR oe-rel.

DEFINE VARIABLE ll-first AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE ll-initial AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE lv-sort-by AS CHARACTER INIT "ord-no" NO-UNDO.
DEFINE VARIABLE lv-sort-by-lab AS CHARACTER INIT "Order#" NO-UNDO.
DEFINE VARIABLE ll-sort-asc AS LOG NO-UNDO.
DEFINE VARIABLE lv-frst-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lv-last-rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lv-show-prev AS LOG NO-UNDO.
DEFINE VARIABLE lv-show-next AS LOG NO-UNDO.
DEFINE VARIABLE lv-last-show-ord-no AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-first-show-ord-no AS INTEGER NO-UNDO.
DEFINE VARIABLE li-prod AS INTEGER NO-UNDO.
DEFINE VARIABLE li-bal AS INTEGER NO-UNDO.
DEFINE VARIABLE li-wip AS INTEGER NO-UNDO.
DEFINE VARIABLE li-pct AS INTEGER NO-UNDO.
DEFINE VARIABLE li-qoh AS INTEGER NO-UNDO.
DEFINE VARIABLE li-act-bol-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE li-act-rel-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE lc-fgitem AS CHARACTER NO-UNDO FORMAT 'X(20)'.
DEFINE VARIABLE ld-xfer-qty LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE search-return AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-col-move AS LOG INIT YES NO-UNDO.
DEFINE VARIABLE v-rec-key-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc-rs AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lc-mi AS CHARACTER NO-UNDO.
DEFINE VARIABLE lr-rel-lib AS HANDLE NO-UNDO.
DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
DEFINE VARIABLE iHandQtyNoalloc AS INTEGER NO-UNDO .
DEFINE VARIABLE lActive AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttRelease NO-UNDO
    FIELD ordlRecID AS RECID
    FIELD lot-no AS CHARACTER
    FIELD po-no AS CHARACTER
        INDEX ttRelease IS PRIMARY ordlRecID
        .

DO TRANSACTION:
    {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""OU1"" }
END.
ll-sort-asc = NO /*oeinq*/.

&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode AND oe-ordl.opened EQ YES AND oe-ordl.stat NE 'C'

&SCOPED-DEFINE for-eachblank ~
    FOR EACH oe-ordl ~
        WHERE {&key-phrase} ~
          AND ((LOOKUP(oe-ordl.cust-no,custcount) NE 0 ~
          AND oe-ordl.cust-no NE '') OR custcount EQ '')

&SCOPED-DEFINE for-each1 ~
    FOR EACH oe-ordl ~
        WHERE {&key-phrase} ~
          AND ((LOOKUP(oe-ordl.cust-no,custcount) NE 0 ~
          AND oe-ordl.cust-no NE '') OR custcount EQ '') ~
          AND oe-ordl.cust-no BEGINS fi_cust-no ~
          AND oe-ordl.i-no BEGINS fi_i-no ~
          AND oe-ordl.part-no BEGINS fi_part-no ~
          AND oe-ordl.po-no   BEGINS fi_po-no1 ~
          AND oe-ordl.est-no BEGINS fi_est-no ~
          AND oe-ordl.s-man[1] BEGINS fi_sman ~
          AND oe-ordl.job-no BEGINS fi_job-no ~
          AND (oe-ordl.job-no2 EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ '') ~
          AND (oe-ordl.i-name BEGINS fi_i-name OR (INDEX(fi_i-name,'*') NE 0 ~
          AND oe-ordl.i-name MATCHES fi_i-name))

&SCOPED-DEFINE for-each11 ~
    FOR EACH oe-ordl ~
        WHERE {&key-phrase} ~
          AND ((LOOKUP(oe-ordl.cust-no,custcount) NE 0 ~
          AND oe-ordl.cust-no NE '') OR custcount EQ '') ~
          AND oe-ordl.cust-no BEGINS fi_cust-no ~
          AND oe-ordl.i-no MATCHES (IF INDEX(fi_i-no, '*') EQ 0 THEN '*' ELSE fi_i-no) ~
          AND oe-ordl.part-no MATCHES (IF INDEX(fi_part-no, '*') EQ 0 THEN '*' ELSE fi_part-no) ~
          AND oe-ordl.po-no     MATCHES (IF index(fi_po-no1, "*") EQ 0 THEN "*" ELSE fi_po-no1) ~
          AND oe-ordl.est-no BEGINS fi_est-no ~
          AND oe-ordl.s-man[1] BEGINS fi_sman ~
          AND oe-ordl.job-no BEGINS fi_job-no ~
          AND (oe-ordl.job-no2 EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ '') ~
          AND (oe-ordl.i-name BEGINS fi_i-name OR (INDEX(fi_i-name,'*') NE 0 ~
          AND oe-ordl.i-name MATCHES fi_i-name))

&SCOPED-DEFINE for-each2 ~
    FIRST oe-ord OF oe-ordl ~
    WHERE (oe-ord.stat NE 'W' AND tb_web EQ NO) ~
       OR (oe-ord.stat EQ 'W' AND tb_web EQ YES) ~
      USE-INDEX ord-no NO-LOCK, ~
    FIRST itemfg ~{&joinScop} NO-LOCK ~
    WHERE itemfg.company EQ oe-ordl.company ~
      AND itemfg.i-no EQ oe-ordl.i-no ~
      AND itemfg.cad-no BEGINS fi_cad-no

&SCOPED-DEFINE for-each3 FIRST oe-ord OF oe-ordl WHERE oe-ord.stat NE 'W' USE-INDEX ord-no NO-LOCK

&SCOPED-DEFINE sortby-log ~
    IF lv-sort-by EQ 'ord-no'    THEN STRING(oe-ordl.ord-no,'9999999999') ELSE ~
    IF lv-sort-by EQ 'cStatus'   THEN oe-ord.stat ELSE ~
    IF lv-sort-by EQ 'lc-rs'     THEN getRS() ELSE ~
    IF lv-sort-by EQ 'lc-mi'     THEN getMI() ELSE ~
    IF lv-sort-by EQ 'ord-date'  THEN STRING(YEAR(oe-ord.ord-date),'9999') + STRING(MONTH(oe-ord.ord-date),'99') + STRING(DAY(oe-ord.ord-date),'99') ELSE ~
    IF lv-sort-by EQ 'cust-no'   THEN oe-ordl.cust-no ELSE ~
    IF lv-sort-by EQ 'cust-name' THEN oe-ord.cust-name ELSE ~
    IF lv-sort-by EQ 'i-no'      THEN oe-ordl.i-no ELSE ~
    IF lv-sort-by EQ 'i-name'    THEN oe-ordl.i-name ELSE ~
    IF lv-sort-by EQ 'part-no'   THEN oe-ordl.part-no ELSE ~
    IF lv-sort-by EQ 'po-no'     THEN oe-ordl.po-no ELSE ~
    IF lv-sort-by EQ 'lot-no'    THEN oe-ordl.lot-no ELSE ~
    IF lv-sort-by EQ 'est-no'    THEN oe-ordl.est-no ELSE ~
    IF lv-sort-by EQ 'job-no'    THEN STRING(oe-ordl.job-no,'x(6)') + STRING(oe-ordl.job-no2,'99')ELSE ~
    IF lv-sort-by EQ 'cad-no'    THEN itemfg.cad-no ELSE ~
    IF lv-sort-by EQ 's-man'     THEN oe-ordl.s-man[1] ELSE ~
    IF lv-sort-by EQ 'e-num'     THEN STRING(oe-ordl.e-num) ELSE ~
                                      STRING(YEAR(oe-ordl.req-date),'9999') + STRING(MONTH(oe-ordl.req-date),'99') + STRING(DAY(oe-ordl.req-date),'99')

&SCOPED-DEFINE sortby BY oe-ordl.ord-no BY oe-ordl.i-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}

DO TRANSACTION:
  {sys/inc/browser.i "OEBROWSE"}
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
&Scoped-define INTERNAL-TABLES oe-ordl oe-ord itemfg

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no oe-ordl.cust-no ~
getRS() @ lc-rs getMI() @ lc-mi getStat() @ cStatus oe-ord.ord-date ~
oe-ordl.req-date oe-ord.cust-name oe-ordl.i-no oe-ordl.part-no ~
oe-ordl.po-no oe-ordl.lot-no oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 ~
itemfg.cad-no oe-ordl.qty get-prod(li-bal) @ li-prod oe-ordl.ship-qty ~
get-xfer-qty () @ ld-xfer-qty oe-ordl.inv-qty get-bal(li-qoh) @ li-bal ~
get-act-rel-qty() @ li-act-rel-qty get-wip() @ li-wip ~
get-pct(li-bal) @ li-pct get-fgitem() @ lc-fgitem oe-ordl.i-name ~
oe-ordl.line oe-ordl.po-no-po oe-ordl.e-num oe-ordl.whsed ~
get-act-bol-qty() @ li-act-bol-qty getTotalReturned() @ dTotQtyRet ~
getReturnedInv() @ dTotRetInv oe-ordl.s-man[1] ~
fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc ~
oe-ordl.managed 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no ~
oe-ordl.cust-no oe-ord.ord-date oe-ordl.req-date ~
oe-ord.cust-name oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no ~
oe-ordl.job-no oe-ordl.job-no2 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ord
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-ordl ~
      WHERE oe-ordl.company EQ g_company ~
AND oe-ordl.ord-no EQ 999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK, ~
      FIRST itemfg OF oe-ordl ~
      WHERE itemfg.company EQ oe-ordl.company ~
AND itemfg.i-no EQ oe-ordl.i-no OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-ordl ~
      WHERE oe-ordl.company EQ g_company ~
AND oe-ordl.ord-no EQ 999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK, ~
      FIRST itemfg OF oe-ordl ~
      WHERE itemfg.company EQ oe-ordl.company ~
AND itemfg.i-no EQ oe-ordl.i-no OUTER-JOIN NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table oe-ord
&Scoped-define THIRD-TABLE-IN-QUERY-Browser-Table itemfg


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tb_web fi_ord-no fi_cust-no fi_i-no ~
fi_part-no fi_po-no1 fi_est-no fi_job-no fi_job-no2 fi_cad-no fi_sman ~
btn_go btn_prev Browser-Table fi_i-name RECT-1 
&Scoped-Define DISPLAYED-OBJECTS tb_web fi_ord-no fi_cust-no fi_i-no ~
fi_part-no fi_po-no1 fi_est-no fi_job-no fi_job-no2 fi_cad-no fi_sman ~
fi_sort-by FI_moveCol fi_i-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fget-qty-nothand B-table-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
  (ipBal AS INTEGER,ipHand AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-bol-qty B-table-Win 
FUNCTION get-act-bol-qty RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-rel-qty B-table-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ()  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-bal B-table-Win 
FUNCTION get-bal RETURNS INTEGER
  (OUTPUT op-qoh AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fgitem B-table-Win 
FUNCTION get-fgitem RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pct B-table-Win 
FUNCTION get-pct RETURNS INTEGER
  (ipBal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod B-table-Win 
FUNCTION get-prod RETURNS INTEGER
  (OUTPUT op-bal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-wip B-table-Win 
FUNCTION get-wip RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-xfer-qty B-table-Win 
FUNCTION get-xfer-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMI B-table-Win 
FUNCTION getMI RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturned B-table-Win 
FUNCTION getReturned RETURNS DECIMAL
  (ipcValueNeeded AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturnedInv B-table-Win 
FUNCTION getReturnedInv RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getRS B-table-Win 
FUNCTION getRS RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getstat B-table-Win 
FUNCTION getstat RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTotalReturned B-table-Win 
FUNCTION getTotalReturned RETURNS DECIMAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 6 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "&Next" 
     SIZE 9 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "&Previous" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE fi_cad-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_cust-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_est-no AS CHARACTER FORMAT "X(8)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-name AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_i-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_job-no2 AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FI_moveCol AS CHARACTER FORMAT "X(4)":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE VARIABLE fi_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_part-no AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no1 AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sman AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY .95
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 156 BY 3.33.

DEFINE VARIABLE tb_web AS LOGICAL INITIAL no 
     LABEL "WebOrders" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-ordl
    FIELDS(oe-ordl.ord-no
      oe-ordl.cust-no
      oe-ordl.req-date
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.lot-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2
      oe-ordl.qty
      oe-ordl.ship-qty
      oe-ordl.inv-qty
      oe-ordl.i-name
      oe-ordl.line
      oe-ordl.po-no-po
      oe-ordl.e-num
      oe-ordl.whsed
      oe-ordl.s-man[1]
      oe-ordl.managed), 
      oe-ord, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-ordl.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      oe-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      getRS() @ lc-rs COLUMN-LABEL "R&S" FORMAT "X":U WIDTH 4 LABEL-BGCOLOR 14
      getMI() @ lc-mi COLUMN-LABEL "MI" FORMAT "X":U WIDTH 4 LABEL-BGCOLOR 14
      getStat() @ cStatus COLUMN-LABEL "Status" FORMAT "x(16)":U LABEL-BGCOLOR 14
      oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
            WIDTH 14.4 LABEL-BGCOLOR 14
      oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            WIDTH 14.2 LABEL-BGCOLOR 14
      oe-ord.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      oe-ordl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      oe-ordl.po-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.lot-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      itemfg.cad-no COLUMN-LABEL "CAD#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.qty COLUMN-LABEL "Ordered Qty" FORMAT "->>,>>>,>>>":U
      get-prod(li-bal) @ li-prod COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
      oe-ordl.ship-qty COLUMN-LABEL "Shipped Qty" FORMAT "->>,>>>,>>>":U
      get-xfer-qty () @ ld-xfer-qty COLUMN-LABEL "Transfer!Qty" FORMAT "->>,>>>,>>>":U
      oe-ordl.inv-qty COLUMN-LABEL "Invoice Qty" FORMAT "->>,>>>,>>>":U
      get-bal(li-qoh) @ li-bal COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
      get-act-rel-qty() @ li-act-rel-qty COLUMN-LABEL "Act. Rel.!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 12.4
      get-wip() @ li-wip COLUMN-LABEL "Production!Balance" FORMAT "->>,>>>,>>>":U
            WIDTH 14.4
      get-pct(li-bal) @ li-pct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
      get-fgitem() @ lc-fgitem COLUMN-LABEL "FG Item#" FORMAT "x(15)":U
      oe-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      oe-ordl.line FORMAT ">>99":U
      oe-ordl.po-no-po FORMAT ">>>>>9":U
      oe-ordl.e-num FORMAT ">>>>>9":U
      oe-ordl.whsed FORMAT "yes/no":U
      get-act-bol-qty() @ li-act-bol-qty COLUMN-LABEL "Act. BOL!Qty" FORMAT "->>,>>>,>>>":U
      getTotalReturned() @ dTotQtyRet COLUMN-LABEL "Tot Returned" FORMAT ">>>,>>9":U
      getReturnedInv() @ dTotRetInv COLUMN-LABEL "Qty Returned Inv" FORMAT ">>>,>>9":U
      oe-ordl.s-man[1] COLUMN-LABEL "Rep" FORMAT "x(3)":U LABEL-BGCOLOR 14
      fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc COLUMN-LABEL "On Hand Qty not Allocated" FORMAT "->>>>>>>>":U
      oe-ordl.managed FORMAT "yes/no":U
  ENABLE
      oe-ordl.ord-no
      oe-ordl.cust-no
      oe-ord.ord-date
      oe-ordl.req-date
      oe-ord.cust-name
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 155 BY 16.5
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tb_web AT ROW 3.14 COL 142 WIDGET-ID 14
     fi_ord-no AT ROW 1.95 COL 2 NO-LABEL
     fi_cust-no AT ROW 1.95 COL 14 COLON-ALIGNED NO-LABEL
     fi_i-no AT ROW 1.95 COL 28 COLON-ALIGNED NO-LABEL
     fi_part-no AT ROW 1.95 COL 48 COLON-ALIGNED NO-LABEL
     fi_po-no1 AT ROW 1.95 COL 68 COLON-ALIGNED NO-LABEL
     fi_est-no AT ROW 1.95 COL 88 COLON-ALIGNED NO-LABEL
     fi_job-no AT ROW 1.95 COL 102 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 1.95 COL 113 COLON-ALIGNED NO-LABEL
     fi_cad-no AT ROW 1.95 COL 117 COLON-ALIGNED NO-LABEL
     fi_sman AT ROW 1.95 COL 138 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     btn_go AT ROW 3.14 COL 2
     btn_prev AT ROW 3.14 COL 8
     btn_next AT ROW 3.14 COL 20
     fi_sort-by AT ROW 3.14 COL 87.2 COLON-ALIGNED NO-LABEL
     FI_moveCol AT ROW 3.14 COL 130 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_i-name AT ROW 3.14 COL 28 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 104
          FGCOLOR 9 FONT 6
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 90
          FGCOLOR 9 FONT 6
     "BrwsrColMode:" VIEW-AS TEXT
          SIZE 16.6 BY 1 AT ROW 3.14 COL 115 WIDGET-ID 6
          FONT 6
     "CAD#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 119
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 13 BY 1 AT ROW 3.14 COL 76 WIDGET-ID 18
          FONT 6
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 2
          FGCOLOR 9 FONT 6
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 16
          FGCOLOR 9 FONT 6
     "FG Item#/Name" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 1.24 COL 30
          FGCOLOR 9 FONT 6
     "Cust Part#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 50
          FGCOLOR 9 FONT 6
     "REP#" VIEW-AS TEXT
          SIZE 6.6 BY .71 AT ROW 1.24 COL 140.2 WIDGET-ID 12
          FGCOLOR 9 FONT 6
     "Cust PO#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 70
          FGCOLOR 9 FONT 6
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
/* BROWSE-TAB Browser-Table FI_moveCol F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

ASSIGN 
       oe-ordl.line:VISIBLE IN BROWSE Browser-Table = FALSE
       oe-ordl.po-no-po:VISIBLE IN BROWSE Browser-Table = FALSE
       oe-ordl.whsed:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI_moveCol IN FRAME F-Main
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
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl,ASI.itemfg OF ASI.oe-ordl"
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _TblOptList       = "USED,, FIRST OUTER, FIRST"
     _Where[1]         = "oe-ordl.company EQ g_company
AND oe-ordl.ord-no EQ 999999"
     _Where[3]         = "itemfg.company EQ oe-ordl.company
AND itemfg.i-no EQ oe-ordl.i-no"
     _FldNameList[1]   > ASI.oe-ordl.ord-no
"oe-ordl.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ordl.cust-no
"oe-ordl.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"getRS() @ lc-rs" "R&S" "X" ? ? ? ? 14 ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"getMI() @ lc-mi" "MI" "X" ? ? ? ? 14 ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"getstat" "Status" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.oe-ord.ord-date
"oe-ord.ord-date" "Order Date" ? "date" ? ? ? 14 ? ? yes ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-ordl.req-date
"oe-ordl.req-date" "Due Date" ? "date" ? ? ? 14 ? ? yes ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.oe-ord.cust-name
"oe-ord.cust-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" "Cust Part#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordl.lot-no
"oe-ordl.lot-no" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" "Est#" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" "Job#" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.itemfg.cad-no
"itemfg.cad-no" "CAD#" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ordl.qty
"oe-ordl.qty" "Ordered Qty" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > "_<CALC>"
"get-prod(li-bal) @ li-prod" "Prod. Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.oe-ordl.ship-qty
"oe-ordl.ship-qty" "Shipped Qty" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"get-xfer-qty () @ ld-xfer-qty" "Transfer!Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.oe-ordl.inv-qty
"oe-ordl.inv-qty" "Invoice Qty" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"get-bal(li-qoh) @ li-bal" "On Hand Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"get-act-rel-qty() @ li-act-rel-qty" "Act. Rel.!Quantity" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"get-wip() @ li-wip" "Production!Balance" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "14.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"get-pct(li-bal) @ li-pct" "O/U%" "->>>>>%" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"get-fgitem() @ lc-fgitem" "FG Item#" "x(15)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > ASI.oe-ordl.i-name
"oe-ordl.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.oe-ordl.line
"oe-ordl.line" ? ">>99" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.oe-ordl.po-no-po
"oe-ordl.po-no-po" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   = ASI.oe-ordl.e-num
     _FldNameList[31]   > ASI.oe-ordl.whsed
"oe-ordl.whsed" ? ? "logical" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"get-act-bol-qty() @ li-act-bol-qty" "Act. BOL!Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"getTotalReturned() @ dTotQtyRet" "Tot Returned" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"getReturnedInv() @ dTotRetInv" "Qty Returned Inv" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > ASI.oe-ordl.s-man[1]
"oe-ordl.s-man[1]" "Rep" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc" "On Hand Qty not Allocated" "->>>>>>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   = ASI.oe-ordl.managed
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
  DEFINE VARIABLE phandle AS HANDLE NO-UNDO.
  DEFINE VARIABLE char-hdl AS cha NO-UNDO.


  {methods/run_link.i "container-source" "select-page" "(2)"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
  li-pct:FGCOLOR IN BROWSE {&BROWSE-NAME} = IF get-pct(get-prod(li-bal)) LT 0 THEN 12 ELSE 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  IF LASTKEY EQ -1 THEN DO:
    APPLY 'ENTRY' TO fi_ord-no IN FRAME {&FRAME-NAME}.
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
  DEFINE VARIABLE lh-column AS HANDLE NO-UNDO.
  DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  DEFINE VARIABLE v-stat AS CHARACTER NO-UNDO.
  DEFINE BUFFER b-cust FOR cust.
  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
     "(oe-ordl.rec_key,{methods/headers/oe-ordl.i})"}
  {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
     "(CAN-FIND(FIRST notes WHERE notes.rec_key = oe-ordl.rec_key))"}
  {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
     "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = oe-ordl.rec_key))"}

  RUN paper-clip-image-proc(INPUT oe-ordl.rec_key).

  RUN spec-book-image-proc.
  RUN dept-pan-image-proc.

  FIND FIRST b-cust WHERE
       b-cust.company EQ oe-ord.company AND
       b-cust.cust-no EQ oe-ord.cust-no
       NO-LOCK NO-ERROR.

  IF AVAILABLE b-cust THEN
     RUN pushpin-image-proc(INPUT b-cust.rec_key).


/* 08291305 - move this to b-ordrel.w */
/*   IF AVAIL oe-ordl THEN                                                            */
/*      FOR EACH b-oe-rel WHERE                                                       */
/*          b-oe-rel.company EQ oe-ordl.company AND                                   */
/*          b-oe-rel.ord-no  EQ oe-ordl.ord-no AND                                    */
/*          b-oe-rel.link-no EQ 0                                                     */
/*          NO-LOCK:                                                                  */
/*                                                                                    */
/*          RUN oe/rel-stat.p (ROWID(b-oe-rel), OUTPUT v-stat).                       */
/*                                                                                    */
/*          IF v-stat NE b-oe-rel.stat THEN                                           */
/*          DO:                                                                       */
/*             FIND oe-rel WHERE                                                      */
/*                  ROWID(oe-rel) EQ ROWID(b-oe-rel) EXCLUSIVE-LOCK NO-ERROR NO-WAIT. */
/*                                                                                    */
/*             IF AVAIL oe-rel THEN                                                   */
/*             DO:                                                                    */
/*                oe-rel.stat = v-stat.                                               */
/*                FIND CURRENT oe-rel NO-LOCK NO-ERROR.                               */
/*             END.                                                                   */
/*          END.                                                                      */
/*     END.                                                                           */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DEFINE VARIABLE v-cust-no AS CHARACTER NO-UNDO .
  DEFINE BUFFER bf-oe-ordl  FOR oe-ordl .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      fi_cust-no
      fi_i-no
      fi_i-name
      fi_part-no
      fi_cust-no
      fi_ord-no
      fi_po-no1
      fi_est-no
      fi_job-no
      fi_job-no2
      fi_cad-no
      fi_sman
      .

    ll-first = NO.
    RUN dispatch ("open-query").
    GET FIRST Browser-Table .
     IF NOT AVAILABLE oe-ord THEN DO:
         IF fi_cust-no NE "" THEN DO:
            v-cust-no = fi_cust-no .
         END.
         ELSE DO:
             v-cust-no = "".
             FIND FIRST bf-oe-ordl NO-LOCK 
                  WHERE bf-oe-ordl.company EQ cocode
                    AND bf-oe-ordl.opened EQ YES
                    AND bf-oe-ordl.stat NE "C"
                    AND (bf-oe-ordl.cust-no BEGINS fi_cust-no OR fi_cust-no EQ "")
                    AND (bf-oe-ordl.part-no BEGINS fi_part-no OR fi_part-no EQ "")
                    AND (bf-oe-ordl.i-no BEGINS fi_i-no OR fi_i-no EQ "")
                    AND (bf-oe-ordl.ord-no EQ fi_ord-no OR fi_ord-no EQ 0)
                    AND (bf-oe-ordl.est-no BEGINS fi_est-no OR fi_est-no EQ "")
                    AND (bf-oe-ordl.job-no BEGINS fi_job-no OR fi_job-no EQ "")
                    AND (bf-oe-ordl.po-no BEGINS fi_po-no1 OR fi_po-no1 = "")
                    AND (bf-oe-ordl.s-man[1] BEGINS fi_sman OR fi_sman EQ "")
                  NO-ERROR.
             IF AVAILABLE bf-oe-ordl THEN DO:
                 v-cust-no = bf-oe-ordl.cust-no.
             END.
         END.
         FIND FIRST cust NO-LOCK
              WHERE cust.company EQ cocode 
                AND cust.cust-no EQ v-cust-no
              NO-ERROR.
         IF AVAILABLE cust AND ou-log AND LOOKUP(cust.cust-no,custcount) EQ 0 THEN
             MESSAGE
                "Customer is not on Users Customer List." SKIP
                "Please add customer to Network Admin - Users Customer List."
             VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_next B-table-Win
ON CHOOSE OF btn_next IN FRAME F-Main /* Next */
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
ON CHOOSE OF btn_prev IN FRAME F-Main /* Previous */
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
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEFINE VARIABLE char-val AS cha NO-UNDO.
   RUN windows/l-cust2.w (INPUT g_company, INPUT {&SELF-NAME}:screen-value, "", OUTPUT char-val).
          IF char-val <> "" THEN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON HELP OF fi_est-no IN FRAME F-Main
DO:
   DEFINE VARIABLE char-val AS cha NO-UNDO.
   DEFINE BUFFER buff-eb FOR eb.
   RUN windows/l-est2.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value,  OUTPUT char-val).
          IF char-val <> "" THEN
              FIND FIRST buff-eb WHERE RECID(buff-eb) EQ int(ENTRY(1,char-val)) NO-LOCK NO-ERROR. 
          IF AVAILABLE buff-eb THEN
              ASSIGN {&SELF-NAME}:SCREEN-VALUE = (buff-eb.est-no).
          RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-name B-table-Win
ON LEAVE OF fi_i-name IN FRAME F-Main
DO:
  /*IF LASTKEY NE -1 THEN DO:
    APPLY "choose" TO btn_go.
  END.
  */
/*   {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON HELP OF fi_i-no IN FRAME F-Main
DO:
   DEFINE VARIABLE char-val AS cha NO-UNDO.
   RUN windows/l-itemfg.w (INPUT g_company,"", INPUT {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
          IF char-val <> "" THEN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON HELP OF fi_job-no IN FRAME F-Main
DO:
   DEFINE VARIABLE char-val AS cha NO-UNDO.
    DEFINE VARIABLE char-rec AS RECID NO-UNDO.
   RUN windows/l-jobno3.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value,  OUTPUT char-val , OUTPUT char-rec ).
          IF char-val <> "" THEN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          RETURN NO-APPLY.
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


&Scoped-define SELF-NAME fi_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-no B-table-Win
ON HELP OF fi_ord-no IN FRAME F-Main
DO:
   DEFINE VARIABLE char-val AS cha NO-UNDO.
   RUN windows/l-ordno2.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value, OUTPUT char-val).
          IF char-val <> "" THEN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON HELP OF fi_part-no IN FRAME F-Main
DO:
  DEFINE VARIABLE char-rec AS RECID NO-UNDO.
  RUN windows/l-cstprt.w (cocode,fi_cust-no:SCREEN-VALUE,{&SELF-NAME}:SCREEN-VALUE,
                         fi_i-no:SCREEN-VALUE,  OUTPUT search-return, OUTPUT char-rec ) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  IF search-return <> "" THEN {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,search-return).
          RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_sman
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_sman B-table-Win
ON HELP OF fi_sman IN FRAME F-Main
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.
    RUN windows/l-sman.w (g_company, OUTPUT char-val).
    IF char-val NE "" THEN 
        {&SELF-NAME}:screen-value = ENTRY(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_sman B-table-Win
ON VALUE-CHANGED OF fi_sman IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_web
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_web B-table-Win
ON VALUE-CHANGED OF tb_web IN FRAME F-Main /* WebOrders */
DO:
    ASSIGN {&SELF-NAME}.
    APPLY 'CHOOSE':U TO btn_go.
    IF {&SELF-NAME} EQ YES THEN
    RUN util/fixcXMLDuplicates.p.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode
&SCOPED-DEFINE cellColumnDat b-ordinq

{sys/inc/f3help.i}
{methods/browsers/setCellColumns.i}

/*  RUN-PROC = "sbo/oerel-recalc-act.p". */
/* {methods/smartrun.i}                  */
/* lr-rel-lib = phandle.                 */
RUN sbo/oerel-recalc-act.p PERSISTENT SET lr-rel-lib.

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OU1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""OU1""}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE browse-rowid B-table-Win 
PROCEDURE browse-rowid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER op-rowid AS ROWID NO-UNDO.


  IF AVAILABLE {&FIRST-TABLE-IN-QUERY-{&browse-name}} THEN
    op-rowid = ROWID({&FIRST-TABLE-IN-QUERY-{&browse-name}}).

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
   DEFINE VARIABLE v-spec AS LOG NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

   FIND FIRST notes WHERE notes.rec_key = oe-ordl.rec_key
       NO-LOCK NO-ERROR.

   IF AVAILABLE notes THEN
      v-spec = TRUE.
   ELSE v-spec = FALSE.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN dept-pen-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ext-get-fgitem B-table-Win 
PROCEDURE ext-get-fgitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER opv-fgitem LIKE ITEMfg.i-no NO-UNDO.
opv-fgitem = get-fgitem().

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
  DEFINE BUFFER b-oe-ord FOR oe-ord.

  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-ord-no LIKE oe-ordl.ord-no NO-UNDO.

  RUN set-defaults.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "OEBROWSE"
                        NO-LOCK NO-ERROR.
  IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN sys-ctrl.company = cocode
           sys-ctrl.name    = "OEBROWSE"
           sys-ctrl.descrip = "# of Records to be displayed in oe browser"
           sys-ctrl.log-fld = YES
           sys-ctrl.char-fld = "CE"
           sys-ctrl.int-fld = 30.
  END.

  IF ll-initial THEN DO:
     {&for-eachblank}
     USE-INDEX opened NO-LOCK,
       {&for-each3}
       BREAK BY oe-ordl.ord-no DESC:
       IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
       lv-ord-no = oe-ordl.ord-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
     END. /* each blank */

     &SCOPED-DEFINE joinScop OUTER-JOIN
     &SCOPED-DEFINE open-query                  ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
                AND oe-ordl.ord-no GE lv-ord-no ~
              USE-INDEX opened NO-LOCK,         ~
              {&for-each2}
     &SCOPED-DEFINE joinScop 
     &SCOPED-DEFINE open-query-cad              ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
                AND oe-ordl.ord-no GE lv-ord-no ~
              USE-INDEX opened NO-LOCK,         ~
              {&for-each2}
     {oeinq/j-ordinq1.i}
  END. /* if initial */
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
    DEFINE OUTPUT PARAMETER opHeader_key AS cha NO-UNDO.

    IF AVAILABLE oe-ordl THEN
       FIND FIRST job WHERE job.company = oe-ordl.company
                  AND job.job-no = oe-ordl.job-no
                  AND job.job-no2 = oe-ordl.job-no2 NO-LOCK NO-ERROR.
    opHeader_key = IF AVAILABLE job THEN STRING(job.job) ELSE "".

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
  DEFINE OUTPUT PARAMETER op-est-no AS cha NO-UNDO.


  op-est-no = IF AVAILABLE oe-ordl THEN oe-ordl.est-no ELSE oe-ord.est-no.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  RUN setCellColumns.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      oe-ordl.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
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
      FI_moveCol = "Sort"
      .

  {methods/winReSizeLocInit.i}

  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
  APPLY 'ENTRY':U TO fi_ord-no IN FRAME {&FRAME-NAME}.

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
  IF ll-first THEN RUN first-query.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-all.
  ELSE DO:
    {oeinq/j-ordinq.i}
  END.

  IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").
    RUN dispatch ("row-changed").

    GET LAST {&browse-name}.
    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-last-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-last-show-ord-no = oe-ordl.ord-no.

    GET FIRST {&browse-name}.
    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
      ASSIGN lv-frst-rowid = ROWID({&first-table-in-query-{&browse-name}})
             lv-first-show-ord-no = oe-ordl.ord-no.   
  END.

  IF AVAILABLE oe-ord THEN APPLY "value-changed" TO BROWSE {&browse-name}.
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
  APPLY 'ENTRY':U TO fi_ord-no IN FRAME {&FRAME-NAME}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT  PARAMETER ip-nav-type AS CHARACTER.
  DEFINE OUTPUT PARAMETER op-nav-type AS CHARACTER.


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE one-row-query B-table-Win 
PROCEDURE one-row-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  IF fi_cust-no EQ "" AND
     fi_i-no    EQ "" AND
     fi_part-no EQ "" AND
     fi_po-no1  EQ "" AND
     fi_est-no  EQ "" AND
     fi_job-no  EQ "" THEN DO:
        &SCOPED-DEFINE joinScop OUTER-JOIN
        &SCOPED-DEFINE open-query                  ~
            OPEN QUERY {&browse-name}              ~
              {&for-eachblank}                     ~
                    AND ROWID(oe-ordl) EQ ip-rowid ~
                  NO-LOCK,                         ~
                  {&for-each2}
        &SCOPED-DEFINE joinScop 
        &SCOPED-DEFINE open-query-cad              ~
            OPEN QUERY {&browse-name}              ~
              {&for-eachblank}                     ~
                    AND ROWID(oe-ordl) EQ ip-rowid ~
                  NO-LOCK,                         ~
                  {&for-each2}
  END.
  ELSE DO:
      &SCOPED-DEFINE joinScop OUTER-JOIN
      &SCOPED-DEFINE open-query                  ~
          OPEN QUERY {&browse-name}              ~
            {&for-each1}                         ~
                  AND ROWID(oe-ordl) EQ ip-rowid ~
                NO-LOCK,                         ~
                {&for-each2}
      &SCOPED-DEFINE joinScop 
      &SCOPED-DEFINE open-query-cad              ~
          OPEN QUERY {&browse-name}              ~
            {&for-each1}                         ~
                  AND ROWID(oe-ordl) EQ ip-rowid ~
                NO-LOCK,                         ~
                {&for-each2}
  END.
  {oeinq/j-ordinq1.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.

   DEFINE VARIABLE v-i-no AS CHARACTER NO-UNDO.
   DEFINE VARIABLE v-est-no AS cha NO-UNDO.
   DEFINE VARIABLE v-att AS LOG NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

   {sys/ref/attachlogic.i}

   IF v-est-no <> "" AND v-i-no <> "" THEN
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              (LOOKUP(attach.rec_key,v-rec-key-list) GT 0 AND
              (TRIM(attach.est-no) = trim(v-est-no)) OR 
               (INDEX(v-i-no,attach.i-no) > 0))).
   ELSE
      IF v-est-no <> "" /*AND v-i-no EQ ""*/ THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              LOOKUP(attach.rec_key,v-rec-key-list) GT 0 AND
              trim(attach.est-no) = trim(v-est-no)).
   ELSE
      IF v-est-no EQ "" AND v-i-no <> "" THEN
         v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              index(v-i-no,attach.i-no) > 0).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attach-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN paper-clip-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pushpin-image-proc B-table-Win 
PROCEDURE pushpin-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHARACTER NO-UNDO.

   DEFINE VARIABLE v-att AS LOG NO-UNDO.
   DEFINE VARIABLE lv-ord-no AS CHARACTER NO-UNDO.

   ASSIGN
      lv-ord-no = STRING(oe-ord.ord-no)
      v-att = CAN-FIND(FIRST asi.attach WHERE
              attach.company = cocode AND
              attach.rec_key = ip-rec_key AND
              (attach.est-no EQ lv-ord-no OR ATTACH.est-no EQ "")).

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'attachcust-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN pushpin-image IN WIDGET-HANDLE(char-hdl) (INPUT v-att).

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
     fi_part-no
     fi_cust-no
     fi_ord-no
     fi_po-no1
     fi_est-no
     fi_job-no
     fi_job-no2
     fi_cad-no
     fi_sman
     .

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
 DEFINE VARIABLE lv-tmp-rowid AS ROWID NO-UNDO.
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
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.
  DEFINE BUFFER b-oe-ord  FOR oe-ord.


  FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.
  IF AVAILABLE b-oe-ord THEN DO:
    FIND FIRST b-oe-ordl OF b-oe-ord NO-LOCK.
    ip-rowid = ROWID(b-oe-ordl).
  END.

  DO WITH FRAME {&FRAME-NAME}:
    RUN dispatch ("open-query").
    RUN repo-query (ip-rowid).
  END.

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
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      RUN one-row-query (ip-rowid).
      REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
    END.
    IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ("row-changed").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query1 B-table-Win 
PROCEDURE repo-query1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.
  DEFINE BUFFER b-oe-ord  FOR oe-ord.


  IF AVAILABLE oe-ordl THEN lv-rowid = ROWID(oe-ordl).

  FIND b-oe-ord WHERE ROWID(b-oe-ord) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAILABLE b-oe-ord THEN DO:
    RUN dispatch ("get-first").
    IF ROWID(oe-ord) EQ ip-rowid THEN RUN dispatch ("get-last").
    IF ROWID(oe-ord) EQ ip-rowid THEN RUN dispatch ("open-query").
    IF AVAILABLE oe-ordl THEN RUN repo-query (ROWID(oe-ordl)).

    IF CAN-FIND(FIRST b-oe-ordl OF b-oe-ord
                WHERE ROWID(b-oe-ordl) EQ lv-rowid) THEN
      ip-rowid = lv-rowid.
    ELSE DO:
      FIND FIRST b-oe-ordl OF b-oe-ord NO-LOCK.
      ip-rowid = ROWID(b-oe-ordl).
    END.
  END.

  RUN repo-query (ip-rowid).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reposition-item B-table-Win 
PROCEDURE reposition-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid-hd AS RECID NO-UNDO.
  DEFINE INPUT PARAMETER ip-recid-line AS RECID NO-UNDO.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.

  IF ip-recid-hd <> ? AND ip-recid-line <> ? THEN DO:
     FIND b-oe-ordl WHERE RECID(b-oe-ordl) = ip-recid-line NO-LOCK NO-ERROR.
     IF AVAILABLE b-oe-ordl THEN DO:
        REPOSITION {&browse-name} TO RECID ip-recid-line NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN DO:
          RUN dispatch ('row-changed').
          APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
        END.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE return-current B-table-Win 
PROCEDURE return-current :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oprOrdRec AS RECID NO-UNDO.
DEFINE OUTPUT PARAMETER oprOrdlRec AS RECID NO-UNDO.
IF AVAIL(oe-ord) THEN
  oprOrdRec = RECID(oe-ord).
IF AVAIL(oe-ordl) THEN
  oprOrdlRec = RECID(oe-ordl).

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

  FIND FIRST cust {sys/ref/custW.i} AND
                  cust.cust-no EQ oe-ord.cust-no
                  USE-INDEX cust NO-LOCK NO-ERROR.

  DEFINE VARIABLE char-hdl AS cha NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
  RUN init-history IN WIDGET-HANDLE(char-hdl) (THIS-PROCEDURE).

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
      fi_part-no:SCREEN-VALUE = ""
      fi_ord-no:SCREEN-VALUE  = ""
      fi_po-no1:SCREEN-VALUE  = ""
      fi_est-no:SCREEN-VALUE  = ""
      fi_job-no:SCREEN-VALUE  = ""
      fi_job-no2:SCREEN-VALUE = "" 
      fi_sman:SCREEN-VALUE    = ""
      .
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
  /*{methods/setfocus.i fi_ord-no}*/

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
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-ord-no AS INTEGER NO-UNDO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "OEBROWSE"
                        NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
   CREATE sys-ctrl.
   ASSIGN sys-ctrl.company = cocode
          sys-ctrl.name    = "OEBROWSE"
          sys-ctrl.descrip = "# of Records to be displayed in OE browser"
          sys-ctrl.log-fld = YES
          sys-ctrl.char-fld = "CE"
          sys-ctrl.int-fld = 30.
END.

RUN set-defaults.

IF lv-show-prev THEN DO:

    {&for-eachblank}
               AND oe-ordl.ord-no <= lv-last-show-ord-no  
        USE-INDEX opened NO-LOCK,
        {&for-each3}
        BREAK BY oe-ordl.ord-no DESC:
      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE joinScop OUTER-JOIN
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
                AND oe-ordl.ord-no GE lv-ord-no ~
                AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX opened NO-LOCK,         ~
              {&for-each2}
    &SCOPED-DEFINE joinScop 
    &SCOPED-DEFINE open-query-cad               ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
                AND oe-ordl.ord-no GE lv-ord-no ~
                AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX opened NO-LOCK,         ~
              {&for-each2}
    {oeinq/j-ordinq1.i}

END.  /* lv-show-prev */
ELSE IF lv-show-next THEN DO:

   {&for-eachblank}
    AND oe-ordl.ord-no >= lv-first-show-ord-no  
    USE-INDEX opened NO-LOCK,
    {&for-each3}
    BREAK BY oe-ordl.ord-no :
    IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
    lv-ord-no = oe-ordl.ord-no.
    IF li GE sys-ctrl.int-fld THEN LEAVE.
   END.

   &SCOPED-DEFINE joinScop OUTER-JOIN
   &SCOPED-DEFINE open-query                   ~
       OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                      ~
               AND oe-ordl.ord-no LE lv-ord-no ~
               AND oe-ordl.ord-no GE lv-first-show-ord-no ~
             USE-INDEX opened NO-LOCK,         ~
             {&for-each2}
   &SCOPED-DEFINE joinScop 
   &SCOPED-DEFINE open-query-cad               ~
       OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                      ~
               AND oe-ordl.ord-no LE lv-ord-no ~
               AND oe-ordl.ord-no GE lv-first-show-ord-no ~
             USE-INDEX opened NO-LOCK,         ~
             {&for-each2}
   {oeinq/j-ordinq1.i}
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
   DEFINE VARIABLE v-spec AS LOG NO-UNDO.
   DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.

   DEFINE BUFFER bf2-itemfg FOR itemfg.

   FIND FIRST bf2-itemfg WHERE
        bf2-itemfg.company = oe-ordl.company AND
        bf2-itemfg.i-no EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.

   IF AVAILABLE bf2-itemfg THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE value-changed-proc B-table-Win 
PROCEDURE value-changed-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xlocal-destroy B-table-Win 
PROCEDURE xlocal-destroy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:    Called from local destroy since local destroy is not available
            to edit (defined in methods\browsers\setCellColumns.i   
------------------------------------------------------------------------------*/

  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fget-qty-nothand B-table-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
  (ipBal AS INTEGER,ipHand AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE irtnValue AS INTEGER NO-UNDO.

    irtnValue = (ipHand - ipBal).


  RETURN irtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-bol-qty B-table-Win 
FUNCTION get-act-bol-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE liReturn AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.

  IF AVAILABLE oe-ordl AND VALID-HANDLE(lr-rel-lib) THEN DO:
     FOR EACH oe-rel WHERE
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.LINE AND
         LOOKUP(oe-rel.stat, "P") GT 0
         NO-LOCK:

             RUN get-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT liReturn).
             li = li + liReturn.

     END.
  END.

  RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-rel-qty B-table-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    () :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INTEGER NO-UNDO.
  DEFINE VARIABLE liReturn AS INTEGER NO-UNDO.
  DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.

  IF AVAILABLE oe-ordl AND VALID-HANDLE(lr-rel-lib) THEN DO:
     FOR EACH oe-rel WHERE
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.LINE AND
         LOOKUP(oe-rel.stat, "P,C,Z") EQ 0
         NO-LOCK:

             RUN get-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT liReturn).
             li = li + liReturn.

     END.
  END.

  RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-bal B-table-Win 
FUNCTION get-bal RETURNS INTEGER
  (OUTPUT op-qoh AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iTotalJobOnHandQty AS INTEGER     NO-UNDO.

/*   IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN */

    FOR EACH job-hdr FIELDS(company job-no job-no2 i-no)
        WHERE job-hdr.company EQ cocode
        AND job-hdr.ord-no EQ oe-ordl.ord-no 
        AND job-hdr.i-no EQ oe-ordl.i-no
        USE-INDEX ord-no
        NO-LOCK
        BREAK BY job-hdr.job-no BY job-hdr.job-no2 BY job-hdr.i-no:
        IF LAST-OF(job-hdr.i-no) THEN 
        DO:    
            FOR EACH fg-bin FIELDS (qty)
                WHERE fg-bin.company EQ job-hdr.company
                AND fg-bin.job-no  EQ job-hdr.job-no
                AND fg-bin.job-no2 EQ job-hdr.job-no2
                AND fg-bin.i-no    EQ job-hdr.i-no
                NO-LOCK:
                iTotalJobOnHandQty = iTotalJobOnHandQty + fg-bin.qty.
            END.
        END.
    END.
    op-qoh = iTotalJobOnHandQty.
RETURN iTotalJobOnHandQty.    /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-fgitem B-table-Win 
FUNCTION get-fgitem RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN oe-ordl.i-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pct B-table-Win 
FUNCTION get-pct RETURNS INTEGER
  (ipBal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN DO:

    rtnValue = ((ipBal / oe-ordl.qty) - 1) * 100.
    IF rtnValue EQ 0 THEN rtnValue = 100.
    IF rtnValue EQ -100 THEN rtnValue = 0.
  END.

  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod B-table-Win 
FUNCTION get-prod RETURNS INTEGER
  (OUTPUT op-bal AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTotalProdQty AS INTEGER     NO-UNDO.
DEFINE VARIABLE iJobProdQty AS INTEGER     NO-UNDO.


IF AVAILABLE oe-ordl THEN
DO:
/*      IF oe-ordl.job-no NE "" THEN                          */
/*         FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK   */
/*            WHERE fg-rcpth.company   EQ cocode              */
/*              AND fg-rcpth.job-no    EQ oe-ordl.job-no      */
/*              AND fg-rcpth.job-no2   EQ oe-ordl.job-no2     */
/*              AND fg-rcpth.i-no      EQ oe-ordl.i-no        */
/*              AND fg-rcpth.rita-code EQ "R"                 */
/*            USE-INDEX job,                                  */
/*            EACH fg-rdtlh FIELDS(qty) NO-LOCK               */
/*            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no       */
/*              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code: */
/*              iTotalProdQty = iTotalProdQty + fg-rdtlh.qty. */
/*         END.                                               */
/*      ELSE                                                  */
    FOR EACH job-hdr FIELDS(company job-no job-no2 i-no) 
        WHERE job-hdr.company EQ cocode 
          AND job-hdr.ord-no EQ oe-ordl.ord-no 
          AND job-hdr.i-no EQ oe-ordl.i-no
        USE-INDEX ord-no
/*         NO-LOCK,                                       */
/*         EACH fg-rcpth FIELDS(r-no rita-code)           */
/*         WHERE fg-rcpth.company   EQ cocode             */
/*           AND fg-rcpth.job-no    EQ job-hdr.job-no     */
/*           AND fg-rcpth.job-no2   EQ job-hdr.job-no2    */
/*           AND fg-rcpth.i-no      EQ oe-ordl.i-no       */
/*           AND fg-rcpth.rita-code EQ "R"                */
/*         USE-INDEX job                                  */
/*         NO-LOCK,                                       */
/*         EACH fg-rdtlh FIELDS(qty)                      */
/*         WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no      */
/*           AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code */
        NO-LOCK
        BREAK BY job-hdr.job-no
        BY job-hdr.job-no2:
        IF FIRST-OF(job-hdr.job-no2) THEN DO:
            RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                                   INPUT job-hdr.job-no,
                                   INPUT job-hdr.job-no2,
                                   INPUT job-hdr.i-no,
                                   INPUT NO,
                                   OUTPUT iJobProdQty).
            iTotalProdQty = iTotalProdQty + iJobProdQty.
        END.
    END.

    IF oe-ordl.po-no-po NE 0 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code)
            WHERE fg-rcpth.company   EQ cocode 
              AND fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) 
              AND fg-rcpth.i-no      EQ oe-ordl.i-no 
              AND fg-rcpth.rita-code EQ "R"
            NO-LOCK,
            EACH fg-rdtlh FIELDS(qty) 
            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no 
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            NO-LOCK:
            iTotalProdQty = iTotalProdQty + fg-rdtlh.qty.
        END.
END.

op-bal = iTotalProdQty.
RETURN iTotalProdQty.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-wip B-table-Win 
FUNCTION get-wip RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  DEFINE BUFFER b-oe-ordl FOR oe-ordl.


  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK NO-ERROR.

  rtnValue = oe-ordl.qty - (li-qoh + oe-ordl.ship-qty).
  IF rtnValue LT 0 OR
     rtnValue LT oe-ordl.qty * b-oe-ordl.under-pct / 100 THEN
  rtnValue = 0.
  RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-xfer-qty B-table-Win 
FUNCTION get-xfer-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  

Regardless of Customer Bill to.
1)  Release Status must be Z or C to add the to the Total Transfer Qty.

2) If Order Type = T then the Release S/I status is irrelevant.  (ie.  Customer X)
     Add Actual Qty transferred for the FG item with Status Z or C.

3) If Order Type not = T.  (Any Customer except Customer X)
     Then Release S/I field MUST BE T and Release Status Z or C.
      Add all Actual Qty transferred for the FG item.

------------------------------------------------------------------------------*/
  DEFINE BUFFER buf-oe-rel FOR oe-rel.
  DEFINE BUFFER buf-reftable FOR reftable.
  DEFINE VARIABLE vTransfer-Qty LIKE oe-ordl.ship-qty NO-UNDO INIT 0.

  FOR EACH buf-oe-rel NO-LOCK WHERE 
           buf-oe-rel.company EQ cocode AND
           buf-oe-rel.ord-no  EQ oe-ordl.ord-no AND
           buf-oe-rel.i-no    EQ oe-ordl.i-no   AND
           buf-oe-rel.line    EQ oe-ordl.LINE AND
          (buf-oe-rel.stat = "C" OR buf-oe-rel.stat = "Z"):

      /* If Order Type = T, Skip release when status is NOT Z or C. */
/*       IF oe-ord.TYPE = "T" AND LOOKUP(buf-oe-rel.stat,"Z,C") = 0  THEN NEXT.  */

        IF buf-oe-rel.s-code = "" THEN
          NEXT.
        IF oe-ord.TYPE <> "T" AND buf-oe-rel.s-code <> "T" THEN NEXT.  

      ASSIGN vTransfer-Qty = (vTransfer-Qty + buf-oe-rel.qty).
  END.

  RETURN vTransfer-Qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMI B-table-Win 
FUNCTION getMI RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.

    lc-result = "".
        IF oe-ordl.managed = true THEN
            lc-result = "X".
        RETURN lc-result.   /* Function return value. */


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturned B-table-Win 
FUNCTION getReturned RETURNS DECIMAL
  (ipcValueNeeded AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.

    IF AVAILABLE oe-ordl THEN 
    DO:

        FOR EACH ar-invl WHERE ar-invl.company EQ oe-ordl.company
            AND ar-invl.ord-no EQ oe-ordl.ord-no
            AND ar-invl.i-no EQ oe-ordl.i-no
            NO-LOCK
            BREAK BY ar-invl.inv-no:

            IF FIRST-OF(ar-invl.inv-no) THEN 
            DO:

                FOR EACH oe-reth WHERE oe-reth.company EQ ar-invl.company
                    AND oe-reth.posted EQ TRUE
                    AND oe-reth.applied EQ TRUE 
                    AND oe-reth.cust-no EQ oe-ordl.cust-no
                    AND oe-reth.inv-no EQ ar-invl.inv-no
                    NO-LOCK,
                    EACH oe-retl 
                    WHERE oe-retl.company EQ oe-reth.company
                    AND oe-retl.r-no    EQ oe-reth.r-no
                    AND oe-retl.i-no    EQ ar-invl.i-no
                    NO-LOCK
                    :

                    ASSIGN
                        dTotQtyRet = dTotQtyRet + oe-retl.tot-qty-return        
                        dTotRetInv = dTotRetInv + oe-retl.qty-return-inv.
                END.  /* for each return */
            END. /* if first-of then do */
        END. /* for each ar-invl */
    END. /* avail? */
     IF ipcValueNeeded EQ "TotalReturned" THEN
       dResult = dTotQtyRet.
     ELSE
       dResult = dTotRetInv.


    RETURN dResult.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturnedInv B-table-Win 
FUNCTION getReturnedInv RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.

    dResult = getReturned("ReturnedInv"). 

    RETURN dResult.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getRS B-table-Win 
FUNCTION getRS RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    lc-result = "".
    IF oe-ordl.whsed THEN lc-result = "X".

    RETURN lc-result.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getstat B-table-Win 
FUNCTION getstat RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lc-result AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    
    IF AVAILABLE oe-ord THEN DO: 
        lc-result = oe-ord.stat .
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .
    END.
    RETURN lc-result.   /* Function return value. */
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTotalReturned B-table-Win 
FUNCTION getTotalReturned RETURNS DECIMAL
  (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.


    dResult = getReturned("TotalREturned"). 

    RETURN dResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

