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

  File:  oeinq\b-ordinq.w
  ALERT: DO NOT EDIT THIS WITH APPBUILDER
         There seems to be a limitation on number of browser columns and
         if this file is opened in appbuilder, it will delete the PO column.
         Edit with procedure editor only.
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */
     /* mod 01 Task 10111315  */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
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
{sys/inc/ou6brows.i}

DEF VAR ll-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-sort-by AS CHAR INIT "req-date"  NO-UNDO.
DEF VAR lv-sort-by-lab AS CHAR INIT "Due Date"  NO-UNDO.
DEF VAR ll-sort-asc AS LOG NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR phandle AS HANDLE NO-UNDO.
DEF VAR lv-frst-rowid AS ROWID NO-UNDO.
DEF VAR lv-last-rowid AS ROWID NO-UNDO.
DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR lv-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEF VAR lv-inv-qty LIKE oe-ordl.inv-qty NO-UNDO.
DEF VAR li-prod AS INT NO-UNDO.
DEF VAR li-bal AS INT NO-UNDO.
DEF VAR ld-cost AS DEC FORMAT "->>,>>>,>>9.99<<<<" NO-UNDO.
DEF VAR ld-cost-uom AS CHAR NO-UNDO.
DEF VAR fi_cad-no AS CHAR NO-UNDO.
DEF VAR ll-alloc AS LOG INIT NO NO-UNDO.
DEF VAR ll-browse-first AS LOG INIT YES NO-UNDO.
DEF VAR lv-show-prev AS LOG NO-UNDO.
DEF VAR lv-show-next AS LOG NO-UNDO.
DEF VAR lv-last-show-ord-no AS INT NO-UNDO.
DEF VAR lv-first-show-ord-no AS INT NO-UNDO.
DEF VAR li-act-bol-qty AS INT NO-UNDO.
DEF VAR lv-last-ord-no AS INT NO-UNDO.
DEF VAR lv-first-ord-no AS INT NO-UNDO.
DEF VAR ll-show-all AS LOG NO-UNDO.
DEF VAR li-wip AS INT NO-UNDO.
DEF VAR li-pct AS INT NO-UNDO.
DEF VAR li-qoh AS INT NO-UNDO.
DEF VAR li-act-rel-qty AS INT NO-UNDO.
DEF VAR lc-fgitem AS CHAR NO-UNDO FORMAT 'X(20)'.
DEF VAR lc-ord-po AS CHAR NO-UNDO FORMAT 'X(20)'.
DEF VAR li-ship-qty AS DEC NO-UNDO.
DEF VAR ld-price LIKE oe-ordl.price NO-UNDO.
DEF VAR ld-xfer-qty LIKE oe-ordl.ship-qty NO-UNDO.
DEF VAR ld-uom LIKE oe-ordl.pr-uom NO-UNDO.
DEF VAR lr-rel-lib AS HANDLE NO-UNDO.
DEF VAR ld-t-price LIKE oe-ordl.t-price NO-UNDO.
DEF VAR v-col-move AS LOG INIT TRUE NO-UNDO.
DEF VAR v-print-fmt AS CHAR NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR v-last-shipto LIKE oe-ordl.ship-id NO-UNDO.
DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
DEFINE VARIABLE iHandQtyNoalloc AS INTEGER NO-UNDO .
DEF VAR lActive AS LOG NO-UNDO.

 DO TRANSACTION:
     {sys/ref/CustList.i NEW}
    {sys/inc/custlistform.i ""OQ1"" }
 END.

/* DEF VAR lv-first-cust AS CHAR NO-UNDO.  */
/* DEF VAR lv-last-cust  AS CHAR NO-UNDO.  */

DEF BUFFER b-itemfg FOR itemfg.
DEFINE BUFFER bff-oe-ord FOR oe-ord.

ll-sort-asc = NOT oeinq.

IF ou6brows EQ 'Order Entry' THEN
ASSIGN
  lv-sort-by = "ord-no"
  lv-sort-by-lab = "Order#".

&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode

&SCOPED-DEFINE for-eachblank                        ~
    FOR EACH oe-ordl                                ~
        WHERE {&key-phrase}                         ~
          AND ( (lookup(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") ~
          AND ((tb_open AND oe-ordl.opened  EQ YES) OR (tb_closed AND (oe-ordl.opened EQ NO OR oe-ordl.stat = "C"))) ~
          AND ((tb_open AND oe-ordl.stat    NE "C") OR (tb_closed AND oe-ordl.stat EQ "C") OR oe-ordl.stat EQ "")

&SCOPED-DEFINE for-each1                            ~
    FOR EACH oe-ordl                                ~
        WHERE {&key-phrase}                         ~
          AND ((oe-ordl.opened  EQ YES AND tb_open) OR ((oe-ordl.opened EQ NO OR oe-ordl.stat = "C") AND tb_closed)) ~
          AND ((oe-ordl.stat    NE "C" AND tb_open) OR (oe-ordl.stat EQ "C" AND tb_closed) OR oe-ordl.stat EQ "") ~
          AND ( (lookup(oe-ordl.cust-no,custcount) <> 0 AND oe-ordl.cust-no <> "") OR custcount = "") ~
          AND oe-ordl.cust-no   BEGINS fi_cust-no   ~
          AND (oe-ordl.i-no      BEGINS fi_i-no OR      ~
               (INDEX(fi_i-no,'*') NE 0 AND oe-ordl.i-no    MATCHES fi_i-no)) ~
          AND (oe-ordl.i-name    BEGINS fi_i-name OR ~
              (INDEX(fi_i-name,'*') NE 0 AND oe-ordl.i-name MATCHES fi_i-name)) ~
          AND (oe-ordl.part-no    BEGINS fi_part-no OR ~
              (INDEX(fi_part-no,'*') NE 0 AND oe-ordl.part-no MATCHES fi_part-no)) ~
          AND (oe-ordl.po-no     BEGINS fi_po-no-2 OR  ~
              (INDEX(fi_po-no-2,'*') NE 0 AND oe-ordl.po-no    MATCHES fi_po-no-2)) ~
          AND oe-ordl.est-no    BEGINS fi_est-no    ~
          AND oe-ordl.job-no    BEGINS fi_job-no    ~
          AND (oe-ordl.job-no2  EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ "")

&SCOPED-DEFINE for-each2                              ~
    FIRST oe-ord NO-LOCK                              ~
        WHERE oe-ord.company  EQ oe-ordl.company      ~
          AND oe-ord.ord-no   EQ oe-ordl.ord-no       ~
          AND oe-ord.ord-date  GE fi_ord-date         ~
          AND (oe-ord.type    NE "T" OR NOT ll-alloc) ~
          AND (oe-ord.po-no    BEGINS fi_po-no1 OR    ~
              (INDEX(fi_po-no1,'*') NE 0 AND oe-ord.po-no    MATCHES fi_po-no1)) ~
          AND oe-ord.stat     NE "W" /* gdm - 04080909 */  ~
          AND ((oe-ord.opened EQ YES AND tb_open) OR ((oe-ord.opened EQ NO OR oe-ordl.stat = "C") AND tb_closed)) ~
        USE-INDEX ord-no  

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "ord-no"    THEN STRING(oe-ordl.ord-no,"9999999999")                                                                              ELSE ~
    IF lv-sort-by EQ "stat"      THEN oe-ord.stat                                                                                                      ELSE ~
    IF lv-sort-by EQ "ord-date"  THEN STRING(YEAR(oe-ord.ord-date),"9999") + STRING(MONTH(oe-ord.ord-date),"99") + STRING(DAY(oe-ord.ord-date),"99")   ELSE ~
    IF lv-sort-by EQ "qty"       THEN STRING(9999999999.9999999999 + oe-ordl.qty,"-9999999999.9999999999")                                             ELSE ~
    IF lv-sort-by EQ "cust-no"   THEN oe-ordl.cust-no                                                                                                  ELSE ~
    IF lv-sort-by EQ "cust-name" THEN oe-ord.cust-name                                                                                                 ELSE ~
    IF lv-sort-by EQ "i-no"      THEN oe-ordl.i-no                                                                                                     ELSE ~
    IF lv-sort-by EQ "i-name"    THEN oe-ordl.i-name                                                                                                   ELSE ~
    IF lv-sort-by EQ "part-no"   THEN oe-ordl.part-no                                                                                                  ELSE ~
    IF lv-sort-by-lab EQ "Item PO#"     THEN oe-ordl.po-no                                                                                                    ELSE ~
    IF lv-sort-by-lab EQ "Order PO#"     THEN oe-ord.po-no                                                                                                    ELSE ~
    IF lv-sort-by EQ "est-no"    THEN oe-ordl.est-no                                                                                                   ELSE ~
    IF lv-sort-by EQ "price"     THEN STRING(get-price-disc(),'-9999999999.9999999999')                                                                ELSE ~
    IF lv-sort-by EQ "pr-uom"    THEN get-pr-uom()                                                                                                     ELSE ~
    IF lv-sort-by EQ "t-price"   THEN string(get-extended-price(),'-9999999999.9999999999')                                                            ELSE ~
    IF lv-sort-by EQ "job-no"    THEN STRING(oe-ordl.job-no,"x(6)") + STRING(oe-ordl.job-no2,"99")                                                     ELSE ~
    IF lv-sort-by EQ "e-num"     THEN string(oe-ordl.e-num)                                                                                            ELSE ~
    IF lv-sort-by EQ "v-last-shipto" THEN  get-last-shipto()                                                                                          ELSE ~
    IF lv-sort-by EQ "cost" THEN  STRING(oe-ordl.cost,"-9999999999.99")                                                                               ELSE ~
    IF lv-sort-by EQ "i-name"    THEN oe-ordl.i-name                                                                                                   ELSE ~
                                      STRING(YEAR(oe-ordl.req-date),"9999") + STRING(MONTH(oe-ordl.req-date),"99") + STRING(DAY(oe-ordl.req-date),"99")

&SCOPED-DEFINE sortby BY oe-ordl.ord-no BY oe-ordl.i-no

&SCOPED-DEFINE sortby-phrase-asc  ~
    BY ({&sortby-log})            ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc  ~
    BY ({&sortby-log}) DESC        ~
    {&sortby}


DEFINE TEMP-TABLE tt-ord
    FIELD ord-no LIKE oe-ordl.ord-no
    FIELD cust-no LIKE oe-ordl.cust-no
    FIELD cust-name LIKE oe-ord.cust-name
    FIELD i-no LIKE oe-ordl.i-no
    FIELD part-no LIKE oe-ordl.part-no
    FIELD ord-date LIKE oe-ord.ord-date.


FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "INVPRINT"
      NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl THEN
    ASSIGN
     v-print-fmt  = sys-ctrl.char-fld.

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
&Scoped-define FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no oe-ordl.cust-no ~
oe-ord.cust-name oe-ordl.qty oe-ordl.ship-qty get-xfer-qty () @ ld-xfer-qty oe-ordl.req-date ~
get-price-disc () @ ld-price get-pr-uom() @ ld-uom ~
get-extended-price() @ ld-t-price oe-ordl.i-no oe-ordl.part-no ~
oe-ordl.po-no get-ord-po-no() @ lc-ord-po oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date ~
oe-ord.stat get-ord-qty () @ lv-ord-qty get-ship-qty() @ li-ship-qty ~
oe-ordl.inv-qty get-prod (li-bal) @ li-prod get-bal (li-qoh) @ li-bal ~
get-act-rel-qty() @ li-act-rel-qty get-wip() @ li-wip ~
get-pct(li-bal) @ li-pct get-fgitem() @ lc-fgitem oe-ordl.i-name ~
oe-ordl.line oe-ordl.cost get-cost-uom() @ ld-cost-uom ~
oe-ordl.po-no-po get-cost() @ ld-cost ~
get-last-shipto() @ v-last-shipto ~
get-act-bol-qty() @ li-act-bol-qty  ~
oe-ordl.cust-no oe-ord.cust-name oe-ordl.qty oe-ordl.ship-qty ~
oe-ordl.req-date oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no ~
oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ord.stat oe-ordl.inv-qty ~
oe-ordl.i-name getTotalReturned() @ dTotQtyRet getReturnedInv() @ dTotRetInv ~
fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-ENABLED-TABLE-IN-QUERY-Browser-Table oe-ord
&Scoped-define QUERY-STRING-Browser-Table FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.company eq g_company and ~
oe-ordl.ord-no eq 999999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH oe-ordl WHERE ~{&KEY-PHRASE} ~
      AND oe-ordl.company eq g_company and ~
oe-ordl.ord-no eq 999999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table oe-ordl oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-Browser-Table oe-ord


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_ord-no fi_cust-no fi_i-no fi_part-no ~
fi_po-no1 fi_est-no fi_job-no fi_job-no2 tb_open tb_closed btn_go btn_show ~
btn_prev Browser-Table fi_po-no-2 RECT-1 fi_i-name fi_ord-date btnCalendar-1 
&Scoped-Define DISPLAYED-OBJECTS fiTotal FI_moveCol fi_ord-no fi_cust-no ~
fi_i-no fi_part-no fi_po-no1 fi_est-no fi_job-no fi_job-no2 tb_open ~
tb_closed fi_sort-by fi_po-no-2 fi_i-name fi_ord-date

&Scoped-define btnCalendar-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-cost-uom B-table-Win 
FUNCTION get-cost-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-extended-price B-table-Win 
FUNCTION get-extended-price RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-fgitem B-table-Win 
FUNCTION get-fgitem RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-inv-qty B-table-Win 
FUNCTION get-inv-qty RETURNS INT
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-last-shipto B-table-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ord-po-no B-table-Win 
FUNCTION get-ord-po-no RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ord-qty B-table-Win 
FUNCTION get-ord-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pct B-table-Win 
FUNCTION get-pct RETURNS INTEGER
  (ipBal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-pr-uom B-table-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod B-table-Win 
FUNCTION get-prod RETURNS INTEGER
  ( OUTPUT op-bal AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fget-qty-nothand B-table-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
  (ipBal AS INTEGER,ipHand AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-ship-qty B-table-Win 
FUNCTION get-ship-qty RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getNumTempRecs B-table-Win 
FUNCTION getNumTempRecs RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturned B-table-Win
FUNCTION getReturned RETURNS DECIMAL 
  (ipcValueNeeded AS CHAR  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getReturnedInv B-table-Win 
FUNCTION getReturnedInv RETURNS DECIMAL
  (  ) FORWARD.

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
     SIZE 12 BY 1
     FONT 6.

DEFINE BUTTON btn_next 
     LABEL "Show &Next" 
     SIZE 15 BY 1
     FONT 6.

DEFINE BUTTON btn_prev 
     LABEL "Show &Previous" 
     SIZE 20 BY 1
     FONT 6.

DEFINE BUTTON btn_show 
     LABEL "&Show All" 
     SIZE 12 BY 1
     FONT 6.

DEFINE VARIABLE fiTotal AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Total Records" 
      VIEW-AS TEXT 
     SIZE 10 BY .71 NO-UNDO.

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
     SIZE 17 BY 1
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

DEFINE VARIABLE fi_i-name AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no-2 AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_po-no1 AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_sort-by AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.2 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 5.

DEFINE VARIABLE tb_closed AS LOGICAL INITIAL NO 
     LABEL "Closed" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1
     FGCOLOR 12 FONT 6 NO-UNDO.

DEFINE VARIABLE tb_open AS LOGICAL INITIAL YES 
     LABEL "Open" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi_ord-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001
     LABEL "From Date"
     VIEW-AS FILL-IN 
     SIZE 15 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-ordl
    FIELDS(oe-ordl.ord-no
      oe-ordl.cust-no
      oe-ordl.qty
      oe-ordl.ship-qty
      oe-ordl.req-date
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2
      oe-ordl.inv-qty
      oe-ordl.i-name
      oe-ordl.line
      oe-ordl.po-no-po), 
      oe-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      oe-ordl.ord-no FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      oe-ordl.cust-no COLUMN-LABEL "Customer#" FORMAT "x(8)":U
            LABEL-BGCOLOR 14
      oe-ord.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      oe-ordl.qty COLUMN-LABEL "Order!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 15.4 LABEL-BGCOLOR 14
      oe-ordl.ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>>":U
            LABEL-BGCOLOR 14
      get-xfer-qty () @ ld-xfer-qty COLUMN-LABEL "Transfer!Qty" FORMAT "->>,>>>,>>>":U
      oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      get-price-disc () @ ld-price COLUMN-LABEL "    Sell Price" FORMAT ">>,>>>,>>9.99<<<<":U
            WIDTH 20
      get-pr-uom() @ ld-uom COLUMN-LABEL "UOM" FORMAT "X(4)":U
      get-extended-price() @ ld-t-price COLUMN-LABEL "Extended!Price" FORMAT "->>,>>>,>>9.99":U
            WIDTH 20
      oe-ordl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.part-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.po-no FORMAT "x(15)":U LABEL-BGCOLOR 14
      get-ord-po-no() @ lc-ord-po COLUMN-LABEL "Order PO#" FORMAT "X(15)":U
                 WIDTH 21 LABEL-BGCOLOR 14
      oe-ordl.est-no COLUMN-LABEL "Est #" FORMAT "x(8)":U WIDTH 13.8
            LABEL-BGCOLOR 14
      oe-ordl.job-no COLUMN-LABEL "Job #" FORMAT "x(6)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
            LABEL-BGCOLOR 14
      oe-ord.stat COLUMN-LABEL "Status" FORMAT "x":U LABEL-BGCOLOR 14
      get-ord-qty () @ lv-ord-qty COLUMN-LABEL "Order!Quantity" FORMAT "->>,>>>,>>>":U
            LABEL-BGCOLOR 14
      get-ship-qty() @ li-ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 14
      oe-ordl.inv-qty COLUMN-LABEL "Invoice!Quantity" FORMAT "->>,>>>,>>>":U
            LABEL-BGCOLOR 14
      get-prod (li-bal) @ li-prod COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
            WIDTH 14
      get-bal (li-qoh) @ li-bal COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
            WIDTH 16
      get-act-rel-qty() @ li-act-rel-qty COLUMN-LABEL "Act. Rel.!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 15.4
      get-wip() @ li-wip COLUMN-LABEL "Production!Balance" FORMAT "->>,>>>,>>>":U
            WIDTH 14.2
      get-pct(li-bal) @ li-pct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
      get-fgitem() @ lc-fgitem COLUMN-LABEL "FG Item#" FORMAT "X(15)":U
            WIDTH 21
      oe-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      oe-ordl.line FORMAT ">>99":U
      get-cost() @ ld-cost COLUMN-LABEL "Invoice Line Cost" WIDTH 24 
      get-cost-uom() @ ld-cost-uom COLUMN-LABEL "Cost!UOM"
      oe-ordl.po-no-po FORMAT ">>>>>9":U
      get-last-shipto() @ v-last-shipto COLUMN-LABEL "Last!ShipTo" FORMAT "x(8)":U
      get-act-bol-qty() @ li-act-bol-qty COLUMN-LABEL "Act. BOL!Qty" FORMAT "->>,>>>,>>>":U
            WIDTH 15.4  LABEL-BGCOLOR 14  /* mod 01 Task 10111317  */
      getTotalReturned() @ dTotQtyRet COLUMN-LABEL "Total Qty Returned" FORMAT ">>>,>>9":U
      getReturnedInv() @ dTotRetInv COLUMN-LABEL "Qty Ret. Inventory" FORMAT ">>>,>>9":U
      fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc COLUMN-LABEL "On Hand Qty not Allocated" FORMAT "->>>>>>>>":U
      oe-ordl.cost COLUMN-LABEL "Order Line Cost" WIDTH 23 LABEL-BGCOLOR 14

  ENABLE
      oe-ordl.ord-no
      oe-ordl.cust-no
      oe-ord.cust-name
      oe-ordl.qty
      oe-ordl.ship-qty
      oe-ordl.req-date
      oe-ordl.i-no
      oe-ordl.part-no
      oe-ordl.po-no
      oe-ordl.est-no
      oe-ordl.job-no
      oe-ordl.job-no2
      oe-ord.ord-date
      oe-ord.stat
      oe-ordl.inv-qty
      oe-ordl.i-name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 148 BY 14.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiTotal AT ROW 3.57 COL 141 COLON-ALIGNED WIDGET-ID 12
     FI_moveCol AT ROW 4.57 COL 135.8 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     fi_ord-no AT ROW 2.19 COL 2 NO-LABEL
     fi_cust-no AT ROW 2.19 COL 15 COLON-ALIGNED NO-LABEL
     fi_i-no AT ROW 2.19 COL 30 COLON-ALIGNED NO-LABEL
     fi_part-no AT ROW 2.19 COL 51 COLON-ALIGNED NO-LABEL
     fi_po-no1 AT ROW 2.19 COL 72 COLON-ALIGNED NO-LABEL
     fi_est-no AT ROW 2.19 COL 93 COLON-ALIGNED NO-LABEL
     fi_job-no AT ROW 2.19 COL 110 COLON-ALIGNED NO-LABEL
     fi_job-no2 AT ROW 2.19 COL 127 COLON-ALIGNED NO-LABEL
     tb_open AT ROW 1.24 COL 135
     tb_closed AT ROW 2.43 COL 135
     btn_go AT ROW 4.57 COL 2
     btn_show AT ROW 4.57 COL 49.4
     fi_sort-by AT ROW 4.57 COL 75.8 COLON-ALIGNED NO-LABEL
     btn_prev AT ROW 4.57 COL 14
     btn_next AT ROW 4.57 COL 34.2
     Browser-Table AT ROW 6 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     fi_i-name AT ROW 3.38 COL 30 COLON-ALIGNED NO-LABEL
     fi_po-no-2 AT ROW 3.38 COL 72 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     fi_ord-date AT ROW 3.38 COL 103.5 COLON-ALIGNED 
     btnCalendar-1 AT ROW 3.38 COL 121
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 119
          FGCOLOR 9 FONT 6
     "Sorted By:" VIEW-AS TEXT
          SIZE 12 BY 1 AT ROW 4.57 COL 64.8
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
     "Order / Item PO#" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 75
          FGCOLOR 9 FONT 6
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 96
          FGCOLOR 9 FONT 6
     "Browser Col. Mode:" VIEW-AS TEXT
          SIZE 22.6 BY .62 AT ROW 4.81 COL 114.2 WIDGET-ID 6
          FONT 6
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
/* BROWSE-TAB Browser-Table btn_next F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

ASSIGN 
       oe-ordl.line:VISIBLE IN BROWSE Browser-Table = FALSE
       oe-ordl.po-no-po:VISIBLE IN BROWSE Browser-Table = FALSE.

/* SETTINGS FOR BUTTON btn_next IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiTotal IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fiTotal:READ-ONLY IN FRAME F-Main        = TRUE.

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
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,, FIRST OUTER USED"
     _Where[1]         = "oe-ordl.company eq g_company and
oe-ordl.ord-no eq 999999999"
     _FldNameList[1]   > ASI.oe-ordl.ord-no
"oe-ordl.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ordl.cust-no
"oe-ordl.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ord.cust-name
"oe-ord.cust-name" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-ordl.qty
"oe-ordl.qty" "Order!Quantity" "->>,>>>,>>>" "decimal" ? ? ? 14 ? ? yes ? no no "15.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.oe-ordl.ship-qty
"oe-ordl.ship-qty" "Shipped!Quantity" "->>,>>>,>>>" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no

     _FldNameList[6]   > "_<CALC>"
"get-xfer-qty () @ ld-xfer-qty" "Transfer!Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > ASI.oe-ordl.req-date

     _FldNameList[7]   > ASI.oe-ordl.req-date
"oe-ordl.req-date" "Due Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"get-price-disc () @ ld-price" "    Sell Price" ">>,>>>,>>9.99<<<<" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > "_<CALC>"
"get-pr-uom() @ ld-uom" "UOM" "X(4)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"get-extended-price() @ ld-t-price" "Extended!Price" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" "FG Item#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" "Est #" "x(8)" "character" ? ? ? 14 ? ? yes ? no no "13.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" "Job #" ? "character" ? ? ? 14 ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "" ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ord.ord-date
"oe-ord.ord-date" "Order Date" ? "date" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-ord.stat
"oe-ord.stat" "Status" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > "_<CALC>"
"get-ord-qty () @ lv-ord-qty" "Order!Quantity" "->>,>>>,>>>" ? ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > "_<CALC>"
"get-ship-qty() @ li-ship-qty" "Shipped!Quantity" "->>,>>>,>>>" ? ? ? ? ? ? ? no "Billable Quantity Shipped." no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.oe-ordl.inv-qty
"oe-ordl.inv-qty" "Invoice!Quantity" "->>,>>>,>>>" "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"get-prod (li-bal) @ li-prod" "Prod. Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > "_<CALC>"
"get-bal (li-qoh) @ li-bal" "On Hand Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"get-act-rel-qty() @ li-act-rel-qty" "Act. Rel.!Quantity" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "15.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"get-wip() @ li-wip" "Production!Balance" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "14.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"get-pct(li-bal) @ li-pct" "O/U%" "->>>>>%" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"get-fgitem() @ lc-fgitem" "FG Item#" "X(15)" ? ? ? ? ? ? ? no ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > ASI.oe-ordl.i-name
"oe-ordl.i-name" "Item Name" ? "character" ? ? ? 14 ? ? yes "" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.oe-ordl.line
"oe-ordl.line" ? ">>99" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > "_<CALC>"
"get-cost() @ ld-cost" "Invoice Line Cost" ? ? ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"get-cost-uom() @ ld-cost-uom" "Cost!UOM" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > ASI.oe-ordl.po-no-po
"oe-ordl.po-no-po" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"get-last-shipto() @ v-last-shipto" "Last!ShipTo" "x(8)" ? ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"get-act-bol-qty() @ li-act-bol-qty" "Act. BOL!Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"getTotalReturned() @ dTotQtyRet" "Total Qty Returned" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"getReturnedInv() @ dTotRetInv" "Qty Ret. Inventory" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
    _FldNameList[35]   > "_<CALC>"
"fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc" "On Hand Qty not Allocated" "->>>>>>>>" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > ASI.oe-ordl.cost
"oe-ordl.cost" "Order Line Cost" ? ? ? ? ? 14 ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-DISPLAY OF Browser-Table IN FRAME F-Main
DO:
  IF NOT CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ oe-ord.company
                                   AND sys-ctrl.name EQ 'OECOMM'
                                   AND sys-ctrl.log-fld EQ YES) THEN
  ASSIGN
    ld-cost:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ?
    ld-cost:BGCOLOR = 8
    ld-cost:FGCOLOR = 8
    ld-cost-uom:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = ?
    ld-cost-uom:BGCOLOR = 8
    ld-cost-uom:FGCOLOR = 8.
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

  IF lv-column-nam BEGINS "li" THEN RETURN NO-APPLY.

  ELSE
  IF lv-column-nam BEGINS "job-no" THEN
    ASSIGN
     lv-column-nam = "job-no"
     lv-column-lab = "Job#".

  ELSE
  IF lv-column-nam EQ "lv-ord-no" THEN
    ASSIGN
     lv-column-nam = "ord-no"
     lv-column-lab = "Order#".

  ELSE
  IF lv-column-nam EQ "lv-ord-qty" OR
     lv-column-nam EQ "qty"        THEN
    ASSIGN
     lv-column-nam = "qty"
     lv-column-lab = "Order Quantity".
  ELSE 
   IF lv-column-nam EQ "i-name" OR
      lv-column-nam EQ "Item Name"  THEN
   ASSIGN
      lv-column-nam = "i-name"
      lv-column-lab = "Item Name".  

   ELSE 
   IF lv-column-lab EQ "Order PO#"  THEN
   ASSIGN
      lv-column-nam = "ord-po-no"
      lv-column-lab = "Order PO#".  

  IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.

  ELSE
     ASSIGN
     lv-sort-by     = lv-column-nam
     lv-sort-by-lab = lv-column-lab.

  APPLY 'END-SEARCH' TO {&BROWSE-NAME}.

  RUN yellow-open-query.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}

  RUN set-rec_key.

  IF ll-browse-first THEN DO: /* don't know but focus is not on fi_ord-no without this*/
    APPLY 'ENTRY':U TO fi_ord-no IN FRAME {&FRAME-NAME}.
    ll-browse-first = NO.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
DO:
  DEF VAR v-cust-no AS CHAR NO-UNDO .
  DEF BUFFER bf-oe-ordl  FOR oe-ordl .
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     tb_open
     tb_closed
     fi_cust-no
     fi_i-no
     fi_i-name
     fi_part-no
     fi_cust-no
     fi_ord-no
     fi_po-no1
     fi_po-no-2
     fi_est-no
     fi_job-no
     fi_job-no2
     fi_ord-date.

    ll-first = NO.

    RUN dispatch ("open-query").
    GET FIRST Browser-Table .
     IF NOT AVAIL oe-ord THEN do:
         IF fi_cust-no <> "" THEN DO:
             v-cust-no = fi_cust-no .
         END.
         ELSE do:
             FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company = cocode
                 AND (bf-oe-ordl.cust-no BEGINS fi_cust-no OR fi_cust-no = "")
                 AND (bf-oe-ordl.part-no BEGINS fi_part-no OR fi_part-no = "")
                 AND (bf-oe-ordl.i-no BEGINS fi_i-no OR fi_i-no = "")
                 AND (bf-oe-ordl.ord-no = fi_ord-no OR fi_ord-no = 0)
                 AND (bf-oe-ordl.est-no BEGINS fi_est-no OR fi_est-no = "")
                 AND (bf-oe-ordl.job-no BEGINS fi_job-no OR fi_job-no = "") NO-LOCK NO-ERROR.

             IF AVAIL bf-oe-ordl THEN
                 v-cust-no = bf-oe-ordl.cust-no .
             ELSE v-cust-no = "".
         END.

         FIND FIRST cust WHERE cust.company = cocode 
             AND cust.cust-no = v-cust-no NO-LOCK NO-ERROR.
         IF AVAIL cust AND ou-log AND LOOKUP(cust.cust-no,custcount) = 0 THEN
             MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX WARNING BUTTONS OK.
     END.
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

    lv-show-next = YES.

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

    lv-show-prev = YES.

    ENABLE btn_next .
    APPLY "choose" TO btn_go.
  END.

  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    /*RUN set-defaults.

    ASSIGN
      tb_closed:SCREEN-VALUE  = "yes"*/
      ll-show-all = YES.

    APPLY "choose" TO btn_go.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON HELP OF fi_cust-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-cust2.w (INPUT g_company, INPUT {&SELF-NAME}:screen-value,"", OUTPUT char-val).
          if char-val <> "" then {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-no B-table-Win
ON HELP OF fi_ord-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-ordno2.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value, OUTPUT char-val).
          if char-val <> "" then {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON HELP OF fi_i-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   RUN windows/l-itemfg.w (INPUT g_company,"", INPUT {&SELF-NAME}:SCREEN-VALUE, OUTPUT char-val).
          if char-val <> "" then {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_est-no B-table-Win
ON HELP OF fi_est-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
   DEF BUFFER buff-eb FOR eb.
   RUN windows/l-est2.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value,  OUTPUT char-val).
          if char-val <> "" then
              FIND FIRST buff-eb where recid(buff-eb) eq int(entry(1,char-val)) no-lock no-error. 
          IF AVAIL buff-eb THEN
              ASSIGN {&SELF-NAME}:SCREEN-VALUE = (buff-eb.est-no).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_job-no B-table-Win
ON HELP OF fi_job-no IN FRAME F-Main
DO:
   DEF VAR char-val AS cha NO-UNDO.
    DEF VAR char-rec AS RECID NO-UNDO.
   RUN windows/l-jobno3.w (INPUT g_company,"", INPUT {&SELF-NAME}:screen-value,  OUTPUT char-val , OUTPUT char-rec ).
          if char-val <> "" then {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_i-no B-table-Win
ON VALUE-CHANGED OF fi_i-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
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


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON VALUE-CHANGED OF fi_part-no IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&SELF-NAME}:SCREEN-VALUE = CAPS({&SELF-NAME}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_part-no B-table-Win
ON HELP OF fi_part-no IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR char-rec AS RECID NO-UNDO.
  RUN windows/l-cstprt.w (cocode,fi_cust-no:SCREEN-VALUE,{&SELF-NAME}:SCREEN-VALUE,
                         fi_i-no:SCREEN-VALUE,  OUTPUT char-val, OUTPUT char-rec ) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  if char-val <> "" then {&SELF-NAME}:SCREEN-VALUE = ENTRY(1,char-val).
          return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no-2 B-table-Win
ON VALUE-CHANGED OF fi_po-no-2 IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME fi_ord-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ord-date B-table-Win
ON HELP OF fi_ord-date IN FRAME F-Main /*  Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 B-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i fi_ord-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_po-no1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_po-no1 B-table-Win
ON VALUE-CHANGED OF fi_po-no1 IN FRAME F-Main
DO:
  IF LASTKEY <> 32 THEN {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

&SCOPED-DEFINE cellColumnDat oeinqb-ordinq

{methods/browsers/setCellColumns.i}
RUN sbo/oerel-recalc-act.p PERSISTENT SET lr-rel-lib.

RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OQ1',
                            INPUT YES,
                            OUTPUT lActive).
{sys/inc/chblankcust.i ""OQ1""}

SESSION:DATA-ENTRY-RETURN = YES.


DO WITH FRAME {&FRAME-NAME}:
   /* fi_ord-date = DATE(fi_ord-date:SCREEN-VALUE)   .*/
    
    IF fi_ord-date LT TODAY - 365  THEN
        ASSIGN
           fi_ord-date:SCREEN-VALUE = STRING(TODAY - 365) 
           fi_ord-date = TODAY - 365 .
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-list B-table-Win 
PROCEDURE build-list :
/*------------------------------------------------------------------------------
  Purpose:     Build a temp-table with the current query data.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  EMPTY TEMP-TABLE tt-ord.

  GET FIRST Browser-Table.

  DO WHILE NOT QUERY-OFF-END("Browser-Table"):
    CREATE tt-ord.
    BUFFER-COPY oe-ordl TO tt-ord.
    ASSIGN tt-ord.ord-date = oe-ord.ord-date.
    GET NEXT Browser-Table.
  END.

  ASSIGN fiTotal:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(getNumTempRecs()).

/*   MESSAGE "Query Num Results: " NUM-RESULTS("Browser-Table") SKIP  */
/*           "Temp-table recs: " getNumTempRecs()                     */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK.                           */

  RETURN.

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
  IF ll-first THEN DO:
    RUN set-defaults.
    RUN query-first.
  END.
  ELSE
     RUN query-go.

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

    IF AVAIL oe-ordl THEN
       FIND FIRST job WHERE job.company = oe-ordl.company
                  AND job.job-no = oe-ordl.job-no
                  AND job.job-no2 = oe-ordl.job-no2 NO-LOCK NO-ERROR.
    opHeader_key = IF AVAIL job THEN STRING(job.job) ELSE "".

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
  DEF OUTPUT PARAMETER op-est-no AS cha NO-UNDO.


  op-est-no = IF AVAILABLE oe-ordl THEN oe-ordl.est-no ELSE oe-ord.est-no.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tt-custnum-range B-table-Win 
PROCEDURE get-tt-custnum-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER pcCustNoFrom AS CHAR NO-UNDO INIT "".
 DEFINE OUTPUT PARAMETER pcCustNoTo AS CHAR NO-UNDO INIT "".
 DEFINE BUFFER buf-ord FOR tt-ord.

 FOR EACH buf-ord BREAK BY buf-ord.cust-no:
     IF FIRST(cust-no) THEN
         ASSIGN pcCustNoFrom = buf-ord.cust-no.
     IF LAST(cust-no) THEN
         ASSIGN pcCustNoTo = buf-ord.cust-no.
 END.


 RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tt-date-range B-table-Win 
PROCEDURE get-tt-date-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER pdDateFrom AS DATE NO-UNDO INIT ?.
 DEFINE OUTPUT PARAMETER pdDateTo AS DATE NO-UNDO INIT ?.
 DEFINE BUFFER buf-ord FOR tt-ord.

 FOR EACH buf-ord BREAK BY buf-ord.ord-date:
     IF FIRST(ord-date) THEN
         ASSIGN pdDateFrom = buf-ord.ord-date.
     IF LAST(ord-date) THEN
         ASSIGN pdDateTo = buf-ord.ord-date.
 END.


 RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tt-item-range B-table-Win 
PROCEDURE get-tt-item-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER pcItemFrom AS CHAR NO-UNDO INIT "".
 DEFINE OUTPUT PARAMETER pcItemTo AS CHAR NO-UNDO INIT "".
 DEFINE BUFFER buf-ord FOR tt-ord.

 FOR EACH buf-ord BREAK BY buf-ord.i-no:
     IF FIRST(i-no) THEN
         ASSIGN pcItemFrom = buf-ord.i-no.
     IF LAST(i-no) THEN
         ASSIGN pcItemTo = buf-ord.i-no.
 END.


 RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tt-order-range B-table-Win 
PROCEDURE get-tt-order-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER piOrderFrom AS INT NO-UNDO INIT 0.
 DEFINE OUTPUT PARAMETER piOrderTo AS INT NO-UNDO INIT 0.
 DEFINE BUFFER buf-ord FOR tt-ord.

 FOR EACH buf-ord BREAK BY buf-ord.ord-no:
     IF FIRST(ord-no) THEN
         ASSIGN piOrderFrom = buf-ord.ord-no.
     IF LAST(ord-no) THEN
         ASSIGN piOrderTo = buf-ord.ord-no.
 END.


 RETURN.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-tt-part-range B-table-Win 
PROCEDURE get-tt-part-range :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEFINE OUTPUT PARAMETER pcPartFrom AS CHAR NO-UNDO INIT "".
 DEFINE OUTPUT PARAMETER pcPartTo AS CHAR NO-UNDO INIT "".
 DEFINE BUFFER buf-ord FOR tt-ord.

 FOR EACH buf-ord BREAK BY buf-ord.part-no:
     IF FIRST(part-no) THEN
         ASSIGN pcPartFrom = buf-ord.part-no.
     IF LAST(part-no) THEN
         ASSIGN pcPartTo = buf-ord.part-no.
 END.


 RETURN.


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
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-open AS LOG INIT ? NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN setCellColumns.

  ASSIGN oe-ordl.ord-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.stat:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.ord-date:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.req-date:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.cust-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ord.cust-name:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.i-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.part-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.po-no:READ-ONLY IN BROWSE {&browse-name} = YES
/*       oe-ord.po-no:READ-ONLY IN BROWSE {&browse-name} = YES */
      oe-ordl.est-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.job-no:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.qty:READ-ONLY IN BROWSE {&browse-name} = YES
      oe-ordl.i-name:READ-ONLY IN BROWSE {&browse-name} = YES 
      oe-ordl.ship-qty:READ-ONLY IN BROWSE {&browse-name} = YES.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"inquiry-source",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO WITH FRAME {&FRAME-NAME}:
    RUN get-ip-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid, OUTPUT ll-open).

    FIND itemfg WHERE ROWID(itemfg) EQ lv-rowid NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN DO:
      ASSIGN
       ll-first               = NO
       fi_i-no:SCREEN-VALUE   = itemfg.i-no
       tb_open:SCREEN-VALUE   = STRING(ll-open EQ ? OR ll-open)
       tb_closed:SCREEN-VALUE = STRING(ll-open EQ ? OR NOT ll-open)
       tb_open:SENSITIVE      = NO
       tb_closed:SENSITIVE    = NO.

      APPLY "choose" TO btn_go.
      APPLY "entry" TO fi_i-no.

    END.
    ELSE do:
         FIND bff-oe-ord WHERE ROWID(bff-oe-ord) EQ lv-rowid NO-LOCK NO-ERROR.
         IF AVAIL bff-oe-ord THEN DO:
             ASSIGN
                 ll-first               = NO
                 fi_ord-no:SCREEN-VALUE   = string(bff-oe-ord.ord-no)
                 tb_open:SCREEN-VALUE   = STRING(ll-open EQ ? OR ll-open)
                 tb_closed:SCREEN-VALUE = STRING(ll-open EQ ? OR NOT ll-open)
                 tb_open:SENSITIVE      = NO
                 tb_closed:SENSITIVE    = NO.
             
             APPLY "choose" TO btn_go.
         END.
         APPLY 'ENTRY':U TO fi_ord-no IN FRAME {&FRAME-NAME}.
    END.
  END.
  ELSE APPLY 'ENTRY':U TO fi_ord-no IN FRAME {&FRAME-NAME}.

  FI_moveCol = "Sort".
  DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.

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

  IF ll-show-all THEN DO:

    {oeinq/j-ordinq3.i}
  END.
  ELSE IF lv-show-prev OR lv-show-next THEN RUN show-prev-next.
  ELSE /*first query or go button */ RUN first-query.

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    /*RUN dispatch ('get-last':U).*/
    GET LAST {&browse-name}.

    IF AVAIL oe-ordl THEN DO:

      IF ll-sort-asc = NO THEN
        ASSIGN lv-last-rowid  = ROWID(oe-ordl)
               lv-last-show-ord-no = oe-ordl.ord-no.
      ELSE
        ASSIGN lv-frst-rowid = ROWID(oe-ordl)
               lv-first-show-ord-no = oe-ordl.ord-no. 

    END.
    /*RUN dispatch ('get-first':U).*/
    GET FIRST {&browse-name}.

    IF AVAIL oe-ordl THEN DO:

      IF ll-sort-asc = NO THEN
        ASSIGN lv-frst-rowid  = ROWID(oe-ordl)
               lv-first-show-ord-no = oe-ordl.ord-no.
      ELSE
        ASSIGN lv-last-rowid  = ROWID(oe-ordl)
               lv-last-show-ord-no = oe-ordl.ord-no.
    END.
  END.

  ASSIGN
    lv-show-prev = NO
    lv-show-next = NO
    ll-show-all = NO.

  RUN set-rec_key.

  RUN build-list.
  GET FIRST {&browse-name}.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE paper-clip-image-proc B-table-Win 
PROCEDURE paper-clip-image-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   DEF VAR v-i-no AS CHAR NO-UNDO.
   DEF VAR v-est-no AS cha NO-UNDO.
   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

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
   DEFINE INPUT PARAMETER ip-rec_key AS CHAR NO-UNDO.

   DEF VAR v-att AS LOG NO-UNDO.
   DEF VAR lv-ord-no AS CHAR NO-UNDO.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-first B-table-Win 
PROCEDURE query-first :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ordl.ord-no NO-UNDO.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "OEBROWSE"
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
               sys-ctrl.name    = "OEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in oe browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
  END.


     {&for-eachblank}
        AND oe-ordl.opened EQ YES
        USE-INDEX opened NO-LOCK,
        {&for-each2}
        BREAK BY oe-ordl.ord-no DESC:  
      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                  ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
              AND oe-ordl.opened EQ YES         ~
              AND oe-ordl.ord-no GE lv-ord-no   ~
              USE-INDEX opened NO-LOCK,         ~
              {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-go B-table-Win 
PROCEDURE query-go :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ordl.ord-no NO-UNDO.

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "OEBROWSE"
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
               sys-ctrl.name    = "OEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in oe browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.
  END.

  IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
  IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

  IF fi_ord-no NE 0 THEN DO:

    IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
       fi_part-no EQ "" AND fi_po-no1 EQ "" AND
       fi_est-no EQ "" AND fi_job-no EQ "" AND fi_po-no-2 EQ "" THEN
    DO:
       &SCOPED-DEFINE open-query                ~
        OPEN QUERY {&browse-name}               ~
          {&for-eachblank}                      ~
              AND oe-ordl.ord-no EQ fi_ord-no   ~
              USE-INDEX ord-no NO-LOCK,         ~
              {&for-each2}
    END.
    ELSE
    DO:
       &SCOPED-DEFINE open-query                   ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
                 AND oe-ordl.ord-no EQ fi_ord-no   ~
                 USE-INDEX ord-no NO-LOCK,         ~
                 {&for-each2}
    END.

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_po-no1 NE "" THEN DO:

    {&for-each1}
      USE-INDEX po-no NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query         ~
        OPEN QUERY {&browse-name}        ~
            {&for-each1}                 ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
            USE-INDEX po-no NO-LOCK, ~
            {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_po-no-2 NE "" THEN DO:

    {&for-each1}
      USE-INDEX po-no NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query         ~
        OPEN QUERY {&browse-name}        ~
            {&for-each1}                 ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
            USE-INDEX po-no NO-LOCK, ~
            {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_i-no NE "" THEN DO:

    {&for-each1}
      USE-INDEX ITEM NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query        ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_job-no NE "" THEN DO:

    {&for-each1}
      USE-INDEX job NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query       ~
        OPEN QUERY {&browse-name}      ~
            {&for-each1}               ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
                USE-INDEX job NO-LOCK, ~
                {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_est-no NE "" THEN DO:

    {&for-each1}
      USE-INDEX est NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query       ~
        OPEN QUERY {&browse-name}      ~
            {&for-each1}               ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
                USE-INDEX est NO-LOCK, ~
                {&for-each2}

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_part-no NE "" THEN DO:
     {&for-each1}
      USE-INDEX part NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query         ~
        OPEN QUERY {&browse-name}        ~
            {&for-each1}                 ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
            USE-INDEX part NO-LOCK, ~
            {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_cust-no NE "" THEN DO:
     {&for-each1}
      USE-INDEX cust NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:

      IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
      lv-ord-no = oe-ordl.ord-no.
      IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query         ~
        OPEN QUERY {&browse-name}        ~
            {&for-each1}                 ~
            AND oe-ordl.ord-no GE lv-ord-no   ~
            USE-INDEX cust NO-LOCK, ~
            {&for-each2}

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE DO:

    IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
       fi_part-no EQ "" AND fi_po-no1 EQ "" AND
       fi_est-no EQ "" AND fi_job-no EQ ""  THEN
    DO:
       {&for-eachblank}
         USE-INDEX opened NO-LOCK,
         {&for-each2}
         BREAK BY oe-ordl.ord-no DESC:

         IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
         lv-ord-no = oe-ordl.ord-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query          ~
           OPEN QUERY {&browse-name}         ~
               {&for-eachblank}              ~
               AND oe-ordl.ord-no GE lv-ord-no   ~
                   USE-INDEX opened NO-LOCK, ~
                   {&for-each2}
    END.
    ELSE
    DO:
       {&for-each1}
         USE-INDEX opened NO-LOCK,
         {&for-each2}
         BREAK BY oe-ordl.ord-no DESC:

         IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
         lv-ord-no = oe-ordl.ord-no.
         IF li GE sys-ctrl.int-fld THEN LEAVE.
       END.

       &SCOPED-DEFINE open-query          ~
           OPEN QUERY {&browse-name}         ~
               {&for-each1}                  ~
               AND oe-ordl.ord-no GE lv-ord-no   ~
                   USE-INDEX opened NO-LOCK, ~
                   {&for-each2}
    END.

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  END.

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

  FIND FIRST cust {sys/ref/custW.i} AND
                  cust.cust-no EQ oe-ord.cust-no
                  USE-INDEX cust NO-LOCK NO-ERROR.

  DEF VAR char-hdl AS cha NO-UNDO.
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

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-sort-data B-table-Win 
PROCEDURE send-sort-data :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER pcCustFrom AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER pcCustTo   AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER pcItemFrom AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER pcItemTo   AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER pcPartFrom AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER pcPartTo   AS CHAR NO-UNDO INIT "".
  DEFINE OUTPUT PARAMETER piOrderFrom AS INT NO-UNDO INIT 0.
  DEFINE OUTPUT PARAMETER piOrderTo AS INT NO-UNDO INIT 0.
  DEFINE OUTPUT PARAMETER pdDateFrom AS DATE NO-UNDO INIT ?.
  DEFINE OUTPUT PARAMETER pdDateTo AS DATE NO-UNDO INIT ?.

  RUN get-tt-custnum-range (OUTPUT pcCustFrom, OUTPUT pcCustTo).
  RUN get-tt-item-range (OUTPUT pcItemFrom, OUTPUT pcItemTo).
  RUN get-tt-part-range (OUTPUT pcPartFrom, OUTPUT pcPartTo).
  RUN get-tt-order-range (OUTPUT piOrderFrom, OUTPUT piOrderTo).
  RUN get-tt-date-range (OUTPUT pdDateFrom, OUTPUT pdDateTo).



  RETURN.

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
     fi_part-no:SCREEN-VALUE = ""
     fi_ord-no:SCREEN-VALUE  = ""
     fi_po-no1:SCREEN-VALUE   = ""
     fi_po-no-2:SCREEN-VALUE   = ""
     fi_est-no:SCREEN-VALUE  = ""
     fi_job-no:SCREEN-VALUE  = ""
     fi_job-no2:SCREEN-VALUE = "".
    IF fi_ord-date:SCREEN-VALUE = "" THEN
     fi_ord-date:SCREEN-VALUE  = STRING(TODAY - 365) .
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
/*
  {methods/setfocus.i {&BROWSE-NAME}}

  DO WITH FRAME {&FRAME-NAME}:
    APPLY "tab" TO {&BROWSE-NAME}.
  END.
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-rec_key B-table-Win 
PROCEDURE set-rec_key :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-ordl FOR oe-ordl.

  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR phandle AS HANDLE NO-UNDO.
  DEF BUFFER b-cust FOR cust.

  IF AVAIL oe-ordl THEN DO:
    FIND b-ordl NO-LOCK WHERE ROWID(b-ordl) EQ ROWID(oe-ordl) NO-ERROR.
    {methods/run_link.i "CONTAINER-SOURCE" "Set-Rec-Key_Header"
       "(b-ordl.rec_key,{methods/headers/oe-ordl.i})"}
    {methods/run_link.i "CONTAINER-SOURCE" "Notes-Message"
       "(CAN-FIND(FIRST notes WHERE notes.rec_key = b-ordl.rec_key))"}
    {methods/run_link.i "CONTAINER-SOURCE" "MF-Message"
       "(CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key = b-ordl.rec_key))"}

    RUN paper-clip-image-proc(INPUT b-ordl.rec_key).

    RUN spec-book-image-proc.

    FIND FIRST b-cust WHERE
         b-cust.company EQ oe-ord.company AND
         b-cust.cust-no EQ oe-ord.cust-no
         NO-LOCK NO-ERROR.

     IF AVAIL b-cust THEN
        RUN pushpin-image-proc(INPUT b-cust.rec_key).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE show-prev-next B-table-Win 
PROCEDURE show-prev-next :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-ord-no AS INT NO-UNDO.

  IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
  IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "OEBROWSE"
                        NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN sys-ctrl.company = cocode
               sys-ctrl.name    = "OEBROWSE"
               sys-ctrl.descrip = "# of Records to be displayed in oe browser"
               sys-ctrl.log-fld = YES
               sys-ctrl.char-fld = "CE"
               sys-ctrl.int-fld = 30.

  END.

  IF lv-show-prev THEN DO:

    IF fi_ord-no NE 0 THEN DO:

      IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
         fi_part-no EQ "" AND fi_po-no1 EQ "" AND
         fi_est-no EQ "" AND fi_job-no EQ "" AND fi_po-no-2 EQ "" THEN
      DO:
         &SCOPED-DEFINE open-query                 ~
           OPEN QUERY {&browse-name}               ~
             {&for-eachblank}                      ~
                   AND oe-ordl.ord-no EQ fi_ord-no ~
                   USE-INDEX ord-no NO-LOCK, ~
                 {&for-each2}
      END.
      ELSE
      DO:
         &SCOPED-DEFINE open-query                 ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
                   AND oe-ordl.ord-no EQ fi_ord-no ~
                   USE-INDEX ord-no NO-LOCK, ~
                 {&for-each2}
      END.

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END. /* order # */

    ELSE IF fi_po-no1 NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX po-no NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX po-no NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE IF fi_po-no-2 NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX po-no NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX po-no NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE IF fi_i-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX ITEM NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX ITEM NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE IF fi_job-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX job NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX job NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.

    ELSE IF fi_est-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX est NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX est NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.
    ELSE IF fi_part-no NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX part NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX part NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.
    ELSE IF fi_cust-no NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no LE lv-last-show-ord-no
      USE-INDEX cust NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no DESC:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no GE lv-ord-no ~
              AND oe-ordl.ord-no LE lv-last-show-ord-no ~
              USE-INDEX cust NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE DO:

      IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
         fi_part-no EQ "" AND fi_po-no1 EQ "" AND
         fi_est-no EQ "" AND fi_job-no EQ "" THEN
         DO:
            {&for-eachblank}
            AND oe-ordl.ord-no LE lv-last-show-ord-no
            USE-INDEX opened NO-LOCK,
            {&for-each2}
            BREAK BY oe-ordl.ord-no DESC:
              IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
              lv-ord-no = oe-ordl.ord-no.
              IF li GE sys-ctrl.int-fld THEN LEAVE.
            END.

            &SCOPED-DEFINE open-query               ~
            OPEN QUERY {&browse-name}               ~
              {&for-eachblank}                      ~
                    AND oe-ordl.ord-no GE lv-ord-no ~
                    AND oe-ordl.ord-no LE lv-last-show-ord-no ~
                    USE-INDEX opened NO-LOCK,         ~
                  {&for-each2}
         END.
      ELSE
      DO:
         {&for-each1}
         AND oe-ordl.ord-no LE lv-last-show-ord-no
         USE-INDEX opened NO-LOCK,
         {&for-each2}
         BREAK BY oe-ordl.ord-no DESC:
           IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
           lv-ord-no = oe-ordl.ord-no.
           IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
                 AND oe-ordl.ord-no GE lv-ord-no ~
                 AND oe-ordl.ord-no LE lv-last-show-ord-no ~
                 USE-INDEX opened NO-LOCK,         ~
               {&for-each2}
      END.


      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.

  END. /*lv-show-prev*/
  ELSE DO:

    IF fi_ord-no NE 0 THEN DO:

      IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
         fi_part-no EQ "" AND fi_po-no1 EQ "" AND
         fi_est-no EQ "" AND fi_job-no EQ "" THEN
      DO:
         &SCOPED-DEFINE open-query                 ~
           OPEN QUERY {&browse-name}               ~
             {&for-eachblank}                      ~
                   AND oe-ordl.ord-no EQ fi_ord-no ~
                   USE-INDEX ord-no NO-LOCK, ~
                 {&for-each2}
      END.
      ELSE
      DO:
         &SCOPED-DEFINE open-query                 ~
           OPEN QUERY {&browse-name}               ~
             {&for-each1}                          ~
                   AND oe-ordl.ord-no EQ fi_ord-no ~
                   USE-INDEX ord-no NO-LOCK, ~
                 {&for-each2}
      END.

        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                       ELSE {&open-query} {&sortby-phrase-desc}.

    END. /* order # */

    ELSE IF fi_po-no1 NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX po-no NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX po-no NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE IF fi_i-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX ITEM NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX ITEM NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE IF fi_job-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX job NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.  
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX job NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.

    ELSE IF fi_est-no NE "" THEN DO:
      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX est NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX est NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.
    ELSE IF fi_part-no NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX part NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX part NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.
    ELSE IF fi_cust-no NE "" THEN DO:

      {&for-each1}
      AND oe-ordl.ord-no GE lv-first-show-ord-no
      USE-INDEX cust NO-LOCK,
      {&for-each2}
      BREAK BY oe-ordl.ord-no:
        IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
        lv-ord-no = oe-ordl.ord-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
      END.

      &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no LE lv-ord-no ~
              AND oe-ordl.ord-no GE lv-first-show-ord-no ~
              USE-INDEX cust NO-LOCK,         ~
            {&for-each2}

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.

    END.

    ELSE DO:

      IF fi_cust-no EQ "" AND fi_i-no EQ "" AND
         fi_part-no EQ "" AND fi_po-no1 EQ "" AND
         fi_est-no EQ "" AND fi_job-no EQ "" THEN
      DO:
         {&for-eachblank}
         AND oe-ordl.ord-no GE lv-first-show-ord-no
         USE-INDEX opened NO-LOCK,
         {&for-each2}
         BREAK BY oe-ordl.ord-no:
           IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
           lv-ord-no = oe-ordl.ord-no.
           IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query               ~
         OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                      ~
                 AND oe-ordl.ord-no LE lv-ord-no ~
                 AND oe-ordl.ord-no GE lv-first-show-ord-no ~
                 USE-INDEX opened NO-LOCK,         ~
               {&for-each2}
      END.
      ELSE
      DO:
         {&for-each1}
         AND oe-ordl.ord-no GE lv-first-show-ord-no
         USE-INDEX opened NO-LOCK,
         {&for-each2}
         BREAK BY oe-ordl.ord-no:
           IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
           lv-ord-no = oe-ordl.ord-no.
           IF li GE sys-ctrl.int-fld THEN LEAVE.
         END.

         &SCOPED-DEFINE open-query               ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
                 AND oe-ordl.ord-no LE lv-ord-no ~
                 AND oe-ordl.ord-no GE lv-first-show-ord-no ~
                 USE-INDEX opened NO-LOCK,         ~
               {&for-each2}
      END.

      IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                     ELSE {&open-query} {&sortby-phrase-desc}.
    END.
  END. /*lv-show-next*/

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

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE, 'spec-target':U, OUTPUT char-hdl).

   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
   DO:
      FIND FIRST bf2-itemfg WHERE
           bf2-itemfg.company = oe-ordl.company AND
           bf2-itemfg.i-no EQ oe-ordl.i-no
           NO-LOCK NO-ERROR.

      IF AVAIL bf2-itemfg THEN
         v-spec = CAN-FIND(FIRST notes WHERE
                  notes.rec_key = bf2-itemfg.rec_key AND
                  notes.note_type = "S").

      RUN spec-book-image IN WIDGET-HANDLE(char-hdl) (INPUT v-spec).
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
   DO WITH FRAME {&FRAME-NAME}:
      APPLY "VALUE-CHANGED" TO BROWSE {&browse-name}.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE yellow-open-query B-table-Win 
PROCEDURE yellow-open-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /* Code placed here will execute PRIOR to standard behavior. */

  IF lv-first-show-ord-no LE lv-last-show-ord-no THEN
     ASSIGN lv-first-ord-no = lv-first-show-ord-no
            lv-last-ord-no = lv-last-show-ord-no.
  ELSE
     ASSIGN lv-first-ord-no = lv-last-show-ord-no
            lv-last-ord-no = lv-first-show-ord-no.

  {oeinq/j-ordinq2.i}

  IF AVAIL {&first-table-in-query-{&browse-name}} THEN DO:
    RUN dispatch ("display-fields").

    RUN dispatch ("row-changed").

    GET LAST {&browse-name}.

    IF AVAIL oe-ordl THEN DO:

      IF ll-sort-asc = NO THEN
        ASSIGN lv-last-rowid  = ROWID(oe-ordl)
               lv-last-show-ord-no = oe-ordl.ord-no.
      ELSE
        ASSIGN lv-frst-rowid = ROWID(oe-ordl)
               lv-first-show-ord-no = oe-ordl.ord-no.
    END.

    GET FIRST {&browse-name}.

    IF AVAIL oe-ordl THEN DO:

      IF ll-sort-asc = NO THEN
        ASSIGN lv-frst-rowid  = ROWID(oe-ordl)
               lv-first-show-ord-no = oe-ordl.ord-no.
      ELSE
        ASSIGN lv-last-rowid  = ROWID(oe-ordl)
               lv-last-show-ord-no = oe-ordl.ord-no.
    END.
    IF lv-sort-by NE "ord-no" THEN DO:
      GET FIRST {&browse-name}.
      DO WHILE TRUE:
         IF oe-ordl.ord-no LT lv-first-show-ord-no THEN
             lv-first-show-ord-no = oe-ordl.ord-no.
         IF oe-ordl.ord-no GT lv-last-show-ord-no THEN
             lv-last-show-ord-no = oe-ordl.ord-no.
         GET NEXT {&browse-name}.
         IF NOT AVAILABLE(oe-ordl) THEN
             LEAVE.
      END.
    END.

  END.

  RUN set-rec_key.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-bol-qty B-table-Win 
FUNCTION get-act-bol-qty RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.
  DEF VAR liReturn AS INT NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.

  IF AVAIL oe-ordl AND VALID-HANDLE(lr-rel-lib) THEN DO:
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
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.

  IF AVAIL oe-ordl THEN
     FOR EACH oe-rel WHERE 
         oe-rel.company EQ cocode AND
         oe-rel.ord-no  EQ oe-ordl.ord-no AND
         oe-rel.i-no    EQ oe-ordl.i-no AND
         oe-rel.line    EQ oe-ordl.line
         NO-LOCK:

         RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

         IF INDEX("A,B,P",lv-stat) > 0 THEN
            li = li + oe-rel.qty.
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
        WHERE job-hdr.company EQ oe-ordl.company 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost B-table-Win 
FUNCTION get-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST ar-invl NO-LOCK WHERE ar-invl.company EQ oe-ord.company
                               AND ar-invl.ord-no EQ oe-ord.ord-no
                               AND ar-invl.i-no EQ oe-ordl.i-no NO-ERROR.
  RETURN IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-cost-uom B-table-Win 
FUNCTION get-cost-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  FIND FIRST ar-invl NO-LOCK WHERE ar-invl.company EQ oe-ord.company
                               AND ar-invl.ord-no EQ oe-ord.ord-no
                               AND ar-invl.i-no EQ oe-ordl.i-no NO-ERROR.
  IF AVAILABLE ar-invl THEN
  DO:
     IF ar-invl.dscr[1] EQ "" THEN
        RETURN "M".
     ELSE
        RETURN ar-invl.dscr[1].
  END.
  ELSE
     RETURN "M".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-extended-price B-table-Win 
FUNCTION get-extended-price RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR v-tmp-price AS DEC FORMAT "->,>>>,>>9.9999" NO-UNDO.
  DEF VAR lv-t-price AS DEC NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.t-price.

  IF oeinq-char NE "Order Price" THEN
  FOR EACH ar-invl FIELDS(inv-no amt i-no unit-pr disc) WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ b-oe-ordl.ord-no AND
      ar-invl.i-no EQ b-oe-ordl.i-no
      NO-LOCK
      BY ar-invl.inv-no DESC:

      FIND FIRST itemfg
      {sys/look/itemfgrlW.i}
        AND itemfg.i-no EQ ar-invl.i-no
        NO-LOCK NO-ERROR.

      ASSIGN
         v-tmp-price = IF b-oe-ordl.pr-uom BEGINS "L" AND b-oe-ordl.pr-uom NE "LB" THEN
                       IF b-oe-ordl.qty LT 0 THEN -1 ELSE 1
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "CS" THEN
                          b-oe-ordl.qty / (IF b-oe-ordl.cas-cnt NE 0 THEN b-oe-ordl.cas-cnt ELSE
                                          IF AVAIL itemfg AND itemfg.case-count NE 0
                                                         THEN itemfg.case-count ELSE
                                                              1)
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "C" THEN
                          b-oe-ordl.qty / 100
                       ELSE
                       IF b-oe-ordl.pr-uom EQ "M" THEN
                         b-oe-ordl.qty / 1000
                       ELSE
                         b-oe-ordl.qty

         lv-t-price = v-tmp-price * ar-invl.unit-pr
         ld =  IF v-print-fmt EQ "Dayton" THEN 
                (lv-t-price - ROUND(lv-t-price * ar-invl.disc / 100,2))
              ELSE
                ROUND(lv-t-price * (1 - (ar-invl.disc / 100)),2).

      LEAVE.
  END.

  RETURN ld.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-inv-qty B-table-Win 
FUNCTION get-inv-qty RETURNS INT
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lp-inv-qty AS INT NO-UNDO.

  ASSIGN lp-inv-qty = 0.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  FOR EACH ar-invl  WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ oe-ordl.ord-no AND
      ar-invl.i-no EQ oe-ordl.i-no
      NO-LOCK:

      lp-inv-qty = lp-inv-qty + ar-invl.inv-qty.
  END.

  RETURN lp-inv-qty.

  /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-last-shipto B-table-Win 
FUNCTION get-last-shipto RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  Task 09111201 
Add new field called  "Last Shipto" on  O-Q-1.
This will print the last or next shipto code form the Release Folder.

When multiple release lines exist for the item, the program will look for a release in the following priority:
Release Status C for Completed.
Release Status Z for Invoiced.
Release Status P for posted release that is in the bill of lading file.
Release Status B for back ordered release
Release Status A for Actual Release.
Release Status I for Invoicable / past warehouse terms.
Release Status L for late.
Release Status S for released.

The logic is to print the history of the shipment first back to the release status.
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE BUFFER buf-oe-rel FOR oe-rel.

  FOR EACH buf-oe-rel NO-LOCK WHERE 
    buf-oe-rel.company EQ cocode AND
    buf-oe-rel.ord-no  EQ oe-ordl.ord-no AND
    buf-oe-rel.i-no    EQ oe-ordl.i-no   AND
    buf-oe-rel.line    EQ oe-ordl.LINE
    BY (IF buf-oe-rel.stat = "C" THEN 1 
        ELSE IF buf-oe-rel.stat = "Z" THEN 2 
        ELSE IF buf-oe-rel.stat = "P" THEN 3 
        ELSE IF buf-oe-rel.stat = "B" THEN 4 
        ELSE IF buf-oe-rel.stat = "A" THEN 5 
        ELSE IF buf-oe-rel.stat = "I" THEN 6 
        ELSE IF buf-oe-rel.stat = "L" THEN 7 
        ELSE IF buf-oe-rel.stat = "S" THEN 8         
        ELSE 12)
    :

        RETURN buf-oe-rel.ship-id.        

  END.

  RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ord-po-no B-table-Win 
FUNCTION get-ord-po-no RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN oe-ord.po-no.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ord-qty B-table-Win 
FUNCTION get-ord-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0. 
                           /* Function return value. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-pr-uom B-table-Win 
FUNCTION get-pr-uom RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR lv-uom AS CHAR NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  lv-uom = b-oe-ordl.pr-uom.

  FOR EACH ar-invl FIELDS(inv-no pr-uom) WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ b-oe-ordl.ord-no AND
      ar-invl.i-no EQ b-oe-ordl.i-no
      NO-LOCK
      BY ar-invl.inv-no DESC:

      lv-uom = ar-invl.pr-uom.
      LEAVE.
  END.

  RETURN lv-uom.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-price-disc B-table-Win 
FUNCTION get-price-disc RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF BUFFER b-oe-ordl FOR oe-ordl.

  DEF VAR ld AS DEC NO-UNDO.

  FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-LOCK.

  ld = b-oe-ordl.price * (1 - (b-oe-ordl.disc / 100)).

  FOR EACH ar-invl FIELDS(inv-no unit-pr disc) WHERE
      ar-invl.company EQ cocode AND
      ar-invl.ord-no EQ b-oe-ordl.ord-no AND
      ar-invl.i-no EQ b-oe-ordl.i-no
      NO-LOCK
      BY ar-invl.inv-no DESC:

      ld = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
      LEAVE.
  END.

  RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod B-table-Win 
FUNCTION get-prod RETURNS INTEGER
  ( OUTPUT op-bal AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE iTotalProdQty AS INTEGER     NO-UNDO.
DEFINE VARIABLE iJobProdQty AS INTEGER     NO-UNDO.


IF AVAIL oe-ordl THEN
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fget-qty-nothand B-table-Win 
FUNCTION fget-qty-nothand RETURNS INTEGER
  (ipBal AS INTEGER,ipHand AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE irtnValue AS INTEGER NO-UNDO.

    irtnValue = (ipHand  - ipBal ).


  RETURN irtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-ship-qty B-table-Win 
FUNCTION get-ship-qty RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN oe-ordl.ship-qty.   /* Function return value. */

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

  DEF BUFFER b-oe-ordl FOR oe-ordl.


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

      /* Get reftable.code */


      /* If order type NOT T, skip if S/I code is NOT "T". */
      IF oe-ord.TYPE <> "T" AND buf-oe-rel.s-code <> "T" THEN NEXT.


      ASSIGN vTransfer-Qty = (vTransfer-Qty + buf-oe-rel.qty).
  END.

  RETURN vTransfer-Qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getNumTempRecs B-table-Win 
FUNCTION getNumTempRecs RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE icount AS INT NO-UNDO INIT 0.
  DEFINE BUFFER buf-ord FOR tt-ord.

  FOR EACH buf-ord NO-LOCK:
      ASSIGN iCount = iCount + 1.
  END.


  RETURN iCount.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getReturned B-table-Win
FUNCTION getReturned RETURNS DECIMAL 
  (ipcValueNeeded AS CHAR ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dResult    AS DECIMAL NO-UNDO.

    IF AVAIL oe-ordl THEN 
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

