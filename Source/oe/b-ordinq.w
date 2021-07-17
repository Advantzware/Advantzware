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
{methods/template/brwcustomdef.i}

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
DEFINE VARIABLE cStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lr-rel-lib AS HANDLE NO-UNDO.
DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.
DEFINE VARIABLE iHandQtyNoalloc AS INTEGER NO-UNDO .
DEFINE VARIABLE lActive AS LOG NO-UNDO.
DEFINE VARIABLE lSwitchToWeb AS LOG NO-UNDO.
DEFINE VARIABLE iPreOrder AS INTEGER NO-UNDO .
DEFINE VARIABLE iInvQty AS CHARACTER NO-UNDO.
DEFINE VARIABLE lButtongoPressed AS LOGICAL   NO-UNDO.
DEFINE VARIABLE dSellPrice       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtendedPrice   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cPriceUom        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCostUom         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dInvoiceLineCost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dProdBalance     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOEBrowse        AS INTEGER   NO-UNDO.

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

/* 
  Ticket #93360 Enhance Order entry browse to display current records first, add limit to query to prevent delayed search
  Moved the logic to the new private procedures(pPrepareAndExecuteQuery,pPrepareAndExecuteQueryForPrevNext and 
  pPrepareAndExecuteQueryForShowAll) which will internally calls BrowserProcs.p and creates the query at run time.
  
  IF fiOrderDate is LE 10/01/2018 THen the old query will be use i.e by Order Number ELSE the new query will be used
  i.e fetch the records by rec_key DESC
  
  Use of OEBrowse NK1 has been discarded and replaced with the new NK1 SearchLimits for record Limit and time limit
  
  Procedures Removed:
  - Show-all
  - First-Query    

          
&SCOPED-DEFINE for-eachblank2 ~
    EACH oe-ordl ~
        WHERE {&key-phrase} ~
          and oe-ordl.ord-no eq oe-ord.ord-no ~
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
          AND {system/brMatches.i  oe-ordl.i-name fi_i-name}

&SCOPED-DEFINE for-each11 ~
    FOR EACH oe-ordl ~
        WHERE {&key-phrase} ~
          AND ((LOOKUP(oe-ordl.cust-no,custcount) NE 0 ~
          AND oe-ordl.cust-no NE '') OR custcount EQ '') ~
          AND oe-ordl.cust-no BEGINS fi_cust-no ~
          AND {system/brMatches.i  oe-ordl.i-no fi_i-no}   ~
          AND {system/brMatches.i  oe-ordl.part-no fi_part-no}   ~
          AND {system/brMatches.i  oe-ordl.po-no fi_po-no1}   ~
          AND oe-ordl.est-no BEGINS fi_est-no ~
          AND oe-ordl.s-man[1] BEGINS fi_sman ~
          AND oe-ordl.job-no BEGINS fi_job-no ~
          AND (oe-ordl.job-no2 EQ fi_job-no2 OR fi_job-no2 EQ 0 OR fi_job-no EQ '') ~
          AND {system/brMatches.i  oe-ordl.i-name fi_i-name}

&SCOPED-DEFINE for-each3 FIRST oe-ord OF oe-ordl WHERE ~
   (tb_web EQ NO and oe-ord.stat NE 'W') OR (tb_web AND oe-ord.stat EQ 'W') ~
   USE-INDEX ord-no NO-LOCK

&SCOPED-DEFINE for-each4 for each oe-ord  WHERE ~
    oe-ord.company eq cocode ~
    and oe-ord.stat EQ 'W' ~
   USE-INDEX ord-no NO-LOCK
   
&SCOPED-DEFINE sortby-log ~
    IF lv-sort-by EQ 'ord-no'    THEN STRING(oe-ordl.ord-no,'9999999999') ELSE ~
    IF lv-sort-by EQ 'cStatus'   THEN oe-ord.stat ELSE ~
    IF lv-sort-by EQ 'whsed'     THEN oe-ordl.whsed ELSE ~
    IF lv-sort-by EQ 'managed'   THEN oe-ordl.managed ELSE ~
    IF lv-sort-by EQ 'ord-date'  THEN STRING(YEAR(oe-ord.ord-date),'9999') + STRING(MONTH(oe-ord.ord-date),'99') + STRING(DAY(oe-ord.ord-date),'99') ELSE ~
    IF lv-sort-by EQ 'cust-no'   THEN oe-ordl.cust-no ELSE ~
    IF lv-sort-by EQ 'cust-name' THEN oe-ord.cust-name ELSE ~
    IF lv-sort-by EQ 'i-no'      THEN oe-ordl.i-no ELSE ~
    IF lv-sort-by EQ 'i-name'    THEN oe-ordl.i-name ELSE ~
    IF lv-sort-by EQ 'part-no'   THEN oe-ordl.part-no ELSE ~
    IF lv-sort-by EQ 'po-no'     THEN oe-ordl.po-no ELSE ~
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

ll-initial = browser-log. */


/* Ticket 97427 O-U-1 Review Items -> Below columns have been removed
    get-xfer-qty () @ ld-xfer-qty COLUMN-LABEL "Transfer!Qty" FORMAT "->>,>>>,>>>":U
    get-bal(li-qoh) @ li-bal COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
    get-wip() @ li-wip COLUMN-LABEL "Production!Balance" FORMAT "->>,>>>,>>>":U
            WIDTH 14.4
    get-fgitem() @ lc-fgitem COLUMN-LABEL "FG Item#" FORMAT "x(15)":U
    get-act-bol-qty() @ li-act-bol-qty COLUMN-LABEL "Act. BOL!Qty" FORMAT "->>,>>>,>>>":U
    fget-qty-nothand(get-act-rel-qty() + get-act-bol-qty(),li-qoh) @ iHandQtyNoalloc COLUMN-LABEL "On Hand Qty not Allocated" FORMAT "->>>>>>>>":U
    fnPrevOrder(oe-ordl.est-no,oe-ordl.ord-no) @ iPreOrder COLUMN-LABEL "Prev Order" FORMAT ">>>>>>>>":U
    pGetInvoiceLineCost() @ dInvoiceLineCost COLUMN-LABEL "Invoice Line Cost" FORMAT "->>>,>>>,>>9.99<<<<":U
*/

&SCOPED-DEFINE key-phrase oe-ordl.company EQ cocode AND oe-ordl.opened EQ YES AND oe-ordl.stat NE 'C'

&SCOPED-DEFINE for-eachblank ~
    FOR EACH oe-ordl ~
        WHERE {&key-phrase} ~
          AND ((LOOKUP(oe-ordl.cust-no,custcount) NE 0 ~
          AND oe-ordl.cust-no NE '') OR custcount EQ '')
          
&SCOPED-DEFINE for-each2 ~
    FIRST oe-ord OF oe-ordl ~
     WHERE ((oe-ord.stat NE 'H' AND tbOther EQ YES) ~
       OR (oe-ord.stat EQ 'W' AND tbWeb EQ YES) ~
       OR (oe-ord.stat EQ 'H' AND tbHold EQ YES)) ~
       USE-INDEX ord-no NO-LOCK, ~
    FIRST itemfg ~{&joinScop} NO-LOCK ~
    WHERE itemfg.company EQ oe-ordl.company ~
      AND itemfg.i-no EQ oe-ordl.i-no ~
      AND itemfg.cad-no BEGINS fi_cad-no

DEFINE VARIABLE iRecordLimit       AS INTEGER   NO-UNDO.
DEFINE VARIABLE dQueryTimeLimit    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cFirstRecKey       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLastRecKey        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lEnableShowAll     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lShowAll           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cBrowseWhereClause AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQueryBuffers      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldBuffer       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFieldName         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIsBreakByUsed     AS LOGICAL   NO-UNDO.

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
oe-ordl.whsed oe-ordl.managed getstat() @ cStatus oe-ord.ord-date ~
oe-ordl.req-date oe-ord.cust-name oe-ordl.i-no oe-ordl.part-no oe-ord.po-no ~
oe-ordl.po-no oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 itemfg.cad-no ~
oe-ordl.qty get-prod(li-bal) @ li-prod oe-ordl.ship-qty ~
get-inv-qty() @ iInvQty get-act-rel-qty() @ li-act-rel-qty ~
get-pct(li-bal) @ li-pct oe-ordl.i-name oe-ordl.line oe-ordl.po-no-po ~
oe-ordl.e-num getTotalReturned() @ dTotQtyRet getReturnedInv() @ dTotRetInv ~
oe-ordl.s-man[1] oe-ordl.cost pGetSellPrice() @ dSellPrice ~
pGetExtendedPrice() @ dExtendedPrice pGetPriceUom() @ cPriceUom ~
pGetCostUom() @ cCostUom oe-ord.entered-id itemfg.q-onh ~
fnProdBalance(oe-ordl.qty,get-prod(li-bal)) @ dProdBalance 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table oe-ordl.ord-no ~
oe-ordl.cust-no oe-ord.ord-date oe-ordl.req-date oe-ord.cust-name ~
oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ordl.est-no oe-ordl.job-no ~
oe-ordl.job-no2 
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
&Scoped-Define ENABLED-OBJECTS Browser-Table cbType fiItemPo tbOther tbHold ~
tbWeb fiOrderDate btSHowAll fi_ord-no fi_cust-no fi_i-no fi_part-no ~
fi_po-no1 fi_est-no fi_job-no fi_job-no2 fi_cad-no fi_sman btn_go btn_prev ~
fi_i-name RECT-1 
&Scoped-Define DISPLAYED-OBJECTS cbType fiItemPo tbOther tbHold tbWeb ~
fiOrderDate fi_ord-no fi_cust-no fi_i-no fi_part-no fi_po-no1 fi_est-no ~
fi_job-no fi_job-no2 fi_cad-no fi_sman fi_sort-by fi_i-name 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnPrevOrder B-table-Win 
FUNCTION fnPrevOrder RETURNS INTEGER
  ( ipcEstNo AS CHARACTER, ipiOrdNo AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnProdBalance B-table-Win 
FUNCTION fnProdBalance RETURNS DECIMAL
  ( ipOrderQty AS DECIMAL, ipProdQty AS INTEGER )  FORWARD.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-inv-qty B-table-Win 
FUNCTION get-inv-qty RETURNS INT
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD isFilterBlank B-table-Win 
FUNCTION isFilterBlank RETURNS LOGICAL
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetCostUom B-table-Win 
FUNCTION pGetCostUom RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetExtendedPrice B-table-Win 
FUNCTION pGetExtendedPrice RETURNS DECIMAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetInvoiceLineCost B-table-Win 
FUNCTION pGetInvoiceLineCost RETURNS DECIMAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetPriceUom B-table-Win 
FUNCTION pGetPriceUom RETURNS CHARACTER PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetSellPrice B-table-Win 
FUNCTION pGetSellPrice RETURNS DECIMAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetSortCondition B-table-Win 
FUNCTION pGetSortCondition RETURNS CHARACTER
  (ipcSortBy AS CHARACTER,ipcSortLabel AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pGetWhereCriteria B-table-Win 
FUNCTION pGetWhereCriteria RETURNS CHARACTER
  ( ipcTable AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD pIsValidSearch B-table-Win 
FUNCTION pIsValidSearch RETURNS LOGICAL PRIVATE
  (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
     LABEL "&Go" 
     SIZE 6 BY 1
     FONT 22.

DEFINE BUTTON btn_next 
     LABEL "&Next" 
     SIZE 9 BY 1
     FONT 22.

DEFINE BUTTON btn_prev 
     LABEL "&Prev" 
     SIZE 9 BY 1
     FONT 22.

DEFINE BUTTON btSHowAll 
     LABEL "Show All" 
     SIZE 9.6 BY 1.

DEFINE VARIABLE cbType AS CHARACTER FORMAT "X(256)":U INITIAL "Open" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Open","Closed" 
     DROP-DOWN-LIST
     SIZE 13 BY 1 NO-UNDO.

DEFINE VARIABLE fiItemPo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE fiOrderDate AS DATE FORMAT "99/99/9999":U 
     LABEL "From Date" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 15  NO-UNDO.

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
     SIZE 34.6 BY 1
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
     SIZE 18 BY .95
     BGCOLOR 14 FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 168 BY 3.33.

DEFINE VARIABLE tbHold AS LOGICAL INITIAL yes 
     LABEL "Hold" 
     VIEW-AS TOGGLE-BOX
     SIZE 8 BY .81
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tbOther AS LOGICAL INITIAL yes 
     LABEL "Other" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.2 BY .81
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE tbWeb AS LOGICAL INITIAL yes 
     LABEL "Web" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.6 BY .81
     BGCOLOR 15 FGCOLOR 9  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      oe-ordl, 
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
      oe-ordl.whsed COLUMN-LABEL "R&S" FORMAT "X/":U LABEL-BGCOLOR 14
      oe-ordl.managed COLUMN-LABEL "MI" FORMAT "X/":U LABEL-BGCOLOR 14
      getstat() @ cStatus COLUMN-LABEL "Status" FORMAT "X(16)":U
      oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
            WIDTH 14.4 LABEL-BGCOLOR 14
      oe-ordl.req-date COLUMN-LABEL "Due Date" FORMAT "99/99/9999":U
            WIDTH 14.2 LABEL-BGCOLOR 14
      oe-ord.cust-name FORMAT "x(30)":U LABEL-BGCOLOR 14
      oe-ordl.i-no COLUMN-LABEL "FG Item#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.part-no COLUMN-LABEL "Cust Part#" FORMAT "x(15)":U
            LABEL-BGCOLOR 14
      oe-ord.po-no COLUMN-LABEL "Order PO#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.po-no COLUMN-LABEL "Item PO#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.est-no COLUMN-LABEL "Est#" FORMAT "x(8)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no COLUMN-LABEL "Job#" FORMAT "x(6)":U WIDTH 12
            LABEL-BGCOLOR 14
      oe-ordl.job-no2 COLUMN-LABEL "" FORMAT ">9":U LABEL-BGCOLOR 14
      itemfg.cad-no COLUMN-LABEL "CAD#" FORMAT "x(15)":U LABEL-BGCOLOR 14
      oe-ordl.qty COLUMN-LABEL "Ordered Qty" FORMAT "->>,>>>,>>>":U
      get-prod(li-bal) @ li-prod COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>>":U
      oe-ordl.ship-qty COLUMN-LABEL "Shipped Qty" FORMAT "->>,>>>,>>>":U
      get-inv-qty() @ iInvQty COLUMN-LABEL "Invoice Qty" FORMAT "->>,>>>,>>>":U
      get-act-rel-qty() @ li-act-rel-qty COLUMN-LABEL "Act. Rel.!Quantity" FORMAT "->>,>>>,>>>":U
            WIDTH 12.4
      get-pct(li-bal) @ li-pct COLUMN-LABEL "O/U%" FORMAT "->>>>>%":U
      oe-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
            LABEL-BGCOLOR 14
      oe-ordl.line FORMAT ">>99":U
      oe-ordl.po-no-po FORMAT ">>>>>9":U
      oe-ordl.e-num FORMAT ">>>>>9":U LABEL-BGCOLOR 14
      getTotalReturned() @ dTotQtyRet COLUMN-LABEL "Tot Returned" FORMAT ">>>,>>9":U
      getReturnedInv() @ dTotRetInv COLUMN-LABEL "Qty Returned Inv" FORMAT ">>>,>>9":U
      oe-ordl.s-man[1] COLUMN-LABEL "Rep" FORMAT "x(3)":U LABEL-BGCOLOR 14
      oe-ordl.cost COLUMN-LABEL "Order Line Cost" FORMAT "->>>,>>>,>>9.99":U
            LABEL-BGCOLOR 14
      pGetSellPrice() @ dSellPrice COLUMN-LABEL "Sell Price" FORMAT ">>,>>>,>>9.99<<<<":U
      pGetExtendedPrice() @ dExtendedPrice COLUMN-LABEL "Extended! Price" FORMAT "->>,>>>,>>9.99":U
      pGetPriceUom() @ cPriceUom COLUMN-LABEL "UOM" FORMAT "X(4)":U
      pGetCostUom() @ cCostUom COLUMN-LABEL "Cost!Uom" FORMAT "x(4)":U
      oe-ord.entered-id COLUMN-LABEL "Entered By" FORMAT "x(8)":U
      itemfg.q-onh COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>>":U
            WIDTH 16
      fnProdBalance(oe-ordl.qty,get-prod(li-bal)) @ dProdBalance COLUMN-LABEL "Prod. Balance" FORMAT "->>,>>>,>>9.9<<<":U
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
    WITH NO-ASSIGN SEPARATORS SIZE 181 BY 16.52
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 4.33 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     cbType AT ROW 1.95 COL 146.2 COLON-ALIGNED NO-LABEL WIDGET-ID 32
     fiItemPo AT ROW 3.14 COL 68.2 COLON-ALIGNED NO-LABEL WIDGET-ID 36
     tbOther AT ROW 3.14 COL 154.6 WIDGET-ID 28
     tbHold AT ROW 3.14 COL 146.4 WIDGET-ID 26
     tbWeb AT ROW 3.14 COL 138.6 WIDGET-ID 24
     fiOrderDate AT ROW 3.14 COL 109.6 COLON-ALIGNED WIDGET-ID 20
     btSHowAll AT ROW 3.14 COL 25.6 WIDGET-ID 16
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
     btn_prev AT ROW 3.14 COL 7.8
     btn_next AT ROW 3.14 COL 16.6
     fi_sort-by AT ROW 3.14 COL 107.4 COLON-ALIGNED NO-LABEL
     fi_i-name AT ROW 3.14 COL 33.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     "Job#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 104
          FGCOLOR 9 FONT 22
     "CAD#" VIEW-AS TEXT
          SIZE 8 BY .71 AT ROW 1.24 COL 119
          FGCOLOR 9 FONT 22
     "Order#" VIEW-AS TEXT
          SIZE 10 BY .71 AT ROW 1.24 COL 2
          FGCOLOR 9 FONT 22
     "Customer#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 16
          FGCOLOR 9 FONT 22
     "FG Item#/Name" VIEW-AS TEXT
          SIZE 19 BY .71 AT ROW 1.24 COL 30
          FGCOLOR 9 FONT 22
     "Cust Part#" VIEW-AS TEXT
          SIZE 13 BY .71 AT ROW 1.24 COL 50
          FGCOLOR 9 FONT 22
     "Open" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.24 COL 150.6 WIDGET-ID 34
          FGCOLOR 9 FONT 22
     "REP#" VIEW-AS TEXT
          SIZE 6.6 BY .71 AT ROW 1.24 COL 140.2 WIDGET-ID 12
          FGCOLOR 9 FONT 22
     "Order/Item PO" VIEW-AS TEXT
          SIZE 18 BY .71 AT ROW 1.24 COL 70
          FGCOLOR 9 FONT 22
     "Estimate#" VIEW-AS TEXT
          SIZE 12 BY .71 AT ROW 1.24 COL 90
          FGCOLOR 9 FONT 22
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
         WIDTH              = 181.8.
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
       Browser-Table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 2.

ASSIGN 
       oe-ordl.line:VISIBLE IN BROWSE Browser-Table = FALSE
       oe-ordl.po-no-po:VISIBLE IN BROWSE Browser-Table = FALSE.

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
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl,ASI.itemfg OF ASI.oe-ordl"
     _Options          = "NO-LOCK SORTBY-PHRASE"
     _TblOptList       = ",, FIRST OUTER"
     _Where[1]         = "oe-ordl.company EQ g_company
AND oe-ordl.ord-no EQ 999999"
     _Where[3]         = "itemfg.company EQ oe-ordl.company
AND itemfg.i-no EQ oe-ordl.i-no"
     _FldNameList[1]   > ASI.oe-ordl.ord-no
"oe-ordl.ord-no" ? ? "integer" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ordl.cust-no
"oe-ordl.cust-no" "Customer#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ordl.whsed
"oe-ordl.whsed" "R&S" "X/" "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-ordl.managed
"oe-ordl.managed" "MI" "X/" "logical" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "FILL-IN" "," ? ? 5 no 0 no no
     _FldNameList[5]   > "_<CALC>"
"getstat() @ cStatus" "Status" "X(16)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
     _FldNameList[11]   > ASI.oe-ord.po-no
"oe-ord.po-no" "Order PO#" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" "Item PO#" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
"get-inv-qty() @ iInvQty" "Invoice Qty" "->>,>>>,>>>" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > "_<CALC>"
"get-act-rel-qty() @ li-act-rel-qty" "Act. Rel.!Quantity" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "12.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > "_<CALC>"
"get-pct(li-bal) @ li-pct" "O/U%" "->>>>>%" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.oe-ordl.i-name
"oe-ordl.i-name" "Item Name" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > ASI.oe-ordl.line
"oe-ordl.line" ? ">>99" "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > ASI.oe-ordl.po-no-po
"oe-ordl.po-no-po" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > ASI.oe-ordl.e-num
"oe-ordl.e-num" ? ? "integer" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"getTotalReturned() @ dTotQtyRet" "Tot Returned" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"getReturnedInv() @ dTotRetInv" "Qty Returned Inv" ">>>,>>9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.oe-ordl.s-man[1]
"oe-ordl.s-man[1]" "Rep" ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   > ASI.oe-ordl.cost
"oe-ordl.cost" "Order Line Cost" ? "decimal" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[31]   > "_<CALC>"
"pGetSellPrice() @ dSellPrice" "Sell Price" ">>,>>>,>>9.99<<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"pGetExtendedPrice() @ dExtendedPrice" "Extended! Price" "->>,>>>,>>9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > "_<CALC>"
"pGetPriceUom() @ cPriceUom" "UOM" "X(4)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"pGetCostUom() @ cCostUom" "Cost!Uom" "x(4)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > ASI.oe-ord.entered-id
"oe-ord.entered-id" "Entered By" "x(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > ASI.itemfg.q-onh
"itemfg.q-onh" "On Hand Qty" "->>,>>>,>>>" ? ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"fnProdBalance(oe-ordl.qty,get-prod(li-bal)) @ dProdBalance" "Prod. Balance" "->>,>>>,>>9.9<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}    
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
{methods/template/sortindicator.i}
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

  {methods/template/sortindicatorend.i}
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
      fi_ord-no
      fi_po-no1
      fi_est-no
      fi_job-no
      fi_job-no2
      fi_cad-no
      fi_sman
      tbWeb
      tbHold
      tbOther
      cbType
      fiOrderDate
      lButtongoPressed = YES
      .
    IF NOT pIsValidSearch() THEN 
        RETURN.
        
    IF tbWeb:CHECKED THEN     
      RUN util/fixcXMLDuplicates.p.  
      
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
ON CHOOSE OF btn_prev IN FRAME F-Main /* Prev */
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


&Scoped-define SELF-NAME btSHowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSHowAll B-table-Win
ON CHOOSE OF btSHowAll IN FRAME F-Main /* Show All */
DO:          
    lShowAll = YES.
    APPLY "choose" TO btn_go.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cbType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cbType B-table-Win
ON VALUE-CHANGED OF cbType IN FRAME F-Main
DO:
    ASSIGN {&SELF-NAME}.
    IF cbType EQ "Open" THEN DO:
        ASSIGN 
            tbOther:HIDDEN  = NO
            tbWeb:HIDDEN    = NO
            tbHold:HIDDEN   = NO
            tbOther:CHECKED = YES
            tbWeb:CHECKED   = YES 
            tbHold:CHECKED  = YES
            .
    END.
    ELSE 
        ASSIGN 
            tbOther:HIDDEN  = YES
            tbWeb:HIDDEN    = YES
            tbHold:HIDDEN   = YES
            .      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cad-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cad-no B-table-Win
ON HELP OF fi_cad-no IN FRAME F-Main
DO:
    DEFINE VARIABLE char-val AS cha NO-UNDO.
    RUN windows/l-itemfc.w  (g_company,fi_cad-no:screen-value, OUTPUT char-val). 
    IF char-val <> "" THEN 
        {&SELF-NAME}:screen-value = ENTRY(1,char-val).

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

&Scoped-define SELF-NAME fi_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON LEAVE OF fi_cust-no IN FRAME F-Main
DO:
    {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust-no B-table-Win
ON VALUE-CHANGED OF fi_cust-no IN FRAME F-Main
DO:
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
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.
  
    RUN system/openLookup.p (
        INPUT  g_company, 
        INPUT  "",  /* Lookup ID */
        INPUT  29,  /* Subject ID */
        INPUT  "",  /* User ID */
        INPUT  0,   /* Param Value ID */
        OUTPUT cFieldsValue, 
        OUTPUT cFoundValue, 
        OUTPUT recFoundRecID
        ).
    IF cFoundValue NE "" THEN 
        {&SELF-NAME}:screen-value = cFoundValue.
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
&SCOPED-DEFINE cellColumnDat b-ordinq

{methods/ctrl-a_browser.i}
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

&Scoped-define sdQuery {&BROWSE-NAME}
{methods/pSessionAuditKey.i}
{methods/winReSize.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sort-by:HIDDEN  = TRUE.
fi_sort-by:VISIBLE = FALSE.

RUN sys/ref/nk1look.p (INPUT cocode,"OEBrowse", "DT" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/, 
                       OUTPUT cRtnChar, OUTPUT lRecFound).
    IF lRecFound AND DATE(cRtnChar) NE ? THEN 
        ASSIGN
        fiOrderDate = DATE(cRtnChar) NO-ERROR.
    ELSE
        fiOrderDate = TODAY - 365.
        
RUN sys/ref/nk1look.p (INPUT cocode,"OEBrowse", "I" /* Logical */, NO /* check by cust */, 
                       INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/, 
                       OUTPUT cRtnChar, OUTPUT lRecFound).
    IF lRecFound THEN 
        ASSIGN
        iOEBrowse = INTEGER(cRtnChar) NO-ERROR.        
        
IF lEnableShowAll THEN 
    btSHowAll:SENSITIVE = lEnableShowAll.

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
  DEFINE VARIABLE cScreenType AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  {methods/run_link.i "container-source" "GetScreenType" "(Output cScreenType)"}
  
  RUN Browser_GetRecordAndTimeLimit(
    INPUT  cocode,
    INPUT  cScreenType,
    OUTPUT iRecordLimit,
    OUTPUT dQueryTimeLimit,
    OUTPUT lEnableShowAll
    ).
   IF cScreentype EQ "OW" OR cScreenType EQ "OC" THEN 
       RUN pSetDefaults(
           INPUT cScreenType
           ).
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
      oe-ordl.job-no2:READ-ONLY IN BROWSE {&browse-name} = YES.
    /*  FI_moveCol = "Sort"
      .*/
      oe-ordl.cust-no:WIDTH IN BROWSE {&BROWSE-NAME} = 20 .
      oe-ordl.po-no:WIDTH IN BROWSE {&BROWSE-NAME}   = 30 .
      oe-ordl.job-no:WIDTH IN BROWSE {&BROWSE-NAME}  = 12 .
  {methods/winReSizeLocInit.i}

 // DISPLAY FI_moveCol WITH FRAME {&FRAME-NAME}.
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
    IF fi_est-no NE "" THEN 
        fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
    IF fi_job-no NE "" THEN 
        fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).                 

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF ll-first THEN 
        RUN pQueryFirst.
    ELSE IF lShowAll THEN 
        RUN pPrepareAndExecuteQueryForShowAll.
   
    ELSE IF lv-show-prev OR lv-show-next THEN DO:
                              
        RUN pPrepareAndExecuteQueryForPrevNext(
            IF lv-show-prev THEN lv-last-show-ord-no ELSE lv-first-show-ord-no,
            INPUT lv-show-prev
            ).
    END.        
    ELSE 
        RUN pPrepareAndExecuteQuery(
            INPUT IF lButtongoPressed THEN NO ELSE YES /* If Button go is pressed then only show the limit alert */ 
            ).

    IF lButtongoPressed THEN 
        lButtongoPressed = NO.        

    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN 
    DO:
        RUN dispatch ("display-fields").
        RUN dispatch ("row-changed").

        GET LAST {&browse-name}.
        IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
            ASSIGN lv-last-rowid       = ROWID({&first-table-in-query-{&browse-name}})
                lv-last-show-ord-no = oe-ordl.ord-no.    
        IF AVAILABLE oe-ord THEN 
            cLastRecKey = oe-ord.rec_key.

        GET FIRST {&browse-name}.
        IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN
            ASSIGN lv-frst-rowid        = ROWID({&first-table-in-query-{&browse-name}})
                lv-first-show-ord-no = oe-ordl.ord-no.
          
        IF AVAILABLE oe-ord THEN          
            cFirstRecKey = oe-ord.rec_key
                .   
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
     v-col-move = NOT v-col-move.
   /*  FI_moveCol = IF v-col-move = NO THEN "Move" ELSE "Sort".
  DISPLAY FI_moveCol.*/
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

    DEFINE VARIABLE cQuery    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse AS CHARACTER NO-UNDO.
    
    IF fiOrderDate LE  10/01/2018 AND lv-sort-by EQ "rec_key" THEN 
        lv-sort-by = "ord-no".
        
    cQuery = "FOR EACH oe-ordl NO-LOCK"
             + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
             + " AND ROWID(oe-ordl)    EQ " + "TO-ROWID(" + "'" + STRING(ip-rowid) + "')"
             + pGetWhereCriteria("oe-ordl")
             + ",FIRST oe-ord OF oe-ordl NO-LOCK"
             + " WHERE " + pGetWhereCriteria("oe-ord")
             + ",FIRST itemfg " + (IF fi_cad-no EQ "" THEN "OUTER-JOIN" ELSE "") + " NO-LOCK"
             + " WHERE itemfg.company EQ oe-ordl.company"
             + "   AND itemfg.i-no    EQ oe-ordl.i-no"
             + ( IF fi_cad-no NE "" THEN " AND itemfg.cad-no BEGINS " + QUOTER(fi_cad-no) ELSE "")
             + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY oe-ordl.ord-no BY oe-ordl.i-no"
             .
                 
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cQuery,                      /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse
        ).               
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAssignCommonRecords B-table-Win 
PROCEDURE pAssignCommonRecords PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Assign Common Records for the query
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcQueryBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldBuffer      AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcFieldName        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplIsBreakByUsed    AS LOGICAL   NO-UNDO.
   
    /* Use below logic to sort records by rec-key */
    
    /*ASSIGN   
        iopcQueryBuffer   = "oe-ord,oe-ordl"
        iopcFieldBuffer   = "oe-ord"
        iopcFieldName     = "rec_key"
        ioplIsBreakByUsed = NO  
        . */
      
    ASSIGN         
        iopcQueryBuffer   = "oe-ordl,oe-ord"
        iopcFieldBuffer   = "oe-ordl"
        iopcFieldName     = "ord-no" 
        ioplIsBreakByUsed = YES 
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQuery B-table-Win 
PROCEDURE pPrepareAndExecuteQuery :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to prepare and execute query in browse
 Notes:
------------------------------------------------------------------------------*/   
    DEFINE INPUT PARAMETER iplInitialLoad    AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cLimitingQuery        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderTypeWhereClause AS CHARACTER NO-UNDO.
    
    RUN pAssignCommonRecords(
        INPUT-OUTPUT cQueryBuffers,
        INPUT-OUTPUT cFieldBuffer,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT lIsBreakByUsed
        ).
          
    cLimitingQuery = "FOR EACH oe-ordl NO-LOCK"
                     + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                     + pGetWhereCriteria("oe-ordl") 
                     + ", FIRST oe-ord OF oe-ordl NO-LOCK"
                     +  " WHERE " + pGetWhereCriteria("oe-ord")
                     + " BREAK BY oe-ordl.ord-no DESC"
                     .  
     /* Use below query to sort records by rec_key */
                              
    /* cLimitingQuery = "For EACH oe-ord NO-LOCK"
                     + " WHERE oe-ord.company EQ " + QUOTER(cocode)
                     + " AND " + pGetWhereCriteria("oe-ord")                  
                     + ",FIRST oe-ordl of oe-ord NO-LOCK "
                     + " WHERE " + pGetWhereCriteria("oe-ordl")
                     + " BY oe-ord.rec_key DESC"
                     .      */                       
         
    /* Limit the query if order no is 0 or cadd No is Blank */                    
    IF fi_ord-no EQ 0 AND fi_cad-no EQ "" THEN             
        RUN Browse_PrepareAndExecuteLimitingQuery(
            INPUT  cLimitingQuery,   /* Query */
            INPUT  cQueryBuffers,    /* Buffers Name */
            INPUT  iRecordLimit,     /* Record Limit */
            INPUT  dQueryTimeLimit,  /* Time Limit*/
            INPUT  lEnableShowAll,   /* Enable ShowAll Button? */
            INPUT  cFieldBuffer,     /* Buffer name to fetch the field's value*/
            INPUT  cFieldName,       /* Field Name*/
            INPUT  iplInitialLoad,   /* Initial Query*/
            INPUT  lIsBreakByUsed,   /* Is breakby used */
            OUTPUT cResponse           
            ).       
  
    ELSE 
        cResponse = "OrderNo". /* For identification purpose */
        
    IF cResponse EQ "" AND lButtongoPressed THEN  
        MESSAGE "No Records Found..."
            VIEW-AS ALERT-BOX ERROR.
              
    ELSE IF cResponse EQ "ShowALL" THEN 
        RUN pPrepareAndExecuteQueryForShowAll.
            
    ELSE DO:
        /* Use below logic to sort records by rec_key */
        /* cBrowseWhereClause = (IF fi_ord-no EQ 0 AND fi_cad-no EQ "" THEN " AND oe-ordl.rec_key GE " +  QUOTER(cResponse) ELSE "" ).  */ 
        
        cBrowseWhereClause = (IF fi_ord-no EQ 0 AND fi_cad-no EQ "" THEN " AND oe-ordl.ord-no  GE " + STRING(INTEGER(cResponse)) ELSE "").        
               
        cBrowseQuery = "FOR EACH oe-ordl NO-LOCK"
                       + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                       + cBrowseWhereClause   
                       + pGetWhereCriteria("oe-ordl")             
                       + ", FIRST oe-ord OF oe-ordl NO-LOCK"
                       + " WHERE " + pGetWhereCriteria("oe-ord")
                       + ",FIRST itemfg " + (IF fi_cad-no EQ "" THEN "OUTER-JOIN" ELSE "") + " NO-LOCK"
                       + " WHERE itemfg.company EQ oe-ordl.company"
                       + "   AND itemfg.i-no    EQ oe-ordl.i-no"
                       + ( IF fi_cad-no NE "" THEN " AND itemfg.cad-no BEGINS " + QUOTER(fi_cad-no) ELSE "")
                       + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY oe-ordl.ord-no BY oe-ordl.i-no"
                       .                                                               
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
            INPUT  cBrowseQuery,                /* BRowse Query */             
            INPUT  NO,                          /* Show limit alert? */        
            INPUT  0,                           /* Record limit */             
            INPUT  0,                           /* Time Limit */               
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
            OUTPUT cResponse                                                   
            ).
    END.                                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForPrevNext B-table-Win 
PROCEDURE pPrepareAndExecuteQueryForPrevNext PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to parepare an execute query for prev and next 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcValue    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrevious AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cLimitingQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse      AS CHARACTER NO-UNDO. 
           
    RUN pAssignCommonRecords(
        INPUT-OUTPUT cQueryBuffers,
        INPUT-OUTPUT cFieldBuffer,
        INPUT-OUTPUT cFieldName,
        INPUT-OUTPUT lIsBreakByUsed
        ).   
    /* Use Below logic to soet records by rec_key */
           
    /* cLimitingQuery = "For EACH oe-ord NO-LOCK"
                     + " WHERE oe-ord.company EQ " + QUOTER(cocode)
                     + " AND " + pGetWhereCriteria("oe-ord")
                     + " AND oe-ord.rec_key " + (IF iplPrevious THEN "LE " ELSE "GE ") + QUOTER(ipcValue)
                     + ", FIRST oe-ordl of oe-ord NO-LOCK "
                     + " WHERE " + pGetWhereCriteria("oe-ord")
                     + " BY oe-ord.rec_key DESC"
                     . */
                      
    cLimitingQuery = "FOR EACH oe-ordl NO-LOCK"
                     + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                     + " AND oe-ordl.ord-no " + (IF iplPrevious THEN "LE " ELSE "GE ") + STRING(INTEGER(ipcValue))
                     + pGetWhereCriteria("oe-ordl")  
                     + ", FIRST oe-ord OF oe-ordl NO-LOCK"
                     + " WHERE " + pGetWhereCriteria("oe-ord")
                     + " BREAK BY oe-ordl.ord-no " + (IF iplPrevious THEN "DESCENDING" ELSE "" )
                     .  
    RUN Browse_PrepareAndExecuteLimitingQuery(
        INPUT  cLimitingQuery,   /* Query */
        INPUT  cQueryBuffers,    /* Buffers Name */
        INPUT  iRecordLimit,     /* Record Limit */
        INPUT  dQueryTimeLimit,  /* Time Limit*/
        INPUT  lEnableShowAll,   /* Enable ShowAll Button? */
        INPUT  cFieldBuffer,     /* Buffer name to fetch the field's value*/
        INPUT  cFieldName,       /* Field Name*/
        INPUT  NO,               /* Initial Query*/
        INPUT  lIsBreakByUsed,   /* Is breakby used */
        OUTPUT cResponse           
        ). 
    
    IF cResponse EQ "" THEN
        MESSAGE "No Records Found..."
            VIEW-AS ALERT-BOX ERROR. 
              
    ELSE IF cResponse EQ "ShowAll" THEN
        RUN pPrepareAndExecuteQueryForShowAll.
    
    ELSE DO:
            /* Use below logic to sort records by rec_key */
            /* cBrowseWhereClause = " AND oe-ordl.rec_key "   + (IF iplPrevious THEN "LE " ELSE "GE ") + QUOTER(ipcValue)
                                 + " AND oe-ordl.rec_key " + (IF iplPrevious THEN "GE " ELSE "LE ") + QUOTER(cResponse). */
                                 
            cBrowseWhereClause = " AND oe-ordl.ord-no "   + (IF iplPrevious THEN "LE " ELSE "GE ") + STRING(INTEGER(ipcValue))
                                 + " AND oe-ordl.ord-no " + (IF iplPrevious THEN "GE " ELSE "LE ") + STRING (INTEGER(cResponse)).
             
        cBrowseQuery = "FOR EACH oe-ordl NO-LOCK"
                        + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                        +  cBrowseWhereClause
                        + pGetWhereCriteria("oe-ordl")  
                        + ",FIRST oe-ord OF oe-ordl NO-LOCK"
                        + " WHERE " + pGetWhereCriteria("oe-ord")
                        + ",FIRST itemfg " + (IF fi_cad-no EQ "" THEN "OUTER-JOIN" ELSE "") + " NO-LOCK"
                        + " WHERE itemfg.company EQ oe-ordl.company"
                        + "   AND itemfg.i-no    EQ oe-ordl.i-no"
                        + ( IF fi_cad-no NE "" THEN " AND itemfg.cad-no BEGINS " + QUOTER(fi_cad-no) ELSE "")
                        + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY oe-ordl.ord-no BY oe-ordl.i-no"
                        .   
                        
        RUN Browse_PrepareAndExecuteBrowseQuery(
            INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */
            INPUT  cBrowseQuery,                /* BRowse Query */
            INPUT  NO,                          /* Show limit alert? */
            INPUT  0,                           /* Record limit */
            INPUT  0,                           /* Time Limit */
            INPUT  lEnableShowAll,              /* Enable ShowAll Button */
            OUTPUT cResponse
            ).
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQueryForShowAll B-table-Win 
PROCEDURE pPrepareAndExecuteQueryForShowAll PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Private procedure to show all records in browse
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cShowAllQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.
               
    cShowAllQuery = "FOR EACH oe-ordl NO-LOCK"
                    + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                    + pGetWhereCriteria("oe-ordl") 
                    + ", FIRST oe-ord OF oe-ordl NO-LOCK"
                    + " WHERE " + pGetWhereCriteria("oe-ord")
                    + ",FIRST itemfg " + (IF fi_cad-no EQ "" THEN "OUTER-JOIN" ELSE "") + " NO-LOCK"
                    + " WHERE itemfg.company EQ oe-ordl.company"
                    + "   AND itemfg.i-no    EQ oe-ordl.i-no"
                    + ( IF fi_cad-no NE "" THEN " AND itemfg.cad-no BEGINS " + QUOTER(fi_cad-no) ELSE "")
                    + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY oe-ordl.ord-no BY oe-ordl.i-no"
                    .               
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cShowAllQuery,               /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse                                                  
        ).
    lShowAll = NO.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDefaults B-table-Win 
PROCEDURE pSetDefaults PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Set the toggle box defaults based on screen type
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcScreen AS CHARACTER NO-UNDO.
    
    IF ipcScreen EQ "OW" THEN 
        ASSIGN 
            tbOther = NO
            tbHold  = NO
            .
    ELSE IF ipcScreen EQ "OC" THEN 
        ASSIGN 
            tbWeb   = NO
            tbOther = NO
            tbHold  = YES
            .                        
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
  
  RUN set-defaults.
  APPLY "VALUE-CHANGED":U TO cbType IN FRAME {&FRAME-NAME}.
  ASSIGN FRAME {&FRAME-NAME}
     fi_cust-no
     fi_i-no
     fi_part-no
     fi_ord-no
     fi_po-no1
     fi_est-no
     fi_job-no
     fi_job-no2
     fi_cad-no
     fi_sman
     cbType:SCREEN-VALUE = "Open"
     cbType
     tbOther
     tbHold
     tbWeb
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
    
 IF AVAIL oe-ordl THEN   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query2 B-table-Win 
PROCEDURE reopen-query2 :
/*------------------------------------------------------------------------------
  Purpose:   This procedure is a copy of reopen-query1 without open-query call. 
             Rebuild of jobstuds was taking long time because of executing 
             open-query. So, that call has been removed in this procedure and 
             reopen-query1 call replaced with this Procedures's call to improve 
             performance while Rebuilding jobstuds
             
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.

    FIND FIRST bf-oe-ord NO-LOCK
         WHERE ROWID(bf-oe-ord) EQ ipRowID  
         NO-ERROR.
    IF AVAILABLE bf-oe-ord THEN DO:
        FIND FIRST bf-oe-ordl OF bf-oe-ord NO-LOCK.
        ipRowID = ROWID(bf-oe-ordl).
    END.

    DO WITH FRAME {&FRAME-NAME}:
        RUN repo-query (ipRowID).
    END.
    
    RELEASE bf-oe-ordl.
    RELEASE bf-oe-ord.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pQueryFirst B-table-Win 
PROCEDURE pQueryFirst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE li AS INT NO-UNDO.
  DEFINE VARIABLE iOrderNo LIKE oe-ordl.ord-no NO-UNDO.
  DEFINE VARIABLE cShowAllQuery AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cResponse     AS CHARACTER NO-UNDO.

  {&for-eachblank}
      AND oe-ordl.opened EQ YES
      USE-INDEX opened NO-LOCK,
      {&for-each2}
      AND oe-ord.ord-date GE fiOrderDate       
      BREAK BY oe-ordl.ord-no DESC:  
    IF FIRST-OF(oe-ordl.ord-no) THEN li = li + 1.
    iOrderNo = oe-ordl.ord-no.
    IF li GE iOEBrowse THEN LEAVE.
  END.
                   
  cShowAllQuery = "FOR EACH oe-ordl NO-LOCK"
                    + " WHERE oe-ordl.company EQ " + QUOTER(cocode)
                    + "AND oe-ordl.ord-no GE " + QUOTER(iOrderNo)
                    + pGetWhereCriteria("oe-ordl") 
                    + ", FIRST oe-ord OF oe-ordl NO-LOCK"
                    + " WHERE " + pGetWhereCriteria("oe-ord")
                    + ",FIRST itemfg " + (IF fi_cad-no EQ "" THEN "OUTER-JOIN" ELSE "") + " NO-LOCK"
                    + " WHERE itemfg.company EQ oe-ordl.company"
                    + "   AND itemfg.i-no    EQ oe-ordl.i-no"
                    + ( IF fi_cad-no NE "" THEN " AND itemfg.cad-no BEGINS " + QUOTER(fi_cad-no) ELSE "")
                    + " BY " + pGetSortCondition(lv-sort-by,lv-sort-by-lab) + ( IF ll-sort-asc THEN  "" ELSE " DESC") +  " BY oe-ordl.ord-no DESC BY oe-ordl.i-no"
                    .               
    RUN Browse_PrepareAndExecuteBrowseQuery(
        INPUT  BROWSE {&BROWSE-NAME}:QUERY, /* Browse Query Handle */      
        INPUT  cShowAllQuery,               /* BRowse Query */             
        INPUT  NO,                          /* Show limit alert? */        
        INPUT  0,                           /* Record limit */             
        INPUT  0,                           /* Time Limit */               
        INPUT  lEnableShowAll,              /* Enable ShowAll Button */    
        OUTPUT cResponse                                                  
        ).
    lShowAll = NO.         
    
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnPrevOrder B-table-Win 
FUNCTION fnPrevOrder RETURNS INTEGER
  ( ipcEstNo AS CHARACTER, ipiOrdNo AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
     DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
     DEF BUFFER bf-oe-ordl FOR oe-ordl.
        IF ipcEstNo GT "" THEN 
        DO:
            FIND LAST bf-oe-ordl NO-LOCK
                WHERE bf-oe-ordl.company EQ cocode
                  AND bf-oe-ordl.est-no  EQ ipcEstNo
                  AND bf-oe-ordl.ord-no  LT ipiOrdNo
                NO-ERROR.
            IF AVAILABLE bf-oe-ordl THEN
                iResult = (bf-oe-ordl.ord-no).
        END.
            RETURN iResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnProdBalance B-table-Win 
FUNCTION fnProdBalance RETURNS DECIMAL
  ( ipOrderQty AS DECIMAL, ipProdQty AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
     DEFINE VARIABLE iResult AS INTEGER NO-UNDO.
     
     ASSIGN
        iResult = ( ipOrderQty - ipProdQty ) .
     
            RETURN iResult.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-inv-qty B-table-Win 
FUNCTION get-inv-qty RETURNS INT
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/  

  DEF VAR lp-inv-qty AS INT NO-UNDO.

  ASSIGN lp-inv-qty = oe-ordl.inv-qty - int(getReturned("ReturnedInv")) NO-ERROR .   

  RETURN lp-inv-qty.

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
        lc-result = oe-ord.stat.
        RUN oe/getStatusDesc.p (oe-ord.stat, OUTPUT cResult).
        IF cResult NE "" THEN
        lc-result = cResult.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION isFilterBlank B-table-Win 
FUNCTION isFilterBlank RETURNS LOGICAL
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
                DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
                /* Determine if first-query can be used since no filters are applied */
                DO WITH FRAME {&frame-name}:
            IF  fi_cad-no:SCREEN-VALUE EQ "" AND
                fi_cust-no:SCREEN-VALUE EQ "" AND
                fi_est-no:SCREEN-VALUE EQ "" AND
                fi_i-name:SCREEN-VALUE EQ "" AND
                fi_i-no:SCREEN-VALUE EQ "" AND
                fi_job-no:SCREEN-VALUE EQ "" AND
                fi_job-no2:SCREEN-VALUE EQ "" AND
                fi_ord-no:SCREEN-VALUE EQ "" AND
                fi_part-no:SCREEN-VALUE EQ "" AND
                fi_po-no1:SCREEN-VALUE EQ "" AND
                fi_sman:SCREEN-VALUE EQ "" THEN 
                lResult = TRUE. 
             ELSE 
               lResult = FALSE. 
         END.
                RETURN lResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetCostUom B-table-Win 
FUNCTION pGetCostUom RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND FIRST bf-ar-invl NO-LOCK 
         WHERE bf-ar-invl.company EQ oe-ord.company
           AND bf-ar-invl.ord-no  EQ oe-ord.ord-no
           AND bf-ar-invl.i-no    EQ oe-ordl.i-no 
         NO-ERROR.
    IF AVAILABLE bf-ar-invl THEN DO:
       IF bf-ar-invl.dscr[1] EQ "" THEN
          RETURN "M".
       ELSE
          RETURN bf-ar-invl.dscr[1].
    END.
    ELSE
       RETURN "M".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetExtendedPrice B-table-Win 
FUNCTION pGetExtendedPrice RETURNS DECIMAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE dExtendedPrice AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTempPrics     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderQuantity AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE ROWID(bf-oe-ordl) EQ ROWID(oe-ordl) 
         NO-ERROR.
    
    dExtendedPrice = bf-oe-ordl.t-price.
    
    /* #97347 NK1 = OEINQ - Remove Invoice Price */
    
    /*IF oeinq-char NE "Order Price" THEN
    FOR EACH ar-invl FIELDS(inv-no amt i-no unit-pr disc) NO-LOCK 
        WHERE ar-invl.company EQ cocode 
          AND ar-invl.ord-no  EQ bf-oe-ordl.ord-no 
          AND ar-invl.i-no    EQ bf-oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING:
    
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ cocode
               AND itemfg.i-no    EQ ar-invl.i-no
             NO-ERROR.
    
        ASSIGN
           dOrderQuantity = IF bf-oe-ordl.pr-uom BEGINS "L" AND bf-oe-ordl.pr-uom NE "LB" THEN
                            IF bf-oe-ordl.qty LT 0 THEN -1 ELSE 1
                            ELSE
                            IF bf-oe-ordl.pr-uom EQ "CS" THEN
                               bf-oe-ordl.qty / (IF bf-oe-ordl.cas-cnt NE 0 THEN bf-oe-ordl.cas-cnt ELSE
                                               IF AVAILABLE itemfg AND itemfg.case-count NE 0
                                                              THEN itemfg.case-count ELSE
                                                                   1)
                            ELSE
                            IF bf-oe-ordl.pr-uom EQ "C" THEN
                               bf-oe-ordl.qty / 100
                            ELSE
                            IF bf-oe-ordl.pr-uom EQ "M" THEN
                              bf-oe-ordl.qty / 1000
                            ELSE
                              bf-oe-ordl.qty
    
           dTempPrics = dOrderQuantity * ar-invl.unit-pr.
/*           dExtendedPrice =  IF v-print-fmt EQ "Dayton" THEN             */
/*                  (dTempPrice - ROUND(dTempPrice * ar-invl.disc / 100,2))*/
/*                ELSE                                                     */
/*                  ROUND(dTempPrice * (1 - (ar-invl.disc / 100)),2).      */
    
        LEAVE.
    END.
    */
    RETURN dExtendedPrice.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetInvoiceLineCost B-table-Win 
FUNCTION pGetInvoiceLineCost RETURNS DECIMAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ar-invl FOR ar-invl.
    
    FIND FIRST bf-ar-invl NO-LOCK 
         WHERE bf-ar-invl.company EQ oe-ord.company
           AND bf-ar-invl.ord-no  EQ oe-ord.ord-no
           AND bf-ar-invl.i-no    EQ oe-ordl.i-no 
         NO-ERROR.
         
    RETURN IF AVAILABLE bf-ar-invl THEN bf-ar-invl.cost ELSE 0.  
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetPriceUom B-table-Win 
FUNCTION pGetPriceUom RETURNS CHARACTER PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE cPriceUom AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-oe-ordl NO-LOCK 
         WHERE ROWID(bf-oe-ordl) EQ ROWID(oe-ordl) 
         NO-ERROR.
    
    cPriceUOm = bf-oe-ordl.pr-uom.
    
    FOR EACH ar-invl FIELDS(inv-no pr-uom) NO-LOCK 
        WHERE ar-invl.company EQ cocode 
          AND ar-invl.ord-no  EQ bf-oe-ordl.ord-no 
          AND ar-invl.i-no    EQ bf-oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING: 
        cPriceUom = ar-invl.pr-uom.
        LEAVE.
    END. 
    RETURN cPriceUom.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetSellPrice B-table-Win 
FUNCTION pGetSellPrice RETURNS DECIMAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    DEFINE VARIABLE dSellPrice AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE ROWID(bf-oe-ordl) EQ ROWID(oe-ordl) 
         NO-ERROR.
    
    dSellPrice = bf-oe-ordl.price * (1 - (bf-oe-ordl.disc / 100)).
    
    FOR EACH ar-invl FIELDS(inv-no unit-pr disc) NO-LOCK
        WHERE ar-invl.company EQ cocode 
          AND ar-invl.ord-no  EQ bf-oe-ordl.ord-no 
          AND ar-invl.i-no    EQ bf-oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING:   
        dSellPrice = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
        LEAVE.
    END.
    RETURN dSellPrice.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetSortCondition B-table-Win 
FUNCTION pGetSortCondition RETURNS CHARACTER
  (ipcSortBy AS CHARACTER,ipcSortLabel AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Retuns the sort condition based on the input 
 Notes:
------------------------------------------------------------------------------*/
    
    RETURN (IF ipcSortBy EQ 'ord-no'    THEN "oe-ordl.ord-no"                        ELSE ~
            IF ipcSortBy EQ 'cStatus'   THEN "oe-ord.stat"                           ELSE ~
            IF ipcSortBy EQ 'whsed'     THEN "oe-ordl.whsed"                         ELSE ~
            IF ipcSortBy EQ 'managed'   THEN "oe-ordl.managed"                       ELSE ~
            IF ipcSortBy EQ 'ord-date'  THEN "STRING(YEAR(oe-ord.ord-date),'9999')   
                                             + STRING(MONTH(oe-ord.ord-date),'99')   
                                             + STRING(DAY(oe-ord.ord-date),'99')"    ELSE ~
            IF ipcSortBy EQ 'cust-no'   THEN "oe-ordl.cust-no"                       ELSE ~
            IF ipcSortBy EQ 'po-no'     THEN "oe-ord.po-no"                          ELSE ~
            IF ipcSortBy EQ 'cust-name' THEN "oe-ord.cust-name"                      ELSE ~
            IF ipcSortBy EQ 'i-no'      THEN "oe-ordl.i-no"                          ELSE ~
            IF ipcSortBy EQ 'i-name'    THEN "oe-ordl.i-name"                        ELSE ~
            IF ipcSortBy EQ 'part-no'   THEN "oe-ordl.part-no"                       ELSE ~
            IF ipcSortBy EQ 'po-no'     THEN (IF ipcSortLabel EQ "Order PO#" THEN    
                                              oe-ord.po-no ELSE oe-ordl.po-no)       ELSE ~
            IF ipcSortBy EQ 'est-no'    THEN "oe-ordl.est-no"                        ELSE ~
            IF ipcSortBy EQ 'job-no'    THEN "STRING(oe-ordl.job-no,'x(6)')          
                                                 + STRING(oe-ordl.job-no2,'99')"     ELSE ~
            IF ipcSortBy EQ 'cad-no'    THEN "itemfg.cad-no"                         ELSE ~
            IF ipcSortBy EQ 'q-onh'     THEN "itemfg.q-onh"                          ELSE ~
            IF ipcSortBy EQ 's-man'     THEN "oe-ordl.s-man[1]"                      ELSE ~
            IF ipcSortBy EQ 'e-num'     THEN "STRING(oe-ordl.e-num)"                 ELSE ~
            IF ipcSortBy EQ 'cost'      THEN "STRING(oe-ordl.cost,'-9999999999.99')" ELSE ~
            IF ipcSortBy EQ 'rec_key'   THEN "STRING(oe-ord.rec_key)"                ELSE ~
            IF ipcSortBy EQ 'i-name'    THEN 'oe-ordl.i-name'                        ELSE ~
            IF ipcSortBy EQ 'e-num'     THEN 'oe-ordl.e-num'                         ELSE ~
            IF ipcSortBy EQ 'qty'       THEN 'oe-ordl.qty'                           ELSE ~
            IF ipcSortBy EQ 'ship-qty'  THEN 'oe-ordl.ship-qty'                      ELSE ~           
                                             "STRING(YEAR(oe-ordl.req-date),'9999')
                                             + STRING(MONTH(oe-ordl.req-date),'99')
                                             + STRING(DAY(oe-ordl.req-date),'99')"
            ).
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pGetWhereCriteria B-table-Win 
FUNCTION pGetWhereCriteria RETURNS CHARACTER
  ( ipcTable AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: Prepares and returns the where clause criteria based on table name
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cWhereCriteria AS CHARACTER NO-UNDO.
    
    IF ipcTable EQ "oe-ord" THEN DO: 
        IF cbType EQ "Closed" OR (cbType EQ "Open" AND tbOther) THEN 
            cWhereCriteria = " oe-ord.stat NE 'W'".
              
        IF (cbType EQ "Open" AND tbOther AND NOT tbHold) THEN DO:
            IF cWhereCriteria EQ "" THEN 
                cWhereCriteria = " oe-ord.stat NE 'H'".
            ELSE DO: 
                cWhereCriteria = cWhereCriteria + " AND oe-ord.stat NE 'H'". 
                cWhereCriteria = "(" + cWhereCriteria + ")".    
            END.                
        END.  
           
        IF (cbType EQ "Open" AND tbHold) THEN DO:
            IF cWhereCriteria EQ "" THEN 
                cWhereCriteria = " oe-ord.stat EQ 'H'".
            ELSE     
                cWhereCriteria = cWhereCriteria + " OR oe-ord.stat EQ 'H'".          
        END.   
        IF cbType EQ "Open" AND tbWeb THEN DO:
            IF cWhereCriteria EQ "" THEN 
                cWhereCriteria = " oe-ord.stat EQ 'W'" .
            ELSE     
                cWhereCriteria = cWhereCriteria + " OR oe-ord.stat EQ 'W'".             
        END.                 
        IF cbType NE "All" THEN 
            cWhereCriteria = "(" + cWhereCriteria + ")".
        
        IF cbType EQ "Closed" THEN DO:
            /* Use below logic to sort records by rec_key */ 
                    
            /* cWhereCriteria = cWhereCriteria + " AND (oe-ord.opened EQ NO OR oe-ord.opened EQ YES)". */
            
            cWhereCriteria = cWhereCriteria + " AND ((oe-ord.opened EQ YES) OR (oe-ord.opened EQ NO OR oe-ordl.stat EQ 'C'))".                   
        END.    
            
        ELSE IF cbType EQ "Open" THEN 
            cWhereCriteria = cWhereCriteria + " AND oe-ord.opened EQ YES".
                    
        cWhereCriteria =  cWhereCriteria  + (IF cbType NE "All" THEN " AND " ELSE " ") + "oe-ord.ord-date  GE " + STRING(fiOrderDate) 
                          + (IF fi_po-no1 NE "" THEN " AND oe-ord.po-no BEGINS " + QUOTER(fi_po-no1) ELSE "")
                          .                  
    END. 
    ELSE DO:
        IF cbType EQ "Closed" THEN 
            cWhereCriteria = cWhereCriteria +  " AND oe-ordl.opened EQ NO".
        ELSE IF cbType EQ "open" THEN
            cWhereCriteria = cWhereCriteria  + " AND oe-ordl.opened EQ YES AND oe-ordl.stat NE 'C'".   
            
        cWhereCriteria = cWhereCriteria  
                         + (IF custCount  NE "" THEN " AND ((LOOKUP(oe-ordl.cust-no," + QUOTER(custcount) + ") NE 0" + " AND oe-ordl.cust-no NE '') OR " + QUOTER(custcount) + " EQ '')" ELSE "")
                         + (IF fi_ord-no  NE 0  THEN " AND oe-ordl.ord-no  EQ "       + STRING(fi_ord-no)   ELSE "")
                         + (IF fi_cust-no NE "" THEN " AND oe-ordl.cust-no BEGINS "   + QUOTER(fi_cust-no)  ELSE "")
                         + (IF fi_est-no  NE "" THEN " AND oe-ordl.est-no BEGINS "    + QUOTER(fi_est-no)   ELSE "")
                         + (IF fi_job-no  NE "" THEN " AND oe-ordl.job-no BEGINS "    + QUOTER(fi_job-no)   ELSE "")
                         + (IF fi_job-no  NE "" AND fi_job-no2 NE 0 THEN " AND oe-ordl.job-no2 EQ " + STRING(fi_job-no2)  ELSE "")
                         + (IF fi_i-no    NE "" THEN " AND oe-ordl.i-no   BEGINS "    + QUOTER (fi_i-no)    ELSE "")
                         + (IF fi_part-no NE "" THEN " AND oe-ordl.part-no BEGINS "   + QUOTER(fi_part-no)  ELSE "")
                         + (IF fiItemPo   NE "" THEN " AND oe-ordl.po-no BEGINS "     + QUOTER(fiItemPo)    ELSE "")
                         + (IF fi_sman    NE "" THEN " AND oe-ordl.s-man[1] BEGINS "  + QUOTER(fi_sman)     ELSE "")
                         + (IF fi_i-name  NE "" THEN " AND oe-ordl.i-name BEGINS "    + QUOTER(fi_i-name)   ELSE "") . 
     
    END.     
    RETURN cWhereCriteria.      
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION pIsValidSearch B-table-Win 
FUNCTION pIsValidSearch RETURNS LOGICAL PRIVATE
  (  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF cbType EQ "Open" AND NOT tbWeb
       AND NOT tbOther AND NOT tbHold THEN DO:
        MESSAGE "Atleast one toggle box should be checked." 
        VIEW-AS ALERT-BOX ERROR.        
        RETURN NO.   
    END.
    ELSE 
        RETURN YES.        
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

