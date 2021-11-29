&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME d-oeitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS d-oeitem 
/*------------------------------------------------------------------------

  File: oe\d-oeitem.w
  
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{oe/tt-item-qty-price.i} 

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ip-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEFINE INPUT PARAMETER ip-type AS cha NO-UNDO .   /* add,update,view */
DEFINE INPUT PARAMETER TABLE FOR tt-item-qty-price.

DEFINE OUTPUT PARAMETER op-rowid-list AS CHARACTER NO-UNDO. /* if added records from history */
DEFINE OUTPUT PARAMETER op-cancel AS LOG NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/w-job.i}
DEFINE TEMP-TABLE w-jobs NO-UNDO LIKE w-job.
DEFINE TEMP-TABLE ttAllocated NO-UNDO
    FIELD ord-no    LIKE oe-ordl.ord-no
    FIELD cust-no   LIKE oe-ordl.cust-no
    FIELD cust-name LIKE oe-ord.cust-name
    FIELD qty       LIKE oe-ordl.qty
    FIELD ship-qty  LIKE oe-ordl.ship-qty
    FIELD due-date  LIKE oe-ord.due-date
    FIELD price     LIKE oe-ordl.price
    FIELD pr-uom    LIKE oe-ordl.pr-uom
    FIELD part-no   LIKE oe-ordl.part-no
    FIELD po-no     LIKE oe-ordl.po-no
    FIELD allocated AS DECIMAL LABEL "Allocated" FORMAT "->>,>>>,>>9.9<<"
    FIELD loc       LIKE oe-ord.loc LABEL "Whse"
    .

DEFINE VARIABLE ll-new-file AS LOG   NO-UNDO.
DEFINE VARIABLE cp-part-no  LIKE itemfg.part-no NO-UNDO.
DEFINE VARIABLE cp-rowid    AS ROWID NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared}
{oe/ordholdstat.i}
DEFINE VARIABLE ll-valid     AS LOG       NO-UNDO.
DEFINE VARIABLE v-use-rel    LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE v-upd-comm   AS LOG       INIT YES NO-UNDO.
DEFINE VARIABLE v-dup-item   AS LOG       NO-UNDO.
DEFINE VARIABLE v-rtn-code   AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-rtn-char   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gv-ship-from AS CHARACTER NO-UNDO.
DEFINE NEW SHARED BUFFER xest FOR est.
DEFINE NEW SHARED BUFFER xeb  FOR eb.
DEFINE NEW SHARED BUFFER xef  FOR ef.
DEFINE VARIABLE v-valdcode AS cha INIT "ON,BY,MH" NO-UNDO.
DEFINE VARIABLE v-bld-job  AS cha NO-UNDO.
DEFINE VARIABLE v-est-no   AS cha NO-UNDO.  /* for adjust est-no */
/* for oe/oe-price.p ========*/
DEFINE NEW SHARED BUFFER xoe-ord FOR oe-ord.    /* BUFFER WITH ORDER HEADER */
/* ==== FOR REPRICE ===*/
DEFINE NEW SHARED VARIABLE v-procat    LIKE oe-prmtx.procat NO-UNDO. /* ITEM CATEGORY */
DEFINE NEW SHARED VARIABLE v-price-lev AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE s-est-no    AS cha     NO-UNDO.  /* for fgadd2.p */
DEFINE NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
DEFINE NEW SHARED VARIABLE save_id             AS RECID     NO-UNDO.  /* RECORD ID FOR ORDER LINE */
DEFINE NEW SHARED VARIABLE v-i-item            LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEFINE NEW SHARED VARIABLE v-i-qty             LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEFINE NEW SHARED VARIABLE price-ent           AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE matrixExists        AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id              AS RECID     NO-UNDO. 
DEFINE NEW SHARED VARIABLE nufile              AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-qty-mod           AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-fr-tax            LIKE oe-ctrl.f-tax NO-UNDO.
DEFINE NEW SHARED VARIABLE v-create-job        AS LOG       NO-UNDO.

DEFINE            VARIABLE lv-ordl-recid       AS RECID     NO-UNDO.
DEFINE            VARIABLE lv-change-prom-date AS LOG       NO-UNDO.  /* flag for updating oe-ordl.prom-date*/
DEFINE            VARIABLE lv-change-cst-po    AS LOG       NO-UNDO.    /* flag for updateing oe-ordl.po-no */
DEFINE            VARIABLE cUOMListQty         AS cha       INIT "M,EA,L,CS,C,LB,DRM,ROL,PLT,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE            VARIABLE cUOMListPrice       AS cha       INIT "M,EA,L,CS,C,LB,DRM,ROL,PLT,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE            VARIABLE lv-valid-uom        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-valtype           AS cha       INIT "O,R,C" NO-UNDO.
DEFINE            VARIABLE v-duelist           AS cha       INIT "AM,ASAP,BY,CPU,CR,HFR,HOLD,HOT,INK,MH,MUST,NB4,NCUST,NITEM,NCNI,OE,ON,PPR,RWRK,RUSH,TOOL,WO,$$$" NO-UNDO.
DEFINE            VARIABLE v-ship-id           AS CHARACTER NO-UNDO.

DEFINE            VARIABLE ll-new-record       AS LOG       NO-UNDO.
DEFINE BUFFER xoe-ordl FOR oe-ordl.
DEFINE VARIABLE lv-item-recid      AS RECID NO-UNDO.
DEFINE VARIABLE first-cust-part-no AS cha   NO-UNDO.
DEFINE VARIABLE ll-ok-i-no         AS LOG   NO-UNDO.
DEFINE VARIABLE ls-stock           AS cha   NO-UNDO.  /* for eb.stock-no */
DEFINE VARIABLE ll-help-ran        AS LOG   NO-UNDO.  /* qty help */
DEFINE VARIABLE ll-bypass          AS LOG   NO-UNDO.    /* bypass fields for price */
{ce/print4.i "new shared"}
{ce/print42.i "new shared"}

DEFINE NEW SHARED VARIABLE lv-qty       AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE qty          AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE v-shared-rel AS INTEGER NO-UNDO.

DEFINE BUFFER xoe-rel FOR oe-rel.
DEFINE VARIABLE ld-prev-price          AS DECIMAL       NO-UNDO.
DEFINE VARIABLE ll-got-qtprice         AS LOG           NO-UNDO.
DEFINE VARIABLE li-prev-qty            AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-add-mode            AS LOG           NO-UNDO. 
DEFINE VARIABLE lv-help-qty            AS INTEGER       NO-UNDO.
DEFINE VARIABLE ll-qty-leave-done      AS LOG           NO-UNDO.
DEFINE VARIABLE ll-new-fg-created      AS LOG           NO-UNDO.
DEFINE VARIABLE lv-new-tandem          AS ROWID         NO-UNDO.
DEFINE VARIABLE ll-is-tandem           AS LOG           NO-UNDO.
DEFINE VARIABLE ll-do-entry            AS LOG           NO-UNDO.
DEFINE VARIABLE lv-update-job-stdate   AS LOG           NO-UNDO.
DEFINE VARIABLE v-print-head           LIKE sys-ctrl.log-fld NO-UNDO.
DEFINE VARIABLE v-print-fmt            LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE lv-q-no                LIKE quotehd.q-no NO-UNDO.
DEFINE VARIABLE v-run-schedule         AS LOG           NO-UNDO.
DEFINE VARIABLE lv-type-codes          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-type-dscrs          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE K_FRAC                 AS DECIMAL       INIT 6.25 NO-UNDO.
DEFINE VARIABLE ld-prev-t-price        LIKE oe-ordl.t-price NO-UNDO.
DEFINE VARIABLE li-prev-ord-qty        LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE ld-prev-prom-date      AS DATE          NO-UNDO.
DEFINE VARIABLE dtPrevDueDate          AS DATE          NO-UNDO.
DEFINE VARIABLE v-duplicateFGDayClient AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-rec-found            AS LOG           NO-UNDO.
DEFINE VARIABLE v-orig-ip-type         AS CHARACTER     NO-UNDO.
DEFINE VARIABLE op-error               AS LOG           NO-UNDO.

DEFINE VARIABLE historyQty             AS DECIMAL       NO-UNDO.
DEFINE VARIABLE historyPrice           LIKE oe-ordl.price NO-UNDO.
DEFINE VARIABLE historyPrUOM           LIKE oe-ordl.pr-uom NO-UNDO.
DEFINE VARIABLE setFromHistory         AS LOGICAL       NO-UNDO.
DEFINE VARIABLE historyButton          AS LOGICAL       NO-UNDO.
DEFINE VARIABLE v-rel                  AS INTEGER       NO-UNDO.
DEFINE VARIABLE v-margin               AS DECIMAL       NO-UNDO.
DEFINE VARIABLE llGotLowerPrice        AS LOG           NO-UNDO.
DEFINE VARIABLE v-widhand              AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE v-pricehand            AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE v-custparthand         AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE oesetxfer-log          AS LOG           NO-UNDO.
DEFINE VARIABLE oefgadd-log            AS LOG           NO-UNDO.
DEFINE VARIABLE OESellPriceXfer-log    AS LOG           NO-UNDO.
DEFINE VARIABLE lv-multi-select        AS LOG           NO-UNDO INIT NO.
DEFINE VARIABLE llOEFGAdd-sec          AS LOG           NO-UNDO.
DEFINE VARIABLE llAutoadd-sec          AS LOG           NO-UNDO.
DEFINE VARIABLE llOEPrcChg-sec         AS LOGICAL       NO-UNDO.
DEFINE VARIABLE oeDateAuto-log         AS LOG           NO-UNDO.
DEFINE VARIABLE oeDateAuto-char        AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-access-close         AS LOG           NO-UNDO.
DEFINE VARIABLE v-access-list          AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lInvoiceFound          AS LOGICAL       NO-UNDO.

DEFINE TEMP-TABLE tt-qty-price
    FIELD oeordl-rowid      AS ROWID
    FIELD tt-historyQty     LIKE oe-ordl.qty
    FIELD tt-historyPrice   LIKE oe-ordl.price
    FIELD tt-historyPrUOM   AS CHARACTER
    FIELD tt-setFromHistory AS LOG
    INDEX oe-rowid oeordl-rowid.
 
DEFINE VARIABLE ll-calc-disc-FIRST AS LOG       NO-UNDO.
DEFINE VARIABLE v-format           LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE v-basis            LIKE sman.commbasis INIT "" NO-UNDO.
DEFINE VARIABLE OEPO#Xfer-log      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE oeDateChange-log   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE oeDateChange-chr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLastDateChange   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPromManualChanged AS LOGICAL   NO-UNDO. //97238 MFG Date - Weekends
DEFINE VARIABLE cDueManualChanged  AS LOGICAL   NO-UNDO. //97238 MFG Date - Weekends

DEFINE VARIABLE hdSalesManProcs    AS HANDLE    NO-UNDO.
DEFINE VARIABLE lAvailable         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE matrixTag          AS CHARACTER NO-UNDO.
{system/ttTag.i &Table-Name=ttTag}
{system/ttTag.i &Table-Name=ttTempTag}

DEFINE VARIABLE deAutoOver                AS DECIMAL        NO-UNDO.
DEFINE VARIABLE deAutoUnder               AS DECIMAL        NO-UNDO.
DEFINE VARIABLE cRtnChar                  AS CHARACTER      NO-UNDO.
DEFINE VARIABLE lRecFound                 AS LOGICAL        NO-UNDO.
DEFINE VARIABLE cFGOversDefault           AS CHARACTER      NO-UNDO.
DEFINE VARIABLE dRemainingQty             AS DECIMAL        NO-UNDO.
DEFINE VARIABLE cLocation                 AS CHARACTER      NO-UNDO.
DEFINE VARIABLE dtRelDate                 AS DATE           NO-UNDO COLUMN-LABEL "Release!Date" FORMAT "99/99/9999".
DEFINE VARIABLE iXferQty                  AS INTEGER        NO-UNDO COLUMN-LABEL "Transfer!Qty" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE dPrice                    AS DECIMAL        NO-UNDO COLUMN-LABEL "Sell Price" FORMAT ">>,>>>,>>9.99<<<<".
DEFINE VARIABLE cUOM                      AS CHARACTER      NO-UNDO COLUMN-LABEL "UOM" FORMAT "x(4)".
DEFINE VARIABLE iProdQty                  AS INTEGER        NO-UNDO COLUMN-LABEL "Prod. Qty" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE iBalQty                   AS INTEGER        NO-UNDO COLUMN-LABEL "On Hand Qty" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE iActRelQty                AS INTEGER        NO-UNDO COLUMN-LABEL "Act. Rel.!Quantity" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE iWIP                      AS INTEGER        NO-UNDO COLUMN-LABEL "Production!Balance" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE iPct                      AS INTEGER        NO-UNDO COLUMN-LABEL "O/U%" FORMAT "->>>>>%".
DEFINE VARIABLE dInvLineCost              AS DECIMAL        NO-UNDO COLUMN-LABEL "Invoice!Line Cost".
DEFINE VARIABLE cCostUOM                  AS CHARACTER      NO-UNDO COLUMN-LABEL "Cost!UOM".
DEFINE VARIABLE cLastShipTo               AS CHARACTER      NO-UNDO COLUMN-LABEL "Last!ShipTo" FORMAT "x(8)".
DEFINE VARIABLE iActBOLQty                AS INTEGER        NO-UNDO COLUMN-LABEL "Actual!BOL Qty" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE dTotQtyRet                AS INTEGER        NO-UNDO COLUMN-LABEL "Total Qty!Returned" FORMAT ">>>,>>9".
DEFINE VARIABLE dTotRetInv                AS INTEGER        NO-UNDO COLUMN-LABEL "Qty Return!Inventory" FORMAT ">>>,>>9".
DEFINE VARIABLE iHandQtyNoAlloc           AS INTEGER        NO-UNDO COLUMN-LABEL "On Hand Qty!not Allocated" FORMAT "->>,>>>,>>9".
DEFINE VARIABLE li-qoh                    AS INTEGER        NO-UNDO.
DEFINE VARIABLE li-bal                    AS INTEGER        NO-UNDO.

DEFINE VARIABLE cDisplayFGLocationDetails AS CHARACTER      NO-UNDO.
DEFINE VARIABLE cFGDefaultQtyDisplay      AS CHARACTER      NO-UNDO.

RUN salrep/SalesManProcs.p PERSISTENT SET hdSalesManProcs.

cocode = g_company.

{oe/oe-sysct1.i NEW} 

{custom/framechk.i NEW} 

DEFINE BUFFER bf-ef      FOR ef.
DEFINE BUFFER bf-eb      FOR eb.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-rel  FOR oe-rel.

/* gdm - 06220908*/
DEFINE VARIABLE v-relflg             AS LOG     NO-UNDO.

/* gdm - 11090905*/
DEFINE VARIABLE v-ponoUp             AS LOG     NO-UNDO.
DEFINE VARIABLE lv-change-inv-po     AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOEPriceWarning      AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCheckFgForceWarning AS LOGICAL NO-UNDO.
DEFINE VARIABLE llOEDiscount         AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE w-est-no NO-UNDO 
    FIELD w-est-no LIKE itemfg.est-no 
    FIELD w-run    AS LOG.
DEFINE VARIABLE cFreightCalculationValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCheckMessage            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lQuotePriceMatrix        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCreateJobFromFG         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUnspecified             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGItem                  AS CHARACTER NO-UNDO.

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT"
    NO-LOCK NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN 
DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
        sys-ctrl.company = cocode
        sys-ctrl.name    = "INVPRINT"
        sys-ctrl.descrip = "Print Invoice Headers on Invoice Form".
    MESSAGE "Invoice Format:" UPDATE sys-ctrl.char-fld.
END.
ASSIGN
    v-format           = sys-ctrl.char-fld
    ll-calc-disc-FIRST = v-format EQ "Dayton".

RUN sys/ref/nk1look.p (INPUT cocode, "OESETXFER", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
oesetxfer-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEFGADD", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
oefgadd-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OESellPriceXfer", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
OESellPriceXfer-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEPO#Xfer", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
OEPO#Xfer-log = LOGICAL(v-rtn-char) NO-ERROR.


RUN sys/ref/nk1look.p (cocode, "oeDateChange", "L", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (cocode, "oeDateChange", "C", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateChange-chr = v-rtn-char NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    oeDateAuto-char = v-rtn-char NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "OEPriceWarning", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    lOEPriceWarning = LOGICAL(v-rtn-char) NO-ERROR.

                     

RUN sys/ref/nk1look.p (INPUT cocode, "FreightCalculation", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    cFreightCalculationValue = v-rtn-char NO-ERROR.
    
RUN sys/ref/nk1look.p (INPUT cocode, "QuotePriceMatrix", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
IF v-rec-found THEN
    lQuotePriceMatrix = LOGICAL(v-rtn-char) NO-ERROR.    

RUN sys/ref/nk1look.p (INPUT cocode, "JobCreateFromFG", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
lCreateJobFromFG = LOGICAL(v-rtn-char) NO-ERROR.
    
DO TRANSACTION:
    {sys/inc/oeship.i}
    {sys/inc/oereleas.i}
    {sys/inc/oescreen.i}
    {sys/inc/job#.i}    
    {sys/inc/oeround.i}
    {sys/inc/fgmaster.i}
    {sys/inc/oeestcom.i}
    {sys/inc/fgsecur.i} 
    {sys/inc/runship.i}
    {sys/inc/oepricecheck.i} 
    {sys/inc/reltype.i} 
    {sys/inc/shiptorep.i}
END.
{sys/ref/fgoecost.i}
{sys/ref/oecustpart.i}
{sys/ref/oecount.i}
{sys/inc/f16to32.i}
{sys/inc/funcToWorkDay.i}
{sys/inc/vendItemCost.i}
{system/ttConversionProcs.i}
DEFINE BUFFER b-vendItemCost FOR vendItemCost.

DO TRANSACTION:

    /* task# 09130412  09/14/04*/
    /*
      v-run-schedule = IF sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                       ELSE IF sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                       ELSE NO.   /*sys-ctrl.log-fld.*/
    */

  
    RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).


    {sys/inc/schedule.i}
    v-run-schedule = NOT (AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld).

    {sys/inc/graphic.i} 

END.

/* gdm - 06220908 - INSTEAD OF CHANGING sys/inc/oereleas.i */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "OERELEAS" NO-ERROR.
IF AVAILABLE sys-ctrl   AND
    sys-ctrl.log-fld AND
    sys-ctrl.int-fld = 1
    THEN ASSIGN v-relflg = YES.
/* gdm - 06220908 end */
/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "OEAutoFG",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT llAutoadd-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


RUN methods/prgsecur.p
    (INPUT "OEFGAdd",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT llOEFGAdd-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */

RUN methods/prgsecur.p
    (INPUT "OEPrcChg",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT llOEPrcChg-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */
     
RUN methods/prgsecur.p
    (INPUT "OEDiscount",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT llOEDiscount, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */    
     
DEFINE VARIABLE lcReturn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE llRecFound          AS LOG       NO-UNDO.
DEFINE VARIABLE llOeShipFromLog     AS LOG       NO-UNDO.
DEFINE VARIABLE lFGForcedCommission AS LOGICAL   NO-UNDO .
DEFINE VARIABLE dFGForcedCommission AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lShowWarning        AS LOGICAL   NO-UNDO.
RUN sys/ref/nk1look.p (cocode, "OESHIPFROM", "L", NO, NO, "", "", 
    OUTPUT lcReturn, OUTPUT llRecFound).
IF llRecFound THEN
    llOeShipFromLog = LOGICAL(lcReturn) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "FGForceCommission", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT llRecFound).
IF llRecFound THEN
    lFGForcedCommission = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT cocode, "FGForceCommission", "D" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT v-rtn-char, OUTPUT llRecFound).
IF llRecFound THEN
    dFGForcedCommission = DECIMAL(v-rtn-char) NO-ERROR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME d-oeitem
&Scoped-define BROWSE-NAME browseAllocated

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttAllocated job-hdr job w-jobs po-ordl ~
po-ord oe-rel oe-ordl oe-ord itemfg

/* Definitions for BROWSE browseAllocated                               */
&Scoped-define FIELDS-IN-QUERY-browseAllocated ttAllocated.ord-no ttAllocated.cust-no ttAllocated.cust-name ttAllocated.qty ttAllocated.ship-qty ttAllocated.due-date ttAllocated.price ttAllocated.pr-uom ttAllocated.part-no ttAllocated.po-no ttAllocated.allocated   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseAllocated   
&Scoped-define SELF-NAME browseAllocated
&Scoped-define QUERY-STRING-browseAllocated FOR EACH ttAllocated
&Scoped-define OPEN-QUERY-browseAllocated OPEN QUERY {&SELF-NAME} FOR EACH ttAllocated.
&Scoped-define TABLES-IN-QUERY-browseAllocated ttAllocated
&Scoped-define FIRST-TABLE-IN-QUERY-browseAllocated ttAllocated


/* Definitions for BROWSE browseJobs                                    */
&Scoped-define FIELDS-IN-QUERY-browseJobs job-hdr.job-no job-hdr.job-no2 ~
job-hdr.est-no job-hdr.ord-no job-hdr.cust-no job-hdr.due-date job.stat ~
job-hdr.qty job-hdr.i-no job-hdr.opened job-hdr.loc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseJobs 
&Scoped-define QUERY-STRING-browseJobs FOR EACH job-hdr ~
      WHERE job-hdr.company EQ itemfg.company AND ~
job-hdr.i-no EQ cFGItem AND ~
job-hdr.opened EQ YES AND ~
(job-hdr.loc EQ w-jobs.loc OR ~
w-jobs.loc EQ "*ALL") NO-LOCK, ~
      FIRST job OF job-hdr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browseJobs OPEN QUERY browseJobs FOR EACH job-hdr ~
      WHERE job-hdr.company EQ itemfg.company AND ~
job-hdr.i-no EQ cFGItem AND ~
job-hdr.opened EQ YES AND ~
(job-hdr.loc EQ w-jobs.loc OR ~
w-jobs.loc EQ "*ALL") NO-LOCK, ~
      FIRST job OF job-hdr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browseJobs job-hdr job
&Scoped-define FIRST-TABLE-IN-QUERY-browseJobs job-hdr
&Scoped-define SECOND-TABLE-IN-QUERY-browseJobs job


/* Definitions for BROWSE browseLocations                               */
&Scoped-define FIELDS-IN-QUERY-browseLocations w-jobs.loc w-jobs.loc-desc w-jobs.onHand w-jobs.onOrder w-jobs.allocated w-jobs.backOrder w-jobs.qtyAvailable   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseLocations   
&Scoped-define SELF-NAME browseLocations
&Scoped-define QUERY-STRING-browseLocations FOR EACH w-jobs WHERE ((w-jobs.qtyAvailable NE 0 AND iPrintAvailQty EQ 2) OR (w-jobs.qtyAvailable LT 0 AND iPrintAvailQty EQ 3) OR (w-jobs.qtyAvailable EQ 0 AND iPrintAvailQty EQ 4) OR (iPrintAvailQty EQ 1))
&Scoped-define OPEN-QUERY-browseLocations OPEN QUERY {&SELF-NAME} FOR EACH w-jobs WHERE ((w-jobs.qtyAvailable NE 0 AND iPrintAvailQty EQ 2) OR (w-jobs.qtyAvailable LT 0 AND iPrintAvailQty EQ 3) OR (w-jobs.qtyAvailable EQ 0 AND iPrintAvailQty EQ 4) OR (iPrintAvailQty EQ 1)).
&Scoped-define TABLES-IN-QUERY-browseLocations w-jobs
&Scoped-define FIRST-TABLE-IN-QUERY-browseLocations w-jobs


/* Definitions for BROWSE browsePOs                                     */
&Scoped-define FIELDS-IN-QUERY-browsePOs po-ordl.po-no po-ordl.vend-no ~
po-ordl.due-date po-ordl.job-no po-ordl.ord-qty po-ordl.t-rec-qty ~
po-ordl.cost po-ordl.line ~
po-ordl.ord-qty - po-ordl.t-rec-qty @ dRemainingQty po-ord.loc 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browsePOs 
&Scoped-define QUERY-STRING-browsePOs FOR EACH po-ordl ~
      WHERE po-ordl.company EQ itemfg.company AND ~
po-ordl.i-no EQ cFGItem AND ~
po-ordl.item-type EQ NO AND ~
po-ordl.opened EQ YES NO-LOCK, ~
      FIRST po-ord WHERE TRUE /* Join to po-ordl incomplete */ ~
      AND po-ord.company EQ po-ordl.company AND ~
po-ord.po-no EQ po-ordl.po-no AND ~
(po-ord.loc EQ w-jobs.loc OR ~
w-jobs.loc EQ "*ALL") NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browsePOs OPEN QUERY browsePOs FOR EACH po-ordl ~
      WHERE po-ordl.company EQ itemfg.company AND ~
po-ordl.i-no EQ cFGItem AND ~
po-ordl.item-type EQ NO AND ~
po-ordl.opened EQ YES NO-LOCK, ~
      FIRST po-ord WHERE TRUE /* Join to po-ordl incomplete */ ~
      AND po-ord.company EQ po-ordl.company AND ~
po-ord.po-no EQ po-ordl.po-no AND ~
(po-ord.loc EQ w-jobs.loc OR ~
w-jobs.loc EQ "*ALL") NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browsePOs po-ordl po-ord
&Scoped-define FIRST-TABLE-IN-QUERY-browsePOs po-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-browsePOs po-ord


/* Definitions for BROWSE browseReleases                                */
&Scoped-define FIELDS-IN-QUERY-browseReleases oe-rel.spare-char-1 ~
oe-ord.cust-no oe-ord.ord-no oe-rel.tot-qty oe-rel.stat oe-ordl.qty ~
oe-ordl.ship-qty fGetRelDate() @ dtRelDate oe-ordl.req-date ~
fGetXferQty() @ iXferQty fGetPriceDisc() @ dPrice fGetPrUOM() @ cUOM ~
oe-ordl.t-price oe-ordl.i-no oe-ordl.part-no oe-ordl.po-no oe-ord.po-no ~
oe-ordl.est-no oe-ordl.job-no oe-ordl.job-no2 oe-ord.ord-date oe-ord.stat ~
oe-ordl.inv-qty fGetProd(li-bal) @ iProdQty fGetBal(li-qoh) @ iBalQty ~
fGetActRelQty() @ iActRelQty fGetWIP() @ iWIP fGetPct(li-bal) @ iPct ~
oe-ordl.i-name oe-ordl.line fGetCost() @ dInvLineCost ~
fGetCostUOM() @ cCostUOM oe-ordl.po-no-po fGetLastShipTo() @ cLastShipTo ~
fGetTotalReturned() @ dTotQtyRet fGetReturnedInv() @ dTotRetInv ~
fGetActRelQty() - fGetActBOLQty() @ iHandQtyNoAlloc oe-ordl.cost 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseReleases 
&Scoped-define QUERY-STRING-browseReleases FOR EACH oe-rel ~
      WHERE oe-rel.company EQ oe-ordl.company AND ~
oe-rel.ord-no EQ oe-ordl.ord-no AND ~
oe-rel.i-no EQ cFGItem AND ~
oe-rel.line EQ oe-ordl.line AND ~
(oe-rel.spare-char-1 EQ w-jobs.loc OR ~
w-jobs.loc EQ "*All" ) AND ~
LOOKUP(oe-rel.s-code,"B,S") NE 0 AND ~
LOOKUP(oe-rel.stat,"S,A,L,B,Z") NE 0 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browseReleases OPEN QUERY browseReleases FOR EACH oe-rel ~
      WHERE oe-rel.company EQ oe-ordl.company AND ~
oe-rel.ord-no EQ oe-ordl.ord-no AND ~
oe-rel.i-no EQ cFGItem AND ~
oe-rel.line EQ oe-ordl.line AND ~
(oe-rel.spare-char-1 EQ w-jobs.loc OR ~
w-jobs.loc EQ "*All" ) AND ~
LOOKUP(oe-rel.s-code,"B,S") NE 0 AND ~
LOOKUP(oe-rel.stat,"S,A,L,B,Z") NE 0 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browseReleases oe-rel
&Scoped-define FIRST-TABLE-IN-QUERY-browseReleases oe-rel


/* Definitions for DIALOG-BOX d-oeitem                                  */
&Scoped-define FIELDS-IN-QUERY-d-oeitem oe-ordl.est-no ~
oe-ordl.sourceEstimateID oe-ordl.job-no oe-ordl.job-no2 oe-ordl.qty ~
oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 ~
oe-ordl.part-dscr2 oe-ordl.part-dscr3 oe-ordl.po-no oe-ordl.e-num ~
oe-ordl.po-no-po oe-ordl.vend-no oe-ordl.price oe-ordl.pr-uom oe-ordl.tax ~
oe-ordl.disc oe-ordl.cas-cnt oe-ordl.t-price oe-ordl.partial oe-ordl.cost ~
oe-ordl.cases-unit oe-ordl.type-code oe-ordl.customField oe-ordl.managed ~
oe-ordl.whsed oe-ordl.s-man[1] oe-ordl.s-pct[1] oe-ordl.s-comm[1] ~
oe-ordl.s-man[2] oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.s-man[3] ~
oe-ordl.s-pct[3] oe-ordl.s-comm[3] oe-ordl.over-pct oe-ordl.under-pct ~
oe-ordl.req-code oe-ordl.prom-code oe-ordl.req-date oe-ordl.prom-date ~
oe-ordl.spare-char-1 oe-ordl.spare-dec-1 oe-ordl.spare-char-2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-d-oeitem oe-ordl.est-no ~
oe-ordl.sourceEstimateID oe-ordl.qty oe-ordl.i-no oe-ordl.part-no ~
oe-ordl.i-name oe-ordl.part-dscr1 oe-ordl.part-dscr2 oe-ordl.part-dscr3 ~
oe-ordl.po-no oe-ordl.e-num oe-ordl.po-no-po oe-ordl.price oe-ordl.pr-uom ~
oe-ordl.tax oe-ordl.disc oe-ordl.cas-cnt oe-ordl.partial oe-ordl.cases-unit ~
oe-ordl.type-code oe-ordl.customField oe-ordl.managed oe-ordl.whsed ~
oe-ordl.s-man[1] oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] ~
oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] ~
oe-ordl.s-comm[3] oe-ordl.over-pct oe-ordl.under-pct oe-ordl.req-code ~
oe-ordl.prom-code oe-ordl.req-date oe-ordl.prom-date oe-ordl.spare-char-1 ~
oe-ordl.spare-dec-1 oe-ordl.spare-char-2 
&Scoped-define ENABLED-TABLES-IN-QUERY-d-oeitem oe-ordl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-d-oeitem oe-ordl
&Scoped-define QUERY-STRING-d-oeitem FOR EACH oe-ordl SHARE-LOCK, ~
      EACH oe-ord OF oe-ordl SHARE-LOCK, ~
      EACH itemfg OF oe-ordl SHARE-LOCK
&Scoped-define OPEN-QUERY-d-oeitem OPEN QUERY d-oeitem FOR EACH oe-ordl SHARE-LOCK, ~
      EACH oe-ord OF oe-ordl SHARE-LOCK, ~
      EACH itemfg OF oe-ordl SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-d-oeitem oe-ordl oe-ord itemfg
&Scoped-define FIRST-TABLE-IN-QUERY-d-oeitem oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-d-oeitem oe-ord
&Scoped-define THIRD-TABLE-IN-QUERY-d-oeitem itemfg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-ordl.est-no oe-ordl.sourceEstimateID ~
oe-ordl.qty oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 ~
oe-ordl.part-dscr2 oe-ordl.part-dscr3 oe-ordl.po-no oe-ordl.e-num ~
oe-ordl.po-no-po oe-ordl.price oe-ordl.pr-uom oe-ordl.tax oe-ordl.disc ~
oe-ordl.cas-cnt oe-ordl.partial oe-ordl.cases-unit oe-ordl.type-code ~
oe-ordl.customField oe-ordl.managed oe-ordl.whsed oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] oe-ordl.s-pct[2] ~
oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3] ~
oe-ordl.over-pct oe-ordl.under-pct oe-ordl.req-code oe-ordl.prom-code ~
oe-ordl.req-date oe-ordl.prom-date oe-ordl.spare-char-1 oe-ordl.spare-dec-1 ~
oe-ordl.spare-char-2 
&Scoped-define ENABLED-TABLES oe-ordl
&Scoped-define FIRST-ENABLED-TABLE oe-ordl
&Scoped-Define ENABLED-OBJECTS fi_qty-uom Btn_OK Btn_Done Btn_Cancel ~
Btn_hist fi_jobStartDate btn-quotes btnTagsUnder 
&Scoped-Define DISPLAYED-FIELDS oe-ordl.est-no oe-ordl.sourceEstimateID ~
oe-ordl.job-no oe-ordl.job-no2 oe-ordl.qty oe-ordl.i-no oe-ordl.part-no ~
oe-ordl.i-name oe-ordl.part-dscr1 oe-ordl.part-dscr2 oe-ordl.part-dscr3 ~
oe-ordl.po-no oe-ordl.e-num oe-ordl.po-no-po oe-ordl.vend-no oe-ordl.price ~
oe-ordl.pr-uom oe-ordl.tax oe-ordl.disc oe-ordl.cas-cnt oe-ordl.t-price ~
oe-ordl.partial oe-ordl.cost oe-ordl.cases-unit oe-ordl.type-code ~
oe-ordl.customField oe-ordl.managed oe-ordl.whsed oe-ordl.s-man[1] ~
oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] oe-ordl.s-pct[2] ~
oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3] ~
oe-ordl.over-pct oe-ordl.under-pct oe-ordl.req-code oe-ordl.prom-code ~
oe-ordl.req-date oe-ordl.prom-date oe-ordl.spare-char-1 oe-ordl.spare-dec-1 ~
oe-ordl.spare-char-2 
&Scoped-define DISPLAYED-TABLES oe-ordl
&Scoped-define FIRST-DISPLAYED-TABLE oe-ordl
&Scoped-Define DISPLAYED-OBJECTS fiPrevOrder fiPromDtLabel fi_type-dscr ~
fi_qty-uom spare-dec-1 fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl fi_sname-1 ~
fi_sname-2 fi_sname-3 fi_sname-lbl fi_jobStartDate 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 oe-ordl.job-no oe-ordl.job-no2 oe-ordl.t-price ~
oe-ordl.cost oe-ordl.type-code fi_sname-1 fi_sname-2 fi_sname-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetActBOLQty d-oeitem 
FUNCTION fGetActBOLQty RETURNS INTEGER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetActRelQty d-oeitem 
FUNCTION fGetActRelQty RETURNS INTEGER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetBal d-oeitem 
FUNCTION fGetBal RETURNS INTEGER
    (OUTPUT op-qoh AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCost d-oeitem 
FUNCTION fGetCost RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCostUOM d-oeitem 
FUNCTION fGetCostUOM RETURNS CHARACTER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetLastShipTo d-oeitem 
FUNCTION fGetLastShipTo RETURNS CHARACTER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPct d-oeitem 
FUNCTION fGetPct RETURNS INTEGER
    (ipBal AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPriceDisc d-oeitem 
FUNCTION fGetPriceDisc RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetProd d-oeitem 
FUNCTION fGetProd RETURNS INTEGER
    (OUTPUT op-bal AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetPrUOM d-oeitem 
FUNCTION fGetPrUOM RETURNS CHARACTER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetRelDate d-oeitem 
FUNCTION fGetRelDate RETURNS DATE
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetReturned d-oeitem 
FUNCTION fGetReturned RETURNS DECIMAL
    (ipcValueNeeded AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetReturnedInv d-oeitem 
FUNCTION fGetReturnedInv RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTaxable d-oeitem 
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER, ipcCust AS CHARACTER , ipcShipto AS CHARACTER, ipcFGItemID AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTotalReturned d-oeitem 
FUNCTION fGetTotalReturned RETURNS DECIMAL
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetWIP d-oeitem 
FUNCTION fGetWIP RETURNS INTEGER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetXferQty d-oeitem 
FUNCTION fGetXferQty RETURNS INTEGER
    (  ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fIsCustPriceHoldExempt d-oeitem 
FUNCTION fIsCustPriceHoldExempt RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER, ipcCustomerID AS CHARACTER, ipcShipToID AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fnPrevOrder d-oeitem 
FUNCTION fnPrevOrder RETURNS CHARACTER
    (ipcEstNo AS CHARACTER, ipiOrdNo AS INTEGER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fOEScreenUOMConvert d-oeitem 
FUNCTION fOEScreenUOMConvert RETURNS DECIMAL
    ( ipdStartQuantity AS DECIMAL , ipcUOM AS CHARACTER, ipdCount AS DECIMAL, ipcItemID AS CHARACTER) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUseNewEstimating d-oeitem 
FUNCTION fUseNewEstimating RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-colonial-rel-date d-oeitem 
FUNCTION get-colonial-rel-date RETURNS DATE
    ( iprRel AS ROWID)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-itemfg-cost d-oeitem 
FUNCTION get-itemfg-cost RETURNS DECIMAL
    ( ipv-item AS CHARACTER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-quotes 
     LABEL "Quoted Prices" 
     SIZE 18 BY 1.14
     FONT 1.

DEFINE BUTTON btnAllocated 
     LABEL "Allocated" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnJobs 
     LABEL "Jobs" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnLocations 
     LABEL "Locations" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnPOs 
     LABEL "POs" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnReleases 
     LABEL "Releases" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnTags 
     IMAGE-UP FILE "Graphics/16x16/question.png":U
     LABEL "" 
     SIZE 4.2 BY .95 TOOLTIP "Show Details".

DEFINE BUTTON btnTagsOverrn 
     IMAGE-UP FILE "Graphics/16x16/question.png":U
     LABEL "" 
     SIZE 4.2 BY .95 TOOLTIP "Show Details".

DEFINE BUTTON btnTagsUnder 
     IMAGE-UP FILE "Graphics/16x16/question.png":U
     LABEL "" 
     SIZE 4.2 BY .95 TOOLTIP "Show Details".

DEFINE BUTTON btnViewDetail 
     LABEL "View Details" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Ca&ncel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Done AUTO-END-KEY DEFAULT 
     LABEL "&Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_hist 
     LABEL "&History" 
     SIZE 15 BY 1
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "&Save" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fiPrevOrder AS CHARACTER FORMAT "X(256)":U 
     LABEL "Prev Order" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fiPromDtLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Promise Date:" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_jobStartDate AS DATE FORMAT "99/99/9999":U 
     LABEL "Job Start Date" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi_qty-uom AS CHARACTER FORMAT "x(4)" INITIAL "EA" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi_s-comm-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Comm.%" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_s-pct-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "% of Sales" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sman-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Sales Rep" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_sname-1 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE fi_sname-2 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE fi_sname-3 AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1
     BGCOLOR 15 FGCOLOR 1 .

DEFINE VARIABLE fi_sname-lbl AS CHARACTER FORMAT "X(256)":U INITIAL "Name" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .71
     FGCOLOR 9  NO-UNDO.

DEFINE VARIABLE fi_type-dscr AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE spare-dec-1 LIKE itemfg.spare-dec-1
     LABEL "Full Cost" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 15 FGCOLOR 1  NO-UNDO.

DEFINE VARIABLE iPrintAvailQty AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "All Locations", 1,
"Non-Zero", 2,
"Negative", 3,
"Zero Balance", 4
     SIZE 77 BY .91 NO-UNDO.

DEFINE RECTANGLE RECT-31
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 77.2 BY 4.86.

DEFINE RECTANGLE RECT-39
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 77.2 BY 10.91.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 64 BY 10.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 64 BY 5.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseAllocated FOR 
      ttAllocated SCROLLING.

DEFINE QUERY browseJobs FOR 
      job-hdr, 
      job SCROLLING.

DEFINE QUERY browseLocations FOR 
      w-jobs SCROLLING.

DEFINE QUERY browsePOs FOR 
      po-ordl, 
      po-ord SCROLLING.

DEFINE QUERY browseReleases FOR 
      oe-rel SCROLLING.

DEFINE QUERY d-oeitem FOR 
      oe-ordl, 
      oe-ord, 
      itemfg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseAllocated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseAllocated d-oeitem _FREEFORM
  QUERY browseAllocated NO-LOCK DISPLAY
      ttAllocated.ord-no
    ttAllocated.cust-no
    ttAllocated.cust-name
    ttAllocated.qty
    ttAllocated.ship-qty
    ttAllocated.due-date
    ttAllocated.price
    ttAllocated.pr-uom
    ttAllocated.part-no
    ttAllocated.po-no
    ttAllocated.allocated
    ttAllocated.loc
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 15.95
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE BGCOLOR 15 FGCOLOR 1 "Allocated".

DEFINE BROWSE browseJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseJobs d-oeitem _STRUCTURED
  QUERY browseJobs NO-LOCK DISPLAY
      job-hdr.job-no FORMAT "x(6)":U
      job-hdr.job-no2 FORMAT ">9":U
      job-hdr.est-no FORMAT "x(5)":U
      job-hdr.ord-no FORMAT ">>>>>9":U
      job-hdr.cust-no FORMAT "x(8)":U
      job-hdr.due-date FORMAT "99/99/9999":U
      job.stat FORMAT "x":U
      job-hdr.qty FORMAT ">>,>>>,>>9":U
      job-hdr.i-no FORMAT "x(15)":U
      job-hdr.opened COLUMN-LABEL "Open" FORMAT "yes/no":U VIEW-AS TOGGLE-BOX
      job-hdr.loc COLUMN-LABEL "Whse" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 15.95
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE BGCOLOR 15 FGCOLOR 1 "Jobs".

DEFINE BROWSE browseLocations
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseLocations d-oeitem _FREEFORM
  QUERY browseLocations DISPLAY
      w-jobs.loc LABEL "Whse" WIDTH 10
    w-jobs.loc-desc LABEL "Name"
    w-jobs.onHand
    w-jobs.onOrder
    w-jobs.allocated
    w-jobs.backOrder
    w-jobs.qtyAvailable
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 15
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE BGCOLOR 15 FGCOLOR 1 "Locations".

DEFINE BROWSE browsePOs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browsePOs d-oeitem _STRUCTURED
  QUERY browsePOs NO-LOCK DISPLAY
      po-ordl.po-no FORMAT ">>>>>>>9":U
      po-ordl.vend-no FORMAT "x(8)":U
      po-ordl.due-date FORMAT "99/99/9999":U
      po-ordl.job-no FORMAT "x(6)":U
      po-ordl.ord-qty FORMAT "->>>,>>>,>>9.9<<<<<":U
      po-ordl.t-rec-qty FORMAT "->>>,>>>,>>9.9<<<<<":U
      po-ordl.cost FORMAT "->,>>>,>>9.99<<<<":U
      po-ordl.line FORMAT "99":U
      po-ordl.ord-qty - po-ordl.t-rec-qty @ dRemainingQty COLUMN-LABEL "Remaining Quantity" FORMAT "->>>,>>>,>>9.9<<<<<":U
      po-ord.loc FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 15.95
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE BGCOLOR 15 FGCOLOR 1 "POs".

DEFINE BROWSE browseReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseReleases d-oeitem _STRUCTURED
  QUERY browseReleases NO-LOCK DISPLAY
      oe-rel.spare-char-1 COLUMN-LABEL "From" FORMAT "x(8)":U
      oe-ord.cust-no COLUMN-LABEL "Customer" FORMAT "x(8)":U
      oe-ord.ord-no COLUMN-LABEL "Order" FORMAT ">>>>>9":U
      oe-rel.tot-qty COLUMN-LABEL "Release!Qty" FORMAT "->>,>>>,>>9":U
      oe-rel.stat FORMAT "X":U
      oe-ordl.qty FORMAT "->>,>>>,>>9.9<<":U
      oe-ordl.ship-qty COLUMN-LABEL "Shipped!Quantity" FORMAT "->>,>>>,>>9.99":U
      fGetRelDate() @ dtRelDate
      oe-ordl.req-date COLUMN-LABEL "Request!Date" FORMAT "99/99/9999":U
      fGetXferQty() @ iXferQty
      fGetPriceDisc() @ dPrice
      fGetPrUOM() @ cUOM
      oe-ordl.t-price COLUMN-LABEL "Extended!Price" FORMAT "->>,>>>,>>9.99":U
      oe-ordl.i-no COLUMN-LABEL "Item" FORMAT "x(15)":U
      oe-ordl.part-no COLUMN-LABEL "Customer Part" FORMAT "x(15)":U
      oe-ordl.po-no COLUMN-LABEL "Customer PO" FORMAT "x(15)":U
      oe-ord.po-no COLUMN-LABEL "Order PO" FORMAT "x(15)":U
      oe-ordl.est-no COLUMN-LABEL "Estimate" FORMAT "x(5)":U
      oe-ordl.job-no COLUMN-LABEL "Job!Number" FORMAT "x(6)":U
      oe-ordl.job-no2 COLUMN-LABEL "Run" FORMAT ">9":U
      oe-ord.ord-date COLUMN-LABEL "Order Date" FORMAT "99/99/9999":U
      oe-ord.stat COLUMN-LABEL "Order!Status" FORMAT "x":U
      oe-ordl.inv-qty COLUMN-LABEL "Invoice!Quantity" FORMAT "->>,>>>,>>9.99":U
      fGetProd(li-bal) @ iProdQty
      fGetBal(li-qoh) @ iBalQty
      fGetActRelQty() @ iActRelQty
      fGetWIP() @ iWIP
      fGetPct(li-bal) @ iPct
      oe-ordl.i-name COLUMN-LABEL "Item Name" FORMAT "x(30)":U
      oe-ordl.line FORMAT "99":U
      fGetCost() @ dInvLineCost
      fGetCostUOM() @ cCostUOM
      oe-ordl.po-no-po COLUMN-LABEL "Board PO" FORMAT ">>>>>>>9":U
      fGetLastShipTo() @ cLastShipTo
      fGetTotalReturned() @ dTotQtyRet
      fGetReturnedInv() @ dTotRetInv
      fGetActRelQty() - fGetActBOLQty() @ iHandQtyNoAlloc
      oe-ordl.cost FORMAT "->>>,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 127 BY 15.95
         BGCOLOR 15 FGCOLOR 1 FONT 6
         TITLE BGCOLOR 15 FGCOLOR 1 "Releases".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME d-oeitem
     browseLocations AT ROW 1.24 COL 145
     browseJobs AT ROW 1.24 COL 145
     iPrintAvailQty AT ROW 16.24 COL 147 NO-LABEL
     fiPrevOrder AT ROW 9.57 COL 93.6 COLON-ALIGNED WIDGET-ID 28
     fiPromDtLabel AT ROW 14.81 COL 104 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     fi_type-dscr AT ROW 6.71 COL 119.6 COLON-ALIGNED NO-LABEL
     oe-ordl.est-no AT ROW 1.48 COL 15.8 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.sourceEstimateID AT ROW 1.48 COL 60.8 COLON-ALIGNED
          LABEL "Pricing Est ID" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.job-no AT ROW 1.38 COL 95.2 COLON-ALIGNED FORMAT "x(6)"
          VIEW-AS FILL-IN 
          SIZE 16.6 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.job-no2 AT ROW 1.38 COL 112.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.qty AT ROW 2.43 COL 15.8 COLON-ALIGNED
          LABEL "Quantity" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
          BGCOLOR 15 FGCOLOR 1 
     fi_qty-uom AT ROW 2.43 COL 33.4 COLON-ALIGNED HELP
          "Enter Unit of Measure for Purchasing this Raw Material" NO-LABEL
     oe-ordl.i-no AT ROW 3.38 COL 15.8 COLON-ALIGNED
          LABEL "FG Item#" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.part-no AT ROW 4.33 COL 15.8 COLON-ALIGNED
          LABEL "Cust Part #" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 28 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.i-name AT ROW 5.29 COL 15.6 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.part-dscr1 AT ROW 6.24 COL 15.6 COLON-ALIGNED
          LABEL "Desc 1" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1.05
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.part-dscr2 AT ROW 7.24 COL 15.6 COLON-ALIGNED
          LABEL "Desc 2"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1.05
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.part-dscr3 AT ROW 8.24 COL 15.6 COLON-ALIGNED WIDGET-ID 14
          LABEL "Desc 3"
          VIEW-AS FILL-IN 
          SIZE 51 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.po-no AT ROW 9.57 COL 15.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.e-num AT ROW 9.57 COL 55.2 COLON-ALIGNED HELP
          "Customer PO Line Number"
          LABEL "Ln#" FORMAT ">>>>>"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.po-no-po AT ROW 10.76 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.vend-no AT ROW 10.76 COL 43.4 COLON-ALIGNED
          LABEL "Vendor"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.price AT ROW 2.81 COL 93.2 COLON-ALIGNED FORMAT "->>,>>>,>>9.99<<<<"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.pr-uom AT ROW 2.81 COL 121.8 COLON-ALIGNED
          LABEL "UOM" FORMAT "XXX"
          VIEW-AS FILL-IN 
          SIZE 8.4 BY 1
          BGCOLOR 15 FGCOLOR 1 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME d-oeitem
     oe-ordl.tax AT ROW 2.81 COL 133
          LABEL "Tax"
          VIEW-AS TOGGLE-BOX
          SIZE 9 BY .81
     oe-ordl.disc AT ROW 3.76 COL 93.2 COLON-ALIGNED FORMAT ">>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.cas-cnt AT ROW 3.76 COL 127.4 COLON-ALIGNED
          LABEL "Qty/Unit" FORMAT ">>>,>>>"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.t-price AT ROW 4.71 COL 93.2 COLON-ALIGNED
          LABEL "Total Price"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.partial AT ROW 4.71 COL 127.4 COLON-ALIGNED
          LABEL "Partial" FORMAT "->>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.cost AT ROW 5.67 COL 93.2 COLON-ALIGNED
          LABEL "Cost/M"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.cases-unit AT ROW 5.67 COL 127.4 COLON-ALIGNED
          LABEL "Units/Pallet" FORMAT ">>>>"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 FGCOLOR 1 
     spare-dec-1 AT ROW 6.71 COL 93.2 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Full Cost" FORMAT "->>,>>9.99"
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.type-code AT ROW 6.71 COL 113.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1 TOOLTIP "(O)riginal, (R)epeat, Repeat with (C)hange, inhouse (T)ransfer"
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.customField AT ROW 7.91 COL 93.2 COLON-ALIGNED
          LABEL "Custom1"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.managed AT ROW 9.24 COL 115.8
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .81
     oe-ordl.whsed AT ROW 10.1 COL 115.8 HELP
          "Is line item warehoused?" WIDGET-ID 2
          LABEL "Run && Ship"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     oe-ordl.s-man[1] AT ROW 13.38 COL 2.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-pct[1] AT ROW 13.38 COL 45.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-comm[1] AT ROW 13.38 COL 60.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-man[2] AT ROW 14.38 COL 2.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-pct[2] AT ROW 14.38 COL 45.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-comm[2] AT ROW 14.38 COL 60.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-man[3] AT ROW 15.38 COL 2.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 15 FGCOLOR 1 
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME d-oeitem
     oe-ordl.s-pct[3] AT ROW 15.38 COL 45.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.s-comm[3] AT ROW 15.38 COL 60.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          BGCOLOR 15 FGCOLOR 1 
     fi_s-pct-lbl AT ROW 12.67 COL 45.4 COLON-ALIGNED NO-LABEL
     fi_s-comm-lbl AT ROW 12.67 COL 60.4 COLON-ALIGNED NO-LABEL
     fi_sman-lbl AT ROW 12.62 COL 1.4 COLON-ALIGNED NO-LABEL
     fi_sname-1 AT ROW 13.38 COL 13.4 COLON-ALIGNED NO-LABEL
     fi_sname-2 AT ROW 14.38 COL 13.4 COLON-ALIGNED NO-LABEL
     fi_sname-3 AT ROW 15.38 COL 13.4 COLON-ALIGNED NO-LABEL
     fi_sname-lbl AT ROW 12.62 COL 18.4 COLON-ALIGNED NO-LABEL
     oe-ordl.over-pct AT ROW 11.57 COL 121.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.under-pct AT ROW 12.67 COL 121.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.req-code AT ROW 13.62 COL 93.4 COLON-ALIGNED
          LABEL "Priority" FORMAT "XXXXX"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.prom-code AT ROW 14.71 COL 93.4 COLON-ALIGNED
          LABEL "Priority" FORMAT "XXXXX"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.req-date AT ROW 13.71 COL 121.2 COLON-ALIGNED
          LABEL "Due Date"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.prom-date AT ROW 14.76 COL 121.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     Btn_OK AT ROW 17.43 COL 52
     Btn_Done AT ROW 17.43 COL 72
     Btn_Cancel AT ROW 17.43 COL 92.2
     Btn_hist AT ROW 3.38 COL 46.8
     oe-ordl.spare-char-1 AT ROW 1.43 COL 133.4 COLON-ALIGNED WIDGET-ID 12
          LABEL "Status" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 5.8 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.spare-dec-1 AT ROW 2.43 COL 51.2 COLON-ALIGNED WIDGET-ID 18
          LABEL "Cust" FORMAT "->>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FGCOLOR 1 
     oe-ordl.spare-char-2 AT ROW 2.43 COL 69.2 COLON-ALIGNED NO-LABEL WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FGCOLOR 1 
     fi_jobStartDate AT ROW 15.91 COL 121.2 COLON-ALIGNED WIDGET-ID 22
     btn-quotes AT ROW 17.43 COL 2 WIDGET-ID 20
     btnTagsOverrn AT ROW 11.57 COL 137.8 WIDGET-ID 36
     btnTagsUnder AT ROW 12.57 COL 137.8 WIDGET-ID 38
     btnTags AT ROW 2.76 COL 113.2 WIDGET-ID 40
     btnViewDetail AT ROW 17.43 COL 129
     btnLocations AT ROW 17.43 COL 145
     btnJobs AT ROW 17.43 COL 161
     btnPOs AT ROW 17.43 COL 177
     btnAllocated AT ROW 17.43 COL 193
     btnReleases AT ROW 17.43 COL 209
     browsePOs AT ROW 1.24 COL 145
     browseAllocated AT ROW 1.24 COL 145
     browseReleases AT ROW 1.24 COL 145
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         DEFAULT-BUTTON Btn_Done CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME d-oeitem
     RECT-31 AT ROW 12.33 COL 2
     RECT-39 AT ROW 1.24 COL 2
     RECT-40 AT ROW 1.24 COL 80.2 WIDGET-ID 8
     RECT-41 AT ROW 11.38 COL 80.2 WIDGET-ID 10
     SPACE(127.80) SKIP(1.38)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Order Item Detail"
         DEFAULT-BUTTON Btn_Done CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX d-oeitem
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB browseLocations 1 d-oeitem */
/* BROWSE-TAB browseJobs browseLocations d-oeitem */
/* BROWSE-TAB browsePOs RECT-41 d-oeitem */
/* BROWSE-TAB browseAllocated browsePOs d-oeitem */
/* BROWSE-TAB browseReleases browseAllocated d-oeitem */
ASSIGN 
       FRAME d-oeitem:SCROLLABLE       = FALSE.

/* SETTINGS FOR BROWSE browseAllocated IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       browseAllocated:HIDDEN  IN FRAME d-oeitem                = TRUE.

/* SETTINGS FOR BROWSE browseJobs IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       browseJobs:HIDDEN  IN FRAME d-oeitem                = TRUE.

/* SETTINGS FOR BROWSE browseLocations IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       browseLocations:HIDDEN  IN FRAME d-oeitem                = TRUE.

/* SETTINGS FOR BROWSE browsePOs IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       browsePOs:HIDDEN  IN FRAME d-oeitem                = TRUE.

/* SETTINGS FOR BROWSE browseReleases IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       browseReleases:HIDDEN  IN FRAME d-oeitem                = TRUE.

/* SETTINGS FOR BUTTON btnAllocated IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnAllocated:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR BUTTON btnJobs IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnJobs:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR BUTTON btnLocations IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnLocations:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR BUTTON btnPOs IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnPOs:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR BUTTON btnReleases IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnReleases:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR BUTTON btnTags IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnTagsOverrn IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btnViewDetail IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       btnViewDetail:HIDDEN IN FRAME d-oeitem           = TRUE.

ASSIGN 
       Btn_Done:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR FILL-IN oe-ordl.cas-cnt IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.cases-unit IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.cost IN FRAME d-oeitem
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN oe-ordl.customField IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.disc IN FRAME d-oeitem
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ordl.e-num IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN oe-ordl.est-no IN FRAME d-oeitem
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fiPrevOrder IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiPromDtLabel IN FRAME d-oeitem
   NO-ENABLE                                                            */
ASSIGN 
       fiPromDtLabel:READ-ONLY IN FRAME d-oeitem        = TRUE.

/* SETTINGS FOR FILL-IN fi_s-comm-lbl IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_s-pct-lbl IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sman-lbl IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_sname-1 IN FRAME d-oeitem
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-2 IN FRAME d-oeitem
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-3 IN FRAME d-oeitem
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname-lbl IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_type-dscr IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-name IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.i-no IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET iPrintAvailQty IN FRAME d-oeitem
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       iPrintAvailQty:HIDDEN IN FRAME d-oeitem           = TRUE.

/* SETTINGS FOR FILL-IN oe-ordl.job-no IN FRAME d-oeitem
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN oe-ordl.job-no2 IN FRAME d-oeitem
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr1 IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr2 IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-dscr3 IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.part-no IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.partial IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.pr-uom IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       oe-ordl.pr-uom:PRIVATE-DATA IN FRAME d-oeitem     = 
                "111".

/* SETTINGS FOR FILL-IN oe-ordl.price IN FRAME d-oeitem
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-ordl.prom-code IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.prom-date IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.qty IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RECTANGLE RECT-31 IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-39 IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-40 IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-41 IN FRAME d-oeitem
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.req-code IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.req-date IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.sourceEstimateID IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.spare-char-1 IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.spare-char-2 IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN spare-dec-1 IN FRAME d-oeitem
   NO-ENABLE LIKE = asi.itemfg. EXP-LABEL EXP-FORMAT                    */
/* SETTINGS FOR FILL-IN oe-ordl.spare-dec-1 IN FRAME d-oeitem
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-ordl.t-price IN FRAME d-oeitem
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR TOGGLE-BOX oe-ordl.tax IN FRAME d-oeitem
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-ordl.type-code IN FRAME d-oeitem
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN oe-ordl.vend-no IN FRAME d-oeitem
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR TOGGLE-BOX oe-ordl.whsed IN FRAME d-oeitem
   EXP-LABEL EXP-HELP                                                   */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseAllocated
/* Query rebuild information for BROWSE browseAllocated
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttAllocated.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Query            is NOT OPENED
*/  /* BROWSE browseAllocated */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseJobs
/* Query rebuild information for BROWSE browseJobs
     _TblList          = "ASI.job-hdr,ASI.job OF ASI.job-hdr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Where[1]         = "job-hdr.company EQ itemfg.company AND
job-hdr.i-no EQ cFGItem AND
job-hdr.opened EQ YES AND
(job-hdr.loc EQ w-jobs.loc OR
w-jobs.loc EQ ""*ALL"")"
     _FldNameList[1]   = ASI.job-hdr.job-no
     _FldNameList[2]   = ASI.job-hdr.job-no2
     _FldNameList[3]   = ASI.job-hdr.est-no
     _FldNameList[4]   = ASI.job-hdr.ord-no
     _FldNameList[5]   = ASI.job-hdr.cust-no
     _FldNameList[6]   = ASI.job-hdr.due-date
     _FldNameList[7]   = ASI.job.stat
     _FldNameList[8]   = ASI.job-hdr.qty
     _FldNameList[9]   = ASI.job-hdr.i-no
     _FldNameList[10]   > ASI.job-hdr.opened
"job-hdr.opened" "Open" "yes/no" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "TOGGLE-BOX" "," ? ? 5 no 0 no no
     _FldNameList[11]   > ASI.job-hdr.loc
"job-hdr.loc" "Whse" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE browseJobs */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseLocations
/* Query rebuild information for BROWSE browseLocations
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH w-jobs
WHERE ((w-jobs.qtyAvailable NE 0 AND iPrintAvailQty EQ 2)
OR (w-jobs.qtyAvailable LT 0 AND iPrintAvailQty EQ 3)
OR (w-jobs.qtyAvailable EQ 0 AND iPrintAvailQty EQ 4)
OR (iPrintAvailQty EQ 1)).
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE browseLocations */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browsePOs
/* Query rebuild information for BROWSE browsePOs
     _TblList          = "ASI.po-ordl,ASI.po-ord WHERE ASI.po-ordl ..."
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST"
     _Where[1]         = "po-ordl.company EQ itemfg.company AND
po-ordl.i-no EQ cFGItem AND
po-ordl.item-type EQ NO AND
po-ordl.opened EQ YES"
     _Where[2]         = "po-ord.company EQ po-ordl.company AND
po-ord.po-no EQ po-ordl.po-no AND
(po-ord.loc EQ w-jobs.loc OR
w-jobs.loc EQ ""*ALL"")"
     _FldNameList[1]   = ASI.po-ordl.po-no
     _FldNameList[2]   = ASI.po-ordl.vend-no
     _FldNameList[3]   = ASI.po-ordl.due-date
     _FldNameList[4]   = ASI.po-ordl.job-no
     _FldNameList[5]   = ASI.po-ordl.ord-qty
     _FldNameList[6]   = ASI.po-ordl.t-rec-qty
     _FldNameList[7]   = ASI.po-ordl.cost
     _FldNameList[8]   = ASI.po-ordl.line
     _FldNameList[9]   > "_<CALC>"
"po-ordl.ord-qty - po-ordl.t-rec-qty @ dRemainingQty" "Remaining Quantity" "->>>,>>>,>>9.9<<<<<" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.po-ord.loc
     _Query            is NOT OPENED
*/  /* BROWSE browsePOs */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseReleases
/* Query rebuild information for BROWSE browseReleases
     _TblList          = "ASI.oe-rel"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _TblOptList       = ", FIRST,"
     _Where[1]         = "oe-rel.company EQ oe-ordl.company AND
oe-rel.ord-no EQ oe-ordl.ord-no AND
oe-rel.i-no EQ cFGItem AND
oe-rel.line EQ oe-ordl.line AND
(oe-rel.spare-char-1 EQ w-jobs.loc OR
w-jobs.loc EQ ""*All"" ) AND
LOOKUP(oe-rel.s-code,""B,S"") NE 0 AND
LOOKUP(oe-rel.stat,""S,A,L,B,Z"") NE 0"
     _FldNameList[1]   > ASI.oe-rel.spare-char-1
"oe-rel.spare-char-1" "From" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.oe-ord.cust-no
"oe-ord.cust-no" "Customer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.oe-ord.ord-no
"oe-ord.ord-no" "Order" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.oe-rel.tot-qty
"oe-rel.tot-qty" "Release!Qty" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ASI.oe-rel.stat
     _FldNameList[6]   = ASI.oe-ordl.qty
     _FldNameList[7]   > ASI.oe-ordl.ship-qty
"oe-ordl.ship-qty" "Shipped!Quantity" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
"fGetRelDate() @ dtRelDate" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > ASI.oe-ordl.req-date
"oe-ordl.req-date" "Request!Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > "_<CALC>"
"fGetXferQty() @ iXferQty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > "_<CALC>"
"fGetPriceDisc() @ dPrice" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > "_<CALC>"
"fGetPrUOM() @ cUOM" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > ASI.oe-ordl.t-price
"oe-ordl.t-price" "Extended!Price" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > ASI.oe-ordl.i-no
"oe-ordl.i-no" "Item" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > ASI.oe-ordl.part-no
"oe-ordl.part-no" "Customer Part" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > ASI.oe-ordl.po-no
"oe-ordl.po-no" "Customer PO" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > ASI.oe-ord.po-no
"oe-ord.po-no" "Order PO" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > ASI.oe-ordl.est-no
"oe-ordl.est-no" "Estimate" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > ASI.oe-ordl.job-no
"oe-ordl.job-no" "Job!Number" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "Run" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > ASI.oe-ord.ord-date
"oe-ord.ord-date" "Order Date" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > ASI.oe-ord.stat
"oe-ord.stat" "Order!Status" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > ASI.oe-ordl.inv-qty
"oe-ordl.inv-qty" "Invoice!Quantity" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > "_<CALC>"
"fGetProd(li-bal) @ iProdQty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > "_<CALC>"
"fGetBal(li-qoh) @ iBalQty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > "_<CALC>"
"fGetActRelQty() @ iActRelQty" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > "_<CALC>"
"fGetWIP() @ iWIP" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > "_<CALC>"
"fGetPct(li-bal) @ iPct" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > ASI.oe-ordl.i-name
"oe-ordl.i-name" "Item Name" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[30]   = ASI.oe-ordl.line
     _FldNameList[31]   > "_<CALC>"
"fGetCost() @ dInvLineCost" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[32]   > "_<CALC>"
"fGetCostUOM() @ cCostUOM" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[33]   > ASI.oe-ordl.po-no-po
"oe-ordl.po-no-po" "Board PO" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[34]   > "_<CALC>"
"fGetLastShipTo() @ cLastShipTo" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[35]   > "_<CALC>"
"fGetTotalReturned() @ dTotQtyRet" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[36]   > "_<CALC>"
"fGetReturnedInv() @ dTotRetInv" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[37]   > "_<CALC>"
"fGetActRelQty() - fGetActBOLQty() @ iHandQtyNoAlloc" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[38]   = ASI.oe-ordl.cost
     _Query            is NOT OPENED
*/  /* BROWSE browseReleases */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX d-oeitem
/* Query rebuild information for DIALOG-BOX d-oeitem
     _TblList          = "ASI.oe-ordl,asi.oe-ord OF ASI.oe-ordl,asi.itemfg OF ASI.oe-ordl"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX d-oeitem */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME d-oeitem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-oeitem d-oeitem
ON HELP OF FRAME d-oeitem /* Order Item Detail */
DO:
        DEFINE VARIABLE char-val    AS cha           NO-UNDO.
        DEFINE VARIABLE look-recid  AS RECID         NO-UNDO.
        DEFINE VARIABLE lw-focus    AS WIDGET-HANDLE NO-UNDO.
        DEFINE VARIABLE cMainField  AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE cAllFields  AS CHARACTER     NO-UNDO.
        DEFINE VARIABLE recRecordID AS RECID         NO-UNDO.   
  
        FIND oe-ord NO-LOCK
            WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

        DO WITH FRAME {&FRAME-NAME}:
            lw-focus = FOCUS.
            CASE lw-focus:NAME :
                WHEN "est-no" THEN 
                    DO:
                        RUN windows/l-estcst.w (g_company,g_loc,oe-ord.cust-no,0,lw-focus:SCREEN-VALUE, OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO:
                            lCheckMessage = NO.
                            RUN display-est-detail (char-val).
                        END. 
                    END. 
                WHEN "type-code" THEN 
                    DO:
                        RUN windows/l-ordtyp.w (oe-ordl.type-code:SCREEN-VALUE, OUTPUT char-val).
                        IF char-val NE "" AND oe-ordl.type-code:SCREEN-VALUE NE ENTRY(1,char-val) THEN 
                        DO:
                            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                            RUN new-type.
                        END.
                    END.   

                WHEN "spare-char-1" THEN 
                    DO:

                        RUN windows/l-holdtype.w (OUTPUT char-val).
                        /* If value selected, set code to first entry of string,
                           set tooltip to second entry of string (description). */
                        IF char-val <> "" THEN
                            ASSIGN lw-focus:SCREEN-VALUE = TRIM(char-val)
                                lw-focus:TOOLTIP      = getOrdStatDescr(TRIM(char-val)).

                    /*                 ASSIGN lw-focus:SCREEN-VALUE = SUBSTRING(char-val,1,1). */
                    END.

                WHEN "qty" THEN 
                    DO:
                        IF oe-ordl.est-no:SCREEN-VALUE <> "" AND NOT lQuotePriceMatrix THEN 
                        DO:
                            RUN windows/l-ordqty.w (g_company, oe-ordl.est-no:screen-value, lw-focus:screen-valu, OUTPUT char-val).
                            IF char-val NE "" THEN 
                            DO:
                                ASSIGN
                                    lw-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                                    oe-ordl.price:screen-value  = ENTRY(2,char-val)
                                    oe-ordl.pr-uom:screen-value = IF ENTRY(3,char-val) <> "" THEN ENTRY(3,char-val) ELSE "M"                        
                                    ll-help-ran                 = YES
                                    lv-help-qty                 = INT(ENTRY(1,char-val)).
                                IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                                    oeestcom-log = YES THEN
                                    RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).

                                APPLY "tab" TO oe-ordl.qty.
                            END.
                        END.
                        ELSE 
                        DO:
                            FIND FIRST cust NO-LOCK
                                WHERE cust.company = oe-ord.company
                                AND cust.cust-no = oe-ord.cust-no NO-ERROR. 
                     
                            FIND FIRST itemfg  NO-LOCK
                                WHERE itemfg.company EQ g_company
                                AND itemfg.i-no    EQ oe-ordl.i-no:SCREEN-VALUE NO-ERROR.     
                            RUN windows/lOePrmtx.w (g_company, oe-ordl.i-no:screen-value,oe-ord.cust-no,(IF AVAILABLE cust THEN cust.TYPE ELSE ""),
                                (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),oe-ord.ship-id, OUTPUT char-val).
                            IF char-val NE "" THEN 
                            DO:
                                ASSIGN        
                                    oe-ordl.qty:screen-value    = ENTRY(1,char-val)
                                    oe-ordl.price:screen-value  = ENTRY(2,char-val)
                                    oe-ordl.pr-uom:screen-value = ENTRY(3,char-val) .
                            END.  
                                         
                        END.
                    END.
                WHEN "i-no" THEN 
                    DO:
                        RUN windows/l-itemfa.w (g_company, oe-ord.cust-no, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
                        IF char-val <> "" THEN 
                        DO:
                            ASSIGN 
                                lw-focus:SCREEN-VALUE       = ENTRY(1,char-val)
                                oe-ordl.i-name:screen-value = ENTRY(2,char-val).
                            RUN display-fgitem NO-ERROR.
                            IF btnViewDetail:LABEL EQ "Close Detail" THEN
                            RUN pViewDetail ("Locations").
                            IF NOT ERROR-STATUS:ERROR THEN 
                            DO:
                                IF AVAILABLE oe-ord THEN
                                    RUN pGetOverUnderPct(oe-ord.cust-no,oe-ord.ship-id,oe-ord.ord-no) .
                                ll-ok-i-no = YES.
                                IF oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
                                DO:
                   
                                    IF oescreen-cha EQ "item-qty" THEN
                                        APPLY "entry" TO oe-ordl.qty.
                                    ELSE IF oe-ordl.price:SENSITIVE THEN 
                                            APPLY "entry" TO oe-ordl.price.
                                END.
                            END.
                        END. 
                    END.
                WHEN "part-no" THEN 
                    DO:
                        /*                IF ll-new-file THEN                                                                                                                        */
                        /*                  run windows/l-cstprt.w (g_company, oe-ord.cust-no, lw-focus:screen-value,oe-ordl.i-no:screen-value, output char-val, output look-recid). */
                        /*                ELSE                                                                                                                                      */
              
                        RUN system/openlookup.p (INPUT  "",  /* company */ 
                            INPUT  "",  /* lookup field */
                            INPUT  152, /* Subject ID */
                            INPUT  "",  /* User ID */
                            INPUT  0,   /* Param value ID */ 
                            OUTPUT cAllFields, 
                            OUTPUT cMainField,  
                            OUTPUT recRecordID).               
                        IF cMainField <> "" THEN 
                        DO:
                            ASSIGN 
                                lw-focus:SCREEN-VALUE           = cMainField
                                oe-ordl.part-dscr1:screen-value = IF NUM-ENTRIES(cAllFields,"|") GE 2 THEN
                                                           ENTRY(2, cAllFields, "|")
                                                           ELSE ""
                                oe-ordl.part-dscr2:screen-value = IF NUM-ENTRIES(cAllFields,"|") GE 6 THEN
                                                           ENTRY(6, cAllFields, "|")
                                                           ELSE "".
                            IF oe-ordl.i-no:SCREEN-VALUE = "" OR oe-ordl.i-no:SCREEN-VALUE = "0" 
                                THEN oe-ordl.i-no:SCREEN-VALUE = IF NUM-ENTRIES(cAllFields,"|") GE 5 THEN
                                    ENTRY(5, cAllFields, "|")
                                    ELSE "".
                            RUN display-fgpart (look-recid).
                            IF oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
                            DO:
                                IF oescreen-cha EQ "item-qty" THEN
                                    APPLY "entry" TO oe-ordl.qty.
                                ELSE IF oe-ordl.price:SENSITIVE THEN 
                                        APPLY "entry" TO oe-ordl.price.
                            END.
                        END.
                    END.
                WHEN "s-man" THEN 
                    DO:
                        RUN system/openLookup.p (
                            INPUT  g_company, 
                            INPUT  "",  /* Lookup ID */
                            INPUT  29,  /* Subject ID */
                            INPUT  "",  /* User ID */
                            INPUT  0,   /* Param Value ID */
                            OUTPUT cAllFields, 
                            OUTPUT cMainField, 
                            OUTPUT recRecordID
                            ).
                        IF cMainField NE "" AND lw-focus:SCREEN-VALUE NE cMainField THEN 
                        DO:
                            lw-focus:SCREEN-VALUE = cMainField.
                            RUN new-s-man (lw-focus:INDEX).
                        END.
                    END.  
                WHEN "price" THEN 
                    DO:       /* oe/history2.p */              
                        RUN windows/l-report.w (g_company,oe-ord.cust-no,oe-ordl.i-no:screen-value,oe-ordl.pr-uom:screen-value,OUTPUT char-val).
                        IF char-val <> "" THEN 
                        DO:
                            lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                            IF oe-ordl.pr-uom:screen-value EQ "" THEN oe-ordl.pr-uom:screen-value = "M".

                            IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                                oeestcom-log = YES THEN
                                RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                        END.
                    END.
                WHEN "fi_qty-uom" THEN 
                    DO:
                        RUN get-valid-uom-tt (lw-focus, oe-ordl.i-no:SCREEN-VALUE, "Qty").
                        RUN windows/l-itemuom.w (g_company,oe-ordl.pr-uom:screen-value,INPUT TABLE ttUOMEffective, OUTPUT char-val).
                        IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                    END.
                WHEN "pr-uom" THEN 
                    DO:
                        RUN get-valid-uom-tt (lw-focus, oe-ordl.i-no:SCREEN-VALUE, "Price").
                        RUN windows/l-itemuom.w (g_company,oe-ordl.pr-uom:screen-value, INPUT TABLE ttUOMEffective, OUTPUT char-val).
                        IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                        IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                            oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                    END.
                WHEN "req-code" OR 
                WHEN "prom-code" THEN 
                    DO:
                        RUN windows/l-dcode.w (v-duelist, OUTPUT char-val).
                        IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).                                            
                    END.
                WHEN "vend-no" THEN 
                    DO:
                        RUN windows/l-vendno.w (g_company, "A", lw-focus:SCREEN-VALUE, OUTPUT char-val).
                        IF char-val <> "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).         
                    END.
                WHEN "po-no-po" THEN 
                    DO:
                        RUN windows/l-ponopo.w (g_company,YES,lw-focus:SCREEN-VALUE, OUTPUT char-val).
                        IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE        = ENTRY(1,char-val)
                                oe-ordl.vend-no:screen-value = ENTRY(2,char-val)  .         
                    END.
          
            END CASE.
        END.

        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-oeitem d-oeitem
ON RETURN OF FRAME d-oeitem /* Order Item Detail */
ANYWHERE
    DO:
        APPLY "tab" TO SELF.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL d-oeitem d-oeitem
ON WINDOW-CLOSE OF FRAME d-oeitem /* Order Item Detail */
DO:
        RUN exit-delete.

        APPLY "go":U TO FRAME {&FRAME-NAME}. 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-quotes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-quotes d-oeitem
ON CHOOSE OF btn-quotes IN FRAME d-oeitem /* Quoted Prices */
DO:
        RUN chooseQuotedPrice.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAllocated
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAllocated d-oeitem
ON CHOOSE OF btnAllocated IN FRAME d-oeitem /* Allocated */
DO:
        RUN pViewDetail ("Allocated").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnJobs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnJobs d-oeitem
ON CHOOSE OF btnJobs IN FRAME d-oeitem /* Jobs */
DO:
        RUN pViewDetail ("Jobs").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLocations
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLocations d-oeitem
ON CHOOSE OF btnLocations IN FRAME d-oeitem /* Locations */
DO:
        RUN pViewDetail ("Locations").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPOs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPOs d-oeitem
ON CHOOSE OF btnPOs IN FRAME d-oeitem /* POs */
DO:
        RUN pViewDetail ("POs").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnReleases
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnReleases d-oeitem
ON CHOOSE OF btnReleases IN FRAME d-oeitem /* Releases */
DO:
        RUN pViewDetail ("Releases").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTags
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTags d-oeitem
ON CHOOSE OF btnTags IN FRAME d-oeitem
DO:
        RUN system/d-TagViewer.w (
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "",
            INPUT "Price-Source"
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTagsOverrn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTagsOverrn d-oeitem
ON CHOOSE OF btnTagsOverrn IN FRAME d-oeitem
DO:
        RUN system/d-TagViewer.w (
            INPUT oe-ordl.rec_key,
            INPUT "",
            INPUT "Over Percentage"
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTagsUnder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTagsUnder d-oeitem
ON CHOOSE OF btnTagsUnder IN FRAME d-oeitem
DO:
        RUN system/d-TagViewer.w (
            INPUT oe-ordl.rec_key,
            INPUT "",
            INPUT "Under Percentage"
            ).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnViewDetail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnViewDetail d-oeitem
ON CHOOSE OF btnViewDetail IN FRAME d-oeitem /* View Details */
DO:
        RUN pViewDetail (IF SELF:LABEL EQ "View Detail" THEN "Locations" ELSE "").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel d-oeitem
ON CHOOSE OF Btn_Cancel IN FRAME d-oeitem /* Cancel */
DO:
        lv-add-mode = NO.
        RUN ClearTagsForGroup(
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "Price-Source"
            ).
        RUN ClearTagsForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "Under Percentage"
            ).
        RUN ClearTagsForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "Over Percentage"
            ).
        IF ip-type EQ  'Update' THEN       
            FOR EACH ttTag:
                RUN AddTagInfoForGroup(
                    INPUT ttTag.linkRecKey,
                    INPUT ttTag.linkTable,
                    INPUT ttTag.description,
                    INPUT "",
                    INPUT ttTag.groupCode
                    ). /*From TagProcs Super Proc*/
            END.
        RUN exit-delete.

        IF lv-new-tandem NE ? THEN 
        DO:
            FIND eb WHERE ROWID(eb) EQ lv-new-tandem NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN 
            DO:
                FIND FIRST ef OF eb NO-ERROR.
                IF AVAILABLE ef THEN DELETE ef.
            END.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done d-oeitem
ON CHOOSE OF Btn_Done IN FRAME d-oeitem /* Done */
DO:
        op-cancel = NO.

  &IF DEFINED (adm-panel) <> 0 &THEN
        RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
        APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_hist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_hist d-oeitem
ON CHOOSE OF Btn_hist IN FRAME d-oeitem /* History */
DO:
        DEFINE VARIABLE lv-rowid               AS ROWID     NO-UNDO.
        DEFINE VARIABLE lv-qty                 AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE io-rowid-list          AS CHARACTER NO-UNDO.
        DEFINE VARIABLE io-qty-list            AS CHARACTER NO-UNDO.
        DEFINE VARIABLE li                     AS INTEGER   NO-UNDO.

        DEFINE VARIABLE lr-save-xoeordl-buffer AS ROWID     NO-UNDO.
        DEFINE VARIABLE lv-on-screen-item      AS CHARACTER NO-UNDO. /* Others pulled in from history not shown */

        DEFINE BUFFER b-oe-ordl      FOR oe-ordl.
        DEFINE BUFFER bf-new-oe-ordl FOR oe-ordl.
  
        IF AVAILABLE oe-ordl THEN 
        DO WITH FRAME {&FRAME-NAME}:
    
            lv-rowid = ROWID(oe-ordl).
            io-rowid-list = STRING(ROWID(oe-ordl)).
    
            /* if there is an existing order line, pass this to the     */
            /* procedure so it won't be a multi-select                  */
            /* add an output parameter to this call with the rowid list */
            RUN oe/d-oehist.w (INPUT-OUTPUT io-rowid-list,THIS-PROCEDURE, OUTPUT io-qty-list).
            IF NUM-ENTRIES(io-rowid-list) GT 1 THEN
                lv-multi-select = TRUE.
            ELSE
                lv-multi-select = FALSE.

            EACH-SELECTED:
            DO li = 1 TO NUM-ENTRIES(io-rowid-list):
                IF io-rowid-list = "" THEN
                    LEAVE.
                lv-rowid = TO-ROWID(ENTRY(li, io-rowid-list)).
                lv-qty   = DECIMAL(ENTRY(li, io-qty-list)).
                /* do this for each rowid returned: */
                FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ lv-rowid NO-LOCK NO-ERROR.
                IF AVAILABLE b-oe-ordl THEN
                    RUN getQtyPrice (INPUT ROWID(b-oe-ordl)).

                IF li EQ 1 THEN 
                DO:

                    /* Process row currently on screen, i.e. first rowid returned */        
                    IF AVAILABLE b-oe-ordl THEN 
                    DO:
          
                        IF ip-type NE 'Update' THEN
                            ASSIGN oe-ordl.i-no:SCREEN-VALUE = b-oe-ordl.i-no
            
                                /* oe-ordl.i-no = b-oe-ordl.i-no */.
          
                        FIND FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no EQ oe-ordl.i-no:SCREEN-VALUE NO-ERROR.
                        IF NOT AVAILABLE itemfg OR oe-ordl.i-no:SCREEN-VALUE EQ "" THEN 
                        DO:                      
                            APPLY 'ENTRY' TO oe-ordl.i-no.
                            NEXT EACH-SELECTED.
                        END.
              
                        IF AVAILABLE itemfg THEN
                            ASSIGN
                                oe-ordl.part-no:SCREEN-VALUE    = itemfg.part-no
                                oe-ordl.i-name:SCREEN-VALUE     = itemfg.i-name
                                oe-ordl.part-dscr1:SCREEN-VALUE = itemfg.part-dscr1
                                oe-ordl.part-dscr2:SCREEN-VALUE = itemfg.part-dscr2
                                oe-ordl.part-dscr3:SCREEN-VALUE = itemfg.part-dscr3.
          
                        ASSIGN
                            oe-ordl.price:SCREEN-VALUE  = STRING(b-oe-ordl.price)
                            oe-ordl.pr-uom:SCREEN-VALUE = b-oe-ordl.pr-uom
                            price-ent                   = YES.
          
                        IF INTEGER(oe-ordl.qty:SCREEN-VALUE) EQ 0 THEN
                            oe-ordl.qty:SCREEN-VALUE = STRING(lv-qty /* b-oe-ordl.qty */) .
                        lv-on-screen-item = oe-ordl.i-no.
                        IF oe-ordl.est-no:SCREEN-VALUE NE "" 
                            AND oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
          
                    END. /* if avail */            
                    ELSE NEXT. /* RETURN NO-APPLY. */
        
                    IF setFromHistory AND INTEGER(oe-ordl.qty:SCREEN-VALUE) EQ 0 THEN
                        oe-ordl.qty:SCREEN-VALUE = STRING(historyQty).
        
                    ASSIGN
                        save_id      = RECID(oe-ordl)
                        v-i-item     = oe-ordl.i-no:SCREEN-VALUE
                        v-i-qty      = INTEGER(oe-ordl.qty:SCREEN-VALUE)
                        price-ent    = NO
                        matrixExists = NO.
                    FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ ROWID(oe-ordl)
                        EXCLUSIVE-LOCK NO-ERROR.
                    SAVE_id = RECID(oe-ordl).
                    /* Depends on v-i-item */   
                    IF v-i-item GT "" THEN
                        RUN oe/oe-price.p.
        
                    IF matrixExists THEN RUN get-price.
                    ELSE IF setFromHistory THEN 
                        DO:
          
                            MESSAGE 'No price exists in the price matrix,' SKIP
                                'Import Sell Price from History?' VIEW-AS ALERT-BOX
                                QUESTION BUTTONS YES-NO UPDATE setFromHistory.
                            IF setFromHistory THEN
                            DO:
                                ASSIGN
                                    oe-ordl.price:SCREEN-VALUE  = STRING(historyPrice)
                                    oe-ordl.pr-uom:SCREEN-VALUE = STRING(historyPrUOM)
                                    price-ent                   = YES.
            
                                IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                                    oeestcom-log = YES THEN
                                    RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                                RUN pAddTagInfoForGroup(
                                    oe-ordl.rec_key,
                                    INPUT "History Price"
                                    ).
                            END.
          
                        END. /* not matrixexits */
        
                    /* Only do this for the order currently in the viewer */
                    ASSIGN
                        ll-ok-i-no    = NO
                        historyButton = YES.
                    APPLY 'LEAVE' TO oe-ordl.i-no.
                    IF oe-ordl.price:SENSITIVE   THEN 
                        APPLY 'ENTRY' TO oe-ordl.price.
        
                END. /* if li = 1 */
                ELSE 
                DO:
                    /* Process new oe-ordl records if multi-select */               
                    IF AVAILABLE b-oe-ordl THEN 
                    DO:
          
                        RUN create-item.
                        FIND bf-new-oe-ordl WHERE RECID(bf-new-oe-ordl) = lv-item-recid
                            EXCLUSIVE-LOCK NO-ERROR.
                        bf-new-oe-ordl.i-no = b-oe-ordl.i-no.
                        IF lv-on-screen-item NE bf-new-oe-ordl.i-no THEN
                            op-rowid-list = op-rowid-list + STRING(ROWID(bf-new-oe-ordl)) + ",".
          
                        FIND FIRST itemfg NO-LOCK
                            WHERE itemfg.company EQ cocode
                            AND itemfg.i-no EQ bf-new-oe-ordl.i-no NO-ERROR.
          
                        IF AVAILABLE itemfg THEN
                            ASSIGN
                                bf-new-oe-ordl.part-no    = itemfg.part-no
                                bf-new-oe-ordl.i-name     = itemfg.i-name
                                bf-new-oe-ordl.part-dscr1 = itemfg.part-dscr1
                                bf-new-oe-ordl.part-dscr2 = itemfg.part-dscr2
                                bf-new-oe-ordl.part-dscr3 = itemfg.part-dscr3 
                                bf-new-oe-ordl.cas-cnt    = itemfg.case-count
                                bf-new-oe-ordl.cases-unit = itemfg.case-pall .
            
          
                        ASSIGN
                            bf-new-oe-ordl.price  = b-oe-ordl.price
                            bf-new-oe-ordl.pr-uom = b-oe-ordl.pr-uom
                            price-ent             = YES.
          
                        /* IF INTEGER(bf-new-oe-ordl.qty) EQ 0 THEN */
                        bf-new-oe-ordl.qty = lv-qty.
          
                        IF bf-new-oe-ordl.est-no NE "" 
                            AND oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(bf-new-oe-ordl), INPUT NO).
          
                    END. /* if avail */
        

                    ELSE NEXT. /* RETURN NO-APPLY. */
        
                    IF setFromHistory AND bf-new-oe-ordl.qty EQ 0 THEN
                        bf-new-oe-ordl.qty = historyQty.
        
                    ASSIGN
                        save_id      = RECID(bf-new-oe-ordl)
                        v-i-item     = bf-new-oe-ordl.i-no
                        v-i-qty      = bf-new-oe-ordl.qty
                        price-ent    = NO
                        matrixExists = NO.
        
                    /* Code modified to operate on bf-new-oe-ordl - bring in multiple from history */
                    lr-save-xoeordl-buffer = ?.
                    IF AVAILABLE xoe-ordl THEN
                        lr-save-xoeordl-buffer = ROWID(xoe-ordl).
        
                    FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ ROWID(bf-new-oe-ordl)
                        EXCLUSIVE-LOCK NO-ERROR.
                    /* Depends on v-i-item */        
                    RUN oe/oe-price.p.
                
                    /* Modify from get-price to operate on new buffer */
                    IF matrixExists THEN RUN get-price-hidden (INPUT ROWID(bf-new-oe-ordl)).
                    ELSE IF setFromHistory THEN 
                        DO:
                            MESSAGE 'No price exists in the price matrix for item ' + xoe-ordl.i-no + ',' SKIP
                                'Import Sell Price from History?' VIEW-AS ALERT-BOX
                                QUESTION BUTTONS YES-NO UPDATE setFromHistory.
                            IF setFromHistory THEN
                            DO:
                                ASSIGN
                                    bf-new-oe-ordl.price  = historyPrice
                                    bf-new-oe-ordl.pr-uom = historyPrUOM
                                    price-ent             = YES.
            
                                /* Modified get-est-comm for new buffer */
                                IF bf-new-oe-ordl.est-no NE "" 
                                    AND oeestcom-log = YES THEN
                                    RUN get-est-comm (INPUT ROWID(bf-new-oe-ordl), INPUT NO).
                            END.
                        END. /* not matrixexits */

                    /* Restore xoe-ordl to original row in case needed somewhere else */
                    IF lr-save-xoeordl-buffer NE ? THEN
                        FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lr-save-xoeordl-buffer
                            EXCLUSIVE-LOCK NO-ERROR.


                END. /* new records from multi-select */
            END. /* Each li */
            op-rowid-list = TRIM(op-rowid-list, ",").
            /* End loop processing */
            RETURN NO-APPLY.
    
        END. /* if avail oe-ordl */
    END. /* do: */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK d-oeitem
ON CHOOSE OF Btn_OK IN FRAME d-oeitem /* Save */
DO:
        RUN OnSaveButton.
 
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.cas-cnt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.cas-cnt d-oeitem
ON LEAVE OF oe-ordl.cas-cnt IN FRAME d-oeitem /* Qty/Unit */
DO:
        DEFINE VARIABLE lv-calc-qty AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lv-case-qty AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lv-uom      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE op-value    AS LOG       NO-UNDO .
        DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.
    
        RUN valid-cas-cnt (INPUT "NOMSG").
        RUN valid-uom (fi_qty-uom:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

        RUN Conv_CalcTotalPrice(cocode, 
            oe-ordl.i-no:SCREEN-VALUE,
            DECIMAL(oe-ordl.qty:SCREEN-VALUE),
            DECIMAL(oe-ordl.price:SCREEN-VALUE),
            oe-ordl.pr-uom:SCREEN-VALUE,
            DECIMAL(oe-ordl.disc:SCREEN-VALUE),
            DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
            OUTPUT dTotalPrice).
        oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
    //{oe/ordltot.i oe-ordl qty oe-ordl}  
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.cas-cnt d-oeitem
ON VALUE-CHANGED OF oe-ordl.cas-cnt IN FRAME d-oeitem /* Qty/Unit */
DO:
    /*RUN calc-qty.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.disc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.disc d-oeitem
ON LEAVE OF oe-ordl.disc IN FRAME d-oeitem /* Discount */
DO:
        DEFINE VARIABLE dTotalPrice AS DECIMAL NO-UNDO.
    
        IF DEC(oe-ordl.disc:SCREEN-VALUE) GT 100 THEN
        DO:
            MESSAGE "Discount % Cannot Be Greater Than 100."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            RETURN NO-APPLY.
        END.

        IF SELF:modified THEN 
        DO:
            /*oe-ordl.t-price:screen-value = string( (input oe-ordl.qty *
                              input oe-ordl.price) - round( ((input oe-ordl.qty *
                              input oe-ordl.price) * input oe-ordl.disc) / 100, 2) ). */
            RUN Conv_CalcTotalPrice(cocode, 
                oe-ordl.i-no:SCREEN-VALUE,
                DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                DECIMAL(oe-ordl.price:SCREEN-VALUE),
                oe-ordl.pr-uom:SCREEN-VALUE,
                DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                OUTPUT dTotalPrice).
            oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
    //{oe/ordltot.i oe-ordl qty oe-ordl  }
        END.
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.est-no d-oeitem
ON LEAVE OF oe-ordl.est-no IN FRAME d-oeitem /* Estimate # */
DO:
        IF LASTKEY = -1 THEN RETURN.
    
        IF oe-ordl.est-no:screen-value <> "" THEN 
        DO:
            v-est-no = oe-ordl.est-no:screen-value.
            RUN util/rjust.p (INPUT-OUTPUT v-est-no,8).
            oe-ordl.est-no:screen-value = v-est-no.
            FIND FIRST est WHERE est.company = oe-ordl.company
                AND est.est-no = oe-ordl.est-no:screen-value
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE est THEN 
            DO:
                MESSAGE "Invalid Estimate#. Try help." VIEW-AS ALERT-BOX ERROR.
                RETURN NO-APPLY.
            END.
            ELSE 
            DO:
                /* check eb.stock# if FGITEM = "HOLD" */
                IF v-est-fg1 = "Hold" THEN 
                DO:
                    FIND FIRST eb WHERE eb.company = cocode AND
                        eb.est-no = oe-ordl.est-no:SCREEN-VALUE AND
                        eb.stock-no = ""                            
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE eb THEN 
                    DO:
                        MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                            VIEW-AS ALERT-BOX ERROR.
                        APPLY "ENTRY" TO OE-ORDL.EST-NO.
                        RETURN NO-APPLY.
                    END.
                END.

                FIND FIRST eb WHERE eb.company = cocode AND
                    eb.est-no = oe-ordl.est-no:screen-value AND
                    eb.est-no NE ""
                    AND eb.cust-no = oe-ord.cust-no
                    AND ((eb.est-type = 1 AND eb.form-no <> 0) OR
                    (eb.est-type = 2 AND eb.form-no = 0) OR
                    (eb.est-type = 5 AND eb.form-no <> 0) OR
                    (eb.est-type = 6 AND eb.form-no = 0) )
                    NO-LOCK NO-ERROR.

                IF AVAILABLE eb AND ( 
                    (eb.est-type = oe-ord.est-type) OR
                    ((eb.est-type <= 2 OR eb.est-type >= 5 ) AND
                    oe-ord.est-type = 0) )
                    THEN 
                DO:

                    RUN display-est-detail (RECID(eb)).
                    DISABLE oe-ordl.part-no WITH FRAME {&frame-name}.
                END.   
                ELSE 
                DO:
                    MESSAGE "Estimate number: " SELF:screen-value "is either a Combo or Tandem"
                        "Estimate or Does not exist for this customer." SKIP
                        "Try Help please. "
                        VIEW-AS ALERT-BOX ERROR .                       
                    RETURN NO-APPLY.
                END.
                fiPrevOrder:SCREEN-VALUE = fnPrevOrder(eb.est-no, oe-ord.ord-no).
            END.
      
            IF oe-ordl.est-no:SCREEN-VALUE GT "" AND runship-char EQ "RUN&SHIP Prompt" THEN 
                asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
            ELSE IF oe-ordl.est-no:SCREEN-VALUE GT "" AND runship-char EQ "DefaultOnly" AND runship-log = YES THEN 
                    asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
          
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.est-no d-oeitem
ON VALUE-CHANGED OF oe-ordl.est-no IN FRAME d-oeitem /* Estimate # */
DO:
        lCheckMessage = NO .    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_jobStartDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_jobStartDate d-oeitem
ON LEAVE OF fi_jobStartDate IN FRAME d-oeitem /* Job Start Date */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN validate-start-date.
            IF NOT ll-valid THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_jobStartDate d-oeitem
ON VALUE-CHANGED OF fi_jobStartDate IN FRAME d-oeitem /* Job Start Date */
DO:
        lShowWarning = NO.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_qty-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_qty-uom d-oeitem
ON ENTRY OF fi_qty-uom IN FRAME d-oeitem
DO:
    /*li-prev-qty = INT(oe-ordl.qty:SCREEN-VALUE).*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_qty-uom d-oeitem
ON LEAVE OF fi_qty-uom IN FRAME d-oeitem
DO:
        IF  oescreen-log 
            AND oe-ordl.est-no:SCREEN-VALUE EQ "" THEN ASSIGN
                oe-ordl.spare-char-2:SCREEN-VALUE = UPPER(fi_qty-uom:SCREEN-VALUE).

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-uom (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN leave-qty NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.

        IF oescreen-log 
            AND asi.oe-ordl.est-no:SCREEN-VALUE EQ ""
            AND oescreen-cha EQ "item-qty" THEN 
        DO:
            IF oe-ordl.price:SENSITIVE  THEN 
                APPLY "entry" TO oe-ordl.price.
            ELSE IF oe-ordl.pr-uom:SENSITIVE THEN 
                    APPLY "entry" TO oe-ordl.pr-uom.
                ELSE APPLY 'entry' TO oe-ordl.part-no. 
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_qty-uom d-oeitem
ON VALUE-CHANGED OF fi_qty-uom IN FRAME d-oeitem
DO:
        IF oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
        DO:
            ASSIGN
                asi.oe-ordl.spare-dec-1:SENSITIVE  = YES
                asi.oe-ordl.spare-char-2:SENSITIVE = YES.

            asi.oe-ordl.spare-char-2:SCREEN-VALUE = fi_qty-uom:SCREEN-VALUE.

            ASSIGN
                asi.oe-ordl.spare-dec-1:SENSITIVE  = NO
                asi.oe-ordl.spare-char-2:SENSITIVE = NO.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.i-no d-oeitem
ON ENTRY OF oe-ordl.i-no IN FRAME d-oeitem /* FG Item# */
DO:
        ll-ok-i-no = NO.
        IF (INDEX("ONT",oe-ordl.type-code:SCREEN-VALUE) GT 0 OR oe-ordl.i-no:SCREEN-VALUE EQ "") AND
            (oe-ordl.est-no:SCREEN-VALUE = "" OR ls-stock = "") THEN 
        DO:
        END.
        ELSE 
        DO:
            ll-bypass = YES. /* bypass leave trigger */
            APPLY "tab" TO SELF.     
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.i-no d-oeitem
ON LEAVE OF oe-ordl.i-no IN FRAME d-oeitem /* FG Item# */
DO:
        DEFINE VARIABLE ls-i-no    AS cha       NO-UNDO.
        DEFINE VARIABLE ls-part-no AS cha       NO-UNDO.
        DEFINE VARIABLE ls-est-no  AS cha       NO-UNDO.
        DEFINE VARIABLE ls-uom     AS cha       NO-UNDO.
        DEFINE VARIABLE ll-secure  AS LOG       NO-UNDO.
        DEFINE VARIABLE cLoc       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cLocBin    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAvailable AS LOGICAL. 


        IF LASTKEY EQ -1 AND NOT historyButton THEN 
        DO:
            IF ll-bypass THEN ll-bypass = NO.
            RETURN.
        END.
        historyButton = NO.

        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
        IF NOT AVAILABLE oe-ord THEN
            FIND FIRST oe-ord NO-LOCK 
                WHERE oe-ord.company EQ cocode
                AND oe-ord.ord-no  EQ oe-ordl.ord-no
                NO-ERROR.
        IF AVAILABLE oe-ord THEN 
            FIND FIRST shipto NO-LOCK 
                WHERE shipto.company EQ oe-ord.company
                AND shipto.cust-no EQ oe-ord.cust-no
                AND shipto.ship-id EQ oe-ord.ship-id
                NO-ERROR.
        IF AVAILABLE shipto THEN 
            ASSIGN 
                cLoc    = shipto.loc
                cLocBin = shipto.loc-bin
                . 
            
        IF AVAILABLE oe-ord THEN
            RUN pGetOverUnderPct(oe-ord.cust-no,oe-ord.ship-id,oe-ord.ord-no) .
      
        IF ll-bypass THEN 
        DO:
            ll-bypass = NO.
            RETURN.
        END.

        IF oe-ordl.part-no:SCREEN-VALUE EQ "" THEN
            ASSIGN oe-ordl.part-no:SCREEN-VALUE = SELF:SCREEN-VALUE .

        RUN pSetValidUOMList(g_company, oe-ordl.i-no:SCREEN-VALUE).
        /*    IF lUseItemUoM  THEN DO:                                                                         */
        /*        cUOMListQty = DYNAMIC-FUNCTION("fGetValidUoMsForItem",oe-ord.company,"FG",SELF:SCREEN-VALUE).*/
        /*    END.                                                                                             */
        /*    ELSE ASSIGN                                                                                      */
        /*        cUOMListQty = DYNAMIC-FUNCTION("fGetValidItemUoMs",oe-ord.company,"FG").                     */

        IF /*self:modified and*/ SELF:screen-value <> "0" AND NOT ll-ok-i-no /* done in leave trigger */
            THEN 
        DO: 
    
            RUN display-fgitem NO-ERROR.

            IF ERROR-STATUS:ERROR THEN 
                IF CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ g_company
                    AND itemfg.i-no    EQ oe-ordl.i-no:SCREEN-VALUE) THEN RETURN NO-APPLY.
                ELSE 
                DO:
                    /*  message "This item does not exist, would you like to add it?" view-as alert-box question
                              button yes-no update ll-ans as log.  
                      if ll-ans then do:
                    */
                    ASSIGN 
                        ls-i-no    = oe-ordl.i-no:SCREEN-VALUE
                        ls-part-no = oe-ordl.part-no:SCREEN-VALUE
                        ls-est-no  = oe-ordl.est-no:SCREEN-VALUE
                        ls-uom     = oe-ordl.pr-uom:SCREEN-VALUE.

                    RUN default-type (BUFFER itemfg).

                    /* wfk - This has been here for years but Joe says it is incorrect */
                    /*        IF oe-ord.est-no = "" AND oe-ordl.est-no:SCREEN-VALUE = "" THEN DO: */
                    /*           RUN sys/ref/d-passwd.w (4, OUTPUT ll-secure).                    */
                    /*           IF NOT ll-secure THEN RETURN NO-APPLY.                           */
                    /*        END.                                                                */
     
                    /* Per Joe, when there is an estimate, this validation should not apply */
                    /* Task 04171308 */
                    IF (oefgadd-log AND llOEFGAdd-sec) OR ls-est-no GT "" THEN
                        RUN oe/d-citmfg.w (ls-est-no, INPUT-OUTPUT ls-i-no,
                            INPUT-OUTPUT ls-part-no,INPUT-OUTPUT ls-uom, INPUT-OUTPUT cLoc, INPUT-OUTPUT cLocBin) NO-ERROR.
                    ELSE IF ls-i-no NE "" AND ls-est-no EQ "" THEN 
                        DO:
                            MESSAGE "Please enter a valid item number."
                                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                            ASSIGN 
                                oe-ordl.part-no:SCREEN-VALUE = "".
                            APPLY 'entry' TO oe-ordl.i-no.
                            RETURN NO-APPLY.
                        END.
           

                    IF ls-i-no = "" THEN 
                    DO:
                        APPLY "entry" TO oe-ordl.i-no.
                        RETURN NO-APPLY.  /* cancel */
                    END.
                    ELSE 
                    DO:   
                        ASSIGN 
                            oe-ordl.i-no:screen-value    = ls-i-no
                            oe-ordl.part-no:screen-value = ls-part-no
                            oe-ordl.pr-uom:SCREEN-VALUE  = ls-uom.
  
                        FIND FIRST xest WHERE xest.company = g_company 
                            AND xest.est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no:SCREEN-VALUE))) +
                            TRIM(oe-ordl.est-no:SCREEN-VALUE)
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE xest THEN 
                        DO: 
                            FIND xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                                AND xeb.form-no = 0 NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE xeb THEN FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                                    AND xeb.form-no = oe-ordl.form-no
                                    AND xeb.blank-no = oe-ordl.blank-no
                                    NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE xeb THEN
                                FIND FIRST xeb
                                    WHERE xeb.company EQ g_company
                                    AND xeb.est-no  EQ xest.est-no
                                    AND xeb.part-no EQ ls-part-no
                                    NO-LOCK NO-ERROR.

                            IF AVAILABLE xeb THEN 
                            DO:
                                FIND xef WHERE xef.company = g_company AND xef.est-no = xeb.est-no
                                    AND xef.form-no = xeb.form-no
                                    NO-LOCK NO-ERROR.      

                                RUN crt-itemfg (SELF:screen-value,oe-ordl.pr-uom:SCREEN-VALUE, cLoc, cLocBin). /*(self:screen-value,"M")*/
                            END.    
                        END.   
                        ELSE  /* no xest or oe-ordl.est-no = "" */
                            RUN crt-itemfg (SELF:screen-value,oe-ordl.pr-uom:screen-value, cLoc, cLocBin).  /*(self:screen-value,"M")*/          
                    END. 
       
                    RUN display-fgitem NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
                END. /* error and no itemfg */

            ll-ok-i-no = YES.

            IF oescreen-cha NE "item-qty" 
                AND oescreen-log 
                AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
            DO:
                IF oe-ordl.price:SENSITIVE  THEN
                    APPLY "entry" TO oe-ordl.price.
                ELSE 
                    APPLY "entry" TO oe-ordl.pr-uom.
                RETURN NO-APPLY.
            END.
            IF ll-new-record THEN
                RUN itemfg-sman.    

        END. /* modified */
        RUN Tag_IsTagRecordAvailableForGroup(
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "oe-ordl",
            INPUT "Price-Source",
            OUTPUT lAvailable
            ).
        IF lAvailable THEN  
            btnTags:SENSITIVE = TRUE
                .
        ELSE 
            btnTags:SENSITIVE = FALSE.
        IF btnViewDetail:LABEL EQ "Close Detail" THEN
        RUN pViewDetail ("Locations").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.i-no d-oeitem
ON VALUE-CHANGED OF oe-ordl.i-no IN FRAME d-oeitem /* FG Item# */
DO:
        lCheckFgForceWarning = NO .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME iPrintAvailQty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL iPrintAvailQty d-oeitem
ON VALUE-CHANGED OF iPrintAvailQty IN FRAME d-oeitem
DO:
        ASSIGN {&SELF-NAME}.
        {&OPEN-QUERY-browseLocations}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.job-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.job-no d-oeitem
ON LEAVE OF oe-ordl.job-no IN FRAME d-oeitem /* Job Number */
DO:
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
   
        ASSIGN 
            v-bld-job = "".
        DO i = 1 TO 6:
            IF SUBSTRING(INPUT oe-ordl.job-no,i,1) NE " " THEN
                ASSIGN v-bld-job = v-bld-job +     substring(INPUT oe-ordl.job-no,i,1).
        END. /* 1 - 6 */
        ASSIGN 
            oe-ordl.job-no:screen-value = STRING(FILL(" ",6 - length(v-bld-job))) +
                   (TRIM(v-bld-job)).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.job-no2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.job-no2 d-oeitem
ON LEAVE OF oe-ordl.job-no2 IN FRAME d-oeitem /* Run # */
DO:
        RUN util/rjust.p (INPUT-OUTPUT v-bld-job, INPUT 6).
        FIND FIRST job-hdr WHERE job-hdr.company = cocode AND
            job-hdr.job-no = v-bld-job AND
            job-hdr.job-no2 = INPUT oe-ordl.job-no2
            USE-INDEX job-no NO-LOCK NO-ERROR.
        IF AVAILABLE job-hdr THEN 
        DO:
            MESSAGE "   JOB NUMBER " + string(job-hdr.job-no) + "-" +
                string(job-hdr.job-no2) + " has already been used."
                VIEW-AS ALERT-BOX ERROR  .
            RETURN NO-APPLY.
        END.        
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.over-pct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.over-pct d-oeitem
ON LEAVE OF oe-ordl.over-pct IN FRAME d-oeitem /* Overrun % */
DO:
        IF LASTKEY NE -1 AND 
            deAutoOver NE oe-ordl.over-pct:INPUT-VALUE THEN 
            RUN pAddTag("Over Percentage", "Enter Manualy").

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.part-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.part-no d-oeitem
ON ENTRY OF oe-ordl.part-no IN FRAME d-oeitem /* Cust Part # */
DO:
        IF oe-ordl.est-no:screen-value <> "" AND lv-new-tandem EQ ? THEN 
        DO:
            APPLY "tab" TO SELF.
            RETURN NO-APPLY.
        END.   

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.part-no d-oeitem
ON LEAVE OF oe-ordl.part-no IN FRAME d-oeitem /* Cust Part # */
DO:
        DEFINE VARIABLE lErrorPart AS LOGICAL NO-UNDO .
        IF LASTKEY = -1 THEN RETURN.

        IF SELF:modified AND SELF:screen-value <> "" THEN 
        DO:
            IF ll-new-file AND (oe-ordl.i-no:SCREEN-VALUE EQ ""  OR oe-ordl.i-no:SCREEN-VALUE EQ "0" ) THEN 
            DO:
                ASSIGN
                    cp-part-no = oe-ordl.part-no:SCREEN-VALUE
                    cp-rowid   = ?.
                RUN custom/getcpart.p (cocode, oe-ord.cust-no,
                    INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
                FIND itemfg WHERE ROWID(itemfg) EQ cp-rowid NO-LOCK NO-ERROR.
            END.
            ELSE 
            DO:
                FIND FIRST itemfg WHERE itemfg.company = g_company 
                    AND itemfg.i-no = oe-ordl.i-no:screen-value
                    AND itemfg.part-no = oe-ordl.part-no:screen-value
                    NO-LOCK NO-ERROR.
            END.

            IF NOT AVAILABLE itemfg THEN
                FIND FIRST itemfg WHERE itemfg.company = g_company 
                    AND itemfg.part-no = oe-ordl.part-no:screen-value
                    AND itemfg.cust-no = oe-ord.cust-no
                    NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg THEN 
            DO:
                FIND FIRST itemfg WHERE itemfg.company = g_company 
                    AND itemfg.part-no = oe-ordl.part-no:screen-value
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE itemfg THEN 
                DO:
                    RUN pCrtPart(INPUT-OUTPUT cp-rowid,OUTPUT lErrorPart ) .
                    IF lErrorPart THEN
                        RETURN NO-APPLY.
                END.
                ELSE 
                DO:
                    FIND FIRST cust WHERE cust.company = oe-ord.company
                        AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
                    IF itemfg.cust-no NE oe-ord.cust-no AND itemfg.cust-no NE "" AND
                        AVAILABLE cust AND NOT cust.internal                         THEN 
                    DO:
                        FIND FIRST cust WHERE cust.company = g_company AND
                            cust.cust-no = itemfg.cust-no
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE cust AND NOT cust.internal THEN 
                        DO:                      
                            MESSAGE "This item exists for a different customer!. Do you want to continue?"
                                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
                            IF NOT ll-ans THEN  RETURN NO-APPLY.       
                        END.  
                    END.   
                END.   
            END.  
      
            IF itemfg.stat EQ "I" THEN 
            DO:        
                MESSAGE "The only FG Item " + itemfg.i-no + " found is not active.  Enter a different part number or make this FG item active " VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.        
            END.

            IF itemfg.prod-uom EQ "" THEN 
            DO:
                MESSAGE "FG Item " + itemfg.i-no + " has no cost UOM. Please correct and try again. " VIEW-AS ALERT-BOX.
                RETURN NO-APPLY.        
            END.
     
            IF oe-ordl.i-no:SCREEN-VALUE = "" OR oe-ordl.i-no:SCREEN-VALUE = "0"
                THEN oe-ordl.i-no:SCREEN-VALUE = itemfg.i-no.

            RUN valid-part-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            RUN display-fgpart (RECID(itemfg)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
     
            IF oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
            DO:
                IF oescreen-cha EQ "item-qty" THEN
                    APPLY "entry" TO oe-ordl.qty.
                ELSE IF oe-ordl.price:SENSITIVE THEN 
                        APPLY "entry" TO oe-ordl.price.
                    ELSE IF oe-ordl.pr-uom:SENSITIVE THEN
                            APPLY "entry" TO oe-ordl.pr-uom.
                        ELSE IF  oe-ordl.disc:SENSITIVE THEN
                                APPLY "entry" TO oe-ordl.disc.
                            ELSE
                                APPLY "entry" TO oe-ordl.cas-cnt.
                RETURN NO-APPLY.
            END.
      
      
        END.
        IF SELF:screen-value EQ "" THEN
            ASSIGN SELF:screen-value = oe-ordl.i-no:SCREEN-VALUE .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.part-no d-oeitem
ON VALUE-CHANGED OF oe-ordl.part-no IN FRAME d-oeitem /* Cust Part # */
DO:
        lCheckFgForceWarning = NO .
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.partial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.partial d-oeitem
ON VALUE-CHANGED OF oe-ordl.partial IN FRAME d-oeitem /* Partial */
DO:
    /*RUN calc-qty.*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.po-no d-oeitem
ON LEAVE OF oe-ordl.po-no IN FRAME d-oeitem /* Cust PO# */
DO:

        /* gdm - 11090905*/
        IF SELF:MODIFIED THEN v-ponoUp = TRUE.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-po-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.

        FIND LAST bf-oe-ordl 
            WHERE bf-oe-ordl.company EQ oe-ordl.company
            AND bf-oe-ordl.ord-no  EQ oe-ordl.ord-no
            NO-LOCK USE-INDEX ord-no.

        IF SELF:modified AND AVAIL(bf-oe-ordl) AND bf-oe-ordl.line > 1 THEN 
        DO:
            MESSAGE "Change all Customer PO# on order? " 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF ll-ans THEN lv-change-cst-po = YES.
            ELSE lv-change-cst-po = NO.
        END.

        IF SELF:MODIFIED AND OEPO#Xfer-log THEN 
        DO:
            lInvoiceFound = FALSE.
            FOR EACH oe-boll 
                WHERE oe-boll.company EQ oe-ordl.company
                AND oe-boll.ord-no EQ oe-ordl.ord-no
                AND oe-boll.i-no   EQ oe-ordl.i-no
                AND oe-boll.LINE   EQ oe-ordl.LINE
                NO-LOCK,
                EACH inv-head WHERE inv-head.company EQ oe-boll.company
                AND inv-head.bol-no EQ oe-boll.bol-no
                NO-LOCK,
       
                FIRST inv-line 
                WHERE inv-line.r-no   EQ inv-head.r-no 
                AND inv-line.ord-no  EQ oe-boll.ord-no 
                AND inv-line.b-no    EQ oe-boll.b-no
                AND inv-line.i-no    EQ oe-boll.i-no
                AND inv-line.line    EQ oe-boll.line
                AND inv-line.po-no   EQ oe-boll.po-no
                NO-LOCK .
                lInvoiceFound = TRUE.
                LEAVE.
            END. /* Each oe-boll */

            IF lInvoiceFound EQ TRUE THEN 
            DO:
                ll-ans = NO.
                MESSAGE 
                    "Unposted Invoice Exists, transfer new Customer PO# to Invoice?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
                lv-change-inv-po = ll-ans.
            END. /* If lInvoiceFound */

        END. /* if po was modified and inv-line should be changed */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.po-no-po
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.po-no-po d-oeitem
ON LEAVE OF oe-ordl.po-no-po IN FRAME d-oeitem /* Board PO # */
DO:
        IF LASTKEY = -1 THEN RETURN.
    
        DEFINE VARIABLE ld-cost AS DECIMAL NO-UNDO.
    
        IF SELF:screen-value NE "0" THEN 
        DO:
            FIND FIRST po-ord WHERE po-ord.company EQ cocode
                AND po-ord.po-no   EQ INPUT oe-ordl.po-no-po
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE po-ord THEN 
            DO:
                MESSAGE "You have entered an invalid Purchase Order."
                    VIEW-AS ALERT-BOX ERROR .
                RETURN NO-APPLY.
            END.
            
            IF oe-ordl.job-no NE "" THEN 
            DO:
                FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
                    AND job-hdr.job-no  EQ oe-ordl.job-no:screen-value
                    AND job-hdr.job-no2 EQ int(oe-ordl.job-no2:screen-value)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job-hdr THEN
                    FOR EACH job-mat WHERE job-mat.company EQ cocode
                        AND job-mat.job-no  EQ job-hdr.job-no
                        AND job-mat.job-no2 EQ job-hdr.job-no2
                        AND job-mat.job EQ job-hdr.job  NO-LOCK,
                        FIRST item WHERE item.company  EQ cocode
                        AND item.i-no     EQ job-mat.rm-i-no
                        AND item.mat-type EQ "B" NO-LOCK:                 
  
                        FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                            AND po-ordl.i-no      EQ job-mat.rm-i-no
                            AND po-ordl.po-no     EQ po-ord.po-no
                            AND po-ordl.item-type EQ YES
                            USE-INDEX item-ordno NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE po-ordl THEN 
                        DO:
                            MESSAGE "You have entered an invalid Purchase Order for this Item."
                                VIEW-AS ALERT-BOX ERROR.
                            RETURN NO-APPLY.
                        END.
                    END. /* for each job-mat */
            END. /* job-no ne "" */
            ELSE 
            DO:
                FIND FIRST po-ordl  WHERE po-ordl.company   EQ cocode
                    AND po-ordl.i-no      EQ oe-ordl.i-no:screen-value
                    AND po-ordl.po-no     EQ po-ord.po-no
                    AND po-ordl.item-type EQ NO
                    USE-INDEX item-ordno NO-LOCK NO-ERROR.
                IF NOT AVAILABLE po-ordl THEN 
                DO:
                    MESSAGE "You have entered an invalid Purchase Order for this Item."
                        VIEW-AS ALERT-BOX ERROR.
                    RETURN NO-APPLY.
                END.              
                IF po-ordl.cons-uom EQ "M" THEN
                    oe-ordl.cost:screen-value = STRING(po-ordl.cons-cost). 
                ELSE 
                DO:                 
                    RUN sys/ref/convcuom.p (po-ordl.cons-uom, "M", 0, 0, 0, 0,
                        po-ordl.cons-cost, OUTPUT ld-cost).

                    oe-ordl.cost:screen-value = STRING(ld-cost).                          
                END.
                IF lNewVendorItemCost THEN .
                ELSE 
                    FIND FIRST e-itemfg-vend WHERE
                        e-itemfg-vend.company EQ po-ordl.company AND
                        e-itemfg-vend.i-no EQ po-ordl.i-no AND
                        e-itemfg-vend.vend-no EQ po-ord.vend-no AND
                        e-itemfg-vend.est-no EQ ""
                        NO-LOCK NO-ERROR.

                IF AVAILABLE e-itemfg-vend THEN
                DO:
                    oe-ordl.cost:SCREEN-VALUE = STRING(DEC(oe-ordl.cost:SCREEN-VALUE) * (1 + (e-itemfg-vend.markup / 100.0 ))).
                END.
            END.
        END.
        ELSE IF SELF:screen-value EQ "0" THEN
                ASSIGN oe-ordl.vend-no:SCREEN-VALUE = "".          /*Task# 03201407*/
   
        IF oescreen-log AND ll-new-record 
            AND asi.oe-ordl.est-no:SCREEN-VALUE EQ ""
            AND oescreen-cha EQ "item-qty" THEN 
        DO:
            APPLY "entry" TO oe-ordl.s-man[1] .
            RETURN NO-APPLY.
        END.   /* ticket 15474 */
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.pr-uom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.pr-uom d-oeitem
ON LEAVE OF oe-ordl.pr-uom IN FRAME d-oeitem /* UOM */
DO:    
        DEFINE VARIABLE dTotalPrice AS DECIMAL.
        IF LASTKEY NE -1 THEN 
        DO:

            IF TRIM(oe-ordl.pr-uom:SCREEN-VALUE)EQ "" THEN 
            DO:
                MESSAGE 
                    "UOM can't be blank. Please enter a valid UOM."
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.

            RUN valid-uom (FOCUS) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

            IF oe-ordl.pr-uom:SCREEN-VALUE NE oe-ordl.pr-uom AND
                oe-ordl.est-no:SCREEN-VALUE NE "" AND
                oeestcom-log = YES THEN
                RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
       
            RUN Conv_CalcTotalPrice(cocode, 
                oe-ordl.i-no:SCREEN-VALUE,
                DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                DECIMAL(oe-ordl.price:SCREEN-VALUE),
                oe-ordl.pr-uom:SCREEN-VALUE,
                DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                OUTPUT dTotalPrice).
            oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.price
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.price d-oeitem
ON ENTRY OF oe-ordl.price IN FRAME d-oeitem /* Price */
DO:
        IF v-quo-price-int EQ 1              AND
            v-quo-price-log                   AND
            oe-ordl.est-no:SCREEN-VALUE NE "" THEN RETURN NO-APPLY.

        ld-prev-price = DEC(SELF:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.price d-oeitem
ON LEAVE OF oe-ordl.price IN FRAME d-oeitem /* Price */
DO:
        DEFINE VARIABLE lAvailable AS LOGICAL.
        IF LASTKEY NE -1 THEN 
        DO:
            IF DEC({&self-name}:SCREEN-VALUE) NE ld-prev-price THEN
            DO:
                price-ent = YES.

                IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                    oeestcom-log = YES THEN
                    RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "Price was manually entered"
                    ).
            END.
    
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.price d-oeitem
ON VALUE-CHANGED OF oe-ordl.price IN FRAME d-oeitem /* Price */
DO:
        DEFINE VARIABLE dTotalPrice AS DECIMAL NO-UNDO.
        RUN Conv_CalcTotalPrice(cocode, 
            oe-ordl.i-no:SCREEN-VALUE,
            DECIMAL(oe-ordl.qty:SCREEN-VALUE),
            DECIMAL(oe-ordl.price:SCREEN-VALUE),
            oe-ordl.pr-uom:SCREEN-VALUE,
            DECIMAL(oe-ordl.disc:SCREEN-VALUE),
            DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
            OUTPUT dTotalPrice).
        oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
//  {oe/ordltot.i oe-ordl qty oe-ordl}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.prom-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.prom-date d-oeitem
ON LEAVE OF oe-ordl.prom-date IN FRAME d-oeitem /* Prom. Date */
DO:
        DEFINE VARIABLE dCalcDueDate  AS DATE NO-UNDO.
        DEFINE VARIABLE dCalcPromDate AS DATE NO-UNDO.

        IF SELF:modified AND oe-ordl.line > 1 THEN 
        DO:
            MESSAGE "Change all promise dates on order? " 
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF ll-ans THEN lv-change-prom-date = YES.
            ELSE lv-change-prom-date = NO.
        END.


        IF SELF:MODIFIED AND oeDateAuto-log THEN 
        DO:
    
            IF NOT cDueManualChanged THEN 
            DO:      
                RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                    INPUT oe-ordl.req-date:SCREEN-VALUE,
                    INPUT oe-ordl.prom-date:SCREEN-VALUE,
                    INPUT "PromiseDate",
                    INPUT ROWID(oe-ordl),
                    OUTPUT dCalcDueDate,
                    OUTPUT dCalcPromDate).
                oe-ordl.req-date:SCREEN-VALUE = STRING(dCalcDueDate).
            END.

            /* Used to update due-date on header */
            IF gcLastDateChange EQ "" THEN
                gcLastDateChange = "prom-date".
        END.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.prom-date d-oeitem
ON VALUE-CHANGED OF oe-ordl.prom-date IN FRAME d-oeitem /* Prom. Date */
DO:
        cPromManualChanged = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.qty
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.qty d-oeitem
ON ENTRY OF oe-ordl.qty IN FRAME d-oeitem /* Quantity */
DO:
        li-prev-qty = INT({&self-name}:SCREEN-VALUE).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.qty d-oeitem
ON LEAVE OF oe-ordl.qty IN FRAME d-oeitem /* Quantity */
DO: 
        DEFINE VARIABLE op-value AS LOG NO-UNDO .
  
        IF LASTKEY NE -1 THEN 
        DO:
            /*RUN valid-qty (FOCUS) NO-ERROR.            */
            /*IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.*/ /* ticket 56847 */

            IF NOT fi_qty-uom:SENSITIVE THEN RUN leave-qty.
            ELSE RUN new-qty.
            IF  oescreen-log AND integer(oe-ordl.spare-dec-1:SCREEN-VALUE) EQ 0 
                AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
            DO:
                ASSIGN
                    asi.oe-ordl.spare-dec-1:SENSITIVE  = YES
                    asi.oe-ordl.spare-char-2:SENSITIVE = YES.

                asi.oe-ordl.spare-dec-1:SCREEN-VALUE = oe-ordl.qty:SCREEN-VALUE.

                ASSIGN
                    asi.oe-ordl.spare-dec-1:SENSITIVE  = NO
                    asi.oe-ordl.spare-char-2:SENSITIVE = NO.
            END.

        END.

    /*   if int(oe-ordl.qty:screen-value) < int(oe-ordl.cas-cnt:screen-value) then do: */
    /*                                                                                 */
    /*      RUN oe/d-copyqty.w (OUTPUT op-value) .                                     */
    /*      IF op-value EQ YES THEN do:                                                */
    /*       oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value.                  */
    /*       APPLY "entry" TO oe-ordl.cas-cnt.                                         */
    /*       RETURN no-apply.                                                          */
    /*      END.                                                                       */
    /*      IF op-value EQ NO THEN do:                                                 */
    /*           APPLY "entry" TO oe-ordl.qty.                                         */
    /*           RETURN no-apply.                                                      */
    /*      END.                                                                       */
    /*   END.                                                                          */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.qty d-oeitem
ON VALUE-CHANGED OF oe-ordl.qty IN FRAME d-oeitem /* Quantity */
DO:
        /*  RUN new-qty. - Removed for performance purposses 31625 - GetPrice already run on LEAVE*/
        IF  oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
        DO:
            ASSIGN
                asi.oe-ordl.spare-dec-1:SENSITIVE  = YES
                asi.oe-ordl.spare-char-2:SENSITIVE = YES.

            asi.oe-ordl.spare-dec-1:SCREEN-VALUE = oe-ordl.qty:SCREEN-VALUE.

            ASSIGN
                asi.oe-ordl.spare-dec-1:SENSITIVE  = NO
                asi.oe-ordl.spare-char-2:SENSITIVE = NO.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.req-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.req-date d-oeitem
ON LEAVE OF oe-ordl.req-date IN FRAME d-oeitem /* Due Date */
DO:
        DEFINE VARIABLE dCalcDueDate  AS DATE NO-UNDO.
        DEFINE VARIABLE dCalcPromDate AS DATE NO-UNDO.

        IF LASTKEY NE -1 THEN 
        DO:
            RUN validate-due-date NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
        IF SELF:MODIFIED AND oeDateAuto-log  THEN 
        DO:
            IF NOT cPromManualChanged THEN 
            DO:
                RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                    INPUT oe-ordl.req-date:SCREEN-VALUE,
                    INPUT oe-ordl.prom-date:SCREEN-VALUE,
                    INPUT "DueDate",
                    INPUT ROWID(oe-ordl),
                    OUTPUT dCalcDueDate,
                    OUTPUT dCalcPromDate).
    
                oe-ordl.prom-date:SCREEN-VALUE = STRING(dCalcPromDate).
            END.

            /* Used to set date on header */
            IF gcLastDateChange EQ "" THEN
                gcLastDateChange = "req-date".

        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.req-date d-oeitem
ON VALUE-CHANGED OF oe-ordl.req-date IN FRAME d-oeitem /* Due Date */
DO:
        cDueManualChanged = YES.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-man[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[1] d-oeitem
ON LEAVE OF oe-ordl.s-man[1] IN FRAME d-oeitem /* Salemsman[1] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[1] d-oeitem
ON VALUE-CHANGED OF oe-ordl.s-man[1] IN FRAME d-oeitem /* Salemsman[1] */
DO:
        RUN new-s-man (1).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-man[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[2] d-oeitem
ON LEAVE OF oe-ordl.s-man[2] IN FRAME d-oeitem /* Salemsman[2] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[2] d-oeitem
ON VALUE-CHANGED OF oe-ordl.s-man[2] IN FRAME d-oeitem /* Salemsman[2] */
DO:
        RUN new-s-man (2).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-man[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[3] d-oeitem
ON LEAVE OF oe-ordl.s-man[3] IN FRAME d-oeitem /* Salemsman[3] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-man (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-man[3] d-oeitem
ON VALUE-CHANGED OF oe-ordl.s-man[3] IN FRAME d-oeitem /* Salemsman[3] */
DO:
        RUN new-s-man (3).
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-pct[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-pct[1] d-oeitem
ON LEAVE OF oe-ordl.s-pct[1] IN FRAME d-oeitem /* Pct of Sale[1] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (1) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-pct[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-pct[2] d-oeitem
ON LEAVE OF oe-ordl.s-pct[2] IN FRAME d-oeitem /* Pct of Sale[2] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (2) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.s-pct[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.s-pct[3] d-oeitem
ON LEAVE OF oe-ordl.s-pct[3] IN FRAME d-oeitem /* Pct of Sale[3] */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-s-pct (3) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.tax
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.tax d-oeitem
ON VALUE-CHANGED OF oe-ordl.tax IN FRAME d-oeitem /* Tax */
DO:
        IF NOT AVAILABLE oe-ord THEN FIND oe-ord WHERE oe-ord.company = g_company AND
                oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR. 
        IF SELF:screen-value = "yes" AND oe-ord.tax-gr = "" THEN 
        DO:
            MESSAGE "Invalid tax code on order header. " VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.type-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.type-code d-oeitem
ON ENTRY OF oe-ordl.type-code IN FRAME d-oeitem /* Type Code */
DO:
        IF SELF:SCREEN-VALUE EQ "T" AND
            CAN-FIND(FIRST cust WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            AND cust.internal EQ YES) THEN 
        DO:
            APPLY "leave" TO SELF.
            RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.type-code d-oeitem
ON LEAVE OF oe-ordl.type-code IN FRAME d-oeitem /* Type Code */
DO:
        IF LASTKEY NE -1 THEN 
        DO:
            RUN valid-type NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.   
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.type-code d-oeitem
ON VALUE-CHANGED OF oe-ordl.type-code IN FRAME d-oeitem /* Type Code */
DO:
        RUN new-type.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.under-pct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.under-pct d-oeitem
ON LEAVE OF oe-ordl.under-pct IN FRAME d-oeitem /* Underrun % */
DO:
        IF LASTKEY NE -1 AND 
            deAutoUnder NE oe-ordl.under-pct:INPUT-VALUE THEN 
            RUN pAddTag("Under Percentage", "Enter Manualy").
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.vend-no d-oeitem
ON LEAVE OF oe-ordl.vend-no IN FRAME d-oeitem /* Vendor */
DO:
        IF LASTKEY EQ -1 THEN 
        DO:
            RUN valid-vend-no NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-ordl.whsed
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-ordl.whsed d-oeitem
ON LEAVE OF oe-ordl.whsed IN FRAME d-oeitem /* Run  Ship */
DO:
        IF oescreen-log AND ll-new-record 
            AND asi.oe-ordl.est-no:SCREEN-VALUE EQ ""
            AND oescreen-cha EQ "item-qty" THEN 
        DO:
            APPLY "entry" TO oe-ordl.i-name.
            RETURN NO-APPLY.
        END.
     
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseAllocated
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK d-oeitem 


/* ***************************  Main Block  *************************** */
 
{sys/inc/f3helpj.i}   /*Task 09191305 */

ASSIGN 
    cocode = g_company
    locode = g_loc.
                              
SESSION:DATA-ENTRY-RETURN = YES.       
/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
    THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    EMPTY TEMP-TABLE ttTag.
    DEFINE VARIABLE ll-master AS LOG NO-UNDO.

    IF oe-ordl.vend-no:SCREEN-VALUE EQ "0" THEN
        ASSIGN oe-ordl.vend-no:SCREEN-VALUE = "".

    v-orig-ip-type = ip-type.
    RUN oe/oe-sysct.p.

    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "JOBCREAT"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company = cocode
            sys-ctrl.name    = "JOBCREAT"
            sys-ctrl.descrip = "Create Job Standards during OE?"
            sys-ctrl.log-fld = NO.
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
    END.
  
    v-create-job = sys-ctrl.log-fld.
      
    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "INVPRINT"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        ASSIGN
            v-print-head = sys-ctrl.log-fld
            v-print-fmt  = sys-ctrl.char-fld.
  
    /* rtc */
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OEPROMPT" NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN 
        v-duplicateFGDayClient = sys-ctrl.char-fld.
   
    {ce/msfcalc.i}

    DO TRANSACTION:
        {sys/inc/lastship.i}
    END.
   
    IF ip-type EQ "MASTER" THEN
        ASSIGN
            ip-type   = "ADD"
            ll-master = YES.

    IF ip-type = "ADD" THEN 
    DO:
        lv-add-mode = YES.
        FOR LAST bf-oe-ordl WHERE bf-oe-ordl.company EQ g_company 
            AND bf-oe-ordl.ord-no EQ ip-ord-no NO-LOCK,
            LAST bf-oe-rel WHERE bf-oe-rel.company EQ g_company 
            AND bf-oe-rel.ord-no EQ bf-oe-ordl.ord-no 
            AND bf-oe-rel.LINE EQ bf-oe-ordl.LINE NO-LOCK.

            v-ship-id = bf-oe-rel.ship-id.
        END.
        RUN create-item.
        RUN sys/ref/nk1look.p (g_company, "FGOversDefault", "C", NO, NO, "", "", 
            OUTPUT cRtnChar, OUTPUT lRecFound).
        IF lRecFound THEN
        DO:         
            cFGOversDefault = STRING(cRtnChar) NO-ERROR.      
            IF cFGOversDefault NE  "FG category" THEN 
                RUN pGetOverUnderPct("", "", ip-ord-no) .  
        END.   
        deAutoOver = oe-ordl.over-pct:INPUT-VALUE IN FRAME {&frame-name}.
        deAutoUnder = oe-ordl.under-pct:INPUT-VALUE IN FRAME {&frame-name}.
    /*find oe-ordl where recid(oe-ordl) = lv-item-recid no-error.*/
    END.
    ELSE 
    DO:
        FIND oe-ordl WHERE RECID(oe-ordl) = ip-recid NO-ERROR.
    
        IF ip-type EQ "update-3" THEN 
        DO: 
            ASSIGN
                ll-do-entry = YES
                ip-type     = "update-2".
            /* If update-3, pulling items from history, so just take last line */
            /* added for ship-to                                               */
            FOR LAST bf-oe-ordl WHERE bf-oe-ordl.company EQ g_company 
                AND bf-oe-ordl.ord-no EQ ip-ord-no NO-LOCK,
                LAST bf-oe-rel WHERE bf-oe-rel.company EQ g_company 
                AND bf-oe-rel.ord-no EQ bf-oe-ordl.ord-no 
                NO-LOCK.

                v-ship-id = bf-oe-rel.ship-id.
            
            END.

        END.
        ll-new-record = ip-type EQ "update-2".
    END.

    FIND oe-ord OF oe-ordl NO-LOCK.
    FIND xoe-ord OF oe-ordl NO-LOCK.
    FIND FIRST itemfg OF oe-ordl NO-LOCK WHERE
        itemfg.company EQ oe-ordl.company AND
        itemfg.i-no EQ oe-ordl.i-no NO-ERROR.
    RUN getTagsToReset.
    
    ASSIGN
        v-rel             = oe-ordl.rel
        ld-prev-t-price   = oe-ordl.t-price
        li-prev-ord-qty   = oe-ordl.qty
        ld-prev-prom-date = oe-ordl.prom-date
        dtPrevDueDate     = oe-ordl.req-date
        v-margin          = oe-ordl.q-qty.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            deAutoOver  = oe-ordl.over-pct:INPUT-VALUE  IN FRAME {&frame-name}
            deAutoUnder = oe-ordl.under-pct:INPUT-VALUE IN FRAME {&frame-name}
            .

        IF oeround-int LT 6 THEN
            oe-ordl.price:FORMAT =
                SUBSTR(oe-ordl.price:FORMAT,1,
                LENGTH(oe-ordl.price:FORMAT) - (6 - oeround-int)) NO-ERROR.
                 
        IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
            ASSIGN fiPromDtLabel:screen-value = "     Mfg Date:"
                fiPromDtLabel              = "     Mfg Date:".
        ELSE 
            ASSIGN fiPromDtLabel:screen-value = "Promise Date:"
                fiPromDtLabel              = "Promise Date:".

        /* when update */
        IF ip-type NE "view" AND NOT oe-ordl.is-a-component THEN 
        DO: 
            RUN enable_UI. 

            IF v-oeCustPartInt EQ 2 THEN 
                DISABLE oe-ordl.part-no WITH FRAME {&frame-name}.
            ELSE
                ENABLE oe-ordl.part-no WITH FRAME {&frame-name}.

            IF oescreen-cha = "Item-Qty" 
                AND oescreen-log 
                AND asi.oe-ordl.est-no:SCREEN-VALUE EQ "" THEN 
            DO:
                ASSIGN 
                    v-widhand      = asi.oe-ordl.qty:HANDLE
                    v-pricehand    = asi.oe-ordl.price:HANDLE
                    v-custPartHand = asi.oe-ordl.spare-char-2:HANDLE.
                asi.oe-ordl.i-no:MOVE-BEFORE-TAB-ITEM(v-widhand).
                asi.oe-ordl.part-no:MOVE-AFTER-TAB-ITEM(v-custPartHand).
            END.
            RUN get-prom-dt-info.
            RUN get-eb-info.
            RUN new-type.
            RUN new-s-man (0).

            IF AVAILABLE itemfg THEN
                ASSIGN spare-dec-1:SCREEN-VALUE = STRING(itemfg.spare-dec-1).
            IF oe-ordl.spare-int-2 > 0 THEN 
                fi_jobStartDate:SCREEN-VALUE = STRING(DATE(oe-ordl.spare-int-2)).

            IF INDEX(ip-type,"Update-") NE 0 AND oe-ordl.est-no NE "" THEN
                RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
            RUN pGetPartComm (YES) .
            ASSIGN 
                btn_done:hidden = YES.
            RUN custom/framechk.p (1, FRAME {&FRAME-NAME}:HANDLE).

            IF runship-char EQ "RUN&SHIP Prompt" THEN 
            DO:
                IF oe-ordl.est-no:SCREEN-VALUE GT ""  THEN
                    asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
                    /*asi.oe-ordl.whsed:SENSITIVE = NO  - 11301204 */.
            END.
            ELSE IF runship-char EQ "DefaultOnly" AND runship-log = YES THEN
                    IF oe-ordl.est-no:SCREEN-VALUE GT ""  THEN
                        asi.oe-ordl.whsed:SCREEN-VALUE = "YES".

        END. 
        /* When View */
        ELSE 
        DO:
            RUN display-item.
            ASSIGN 
                btn_done:hidden    = NO
                btn_done:sensitive = YES
                btn_ok:hidden      = YES
                btn_cancel:hidden  = YES 
                btn_hist:hidden    = YES.
        END.
        IF NOT v-oecomm-log THEN RUN hide-comm (YES).
        first-cust-part-no = oe-ordl.part-no.

        IF v-quo-price-int EQ 1 AND v-quo-price-log AND oe-ordl.est-no NE "" THEN
            DISABLE oe-ordl.price oe-ordl.pr-uom.

        RUN oescreen-values.

        IF oe-ord.est-no NE "" THEN 
        DO:
            DISABLE oe-ordl.est-no.
      
            FIND FIRST est
                WHERE est.company EQ oe-ordl.company
                AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no))) +
                TRIM(oe-ord.est-no)
                NO-LOCK NO-ERROR.
      
            IF ll-master THEN 
            DO WITH FRAME {&FRAME-NAME}:
                RUN new-tandem.
                IF NOT v-qty-mod THEN APPLY "choose" TO Btn_Cancel.
            END.
      
            IF AVAILABLE est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN 
            DO:
                RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-is-tandem).
                IF oe-ordl.qty NE 0 AND NOT ll-is-tandem AND
                    oe-ordl.est-no EQ oe-ord.est-no THEN DISABLE oe-ordl.qty.
                IF ll-new-record AND NOT ll-do-entry AND lv-new-tandem EQ ? AND
                    oe-ordl.i-no NE "" AND oe-ordl.price NE 0                THEN 
                DO:
                    APPLY "choose" TO btn_ok.
                    RETURN.
                END.
            END.
        END.

        IF ip-type EQ "update" OR oe-ordl.i-no GT "0" THEN 
        DO:
            DISABLE oe-ordl.est-no oe-ordl.i-no /*oe-ordl.part-no*/   .
            ll-ok-i-no = CAN-FIND(FIRST itemfg
                WHERE itemfg.company EQ oe-ordl.company
                AND itemfg.i-no    EQ oe-ordl.i-no).
                                  
            deAutoOver = oe-ordl.over-pct:INPUT-VALUE IN FRAME {&frame-name}.
            deAutoUnder = oe-ordl.under-pct:INPUT-VALUE IN FRAME {&frame-name}.
        END.

        /*     IF runship-char EQ "RUN&SHIP Prompt" THEN DO: */
        /*        DISABLE asi.oe-ordl.whsed.                 */
        /*     END.                                          */

        IF NOT oescreen-log OR 
            (oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE NE "") THEN
            ASSIGN
                fi_qty-uom:SENSITIVE = NO
                fi_qty-uom:HIDDEN    = YES.

        IF ip-type EQ "WebUpdate" THEN 
        DO:  
            DISABLE {&ENABLED-FIELDS} WITH FRAME {&FRAME-NAME}. 
            ENABLE oe-ordl.qty.
        END. /* webupdate */

        /*    END.*/
 
        IF ip-type NE "view" THEN 
        DO:
            IF llOEPrcChg-sec OR fIsCustPriceHoldExempt(oe-ordl.company, oe-ordl.cust-no, oe-ordl.ship-id) THEN  
                ASSIGN
                    oe-ordl.price:SENSITIVE  IN FRAME {&FRAME-NAME}  = YES
                    oe-ordl.pr-uom:SENSITIVE  IN FRAME {&FRAME-NAME} = YES .
            ELSE 
            DO:  
                ASSIGN
                    oe-ordl.price:SENSITIVE  IN FRAME {&FRAME-NAME}  = NO
                    oe-ordl.pr-uom:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.
            END.    
            oe-ordl.SourceEstimateID:SENSITIVE  IN FRAME {&FRAME-NAME} = NO.  
            IF oescreen-cha EQ "item-qty" AND ip-type EQ "ADD" AND oe-ord.est-no NE ""   THEN
                APPLY "entry" TO oe-ordl.i-no IN FRAME {&FRAME-NAME}.
        END.

        IF fgsecurity-log THEN
        DO:
            FIND FIRST usergrps WHERE
                usergrps.usergrps = fgsecurity-char
                NO-LOCK NO-ERROR.

            IF AVAILABLE usergrps AND
                (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
                TRIM(usergrps.users) NE "*") THEN
                ASSIGN
                    oe-ordl.cost:VISIBLE IN FRAME {&FRAME-NAME} = NO.
        END.

        IF NOT oescreen-log
            OR (oescreen-log AND asi.oe-ordl.est-no:SCREEN-VALUE  IN FRAME {&FRAME-NAME} NE "") THEN
            ASSIGN
                asi.oe-ordl.spare-char-2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                asi.oe-ordl.spare-char-2:HIDDEN IN FRAME {&FRAME-NAME}    = YES
                asi.oe-ordl.spare-dec-1:SENSITIVE IN FRAME {&FRAME-NAME}  = NO
                asi.oe-ordl.spare-dec-1:HIDDEN IN FRAME {&FRAME-NAME}     = YES.
        ELSE
            ASSIGN
                asi.oe-ordl.spare-char-2:SENSITIVE IN FRAME {&FRAME-NAME} = NO
                asi.oe-ordl.spare-dec-1:SENSITIVE IN FRAME {&FRAME-NAME}  = NO.

        RUN spGetSettingByName("DisplayFGLocationDetails", OUTPUT cDisplayFGLocationDetails).

        IF cDisplayFGLocationDetails EQ ? THEN
            cDisplayFGLocationDetails = "NO".
        IF cDisplayFGLocationDetails NE "NO" THEN 
        DO:
            RUN sys/ref/nk1look.p (
                cocode,
                "FGDefaultQtyDisplay",
                "I",
                NO, 
                YES,
                "",
                "",
                OUTPUT cFGDefaultQtyDisplay,
                OUTPUT lFound
                ).
            IF lFound THEN 
                iPrintAvailQty = INTEGER(cFGDefaultQtyDisplay).
            IF iPrintAvailQty EQ 0 THEN
                iPrintAvailQty = 1.
            ASSIGN
                iPrintAvailQty:SCREEN-VALUE = STRING(iPrintAvailQty)
                btnViewDetail:SENSITIVE     = YES
                btnViewDetail:HIDDEN        = NO
                .
            IF cDisplayFGLocationDetails EQ "YES" THEN
                RUN pViewDetail ("Locations").
            ELSE
                RUN pViewDetail ("").
        END.
        ELSE
            FRAME {&FRAME-NAME}:WIDTH = 146.

        IF NOT llOEDiscount THEN
            asi.oe-ordl.disc:SENSITIVE IN FRAME {&FRAME-NAME} = NO.  
    END.

    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

&Scoped-define Source OU1
{methods/build-table.i "b"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ask-release-questions d-oeitem 
PROCEDURE ask-release-questions :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER ipcShipTo AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER ipcShipFrom AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplRelFlg2 AS LOGICAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNumShipTo AS INTEGER     NO-UNDO.

    DEFINE VARIABLE llRelFlg2   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE liNumShipto AS INTEGER   NO-UNDO.

    DEFINE VARIABLE v-ship-from AS CHARACTER NO-UNDO.

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-rel    FOR oe-rel.
    v-ship-from = ipcShipFrom.
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ iprOeOrdl NO-LOCK NO-ERROR.

    /* Default is 'yes', i.e. to create the release */
    llRelFlg2 = TRUE.
    /* v-relflg is a global set by sys-ctrl oerelease */    
    IF v-relflg THEN
        MESSAGE  " CREATE RELEASE ? "
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE llRelFlg2.


    oplRelFlg2 = llRelFlg2.


    liNumShipTo = 0.
    FOR EACH shipto WHERE shipto.company EQ cocode
        AND shipto.cust-no EQ b-oe-ordl.cust-no
        NO-LOCK:
        ASSIGN 
            liNumShipTo = liNumShipTo + 1.
    END.
    opiNumShipTo = liNumShipTo.
   
    /* Now must always prompt if also prompting for ship-from */
    IF liNumShipTo GT 1 OR llOeShipFromLog THEN 
    DO:
        /* If it's OESHIPTO and they've alread chosen a v-ship-id, then keep it for the next item */
        IF NOT (oeship-cha = "OESHIPTO" AND v-ship-id GT "") THEN 
        DO:
                        
            IF oe-ord.est-no EQ "" AND b-oe-ordl.est-no NE "" AND oeship-cha EQ "EstShipto" THEN 
            DO:
                FOR EACH eb
                    WHERE eb.company  EQ b-oe-ordl.company
                    AND eb.est-no   EQ b-oe-ordl.est-no
                    AND eb.cust-no  EQ oe-ord.cust-no
                    AND eb.ship-id  NE ""
                    NO-LOCK
                    BREAK BY eb.stock-no DESCENDING:
                    IF LAST(eb.stock-no)           OR
                        eb.stock-no EQ b-oe-ordl.i-no THEN 
                    DO:
                        v-ship-id = eb.ship-id.
                        LEAVE.
                    END.
                END.
            END. /* oe-ord.est-no EQ "" AND oe-ordl.est-no NE "" */
            ELSE 
            DO: 
                FOR EACH shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ oe-ord.cust-no
                    AND (shipto.ship-id = oe-ord.ship-id OR oe-ord.ship-id EQ "")
                    NO-LOCK
                    BREAK BY shipto.ship-no DESCENDING:
                    IF oe-ord.ship-id NE "" THEN 
                    DO:
                        v-ship-id = shipto.ship-id.
                        LEAVE.
                    END.
                    ELSE IF shipto.ship-id EQ oe-ord.cust-no THEN 
                        DO:
                            v-ship-id = shipto.ship-id.
                            LEAVE.
                        END.
                END. /* for each shipto */
            END. /* else do*/
        END.
    
        /* task# 09160502 */                                                                                                              
        IF oe-ord.est-no = "" THEN
            FIND FIRST bf-ordl WHERE bf-ordl.company = b-oe-ordl.company
                AND bf-ordl.ord-no = b-oe-ordl.ord-no
                AND bf-ordl.i-no <> ""
                AND RECID(bf-ordl) <> RECID(b-oe-ordl) 
                NO-LOCK NO-ERROR.
    
        IF  NOT llOeShipFromLog 
            AND AVAILABLE bf-ordl AND oeship-cha = "OEShipto" 
            AND NOT (v-orig-ip-type EQ "ADD" AND lv-multi-select = YES) THEN 
        DO:
            FIND FIRST bf-rel WHERE bf-rel.company EQ bf-ordl.company
                AND bf-rel.ord-no  EQ bf-ordl.ord-no
                AND bf-rel.i-no    EQ bf-ordl.i-no
                AND bf-rel.LINE    EQ bf-ordl.LINE 
                NO-LOCK NO-ERROR.
            v-ship-id = IF AVAILABLE bf-rel THEN bf-rel.ship-id ELSE v-ship-id.
        END.
        ELSE
            /* gdm - 06220908 */
            IF llRelFlg2 OR llOeShipFromLog THEN 
            DO:
                IF v-ship-id EQ "" THEN 
                DO:
                    /* In case no default shipto exists for this cust */
                    FIND FIRST shipto
                        WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ oe-ord.cust-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE shipto THEN
                        v-ship-id = shipto.ship-id.
                END.
                IF v-ship-id NE "" THEN 
                DO:
                    FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.ship-id EQ v-ship-id
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE shipto AND v-ship-from EQ "" THEN
                        v-ship-from = shipto.loc.
                END.
                IF llOeShipFromLog THEN 
                DO:
                    RUN oe/d-shipid.w (INPUT b-oe-ordl.cust-no,
                        INPUT oe-ordl.qty, INPUT oe-ordl.i-no,
                        INPUT-OUTPUT v-ship-id,
                        INPUT-OUTPUT v-ship-from).
                END.
            END.
        ipcShipTo = v-ship-id.
        ipcShipFRom = v-ship-from.
    END. /* # ship to's is > 1 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-alloc-qty d-oeitem 
PROCEDURE calc-alloc-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-alloc AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-type AS cha NO-UNDO.
       
    FIND FIRST itemfg WHERE itemfg.company = cocode AND
        itemfg.i-no = oe-ordl.i-no
        NO-LOCK NO-ERROR.                          
    op-alloc = 0.
  
    FOR EACH oe-ordl WHERE oe-ordl.company EQ cocode
        AND oe-ordl.i-no    EQ itemfg.i-no
        USE-INDEX item NO-LOCK,   
        FIRST oe-ord  WHERE oe-ord.company EQ cocode
        AND oe-ord.ord-no  EQ oe-ordl.ord-no
        AND index("CZ",oe-ord.stat) EQ 0
        USE-INDEX ord-no NO-LOCK:
          
        FOR EACH oe-rel WHERE oe-rel.company EQ cocode
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            USE-INDEX ord-item NO-LOCK:
        
            {oe/rel-stat.i v-type} 
            IF v-type EQ "P" THEN op-alloc = op-alloc + oe-rel.qty.
            ELSE 
                IF v-type EQ "Z" AND NOT oe-ctrl.u-inv THEN
                    FOR EACH inv-line FIELDS(ship-qty) WHERE inv-line.company EQ cocode
                        AND inv-line.ord-no  EQ oe-bolh.ord-no
                        AND inv-line.b-no    EQ oe-bolh.b-no
                        AND inv-line.i-no    EQ oe-boll.i-no
                        AND inv-line.line    EQ oe-boll.line
                        NO-LOCK:        
                        op-alloc = op-alloc + inv-line.ship-qty.
                    END.
        END.
    
        FOR EACH oe-relh  WHERE oe-relh.company EQ cocode
            AND oe-relh.ord-no  EQ oe-ordl.ord-no
            AND oe-relh.posted  EQ NO
            AND oe-relh.deleted EQ NO
            USE-INDEX order NO-LOCK,    
            EACH oe-rell FIELDS(qty)  WHERE oe-rell.company EQ cocode
            AND oe-rell.r-no    EQ oe-relh.r-no
            AND oe-rell.ord-no  EQ oe-ordl.ord-no
            AND oe-rell.i-no    EQ oe-ordl.i-no
            AND oe-rell.line    EQ oe-ordl.line
            NO-LOCK:
            op-alloc = op-alloc + oe-rell.qty.  
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-qty d-oeitem 
PROCEDURE calc-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*DO WITH FRAME {&FRAME-NAME}:
  oe-ordl.qty:SCREEN-VALUE = 
      STRING((DEC(oe-ordl.cases:SCREEN-VALUE) *
              DEC(oe-ordl.cas-cnt:SCREEN-VALUE)) +
             DEC(oe-ordl.partial:SCREEN-VALUE),
             oe-ordl.qty:FORMAT).
END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-duplicateFGDayClient d-oeitem 
PROCEDURE check-duplicateFGDayClient :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER b-oe-ord  FOR oe-ord.

    DEFINE VARIABLE v-cnt AS INTEGER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        FOR EACH b-oe-ordl NO-LOCK WHERE b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.cust-no EQ oe-ordl.cust-no
            AND b-oe-ordl.i-no    EQ oe-ordl.i-no
            AND RECID(b-oe-ordl) NE RECID(oe-ordl),
            FIRST b-oe-ord WHERE b-oe-ord.company = b-oe-ordl.company
            AND b-oe-ord.ord-no  = b-oe-ordl.ord-no
            AND b-oe-ord.ord-date = oe-ord.ord-date NO-LOCK:
            v-cnt = v-cnt + 1.

        END.
        IF v-cnt > 0 THEN
            RUN oe/d-oeckit.w(INPUT oe-ordl.company,
                INPUT oe-ordl.cust-no,
                INPUT oe-ordl.i-no,
                INPUT oe-ord.ord-date).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-quote d-oeitem 
PROCEDURE check-quote :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-part-no LIKE oe-ordl.part-no NO-UNDO.

    DEFINE VARIABLE ll           AS LOG     NO-UNDO.
    DEFINE VARIABLE ldQuotePrice AS DECIMAL NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT lQuotePriceMatrix AND AVAILABLE xest AND v-quo-price-log      AND
            v-quo-price-dec EQ 1 AND oe-ordl.est-no:SCREEN-VALUE NE "" THEN 
        DO:

            FOR EACH quotehd
                WHERE quotehd.company EQ cocode
                AND quotehd.loc     EQ xest.loc
                AND quotehd.est-no  EQ xest.est-no
                NO-LOCK,
    
                EACH quoteitm OF quotehd
                WHERE (quoteitm.part-no  EQ oe-ordl.part-no:SCREEN-VALUE OR
                (quoteitm.part-no EQ ip-part-no AND ip-part-no NE ""))
                NO-LOCK,

                EACH quoteqty OF quoteitm
                WHERE quoteqty.qty LE INT(oe-ordl.qty:SCREEN-VALUE)
                NO-LOCK

                BY quoteqty.qty DESCENDING:
                ldQuotePrice = quoteqty.price.
                LEAVE.
            END.
              
            ll = AVAILABLE quoteqty.

            IF ll THEN 
            DO:
                ll = quoteqty.qty EQ INT(oe-ordl.qty:SCREEN-VALUE).

                IF NOT ll THEN
                    MESSAGE "Quote does not exist for order quantity, import smaller quoted quantity sell price?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                        UPDATE ll.
            END.

            ELSE MESSAGE "No quotes exists for estimate/quantity..."
                    VIEW-AS ALERT-BOX ERROR.

            IF NOT ll THEN 
            DO:  
                APPLY "entry" TO oe-ordl.qty.
                RETURN ERROR.
            END.
            ELSE 
            DO:  /* get lower qty price from quote */
                IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
                    tt-item-qty-price.tt-selected = YES AND
                    (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                    (tt-item-qty-price.part-no EQ ip-part-no AND ip-part-no EQ ""))) 
                    THEN 
                DO:

                    FOR EACH quotehd NO-LOCK 
                        WHERE quotehd.company EQ oe-ord.company AND
                        quotehd.est-no EQ oe-ordl.est-no:SCREEN-VALUE AND 
                        quotehd.quo-date LE TODAY AND
                        (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?) ,
                        EACH quoteitm OF quotehd WHERE quoteitm.company = oe-ord.company AND
                        quoteitm.est-no = oe-ordl.est-no:SCREEN-VALUE  AND
                        (quoteitm.part-no = ip-part-no OR ip-part-no <> ""),
                        EACH quoteqty WHERE quoteqty.company = quoteitm.company AND
                        quoteqty.loc = quoteitm.loc AND
                        quoteqty.q-no = quoteitm.q-no AND
                        quoteqty.line = quoteitm.LINE AND
                        quoteqty.qty < INT(oe-ordl.qty:SCREEN-VALUE)
                        BY quoteitm.q-no DESCENDING
                        BY quoteitm.LINE
                        BY quoteqty.qty DESCENDING:

                        CREATE tt-item-qty-price.
                        ASSIGN 
                            tt-item-qty-price.q-no        = quoteitm.q-no
                            tt-item-qty-price.LINE        = quoteitm.LINE
                            tt-item-qty-price.quote-date  = quoteqty.quote-date
                            tt-item-qty-price.part-no     = quoteitm.part-no
                            tt-item-qty-price.qty         = quoteqty.qty
                            tt-item-qty-price.price       = quoteqty.price
                            tt-item-qty-price.uom         = quoteqty.uom
                            tt-item-qty-price.rels        = quoteqty.rels
                            tt-item-qty-price.quote-user  = quoteqty.quote-user
                            tt-item-qty-price.tt-selected = YES
                            llGotLowerPrice               = YES.
        
                        LEAVE.
                    END.
                END.
            END.
        END.
    END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-quote-qty d-oeitem 
PROCEDURE check-quote-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-est-no  AS cha NO-UNDO.
    DEFINE VARIABLE v-tmp-part AS cha NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
  
        lv-est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no:SCREEN-VALUE))) +
            TRIM(oe-ordl.est-no:SCREEN-VALUE).
        IF lv-est-no NE "" AND NOT AVAILABLE xest THEN
            FIND FIRST xest WHERE xest.company EQ cocode AND
                xest.est-no EQ lv-est-no NO-LOCK NO-ERROR.
        v-tmp-part = oe-ordl.i-no:screen-value.
    
        RUN check-quote (v-tmp-part) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR .  

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chooseQuotedPrice d-oeitem 
PROCEDURE chooseQuotedPrice :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lxPrice     LIKE oe-ordl.price NO-UNDO.
    DEFINE VARIABLE lxUom       LIKE oe-ordl.pr-uom NO-UNDO.
    DEFINE VARIABLE lxQty       LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE lcChoice    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQutNo      AS INTEGER   NO-UNDO .
    DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuoteEst   AS CHARACTER NO-UNDO.

    IF lQuotePriceMatrix THEN
    DO:
        MESSAGE "Quotes should not be used as NK1 = QuotePriceMatrix is set such that only the price matrix should be used."
            VIEW-AS ALERT-BOX INFORMATION .
        RETURN.        
    END.

    DO WITH FRAME {&FRAME-NAME}:
        cQuoteEst = IF oe-ordl.SourceEstimateID:SCREEN-VALUE NE "" THEN oe-ordl.SourceEstimateID:SCREEN-VALUE ELSE oe-ordl.est-no:SCREEN-VALUE .
        RUN oe/d-quotedprices.w("Button",cocode,
            locode,
            cQuoteEst,
            oe-ordl.cust-no,
            oe-ordl.part-no:SCREEN-VALUE,
            oe-ordl.i-no:SCREEN-VALUE,
            INPUT-OUTPUT lxPrice,
            INPUT-OUTPUT lxUom,
            INPUT-OUTPUT lxQty,
            INPUT-OUTPUT iQutNo,
            OUTPUT lcChoice).


        CASE lcChoice:
            WHEN "PRICE" THEN 
                DO:
                    ASSIGN 
                        oe-ordl.price:SCREEN-VALUE  = STRING(lxPrice,">>>,>>>,>99.99<<<")
                        oe-ordl.pr-uom:SCREEN-VALUE = lxUom
                        .
                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Quoted Price - Quote Number:" + string(iQutNo) + " Quantity:" + string(lxQty)
                        ).
                END.
            WHEN "PRICEQTY" THEN 
                DO:
                    ASSIGN
                        oe-ordl.price:SCREEN-VALUE  = STRING(lxPrice,">>>,>>>,>99.99<<<")                
                        oe-ordl.pr-uom:SCREEN-VALUE = lxUom
                        oe-ordl.qty:SCREEN-VALUE    = STRING(lxQty,">>>,>>>,>>9")
                        .
                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Quoted Price - Quote Number:" + string(iQutNo) + " Quantity:" + string(lxQty)
                        ).
                END.
        END CASE.
        RUN Conv_CalcTotalPrice(cocode, 
            oe-ordl.i-no:SCREEN-VALUE,
            DECIMAL(oe-ordl.qty:SCREEN-VALUE),
            DECIMAL(oe-ordl.price:SCREEN-VALUE),
            oe-ordl.pr-uom:SCREEN-VALUE,
            DECIMAL(oe-ordl.disc:SCREEN-VALUE),
            DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
            OUTPUT dTotalPrice).
        oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
    //{oe/ordltot.i oe-ordl qty oe-ordl} 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clean-s-pct d-oeitem 
PROCEDURE clean-s-pct :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/   
    DEFINE VARIABLE ld-pct LIKE oe-ordl.s-pct EXTENT 4 NO-UNDO.
    DEFINE VARIABLE li     AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            ld-pct[1] = DEC(oe-ordl.s-pct[1]:SCREEN-VALUE)
            ld-pct[2] = DEC(oe-ordl.s-pct[2]:SCREEN-VALUE)
            ld-pct[3] = DEC(oe-ordl.s-pct[3]:SCREEN-VALUE)
            ld-pct[4] = ld-pct[1] + ld-pct[2] + ld-pct[3].

        IF ld-pct[4] NE 100 THEN 
        DO li = 1 TO 3:
            ld-pct[li] = ld-pct[li] / ld-pct[4] * 100.
        END.

        ld-pct[1] = ld-pct[1] + (100 - (ld-pct[1] + ld-pct[2] + ld-pct[3])).

        ASSIGN
            oe-ordl.s-pct[1]:SCREEN-VALUE = STRING(ld-pct[1])
            oe-ordl.s-pct[2]:SCREEN-VALUE = STRING(ld-pct[2])
            oe-ordl.s-pct[3]:SCREEN-VALUE = STRING(ld-pct[3]).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CopyShipNote d-oeitem 
PROCEDURE CopyShipNote PRIVATE :
/*------------------------------------------------------------------------------
     Purpose: Copies Ship Note from rec_key to rec_key
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-item d-oeitem 
PROCEDURE create-item :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-ans AS LOG INIT NO NO-UNDO.

    DEFINE BUFFER b-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    FIND oe-ord WHERE oe-ord.company = g_company 
        AND oe-ord.ord-no = ip-ord-no NO-LOCK NO-ERROR.

    IF AVAILABLE oe-ord THEN 
    DO:

        CREATE bf-oe-ordl.
        ASSIGN 
            lv-item-recid = RECID(bf-oe-ordl)
            ll-new-record = YES.
        FIND FIRST cust {sys/ref/custW.i} AND cust.cust-no = oe-ord.cust-no
            USE-INDEX cust NO-LOCK.
        ASSIGN
            bf-oe-ordl.company   = cocode
            bf-oe-ordl.ord-no    = oe-ord.ord-no
            bf-oe-ordl.type-code = oe-ord.type
            bf-oe-ordl.cust-no   = oe-ord.cust-no
            bf-oe-ordl.ship-id   = oe-ord.ship-id
            bf-oe-ordl.po-no     = oe-ord.po-no
            bf-oe-ordl.req-code  = oe-ord.due-code
            bf-oe-ordl.req-date  = oe-ord.due-date
            bf-oe-ordl.prom-code = oe-ord.due-code
            bf-oe-ordl.prom-date = oe-ord.due-date
            bf-oe-ordl.disc      = cust.disc
            bf-oe-ordl.over-pct  = oe-ord.over-pct   
            bf-oe-ordl.under-pct = oe-ord.under-pct
            .

        IF lastship-cha = "Stock/Custom" AND NOT (oeDateAuto-log AND OeDateAuto-Char = "Colonial" ) THEN 
        DO:
            /* If order has no estimate. */
            IF bf-oe-ordl.est-no = "" THEN
                ASSIGN bf-oe-ordl.req-date = (TODAY + lastship-int).
            ELSE
                ASSIGN bf-oe-ordl.req-date = (TODAY + INT(lastship-dec)).

            ASSIGN 
                bf-oe-ordl.prom-date = bf-oe-ordl.req-date.
        END.
    
        bf-oe-ordl.tax = fGetTaxable(oe-ord.company, oe-ord.cust-no, oe-ord.ship-id, "").
  
        FOR LAST b-oe-ordl OF oe-ord
            WHERE ROWID(b-oe-ordl) NE ROWID(bf-oe-ordl)
            NO-LOCK
            BY b-oe-ordl.line:
            bf-oe-ordl.line = b-oe-ordl.line + 1.
        END.
        IF bf-oe-ordl.line EQ 0 THEN oe-ordl.line = 1.

        /*
        if oe-ord.est-no ne "" then
           assign
            oe-ordl.job-no  = oe-ord.job-no
            oe-ordl.job-no2 = oe-ord.job-no2
            oe-ordl.est-no   = oe-ord.est-no
            oe-ordl.e-num    = oe-ord.e-num
            oe-ordl.est-type = oe-ord.est-type.
        */
             
        DO i = 1 TO 3:

            ASSIGN
                bf-oe-ordl.s-man[i]  = oe-ord.sman[i]
                bf-oe-ordl.s-pct[i]  = oe-ord.s-pct[i]
                bf-oe-ordl.s-comm[i] = oe-ord.s-comm[i].
        END.

        ASSIGN
            bf-oe-ordl.q-qty   = oe-ord.t-fuel
            v-margin           = oe-ord.t-fuel
            bf-oe-ordl.managed = oe-ord.managed
            .

        IF runship-char EQ "DefaultOnly" THEN 
        DO:
            bf-oe-ordl.whsed = YES.
            /*asi.oe-ordl.whsed:SENSITIVE = NO  - 11301204 */.
        END.

        IF bf-oe-ordl.LINE EQ 1 OR NOT AVAILABLE oe-ordl THEN
            FIND oe-ordl WHERE ROWID(oe-ordl) = ROWID(bf-oe-ordl) NO-ERROR.
  
    END. /* avail oe-ord */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-job d-oeitem 
PROCEDURE create-job :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipEstNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER op-recid AS RECID NO-UNDO.
    DEFINE VARIABLE v-job-job  LIKE job.job NO-UNDO.
    DEFINE VARIABLE v-job-no   LIKE job.job-no NO-UNDO.
    DEFINE VARIABLE v-job-no2  LIKE job.job-no2 NO-UNDO.
    DEFINE VARIABLE li-j-no    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-i        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-prod-cat AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
        
    /* === from oe/oe-ord1.p  ============= */
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
                            
    v-job-job = 1.
    FIND LAST job WHERE job.company EQ cocode USE-INDEX job NO-LOCK NO-ERROR.
    FIND LAST job-hdr WHERE job-hdr.company EQ cocode
        USE-INDEX job NO-LOCK NO-ERROR.
    /* In case job is not found and 1 is not the true last job# */
    IF AVAILABLE job-hdr AND  job-hdr.job GT v-job-job THEN v-job-job = job-hdr.job + 1.
    IF AVAILABLE job AND job.job GE v-job-job THEN v-job-job = job.job + 1.

    DO v-i = 1 TO 10:
        FIND job WHERE job.company EQ cocode 
            AND job.job = v-job-job USE-INDEX job 
            NO-LOCK NO-ERROR.
        IF  NOT AVAILABLE job THEN
            LEAVE.      
        v-job-job = v-job-job + 1.
    END.

    IF oe-ord.job-no <> "" THEN 
        ASSIGN v-job-no  = oe-ord.job-no
            v-job-no2 = oe-ord.job-no2.
    ELSE  IF oe-ordl.job-no EQ "" THEN 
        DO:
            FIND FIRST est
                WHERE est.company EQ cocode
                AND est.est-no  EQ ipEstNo NO-LOCK NO-ERROR.
            IF AVAILABLE est THEN  
                FIND FIRST eb
                    WHERE eb.company  EQ oe-ordl.company
                    AND eb.est-no   EQ ipEstNo
                    AND eb.cust-no  EQ oe-ord.cust-no NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN 
                v-prod-cat = eb.procat.
            v-job-no = FILL(" ",6 - length(TRIM(STRING(oe-ordl.ord-no)))) + string(oe-ordl.ord-no).
            RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                INPUT-OUTPUT v-job-no2,
                INPUT v-prod-cat,
                INPUT FILL(" ",6 - LENGTH(TRIM(oe-ordl.est-no))) + TRIM(oe-ordl.est-no)).
     
            IF v-job-no NE "" THEN 
            DO:
                ASSIGN
                    oe-ordl.job-no  = v-job-no
                    oe-ordl.job-no2 = v-job-no2.
        
                IF oe-ordl.est-no EQ "" THEN 
                    ASSIGN 
                        oe-ordl.est-no = ipEstNo.
            
                DISPLAY oe-ordl.job-no oe-ordl.job-no2 oe-ordl.est-no WITH FRAME {&frame-name}.
            END.
        END.
        ELSE
            IF oe-ordl.job-no NE "" THEN
                ASSIGN v-job-no  = oe-ordl.job-no
                    v-job-no2 = oe-ordl.job-no2.

    IF v-job-no NE "" THEN
        FOR EACH job
            WHERE job.company EQ cocode
            AND job.job-no  EQ v-job-no
            AND job.job-no2 EQ v-job-no2:
            DELETE job.
        END.
  
    CREATE job.
    ASSIGN 
        job.job        = v-job-job
        job.company    = cocode
        job.loc        = locode
        job.est-no     = ipEstNo
        job.job-no     = oe-ordl.job-no
        job.job-no2    = oe-ordl.job-no2
        job.stat       = "P"
        job.ordertype  = oe-ord.type
        job.csrUser_id = IF AVAILABLE oe-ord THEN oe-ord.csrUser_id ELSE ""
        op-recid       = RECID(job) 
        job.shipFromLocation = locode
        .

    FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
        AND job-hdr.job-no  EQ oe-ordl.job-no
        AND job-hdr.job-no2 EQ oe-ordl.job-no2
        AND job-hdr.ord-no  EQ oe-ordl.ord-no
        AND job-hdr.i-no    EQ oe-ordl.i-no NO-ERROR.

    IF NOT AVAILABLE job-hdr THEN 
    DO:
        FIND FIRST itemfg WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.
        CREATE job-hdr.
        ASSIGN 
            job-hdr.company = cocode
            job-hdr.loc     = locode
            job-hdr.e-num   = oe-ordl.e-num
            job-hdr.est-no  = ipEstNo
            job-hdr.i-no    = oe-ordl.i-no
            /*     job-hdr.qty          = oe-ordl.qty */
            job-hdr.cust-no = oe-ordl.cust-no
            job-hdr.ord-no  = oe-ordl.ord-no
            job-hdr.po-no   = oe-ordl.po-no
            job-hdr.job     = job.job
            job-hdr.job-no  = job.job-no
            job-hdr.job-no2 = job.job-no2
            .

        IF AVAILABLE itemfg THEN
            ASSIGN job-hdr.std-mat-cost = itemfg.std-mat-cost
                job-hdr.std-lab-cost = itemfg.std-lab-cost
                job-hdr.std-var-cost = itemfg.std-var-cost
                job-hdr.std-fix-cost = itemfg.std-fix-cost.

        ASSIGN 
            job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
    END.
    ASSIGN 
        job-hdr.est-no  = ipEstNo
        job-hdr.job     = job.job
        job-hdr.job-no  = job.job-no
        job-hdr.job-no2 = job.job-no2
        oe-ordl.j-no    = job-hdr.j-no.
    ASSIGN 
        v-job-no  = job.job-no
        v-job-no2 = job.job-no2.
        
    IF oe-ord.stat EQ "H" THEN
        RUN oe/syncJobHold.p (INPUT oe-ord.company, INPUT oe-ord.ord-no, INPUT "Hold").
        
    FIND FIRST bf-oe-rel NO-LOCK 
         WHERE bf-oe-rel.company EQ cocode
         AND bf-oe-rel.ord-no EQ oe-ordl.ord-no
         AND bf-oe-rel.i-no EQ oe-ordl.i-no
         AND bf-oe-rel.LINE EQ oe-ordl.LINE NO-ERROR.
                  
    IF AVAILABLE bf-oe-rel AND bf-oe-rel.spare-char-1 NE "" THEN
    ASSIGN
    job.shipFromLocation = bf-oe-rel.spare-char-1
    job-hdr.loc          = bf-oe-rel.spare-char-1.
           
    RELEASE job.
    RELEASE job-hdr.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-release d-oeitem 
PROCEDURE create-release :
/*------------------------------------------------------------------------------
      Purpose:     from oe/oe-ordl.x for nufile
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-qty-sum           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-nxt-r-no          AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE v-lst-rel           AS DATE      NO-UNDO.
    DEFINE VARIABLE v-pct-chg           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-ship-id           LIKE oe-rel.ship-id NO-UNDO.
    DEFINE VARIABLE v-num-shipto        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-relType           AS cha       NO-UNDO.
    DEFINE VARIABLE cShipSlsRep         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFirstReleaseOfItem AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-relflg2           AS LOGICAL   INIT YES NO-UNDO.
    DEFINE VARIABLE dCalcDueDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE dCalcPromDate       AS DATE      NO-UNDO.
    DEFINE BUFFER bf-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-rel    FOR oe-rel.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
    IF AVAIL(oe-ord) AND NOT AVAILABLE cust THEN 
        FIND FIRST cust WHERE cust.company EQ oe-ord.company
            AND cust.cust-no EQ oe-ord.cust-no 
            NO-LOCK NO-ERROR.
    ASSIGN 
        v-qty-sum = 0.

    FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
        AND oe-rel.ord-no EQ oe-ord.ord-no 
        NO-LOCK NO-ERROR.

    IF gv-ship-from EQ "" AND AVAILABLE oe-rel AND oe-rel.spare-char-1 NE "" THEN
        gv-ship-from = oe-rel.spare-char-1.

    FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
        AND oe-rel.ord-no EQ oe-ord.ord-no
        AND oe-rel.i-no EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.
  
    lFirstReleaseOfItem = IF AVAILABLE oe-rel THEN NO ELSE YES.

    /* when OESHIPTO then retaining same shipid for next prompt */
    IF NOT oeship-cha = "OESHIPTO" THEN 
        v-ship-id = "".
   
    /* Prompt user as required first before proceeding with transaction */
    RUN ask-release-questions (INPUT ROWID(oe-ordl),
        INPUT-OUTPUT v-ship-id,
        INPUT-OUTPUT gv-ship-from,
        OUTPUT v-relflg2,
        OUTPUT v-num-shipto).

    /* prompt is in ask-release-questions */
    IF v-relflg2 THEN 
    DO:  
    {oe/oe-rel.a &fil="oe-ordl"}.
        /* stores oe-rel due date */
        IF lfirstReleaseofItem THEN 
        DO:  
            oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,".     
        END.
    
    END.






    /* gdm - 06220908 end */

    ASSIGN /* v-ship-id = "" */
        lv-qty = oe-ordl.qty.

    FIND FIRST xoe-rel WHERE xoe-rel.company EQ cocode
        AND xoe-rel.ord-no  EQ oe-ordl.ord-no
        AND RECID(xoe-rel)  NE RECID(oe-rel)
        AND xoe-rel.link-no EQ 0
        NO-LOCK NO-ERROR.

    IF TRUE OR ( NOT AVAILABLE xoe-rel OR oe-ordl.est-no NE "" ) THEN 
    DO:


        /* Calculate number of shipto records for this customer in ask-release-questions */
        /*   FOR EACH shipto WHERE shipto.company EQ cocode */
        /*     AND shipto.cust-no EQ oe-ordl.cust-no:       */
        /*     ASSIGN v-num-shipto = v-num-shipto + 1.      */
        /*   END.                                           */
  
        IF v-num-shipto GT 1 THEN
        DO:
            /* More than one ship-to for this customer ... */

            /* gdm - 06220908*/
            IF v-relflg2 OR llOeShipFromLog THEN
                ASSIGN oe-rel.ship-id = TRIM(v-ship-id).
    
            FIND FIRST shipto WHERE shipto.company = cocode AND
                shipto.cust-no = xoe-ord.cust-no  AND
                shipto.ship-id = v-ship-id
                USE-INDEX ship-id NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN 
            DO:

                ASSIGN 
                    v-ship-id = shipto.ship-id.

                IF v-shiptorep-log AND AVAILABLE shipto THEN 
                DO:  /* task 05301401 */
                    IF shipto.spare-char-1 <> "" THEN 
                    DO:
                        FIND CURRENT oe-ordl EXCLUSIVE-LOCK NO-ERROR.
                        FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
                        ASSIGN
                            oe-ordl.s-man[1]                                     = shipto.spare-char-1
                            oe-ordl.s-man[1]:screen-value IN FRAME {&frame-name} = shipto.spare-char-1
                            oe-ord.sman[1]                                       = shipto.spare-char-1 .

                        FIND sman WHERE sman.company = oe-ord.company
                            AND sman.sman = oe-ordl.s-man[1]
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sman THEN ASSIGN oe-ord.sname[1]                                       = sman.sname
                                oe-ord.s-comm[1]                                      = (sman.scomm)
                                oe-ordl.s-comm[1]                                     = (sman.scomm)
                                oe-ordl.s-comm[1]:screen-value IN FRAME {&frame-name} = STRING(sman.scomm).
                        FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
                        FIND CURRENT oe-ord NO-LOCK NO-ERROR.
                    END.
                END.
      
                /* gdm - 06220908 */
                IF v-relflg2 THEN 
                DO:
                    ASSIGN 
                        oe-rel.ship-no      = shipto.ship-no
                        oe-rel.ship-id      = shipto.ship-id
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-addr[2] = shipto.ship-addr[2]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        oe-rel.ship-i[1]    = shipto.notes[1]
                        oe-rel.ship-i[2]    = shipto.notes[2]
                        oe-rel.ship-i[3]    = shipto.notes[3]
                        oe-rel.ship-i[4]    = shipto.notes[4]
                        oe-rel.spare-char-1 = shipto.loc.
                    /* gdm - 06220908 end */
                    RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
                END. /*v-relflg2*/
                /* if add mode then use default carrier */
                /*   if sel = 3 /* and NOT oe-rel.carrier ENTERED */ then do: */
                FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.NAME    EQ "OECARIER"
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE sys-ctrl THEN 
                DO:
                    CREATE sys-ctrl.
                    ASSIGN
                        sys-ctrl.company  = cocode
                        sys-ctrl.NAME     = "OECARIER"
                        sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                        sys-ctrl.char-fld = "ShipTo".
        
                    DO WHILE TRUE:
                        MESSAGE "Default Shipping Carrier from Header or Shipto?"
                            UPDATE sys-ctrl.char-fld.
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE.
                    END.
                END.
      
                /* gdm - 06220908 */
                IF v-relflg2 THEN
                    ASSIGN oe-rel.carrier = IF sys-ctrl.char-fld = "Shipto"
      THEN shipto.carrier
      ELSE xoe-ord.carrier.
      
                IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN
                DO:
                    IF NOT cPromManualChanged AND  cDueManualChanged THEN 
                    DO:
                
                        RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                            INPUT oe-ordl.req-date,
                            INPUT oe-ordl.prom-date,
                            INPUT "DueDate",
                            INPUT ROWID(oe-ordl),
                            OUTPUT dCalcDueDate,
                            OUTPUT dCalcPromDate).
                        FIND CURRENT oe-ordl EXCLUSIVE-LOCK.
                        oe-ordl.prom-date = dCalcPromDate.
                        FIND CURRENT oe-ordl NO-LOCK.
                    END.
                    ELSE IF NOT cDueManualChanged AND  cPromManualChanged THEN 
                        DO:
                
                            RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                                INPUT oe-ordl.req-date,
                                INPUT oe-ordl.prom-date,
                                INPUT "PromiseDate",
                                INPUT ROWID(oe-ordl),
                                OUTPUT dCalcDueDate,
                                OUTPUT dCalcPromDate).
                            FIND CURRENT oe-ordl EXCLUSIVE-LOCK.
                            oe-ordl.req-date = dCalcDueDate.
                            FIND CURRENT oe-ordl NO-LOCK.
                        END.
                END.

            END.
            IF gv-ship-from GT "" THEN
                oe-rel.spare-char-1 = gv-ship-from.
    
            /* Run Freight calculation  */
            RUN oe/oe-frtcl.p.
    
        END.  /* multi ship to */
        ELSE 
        DO:
            /* If not multi ship-to */
            FIND FIRST shipto WHERE shipto.company EQ cocode AND
                shipto.cust-no EQ xoe-ord.cust-no AND
                shipto.ship-id EQ v-ship-id
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE shipto THEN
                FIND FIRST shipto WHERE shipto.company EQ cocode 
                    AND shipto.cust-no EQ xoe-ord.cust-no
                    NO-LOCK NO-ERROR.
            IF AVAILABLE shipto THEN 
            DO:
      
                /* gdm - 06220908 */
                IF v-relflg2 THEN 
                DO:
                    ASSIGN  
                        oe-rel.ship-no      = shipto.ship-no
                        oe-rel.ship-id      = shipto.ship-id
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-addr[2] = shipto.ship-addr[2]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        oe-rel.ship-i[1]    = shipto.notes[1]
                        oe-rel.ship-i[2]    = shipto.notes[2]
                        oe-rel.ship-i[3]    = shipto.notes[3]
                        oe-rel.ship-i[4]    = shipto.notes[4]
                        oe-rel.spare-char-1 = shipto.loc.
                    RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
                END.
                /* check that itemfg-loc exists */
                IF oe-rel.spare-char-1 GT "" THEN
                    RUN fg/chkfgloc.p (INPUT oe-rel.i-no, INPUT oe-rel.spare-char-1).
  
      
                /* if add mode then use default carrier */
                IF ll-new-record /* and NOT oe-rel.carrier ENTERED */ THEN 
                DO:
                    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.NAME    EQ "OECARIER"
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE sys-ctrl THEN 
                    DO:
                        CREATE sys-ctrl.
                        ASSIGN 
                            sys-ctrl.company  = cocode
                            sys-ctrl.NAME     = "OECARIER"
                            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                            sys-ctrl.char-fld = "ShipTo".
          
                        DO WHILE TRUE:
                            MESSAGE "Default Shipping Carrier from Header or Shipto?"
                                UPDATE sys-ctrl.char-fld.
                            IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
                        END.
                    END.
                    /* gdm - 06220908 */
                    IF v-relflg2 THEN
                        oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
        ELSE xoe-ord.carrier.
        
                    /* sman by itemfg overrides that of the shipto */
                    RUN itemfg-sman.
                    IF oe-ordl.s-man[1]:screen-value IN FRAME {&frame-name} <> oe-ordl.s-man[1] THEN 
                    DO:
                        FIND CURRENT oe-ordl EXCLUSIVE-LOCK NO-ERROR.
                        FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.

                        ASSIGN
                            oe-ordl.s-man[1] = oe-ordl.s-man[1]:screen-value IN FRAME {&frame-name}          
                            oe-ord.sman[1]   = oe-ordl.s-man[1] .

                        FIND sman WHERE sman.company = oe-ord.company
                            AND sman.sman = oe-ordl.s-man[1]
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE sman THEN ASSIGN oe-ord.sname[1]                                       = sman.sname
                                oe-ord.s-comm[1]                                      = (sman.scomm)
                                oe-ordl.s-comm[1]                                     = (sman.scomm)
                                oe-ordl.s-comm[1]:screen-value IN FRAME {&frame-name} = STRING(sman.scomm).
                        FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
                        FIND CURRENT oe-ord NO-LOCK NO-ERROR.

                    END.

                END.
            END. /* avail shipto */
        END. /* not multi */
        IF gv-ship-from GT "" THEN
            oe-rel.spare-char-1 = gv-ship-from.

    END. /* if no oe-rel was found */
    ELSE 
    DO:
        /* If oe-rel was already available */
        FIND FIRST shipto WHERE shipto.company = cocode AND
            shipto.cust-no = xoe-ord.cust-no  AND
            shipto.ship-id = xoe-rel.ship-id
            USE-INDEX ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN 
        DO:
            /* gdm - 06220908 */
            IF v-relflg2 THEN 
            DO:
                ASSIGN 
                    oe-rel.ship-no      = shipto.ship-no
                    oe-rel.ship-id      = shipto.ship-id
                    oe-rel.ship-addr[1] = shipto.ship-addr[1]
                    oe-rel.ship-addr[2] = shipto.ship-addr[2]
                    oe-rel.ship-city    = shipto.ship-city
                    oe-rel.ship-state   = shipto.ship-state
                    oe-rel.ship-zip     = shipto.ship-zip
                    oe-rel.ship-i[1]    = shipto.notes[1]
                    oe-rel.ship-i[2]    = shipto.notes[2]
                    oe-rel.ship-i[3]    = shipto.notes[3]
                    oe-rel.ship-i[4]    = shipto.notes[4]
                    oe-rel.spare-char-1 = shipto.loc.
                RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
            END. /*v-relflg2*/
    
            /* if add mode then use default carrier */
            IF ll-new-record THEN 
            DO:
                FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.NAME    EQ "OECARIER"
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE sys-ctrl THEN 
                DO:
                    CREATE sys-ctrl.
                    ASSIGN 
                        sys-ctrl.company  = cocode
                        sys-ctrl.NAME     = "OECARIER"
                        sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                        sys-ctrl.char-fld = "ShipTo".
        
                    DO WHILE TRUE:
                        MESSAGE "Default Shipping Carrier from Header or Shipto?"
                            UPDATE sys-ctrl.char-fld.
                        IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "Sh~ipTo" THEN LEAVE.
                    END.
                END.
                /* gdm - 06220908 */
                IF v-relflg2 THEN
                    oe-rel.carrier   = IF sys-ctrl.char-fld = "Shipto" THEN shipto~.carrier
      ELSE xoe-ord.carrier.
            END.  /* if new record */
        END. /* if avail shipto */
    END. /* ... else (if oe-rel was already available */


    /* Update reftable for order type */
    IF v-relflg2 THEN 
    DO:
  
        /* task 04011103*/
        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.NAME EQ "RelType" NO-LOCK NO-ERROR.
        IF AVAILABLE sys-ctrl THEN
            FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                AND sys-ctrl-ship.ship-id = oe-rel.ship-id NO-LOCK NO-ERROR.
        IF NOT AVAILABLE sys-ctrl-shipto THEN
            FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
                AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
        ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.
  
        IF v-relType <> "" THEN 
        DO:
    
            FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
                EXCLUSIVE-LOCK.
            bf-oe-rel.s-code = IF oe-ordl.is-a-component THEN "S"
            ELSE SUBSTRING(v-relType,1,1).
            IF oe-ord.TYPE = "T" THEN
                ASSIGN bf-oe-rel.s-code = "T".      
            RELEASE bf-oe-rel.
        END. /* v-reltype <> "" */
    END. /* if v-relflg2 */

    /* Assign qty to itemfg-loc */
    RUN fg/fgitmloc.p (INPUT oe-ordl.i-no, INPUT ROWID(oe-ordl)).

    RELEASE oe-rel.
    RELEASE reftable.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-itemfg d-oeitem 
PROCEDURE crt-itemfg :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* -------------------------------------------------- fg/ce-addfg.p 08/98 JLF */
    /* Add FG thru estimating                                                     */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT PARAMETER v-item LIKE itemfg.i-no.
    DEFINE INPUT PARAMETER v-uom LIKE itemfg.prod-uom.
    DEFINE INPUT PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocBin AS CHARACTER NO-UNDO.

    DEFINE VARIABLE tmpstore    AS cha     NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER NO-UNDO.
    DEFINE VARIABLE ll-one-part AS LOG     NO-UNDO.


    DEFINE BUFFER x-eb            FOR eb.
    DEFINE BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE BUFFER b-eb2           FOR eb.

    DEFINE BUFFER bf-itemfg       FOR itemfg.

    {sys/inc/setprint.i}
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    FIND FIRST cust  WHERE cust.company EQ cocode
        AND cust.cust-no EQ xeb.cust-no
        NO-LOCK NO-ERROR.
    {oe\fgfreight.i}


    CREATE itemfg.  /*create.trg applies all defaults for FGMaster*/
    ASSIGN /*order specific overrides to FGMaster and core defaults*/
        itemfg.i-no       = v-item
        itemfg.i-name     = oe-ordl.i-name:SCREEN-VALUE  IN FRAME {&frame-name}
        itemfg.part-dscr1 = oe-ordl.part-dscr1:SCREEN-VALUE 
        itemfg.part-dscr2 = oe-ordl.part-dscr2:SCREEN-VALUE 
        itemfg.part-dscr3 = oe-ordl.part-dscr3:SCREEN-VALUE 
        itemfg.sell-price = DECIMAL(oe-ordl.price:SCREEN-VALUE)

        itemfg.cust-no    = oe-ord.cust-no
        itemfg.cust-name  = oe-ord.cust-name
        itemfg.part-no    = oe-ordl.part-no:screen-value
        itemfg.taxable    = fGetTaxable(itemfg.company, (IF AVAILABLE cust THEN cust.cust-no ELSE ""),"", "") 
        itemfg.sell-uom   = oe-ordl.pr-uom:SCREEN-VALUE
        itemfg.alloc      = IF AVAILABLE xeb AND xeb.est-type LE 4 THEN v-allocf ELSE v-alloc
        .
   

    IF v-graphic-char NE "" THEN 
    DO:
        IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
            v-graphic-char = v-graphic-char + "\".

        IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
            itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
    END.

    IF AVAILABLE xeb THEN 
    DO:
        ASSIGN 
            itemfg.die-no       = xeb.die-no
            itemfg.plate-no     = xeb.plate-no
            itemfg.style        = xeb.style
            itemfg.cad-no       = xeb.cad-no
            itemfg.upc-no       = xeb.upc-no
            itemfg.spc-no       = xeb.spc-no
            itemfg.isaset       = xeb.form-no EQ 0
            itemfg.procat       = xeb.procat
            itemfg.alloc        = xeb.set-is-assembled
            itemfg.pur-man      = xeb.pur-man
            itemfg.trno         = xeb.tr-no 
            itemfg.spare-char-4 = xeb.dest-code.

        /*IF xeb.pur-man THEN itemfg.pur-uom = "EA".*/

        /* see task 10241105 */
        IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.

            {oe/fgfreighta.i "xeb"}


            {fg/set-inks1.i itemfg xeb}
 
        {sys/inc/fgcascnt.i itemfg xeb}

        /* {sys/inc/updfgdim.i "xeb"} replaced with below (02211202) */
        RUN oe/updfgdim.p (INPUT ROWID(xeb), INPUT ROWID(itemfg)).
        FIND CURRENT xeb EXCLUSIVE-LOCK.
        FIND CURRENT itemfg EXCLUSIVE-LOCK.

        IF xeb.form-no EQ 0 THEN 
        DO:
      
            itemfg.pur-man = NOT CAN-FIND(FIRST x-eb
                WHERE x-eb.company EQ xeb.company 
                AND x-eb.est-no  EQ xeb.est-no
                AND x-eb.form-no NE 0
                AND x-eb.pur-man EQ NO).

            FOR EACH x-eb
                WHERE x-eb.company EQ xeb.company 
                AND x-eb.est-no  EQ xeb.est-no
                AND x-eb.form-no NE 0
                NO-LOCK BREAK BY x-eb.form-no:
                ll-one-part = FIRST(x-eb.form-no) AND LAST(x-eb.form-no).
                LEAVE.
            END.
        /* Wade Kaldawi   3/9/16
           Ticket 13466, ll-on-part should not change itemfg.alloc */       
        /*  IF ll-one-part THEN itemfg.alloc = YES. */
        END.
    END.
    ELSE IF fgmaster-cha EQ "FGITEM" THEN 
        DO:
            FIND FIRST cust WHERE cust.company = cocode AND
                cust.active  = "X"    NO-LOCK NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN 
                DO:
                    ASSIGN 
                        itemfg.def-loc     = shipto.loc        
                        itemfg.def-loc-bin = shipto.loc-bin.
                END.
            END.
        END.

    IF itemfg.def-loc EQ "" THEN itemfg.def-loc = ipcLoc.
    IF itemfg.def-loc-bin EQ "" THEN itemfg.def-loc-bin = ipcLocBin.

        {est/fgupdtax.i oe-ord}
    ll-new-fg-created = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE default-type d-oeitem 
PROCEDURE default-type :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER io-itemfg   FOR itemfg.

    DEFINE           BUFFER def-oe-ordl FOR oe-ordl.
  
    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordl.type-code:SCREEN-VALUE NE "T" AND lv-add-mode THEN 
        DO:
            oe-ordl.type-code:SCREEN-VALUE = "O".
            IF AVAILABLE io-itemfg THEN 
            DO:
                IF oe-ordl.type-code:SCREEN-VALUE EQ "O" 
                    AND oe-ordl.i-no:SCREEN-VALUE NE ""
                    AND io-itemfg.i-no NE "0"  
                    AND  CAN-FIND(FIRST def-oe-ordl WHERE 
                    def-oe-ordl.company EQ io-itemfg.company AND 
                    def-oe-ordl.i-no    EQ io-itemfg.i-no AND 
                    def-oe-ordl.ord-no  LT oe-ordl.ord-no AND 
                    ROWID(def-oe-ordl)  NE ROWID(oe-ordl)) THEN
                    oe-ordl.type-code:SCREEN-VALUE = "R".
                ELSE IF TRIM(itemfg.type-code) NE ""  
                        AND itemfg.type-code       NE "T" THEN
                        oe-ordl.type-code:SCREEN-VALUE = io-itemfg.type-code.
            END.
        END.
        RUN new-type.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI d-oeitem  _DEFAULT-DISABLE
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
  HIDE FRAME d-oeitem.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-est-detail d-oeitem 
PROCEDURE display-est-detail :
/*------------------------------------------------------------------------------
      Purpose:     from oe/ordlest.p
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
    DEFINE VARIABLE lv-qty           LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE lv-price         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-pr-uom        AS cha       NO-UNDO.
    DEFINE VARIABLE lv-rel           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-i-no          LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE v-job-no         LIKE oe-ordl.job-no NO-UNDO.
    DEFINE VARIABLE v-job-no2        LIKE oe-ordl.job-no2 NO-UNDO.
    DEFINE VARIABLE li-cnt           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-do-job        AS LOG       NO-UNDO.
    DEFINE VARIABLE li-cases         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-disp-prod-cat  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-marg%         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-com            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-tmp-price-2    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-price-per-1000 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dTotalPrice      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuoteEst        AS CHARACTER NO-UNDO .
  
    DEFINE BUFFER b-eb      FOR eb.
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
                            
    RUN pGetMiscEst(INPUT-OUTPUT ip-recid) .                          

    FIND FIRST eb WHERE RECID(eb) = ip-recid NO-LOCK NO-ERROR.       
                        
    IF AVAILABLE eb THEN 
    DO WITH FRAME {&Frame-name}:
        IF eb.stock-no NE "" THEN
            FIND FIRST itemfg
                WHERE itemfg.company EQ eb.company
                AND itemfg.i-no    EQ eb.stock-no
                NO-LOCK NO-ERROR.
        FIND est WHERE est.company = eb.company
            AND est.est-no = eb.est-no NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            ASSIGN
                oe-ordl.part-dscr2:SCREEN-VALUE = itemfg.part-dscr2
                oe-ordl.part-dscr3:SCREEN-VALUE = itemfg.part-dscr3
                /*35645 - Taxable set by FG item flag only*/
                oe-ordl.tax:SCREEN-VALUE        = STRING(fGetTaxable(itemfg.company, eb.cust-no, eb.ship-id, itemfg.i-no),"Y/N")
                .
            IF DECIMAL(oe-ordl.price:SCREEN-VALUE) = 0 THEN
            DO:
                ASSIGN
                    oe-ordl.price:SCREEN-VALUE  = STRING(itemfg.sell-price) 
                    oe-ordl.pr-uom:SCREEN-VALUE = itemfg.sell-uom
                    .  
            
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "Item fg sell price Item-No:" + string(itemfg.i-no)
                    ).
            END.
          
        END.

        RUN default-type (BUFFER itemfg).

        lv-qty = IF est.est-type EQ 3 OR
            est.est-type EQ 4 OR
            est.est-type EQ 8 THEN eb.bl-qty ELSE eb.eqty.

        IF est.est-type EQ 2 OR est.est-type EQ 6 THEN 
        DO:
            ll-do-job = CAN-FIND(FIRST b-eb WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.pur-man EQ NO
                AND b-eb.form-no NE 0).
            FIND FIRST b-eb
                WHERE b-eb.company EQ est.company
                AND b-eb.est-no  EQ est.est-no
                AND b-eb.form-no NE 0
                NO-LOCK NO-ERROR.
        END.

        ELSE 
        DO:
            FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
            ll-do-job = NOT b-eb.pur-man.
        END.
        IF NOT ll-do-job THEN ll-do-job = job#-int EQ 0.
     
        FIND FIRST oe-ctrl  NO-LOCK
            WHERE oe-ctrl.company EQ g_company 
            NO-ERROR.

        IF AVAILABLE oe-ord AND oe-ord.stat EQ "H" AND  AVAILABLE oe-ctrl AND NOT oe-ctrl.p-job THEN
        DO:
            ll-do-job = NO .
            IF NOT lCheckMessage THEN
                RUN displayMessage ( INPUT "48").
            lCheckMessage = YES.
        END.

        RUN est/getcscnt.p ((IF eb.est-type EQ 6 AND
            eb.cas-no NE ""  THEN ROWID(eb) ELSE ROWID(b-eb)),
            OUTPUT li-cnt,OUTPUT li-cases).
     
        ASSIGN
            ls-stock                        = eb.stock-no
            oe-ordl.est-no:SCREEN-VALUE     = eb.est-no
            oe-ordl.i-no:SCREEN-VALUE       = eb.stock-no
            oe-ordl.part-no:SCREEN-VALUE    = eb.part-no
            oe-ordl.i-name:SCREEN-VALUE     = eb.part-dscr1 
            oe-ordl.part-dscr1:SCREEN-VALUE = eb.part-dscr2
            oe-ordl.cas-cnt:SCREEN-VALUE    = STRING(li-cnt)
            oe-ordl.cases-unit:SCREEN-VALUE = STRING(li-cases)
            oe-ordl.s-pct[1]:SCREEN-VALUE   = STRING(100).

        IF oe-ordl.qty:SCREEN-VALUE = "0" THEN   /* task 06221510 */
            oe-ordl.qty:SCREEN-VALUE        = STRING(lv-qty) .

        IF oe-ordl.pr-uom:SCREEN-VALUE EQ "" THEN
            oe-ordl.pr-uom:SCREEN-VALUE = "M".

        IF AVAILABLE b-eb THEN 
        DO:
        
            ASSIGN
                oe-ordl.s-man[1]:SCREEN-VALUE  = b-eb.sman
                oe-ordl.s-comm[1]:SCREEN-VALUE = STRING(b-eb.comm)
                v-margin                       = 0.

            IF v-foamdate-log AND
                CAN-FIND(FIRST style WHERE style.company EQ b-eb.company
                AND style.style   EQ b-eb.style
                AND style.type    EQ "F") THEN 
            DO:
                oe-ordl.req-date:SCREEN-VALUE = STRING(oe-ord.ord-date + v-foamdate-int).
        
                IF DATE(oe-ordl.req-date:SCREEN-VALUE)  GT
                    DATE(oe-ordl.prom-date:SCREEN-VALUE) THEN
                    oe-ordl.prom-date:SCREEN-VALUE = oe-ordl.req-date:SCREEN-VALUE.
            END.
            RUN pGetOverUnderPct(b-eb.cust-no,b-eb.ship-id,ip-ord-no) .
        END. /*avail b-eb*/

        IF lastship-cha = "Stock/Custom" THEN 
        DO:
            /* If order has no estimate. */
            IF oe-ordl.est-no:SCREEN-VALUE = "" THEN
                ASSIGN oe-ordl.req-date:SCREEN-VALUE = STRING(TODAY + lastship-int).
            ELSE
                ASSIGN oe-ordl.req-date:SCREEN-VALUE = STRING(TODAY + INT(lastship-dec)).

            ASSIGN 
                oe-ordl.prom-date:SCREEN-VALUE = oe-ordl.req-date:SCREEN-VALUE.
        END.


        IF oe-ordl.i-no:SCREEN-VALUE EQ "" THEN 
        DO:
            IF v-est-fg THEN lv-i-no = eb.part-no.

            ELSE
                IF v-est-fg1 NE "Manual" THEN 
                DO:
                    FIND FIRST itemfg
                        WHERE itemfg.company EQ eb.company
                        AND itemfg.part-no EQ eb.part-no
                        AND itemfg.cust-no EQ eb.cust-no
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE itemfg THEN
                        ASSIGN
                            lv-i-no                         = itemfg.i-no
                            oe-ordl.part-dscr2:SCREEN-VALUE = itemfg.part-dscr2 
                            oe-ordl.part-dscr3:SCREEN-VALUE = itemfg.part-dscr3.
                END.
       
            RUN fg/GetFGItemID.p (ROWID(eb), "", OUTPUT lv-i-no).          
       
            IF lv-i-no NE "" THEN oe-ordl.i-no:SCREEN-VALUE = lv-i-no.
        END. /* oe-ordl.i-no:SCREEN-VALUE EQ "" */
    
        FIND FIRST est-qty NO-LOCK
            WHERE est-qty.company EQ est.company
            AND est-qty.est-no  EQ est.est-no
            NO-ERROR.
        IF NOT lQuotePriceMatrix AND v-quo-price-log AND AVAILABLE est-qty AND est-qty.qty[1] NE 0 AND
            (est-qty.qty[2] NE 0 OR est-qty.qty[3] NE 0 OR est-qty.qty[4] NE 0) AND
            NOT CAN-FIND(FIRST tt-item-qty-price WHERE
            tt-item-qty-price.tt-selected = YES AND
            (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
            (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE "")))
            AND oe-ordl.sourceEstimateID:SCREEN-VALUE EQ "" THEN
        DO:
            FIND FIRST quotehd NO-LOCK 
                WHERE quotehd.company EQ est.company AND
                quotehd.est-no EQ est.est-no AND 
                quotehd.quo-date LE TODAY AND
                (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)  NO-ERROR .
           
            IF AVAILABLE quotehd THEN 
            DO:
                RUN oe/d-ordqty.w (RECID(est-qty), OUTPUT lv-qty, OUTPUT lv-price, OUTPUT lv-pr-uom,
                    OUTPUT lv-rel, OUTPUT op-error, OUTPUT TABLE tt-item-qty-price).
            END.
            ELSE 
            DO:
                op-error = YES .
            END.

            IF op-error EQ NO THEN
            DO:
                ASSIGN
                    oe-ordl.price:SCREEN-VALUE  = STRING(lv-price)
                    oe-ordl.qty:SCREEN-VALUE    = STRING(lv-qty)
                    oe-ordl.pr-uom:SCREEN-VALUE = lv-pr-uom
                    ll-got-qtprice              = YES
                    v-rel                       = lv-rel.
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "EST - Detail Quote EST No: " + STRING(quotehd.est-no) + " Quantity:" + string(lv-qty) + "Expiration Date: " + string(quotehd.expireDate)
                    ). 
            END.
                 
        END.
        ELSE IF NOT lQuotePriceMatrix AND CAN-FIND(FIRST tt-item-qty-price WHERE
                tt-item-qty-price.tt-selected = YES AND
                (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE ""))) THEN
            DO: 
                FIND FIRST tt-item-qty-price WHERE
                    tt-item-qty-price.tt-selected = YES AND 
                    (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                    (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE "")).
             
                ASSIGN
                    lv-qty                      = tt-item-qty-price.qty
                    lv-price                    = tt-item-qty-price.price
                    lv-pr-uom                   = tt-item-qty-price.uom
                    lv-rel                      = tt-item-qty-price.rel
                    v-rel                       = lv-rel
                    op-error                    = NO
                    oe-ordl.price:SCREEN-VALUE  = STRING(lv-price)
                    oe-ordl.qty:SCREEN-VALUE    = STRING(lv-qty)
                    oe-ordl.pr-uom:SCREEN-VALUE = lv-pr-uom
                    ll-got-qtprice              = YES.
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "Item Qty Price"  + " Quantity:" + string(lv-qty)
                    ).
            END.

            ELSE
                v-rel = est-qty.qty[21].

        IF v-rel = 0 THEN
            v-rel = 1.
     
        IF lv-new-tandem EQ ? AND ll-do-job THEN 
        DO:
        
            ASSIGN
                v-disp-prod-cat = eb.procat
                v-job-no        = FILL(" ",6 - length(TRIM(STRING(oe-ordl.ord-no)))) + string(oe-ordl.ord-no).

            RUN jc/job-no.p (INPUT-OUTPUT v-job-no, 
                INPUT-OUTPUT v-job-no2,
                INPUT v-disp-prod-cat,
                INPUT FILL(" ",6 - LENGTH(TRIM(oe-ordl.est-no:SCREEN-VALUE))) + TRIM(oe-ordl.est-no:SCREEN-VALUE)).
            IF v-job-no NE "" THEN 
            DO:
                ASSIGN
                    oe-ordl.job-no:SCREEN-VALUE  = v-job-no
                    oe-ordl.job-no2:SCREEN-VALUE = STRING(v-job-no2) .
            /*DISPLAY oe-ordl.job-no
                    oe-ordl.job-no2.*/
            END.
            FOR EACH job-hdr  WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ oe-ordl.job-no:SCREEN-VALUE
                USE-INDEX job-no NO-LOCK
                BY job-hdr.job-no DESCENDING BY job-hdr.job-no2 DESCENDING:
                oe-ordl.job-no2:SCREEN-VALUE = STRING(IF AVAILABLE job-hdr THEN job-hdr.job-no2 + 1 ELSE 0).
                LEAVE.
            END.
        END. /* lv-new-tandem EQ ? AND ll-do-job */
      
        FIND xest WHERE xest.company = eb.company AND
            xest.est-no = eb.est-no NO-LOCK NO-ERROR.
    END.

    /* get price from quote */  
  
    ASSIGN 
        lv-price  = dec(oe-ordl.price:SCREEN-VALUE)
        lv-pr-uom = oe-ordl.pr-uom:SCREEN-VALUE
        lv-qty    = dec(oe-ordl.qty:SCREEN-VALUE).
    cQuoteEst = IF oe-ordl.SourceEstimateID:SCREEN-VALUE NE "" THEN oe-ordl.SourceEstimateID:SCREEN-VALUE ELSE oe-ordl.est-no:SCREEN-VALUE .
    IF NOT lQuotePriceMatrix AND AVAILABLE xest AND v-quo-price-log AND NOT ll-got-qtprice AND
        NOT CAN-FIND(FIRST tt-item-qty-price WHERE
        tt-item-qty-price.tt-selected = YES AND
        (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
        (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE ""))) THEN 
    DO:
        ll-got-qtprice = YES.
        
        RUN pGetQuoteRec(cQuoteEst /*xest.est-no*/,oe-ordl.part-no:SCREEN-VALUE,
            oe-ordl.i-no:SCREEN-VALUE,
            INPUT-OUTPUT lv-price ,
            INPUT-OUTPUT lv-pr-uom,
            OUTPUT lv-q-no,
            INPUT-OUTPUT lv-qty).

        oe-ordl.qty:SCREEN-VALUE  = STRING(lv-qty).
        RUN pAddTagInfoForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "Quoted Price Quote Est:" + STRING(cQuoteEst) + " Quote No:" + STRING(lv-q-no) + " Quantity:" + string(lv-qty)
            ).
    END.
    IF lv-qty GT 0 AND AVAILABLE est-qty THEN 
    DO:
        DO iCount = 1 TO EXTENT(est-qty.qty):
            IF est-qty.qty[iCount] EQ lv-qty OR est-qty.qty[iCount] EQ 0 THEN LEAVE.
        END.
        IF iCount GT 0 THEN oe-ordl.whsed:SCREEN-VALUE = STRING(est-qty.whsed[iCount], "YES/NO"). 
    END.
     
    IF NOT ll-got-qtprice AND CAN-FIND(FIRST tt-item-qty-price WHERE
        tt-item-qty-price.tt-selected = YES AND
        (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
        (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE ""))) THEN
    DO:
        FIND FIRST tt-item-qty-price WHERE
            tt-item-qty-price.tt-selected = YES AND
            (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
            (tt-item-qty-price.part-no EQ oe-ordl.i-no:SCREEN-VALUE AND oe-ordl.i-no:SCREEN-VALUE NE ""))
            NO-ERROR.

        IF AVAILABLE tt-item-qty-price THEN
            ASSIGN
                ll-got-qtprice = YES
                lv-price       = tt-item-qty-price.price
                lv-pr-uom      = tt-item-qty-price.uom
                lv-q-no        = tt-item-qty-price.q-no.
        RUN pAddTagInfoForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "Item Qty Price Quote No:" + STRING(lv-q-no)  + " Quantity:" + string(lv-qty)
            ).
    END.

    ASSIGN
        oe-ordl.price:SCREEN-VALUE  = STRING(lv-price)
        oe-ordl.pr-uom:SCREEN-VALUE = STRING(lv-pr-uom).

    RUN Conv_CalcTotalPrice(cocode, 
        oe-ordl.i-no:SCREEN-VALUE,
        DECIMAL(oe-ordl.qty:SCREEN-VALUE),
        DECIMAL(oe-ordl.price:SCREEN-VALUE),
        oe-ordl.pr-uom:SCREEN-VALUE,
        DECIMAL(oe-ordl.disc:SCREEN-VALUE),
        DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
        OUTPUT dTotalPrice).
    oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
  //{oe/ordltot.i oe-ordl qty oe-ordl}

    oe-ordl.rec_key = est.rec_key.

    IF oeestcom-log = YES AND AVAILABLE b-eb THEN
    DO:
        FIND FIRST sman WHERE
            sman.company EQ b-eb.company AND
            sman.sman EQ b-eb.sman
            NO-LOCK NO-ERROR.

        IF AVAILABLE sman AND sman.commbasis EQ "M" THEN
        DO:
            IF oe-ordl.pr-uom:SCREEN-VALUE NE "M" THEN
            DO:
                FIND FIRST itemfg WHERE
                    itemfg.company EQ cocode AND
                    itemfg.i-no EQ oe-ordl.i-no:SCREEN-VALUE
                    NO-LOCK NO-ERROR.

                ASSIGN
                    v-tmp-price-2    = IF oe-ordl.pr-uom:SCREEN-VALUE BEGINS "L" AND
                             oe-ordl.pr-uom:SCREEN-VALUE NE "LB" THEN
                             IF DEC(oe-ordl.qty:SCREEN-VALUE) LT 0 THEN -1
                             ELSE 1
                          ELSE
                          IF oe-ordl.pr-uom:SCREEN-VALUE EQ "CS" OR oe-ordl.pr-uom EQ "PLT" THEN
                             DEC(oe-ordl.qty:SCREEN-VALUE) / (IF DEC(oe-ordl.cas-cnt:SCREEN-VALUE) NE 0 THEN DEC(oe-ordl.cas-cnt:SCREEN-VALUE) ELSE
                                IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN
                                         itemfg.case-count ELSE 1)
                           ELSE
                           IF oe-ordl.pr-uom:SCREEN-VALUE EQ "C" THEN
                              DEC(oe-ordl.qty:SCREEN-VALUE) / 100
                           ELSE
                              DEC(oe-ordl.qty:SCREEN-VALUE)
                
                    v-tmp-price-2    = v-tmp-price-2 * DEC(oe-ordl.price:SCREEN-VALUE)
                    v-price-per-1000 = v-tmp-price-2 / ( DEC(oe-ordl.qty:SCREEN-VALUE) / 1000).
            END.
            ELSE
                v-price-per-1000 = DEC(oe-ordl.price:SCREEN-VALUE).

            IF NOT(b-eb.est-type EQ 4 OR b-eb.est-type EQ 8) THEN
            DO:
                FOR EACH probe WHERE
                    probe.company = b-eb.company AND
                    probe.est-no = b-eb.est-no AND
                    probe.probe-date NE ? AND
                    probe.est-qty EQ INT(oe-ordl.qty:screen-value)
                    NO-LOCK
                    BY probe.probe-date DESCENDING
                    BY probe.probe-time DESCENDING:
               
                    IF probe.sell-price EQ v-price-per-1000 OR
                        ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                        LEAVE.
                END.
          
                IF NOT AVAILABLE probe THEN
                    FOR EACH probe WHERE
                        probe.company = b-eb.company AND
                        probe.est-no = b-eb.est-no AND
                        probe.probe-date NE ? AND
                        probe.est-qty EQ INT(oe-ordl.qty:screen-value)
                        NO-LOCK
                        BY probe.probe-date DESCENDING
                        BY probe.probe-time DESCENDING:
                  
                        LEAVE.
                    END.
            END.
            ELSE
            DO:
                FOR EACH probe WHERE
                    probe.company = b-eb.company AND
                    probe.est-no = b-eb.est-no AND
                    probe.probe-date NE ?
                    NO-LOCK
                    BY probe.probe-date DESCENDING
                    BY probe.probe-time DESCENDING:
               
                    IF probe.sell-price EQ v-price-per-1000 OR
                        ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                        LEAVE.
                END.
          
                IF NOT AVAILABLE probe THEN
                    FOR EACH probe WHERE
                        probe.company = b-eb.company AND
                        probe.est-no = b-eb.est-no AND
                        probe.probe-date NE ?
                        NO-LOCK
                        BY probe.probe-date DESCENDING
                        BY probe.probe-time DESCENDING:
                  
                        LEAVE.
                    END.
            END.

            IF AVAILABLE probe THEN
            DO:
                ASSIGN
                    oe-ordl.s-comm[1]:SCREEN-VALUE = STRING(probe.comm)
                    v-margin                       = probe.market-price.
       
                RELEASE probe.
            END.
        END.
    END.

    IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 THEN
        IF INT(oe-ordl.qty:screen-value) LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
            oe-ordl.cas-cnt:SCREEN-VALUE = oe-ordl.qty:SCREEN-VALUE.
        ELSE
            IF INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN
                oe-ordl.cas-cnt:SCREEN-VALUE = "1".

    IF INT(oe-ordl.cases-unit:SCREEN-VALUE) EQ 0 THEN
        oe-ordl.cases-unit:SCREEN-VALUE = "1".
    IF AVAILABLE itemfg THEN
        IF itemfg.CLASS EQ "*" OR itemfg.exempt-disc THEN oe-ordl.disc:SCREEN-VALUE = "0".
    RUN pGetPartComm (YES) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgitem d-oeitem 
PROCEDURE display-fgitem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li-cnt      LIKE oe-ordl.cas-cnt NO-UNDO.
    DEFINE VARIABLE li-unit     LIKE oe-ordl.cases-unit NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE x           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-alloc    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-recid    AS RECID     NO-UNDO.
    DEFINE VARIABLE lv-price    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-pr-uom   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-tmp-part  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-cost-uom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-new-i-no LIKE oe-ordl.i-no NO-UNDO.
    DEFINE VARIABLE lv-calc-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-case-qty AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-uom      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    DO WITH FRAME {&FRAME-NAME}:
        IF NOT AVAILABLE xoe-ord THEN FIND xoe-ord WHERE RECID(xoe-ord) = RECID(oe-ord) NO-LOCK NO-ERROR.

        RELEASE itemfg.

        DO WHILE NOT AVAILABLE itemfg:
            FIND FIRST itemfg
                WHERE itemfg.company EQ g_company
                AND itemfg.i-no    EQ oe-ordl.i-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE itemfg THEN 
            DO:

                IF ll-new-file THEN
                    RUN custom/getobitm.p (cocode, oe-ordl.i-no:SCREEN-VALUE,
                               INDEX(ip-type,"Update-") EQ 0,
                        OUTPUT lv-new-i-no).
                IF lv-new-i-no EQ "" OR lv-new-i-no EQ FILL("?",30) THEN RETURN ERROR.
                ELSE oe-ordl.i-no:SCREEN-VALUE = lv-new-i-no.
            END.
        END.

        IF /*ip-type <> "add" and */ 
            oe-ordl.est-no:screen-value <> "" THEN 
        DO:
     
            IF oe-ord.est-no EQ "" THEN 
            DO:
                IF NOT AVAILABLE xest THEN
                    FIND FIRST xest WHERE xest.company EQ cocode AND
                        xest.est-no EQ INPUT oe-ordl.est-no NO-LOCK NO-ERROR.
                IF AVAILABLE xest AND v-quo-price-log AND
                    (ld-prev-price = 0 OR ld-prev-price = dec(oe-ordl.price:screen-value) )
                    /* to allow user's overriding price */  
                    THEN 
                DO:
                    IF ll-got-qtprice THEN 
                    DO:
                    /*ll-got-qtprice = no.*/
                    END.
                    ELSE 
                    DO:
                        ASSIGN 
                            lv-price       = dec(oe-ordl.price:screen-value)
                            lv-pr-uom      = oe-ordl.pr-uom:screen-value
                            v-tmp-part     = oe-ordl.i-no:screen-value
                            lv-qty         = dec(oe-ordl.qty:SCREEN-VALUE)
                            ll-got-qtprice = YES.

                        IF NOT CAN-FIND(FIRST tt-item-qty-price WHERE
                            tt-item-qty-price.tt-selected = YES AND
                            (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                            (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))) 
                            AND oe-ordl.sourceEstimateID:SCREEN-VALUE EQ "" THEN
                        DO:
                            RUN pGetQuoteRec(xest.est-no,oe-ordl.part-no:SCREEN-VALUE,
                                v-tmp-part,
                                INPUT-OUTPUT lv-price ,
                                INPUT-OUTPUT lv-pr-uom,
                                OUTPUT lv-q-no,
                                INPUT-OUTPUT lv-qty).
                    
                            oe-ordl.qty:SCREEN-VALUE = STRING(lv-qty).
                            RUN pAddTagInfoForGroup(
                                INPUT oe-ordl.rec_key,
                                INPUT "Quoted Price Quote No:" + string(lv-q-no) + " Quantity:" + string(lv-qty)
                                ).
                        END.
                        ELSE
                        DO:
                            FIND FIRST tt-item-qty-price WHERE
                                tt-item-qty-price.tt-selected = YES AND
                                (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                                (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))
                                NO-ERROR.
                 
                            IF AVAILABLE tt-item-qty-price THEN
                            DO:
                                ASSIGN
                                    lv-price  = tt-item-qty-price.price
                                    lv-pr-uom = tt-item-qty-price.uom
                                    lv-q-no   = tt-item-qty-price.q-no.
                                RUN pAddTagInfoForGroup(
                                    INPUT oe-ordl.rec_key,
                                    INPUT "Item Qty Price Quote No:" + string(lv-q-no) + " Quantity:" + string(lv-qty)
                                    ).
                            END.
                        END.

                        ASSIGN 
                            oe-ordl.price:screen-value  = STRING(lv-price)
                            oe-ordl.pr-uom:screen-value = lv-pr-uom.
                        IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                            oeestcom-log = YES THEN
                            RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
                        RUN Conv_CalcTotalPrice(cocode, 
                            oe-ordl.i-no:SCREEN-VALUE,
                            DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                            DECIMAL(oe-ordl.price:SCREEN-VALUE),
                            oe-ordl.pr-uom:SCREEN-VALUE,
                            DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                            DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                            OUTPUT dTotalPrice).
                        oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
              //{oe/ordltot.i oe-ordl qty oe-ordl}
                    END.           
                END. 
            END.     /* oe-ordl.est-no <> "" */

            RUN validate-fgitem NO-ERROR.
            IF ERROR-STATUS:ERROR THEN RETURN ERROR.

            /*find first itemfg where itemfg.company = g_company and
                                 itemfg.i-no = oe-ordl.i-no:screen-value
                                 no-lock no-error.
            if not avail itemfg then return error.*/

            RETURN.
        END.  /* update and est-no <> "" */

        RUN validate-fgitem NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        /*35645 - Taxable set by FG item flag only*/
        oe-ordl.tax:SCREEN-VALUE = STRING(fGetTaxable(itemfg.company, oe-ord.cust-no, oe-ord.ship-id, itemfg.i-no),"Y/N").

        RUN default-type (BUFFER itemfg).

        IF NOT AVAILABLE oe-ord THEN FIND oe-ord WHERE oe-ord.company = g_company AND
                oe-ord.ord-no = oe-ordl.ord-no NO-LOCK. 
        IF itemfg.CLASS EQ "*" OR itemfg.exempt-disc THEN oe-ordl.disc:SCREEN-VALUE = "0".


        IF oe-ordl.type-code:SCREEN-VALUE EQ "O" AND oe-ordl.est-no NE "" THEN
            ASSIGN oe-ordl.i-name:screen-value     = IF itemfg.i-name <> "" THEN itemfg.i-name ELSE oe-ordl.i-name:screen-value 
                oe-ordl.i-no:screen-value       = IF itemfg.i-no <> "" THEN itemfg.i-no ELSE oe-ordl.i-no:screen-value 
                oe-ordl.part-dscr2:screen-value = IF itemfg.part-dscr2 <> "" THEN itemfg.part-dscr2 ELSE oe-ordl.part-dscr2:screen-value
                oe-ordl.part-dscr3:screen-value = IF itemfg.part-dscr3 <> "" THEN itemfg.part-dscr3 ELSE oe-ordl.part-dscr3:screen-value
                .
        ELSE
        DO:
            ASSIGN 
                oe-ordl.i-name:screen-value     = IF itemfg.i-name <> "" THEN itemfg.i-name ELSE oe-ordl.i-name:screen-value
                oe-ordl.i-no:screen-value       = IF itemfg.i-no <> "" AND oe-ordl.i-no:SCREEN-VALUE = "" THEN itemfg.i-no ELSE oe-ordl.i-no:screen-value
                oe-ordl.price:screen-value      = IF setFromHistory THEN STRING(historyPrice) ELSE
                                          IF itemfg.sell-price <> 0 THEN STRING(itemfg.sell-price) ELSE oe-ordl.price:screen-value
                oe-ordl.pr-uom:screen-value     = IF setFromHistory THEN STRING(historyPrUOM) ELSE
                                          IF itemfg.sell-uom <> "" AND oe-ordl.pr-uom:SCREEN-VALUE = "" THEN itemfg.sell-uom ELSE  oe-ordl.pr-uom:screen-value 
                oe-ordl.cas-cnt:screen-value    = IF itemfg.case-count <> 0 THEN STRING(itemfg.case-count) ELSE oe-ordl.cas-cnt:screen-value
                oe-ordl.cases-unit:screen-value = IF itemfg.case-pall <> 0 THEN STRING(itemfg.case-pall) ELSE oe-ordl.cases-unit:screen-value  
                oe-ordl.part-dscr2:screen-value = IF itemfg.part-dscr2 <> "" THEN itemfg.part-dscr2 ELSE oe-ordl.part-dscr2:screen-value
                oe-ordl.part-dscr3:screen-value = IF itemfg.part-dscr3 <> "" THEN itemfg.part-dscr3 ELSE oe-ordl.part-dscr3:screen-value.

            IF oe-ordl.part-no:SCREEN-VALUE EQ "" THEN
                ASSIGN oe-ordl.part-no:SCREEN-VALUE = oe-ordl.i-no:SCREEN-VALUE .

            IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                oeestcom-log = YES THEN
                RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
      
            IF setFromHistory THEN 
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "History Price"
                    ).
            ELSE IF itemfg.sell-price <> 0 THEN 
                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Item Sell Price"
                        ).
        END.

        IF oe-ordl.est-no:screen-value EQ "" THEN 
        DO:
            ASSIGN 
                oe-ordl.part-no:screen-value    = IF itemfg.part-no <> "" THEN itemfg.part-no ELSE oe-ordl.part-no:screen-value
                oe-ordl.part-dscr1:screen-value = IF itemfg.part-dscr1 <> "" THEN itemfg.part-dscr1 ELSE oe-ordl.part-dscr1:screen-value
                oe-ordl.cas-cnt:screen-value    = IF itemfg.case-count <> 0 THEN STRING(itemfg.case-count) ELSE oe-ordl.cas-cnt:SCREEN-VALUE
                oe-ordl.cases-unit:screen-value = IF itemfg.case-pall <> 0 THEN STRING(itemfg.case-pall) ELSE oe-ordl.cases-unit:SCREEN-VALUE
                oe-ordl.part-dscr2:screen-value = IF itemfg.part-dscr2 <> "" THEN itemfg.part-dscr2 ELSE oe-ordl.part-dscr2:screen-value
                oe-ordl.part-dscr3:screen-value = IF itemfg.part-dscr3 <> "" THEN itemfg.part-dscr3 ELSE oe-ordl.part-dscr3:SCREEN-VALUE  .

            IF oe-ordl.part-no:SCREEN-VALUE EQ "" THEN
                ASSIGN oe-ordl.part-no:SCREEN-VALUE = oe-ordl.i-no:SCREEN-VALUE .

            IF ll-new-file THEN 
            DO:
                ASSIGN
                    cp-part-no = ""
                    cp-rowid   = ROWID(itemfg).
                RUN custom/getcpart.p (cocode, oe-ord.cust-no,
                    INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
                IF cp-part-no NE "" THEN oe-ordl.part-no:SCREEN-VALUE = cp-part-no.
            END.
        END.
        ELSE 
        DO:
            RUN oe/oe-cnt.p(RECID(oe-ordl), OUTPUT li-cnt, OUTPUT li-unit).
            ASSIGN
                oe-ordl.cas-cnt:screen-value    = STRING(li-cnt)
                oe-ordl.cases-unit:screen-value = STRING(li-unit).
        END.

        IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 THEN 
        DO:
            ASSIGN
                lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE)
                lv-uom      = fi_qty-uom:SCREEN-VALUE
                lv-case-qty = (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                     IF lv-uom EQ "C"  THEN 100 ELSE 
                     IF lv-uom EQ "EA" THEN 1 ELSE 1000)
                lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE).
            IF oe-ordl.pr-uom:SCREEN-VALUE NE "EA" THEN
                ASSIGN
                    lv-calc-qty = lv-calc-qty * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                     IF lv-uom EQ "C"  THEN 100 ELSE 
                     IF lv-uom EQ "EA" THEN 1 ELSE 1000).

            IF lv-calc-qty LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
                oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-calc-qty).

            IF INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN 
            DO:
                IF lv-calc-qty LE lv-case-qty THEN
                    oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-calc-qty).
                ELSE
                    oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-case-qty).
            END.
           
        END.

        IF INT(oe-ordl.cases-unit:SCREEN-VALUE) EQ 0 THEN
            oe-ordl.cases-unit:SCREEN-VALUE = "1".

        /*ASSIGN
         oe-ordl.cases:SCREEN-VALUE   = STRING(TRUNC(INT(oe-ordl.qty:SCREEN-VALUE) / INT(oe-ordl.cas-cnt:SCREEN-VALUE),0))
         oe-ordl.partial:SCREEN-VALUE = STRING(INT(oe-ordl.qty:SCREEN-VALUE) MOD INT(oe-ordl.cas-cnt:SCREEN-VALUE)). */

        IF v-foamdate-log                                             AND
            itemfg.style NE ""                                         AND
            CAN-FIND(FIRST style WHERE style.company EQ itemfg.company
            AND style.style   EQ itemfg.style
            AND style.type    EQ "F")           THEN 
        DO:
            oe-ordl.req-date:SCREEN-VALUE = STRING(oe-ord.ord-date + v-foamdate-int).

            IF DATE(oe-ordl.req-date:SCREEN-VALUE)  GT
                DATE(oe-ordl.prom-date:SCREEN-VALUE) THEN
                oe-ordl.prom-date:SCREEN-VALUE = oe-ordl.req-date:SCREEN-VALUE.
        END.

        RUN itemfg-cost.
        /* ======= end of oe/ordlfg.i ========*/

        IF itemfg.isaset AND itemfg.t-sqft EQ 0 AND
            CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
            AND fg-set.set-no  EQ itemfg.i-no
            AND fg-set.part-no NE fg-set.set-no) THEN
            RUN fg/updsetdm.p (RECID(itemfg)).

        IF oe-ordl.est-no:screen-value EQ "" THEN 
        DO:
            IF v-upd-comm THEN 
            DO:               
                {oe/oescomm.i oe-ordl.s-man[1]:screen-value 1}
                {oe/oescomm.i oe-ordl.s-man[2]:screen-value 2}
                {oe/oescomm.i oe-ordl.s-man[3]:screen-value 3}   

                /* Populate 2nd sales rep per NK1 value */
                FIND FIRST sys-ctrl NO-LOCK
                    WHERE sys-ctrl.company EQ cocode
                    AND sys-ctrl.name    EQ "SALESREP" NO-ERROR.
                IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
                DO:
                    FIND FIRST sys-ctrl-shipto NO-LOCK
                        WHERE sys-ctrl-shipto.company EQ cocode
                        AND sys-ctrl-shipto.name    EQ "SALESREP" 
                        AND sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no
                        NO-ERROR.
                    IF AVAILABLE sys-ctrl-shipto AND oe-ordl.s-man[1]:SCREEN-VALUE NE sys-ctrl-shipto.char-fld THEN 
                    DO:
                        ASSIGN 
                            oe-ordl.s-man[2]:SCREEN-VALUE  = sys-ctrl-shipto.char-fld
                            oe-ordl.s-comm[2]:SCREEN-VALUE = STRING(sys-ctrl-shipto.dec-fld)
                            oe-ordl.s-pct[2]:screen-value  = "100.00".
                    /*
                    find sman where sman.company = oe-ordl.company
                       and sman.sman = oe-ordl.s-man[2]:SCREEN-VALUE
                       no-lock no-error.
                    if avail sman then assign oe-ordl.s-name[2]:screen-value = sman.sname.
                    */
                    END.

                END.
            END.
        END.

        IF lv-add-mode THEN 
        DO:
            ASSIGN oe-ordl.pr-uom oe-ordl.price.
            RUN get-price.
        END.
        RUN pGetPartComm(NO) .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-fgpart d-oeitem 
PROCEDURE display-fgpart :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE li-cnt      LIKE oe-ordl.cas-cnt NO-UNDO.
    DEFINE VARIABLE li-unit     LIKE oe-ordl.cases-unit NO-UNDO.
    DEFINE VARIABLE x           AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-recid    AS RECID   NO-UNDO.
    DEFINE VARIABLE v-cost      AS DECIMAL DECIMALS 10 NO-UNDO.
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    IF oe-ordl.est-no:screen-value IN FRAME {&frame-name} <> "" THEN RETURN.
  

    IF NOT AVAILABLE oe-ord THEN 
    DO:
        FIND oe-ord WHERE oe-ord.company = g_company AND
            oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.
    END.

    RUN validate-fgitem NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
 
    DO WITH FRAME {&frame-name}:  
        FIND itemfg WHERE RECID(itemfg) = ip-recid NO-LOCK.
        IF itemfg.CLASS EQ "*" OR itemfg.exempt-disc THEN oe-ordl.disc:SCREEN-VALUE = "0".


        RUN default-type (BUFFER itemfg).

        IF oe-ordl.type-code:SCREEN-VALUE EQ "O" AND oe-ordl.est-no NE "" THEN
            ASSIGN oe-ordl.i-name:screen-value     = IF oe-ordl.i-name:SCREEN-VALUE = "" THEN itemfg.i-name ELSE oe-ordl.i-name:SCREEN-VALUE
                oe-ordl.i-no:screen-value       = IF oe-ordl.i-no:SCREEN-VALUE = "" THEN itemfg.i-no ELSE oe-ordl.i-no:SCREEN-VALUE
                oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2
                oe-ordl.part-dscr3:screen-value = itemfg.part-dscr3 .
        ELSE
        DO:
            ASSIGN 
                oe-ordl.i-name:screen-value     = IF oe-ordl.i-name:SCREEN-VALUE = "" THEN itemfg.i-name ELSE oe-ordl.i-name:SCREEN-VALUE
                oe-ordl.i-no:screen-value       = IF oe-ordl.i-no:SCREEN-VALUE = "" THEN itemfg.i-no ELSE oe-ordl.i-no:SCREEN-VALUE
                oe-ordl.price:screen-value      = IF setFromHistory THEN STRING(historyPrice) ELSE STRING(itemfg.sell-price)
                oe-ordl.pr-uom:screen-value     = IF setFromHistory THEN STRING(historyPrUOM) ELSE itemfg.sell-uom
                oe-ordl.cas-cnt:screen-value    = STRING(itemfg.case-count)
                oe-ordl.cases-unit:screen-value = STRING(itemfg.case-pall)
                oe-ordl.part-dscr2:screen-value = itemfg.part-dscr2
                oe-ordl.part-dscr3:screen-value = itemfg.part-dscr3    .

            IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                oeestcom-log = YES THEN
                RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
            IF setFromHistory THEN 
                RUN pAddTagInfoForGroup(
                    INPUT oe-ordl.rec_key,
                    INPUT "History Price "
                    ). 
            ELSE IF itemfg.sell-price <> 0 THEN 
                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Item Sell Price"
                        ). 
        END.

        IF oe-ordl.est-no:screen-value EQ "" THEN 
        DO:
            ASSIGN 
                oe-ordl.part-no:screen-value    = itemfg.part-no
                oe-ordl.part-dscr1:screen-value = itemfg.part-dscr1
                oe-ordl.cas-cnt:screen-value    = STRING(itemfg.case-count).

            IF ll-new-file THEN 
            DO:
                ASSIGN
                    cp-part-no = ""
                    cp-rowid   = ROWID(itemfg).
                RUN custom/getcpart.p (cocode, oe-ord.cust-no,
                    INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
                IF cp-part-no NE "" THEN oe-ordl.part-no:SCREEN-VALUE = cp-part-no.
            END.
        END.

        ELSE 
        DO:
            RUN oe/oe-cnt.p(RECID(oe-ordl), OUTPUT li-cnt, OUTPUT li-unit).
            ASSIGN
                oe-ordl.cas-cnt:screen-value    = STRING(li-cnt)
                oe-ordl.cases-unit:screen-value = STRING(li-unit).
        END.
 
        IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 THEN
            IF INT(oe-ordl.qty:screen-value) LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
                oe-ordl.cas-cnt:SCREEN-VALUE = oe-ordl.qty:SCREEN-VALUE.
            ELSE
                IF INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN
                    oe-ordl.cas-cnt:SCREEN-VALUE = "1".

        IF INT(oe-ordl.cases-unit:SCREEN-VALUE) EQ 0 THEN
            oe-ordl.cases-unit:SCREEN-VALUE = "1".

        /*ASSIGN
         oe-ordl.cases:SCREEN-VALUE   = STRING(TRUNC(INT(oe-ordl.qty:SCREEN-VALUE) / INT(oe-ordl.cas-cnt:SCREEN-VALUE),0))
         oe-ordl.partial:SCREEN-VALUE = STRING(INT(oe-ordl.qty:SCREEN-VALUE) MOD INT(oe-ordl.cas-cnt:SCREEN-VALUE)).*/

        IF oe-ordl.job-no:screen-value = "" THEN 
        DO:
            FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                AND po-ordl.i-no      EQ oe-ordl.i-no:screen-value
                AND po-ordl.po-no     EQ int(oe-ordl.po-no-po:screen-value)
                AND po-ordl.item-type EQ NO
                USE-INDEX item-ordno NO-LOCK NO-ERROR.

            IF AVAILABLE po-ordl THEN
                ASSIGN oe-ordl.pr-uom:screen-value = po-ordl.cons-uom
                    oe-ordl.cost:screen-value   = STRING(po-ordl.cons-cost)
                    v-cost                      = po-ordl.cons-cost.
            ELSE ASSIGN oe-ordl.pr-uom:screen-value = itemfg.prod-uom
                    oe-ordl.cost:screen-value   = STRING(get-itemfg-cost(itemfg.i-no))
                    v-cost                      = get-itemfg-cost(itemfg.i-no).
      
            IF oe-ordl.pr-uom NE "M" THEN
                RUN sys/ref/convcuom.p(oe-ordl.pr-uom:screen-value, "M", 0, 0, 0, 0,
                    v-cost, OUTPUT lv-out-cost).
            ELSE
                lv-out-cost = v-cost.

            ASSIGN 
                oe-ordl.cost:screen-value   = STRING(lv-out-cost)
                oe-ordl.pr-uom:screen-value = itemfg.sell-uom.

            IF AVAILABLE po-ordl THEN
            DO:
                FIND FIRST po-ord WHERE
                    po-ord.company EQ po-ordl.company AND
                    po-ord.po-no EQ po-ordl.po-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE po-ord THEN
                DO:
                    IF NOT lNewVendorItemCost THEN 
                        FIND FIRST e-itemfg-vend WHERE
                            e-itemfg-vend.company EQ po-ordl.company AND
                            e-itemfg-vend.i-no EQ po-ordl.i-no AND
                            e-itemfg-vend.vend-no EQ po-ord.vend-no AND
                            e-itemfg-vend.est-no EQ ""
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE e-itemfg-vend THEN
                    DO:
                        oe-ordl.cost:SCREEN-VALUE = STRING(DEC(oe-ordl.cost:SCREEN-VALUE) * (1 + (e-itemfg-vend.markup / 100.0 ))).
                    END.
                    RELEASE po-ord.
                END.
            END.
        END.
  
    END.  /* frame {&frame-name} */

    IF itemfg.isaset AND itemfg.t-sqft EQ 0 AND
        CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
        AND fg-set.set-no  EQ itemfg.i-no
        AND fg-set.part-no NE fg-set.set-no) THEN
        RUN fg/updsetdm.p (RECID(itemfg)).

    IF oe-ordl.est-no:screen-value EQ "" THEN 
    DO:
        IF v-upd-comm THEN 
        DO:               
       {oe/oescomm.i oe-ordl.s-man[1]:screen-value 1}
       {oe/oescomm.i oe-ordl.s-man[2]:screen-value 2}
       {oe/oescomm.i oe-ordl.s-man[3]:screen-value 3}    
            /* Populate 2nd sales rep per NK1 value */
            FIND FIRST sys-ctrl NO-LOCK
                WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "SALESREP" NO-ERROR.
            IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN 
            DO:
                FIND FIRST sys-ctrl-shipto NO-LOCK
                    WHERE sys-ctrl-shipto.company EQ cocode
                    AND sys-ctrl-shipto.name    EQ "SALESREP" 
                    AND sys-ctrl-shipto.cust-vend-no = oe-ord.cust-no
                    NO-ERROR.
                IF AVAILABLE sys-ctrl-shipto AND oe-ordl.s-man[1]:SCREEN-VALUE NE sys-ctrl-shipto.char-fld THEN 
                DO:
                    ASSIGN 
                        oe-ordl.s-man[2]:SCREEN-VALUE  = sys-ctrl-shipto.char-fld
                        oe-ordl.s-comm[2]:SCREEN-VALUE = STRING(sys-ctrl-shipto.dec-fld)
                        oe-ordl.s-pct[2]:screen-value  = "100.00".
                /*
                find sman where sman.company = oe-ordl.company
                   and sman.sman = oe-ordl.s-man[2]:SCREEN-VALUE
                   no-lock no-error.
                if avail sman then assign oe-ordl.s-name[2]:screen-value = sman.sname.
                */
                END.

            END.
        END.
    END.

    RUN get-price.

    RUN pGetPartComm(NO) .
 
    IF oe-ordl.qty:screen-value = "0" OR oe-ordl.qty:screen-value = "" THEN
        APPLY "entry" TO oe-ordl.qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-item d-oeitem 
PROCEDURE display-item :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF AVAILABLE oe-ordl THEN 
    DO:
        DISPLAY oe-ordl.est-no
            oe-ordl.type-code
            oe-ordl.job-no
            oe-ordl.job-no2
            oe-ordl.qty
            fi_qty-uom
            oe-ordl.i-no 
            oe-ordl.price
            oe-ordl.pr-uom
            oe-ordl.tax
            oe-ordl.part-no
            oe-ordl.disc 
            oe-ordl.cas-cnt
            oe-ordl.partial
            oe-ordl.cases-unit
            oe-ordl.i-name
            oe-ordl.t-price
            oe-ordl.part-dscr1 
            oe-ordl.cost
            oe-ordl.part-dscr2
            oe-ordl.part-dscr3
            oe-ordl.req-code
            oe-ordl.req-date 
            oe-ordl.po-no
            oe-ordl.e-num
            oe-ordl.po-no-po
            oe-ordl.vend-no
            oe-ordl.prom-code 
            oe-ordl.prom-date
            oe-ordl.s-man[1]
            fi_sname-1
            oe-ordl.s-pct[1]
            oe-ordl.s-comm[1] 
            oe-ordl.s-man[2]
            fi_sname-2
            oe-ordl.s-pct[2]
            oe-ordl.s-comm[2]
            oe-ordl.s-man[3]
            fi_sname-3
            oe-ordl.s-pct[3]
            oe-ordl.s-comm[3] 
            oe-ordl.over-pct oe-ordl.under-pct
            oe-ordl.spare-char-1
            oe-ordl.customField
            oe-ordl.whsed
            fi_sman-lbl 
            fi_sname-1
            fi_s-pct-lbl
            fi_s-comm-lbl
            oe-ordl.SourceEstimateID            
            oe-ordl.managed
            WITH FRAME {&frame-name}.

        /*     IF oe-ordl.whsed:HIDDEN = NO THEN                  */
        /*        DISPLAY oe-ordl.whsed WITH FRAME {&FRAME-NAME}. */

        IF AVAILABLE itemfg THEN 
        DO:
            ASSIGN 
                spare-dec-1:SCREEN-VALUE = STRING(itemfg.spare-dec-1).
            RUN pSetValidUOMList(itemfg.company, itemfg.i-no).
        END.
        IF oe-ordl.vend-no:SCREEN-VALUE EQ "0" THEN
            ASSIGN oe-ordl.vend-no:SCREEN-VALUE = "".   /*task 03201407 */

        ASSIGN 
            oe-ordl.spare-char-1:TOOLTIP = getOrdStatDescr(oe-ordl.spare-char-1).
        IF oe-ordl.spare-int-2 > 0 THEN
            fi_JobStartDate:SCREEN-VALUE = STRING(DATE(oe-ordl.spare-int-2)).
        fiPrevOrder:SCREEN-VALUE = fnPrevOrder(oe-ordl.est-no:SCREEN-VALUE, oe-ord.ord-no).
        RUN new-type.
        RUN new-s-man (0).

    END.

    ENABLE btn_ok btn_cancel WITH FRAME {&frame-name}.    
    VIEW FRAME {&frame-name}.
    APPLY "entry" TO FRAME {&frame-name}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI d-oeitem  _DEFAULT-ENABLE
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
  DISPLAY fiPrevOrder fiPromDtLabel fi_type-dscr fi_qty-uom spare-dec-1 
          fi_s-pct-lbl fi_s-comm-lbl fi_sman-lbl fi_sname-1 fi_sname-2 
          fi_sname-3 fi_sname-lbl fi_jobStartDate 
      WITH FRAME d-oeitem.
  IF AVAILABLE oe-ordl THEN 
    DISPLAY oe-ordl.est-no oe-ordl.sourceEstimateID oe-ordl.job-no oe-ordl.job-no2 
          oe-ordl.qty oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name 
          oe-ordl.part-dscr1 oe-ordl.part-dscr2 oe-ordl.part-dscr3 oe-ordl.po-no 
          oe-ordl.e-num oe-ordl.po-no-po oe-ordl.vend-no oe-ordl.price 
          oe-ordl.pr-uom oe-ordl.tax oe-ordl.disc oe-ordl.cas-cnt 
          oe-ordl.t-price oe-ordl.partial oe-ordl.cost oe-ordl.cases-unit 
          oe-ordl.type-code oe-ordl.customField oe-ordl.managed oe-ordl.whsed 
          oe-ordl.s-man[1] oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] 
          oe-ordl.s-pct[2] oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] 
          oe-ordl.s-comm[3] oe-ordl.over-pct oe-ordl.under-pct oe-ordl.req-code 
          oe-ordl.prom-code oe-ordl.req-date oe-ordl.prom-date 
          oe-ordl.spare-char-1 oe-ordl.spare-dec-1 oe-ordl.spare-char-2 
      WITH FRAME d-oeitem.
  ENABLE oe-ordl.est-no oe-ordl.sourceEstimateID oe-ordl.qty fi_qty-uom 
         oe-ordl.i-no oe-ordl.part-no oe-ordl.i-name oe-ordl.part-dscr1 
         oe-ordl.part-dscr2 oe-ordl.part-dscr3 oe-ordl.po-no oe-ordl.e-num 
         oe-ordl.po-no-po oe-ordl.price oe-ordl.pr-uom oe-ordl.tax oe-ordl.disc 
         oe-ordl.cas-cnt oe-ordl.partial oe-ordl.cases-unit oe-ordl.type-code 
         oe-ordl.customField oe-ordl.managed oe-ordl.whsed oe-ordl.s-man[1] 
         oe-ordl.s-pct[1] oe-ordl.s-comm[1] oe-ordl.s-man[2] oe-ordl.s-pct[2] 
         oe-ordl.s-comm[2] oe-ordl.s-man[3] oe-ordl.s-pct[3] oe-ordl.s-comm[3] 
         oe-ordl.over-pct oe-ordl.under-pct oe-ordl.req-code oe-ordl.prom-code 
         oe-ordl.req-date oe-ordl.prom-date Btn_OK Btn_Done Btn_Cancel Btn_hist 
         oe-ordl.spare-char-1 oe-ordl.spare-dec-1 oe-ordl.spare-char-2 
         fi_jobStartDate btn-quotes btnTagsUnder 
      WITH FRAME d-oeitem.
  {&OPEN-BROWSERS-IN-QUERY-d-oeitem}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exit-delete d-oeitem 
PROCEDURE exit-delete :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER exit-oe-ordl FOR oe-ordl.
    DEFINE BUFFER temp-itemfg  FOR itemfg.

    IF lv-item-recid NE ? THEN 
    DO WHILE TRUE:
        FIND exit-oe-ordl WHERE RECID(exit-oe-ordl) EQ lv-item-recid
        EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAILABLE exit-oe-ordl THEN 
        DO:
            DELETE exit-oe-ordl.
            LEAVE.
        END.
    END.

    IF AVAILABLE oe-ord THEN 
    DO:
        IF CAN-FIND(FIRST b-oe-ordl 
            WHERE b-oe-ordl.company EQ oe-ord.company
            AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
            AND b-oe-ordl.line    GE 1
            AND b-oe-ordl.line    LT 99999999) THEN
            FOR EACH b-oe-ordl
                WHERE b-oe-ordl.company EQ oe-ord.company
                AND b-oe-ordl.ord-no  EQ oe-ord.ord-no
                AND (b-oe-ordl.line   LT 1 OR
                b-oe-ordl.line   GE 99999999):
                DELETE b-oe-ordl.
            END.  
    END.

    op-cancel = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-steps d-oeitem 
PROCEDURE final-steps :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE VARIABLE v-pallet-cnt AS DECIMAL NO-UNDO.
    DEFINE BUFFER temp-itemfg FOR itemfg.
    DEFINE VARIABLE lv-job-recid AS RECID NO-UNDO.    

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
    fil_id = RECID(oe-ordl).

    IF oe-ordl.est-no NE "" AND NOT AVAILABLE xest THEN
        FIND FIRST xest
            WHERE xest.company EQ cocode
            AND xest.est-no  EQ oe-ordl.est-no
            NO-LOCK NO-ERROR.
    IF AVAILABLE xest THEN 
    DO:
        IF lv-new-tandem NE ? THEN RUN upd-new-tandem.
        ELSE
            IF ll-is-tandem THEN RUN upd-tandem.

        IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN 
        DO:
            s-est-no = oe-ordl.est-no.
            RUN oe/fgadd2.p.   /** 2pc box fg create/update routine **/
        END.
        /* 06051407 - make sure itemfg.pur-man matches eb records */
        FOR EACH eb WHERE eb.company EQ xest.company
            AND eb.est-no EQ xest.est-no
            AND eb.pur-man EQ TRUE
            NO-LOCK:
            FIND FIRST temp-itemfg WHERE temp-itemfg.company EQ eb.company
                AND temp-itemfg.i-no EQ eb.stock-no
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE temp-itemfg AND temp-itemfg.pur-man NE eb.pur-man THEN
                temp-itemfg.pur-man = eb.pur-man.
            RELEASE temp-itemfg.
        END.
    /*ELSE
    IF v-qty-mod AND xest.est-type GE 3 AND xest.est-type LE 4 THEN RUN oe/tancomup.p.*/
    END.

    IF AVAILABLE oe-ordl AND (oe-ordl.est-no EQ "" AND oe-ordl.job-no EQ "") AND lCreateJobFromFG THEN
    DO:
        FIND FIRST temp-itemfg NO-LOCK
            WHERE temp-itemfg.company EQ cocode
            AND temp-itemfg.i-no EQ oe-ordl.i-no
            NO-ERROR.
          
        IF AVAILABLE temp-itemfg AND temp-itemfg.stat EQ "A" AND temp-itemfg.pur-man EQ NO 
            AND temp-itemfg.i-code EQ "C" AND temp-itemfg.est-no NE "" THEN
        DO:
            MESSAGE "Create a job using the estimate number from the FG item: " 
                VIEW-AS ALERT-BOX  QUESTION BUTTONS YES-NO
                UPDATE lCreateJob AS LOGICAL.
            IF lCreateJob THEN
            DO:                   
                RUN create-job (INPUT temp-itemfg.est-no, OUTPUT lv-job-recid).
                FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.                
            END.
        END.      
    END.
    ELSE 
    DO:
        IF AVAILABLE oe-ordl AND oe-ordl.est-no NE "" AND v-create-job THEN 
        DO:
            FIND FIRST job WHERE job.company EQ cocode
                AND job.job-no  EQ oe-ordl.job-no
                AND job.job-no2 EQ oe-ordl.job-no2 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN 
            DO:
                RUN create-job (INPUT oe-ordl.est-no, OUTPUT lv-job-recid).
                FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
            END.
        END.    
      
    END.
  
    RUN oe/ordlup.p.         /* Update Inventory and Job Costing */
   
    /*                      Moved to final-steps2 JLF 10/25/04
    IF oe-ordl.est-no NE "" THEN DO:
      fil_id = RECID(oe-ordl).
   
      IF oe-ord.est-no EQ "" OR (v-qty-mod AND NOT ll-new-record) THEN DO:
        RUN oe/estupl.p.
        fil_id = RECID(oe-ordl).
      END.
    END.
      FIND CURRENT oe-ordl.
    /*END.*/
  
    /* freight calc */
    run oe/oe-frtcl.p.  /* Calculate Freight  */
  
    oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000.
    */

    IF CAN-FIND(FIRST b-oe-ordl 
        WHERE b-oe-ordl.company EQ oe-ordl.company
        AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
        AND b-oe-ordl.line    GE 1
        AND b-oe-ordl.line    LT 99999999) THEN
        FOR EACH b-oe-ordl
            WHERE b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
            AND (b-oe-ordl.line   LT 1 OR
            b-oe-ordl.line   GE 99999999):
            DELETE b-oe-ordl.
        END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE final-steps2 d-oeitem 
PROCEDURE final-steps2 :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER temp-itemfg FOR itemfg.

    DEFINE VARIABLE v-q-back     AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-q-backl    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lMsgResponse AS LOGICAL NO-UNDO.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    IF oe-ordl.est-no NE "" THEN            /* Est no on line is NOT blank */
    DO TRANSACTION:
        ASSIGN 
            fil_id = RECID(oe-ordl).

        IF NOT v-qty-mod THEN               
            RUN oe/job-qty.p (INPUT  ROWID(oe-ordl), 
                OUTPUT v-qty-mod).

        IF  oe-ord.est-no EQ "" OR          /* Est no on order is blank, or */
            (v-qty-mod AND                  /* qty changed on an existing estimate-based line */
            (NOT ll-new-record OR 
            lv-new-tandem NE ?)
            ) THEN 
        DO:
            FRAME {&frame-name}:SENSITIVE = NO.      
            RUN oe/estupl.p.                /* po/doPo is run from here */
            FRAME {&frame-name}:SENSITIVE = YES.
            ASSIGN 
                fil_id = RECID(oe-ordl).
        END.
         
        IF lv-q-no NE 0 THEN 
        DO:         
            FIND CURRENT oe-ordl.
            ASSIGN 
                oe-ordl.q-no = lv-q-no.                  
        END.
    END.
    ELSE IF oe-ordl.est-no EQ ""            /* Est-no on line is blank and not a transfer order */
            AND oe-ord.type NE "T" THEN 
        DO:  
            IF lv-add-mode                      /* adding a line, */ 
                OR (NOT ip-type BEGINS "update-"    /* or not updating and qty changes or no pono on line, */
                AND (v-qty-mod                  /* or new tandem item */
                OR oe-ordl.po-no-po EQ 0 
                OR lv-new-tandem NE ?      
                OR NOT CAN-FIND(FIRST po-ord WHERE 
                po-ord.company EQ oe-ordl.company AND 
                po-ord.po-no   EQ oe-ordl.po-no-po)
                )
                ) THEN 
            DO:
                ASSIGN 
                    lMsgResponse = TRUE.
            
                IF oe-ord.Pricehold THEN
                    RUN displayMessageQuestionLog (INPUT  "33",
                        OUTPUT lMsgResponse).
                IF lMsgResponse THEN
                    RUN po/doPo.p(YES).
            END. 
        END.
  
    DO TRANSACTION:     
    
        FIND CURRENT oe-ordl.    

        /* freight calc */
        RUN oe/oe-frtcl.p.  /* Calculate Freight  */

        ASSIGN
            oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000
            oe-ordl.q-qty  = v-margin.

        FIND CURRENT oe-ordl NO-LOCK.
    END.
          
              
    /* This section is needed because previous calculations of component
       quantities were based on the quantity before it was updated */
    
    FIND FIRST fg-set WHERE fg-set.company = cocode
        AND fg-set.SET-no  = oe-ordl.i-no NO-LOCK NO-ERROR.
  
    IF AVAILABLE fg-set THEN 
    DO:

        /* Make sure it's the exclusive-lock that finds the itemfg-loc */
     

        FOR EACH fg-set WHERE fg-set.company = cocode
            AND fg-set.SET-no  = oe-ordl.i-no NO-LOCK:
            RELEASE itemfg-loc.
            FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-ordl.company
                AND bf-oe-ordl.ord-no  EQ oe-ordl.ord-no
                AND bf-oe-ordl.i-no    EQ fg-set.part-no 
                NO-LOCK NO-ERROR.
            IF AVAILABLE bf-oe-ordl THEN
                FIND FIRST bf-itemfg WHERE bf-itemfg.company EQ cocode
                    AND bf-itemfg.i-no    EQ fg-set.part-no
                    EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-ord AND avail(bf-itemfg) THEN 
            DO:                           
                RUN fg/chkfgloc.p (INPUT bf-itemfg.i-no, INPUT oe-ord.loc).
                FIND FIRST itemfg-loc 
                    WHERE itemfg-loc.company EQ bf-itemfg.company
                    AND itemfg-loc.i-no    EQ bf-itemfg.i-no
                    AND itemfg-loc.loc     EQ oe-ord.loc
                    EXCLUSIVE-LOCK NO-ERROR.
            END.

            IF AVAILABLE bf-oe-ordl AND avail(bf-itemfg) AND xoe-ord.type NE "T" THEN 
            DO:          
                RUN fg/calcqa&b.p (ROWID(bf-itemfg), OUTPUT bf-itemfg.q-alloc, OUTPUT v-q-back). 
                            
            END.
            IF AVAIL(bf-itemfg) AND bf-itemfg.q-alloc LT 0 THEN 
            DO:
                bf-itemfg.q-alloc = 0.
                IF AVAILABLE itemfg-loc THEN
                    itemfg-loc.q-alloc = 0.
            END.
            IF AVAILABLE itemfg-loc THEN
                itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
            FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
        END.
    END.  

    fil_id = RECID(oe-ordl).
    RUN oe/ordlup.p.         /* Update Inventory and Job Costing */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-eb-info d-oeitem 
PROCEDURE get-eb-info :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF oe-ordl.est-no:screen-value IN FRAME {&frame-name} <> "" THEN 
    DO:
        FIND FIRST eb WHERE eb.company = cocode AND
            eb.est-no = oe-ordl.est-no:screen-value
            AND eb.cust-no = oe-ord.cust-no
            AND ((eb.est-type = 1 AND eb.form-no <> 0) OR
            (eb.est-type = 2 AND eb.form-no = 0) OR
            (eb.est-type = 5 AND eb.form-no <> 0) OR
            (eb.est-type = 6 AND eb.form-no = 0) )
            NO-LOCK NO-ERROR.
        IF AVAILABLE eb THEN ls-stock = eb.stock-no.
        fiPrevOrder:SCREEN-VALUE = fnPrevOrder(oe-ordl.est-no:SCREEN-VALUE, oe-ord.ord-no).
    END.
    ELSE ls-stock = "".
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-est-comm d-oeitem 
PROCEDURE get-est-comm :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
   
    DEFINE VARIABLE v-tmp-price-2    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-price-per-1000 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-value      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-est-no-value   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-pr-uom-value   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-i-no-value     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-cas-cnt-value  LIKE oe-ordl.cas-cnt NO-UNDO.
    DEFINE VARIABLE v-price-value    AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) = ip-ordl-rowid NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN
        RETURN NO-APPLY.

    DO WITH FRAME {&FRAME-NAME}:

        IF ip-on-screen THEN
            ASSIGN v-qty-value     = INT(oe-ordl.qty:screen-value)
                v-est-no-value  = oe-ordl.est-no:SCREEN-VALUE
                v-pr-uom-value  = oe-ordl.pr-uom:SCREEN-VALUE
                v-i-no-value    = oe-ordl.i-no:SCREEN-VALUE
                v-cas-cnt-value = DEC(oe-ordl.cas-cnt:SCREEN-VALUE)
                v-price-value   = DEC(oe-ordl.price:SCREEN-VALUE).
        ELSE
            ASSIGN v-qty-value     = bf-oe-ordl.qty
                v-est-no-value  = bf-oe-ordl.est-no
                v-pr-uom-value  = bf-oe-ordl.pr-uom
                v-i-no-value    = bf-oe-ordl.i-no
                v-cas-cnt-value = bf-oe-ordl.cas-cnt
                v-price-value   = bf-oe-ordl.price.

        IF oeestcom-log = YES AND v-qty-value NE 0 THEN
        DO:
            FIND FIRST eb WHERE
                eb.company EQ cocode AND
                eb.est-no EQ v-est-no-value AND
                ((eb.est-type = 1 AND eb.form-no <> 0) OR
                (eb.est-type = 2 AND eb.form-no = 0) OR
                (eb.est-type = 4 AND eb.form-no <> 0) OR
                (eb.est-type = 5 AND eb.form-no <> 0) OR
                (eb.est-type = 6 AND eb.form-no = 0) OR
                (eb.est-type = 8 AND eb.form-no <> 0))
                NO-LOCK NO-ERROR.
      
            IF AVAILABLE eb THEN
            DO:
                FIND FIRST sman WHERE
                    sman.company EQ eb.company AND
                    sman.sman EQ eb.sman
                    NO-LOCK NO-ERROR.
      
                IF AVAILABLE sman AND sman.commbasis EQ "M" THEN
                DO:
                    IF v-pr-uom-value NE "M" THEN
                    DO:
                        FIND FIRST itemfg WHERE
                            itemfg.company EQ cocode AND
                            itemfg.i-no EQ v-i-no-value
                            NO-LOCK NO-ERROR.
               
                        ASSIGN
                            v-tmp-price-2    = IF v-pr-uom-value BEGINS "L" AND
                                    v-pr-uom-value NE "LB" THEN
                                    IF DEC(v-qty-value) LT 0 THEN -1
                                    ELSE 1
                                 ELSE
                                 IF v-pr-uom-value EQ "CS" OR v-pr-uom-value EQ "PLT" THEN
                                    DEC(v-qty-value) / (IF v-cas-cnt-value NE 0 THEN v-cas-cnt-value ELSE
                                       IF AVAILABLE itemfg AND itemfg.case-count NE 0 THEN
                                                itemfg.case-count ELSE 1)
                                  ELSE
                                  IF v-pr-uom-value EQ "C" THEN
                                     DEC(v-qty-value) / 100
                                  ELSE
                                     DEC(v-qty-value)
                       
                            v-tmp-price-2    = v-tmp-price-2 * v-price-value
                            v-price-per-1000 = v-tmp-price-2 / ( DEC(v-qty-value) / 1000).
                    END.
                    ELSE
                        v-price-per-1000 = v-price-value.
      
                    RELEASE itemfg.

                    /*not combo est*/
                    IF NOT (eb.est-type EQ 4 OR eb.est-type EQ 8) THEN
                    DO:
                        FOR EACH probe WHERE
                            probe.company = eb.company AND
                            probe.est-no = eb.est-no AND
                            probe.probe-date NE ? AND
                            probe.est-qty EQ INT(v-qty-value) 
                            NO-LOCK
                            BY probe.probe-date DESCENDING
                            BY probe.probe-time DESCENDING:
                      
                            IF probe.sell-price EQ v-price-per-1000 OR
                                ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                                LEAVE.
                        END.
                 
                        IF NOT AVAILABLE probe THEN
                            FOR EACH probe WHERE
                                probe.company = eb.company AND
                                probe.est-no = eb.est-no AND
                                probe.probe-date NE ? AND
                                probe.est-qty EQ INT(v-qty-value)
                                NO-LOCK
                                BY probe.probe-date DESCENDING
                                BY probe.probe-time DESCENDING:
                         
                                LEAVE.
                            END.
                    END.
                    ELSE
                    DO:
                        FOR EACH probe WHERE
                            probe.company = eb.company AND
                            probe.est-no = eb.est-no AND
                            probe.probe-date NE ?
                            NO-LOCK
                            BY probe.probe-date DESCENDING
                            BY probe.probe-time DESCENDING:
                      
                            IF probe.sell-price EQ v-price-per-1000 OR
                                ROUND(probe.sell-price,2) EQ v-price-per-1000 THEN

                                LEAVE.
                        END.
                 
                        IF NOT AVAILABLE probe THEN
                            FOR EACH probe WHERE
                                probe.company = eb.company AND
                                probe.est-no = eb.est-no AND
                                probe.probe-date NE ?
                                NO-LOCK
                                BY probe.probe-date DESCENDING
                                BY probe.probe-time DESCENDING:
                         
                                LEAVE.
                            END.
                    END.
      
                    IF AVAILABLE probe THEN
                    DO:
                        FIND oe-ord OF oe-ordl.
                        IF ip-on-screen THEN
                            ASSIGN oe-ordl.s-comm[1]:SCREEN-VALUE = STRING(probe.comm).
                        ELSE
                            ASSIGN bf-oe-ordl.s-comm[1] = probe.comm.

                        ASSIGN
                            oe-ord.s-comm[1] = probe.comm
                            oe-ord.t-fuel    = probe.market-price
                            v-margin         = probe.market-price.

                        FIND oe-ord OF oe-ordl NO-LOCK.
                    END.
     
                    RELEASE probe.
                    RELEASE sman.
                END.
                RELEASE eb.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-next-workday d-oeitem 
PROCEDURE get-next-workday :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ip-date AS DATE.
    DEFINE INPUT PARAMETER ip-next-prev AS cha . /* next or prev */

    DEFINE VARIABLE is-workday AS LOG NO-UNDO.

    IF WEEKDAY(ip-date) = 1 OR WEEKDAY(ip-date) = 7 THEN 
    DO:
        is-workday = NO.
        DO WHILE NOT is-workday:
            IF ip-next-prev = "Next" THEN ip-date = ip-date + 1.
            ELSE IF ip-next-prev = "PREV" THEN ip-date = ip-date - 1.

            IF WEEKDAY(ip-date) = 1 OR WEEKDAY(ip-date) = 7 THEN NEXT.
            ELSE is-workday = YES.

        
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-num-of-hol d-oeitem 
PROCEDURE get-num-of-hol :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-date-from AS DATE  NO-UNDO.
    DEFINE INPUT PARAMETER ip-date-to AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER op-num-of-wkend AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-chk-date AS DATE NO-UNDO.

    lv-chk-date = ip-date-from.
    op-num-of-wkend = 0.
    DO i = 1 TO ip-date-to - ip-date-from + 1:
        IF WEEKDAY(lv-chk-date) = 1 OR WEEKDAY(lv-chk-date) = 7 
            THEN op-num-of-wkend = op-num-of-wkend + 1.
        lv-chk-date = lv-chk-date + 1.
    END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-price d-oeitem 
PROCEDURE get-price :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rowid     AS ROWID   NO-UNDO.
    DEFINE VARIABLE lv-price-ent LIKE price-ent NO-UNDO.
    DEFINE VARIABLE dTotalPrice  AS DECIMAL NO-UNDO.
          
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT price-ent                           AND
            AVAILABLE oe-ordl                           THEN 
        DO:

            lv-price-ent = price-ent.
            IF NOT lv-add-mode THEN price-ent = YES.
  
            IF NOT AVAILABLE xoe-ord THEN
                FIND FIRST xoe-ord
                    WHERE xoe-ord.company EQ g_company
                    AND xoe-ord.ord-no  EQ oe-ordl.ord-no
                    NO-LOCK NO-ERROR.

            ASSIGN
                save_id   = RECID(oe-ordl)
                lv-rowid  = ROWID(oe-ordl)
                v-i-item  = oe-ordl.i-no:SCREEN-VALUE
                v-i-qty   = INT(oe-ordl.qty:SCREEN-VALUE)
                v-qty-mod = oe-ordl.qty:MODIFIED.

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ v-i-item
                NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:          
                RUN oe/oe-price.p.
                IF matrixExists THEN 
                DO:  
                    matrixTag = "Item No:" + string(v-i-item) + " Customer No:" + string(oe-ordl.cust-no) + " Ship ID:" + oe-ordl.ship-id + " Quantity:" + string(v-i-qty). 

                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Price Matrix " + matrixTag
                        ). 
                END.
                FIND oe-ordl WHERE ROWID(oe-ordl) EQ lv-rowid NO-ERROR.
                IF matrixExists THEN 
                    DISPLAY oe-ordl.price oe-ordl.pr-uom oe-ordl.t-price.
                RUN Conv_CalcTotalPrice(cocode, 
                    oe-ordl.i-no:SCREEN-VALUE,
                    DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                    DECIMAL(oe-ordl.price:SCREEN-VALUE),
                    oe-ordl.pr-uom:SCREEN-VALUE,
                    DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                    DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                    OUTPUT dTotalPrice).
                oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
        //{oe/ordltot.i oe-ordl qty oe-ordl}
            END.

            price-ent = lv-price-ent.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-price-hidden d-oeitem 
PROCEDURE get-price-hidden :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-ordl-rowid AS ROWID NO-UNDO.
    DEFINE VARIABLE lv-rowid         AS ROWID NO-UNDO.
    DEFINE VARIABLE lv-price-ent     LIKE price-ent NO-UNDO.
    DEFINE VARIABLE lv-save-xoe-ordl AS ROWID NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ip-ordl-rowid NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN
        RETURN NO-APPLY.
    DO WITH FRAME {&FRAME-NAME}:
        IF NOT price-ent                           AND
            AVAILABLE bf-oe-ordl                           AND
            TRIM(bf-oe-ordl.est-no) EQ "" THEN 
        DO:

            lv-price-ent = price-ent.
            IF NOT lv-add-mode THEN price-ent = YES.
  
            IF NOT AVAILABLE xoe-ord THEN
                FIND FIRST xoe-ord
                    WHERE xoe-ord.company EQ g_company
                    AND xoe-ord.ord-no  EQ bf-oe-ordl.ord-no
                    NO-LOCK NO-ERROR.

            ASSIGN
                save_id   = RECID(bf-oe-ordl)
                lv-rowid  = ROWID(bf-oe-ordl)
                v-i-item  = bf-oe-ordl.i-no
                v-i-qty   = INT(bf-oe-ordl.qty)
                v-qty-mod = YES. /* new record, so will have been modified */

            FIND FIRST itemfg
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ v-i-item
                NO-LOCK NO-ERROR.

            IF AVAILABLE itemfg THEN 
            DO:
                IF AVAILABLE xoe-ordl THEN
                    lv-save-xoe-ordl= ROWID(xoe-ordl).
                /* Depends on xoeitem */
                RUN oe/oe-price.p.

                FIND xoe-ordl WHERE ROWID(xoe-ordl) EQ lv-rowid NO-ERROR.
                RUN Conv_CalcTotalPrice(cocode, 
                    bf-oe-ordl.i-no,
                    bf-oe-ordl.qty,
                    bf-oe-ordl.price,
                    bf-oe-ordl.pr-uom,
                    bf-oe-ordl.disc,
                    bf-oe-ordl.cas-cnt,    
                    OUTPUT bf-oe-ordl.t-price).
    
        //{oe/ordltot3.i bf-oe-ordl qty bf-oe-ordl}
                IF lv-save-xoe-ordl NE ? THEN
                    FIND xoe-ordl WHERE ROWID(xoe-ordl) = lv-save-xoe-ordl
                        EXCLUSIVE-LOCK NO-ERROR.
            END.

            price-ent = lv-price-ent.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-prom-dt-info d-oeitem 
PROCEDURE get-prom-dt-info :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DO WITH FRAME {&FRAME-NAME}:                                 */
/*    FIND FIRST rejct-cd                                         */
/*        WHERE rejct-cd.TYPE = "R"                               */
/*          AND rejct-cd.CODE = oe-ordl.spare-char-3:SCREEN-VALUE */
/*        NO-LOCK NO-ERROR.                                       */
/*     IF AVAIL rejct-cd THEN                                     */
/*       fiPromDtChangeDesc:SCREEN-VALUE = rejct-cd.dscr.         */
/*     oe-ordl.spare-char-4:SCREEN-VALUE = USERID("NOSWEAT").     */
/*   END.                                                         */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-valid-uom d-oeitem 
PROCEDURE get-valid-uom :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO.
  
    DEFINE VARIABLE li AS INTEGER NO-UNDO.

    IF ipcFGITem NE "" THEN 
        RUN pSetValidUOMList(g_company, ipcFGITem).
    lv-valid-uom = "".

    IF ip-focus:NAME EQ "fi_qty-uom" THEN 
    DO:
        DO li = 1 TO NUM-ENTRIES(cUOMListQty):
            IF ENTRY(li,cUOMListQty) NE "L" THEN
                lv-valid-uom = lv-valid-uom + TRIM(ENTRY(li,cUOMListQty)) + ",".
        END.
        lv-valid-uom = TRIM(lv-valid-uom,",").
    END.
    IF ip-focus:NAME EQ "pr-uom" THEN 
    DO:
        DO li = 1 TO NUM-ENTRIES(cUOMListPrice):
            IF ENTRY(li,cUOMListPrice) NE "PLT" THEN          
                lv-valid-uom = lv-valid-uom + TRIM(ENTRY(li,cUOMListPrice)) + ",".              
        END.
        lv-valid-uom = TRIM(lv-valid-uom,",").
    END.
    IF lv-valid-uom EQ "" THEN lv-valid-uom = cUOMListQty.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-valid-uom-tt d-oeitem 
PROCEDURE get-valid-uom-tt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItem AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
  
    EMPTY TEMP-TABLE ttUOM.
  
    RUN pSetValidUOMTT(g_company, ipcFGITem, ipcType).  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCostFromEstimate d-oeitem 
PROCEDURE getCostFromEstimate :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cProgramList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEbCnt       AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-estCostItem   FOR estCostItem.
    
    IF ipcEstimateNo NE "" AND NOT AVAILABLE xest THEN
        FIND FIRST xest NO-LOCK 
            WHERE xest.company EQ cocode 
            AND xest.est-no EQ ipcEstimateNo 
            NO-ERROR.

    IF AVAILABLE xest THEN 
    DO WITH FRAME {&FRAME-NAME}:
        IF fUseNewEstimating(cocode) THEN 
        DO:
            IF oe-ordl.job-no:SCREEN-VALUE NE "" THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    AND bf-estCostHeader.jobID EQ oe-ordl.job-no:SCREEN-VALUE
                    AND bf-estCostHeader.jobID2 EQ INT(oe-ordl.job-no2:SCREEN-VALUE)
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    AND bf-estCostHeader.quantityMaster LE INT(oe-ordl.qty:SCREEN-VALUE)
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostHeader NO-LOCK 
                    WHERE bf-estCostHeader.company EQ cocode
                    AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
                    NO-ERROR.
            IF AVAILABLE bf-estCostHeader THEN 
                FIND FIRST bf-estCostItem NO-LOCK 
                    WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                    AND bf-estCostItem.itemID EQ oe-ordl.i-no:SCREEN-VALUE
                    NO-ERROR.
            IF NOT AVAILABLE bf-estCostItem THEN 
                FIND FIRST bf-estCostItem NO-LOCK 
                    WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                    AND bf-estCostItem.customerPart EQ oe-ordl.part-no:SCREEN-VALUE
                    NO-ERROR. 
            IF AVAILABLE bf-estCostItem THEN 
                oe-ordl.cost:SCREEN-VALUE = STRING((IF v-full-cost THEN bf-estCostItem.costTotalFull ELSE bf-estCostItem.costTotalFactory) /
                    (bf-estCostItem.quantityRequired / 1000)).
        END.
        ELSE 
        DO:
            FIND FIRST xeb NO-LOCK
                WHERE xeb.company EQ xest.company 
                AND xeb.est-no EQ  xest.est-no
                AND xeb.part-no = oe-ordl.part-no:SCREEN-VALUE
                NO-ERROR.
            IF NOT AVAILABLE xeb THEN 
            DO:
                iEbCnt = 0.
                FOR EACH xeb 
                    WHERE xeb.company = g_company 
                    AND xeb.est-no = ipcEstimateNo            
                    NO-LOCK:
                    iEbCnt = iEbCnt + 1.
                END.
                /* If there is only one record, use it 03191507 */
                IF iEbCnt EQ 1 THEN
                    FIND FIRST xeb
                        WHERE xeb.company = g_company 
                        AND xeb.est-no = ipcEstimateNo
                        NO-LOCK NO-ERROR.
            END.
         
            IF AVAILABLE xeb THEN
                FIND FIRST xef
                    WHERE xef.company = g_company 
                    AND xef.est-no = ipcEstimateNo
                    AND (xef.form-no = xeb.form-no OR xeb.form-no = 0)
                    NO-LOCK NO-ERROR.
    
            ASSIGN
                cProgramList = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
                         "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
                         "cec/tan/print4.p,cec/com/print4.p"
                qty          = INT(oe-ordl.qty:SCREEN-VALUE)
                v-shared-rel = v-rel.
    
            IF AVAILABLE xeb AND AVAILABLE xef                                     AND
                xest.est-type NE 3                                          AND
                xest.est-type NE 4                                          AND
                xest.est-type NE 8                                          AND
                (oe-ordl.qty NE qty OR DEC(oe-ordl.cost:SCREEN-VALUE) EQ 0) THEN 
            DO:
    
                RUN VALUE(ENTRY(xest.est-type,cProgramList)).     
    
                oe-ordl.cost:SCREEN-VALUE = STRING((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                    (INT(oe-ordl.qty:SCREEN-VALUE) / 1000)).
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getQtyPrice d-oeitem 
PROCEDURE getQtyPrice :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipOeordlRowid AS ROWID NO-UNDO.

    FIND tt-qty-price WHERE tt-qty-price.oeordl-ROWID EQ ipOeordlRowid
        NO-ERROR.
    IF AVAILABLE tt-qty-price THEN
        ASSIGN historyQty     = tt-historyQty
            historyPrice   = tt-historyPrice
            historyPrUOM   = tt-historyPrUOM
            setFromHistory = tt-setFromHistory.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getTagsToReset d-oeitem 
PROCEDURE getTagsToReset :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttTag.

    RUN Tag_IsTagRecordAvailableForGroup(
        INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
        INPUT "oe-ordl",
        INPUT "Price-Source",
        OUTPUT lAvailable
        ).
    IF lAvailable THEN  
    DO:
        EMPTY TEMP-TABLE ttTempTag.
        RUN GetTags(
            INPUT  STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE), 
            INPUT  "oe-ordl", 
            INPUT  "Price-Source",   
            OUTPUT  TABLE  ttTempTag
            ).
        FOR EACH ttTempTag:
            CREATE ttTag.
            BUFFER-COPY ttTempTag TO ttTag.
        END.      
        btnTags:SENSITIVE IN FRAME {&frame-name}  = TRUE.
    END.
    ELSE     
        btnTags:SENSITIVE IN FRAME {&frame-name}  = FALSE.
        
    RUN Tag_IsTagRecordAvailableForGroup(
        INPUT oe-ordl.rec_key,
        INPUT "oe-ordl",
        INPUT "Over Percentage",
        OUTPUT lAvailable
        ).
    IF lAvailable THEN  
    DO:
        EMPTY TEMP-TABLE ttTempTag.
        RUN GetTags(
            INPUT  oe-ordl.rec_key, 
            INPUT  "oe-ordl", 
            INPUT  "Over Percentage",   
            OUTPUT  TABLE  ttTempTag
            ).
        FOR EACH ttTempTag:
            CREATE ttTag.
            BUFFER-COPY ttTempTag TO ttTag.
        END.
        btnTagsOverrn:SENSITIVE IN FRAME {&frame-name} = TRUE.
    END.
    ELSE 
        btnTagsOverrn:SENSITIVE IN FRAME {&frame-name} = FALSE.
        
    RUN Tag_IsTagRecordAvailableForGroup(
        INPUT oe-ordl.rec_key,
        INPUT "oe-ordl",
        INPUT "Under Percentage",
        OUTPUT lAvailable
        ).
    IF lAvailable THEN  
    DO:
        EMPTY TEMP-TABLE ttTempTag.
        RUN GetTags(
            INPUT  oe-ordl.rec_key, 
            INPUT  "oe-ordl", 
            INPUT  "Under Percentage",   
            OUTPUT  TABLE  ttTempTag
            ).
        FOR EACH ttTempTag:
            CREATE ttTag.
            BUFFER-COPY ttTempTag TO ttTag.
        END.       
        btnTagsUnder:SENSITIVE IN FRAME {&frame-name}  = TRUE.
    END.
    ELSE 
    
        btnTagsUnder:SENSITIVE IN FRAME {&frame-name}  = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-comm d-oeitem 
PROCEDURE hide-comm :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-hidden AS LOG NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            fi_s-pct-lbl:HIDDEN      = ip-hidden
            fi_s-comm-lbl:HIDDEN     = ip-hidden
            oe-ordl.s-pct[1]:HIDDEN  = ip-hidden
            oe-ordl.s-pct[2]:HIDDEN  = ip-hidden
            oe-ordl.s-pct[3]:HIDDEN  = ip-hidden
            oe-ordl.s-comm[1]:HIDDEN = ip-hidden
            oe-ordl.s-comm[2]:HIDDEN = ip-hidden
            oe-ordl.s-comm[3]:HIDDEN = ip-hidden.
   
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemfg-cost d-oeitem 
PROCEDURE itemfg-cost :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-uom  LIKE oe-ordl.pr-uom NO-UNDO.
    DEFINE VARIABLE lv-cost AS DECIMAL DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-cost  AS DECIMAL DECIMALS 10 NO-UNDO.
  
    DO WITH FRAME {&FRAME-NAME}:
    
   
        IF oe-ordl.job-no:screen-value = "" THEN 
        DO:
       
            v-cost = DEC(oe-ordl.cost:SCREEN-VALUE).

            FIND FIRST itemfg
                WHERE itemfg.company = g_company
                AND itemfg.i-no = oe-ordl.i-no:screen-value
                NO-LOCK NO-ERROR.
            FIND FIRST po-ordl WHERE po-ordl.company   EQ cocode
                AND po-ordl.i-no      EQ oe-ordl.i-no:screen-value
                AND po-ordl.po-no     EQ int(oe-ordl.po-no-po:screen-value)
                AND po-ordl.item-type EQ NO
                USE-INDEX item-ordno NO-LOCK NO-ERROR.
            IF AVAILABLE po-ordl AND int(oe-ordl.po-no-po:screen-value) NE 0 THEN
                ASSIGN oe-ordl.pr-uom:screen-value = IF oe-ordl.pr-uom:SCREEN-VALUE = "" THEN po-ordl.cons-uom ELSE oe-ordl.pr-uom:SCREEN-VALUE
                    lv-uom                      = IF po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
                    v-cost                      = po-ordl.cons-cost
                    oe-ordl.cost:SCREEN-VALUE   = STRING(po-ordl.cons-cost).
            ELSE
                IF AVAILABLE itemfg THEN
                    ASSIGN oe-ordl.pr-uom:screen-value = IF itemfg.prod-uom NE "" AND oe-ordl.pr-uom:SCREEN-VALUE = "" THEN itemfg.prod-uom ELSE oe-ordl.pr-uom:screen-value
                        lv-uom                      = IF itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
                        v-cost                      = get-itemfg-cost(itemfg.i-no)
                        oe-ordl.cost:SCREEN-VALUE   = STRING(get-itemfg-cost(itemfg.i-no)).
      
            IF lv-uom NE "M" THEN 
            DO:
                RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                    v-cost, OUTPUT lv-cost).
                ASSIGN 
                    oe-ordl.cost:screen-value = STRING(lv-cost)
                    /*oe-ordl.pr-uom:screen-value = itemfg.sell-uom ?? */ .                        
            END.
      
            IF AVAILABLE po-ordl AND int(oe-ordl.po-no-po:screen-value) NE 0 THEN
            DO:
                FIND FIRST po-ord WHERE
                    po-ord.company EQ po-ordl.company AND
                    po-ord.po-no EQ po-ordl.po-no
                    NO-LOCK NO-ERROR.
      
                IF AVAILABLE po-ord THEN
                DO:
                    IF NOT lNewVendorItemCost THEN 
                        FIND FIRST e-itemfg-vend WHERE
                            e-itemfg-vend.company EQ po-ordl.company AND
                            e-itemfg-vend.i-no EQ po-ordl.i-no AND
                            e-itemfg-vend.vend-no EQ po-ord.vend-no AND
                            e-itemfg-vend.est-no EQ ""
                            NO-LOCK NO-ERROR.

                    IF AVAILABLE e-itemfg-vend THEN
                    DO:
                        oe-ordl.cost:SCREEN-VALUE = STRING(DEC(oe-ordl.cost:SCREEN-VALUE) * (1 + (e-itemfg-vend.markup / 100.0 ))).
                    END.
                    RELEASE po-ord.
                END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE itemfg-sman d-oeitem 
PROCEDURE itemfg-sman :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cNewRep AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    /* if order is from an estimate, use the esitmate sales rep # */
    IF oe-ordl.est-no GT "" THEN
        RETURN.

    DO WITH FRAME {&FRAME-NAME}:
    
   
        FIND FIRST itemfg
            WHERE itemfg.company = g_company
            AND itemfg.i-no = oe-ordl.i-no:screen-value
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE itemfg THEN
            RETURN.

        RUN fg/fgSlsRep.p (INPUT cocode,
            INPUT oe-ord.cust-no,
            INPUT oe-ordl.part-no:screen-value,
            INPUT itemfg.i-no,
            OUTPUT cNewRep).
   
        IF cNewRep GT "" AND cNewRep NE oe-ordl.s-man[1]:SCREEN-VALUE THEN 
        DO:
            oe-ordl.s-man[1]:SCREEN-VALUE = cNewRep. 
            RUN new-s-man (1).
        END.
      
        IF cNewRep GT "" AND cNewRep NE oe-ord.sman[1] THEN 
        DO:

            /* Update the header with the new sales rep */
            FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord)
                EXCLUSIVE-LOCK NO-ERROR.
     
            IF AVAILABLE bf-oe-ord THEN 
            DO:
                ASSIGN 
                    bf-oe-ord.sman[1] = cNewRep.
                FIND sman WHERE sman.company = oe-ord.company
                    AND sman.sman = bf-oe-ord.sman[1]
                    NO-LOCK NO-ERROR.
       
                IF AVAILABLE sman THEN ASSIGN bf-oe-ord.sname[1]  = sman.sname
                        bf-oe-ord.s-comm[1] = (sman.scomm).             
            END. /* fi avail header */

            RELEASE bf-oe-ord.
        END. /* if new rep found */
    END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leave-qty d-oeitem 
PROCEDURE leave-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-est-no   AS CHARACTER.
    DEFINE VARIABLE lv-recid    AS RECID     NO-UNDO.
    DEFINE VARIABLE lv-price    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-pr-uom   AS cha       NO-UNDO.
    DEFINE VARIABLE v-tmp-part  AS cha       NO-UNDO.
    DEFINE VARIABLE v-set       AS cha       NO-UNDO.
    DEFINE VARIABLE v-qty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-checkset  AS LOG       NO-UNDO.
    DEFINE VARIABLE dTotalPrice AS DECIMAL   NO-UNDO.
 
    DO WITH FRAME {&FRAME-NAME}:
        IF (ll-help-ran AND lv-help-qty = int(oe-ordl.qty:SCREEN-VALUE)) THEN 
        DO:
            IF ll-help-ran THEN ASSIGN ll-help-ran       = NO
                    lv-help-qty       = 0
                    ll-qty-leave-done = YES.
            RETURN.
        END.

   
        lv-est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no:SCREEN-VALUE))) +
            TRIM(oe-ordl.est-no:SCREEN-VALUE).

        IF li-prev-qty NE INT(oe-ordl.qty:SCREEN-VALUE) THEN ll-got-qtprice = NO.

        IF li-prev-qty <> INT(oe-ordl.qty:SCREEN-VALUE) OR
            ip-type EQ "update-2" OR ip-type EQ "add"    THEN 
        DO:
   
            oe-ordl.qty:SCREEN-VALUE = FILL(" ",8 - LENGTH(TRIM(oe-ordl.qty:SCREEN-VALUE))) + TRIM(oe-ordl.qty:SCREEN-VALUE).

            li-prev-qty = INT(oe-ordl.qty:SCREEN-VALUE).
       
            /* If case count is too high, this will get picked up on OK button */
            IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 THEN
                IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 AND INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 THEN
                    oe-ordl.cas-cnt:SCREEN-VALUE = oe-ordl.qty:SCREEN-VALUE.
                ELSE
                    IF INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN
                        oe-ordl.cas-cnt:SCREEN-VALUE = "1".

            /*if oe-ordl.i-no:screen-value = "" or oe-ordl.i-no:screen-value = "0" then return.*/
   
            RUN get-price.

            IF lv-est-no NE "" AND NOT AVAILABLE xest THEN
                FIND FIRST xest WHERE xest.company EQ cocode AND
                    xest.est-no EQ lv-est-no NO-LOCK NO-ERROR.

            RUN getCostFromEstimate (lv-est-no).

            IF AVAILABLE xest AND v-quo-price-log AND NOT ll-got-qtprice AND oe-ordl.sourceEstimateID:SCREEN-VALUE EQ "" THEN 
            DO:
                ASSIGN 
                    lv-price   = dec(oe-ordl.price:screen-value)
                    lv-pr-uom  = oe-ordl.pr-uom:screen-value
                    lv-qty     = DEC(oe-ordl.qty:SCREEN-VALUE)
                    v-tmp-part = oe-ordl.i-no:screen-value.

                RUN check-quote (v-tmp-part) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN RETURN ERROR .  

                ll-got-qtprice = YES.

                IF NOT lQuotePriceMatrix AND NOT CAN-FIND(FIRST tt-item-qty-price WHERE
                    tt-item-qty-price.tt-selected = YES AND
                    (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                    (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ ""))) THEN
                DO:
             
                    RUN pGetQuoteRec(xest.est-no,oe-ordl.part-no:screen-value,
                        v-tmp-part,
                        INPUT-OUTPUT lv-price ,
                        INPUT-OUTPUT lv-pr-uom,
                        OUTPUT lv-q-no,
                        INPUT-OUTPUT lv-qty).
                    oe-ordl.qty:SCREEN-VALUE = STRING(lv-qty).
              
                    RUN pAddTagInfoForGroup(
                        INPUT oe-ordl.rec_key,
                        INPUT "Quoted Price Quote No:" + string(lv-q-no) + " Quantity: " + string(lv-qty) 
                        ).
                END.
                ELSE IF NOT lQuotePriceMatrix THEN
                    DO:
                        FIND FIRST tt-item-qty-price WHERE
                            tt-item-qty-price.tt-selected = YES AND
                            (tt-item-qty-price.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                            (tt-item-qty-price.part-no EQ v-tmp-part AND v-tmp-part EQ "")).
                        ASSIGN
                            lv-price  = tt-item-qty-price.price
                            lv-pr-uom = tt-item-qty-price.uom
                            lv-q-no   = tt-item-qty-price.q-no.
                        IF llGotLowerPrice THEN 
                        DO:
                            llGotLowerPrice = NO.
                            DELETE tt-item-qty-price.
                            RUN pAddTagInfoForGroup(
                                INPUT oe-ordl.rec_key,
                                INPUT "Item Qty Price Quote No:" + string(lv-q-no)
                                ).
                        END.
                    END.

                ASSIGN
                    oe-ordl.price:screen-value  = STRING(lv-price)
                    oe-ordl.pr-uom:screen-value = STRING(lv-pr-uom).

                IF oe-ordl.est-no:SCREEN-VALUE NE "" AND
                    oeestcom-log = YES THEN
                    RUN get-est-comm (INPUT ROWID(oe-ordl), INPUT YES).
            END. 

            /* Begin Calculate Weight for Order */
            /* take old qty - new qty find weight and add to order */
            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ oe-ordl.i-no:screen-value NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:    
                IF itemfg.isaset AND itemfg.alloc NE YES AND v-checkset THEN 
                DO:
                    ASSIGN 
                        v-set = oe-ordl.i-no:screen-value
                        v-qty = int(oe-ordl.qty:screen-value).
                    RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
                    IF v-qty LT int(oe-ordl.qty:screen-value) THEN 
                    DO:
                        PAUSE.
                        HIDE FRAME checkset NO-PAUSE.
                    END.
                END.
            END.

            RUN oe/oe-frtcl.p.  /* Calculate Freight  */

            RUN Conv_CalcTotalPrice(cocode, 
                oe-ordl.i-no:SCREEN-VALUE,
                DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                DECIMAL(oe-ordl.price:SCREEN-VALUE),
                oe-ordl.pr-uom:SCREEN-VALUE,
                DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                OUTPUT dTotalPrice).
            oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
        //{oe/ordltot.i oe-ordl qty oe-ordl}
        END.
    END.

    ASSIGN 
        ll-help-ran       = NO
        lv-help-qty       = 0
        ll-qty-leave-done = YES .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-qty d-oeitem 
PROCEDURE new-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        RUN get-price.

    /*ASSIGN
     oe-ordl.cases:SCREEN-VALUE = 
         STRING(TRUNC((DEC(oe-ordl.qty:SCREEN-VALUE) -
                       DEC(oe-ordl.partial:SCREEN-VALUE)) /
                      DEC(oe-ordl.cas-cnt:SCREEN-VALUE),0),
                oe-ordl.cases:FORMAT).
     oe-ordl.partial:SCREEN-VALUE = 
         STRING(DEC(oe-ordl.qty:SCREEN-VALUE) -
                (DEC(oe-ordl.cases:SCREEN-VALUE) *
                 DEC(oe-ordl.cas-cnt:SCREEN-VALUE)),
                oe-ordl.partial:FORMAT).*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-s-man d-oeitem 
PROCEDURE new-s-man :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE lv-sman LIKE sman.sman NO-UNDO.
    DEFINE VARIABLE ll-all  AS LOG     NO-UNDO.
    DEFINE VARIABLE li      AS INTEGER NO-UNDO.


    IF ip-int EQ 0 THEN
        ASSIGN
            li     = 3
            ip-int = 1
            ll-all = YES.
    ELSE
        li = ip-int.

    DO ip-int = ip-int TO li WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN oe-ordl.s-man[3]:SCREEN-VALUE
        ELSE
            IF ip-int EQ 2 THEN oe-ordl.s-man[2]:SCREEN-VALUE
            ELSE oe-ordl.s-man[1]:SCREEN-VALUE.

        IF lv-sman NE "" THEN 
        DO:
            FIND FIRST sman
                WHERE sman.company EQ cocode
                AND sman.sman    EQ lv-sman
                NO-LOCK NO-ERROR.
            IF AVAILABLE sman THEN 
            DO:
                IF ip-int EQ 3 THEN 
                DO:
                    fi_sname-3:SCREEN-VALUE = sman.sname.
                    IF NOT ll-all THEN 
                    DO:
                        IF DEC(oe-ordl.s-pct[3]:SCREEN-VALUE) EQ 0 THEN
                            oe-ordl.s-pct[3]:SCREEN-VALUE = "100".
                        IF DEC(oe-ordl.s-comm[3]:SCREEN-VALUE) EQ 0 THEN
                            oe-ordl.s-comm[3]:SCREEN-VALUE = STRING(sman.scomm).
                    END.
                END.
                ELSE
                    IF ip-int EQ 2 THEN 
                    DO:
                        fi_sname-2:SCREEN-VALUE = sman.sname.
                        IF NOT ll-all THEN 
                        DO:
                            IF DEC(oe-ordl.s-pct[2]:SCREEN-VALUE) EQ 0 THEN
                                oe-ordl.s-pct[2]:SCREEN-VALUE = "100".
                            IF DEC(oe-ordl.s-comm[2]:SCREEN-VALUE) EQ 0 THEN
                                oe-ordl.s-comm[2]:SCREEN-VALUE = STRING(sman.scomm).
                        END.
                    END.
                    ELSE 
                    DO:
                        fi_sname-1:SCREEN-VALUE = sman.sname.
                        IF NOT ll-all THEN 
                        DO:
                            IF DEC(oe-ordl.s-pct[1]:SCREEN-VALUE) EQ 0 THEN
                                oe-ordl.s-pct[1]:SCREEN-VALUE = "100".
                            IF DEC(oe-ordl.s-comm[1]:SCREEN-VALUE) EQ 0 THEN
                                ASSIGN
                                    oe-ordl.s-comm[1]:SCREEN-VALUE = STRING(sman.scomm)
                                    v-margin                       = 0.
                        END.
                    END.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-tandem d-oeitem 
PROCEDURE new-tandem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-eb FOR eb.

    DEFINE VARIABLE v-rowid AS ROWID NO-UNDO.               
                 
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    RUN est/NewEstimateForm.p ('F',ROWID(est), OUTPUT lv-new-tandem).

    FIND eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.

    IF AVAILABLE eb THEN 
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            oe-ordl.est-no:SCREEN-VALUE  = oe-ord.est-no
            oe-ordl.job-no:SCREEN-VALUE  = oe-ord.job-no
            oe-ordl.job-no2:SCREEN-VALUE = STRING(oe-ord.job-no2).

        RUN est/d-selest.w (lv-new-tandem, YES, oe-ord.cust-no,
            OUTPUT v-qty-mod, OUTPUT v-rowid).

        IF v-qty-mod THEN 
        DO:
            FIND FIRST b-eb
                WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.eqty    EQ eb.eqty
                NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN
                ASSIGN
                    eb.cust-no = b-eb.cust-no
                    eb.ship-id = b-eb.ship-id.

            IF CAN-FIND(FIRST b-eb
                WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.eqty    EQ eb.eqty
                AND b-eb.part-no EQ eb.part-no
                AND ROWID(b-eb)  NE ROWID(eb)) THEN
                ASSIGN
                    eb.part-no    = ""
                    eb.stock-no   = ""
                    eb.part-dscr1 = ""
                    eb.part-dscr2 = "".

            RUN display-est-detail (RECID(eb)).

            DISABLE oe-ordl.est-no.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type d-oeitem 
PROCEDURE new-type :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        oe-ordl.type-code:SCREEN-VALUE = CAPS(oe-ordl.type-code:SCREEN-VALUE).

        li = LOOKUP(oe-ordl.type-code:SCREEN-VALUE,lv-type-codes) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN li = 0.

        IF li GT 0 AND li LE NUM-ENTRIES(lv-type-dscrs) THEN 
        DO:
            fi_type-dscr:SCREEN-VALUE = ENTRY(li,lv-type-dscrs).
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oescreen-values d-oeitem 
PROCEDURE oescreen-values :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    IF AVAILABLE oe-ordl THEN 
    DO:
        RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "I" /* Integer */, YES /* check by cust */, 
            INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
            OUTPUT v-rtn-char, OUTPUT v-rec-found).
        oescreen-int = INTEGER(v-rtn-char) NO-ERROR.
    
        IF AVAILABLE oe-ordl THEN
            RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "C" /* Char */, YES /* check by cust */, 
                INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
                OUTPUT v-rtn-char, OUTPUT v-rec-found).
        oescreen-cha = v-rtn-char NO-ERROR.
    
        IF AVAILABLE oe-ordl THEN
            RUN sys/ref/nk1look.p (INPUT cocode, "OESCREEN", "L" /* Logical */, YES /* check by cust */, 
                INPUT YES /* use cust not vendor */, oe-ordl.cust-no, "" /* ship-to*/,
                OUTPUT v-rtn-char, OUTPUT v-rec-found).
        oescreen-log = LOGICAL(v-rtn-char) NO-ERROR.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OnSaveButton d-oeitem 
PROCEDURE OnSaveButton :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ll-price-mod         AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-price             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll-pruom-mod         AS LOG       NO-UNDO.
    DEFINE VARIABLE lv-pruom             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-prev-req-date     AS DATE      NO-UNDO.
    DEFINE VARIABLE lv-stat              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ll                   AS LOG       NO-UNDO.
    DEFINE VARIABLE ld                   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ll-reopen            AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-runship           AS LOG       NO-UNDO.
    DEFINE VARIABLE lInvoiceFound        AS LOG       NO-UNDO.
    DEFINE VARIABLE v-job-rec_key        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-runsh              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-xfer-ord           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE createSetorder       AS LOG       NO-UNDO.
    DEFINE VARIABLE v-date-change-reason AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCalcDueDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE dCalcPromDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE cDueDateChgReason    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-added-rowid        AS ROWID     NO-UNDO.
    DEFINE VARIABLE lPMPrompt            AS LOGICAL.
    DEFINE VARIABLE cPMMessage           AS CHARACTER.
    DEFINE VARIABLE lPMBlock             AS LOGICAL.
    DEFINE VARIABLE lPricehold           AS LOGICAL.
    DEFINE VARIABLE cPriceHoldMessage    AS CHARACTER.
    DEFINE VARIABLE dTotalPrice          AS DECIMAL   NO-UNDO.

    DEFINE BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE BUFFER b-oe-ord  FOR oe-ord.


    DISABLE TRIGGERS FOR LOAD OF xoe-ord.

    IF ip-type EQ "WebUpdate" THEN 
    DO TRANSACTION WITH FRAME {&frame-name}:
        FIND CURRENT oe-ordl EXCLUSIVE-LOCK.
        ASSIGN oe-ordl.qty.
        FIND CURRENT oe-ordl NO-LOCK.
        APPLY "go" TO FRAME {&FRAME-NAME}.
        RETURN.
    END.

    /* display spec notes for the item */   
    DO WITH FRAME {&frame-name}:
        RUN windows/d-spnote.w (oe-ordl.i-no:SCREEN-VALUE).
    END.

    IF ip-type EQ "view" THEN 
    DO:
        APPLY "go" TO FRAME {&FRAME-NAME}.
        RETURN.
    END.

    RUN custom/framechk.p (2, FRAME {&FRAME-NAME}:HANDLE).

    ll-reopen = framechk-i-changed AND oe-ordl.stat EQ "C".

    /* gdm - 10220907 */
    IF TRIM(oe-ordl.pr-uom:SCREEN-VALUE) EQ "" THEN 
    DO:

        MESSAGE "UOM can't be blank. Please enter a valid UOM"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

        APPLY "entry" TO oe-ordl.pr-uom.
        RETURN.

    END.
    /* gdm - 10220907 end */

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            v-qty-mod       = oe-ordl.qty NE li-prev-ord-qty
            li-prev-qty     = oe-ordl.qty
            li-prev-ord-qty = oe-ordl.qty
            ll-price-mod    = oe-ordl.price:MODIFIED
            lv-price        = oe-ordl.price:SCREEN-VALUE
            ll-pruom-mod    = oe-ordl.pr-uom:MODIFIED
            lv-pruom        = oe-ordl.pr-uom:SCREEN-VALUE.
     
        IF oe-ordl.vend-no:SCREEN-VALUE = "0"  THEN
            ASSIGN oe-ordl.vend-no:SCREEN-VALUE = "".

    END.

    lv-prev-req-date = oe-ordl.req-date.
  
    RUN itemfg-cost.
  

    IF oe-ordl.est-no:SCREEN-VALUE <> "" THEN 
    DO:
        RUN check-quote-qty NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            APPLY 'entry' TO oe-ordl.qty.
            RETURN . 
        END.
    END.

    RUN validate-all NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF lOEPriceWarning AND
        (DECIMAL(oe-ordl.cost:SCREEN-VALUE) * decimal(oe-ordl.qty:SCREEN-VALUE) / 1000 ) GT DECIMAL(oe-ordl.t-price:SCREEN-VALUE) THEN
        MESSAGE "Warning: Sell Price is less than the cost." VIEW-AS ALERT-BOX WARNING .

    APPLY "go" TO FRAME {&FRAME-NAME}.
 
    IF runship-char EQ "RUN&SHIP Prompt" AND ip-type = "ADD" THEN 
    DO TRANSACTION:
        IF oe-ordl.est-no:SCREEN-VALUE GT ""  THEN 
            asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
        ELSE 
        DO:
            ll-runship = LOGICAL(asi.oe-ordl.whsed:SCREEN-VALUE).
  
            RUN oe/d-runsh.w (INPUT ll-runship, OUTPUT v-runsh).
        
            IF v-runsh = 1 THEN
                ASSIGN asi.oe-ordl.whsed:SCREEN-VALUE = "YES".
            IF v-runsh = 2 THEN 
            DO:
                ASSIGN 
                    oe-ordl.managed:SCREEN-VALUE = "YES" 
                    oe-ordl.managed              = YES.
            END.        
        END.
    END.
    ELSE
        IF (runship-char EQ "" OR  runship-char EQ "DefaultOnly" ) AND runship-log EQ YES AND oe-ordl.est-no:SCREEN-VALUE NE "" THEN
            ASSIGN asi.oe-ordl.whsed:SCREEN-VALUE = "YES".


    IF oe-ordl.est-no:SCREEN-VALUE EQ "" THEN
        ASSIGN 
            lPMPrompt = NO 
            lPMBlock  = NO.
      
    RUN Price_CheckPriceMatrix ( cocode, oe-ordl.i-no:SCREEN-VALUE,  oe-ord.cust-no, oe-ord.ship-id, DEC(oe-ordl.qty:SCREEN-VALUE),DEC(oe-ordl.price:SCREEN-VALUE),
        OUTPUT lPMPrompt, OUTPUT cPMMessage, OUTPUT lPMBlock).
    IF lPMPrompt THEN 
    DO: 
        MESSAGE cPMMessage VIEW-AS ALERT-BOX.
        IF lPMBlock THEN RETURN NO-APPLY.
    END.
    
    IF oepricecheck-log AND oe-ordl.est-no:SCREEN-VALUE EQ "" AND
        ll-new-record THEN
        RUN prev-quote-proc(INPUT-OUTPUT lv-price,
            INPUT-OUTPUT lv-pruom).

    DO WITH FRAME {&frame-name}:
        IF ll-price-mod THEN oe-ordl.price:SCREEN-VALUE = lv-price.
        IF ll-pruom-mod THEN oe-ordl.pr-uom:SCREEN-VALUE = lv-pruom.

        RUN Conv_CalcTotalPrice(cocode, 
            oe-ordl.i-no:SCREEN-VALUE,
            DECIMAL(oe-ordl.qty:SCREEN-VALUE),
            DECIMAL(oe-ordl.price:SCREEN-VALUE),
            oe-ordl.pr-uom:SCREEN-VALUE,
            DECIMAL(oe-ordl.disc:SCREEN-VALUE),
            DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
            OUTPUT dTotalPrice).
        oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
        //{oe/ordltot.i oe-ordl qty oe-ordl}
    END.

    IF ll-reopen THEN 
    DO:
        ll-reopen = NO.
        MESSAGE "This line item is closed, REOPEN?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE ll-reopen.
    END.

    SESSION:SET-WAIT-STATE ("general").

    DO TRANSACTION :
        
        FIND CURRENT oe-ordl EXCLUSIVE.

        IF ll-reopen THEN oe-ordl.stat = "".

        IF NOT ll-new-record THEN 
        DO:
            RUN oe/upinvqty.p (RECID(oe-ordl)).
        END.
    END. /* Transaction */
    DO TRANSACTION:

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}
                fi_jobStartDate.
            IF asi.oe-ordl.whsed:HIDDEN = FALSE THEN
                ASSIGN oe-ordl.whsed.
            IF STRING(DATE(oe-ordl.spare-int-2)) NE fi_jobStartDate:SCREEN-VALUE THEN 
            DO:
                ASSIGN 
                    oe-ordl.spare-int-2 = INT(DATE(fi_jobStartDate:SCREEN-VALUE)).
                RUN updateStartDate.
            END.

        END.

        RUN Price_CheckPriceHoldForOrder(ROWID(oe-ord),
            YES, /*Prompt*/
            YES, /*Set oe-ord hold fields*/
            OUTPUT lPriceHold, 
            OUTPUT cPriceHoldMessage).
                        
        FIND xoe-ord WHERE RECID(xoe-ord) = recid(oe-ord) EXCLUSIVE.
        FIND FIRST itemfg WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            ASSIGN 
                xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
                oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
                xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.

        /*IF TRIM(oe-ordl.est-no) NE "" AND
           TRIM(xoe-ord.est-no) EQ "" AND
           ll-new-record              THEN
          RUN fg/makenote.p (BUFFER oe-ordl, ?, itemfg.rec_key).*/
        END.
        FIND CURRENT xoe-ord NO-LOCK.

        IF oeDateChange-log 
            AND  NOT ll-new-record
            AND  LOOKUP("promise Date", oeDateChange-chr) GT 0
            AND  oe-ordl.prom-date NE ld-prev-prom-date 
            AND  ld-prev-prom-date NE ?
            AND  gcLastDateChange EQ "prom-date" THEN 
        DO:
    

            RUN oe/d-rsnnot.w /* PERSISTENT SET h_reasonWin */
                (INPUT oe-ordl.rec_key, INPUT "P", INPUT "", INPUT "", INPUT 0, INPUT "PDC", INPUT "",
                OUTPUT v-date-change-reason, OUTPUT v-added-rowid)  .

            IF v-date-change-reason GT "" THEN
                ASSIGN oe-ordl.spare-char-3 = v-date-change-reason               
                    oe-ordl.spare-char-4 = USERID("NOSWEAT").
   
        END.
  
        IF oeDateChange-log 
            AND  NOT ll-new-record
            AND  LOOKUP("Order Line Due Date", oeDateChange-chr) GT 0
            AND  oe-ordl.req-date NE dtPrevDueDate 
            AND  dtPrevDueDate NE ? 
            AND  gcLastDateChange EQ "req-date" THEN 
        DO:
    

            RUN oe/d-pdcnot.w /* PERSISTENT SET h_reasonWin */
                (INPUT oe-ordl.rec_key, INPUT "D", INPUT "", INPUT "", INPUT 0, INPUT "DDC", INPUT "",
                OUTPUT v-date-change-reason, OUTPUT v-added-rowid)  .

            IF v-date-change-reason GT "" THEN
                ASSIGN oe-ordl.spare-char-5 = USERID("NOSWEAT") + "," + v-date-change-reason.                              
   
        END.
  
        IF lv-change-prom-date THEN 
        DO:  
            FOR EACH xoe-ordl WHERE xoe-ordl.company EQ g_company
                AND xoe-ordl.ord-no EQ oe-ord.ord-no
                AND recid(xoe-ordl) NE recid(oe-ordl):
                ASSIGN 
                    xoe-ordl.prom-date = oe-ordl.prom-date.
            END.
        END.
                  
        IF lv-change-cst-po THEN 
        DO:  
            FOR EACH xoe-ordl WHERE xoe-ordl.company EQ g_company
                AND xoe-ordl.ord-no EQ oe-ord.ord-no
                AND recid(xoe-ordl) NE recid(oe-ordl):
                ASSIGN 
                    xoe-ordl.po-no = oe-ordl.po-no.
            END.
        END.
        IF lv-change-inv-po THEN 
        DO:    
            RUN oe/poNoChange.p (INPUT g_company,
                INPUT oe-ord.ord-no,
                INPUT oe-ordl.po-no,
                INPUT (IF lv-change-cst-po THEN "" ELSE oe-ordl.i-no)).
        END.
        RELEASE xoe-ordl.

        RUN update-itemfg.

        ASSIGN {&list-2} .  /* job-no job-no2 */

        FIND CURRENT oe-ordl NO-LOCK.
    END. /* trans */

    IF ip-type NE "update" AND oe-ordl.est-no NE "" THEN
        RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).
  

    IF oereleas-log THEN 
        IF ll-new-record THEN RUN create-release.
        ELSE RUN update-release.
        
  
    DO  TRANSACTION :
        FIND CURRENT oe-ordl EXCLUSIVE.
        FIND CURRENT oe-ord EXCLUSIVE.
        
        IF oeDateAuto-log AND OeDateAuto-Char EQ "Colonial" THEN 
        DO:             
            RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                INPUT oe-ordl.req-date,
                INPUT oe-ordl.prom-date,
                INPUT "DueDate",
                INPUT ROWID(oe-ordl),
                OUTPUT dCalcDueDate,
                OUTPUT dCalcPromDate).
            oe-ordl.prom-date = dCalcPromDate.
            
            IF NOT cDueManualChanged AND cPromManualChanged THEN 
            DO:
                RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                    INPUT oe-ordl.req-date,
                    INPUT oe-ordl.prom-date,
                    INPUT "PromiseDate",
                    INPUT ROWID(oe-ordl),
                    OUTPUT dCalcDueDate,
                    OUTPUT dCalcPromDate).
                oe-ordl.req-date = dCalcDueDate.
            END.
            ELSE
                IF NOT cPromManualChanged AND NOT cDueManualChanged THEN 
                DO:
                    RUN oe/dueDateCalc.p (INPUT oe-ord.cust-no,
                        INPUT oe-ordl.req-date,
                        INPUT oe-ordl.prom-date,
                        INPUT "DueDate",
                        INPUT ROWID(oe-ordl),
                        OUTPUT dCalcDueDate,
                        OUTPUT dCalcPromDate).
                    oe-ordl.prom-date = dCalcPromDate.
                END.
        END.
    
        RUN final-steps.
    END. /* Transaction */
    IF ll-new-record AND oe-ordl.s-man[1]:screen-value IN FRAME {&frame-name} = "" THEN 
    DO  TRANSACTION : 
        RUN itemfg-sman.
        ASSIGN oe-ordl.s-man[1].
    END.
  
    DO  TRANSACTION :
        IF ll-new-record AND (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Order processing") THEN 
        DO:
            RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
            xoe-ord.t-freight = xoe-ord.t-freight + oe-ordl.t-freight.
        END. /* ll-new-record */
      
        /* Update Item Cust Part if Required */
        IF v-oeCustPartInt EQ 1 THEN 
        DO:
            FIND FIRST itemfg 
                WHERE itemfg.company EQ cocode
                AND itemfg.i-no EQ oe-ordl.i-no 
                NO-LOCK NO-ERROR.
          
            IF avail(itemfg) AND itemfg.part-no NE oe-ordl.part-no:screen-value THEN 
            DO:
                FIND CURRENT itemfg EXCLUSIVE-LOCK.
                itemfg.part-no    = oe-ordl.part-no:screen-value.
                FIND CURRENT itemfg NO-LOCK.
            END. /* part # changed */
        END. /* oecustpartint = 1 */
        
        IF ll-new-record AND (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Order processing") THEN 
            RUN oe/ordfrate.p (ROWID(oe-ord)).  
    
        RUN oe/oe-comm.p.  
    
        RUN oe/calcordt.p (ROWID(oe-ord)).
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no NO-ERROR.
        IF (ld-prev-t-price NE oe-ordl.t-price OR ip-type BEGINS "update-")
            AND AVAILABLE cust AND NOT cust.internal AND AVAILABLE oe-ord AND oe-ord.TYPE NE "T" THEN 
        DO:
            RUN oe/creditck.p (ROWID(oe-ord), YES).  
        END.
    
        IF oe-ordl.job-no NE "" THEN
            RUN oe/palchk.p(ROWID(oe-ord), oe-ordl.i-no).
    
        ld-prev-t-price = oe-ordl.t-price.
    
        /* gdm - 11090905 */
        IF ip-type EQ "Update" AND
            v-ponoUp THEN 
        DO:
    
            IF lv-change-cst-po THEN
                FOR EACH job-hdr WHERE
                    job-hdr.company EQ oe-ordl.company AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2 AND
                    job-hdr.ord-no  EQ oe-ordl.ord-no:
                    ASSIGN 
                        job-hdr.po-no = oe-ordl.po-no.
                END.
         
            ELSE
                FOR EACH job-hdr WHERE
                    job-hdr.company EQ oe-ordl.company AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2 AND
                    job-hdr.ord-no EQ oe-ordl.ord-no AND
                    job-hdr.i-no EQ oe-ordl.i-no:
          
                    ASSIGN 
                        job-hdr.po-no = oe-ordl.po-no.
                END.
          
            RELEASE job-hdr.
        END.
        /* gdm - 11090905 end */
    
        IF ip-type EQ "Update" AND
            TRIM(oe-ordl.job-no) EQ "" AND
            TRIM(oe-ord.est-no) NE "" THEN
        DO:
            FIND FIRST job-hdr WHERE
                job-hdr.company EQ oe-ordl.company AND
                job-hdr.ord-no EQ oe-ordl.ord-no AND
                job-hdr.i-no EQ oe-ordl.i-no
                NO-LOCK NO-ERROR.
    
            IF AVAILABLE job-hdr THEN
            DO:
                ASSIGN
                    oe-ordl.job-no  = job-hdr.job-no
                    oe-ordl.job-no2 = job-hdr.job-no2.
    
                IF TRIM(oe-ord.job-no) EQ "" THEN
                    ASSIGN
                        oe-ord.job-no  = job-hdr.job-no
                        oe-ord.job-no2 = job-hdr.job-no2.
    
                RELEASE job-hdr.
            END.
        END.
    
        /* end of job update */
        FIND CURRENT oe-ord NO-LOCK.
        FIND CURRENT oe-ordl NO-LOCK.
    
        IF ll-new-record AND TRIM(v-duplicateFGDayClient) = "DuplicateFGDayClient" THEN 
        DO:
            RUN check-duplicateFGDayClient.
        END.

    END. /* Transaction */
  
    RUN sys/inc/ordlcomp.p (ROWID(oe-ordl)).
  
    RUN final-steps2.
 
    /* need to assign oe-ordl.est-type = eb.est-type  
       job */

    ASSIGN
        v-qty-mod         = NO
        lv-add-mode       = NO
        ll-new-fg-created = NO.

    DO WITH FRAME {&frame-name}:
        DISPLAY {&DISPLAYED-FIELDS}.
    END.
      
    DO TRANSACTION :
        FIND CURRENT oe-ordl EXCLUSIVE.
        FIND CURRENT oe-ord EXCLUSIVE.

        /* assign rec_key to oe-ord for notes */

        IF oe-ord.est-no <> "" THEN
        DO:
            /*if notes frozen from jc/jobnotes.p, don't update rec_key*/

            FIND FIRST job-hdr WHERE
                job-hdr.company EQ cocode AND
                job-hdr.job-no  EQ oe-ordl.job-no AND
                job-hdr.job-no2 EQ oe-ordl.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job-hdr THEN
            DO:
                FIND FIRST job WHERE
                    job.company EQ cocode AND
                    job.job EQ job-hdr.job AND
                    job.job-no EQ job-hdr.job-no AND
                    job.job-no2 EQ job-hdr.job-no2
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job THEN
                DO:
                    v-job-rec_key = job.rec_key.
                    RELEASE job.
                END.

                RELEASE job-hdr.
            END.

            IF oe-ordl.rec_key EQ "" OR
                (v-job-rec_key NE oe-ordl.rec_key) THEN
                oe-ordl.rec_key = est.rec_key.
        END.
    
  
        FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
            AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
            AND RECID(b-oe-ordl) <> RECID(oe-ordl) NO-LOCK NO-ERROR.
        IF NOT AVAILABLE b-oe-ordl AND oe-ordl.est-no <> "" THEN 
        DO:
            FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
            b-oe-ord.rec_key = oe-ordl.rec_key.
            RELEASE b-oe-ord.
        END.
        ELSE 
        DO:
            FIND FIRST b-oe-ordl WHERE  b-oe-ordl.company EQ oe-ordl.company
                AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
                AND RECID(b-oe-ordl) <> RECID(oe-ordl)
                AND b-oe-ordl.est-no <> oe-ordl.est-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE b-oe-ordl AND oe-ordl.est-no <> "" THEN 
            DO:
                FIND b-oe-ord WHERE RECID(b-oe-ord) = RECID(oe-ord) EXCLUSIVE.
                b-oe-ord.rec_key = oe-ordl.rec_key.
                RELEASE b-oe-ord.
            END.
        END.

        /* end of job update */
        FIND CURRENT oe-ord NO-LOCK.
        FIND CURRENT oe-ordl NO-LOCK.
    END. /* trans */



    IF (oe-ordl.req-date NE lv-prev-req-date OR ip-type EQ "ADD"
        /*OR ip-type = "UPdate-2" doen in v-ord.w order-from-est proc */)
        /* update job's start-date when req-date is changed */
        AND oe-ordl.est-no:SCREEN-VALUE NE "" /*AND lv-update-job-stdate */ 
        AND (v-run-schedule OR schedule-log)
        THEN RUN update-start-date.

    IF oe-ordl.job-no NE '' THEN RUN update-due-date.

  
    IF gcLastDateChange GT "" THEN 
    DO TRANSACTION:
        FIND CURRENT oe-ord.
        IF gcLastDateChange EQ "prom-date" THEN
            oe-ord.due-date = oe-ordl.req-date.
           
        IF gcLastDateChange EQ "req-date" THEN
            oe-ord.due-date = oe-ordl.prom-date.    
   
        FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    END.
    DO  TRANSACTION :
        FIND CURRENT oe-ord.    
        IF ll-new-record AND (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Order processing") THEN
            RUN oe/ordfrate.p (ROWID(oe-ord)). /* strange problem with freight */

        ll = NO.
        IF AVAILABLE oe-ord AND (oe-ord.due-date GT oe-ordl.req-date 
            OR oeDateAuto-log AND OeDateAuto-Char = "Colonial") THEN 
        DO:
            IF oeDateAuto-log AND OeDateAuto-Char = "Colonial" THEN 
                ll = YES.
            ELSE
                MESSAGE "Change order header due date to " + TRIM(STRING(oe-ordl.req-date)) "?"
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE ll.
        END.
        IF ll THEN oe-ord.due-date = oe-ordl.req-date.

        FIND CURRENT oe-ord NO-LOCK NO-ERROR.
    END.

    /* Done after oe-ord.due-date is updated */
    IF oeDateAuto-log AND OeDateAuto-Char EQ "Colonial" THEN 
    DO TRANSACTION:
   
        FOR EACH oe-rel 
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            NO-LOCK
            BY oe-rel.rel-date:
          
            IF LOOKUP(oe-rel.stat, 'A,C,P,Z' ) EQ 0 THEN 
            DO:
        
      
                FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ rowid(oe-rel) EXCLUSIVE-LOCK.
        
                bf-oe-rel.spare-char-4 = STRING(oe-ord.due-date) + ",,".

                bf-oe-rel.rel-date = get-colonial-rel-date(ROWID(bf-oe-rel)).
                FIND CURRENT bf-oe-rel NO-LOCK.
                RELEASE bf-oe-rel.
            END.
    
            /* Only consider first one */
            LEAVE.
        END. /* each oe-rel */
    END. 
    SESSION:SET-WAIT-STATE ("").

    IF oesetxfer-log AND (ip-type EQ "ADD" OR (oe-ord.est-no GT "" AND ip-type BEGINS "UPDATE-")) THEN 
    DO TRANSACTION:
        FIND itemfg WHERE itemfg.company = oe-ordl.company
            AND itemfg.i-no    = oe-ordl.i-no
            AND itemfg.isaset
            NO-LOCK NO-ERROR.    

        IF AVAILABLE itemfg AND itemfg.alloc = NO THEN
            RUN oe/d-oexfer.w (INPUT ROWID(oe-ord), OUTPUT v-xfer-ord).
        IF v-xfer-ord GT 0 THEN 
        DO  :
            FIND CURRENT oe-ordl EXCLUSIVE-LOCK.
            oe-ordl.spare-int-1 = v-xfer-ord.
            FIND CURRENT oe-ordl NO-LOCK.
        END.
    END.
  
    /*RUN oe/sman-upd.p (ROWID(oe-ordl)).*/  
    lInvoiceFound = FALSE.
    IF OESellPriceXfer-log AND  (ll-price-mod OR ll-pruom-mod) THEN 
    DO:
        FOR EACH oe-boll 
            WHERE oe-boll.company EQ oe-ordl.company
            AND oe-boll.ord-no EQ oe-ordl.ord-no
            AND oe-boll.i-no   EQ oe-ordl.i-no
            AND oe-boll.LINE   EQ oe-ordl.LINE
            NO-LOCK,
            EACH inv-head WHERE inv-head.company EQ oe-boll.company
            AND inv-head.bol-no EQ oe-boll.bol-no
            NO-LOCK,
         
            FIRST inv-line 
            WHERE inv-line.r-no   EQ inv-head.r-no 
            AND inv-line.ord-no  EQ oe-boll.ord-no 
            AND inv-line.b-no    EQ oe-boll.b-no
            AND inv-line.i-no    EQ oe-boll.i-no
            AND inv-line.line    EQ oe-boll.line
            AND inv-line.po-no   EQ oe-boll.po-no
            NO-LOCK .
            lInvoiceFound = TRUE.
            LEAVE.
        END.
      
        IF lInvoiceFound THEN 
        DO:
            MESSAGE "Unposted invoices were found for this order line." SKIP
                "Would you like to update the price on them?" VIEW-AS ALERT-BOX QUESTION
                BUTTON YES-NO UPDATE ll-ans AS LOG.
            IF  ll-ans THEN 
            DO:
                RUN updateInvoicePrice (INPUT ROWID(oe-ordl), INPUT lv-price, INPUT lv-pruom).


            END. /* if ll-ans */
        END. /* If invoice was found */
    END. /* If Price was modified */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddTag d-oeitem 
PROCEDURE pAddTag :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcSource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDesc AS CHARACTER NO-UNDO.
   
    DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
   
    DO WITH FRAME {&frame-name}:   
        RUN ClearTagsForGroup(
            INPUT oe-ordl.rec_key,
            INPUT ipcSource
            ).
        RUN AddTagInfoForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "oe-ordl",
            INPUT ipcDesc,
            INPUT "",
            INPUT ipcSource
            ). /*From TagProcs Super Proc*/ 
        RUN Tag_IsTagRecordAvailableForGroup(
            INPUT oe-ordl.rec_key,
            INPUT "oe-ordl",
            INPUT ipcSource,
            OUTPUT lAvailable
            ).
        IF lAvailable THEN  
        DO:
            IF ipcSource = "Over Percentage" THEN         
                btnTagsOverrn:SENSITIVE = TRUE.
            ELSE IF ipcSource = "Under Percentage" THEN 
                    btnTagsUnder:SENSITIVE = TRUE.
        END.
        ELSE 
        DO:
            IF ipcSource = "Over Percentage" THEN 
                btnTagsOverrn:SENSITIVE = FALSE.
            ELSE IF ipcSource = "Under Percentage" THEN 
                    btnTagsUnder:SENSITIVE = FALSE.
        END.
   
    END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pAddTagInfoForGroup d-oeitem 
PROCEDURE pAddTagInfoForGroup PRIVATE :
/*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lAvailable AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    DO WITH FRAME {&frame-name}:
    END.
    
    FIND FIRST bf-oe-ordl NO-LOCK 
        WHERE bf-oe-ordl.rec_key EQ ipcRecKey NO-ERROR .
    IF AVAILABLE bf-oe-ordl THEN
    DO:
        
        RUN ClearTagsForGroup(
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "Price-Source"
            ).
        RUN AddTagInfoForGroup(
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "oe-ordl",
            INPUT ipcMessage,
            INPUT "",
            INPUT "Price-Source"
            ). /*From TagProcs Super Proc*/ 
        RUN Tag_IsTagRecordAvailableForGroup(
            INPUT STRING(oe-ordl.ord-no) + STRING(oe-ordl.LINE),
            INPUT "oe-ordl",
            INPUT "Price-Source",
            OUTPUT lAvailable
            ).
        IF lAvailable THEN  
            btnTags:SENSITIVE = TRUE.
        ELSE 
            btnTags:SENSITIVE = FALSE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCrtPart d-oeitem 
PROCEDURE pCrtPart :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-rowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-error AS LOGICAL NO-UNDO.
    DEFINE BUFFER b-cust-part FOR cust-part .
    DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO .

    DO WITH FRAME {&FRAME-NAME}:
        cCustNo = IF AVAILABLE oe-ord THEN oe-ord.cust-no ELSE oe-ordl.cust-no .

        FIND FIRST b-cust-part NO-LOCK
            WHERE b-cust-part.company EQ cocode
            AND b-cust-part.i-no    EQ oe-ordl.i-no:SCREEN-VALUE
            AND b-cust-part.cust-no EQ cCustNo  NO-ERROR .

        IF  AVAILABLE b-cust-part THEN 
        DO:
            MESSAGE "Cust Part# - Customer# already exists for FG Item:" + oe-ordl.i-no:SCREEN-VALUE + " and Part#:" + b-cust-part.part-no 
                VIEW-AS ALERT-BOX ERROR .
            APPLY "entry" TO oe-ordl.part-no .
            op-error = YES .
            RETURN NO-APPLY .
        END.
             
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no EQ oe-ordl.i-no:SCREEN-VALUE NO-ERROR .

        IF AVAILABLE itemfg THEN 
        DO:
            CREATE cust-part .
            ASSIGN
                cust-part.company = cocode
                cust-part.i-no    = oe-ordl.i-no:SCREEN-VALUE
                cust-part.cust-no = IF AVAILABLE oe-ord THEN oe-ord.cust-no ELSE oe-ordl.cust-no
                cust-part.part-no = oe-ordl.part-no:SCREEN-VALUE .
            IF lFGForcedCommission THEN
                cust-part.forcedCommissionPercent = dFGForcedCommission .

            RELEASE cust-part .
            io-rowid = ROWID(itemfg).
        END.
        
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetMiscEst d-oeitem 
PROCEDURE pGetMiscEst :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ipo-Recid AS RECID NO-UNDO.
    DEFINE BUFFER bff-est FOR est .
    DEFINE BUFFER bff-eb  FOR eb.
   
    FIND FIRST bff-eb WHERE RECID(bff-eb) = ipo-Recid NO-LOCK NO-ERROR.
    IF AVAILABLE bff-eb THEN
        FIND FIRST bff-est WHERE bff-est.company = bff-eb.company
            AND bff-est.est-no = bff-eb.est-no NO-LOCK NO-ERROR.
                
    DO WITH FRAME {&FRAME-NAME}:                
        IF AVAILABLE bff-eb AND AVAILABLE bff-est AND bff-est.estimateTypeID EQ "MISC" THEN 
        DO:
         
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ bff-eb.company
                AND trim(eb.est-no) EQ trim(bff-eb.sourceEstimate)  /*trim(bff-eb.est-no)*/
                AND eb.stock-no EQ bff-eb.stock-no NO-ERROR. 
            IF AVAILABLE eb THEN 
            DO:
                ipo-Recid = RECID(eb) .                 
                oe-ordl.SourceEstimateID:SCREEN-VALUE = bff-eb.est-no .
                oe-ordl.est-no:SCREEN-VALUE = bff-eb.sourceEstimate.
            END.
        END.
    END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetOverUnderPct d-oeitem 
PROCEDURE pGetOverUnderPct :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCustNo AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipcShipID AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipiOrdNo  AS INTEGER   NO-UNDO .
    DEFINE VARIABLE dOverPer  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dUnderPer AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-shipto FOR shipto .
    DEFINE VARIABLE cTagDesc   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAvailable AS LOGICAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:                   
        RUN oe/GetOverUnderPct.p(g_company, 
            ipcCustNo,
            TRIM(ipcShipID),
            oe-ordl.i-no:SCREEN-VALUE,
            ipiOrdNo,
            OUTPUT dOverPer , OUTPUT dUnderPer,  OUTPUT cTagDesc  ) .
        oe-ordl.over-pct:SCREEN-VALUE = STRING(dOverPer).
        oe-ordl.Under-pct:SCREEN-VALUE = STRING(dUnderPer). 
        RUN pAddTag("Over Percentage", cTagDesc).
        RUN pAddTag("Under Percentage", cTagDesc).
        deAutoOver = dOverPer.
        deAutoUnder =  dUnderPer.                                                   
    END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetPartComm d-oeitem 
PROCEDURE pGetPartComm :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplCheckValue AS LOGICAL NO-UNDO .
    DEFINE BUFFER b-cust-part FOR cust-part .
    DEFINE VARIABLE cCustNo AS CHARACTER NO-UNDO .
    IF iplCheckValue THEN
        lCheckFgForceWarning = NO .

    DO WITH FRAME {&FRAME-NAME}:
        IF lFGForcedCommission AND NOT lCheckFgForceWarning THEN 
        DO:
            cCustNo = IF AVAILABLE oe-ord THEN oe-ord.cust-no ELSE oe-ordl.cust-no .
           
            FIND FIRST b-cust-part NO-LOCK
                WHERE b-cust-part.company EQ cocode
                AND b-cust-part.i-no    EQ oe-ordl.i-no:SCREEN-VALUE
                AND b-cust-part.cust-no EQ cCustNo
                AND b-cust-part.part-no EQ oe-ordl.part-no:SCREEN-VALUE NO-ERROR .
            IF AVAILABLE b-cust-part THEN 
            DO:
                IF b-cust-part.forcedCommissionPercent EQ 0  THEN 
                    MESSAGE "N-K-1 Setting = FGForceCommission = Yes, but there is no commission percentage set for this item."
                        "Defaulting to normal commission percentage" VIEW-AS ALERT-BOX WARNING .
                ELSE 
                DO:
                    oe-ordl.s-comm[1]:SCREEN-VALUE = STRING(b-cust-part.forcedCommissionPercent) .
                    IF oe-ordl.s-man[2]:SCREEN-VALUE NE "" THEN
                        oe-ordl.s-comm[2]:SCREEN-VALUE = STRING(b-cust-part.forcedCommissionPercent) .
                    IF oe-ordl.s-man[3]:SCREEN-VALUE NE "" THEN
                        oe-ordl.s-comm[3]:SCREEN-VALUE = STRING(b-cust-part.forcedCommissionPercent) .
                END.
                lCheckFgForceWarning = YES .
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetQuoteRec d-oeitem 
PROCEDURE pGetQuoteRec :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcEstNo      AS   CHARACTER.
    DEFINE INPUT        PARAMETER ipcPartNo    LIKE quoteit.part-no.
    DEFINE INPUT        PARAMETER ipcPartNo2   LIKE quoteit.part-no.
    DEFINE INPUT-OUTPUT PARAMETER iopPrice      LIKE oe-ordl.price.
    DEFINE INPUT-OUTPUT PARAMETER iopUom        LIKE oe-ordl.pr-uom.
    DEFINE OUTPUT       PARAMETER iopQ-no       LIKE quotehd.q-no.
    DEFINE INPUT-OUTPUT PARAMETER iop-qty       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lcChoice AS CHARACTER NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:
        j = 0.
        FOR EACH quotehd
            WHERE quotehd.company EQ cocode
            AND quotehd.loc     EQ locode
            AND quotehd.est-no  EQ ipcEstNo
            AND quotehd.quo-date LE TODAY 
            AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
            AND ((quotehd.effectiveDate LE TODAY AND quotehd.approved) OR NOT lQuotePriceMatrix) 
            USE-INDEX quote NO-LOCK,
            
            EACH quoteitm OF quotehd
            WHERE quoteitm.part-no  EQ ipcPartNo OR
            (quoteitm.part-no EQ ipcPartNo2 AND ipcPartNo2 NE "" )
            USE-INDEX q-line NO-LOCK,
            EACH quoteqty OF quoteitm
            USE-INDEX qt-qty NO-LOCK
            
            BY quotehd.q-no DESCENDING
            BY quoteqty.qty DESCENDING:

            j = J + 1 .
            IF J > 1 THEN LEAVE.
           
            ASSIGN
                iopPrice = quoteqty.price
                iopUom   = quoteqty.uom
                iopQ-no  = quoteqty.q-no .
        END.
  
        IF j GT 1 THEN
            RUN oe/d-quotedprices.w("",cocode,
                locode,
                ipcEstNo,
                oe-ordl.cust-no,
                oe-ordl.part-no:SCREEN-VALUE,
                oe-ordl.i-no:SCREEN-VALUE,
                INPUT-OUTPUT iopPrice,
                INPUT-OUTPUT iopUom,
                INPUT-OUTPUT iop-qty,
                INPUT-OUTPUT iopQ-no,
                OUTPUT lcChoice).  
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prev-quote-proc d-oeitem 
PROCEDURE prev-quote-proc :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER lv-price AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER lv-pruom AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lv-choice    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-price-dec LIKE oe-ordl.price NO-UNDO.
    DEFINE VARIABLE lxPrice      LIKE oe-ordl.price NO-UNDO.
    DEFINE VARIABLE lxUom        LIKE oe-ordl.pr-uom NO-UNDO.
    DEFINE VARIABLE lxQty        LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE lxQNo        LIKE quoteitm.q-no NO-UNDO.
    DEFINE VARIABLE lcChoice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTotalPrice  AS DECIMAL   NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        FOR EACH quotehd FIELDS(q-no) WHERE
            quotehd.company EQ cocode AND
            quotehd.cust-no EQ oe-ord.cust-no
            NO-LOCK,
            EACH quoteitm WHERE
            quoteitm.company EQ cocode AND
            quoteitm.loc     EQ locode AND
            quoteitm.q-no    EQ quotehd.q-no
            NO-LOCK
            BY quotehd.q-no DESCENDING:
      
            IF NOT(quoteitm.part-no EQ oe-ordl.part-no:SCREEN-VALUE OR
                (oe-ordl.i-no:SCREEN-VALUE NE "" AND
                quoteitm.part-no EQ oe-ordl.i-no:SCREEN-VALUE)) THEN
                NEXT.

            FIND FIRST quoteqty WHERE
                quoteqty.company = quoteitm.company AND
                quoteqty.loc = quoteitm.loc AND
                quoteqty.q-no = quoteitm.q-no AND
                quoteqty.line = quoteitm.LINE AND
                quoteqty.qty = INT(oe-ordl.qty:SCREEN-VALUE)
                NO-LOCK NO-ERROR.

            IF (AVAILABLE quoteqty AND
                NOT(quoteqty.price EQ DEC(oe-ordl.price:SCREEN-VALUE) AND
                quoteqty.uom EQ oe-ordl.pr-uom:SCREEN-VALUE)) 
                OR NOT AVAILABLE quoteqty 
                AND oe-ordl.sourceEstimateID:SCREEN-VALUE EQ ""
                THEN
            DO WITH FRAME {&FRAME-NAME}:
                RUN oe/d-quotedprices.w("",cocode,
                    locode,
                    oe-ordl.est-no:SCREEN-VALUE,
                    oe-ordl.cust-no,
                    oe-ordl.part-no:SCREEN-VALUE,
                    oe-ordl.i-no:SCREEN-VALUE,
                    INPUT-OUTPUT lxPrice,
                    INPUT-OUTPUT lxUom,
                    INPUT-OUTPUT lxQty,
                    INPUT-OUTPUT lxQno,
                    OUTPUT lcChoice).

                
                CASE lcChoice:
                    WHEN "PRICE" THEN 
                        DO:
                            ASSIGN 
                                oe-ordl.price:SCREEN-VALUE  = STRING(lxPrice,">>>,>>>,>99.99<<<")
                                oe-ordl.pr-uom:SCREEN-VALUE = lxUom                                      
                                lv-price                    = STRING(lxPrice,">>>,>>>,>99.99<<<")
                                lv-pruom                    = lxUom.
                            RUN pAddTagInfoForGroup(
                                INPUT oe-ordl.rec_key,
                                INPUT "Quoted Price Quote No:" + string(lxQno) + " Quantity: " + string(lxQty)
                                ).
                        END.
                    WHEN "PRICEQTY" THEN 
                        DO:
                            ASSIGN
                                oe-ordl.price:SCREEN-VALUE  = STRING(lxPrice,">>>,>>>,>99.99<<<")                
                                oe-ordl.pr-uom:SCREEN-VALUE = lxUom
                                oe-ordl.qty:SCREEN-VALUE    = STRING(lxQty,">>>,>>>,>>9")                                       
                                lv-price                    = STRING(lxPrice,">>>,>>>,>99.99<<<")
                                lv-pruom                    = lxUom.
                            
                        END.
                END CASE.
              


                /*                 RUN oe\d-lastquote.w(INPUT cocode,            */
                /*                                      INPUT locode,            */
                /*                                      INPUT quoteitm.q-no,     */
                /*                                      INPUT quoteitm.line,     */
                /*                                      INPUT-OUTPUT lv-pruom,   */
                /*                                      OUTPUT lv-price-dec,     */
                /*                                      OUTPUT lv-choice).       */
                /*                                                               */
                /*                 IF lv-choice = "OK" THEN                      */
                /*                 DO:                                           */
                /*                    ASSIGN                                     */
                /*                       lv-price = STRING(lv-price-dec)         */
                /*                       oe-ordl.price:SCREEN-VALUE = lv-price   */
                /*                       oe-ordl.pr-uom:SCREEN-VALUE = lv-pruom. */
                RUN Conv_CalcTotalPrice(cocode, 
                    oe-ordl.i-no:SCREEN-VALUE,
                    DECIMAL(oe-ordl.qty:SCREEN-VALUE),
                    DECIMAL(oe-ordl.price:SCREEN-VALUE),
                    oe-ordl.pr-uom:SCREEN-VALUE,
                    DECIMAL(oe-ordl.disc:SCREEN-VALUE),
                    DECIMAL(oe-ordl.cas-cnt:SCREEN-VALUE),    
                    OUTPUT dTotalPrice).
                oe-ordl.t-price:SCREEN-VALUE = STRING(dTotalPrice).
                    //{oe/ordltot.i oe-ordl qty oe-ordl} 
                
            END. /* Do with frame */
            
            LEAVE.
        END. /* For each quotehd */
    END. /* Do with frame */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetValidUOMList d-oeitem 
PROCEDURE pSetValidUOMList PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  Given company and get, set the global UOM list variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-itemfg FOR itemfg.

    IF ipcItemID NE "" THEN 
    DO:
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ ipcCompany
            AND bf-itemfg.i-no EQ ipcItemID
            NO-ERROR.

    END.
    IF AVAILABLE bf-itemfg THEN 
    DO: 
        RUN Conv_GetValidOrderQtyUOMsForItem(ROWID(bf-itemfg), OUTPUT cUOMListQty, OUTPUT lError, OUTPUT cMessage).
        RUN Conv_GetValidPriceUOMsForItem(ROWID(bf-itemfg), OUTPUT cUOMListPrice, OUTPUT lError, OUTPUT cMessage).
    END.
    ELSE 
    DO: 
        RUN Conv_GetValidOrderQtyUOMs(OUTPUT cUOMListQty).
        RUN Conv_GetValidPriceUOMs(OUTPUT cUOMListPrice).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetValidUOMTT d-oeitem 
PROCEDURE pSetValidUOMTT PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  Given company and get, set the global UOM list variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
 
    DEFINE BUFFER bf-itemfg FOR itemfg.

    IF ipcItemID NE "" THEN 
    DO:
        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ ipcCompany
            AND bf-itemfg.i-no EQ ipcItemID
            NO-ERROR.

    END.
    IF AVAILABLE bf-itemfg THEN 
    DO: 
        IF ipcType EQ "Qty" THEN 
            RUN Conv_GetValidOrderQtyUOMTTForItem(ROWID(bf-itemfg), OUTPUT lError, OUTPUT cMessage, OUTPUT TABLE ttUOMEffective).
        ELSE 
            RUN Conv_GetValidPriceUOMTTForItem(ROWID(bf-itemfg), OUTPUT lError, OUTPUT cMessage, OUTPUT TABLE ttUOMEffective).
    END.
    ELSE 
    DO: 
        IF ipcType EQ "Qty" THEN
            RUN Conv_GetValidOrderQtyUOMTT(OUTPUT TABLE ttUOMEffective).
        ELSE 
            RUN Conv_GetValidPriceUOMTT(OUTPUT TABLE ttUOMEffective).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pViewDetail d-oeitem 
PROCEDURE pViewDetail :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcShow AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dWidth AS DECIMAL NO-UNDO.

    DEFINE BUFFER bOEOrd  FOR oe-ord.
    DEFINE BUFFER bOEOrdl FOR oe-ordl.

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            cFGItem = oe-ordl.i-no:SCREEN-VALUE 
            dWidth  = SESSION:WIDTH - 1
            .
        IF dWidth GT 271 THEN dWidth = 271.
        IF dWidth LT 225 THEN dWidth = 225.
        ASSIGN
            FRAME {&FRAME-NAME}:WIDTH = dWidth.
        dWidth = dWidth - BROWSE browseLocations:COL - 1
            .
        ASSIGN
            BROWSE browseAllocated:WIDTH     = dWidth
            BROWSE browseAllocated:SENSITIVE = NO
            BROWSE browseAllocated:HIDDEN    = YES
            BROWSE browseJobs:WIDTH          = dWidth
            BROWSE browseJobs:SENSITIVE      = NO
            BROWSE browseJobs:HIDDEN         = YES
            BROWSE browseLocations:WIDTH     = dWidth
            BROWSE browseLocations:SENSITIVE = NO
            BROWSE browseLocations:HIDDEN    = YES
            BROWSE browsePOs:WIDTH           = dWidth
            BROWSE browsePOs:SENSITIVE       = NO
            BROWSE browsePOs:HIDDEN          = YES
            BROWSE browseReleases:WIDTH      = dWidth
            BROWSE browseReleases:SENSITIVE  = NO
            BROWSE browseReleases:HIDDEN     = YES
            iPrintAvailQty:SENSITIVE         = NO
            iPrintAvailQty:HIDDEN            = YES
            btnAllocated:HIDDEN              = NO
            btnAllocated:SENSITIVE           = YES
            btnJobs:HIDDEN                   = NO
            btnJobs:SENSITIVE                = YES
            btnLocations:HIDDEN              = NO
            btnLocations:SENSITIVE           = YES
            btnPOs:HIDDEN                    = NO
            btnPOs:SENSITIVE                 = YES
            btnReleases:HIDDEN               = NO
            btnReleases:SENSITIVE            = YES
            btnViewDetail:LABEL              = "Close Detail"
            .
        CASE ipcShow:
            WHEN "Allocated" THEN 
                DO:
                    ASSIGN
                        BROWSE browseAllocated:HIDDEN    = NO
                        BROWSE browseAllocated:SENSITIVE = YES
                        btnAllocated:SENSITIVE           = NO
                        .
                    EMPTY TEMP-TABLE ttAllocated.
                    FOR EACH bOEOrdl NO-LOCK
                        WHERE bOEOrdl.company EQ itemfg.company
                        AND bOEOrdl.i-no    EQ cFGItem
                        AND bOEOrdl.opened  EQ YES,
                        FIRST bOEOrd NO-LOCK
                        WHERE bOEOrd.company  EQ bOEOrdl.company
                        AND bOEOrd.ord-no   EQ bOEOrdl.ord-no
                        AND (bOEOrd.loc EQ w-jobs.loc
                        OR w-jobs.loc EQ "*ALL")
                        :
                        CREATE ttAllocated.
                        ASSIGN
                            ttAllocated.ord-no    = bOEOrdl.ord-no
                            ttAllocated.cust-no   = bOEOrdl.cust-no
                            ttAllocated.cust-name = bOEOrd.cust-name
                            ttAllocated.qty       = bOEOrdl.qty
                            ttAllocated.ship-qty  = bOEOrdl.ship-qty
                            ttAllocated.due-date  = bOEOrd.due-date
                            ttAllocated.price     = bOEOrdl.price
                            ttAllocated.pr-uom    = bOEOrdl.pr-uom
                            ttAllocated.part-no   = bOEOrdl.part-no
                            ttAllocated.po-no     = bOEOrdl.po-no
                            ttAllocated.allocated = bOEOrdl.qty - bOEOrdl.ship-qty
                            ttAllocated.loc       = bOEOrd.loc
                            .
                    END.
                    {&OPEN-QUERY-browseAllocated}
                END.
            WHEN "Jobs" THEN 
                DO:
                    ASSIGN
                        BROWSE browseJobs:HIDDEN    = NO
                        BROWSE browseJobs:SENSITIVE = YES
                        btnJobs:SENSITIVE           = NO
                        .
                    {&OPEN-QUERY-browseJobs}
                END.
            WHEN "Locations" THEN 
                DO:
                    ASSIGN
                        BROWSE browseLocations:HIDDEN    = NO
                        BROWSE browseLocations:SENSITIVE = YES
                        btnLocations:SENSITIVE           = NO
                        iPrintAvailQty:HIDDEN            = NO
                        iPrintAvailQty:SENSITIVE         = YES 
                        .
                    RUN build-table.
                    {&OPEN-QUERY-browseLocations}
                END.
            WHEN "POs" THEN 
                DO:
                    ASSIGN
                        BROWSE browsePOs:HIDDEN    = NO
                        BROWSE browsePOs:SENSITIVE = YES
                        btnPOs:SENSITIVE           = NO
                        .
                    {&OPEN-QUERY-browsePOs}
                END.
            WHEN "Releases" THEN 
                DO:
                    ASSIGN
                        BROWSE browseReleases:HIDDEN    = NO
                        BROWSE browseReleases:SENSITIVE = YES
                        btnReleases:SENSITIVE           = NO
                        .
                    {&OPEN-QUERY-browseReleases}
                END.
            OTHERWISE
            DO:
                ASSIGN
                    btnAllocated:SENSITIVE    = NO
                    btnAllocated:HIDDEN       = YES
                    btnJobs:SENSITIVE         = NO
                    btnJobs:HIDDEN            = YES
                    btnLocations:SENSITIVE    = NO
                    btnLocations:HIDDEN       = YES
                    btnPOs:SENSITIVE          = NO
                    btnPOs:HIDDEN             = YES
                    btnReleases:SENSITIVE     = NO
                    btnReleases:HIDDEN        = YES
                    iPrintAvailQty:SENSITIVE  = NO
                    iPrintAvailQty:HIDDEN     = YES
                    btnViewDetail:LABEL       = "View Detail"
                    FRAME {&FRAME-NAME}:WIDTH = 146
                    .            
            END.
        END CASE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setQtyPrice d-oeitem 
PROCEDURE setQtyPrice :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipOeordlRowid AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipPrice LIKE oe-ordl.price NO-UNDO.
    DEFINE INPUT PARAMETER ipPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
    DEFINE INPUT PARAMETER ipSetFromHistory AS LOGICAL NO-UNDO.

    ASSIGN
        historyQty     = ipQty
        historyPrice   = ipPrice
        historyPrUOM   = ipPrUOM
        setFromHistory = ipSetFromHistory.

    FIND tt-qty-price WHERE tt-qty-price.oeordl-ROWID EQ ipOeordlRowid
        NO-ERROR.
    IF NOT AVAILABLE tt-qty-price THEN 
    DO:
        CREATE tt-qty-price.
        ASSIGN 
            tt-qty-price.oeordl-rowid = ipOeordlRowid.
    END.
    ASSIGN
        tt-historyQty     = ipQty
        tt-historyPrice   = ipPrice
        tt-historyPrUOM   = ipPrUom
        tt-setFromHistory = ipSetFromHistory.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-new-tandem d-oeitem 
PROCEDURE upd-new-tandem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-eb      FOR eb.
    DEFINE BUFFER b-ef      FOR ef.
    DEFINE BUFFER b-est-qty FOR est-qty.
    DEFINE BUFFER b-Unit#   FOR reftable.

    DEFINE VARIABLE lv-master AS ROWID   NO-UNDO.
    DEFINE VARIABLE li        AS INTEGER NO-UNDO.

    
    FIND FIRST eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.

    IF AVAILABLE eb THEN 
    DO:
        ASSIGN
            eb.stock-no      = oe-ordl.i-no
            eb.part-no       = oe-ordl.part-no
            eb.part-dscr1    = oe-ordl.i-name
            eb.part-dscr2    = oe-ordl.part-dscr1
            eb.bl-qty        = oe-ordl.qty
            eb.yld-qty       = oe-ordl.qty
            oe-ordl.est-type = eb.est-type
            oe-ordl.form-no  = eb.form-no
            oe-ordl.blank-no = eb.blank-no
            v-qty-mod        = YES.

        /* Add to master estimate */
        IF INT(eb.master-est-no) NE 0 THEN 
        DO:
            FIND FIRST b-eb
                WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.master-est-no
                AND b-eb.part-no EQ eb.part-no
                NO-LOCK NO-ERROR.

            IF NOT AVAILABLE b-eb THEN 
            DO:
                ASSIGN
                    eb.i-col     = 0
                    eb.i-pass    = 0
                    eb.i-coat    = 0
                    eb.i-coat-p  = 0
                    eb.i-coldscr = ""
                    eb.i-code    = ""
                    eb.i-ps      = 0
                    eb.i-dscr    = ""
                    eb.i-%       = 0
                    eb.i-code2   = ""
                    eb.i-ps2     = 0
                    eb.i-dscr2   = ""
                    eb.i-%2      = 0.

                FOR EACH itemfg-ink
                    WHERE itemfg-ink.company EQ eb.company
                    AND itemfg-ink.i-no    EQ eb.stock-no:
                    DELETE itemfg-ink.
                END.

                FIND FIRST xest
                    WHERE xest.company EQ eb.company
                    AND xest.est-no  EQ eb.master-est-no
                    NO-LOCK NO-ERROR.

                IF AVAILABLE xest THEN 
                DO:
                    FIND FIRST ef OF eb NO-LOCK NO-ERROR.
                    RUN est/NewEstimateForm.p ('F', ROWID(xest), OUTPUT lv-master).

                    FIND b-eb WHERE ROWID(b-eb) EQ lv-master NO-ERROR.

                    IF AVAILABLE b-eb THEN 
                    DO:
                        FIND FIRST b-est-qty 
                            WHERE b-est-qty.company EQ b-eb.company
                            AND b-est-qty.est-no  EQ b-eb.est-no
                            NO-LOCK NO-ERROR.

                        BUFFER-COPY eb EXCEPT e-num form-no blank-no est-no rec_key TO b-eb
                            ASSIGN
                            b-eb.master-est-no = ""
                            b-eb.eqty          = b-est-qty.eqty.

                        FIND FIRST b-ef OF b-eb NO-ERROR.
                        IF AVAILABLE b-ef AND AVAILABLE ef THEN
                        DO:
                            BUFFER-COPY ef EXCEPT e-num form-no est-no rec_key TO b-ef
                                ASSIGN
                                b-ef.eqty = b-est-qty.eqty.

                            FIND CURRENT b-ef NO-LOCK.
                            RELEASE b-ef.
                        END.

                    /*RUN est/oeselest.p.*/
                    END.
                END.
            END.
        END.

        FIND xeb WHERE ROWID(xeb) EQ ROWID(eb) NO-LOCK NO-ERROR.
        RUN est/oeselest.p.
        RELEASE xeb.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE upd-tandem d-oeitem 
PROCEDURE upd-tandem :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    IF AVAILABLE oe-ordl THEN
        FOR EACH eb
            WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND eb.part-no  EQ oe-ordl.part-no
            AND eb.stock-no EQ oe-ordl.i-no
            BREAK BY eb.form-no  DESCENDING
            BY eb.blank-no DESCENDING:

            IF LAST(eb.form-no)                     OR
                (eb.form-no EQ oe-ordl.form-no AND
                eb.blank-no EQ oe-ordl.blank-no)    THEN 
            DO:
                ASSIGN
                    eb.part-dscr1 = oe-ordl.i-name
                    eb.part-dscr2 = oe-ordl.part-dscr1
                    eb.bl-qty     = oe-ordl.qty
                    eb.yld-qty    = oe-ordl.qty.
                LEAVE.
            END.
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-due-date d-oeitem 
PROCEDURE update-due-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bJobHdr FOR job-hdr.
    DEFINE BUFFER bJob    FOR job.
      
    DEFINE VARIABLE li AS INTEGER NO-UNDO.


    IF CAN-FIND(FIRST bJobHdr
        WHERE bJobHdr.company EQ oe-ordl.company
        AND bJobHdr.job-no  EQ oe-ordl.job-no
        AND bJobHdr.job-no2 EQ oe-ordl.job-no2
        AND bJobHdr.i-no    EQ oe-ordl.i-no) THEN
    DO WHILE li LT 1000:
        li = li + 1000.
        FIND FIRST bJobHdr EXCLUSIVE-LOCK
            WHERE bJobHdr.company EQ oe-ordl.company
            AND bJobHdr.job-no EQ oe-ordl.job-no
            AND bJobHdr.job-no2 EQ oe-ordl.job-no2
            AND bJobHdr.i-no EQ oe-ordl.i-no NO-WAIT NO-ERROR.
        IF AVAILABLE bJobHdr THEN 
        DO:
            bJobHdr.due-date = oe-ordl.req-date.
            FIND CURRENT bJobHdr NO-LOCK.
            FIND FIRST bJob OF bJobHdr EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE bJob THEN RETURN.
            bJob.due-date = bJobHdr.due-date.
            FIND CURRENT bJob NO-LOCK.
            RELEASE bJobHdr.
            li = 1000.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-itemfg d-oeitem 
PROCEDURE update-itemfg :
/*------------------------------------------------------------------------------
      Purpose:      /* === from oe/assifnfg.p  ===*/
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE v-flag    AS LOG EXTENT 10 NO-UNDO.
    DEFINE VARIABLE v-prompt  AS LOG NO-UNDO.
    DEFINE VARIABLE ls-flag   AS cha NO-UNDO.
    DEFINE VARIABLE lv-est-no LIKE itemfg.est-no NO-UNDO.

    DEFINE BUFFER b-oe-ordl       FOR oe-ordl.
    DEFINE BUFFER b-upd-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER b-eb2           FOR eb.
    DEFINE BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE VARIABLE v-cost-updated AS LOG NO-UNDO.

    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    DISABLE TRIGGERS FOR LOAD OF eb.


    FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OEFGUPDT"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN 
            sys-ctrl.company = cocode
            sys-ctrl.name    = "OEFGUPDT"
            sys-ctrl.descrip = "Update FG? Sell Price,UOM,Count,Name,Desc1,Desc2,Job,Vendor,Est.,Desc3"
            sys-ctrl.log-fld = NO.

        RUN oe/d-asgnfg.w  (RECID(sys-ctrl), oe-ordl.est-no, "New", OUTPUT ls-flag).       


    END. /* not avail sys-ctrl */

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ oe-ordl.i-no
        NO-LOCK NO-ERROR.
    
    IF AVAILABLE itemfg THEN 
    DO:    
        FIND oe-ord OF oe-ordl NO-LOCK.

        IF sys-ctrl.log-fld THEN 
            RUN oe/d-asgnfg.w (RECID(sys-ctrl), oe-ordl.est-no,"exist",OUTPUT ls-flag).

        ELSE 
        DO:
            FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ cocode
                AND sys-ctrl.name    EQ "OEFGUPDT"
                NO-LOCK NO-ERROR.
            ls-flag = sys-ctrl.char-fld.
            SUBSTRING(ls-flag,9,1) = STRING(sys-ctrl.int-fld EQ 1 AND oe-ordl.est-no NE "","Y/N").

        END. 
        ASSIGN 
            v-flag[1]  = SUBSTRING(ls-flag,1,1) = "Y"
            v-flag[2]  = SUBSTRING(ls-flag,2,1) = "Y"
            v-flag[3]  = SUBSTRING(ls-flag,3,1) = "Y"
            v-flag[4]  = SUBSTRING(ls-flag,4,1) = "Y"
            v-flag[5]  = SUBSTRING(ls-flag,5,1) = "Y"
            v-flag[6]  = SUBSTRING(ls-flag,6,1) = "Y"
            v-flag[7]  = SUBSTRING(ls-flag,7,1) = "Y"
            v-flag[8]  = SUBSTRING(ls-flag,8,1) = "Y"
            v-flag[9]  = SUBSTRING(ls-flag,9,1) = "Y"
            v-flag[10] = SUBSTRING(ls-flag,10,1) = "Y"
            .
   
        FIND CURRENT itemfg EXCLUSIVE-LOCK.
  
        IF v-flag[1] OR ll-new-fg-created THEN 
            itemfg.sell-price  = oe-ordl.price.
        IF v-flag[2] THEN itemfg.sell-uom    = oe-ordl.pr-uom. 
        IF v-flag[3] THEN 
            ASSIGN itemfg.case-count = oe-ordl.cas-cnt
                itemfg.case-pall  = oe-ordl.cases-unit.
        IF v-flag[4] THEN itemfg.i-name      = oe-ordl.i-name.
        IF v-flag[5] THEN itemfg.part-dscr1  = oe-ordl.part-dscr1.
        IF v-flag[6] THEN itemfg.part-dscr2  = oe-ordl.part-dscr2.
        IF v-flag[10] THEN itemfg.part-dscr3  = oe-ordl.part-dscr3.
        IF v-flag[7] THEN itemfg.cust-job-no = oe-ordl.job-no + "-" +
                string(oe-ordl.job-no2).
        IF v-flag[8] THEN itemfg.vend-no     = oe-ordl.vend-no.
  
        IF v-flag[9] THEN 
        DO:
            itemfg.est-no = oe-ordl.est-no.

            FOR EACH w-est-no:
                DELETE w-est-no.
            END.

            CREATE w-est-no.
            w-est-no = itemfg.est-no.

            DO WHILE AVAILABLE w-est-no:
                ASSIGN
                    w-run     = YES
                    lv-est-no = w-est-no.

                FOR EACH eb
                    WHERE eb.company   EQ oe-ordl.company
                    AND eb.est-no    EQ lv-est-no
                    AND eb.cust-no   EQ oe-ord.cust-no
                    AND ((eb.part-no EQ first-cust-part-no AND eb.stock-no EQ "") OR
                    eb.stock-no EQ oe-ordl.i-no)
                    AND TRIM(eb.master-est-no) NE ""
                    AND NOT CAN-FIND(FIRST w-est-no WHERE w-est-no EQ eb.master-est-no)
                    NO-LOCK:
         
                    CREATE w-est-no.
                    w-est-no = eb.master-est-no.
                END.

                FIND FIRST w-est-no WHERE w-run EQ NO NO-ERROR.
            END.

            FOR EACH w-est-no BREAK BY w-est-no:
                IF NOT FIRST-OF(w-est-no) THEN DELETE w-est-no.
            END.

            FOR EACH w-est-no,
                EACH eb 
                WHERE eb.company   EQ oe-ordl.company
                AND eb.est-no    EQ w-est-no
                AND eb.cust-no   EQ oe-ord.cust-no
                AND ((eb.part-no EQ first-cust-part-no AND eb.stock-no EQ "") OR
                eb.stock-no EQ oe-ordl.i-no):

                ASSIGN
                    eb.stock-no   = oe-ordl.i-no
                    eb.part-no    = oe-ordl.part-no
                    eb.part-dscr1 = oe-ordl.i-name
                    eb.part-dscr2 = oe-ordl.part-dscr1.

                /*IF oe-ordl.cas-cnt    NE 0 THEN eb.cas-cnt = oe-ordl.cas-cnt.
                IF oe-ordl.cases-unit NE 0 THEN eb.cas-pal = oe-ordl.cases-unit.
          
                eb.tr-cnt = eb.cas-cnt * eb.cas-pal.*/

                IF v-oecount-int EQ 1 THEN 
                DO:
                    IF v-oecount THEN
                        ASSIGN
                            eb.cas-cnt = oe-ordl.cas-cnt
                            eb.tr-cnt  = eb.cas-cnt * eb.cas-pal.

                    ELSE
                        ASSIGN
                            eb.tr-cnt  = oe-ordl.cas-cnt
                            eb.cas-cnt = TRUNC(eb.tr-cnt / (IF eb.cas-pal GT 0 THEN eb.cas-pal
                                                              ELSE 1),0).

                    eb.cas-wt = 0.
                END.
            END.
        END.

        IF ll-new-fg-created THEN itemfg.taxable = oe-ordl.tax.

        IF ll-new-record THEN itemfg.type-code = oe-ordl.type-code.  

        FIND FIRST cust-part WHERE
            cust-part.company EQ oe-ordl.company AND
            cust-part.cust-no EQ oe-ordl.cust-no AND
            cust-part.i-no EQ itemfg.i-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE cust-part THEN
        DO:
            IF v-oecustpart THEN
            DO:
                FOR EACH b-oe-ordl
                    WHERE b-oe-ordl.company EQ itemfg.company
                    AND b-oe-ordl.i-no    EQ itemfg.i-no
                    AND b-oe-ordl.opened  EQ YES
                    AND b-oe-ordl.cust-no EQ oe-ordl.cust-no
                    AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)
                    USE-INDEX item
                    NO-LOCK:
          
                    FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
                        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            
                    IF AVAILABLE b-upd-oe-ordl THEN 
                    DO:
                        b-upd-oe-ordl.part-no                      = oe-ordl.part-no.
                        /*
                        IF v-flag[3] THEN b-upd-oe-ordl.cas-cnt    = oe-ordl.cas-cnt.
                        */
                        IF v-flag[4] THEN b-upd-oe-ordl.i-name     = oe-ordl.i-name.
                        IF v-flag[5] THEN b-upd-oe-ordl.part-dscr1 = oe-ordl.part-dscr1.
                        IF v-flag[6] THEN b-upd-oe-ordl.part-dscr2 = oe-ordl.part-dscr2.
                        IF v-flag[10] THEN b-upd-oe-ordl.part-dscr3 = oe-ordl.part-dscr3.
                    END.
            
                    FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
                        NO-LOCK NO-ERROR.
                END.
       
                FIND CURRENT cust-part EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
       
                IF AVAILABLE cust-part THEN
                DO:
                    cust-part.part-no = oe-ordl.part-no.
                    FIND CURRENT cust-part NO-LOCK.
                END.

            END.
        END. /*avai cust-part*/


        /*farm out/purchased items*/
        IF oe-ordl.est-no NE "" AND itemfg.pur-man THEN
        DO:
            IF lNewVendorItemCost THEN 
            DO:
                IF NOT CAN-FIND(FIRST vendItemCost WHERE vendItemCost.company = itemfg.company AND vendItemCost.ItemID = itemfg.i-no)
                    THEN 
                DO:
                    CREATE vendItemCost.
                    ASSIGN 
                        vendItemCost.company = itemfg.company
                        vendItemCost.itemID  = itemfg.i-no
                        .
                END.

                FOR EACH b-eb2 FIELDS(company est-no form-no blank-no stock-no)
                    WHERE b-eb2.company EQ itemfg.company
                    AND b-eb2.est-no  EQ oe-ordl.est-no
                    AND b-eb2.part-no EQ oe-ordl.part-no
                    NO-LOCK,
                    EACH vendItemCost WHERE vendItemcost.company = b-eb2.company 
                    AND vendItemCost.estimateNo = b-eb2.est-no
                    AND vendItemCost.formNo = b-eb2.form-no 
                    AND vendItemCost.blankNo = b-eb2.blank-no
                    NO-LOCK:

                    v-cost-updated = NO.
              
                    FIND FIRST b-venditemcost WHERE b-venditemcost.company = b-eb2.company 
                        AND b-venditemcost.itemID = itemfg.i-no
                        AND b-venditemcost.estimateNo = ""
                        AND b-venditemcost.vendorID = vendItemCost.vendorID
                        AND b-vendItemCost.customerID = vendItemCost.customerID 
                        NO-ERROR. 
                                          
                    IF NOT AVAILABLE b-vendItemCost THEN
                    DO:
                        CREATE b-vendItemCost.
                        BUFFER-COPY vendItemCost EXCEPT venditemcostID itemID rec_key estimateNo formNo blankNo
                            TO b-vendItemCost
                            ASSIGN 
                            b-vendItemCost.estimateNo = ""
                            /*                            b-vend.eqty = 0*/
                            b-vendItemCost.formNo = 0
                            b-vendItemCost.blankNo = 0
                            b-vendItemCost.itemID = itemfg.i-no
                            v-cost-updated = YES.                 
                    END.
                    ELSE IF b-eb2.stock-no NE "" THEN  
                        DO: /*update costs*/
                            BUFFER-COPY vendItemCost EXCEPT venditemcostID itemID rec_key estimateNo formNo blankNo
                                TO b-vendItemCost.
                            ASSIGN 
                                v-cost-updated = YES.
                        END.
                END.    
            END.
            ELSE 
            DO:  
                IF NOT CAN-FIND(FIRST e-itemfg WHERE
                    e-itemfg.company = itemfg.company AND
                    e-itemfg.i-no = itemfg.i-no) THEN
                DO:
                    CREATE e-itemfg.
                    ASSIGN 
                        e-itemfg.company = itemfg.company
                        e-itemfg.i-no    = itemfg.i-no.
                    RELEASE e-itemfg.
                END.

                FOR EACH b-eb2 FIELDS(company est-no form-no blank-no stock-no)
                    WHERE b-eb2.company EQ itemfg.company
                    AND b-eb2.est-no  EQ oe-ordl.est-no
                    AND b-eb2.part-no EQ oe-ordl.part-no
                    NO-LOCK,
                    EACH e-itemfg-vend WHERE
                    e-itemfg-vend.company  EQ b-eb2.company AND
                    e-itemfg-vend.est-no   EQ b-eb2.est-no AND
                    e-itemfg-vend.form-no  EQ b-eb2.form-no AND
                    e-itemfg-vend.blank-no EQ b-eb2.blank-no
                    NO-LOCK:
              
                    IF e-itemfg-vend.i-no EQ "" THEN 
                    DO:
                        FIND CURRENT e-itemfg-vend EXCLUSIVE-LOCK NO-ERROR .
                        ASSIGN 
                            e-itemfg-vend.i-no = b-eb2.stock-no. 
                        FIND CURRENT e-itemfg-vend NO-LOCK NO-ERROR .    
                    END.

                    v-cost-updated = NO.

                    FIND FIRST b-e-itemfg-vend WHERE
                        b-e-itemfg-vend.company EQ b-eb2.company AND
                        b-e-itemfg-vend.i-no EQ itemfg.i-no AND
                        b-e-itemfg-vend.est-no EQ "" AND
                        b-e-itemfg-vend.vend-no EQ e-itemfg-vend.vend-no AND
                        b-e-itemfg-vend.cust-no EQ e-itemfg-vend.cust-no
                        NO-ERROR.

                    IF NOT AVAILABLE b-e-itemfg-vend THEN
                    DO:
                        CREATE b-e-itemfg-vend.
                        BUFFER-COPY e-itemfg-vend EXCEPT i-no rec_key est-no eqty form-no blank-no
                            TO b-e-itemfg-vend
                            ASSIGN 
                            b-e-itemfg-vend.est-no = ""
                            b-e-itemfg-vend.eqty = 0
                            b-e-itemfg-vend.form-no = 0
                            b-e-itemfg-vend.blank-no = 0
                            b-e-itemfg-vend.i-no = itemfg.i-no
                            v-cost-updated = YES.
                    END.
                    ELSE IF b-eb2.stock-no NE "" THEN /*update costs*/
                            BUFFER-COPY e-itemfg-vend EXCEPT i-no rec_key est-no eqty form-no blank-no
                                TO b-e-itemfg-vend
                                ASSIGN
                                v-cost-updated = YES.
              
                    IF v-cost-updated THEN
                    DO:

                        FIND FIRST e-itemfg WHERE
                            e-itemfg.company EQ e-itemfg-vend.company AND
                            e-itemfg.i-no EQ itemfg.i-no
                            NO-ERROR.
                
                        IF AVAILABLE e-itemfg THEN
                        DO:
                            e-itemfg.std-uom = e-itemfg-vend.std-uom.
                            RELEASE e-itemfg.
                        END.
                    END.


                    RELEASE b-e-itemfg-vend.
                END.
            END.    
        END.
    END. /* If avail itemfg */

    RELEASE itemfg.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-release d-oeitem 
PROCEDURE update-release :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-stat  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-ship  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE li-tries AS INTEGER   NO-UNDO.

    li = 0.
    li-ship = 0.
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        NO-LOCK:

        /*     li = li + oe-rel.qty. */
        li = li + oe-rel.tot-qty. /*sum of scheduled release total*/

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

        IF INDEX("PCZ",lv-stat) GT 0 THEN li-ship = li-ship + oe-rel.qty.
    END.
  
    FIND oe-rel 
        WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line
        AND oe-rel.link-no EQ 0
        NO-LOCK NO-ERROR.

    IF AVAILABLE oe-rel THEN 
    DO:

        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

        IF INDEX("SIL",lv-stat) GT 0 THEN 
            ll = YES. 

    END.


    /* update this here and not below where the records are locked */
    IF li NE oe-ordl.qty                                  AND
        ((oe-ordl.qty GT 0 AND li-ship LT oe-ordl.qty) OR
        (oe-ordl.qty LT 0 AND li-ship GT oe-ordl.qty))    THEN 
    DO:

        IF ll THEN 
        DO:
            ll = NO.
            MESSAGE "Update Release Quantity?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE ll.
        END. /* if ll */


    END. /* if li ne oe-ordl.qty */

    ASSIGN
        lv-stat  = ""
        li-tries = 0.

    DO WHILE TRUE:
        li-tries = li-tries + 1.
        IF li-tries GE 1000 THEN LEAVE.

        FIND oe-rel EXCLUSIVE
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.link-no EQ 0
        NO-WAIT NO-ERROR.

        IF AVAILABLE oe-rel THEN 
        DO:
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

            IF INDEX("SIL",lv-stat) GT 0 THEN 
            DO:
                /*ll = YES.*/

                FIND FIRST shipto WHERE shipto.company = oe-rel.company AND
                    shipto.cust-no = oe-rel.cust-no  AND
                    shipto.ship-id = oe-rel.ship-id
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN 
                DO:
                    ASSIGN 
                        oe-rel.ship-no      = shipto.ship-no
                        oe-rel.ship-id      = shipto.ship-id
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-addr[2] = shipto.ship-addr[2]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        oe-rel.ship-i[1]    = shipto.notes[1]
                        oe-rel.ship-i[2]    = shipto.notes[2]
                        oe-rel.ship-i[3]    = shipto.notes[3]
                        oe-rel.ship-i[4]    = shipto.notes[4].
                    RUN CopyShipNote (shipto.rec_key, oe-rel.rec_key).
          
                /* if add mode then use default carrier */
        
                END. /* if avail ship to */
            END. /* If status is S,I or L */

            IF li NE oe-ordl.qty                                  AND
                ((oe-ordl.qty GT 0 AND li-ship LT oe-ordl.qty) OR
                (oe-ordl.qty LT 0 AND li-ship GT oe-ordl.qty))    THEN 
            DO:

                IF ll THEN 
                DO:
        
                    ASSIGN
                        /*oe-rel.qty     = oe-ordl.qty - li-ship*/
                        oe-rel.tot-qty = oe-ordl.qty - li-ship.
                    RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).

                END.
     
                ELSE
                    MESSAGE "Order Qty differs from Release Quantites,"
                        "click Release Tab to Update..."
                        VIEW-AS ALERT-BOX.


            END. /* if quantity is changed */

            LEAVE.
        END. /* avail oe-rel */
    END. /* do while true */
    FIND CURRENT oe-rel NO-LOCK NO-ERROR.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-start-date d-oeitem 
PROCEDURE update-start-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  
    DEFINE VARIABLE lv-prom-date     AS DATE    NO-UNDO.
    DEFINE VARIABLE lv-day-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job2-time     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-prev-end-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-lap-time      AS INTEGER NO-UNDO.

    IF oe-ordl.job-no = ""  THEN RETURN.
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.

    DEFINE BUFFER bx-ordl FOR oe-ordl.
    DEFINE VARIABLE lv-first-due-date AS DATE NO-UNDO.
    lv-first-due-date = oe-ordl.req-date.

    FOR EACH bx-ordl FIELDS(req-date) WHERE
        bx-ordl.company = oe-ordl.company AND
        bx-ordl.job-no = oe-ordl.job-no AND
        bx-ordl.job-no2 = oe-ordl.job-no2 AND
        RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
        lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
        ELSE lv-first-due-date.
    END.

    DEFINE BUFFER bf-hdr  FOR job-hdr.
    DEFINE BUFFER bf-mch  FOR job-mch.

    DEFINE BUFFER bf2-hdr FOR job-hdr.
    DEFINE BUFFER bf2-mch FOR job-mch.

    DEFINE BUFFER bf-job  FOR job.
    DEFINE VARIABLE lv-start-date    AS DATE    NO-UNDO.
    DEFINE VARIABLE lv-m-time        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-run-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-mr-time       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-maccum-time   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-hr        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-day       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-wrk-st-time   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-chk-date      AS DATE    NO-UNDO.
    DEFINE VARIABLE li-num-of-wkend  AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-start-date-fr AS DATE    NO-UNDO.
    DEFINE VARIABLE lv-start-time    AS INTEGER NO-UNDO.

    /*===  calculate start date from due-date === */
    ASSIGN 
        lv-mr-time     = 0
        lv-run-time    = 0
        lv-job-time    = 0
        lv-maccum-time = 0.
    IF oe-ord.job-no GT "" THEN
        FIND FIRST job WHERE job.company EQ oe-ord.company
            AND job.job-no  EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ordl.job-no2
            NO-LOCK NO-ERROR.
    FOR EACH bf-hdr FIELDS(company job-no job-no2) WHERE bf-hdr.company = oe-ord.company
        AND bf-hdr.job-no = oe-ordl.job-no 
        AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK,
        EACH bf-mch FIELDS(mr-hr run-hr) WHERE bf-mch.company = bf-hdr.company
        AND bf-mch.job-no = bf-hdr.job-no
        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
        ASSIGN
            lv-mr-time  = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
            lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
            lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
    END.

    ASSIGN
        lv-job-hr  = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
                  ELSE TRUNCATE(lv-job-time / 3600,0)
        lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day /*- 1. */
     lv-update-job-stdate = NO.

    FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
    lv-prom-date = TODAY + lv-job-day.
    IF lv-start-date < TODAY /*AND (ip-type = "add")*/  /* ip-type = "Update-2" is from v-ord.w*/
        AND v-run-schedule AND schedule-log
        THEN 
    DO:
        MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
            "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
        lv-start-date = TODAY.
    END.
    ELSE IF lv-start-date < TODAY THEN lv-start-date = TODAY.

    IF v-run-schedule THEN 
    DO:

        /* === reset start-date === */
        ASSIGN 
            lv-mr-time      = 0
            lv-run-time     = 0
            lv-job-time     = 0
            lv-maccum-time  = 0
            li-num-of-wkend = 0
            lv-day-time     = 0
            lv-start-time   = 0.
  
        FOR EACH bf2-hdr NO-LOCK WHERE bf2-hdr.company = oe-ord.company
            AND bf2-hdr.job-no = oe-ordl.job-no
            AND bf2-hdr.job-no2 = oe-ordl.job-no2,
            EACH bf2-mch NO-LOCK WHERE bf2-mch.company = bf2-hdr.company
            AND bf2-mch.job-no = bf2-hdr.job-no
            AND bf2-mch.job-no2 = bf2-hdr.job-no2
            AND NOT bf2-mch.anchored
            BREAK BY bf2-mch.frm BY bf2-mch.blank-no BY bf2-mch.pass BY bf2-mch.m-code:

            REPEAT:
                FIND bf-hdr WHERE ROWID(bf-hdr) EQ ROWID(bf2-hdr) EXCLUSIVE NO-ERROR NO-WAIT.
                FIND bf-mch WHERE ROWID(bf-mch) EQ ROWID(bf2-mch) EXCLUSIVE NO-ERROR NO-WAIT.

                IF AVAILABLE bf-hdr AND AVAILABLE bf-mch THEN
                    LEAVE.
            END.

            FIND FIRST mach-calendar WHERE
                mach-calendar.company = job.company AND
                mach-calendar.m-code = bf-mch.m-code AND
                mach-calendar.m-date = lv-start-date
                NO-LOCK NO-ERROR.

            lv-m-time = IF AVAILABLE mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
            ELSE 28800. /* 8 HRs*/
            IF lv-m-time LT 0 THEN lv-m-time = 28800.
            lv-maccum-time = lv-maccum-time + lv-m-time.
            IF FIRST(bf2-mch.frm) THEN 
            DO:
                REPEAT:
                    FIND FIRST bf-job OF bf-hdr EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                    IF AVAILABLE bf-job THEN
                        LEAVE.
                END.

                ASSIGN
                    bf-job.start-date = lv-start-date
                    lv-wrk-st-time    = IF AVAILABLE mach-calendar THEN mach-calendar.start-time ELSE 0.
            END.
            IF FIRST-OF(bf2-mch.frm) THEN
                bf-hdr.start-date = job.start-date.

            ASSIGN
                lv-start-time        = lv-wrk-st-time
                lv-mr-time           = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                         ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
                lv-run-time          = IF bf-mch.run-hr = 0 THEN 0 ELSE
                           TRUNCATE(bf-mch.run-hr,0) * 3600 +
                          ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
                bf-mch.seq-no        = 0                 
                bf-mch.start-time-su = lv-wrk-st-time + lv-job-time + lv-day-time
                bf-mch.start-date-su = lv-start-date 
                lv-start-date-fr     = lv-start-date
                lv-job-time          = lv-job-time + lv-mr-time + lv-run-time
                lv-start-date        = lv-start-date + 
                             IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                             ELSE 0.
     
            IF lv-mr-time > lv-m-time THEN 
            DO:
                ASSIGN
                    lv-job2-time = lv-mr-time - lv-m-time
                    lv-lap-time  = bf-mch.start-time-su - lv-start-time.

                FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                    AND mach-calendar.m-code = bf-mch.m-code
                    AND mach-calendar.m-date = lv-start-date 
                    NO-LOCK NO-ERROR.
                lv-m-time = IF AVAILABLE mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                ELSE 28800. /* 8 HRs*/.
                IF lv-m-time LT 0 THEN
                    lv-m-time = 28800.

                ASSIGN
                    lv-start-time      = IF AVAILABLE mach-calendar THEN mach-calendar.start-time ELSE 0
                    bf-mch.end-time-su = lv-start-time + lv-job2-time + lv-lap-time
                    bf-mch.start-time  = lv-start-time + lv-job2-time  + lv-lap-time
                    bf-mch.end-date-su = lv-start-date
                    bf-mch.start-date  = lv-start-date 
                    lv-day-time        = lv-start-time - lv-prev-end-time + 86400.
            END.
        
            ELSE
                ASSIGN bf-mch.end-time-su = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                    bf-mch.start-time  = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                    bf-mch.end-date-su = lv-start-date
                    bf-mch.start-date  = lv-start-date 
                    lv-lap-time        = 0.

            ASSIGN
                lv-start-date = lv-start-date +
                             IF (lv-run-time) > lv-m-time THEN TRUNCATE((lv-run-time + lv-mr-time) / lv-m-time,0)
                             ELSE 0
             lv-start-date-fr = lv-start-date.

            IF (lv-run-time) > lv-m-time THEN 
            DO:
                ASSIGN
                    lv-job2-time = lv-run-time - lv-m-time
                    lv-lap-time  = bf-mch.start-time - lv-start-time.
                FIND FIRST mach-calendar WHERE
                    mach-calendar.company = job.company AND
                    mach-calendar.m-code = bf-mch.m-code AND
                    mach-calendar.m-date = lv-start-date 
                    NO-LOCK NO-ERROR.

                lv-m-time = IF AVAILABLE mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                ELSE 28800. /* 8 HRs*/
                IF lv-m-time LT 0 THEN
                    lv-m-time = 28800.

                ASSIGN
                    lv-start-time   = IF AVAILABLE mach-calendar THEN mach-calendar.start-time ELSE 0
                    bf-mch.end-time = lv-start-time + lv-job2-time             
                    bf-mch.end-date = lv-start-date
                    lv-day-time     = lv-day-time + lv-start-time - lv-prev-end-time + 86400.
            END.
            ELSE
                ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time             
                    bf-mch.end-date = lv-start-date
                    lv-lap-time     = 0.

            lv-prev-end-time = IF AVAILABLE mach-calendar THEN mach-calendar.end-time ELSE 86400. /* 24 HRs*/

            IF STRING(bf-mch.end-time,"hh:mm:ss") > string(lv-prev-end-time,"hh:mm:ss") THEN 
            DO:
                ASSIGN
                    lv-start-date = lv-start-date + 1
                    lv-lap-time   = bf-mch.end-time - lv-prev-end-time.

                FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                    AND mach-calendar.m-code = bf-mch.m-code
                    AND mach-calendar.m-date = lv-start-date 
                    NO-LOCK NO-ERROR.
                lv-m-time = IF AVAILABLE mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                ELSE 28800. /* 8 HRs*/.
                IF lv-m-time LT 0 THEN lv-m-time = 28800.
                ASSIGN
                    lv-start-time   = IF AVAILABLE mach-calendar THEN mach-calendar.start-time ELSE 0
                    bf-mch.end-time = lv-start-time + lv-lap-time             
                    bf-mch.end-date = lv-start-date
                    lv-day-time     = lv-day-time + lv-start-time - lv-prev-end-time + 86400.
            END.

            FIND CURRENT bf-hdr NO-LOCK NO-ERROR.
            FIND CURRENT bf-mch NO-LOCK NO-ERROR.
        
        END. /*end bf-hdr*/
    END. /* if v-run-schedule*/

    IF schedule-log THEN
        ASSIGN
            bx-ordl.prom-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.prom-date
            bx-ordl.req-date  = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateInvoicePrice d-oeitem 
PROCEDURE updateInvoicePrice :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeOrdl AS ROWID       NO-UNDO.
    DEFINE INPUT  PARAMETER ipdPrice  AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUom    AS CHARACTER   NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    FIND bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ iprOeOrdl
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN
        RETURN.

    FOR EACH oe-boll 
        WHERE oe-boll.company EQ bf-oe-ordl.company
        AND oe-boll.ord-no EQ bf-oe-ordl.ord-no
        AND oe-boll.i-no   EQ bf-oe-ordl.i-no
        AND oe-boll.LINE   EQ bf-oe-ordl.LINE
        NO-LOCK,
        EACH inv-head WHERE inv-head.company EQ oe-boll.company
        AND inv-head.bol-no EQ oe-boll.bol-no
        NO-LOCK,
   
        EACH inv-line 
        WHERE inv-line.r-no EQ inv-head.r-no 
        AND inv-line.ord-no EQ oe-boll.ord-no 
        AND inv-line.b-no EQ oe-boll.b-no
        AND inv-line.i-no   EQ oe-boll.i-no
        AND inv-line.line   EQ oe-boll.line
        AND inv-line.po-no  EQ oe-boll.po-no
        EXCLUSIVE-LOCK .
         
        ASSIGN 
            inv-line.price  = ipdPrice  
            inv-line.pr-uom = ipcUom.


        /* ##PN## Calculate related values on invoice line */
        FIND cust 
            WHERE cust.company EQ inv-line.company
            AND cust.cust-no EQ inv-line.cust-no
            NO-LOCK NO-ERROR.
        FIND itemfg 
            WHERE itemfg.company EQ inv-line.company
            AND itemfg.i-no    EQ inv-line.i-no
            NO-LOCK NO-ERROR.

        RUN Conv_CalcTotalPrice(inv-line.company, 
            inv-line.i-no,
            inv-line.inv-qty,
            inv-line.price,
            inv-line.pr-uom,
            inv-line.disc,
            inv-line.cas-cnt,    
            OUTPUT inv-line.t-price).
        /* This section should be merged with code from oe/oe-bolp7.i */
        /*inv-line.t-price = inv-line.inv-qty / 1000 * inv-line.price.                                            */
        /*                                                                                                        */
        /*IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN                                          */
        /*         inv-line.t-price = inv-line.price *                                                            */
        /*                           IF inv-line.inv-qty LT 0 THEN -1 ELSE IF inv-line.inv-qty EQ 0 THEN 0 ELSE 1.*/
        /*ELSE IF inv-line.pr-uom EQ "CS" THEN                                                                    */
        /*    inv-line.t-price = inv-line.inv-qty /                                                               */
        /*                           (IF inv-line.cas-cnt NE 0 THEN                                               */
        /*                             inv-line.cas-cnt                                                           */
        /*                            ELSE                                                                        */
        /*                            IF itemfg.case-count NE 0 THEN                                              */
        /*                              itemfg.case-count ELSE 1) *                                               */
        /*                           inv-line.price.                                                              */
        /*ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN                                                   */
        /*       inv-line.t-price = inv-line.inv-qty * inv-line.price.                                            */
        /*ELSE                                                                                                    */
        /*  FOR EACH uom                                                                                          */
        /*      WHERE uom.uom  EQ inv-line.pr-uom                                                                 */
        /*        AND uom.mult NE 0                                                                               */
        /*      NO-LOCK:                                                                                          */
        /*    inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.                                    */
        /*    LEAVE.                                                                                              */
        /*  END.                                                                                                  */
        /*inv-line.t-price = ROUND(inv-line.t-price,2).                                                           */
        /*                                                                                                        */
        /*IF inv-line.disc NE 0 THEN                                                                              */
        /*   inv-line.t-price =                                                                                   */
        /*        IF ll-calc-disc-first THEN                                                                      */
        /*          (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))                          */
        /*        ELSE                                                                                            */
        /*          ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).                                      */
        /* Note: inv-line.t-cost does not change when price changes */

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
            RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
                cust.cust-no,
                OUTPUT v-basis).

            IF v-basis EQ "G" THEN
                inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                    * inv-line.s-comm[i]) / 100,2).

            ELSE
                inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                    * inv-line.s-pct[i]) / 100)
                    * inv-line.s-comm[i]) / 100,2).        
      
        END.


        /* ##PN## Calulate Invoice Header Totals */
        RUN oe/oeinvup2.p (INPUT ROWID(inv-head), INPUT NO).


    /* ##PN## Calculate oe-ord invoice totals */
    /** Apparently, these are never calculated **/
    /* ASSIGN                     */
    /*  oe-ord.t-inv-tax     = 0  */
    /*  oe-ord.t-inv-freight = 0  */
    /*  oe-ord.t-inv-rev     = 0  */
    /*  oe-ord.t-inv-cost    = 0. */

    END. /* each oe-boll ... */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateStartDate d-oeitem 
PROCEDURE UpdateStartDate :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-job     FOR job.

    FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ oe-ord.company
        AND sys-ctrl.name EQ 'SCHEDULE' NO-ERROR.
    IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "NoDate" THEN
        /*update start dates of other orderlines for this orderline's job - e.g tandem/combo*/
        FOR EACH bf-oe-ordl WHERE bf-oe-ordl.company EQ oe-ordl.company
            AND bf-oe-ordl.job-no EQ oe-ordl.job-no
            AND bf-oe-ordl.job-no2 EQ oe-ordl.job-no2
            AND bf-oe-ordl.ord-no EQ oe-ordl.ord-no
            AND ROWID(bf-oe-ordl) NE ROWID(oe-ordl)
            EXCLUSIVE-LOCK:
            bf-oe-ordl.spare-int-2 = oe-ordl.spare-int-2.
        END.

    IF oe-ordl.vend-no EQ "0" THEN
        ASSIGN oe-ordl.vend-no = "".   /*task 03201407*/

/*update job start date*/
/* FIND FIRST bf-job WHERE bf-job.company EQ oe-ordl.company     */
/*     AND bf-job.job-no EQ oe-ordl.job-no                       */
/*     AND bf-job.job-no2 EQ oe-ordl.job-no2                     */
/*     NO-LOCK NO-ERROR.                                         */
/* IF AVAIL bf-job THEN                                          */
/*     RUN jc/UpdateSchedule.p (INPUT DATE(oe-ordl.spare-int-2), */
/*                              INPUT ROWID(bf-job)).            */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cas-cnt d-oeitem 
PROCEDURE valid-cas-cnt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcNoMsg AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        DEFINE VARIABLE lv-calc-qty AS DECIMAL   NO-UNDO.
        DEFINE VARIABLE lv-case-qty AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lv-uom      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE op-value    AS LOG       NO-UNDO .

        IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 AND fi_qty-uom:SCREEN-VALUE > "" THEN 
        DO:
            FIND FIRST itemfg WHERE itemfg.company = cocode AND itemfg.i-no EQ INPUT oe-ordl.i-no
                NO-LOCK NO-ERROR.
            lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE).
            IF AVAILABLE itemfg THEN
                ASSIGN
                    lv-uom      = fi_qty-uom:SCREEN-VALUE
                    lv-case-qty = (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                     IF lv-uom EQ "C"  THEN 100 ELSE 
                     IF lv-uom EQ "EA" THEN 1 ELSE 1000)
                    lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE).
            IF oe-ordl.pr-uom:SCREEN-VALUE NE "EA" AND avail(itemfg) THEN
                ASSIGN
                    lv-calc-qty = lv-calc-qty * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                     IF lv-uom EQ "C"  THEN 100 ELSE 
                     IF lv-uom EQ "EA" THEN 1 ELSE 1000).
            /*
            IF lv-calc-qty LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
               oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-calc-qty).
            */
            IF ipcNoMsg NE "NOMSG" AND int(oe-ordl.cas-cnt:screen-value) > lv-calc-qty THEN 
            DO:
                /* message "Unit count may not be greater than quantity." skip
                         "Setting unit count to equal quantity. "
                         view-as alert-box information. 
                 oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value.   
                APPLY "entry" TO oe-ordl.cas-cnt. */
                RUN oe/d-copyqty.w(OUTPUT op-value) .
                IF op-value EQ YES THEN 
                DO:
                    ASSIGN 
                        oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value. 
                /*  APPLY "entry" TO oe-ordl.cas-cnt.
                  RETURN NO-APPLY. */
                END.
                ELSE IF op-value EQ NO THEN 
                    DO:
                        APPLY "entry" TO oe-ordl.qty .
                        RETURN ERROR.
                    END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-case-cnt d-oeitem 
PROCEDURE valid-case-cnt :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcNoMsg AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE lv-calc-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-case-qty AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-uom      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE op-value    AS LOG       NO-UNDO .
    DO WITH FRAME {&FRAME-NAME}:

        IF INT(oe-ordl.qty:SCREEN-VALUE) GT 0 AND oe-ordl.pr-uom:SCREEN-VALUE > "" THEN 
        DO:
            FIND FIRST itemfg WHERE itemfg.company = cocode AND itemfg.i-no EQ INPUT oe-ordl.i-no
                NO-LOCK NO-ERROR.
            lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE).
            IF AVAILABLE itemfg THEN
                ASSIGN
                    lv-uom      = fi_qty-uom:SCREEN-VALUE
                    lv-case-qty = (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                       IF lv-uom EQ "C"  THEN 100 ELSE 
                       IF lv-uom EQ "EA" THEN 1 ELSE 1000)
                    lv-calc-qty = DEC(oe-ordl.qty:SCREEN-VALUE).
            IF oe-ordl.pr-uom:SCREEN-VALUE NE "EA" AND avail(itemfg) THEN
                ASSIGN
                    lv-calc-qty = lv-calc-qty * (IF lv-uom EQ "CS" OR lv-uom EQ "PLT" THEN itemfg.case-count ELSE
                       IF lv-uom EQ "C"  THEN 100 ELSE 
                       IF lv-uom EQ "EA" THEN 1 ELSE 1000).
            /*
            IF lv-calc-qty LT INT(oe-ordl.cas-cnt:SCREEN-VALUE) THEN
               oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-calc-qty).
            */
            IF INT(oe-ordl.cas-cnt:SCREEN-VALUE) EQ 0 AND oe-ordl.i-no:SCREEN-VALUE NE "0" THEN 
            DO:
                IF lv-calc-qty LE lv-case-qty THEN
                    oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-calc-qty).
                ELSE
                    oe-ordl.cas-cnt:SCREEN-VALUE = STRING(lv-case-qty).
            END.
            IF ipcNoMsg NE "NOMsg" AND int(oe-ordl.cas-cnt:screen-value) > lv-calc-qty THEN 
            DO:
                /* message "Unit count may not be greater than quantity." skip
                         "Setting unit count to equal quantity. "
                         view-as alert-box information. */

                RUN oe/d-copyqty.w (OUTPUT op-value) .
                IF op-value EQ YES THEN 
                DO:
                    oe-ordl.cas-cnt:screen-value = oe-ordl.qty:screen-value. 
                    APPLY "entry" TO oe-ordl.cas-cnt.
                    RETURN NO-APPLY.
                END.
                IF op-value EQ NO THEN 
                DO:
                    APPLY "entry" TO oe-ordl.qty.
                    RETURN NO-APPLY.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date-change d-oeitem 
PROCEDURE valid-date-change :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
    /* Taking this out for now */
    /*     DEF VAR v-reject-code AS CHAR NO-UNDO.                    */
    /*     v-reject-code = oe-ordl.spare-char-3:SCREEN-VALUE.        */
    /*                                                               */
    /*    FIND FIRST rejct-cd                                        */
    /*        WHERE rejct-cd.TYPE = "R"                              */
    /*          AND rejct-cd.CODE = v-reject-code                    */
    /*        NO-LOCK NO-ERROR.                                      */
    /*                                                               */
    /*                                                               */
    /*     IF NOT AVAIL rejct-cd AND v-reject-code GT "" THEN DO:    */
    /*       MESSAGE "Invalid " + TRIM(oe-ordl.spare-char-3:LABEL) + */
    /*               ", try help..." VIEW-AS ALERT-BOX.              */
    /*       APPLY "entry" TO oe-ordl.spare-char-3.                  */
    /*       RETURN ERROR.                                           */
    /*     END.                                                      */
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no d-oeitem 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-msg   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lActive AS LOG       NO-UNDO.

    DEFINE BUFFER bf-ordl FOR oe-ordl.

      
    DO WITH FRAME {&FRAME-NAME}:
        v-msg = "".

        IF v-msg EQ "" THEN
            IF oe-ordl.i-no:SCREEN-VALUE EQ "0" THEN v-msg = "may not be 0".

        IF v-msg EQ "" THEN
            IF oe-ordl.i-no:SCREEN-VALUE EQ "" THEN v-msg = "may not be blank".

        /*     IF v-msg EQ "" THEN                                                            */
        /*       IF CAN-FIND(FIRST bf-ordl WHERE bf-ordl.company EQ oe-ordl.company           */
        /*                                   AND bf-ordl.ord-no  EQ oe-ordl.ord-no            */
        /*                                   AND bf-ordl.i-no    EQ oe-ordl.i-no:SCREEN-VALUE */
        /*                                    AND NOT bf-ordl.is-a-component                  */
        /*                                   AND ROWID(bf-ordl)  NE ROWID(oe-ordl))           */
        /*       THEN v-msg = "has already been entered on this order".                       */

        IF v-msg EQ "" THEN
            IF oe-ordl.est-no:SCREEN-VALUE NE "" THEN
                FOR EACH eb
                    WHERE eb.company   EQ oe-ordl.company
                    AND eb.est-no    EQ oe-ordl.est-no:SCREEN-VALUE
                    AND (eb.est-type EQ 2 OR eb.est-type EQ 6)
                    AND eb.form-no   NE 0
                    NO-LOCK BREAK BY eb.est-no:
                    IF (NOT FIRST(eb.est-no) OR NOT LAST(eb.est-no)) AND
                        (eb.stock-no EQ oe-ordl.i-no:SCREEN-VALUE OR
                        eb.part-no  EQ oe-ordl.i-no:SCREEN-VALUE)    THEN
                        v-msg = "is a component on this estimate".
                END.

        /* task: 05150314 */
        IF v-msg EQ "" THEN
            IF oe-ordl.i-no:SCREEN-VALUE NE "" THEN 
            DO:
                RUN fg/GetItemfgActInact.p (INPUT g_company,
                    INPUT oe-ordl.i-no:SCREEN-VALUE,
                    OUTPUT lActive).
                /*         FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"                */
                /*                               AND reftable.company  EQ g_company                 */
                /*                               AND reftable.loc      EQ ""                        */
                /*                               AND reftable.code     EQ oe-ordl.i-no:SCREEN-VALUE */
                /*                             NO-LOCK NO-ERROR.                                    */
                /*         IF AVAIL reftable AND reftable.code2 = "I" THEN                          */
                IF NOT lActive THEN
                    v-msg = oe-ordl.i-no:SCREEN-VALUE + " has InActive Status. Order cannot be placed for the Inactive Item.".      
            END.

        IF v-msg NE "" THEN 
        DO:
            MESSAGE TRIM(oe-ordl.i-no:LABEL) +
                " " + TRIM(v-msg) + ", please re-enter or try help..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.i-no.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-part-no d-oeitem 
PROCEDURE valid-part-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordl.part-no:SCREEN-VALUE EQ "" THEN
            ASSIGN oe-ordl.part-no:SCREEN-VALUE = oe-ordl.i-no:SCREEN-VALUE .

        RUN sys/inc/valpart#.p (oe-ordl.part-no:SCREEN-VALUE,
            oe-ordl.i-no:SCREEN-VALUE) NO-ERROR.

        IF ERROR-STATUS:ERROR THEN 
        DO:
            APPLY "entry" TO oe-ordl.part-no.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-po-no d-oeitem 
PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER cust-po-mand FOR reftable.

  
    DO WITH FRAME {&FRAME-NAME}:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ oe-ord.company
            AND cust.cust-no EQ oe-ord.cust-no
            AND cust.po-mandatory
            NO-ERROR.
    
        IF AVAILABLE cust AND TRIM(oe-ordl.po-no:SCREEN-VALUE) EQ "" THEN 
        DO:
            MESSAGE "PO# is mandatory for this Customer..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.po-no.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-qty d-oeitem 
PROCEDURE valid-qty :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        IF DEC(ip-focus:SCREEN-VALUE) EQ 0 THEN 
        DO:
            MESSAGE TRIM(ip-focus:LABEL) + " may not be 0, please try again..."
                VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.qty.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-man d-oeitem 
PROCEDURE valid-s-man :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE li       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-sman  LIKE sman.sman NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.


    li = ip-int.

    IF li EQ 0 THEN
        ASSIGN
            ip-int = 1
            li     = 3.

    DO ip-int = ip-int TO li WITH FRAME {&FRAME-NAME}:
        lv-sman = IF ip-int EQ 3 THEN oe-ordl.s-man[3]:SCREEN-VALUE
        ELSE
            IF ip-int EQ 2 THEN oe-ordl.s-man[2]:SCREEN-VALUE
            ELSE oe-ordl.s-man[1]:SCREEN-VALUE.
    
        IF lv-sman NE "" THEN 
        DO:
            RUN SalesMan_ValidateSalesRep IN hdSalesManProcs(  
                INPUT  cocode,
                INPUT  lv-sman,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ).
            IF NOT lSuccess THEN 
            DO:    
                /*  MESSAGE cMessage 
                      VIEW-AS ALERT-BOX ERROR.*/
                IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordl.s-man[3].
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordl.s-man[2].
                    ELSE APPLY "entry" TO oe-ordl.s-man[1].
                RETURN ERROR.
            END.
        END.

        ELSE 
        DO:
            IF ip-int EQ 3 THEN
                ASSIGN
                    oe-ordl.s-pct[3]:SCREEN-VALUE  = "0"
                    oe-ordl.s-comm[3]:SCREEN-VALUE = "0".
            ELSE
                IF ip-int EQ 2 THEN
                    ASSIGN
                        oe-ordl.s-pct[2]:SCREEN-VALUE  = "0"
                        oe-ordl.s-comm[2]:SCREEN-VALUE = "0".
                ELSE
                    ASSIGN
                        oe-ordl.s-pct[1]:SCREEN-VALUE  = "0"
                        oe-ordl.s-comm[1]:SCREEN-VALUE = "0"
                        v-margin                       = 0.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-s-pct d-oeitem 
PROCEDURE valid-s-pct :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-int AS INTEGER NO-UNDO.

    DEFINE VARIABLE ld-pct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE ll     AS LOG     NO-UNDO.

   
    DO WITH FRAME {&FRAME-NAME}:
        ld-pct = IF ip-int EQ 1 THEN DEC(oe-ordl.s-pct[1]:SCREEN-VALUE)
        ELSE
            IF ip-int EQ 2 THEN DEC(oe-ordl.s-pct[2]:SCREEN-VALUE)
            ELSE
            IF ip-int EQ 3 THEN DEC(oe-ordl.s-pct[3]:SCREEN-VALUE)
            ELSE (DEC(oe-ordl.s-pct[1]:SCREEN-VALUE) +
            DEC(oe-ordl.s-pct[2]:SCREEN-VALUE) +
            DEC(oe-ordl.s-pct[3]:SCREEN-VALUE)).

        IF (oe-ordl.s-man[1]:SCREEN-VALUE NE "" OR
            oe-ordl.s-man[2]:SCREEN-VALUE NE "" OR
            oe-ordl.s-man[3]:SCREEN-VALUE NE "")   AND
            ( /* (ip-int EQ 0 AND ld-pct NE 100) OR */
            (ip-int NE 0 AND ld-pct GT 100)) THEN 
        DO:

            IF ip-int EQ 0 THEN
                /*  Per Joe, total does not need to equal 100%
          MESSAGE "Item's Sales Rep Commission % of Sales does not equal 100%, continue?" SKIP(1)
                  "(Please Note: Yes will result in inaccurate totals on some Sales History Reports)"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
              */
                .
            ELSE 
                MESSAGE "Sales Rep Commission % of Sales is over 100%..."
                    VIEW-AS ALERT-BOX ERROR.
            IF NOT ll THEN 
            DO:
                IF ip-int EQ 3 THEN APPLY "entry" TO oe-ordl.s-pct[3].
                ELSE
                    IF ip-int EQ 2 THEN APPLY "entry" TO oe-ordl.s-pct[2].
                    ELSE APPLY "entry" TO oe-ordl.s-pct[1].
                RETURN ERROR.
            END.
      
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-start-date d-oeitem 
PROCEDURE valid-start-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-prom-date AS DATE NO-UNDO.

    IF oe-ordl.job-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""  THEN RETURN.
 
    DEFINE BUFFER bx-ordl FOR oe-ordl.
    DEFINE VARIABLE lv-first-due-date AS DATE NO-UNDO.

    lv-first-due-date = DATE(oe-ordl.req-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    FOR EACH bx-ordl FIELDS(req-date) WHERE
        bx-ordl.company = oe-ordl.company AND
        bx-ordl.job-no = oe-ordl.job-no AND
        bx-ordl.job-no2 = oe-ordl.job-no2 AND
        RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
        lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
        ELSE lv-first-due-date.
    END.

    IF lv-first-due-date = ? THEN 
        lv-first-due-date = DATE(oe-ordl.req-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}).

    DEFINE BUFFER bf-hdr FOR job-hdr.
    DEFINE BUFFER bf-mch FOR job-mch.
    DEFINE BUFFER bf-job FOR job.
    DEFINE VARIABLE lv-start-date  AS DATE    NO-UNDO.
    DEFINE VARIABLE lv-m-time      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-run-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-mr-time     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-time    AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-maccum-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-hr      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-job-day     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-wrk-st-time AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-chk-date    AS DATE    NO-UNDO.
    DEFINE VARIABLE li-num-wkend   AS INTEGER NO-UNDO.

    /*===  calculate start date from due-date === */
    ASSIGN 
        lv-mr-time     = 0
        lv-run-time    = 0
        lv-job-time    = 0
        lv-maccum-time = 0.

    FOR EACH bf-hdr FIELDS(company job-no job-no2) WHERE
        bf-hdr.company = oe-ord.company AND
        bf-hdr.job-no = oe-ordl.job-no AND
        bf-hdr.job-no2 = oe-ordl.job-no2
        NO-LOCK,
        EACH bf-mch FIELDS(mr-hr run-hr) WHERE
        bf-mch.company = bf-hdr.company AND
        bf-mch.job-no = bf-hdr.job-no AND
        bf-mch.job-no2 = bf-hdr.job-no2
        NO-LOCK:

        ASSIGN
            lv-mr-time  = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60
            lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          TRUNCATE(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60
            lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
    END.
  
    ASSIGN
        lv-job-hr  = IF lv-job-time MOD 3600 > 0 THEN TRUNCATE(lv-job-time / 3600,0) + 1
                 ELSE TRUNCATE(lv-job-time / 3600,0)
        lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
                  ELSE TRUNCATE(lv-job-hr / 8,0)
     lv-start-date = lv-first-due-date - lv-job-day
     lv-prom-date = TODAY + lv-job-day
     lv-update-job-stdate = NO.

    IF lv-start-date < TODAY AND schedule-log THEN 
    DO:
     
        MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
            "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
            VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.
        RETURN.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type d-oeitem 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF LOOKUP(oe-ordl.type-code:SCREEN-VALUE,lv-type-codes) LE 0 OR
            (oe-ordl.type-code:SCREEN-VALUE EQ "T" AND
            NOT CAN-FIND(FIRST cust WHERE cust.company EQ cocode
            AND cust.cust-no EQ oe-ord.cust-no
            AND cust.internal EQ YES)) THEN 
        DO:
            MESSAGE "Invalid Type, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.type-code.
            RETURN ERROR.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom d-oeitem 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

    DEFINE VARIABLE lv-uom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld     AS DECIMAL   NO-UNDO.
    

    IF ip-focus:SENSITIVE THEN
    DO WITH FRAME {&FRAME-NAME}:
    
        RUN get-valid-uom (ip-focus, oe-ordl.i-no:SCREEN-VALUE).

        IF ip-focus:SCREEN-VALUE EQ "" THEN
            ip-focus:SCREEN-VALUE = IF ip-focus:NAME EQ "fi_qty-uom" THEN "EA"
            ELSE "M".
        ip-focus:SCREEN-VALUE = CAPS(TRIM(ip-focus:SCREEN-VALUE)).

        lv-uom = ip-focus:SCREEN-VALUE.

        IF NOT CAN-DO(lv-valid-uom, lv-uom) THEN 
        DO:
            MESSAGE "UOM is invalid, try help..."
                VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END.
        IF ip-focus:NAME EQ "fi_qty-uom" THEN 
        DO:
        
            ASSIGN
                ld                       = fOEScreenUOMConvert(DEC(oe-ordl.qty:SCREEN-VALUE), lv-uom, DEC(oe-ordl.cas-cnt:SCREEN-VALUE), oe-ordl.i-no:SCREEN-VALUE)
                oe-ordl.qty:SCREEN-VALUE = STRING(ld)
                ip-focus:SCREEN-VALUE    = "EA".

            RUN leave-qty.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no d-oeitem 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DO WITH FRAME {&FRAME-NAME}:
        IF oe-ordl.vend-no:SCREEN-VALUE EQ "0" THEN 
            oe-ordl.vend-no:SCREEN-VALUE = "" .
        IF oe-ordl.vend-no:SCREEN-VALUE NE "" THEN 
        DO:
            FIND FIRST vend
                WHERE vend.company EQ oe-ordl.company
                AND vend.vend-no EQ oe-ordl.vend-no:SCREEN-VALUE
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE vend                                                       OR
                (vend.active NE "A" AND
                (oe-ordl.vend-no NE oe-ordl.vend-no:SCREEN-VALUE OR ll-new-record)) THEN 
            DO:
                IF AVAILABLE vend THEN
                    MESSAGE TRIM(oe-ordl.vend-no:LABEL) + " not active, try help..."
                        VIEW-AS ALERT-BOX ERROR.
                ELSE 
                    MESSAGE "Invalid " + TRIM(oe-ordl.vend-no:LABEL) + ", try help..."
                        VIEW-AS ALERT-BOX ERROR.
                RETURN ERROR.
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-all d-oeitem 
PROCEDURE validate-all :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ls-i-no    AS cha       NO-UNDO.
    DEFINE VARIABLE ls-part-no AS cha       NO-UNDO.
    DEFINE VARIABLE ls-est-no  AS cha       NO-UNDO.
    DEFINE VARIABLE ls-uom     AS cha       NO-UNDO.
    DEFINE VARIABLE ll-secure  AS LOG       NO-UNDO.
    DEFINE VARIABLE cLoc       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLocBin    AS CHARACTER NO-UNDO.
 
    /*DEF VAR v-run-schedule AS LOG NO-UNDO.
   
    find first sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "SCHEDULE" no-lock no-error.
    v-run-schedule = NOT (AVAIL sys-ctrl AND sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld).
    */
    IF NOT AVAILABLE oe-ord THEN
        FIND oe-ord NO-LOCK WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ oe-ordl.ord-no
            NO-ERROR.
    IF AVAILABLE oe-ord THEN 
        FIND FIRST shipto NO-LOCK 
            WHERE shipto.company EQ oe-ord.company
            AND shipto.cust-no EQ oe-ord.cust-no
            AND shipto.ship-id EQ oe-ord.ship-id
            NO-ERROR.
    IF AVAILABLE shipto THEN 
        ASSIGN 
            cLoc    = shipto.loc
            cLocBin = shipto.loc-bin
            . 
    DO WITH FRAME {&frame-name}:
        IF v-est-fg1 = "Hold" AND oe-ordl.est-no:SCREEN-VALUE <> "" THEN 
        DO:
            FIND FIRST eb WHERE eb.company = cocode AND
                int(eb.est-no) = int(oe-ordl.est-no:SCREEN-VALUE) AND
                eb.stock-no = ""                            
                NO-LOCK NO-ERROR.
            IF AVAILABLE eb THEN 
            DO:
                MESSAGE "Sorry, FG item does not exist. Order has not been approved."
                    VIEW-AS ALERT-BOX ERROR.
                APPLY "ENTRY" TO OE-ORDL.EST-NO.
                RETURN ERROR.
            END.
        END.
    
        RUN validate-start-date.
        IF NOT ll-valid THEN RETURN ERROR.

        RUN valid-qty (oe-ordl.qty:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-uom (fi_qty-uom:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-cas-cnt (INPUT "MSG") NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        /*     Out for now                                 */
        /*     RUN valid-date-change NO-ERROR.             */
        /*     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. */

        ls-est-no = oe-ordl.est-no:screen-value.

        IF NOT ll-qty-leave-done THEN RUN getCostFromEstimate (ls-est-no).

        RUN valid-part-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        /*RUN check-quote (oe-ordl.part-no:SCREEN-VALUE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.*/

        RUN valid-i-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        IF oe-ordl.i-no:screen-value <> "0" /*and oe-ordl.est-no:screen-value = "" */ 
            AND NOT ll-ok-i-no
            THEN 
        DO:
            RUN display-fgitem NO-ERROR.  

            IF ERROR-STATUS:ERROR THEN 
                IF CAN-FIND(FIRST itemfg
                    WHERE itemfg.company EQ g_company
                    AND itemfg.i-no    EQ oe-ordl.i-no:SCREEN-VALUE) THEN RETURN ERROR.
                ELSE 
                DO:
                    ASSIGN 
                        ls-i-no    = oe-ordl.i-no:screen-value
                        ls-part-no = oe-ordl.part-no:SCREEN-VALUE
                        ls-uom     = oe-ordl.pr-uom:screen-value.
                    /* need to check security */
                    IF oe-ord.est-no = "" AND oe-ordl.est-no:SCREEN-VALUE = "" THEN 
                    DO:
                        RUN sys/ref/d-passwd.w (4, OUTPUT ll-secure).
                        IF NOT ll-secure THEN RETURN ERROR.
                    END.

                    /* This takes them back to i-no where they can enter a new item # */
                    IF oefgadd-log AND ls-est-no GT "" THEN
                        RUN oe/d-citmfg.w (ls-est-no, INPUT-OUTPUT ls-i-no,
                            INPUT-OUTPUT ls-part-no,INPUT-OUTPUT ls-uom, INPUT-OUTPUT cLoc, INPUT-OUTPUT cLocBin) NO-ERROR.
                    ELSE
                        IF ls-est-no EQ "" THEN 
                        DO:
                            MESSAGE "Please enter a valid item number." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                            APPLY 'entry' TO oe-ordl.i-no.
                            RETURN ERROR.
                        END.

                    IF ls-i-no = "" THEN 
                    DO:
                        APPLY "entry" TO oe-ordl.i-no.
                        RETURN ERROR.  /* cancel */
                    END.
                    ELSE 
                    DO:   
                        ASSIGN 
                            oe-ordl.i-no:screen-value    = ls-i-no
                            oe-ordl.part-no:screen-value = ls-part-no.
                        FIND FIRST xest WHERE xest.company = g_company 
                            AND xest.est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no:SCREEN-VALUE))) +
                            TRIM(oe-ordl.est-no:SCREEN-VALUE)
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE xest THEN 
                        DO: 
                            FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                                AND xeb.form-no = 0 NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE xeb THEN FIND FIRST xeb WHERE xeb.company = g_company AND xeb.est-no = xest.est-no
                                    AND xeb.form-no = oe-ordl.form-no
                                    AND xeb.blank-no = oe-ordl.blank-no
                                    NO-LOCK NO-ERROR.
                            IF NOT AVAILABLE xeb THEN
                                FIND FIRST xeb
                                    WHERE xeb.company EQ g_company
                                    AND xeb.est-no  EQ xest.est-no
                                    AND xeb.part-no EQ ls-part-no
                                    NO-LOCK NO-ERROR.
                            IF AVAILABLE xeb THEN 
                            DO:
                                FIND xef WHERE xef.company = g_company AND xef.est-no = xeb.est-no
                                    AND xef.form-no = xeb.form-no
                                    NO-LOCK NO-ERROR.

                                RUN crt-itemfg (ls-i-no, oe-ordl.pr-uom:SCREEN-VALUE, cLoc, cLocBin). /*(ls-i-no, "M")*/                   
                            END.    
                        END.   
                        ELSE /* no xest or oe-ordl.est-no = "" */
                            RUN crt-itemfg (ls-i-no, oe-ordl.pr-uom:SCREEN-VALUE, cLoc, cLocBin).
             
                    END.  /* ls-i-no */ 

                    RUN display-fgitem NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
                END.   
        END.   /* else */

        RUN valid-uom (oe-ordl.pr-uom:HANDLE) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        IF oe-ordl.req-date:modified AND date(oe-ordl.req-date:screen-value) < oe-ord.ord-date THEN 
        DO:
            MESSAGE "Due Date cannot be earlier than order date..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.req-date.
            RETURN ERROR.
        END.
        IF oe-ordl.prom-date:modified AND date(oe-ordl.prom-date:screen-value) < oe-ord.ord-date THEN 
        DO:
            MESSAGE "Scheduled Date cannot be earlier than order date..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO oe-ordl.prom-date.
            RETURN ERROR.
        END.
        IF oe-ordl.req-code:SCREEN-VALUE NE "" AND 
            LOOKUP(oe-ordl.req-code:SCREEN-VALUE ,v-duelist) EQ 0 THEN 
        DO:
            MESSAGE "Invalid Priority Code. " VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY":U TO oe-ordl.req-code.
            RETURN ERROR.
        END.
        IF oe-ordl.prom-code:SCREEN-VALUE NE "" AND 
            LOOKUP(oe-ordl.prom-code:SCREEN-VALUE ,v-duelist) EQ 0 THEN 
        DO:
            MESSAGE "Invalid Priority Code. " VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY":U TO oe-ordl.prom-code.
            RETURN ERROR.
        END. 
    
        RUN valid-po-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-vend-no NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-s-man (0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-s-pct (0) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN valid-type NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.

        RUN validate-due-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
    
    /*
    IF (oe-ordl.req-date <> date(oe-ordl.req-date:SCREEN-VALUE) 
       AND ip-type = "update")
       AND oe-ordl.est-no:SCREEN-VALUE <> "" 
       AND (v-run-schedule OR schedule-log)
    THEN  DO:
        /*
        RUN valid-start-date NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR.
        */
    END.*/
        
    END.   
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-due-date d-oeitem 
PROCEDURE validate-due-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE ldtDueDate AS DATE    NO-UNDO.
    DEFINE VARIABLE lValid     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue  AS LOGICAL NO-UNDO.
    DO WITH FRAME {&frame-name} :
        ldtDueDate = DATE(oe-ordl.req-date:SCREEN-VALUE). 
        RUN oe/dateFuture.p (INPUT cocode, INPUT ldtDueDate, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).
        IF NOT lValid AND  NOT lContinue THEN 
        DO:
            APPLY "entry" TO oe-ordl.req-date.
            RETURN ERROR.
        END. 
    END.
/* need more work
  
DEF BUFFER bf-job-mch FOR job-mch.
DEF BUFFER bf-tmp-jmch FOR job-mch.
DEF VAR lv-seq AS INT NO-UNDO.
DEF var lv-seq-anchored AS LOG NO-UNDO.
DEF VAR lv-date-wkst AS DATE NO-UNDO.
DEF VAR lv-date-wkend AS DATE NO-UNDO.
DEF VAR lv-time-wkst AS INT NO-UNDO.
DEF VAR lv-time-wkend AS INT NO-UNDO.
DEF VAR lv-start-time AS INT NO-UNDO.
DEF VAR lv-mr-time AS INT NO-UNDO.
DEF VAR lv-run-time AS INT NO-UNDO.
DEF VAR lv-mcode AS cha NO-UNDO.
DEF VAR i AS INT NO-UNDO.



/*===== reschedule job-mch for previous date =======*/
FOR EACH bf-job-mch WHERE bf-job-mch.company = oe-ordl.company
                    AND bf-job-mch.job-no = oe-ordl.job-no 
                    AND NOT bf-job-mch.job-no2 = oe-ordl.job-no2
             BREAK BY bf-job-mch.start-date 
                   BY bf-job-mch.seq-no:

   ASSIGN lv-mr-time = lv-mr-time + bf-job-mch.mr-hr
          lv-run-time = lv-run-time + bf-job-mch.run-hr.
/*

  IF FIRST-OF(bf-job-mch.start-date) THEN DO:
       FIND FIRST mach-calendar WHERE mach-calendar.company = mach.company
                    AND mach-calendar.m-code = mach.m-code
                    AND mach-calendar.m-date = lv-prev-st-date
                    NO-LOCK NO-ERROR.
       lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.

       FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                                   /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                                   AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                                   AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                                   AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                                   AND bf-tmp-jmch.seq-no < lv-old-seq 
                                   NO-LOCK BY bf-tmp-jmch.start-time DESC:
           lv-start-time = bf-tmp-jmch.end-time.
           LEAVE.
       END.
    END.
      
    lv-mr-time = IF bf-job-mch.mr-hr = 0 THEN 0 ELSE
                   truncate(bf-job-mch.mr-hr,0) * 3600 +
                           ((bf-job-mch.mr-hr - truncate(bf-job-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
    lv-run-time = IF bf-job-mch.run-hr = 0 THEN 0 ELSE
              truncate(bf-job-mch.run-hr,0) * 3600 +
            ((bf-job-mch.run-hr - truncate(bf-job-mch.run-hr,0)) * 100 * 60 / 100) * 60.

    lv-seq-anchored = YES.
    DO WHILE NOT lv-seq-anchored:
       FIND FIRST bf-tmp-jmch WHERE bf-tmp-jmch.company = bf-job-mch.company
                             /* AND bf-tmp-jmch.job-no = bf-job-mch.job-no
                              AND bf-tmp-jmch.job-no2 =  bf-job-mch.job-no2*/
                              AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                              AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                              AND bf-tmp-jmch.seq-no = lv-seq 
                              AND bf-tmp-jmch.anchored NO-LOCK NO-ERROR. 
       IF AVAIL bf-tmp-jmch THEN lv-seq = lv-seq + 10.
       ELSE lv-seq-anchored = NO.
    END.
       
    ASSIGN bf-job-mch.seq-no = lv-seq 
           bf-job-mch.start-time-su = lv-start-time 
           bf-job-mch.start-time = lv-start-time + lv-mr-time
           .

    ASSIGN lv-start-time = lv-start-time + lv-mr-time + lv-run-time.


    ASSIGN  bf-job-mch.start-date-su = bf-job-mch.start-date
            bf-job-mch.end-time = lv-start-time
            bf-job-mch.end-time-su = lv-start-time - lv-run-time
            bf-job-mch.end-date = bf-job-mch.start-date + 
                                  IF bf-job-mch.end-time < bf-job-mch.start-time THEN 1 ELSE 0
            bf-job-mch.end-date-su = bf-job-mch.start-date-su +
                                  IF bf-job-mch.end-time-su < bf-job-mch.start-time-su THEN 1 ELSE 0
            .
    lv-seq = lv-seq + 10.

/*      MESSAGE bf-job-mch.seq-no 
            string(bf-job-mch.start-time-su,"hh:mm")
            string(bf-job-mch.end-time-su,"hh:mm")
            string(lv-mr-time,"hh:mm")
            string(lv-run-time,"hh:mm")
            string(bf-job-mch.start-time,"hh:mm")
            string(bf-job-mch.end-time,"hh:mm")
            VIEW-AS ALERT-BOX.
*/
END.

/* reset seq, time for the job */
FOR each bf-tmp-jmch WHERE bf-tmp-jmch.company = job-mch.company
                       AND bf-tmp-jmch.job = job-mch.job
                       AND bf-tmp-jmch.job-no =  job-mch.job-no
                       AND bf-tmp-jmch.job-no2 =  job-mch.job-no2
                      /* AND bf-tmp-jmch.m-code = bf-job-mch.m-code
                       AND bf-tmp-jmch.start-date = bf-job-mch.start-date
                       AND bf-tmp-jmch.seq-no = lv-seq 
                       AND bf-tmp-jmch.anchored*/
                       AND bf-tmp-jmch.frm >= job-mch.frm
                       AND bf-tmp-jmch.blank-no >= job-mch.blank-no
                       AND bf-tmp-jmch.m-code > job-mch.m-code 
                       :
    ASSIGN bf-tmp-jmch.start-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time-su
           bf-tmp-jmch.end-time-su = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time-su
           bf-tmp-jmch.start-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.start-time
           bf-tmp-jmch.end-time = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.end-time           
           bf-tmp-jmch.seq-no = IF tt-sch.start-date <> bf-tmp-jmch.start-date THEN 0 ELSE bf-tmp-jmch.seq-no
           bf-tmp-jmch.start-date = tt-sch.start-date.
    FIND FIRST job-hdr WHERE job-hdr.company = bf-tmp-jmch.company
                         AND job-hdr.job = bf-tmp-jmch.job
                         AND job-hdr.job-no = bf-tmp-jmch.job-no
                         AND job-hdr.job-no2 = bf-tmp-jmch.job-no2.
                           
    job-hdr.start-date = tt-sch.start-date.
    FIND FIRST job OF bf-tmp-jmch.
    job.start-date = tt-sch.start-date.
END.
*/
END.

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-fgitem d-oeitem 
PROCEDURE validate-fgitem :
/*------------------------------------------------------------------------------
      Purpose:     from oe/oe-ordlu.i
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-est-no AS cha NO-UNDO.
    DEFINE BUFFER xeb FOR eb.

    {oe/oe-sysct2.i}
    IF NOT AVAILABLE oe-ord THEN 
    DO:
        FIND oe-ord WHERE oe-ord.company = cocode AND
            oe-ord.ord-no = oe-ordl.ord-no NO-LOCK NO-ERROR.
    END.

    DO WITH FRAME {&frame-name} :
        IF oe-ordl.est-no:screen-value NE "" THEN 
        DO:
            v-est-no = INPUT oe-ordl.est-no.
            RUN util/rjust.p (INPUT-OUTPUT v-est-no,INPUT 8).
            FIND FIRST xeb WHERE xeb.company   EQ cocode
                AND xeb.est-no    EQ v-est-no
                AND xeb.cust-no   EQ oe-ord.cust-no
                AND xeb.form-no   NE 0
                AND (xeb.est-type EQ 1 OR xeb.est-type EQ 5)
                USE-INDEX est-no NO-LOCK NO-ERROR.
            IF AVAILABLE xeb                          AND
                xeb.stock-no NE ""                 AND
                xeb.stock-no NE INPUT oe-ordl.i-no THEN 
            DO:
                MESSAGE "Item # must match Estimate's Item #" VIEW-AS ALERT-BOX ERROR.         
                DISPLAY xeb.stock-no @ oe-ordl.i-no.
                APPLY "entry" TO oe-ordl.i-no.
                RETURN ERROR.
            END.
        END.

        FIND FIRST itemfg WHERE itemfg.company = cocode AND itemfg.i-no EQ INPUT oe-ordl.i-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE itemfg THEN 
        DO:
            IF NOT v-est-fg                  AND
                AVAILABLE xeb                     AND
                itemfg.part-no NE xeb.part-no THEN 
            DO:
                MESSAGE " FG customer part number does not match"
                    "estimate's, continue?" 
                    VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE choice AS LOG.
                IF NOT choice THEN RETURN ERROR.
            END.

            cp-part-no = "".
            IF ll-new-file THEN 
            DO:
                cp-rowid = ROWID(itemfg).
                RUN custom/getcpart.p (cocode, oe-ord.cust-no,
                    INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
            END.
      
            FIND FIRST cust WHERE cust.company = oe-ord.company
                AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
            IF cp-part-no EQ "" AND
                itemfg.cust-no NE oe-ord.cust-no AND itemfg.cust-no NE "" AND
                AVAILABLE cust AND NOT cust.internal                         THEN 
            DO:
                FIND FIRST cust WHERE cust.company = oe-ord.company
                    AND cust.cust-no = itemfg.cust-no NO-LOCK NO-ERROR.
                IF AVAILABLE cust AND NOT cust.internal THEN 
                DO:
                    choice = NO.
                    FIND FIRST sys-ctrl WHERE sys-ctrl.company = oe-ord.company AND
                        sys-ctrl.NAME = "OEITEM" NO-LOCK NO-ERROR.
            
                    IF AVAILABLE sys-ctrl AND NOT sys-ctrl.log-fld THEN 
                        MESSAGE "This item exists for a different customer!"
                            VIEW-AS ALERT-BOX ERROR.
                    ELSE MESSAGE "This item exists for a different customer!  Do you want to continue?"
                            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice.
                    IF NOT choice THEN RETURN ERROR.
                END.
            END.
      
        END.
        IF AVAILABLE itemfg AND  itemfg.prod-uom EQ "" THEN 
        DO:
            MESSAGE "FG Item " + itemfg.i-no + " has no cost UOM. Please correct and try again. " VIEW-AS ALERT-BOX.      
            RETURN ERROR.
        END. /* not avail */
  
        IF NOT AVAILABLE itemfg AND
            (oe-ordl.i-no:screen-value NE "0") THEN RETURN ERROR.     
        IF NOT AVAILABLE itemfg AND
            (oe-ordl.i-no:screen-value = "0") THEN 
        DO:
            MESSAGE "Invalid FG Item#. Try help. " VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
        END. /* not avail */

    END. /* frame {&frame-name} */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-start-date d-oeitem 
PROCEDURE validate-start-date :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        RUN jc/validStartDate.p (INPUT fi_jobStartDate:SCREEN-VALUE,
            OUTPUT cMessage).
        ll-valid = YES.                         
        IF cMessage NE "" AND NOT lShowWarning THEN
        DO:
            MESSAGE cMessage 
                VIEW-AS ALERT-BOX WARNING.
            lShowWarning = YES.       
        END.       
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetActBOLQty d-oeitem 
FUNCTION fGetActBOLQty RETURNS INTEGER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lr-rel-lib AS HANDLE    NO-UNDO.
    DEFINE VARIABLE li         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE liReturn   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat    AS CHARACTER NO-UNDO.

    IF VALID-HANDLE(lr-rel-lib) THEN 
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            AND LOOKUP(oe-rel.stat, "P") GT 0
            :
            RUN get-act-qty IN lr-rel-lib (ROWID(oe-rel), OUTPUT liReturn).
            li = li + liReturn.
        END.
    RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetActRelQty d-oeitem 
FUNCTION fGetActRelQty RETURNS INTEGER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.

    IF AVAILABLE oe-ordl THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
            :
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
            IF INDEX("A,B,P",lv-stat) GT 0 THEN
                li = li + oe-rel.qty.
        END.
    RETURN li.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetBal d-oeitem 
FUNCTION fGetBal RETURNS INTEGER
    (OUTPUT op-qoh AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotalJobOnHandQty AS INTEGER NO-UNDO.

    /*   IF AVAIL oe-ordl AND oe-ordl.job-no NE "" THEN */
    FOR EACH job-hdr FIELDS(company job-no job-no2 i-no) NO-LOCK
        WHERE job-hdr.company EQ oe-ordl.company 
        AND job-hdr.ord-no  EQ oe-ordl.ord-no 
        AND job-hdr.i-no    EQ oe-ordl.i-no
        USE-INDEX ord-no
        BREAK BY job-hdr.job-no
        BY job-hdr.job-no2
        BY job-hdr.i-no
        :
        IF LAST-OF(job-hdr.i-no) THEN    
            FOR EACH fg-bin FIELDS (qty) NO-LOCK
                WHERE fg-bin.company EQ job-hdr.company
                AND fg-bin.job-no  EQ job-hdr.job-no
                AND fg-bin.job-no2 EQ job-hdr.job-no2
                AND fg-bin.i-no    EQ job-hdr.i-no
                :
                iTotalJobOnHandQty = iTotalJobOnHandQty + fg-bin.qty.
            END.
    END.
    op-qoh = iTotalJobOnHandQty.
    RETURN iTotalJobOnHandQty.    /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCost d-oeitem 
FUNCTION fGetCost RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.company EQ oe-ord.company
        AND ar-invl.ord-no  EQ oe-ord.ord-no
        AND ar-invl.i-no    EQ oe-ordl.i-no
        NO-ERROR.
    RETURN IF AVAILABLE ar-invl THEN ar-invl.cost ELSE 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCostUOM d-oeitem 
FUNCTION fGetCostUOM RETURNS CHARACTER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FIND FIRST ar-invl NO-LOCK
        WHERE ar-invl.company EQ oe-ord.company
        AND ar-invl.ord-no  EQ oe-ord.ord-no
        AND ar-invl.i-no    EQ oe-ordl.i-no
        NO-ERROR.
    IF AVAILABLE ar-invl THEN 
    DO:
        RETURN IF ar-invl.dscr[1] EQ "" THEN "M"
        ELSE ar-invl.dscr[1].
    END.
    ELSE
        RETURN "M".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetLastShipTo d-oeitem 
FUNCTION fGetLastShipTo RETURNS CHARACTER
    (  ):
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

    FOR EACH buf-oe-rel NO-LOCK
        WHERE buf-oe-rel.company EQ oe-ordl.company
        AND buf-oe-rel.ord-no  EQ oe-ordl.ord-no
        AND buf-oe-rel.i-no    EQ oe-ordl.i-no
        AND buf-oe-rel.line    EQ oe-ordl.line
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

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPct d-oeitem 
FUNCTION fGetPct RETURNS INTEGER
    (ipBal AS INTEGER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    IF oe-ordl.qty NE 0 THEN 
    DO:
        rtnValue = ((ipBal / oe-ordl.qty) - 1) * 100.
        IF rtnValue EQ 0 THEN rtnValue = 100.
        IF rtnValue EQ -100 THEN rtnValue = 0.
    END.
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPriceDisc d-oeitem 
FUNCTION fGetPriceDisc RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE ld AS DECIMAL NO-UNDO.

    FIND b-oe-ordl NO-LOCK
        WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).

    ld = b-oe-ordl.price * (1 - (b-oe-ordl.disc / 100)).

    FOR EACH ar-invl FIELDS(inv-no unit-pr disc) NO-LOCK
        WHERE ar-invl.company EQ b-oe-ordl.company
        AND ar-invl.ord-no  EQ b-oe-ordl.ord-no
        AND ar-invl.i-no    EQ b-oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING
        :
        ld = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).
        LEAVE.
    END.
    RETURN ld.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetProd d-oeitem 
FUNCTION fGetProd RETURNS INTEGER
    (OUTPUT op-bal AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iTotalProdQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE iJobProdQty   AS INTEGER NO-UNDO.

    FOR EACH job-hdr FIELDS(company job-no job-no2 i-no) NO-LOCK 
        WHERE job-hdr.company EQ oe-ordl.company 
        AND job-hdr.ord-no  EQ oe-ordl.ord-no 
        AND job-hdr.i-no    EQ oe-ordl.i-no
        USE-INDEX ord-no
        BREAK BY job-hdr.job-no
        BY job-hdr.job-no2
        :
        IF FIRST-OF(job-hdr.job-no2) THEN 
        DO:
            RUN fg/GetProductionQty.p (
                job-hdr.company,
                job-hdr.job-no,
                job-hdr.job-no2,
                job-hdr.i-no,
                NO,
                OUTPUT iJobProdQty
                ).
            iTotalProdQty = iTotalProdQty + iJobProdQty.
        END.
    END.
    IF oe-ordl.po-no-po NE 0 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
            WHERE fg-rcpth.company   EQ oe-ordl.company
            AND fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) 
            AND fg-rcpth.i-no      EQ oe-ordl.i-no 
            AND fg-rcpth.rita-code EQ "R",
            EACH fg-rdtlh FIELDS(qty) NO-LOCK 
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no 
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            :
            iTotalProdQty = iTotalProdQty + fg-rdtlh.qty.
        END.
    op-bal = iTotalProdQty.
    RETURN iTotalProdQty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetPrUOM d-oeitem 
FUNCTION fGetPrUOM RETURNS CHARACTER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER b-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lv-uom AS CHARACTER NO-UNDO.

    FIND b-oe-ordl NO-LOCK
        WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl).
    lv-uom = b-oe-ordl.pr-uom.

    FOR EACH ar-invl FIELDS(inv-no pr-uom) NO-LOCK
        WHERE ar-invl.company EQ b-oe-ordl.company
        AND ar-invl.ord-no  EQ b-oe-ordl.ord-no
        AND ar-invl.i-no    EQ b-oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING
        :
        lv-uom = ar-invl.pr-uom.
        LEAVE.
    END.
    RETURN lv-uom.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetRelDate d-oeitem 
FUNCTION fGetRelDate RETURNS DATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dtReturn AS DATE      NO-UNDO.
    DEFINE VARIABLE cRelStat AS CHARACTER NO-UNDO.

    IF AVAILABLE oe-rel THEN 
    DO:
    {oe/rel-stat.i cRelStat}
        IF AVAILABLE oe-rell THEN
            FIND FIRST oe-relh NO-LOCK
                WHERE oe-relh.r-no EQ oe-rell.r-no
                NO-ERROR.
        dtReturn = IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
    END.
    RETURN dtReturn.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetReturned d-oeitem 
FUNCTION fGetReturned RETURNS DECIMAL
    (ipcValueNeeded AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dTotQtyRet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotRetInv AS DECIMAL NO-UNDO.

    FOR EACH ar-invl NO-LOCK
        WHERE ar-invl.company EQ oe-ordl.company
        AND ar-invl.ord-no  EQ oe-ordl.ord-no
        AND ar-invl.i-no    EQ oe-ordl.i-no
        BREAK BY ar-invl.inv-no
        :
        IF FIRST-OF(ar-invl.inv-no) THEN
            FOR EACH oe-reth NO-LOCK
                WHERE oe-reth.company EQ ar-invl.company
                AND oe-reth.posted  EQ TRUE
                AND oe-reth.applied EQ TRUE 
                AND oe-reth.cust-no EQ oe-ordl.cust-no
                AND oe-reth.inv-no  EQ ar-invl.inv-no,
                EACH oe-retl NO-LOCK
                WHERE oe-retl.company EQ oe-reth.company
                AND oe-retl.r-no    EQ oe-reth.r-no
                AND oe-retl.i-no    EQ ar-invl.i-no
                :
                ASSIGN
                    dTotQtyRet = dTotQtyRet + oe-retl.tot-qty-return        
                    dTotRetInv = dTotRetInv + oe-retl.qty-return-inv
                    .
            END.  /* for each return */
    END. /* for each ar-invl */
    RETURN IF ipcValueNeeded EQ "TotalReturned" THEN dTotQtyRet
    ELSE dTotRetInv.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetReturnedInv d-oeitem 
FUNCTION fGetReturnedInv RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RETURN fGetReturned("ReturnedInv").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTaxable d-oeitem 
FUNCTION fGetTaxable RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER, ipcCust AS CHARACTER , ipcShipto AS CHARACTER, ipcFGItemID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Gets the Taxable flag based on inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

    RUN Tax_GetTaxableAR  (ipcCompany, ipcCust, ipcShipto, ipcFGItemID, OUTPUT lTaxable).  
    RETURN lTaxable.


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTotalReturned d-oeitem 
FUNCTION fGetTotalReturned RETURNS DECIMAL
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RETURN fGetReturned("TotalREturned"). 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetWIP d-oeitem 
FUNCTION fGetWIP RETURNS INTEGER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

    rtnValue = oe-ordl.qty - (li-qoh + oe-ordl.ship-qty).
    IF rtnValue LT 0 OR
        rtnValue LT oe-ordl.qty * oe-ordl.under-pct / 100 THEN
        rtnValue = 0.
    RETURN rtnValue.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetXferQty d-oeitem 
FUNCTION fGetXferQty RETURNS INTEGER
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER buf-oe-rel   FOR oe-rel.
    DEFINE BUFFER buf-reftable FOR reftable.

    DEFINE VARIABLE vTransfer-Qty AS INTEGER NO-UNDO INIT 0.

    FOR EACH buf-oe-rel NO-LOCK
        WHERE buf-oe-rel.company EQ oe-ordl.company
        AND buf-oe-rel.ord-no  EQ oe-ordl.ord-no
        AND buf-oe-rel.i-no    EQ oe-ordl.i-no
        AND buf-oe-rel.line    EQ oe-ordl.line
        AND (buf-oe-rel.stat   EQ "C"
        OR buf-oe-rel.stat    EQ "Z")
        :
        IF oe-ord.type NE "T" AND buf-oe-rel.s-code NE "T" THEN NEXT.
        vTransfer-Qty = vTransfer-Qty + buf-oe-rel.qty.
    END.
    RETURN vTransfer-Qty.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fIsCustPriceHoldExempt d-oeitem 
FUNCTION fIsCustPriceHoldExempt RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER, ipcCustomerID AS CHARACTER, ipcShipToID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns true if the customer is not activated for price hold logic
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCustExempt      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPriceHold       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPriceHoldActive AS LOGICAL NO-UNDO.
        
    RUN Price_CheckPriceHoldForCustShip (ipcCompany, ipcCustomerID, ipcShipToID, OUTPUT lPriceHold, OUTPUT lPriceHoldActive).

    lCustExempt = NOT lPriceHold AND lPriceHoldActive.
    
    RETURN lCustExempt.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fnPrevOrder d-oeitem 
FUNCTION fnPrevOrder RETURNS CHARACTER
    (ipcEstNo AS CHARACTER, ipiOrdNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    IF ipcEstNo GT "" THEN 
    DO:
        FIND LAST bf-oe-ordl NO-LOCK
            WHERE bf-oe-ordl.company EQ cocode
            AND bf-oe-ordl.est-no  EQ ipcEstNo
            AND bf-oe-ordl.ord-no  LT ipiOrdNo
            NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN
            cResult = STRING(bf-oe-ordl.ord-no).
    END.
    RETURN cResult.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fOEScreenUOMConvert d-oeitem 
FUNCTION fOEScreenUOMConvert RETURNS DECIMAL
    ( ipdStartQuantity AS DECIMAL , ipcUOM AS CHARACTER, ipdCount AS DECIMAL, ipcItemID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE dQtyInEA AS DECIMAL NO-UNDO.

    RUN Conv_QtyToEA(oe-ord.company, ipcItemID, ipdStartQuantity, ipcUOM, ipdCount, OUTPUT dQtyInEA).
    RETURN dQtyInEa.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUseNewEstimating d-oeitem 
FUNCTION fUseNewEstimating RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Use new estimating
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCEVersion AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "CEVersion", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    lCEVersion = lRecFound AND cRtnChar EQ "New".
    
    RETURN lCEVersion. 
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-colonial-rel-date d-oeitem 
FUNCTION get-colonial-rel-date RETURNS DATE
    ( iprRel AS ROWID) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE opRelDate AS DATE  NO-UNDO.
    DEFINE VARIABLE rShipTo   AS ROWID NO-UNDO.
    DEFINE BUFFER bf-shipto FOR shipto.    
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ iprRel NO-LOCK NO-ERROR.
    RUN sys/ref/shipToOfRel.p (INPUT ROWID(oe-rel), OUTPUT rShipTo).
    FIND bf-shipto WHERE ROWID(bf-shipto) EQ rShipTo NO-LOCK NO-ERROR.
    FIND FIRST bf-oe-ord WHERE bf-oe-ord.company EQ bf-oe-rel.company
        AND bf-oe-ord.ord-no EQ bf-oe-rel.ord-no
        NO-LOCK NO-ERROR.
    /* order header due-date - dock appt days, adjusted for weekends */
    IF AVAILABLE bf-shipto AND AVAIL(bf-oe-ord) THEN 
    DO:      
        IF oereleas-cha EQ "LastShip" THEN
            opRelDate = bf-oe-ord.last-date.
        ELSE IF oereleas-cha EQ "Due Date" THEN
                opRelDate = oe-ordl.req-date.
            ELSE IF oereleas-cha EQ "DueDateLessTransitDays" THEN    
                    opRelDate = oe-ordl.req-date - (IF AVAILABLE bf-shipto THEN bf-shipto.del-time ELSE 0).
                ELSE /*DueDate+1Day*/
                DO:
                    opRelDate = oe-ordl.req-date + 1.
                    IF WEEKDAY(opRelDate) EQ 7 THEN
                        opRelDate = oe-rel.rel-date + 2.
                    ELSE
                        IF WEEKDAY(opRelDate) EQ 1 THEN
                            opRelDate = opRelDate + 1.
                END.
    END.   
    RETURN opRelDate.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-itemfg-cost d-oeitem 
FUNCTION get-itemfg-cost RETURNS DECIMAL
    ( ipv-item AS CHARACTER /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bfItemfg FOR itemfg.
    DEFINE VARIABLE v-cost AS DECIMAL NO-UNDO.
    v-cost = 0.
    FIND FIRST bfItemfg WHERE bfItemfg.company = cocode
        AND bfItemfg.i-no    = ipv-item
        NO-LOCK NO-ERROR.
    IF AVAIL(bfItemfg) THEN
        v-cost = bfItemfg.total-std-cost.
    FIND FIRST fg-ctrl WHERE fg-ctrl.company = cocode NO-LOCK NO-ERROR.
    IF AVAILABLE fg-ctrl THEN 
    DO:
        IF fg-ctrl.inv-meth = "A" AND bfItemfg.avg-cost GT 0 THEN
            v-cost = bfItemfg.avg-cost.
        ELSE
            IF fg-ctrl.inv-meth = "L" AND bfItemfg.last-cost GT 0 THEN
                v-cost = bfItemfg.last-cost.
    END.
    RETURN v-cost.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

