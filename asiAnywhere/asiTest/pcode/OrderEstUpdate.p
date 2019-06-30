 

/*------------------------------------------------------------------------
    File        :OrderEstUpdate.p
    Purpose     : View Order Entry

    Syntax      :

    Description : Return a Dataset of all order Entry

    Author(s)   : Sewa Singh
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{OrderEstUpdate.i}

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmExt         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmUserid      AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmStat        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSold        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmOrdate      AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldName    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueCode     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmDueDate     AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmCustAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLastDate    AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmcustAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldAddr2   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProdDate    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCity        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmState       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmZip         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldCity    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldState   AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSoldZip     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmPonum       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmContact     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOverpct     AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmUnderpct    AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmTerms       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmTermdscr    AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmProd        AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmTaxgr       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFreight     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmCarrier     AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmFob         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman        AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSname       AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman2       AS CHAR NO-UNDO.                   
DEFINE INPUT PARAMETER prmSname2      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSman3       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmSname3      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCtype       AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmcExp        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCnum        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmCauth       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmCustName    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmType        AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmLine        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmWhis        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER VRowid         AS RECID       NO-UNDO.
DEFINE INPUT PARAMETER prmJob         AS CHARACTER.
DEFINE INPUT PARAMETER prmJob2        AS INTEGER.
DEFINE INPUT PARAMETER prmEst         AS CHARACTER.
DEFINE INPUT PARAMETER prmSales1      AS DECIMAL.
DEFINE INPUT PARAMETER prmSales2      AS DECIMAL.
DEFINE INPUT PARAMETER prmSales3      AS DECIMAL.
DEFINE INPUT PARAMETER prmComm1       AS DECIMAL.
DEFINE INPUT PARAMETER prmComm2       AS DECIMAL.
DEFINE INPUT PARAMETER prmComm3       AS DECIMAL.
DEFINE INPUT PARAMETER prmQuote       AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPrice       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUom         AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsOrderEstUpdate.
DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 
DEF VAR vEstim AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-ord FOR oe-ord .
DEFINE BUFFER buff-ord FOR oe-ord.
DEFINE BUFFER buff-ordl FOR oe-ordl.
DEFINE VARIABLE Ordernum AS INTEGER.
DEFINE VARIABLE Custnum AS CHAR NO-UNDO.
DEFINE VARIABLE Custnum2 AS CHAR NO-UNDO.
DEFINE VAR VAction      AS CHARACTER  NO-UNDO.
DEFINE VAR prmCas           AS INTEGER NO-UNDO.
 def var lv-est-no as char.



IF prmUser        = ?  THEN ASSIGN    prmUser = "".
IF prmExt         = ?  THEN ASSIGN    prmExt = "".
IF prmOrderNum    = ?  THEN ASSIGN    prmOrderNum = 0.
IF prmAction      = ?  THEN ASSIGN    prmAction      = "".
IF prmAction      = "" THEN ASSIGN    prmAction      = "Select".
IF prmContact     = ?  THEN ASSIGN    prmContact     = "".      
IF prmCustomer    = ?  THEN ASSIGN    prmCustomer    = "".
IF prmUserid      = ?  THEN ASSIGN    prmUserid      = "".  
IF prmStat        = ?  THEN ASSIGN    prmStat        = "".  
IF prmSold        = ?  THEN ASSIGN    prmSold        = "".  
IF prmSoldName    = ?  THEN ASSIGN    prmSoldName    = "".  
IF prmDueCode     = ?  THEN ASSIGN    prmDueCode     = "".             
IF prmCustAddr    = ?  THEN ASSIGN    prmCustAddr    = "".     
IF prmSoldAddr    = ?  THEN ASSIGN    prmSoldAddr    = "". 
IF prmcustAddr2   = ?  THEN ASSIGN    prmcustAddr2   = "".              
IF prmSoldAddr2   = ?  THEN ASSIGN    prmSoldAddr2   = "".     
IF prmCity        = ?  THEN ASSIGN    prmCity        = "". 
IF prmState       = ?  THEN ASSIGN    prmState       = "". 
IF prmZip         = ?  THEN ASSIGN    prmZip         = "".    
IF prmSoldCity    = ?  THEN ASSIGN    prmSoldCity    = "".  
IF prmSoldState   = ?  THEN ASSIGN    prmSoldState   = "".  
IF prmSoldZip     = ?  THEN ASSIGN    prmSoldZip     = "".  
IF prmPonum       = ?  THEN ASSIGN    prmPonum       = "".  
IF prmOverpct     = ?  THEN ASSIGN    prmOverpct     = 0.  
IF prmUnderpct    = ?  THEN ASSIGN    prmUnderpct    = 0.  
IF prmTerms       = ?  THEN ASSIGN    prmTerms       = "".  
IF prmTermdscr    = ?  THEN ASSIGN    prmTermdscr    = "".       
IF prmProd        = ?  THEN ASSIGN    prmProd        = 0.  
IF prmTaxgr       = ?  THEN ASSIGN    prmTaxgr       = "".  
IF prmFreight     = ?  THEN ASSIGN    prmFreight     = "".  
IF prmCarrier     = ?  THEN ASSIGN    prmCarrier     = "".  
IF prmFob         = ?  THEN ASSIGN    prmFob         = "".  
IF prmSman        = ?  THEN ASSIGN    prmSman        = "".  
IF prmSname       = ?  THEN ASSIGN    prmSname       = "".  
IF prmSman2       = ?  THEN ASSIGN    prmSman2       = "".  
IF prmSname2      = ?  THEN ASSIGN    prmSname2      = "".  
IF prmSman3       = ?  THEN ASSIGN    prmSman3       = "".  
IF prmSname3      = ?  THEN ASSIGN    prmSname3      = "".  
IF prmCtype       = ?  THEN ASSIGN    prmCtype       = "".
IF prmCnum        = ?  THEN ASSIGN    prmCnum        = "". 
IF prmCauth       = ?  THEN ASSIGN    prmCauth       = "". 
IF prmCustName    = ?  THEN ASSIGN    prmCustName    = "". 
IF prmType        = ?  THEN ASSIGN    prmType        = "". 
IF prmLine        = ?  THEN ASSIGN    prmLine        = 0. 
IF prmEst         = ? THEN ASSIGN     prmEst         =  "".
IF prmJob         = ? THEN ASSIGN     prmJob         = "".
IF prmJob2        = ? THEN ASSIGN     prmJob2        = 0.
IF prmQty         = ? THEN ASSIGN     prmQty         = 0.
IF prmPrice       = ? THEN ASSIGN     prmPrice       = 0.
IF prmUom         = ? THEN ASSIGN     prmUom         = "".
IF prmcExp        = ? THEN ASSIGN     prmcExp        = STRING(TODAY).
IF prmProdDate    = ?  THEN ASSIGN    prmProdDate    = "".
IF prmQuote       = ?  THEN ASSIGN    prmQuote       = 0.
/***********************************************************************************/
/*{custom/globdefs.i}*/
def new shared var v-upd-comm as log initial yes no-undo.
DEF NEW SHARED VAR v-misc AS LOG INIT NO NO-UNDO.
DEF NEW SHARED VAR v-fr-tax LIKE oe-ctrl.f-tax NO-UNDO.

DEF WORKFILE w-est-no FIELD w-est-no LIKE itemfg.est-no FIELD w-run AS LOG.

     {sys/inc/var.i "new shared" }
     {sys/inc/fgmaster.i}
    {sys/ref/oecount.i}

 DEFINE NEW  SHARED VARIABLE vError AS CHARACTER NO-UNDO.
def var choice as log no-undo.
  def var hld-id as recid no-undo.
  def var hld-stat like job.stat no-undo.
  def var hld-nufile as log no-undo.
  DEF VAR v-run-schedule AS LOG NO-UNDO.
         
def var lv-job-recid as recid no-undo.
DEF BUFFER b-oe-ord FOR oe-ord.
DEF BUFFER bf-oe-ord FOR oe-ord.
def buffer bfx-ord for oe-ord.
def var li-lead-days as int no-undo.
DEF VAR isset AS CHAR NO-UNDO.
def var ll-f-bill as log no-undo.
def var li-sold-no as int no-undo.
def var ls-ship-i as cha extent 4 no-undo.
def var v-slow-ord as log no-undo.
def var v-beeler as log no-undo.
def var v-ilwalker as log no-undo.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR v-rel AS INT NO-UNDO.
DEF VAR ll-new-fg-created AS LOG NO-UNDO.

DEFINE VAR ttcost AS DECIMAL NO-UNDO.
def new shared var v-create-job as log no-undo.
DEF NEW SHARED VAR v-d-rel AS INT NO-UNDO.
def var v-custype like cust.type no-undo.
def var v-ord-limit like cust.ord-lim no-undo.
def var v-crd-limit like cust.cr-lim no-undo.
def var v-valdcode as cha init "ON,BY,MH" no-undo.
def var v-valtype as cha init "O,R,C" no-undo.
def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
/*def var v-oecount like sys-ctrl.log-fld no-undo.*/
def var v-full-cost as log no-undo.
def var oeprompt like sys-ctrl.log-fld no-undo.
def var v-quo-price like sys-ctrl.log-fld no-undo.
def var v-inactive as log no-undo.
def var save_id as recid no-undo.
def new shared var fil_id as recid no-undo.
def var v-job-no like oe-ord.job-no no-undo.
def var v-job-no2 like oe-ord.job-no2 no-undo.
def var v-exp-limit as int init 10 no-undo.
def var v-n-ord like oe-ctrl.n-ord no-undo.
def var v-estord-id as recid extent 10 no-undo.
def var v-multord as log no-undo.
DEFINE VAR vest AS CHAR.
{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
def NEW SHARED WORKFILE work-ordl LIKE oe-ordl.
def new shared var nufile as log no-undo.
def new shared buffer xoe-ord for oe-ord.
def new shared var lv-qty as int no-undo.  /* for oe-ordl.qty and oe-ordm calc */
def var lv-new-row-id as rowid no-undo.  /* first creation error */
def new shared var v-qty-mod as log no-undo.
def new shared var qty as INT no-undo.
def var ll-order-from-est as log no-undo.  /* is order created from estimate */
def var ll-cust-displayed as log no-undo.
def var ll-est-no-mod as log no-undo.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG INITIAL NO NO-UNDO.
def var ll-is-new-rec as log no-undo.
DEF VAR ll-from-tandem AS LOG NO-UNDO.
DEF VAR lv-old-cust-no LIKE oe-ord.cust-no NO-UNDO.
DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
def var ll-new-record as log no-undo.

DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR Orderline AS INT NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.
def NEW SHARED buffer xest for est.
def NEW SHARED buffer xeb for eb.
def NEW SHARED buffer xef for ef.
def var v-tmp-price as dec format ">,>>>,>>9.9999" no-undo.
DEF VAR v-price-per-1000 AS DEC NO-UNDO.
def var lv-t-price as dec no-undo.
DEFINE VAR prmTPrice AS DECIMAL NO-UNDO.
DEF VAR lv-uom LIKE itemfg.prod-uom NO-UNDO.

DEF NEW SHARED VAR s-est-no AS cha NO-UNDO.  /* for fgadd2.p */

DEF BUFFER oe-ord-whs-order FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.
DEF WORKFILE w-ord FIELD w-ord-no LIKE oe-ord.ord-no.
DEF WORKFILE old-oe-ord LIKE oe-ord.
DEF TEMP-TABLE tt-oe-ordl LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEFINE VAR fgitem AS CHAR NO-UNDO.
DEFINE VAR pro AS CHAR NO-UNDO.
DEFINE VAR fgitemname AS CHAR NO-UNDO.
DEFINE VAR procat AS CHAR NO-UNDO.
DEF VAR vEstType AS INT NO-UNDO.
DEFINE VAR tfright LIKE oe-ordl.t-freight NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.   

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
g_company = prmComp
g_loc     = "Main"
 cocode = g_company
 locode = g_loc. 

      
{oe/oe-sysct1.i NEW}

DO TRANSACTION:
  {sys/inc/oedate.i}
  {sys/inc/oecomb.i}
  {sys/inc/job#.i}
  {sys/inc/graphic.i}
  {sys/inc/oeestcom.i}
END.
{sys/inc/f16to32.i}
   /* RUN oe/oe-sysct.p.*/

  {oe/oe-sysct.i}
{sys/inc/cerun.i C}
{sys/inc/cecomm.i}
{ce/msfcalc.i}

RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).
/*******************************************************************/
def var ls-stock as CHAR no-undo.  /* for eb.stock-no */
DEF VAR lv-new-tandem AS ROWID NO-UNDO.
DEF VAR ll-is-tandem AS LOG NO-UNDO.
DEF VAR ll-got-qtprice AS LOG. 
def var lv-price as dec no-undo.
def var lv-pr-uom as cha no-undo.
DEF VAR lv-i-no AS CHAR NO-UNDO.
def var li-cnt as int no-undo.
DEF VAR ll-tax AS LOG NO-UNDO.
DEF VAR ll-do-job AS LOG NO-UNDO.
def var li-cases as int no-undo.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEFINE VARIABLE v-disp-prod-cat AS CHARACTER  NO-UNDO.
DEF VAR ld-marg% AS DEC NO-UNDO.
DEF VAR v-com AS DEC NO-UNDO.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR ip-est-type like eb.est-type no-undo.
DEF VAR ip-loc AS CHAR NO-UNDO.
DEFINE VAR vlog-fld LIKE sys-ctrl.log-fld NO-UNDO.

/*********itemline***/
def var v-est-type like est.est-type no-undo.
def var li-line-no as int no-undo.
def var v-blk-qty as int no-undo.
DEF VAR v-tot-comm-2 AS DEC NO-UNDO.
DEF VAR ll-use-margin AS LOG NO-UNDO.
DEF VAR lv-sell-by AS CHAR NO-UNDO.
DEF VAR lv-sell-by-ce-ctrl AS CHAR NO-UNDO.
DEF VAR v-pct-2 LIKE eb.comm NO-UNDO.
DEF VAR v-comm-2 as dec no-undo.

DEF VAR v-basis AS CHAR NO-UNDO.

DEF VAR v-sell-price AS DEC DECIMALS 2 NO-UNDO.
DEF VAR v-probe-comm AS DEC DECIMALS 5 NO-UNDO.
DEF VAR v-mp AS DEC DECIMALS 5 NO-UNDO.
DEF VAR v-qty-2 AS DEC NO-UNDO.
DEF VAR v-board-cst AS DEC NO-UNDO.
DEF BUFFER b-ef2 FOR ef.
    /******************************addset var*************************/
    /*DEF TEMP-TABLE tt-cust-part FIELD cust-no  LIKE eb.cust-no
                            FIELD part-no  LIKE eb.part-no
                            FIELD stock-no LIKE eb.stock-no
                            FIELD qty-set  AS   DEC
                            INDEX cust-no cust-no part-no stock-no.*/

DEF BUFFER b-eb2 FOR eb.
    DEF BUFFER b-blk FOR blk.
    DEF BUFFER x-eb FOR eb.
   DEF VAR ll-one-part  AS LOG NO-UNDO.
   def var v-item-no    like itemfg.i-no NO-UNDO.
   def var v-item-part  like eb.part-no NO-UNDO.
def var v-item-name  like itemfg.i-name NO-UNDO.
def var v-item-dscr1 like itemfg.part-dscr1 NO-UNDO.
def var v-item-dscr2 like itemfg.part-dscr2 NO-UNDO.
def var v-part-qty   as dec NO-UNDO.
def var v-tot-qty    like oe-ordl.qty NO-UNDO.
def var v-item-price like oe-ordl.price NO-UNDO.
def var v-eb-part    as ch format "x(20)" NO-UNDO.
DEF VAR lv-set-part  LIKE eb.part-no NO-UNDO.
DEF VAR v-blk-qty-2 AS INT NO-UNDO.
DEF VAR blk-fact LIKE blk.fact NO-UNDO.
DEF VAR v-qty-3 AS DEC NO-UNDO.
DEF BUFFER bf-blk FOR blk.
    DEF VAR v-cost-2 LIKE oe-ordl.cost.
  def var v-run-list as char init
  "oe/calc-one.p,oe/calc-box.p,ce/tan/print4.p,ce/com/print4.p,cec/com/print4.p".
  def var v-est-run like est.est-type no-undo.


/*************************************************************************************/
/*DEF VAR prmComp AS CHAR NO-UNDO.   

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
g_company = prmComp
g_loc     = "Main"
 cocode = g_company
 locode = g_loc.        */
    
 find first sys-ctrl where sys-ctrl.company eq cocode
     and sys-ctrl.name    eq "FGITEM#"
     no-lock no-error.
 if not avail sys-ctrl then do transaction:
     create sys-ctrl.
     assign
         sys-ctrl.company = cocode
         sys-ctrl.name    = "FGITEM#"
         sys-ctrl.descrip = "Order Entry default FG Item Number from Estimate?"
         sys-ctrl.log-fld = yes.
     end.
     assign
         v-est-fg  = sys-ctrl.log-fld
         v-est-fg1 = sys-ctrl.char-fld.

  find first sys-ctrl where sys-ctrl.company eq cocode
     and sys-ctrl.name    eq "OESHIP"
     no-lock no-error.
  IF AVAIL sys-ctrl  THEN DO:
      ASSIGN
         vlog-fld  = sys-ctrl.log-fld  .
  END.


/* ********************  Preprocessor Definitions  ******************** */
    

IF prmExt = "ExtView"  THEN DO:
    
    FOR EACH  usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no NO-LOCK NO-ERROR.
        
        IF AVAIL cust THEN DO:
            CREATE ttOrderEstUpdate.
            ASSIGN
                 ttOrderEstUpdate.VCustomer   = cust.cust-no 
                ttOrderEstUpdate.VCustName = cust.name
                ttOrderEstUpdate.VCustAddr  = cust.addr[1]
                ttOrderEstUpdate.VcustAddr2 = cust.addr[2] 
                ttOrderEstUpdate.VCity   = cust.city
                ttOrderEstUpdate.VState   = cust.state
                ttOrderEstUpdate.VZip     = cust.zip
                ttOrderEstUpdate.VSman   = cust.sman   
                /*ttViewRfq.fob_code  = cust.fob-code*/
                .
                              
    find FIRST sman where sman.company = prmComp AND 
        sman.sman = cust.sman no-lock no-error.
    ASSIGN 
        /*ttViewRfq.comm = sman.scomm*/
        ttOrderEstUpdate.VSname = if avail sman then sman.sname else "".
         END.
END.
END. /* IF prmAction = "ExtView"  THEN DO:*/

IF prmAction = "Delete"  THEN DO:

     FIND FIRST bf-ord WHERE
        bf-ord.company EQ prmComp AND
        bf-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.

    IF  bf-ord.stat = "W"  THEN DO:
                
        FOR EACH buff-ordl WHERE buff-ordl.company EQ bf-ord.company AND
            buff-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK:
            
            FIND first est  where est.company eq prmComp
               and est.est-no  =  FILL(" ",8 - LENGTH(TRIM(bf-ord.est-no))) + TRIM(bf-ord.est-no) NO-LOCK NO-ERROR.
          
              FOR each eb  where eb.company  = prmComp
               and eb.est-no     eq est.est-no
               and eb.stock-no   ne ""
               and eb.part-no  eq buff-ordl.part-no  AND eb.ord-no = bf-ord.ord-no AND
                    (est.est-type eq 2 or est.est-type eq 6 ) EXCLUSIVE-LOCK:

                IF AVAIL eb THEN DO:
               ASSIGN 
                   eb.ord-no = 0 .
               RELEASE eb.
               end.
              end.

              find first itemfg
                  where itemfg.company eq buff-ordl.company
                  and itemfg.i-no    eq buff-ordl.i-no
                  no-error.

              if avail itemfg then do:
                  IF bf-ord.type NE "T" THEN itemfg.q-alloc = itemfg.q-alloc - buff-ordl.qty.
                  IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.
                  
                  assign
                      itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                      itemfg.q-ptd     = itemfg.q-ptd - buff-ordl.qty
                      itemfg.q-ord-ytd = itemfg.q-ord-ytd - buff-ordl.qty.

                  IF bf-ord.type NE "T"                                                   AND
                      NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl buff-ordl}) THEN
                      RUN fg/comp-upd.p (RECID(itemfg), buff-ordl.qty * -1, "q-alloc", 0).
               end. /* avail itemfg */
        END.
    END.
END.



IF prmAction = "Delete"  THEN DO:
   
    FIND FIRST bf-ord WHERE
        bf-ord.company EQ prmComp AND
        bf-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.

    IF  bf-ord.stat = "W"  THEN DO:
                
        FOR EACH buff-ordl WHERE buff-ordl.company EQ bf-ord.company AND
            buff-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK:
            
            IF AVAIL buff-ordl THEN
            DELETE buff-ordl.
                   END.
        DELETE bf-ord.  
   END.  /*IF AVAIL bf-ordl */
   ELSE DO:
       cError = "Can not delete this record".
       RETURN .
   END.
  FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK :
    FIND LAST oe-ord WHERE oe-ord.company = prmComp AND (oe-ord.cust-no = usercust.cust-no) NO-LOCK NO-ERROR.
    ASSIGN prmOrderNum = oe-ord.ord-no 
           prmAction = "Select".
  END.  /*FOR EACH usercust*/
   
END. /*IF prmAction = "delete"*/

/*********************************************************************************/
IF prmAction = "Add" THEN DO:
    IF prmQuote = 0 THEN do:
    FIND FIRST quotehd WHERE quotehd.est-no = prmEst AND prmEst <> "" AND prmEst <> "0" AND quotehd.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL quoteitm THEN DO:
        ASSIGN
            prmQuote  = quoteitm.q-no .
    END.
   END.
  
END.


/***************************************************************************************************/

IF prmAction = "Add" THEN DO:
    
    
    RUN sys/ref/asiseq.p (INPUT cocode, INPUT "order_seq", OUTPUT Ordernum) NO-ERROR.
    
      /*IF ERROR-STATUS:ERROR THEN
        MESSAGE "An error occured, please contact ASI: " RETURN-VALUE
           VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
      DO WHILE CAN-FIND(FIRST bf-oe-ord
                        WHERE bf-oe-ord.company EQ cocode
                          AND bf-oe-ord.ord-no  EQ Ordernum
                          AND ROWID(bf-oe-ord)  NE ROWID(oe-ord)):
        
          RUN sys/ref/asiseq.p (INPUT cocode, 
                              INPUT "order_seq", 
                              OUTPUT Ordernum) NO-ERROR.   
      END.
     
    CREATE oe-ord. 
    ASSIGN
        oe-ord.company           = prmComp
        oe-ord.ord-no            = Ordernum
        oe-ord.est-no            = prmEst
        oe-ord.q-no              = prmquote
        oe-ord.job-no            = prmJob
        oe-ord.job-no2           = prmJob2 
        oe-ord.cust-no           = prmCustomer  
        oe-ord.user-id           = prmUser 
        oe-ord.stat              = "W"
        oe-ord.sold-id           = prmSold 
        oe-ord.type              = prmType
        oe-ord.ord-date          = TODAY
        oe-ord.sold-name         = prmSoldName 
        oe-ord.due-code          = prmDueCode    
        oe-ord.due-date          = prmDueDate             
        oe-ord.addr[1]           = prmCustAddr   
        oe-ord.sold-addr[1]      = prmSoldAddr 
        oe-ord.last-date         = prmLastDate       
        oe-ord.addr[2]           = prmcustAddr2     
        oe-ord.sold-addr[2]      = prmSoldAddr2 
        oe-ord.prod-date         = DATE(prmProdDate)
        oe-ord.city              = prmCity
        oe-ord.state             = prmState
        oe-ord.zip               = prmZip  
        oe-ord.sold-city         = prmSoldCity
        oe-ord.sold-state        = prmSoldState
        oe-ord.sold-zip          = prmSoldZip
        oe-ord.po-no             = prmPonum
        oe-ord.contact           = prmContact
        oe-ord.over-pct          = prmOverpct
        oe-ord.under-pct         = prmUnderpct
        oe-ord.terms             = prmTerms
        oe-ord.terms-d           = prmTermdscr   
        oe-ord.pord-no           = prmProd    
        oe-ord.tax-gr            = prmTaxgr    
        oe-ord.frt-pay           = prmFreight    
        oe-ord.carrier           = prmCarrier    
        oe-ord.fob-code          = prmFob    
        oe-ord.sman[1]           = prmSman    
        oe-ord.sname[1]          = prmSname    
        oe-ord.sman[2]           = prmSman2    
        oe-ord.sname[2]          = prmSname2    
        oe-ord.sman[3]           = prmSman3  
        oe-ord.sname[3]          = prmSname3  
        oe-ord.cc-type           = prmCtype  
        oe-ord.cc-expiration     = DATE(prmcExp)
        oe-ord.cc-num            = prmCnum  
        oe-ord.cc-auth           = prmCauth
        oe-ord.cust-name         = prmCustName
        oe-ord.s-pct[1]          = prmSales1
        oe-ord.s-pct[2]          = prmSales2
        oe-ord.s-pct[3]          = prmSales3
        oe-ord.s-comm[1]         = prmComm1
        oe-ord.s-comm[2]         = prmComm2
        oe-ord.s-comm[3]         = prmComm3 
        oe-ord.q-no              = prmQuote
        oe-ord.whsed             = IF prmWhis = "yes" THEN TRUE ELSE FALSE 
        .
        IF(oe-ord.cc-type = "") THEN
            ASSIGN oe-ord.cc-expiration  = ? .

        IF prmFreight = "B" THEN oe-ord.f-bill = YES.
        ELSE oe-ord.f-bill = NO.
        
       ASSIGN
           save_id = RECID(oe-ord).
       
       find xoe-ord where recid(xoe-ord) = recid(oe-ord) NO-LOCK NO-ERROR.
       FOR EACH oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = oe-ord.ord-no EXCLUSIVE-LOCK :
           IF AVAIL oe-ordl  THEN  DO:
               DELETE oe-ordl.
           END.
                        
       END.
       
IF prmQuote = 0 AND prmEst <> "" THEN DO:
    {ordlfrest.i}
        
END.

/****************************************if prmquote <> 0 then do:*********************************************************************************/
     
 IF prmQuote <> 0 THEN DO:
    FIND FIRST quotehd WHERE quotehd.q-no = prmquote AND quotehd.company = prmComp  NO-LOCK NO-ERROR.
    FOR EACH quoteitm  WHERE quoteitm.q-no = quotehd.q-no NO-LOCK:
        FIND LAST oe-ordl WHERE oe-ordl.ord-no = Ordernum NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN DO:
            ASSIGN    Orderline = oe-ordl.LINE + 1.
        END.  /*IF AVAIL bf-ord THEN DO:*/
        ELSE DO:
            ASSIGN Orderline = 1.
       END.

        ASSIGN 
           fgitem = quoteitm.i-no
           .

       create b-oe-ordl.
       assign 
           b-oe-ordl.company       = prmComp
           b-oe-ordl.ord-no        = Ordernum
           b-oe-ordl.LINE          = Orderline
           b-oe-ordl.type-code     = prmType
           b-oe-ordl.stat          = "W"
           b-oe-ordl.cust-no       = prmCustomer
           b-oe-ordl.po-no         = prmPonum
           b-oe-ordl.req-code      = prmDueCode
           b-oe-ordl.req-date      = prmDueDate
           b-oe-ordl.prom-code     = prmDueCode
           b-oe-ordl.prom-date     = prmDueDate
           b-oe-ordl.over-pct      = prmOverpct
           b-oe-ordl.under-pct     = prmUnderpct
           b-oe-ordl.job-no        = prmJob
           b-oe-ordl.job-no2       = prmJob2
           b-oe-ordl.s-man[1]      = prmSman  
           b-oe-ordl.s-pct[1]      = prmSales1
           b-oe-ordl.s-comm[1]     = prmComm1
           b-oe-ordl.q-no          = prmQuote

           b-oe-ordl.est-no        = quotehd.est-no
           b-oe-ordl.cust-no       = quoteitm.cust-no 
           b-oe-ordl.part-dscr1    = quoteitm.part-dscr1
           b-oe-ordl.i-name        = quoteitm.part-dscr1
           b-oe-ordl.part-no       = quoteitm.part-no
           b-oe-ordl.part-dscr2    = quoteitm.part-dscr2
           b-oe-ordl.part-dscr3    = quoteitm.part-dscr3
           b-oe-ordl.price         = prmPrice
           b-oe-ordl.qty           = prmQty 
           b-oe-ordl.pr-uom        = prmUom
           b-oe-ordl.est-type      = quoteitm.est-type
           .
       

        IF prmQty = 0  THEN DO:
            ASSIGN
                b-oe-ordl.price         = quoteitm.price 
                b-oe-ordl.qty           = quoteitm.qty 
                b-oe-ordl.pr-uom        = quoteitm.uom 
                prmQty                  = quoteitm.qty
                prmPrice                = quoteitm.price 
                prmUom                  = quoteitm.uom 
                 .
               
        END.
          
        ASSIGN

            fil_id = RECID(b-oe-ordl).
       FIND FIRST cust WHERE 
           cust.cust-no EQ quotehd.cust-no AND cust.company = quotehd.company USE-INDEX cust NO-LOCK NO-ERROR.
       ASSIGN
           b-oe-ordl.disc      = cust.disc 
           b-oe-ordl.tax      = cust.sort eq "Y" and oe-ord.tax-gr ne ""  .
       
      

       FIND FIRST est WHERE est.company = prmComp AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
       ASSIGN
            vEstType = est.est-type 
            lv-est-no = FILL(" ",8 - LENGTH(TRIM(est.est-no))) + TRIM(est.est-no).

        
           IF vEstType = 2 OR vEstType = 6 THEN DO:
            FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = lv-est-no AND eb.form-no = 0  NO-LOCK NO-ERROR.
           IF AVAIL eb  THEN   DO:  
                    IF eb.set-is-assembled = TRUE THEN  ASSIGN isset = "Yes".
                    IF eb.set-is-assembled = FALSE THEN  ASSIGN isset = "No".
                    IF eb.set-is-assembled = ? THEN  ASSIGN isset = "Yes".
                   IF fgitem = "" THEN  ASSIGN fgitem  = eb.stock-no.
                   RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).
            
            ASSIGN
               b-oe-ordl.cas-cnt      = li-cnt
               b-oe-ordl.cases-unit = li-cases 
               b-oe-ordl.partial    = b-oe-ordl.qty MOD b-oe-ordl.cas-cnt.
                END.
           END.
           ELSE DO:
               FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = lv-est-no AND eb.part-no = quoteitm.part-no AND eb.form-no NE 0  NO-LOCK NO-ERROR.
              IF AVAIL eb  THEN   DO:
                

                IF eb.set-is-assembled = TRUE THEN  ASSIGN isset = "Yes".
                    IF eb.set-is-assembled = FALSE THEN  ASSIGN isset = "No".
                    IF eb.set-is-assembled = ? THEN  ASSIGN isset = "Yes".
                    IF fgitem = "" THEN  ASSIGN fgitem  = eb.stock-no.
                    RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).
            
            ASSIGN
               b-oe-ordl.cas-cnt      = li-cnt
               b-oe-ordl.cases-unit = li-cases 
               b-oe-ordl.partial    = b-oe-ordl.qty MOD b-oe-ordl.cas-cnt.
                END.
            END.

            
      
       
       IF fgitem = "" THEN DO:
           
           find first xeb  where xeb.company = prmComp 
               and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) and xeb.form-no  eq 0
               no-lock no-error.
            
         
           if v-est-fg THEN
                   b-oe-ordl.i-no = if vEstType eq 2 and avail xeb then
                       xeb.part-no else eb.part-no.   
             else
                 if v-est-fg1 ne "Manual" then do:
                     find first itemfg
                         where itemfg.company eq cocode
                         and itemfg.part-no eq (if vEstType eq 2 and avail xeb then
                             xeb.part-no else eb.part-no)
                         and itemfg.cust-no eq eb.cust-no
                         no-lock no-error.
                      if avail itemfg then
                          assign
                          b-oe-ordl.i-no       = itemfg.i-no
                          b-oe-ordl.part-dscr2 = itemfg.part-dscr2.
                      end.
               
               IF v-est-fg1 EQ "Hughes" THEN
                   RUN fg/hughesfg.p ((IF (vEstType EQ 2 OR vEstType EQ 6 ) AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                      OUTPUT fgitem ).
                   ELSE
                       IF v-est-fg1 EQ "Fibre" THEN
                           RUN fg/fibre-fg.p ((IF (vEstType EQ 2 OR vEstType EQ 6 ) AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                              OUTPUT fgitem).
                        
                        run crt-eb-itemfg (fgitem, prmUom) NO-ERROR.
                        
                        
                        IF (vEstType EQ 2 OR vEstType = 6) THEN DO:
                           FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 0  EXCLUSIVE-LOCK NO-ERROR.
                            IF AVAIL eb THEN DO:
                                ASSIGN
                                    eb.stock-no = fgitem .
                                
                            END.
                        END.
                        ELSE DO:
                            FIND CURRENT eb EXCLUSIVE-LOCK.
                            IF AVAIL eb THEN DO:
                                
                                ASSIGN
                                    eb.stock-no = fgitem .
                                
                             END.
                        END.
           END.

           
                   ASSIGN b-oe-ordl.i-no = fgitem.
            

        v-tmp-price = if b-oe-ordl.pr-uom begins "L" AND b-oe-ordl.pr-uom NE "LB" then
                   if b-oe-ordl.qty lt 0 then -1 else 1
                 else
                 if b-oe-ordl.pr-uom eq "CS" then
                   b-oe-ordl.qty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if b-oe-ordl.pr-uom eq "C" then
                    b-oe-ordl.qty / 100
                 else
                 if  b-oe-ordl.pr-uom eq "M" then
                    b-oe-ordl.qty / 1000
                 else
                    b-oe-ordl.qty .
                            
    lv-t-price = v-tmp-price * b-oe-ordl.price  .
    prmTPrice = (lv-t-price - ROUND(lv-t-price * cust.disc / 100,2)).
    b-oe-ordl.t-price         = prmTPrice.

       FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = b-oe-ordl.i-no  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
            ASSIGN
                itemfg.q-ono = itemfg.q-ono + prmQty 
                itemfg.q-alloc =  itemfg.q-alloc + prmQty .
                
            END.
            ASSIGN
                    oe-ord.t-weight = oe-ord.t-weight - b-oe-ordl.t-weight
                    b-oe-ordl.t-weight = ( b-oe-ordl.qty / 100 ) * itemfg.weight-100
                    oe-ord.t-weight = oe-ord.t-weight + b-oe-ordl.t-weight.

            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company   EQ b-oe-ordl.company
                AND po-ordl.i-no      EQ b-oe-ordl.i-no
                AND po-ordl.po-no     EQ b-oe-ordl.po-no-po
                AND po-ordl.item-type EQ NO
                USE-INDEX item-ordno NO-ERROR.

            IF AVAIL po-ordl THEN DO:
                ASSIGN
                lv-uom       = po-ordl.cons-uom
                b-oe-ordl.cost = po-ordl.cons-cost.
               
                
            END.
            ELSE DO:
                ASSIGN
                    lv-uom       = itemfg.prod-uom
                    b-oe-ordl.cost = itemfg.total-std-cost.
                  END.

                IF lv-uom NE "M" THEN DO:
                    RUN sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                                  b-oe-ordl.cost, OUTPUT ttcost).
                 ASSIGN
                     b-oe-ordl.cost = ttcost .
                 END.
                
                 find first xest where xest.company eq cocode and
                   xest.est-no eq lv-est-no NO-LOCK no-error.
               
               RUN itemfg-cost. 
               ASSIGN v-rel = b-oe-ordl.rel .
               RUN get-est-cost (lv-est-no).
                  

            RUN oe/ordlfrat.p (ROWID(b-oe-ordl), OUTPUT tfright ).
            
                ASSIGN
                    b-oe-ordl.t-freight = tfright
                    oe-ord.t-freight = oe-ord.t-freight + tfright.

               
                 RUN oe/ordfrate.p (ROWID(oe-ord)).
                     
                FIND xoe-ord WHERE RECID(xoe-ord) EQ save_id  NO-LOCK NO-ERROR.
                 IF AVAIL xoe-ord THEN RUN oe/oe-comm.p.
                
                 RUN oe/calcordt.p (ROWID(xoe-ord)).
                 FIND FIRST cust WHERE  cust.cust-no EQ b-oe-ordl.cust-no AND cust.company = b-oe-ordl.company USE-INDEX cust NO-LOCK NO-ERROR.
                  RUN oe/creditck.p (ROWID(cust), YES).  
                 ASSIGN  fgitem = "".


               
   END. /*for each quoteitm*/
 END. /*IF prmQuote <> 0 THEN DO:*/
 

 /******************************est not blank********************************/

IF vEstType = 2 OR  vEstType = 6  THEN DO:
IF vlog-fld = NO  THEN DO:

FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode)  NO-LOCK NO-ERROR.

    find first xest where xest.company eq cocode
                  and xest.est-no  =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
                NO-LOCK no-error.
    
 assign
   j         = 1
   v-job-no  = if oe-ord.job-no <> "" then oe-ord.job-no else string(oe-ord.ord-no)
   v-job-no2 = oe-ord.job-no2  .
  
 
  find first xeb
      where xeb.company = cocode 
        and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
        and xeb.form-no  eq 0
      no-lock no-error.     

      
     IF vEstType eq 2 or vEstType eq 6 then do:
       s-est-no = oe-ord.est-no.
       run oe/fgadd2.p.   /** 2pc box fg create/update routine **/
     END. 
  END.

  /*************vlog-fld = yes and isset = "yes" *******/
 
IF vlog-fld = YES  THEN DO:

FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode)  NO-LOCK NO-ERROR.

    find first xest where xest.company eq cocode
                  and xest.est-no  =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
                NO-LOCK no-error.
    IF AVAIL xest THEN
        v-est-type = xest.est-type - IF xest.est-type GT 4 THEN 4 ELSE 0 .
    
 assign
   j         = 1
   v-job-no  = if oe-ord.job-no <> "" then oe-ord.job-no else string(oe-ord.ord-no)
   v-job-no2 = oe-ord.job-no2  .
  
 
  FIND FIRST xeb
      where xeb.company = cocode 
        and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
        and xeb.form-no  NE 0
      no-lock no-error.
      
     IF vEstType eq 2 or vEstType eq 6 then do:
       ASSIGN  s-est-no = oe-ord.est-no.
       run oe/fgadd2.p.   /** 2pc box fg create/update routine **/
     END.
    
  END.


  /**********************************/

 
IF vlog-fld = YES  THEN DO:
  IF prmEst <> "" AND isset = "No"  THEN  DO:
   
    ASSIGN
        lv-qty = prmQty.
    
     FIND FIRST ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode)  NO-LOCK NO-ERROR.

     FIND FIRST quotehd WHERE quotehd.q-no = prmquote AND quotehd.company = prmComp  NO-LOCK NO-ERROR.
          FIND FIRST  quoteitm  WHERE quoteitm.q-no = quotehd.q-no NO-LOCK NO-ERROR.

    find first xest where xest.company eq cocode
                  and xest.est-no  =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
                NO-LOCK no-error.
    ASSIGN  v-est-run = xest.est-type - if xest.est-type gt 4 then 4 else 0.

        /* RECALC ESTIMATE */
         save_id = RECID(oe-ord).
        
       IF v-est-run EQ 3 OR v-est-run EQ 4 THEN  /* not done ??? */
        RUN VALUE(ENTRY(v-est-run + INT(xest.est-type EQ 8),v-run-list)).


 assign
   j         = 1
   v-job-no  = if oe-ord.job-no <> "" then oe-ord.job-no else string(oe-ord.ord-no)
   v-job-no2 = oe-ord.job-no2 .
   /*fil_id    = save_id. */       /* reset fil_id, scrambled in calc...*/
 
  find first xeb
      where xeb.company = cocode 
        and xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
        and xeb.form-no  eq 0
      no-lock no-error.     


  for each eb
      where eb.company = prmComp 
        and eb.est-no  eq xest.est-no
        and eb.form-no  ne 0
        and eb.blank-no ne 0
        AND TRIM(eb.cust-no) NE ""
      no-lock,
      FIRST ef OF eb NO-LOCK,
      FIRST cust NO-LOCK
      where (cust.company = cocode)
        AND cust.cust-no eq eb.cust-no
      USE-INDEX cust    
      break by eb.est-no by eb.cust-no by eb.form-no by eb.blank-no
      TRANSACTION:

      
        FIND xoe-ord WHERE RECID(xoe-ord) EQ save_id .
        
       find first oe-ordl where oe-ordl.company  eq cocode
          and oe-ordl.ord-no   eq xoe-ord.ord-no
          and oe-ordl.cust-no  eq xoe-ord.cust-no
          and (oe-ordl.part-no eq eb.part-no
           or  (oe-ordl.i-no   eq eb.stock-no and eb.stock-no ne ""))
          and (v-est-type eq 4 or
               can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)))
        no-error.
   
    
    if not avail oe-ordl                                                    or
       can-find(first tt-oe-ordl where tt-oe-ordl.row-id eq rowid(oe-ordl)) then do:
      if not avail oe-ordl then do:
        li-line-no = 1.
        FOR EACH oe-ordl
            WHERE oe-ordl.company EQ xoe-ord.company
              AND oe-ordl.ord-no  EQ xoe-ord.ord-no
            BY oe-ordl.line DESC:
          li-line-no = oe-ordl.line + 1.
          LEAVE.
        END.
        create oe-ordl.
        assign
         oe-ordl.company    = cocode
         oe-ordl.ord-no     = xoe-ord.ord-no  /* input screen-value */
         oe-ordl.line       = li-line-no
         oe-ordl.po-no      = xoe-ord.po-no
         oe-ordl.job-no     = xoe-ord.job-no
         oe-ordl.job-no2    = xoe-ord.job-no2
         oe-ordl.req-code   = xoe-ord.due-code
         oe-ordl.prom-code  = xoe-ord.due-code
         oe-ordl.req-date   = xoe-ord.due-date
         oe-ordl.prom-date  = xoe-ord.due-date
         oe-ordl.i-no       = if v-est-type eq 2 and avail xeb then
                                xeb.stock-no else eb.stock-no
         oe-ordl.qty        = if v-est-type eq 3 or v-est-type eq 4  THEN eb.bl-qty ELSE (lv-qty * eb.yld-qty) 
         v-qty-mod          = YES
         oe-ordl.over-pct   = xoe-ord.over-pct
         oe-ordl.under-pct  = xoe-ord.under-pct
         oe-ordl.q-no          = prmQuote
         .

        IF oe-ordl.i-no EQ "0" THEN oe-ordl.i-no = "".

        if xoe-ord.est-no ne "" then
          assign
           oe-ordl.est-no = xoe-ord.est-no
           oe-ordl.pr-uom = "M".

        do i = 1 to 3:
          assign oe-ordl.s-man[i] = xoe-ord.sman[i]
                 oe-ordl.s-pct[i] = xoe-ord.s-pct[i]
                 oe-ordl.s-comm[i] = xoe-ord.s-comm[i].
        end.

        IF v-foamdate-log                                         AND
           CAN-FIND(FIRST style WHERE style.company EQ eb.company
                                  AND style.style   EQ eb.style
                                  AND style.type    EQ "F")       THEN
          oe-ordl.req-date = xoe-ord.ord-date + v-foamdate-int.
      end.  /* not avail oe-ordl */

     
      /*IF ll-new-fg THEN*/ ASSIGN  oe-ordl.i-no = eb.stock-no.

     
      if oe-ordl.i-no eq "" THEN DO:
       /* if v-est-fg THEN
          oe-ordl.i-no = if v-est-type eq 2 and avail xeb then
                           xeb.part-no else eb.part-no.   

        else*/
        if v-est-fg1 ne "Manual" then do:
          find first itemfg
              where itemfg.company eq cocode
                and itemfg.part-no eq (if v-est-type eq 2 and avail xeb then
                                         xeb.part-no else eb.part-no)
                and itemfg.cust-no eq eb.cust-no
              no-lock no-error.
          if avail itemfg then
            assign
             oe-ordl.i-no       = itemfg.i-no
             oe-ordl.part-dscr2 = itemfg.part-dscr2.
        end.
        
        IF v-est-fg1 EQ "Hughes" THEN  RUN fg/hughesfg.p ( ROWID(eb), OUTPUT fgitem).
        ELSE
        IF v-est-fg1 EQ "Fibre" THEN  RUN fg/fibre-fg.p ( ROWID(eb), OUTPUT fgitem).
             
           ASSIGN
              oe-ordl.i-no   = fgitem .
                
            END.
      END.

     
       /*  FIND FIRST itemfg WHERE itemfg.company = prmComp AND itemfg.i-no = oe-ordl.i-no EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:*/
               
               /* ASSIGN
                    itemfg.q-ono = itemfg.q-ono + prmQty . 
                    itemfg.q-alloc =  itemfg.q-alloc + oe-ordl.qty  .*/


      ASSIGN
       oe-ordl.i-name     = if v-est-type eq 2 and avail xeb then
                              xeb.part-dscr1 else eb.part-dscr1
       oe-ordl.part-no    = if v-est-type eq 2 and avail xeb then
                              xeb.part-no else eb.part-no
       oe-ordl.part-dscr1 = if v-est-type eq 2 and avail xeb then
                                xeb.part-dscr2 else eb.part-dscr2
       oe-ordl.est-type   = eb.est-type
       oe-ordl.form-no    = eb.form-no
       oe-ordl.blank-no   = eb.blank-no
       oe-ordl.cust-no    = xoe-ord.cust-no
       oe-ordl.disc       = cust.disc
       oe-ordl.tax        = cust.sort EQ "Y" AND xoe-ord.tax-gr NE "".

      {custom/shptotax.i xoe-ord.cust-no xoe-ord.sold-id oe-ordl.tax}

      RUN est/getcscnt.p ((IF xest.est-type EQ 6 AND
                              AVAIL xeb          AND
                              xeb.cas-no NE ""   THEN ROWID(xeb) ELSE ROWID(eb)),
                         OUTPUT oe-ordl.cas-cnt,OUTPUT oe-ordl.cases-unit).
                   
      ASSIGN
       oe-ordl.cases      = TRUNC(oe-ordl.qty / oe-ordl.cas-cnt,0)
       oe-ordl.partial    = oe-ordl.qty MOD oe-ordl.cas-cnt.

      IF xest.est-type EQ 6 AND
         AVAIL xeb          AND
         xeb.pur-man        THEN
        ASSIGN
         /*oe-ordl.cases-unit = xeb.cas-pal task# 05010610*/
         oe-ordl.unit-count = xeb.tr-cnt.
      ELSE
        ASSIGN
         /*oe-ordl.cases-unit = eb.cas-pal   task# 05010610*/
         oe-ordl.unit-count = eb.tr-cnt.

      find first itemfg where itemfg.company eq cocode
                          and itemfg.i-no    eq oe-ordl.i-no
          no-lock no-error.  
      IF NOT AVAIL itemfg THEN 
                run crt-itemfg (fgitem, prmUom) NO-ERROR.
      if avail itemfg then 
        assign
         oe-ordl.price      = itemfg.sell-price
         oe-ordl.pr-uom     = itemfg.sell-uom
         oe-ordl.part-dscr2 = itemfg.part-dscr2.

                            
      oe-ordl.type-code = 
            STRING(AVAIL itemfg AND
                   CAN-FIND(FIRST b-oe-ordl
                            WHERE b-oe-ordl.company EQ itemfg.company
                              AND b-oe-ordl.i-no    EQ itemfg.i-no
                              AND b-oe-ordl.ord-no  LT oe-ordl.ord-no
                              AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)),"R/O").

      if v-est-type eq 8 or v-est-type eq 4 then do:
        find first blk where blk.id eq eb.part-no no-lock no-error.           
        if avail blk then do:
          if v-est-type eq 4 OR v-est-type eq 8 then do:
             ASSIGN
                v-blk-qty = 0
                v-tot-comm-2 = 0
                v-blk-qty-2 = 0.
            
            for each blk where blk.id eq eb.part-no no-lock,
                first xjob
                where xjob.form-no  eq blk.snum
                  and xjob.blank-no eq blk.bnum:
              
              IF v-full-cost AND eb.est-type EQ 8 THEN
              DO:
                 IF v-est-fg1 EQ "Fibre" THEN
                    RUN est/usemargin.p (ROWID(xest), OUTPUT ll-use-margin).
                
                 ASSIGN
                    lv-sell-by = ce-ctrl.sell-by
                    lv-sell-by-ce-ctrl = ce-ctrl.sell-by
                    v-pct-2 = ce-ctrl.prof-mrkup
                    v-probe-comm = eb.comm.
                
                 RUN custom/combasis.p (cocode,
                                        eb.sman,
                                        cust.type,
                                        eb.procat,
                                        0,
                                        cust.cust-no,
                                        OUTPUT v-basis).
                
                 IF cust.markup NE 0 THEN
                    v-pct-2 = cust.markup.
                
                 IF NOT cecomm-log THEN v-probe-comm = 0.
                
                 IF ll-use-margin THEN                 /* Get Margin% */
                    RUN est/getsmanmtrx.p (ROWID(xest), "M",
                                           INPUT-OUTPUT v-probe-comm,
                                           INPUT-OUTPUT v-mp).
                
                 RUN custom/markup.p (ROWID(xeb),
                                      INPUT-OUTPUT lv-sell-by,
                                      INPUT-OUTPUT v-pct-2).
                
                 IF ll-use-margin THEN
                    v-pct-2 = v-mp.
                
                 v-qty-2 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
                
                 IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN DO:
                    ASSIGN
                       v-board-cst = 0
                       t-blkqty = 0.
                
                    FOR EACH b-eb2 fields(form-no yrprice yld-qty bl-qty) NO-LOCK WHERE
                        b-eb2.company EQ xest.company AND
                        b-eb2.est-no  EQ xest.est-no AND
                        b-eb2.form-no EQ eb.form-no:
                        /* set total # of blanks on all forms */
                
                        t-blkqty[b-eb2.form-no] = t-blkqty[b-eb2.form-no] +
                                                  if b-eb2.yrprice THEN b-eb2.yld-qty ELSE b-eb2.bl-qty.
                    END.
                
                    FOR EACH b-blk WHERE b-blk.id EQ xeb.part-no,
                        FIRST b-ef2 NO-LOCK
                        WHERE b-ef2.company EQ xest.company
                          AND b-ef2.est-no  EQ xest.est-no
                          AND b-ef2.form-no EQ b-blk.snum,
                        EACH brd WHERE brd.form-no EQ b-ef2.form-no:
                   
                        v-board-cst = v-board-cst + (brd.cost-m * b-blk.pct * (t-blkqty[b-ef2.form-no] / 1000)).
                    END.
                   
                    v-board-cst = v-board-cst / (v-qty-2 / 1000).
                 END.
                
                 RUN custom/sellpric.p (lv-sell-by-ce-ctrl,
                                        lv-sell-by,
                                        v-basis,
                                        (IF lv-sell-by-ce-ctrl NE "B" AND
                                            lv-sell-by EQ "B" THEN v-board-cst
                                         ELSE blk.fact / (v-qty-2 / 1000)),
                                        (IF lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B" THEN 0
                                         ELSE (blk.cost / (v-qty-2 / 1000)) - (blk.fact / (v-qty-2 / 1000))),
                                        (IF ll-use-margin OR
                                            (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN 0
                                         ELSE v-comm-2),
                                        v-pct-2,
                                        OUTPUT v-sell-price,
                                        OUTPUT v-comm-2).
                 IF ll-use-margin OR
                    (lv-sell-by-ce-ctrl NE "B" AND lv-sell-by EQ "B") THEN
                    v-comm-2 = v-sell-price * v-probe-comm / 100.
              END. /*v-full-cost AND eb.est-type EQ 8*/
              ELSE
                  v-comm-2 = 0.

              assign
               v-tot-comm-2 = v-tot-comm-2 + v-comm-2
               oe-ordl.t-weight  = oe-ordl.t-weight + blk.fg-wt
               oe-ordl.t-freight = oe-ordl.t-freight +
                                     (blk.fg-wt$ * (blk.fg-wt / 100))
               v-blk-qty           = v-blk-qty + blk.qyld.
               
              IF eb.est-type EQ 4 OR eb.est-type EQ 8 THEN
                  v-blk-qty-2 = v-blk-qty-2 + (IF eb.yrprice THEN blk.qyld ELSE blk.qreq).
       
              if v-full-cost then
                 oe-ordl.cost = oe-ordl.cost + blk.cost.
               ELSE
               DO:
                 IF eb.est-type EQ 4 THEN
                 DO:
                    ASSIGN
                       blk-fact = 0
                       v-qty-3 = IF eb.yrprice THEN blk.qyld ELSE blk.qreq.
      
                    FOR EACH bf-blk:
                        blk-fact = blk-fact + bf-blk.fact.
                    END. 
      
                    v-cost-2 = blk.fact * (fac-tot / blk-fact).
                 END.
                 ELSE
                    oe-ordl.cost = oe-ordl.cost +
                                  ((xjob.lab + xjob.mat + xjob.voh + xjob.foh) *
                                    v-qty-3 / 1000).
               END.
             end.
      
             IF eb.est-type EQ 4 AND v-full-cost EQ NO THEN
                oe-ordl.cost = oe-ordl.cost + (v-cost-2 / (v-blk-qty-2 / 1000)).
             ELSE
             IF eb.est-type EQ 8 THEN
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty-2 / 1000)) + v-tot-comm-2.
             ELSE
                oe-ordl.cost = (oe-ordl.cost / (v-blk-qty / 1000)) + v-tot-comm-2.
       
           end.

          else do:
            assign
             oe-ordl.t-weight  = blk.fg-wt
             oe-ordl.t-freight = blk.fg-wt$ * (blk.fg-wt / 100).
             
            if v-full-cost then
              oe-ordl.cost = blk.cost -
                               (((blk.fg-wt / 100) * blk.fg-wt$)
                                  * (blk.qyld / xest.est-qty[1])).
                                   
            else do:
              find first xjob no-error.
              if avail xjob then
                oe-ordl.cost = xjob.lab + xjob.mat + xjob.voh + xjob.foh.
            end.  
          end.
        end.  /* avail blk */

        else do:
          message "NO BLANK AVAILABLE!!!" eb.part-no.
          hide message.
        end.
      end.  /* est-type = 3 or 4 */
    end.

    if avail xest and v-quo-price-log then do:
      run oe/getqpric.p (recid(xest), oe-ordl.part-no, "", oe-ordl.qty,
                              input-output oe-ordl.price,
                              input-output oe-ordl.pr-uom,
                              OUTPUT lv-q-no).
      IF lv-q-no NE 0 THEN DO:
        RUN oe/ordlq-no.p (ROWID(oe-ordl), lv-q-no).
        FIND CURRENT oe-ordl NO-ERROR.
      END.
    END.

    oe-ordl.t-price = oe-ordl.price * oe-ordl.qty /
                      (IF oe-ordl.pr-uom EQ "C" THEN 100  ELSE
                       IF oe-ordl.pr-uom EQ "M" THEN 1000 ELSE 
                       IF oe-ordl.pr-uom = "L" THEN oe-ordl.qty ELSE 1).

    {oe/defwhsed.i oe-ordl}

    FIND FIRST tt-oe-ordl WHERE tt-oe-ordl.row-id EQ ROWID(oe-ordl) NO-ERROR.
    IF AVAIL tt-oe-ordl THEN tt-oe-ordl.to-be-deleted = NO.
         
    if v-est-type eq 2 then leave. /** 2pc box & Set headers **/

    IF oeestcom-log = YES THEN        
    DO:
       RELEASE probe.

       FIND FIRST sman WHERE
            sman.company EQ eb.company AND
            sman.sman EQ eb.sman
            NO-LOCK NO-ERROR.

       IF AVAIL sman AND sman.commbasis EQ "M" THEN
       DO:
          /*DEF VAR v-price-per-1000 AS DEC NO-UNDO.*/
          /*DEF VAR v-tmp-price AS DEC NO-UNDO.*/

          IF oe-ordl.pr-uom NE "M" THEN
          DO:
             FIND FIRST itemfg WHERE
                  itemfg.company EQ cocode AND
                  itemfg.i-no EQ oe-ordl.i-no
                  NO-LOCK NO-ERROR.

             assign
              v-tmp-price = if oe-ordl.pr-uom begins "L" AND
                               oe-ordl.pr-uom NE "LB" then
                               if oe-ordl.qty lt 0 then -1
                               else 1
                            else
                            if oe-ordl.pr-uom eq "CS" then
                               oe-ordl.qty / (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                                  if avail itemfg and itemfg.case-count ne 0 THEN
                                           itemfg.case-count ELSE 1)
                             else
                             if oe-ordl.pr-uom eq "C" then
                                oe-ordl.qty / 100
                             else
                                oe-ordl.qty
                  
              v-tmp-price = v-tmp-price * oe-ordl.price
              v-price-per-1000 = v-tmp-price / ( oe-ordl.qty / 1000).
          END.
          ELSE
             v-price-per-1000 = oe-ordl.price.
              
          IF NOT(xest.est-type EQ 4 OR xest.est-type EQ 8) THEN
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company and
                 probe.est-no = xest.est-no and
                 probe.probe-date ne ? and
                 probe.est-qty eq INT(oe-ordl.qty) AND
                 probe.sell-price EQ v-price-per-1000
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 LEAVE.
             END.
            
             IF NOT AVAIL probe THEN
                FOR EACH probe WHERE
                    probe.company = xest.company and
                    probe.est-no = xest.est-no and
                    probe.probe-date ne ? and
                    probe.est-qty eq INT(oe-ordl.qty)
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END.
          END.
          ELSE
          DO:
             FOR EACH probe WHERE
                 probe.company = xest.company and
                 probe.est-no = xest.est-no and
                 probe.probe-date ne ? and
                 probe.sell-price EQ v-price-per-1000
                 NO-LOCK
                 BY probe.probe-date DESC
                 BY probe.probe-time DESC:
                 
                 LEAVE.
             END.
            
             IF NOT AVAIL probe THEN
                FOR EACH probe WHERE
                    probe.company = xest.company and
                    probe.est-no = xest.est-no and
                    probe.probe-date ne ?
                    NO-LOCK
                    BY probe.probe-date DESC
                    BY probe.probe-time DESC:
                    
                    LEAVE.
                END.
          END.

          IF AVAIL probe THEN
          DO:
             ASSIGN
                oe-ordl.s-comm[1] = probe.comm
                xoe-ord.s-comm[1] = oe-ordl.s-comm[1].
         
             RELEASE probe.
          END.
       END.
    
   

  end. /* each eb */
 FIND CURRENT xoe-ord EXCLUSIVE-LOCK .
   FOR EACH oe-ordl OF xoe-ord EXCLUSIVE-LOCK
          WHERE oe-ordl.est-no NE "" 
            AND NOT oe-ordl.is-a-component  , 
          EACH eb EXCLUSIVE
          WHERE eb.company EQ xoe-ord.company
            AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
            AND ((eb.cust-no EQ xoe-ord.cust-no AND
                  eb.part-no EQ oe-ordl.part-no) /*OR
                 eb.est-type EQ 2 OR
                 eb.est-type EQ 6*/  )
          TRANSACTION:
            
        IF AVAIL eb AND AVAIL oe-ordl  THEN DO:
            
          ASSIGN
          
           eb.stock-no   = oe-ordl.i-no.
         
          ASSIGN
               eb.ord-no = oe-ordl.ord-no. 
        END.
        
        RELEASE eb.
        
        ASSIGN  fil_id = recid(oe-ordl). 
        /*run oe/ordlup.p.  */

      END. /* each oe-ordl */
       
END.  /*end or prmEst*/
END. /* end oe    */
 END.  /*  est-type = 6 or 2 */

 FIND CURRENT oe-ord EXCLUSIVE-LOCK .
   FOR EACH oe-ordl OF oe-ord EXCLUSIVE-LOCK
          WHERE oe-ordl.est-no NE "" 
            AND NOT oe-ordl.is-a-component  , 
          EACH eb EXCLUSIVE
          WHERE eb.company EQ oe-ord.company
            AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
            AND ((eb.cust-no EQ oe-ord.cust-no AND
                  eb.part-no EQ oe-ordl.part-no) /*OR
                 eb.est-type EQ 2 OR
                 eb.est-type EQ 6*/  )
          TRANSACTION:
            
        IF AVAIL eb AND AVAIL oe-ordl  THEN DO:
        FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)
            AND est.company = oe-ord.company EXCLUSIVE-LOCK NO-ERROR .

          ASSIGN
               eb.ord-no  = oe-ordl.ord-no
               est.ord-no = oe-ordl.ord-no
               . 
        END.
        
        RELEASE eb.
        
       
      END. /* each oe-ordl */
      FIND FIRST oe-ctrl  
     WHERE oe-ctrl.company EQ cocode 
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

   /* If update is not possible, next session that succeeds will update */
   /* with the next available number                                    */
   IF AVAIL oe-ctrl THEN DO:
     oe-ctrl.n-ord = oe-ord.ord-no + 1.
     RELEASE oe-ctrl.
   END.

 ASSIGN prmOrderNum = Ordernum 
     prmAction = "Select".
END.   /*IF prmAction = "Add" THEN DO:*/
/***************************************************************************************************************/
 
IF prmAction = "Update" THEN DO:
    FIND FIRST oe-ord WHERE
         oe-ord.company = prmComp AND
         oe-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
         FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = prmCustomer NO-LOCK NO-ERROR.
      
    IF AVAIL oe-ord THEN DO:
        ASSIGN
            oe-ord.cust-no           = prmCustomer  
            oe-ord.user-id           = prmUser 
            /*oe-ord.est-no            = prmEst
            oe-ord.q-no              = prmquote*/
           /* oe-ord.stat              = prmStat*/
            oe-ord.sold-id           = prmSold 
            oe-ord.type              = prmType
            oe-ord.ord-date          = prmOrdate
            oe-ord.sold-name         = prmCustName 
            oe-ord.due-code          = prmDueCode    
            oe-ord.due-date          = prmDueDate             
            oe-ord.addr[1]           = prmCustAddr   
            oe-ord.sold-addr[1]      = prmSoldAddr 
           /* oe-ord.last-date         = /*prmLastDate*/ (prmOrdate + cust.ship-days) */  
            oe-ord.addr[2]           = prmCustAddr2     
            oe-ord.sold-addr[2]      = prmSoldAddr2 
            oe-ord.prod-date         = DATE(prmProdDate)
            oe-ord.city              = prmCity
            oe-ord.state             = prmState
            oe-ord.zip               = prmZip
            oe-ord.sold-city         = prmSoldCity
            oe-ord.sold-state        = prmSoldState
            oe-ord.sold-zip          = prmSoldZip
            oe-ord.po-no             = prmPonum
            oe-ord.contact           = prmContact
            oe-ord.over-pct          = prmOverpct
            oe-ord.under-pct         = prmUnderpct
            oe-ord.terms             = prmTerms
            oe-ord.terms-d           = prmTermdscr   
            oe-ord.pord-no           = prmProd    
            oe-ord.tax-gr            = prmTaxgr    
            oe-ord.frt-pay           = prmFreight    
            oe-ord.carrier           = prmCarrier    
            oe-ord.fob-code          = prmFob    
            oe-ord.sman[1]           = prmSman    
            oe-ord.sname[1]          = prmSname    
            oe-ord.sman[2]           = prmSman2    
            oe-ord.sname[2]          = prmSname2    
            oe-ord.sman[3]           = prmSman3  
            oe-ord.sname[3]          = prmSname3  
            oe-ord.cc-type           = prmCtype  
            oe-ord.cc-expiration     = DATE(prmcExp)
            oe-ord.cc-num            = prmCnum  
            oe-ord.cc-auth           = prmCauth
            oe-ord.cust-name         = prmCustName
            oe-ord.whsed             = IF prmWhis = "yes" THEN TRUE ELSE FALSE
            oe-ord.s-pct[1]          = prmSales1
            oe-ord.s-pct[2]          = prmSales2
            oe-ord.s-pct[3]          = prmSales3
            oe-ord.s-comm[1]         = prmComm1
            oe-ord.s-comm[2]         = prmComm2
            oe-ord.s-comm[3]         = prmComm3 .   
            IF(oe-ord.cc-type = "") THEN
            ASSIGN oe-ord.cc-expiration  = ? .
            
       END.  /*if  avail oe-ordl*/ 
    ASSIGN prmAction = "Select".
 END.  /*IF prmAction = "Update" THEN DO: */
/*************************************************************************************************/
IF prmAction = "Select" THEN DO:
    FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = prmCustomer NO-LOCK NO-ERROR.
    FIND FIRST oe-ord WHERE oe-ord.company = prmComp 
                        AND oe-ord.ord-no =  prmOrderNum  NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN DO:
        CREATE ttOrderEstUpdate.
        ASSIGN
            ttOrderEstUpdate.VOrderNum      = oe-ord.ord-no
            ttOrderEstUpdate.VEstimate      = oe-ord.est-no
            ttOrderEstUpdate.VJob           = oe-ord.job-no 
            ttOrderEstUpdate.VJob2          = oe-ord.job-no2
            ttOrderEstUpdate.VCustomer      = oe-ord.cust-no
            ttOrderEstUpdate.VUserid        = oe-ord.user-id
            ttOrderEstUpdate.VStat          = oe-ord.stat
            ttOrderEstUpdate.VSold          = oe-ord.sold-id
            ttOrderEstUpdate.VOrdate        = oe-ord.ord-date
            ttOrderEstUpdate.VCustName      = oe-ord.cust-name
            ttOrderEstUpdate.VSoldName      = oe-ord.sold-name   
            ttOrderEstUpdate.VDueCode       = oe-ord.due-code    
            ttOrderEstUpdate.VDueDate       = oe-ord.due-date    
            ttOrderEstUpdate.VCustAddr      = oe-ord.addr[1]     
            ttOrderEstUpdate.VSoldAddr      = oe-ord.sold-addr[1]
            ttOrderEstUpdate.VLastDate      = oe-ord.last-date   
            ttOrderEstUpdate.VcustAddr2     = oe-ord.addr[2]     
            ttOrderEstUpdate.VSoldAddr2     = oe-ord.sold-addr[2]
            ttOrderEstUpdate.VProdDate      = oe-ord.prod-date   
            ttOrderEstUpdate.VCity          = oe-ord.city        
            ttOrderEstUpdate.VState         = oe-ord.state       
            ttOrderEstUpdate.VZip           = oe-ord.zip         
            ttOrderEstUpdate.VSoldCity      = oe-ord.sold-city   
            ttOrderEstUpdate.VSoldState     = oe-ord.sold-state  
            ttOrderEstUpdate.VSoldZip       = oe-ord.sold-zip    
            ttOrderEstUpdate.VPonum         = oe-ord.po-no       
            ttOrderEstUpdate.VContact       = oe-ord.contact     
            ttOrderEstUpdate.VOverpct       = oe-ord.over-pct    
            ttOrderEstUpdate.VUnderpct      = oe-ord.under-pct   
            ttOrderEstUpdate.VTerms         = oe-ord.terms       
            ttOrderEstUpdate.VTermdscr      = oe-ord.terms-d     
            ttOrderEstUpdate.VProd          = oe-ord.pord-no     
            ttOrderEstUpdate.VTaxgr         = oe-ord.tax-gr      
            ttOrderEstUpdate.VFreight       = oe-ord.frt-pay     
            ttOrderEstUpdate.VCarrier       = oe-ord.carrier     
            ttOrderEstUpdate.VFob           = oe-ord.fob-code    
            ttOrderEstUpdate.VSman          = oe-ord.sman[1]   
            ttOrderEstUpdate.VSname         = oe-ord.sname[1] 
            ttOrderEstUpdate.VSpct          = oe-ord.s-pct[1]          
            ttOrderEstUpdate.VScomm         = oe-ord.s-comm[1]         
            ttOrderEstUpdate.VSman2         = oe-ord.sman[2]           
            ttOrderEstUpdate.VSname2        = oe-ord.sname[2]          
            ttOrderEstUpdate.VSpct2         = oe-ord.s-pct[2]         
            ttOrderEstUpdate.VScomm2        = oe-ord.s-comm[2]         
            ttOrderEstUpdate.VSman3         = oe-ord.sman[3]          
            ttOrderEstUpdate.VSname3        = oe-ord.sname[3]         
            ttOrderEstUpdate.VSpct3         = oe-ord.s-pct[3]         
            ttOrderEstUpdate.VScomm3        = oe-ord.s-comm[3]        
            ttOrderEstUpdate.VCtype         = oe-ord.cc-type         
            ttOrderEstUpdate.VcExp          = oe-ord.cc-expiration    
            ttOrderEstUpdate.VCnum          = oe-ord.cc-num
            ttOrderEstUpdate.VCauth         = oe-ord.cc-auth
            ttOrderEstUpdate.ordtype        = oe-ord.type
            ttOrderEstUpdate.VWhis          = oe-ord.whsed 
            ttOrderEstUpdate.vRfq           = oe-ord.q-no 
            ttOrderEstUpdate.vRecKey        = oe-ord.rec_key 
            .

        IF  oe-ord.frt-pay  = "P" THEN
            ttOrderEstUpdate.VFreightdscr        = "Prepaid".
        ELSE IF oe-ord.frt-pay  = "C" THEN 
            ttOrderEstUpdate.VFreightdscr        = "Collect".
        ELSE IF oe-ord.frt-pay  = "B" THEN
            ttOrderEstUpdate.VFreightdscr       = "Bill".
        ELSE IF oe-ord.frt-pay  = "T" THEN
            ttOrderEstUpdate.VFreightdscr        = "3rd Party".
        IF  oe-ord.fob-code  = "DEST" THEN
            ttOrderEstUpdate.VFob        = "Destination".
        ELSE IF oe-ord.fob-code  = "ORIG" THEN 
            ttOrderEstUpdate.VFob       = "Origin".



        FOR EACH oe-ordl WHERE oe-ordl.ord-no EQ oe-ord.ord-no AND oe-ordl.company = prmComp NO-LOCK:
            FIND FIRST eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) + TRIM(oe-ordl.est-no) AND eb.company = prmComp NO-LOCK NO-ERROR.
            IF AVAIL eb AND ((eb.est-type EQ 2) OR (eb.est-type EQ 6)) THEN DO:
                ASSIGN  ttOrderEstUpdate.vLineTotal = 1.
                RETURN NO-APPLY.
            END.

            ttOrderEstUpdate.vLineTotal        = ttOrderEstUpdate.vLineTotal + 1. 
        END.
    END.
        
        
END.  /*if prmAction*/
 
/****************************************************************/ 
PROCEDURE valid-po-no :

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER cust-po-mand FOR reftable.
  FIND FIRST cust NO-LOCK
        WHERE cust.company EQ prmComp
          AND cust.cust-no EQ prmCustomer
          AND CAN-FIND(FIRST cust-po-mand
                       WHERE cust-po-mand.reftable EQ "cust.po-mand"
                         AND cust-po-mand.company  EQ cust.company
                         AND cust-po-mand.loc      EQ ""
                         AND cust-po-mand.code     EQ cust.cust-no
                         AND cust-po-mand.val[1]   EQ 1)
        NO-ERROR.
    
    IF AVAIL cust AND TRIM(prmPonum) EQ "" THEN DO:
      ASSIGN cError = "PO# is mandatory for this Customer...".
      RETURN .
    END.

    IF NOT ll-valid-po-no AND oeprompt AND prmPonum NE "" THEN
    FIND FIRST b-oe-ordl
        WHERE b-oe-ordl.company EQ oe-ord.company
          AND b-oe-ordl.po-no   EQ prmPonum
          AND b-oe-ordl.cust-no EQ prmCustomer
          AND b-oe-ordl.ord-no  NE prmOrderNum
        NO-LOCK NO-ERROR.

    IF AVAIL b-oe-ordl THEN DO:
      ASSIGN cError = "Customer PO already exists for Order/Item - " .
          /*+ 
              TRIM(STRING(b-oe-ordl.ord-no,">>>>>>>>")) + "/" +
              TRIM(b-oe-ordl.i-no) " ." SKIP
              "Do you want to continue?".*/
     
        RETURN .
      END.
      ELSE ll-valid-po-no = YES.
  
END PROCEDURE.


/*******************************************************************************************************/



PROCEDURE valid-est-no :
    Custnum = "0" .
    DEFINE VAR vEst AS CHAR.
    ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) .
    
IF prmEst <> "" THEN DO:
      FIND FIRST est
          WHERE est.company = prmComp  AND
             est.est-no  = vEst   NO-LOCK NO-ERROR.
        IF NOT AVAIL est THEN DO:
        ASSIGN cError = "Invalid Estimate#, try help...".
        RETURN .
      END.
    
    IF v-quo-price-log AND v-quo-price-dec EQ 1 THEN DO:
        FOR EACH quotehd
            WHERE quotehd.company eq prmComp
              AND quotehd.loc     eq locode
              AND quotehd.est-no  eq vEst
             NO-LOCK,
             EACH quoteitm OF quotehd NO-LOCK,
             EACH quoteqty OF quoteitm NO-LOCK:
          LEAVE.
        END.
              
        IF NOT AVAIL quoteqty THEN DO:
          cError = "No quotes exists for this estimate...".
          RETURN .
        END.
      END.
END.
END PROCEDURE.

  /******************************************************************************PROCEDURE default-type *************************************************/
  PROCEDURE default-type :
      DEF PARAM BUFFER io-itemfg FOR itemfg.
          DEF BUFFER def-oe-ordl FOR oe-ordl.

              IF b-oe-ordl.type-code NE "T"  THEN DO:
                  b-oe-ordl.type-code = "O".
              END.
  END PROCEDURE.
  /* --------------------------------------------------------------------------PROCEDURE ctr-itemfg:----------------------------------------------------------- */
  


PROCEDURE crt-itemfg :

def input parameter v-item like itemfg.i-no.
def input parameter v-uom like itemfg.prod-uom.

def var tmpstore as cha no-undo.
def var i as int no-undo.
/*DEF VAR ll-one-part AS LOG NO-UNDO.*/
{ce/msfcalc.i}
DEF VAR v-FGFreightClass AS LOG NO-UNDO.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "FGFreightClass"
                    NO-LOCK NO-ERROR.


IF AVAIL sys-ctrl THEN
   v-FGFreightClass = sys-ctrl.log-fld.
ELSE
   v-FGFreightClass = NO.
   RELEASE sys-ctrl.

/*DEF BUFFER x-eb FOR eb.*/
DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER b-eb2 FOR eb.

DEF BUFFER bf-itemfg FOR itemfg.

    FIND FIRST xeb WHERE xeb.company = prmComp AND xeb.est-no = eb.est-no AND 
        xeb.form-no = eb.form-no AND xeb.blank-no = eb.blank-no NO-LOCK NO-ERROR.
 
{sys/inc/setprint.i}
       
find first cust  where cust.company eq prmComp
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ prmComp
      AND sys-ctrl.NAME EQ "FGMASTER" NO-ERROR.
IF AVAIL sys-ctrl THEN
  FIND FIRST bf-itemfg NO-LOCK
      WHERE bf-itemfg.company EQ sys-ctrl.company
        AND bf-itemfg.i-no EQ trim(sys-ctrl.char-fld) NO-ERROR.


create itemfg.
assign
 itemfg.company    = prmComp
 itemfg.loc        = g_loc
 itemfg.i-no       = v-item
 itemfg.i-name     = oe-ordl.i-name
 itemfg.part-dscr1 = oe-ordl.part-dscr1
 itemfg.part-dscr2 = oe-ordl.part-dscr2
 itemfg.sell-price = dec(oe-ordl.price)
 itemfg.part-no    = oe-ordl.part-no
 itemfg.cust-no    = oe-ord.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = oe-ordl.pr-uom.

ASSIGN
    itemfg.taxable = IF AVAIL cust 
                      THEN cust.sort EQ "Y" AND cust.tax-gr NE ""
                      ELSE 
                          IF AVAIL bf-itemfg THEN bf-itemfg.taxable
                                             ELSE NO.


 IF fgmaster-cha EQ "FGITEM" THEN
    ASSIGN
       itemfg.sell-uom   = oe-ordl.pr-uom
       itemfg.prod-uom   = v-uom
       itemfg.i-code     = "C"
       itemfg.stocked    = yes
       itemfg.alloc      = IF AVAIL xeb AND xeb.est-type LE 4 THEN v-allocf ELSE v-alloc.
       
    
 IF v-graphic-char NE "" THEN 
 DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
 END.
 

 
 if avail xeb then do:
    assign itemfg.die-no     = xeb.die-no
           itemfg.plate-no   = xeb.plate-no
           itemfg.style      = xeb.style
           itemfg.cad-no     = xeb.cad-no
           itemfg.upc-no     = xeb.upc-no
           itemfg.spc-no     = xeb.spc-no
           itemfg.procat     = xeb.procat
           itemfg.alloc      = xeb.set-is-assembled
           itemfg.pur-man    = xeb.pur-man.
    
      itemfg.isaset = IF  xeb.form-no eq 0 THEN  TRUE ELSE FALSE.
       
    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.
     
    {oe/fgfreighta.i xeb}
 
    {fg/set-inks1.i itemfg xeb}
 
    {sys/inc/fgcascnt.i itemfg xeb} 

    {sys/inc/updfgdim.i "xeb"}

   
    IF xeb.form-no EQ 0 THEN DO:
      
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
       IF ll-one-part THEN itemfg.alloc = YES.
    END.
 end.
 else IF fgmaster-cha EQ "FGITEM" THEN do:
    find first cust where cust.company = prmComp and
                       cust.active  = "X"    no-lock no-error.
    if avail cust then do:
       find first shipto of cust no-lock no-error.
       if avail shipto then do:
          assign itemfg.def-loc     = shipto.loc        
                 itemfg.def-loc-bin = shipto.loc-bin.
       end.
    end. 
 end.

IF fgmaster-cha EQ "FGITEM" THEN DO:

   find first oe-ctrl where oe-ctrl.company eq prmComp no-lock no-error.
   itemfg.i-code = if oe-ord.est-no ne "" then "C"
                   else if avail oe-ctrl then
                           if oe-ctrl.i-code then "S"
                           else "C"
                   else "S".
END.

{est/fgupdtax.i oe-ord}
/*ll-new-fg-created = YES.*/

END PROCEDURE.




PROCEDURE itemfg-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-uom LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.

  
    if b-oe-ordl.job-no = "" then do:

      find first itemfg
          where itemfg.company = prmComp
            and itemfg.i-no = b-oe-ordl.i-no
          NO-LOCK NO-ERROR.
 
      find first po-ordl where po-ordl.company   eq cocode
                           and po-ordl.i-no      eq b-oe-ordl.i-no
                           and po-ordl.po-no     eq int(b-oe-ordl.po-no-po)
                           and po-ordl.item-type eq no
                           use-index item-ordno no-lock no-error.
      if AVAIL po-ordl AND int(b-oe-ordl.po-no-po) NE 0 then
        assign b-oe-ordl.pr-uom = IF b-oe-ordl.pr-uom = "" THEN po-ordl.cons-uom ELSE b-oe-ordl.pr-uom
               lv-uom                      = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
               b-oe-ordl.cost   = (po-ordl.cons-cost).
      else
      IF AVAIL itemfg THEN
        assign b-oe-ordl.pr-uom = if itemfg.prod-uom NE "" AND b-oe-ordl.pr-uom = "" then itemfg.prod-uom else b-oe-ordl.pr-uom
               lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
               b-oe-ordl.cost   = (itemfg.total-std-cost).
 
      if lv-uom ne "M" THEN do:
        run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                               dec(b-oe-ordl.cost), output lv-cost).
        assign b-oe-ordl.cost = (lv-cost)
            /*oe-ordl.pr-uom:screen-value = itemfg.sell-uom ?? */ .                        
      end.

      if AVAIL po-ordl AND int(b-oe-ordl.po-no-po) NE 0 then
      DO:
         FIND FIRST po-ord WHERE
              po-ord.company EQ po-ordl.company AND
              po-ord.po-no   EQ po-ordl.po-no NO-LOCK NO-ERROR.
         IF AVAIL po-ord THEN
         DO:
            FIND FIRST reftable WHERE
                 reftable.reftable EQ 'e-itemfg-vend.markup' AND
                 reftable.company EQ po-ordl.company AND
                 reftable.loc EQ po-ordl.i-no AND
                 reftable.code EQ po-ord.vend-no
                 NO-LOCK NO-ERROR.
            
            IF AVAIL reftable THEN
            DO:
               b-oe-ordl.cost = (DEC(b-oe-ordl.cost) * (1 + (reftable.val[1]/ 100.0 ))).
               RELEASE reftable.
            END.
            RELEASE po-ord.
         END.
      END.
    end.
 
END PROCEDURE.

/********************************************************/

PROCEDURE get-est-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   def input param ip-est-no as CHAR NO-UNDO.

   def var v-run-list as CHAR NO-UNDO.

   if ip-est-no ne "" and not avail xest then
        find first xest where xest.company eq cocode and
                   xest.est-no eq ip-est-no no-lock no-error.

   

   IF AVAIL xest THEN DO :
       
     find first xeb
          where xeb.company = g_company 
            and xeb.est-no = ip-est-no
            and xeb.part-no = b-oe-ordl.part-no
          no-lock no-error.
     IF AVAIL xeb THEN
     find first xef
          where xef.company = g_company 
            and xef.est-no = ip-est-no
            and (xef.form-no = xeb.form-no OR xeb.form-no = 0)
          no-lock no-error.

     ASSIGN
        v-run-list = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
                     "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
                     "cec/tan/print4.p,cec/com/print4.p" 
        qty = INT(b-oe-ordl.qty)
        v-shared-rel = v-rel.


     IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                          /*AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8                                          
        AND (b-oe-ordl.qty NE qty OR DEC(b-oe-ordl.cost) EQ 0)*/ THEN DO:

         

        RUN VALUE(ENTRY(xest.est-type,v-run-list)).     

        b-oe-ordl.cost = ((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                                           (INT(b-oe-ordl.qty) / 1000)).
                                                         
     END.
   END.

END PROCEDURE.


PROCEDURE create-order-job :

    
    IF oe-ord.job-no NE "" THEN DO:
          FIND FIRST job NO-LOCK
              WHERE job.company EQ oe-ord.company
                AND job.job-no  EQ oe-ord.job-no
                AND job.job-no2 EQ oe-ord.job-no2
              NO-ERROR.
         
          IF AVAIL job AND TRIM(job.est-no) NE TRIM(oe-ord.est-no) THEN
            IF CAN-FIND(FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                          AND job-hdr.job     EQ job.job
                          AND job-hdr.job-no  EQ job.job-no
                          AND job-hdr.job-no2 EQ job.job-no2
                          AND job-hdr.ord-no  NE oe-ord.ord-no) OR
               CAN-FIND(FIRST b-oe-ord
                        WHERE b-oe-ord.company EQ job.company
                          AND b-oe-ord.job-no  EQ job.job-no
                          AND b-oe-ord.job-no2 EQ job.job-no2
                          AND b-oe-ord.est-no  EQ job.est-no)   OR
               CAN-FIND(FIRST b-oe-ordl
                        WHERE b-oe-ordl.company EQ job.company
                          AND b-oe-ordl.job-no  EQ job.job-no
                          AND b-oe-ordl.job-no2 EQ job.job-no2
                          AND b-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.
            ELSE
            DO TRANSACTION:
              FIND CURRENT job NO-ERROR.
              IF AVAIL job THEN DELETE job.
            END.
         
          IF NOT AVAIL job THEN DO:
            RUN create-job (OUTPUT lv-job-recid).
            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.
          END.                 
         
          v-qty-mod = YES.

          IF AVAIL job AND INDEX("HWPRL",job.stat) NE 0 THEN DO:
            /*IF NOT v-qty-mod THEN
               RUN oe/job-qty.p (ROWID(oe-ord), OUTPUT v-qty-mod).*/
         
            IF v-qty-mod OR job.stat EQ "P" THEN DO:
              RUN jc/chkrebld.p (RECID(job), OUTPUT choice).     
              IF NOT choice THEN DO:
                ASSIGN hld-id     = fil_id
                       hld-nufile = nufile 
                       hld-stat   = job.stat
                       nufile     = YES.
         
                RUN jc/jc-calc.p(RECID(job)).
                ASSIGN fil_id   = hld-id
                       nufile   = hld-nufile.
               
                IF hld-stat NE "P" THEN DO TRANSACTION:
                  FIND CURRENT job EXCLUSIVE.
                  job.stat = hld-stat.
                  FIND CURRENT job NO-LOCK.
                END.
              END.
            END.
          END.



        find first sys-ctrl where
               sys-ctrl.company eq cocode AND
               sys-ctrl.name    eq "SCHEDULE"
               no-lock no-error.

          v-run-schedule = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO
                           ELSE IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES
                           ELSE NO.

          FOR EACH oe-ordl NO-LOCK
              WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ oe-ord.ord-no
                
              BREAK BY oe-ordl.job-no
                    BY oe-ordl.job-no2:

             /* FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = oe-ordl.i-no  EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg THEN DO:
            ASSIGN
                itemfg.q-ono = itemfg.q-ono + prmQty 
                itemfg.q-alloc =  itemfg.q-alloc + prmQty .
        END.*/
          
              ASSIGN
               nufile     = YES
               hld-id     = fil_id
               hld-nufile = nufile
               fil_id     = RECID(oe-ordl).

              /*run oe/ordlup.p.*/
             
              IF LAST-OF(oe-ordl.job-no2) THEN DO:
              
            /*  RUN po/do-po.p.*/

              /* check oe-ordl.due-date and calc promised date and job's start-date */

            /*  IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.*/
              
              ASSIGN
               fil_id = hld-id
               nufile = hld-nufile.
            END.
          END.
      
    END.
          END PROCEDURE.


PROCEDURE create-job :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def output param op-recid as recid no-undo.

  DEF BUFFER v-ord-job-hdr FOR job-hdr.

  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
    
  /* === from oe/oe-ord1.p  ============= */
         
  find last job where job.company eq cocode no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.
  ASSIGN
   v-job-no  = oe-ord.job-no
   v-job-no2 = oe-ord.job-no2.

  FOR EACH job
      WHERE job.company EQ cocode
        AND job.job-no  EQ v-job-no
        AND job.job-no2 EQ v-job-no2:
    DELETE job.
  END.

  
  create job.
  assign job.job        = v-job-job
         job.company    = cocode
         job.loc        = locode
         job.est-no     = oe-ord.est-no
         job.job-no     = v-job-no
         job.job-no2    = v-job-no2
         job.stat       = "P"
         op-recid = recid(job).

  for each oe-ordl where oe-ordl.company eq oe-ord.company
                     and oe-ordl.ord-no  eq oe-ord.ord-no exclusive:
      find first job-hdr no-lock
          where job-hdr.company eq cocode
            and job-hdr.job-no  eq oe-ord.job-no
            and job-hdr.job-no2 eq oe-ord.job-no2
            and job-hdr.ord-no  eq oe-ord.ord-no
            and job-hdr.i-no    eq oe-ordl.i-no
          no-error.

      if not avail job-hdr then do:
          
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.   
         
         create job-hdr.
         assign job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.est-no       = oe-ord.est-no
                job-hdr.i-no         = oe-ordl.i-no
                job-hdr.qty          = oe-ordl.qty 
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no
                job-hdr.blank-no     = oe-ordl.blank-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
      end.

      ELSE
      DO WHILE TRUE:
        FIND v-ord-job-hdr WHERE ROWID(v-ord-job-hdr) EQ ROWID(job-hdr)
            EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL v-ord-job-hdr THEN DO:
          FIND CURRENT v-ord-job-hdr NO-LOCK NO-ERROR.
          FIND CURRENT job-hdr NO-ERROR.
          LEAVE.
        END.
      END.

      assign job-hdr.est-no  = oe-ord.est-no
             job-hdr.job     = job.job
             job-hdr.job-no  = job.job-no
             job-hdr.job-no2 = job.job-no2
             oe-ordl.est-no  = job-hdr.est-no
             oe-ordl.job-no  = job-hdr.job-no
             oe-ordl.job-no2 = job-hdr.job-no2
             oe-ordl.j-no = job-hdr.j-no.

      FIND CURRENT job-hdr NO-LOCK.
  end.

  FIND CURRENT job NO-LOCK.
   
END PROCEDURE.
/***************************************************************/

PROCEDURE crt-eb-itemfg :

def input parameter v-item like itemfg.i-no.
def input parameter v-uom like itemfg.prod-uom.

def var tmpstore as cha no-undo.
def var i as int no-undo.
/*DEF VAR ll-one-part AS LOG NO-UNDO.*/
{oe/fgfreight.i}
/*DEF BUFFER x-eb FOR eb.*/
DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER b-eb2 FOR eb.

DEF BUFFER bf-itemfg FOR itemfg.

      FIND FIRST xeb WHERE xeb.company = prmComp AND xeb.est-no = eb.est-no AND 
        xeb.form-no = eb.form-no AND xeb.blank-no = eb.blank-no NO-LOCK NO-ERROR.

{sys/inc/setprint.i}
       
find first cust  where cust.company eq cocode
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.NAME EQ "FGMASTER" NO-ERROR.
IF AVAIL sys-ctrl THEN
  FIND FIRST bf-itemfg NO-LOCK
      WHERE bf-itemfg.company EQ sys-ctrl.company
        AND bf-itemfg.i-no EQ trim(sys-ctrl.char-fld) NO-ERROR.

create itemfg.
assign
 itemfg.company    = cocode
 itemfg.loc        = locode
 itemfg.i-no       = v-item
 itemfg.i-name     = b-oe-ordl.i-name
 itemfg.part-dscr1 = b-oe-ordl.part-dscr1
 itemfg.part-dscr2 = b-oe-ordl.part-dscr2
 itemfg.sell-price = dec(b-oe-ordl.price)
 itemfg.part-no    = b-oe-ordl.part-no
 itemfg.cust-no    = b-oe-ordl.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = IF AVAIL bf-itemfg THEN bf-itemfg.pur-uom ELSE "M"
/* gdm - 11190901 */
 itemfg.ship-meth  = IF AVAIL bf-itemfg THEN bf-itemfg.ship-meth ELSE YES
  .

ASSIGN
    itemfg.taxable = IF AVAIL cust 
                      THEN cust.sort EQ "Y" AND cust.tax-gr NE ""
                      ELSE 
                          IF AVAIL bf-itemfg THEN bf-itemfg.taxable
                                             ELSE NO.


 IF fgmaster-cha EQ "FGITEM" THEN
    ASSIGN
       itemfg.sell-uom   = b-oe-ordl.pr-uom
       itemfg.prod-uom   = v-uom
       itemfg.i-code     = "C"
       itemfg.stocked    = yes
       itemfg.alloc      = IF AVAIL xeb AND xeb.est-type LE 4 THEN v-allocf ELSE v-alloc.
    
 IF v-graphic-char NE "" THEN 
 DO:
   IF LOOKUP(SUBSTR(v-graphic-char,LENGTH(v-graphic-char)),"\,/") EQ 0 THEN
      v-graphic-char = v-graphic-char + "\".

   IF SEARCH(v-graphic-char + itemfg.i-no + ".jpg") NE ? THEN
      itemfg.box-image = v-graphic-char + itemfg.i-no + ".jpg".
 END.

 if avail xeb then do:

    
    assign itemfg.die-no     = xeb.die-no
           itemfg.plate-no   = xeb.plate-no
           itemfg.style      = xeb.style
           itemfg.cad-no     = xeb.cad-no
           itemfg.upc-no     = xeb.upc-no
           itemfg.spc-no     = xeb.spc-no
           itemfg.isaset     = xeb.form-no eq 0
           itemfg.procat     = xeb.procat
           itemfg.alloc      = xeb.set-is-assembled
           itemfg.pur-man    = xeb.pur-man.

    /*IF xeb.pur-man THEN itemfg.pur-uom = "EA".*/

    IF itemfg.alloc NE ? THEN itemfg.alloc = NOT itemfg.alloc.
    
    {oe/fgfreighta.i xeb}
    
    {fg/set-inks1.i itemfg xeb}
 
    {sys/inc/fgcascnt.i itemfg xeb}

    {sys/inc/updfgdim.i "xeb"}

    IF xeb.form-no EQ 0 THEN DO:
      
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
       IF ll-one-part THEN itemfg.alloc = YES.
    END.
 end.
 else IF fgmaster-cha EQ "FGITEM" THEN do:
    find first cust where cust.company = cocode and
                       cust.active  = "X"    no-lock no-error.
    if avail cust then do:
       find first shipto of cust no-lock no-error.
       if avail shipto then do:
          assign itemfg.def-loc     = shipto.loc        
                 itemfg.def-loc-bin = shipto.loc-bin.
       end.
    end.
 end.

IF fgmaster-cha EQ "FGITEM" THEN DO:

   find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
   itemfg.i-code = if b-oe-ordl.est-no ne "" then "C"
                   else if avail oe-ctrl then
                           if oe-ctrl.i-code then "S"
                           else "C"
                   else "S".
END.

{est/fgupdtax.i oe-ord}
ll-new-fg-created = YES.

END PROCEDURE.


/****************************************************************/
