                                              
/*------------------------------------------------------------------------
    File        : ViewItemEstimate.p
    Purpose     : OrderItemEstimate

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttGetCost NO-UNDO
    FIELD quantity      AS DECIMAL FORMAT "->>,>>>,>>99.9<<"    
    FIELD vCost         AS  DECIMAL      
   . 
DEFINE DATASET dsGetCost FOR ttGetCost .

DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.        
DEFINE INPUT PARAMETER prmQty           AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmEstimate      AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmPartNum       AS CHAR NO-UNDO. 

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetCost.
DEFINE OUTPUT PARAMETER cError         AS CHAR NO-UNDO.

def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE frighiout AS DECIMAL NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR vLine AS INT NO-UNDO.
IF prmUser     = ? THEN ASSIGN prmUser     = "".   
IF prmQty          = ? THEN ASSIGN  prmQty        = 0.    
IF prmEstimate     = ? THEN ASSIGN  prmEstimate   = "". 
IF prmPartNum      = ? THEN ASSIGN  prmPartNum    = "". 


DEF NEW SHARED WORKFILE tt-fg-set LIKE fg-set
    FIELD isaset LIKE itemfg.isaset
    FIELD alloc  LIKE itemfg.alloc
    FIELD part-qty-dec AS DEC.

DEF VAR ll-new-file AS LOG NO-UNDO.
DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
DEF VAR cp-rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VAR  vErr         AS CHAR NO-UNDO.
DEF VAR lv-t-price AS DECIMAL NO-UNDO.
DEF VAR v-tmp-price AS DECIMAL NO-UNDO.
DEF VAR v-msg AS CHAR NO-UNDO.
DEF BUFFER bf-ordl FOR oe-ordl.
DEF BUFFER cust-po-mand FOR reftable.
DEFINE VAR tfright LIKE oe-ordl.t-freight NO-UNDO.
  
/*{custom/globdefs.i}    */
{sys/inc/var.i new shared}
def var v-use-rel like sys-ctrl.log-fld no-undo.
def var v-upd-comm as log init yes no-undo.
def var v-dup-item as log no-undo.
def var v-rtn-code as int no-undo.
def new shared buffer xest for est.
def new shared buffer xeb for eb.
def new shared buffer xef for ef.
def var v-valdcode as cha init "ON,BY,MH" no-undo.
def var v-bld-job as cha no-undo.
def var v-est-no as cha no-undo.  
def new shared buffer xoe-ord for oe-ord.    /* BUFFER WITH ORDER HEADER */
def new shared var v-procat like oe-prmtx.procat no-undo. /* ITEM CATEGORY */
def new shared var v-price-lev as int no-undo.
DEF NEW SHARED VAR s-est-no AS cha NO-UNDO.  /* for fgadd2.p */
def new shared workfile work-ordl like oe-ordl.
def new shared var save_id as recid no-undo.  /* RECORD ID FOR ORDER LINE */
def new shared var v-i-item like oe-ordl.i-no no-undo. /* INPUT ITEM */
def new shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */
def new shared var price-ent as log NO-UNDO.
DEF NEW SHARED VAR matrixExists AS LOG NO-UNDO.
def new shared var fil_id as recid no-undo. 
def new shared var nufile as log no-undo.
def new shared var v-qty-mod as log no-undo.
def new shared var v-fr-tax like oe-ctrl.f-tax no-undo.
def new shared var v-create-job as log no-undo.
def var lv-ordl-recid as recid no-undo.
def var lv-change-prom-date as log no-undo.  /* flag for updating oe-ordl.prom-date*/
def var lv-change-cst-po as log no-undo.    /* flag for updateing oe-ordl.po-no */
def var lv-uom-list as cha init "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" no-undo.
def var lv-ea-list as CHAR no-undo.
def var lv-valid-uom as CHAR no-undo.
def var v-valtype as cha init "O,R,C" no-undo.
def var ll-new-record as log no-undo.

def var lv-item-recid as recid no-undo.
def var first-cust-part-no as cha no-undo.
def var ll-ok-i-no as log no-undo.
def var ls-stock as cha no-undo.  /* for eb.stock-no */
def var ll-help-ran as log no-undo.  /* qty help */
def var ll-bypass as log no-undo.    /* bypass fields for price */
{ce/print4.i "new shared"}
{ce/print42.i "new shared"}
def new shared var lv-qty as int no-undo.
def new shared var qty as INT NO-UNDO.
def buffer xoe-rel for oe-rel.
def var ld-prev-price as dec no-undo.
def var ll-got-qtprice as log no-undo.
def var li-prev-qty as int no-undo.
DEF VAR lv-add-mode AS LOG NO-UNDO. 
DEF VAR lv-help-qty AS INT NO-UNDO.
DEF VAR ll-qty-leave-done AS LOG NO-UNDO.
DEF VAR ll-new-fg-created AS LOG NO-UNDO.
DEF VAR lv-new-tandem AS ROWID NO-UNDO.
DEF VAR ll-is-tandem AS LOG NO-UNDO.
DEF VAR ll-do-entry AS LOG NO-UNDO.
DEF VAR lv-update-job-stdate AS LOG NO-UNDO.
DEF VAR v-print-head LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR v-print-fmt LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR lv-q-no LIKE quotehd.q-no NO-UNDO.
DEF VAR v-run-schedule AS LOG NO-UNDO.
DEF VAR lv-type-codes AS CHAR NO-UNDO.
DEF VAR lv-type-dscrs AS CHAR NO-UNDO.
DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEF VAR ll-prev-whs-item AS LOG NO-UNDO.
DEF VAR ld-prev-t-price LIKE oe-ordl.t-price NO-UNDO.
DEF VAR li-prev-ord-qty LIKE oe-ordl.qty NO-UNDO.
DEF VARIABLE historyQty AS DECIMAL NO-UNDO.
DEF VARIABLE historyPrice LIKE oe-ordl.price NO-UNDO.
DEF VARIABLE historyPrUOM LIKE oe-ordl.pr-uom NO-UNDO.
DEF VARIABLE setFromHistory AS LOGICAL NO-UNDO.
DEF VARIABLE historyButton AS LOGICAL NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF WORKFILE w-ord FIELD w-ord-no LIKE oe-ord.ord-no.
DEF TEMP-TABLE tt-oe-ordl LIKE oe-ordl
    FIELD to-be-deleted AS LOG INIT YES
    FIELD row-id AS ROWID
    INDEX row-id row-id. 
def var v-inactive as log no-undo.
def var v-job-no like oe-ord.job-no no-undo.
def var v-job-no2 like oe-ord.job-no2 no-undo.
def var v-exp-limit as int init 10 no-undo.
def var v-n-ord like oe-ctrl.n-ord no-undo.
def var v-estord-id as recid extent 10 no-undo.
def var v-multord as log no-undo.
def var v-custype like cust.type no-undo.
DEF VAR ld-lastship-dec AS DEC NO-UNDO.
DEF VAR ld-lastship-cha AS CHAR NO-UNDO.
DEF VAR ll-valid-po-no AS LOG NO-UNDO.
DEF VAR lastship-cha AS CHAR NO-UNDO.
DEF VAR lastship-int AS INT NO-UNDO.
DEF VAR v-job-rec_key     AS   CHAR NO-UNDO.
DEFINE VARIABLE prodDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE dueDateChanged AS LOGICAL NO-UNDO.
DEFINE VARIABLE scheduleHndl AS HANDLE NO-UNDO.
DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.
DEFINE VARIABLE copyRowID AS ROWID NO-UNDO.
 DEF VAR ll-new-po AS LOG NO-UNDO.
DEF VAR ll-new-due AS LOG NO-UNDO.
 DEF VAR lv-uom LIKE oe-ordl.pr-uom NO-UNDO.
  DEF VAR lv-cost AS DEC DECIMALS 10 NO-UNDO.
  def var ls-part-no as cha no-undo.
  DEFINE VARIABLE v-disp-prod-cat AS CHARACTER  NO-UNDO.
/******************************delete-item*****************************************************/

  def var ll-dumb as log no-undo.
  
  /*def buffer bf-ordl for oe-ordl.*/
  
  def var v-delete as log no-undo.
  def var choice as log no-undo.
  def buffer xoe-ordl for oe-ordl.
  def buffer xfg-set for fg-set.
  def buffer xitemfg for itemfg.
  def var v-tax-rate as dec format "->,>>9.99<<<" no-undo.
  def var v-frt-tax-rate like v-tax-rate no-undo.
  def var tmp-tax like oe-ord.tax no-undo init 0 .
  def var tmp-ordm-amt like oe-ordm.amt no-undo init 0.
  def var v-continue as log no-undo.
  def var v-blank-fg-on-est as int no-undo.
  
  DEF VAR ll-tandem AS LOG NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-est-no LIKE oe-ordl.est-no NO-UNDO.
  DEF VAR ll-valid-eb AS LOG NO-UNDO.
  DEF VAR ll-more-ord AS LOG NO-UNDO.
  def var lv-continue as log no-undo.
  def buffer tmp-xoe-ordl for oe-ordl.
  def buffer tmp-xoe-ordm for oe-ordm.
      DEF VAR deleteComps AS LOG NO-UNDO.
      DEF VAR askdel AS LOG NO-UNDO.
      DEFINE VAR fgitem AS CHAR NO-UNDO.
DEFINE VAR pro AS CHAR NO-UNDO.
DEFINE VAR fgitemname AS CHAR NO-UNDO.
DEFINE VAR procat AS CHAR NO-UNDO.
DEF VAR vEstType AS INT NO-UNDO.
DEF VAR prvQty AS INT NO-UNDO.
DEF VAR fgQty AS INT NO-UNDO.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR v-rel AS INT NO-UNDO.
      /**********************************************************************************************/


{sys/ref/fgoecost.i}
/*{sys/ref/oecustpart.i}*/

{oe/oe-sysct1.i NEW}

{custom/framechk.i NEW}
{sys/inc/graphic.i}

DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER oe-ord-whs-order FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.

DEF WORKFILE w-est-no FIELD w-est-no LIKE itemfg.est-no FIELD w-run AS LOG.

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

DO TRANSACTION:
  {sys/inc/schedule.i}
  v-run-schedule = NOT (AVAIL sys-ctrl AND sys-ctrl.char-fld EQ 'NoDate' AND sys-ctrl.log-fld).
  {sys/inc/oeship.i}
  {sys/inc/oereleas.i}
  {sys/inc/oescreen.i}
  {sys/inc/job#.i}
  {sys/inc/oeround.i}
  {sys/inc/fgmaster.i}
  {sys/inc/oeestcom.i}
END.
{sys/inc/f16to32.i}
RUN sys/ref/ordtypes.p (OUTPUT lv-type-codes, OUTPUT lv-type-dscrs).
RUN sys/ref/uom-ea.p (OUTPUT lv-ea-list).


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    g_company  = prmComp
    cocode = g_company
    locode = usercomp.loc.
    
ASSIGN 
    locode = "MAIN".

/*******main******************/
RUN oe/oe-sysct.p.

  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "JOBCREAT"
                        no-lock no-error.
  if not avail sys-ctrl then do transaction:
        create sys-ctrl.
        assign sys-ctrl.company = cocode
               sys-ctrl.name    = "JOBCREAT"
               sys-ctrl.descrip = "Create Job Standards during OE?"
               sys-ctrl.log-fld = no.
        MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.
  end.
  v-create-job = sys-ctrl.log-fld.
      
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVPRINT"
      no-lock no-error.
  if avail sys-ctrl then
    ASSIGN
     v-print-head = sys-ctrl.log-fld
     v-print-fmt  = sys-ctrl.char-fld.

  {sys/ref/oecount.i}
  
  {ce/msfcalc.i}

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


/************************************************************************************/


IF prmAction = "GetCost" THEN DO:

    FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    IF AVAIL est  THEN DO:                                                
        find first eb where eb.company = cocode 
            and eb.est-no = est.est-no
            and eb.form-no  eq 0
            no-lock no-error.  
    END.

    lv-est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate).

    RUN get-est-cost2 (lv-est-no). 
END.



PROCEDURE get-est-cost2 :
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
            and xeb.part-no = prmPartNum
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
        qty = INT(prmQty)
        v-shared-rel = v-rel.
         /*v-shared-rel = 10 .*/

     IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                          /*AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8 */        THEN DO: 
            

            RUN VALUE(ENTRY(xest.est-type,v-run-list)).     

            CREATE ttGetCost.
            ASSIGN
                    ttGetCost.quantity = DECIMAL(prmQty) 
                    ttGetCost.vCost = ((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                                           (INT(prmQty) / 1000)).       
     END.
   END.

END PROCEDURE.
