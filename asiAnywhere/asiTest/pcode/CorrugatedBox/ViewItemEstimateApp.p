                                              



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

DEFINE TEMP-TABLE ttViewItemEstimateApp NO-UNDO
    FIELD abc           AS CHAR
    FIELD OrdNo         AS INT 
    FIELD est-no        LIKE oe-ordl.est-no
    FIELD job-no       LIKE oe-ordl.job-no
    FIELD job-no2        LIKE oe-ordl.job-no2
    FIELD vLine         LIKE oe-ordl.LINE
    FIELD CustPart      AS CHAR FORMAT "x(15)"
    FIELD Item1         AS CHAR FORMAT "x(15)"
    FIELD Name1         AS CHAR FORMAT "x(30)"
    FIELD Dscr          AS CHAR FORMAT "x(30)"
    FIELD Dscr2         AS CHAR FORMAT "x(30)"
    FIELD Dscr3         AS CHAR FORMAT "x(30)"
    FIELD quantity      AS DECIMAL FORMAT "->>,>>>,>>99.9<<"
    FIELD price         AS DECIMAL FORMAT "->>,>>>,>>9.99<<"
    FIELD uom           AS CHAR FORMAT "X(4)"
    FIELD counter       AS INTEGER 
    FIELD custpo        AS CHAR FORMAT "x(15)"
    FIELD taxable       AS LOGICAl Initial true 
    FIELD discount      AS DECIMAL FORMAT ">>>,>>9.99" 
    FIELD requested     AS CHAR FORMAT "x(2)"
    FIELD requestdate   AS DATE FORMAT "99/99/9999"
    FIELD extprice      AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD promised      AS CHAR FORMAT ">>>>9"
    FIELD promisdate    AS DATE FORMAT "99/99/9999"
    FIELD shipqty       AS DECIMAL FORMAT "->>,>>>,>>9.99"
    FIELD alloc         AS LOGICAL Initial true 
    FIELD ord-level     AS Decimal FORMAT ">>>,>>>,>>9.999"
    FIELD q-ono         AS Decimal FORMAT  "->>,>>>,>>9.999 "  
    FIELD q-onh         AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-alloc       AS Decimal FORMAT "->>,>>>,>>9.999"  
    FIELD q-avail       AS Decimal FORMAT "->>,>>>,>>9.999"     
    FIELD q-back        AS Decimal FORMAT "->>,>>>,>>9.999"    
    FIELD vPartial      AS Decimal 
    FIELD VcasUnit      AS INT
    FIELD vEnum         AS INT
    FIELD vPono         AS INT
    FIELD vSman         AS CHAR
    FIELD vSpct         AS Decimal
    FIELD vScomm        AS Decimal 
    FIELD vSman2        AS CHAR
    FIELD vSpct2        AS Decimal
    FIELD vScomm2       AS Decimal
    FIELD vSman3        AS CHAR
    FIELD vSpct3        AS Decimal
    FIELD vScomm3       AS Decimal
    FIELD vType         AS CHAR
    FIELD vOver         AS Decimal
    FIELD vUnder        AS Decimal
    FIELD vManag        AS LOGICAL
    FIELD vBoardVen     AS CHAR
    FIELD vSname        AS CHAR
    FIELD vSname2       AS CHAR   
    FIELD vSname3       AS CHAR   
    FIELD vCost         AS  DECIMAL  
    FIELD vQno          AS INT 
    FIELD vReckey       AS CHAR 
    FIELD vappItem AS CHAR
    FIELD app   AS CHAR 
    
    
    
   . 
DEFINE DATASET dsViewItemEstimateApp FOR ttViewItemEstimateApp .

DEFINE INPUT PARAMETER prmUser          AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      as INT no-undo.
DEFINE INPUT PARAMETER prmLine          as INT no-undo.
DEFINE INPUT PARAMETER prmEstimate      AS CHAR NO-UNDO.     
DEFINE INPUT PARAMETER prmItemNum       AS CHAR NO-UNDO.           
DEFINE INPUT PARAMETER prmPartNum       AS CHAR NO-UNDO.          
DEFINE INPUT PARAMETER prmQty           AS DECIMAL NO-UNDO.    
DEFINE INPUT PARAMETER prmItemName      AS CHAR NO-UNDO.          
DEFINE INPUT PARAMETER prmPartdscr      AS CHAR NO-UNDO.         
DEFINE INPUT PARAMETER prmPartdscr1     AS CHAR NO-UNDO.        
DEFINE INPUT PARAMETER prmPartdscr2     AS CHAR NO-UNDO.        
DEFINE INPUT PARAMETER prmPrice         AS DECIMAL NO-UNDO.      
DEFINE INPUT PARAMETER prmUom           AS CHAR NO-UNDO.      
DEFINE INPUT PARAMETER prmTax           AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmPoNum         AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmJob           AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmJob2           AS INT NO-UNDO.   
DEFINE INPUT PARAMETER prmDiscount      AS DECIMAL  NO-UNDO.
DEFINE INPUT PARAMETER prmCode          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmReqDate       AS DATE NO-UNDO.  
DEFINE INPUT PARAMETER prmTPrice        AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmPromCode      AS CHAR NO-UNDO.    
DEFINE INPUT PARAMETER prmPromDate      AS DATE NO-UNDO.        
DEFINE INPUT PARAMETER prmShip          AS DECIMAL NO-UNDO.  
DEFINE INPUT PARAMETER prmCas           AS INTEGER NO-UNDO.  
DEFINE INPUT PARAMETER prmPartial       AS DECIMAL NO-UNDO. 
DEFINE INPUT PARAMETER prmUnit          AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmEnum          AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmPrevOrder     AS INT NO-UNDO.  
DEFINE INPUT PARAMETER prmSman          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman2         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmSman3         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmType          AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmOver          AS DECIMAL NO-UNDO.   
DEFINE INPUT PARAMETER prmUnder         AS DECIMAL NO-UNDO.   
DEFINE INPUT PARAMETER prmVend          AS CHAR NO-UNDO.   
DEFINE INPUT PARAMETER prmManag         AS CHAR NO-UNDO.  
DEFINE INPUT PARAMETER prmLn            AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname         AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname2        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSname3        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSpct          AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSpct2         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmSpct3         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmComm          AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmComm2         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmComm3         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmCost          AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmQno           AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmNewItemCreated  AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmMultiItem  AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmApp  AS CHAR NO-UNDO. 


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewItemEstimateApp.
DEFINE OUTPUT PARAMETER cError         AS CHAR NO-UNDO.
def var v-duelist as cha init "ASAP,NB4,MUST,HOT,RUSH,WO,HOLD,CR,BY,ON,MH,$$$,AM,INK,OE,RWRK,TOOL,HFR" no-undo.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE frighiout AS DECIMAL NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR vLine AS INT NO-UNDO.
IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = 0.
IF prmLine = ? THEN ASSIGN prmLine = 0.
IF prmEstimate     = ? THEN ASSIGN  prmEstimate   = "".  
IF prmItemNum      = ? THEN ASSIGN  prmItemNum    = "".     
IF prmPartNum      = ? THEN ASSIGN  prmPartNum    = "".    
IF prmQty          = ? THEN ASSIGN  prmQty        = 0.    
IF prmItemName     = ? THEN ASSIGN  prmItemName   = "".    
IF prmPartdscr     = ? THEN ASSIGN  prmPartdscr   = "".    
IF prmPartdscr1    = ? THEN ASSIGN  prmPartdscr1  = "".     
IF prmPartdscr2    = ? THEN ASSIGN  prmPartdscr2  = "".     
IF prmPrice        = ? THEN ASSIGN  prmPrice      = 0.     
IF prmUom          = ? THEN ASSIGN  prmUom        = "".     
IF prmTax          = ? THEN ASSIGN  prmTax        = "".     
IF prmPoNum        = ? THEN ASSIGN  prmPoNum      = "".     
IF prmJob          = ? THEN ASSIGN  prmJob        = "".                       
IF prmJob2          = ? THEN ASSIGN  prmJob2        = 0.                       
IF prmDiscount     = ? THEN ASSIGN  prmDiscount   = 0.                       
IF prmCode         = ? THEN ASSIGN  prmCode       = "".   

IF prmTPrice       = ? THEN ASSIGN  prmTPrice     = 0.    
IF prmPromCode     = ? THEN ASSIGN  prmPromCode   = "".    

IF prmShip         = ? THEN ASSIGN  prmShip       = 0.                      
IF prmCas          = ? THEN ASSIGN  prmCas        = 0.    
IF prmPartial      = ? THEN ASSIGN  prmPartial    = 0.   
IF prmUnit         = ? THEN ASSIGN  prmUnit       = 0.   
IF prmEnum         = ? THEN ASSIGN  prmEnum       = 0.   
IF prmPrevOrder    = ? THEN ASSIGN  prmPrevOrder  = 0.                     
IF prmSman         = ? THEN ASSIGN  prmSman       = "".                     
IF prmSman2        = ? THEN ASSIGN  prmSman2      = "".   
IF prmSman3        = ? THEN ASSIGN  prmSman3      = "".   
IF prmType         = ? THEN ASSIGN  prmType       = "".   
IF prmOver         = ? THEN ASSIGN  prmOver       = 0.   
IF prmUnder        = ? THEN ASSIGN  prmUnder      = 0.                     
IF prmVend         = ? THEN ASSIGN  prmVend       = "".                     
IF prmManag        = ? THEN ASSIGN  prmManag      = "".   
IF prmLn           = ? THEN ASSIGN  prmLn         = "".   
IF prmSname        = ? THEN ASSIGN  prmSname      = "".  
IF prmSname2       = ? THEN ASSIGN  prmSname2     = "".  
IF prmSname3       = ? THEN ASSIGN  prmSname3     = "".  
IF prmSpct         = ? THEN ASSIGN  prmSpct       = 0.  
IF prmSpct2        = ? THEN ASSIGN  prmSpct2      = 0.  
IF prmSpct3        = ? THEN ASSIGN  prmSpct3      = 0.
IF prmComm         = ? THEN ASSIGN  prmComm       = 0.  
IF prmComm2        = ? THEN ASSIGN  prmComm2      = 0.  
IF prmComm3        = ? THEN ASSIGN  prmComm3      = 0.
IF prmCost         = ? THEN ASSIGN  prmCost       = 0.
IF prmQno          = ? THEN ASSIGN  prmQno        = 0.

IF prmNewItemCreated       = ? THEN ASSIGN  prmNewItemCreated     = "". 




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
def var li-cnt as int no-undo.
def var li-cases as int no-undo.
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

DEFINE VAR vUsers AS CHAR NO-UNDO.
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
IF AVAILABLE users THEN DO:
    IF users.internal-user = NO THEN DO:
        ASSIGN vUsers = "external".
    END. /*IF users.internal-user = NO*/
    IF users.internal-user = YES THEN DO:
        ASSIGN vUsers = "internal".
    END. /*IF users.internal-user = yes*/
END. /*IF AVAILABLE users*/

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
        /*MESSAGE sys-ctrl.descrip
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE sys-ctrl.log-fld.*/
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


/*Validate logic*/  
      
IF prmAction = "AddValidate" THEN DO:
    
    FIND FIRST  oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = prmOrderNum NO-LOCK NO-ERROR.
   
 /*RUN validate-all NO-ERROR.*/

  IF prmQty = 0 THEN DO:
      RETURN.
  END.
  

    IF cError EQ "" AND prmItemNum <> "" THEN DO:
        
      FIND FIRST bf-ordl WHERE bf-ordl.company EQ prmComp
                                  AND bf-ordl.ord-no  EQ prmOrderNum
                                  AND bf-ordl.i-no    EQ prmItemNum
                                  AND bf-ordl.job-no  EQ prmJob
                                  AND bf-ordl.job-no2 EQ INT(prmJob2)
                                  AND NOT bf-ordl.is-a-component
                                  AND ROWID(bf-ordl)  NE ROWID(oe-ordl) NO-LOCK NO-ERROR.
      IF AVAIL bf-ordl THEN DO:
      ASSIGN  cError = "has already been entered on this order".
      RETURN.
       END.
      END.
    IF cError EQ "" THEN DO:
      IF prmEstimate NE "" THEN
      FOR EACH eb
          WHERE eb.company   EQ prmComp
            AND eb.est-no    EQ prmEstimate
            AND (eb.est-type EQ 2 OR eb.est-type EQ 6)
            AND eb.form-no   NE 0
          NO-LOCK BREAK BY eb.est-no:
        IF (NOT FIRST(eb.est-no) OR NOT LAST(eb.est-no)) AND
           (eb.stock-no EQ prmItemNum OR
            eb.part-no  EQ prmItemNum)    THEN DO:
          cError = "This Item is a component on this estimate".
         
        RETURN.
         END.
      END.
     END.
    IF cError EQ "" THEN
      IF prmItemNum NE "" THEN DO:
        FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
                              AND reftable.company  EQ prmComp
                              AND reftable.loc      EQ ""
                              AND reftable.code     EQ prmItemNum
                            NO-LOCK NO-ERROR.
        
        IF AVAIL reftable AND reftable.code2 = "I" THEN  DO:
          cError = "Item has InActive Status. Order cannot be placed for the Inactive Item.".
        RETURN.
        END.
      END.

 FIND FIRST cust NO-LOCK
        WHERE cust.company EQ oe-ord.company
          AND cust.cust-no EQ oe-ord.cust-no
          AND CAN-FIND(FIRST cust-po-mand
                       WHERE cust-po-mand.reftable EQ "cust.po-mand"
                         AND cust-po-mand.company  EQ prmComp
                         AND cust-po-mand.loc      EQ ""
                         AND cust-po-mand.code     EQ cust.cust-no
                         AND cust-po-mand.val[1]   EQ 1)
        NO-ERROR.
    
    IF AVAIL cust AND TRIM(prmPoNum) EQ "" THEN DO:
ASSIGN cError =  "PO# is mandatory for this Customer...".
      RETURN .
    END.

if prmEstimate <> "" AND prmItemNum = ""  then do:
     find first eb where eb.company = prmComp and
                      eb.est-no = prmEstimate
                  and eb.cust-no = oe-ord.cust-no
                  and ((eb.est-type = 1 and eb.form-no <> 0) or
                  (eb.est-type = 2 and eb.form-no = 0) or
                               (eb.est-type = 5 and eb.form-no <> 0) or
                               (eb.est-type = 6 and eb.form-no = 0) )
                              no-lock no-error.
     if avail eb then prmItemNum = eb.stock-no.
  end.

IF LOOKUP(prmType,lv-type-codes) LE 0 OR
       (prmType EQ "T" AND
        NOT CAN-FIND(FIRST cust WHERE cust.company EQ cocode
                                  AND cust.cust-no EQ oe-ord.cust-no
                                  AND cust.active  EQ "X")) THEN DO:
      ASSIGN cError = "Invalid Type, try help..." .
      RETURN .
    END.  

   /* FIND FIRST itemfg WHERE itemfg.company = prmComp AND (itemfg.i-no = prmItemNum OR prmItemNum = "") NO-LOCK NO-ERROR.
    if not avail itemfg THEN DO:
        ASSIGN cError =  "Invalid FG Item#. Try help. " .
        RETURN.
        
   end. /* not avail */ */

     IF date(prmReqDate) > DATE("12/31/2099") THEN DO:
       ASSIGN 
               cError ="Date lies between 01/01/2009 & 12/12/2099".
           RETURN.
            END.

   if  date(prmReqDate) < oe-ord.ord-date THEN do:
       ASSIGN cError = "Due Date cannot be earlier than order date..." .
       RETURN.  
       END.

       IF prmEstimate <> ""  THEN DO:
       if date(prmPromDate) < oe-ord.ord-date then do:
       ASSIGN
           cError = "Scheduled Date cannot be earlier than order date..." .
        RETURN.  
       END.
       end.
    if index(v-duelist,prmCode) <= 0 then do:
       ASSIGN cError = "Invalid Priority Code. " .
      RETURN.
    end.
    if index(v-duelist,prmPromCode) <= 0 then do:
       cError = "Invalid Priority Code. " .
      RETURN.
    end.
IF vUsers = "external" THEN DO:
    IF prmItemNum = "" THEN DO:
        cError = "FG Item must be enter...".
        RETURN.
    END.

IF prmItemNum <> "" THEN DO:
        FIND FIRST itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no = prmItemNum NO-LOCK NO-ERROR.
         IF NOT AVAIL itemfg THEN DO:
           ASSIGN cError = "Invalid FG Item. Try help. " .
           RETURN.
            END.

    IF prmPartNum <> "" THEN DO:
        
        FIND FIRST cust-part WHERE  cust-part.company = prmComp AND cust-part.cust-no = oe-ord.cust-no AND
                     (cust-part.i-no = prmItemNum ) AND cust-part.part-no = prmPartNum  NO-LOCK NO-ERROR.

        if not avail cust-part then do:
         FIND FIRST itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no = prmItemNum 
             AND itemfg.part-no = prmPartNum NO-LOCK NO-ERROR.
         IF NOT AVAIL itemfg THEN DO:
           ASSIGN cError = "Invalid Cust Part#. Try help. " .
           RETURN.
            END.
    end. 
 END.
END.
END.

IF vUsers = "internal" THEN DO:
     IF prmQno = 0 AND prmItemNum = ""  THEN do:
         ASSIGN
             cError = "FG Item must be enter...".
         RETURN.
     END.
END.


    IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman AND sman.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
        ASSIGN cError = "Invalid Salesman".
        RETURN.
    END.
    END.
    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2 AND sman.company = prmComp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
       ASSIGN cError = "Invalid Salesman".
   END.
   END.
   IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3 AND sman.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:
          ASSIGN cError = "Invalid Salesman".
          RETURN.
       END.
   END.
   IF prmUom <> "" THEN DO:
       FIND uom WHERE uom.uom = prmUom   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE uom THEN DO:
           ASSIGN cError = "Invalid Uom".
           RETURN.
        END.
    END.

 /*IF prmPrevOrder <> 0 THEN DO:
   FIND po-ord WHERE po-ord.po-no = prmPrevOrder AND po-ord.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE po-ord THEN DO:

       ASSIGN cError = "Invalid Board Po#".
       RETURN.
      END.

      END.*/

      IF prmQno <> 0 THEN DO:
          FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQno  NO-LOCK  NO-ERROR.
          IF NOT AVAIL quotehd  THEN DO:
              ASSIGN
                  cError = "Invalid Quote try help..." .
              RETURN.
           END.
       END.

/*IF prmVend NE "" THEN DO:
      FIND FIRST vend
          WHERE vend.company EQ oe-ordl.company
            AND vend.vend-no EQ prmVend
          NO-LOCK NO-ERROR.
      IF NOT AVAILABLE vend THEN DO:
      ASSIGN
       cError = "Invalid Vendor , try help...".
        RETURN.
      END.
END.  /*IF prmVend NE "" THEN DO:*/*/
        

 END.   /*IF prmAction = "Add" THEN DO:*/   
                           
/* ********************  Preprocessor Definitions  ******************** */
 IF prmAction = "Delete"  THEN  DO:
   FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND
            oe-ordl.ord-no = prmOrderNum AND oe-ordl.LINE = prmLine AND  oe-ordl.stat = "W" EXCLUSIVE-LOCK NO-ERROR.
    
     find first itemfg
                 where itemfg.company eq oe-ordl.company
                 and itemfg.i-no    eq oe-ordl.i-no
                 no-error.

             if avail itemfg then do:
                 /*IF oe-ord.type NE "T" THEN*/ ASSIGN  itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
                 IF itemfg.q-alloc LT 0 THEN itemfg.q-alloc = 0.

                 assign
                     itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                     itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
                     itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.
                 
                 IF /*oe-ord.type NE "T"                                                   AND*/
                     NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}) THEN
                     RUN fg/comp-upd.p (RECID(itemfg), oe-ordl.qty * -1, "q-alloc", 0).
              end. /* avail itemfg */
 END.


IF prmAction = "Delete"  THEN DO:
    FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND
            oe-ordl.ord-no = prmOrderNum AND oe-ordl.LINE = prmLine AND  oe-ordl.stat = "W" EXCLUSIVE-LOCK NO-ERROR.
   
    IF AVAIL oe-ordl THEN DO:
       RUN delete-item(INPUT YES, INPUT NO).
       DELETE oe-ordl.                        
   END.
   ELSE DO:
       cError = "Can not delete this record".
       RETURN.
   END.
     FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK :
    FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum   NO-LOCK NO-ERROR.
    ASSIGN prmOrderNum = oe-ordl.ord-no 
        prmLine     = oe-ordl.LINE
          prmAction = "Select".
    END.  
    
END. /*IF prmAction = "delete"*/      


/**********************************************************************************************************/

IF prmAction = "Add" THEN DO:
    FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum AND oe-ord.company =  prmComp NO-LOCK NO-ERROR .
    FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
        ASSIGN    vLine = oe-ordl.LINE + 1.
    END.
    ELSE DO:
        ASSIGN  vLine = 1.
    END.
     ASSIGN 
           fgitem = prmItemNum.
     
     FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQno NO-LOCK NO-ERROR.
    
     IF (vEstType = 2 OR vEstType = 6) THEN DO:
      FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 0   NO-LOCK NO-ERROR.
       IF AVAIL eb  THEN   DO:
           IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
           END.
         END.
        END.
        ELSE DO:
        FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
          IF AVAIL eb  THEN   DO:
              IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
            
           END.
        END.
      END.
     
     IF fgitem = ""  THEN DO:
      
           FIND FIRST est WHERE est.company = prmComp AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
           IF AVAIL est THEN
           ASSIGN vEstType = est.est-type.

          IF v-est-fg1 EQ "Hughes" THEN
                   RUN fg/hughesfg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                      OUTPUT fgitem).
                   ELSE
                       IF v-est-fg1 EQ "Fibre" THEN
                           RUN fg/fibre-fg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                              OUTPUT fgitem).
               
              run crt-itemfg (fgitem, prmUom) NO-ERROR.

              IF prmEstimate <> "" THEN DO:
                IF prmQno <> 0  THEN DO:
                FIND CURRENT eb EXCLUSIVE-LOCK.
                IF AVAIL eb  THEN
                  ASSIGN
                      eb.stock-no = fgitem .

                    END.
                 END.
       END.
       
       FIND FIRST itemfg WHERE itemfg.company = prmComp AND itemfg.i-no = fgitem NO-LOCK NO-ERROR.
       IF NOT AVAIL itemfg THEN DO:
        run crt-itemfg (fgitem, prmUom) NO-ERROR.
       END.
       
    /****************************************************************************/

     IF prmJob <> "" THEN DO:
         
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
      ASSIGN
          v-disp-prod-cat = eb.procat.
      END.
      ASSIGN
          v-job-no = fill(" ",6 - length(trim(string(prmOrderNum)))) + string(prmOrderNum).

       RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-disp-prod-cat).
       IF v-job-no NE "" THEN DO:
         ASSIGN
          prmJob   = v-job-no
          prmJob2  = v-job-no2.
       END.
        
     END.

    /*******************************************************************************************/
    if prmJob = "" then do:
        
     find first itemfg
         where itemfg.company = prmComp
           and itemfg.i-no = fgitem
         NO-LOCK NO-ERROR.
     
     find first po-ordl where po-ordl.company   eq prmComp
                          and po-ordl.i-no      eq fgitem
                          and po-ordl.po-no     eq prmPrevOrder
                          and po-ordl.item-type eq no
                          use-index item-ordno no-lock no-error.
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
       assign prmUom = IF prmUom = "" THEN po-ordl.cons-uom ELSE prmUom
              lv-uom       = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
              prmCost      = po-ordl.cons-cost.
     else
     IF AVAIL itemfg THEN
       assign prmUom = if itemfg.prod-uom NE "" AND prmUom = "" then itemfg.prod-uom else prmUom
              lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
              prmCost   = itemfg.total-std-cost.

     if lv-uom ne "M" THEN do:
       run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                              prmCost, output lv-cost).
       assign prmCost = lv-cost.
     end.                       
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
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
              prmCost =(prmCost * (1 + (reftable.val[1]/ 100.0 ))).
              
              RELEASE reftable.
           END.
           RELEASE po-ord.
        END.
     END.     

   END.



    /**************************************************************/
     FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  /*prmDiscount = cust.disc.*/
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * prmDiscount / 100,2)).
    
     
    CREATE bf-ordl.
    ASSIGN
             bf-ordl.ord-no               = prmOrderNum
             bf-ordl.company              = prmComp
             bf-ordl.LINE                 = vLine
             bf-ordl.stat                 = "W" 
             bf-ordl.est-no               = prmEstimate 
             bf-ordl.q-no                 = prmQno
             bf-ordl.i-no                 = fgitem
             bf-ordl.part-no              = prmPartNum 
             bf-ordl.qty                  = prmQty         
             bf-ordl.i-name               = prmItemName    
             bf-ordl.part-dscr1           = prmPartdscr    
             bf-ordl.part-dscr2           = prmPartdscr1   
             bf-ordl.part-dscr3           = prmPartdscr2   
             bf-ordl.price                = prmPrice
             bf-ordl.t-price              = prmTPrice
             bf-ordl.pr-uom               = prmUom         
             bf-ordl.tax                  = IF prmTax = "yes" THEN TRUE ELSE FALSE    
             bf-ordl.po-no                = prmPoNum       
             bf-ordl.job-no               = prmJob
             bf-ordl.job-no2              = prmJob2
             bf-ordl.disc                 = prmDiscount            
             bf-ordl.req-code             = prmCode
             bf-ordl.req-date             = prmReqDate  
             bf-ordl.prom-code            = prmPromCode    
             bf-ordl.prom-date            = prmPromDate   
             bf-ordl.ship-qty             = prmShip        
             bf-ordl.cas-cnt              = prmCas         
             bf-ordl.partial              = prmPartial     
             bf-ordl.cases-unit           = prmUnit        
             bf-ordl.e-num                = prmEnum         
             bf-ordl.po-no-po             = prmPrevOrder   
             bf-ordl.s-man[1]             = prmSman       
             bf-ordl.s-man[2]             = prmSman2       
             bf-ordl.s-man[3]             = prmSman3       
             bf-ordl.type-code            = prmType        
             bf-ordl.over-pct             = prmOver        
             bf-ordl.under-pct            = prmUnder        
             bf-ordl.vend-no              = prmVend        
             bf-ordl.whsed                = IF prmManag = "yes" THEN TRUE ELSE FALSE 
             bf-ordl.s-pct[1]             = prmSpct       
             bf-ordl.s-pct[2]             = prmSpct2        
             bf-ordl.s-pct[3]             = prmSpct3 
             bf-ordl.s-comm[1]            = prmComm       
             bf-ordl.s-comm[2]            = prmComm2        
             bf-ordl.s-comm[3]            = prmComm3  
             bf-ordl.cost                 = prmCost      .
             FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
             FIND FIRST oe-ord WHERE oe-ord.ord-no = oe-ordl.ord-no AND oe-ord.company =  prmComp EXCLUSIVE-LOCK NO-ERROR.
             IF AVAIL oe-ordl THEN DO :
                 RUN MainBlock.
                 RUN check-use-1.
                 /*RUN check-use-2.                                 */
              
              IF  oe-ordl.est-no NE "" THEN
                  RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).
              
              IF oereleas-log THEN 
                  /*RUN create-release.*/
              /*RUN update-release.*/

              /****************************************************************************/
    
            
            RUN get-est-cost (prmEstimate).

            /****************************************************************************/

              FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = fgitem  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                ASSIGN
                    /*  itemfg.q-ono     = itemfg.q-ono +  prmQty*/
                    itemfg.q-alloc   = itemfg.q-alloc +  prmQty
                 .
             END.
             find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
             find first itemfg where itemfg.company eq cocode
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             if avail itemfg then do:
                 assign 
                     xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
                     oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
                     xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
                 END.

                ll-new-fg-created = NO. 

                RUN update-itemfg.
                 
                 RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT tfright ).
                 ASSIGN
                     oe-ordl.t-freight = tfright
                     oe-ord.t-freight = oe-ord.t-freight + tfright.
                 ASSIGN
                      oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000.

                 RUN oe/ordfrate.p (ROWID(oe-ord)). 
                 RUN oe/oe-comm.p.
                 RUN oe/calcordt.p (ROWID(oe-ord)).

                 
                  
                /* RUN   final-steps2.*/
         END. /* trans */

         FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN ASSIGN
            oe-ord.USER-ID = prmUser .
         FIND CURRENT oe-ord NO-LOCK NO-ERROR.
              
             /*ASSIGN prmLine = vLine
                 prmAction = "Select".*/
      
        
END.

/****************  Add App ****************************/

IF prmAction = "Add" AND prmMultiItem <> "" THEN DO:
    DO i = 1 TO NUM-ENTRIES(prmMultiItem).
        FIND FIRST quotehd WHERE quotehd.company = prmComp AND (quotehd.q-no = prmQno )  NO-LOCK NO-ERROR.
        IF AVAIL quotehd THEN
        FIND FIRST quoteitm  WHERE quoteitm.q-no = quotehd.q-no 
            AND quoteitm.company = prmComp AND quoteitm.i-no = ENTRY(i,prmMultiItem) NO-LOCK NO-ERROR.
        MESSAGE "ENTRY(i,prmMultiItem) " ENTRY(i,prmMultiItem) .
        IF NOT AVAIL quoteitm THEN NEXT.

        IF AVAIL quoteitm THEN DO:

            FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum AND oe-ord.company =  prmComp NO-LOCK NO-ERROR .
            FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN DO:
                ASSIGN    vLine = oe-ordl.LINE + 1.
            END.
            ELSE DO:
                ASSIGN  vLine = 1.
            END.

         ASSIGN 
           fgitem = quoteitm.i-no 
           prmEstimate = quotehd.est-no
           prmUom  = quoteitm.uom  
           prmQty     = quoteitm.qty  
           prmPrice   = quoteitm.price                      
           prmDiscount  = 0    
           /*prmType    = quoteitm.est-type */
           prmCas     = 0
           prmPartial  = 0                   
           prmUnit     = 0                  
           prmCost     = 0 .   

         FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no NE 0  NO-LOCK NO-ERROR.
                 IF AVAIL eb THEN
                   RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).

                   ASSIGN
                       prmCas      = li-cnt 
                       prmUnit   = li-cases .
    
     IF (vEstType = 2 OR vEstType = 6) THEN DO:
      FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 0   NO-LOCK NO-ERROR.
       IF AVAIL eb  THEN   DO:
           IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
           END.
         END.
        END.
        ELSE DO:
        FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
          IF AVAIL eb  THEN   DO:
              IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
            
           END.
        END.
      END.
     
     IF fgitem = ""  THEN DO:
      
           FIND FIRST est WHERE est.company = prmComp AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
           IF AVAIL est THEN
           ASSIGN vEstType = est.est-type.

          IF v-est-fg1 EQ "Hughes" THEN
                   RUN fg/hughesfg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                      OUTPUT fgitem).
                   ELSE
                       IF v-est-fg1 EQ "Fibre" THEN
                           RUN fg/fibre-fg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                              OUTPUT fgitem).
               
              run crt-itemfg (fgitem, prmUom) NO-ERROR.

              IF prmEstimate <> "" THEN DO:
                IF prmQno <> 0  THEN DO:
                FIND CURRENT eb EXCLUSIVE-LOCK.
                IF AVAIL eb  THEN
                  ASSIGN
                      eb.stock-no = fgitem .

                    END.
                 END.
       END.
       
       FIND FIRST itemfg WHERE itemfg.company = prmComp AND itemfg.i-no = fgitem NO-LOCK NO-ERROR.
       IF NOT AVAIL itemfg THEN DO:
        run crt-itemfg (fgitem, prmUom) NO-ERROR.
       END.
       
    /****************************************************************************/

     IF prmJob <> "" THEN DO:
         
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
      ASSIGN
          v-disp-prod-cat = eb.procat.
      END.
      ASSIGN
          v-job-no = fill(" ",6 - length(trim(string(prmOrderNum)))) + string(prmOrderNum).

       RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-disp-prod-cat).
       IF v-job-no NE "" THEN DO:
         ASSIGN
          prmJob   = v-job-no
          prmJob2  = v-job-no2.
       END.
        
     END.

    /*******************************************************************************************/
    if prmJob = "" then do:
        
     find first itemfg
         where itemfg.company = prmComp
           and itemfg.i-no = fgitem
         NO-LOCK NO-ERROR.
     
     find first po-ordl where po-ordl.company   eq prmComp
                          and po-ordl.i-no      eq fgitem
                          and po-ordl.po-no     eq prmPrevOrder
                          and po-ordl.item-type eq no
                          use-index item-ordno no-lock no-error.
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
       assign prmUom = IF prmUom = "" THEN po-ordl.cons-uom ELSE prmUom
              lv-uom       = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
              prmCost      = po-ordl.cons-cost.
     else
     IF AVAIL itemfg THEN
       assign prmUom = if itemfg.prod-uom NE "" AND prmUom = "" then itemfg.prod-uom else prmUom
              lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
              prmCost   = itemfg.total-std-cost.

     if lv-uom ne "M" THEN do:
       run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                              prmCost, output lv-cost).
       assign prmCost = lv-cost.
     end.                       
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
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
              prmCost =(prmCost * (1 + (reftable.val[1]/ 100.0 ))).
              
              RELEASE reftable.
           END.
           RELEASE po-ord.
        END.
     END.     

   END.



    /**************************************************************/
     FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  prmDiscount = cust.disc.
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * prmDiscount / 100,2)).
    
     
    CREATE bf-ordl.
    ASSIGN
             bf-ordl.ord-no               = prmOrderNum
             bf-ordl.company              = prmComp
             bf-ordl.LINE                 = vLine
             bf-ordl.stat                 = "W" 
             bf-ordl.est-no               = prmEstimate 
             bf-ordl.q-no                 = prmQno
             bf-ordl.i-no                 = fgitem
             bf-ordl.part-no              = quoteitm.part-no
             bf-ordl.qty                  = prmQty         
             bf-ordl.i-name               = itemfg.i-name    
             bf-ordl.part-dscr1           = quoteitm.part-dscr1    
             bf-ordl.part-dscr2           = quoteitm.part-dscr2   
             bf-ordl.part-dscr3           = quoteitm.part-dscr3  
             bf-ordl.price                = prmPrice
             bf-ordl.t-price              = prmTPrice
             bf-ordl.pr-uom               = prmUom         
             bf-ordl.tax                  = IF prmTax = "yes" THEN TRUE ELSE FALSE    
             bf-ordl.po-no                = prmPoNum       
             bf-ordl.job-no               = prmJob
             bf-ordl.job-no2              = prmJob2
             bf-ordl.disc                 = prmDiscount            
             bf-ordl.req-code             = prmCode
             bf-ordl.req-date             = prmReqDate  
             bf-ordl.prom-code            = prmPromCode    
             bf-ordl.prom-date            = prmPromDate   
             bf-ordl.ship-qty             = prmShip        
             bf-ordl.cas-cnt              = prmCas         
             bf-ordl.partial              = prmPartial     
             bf-ordl.cases-unit           = prmUnit        
             bf-ordl.e-num                = prmEnum         
             bf-ordl.po-no-po             = prmPrevOrder   
             bf-ordl.s-man[1]             = prmSman       
             bf-ordl.s-man[2]             = prmSman2       
             bf-ordl.s-man[3]             = prmSman3       
             bf-ordl.type-code            = prmType        
             bf-ordl.over-pct             = prmOver        
             bf-ordl.under-pct            = prmUnder        
             bf-ordl.vend-no              = prmVend        
             bf-ordl.whsed                = IF prmManag = "yes" THEN TRUE ELSE FALSE 
             bf-ordl.s-pct[1]             = prmSpct       
             bf-ordl.s-pct[2]             = prmSpct2        
             bf-ordl.s-pct[3]             = prmSpct3 
             bf-ordl.s-comm[1]            = prmComm       
             bf-ordl.s-comm[2]            = prmComm2        
             bf-ordl.s-comm[3]            = prmComm3  
             bf-ordl.cost                 = prmCost      .
             FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
             FIND FIRST oe-ord WHERE oe-ord.ord-no = oe-ordl.ord-no AND oe-ord.company =  prmComp EXCLUSIVE-LOCK NO-ERROR.
             IF AVAIL oe-ordl THEN DO :
                 RUN MainBlock.
                 RUN check-use-1.
                 /*RUN check-use-2.                                 */
              
              IF  oe-ordl.est-no NE "" THEN
                  RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).
              
              IF oereleas-log THEN 
                  /*RUN create-release.*/
              /*RUN update-release.*/

              /****************************************************************************/
    
            
            RUN get-est-cost (prmEstimate).

            /****************************************************************************/

              FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = fgitem  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                ASSIGN
                    /*  itemfg.q-ono     = itemfg.q-ono +  prmQty*/
                    itemfg.q-alloc   = itemfg.q-alloc +  prmQty
                 .
             END.
             find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
             find first itemfg where itemfg.company eq cocode
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             if avail itemfg then do:
                 assign 
                     xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
                     oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
                     xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
                 END.

                ll-new-fg-created = NO. 

                RUN update-itemfg.
                 
                 RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT tfright ).
                 ASSIGN
                     oe-ordl.t-freight = tfright
                     oe-ord.t-freight = oe-ord.t-freight + tfright.
                 ASSIGN
                      oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000.

                 RUN oe/ordfrate.p (ROWID(oe-ord)). 
                 RUN oe/oe-comm.p.
                 RUN oe/calcordt.p (ROWID(oe-ord)).

                 
                  
                /* RUN   final-steps2.*/
         END. /* trans */

         FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN ASSIGN
            oe-ord.USER-ID = prmUser .
         FIND CURRENT oe-ord NO-LOCK NO-ERROR.



        END.  /* avail quoteitm */

    END. /* do i*/



END.  /* add for app  */








/****************************************************************************************************************/


IF prmAction = "validateUpdate" THEN DO:    

    FIND FIRST  oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST  oe-ord WHERE oe-ord.company = oe-ordl.company AND oe-ordl.ord-no = oe-ordl.ord-no EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = oe-ordl.company AND (itemfg.i-no = prmItemNum OR prmItemNum = "")NO-LOCK NO-ERROR.
if not avail itemfg THEN DO:
     ASSIGN cError =  "Invalid FG Item#. Try help. " .
     RETURN.
             
   end. /* not avail */

   IF prmQty = 0 THEN DO:
       RETURN.
   END.

   IF date(prmReqDate) > DATE("12/31/2099") THEN DO:
       ASSIGN 
               cError ="Date lies between 01/01/1990 & 12/31/2099".
           RETURN.
            END.

      IF date(prmReqDate) < DATE("01/01/1990") THEN DO:
       ASSIGN 
               cError ="Date lies between 01/01/1990 & 12/31/2099".
           RETURN.
            END.

     /*if  date(prmReqDate) < oe-ord.ord-date THEN do:
       ASSIGN cError = "Due Date cannot be earlier than order date..." .
      RETURN.
       END.*/
    IF prmEstimate <> "" THEN DO:
    if date(prmPromDate) < oe-ord.ord-date then do:
       ASSIGN
           cError = "Scheduled Date cannot be earlier than order date..." .
       RETURN.
       END.
    end.
    if index(v-duelist,prmCode) <= 0 then do:
       ASSIGN cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    if index(v-duelist,prmPromCode) <= 0 then do:
       cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    
  

         /*
         else do:
            find first cust where cust.company = oe-ord.company
                              and cust.cust-no = oe-ord.cust-no no-lock no-error.
            if itemfg.cust-no ne oe-ord.cust-no and itemfg.cust-no ne "" and
               avail cust and cust.active ne "X"                         then do:
               find first cust where cust.company = prmComp and
                                     cust.cust-no = itemfg.cust-no
                                     no-lock no-error.
               if avail cust and cust.active ne "X" then do:                      
                  cError =  "This item exists for a different customer!. Do you want to continue?".
                  
               end.  
            end.  
         END.
         
*/
          IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman AND sman.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
     ASSIGN cError = "Invalid Salesman".
     RETURN.
    END.
    END.

    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2 AND sman.company = prmComp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
   ASSIGN cError = "Invalid Salesman".
   RETURN.
   END.
   END.

IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3 AND sman.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:
      ASSIGN cError = "Invalid Salesman".
      RETURN.
      END.
      END.

IF prmUom <> "" THEN DO:
   FIND uom WHERE uom.uom = prmUom   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE uom THEN DO:
       ASSIGN cError = "Invalid Uom".
       RETURN.
      END.
      END.

  /*IF prmPrevOrder <> 0 THEN DO:
   FIND po-ord WHERE po-ord.po-no = prmPrevOrder AND po-ord.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE po-ord THEN DO:
       ASSIGN cError = "Invalid Board Po#".
       RETURN.
      END.
      END.*/
  IF prmQno <> 0 THEN DO:
      FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQno  NO-LOCK  NO-ERROR.
      IF NOT AVAIL quotehd  THEN DO:
          ASSIGN
              cError = "Invalid Quote try help..." .
              RETURN.
      END.
  END.

END.   /*IF prmAction = "validateUpdate" THEN DO:*/

/***************************************************************************************************************/

IF prmAction = "Update" THEN DO:    

    FIND FIRST  oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST  oe-ord WHERE oe-ord.company = oe-ordl.company AND oe-ordl.ord-no = oe-ordl.ord-no EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = oe-ordl.company AND (itemfg.i-no = prmItemNum OR prmItemNum = "")NO-LOCK NO-ERROR.
if not avail itemfg THEN DO:
     ASSIGN cError =  "Invalid FG Item#. Try help. " .
     RETURN.
             
   end. /* not avail */

   IF date(prmReqDate) > DATE("12/31/2099") THEN DO:
       ASSIGN 
               cError ="Date lies between 01/01/1990 & 12/31/2099".
           RETURN.
            END.

      IF date(prmReqDate) < DATE("01/01/1990") THEN DO:
       ASSIGN 
               cError ="Date lies between 01/01/1990 & 12/31/2099".
           RETURN.
            END.

     /*if  date(prmReqDate) < oe-ord.ord-date THEN do:
       ASSIGN cError = "Due Date cannot be earlier than order date..." .
      RETURN.
       END.*/
    IF prmEstimate <> "" THEN DO:
    if date(prmPromDate) < oe-ord.ord-date then do:
       ASSIGN
           cError = "Scheduled Date cannot be earlier than order date..." .
       RETURN.
       END.
    end.
    if index(v-duelist,prmCode) <= 0 then do:
       ASSIGN cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    if index(v-duelist,prmPromCode) <= 0 then do:
       cError = "Invalid Priority Code. " .
       RETURN.
      
    end.

    
  

         /*
         else do:
            find first cust where cust.company = oe-ord.company
                              and cust.cust-no = oe-ord.cust-no no-lock no-error.
            if itemfg.cust-no ne oe-ord.cust-no and itemfg.cust-no ne "" and
               avail cust and cust.active ne "X"                         then do:
               find first cust where cust.company = prmComp and
                                     cust.cust-no = itemfg.cust-no
                                     no-lock no-error.
               if avail cust and cust.active ne "X" then do:                      
                  cError =  "This item exists for a different customer!. Do you want to continue?".
                  
               end.  
            end.  
         END.
         
*/
          IF prmSman <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman AND sman.company = prmComp NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sman THEN DO:
     ASSIGN cError = "Invalid Salesman".
     RETURN.
    END.
    END.

    IF prmSman2 <> "" THEN DO:
    FIND sman WHERE sman.sman = prmSman2 AND sman.company = prmComp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE sman THEN DO:
   ASSIGN cError = "Invalid Salesman".
   RETURN.
   END.
   END.

IF prmSman3 <> "" THEN DO:
   FIND sman WHERE sman.sman = prmSman3 AND sman.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sman THEN DO:
      ASSIGN cError = "Invalid Salesman".
      RETURN.
      END.
      END.

IF prmUom <> "" THEN DO:
   FIND uom WHERE uom.uom = prmUom   NO-LOCK NO-ERROR.
      IF NOT AVAILABLE uom THEN DO:
       ASSIGN cError = "Invalid Uom".
       RETURN.
      END.
      END.

  /*IF prmPrevOrder <> 0 THEN DO:
   FIND po-ord WHERE po-ord.po-no = prmPrevOrder AND po-ord.company = prmComp  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE po-ord THEN DO:
       ASSIGN cError = "Invalid Board Po#".
       RETURN.
      END.
      END.*/
  IF prmQno <> 0 THEN DO:
      FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQno  NO-LOCK  NO-ERROR.
      IF NOT AVAIL quotehd  THEN DO:
          ASSIGN
              cError = "Invalid Quote try help..." .
              RETURN.
      END.
  END.

END.   /*IF prmAction = "Update" THEN DO:*/


IF prmAction = "Update" THEN DO:       
    FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
              oe-ordl.ord-no = prmOrderNum AND
              oe-ordl.LINE = prmLine
             EXCLUSIVE-LOCK NO-ERROR.
    /*********************************************************************************/
 /*   PROCEDURE itemfg-cost :*/
     
 
  if prmJob = "" then do:

      find first itemfg
          where itemfg.company = prmComp
            and itemfg.i-no = prmItemNum
          NO-LOCK NO-ERROR.
      
      find first po-ordl where po-ordl.company   eq prmComp
                           and po-ordl.i-no      eq prmItemNum
                           and po-ordl.po-no     eq prmPrevOrder
                           and po-ordl.item-type eq no
                           use-index item-ordno no-lock no-error.
      if AVAIL po-ordl AND prmPrevOrder NE 0 then
        assign prmUom = IF prmUom = "" THEN po-ordl.cons-uom ELSE prmUom
               lv-uom       = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
               prmCost      = po-ordl.cons-cost.
      else
      IF AVAIL itemfg THEN
        assign prmUom = if itemfg.prod-uom NE "" AND prmUom = "" then itemfg.prod-uom else prmUom
               lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
               prmCost   = itemfg.total-std-cost.
      
      if lv-uom ne "M" THEN do:
        run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                               prmCost, output lv-cost).
        assign prmCost = lv-cost.
      end.                       
      if AVAIL po-ordl AND prmPrevOrder NE 0 then
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
               prmCost =(prmCost * (1 + (reftable.val[1]/ 100.0 ))).
               
               RELEASE reftable.
            END.
            RELEASE po-ord.
         END.
      END.
    END.


    /**********************************************************************************/
  
    FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
  /*prmDiscount = cust.disc.*/
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * prmDiscount / 100,2)).

    ASSIGN
        prvQty  = oe-ordl.qty .
    /*************************************************/

    FIND FIRST oe-ord where oe-ord.company EQ prmComp AND
              oe-ord.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN
        ASSIGN 
          oe-ord.stat = "W".
    /****************************************************/

         IF AVAIL oe-ordl THEN DO:
             
        ASSIGN
           
            oe-ordl.qty             = prmQty
            oe-ordl.i-no            = prmItemNum
            oe-ordl.i-name          = prmItemName 
            oe-ordl.part-no         = prmPartNum
            oe-ordl.part-dscr1      = prmPartdscr    
            oe-ordl.part-dscr2      = prmPartdscr1             
            oe-ordl.part-dscr3      = prmPartdscr2   
            oe-ordl.price           = prmPrice 
            oe-ordl.t-price         = prmTPrice
            oe-ordl.pr-uom          = prmUom    
            oe-ordl.tax             = IF prmTax = "yes" THEN TRUE ELSE FALSE    
            oe-ordl.po-no           = prmPoNum 
            oe-ordl.disc            = prmDiscount
            oe-ordl.req-code        = prmCode
            oe-ordl.req-date        = prmReqDate
            oe-ordl.t-price         = prmTPrice
            oe-ordl.prom-code       = prmPromCode
            oe-ordl.prom-date       = prmPromDate
            oe-ordl.ship-qty        = prmShip
            oe-ordl.cas-cnt         = prmCas
            oe-ordl.partial         = prmPartial
            oe-ordl.cases-unit      = prmUnit
            oe-ordl.e-num           = prmEnum
            oe-ordl.po-no-po        = prmPrevOrder   
            oe-ordl.s-man[1]        = prmSman 
            oe-ordl.s-man[2]        = prmSman2    
            oe-ordl.s-man[3]        = prmSman3    
            oe-ordl.type-code       = prmType  
            oe-ordl.over-pct        = prmOver  
            oe-ordl.under-pct       = prmUnder  
            oe-ordl.vend-no         = prmVend  
            oe-ordl.whsed           = IF prmManag = "yes" THEN TRUE ELSE FALSE
            oe-ordl.s-pct[1]        = prmSpct       
            oe-ordl.s-pct[2]        = prmSpct2        
            oe-ordl.s-pct[3]        = prmSpct3 
            oe-ordl.s-comm[1]       = prmComm       
            oe-ordl.s-comm[2]       = prmComm2        
            oe-ordl.s-comm[3]       = prmComm3  
            /*oe-ordl.cost            = prmCost*/      
            .

            assign
                lv-qty = oe-ordl.qty
                .

            lv-est-no = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) + TRIM(oe-ordl.est-no).
             ASSIGN v-rel = oe-ordl.rel .
            RUN get-est-cost2 (lv-est-no).

            
         
       END.  /*if  avail oe-ordl*/ 
       IF prvQty < prmQty THEN DO:
           fgQty = prmQty - prvQty .
           FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = oe-ordl.i-no  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                ASSIGN
                    itemfg.q-alloc   = itemfg.q-alloc +  fgQty.
                
                IF oe-ordl.est-no <> "" THEN DO:
                    ASSIGN
                        itemfg.q-ono     = itemfg.q-ono +  fgQty.
                END.
             END.
       END.
       IF prvQty > prmQty THEN DO:
           fgQty = prvQty - prmQty .
           FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = oe-ordl.i-no  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                ASSIGN
                    itemfg.q-alloc   = itemfg.q-alloc -  fgQty.
                
                   IF oe-ordl.est-no <> "" THEN DO:
                    ASSIGN
                        itemfg.q-ono     = itemfg.q-ono -  fgQty.
                END.
             END.
       END.

       
       find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
             find first itemfg where itemfg.company eq cocode
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             if avail itemfg then do:       
                 assign 
                     xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
                     oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
                     xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
                

                 END.
                                 
                 

                 ll-new-fg-created = NO.
                 
                 RUN update-itemfg.

                 RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT tfright ).
                 ASSIGN
                     oe-ordl.t-freight = tfright  .
                     FOR EACH oe-ordl OF oe-ord :
                         RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT frighiout).
                         ld = ld + frighiout .
                         END.
                         ASSIGN
                             oe-ord.t-freight = ld .

                   
                 RUN oe/ordfrate.p (ROWID(oe-ord)).
                  RUN oe/oe-comm.p.
                 RUN oe/calcordt.p (ROWID(oe-ord)).                 

                 FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no))) + TRIM(oe-ord.est-no) AND est.company = prmComp NO-LOCK NO-ERROR.
                 IF AVAIL est  THEN DO:
                     
                     IF est.est-type = 2 OR est.est-type = 6 THEN DO:
                        
                         find first eb
                             where eb.company = cocode 
                             and eb.est-no = est.est-no
                             and eb.form-no  eq 0
                             no-lock no-error.  
                    
                         find first oe-ordl where oe-ordl.company  eq cocode
                                 and oe-ordl.ord-no   eq oe-ord.ord-no
                                 and oe-ordl.cust-no  eq oe-ord.cust-no
                                 and (oe-ordl.part-no eq eb.part-no
                                      or  (oe-ordl.i-no   eq eb.stock-no and eb.stock-no ne "")) NO-LOCK NO-ERROR.
                         IF AVAIL oe-ordl THEN DO:
                                ASSIGN
                                    prmQty = oe-ordl.qty .
                                
                         for each eb
                             where eb.company = prmComp 
                             and eb.est-no  eq est.est-no
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
                             
                             find first oe-ordl where oe-ordl.company  eq cocode
                                 and oe-ordl.ord-no   eq oe-ord.ord-no
                                 and oe-ordl.cust-no  eq oe-ord.cust-no
                                 and (oe-ordl.part-no eq eb.part-no
                                      or  (oe-ordl.i-no   eq eb.stock-no and eb.stock-no ne "")) EXCLUSIVE-LOCK no-error.
                                 IF AVAIL oe-ordl THEN  ASSIGN
                                     oe-ordl.qty        = prmQty  * eb.yld-qty.
                         END.
                         END.

                     END.
                 END.
                 FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL oe-ord THEN ASSIGN
            oe-ord.USER-ID = prmUser .
         FIND CURRENT oe-ord NO-LOCK NO-ERROR.
                 
       ASSIGN prmAction = "Select".
END.
/******************************************************************************************************************/

  
  IF prmAction = "AddWebItem" THEN DO:
    FIND FIRST oe-ord WHERE oe-ord.ord-no = prmOrderNum AND oe-ord.company =  prmComp NO-LOCK NO-ERROR .
    FIND LAST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrderNum NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
        ASSIGN    vLine = oe-ordl.LINE + 1.
    END.
    ELSE DO:
        ASSIGN  vLine = 1.
    END.
     ASSIGN 
           fgitem = prmItemNum.
     
     FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQno NO-LOCK NO-ERROR.

     FIND FIRST  quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp
         AND quoteitm.i-no = fgitem NO-LOCK NO-ERROR.
     IF NOT AVAIL quoteitm THEN
        FIND FIRST  quoteitm  WHERE quoteitm.q-no = quotehd.q-no AND quoteitm.company = prmComp
          NO-LOCK NO-ERROR.

     IF AVAIL quoteitm THEN
         ASSIGN
         prmEstimate   = quotehd.est-no  
         /*ttItemQuoteLook.vcust        = quotehd.cust-no */
         prmPartNum    = quoteitm.part-no
         prmUom         = quoteitm.uom
         prmPartdscr    = quoteitm.part-dscr1
         prmPartdscr1  = quoteitm.part-dscr2
         prmPartdscr2  = quoteitm.part-dscr3
         prmPrice      = quoteitm.price
         prmQty        = quoteitm.qty 
         /*prmType       = quoteitm.est-type*/ .
    
       IF AVAIL oe-ord THEN
           ASSIGN
           prmPoNum        =  oe-ord.po-no 
           prmJob          = STRING(oe-ord.ord-no)                  
           prmJob2         =  0 
           prmReqDate      = oe-ord.due-date                     
           prmPromCode     =  oe-ord.due-code                          
           prmPromDate     =  oe-ord.prod-date 
           prmOver         =  oe-ord.over-pct                     
           prmUnder        =  oe-ord.under-pct 
           prmSman         =  oe-ord.sman[1]  .



      FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no NE 0  NO-LOCK NO-ERROR.

                   RUN est/getcscnt.p (ROWID(eb),
                         OUTPUT li-cnt,OUTPUT li-cases).

                   ASSIGN
                       prmCas      = li-cnt 
                       prmUnit   = li-cases 
                       .
                IF fgitem = "" THEN
                    fgitem = eb.stock-no .
              
      FIND FIRST est WHERE est.company = prmComp AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
           IF AVAIL est THEN
           ASSIGN vEstType = est.est-type.
    
     IF (vEstType = 2 OR vEstType = 6) THEN DO:
      FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 0   NO-LOCK NO-ERROR.
       IF AVAIL eb  THEN   DO:
           IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
           END.
         END.
        END.
        ELSE DO:
        FIND FIRST eb WHERE  eb.company = prmComp AND eb.est-no = quotehd.est-no AND eb.form-no = 1 NO-LOCK NO-ERROR.
          IF AVAIL eb  THEN   DO:
              IF fgitem = "" THEN DO:
           ASSIGN
               fgitem = eb.stock .
            
           END.
        END.
      END.
     
     IF fgitem = ""  THEN DO:
      
           FIND FIRST est WHERE est.company = prmComp AND est.est-no = quotehd.est-no NO-LOCK NO-ERROR.
           IF AVAIL est THEN
           ASSIGN vEstType = est.est-type.

          IF v-est-fg1 EQ "Hughes" THEN
                   RUN fg/hughesfg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                      OUTPUT fgitem).
                   ELSE
                       IF v-est-fg1 EQ "Fibre" THEN
                           RUN fg/fibre-fg.p ((IF vEstType EQ 2 AND AVAIL xeb THEN ROWID(xeb) ELSE ROWID(eb)),
                                              OUTPUT fgitem).
               
              run crt-itemfg (fgitem, prmUom) NO-ERROR.

              IF prmEstimate <> "" THEN DO:
                IF prmQno <> 0  THEN DO:
                FIND CURRENT eb EXCLUSIVE-LOCK.
                IF AVAIL eb  THEN
                  ASSIGN
                      eb.stock-no = fgitem .

                    END.
                 END.
       END.
       
       FIND FIRST itemfg WHERE itemfg.company = prmComp AND itemfg.i-no = fgitem NO-LOCK NO-ERROR.

       IF NOT AVAIL itemfg THEN DO:
        run crt-itemfg (fgitem, prmUom) NO-ERROR.
       END.
       ELSE
           prmItemName  = itemfg.i-name .
           
    /****************************************************************************/

     IF prmJob <> "" THEN DO:
         
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
      IF AVAIL eb THEN DO:
      ASSIGN
          v-disp-prod-cat = eb.procat.
      END.
      ASSIGN
          v-job-no = fill(" ",6 - length(trim(string(prmOrderNum)))) + string(prmOrderNum).

       RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-disp-prod-cat).
       IF v-job-no NE "" THEN DO:
         ASSIGN
          prmJob   = v-job-no
          prmJob2  = v-job-no2.
       END.
        
     END.

    /*******************************************************************************************/
    if prmJob = "" then do:
        
     find first itemfg
         where itemfg.company = prmComp
           and itemfg.i-no = fgitem
         NO-LOCK NO-ERROR.
     
     find first po-ordl where po-ordl.company   eq prmComp
                          and po-ordl.i-no      eq fgitem
                          and po-ordl.po-no     eq prmPrevOrder
                          and po-ordl.item-type eq no
                          use-index item-ordno no-lock no-error.
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
       assign prmUom = IF prmUom = "" THEN po-ordl.cons-uom ELSE prmUom
              lv-uom       = if po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
              prmCost      = po-ordl.cons-cost.
     else
     IF AVAIL itemfg THEN
       assign prmUom = if itemfg.prod-uom NE "" AND prmUom = "" then itemfg.prod-uom else prmUom
              lv-uom                      = if itemfg.prod-uom NE "" THEN itemfg.prod-uom ELSE "M"
              prmCost   = itemfg.total-std-cost.

     if lv-uom ne "M" THEN do:
       run sys/ref/convcuom.p(lv-uom, "M", 0, 0, 0, 0,
                              prmCost, output lv-cost).
       assign prmCost = lv-cost.
     end.                       
     if AVAIL po-ordl AND prmPrevOrder NE 0 then
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
              prmCost =(prmCost * (1 + (reftable.val[1]/ 100.0 ))).
              
              RELEASE reftable.
           END.
           RELEASE po-ord.
        END.
     END.     

   END.



    /**************************************************************/
     FIND FIRST cust WHERE cust.company = prmComp AND cust.cust-no = oe-ord.cust-no NO-LOCK NO-ERROR.
    prmDiscount = cust.disc.
   v-tmp-price = if prmUom begins "L" AND prmUom NE "LB" then
                   if prmQty lt 0 then -1 else 1
                 else
                 if prmUom eq "CS" then
                   prmQty / (if prmCas ne 0 THEN prmCas else
                                    if  prmCas ne 0
                                                   then prmCas else
                                                        1)
                 else
                 if prmUom eq "C" then
                    prmQty / 100
                 else
                 if  prmUom eq "M" then
                    prmQty / 1000
                 else
                    prmQty.
                            
    lv-t-price = v-tmp-price * prmPrice.
    prmTPrice = (lv-t-price - ROUND(lv-t-price * prmDiscount / 100,2)).
    
     
    CREATE bf-ordl.
    ASSIGN
             bf-ordl.ord-no               = prmOrderNum
             bf-ordl.company              = prmComp
             bf-ordl.LINE                 = vLine
             bf-ordl.stat                 = "W" 
             bf-ordl.est-no               = prmEstimate 
             bf-ordl.q-no                 = prmQno
             bf-ordl.i-no                 = fgitem
             bf-ordl.part-no              = prmPartNum 
             bf-ordl.qty                  = prmQty         
             bf-ordl.i-name               = prmItemName    
             bf-ordl.part-dscr1           = prmPartdscr    
             bf-ordl.part-dscr2           = prmPartdscr1   
             bf-ordl.part-dscr3           = prmPartdscr2   
             bf-ordl.price                = prmPrice
             bf-ordl.t-price              = prmTPrice
             bf-ordl.pr-uom               = prmUom         
             bf-ordl.tax                  = IF prmTax = "yes" THEN TRUE ELSE FALSE    
             bf-ordl.po-no                = prmPoNum       
             bf-ordl.job-no               = prmJob
             bf-ordl.job-no2              = prmJob2
             bf-ordl.disc                 = prmDiscount            
             bf-ordl.req-code             = prmCode
             bf-ordl.req-date             = prmReqDate  
             bf-ordl.prom-code            = prmPromCode    
             bf-ordl.prom-date            = prmPromDate   
             bf-ordl.ship-qty             = prmShip        
             bf-ordl.cas-cnt              = prmCas         
             bf-ordl.partial              = prmPartial     
             bf-ordl.cases-unit           = prmUnit        
             bf-ordl.e-num                = prmEnum         
             bf-ordl.po-no-po             = prmPrevOrder   
             bf-ordl.s-man[1]             = prmSman       
             bf-ordl.s-man[2]             = prmSman2       
             bf-ordl.s-man[3]             = prmSman3       
             bf-ordl.type-code            = prmType        
             bf-ordl.over-pct             = prmOver        
             bf-ordl.under-pct            = prmUnder        
             bf-ordl.vend-no              = prmVend        
             bf-ordl.whsed                = IF prmManag = "yes" THEN TRUE ELSE FALSE 
             bf-ordl.s-pct[1]             = prmSpct       
             bf-ordl.s-pct[2]             = prmSpct2        
             bf-ordl.s-pct[3]             = prmSpct3 
             bf-ordl.s-comm[1]            = prmComm       
             bf-ordl.s-comm[2]            = prmComm2        
             bf-ordl.s-comm[3]            = prmComm3  
             bf-ordl.cost                 = prmCost      .
             FIND FIRST oe-ordl WHERE oe-ordl.ord-no = prmOrderNum EXCLUSIVE-LOCK NO-ERROR.
             FIND FIRST oe-ord WHERE oe-ord.ord-no = oe-ordl.ord-no AND oe-ord.company =  prmComp EXCLUSIVE-LOCK NO-ERROR.
             IF AVAIL oe-ordl THEN DO :
                 RUN MainBlock.
                 RUN check-use-1.
                 /*RUN check-use-2.                                 */
              
              IF  oe-ordl.est-no NE "" THEN
                  RUN oe/ordlmisc.p (ROWID(oe-ordl), oe-ordl.qty).
              
              IF oereleas-log THEN 
                  /*RUN create-release.*/
              /*RUN update-release.*/

              /****************************************************************************/
    
            
            RUN get-est-cost (prmEstimate).

            /****************************************************************************/

              FIND FIRST itemfg WHERE itemfg.company = prmComp AND
            itemfg.i-no = fgitem  EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                ASSIGN
                    /*  itemfg.q-ono     = itemfg.q-ono +  prmQty*/
                    itemfg.q-alloc   = itemfg.q-alloc +  prmQty
                 .
             END.
             find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
             find first itemfg where itemfg.company eq cocode
                 and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             if avail itemfg then do:
                 assign 
                     xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
                     oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
                     xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
                 END.

                ll-new-fg-created = NO. 

                RUN update-itemfg.
                 
                 RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT tfright ).
                 ASSIGN
                     oe-ordl.t-freight = tfright
                     oe-ord.t-freight = oe-ord.t-freight + tfright.
                 ASSIGN
                      oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000.

                 RUN oe/ordfrate.p (ROWID(oe-ord)). 
                 RUN oe/oe-comm.p.
                 RUN oe/calcordt.p (ROWID(oe-ord)).

                 
                  
                /* RUN   final-steps2.*/
         END. /* trans */
              
             ASSIGN prmLine = vLine
                 prmAction = "Select".
      
        
END.    /* add item web   */





/*******************************************************************************************************************/

IF prmAction = "Select" THEN DO:
    
FIND FIRST oe-ordl where oe-ordl.company EQ prmComp AND
             oe-ordl.ord-no = prmOrderNum AND
             oe-ordl.LINE = prmLine
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ordl THEN DO:
            create ttViewItemEstimateApp.
            assign 
                ttViewItemEstimateApp.OrdNo          = oe-ordl.ord-no
                ttViewItemEstimateApp.vLine          = oe-ordl.LINE
                ttViewItemEstimateApp.est-no         = oe-ordl.est-no
                ttViewItemEstimateApp.Item1          = oe-ordl.i-no
                ttViewItemEstimateApp.CustPart       = oe-ordl.part-no
                ttViewItemEstimateApp.quantity       = oe-ordl.qty
                ttViewItemEstimateApp.Name1          = oe-ordl.i-name
                ttViewItemEstimateApp.Dscr           = oe-ordl.part-dscr1 
                ttViewItemEstimateApp.Dscr2          = oe-ordl.part-dscr2 
                ttViewItemEstimateApp.Dscr3          = oe-ordl.part-dscr3
                ttViewItemEstimateApp.price          = oe-ordl.price
                ttViewItemEstimateApp.uom            = oe-ordl.pr-uom
                ttViewItemEstimateApp.taxable        = oe-ordl.tax
                ttViewItemEstimateApp.custpo         = oe-ordl.po-no
                ttViewItemEstimateApp.job-no         = oe-ordl.job-no
                ttViewItemEstimateApp.job-no2         = oe-ordl.job-no2
                ttViewItemEstimateApp.discount       = oe-ordl.disc
                ttViewItemEstimateApp.requested      = oe-ordl.req-code
                ttViewItemEstimateApp.requestdate    = oe-ordl.req-date
                ttViewItemEstimateApp.extprice       = oe-ordl.t-price
                ttViewItemEstimateApp.promised       = oe-ordl.prom-code
                ttViewItemEstimateApp.promisdate     = oe-ordl.prom-date
                ttViewItemEstimateApp.shipqty        = oe-ordl.ship-qty
                ttViewItemEstimateApp.counter        = oe-ordl.cas-cnt
                ttViewItemEstimateApp.vPartial       =  oe-ordl.partial
                ttViewItemEstimateApp.VcasUnit       =  oe-ordl.cases-unit 
                ttViewItemEstimateApp.vEnum          = oe-ordl.e-num 
                ttViewItemEstimateApp.vPono          = oe-ordl.po-no-po 
                ttViewItemEstimateApp.vSman          =  oe-ordl.s-man[1]
                ttViewItemEstimateApp.vSpct          =  oe-ordl.s-pct[1]
                ttViewItemEstimateApp.vScomm         = oe-ordl.s-comm[1]
                ttViewItemEstimateApp.vSman2         =  oe-ordl.s-man[2]
                ttViewItemEstimateApp.vSpct2         =  oe-ordl.s-pct[2]
                ttViewItemEstimateApp.vScomm2        =  oe-ordl.s-comm[2]
                ttViewItemEstimateApp.vSman3         = oe-ordl.s-man[3]
                ttViewItemEstimateApp.vSpct3         =  oe-ordl.s-pct[3]
                ttViewItemEstimateApp.vScomm3        = oe-ordl.s-comm[3]
                ttViewItemEstimateApp.vType          = oe-ordl.type-code
                ttViewItemEstimateApp.vOver          = oe-ordl.over-pct 
                ttViewItemEstimateApp.vUnder         = oe-ordl.under-pct
                ttViewItemEstimateApp.vBoardVen      = oe-ordl.vend-no
                ttViewItemEstimateApp.vManag         = oe-ordl.whsed
                ttViewItemEstimateApp.vCost          = oe-ordl.cost  
                ttViewItemEstimateApp.vQno           = oe-ordl.q-no
                ttViewItemEstimateApp.vReckey           = oe-ordl.rec_key

                           .
                               


                                             
            FOR EACH sman NO-LOCK :
                IF sman.sman = oe-ordl.s-man[1] THEN
                    ASSIGN ttViewItemEstimateApp.vSname         = sman.sname .
                IF sman.sman = oe-ordl.s-man[2] THEN 
                    ASSIGN ttViewItemEstimateApp.vSname2        = sman.sname.
                IF sman.sman = oe-ordl.s-man[3] THEN 
                    ASSIGN ttViewItemEstimateApp.vSname3        = sman.sname.
            END.   /*for each sman */

END.   /*if avail oe-ordl*/
END.  /*PrmAction = Select*/



/****************************************************************************************************/
PROCEDURE get-eb-info :
  
  /*else prmItemNum = "".*/
  
END PROCEDURE.
/****************************************************************************************************/
/*PROCEDURE new-s-man :
    DEF INPUT PARAM ip-int AS INT NO-UNDO.
    DEF VAR lv-sman LIKE sman.sman NO-UNDO.
    DEF VAR ll-all AS LOG NO-UNDO.
    DEF VAR li AS INT NO-UNDO.
    
    IF ip-int EQ 0 THEN
        ASSIGN
        li     = 3
        ip-int = 1
        ll-all = YES.
    ELSE
        li = ip-int.
        
        DO ip-int = ip-int TO li :
            lv-sman = IF ip-int EQ 3 THEN prmSman3
                ELSE
                    IF ip-int EQ 2 THEN prmSman2
                        ELSE prmSman.
                             
    IF lv-sman NE "" THEN DO:
      FIND FIRST sman
          WHERE sman.company EQ prmComp
            AND sman.sman    EQ prmSman
          NO-LOCK NO-ERROR.
      IF AVAIL sman THEN DO:
        IF ip-int EQ 3 THEN DO:
          prmSname3 = sman.sname.
          IF NOT ll-all THEN DO:
            IF prmSpct3 EQ 0 THEN
              prmSpct3 = 100.
            IF prmComm3 EQ 0 THEN
              prmComm3 = sman.scomm.
          END.
        END.
        ELSE
        IF ip-int EQ 2 THEN DO:
          prmSname2 = sman.sname.
          IF NOT ll-all THEN DO:
            IF prmSpct2 EQ 0 THEN
              prmSpct2 = "100".
            IF prmComm2 EQ 0 THEN
              prmComm2 = sman.scomm.
          END.
        END.
        ELSE DO:
          prmSname = sman.sname.
          IF NOT ll-all THEN DO:
            IF prmSpct EQ 0 THEN
              prmSpct = "100".
            IF PrmComm EQ 0 THEN
              prmComm = sman.scomm.
          END.
        END.
      END.
    END.
  END.

END PROCEDURE.
*/

/************************************************************************************************/
/*
PROCEDURE whs-item :
  DEF INPUT PARAM ip-int AS INT NO-UNDO.
  FIND FIRST oe-ordl-whs-item NO-LOCK
      WHERE oe-ordl-whs-item.reftable EQ "oe-ordl.whs-item"
        AND oe-ordl-whs-item.company  EQ oe-ordl.company
        AND oe-ordl-whs-item.loc      EQ STRING(oe-ordl.ord-no,"9999999999")
        AND oe-ordl-whs-item.code     EQ oe-ordl.i-no
        AND oe-ordl-whs-item.code2    EQ STRING(oe-ordl.line,"9999999999")
      NO-ERROR.

  IF ip-int EQ 0 THEN DO :
    ASSIGN
     ll-prev-whs-item = AVAIL oe-ordl-whs-item AND oe-ordl-whs-item.val[1] EQ 1
     tb_whs-item:SCREEN-VALUE = STRING(ll-prev-whs-item,"yes/no").
  END.

  ELSE
  IF AVAIL oe-ordl-whs-item THEN DO TRANSACTION:
    IF ll-prev-whs-item NE tb_whs-item THEN v-qty-mod = YES.
    ll-prev-whs-item = tb_whs-item.
    FIND CURRENT oe-ordl-whs-item.
    oe-ordl-whs-item.val[1] = INT(tb_whs-item).
    FIND CURRENT oe-ordl-whs-item NO-LOCK.
  END.

END PROCEDURE.*/
/*********************************************************************************************/
/***********************************************************************************************************************/
PROCEDURE valid-po-no :
  
END PROCEDURE.

/******************************************************************************************************/

PROCEDURE update-itemfg :
def var v-flag as log extent 9 no-undo.
def var v-prompt as log no-undo.
def var ls-flag as cha no-undo.
DEF VAR lv-est-no LIKE itemfg.est-no NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-upd-oe-ordl FOR oe-ordl.

DISABLE TRIGGERS FOR LOAD OF eb.
    
find first sys-ctrl  where sys-ctrl.company eq prmComp
                       and sys-ctrl.name    eq "OEFGUPDT"
                       no-lock no-error.
/*if not avail sys-ctrl then do:
   create sys-ctrl.
   assign sys-ctrl.company = prmComp
          sys-ctrl.name    = "OEFGUPDT"
          sys-ctrl.descrip =
              "Update FG? Sell Price,UOM,Count,Name,Desc1,Desc2,Job,Vendor,Est."
          sys-ctrl.log-fld = no.

   run oe/d-asgnfg.w  (recid(sys-ctrl), oe-ordl.est-no, "New", output ls-flag).       


end. /* not avail sys-ctrl */

*/


find first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-ordl.i-no
    exclusive-lock no-error.
    
if avail itemfg then do:    
  /*FIND oe-ord OF oe-ordl NO-LOCK.*/

  /*if sys-ctrl.log-fld then 
     run oe/d-asgnfg.w (recid(sys-ctrl), oe-ordl.est-no,"exist",output ls-flag).

  else do:
  */
     find first sys-ctrl  where sys-ctrl.company eq prmComp
                       and sys-ctrl.name    eq "OEFGUPDT"
                       no-lock no-error.
     ls-flag = sys-ctrl.char-fld.
     substring(ls-flag,9,1) = string(sys-ctrl.int-fld eq 1 and oe-ordl.est-no ne "","Y/N").

  /*end. */
  assign v-flag[1] = substring(ls-flag,1,1) = "Y"
         v-flag[2] = substring(ls-flag,2,1) = "Y"
         v-flag[3] = substring(ls-flag,3,1) = "Y"
         v-flag[4] = substring(ls-flag,4,1) = "Y"
         v-flag[5] = substring(ls-flag,5,1) = "Y"
         v-flag[6] = substring(ls-flag,6,1) = "Y"
         v-flag[7] = substring(ls-flag,7,1) = "Y"
         v-flag[8] = substring(ls-flag,8,1) = "Y"
         v-flag[9] = substring(ls-flag,9,1) = "Y"
         .



  if v-flag[1] or ll-new-fg-created OR prmNewItemCreated = "y" then 
                    itemfg.sell-price  = dec(prmPrice).
  if v-flag[2] OR prmNewItemCreated = "y" then itemfg.sell-uom    = prmUom.
  if v-flag[3] then 
             assign itemfg.case-count  = oe-ordl.cas-cnt
                    itemfg.case-pall   = oe-ordl.cases-unit.
  if v-flag[4] then itemfg.i-name      = prmItemName.
  if v-flag[5] then itemfg.part-dscr1  = prmPartdscr.
  if v-flag[6] then itemfg.part-dscr2  = prmPartdscr2.
  if v-flag[7] then itemfg.cust-job-no = oe-ordl.job-no + "-" +
                                         string(oe-ordl.job-no2).
  if v-flag[8] then itemfg.vend-no     = oe-ordl.vend-no.
  
  if v-flag[9] then do:
    itemfg.est-no = oe-ordl.est-no.
    
    
    FOR EACH w-est-no:
      DELETE w-est-no.
    END.

    CREATE w-est-no.
    w-est-no = itemfg.est-no.

    
    DO WHILE AVAIL w-est-no:
      ASSIGN
       w-run     = YES
       lv-est-no = w-est-no.

       
      FOR EACH eb
          WHERE eb.company   EQ oe-ordl.company
            AND eb.est-no    EQ lv-est-no
            AND eb.cust-no   EQ oe-ord.cust-no
            AND ((eb.part-no EQ oe-ordl.part-no AND eb.stock-no EQ "") OR
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
          AND ((eb.part-no EQ oe-ordl.part-no AND eb.stock-no EQ "") OR
               eb.stock-no EQ oe-ordl.i-no):

      ASSIGN
       eb.stock-no   = oe-ordl.i-no
       eb.part-no    = oe-ordl.part-no
       eb.part-dscr1 = oe-ordl.i-name
       eb.part-dscr2 = oe-ordl.part-dscr1.

      
      IF v-oecount-int EQ 1 THEN DO:
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

  /*FOR EACH b-oe-ordl
      WHERE b-oe-ordl.company EQ itemfg.company
        AND b-oe-ordl.i-no    EQ itemfg.i-no
        AND b-oe-ordl.opened  EQ YES
        AND ROWID(b-oe-ordl)  NE ROWID(oe-ordl)
      NO-LOCK:

    FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF AVAIL b-upd-oe-ordl THEN DO:
      b-upd-oe-ordl.part-no                      = oe-ordl.part-no.
      /*
      IF v-flag[3] THEN b-upd-oe-ordl.cas-cnt    = oe-ordl.cas-cnt.
      */
      IF v-flag[4] THEN b-upd-oe-ordl.i-name     = oe-ordl.i-name.
      IF v-flag[5] THEN b-upd-oe-ordl.part-dscr1 = oe-ordl.part-dscr1.
      IF v-flag[6] THEN b-upd-oe-ordl.part-dscr2 = oe-ordl.part-dscr2.
    END.

    MESSAGE "inam" oe-ordl.i-name  v-flag[4].

    FIND b-upd-oe-ordl WHERE ROWID(b-upd-oe-ordl) EQ ROWID(b-oe-ordl)
        NO-LOCK NO-ERROR.
  END. 
  */
end.


RELEASE itemfg.

END PROCEDURE.
/*********************************************************************************************/

PROCEDURE create-release :
  def var v-qty-sum as int no-undo.
  def var v-nxt-r-no as int init 1 no-undo.
  def var v-lst-rel as date no-undo.
  def var v-pct-chg as dec no-undo.
  def var v-ship-id like oe-rel.ship-id no-undo.
  def var v-num-shipto as int no-undo.
  

  assign v-qty-sum  = 0.
  {oe/oe-rel.a &fil="oe-ordl"}
  assign v-ship-id = ""
         lv-qty = oe-ordl.qty
         .
  
  find first xoe-rel where xoe-rel.company eq prmComp
                       and xoe-rel.ord-no  eq oe-ordl.ord-no
                       and recid(xoe-rel)  ne recid(oe-rel)
                       and xoe-rel.link-no eq 0
                       no-lock no-error.
  if not avail xoe-rel OR oe-ordl.est-no NE "" then do:
     for each shipto where shipto.company eq prmComp
                       and shipto.cust-no eq oe-ordl.cust-no:
               assign v-num-shipto = v-num-shipto + 1.
     end.
     if v-num-shipto gt 1 then
     do:
         IF oe-ordl.est-no NE "" THEN
         FOR EACH eb
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.cust-no  EQ oe-ord.cust-no
               AND eb.ship-id  NE ""
             NO-LOCK
             BREAK BY eb.stock-no DESC:
           IF LAST(eb.stock-no)           OR
              eb.stock-no EQ oe-ordl.i-no THEN DO:
             v-ship-id = eb.ship-id.
             LEAVE.
           END.
         END.

         ELSE
         FOR EACH shipto
             WHERE shipto.company EQ prmComp
               AND shipto.cust-no EQ oe-ord.cust-no
             NO-LOCK
             BREAK BY shipto.ship-no DESC:
           IF shipto.ship-id EQ oe-ord.cust-no THEN DO:
             v-ship-id = shipto.ship-id.
             LEAVE.
           END.
         END.
         /* task# 09160502 */
         DEF BUFFER bf-ordl FOR oe-ordl.
         DEF BUFFER bf-rel FOR oe-rel.
         IF oe-ord.est-no = "" THEN
            FIND FIRST bf-ordl WHERE bf-ordl.company = oe-ordl.company
                              AND bf-ordl.ord-no = oe-ordl.ord-no
                              AND bf-ordl.i-no <> ""
                              AND RECID(bf-ordl) <> RECID(oe-ordl) NO-LOCK NO-ERROR.

         IF AVAIL bf-ordl AND oeship-cha = "OEShipto" THEN do:
            FIND FIRST bf-rel WHERE bf-rel.company EQ bf-ordl.company 
                                AND bf-rel.ord-no  EQ bf-ordl.ord-no 
                                AND bf-rel.i-no    EQ bf-ordl.i-no   
                                AND bf-rel.line    EQ bf-ordl.line NO-LOCK NO-ERROR.
            v-ship-id = IF AVAIL bf-rel THEN bf-rel.ship-id ELSE v-ship-id.
         END.
         /*ELSE run oe/d-shipid.w (input oe-ordl.cust-no, input-output v-ship-id)  .*/
         FIND FIRST shipto WHERE shipto.company = g_company
                       AND shipto.cust-no = oe-ordl.cust-no
                       AND shipto.ship-id = v-ship-id
                       NO-LOCK NO-ERROR.
   /*IF NOT AVAIL shipto THEN DO:
      MESSAGE "Invalid Shipto. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO v-ship-id.
      RETURN NO-APPLY.*/

        assign oe-rel.ship-id = trim(v-ship-id).
                  find first shipto where shipto.company = prmComp and
                                  shipto.cust-no = xoe-ord.cust-no  and
                                  shipto.ship-id = v-ship-id
                                  use-index ship-id no-lock no-error.
         if AVAIL shipto then do:
            assign v-ship-id           = shipto.ship-id
                   oe-rel.ship-no      = shipto.ship-no
                                oe-rel.ship-id      = shipto.ship-id
                                oe-rel.ship-addr[1] = shipto.ship-addr[1]
                                oe-rel.ship-addr[2] = shipto.ship-addr[2]
                                oe-rel.ship-city    = shipto.ship-city
                                oe-rel.ship-state   = shipto.ship-state
                                oe-rel.ship-zip     = shipto.ship-zip
                                oe-rel.ship-i[1] = shipto.notes[1]
                                oe-rel.ship-i[2] = shipto.notes[2]
                                oe-rel.ship-i[3] = shipto.notes[3]
                               oe-rel.ship-i[4] = shipto.notes[4].
            find first sys-ctrl where sys-ctrl.company eq prmComp
                                  and sys-ctrl.name    eq "OECARIER"
                             no-lock no-error.
            if not avail sys-ctrl then do:
                               create sys-ctrl.
                               assign
                                 sys-ctrl.company  = prmComp
                                 sys-ctrl.name     = "OECARIER"
                                 sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                                 sys-ctrl.char-fld = "ShipTo".
       
                               do while true:
                                 
                                   update sys-ctrl.char-fld.
                                 if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "ShipTo" then leave. 
                               end.
            end.
            oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto.carrier
                               else xoe-ord.carrier.
         end.
         /* Run Freight calculation  */
         run oe/oe-frtcl.p.

      end.  /* multi ship to */
      else do:
           find first shipto where shipto.company eq prmComp and
                                        shipto.cust-no eq xoe-ord.cust-no and
                                        shipto.ship-id eq v-ship-id
                                  no-lock no-error.
            if not avail shipto then
                 find first shipto where shipto.company eq prmComp and
                                          shipto.cust-no eq xoe-ord.cust-no
                                    no-lock no-error.
            if AVAIL shipto then do:
               assign oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                         oe-rel.ship-addr[1] = shipto.ship-addr[1]
                         oe-rel.ship-addr[2] = shipto.ship-addr[2]
                         oe-rel.ship-city    = shipto.ship-city
                         oe-rel.ship-state   = shipto.ship-state
                         oe-rel.ship-zip     = shipto.ship-zip
                         oe-rel.ship-i[1] = shipto.notes[1]
                         oe-rel.ship-i[2] = shipto.notes[2]
                         oe-rel.ship-i[3] = shipto.notes[3]
                         oe-rel.ship-i[4] = shipto.notes[4].
               
               if ll-new-record /* and NOT oe-rel.carrier ENTERED */ then do:
                  find first sys-ctrl where sys-ctrl.company eq prmComp
                                        and sys-ctrl.name    eq "OECARIER"
                       no-lock no-error.
                  if not avail sys-ctrl then do:
                     create sys-ctrl.
                     assign sys-ctrl.company  = prmComp
                            sys-ctrl.name     = "OECARIER"
                            sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                            sys-ctrl.char-fld = "ShipTo".
       
                     do while true:
                        /*message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.*/
                        if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "Sh~ipTo" then leave. 
                     end.
                  end.
                  oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                     else xoe-ord.carrier.
               end.
            end. /* avail shipto */
      end. /* not multi */
  end. /* if no oe-rel */
  else do:
       find first shipto where shipto.company = prmComp and
                               shipto.cust-no = xoe-ord.cust-no  and
                               shipto.ship-id = xoe-rel.ship-id
                               use-index ship-id no-lock no-error.
       if AVAIL shipto then do:
           assign oe-rel.ship-no      = shipto.ship-no
                      oe-rel.ship-id      = shipto.ship-id
                       oe-rel.ship-addr[1] = shipto.ship-addr[1]
                       oe-rel.ship-addr[2] = shipto.ship-addr[2]
                       oe-rel.ship-city    = shipto.ship-city
                       oe-rel.ship-state   = shipto.ship-state
                       oe-rel.ship-zip     = shipto.ship-zip
                       oe-rel.ship-i[1] = shipto.notes[1]
                       oe-rel.ship-i[2] = shipto.notes[2]
                       oe-rel.ship-i[3] = shipto.notes[3]
                       oe-rel.ship-i[4] = shipto.notes[4].
         
           if ll-new-record then do:                
                 find first sys-ctrl where sys-ctrl.company eq prmComp
                                       and sys-ctrl.name    eq "OECARIER"
                 no-lock no-error.
                 if not avail sys-ctrl then do:
                    create sys-ctrl.
                    assign sys-ctrl.company  = prmComp
                           sys-ctrl.name     = "OECARIER"
                           sys-ctrl.descrip  = "Default carrier from Header or ShipTo~:"
                           sys-ctrl.char-fld = "ShipTo".
       
                    do while true:
                        /*message "Default Shipping Carrier from Header or Shipto?" 
                        update sys-ctrl.char-fld.*/
                        if sys-ctrl.char-fld = "Header" or sys-ctrl.char-fld = "Sh~ipTo" then leave. 
                    end.
                 end.
                 oe-rel.carrier   = if sys-ctrl.char-fld = "Shipto" then shipto~.carrier
                                    else xoe-ord.carrier.
           end.           
       end.           
  end.

END PROCEDURE.
/***************************************************************************************************/
PROCEDURE final-steps :
  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF VAR v-pallet-cnt AS DEC NO-UNDO.
      
  def var lv-job-recid as recid no-undo.


  fil_id = recid(oe-ordl).

  IF oe-ordl.est-no ne "" and not avail xest then
  find first xest
      where xest.company eq prmComp
        and xest.est-no  eq oe-ordl.est-no
      no-lock no-error.
  if avail xest THEN DO:
     IF lv-new-tandem NE ? THEN RUN upd-new-tandem.
     ELSE
     IF ll-is-tandem THEN RUN upd-tandem.

     IF xest.est-type eq 2 or xest.est-type eq 6 then do:
       s-est-no = oe-ordl.est-no.
       run oe/fgadd2.p.   /** 2pc box fg create/update routine **/
     END.
     end.

  if avail oe-ordl and (oe-ordl.est-no ne "" and oe-ordl.job-no eq "") THEN do:
  /*message "Since job number is blank, a job will not be created "
            view-as alert-box*/. 
  END.
   ELSE DO:
    IF AVAIL oe-ordl AND oe-ordl.est-no NE "" AND v-create-job THEN do:
      find first job where job.company eq prmComp
                       and job.job-no  eq oe-ordl.job-no
                       and job.job-no2 eq oe-ordl.job-no2 no-lock no-error.
      if not avail job then do:
        run create-job (output lv-job-recid).
        find job where recid(job) = lv-job-recid no-lock.
      END.
    end.

    run oe/ordlup.p.         /* Update Inventory and Job Costing */
  end.
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
/**********************************************************************************/
PROCEDURE final-steps2 :
   IF oe-ordl.est-no NE "" THEN DO TRANSACTION:
    fil_id = RECID(oe-ordl).

    IF NOT v-qty-mod THEN RUN oe/job-qty.p (ROWID(oe-ordl), OUTPUT v-qty-mod).

    IF oe-ord.est-no EQ ""                                       OR
       (v-qty-mod AND (NOT ll-new-record OR lv-new-tandem NE ?)) THEN DO:
      RUN oe/estupl.p.
      fil_id = RECID(oe-ordl).
    END.

    IF lv-q-no NE 0 THEN DO:
      RUN oe/ordlq-no.p (ROWID(oe-ordl), lv-q-no).
      FIND CURRENT oe-ordl.
    END.
  END.
  DO TRANSACTION:
    IF oe-ord.type NE "T"  AND
       ((v-qty-mod OR oe-ordl.po-no-po EQ 0 OR lv-new-tandem NE ? OR
          NOT CAN-FIND(FIRST po-ord
                       WHERE po-ord.company EQ oe-ordl.company
                         AND po-ord.po-no   EQ oe-ordl.po-no-po)))       THEN
      /*RUN po/do-po.p.*/

    FIND CURRENT oe-ordl.

    
    RUN oe/oe-frtcl.p.  

    oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000.

    FIND CURRENT oe-ordl NO-LOCK.
  END.

END PROCEDURE.
/******************************************************************************************/

PROCEDURE update-start-date :
  DEF VAR lv-prom-date AS DATE NO-UNDO.
  DEF VAR lv-day-time AS INT NO-UNDO.
  DEF VAR lv-job2-time AS INT NO-UNDO.
  DEF VAR lv-prev-end-time AS INT NO-UNDO.
  DEF VAR lv-lap-time AS INT NO-UNDO.

  IF oe-ordl.job-no = ""  THEN RETURN.

   DEF BUFFER bx-ordl FOR oe-ordl.
   DEF VAR lv-first-due-date AS DATE NO-UNDO.
   lv-first-due-date = oe-ordl.req-date.

   FOR EACH bx-ordl WHERE bx-ordl.company = oe-ordl.company
                      AND bx-ordl.job-no = oe-ordl.job-no
                      AND bx-ordl.job-no2 = oe-ordl.job-no2 
                      AND RECID(bx-ordl) <> RECID(oe-ordl) NO-LOCK:
       lv-first-due-date = IF bx-ordl.req-date < lv-first-due-date THEN bx-ordl.req-date
                           ELSE lv-first-due-date.
   END.

  DEF BUFFER bf-hdr FOR job-hdr.
  DEF BUFFER bf-mch FOR job-mch.
  DEF BUFFER bf-job FOR job.
  DEF VAR lv-start-date AS DATE NO-UNDO.
  DEF VAR lv-m-time AS INT no-undo.
  DEF VAR lv-run-time AS INT NO-UNDO.
  DEF VAR lv-mr-time AS INT NO-UNDO.
  DEF VAR lv-job-time  AS INT NO-UNDO.
  DEF VAR lv-maccum-time AS INT NO-UNDO.
  DEF VAR lv-job-hr AS INT NO-UNDO.
  DEF VAR lv-job-day AS INT NO-UNDO.
  DEF VAR lv-wrk-st-time AS INT NO-UNDO.
  DEF VAR lv-chk-date AS DATE NO-UNDO.
  DEF VAR li-num-of-wkend AS INT NO-UNDO.
  DEF VAR lv-start-date-fr AS DATE NO-UNDO.
  DEF VAR lv-start-time AS INT NO-UNDO.

  /*===  calculate start date from due-date === */
  ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0.

  FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no 
                    AND bf-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK:
      FOR EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                        AND bf-mch.job-no = bf-hdr.job-no
                        AND bf-mch.job-no2 = bf-hdr.job-no2 NO-LOCK:
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.mr-hr,0) * 3600 +
                        ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                          truncate(bf-mch.run-hr,0) * 3600 +
                        ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

          lv-job-time = lv-job-time + lv-mr-time +  lv-run-time.
      END.
  END.
  
  

  lv-job-hr = IF lv-job-time MOD 3600 > 0 THEN truncate(lv-job-time / 3600,0) + 1
               ELSE truncate(lv-job-time / 3600,0).
  lv-job-day = IF (lv-job-hr MOD 8) > 0 THEN truncate(lv-job-hr / 8,0) + 1
               ELSE TRUNCATE(lv-job-hr / 8,0).

  lv-start-date = lv-first-due-date - lv-job-day . /*- 1. */
  lv-update-job-stdate = NO.
  FIND bx-ordl WHERE RECID(bx-ordl) = RECID(oe-ordl).
  lv-prom-date = TODAY + lv-job-day.
  IF lv-start-date < TODAY /*AND (ip-type = "add")*/  /* ip-type = "Update-2" is from v-ord.w*/
     AND v-run-schedule AND schedule-log
  THEN DO:
    
      /*MESSAGE "Calculated Promised DATE is   " lv-prom-date SKIP
             "Due Date is before Calculates Promised Date. Update Due Date?" UPDATE lv-update-job-stdate
             VIEW-AS ALERT-BOX WARNING BUTTON YES-NO.*/
     lv-start-date = TODAY.
  END.
  ELSE IF lv-start-date < TODAY THEN lv-start-date = TODAY.

  IF v-run-schedule THEN DO:

    /* === reset start-date === */
    ASSIGN lv-mr-time = 0
         lv-run-time = 0
         lv-job-time = 0
         lv-maccum-time = 0
         li-num-of-wkend = 0
         lv-day-time = 0
         lv-start-time = 0.
  
    FOR EACH bf-hdr WHERE bf-hdr.company = oe-ord.company
                    AND bf-hdr.job-no = oe-ordl.job-no
                    AND bf-hdr.job-no2 = oe-ordl.job-no2,
      EACH bf-mch WHERE bf-mch.company = bf-hdr.company
                    AND bf-mch.job-no = bf-hdr.job-no
                    AND bf-mch.job-no2 = bf-hdr.job-no2
                    AND NOT bf-mch.anchored
               BREAK BY bf-mch.frm BY bf-mch.blank-no by bf-mch.pass BY bf-mch.m-code:

          FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                            AND mach-calendar.m-code = bf-mch.m-code
                            AND mach-calendar.m-date = lv-start-date
                            NO-LOCK NO-ERROR.
          lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                      ELSE 28800. /* 8 HRs*/
          IF lv-m-time LT 0 THEN lv-m-time = 28800.
          lv-maccum-time = lv-maccum-time + lv-m-time.
          IF FIRST(bf-mch.frm) THEN DO:
             FIND FIRST bf-job OF bf-hdr.
             bf-job.start-date = lv-start-date.
             lv-wrk-st-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
          END.
          IF FIRST-OF(bf-mch.frm) THEN
                bf-hdr.start-date = job.start-date.
          lv-start-time = lv-wrk-st-time.
          lv-mr-time = IF bf-mch.mr-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.mr-hr,0) * 3600 +
                    ((bf-mch.mr-hr - truncate(bf-mch.mr-hr,0)) * 100 * 60 / 100) * 60.
          lv-run-time = IF bf-mch.run-hr = 0 THEN 0 ELSE
                      truncate(bf-mch.run-hr,0) * 3600 +
                    ((bf-mch.run-hr - truncate(bf-mch.run-hr,0)) * 100 * 60 / 100) * 60.

          ASSIGN bf-mch.seq-no = 0                 
                 bf-mch.start-time-su = lv-wrk-st-time + lv-job-time + lv-day-time
                 /*bf-mch.start-time = lv-wrk-st-time + lv-mr-time*/
                 bf-mch.start-date-su = lv-start-date 
                 .

          lv-start-date-fr = lv-start-date.
          lv-job-time = lv-job-time + lv-mr-time + lv-run-time .
          lv-start-date = lv-start-date + 
                          IF lv-mr-time > lv-m-time THEN TRUNCATE(lv-mr-time / lv-m-time,0) 
                          ELSE 0.
          IF lv-mr-time > lv-m-time THEN DO:
             lv-job2-time = lv-mr-time - lv-m-time.
             lv-lap-time = bf-mch.start-time-su - lv-start-time.
             FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date 
                        NO-LOCK NO-ERROR.
             lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                         ELSE 28800. /* 8 HRs*/.
             IF lv-m-time LT 0 THEN lv-m-time = 28800.
             lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
             
             ASSIGN bf-mch.end-time-su = lv-start-time + lv-job2-time + lv-lap-time
                    bf-mch.start-time = lv-start-time + lv-job2-time  + lv-lap-time
                    bf-mch.end-date-su = lv-start-date
                    bf-mch.start-date = lv-start-date 
                    lv-day-time = lv-start-time - lv-prev-end-time + 86400
                    .      
            
          END.
        
          ELSE ASSIGN bf-mch.end-time-su = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                      bf-mch.start-time = lv-start-time + lv-job-time - lv-run-time + lv-day-time
                      bf-mch.end-date-su = lv-start-date
                      bf-mch.start-date = lv-start-date 
                      lv-lap-time = 0.


        
          lv-start-date-fr = lv-start-date.
          lv-start-date-fr = lv-start-date.
          
          lv-start-date = lv-start-date + 
                          
                          IF (lv-run-time) > lv-m-time THEN TRUNCATE((lv-run-time + lv-mr-time) / lv-m-time,0)
                          ELSE 0.
          
          lv-start-date-fr = lv-start-date.
          IF (lv-run-time) > lv-m-time THEN DO:
             lv-job2-time = lv-run-time - lv-m-time.
             lv-lap-time = bf-mch.start-time - lv-start-time.
             FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date 
                        NO-LOCK NO-ERROR.
             lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                     ELSE 28800. /* 8 HRs*/.
             IF lv-m-time LT 0 THEN lv-m-time = 28800.
             lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
             ASSIGN bf-mch.end-time = lv-start-time + lv-job2-time             
                    bf-mch.end-date = lv-start-date
                    lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400
                    .
          END.
          ELSE ASSIGN bf-mch.end-time = bf-mch.start-time + lv-run-time             
                      bf-mch.end-date = lv-start-date
                      lv-lap-time = 0.

          lv-prev-end-time = IF AVAIL mach-calendar THEN mach-calendar.end-time ELSE 86400. /* 24 HRs*/

          IF string(bf-mch.end-time,"hh:mm:ss") > string(lv-prev-end-time,"hh:mm:ss") THEN DO:
             lv-start-date = lv-start-date + 1.
             lv-lap-time = bf-mch.end-time - lv-prev-end-time.
             FIND FIRST mach-calendar WHERE mach-calendar.company = job.company
                        AND mach-calendar.m-code = bf-mch.m-code
                        AND mach-calendar.m-date = lv-start-date 
                        NO-LOCK NO-ERROR.
             lv-m-time = IF AVAIL mach-calendar THEN mach-calendar.end-time - mach-calendar.start-time
                     ELSE 28800. /* 8 HRs*/.
             IF lv-m-time LT 0 THEN lv-m-time = 28800.
             lv-start-time = IF AVAIL mach-calendar THEN mach-calendar.start-time ELSE 0.
             ASSIGN bf-mch.end-time = lv-start-time + lv-lap-time             
                    bf-mch.end-date = lv-start-date
                    lv-day-time = lv-day-time + lv-start-time - lv-prev-end-time + 86400
                    .
          END.
        
    END.
  END. /* if v-run-schedule*/

  IF schedule-log THEN DO:
     bx-ordl.prom-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.prom-date.
     bx-ordl.req-date = IF lv-update-job-stdate THEN lv-prom-date ELSE bx-ordl.req-date.
  END.
  
END PROCEDURE.
/*****************************************************************************/
PROCEDURE update-due-date :
  DEF BUFFER bJobHdr FOR job-hdr.
  DEF BUFFER bJob FOR job.
      
  DEF VAR li AS INT NO-UNDO.


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
    IF AVAIL bJobHdr THEN DO:
      bJobHdr.due-date = oe-ordl.req-date.
      FIND CURRENT bJobHdr NO-LOCK.
      FIND FIRST bJob OF bJobHdr EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL bJob THEN RETURN.
      bJob.due-date = bJobHdr.due-date.
      FIND CURRENT bJob NO-LOCK.
      li = 1000.
    END.
  END.

END PROCEDURE.
/*******************************************************************************************************/
/************************************************************************************************************************/
PROCEDURE MainBlock:
    DEF VAR ll-price-mod      AS   LOG  NO-UNDO.
  DEF VAR lv-price          AS   DECIMAL NO-UNDO.
  DEF VAR ll-pruom-mod      AS   LOG  NO-UNDO.
  DEF VAR lv-pruom          AS   CHAR NO-UNDO.
  DEF VAR lv-prev-req-date  AS   DATE NO-UNDO.
  DEF VAR lv-stat           AS   CHAR NO-UNDO.
  DEF VAR ll                AS   LOG  NO-UNDO.
  DEF VAR ld                AS   DEC  NO-UNDO.
  DEF VAR ll-reopen         AS   LOG  NO-UNDO.
  

  DEF BUFFER b-oe-ordl FOR oe-ordl.
  DEF BUFFER b-oe-ord FOR oe-ord.


  DISABLE TRIGGERS FOR LOAD OF xoe-ord.
  find oe-ord of oe-ordl no-lock.
  find xoe-ord of oe-ordl no-lock.
  
  

  ASSIGN
   ld-prev-t-price = oe-ordl.t-price
   li-prev-ord-qty = oe-ordl.qty.
     FIND FIRST est
          WHERE est.company EQ oe-ordl.company
            AND est.est-no  EQ FILL(" ",8 - LENGTH(TRIM(oe-ord.est-no))) +
                               TRIM(oe-ord.est-no)
          NO-LOCK NO-ERROR.
     
          RUN new-tandem.

      IF AVAIL est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN DO:
        RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-is-tandem).
      END.
    
    ASSIGN
     v-qty-mod       = YES
     li-prev-qty     = oe-ordl.qty
     li-prev-ord-qty = oe-ordl.qty
     ll-price-mod    = YES
     lv-price        = prmPrice
     ll-pruom-mod    = YES
     lv-pruom        = prmUom
     lv-prev-req-date = oe-ordl.req-date
  .

  
    IF ll-price-mod THEN prmPrice = lv-price.
    IF ll-pruom-mod THEN prmUom = lv-pruom.

    
  
  
  FIND CURRENT oe-ordl EXCLUSIVE.
  
  /*IF NOT ll-new-rrd THEN RUN oe/upinvqty.p (RECID(oe-ordl)).*/
  /*RUN whs-item (1).*/
 /* find xoe-ord where recid(xoe-ord) = recid(oe-ord) EXCLUSIVE.
  find first itemfg where itemfg.company eq cocode
                      and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
  if avail itemfg then do:
       assign 
        xoe-ord.t-weight = xoe-ord.t-weight - oe-ordl.t-weight
        oe-ordl.t-weight = ( oe-ordl.qty / 100 ) * itemfg.weight-100
        xoe-ord.t-weight = xoe-ord.t-weight + oe-ordl.t-weight.
  END.
    IF TRIM(oe-ordl.est-no) NE "" AND
       TRIM(xoe-ord.est-no) EQ "" AND
       ll-new-record              THEN
      RUN fg/makenote.p (BUFFER oe-ordl, ?, itemfg.rec_key).end.
  FIND CURRENT xoe-ord NO-LOCK.

  MESSAGE "order" oe-ord.ord-no oe-ord.ord-no . 
    RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT tfright ).
                ASSIGN
                    oe-ordl.t-freight = tfright
                    oe-ord.t-freight = oe-ord.t-freight + tfright.
                RUN oe/ordfrate.p (ROWID(oe-ord)). */

   
  
  END PROCEDURE.

  /****************************************************************************************************/

  PROCEDURE new-tandem :
  DEF BUFFER b-eb FOR eb.
  DEF VAR v-rowid AS ROWID NO-UNDO. 
  RUN ce/new-form.p (ROWID(est), OUTPUT lv-new-tandem).
  FIND eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.
  IF AVAIL eb THEN DO :
    ASSIGN
     prmEstimate  = oe-ord.est-no
     prmJob  = oe-ord.job-no
     prmJob2 = oe-ord.job-no2.

   /* RUN est/d-selest.w (lv-new-tandem, YES, oe-ord.cust-no,
                        OUTPUT v-qty-mod, OUTPUT v-rowid).*/
    IF v-qty-mod THEN DO:
      FIND FIRST b-eb
          WHERE b-eb.company EQ eb.company
            AND b-eb.est-no  EQ eb.est-no
            AND b-eb.eqty    EQ eb.eqty
          NO-LOCK NO-ERROR.
      IF AVAIL eb THEN
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

      /*RUN display-est-detail (RECID(eb)).*/  /**test*/

      DISABLE oe-ordl.est-no.
    END.
  END.

END PROCEDURE.

/**********************************************************************/

PROCEDURE validate-all :
def var ls-i-no as cha no-undo.
 
 def var ls-est-no as cha no-undo.
 def var ls-uom as cha no-undo.
 DEF VAR ll-secure AS LOG NO-UNDO.
 def var ip-est-no as cha no-undo.
def VAR  iop-i-no as cha no-undo.
def VAR iop-part-no as cha no-undo.
def VAR iop-uom as cha no-undo.
 
 ASSIGN ls-est-no = prmEstimate.

 
 RUN get-est-cost (prmEstimate).
   
   /* ASSIGN 
        ls-i-no = prmItemNum
        ls-uom = prmUom
        ls-part-no = prmPartNum
        iop-i-no = ls-i-no
        iop-part-no = ls-part-no
        iop-uom = ls-uom .


        find FIRST xest where xest.company = prmComp
                         and xest.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) +
                                           TRIM(prmEstimate)
                         no-lock no-error.
        if avail xest then do: 
                find FIRST xeb where xeb.company = prmComp and xeb.est-no = xest.est-no
                         and xeb.form-no = 0 no-lock no-error.
                if not avail xeb then find first xeb where xeb.company = prmComp and xeb.est-no = xest.est-no
                                                    and xeb.form-no = oe-ordl.form-no
                                                    and xeb.blank-no = oe-ordl.blank-no
                                                    no-lock no-error.
                IF NOT AVAIL xeb THEN
                FIND FIRST xeb
                    WHERE xeb.company EQ prmComp
                      AND xeb.est-no  EQ xest.est-no
                      AND xeb.part-no EQ ls-part-no
                    NO-LOCK NO-ERROR.
                if avail xeb then do:
                   find xef where xef.company = g_company and xef.est-no = xeb.est-no
                            and xef.form-no = xeb.form-no
                            no-lock no-error.

                   
                   run crt-itemfg (prmItemNum, "M").    /*check test*/
                end.    
             end.   
             */
        
    /*RUN valid-s-pct (0) NO-ERROR.
    RUN valid-type NO-ERROR.*/
END PROCEDURE.
/**********************************************************/
PROCEDURE valid-uom :
  DEF VAR lv-uom AS CHAR NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  
    /*RUN get-valid-uom .*/

    IF prmUom = ""
         THEN  prmUom = "EA".
         ELSE  prmUom = "M".
    IF NOT CAN-FIND(FIRST uom
                    WHERE uom.uom EQ prmUom
                      AND CAN-DO(prmUom,uom.uom)) THEN DO:
        cError = "Uom Is invalid, try help...".
        RETURN.
    END.
      
    
    IF   NOT CAN-DO("," + TRIM(lv-ea-list),prmUom) AND
       (prmUom NE "CS" OR
        DEC(prmCas) NE 0)  THEN DO:

      ASSIGN
       ld = DEC(prmQty)
       ld = ld * (IF prmUom EQ "CS" THEN DEC(prmCas) ELSE
                  IF prmUom EQ "C"  THEN 100 ELSE 1000)

       prmQty = ld.
      
  END.

END PROCEDURE.

/***********************************************************************************************/
PROCEDURE get-est-cost :
    def input param ip-est-no as CHAR NO-UNDO.
   def var v-run-list as CHAR NO-UNDO.

   
   
   if prmEstimate ne "" and not avail xest then
        find first xest where xest.company eq prmComp and
                   xest.est-no eq ip-est-no no-lock no-error.

   IF AVAIL xest THEN DO :
       
     find first xeb
          where xeb.company = prmComp 
            and xeb.est-no = ip-est-no
            and xeb.part-no = prmPartNum
          no-lock no-error.
     IF AVAIL xeb THEN
     find first xef
          where xef.company = prmComp 
            and xef.est-no = ip-est-no
            and (xef.form-no = xeb.form-no OR xeb.form-no = 0)
          no-lock no-error.

     ASSIGN
        v-run-list = "ce/print4.p,ce/box/print42.p,ce/tan/print4.p," +
                     "ce/com/print4.p,cec/print4.p,cec/box/print42.p," +
                     "cec/tan/print4.p,cec/com/print4.p"
      
        qty = INT(prmQty).

        
   
     /*IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                          AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8                                          AND
        (oe-ordl.qty NE qty OR DEC(prmCost) EQ 0) THEN DO:
        */
     IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                          /*AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8                                          AND
        (DEC(prmCost) EQ 0)*/ THEN DO:

         
        RUN VALUE(ENTRY(xest.est-type,v-run-list)).

        prmCost = ((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                                           (INT(prmQty) / 1000)).

        
     END.
   END.

END PROCEDURE.


/*******************************************************************/
PROCEDURE upd-new-tandem :
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-est-qty FOR est-qty.
  DEF BUFFER b-Unit# FOR reftable.

  DEF VAR lv-master AS ROWID NO-UNDO.
  DEF VAR li AS INT NO-UNDO.

    
  FIND FIRST eb WHERE ROWID(eb) EQ lv-new-tandem NO-ERROR.

  IF AVAIL eb THEN DO:
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
    IF INT(eb.master-est-no) NE 0 THEN DO:
      FIND FIRST b-eb
          WHERE b-eb.company EQ eb.company
            AND b-eb.est-no  EQ eb.master-est-no
            AND b-eb.part-no EQ eb.part-no
          NO-LOCK NO-ERROR.

      IF NOT AVAIL b-eb THEN DO:
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

        DO li = 1 TO 2:
          FOR EACH b-Unit#
              WHERE b-Unit#.reftable EQ "ce/v-est3.w Unit#" + TRIM(STRING(li - 1,">"))
                AND b-Unit#.company  EQ eb.company
                AND b-Unit#.loc      EQ eb.est-no
                AND b-Unit#.code     EQ STRING(eb.form-no,"9999999999")
                AND b-Unit#.code2    EQ STRING(eb.blank-no,"9999999999"):
            DELETE b-Unit#.
          END.
        END.

        FIND FIRST xest
            WHERE xest.company EQ eb.company
              AND xest.est-no  EQ eb.master-est-no
            NO-LOCK NO-ERROR.

        IF AVAIL xest THEN DO:
          FIND FIRST ef OF eb NO-LOCK NO-ERROR.
          RUN ce/new-form.p (ROWID(xest), OUTPUT lv-master).

          FIND b-eb WHERE ROWID(b-eb) EQ lv-master NO-ERROR.

          IF AVAIL b-eb THEN DO:
            FIND FIRST b-est-qty 
                WHERE b-est-qty.company EQ b-eb.company
                  AND b-est-qty.est-no  EQ b-eb.est-no
                NO-LOCK NO-ERROR.

            BUFFER-COPY eb EXCEPT e-num form-no blank-no est-no rec_key TO b-eb
            ASSIGN
             b-eb.master-est-no = ""
             b-eb.eqty          = b-est-qty.eqty.

            FIND FIRST b-ef OF b-eb NO-ERROR.
            IF AVAIL b-ef AND AVAIL ef THEN
            BUFFER-COPY ef EXCEPT e-num form-no est-no rec_key TO b-ef
            ASSIGN
             b-ef.eqty = b-est-qty.eqty.

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
/*********************************************************************************************/
PROCEDURE upd-tandem :
  IF AVAIL oe-ordl THEN
  FOR EACH eb
      WHERE eb.company  EQ oe-ordl.company
        AND eb.est-no   EQ oe-ordl.est-no
        AND eb.part-no  EQ oe-ordl.part-no
        AND eb.stock-no EQ oe-ordl.i-no
      BREAK BY eb.form-no  DESC
            BY eb.blank-no DESC:

    IF LAST(eb.form-no)                     OR
       (eb.form-no EQ oe-ordl.form-no AND
        eb.blank-no EQ oe-ordl.blank-no)    THEN DO:
      ASSIGN
       eb.part-dscr1    = oe-ordl.i-name
       eb.part-dscr2    = oe-ordl.part-dscr1
       eb.bl-qty        = oe-ordl.qty
       eb.yld-qty       = oe-ordl.qty.
      .
    END.
  END.

END PROCEDURE.

/********************************************************************************************************/
PROCEDURE create-job :
  def output param op-recid as recid no-undo.
  def var v-job-job like job.job no-undo.
  def var v-job-no like job.job-no no-undo.
  def var v-job-no2 like job.job-no2 no-undo.
  def var li-j-no as int no-undo.
  DEFINE VARIABLE v-prod-cat AS CHARACTER  NO-UNDO.
  find last job where job.company eq cocode use-index job no-lock no-error.
  v-job-job = if avail job then job.job + 1 else 1.

  if oe-ord.job-no <> "" then 
     assign v-job-no = oe-ord.job-no
            v-job-no2 =  oe-ord.job-no2.
  else
  if oe-ordl.job-no eq "" then do:
    FIND FIRST est
      WHERE est.company EQ cocode
        and est.est-no  EQ oe-ordl.est-no NO-LOCK NO-ERROR.
    IF AVAIL est THEN  
       FIND FIRST eb
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.cust-no  EQ oe-ord.cust-no NO-LOCK NO-ERROR.
    IF AVAIL eb THEN 
        v-prod-cat = eb.procat.
     v-job-no = fill(" ",6 - length(trim(string(oe-ordl.ord-no)))) + string(oe-ordl.ord-no).
     RUN jc/job-no.p (INPUT-OUTPUT v-job-no, INPUT-OUTPUT v-job-no2,INPUT v-prod-cat).
     IF v-job-no NE "" THEN DO:
       assign
        oe-ordl.job-no  = v-job-no
        oe-ordl.job-no2 = v-job-no2.
    end.
  END.
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
         job.est-no     = oe-ordl.est-no
         job.job-no     = oe-ordl.job-no
         job.job-no2    = oe-ordl.job-no2
         job.stat       = "P"
         op-recid = recid(job).

   find first job-hdr where job-hdr.company eq cocode
                       and job-hdr.job-no  eq oe-ordl.job-no
                       and job-hdr.job-no2 eq oe-ordl.job-no2
                       and job-hdr.ord-no  eq oe-ordl.ord-no
                       and job-hdr.i-no    eq oe-ordl.i-no no-error.

   if not avail job-hdr then do:
         find first itemfg where itemfg.company eq oe-ordl.company
                             and itemfg.i-no    eq oe-ordl.i-no
                             no-lock no-error.

         create job-hdr.
         assign job-hdr.company      = cocode
                job-hdr.loc          = locode
                job-hdr.e-num        = oe-ordl.e-num
                job-hdr.est-no       = oe-ordl.est-no
                job-hdr.i-no         = oe-ordl.i-no
            /*     job-hdr.qty          = oe-ordl.qty */
                job-hdr.cust-no      = oe-ordl.cust-no
                job-hdr.ord-no       = oe-ordl.ord-no
                job-hdr.po-no        = oe-ordl.po-no.

         if avail itemfg then
              assign job-hdr.std-mat-cost = itemfg.std-mat-cost
                     job-hdr.std-lab-cost = itemfg.std-lab-cost
                     job-hdr.std-var-cost = itemfg.std-var-cost
                     job-hdr.std-fix-cost = itemfg.std-fix-cost.

         assign job-hdr.std-tot-cost = (job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                                        job-hdr.std-var-cost + job-hdr.std-fix-cost).
   end.
   assign job-hdr.est-no  = oe-ordl.est-no
          job-hdr.job     = job.job
          job-hdr.job-no  = job.job-no
          job-hdr.job-no2 = job.job-no2
          oe-ordl.j-no = job-hdr.j-no.
 
END PROCEDURE.

/********************************************************************************************************/

PROCEDURE valid-part-no :


/*
    IF ERROR-STATUS:ERROR THEN DO:
      APPLY "entry" TO oe-ordl.part-no.
      RETURN ERROR.
    END.           */
END PROCEDURE.
/****************************************************************************************************************/
PROCEDURE valid-i-no :
  
END PROCEDURE.


/******************************************************************************************************/
PROCEDURE valid-s-pct :
DEF INPUT PARAM ip-int AS INT NO-UNDO.
DEF VAR ld-pct AS DEC NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
    ld-pct = IF ip-int EQ 1 THEN DEC(prmSpct)
             ELSE
             IF ip-int EQ 2 THEN DEC(prmSpct2)
             ELSE
             IF ip-int EQ 3 THEN DEC(prmSpct3)
             ELSE (DEC(prmSpct) +
                   DEC(prmSpct2) +
                   DEC(prmSpct3)).

    IF (prmSman NE "" OR
        prmSman2 NE "" OR
        prmSman3 NE "")   AND
       ((ip-int EQ 0 AND ld-pct NE 100) OR
        (ip-int NE 0 AND ld-pct GT 100)) THEN DO:
      IF ip-int EQ 0 THEN
          ASSIGN cError = "Item's Salesman Commission % of Sales does not equal 100%, continue?".
      RETURN.
       /*MESSAGE "Item's Salesman Commission % of Sales does not equal 100%, continue?" SKIP(1)
                "(Please Note: Yes will result in inaccurate totals on some Sales History Reports)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE ll.*/
      END.
  END PROCEDURE.
/**********************************************************************/

PROCEDURE valid-type :
    

END PROCEDURE.
/****************************************************************************//*
PROCEDURE order-from-est :
{oe/ordfrest.i}
END PROCEDURE.
*/
/*****************************************************************************************************/

PROCEDURE local-assign-record :

  DEF VAR lv-date LIKE oe-ord.due-date NO-UNDO.
  DEF VAR lv-stat AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE oe-ord.ord-no NO-UNDO.
  DEF VAR calcDueDate AS DATE NO-UNDO.
  DEF VAR calcStartDate AS DATE NO-UNDO.
  DEF VAR li-tries AS INT NO-UNDO.

  DEF BUFFER b-oe-rel FOR oe-rel.
  DEF BUFFER due-job-hdr FOR job-hdr.
  ASSIGN
   lv-date   = prmReqDate
   lv-ord-no = prmOrderNum.
  IF oe-ord.ord-no NE lv-ord-no THEN DO:
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ prmComp EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL oe-ctrl                  AND
       lv-ord-no + 1 EQ oe-ctrl.n-ord THEN oe-ctrl.n-ord = oe-ctrl.n-ord - 1.
    FIND CURRENT oe-ctrl NO-LOCK NO-ERROR.

    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ prmComp
          AND oe-ordl.ord-no  EQ lv-ord-no:
      oe-ordl.ord-no = oe-ord.ord-no.
    END.
  END.

  IF oe-ord.job-no NE '' THEN DO:
    FIND FIRST job EXCLUSIVE-LOCK WHERE job.company EQ oe-ord.company
                                    AND job.job-no EQ oe-ord.job-no
                                    AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.
    IF AVAILABLE job THEN DO:
      IF dueDateChanged THEN DO:
        job.due-date = oe-ord.due-date.
        FOR EACH due-job-hdr EXCLUSIVE-LOCK
            WHERE due-job-hdr.company EQ job.company
              AND due-job-hdr.job     EQ job.job
              AND due-job-hdr.job-no  EQ job.job-no
              AND due-job-hdr.job-no2 EQ job.job-no2:
          due-job-hdr.due-date = oe-ord.due-date.
        END. /* each due-job-hdr */
      END. /* if duedatechanged */
      FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ prmComp
                                    AND sys-ctrl.name EQ 'SCHEDULE' NO-ERROR.
     /* IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN DO:
        IF prodDateChanged THEN
        job.start-date = IF sys-ctrl.char-fld NE 'NoDate' THEN oe-ord.prod-date
                         ELSE ?.
        IF dueDateChanged AND sys-ctrl.char-fld EQ 'PlanDate' THEN DO:
          IF NOT VALID-HANDLE(scheduleHndl) THEN
          RUN custom/schedule.p PERSISTENT SET scheduleHndl.
          RUN scheduleJob IN scheduleHndl (ROWID(job),OUTPUT calcStartDate,OUTPUT calcDueDate).
          IF calcDueDate NE oe-ord.due-date THEN
          MESSAGE 'Machine Capacity calulated Scheduled Completion date of'
            calcDueDate SKIP 'Update Due Date and Promise Date on Order?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE updateDueDate AS LOGICAL.
          IF updateDueDate THEN
          ASSIGN
            job.due-date = calcDueDate
            oe-ord.due-date = calcDueDate.
        END. /* if duedatechanged */
        job.start-date = calcStartDate.
        FIND CURRENT job NO-LOCK.
      END. /* avail sys-ctrl */*/
    END. /* avail job */
  END. /* if job-no ne '' */
  ASSIGN
    prodDateChanged = NO
    dueDateChanged = NO
    oe-ord.type = prmType.

  /*RUN whs-order (1).*/

  FOR EACH oe-ordl OF oe-ord:
    IF ll-new-po THEN DO:
      oe-ordl.po-no = oe-ord.po-no.
      FOR EACH oe-rel NO-LOCK
          WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line:
/*ASSIGN
    lv-stat = oe-rel.stat.
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).*/

        li-tries = 0.
        IF INDEX("SLIW",lv-stat) GT 0 THEN DO WHILE TRUE:
          li-tries = li-tries + 1.
          IF li-tries GE 1000 THEN LEAVE.

          FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel)
              EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL b-oe-rel THEN DO:
            b-oe-rel.po-no = oe-ordl.po-no.
            LEAVE.
          END.
        END.
      END.
    END.

    IF oe-ordl.est-no NE "" THEN DO:
      FIND eb
          WHERE eb.company  EQ oe-ordl.company
            AND eb.est-no   EQ oe-ordl.est-no
            AND ((eb.est-type NE 2 AND eb.est-type NE 6) OR
                 ((eb.est-type EQ 2 OR eb.est-type EQ 6) AND
                  eb.form-no EQ 0))
          EXCLUSIVE NO-ERROR.
      IF AVAIL eb THEN eb.stock-no = oe-ordl.i-no.
    END.
  END.

  IF oe-ord.due-date NE lv-date THEN
  FOR EACH oe-ordl OF oe-ord BREAK BY oe-ordl.line:
    IF NOT ll-new-due THEN
      ll-new-due = FIRST(oe-ordl.line) AND LAST(oe-ordl.line).

    /*IF NOT ll-new-due THEN
      MESSAGE "Update all line items with this Due Date?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll-new-due.*/

    IF ll-new-due THEN DO:
      oe-ordl.req-date = oe-ord.due-date.

      IF oe-ordl.req-date GT oe-ordl.prom-date THEN
        oe-ordl.prom-date = oe-ordl.req-date.
    END.
    ELSE LEAVE.
  END. /* each oe-ordl */
  FIND CURRENT oe-ordl NO-LOCK NO-ERROR.
  
  RELEASE eb.
  RELEASE oe-rel.

  RELEASE oe-ordl.

  IF oe-ord.frt-pay = "B" THEN oe-ord.f-bill = YES.
  ELSE oe-ord.f-bill = NO.

END PROCEDURE.


/*****************************************************************************************/
PROCEDURE check-use-1 :    
    IF prmEstimate NE "" THEN DO:
      FIND FIRST est
           WHERE est.company EQ prmComp
             AND est.est-no  EQ prmEstimate
           NO-LOCK NO-ERROR.

      {est/checkuse.i}
    END.
END PROCEDURE.
/****************************************************************************************/ 
PROCEDURE check-use-2 :
    &Scoped-define SECOND-EXTERNAL-TABLE itemfg
    &Scoped-define THIRD-EXTERNAL-TABLE job
    &Scoped-define FOURTH-EXTERNAL-TABLE job-hdr
    &Scoped-define FIFTH-EXTERNAL-TABLE 
    
    FOR EACH oe-ordl
        WHERE oe-ordl.company EQ prmComp
          AND oe-ordl.ord-no  EQ prmOrderNum
          NO-LOCK:

      FIND FIRST itemfg
          WHERE itemfg.company EQ prmComp
            AND itemfg.i-no    EQ prmItemNum
            NO-LOCK NO-ERROR.

      IF TRIM(oe-ordl.job-no) NE "" THEN
      FIND FIRST job
          WHERE job.company EQ prmComp
            AND job.job-no  EQ prmJob
            AND job.job-no2 EQ prmJob2
          NO-LOCK NO-ERROR.

      IF AVAIL job THEN
      FIND FIRST job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.i-no    EQ prmItemNum
          NO-LOCK NO-ERROR.
      DO WHILE TRUE:
/*        {custom/checkuse.i "no cancel"}*/

        RELEASE itemfg.

        IF AVAIL job-hdr THEN
        FIND NEXT job-hdr
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
              AND job-hdr.i-no    EQ oe-ordl.i-no
            NO-LOCK NO-ERROR.
        IF NOT AVAIL job-hdr THEN LEAVE.
      END.
    END.

    &Scoped-define SECOND-EXTERNAL-TABLE
    &Scoped-define THIRD-EXTERNAL-TABLE
    &Scoped-define FOURTH-EXTERNAL-TABLE
    &Scoped-define FIFTH-EXTERNAL-TABLE
END PROCEDURE.
/*****************************customer****************************************/

PROCEDURE est-from-tandem :
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ll-new-tandem AS LOG NO-UNDO.

  DEF BUFFER est FOR est.
  DEF BUFFER eb FOR eb.

    /*RUN est/d-selest.w (?, NO,oe-ordl.cust-no,
                        OUTPUT ll-new-tandem, OUTPUT lv-rowid).*/

    FIND eb WHERE ROWID(eb) EQ lv-rowid NO-LOCK NO-ERROR.

    IF ll-new-tandem THEN DO:
      prmEstimate = eb.est-no.
      FIND FIRST xest OF eb NO-LOCK NO-ERROR.

      FOR EACH eb OF xest EXCLUSIVE:
        eb.cust-no = oe-ordl.cust-no.

        FOR EACH shipto
            WHERE shipto.company EQ eb.company
              AND shipto.cust-no EQ eb.cust-no
            NO-LOCK
            BREAK BY shipto.ship-no DESC:
          IF shipto.ship-id EQ eb.ship-id THEN LEAVE.
          IF LAST(shipto.ship-no) THEN eb.ship-id = shipto.ship-id.
        END.
      END.

      RELEASE xeb.       
      RUN est/oeselest.p.
      RUN get-from-est.  
    END.
    ELSE DO:
      FIND FIRST est OF eb EXCLUSIVE NO-ERROR.
      IF AVAIL est THEN DELETE est.
    END.
  
/*  RUN release-shared-buffers.*/

END PROCEDURE.

/***************************************************************************************/
/****************************************************************************************************/
PROCEDURE delete-item :
  DEF INPUT PARAM ip-ask-delete AS LOG NO-UNDO.
  DEF INPUT PARAM ip-comps-deleted AS LOG NO-UNDO.

  ASSIGN deleteComps = ip-comps-deleted
      askdel = ip-ask-delete.
   FIND FIRST oe-ordl WHERE oe-ordl.company EQ prmComp AND
            oe-ordl.ord-no = prmOrderNum AND oe-ordl.LINE = prmLine EXCLUSIVE-LOCK NO-ERROR.
       if avail oe-ordl and oe-ordl.rel-stat then do:
           cError =  "Previous Quantities have been released for this item..." .
           return.
  end.
                 
  IF AVAIL oe-ordl AND oe-ordl.po-no-po <> 0 THEN DO:
       cError = "Cannot delete, purchase order for board exists...".
       RETURN.
  END.

  {sys/inc/oedelete.i}

  {sys/inc/oecomb.i}

  /*RUN clear-tt.*/
       FOR EACH tt-oe-ordl:
    DELETE tt-oe-ordl.
  END.

  ASSIGN
   lv-est-no   = FILL(" ",8 - LENGTH(TRIM(oe-ordl.est-no))) +
                 TRIM(oe-ordl.est-no)
   ll-valid-eb = CAN-FIND(FIRST eb
                           WHERE eb.company  EQ oe-ordl.company
                             AND eb.est-no   EQ lv-est-no
                             AND eb.stock-no EQ oe-ordl.i-no)
   ll-more-ord = CAN-FIND(FIRST bf-ordl
                          WHERE bf-ordl.company EQ oe-ordl.company
                            AND bf-ordl.ord-no  EQ oe-ordl.ord-no
                            AND ROWID(bf-ordl)  NE ROWID(oe-ordl)).

  IF askdel THEN
    IF AVAIL oe-ordl                    AND
       ((oe-ordl.est-type NE 3 AND
         oe-ordl.est-type NE 4 AND
         oe-ordl.est-type NE 8)     OR
        NOT ll-valid-eb OR oedelete)    THEN DO:  
      ll = YES.

      /* security for combo estimage task#01180506*/
      FIND FIRST est
          WHERE est.company EQ oe-ord.company
            AND est.est-no  EQ lv-est-no
          NO-LOCK NO-ERROR.
      IF AVAIL est AND (est.est-type EQ 4 OR est.est-type EQ 8) THEN DO:
        RUN ce/com/istandem.p (ROWID(est), OUTPUT ll-tandem) . 
        
        
        
        /* check the estimate whether it's tandem or combo - same est-type 4 */           
       /* IF NOT ll-tandem AND lv-passwd NE v-oecomb-val THEN DO:
          IF v-oecomb THEN DO:
            RUN custom/d-passwd.w (OUTPUT lv-passwd).
            ll = lv-passwd EQ v-oecomb-val.
          END.*/
        END.
      END.

      /*IF ll THEN DO:
        ll = NO.
        /*{custom/askdel.i} is for local-delete-record */
        MESSAGE "Delete Currently Selected Record(s)?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.            
      END.

      IF NOT ll THEN RETURN.
    END.

    else do:
      message "Deletion not allowed, you must delete the entire order..."
          view-as alert-box error.
      return.
    end.*/
  lv-item-recid = recid(oe-ordl).
  FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-LOCK NO-ERROR.
  run oe/oe-ordd.p  (lv-item-recid, output lv-continue).  /* deleting bol,rel */
  

  for each oe-rel where oe-rel.company eq oe-ordl.company
                    and oe-rel.ord-no  eq oe-ordl.ord-no
                    and oe-rel.i-no    eq oe-ordl.i-no
                    and oe-rel.line    eq oe-ordl.line:
      
        delete oe-rel.
        
  end.

  tmp-ordm-amt = 0.
  if oe-ord.est-no eq ""                              and
     oe-ordl.est-no ne ""                             and
     (oe-ordl.est-type lt 3 or oe-ordl.est-type gt 4) and
     oe-ordl.est-type ne 8                            then
  for each oe-ordm where oe-ordm.company = oe-ordl.company
                     and oe-ordm.ord-no = oe-ordl.ord-no 
                     and oe-ordm.est-no = oe-ordl.est-no:
      if oe-ordm.bill = "Y" then tmp-ordm-amt = tmp-ordm-amt + oe-ordm.amt.               
      
      delete oe-ordm.
  end.
      
  IF oe-ordl.job-no <> "" THEN DO:
     IF ll-valid-eb AND
        CAN-FIND(FIRST job
                 WHERE job.company = oe-ordl.company
                   AND job.job-no = oe-ordl.job-no
                   AND job.job-no2 = oe-ordl.job-no2
                   AND (job.stat = "C" or job.stat = "W" or job.stat = "Z")
                 USE-INDEX job-no)                   
     THEN DO:
        cError = "Item cannot be deleted, Job has been processed or closed. You must close the order. ".
        return.
     end.
     find first job where job.company = oe-ordl.company and
                          job.job-no = oe-ordl.job-no and
                          job.job-no2 = oe-ordl.job-no2 
                          use-index job-no no-lock no-error.
     IF CAN-FIND(FIRST job-hdr
                 WHERE job-hdr.company EQ cocode
                   AND job-hdr.est-no  EQ oe-ordl.est-no
                   AND job-hdr.job-no  EQ oe-ordl.job-no
                   AND job-hdr.job-no2 EQ oe-ordl.job-no2
                   AND job-hdr.ord-no  EQ oe-ordl.ord-no
                   AND job-hdr.i-no    EQ oe-ordl.i-no) THEN DO:
       for each job-hdr where job-hdr.company eq cocode
                        and job-hdr.est-no   eq oe-ordl.est-no
                        and job-hdr.job-no  eq oe-ordl.job-no
                        and job-hdr.job-no2 eq oe-ordl.job-no2
                        and job-hdr.ord-no  eq oe-ordl.ord-no
                        and job-hdr.i-no    eq oe-ordl.i-no  /* use-index enum */ :
          if avail job and job.stat ne "P" then do:
             find first itemfg where itemfg.company eq cocode
                                 and itemfg.i-no    eq oe-ordl.i-no no-error.
          end.
          
          if avail job-hdr then do:
             {util/dljobkey.i}
          end.
          delete job-hdr.
       end.
     END.

     RELEASE job.

     FOR EACH job where job.company eq cocode
                    and job.job-no  eq oe-ordl.job-no
                    and job.job-no2 eq oe-ordl.job-no2
                    and not can-find(first job-hdr
                                     where job-hdr.company eq cocode
                                       and job-hdr.job     eq job.job
                                       and job-hdr.job-no  eq job.job-no
                                       and job-hdr.job-no2 eq job.job-no2
                                     use-index job-no):
 
        run jc/jc-dall.p (recid(job)).                    
 
        for each job-mat where job-mat.company  eq cocode
                           and job-mat.job      eq job.job
                           and job-mat.job-no   eq job.job-no
                           and job-mat.job-no2  eq job.job-no2
                          use-index seq-idx:
            delete job-mat.
        end.
        for each mat-act where mat-act.company  eq cocode
                           and mat-act.job      eq job.job
                           and mat-act.job-no   eq job.job-no
                           and mat-act.job-no2  eq job.job-no2
                           use-index job:
           delete mat-act.
        end.
        for each job-mch where job-mch.company  eq cocode
                           and job-mch.job      eq job.job
                           and job-mch.job-no   eq job.job-no
                           and job-mch.job-no2  eq job.job-no2
                         use-index seq-idx:
           delete job-mch.
        end.
        for each job-prep where job-prep.company  eq cocode
                            and job-prep.job      eq job.job
                            and job-prep.job-no   eq job.job-no
                            and job-prep.job-no2  eq job.job-no2:
           delete job-prep.
        end.
        for each misc-act where misc-act.company  eq cocode
                            and misc-act.job      eq job.job
                            and misc-act.job-no   eq job.job-no
                            and misc-act.job-no2  eq job.job-no2
                          use-index misc-idx:
           delete misc-act.
        end.
            
        if job.exported then do:
           job.stat = "X".
           run jc/kiwiexp2.p (recid(job)).
        end.

        delete job.
     END. /* for each job where not can-find job-hdr for oe-ordl */
  end. /* oe-ordl.job-no <> "" */

  v-delete = no.
  find first itemfg where itemfg.company eq prmComp
                      and itemfg.i-no    eq oe-ordl.i-no no-error.
  if avail itemfg then do:
     IF NOT deleteComps THEN DO:
     /*  IF oe-ord.type NE "T" THEN
         run fg/comp-upd.p (recid(itemfg), oe-ordl.qty * -1,"q-alloc", 0).      */                    
       IF itemfg.isaset THEN DO:
         RUN fg/fullset.p (ROWID(itemfg)).

         FOR EACH tt-fg-set,
             FIRST xitemfg
             WHERE xitemfg.company EQ tt-fg-set.company
               AND xitemfg.i-no    EQ tt-fg-set.part-no:

         /*  ASSIGN
            xitemfg.q-ptd     = xitemfg.q-ptd -
                                (oe-ordl.qty * tt-fg-set.part-qty-dec)
            xitemfg.q-ord-ytd = xitemfg.q-ord-ytd -
                                (oe-ordl.qty * tt-fg-set.part-qty-dec).  */
         END.

         IF v-delete THEN
         FOR EACH fg-set
             WHERE fg-set.company EQ itemfg.company
               AND fg-set.set-no  EQ itemfg.i-no,
             FIRST xitemfg
             WHERE xitemfg.company EQ fg-set.company
               AND xitemfg.i-no    EQ fg-set.part-no:
           FIND FIRST xoe-ordl
               WHERE xoe-ordl.company EQ fg-set.company
                 AND xoe-ordl.i-no    EQ fg-set.part-no
               USE-INDEX item NO-LOCK NO-ERROR.
           IF NOT AVAIL xoe-ordl THEN DO:
             FIND FIRST xfg-set
                 WHERE xfg-set.company EQ fg-set.company
                   AND xfg-set.part-no EQ fg-set.part-no
                   AND ROWID(xfg-set)  NE ROWID(fg-set)
                 USE-INDEX part-no NO-LOCK NO-ERROR.
             IF NOT AVAIL fg-set THEN DELETE xitemfg.
           END. /* not avail xoe-ordl */ 
           DELETE fg-set.
         END.
       END.
     END.

     find xoe-ord where xoe-ord.company eq cocode
                       and xoe-ord.ord-no  eq oe-ordl.ord-no  no-error.
     if avail xoe-ord then do:
           xoe-ord.t-weight = xoe-ord.t-weight -
                              (oe-ordl.qty / 100 * itemfg.weight-100).
           if xoe-ord.t-weight lt 0 then xoe-ord.t-weight = 0.
           run oe/oe-frtcl.p.
     end.

    /* if v-delete then delete itemfg.
     else do:
           IF oe-ord.type NE "T" THEN
             itemfg.q-alloc = itemfg.q-alloc - oe-ordl.qty.
           if itemfg.q-alloc lt 0 then assign itemfg.q-alloc = 0.
           assign itemfg.q-avail   = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc
                  itemfg.q-ptd     = itemfg.q-ptd - oe-ordl.qty
                  itemfg.q-ord-ytd = itemfg.q-ord-ytd - oe-ordl.qty.
     end.*/

     find first itemfg where itemfg.company eq cocode
                            and itemfg.i-no    eq oe-ordl.i-no no-lock no-error.
     if not avail itemfg then v-blank-fg-on-est = 2.
     if v-blank-fg-on-est      eq 2                              or
           (itemfg.avg-cost       eq 0 and itemfg.last-cost eq 0 and
            itemfg.total-std-cost eq 0 and itemfg.beg-bal   eq 0 and
            itemfg.q-onh          eq 0 and itemfg.q-ono     eq 0 and
            itemfg.q-alloc        eq 0 and itemfg.q-back    eq 0 and
            itemfg.q-avail        eq 0)                             then
     for each est where est.company eq xoe-ord.company
                    and est.est-no  eq fill(" ",8 - length(trim(oe-ordl.est-no))) +
                        trim(oe-ordl.est-no):

       find last xoe-ord where xoe-ord.company EQ cocode and
                               xoe-ord.est-no  EQ oe-ordl.est-no and
                               xoe-ord.ord-no  NE oe-ordl.ord-no
                               no-lock no-error.
       if avail xoe-ord then
         assign
          est.ord-date = xoe-ord.ord-date
          est.ord-no   = xoe-ord.ord-no.
       else do:
         if est.ord-date eq oe-ord.ord-date then est.ord-date = ?.
         if est.ord-no eq oe-ord.ord-no then est.ord-no = 0.
       end.

       for each eb where eb.company = est.company
                      and eb.est-no  eq est.est-no
                      and eb.stock-no   ne ""
                      and (eb.stock-no  eq oe-ordl.i-no or
                           est.est-type eq 2 or est.est-type eq 6):
          
            if v-blank-fg-on-est EQ 0 AND 
               oe-ordl.est-type NE 3  AND
               oe-ordl.est-type NE 4  AND
               oe-ordl.est-type NE 8  then do:
              choice = yes.
              cError =  "Remove FG Item# from the Estimate?".
                 
              v-blank-fg-on-est = int(choice) + 1.
            end.
            if v-blank-fg-on-est eq 2 then eb.stock-no = "".
       end.
     end.    
  end. /* avail itemfg */

  find xoe-ord where xoe-ord.company eq cocode
                       and xoe-ord.ord-no  eq oe-ordl.ord-no  no-error.

  if oe-ordl.est-type eq 1 or oe-ordl.est-type eq 5 then do:
     find first job where job.company  eq cocode
                      and job.job-no   eq oe-ordl.job-no
                      and job.job-no2  eq oe-ordl.job-no2
                      and job.stat     eq "P"
                      use-index stat-idx no-error.
     if avail job then do:
        find first job-hdr where job-hdr.company  eq cocode
                             and job-hdr.loc      eq locode
                             and job-hdr.est-no    eq oe-ordl.est-no
                             and job-hdr.job-no   eq oe-ordl.job-no
                             and job-hdr.job-no2  eq oe-ordl.job-no2
                             and job-hdr.ord-no   eq oe-ordl.ord-no no-error.
        if avail job-hdr then do:
           {util/dljobkey.i}        
           delete job-hdr.
        end.          
        run jc/jc-dall.p (recid(job)).          
        delete job.
     end.
  end.
  else if oe-ordl.est-type eq 2 or oe-ordl.est-type eq 6 then do:
       find first job-hdr where job-hdr.company eq cocode
                            and job-hdr.loc     eq locode
                            and job-hdr.est-no   eq oe-ordl.est-no
                            and job-hdr.job-no  eq oe-ordl.job-no
                            and job-hdr.job-no2 eq oe-ordl.job-no2
                            and job-hdr.ord-no  eq oe-ordl.ord-no  no-error.
       if avail job-hdr then delete job-hdr.
  end.
  else
  if (oe-ordl.est-type eq 3 or
      oe-ordl.est-type eq 4 or
      oe-ordl.est-type eq 8)    and
     askdel              then do:      
    {ce/tan-del.i}
    FIND CURRENT bf-ordl NO-ERROR.
    IF AVAIL bf-ordl THEN DELETE bf-ordl.
      /*RUN order-from-est IN WIDGET-HANDLE(char-hdl) (?).*/
  end.

  find xoe-ord where xoe-ord.company eq cocode
                 and xoe-ord.ord-no  eq oe-ord.ord-no no-error.
  FIND FIRST cust
      {sys/ref/cust.w}
        AND cust.cust-no EQ xoe-ord.cust-no
      USE-INDEX cust NO-LOCK NO-ERROR.

  if cust.auto-reprice then run oe/oe-rpric.p.

  run oe/oe-comm.p.

  FIND xoe-ord WHERE RECID(xoe-ord) EQ fil_id NO-ERROR.
       /** change order status to (u)pdated **/
  IF AVAIL xoe-ord THEN DO:
    IF xoe-ord.stat NE "N" AND
       xoe-ord.stat NE "H" AND
       xoe-ord.stat NE "A" THEN xoe-ord.stat = "U".
    xoe-ord.t-freight = xoe-ord.t-freight - oe-ordl.t-freight.
    
  END.
  find xoe-ord where recid(xoe-ord) = fil_id no-lock no-error.
    FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-ERROR.
    IF AVAIL bf-ordl THEN DELETE bf-ordl.
    
      
  ELSE DO:
    CREATE tt-oe-ordl.
    ASSIGN
     tt-oe-ordl.company = oe-ord.company
     tt-oe-ordl.ord-no  = oe-ord.ord-no
     tt-oe-ordl.line    = 0.

    FIND bf-ordl WHERE RECID(bf-ordl) EQ lv-item-recid NO-ERROR.
    BUFFER-COPY tt-oe-ordl EXCEPT rec_key TO bf-ordl.

  END.
  

  RUN oe/calcordt.p (ROWID(oe-ord)).

  RUN oe/creditck.p (ROWID(oe-ord), YES).

    
END PROCEDURE.
/*******************************************************************************************************/
PROCEDURE crt-itemfg :
def input parameter v-item like itemfg.i-no.
def input parameter v-uom like itemfg.prod-uom.

def var tmpstore as cha no-undo.
def var i as int no-undo.
DEF VAR ll-one-part AS LOG NO-UNDO.
{oe/fgfreight.i}
DEF BUFFER x-eb FOR eb.
DEF BUFFER b-e-itemfg-vend FOR e-itemfg-vend.
DEF BUFFER b-eb2 FOR eb.

DEF BUFFER bf-itemfg FOR itemfg.
    FIND FIRST xeb WHERE xeb.rec_key = eb.rec_key  NO-LOCK NO-ERROR.

{sys/inc/setprint.i}
       
find first cust  where cust.company eq prmComp
                   and cust.cust-no eq xeb.cust-no
    no-lock no-error.

ASSIGN ll-new-record = yes.

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
 itemfg.loc        = locode
 itemfg.i-no       = v-item
 itemfg.i-name     = prmItemName
 itemfg.part-dscr1 = prmPartdscr
 itemfg.part-dscr2 = prmPartdscr2
 itemfg.sell-price = dec(prmPrice)
 itemfg.part-no    = prmPartNum
 itemfg.cust-no    = oe-ord.cust-no
 itemfg.cust-name  = oe-ord.cust-name
 itemfg.pur-uom    = prmUom.


ASSIGN
    itemfg.taxable = IF AVAIL cust 
                      THEN cust.sort EQ "Y" AND cust.tax-gr NE ""
                      ELSE 
                          IF AVAIL bf-itemfg THEN bf-itemfg.taxable
                                             ELSE NO.


IF fgmaster-cha EQ "FGITEM" THEN
    ASSIGN
       itemfg.sell-uom   = prmUom
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
/************************************************************************************/


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
            and xeb.part-no = oe-ordl.part-no
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
        qty = INT(oe-ordl.qty)
        v-shared-rel = v-rel.

    


     IF AVAIL xeb AND AVAIL xef                                     AND
        xest.est-type NE 3                                          /*AND
        xest.est-type NE 4                                          AND
        xest.est-type NE 8 */     
        /*AND (oe-ordl.qty NE qty OR DEC(oe-ordl.cost) EQ 0)*/  THEN DO: 

       
        RUN VALUE(ENTRY(xest.est-type,v-run-list)).     

        oe-ordl.cost = ((IF v-full-cost THEN tt-tot ELSE ord-cost) /
                                           (INT(oe-ordl.qty) / 1000)).

                                                        

     END.
   END.

END PROCEDURE.
