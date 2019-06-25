/*------------------------------------------------------------------------
    File        : CorrugateEstimate.p
    Purpose     :  Corrugated Estimate

    Syntax      :

    Description : Return a Dataset of Corrugated Estimates

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrugateEstimate NO-UNDO
    FIELD vEst              AS CHAR FORMAT "x(8)"
    FIELD vCust             AS CHAR FORMAT "x(8)"
    FIELD vCustPart         AS CHAR FORMAT "x(15)"
    FIELD vShipTo           AS CHAR FORMAT "x(8)"
    FIELD vItemName         AS CHAR FORMAT "x(30)"
    FIELD vFgItem           AS CHAR FORMAT "x(15)"
    FIELD vEstQty           AS DECIMAL  FORMAT ">>>>>>>9"
    FIELD vStyle            AS CHAR FORMAT "x(6)"
    FIELD vFlute            AS CHAR FORMAT "XXX"
    FIELD vTest             AS CHAR FORMAT "x(6)"
    FIELD vBoard            AS CHAR FORMAT "x(12)"
    FIELD vCaliper          AS DECIMAL  FORMAT ">9.99999<"
    FIELD vCategory         AS CHAR FORMAT "x(5)"
    FIELD vLenght           AS DECIMAL  FORMAT ">>9.99"
    FIELD vWidth            AS DECIMAL  FORMAT ">>9.99"
    FIELD vDepth            AS DECIMAL  FORMAT ">>9.99"
    FIELD vForm             AS INT  FORMAT ">9"
    FIELD vBlank            AS INT  FORMAT ">9"
    FIELD vTab              AS CHAR
    FIELD vColor            AS INT  FORMAT ">9"
    FIELD vPasses           AS INT  FORMAT ">9"
    FIELD vCoating          AS INT  FORMAT ">9"
    FIELD vCoatPasses       AS INT  FORMAT ">9"
    FIELD vQtySet           AS DECIMAL FORMAT "->>>>>>>9"
    FIELD vInkFrom          AS INT  FORMAT ">>"
    FIELD vPassesFrom       AS INT  FORMAT ">>"
    FIELD vCoatingFrom      AS INT  FORMAT ">>"
    FIELD vCoatPassesFrom   AS INT  FORMAT ">>"
    FIELD vPurchManuf       AS CHAR
    FIELD vEstDate          AS DATETIME  
    FIELD vEstType          AS INT 
    FIELD vReckey            AS CHAR   
    FIELD vQtyExtent  AS DECIMAL EXTENT 20
    FIELD vRelQtyExtent  AS DECIMAL EXTENT 20  
    FIELD vTlen             AS DECIMAL 
    FIELD vTwid             AS DECIMAL  .


DEFINE DATASET dsCorrugateEstimate FOR ttCorrugateEstimate.

    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstimate          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCust              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShipTo            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmItemName          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFgItem            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmStyle             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFlute             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTest              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoard             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCalliper          AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmCategory          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLength            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmWidth             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmDepth             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmFrom              AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank             AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmTab               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmColor             AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPasses            AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoating           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatPasses        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmQtySet            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmInkFrom           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPassesFrom        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatingFrom       AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmCoatPassesFrom    AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmPurchManuf        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEstDate           AS DATETIME NO-UNDO.
    DEFINE INPUT PARAMETER prmType              AS CHAR NO-UNDO. 
    DEFINE INPUT PARAMETER prmMassType          AS CHAR NO-UNDO.
    
    DEFINE INPUT PARAMETER prmEstQty2            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty3           AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty4            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty5            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty6            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty7            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty8            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty9            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty10            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty11            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty12            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty13            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty14            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty15            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty16            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty17            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty18            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty19            AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmEstQty20            AS DECIMAL NO-UNDO.

    DEFINE INPUT PARAMETER prmRelQty1             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty2             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty3             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty4             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty5             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty6             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty7             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty8             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty9             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty10             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty11             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty12             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty13             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty14             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty15             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty16             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty17             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty18             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty19             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmRelQty20             AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmlvcopied            AS CHAR NO-UNDO.

    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrugateEstimate.

    IF   prmAction         = ?      THEN    prmAction       = "".       
    IF   prmUser           = ?      THEN    prmUser         = "".       
    IF   prmEstimate       = ?      THEN    prmEstimate     = "".       
    IF   prmCust           = ?      THEN    prmCust         = "".       
    IF   prmCustPart       = ?      THEN    prmCustPart     = "".       
    IF   prmShipTo         = ?      THEN    prmShipTo       = "".       
    IF   prmItemName       = ?      THEN    prmItemName     = "".       
    IF   prmFgItem         = ?      THEN    prmFgItem       = "".       
    IF   prmEstQty         = ?      THEN    prmEstQty       = 0.       
    IF   prmStyle          = ?      THEN    prmStyle        = "".       
    IF   prmFlute          = ?      THEN    prmFlute        = "".       
    IF   prmTest           = ?      THEN    prmTest         = "".       
    IF   prmBoard          = ?      THEN    prmBoard        = "".       
    IF   prmCalliper       = ?      THEN    prmCalliper     = 0.       
    IF   prmCategory       = ?      THEN    prmCategory     = "".       
    IF   prmLength         = ?      THEN    prmLength       = 0.       
    IF   prmWidth          = ?      THEN    prmWidth        = 0.       
    IF   prmDepth          = ?      THEN    prmDepth        = 0.       
    IF   prmFrom           = ?      THEN    prmFrom         = 0.       
    IF   prmBlank          = ?      THEN    prmBlank        = 0.       
    IF   prmTab            = ?      THEN    prmTab          = "".       
    IF   prmColor          = ?      THEN    prmColor        = 0.       
    IF   prmPasses         = ?      THEN    prmPasses       = 0.       
    IF   prmCoating        = ?      THEN    prmCoating      = 0.       
    IF   prmCoatPasses     = ?      THEN    prmCoatPasses   = 0.       
    IF   prmQtySet         = ?      THEN    prmQtySet       = 1.       
    IF   prmInkFrom        = ?      THEN    prmInkFrom      = 1.       
    IF   prmPassesFrom     = ?      THEN    prmPassesFrom   = 1.       
    IF   prmCoatingFrom    = ?      THEN    prmCoatingFrom  = 1.       
    IF   prmCoatPassesFrom = ?      THEN    prmCoatPassesFrom = 1.     
    IF   prmPurchManuf     = ?      THEN    prmPurchManuf     = "".
    IF   prmlvcopied       = ?      THEN    prmlvcopied       = "".


    IF  prmEstQty2    = ?      THEN   prmEstQty2    = 0.       
    IF  prmEstQty3    = ?      THEN   prmEstQty3    = 0.       
    IF  prmEstQty4    = ?      THEN   prmEstQty4    = 0.       
    IF  prmEstQty5    = ?      THEN   prmEstQty5    = 0.       
    IF  prmEstQty6    = ?      THEN   prmEstQty6    = 0.       
    IF  prmEstQty7    = ?      THEN   prmEstQty7    = 0.       
    IF  prmEstQty8    = ?      THEN   prmEstQty8    = 0.       
    IF  prmEstQty9    = ?      THEN   prmEstQty9    = 0.       
    IF  prmEstQty10   = ?      THEN   prmEstQty10   = 0.       
    IF  prmEstQty11   = ?      THEN   prmEstQty11   = 0.       
    IF  prmEstQty12   = ?      THEN   prmEstQty12   = 0.       
    IF  prmEstQty13   = ?      THEN   prmEstQty13   = 0.       
    IF  prmEstQty14   = ?      THEN   prmEstQty14   = 0.       
    IF  prmEstQty15   = ?      THEN   prmEstQty15   = 0.       
    IF  prmEstQty16   = ?      THEN   prmEstQty16   = 0.       
    IF  prmEstQty17   = ?      THEN   prmEstQty17   = 0.       
    IF  prmEstQty18   = ?      THEN   prmEstQty18   = 0.       
    IF  prmEstQty19   = ?      THEN   prmEstQty19   = 0.       
    IF  prmEstQty20   = ?      THEN   prmEstQty20   = 0.       
    
    IF  prmRelQty1    = ?      THEN   prmRelQty1    = 0.       
    IF  prmRelQty2    = ?      THEN   prmRelQty2    = 0.       
    IF  prmRelQty3    = ?      THEN   prmRelQty3    = 0.       
    IF  prmRelQty4    = ?      THEN   prmRelQty4    = 0.       
    IF  prmRelQty5    = ?      THEN   prmRelQty5    = 0.       
    IF  prmRelQty6    = ?      THEN   prmRelQty6    = 0.       
    IF  prmRelQty7    = ?      THEN   prmRelQty7    = 0.       
    IF  prmRelQty8    = ?      THEN   prmRelQty8    = 0.       
    IF  prmRelQty9    = ?      THEN   prmRelQty9    = 0.       
    IF  prmRelQty10   = ?      THEN   prmRelQty10   = 0.     
    IF  prmRelQty11   = ?      THEN   prmRelQty11   = 0.
    IF  prmRelQty12   = ?      THEN   prmRelQty12   = 0.
    IF  prmRelQty13   = ?      THEN   prmRelQty13   = 0.
    IF  prmRelQty14   = ?      THEN   prmRelQty14   = 0.
    IF  prmRelQty15   = ?      THEN   prmRelQty15   = 0.
    IF  prmRelQty16   = ?      THEN   prmRelQty16   = 0.
    IF  prmRelQty17   = ?      THEN   prmRelQty17   = 0.
    IF  prmRelQty18   = ?      THEN   prmRelQty18   = 0.
    IF  prmRelQty19   = ?      THEN   prmRelQty19   = 0.
    IF  prmRelQty20   = ?      THEN   prmRelQty20   = 0.
    
    DEFINE VAR estprpqty AS INT NO-UNDO.
def buffer bb for eb.
def buffer bf for ef.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER del-eb FOR eb.
DEF BUFFER bf1-eb FOR eb.
def buffer bf-eb-ship for eb .
def buffer bf-eb for eb .
DEF BUFFER bf-ef FOR ef.
    DEF BUFFER b-ref FOR reftable.
DEFINE TEMP-TABLE ttTestLook NO-UNDO 
FIELD vCorrTest         AS CHARACTER .
DEF TEMP-TABLE tt-eb LIKE eb FIELD row-id AS ROWID INDEX row-id row-id.
DEF VAR op-comm    AS DEC DECIMALS 10  NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
/*DEF VAR ll-copied-from-eb AS LOG NO-UNDO.*/

DEF VAR ll-valid AS LOG INIT NO NO-UNDO.
DEF VAR li-pnt AS INT NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-count2 AS INT NO-UNDO.
DEFINE VAR outcomm AS DECIMAL NO-UNDO.
 DEF VAR ld-markup AS DEC NO-UNDO.
 
  def buffer bqty for est-qty.
  def buffer best for est.
 
  def var ll-dum as log no-undo.
  def var lv-ef-recid as recid no-undo.
  def var lv-eb-recid as recid no-undo.
  DEF VAR ll-mass-del AS LOG NO-UNDO.
  def var li-est-type like est.est-type no-undo.
  def var lv-layers AS DEC no-undo.
  

/*{sys/inc/var.i SHARED} */
def var lv-foam as log no-undo.
DEF BUFFER recalc-mr FOR reftable.
def var li-new-estnum as INT no-undo.
DEF VAR li AS INT NO-UNDO.
DEF VAR prev-cust LIKE eb.cust-no NO-UNDO.
DEF VAR prev-ship LIKE eb.ship-id NO-UNDO.
DEF VAR prev-style LIKE eb.style NO-UNDO.
DEF VAR ls-part-no AS cha NO-UNDO.
DEF VAR prev-yrprice LIKE eb.yrprice NO-UNDO.
def var ll-is-copy-record as log no-undo.
DEF VAR ll-new-shipto AS LOG NO-UNDO.
DEF VAR lv-copy-what AS cha NO-UNDO.
DEFINE VAR vQty AS DECIMAL NO-UNDO.
DEFINE VAR prmLoc AS CHAR NO-UNDO.
def var lv-rowid as rowid no-undo.
def var ll-dumb as log no-undo.
def var char-hdl as cha no-undo.
def var li-row-num as int no-undo.
def var li-cnt as int no-undo.
DEF VAR lv-part-no LIKE eb.part-no NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEFINE VAR CorType AS INTEGER NO-UNDO.
DEFINE VAR varEqty AS INT NO-UNDO.
def var lv-hld-eqty like est-qty.eqty no-undo.
def var xx as dec no-undo.
def var co as int no-undo.

def var k_frac as dec init 6.25 no-undo.
def var ls-add-what as cha no-undo.
def buffer bf-est for est.
    def buffer bf-eb-comm for eb.

DEF BUFFER xbox-design-hdr FOR box-design-hdr.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
DEF NEW SHARED TEMP-TABLE tt-eb-set NO-UNDO LIKE eb.
DEF NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
DEF NEW SHARED var locode     as   char  format "x(5)"  no-undo.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
DEF new shared temp-table formule field formule as dec extent 12.


DEF NEW SHARED var  x  as   int no-undo.
DEF NEW SHARED var  y  as   int no-undo.
DEF NEW SHARED  VAR  k  as   int no-undo.
DEF VAR lv-hld-style LIKE eb.style NO-UNDO.
DEF VAR vTr-no AS CHAR NO-UNDO.

def var i          as   int no-undo.
def var j          as   int no-undo.

DEF VAR lv-cad-path AS cha NO-UNDO.  /* cad file - boximage path for Fibre */
DEF VAR lv-cad-ext AS cha NO-UNDO.
DEF VAR dieFile AS CHARACTER NO-UNDO.
DEF VAR cadFile AS CHARACTER NO-UNDO.
def var lv-estqty-recid as recid no-undo.
def var char-val as cha no-undo.
def var char-val2 as cha no-undo. 
def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-copy-qty as int extent 20 no-undo.
     DEF VAR ls-delete AS CHAR NO-UNDO.
     DEF VAR li-delete AS INT NO-UNDO.

/*local assign varable*/
DEF VAR lv-hld-fcol LIKE ef.f-col NO-UNDO.
  DEF VAR lv-hld-fpas LIKE ef.f-pass NO-UNDO.
  DEF VAR lv-hld-fcot LIKE ef.f-coat NO-UNDO.
  DEF VAR lv-hld-fctp LIKE ef.f-coat-p NO-UNDO.

{sys/inc/f16to32.i}
DEF TEMP-TABLE tt-est-op LIKE est-op.
{cec/descalc.i new}
    DEFINE VAR prmComp AS CHAR NO-UNDO.
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .
ASSIGN
    cocode = prmComp 
    locode = prmLoc 
    g_company = prmComp 
    g_loc     = prmLoc  .

 {sys/inc/ceroute.i C} 
 {ce/cecopy.i}

  FUNCTION display-combo-qty RETURNS DECIMAL():
      DEF VAR lv-qty LIKE est-qty.eqty NO-UNDO.
      DEF BUFFER b-eb FOR eb.
          IF AVAIL est-qty AND AVAIL eb THEN DO: 
            ASSIGN  lv-qty = est-qty.eqty  .

              FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
              IF AVAIL b-eb AND b-eb.est-type EQ 8 THEN lv-qty = b-eb.bl-qty.
              END.
              RETURN lv-qty.   /* Function return value. */
   END FUNCTION.

   
FUNCTION display-cw-dim RETURNS DECIMAL( input ip-is-corr-style as log, input  ip-dim as decimal ) :
    def var out-dim as dec no-undo.
    if ip-is-corr-style and ip-dim <> 0 then 
        out-dim = round(trunc(ip-dim,0) + ((ip-dim - trunc(ip-dim,0)) / K_FRAC),2).
    else out-dim = ip-dim.
       RETURN out-dim.   /* Function return value. */
END FUNCTION.
/******************vali****************************/



/*******************ADD*********************/

IF prmAction = "Add" THEN do:
  IF prmType = "Single" THEN DO:
      ASSIGN 
          CorType = 5.
  END.
  IF prmType = "Set" THEN DO:
      ASSIGN 
          CorType = 6.
  END.
  IF prmType = "Tandem" THEN DO:
      ASSIGN 
          CorType = 8.
  END.
FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc  EXCLUSIVE-LOCK NO-ERROR.
                          
li-new-estnum = int(prmEstimate) .


CREATE est.
assign est.est-type = CorType
       est.company = prmComp
       est.loc     = prmLoc
       est.est-no = string(li-new-estnum,">>>>>>>9")
       est.form-qty = 1
       est.est-date = today
       est.mod-date = today
       est.entered-id   = prmUser
      
       .

/*{sys/ref/est-add.i est F} */

CREATE est-qty.
ASSIGN 
    est-qty.company  = prmComp
    est-qty.est-no   = est.est-no
    est-qty.eqty     = prmEstQty
    est-qty.qty-date = est.est-date.

FIND LAST ef  WHERE ef.company EQ est-qty.company
     AND ef.est-no  EQ est-qty.est-no
      AND ef.eqty    EQ est-qty.eqty
    USE-INDEX est-qty NO-LOCK NO-ERROR.
li = IF AVAIL ef THEN ef.form-no ELSE 0.

         CREATE ef.
         ASSIGN
             ef.est-type  = est.est-type
             ef.company   = prmComp
             ef.loc       = prmLoc
             ef.e-num     = est.e-num
             ef.est-no    = est.est-no
             ef.eqty      = est-qty.eqty
             ef.form-no   = li + 1
             ef.cust-seq  = 1
             ef.blank-qty = 1
             ef.lsh-wid   = prmLength
             ef.lsh-len   = prmWidth
             ef.cost-uom  = "MSF"
             ef.f-col     = prmColor 
             ef.f-pass    = prmPasses
             ef.f-coat    = prmCoating
             ef.f-coat-p  = prmCoatPasses  
             ef.board  = prmBoard
             ef.cal    = prmCalliper
             .

             FIND FIRST item NO-LOCK
                 WHERE item.company EQ ef.company
                 AND item.i-no    EQ ef.board
                 NO-ERROR.
             IF AVAIL item THEN DO:
                 ASSIGN
                     ef.brd-dscr = item.i-name
                     ef.weight   = item.basis-w.  
                 FIND FIRST e-item OF item NO-LOCK NO-ERROR.
                 IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.
              END.
         

         FOR EACH eb
             WHERE eb.company EQ ef.company
             AND eb.est-no  EQ ef.est-no
             AND eb.eqty    EQ ef.eqty
             NO-LOCK
             BY eb.form-no  DESC
             BY eb.blank-no DESC:
             prev-yrprice = eb.yrprice.
           LEAVE.
        END.


        FIND LAST eb NO-LOCK
            WHERE eb.company EQ ef.company
            AND eb.est-no  EQ ef.est-no
            AND eb.form-no NE 0
            USE-INDEX est-qty NO-ERROR.
        IF AVAIL eb THEN
            ASSIGN
            prev-cust  = eb.cust-no
            prev-ship  = eb.ship-id
            prev-style = eb.style.
        
        FIND LAST eb NO-LOCK
            WHERE eb.company EQ ef.company
            AND eb.est-no  EQ ef.est-no
            AND eb.form-no EQ ef.form-no
            USE-INDEX est-qty NO-ERROR.
        li = IF AVAIL eb THEN eb.blank-no ELSE 0.

              CREATE eb.
              ASSIGN
                  eb.est-type = ef.est-type
                  eb.company  = ef.company
                  eb.loc      = ef.loc
                  eb.e-num    = ef.e-num
                  eb.est-no   = ef.est-no
                  eb.est-int  = INT(ef.est-no)
                  eb.eqty     = ef.eqty
                  eb.form-no  = ef.form-no
                  eb.cust-seq = 1
                  eb.blank-no = li + 1 
                  eb.cas-no   = ce-ctrl.def-case
                  eb.tr-no    = ce-ctrl.def-pal
                  eb.tr-cas    = 1
                  eb.cust-%   = INT(ef.est-type EQ 2) .

              {est/blankcp2.i}

                 ASSIGN
                  eb.part-no  = prmCustPart
                  eb.cust-no  = prmCust
                  eb.stock-no  = prmFgItem
                  eb.ship-id  = prmShipTo
                  eb.style    = prmStyle
                  eb.part-dscr1 = prmItemName
                  eb.flute     = prmFlute
                  eb.test  = prmTest
                  eb.yld-qty = prmQtySet 
                  eb.bl-qty = prmEstQty 
                  eb.procat = prmCategory
                  eb.form-no = 1
                  eb.blank-no = 1 
                  eb.i-col = prmColor
                  eb.i-pass = prmPasses
                  eb.i-coat = prmCoating 
                  eb.i-coat-p =  prmCoatPasses
                  eb.pur-man  = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                  eb.tab-in   = IF prmTab = "In" THEN TRUE ELSE FALSE 
                  eb.len   = prmLength
                  eb.wid      = prmWidth
                  eb.dep      = prmDepth
                  est.est-date = prmEstDate 

                  eb.yrprice  = prev-yrprice                      
                  eb.cust-%   = INT(ef.est-type EQ 2)
                  eb.chg-method = "P"  .
                  

                  FIND FIRST ITEM WHERE ITEM.company = ef.company
                      AND ITEM.mat-type = "C"
                      AND item.i-no eq eb.cas-no no-lock no-error.
                  if avail item then do:
                      find first e-item
                          where e-item.company eq item.company
                          /* and e-item.loc     eq item.loc */
                          and e-item.i-no    eq item.i-no   no-lock no-error.
                      find first itemfg
                          where itemfg.company eq prmComp
                          and itemfg.i-no    eq eb.stock-no   no-lock no-error.
                      if avail e-item then
                          ASSIGN eb.cas-len = e-item.case-l
                          eb.cas-wid = e-item.case-w
                          eb.cas-dep = e-item.case-d
                          eb.cas-wt  = e-item.avg-w
                          eb.cas-pal = e-item.case-pall
                          eb.cas-cnt = if avail itemfg then   itemfg.case-count else e-item.box-case.

                       if eb.cas-len eq 0 then eb.cas-len = item.case-l.
                       if eb.cas-wid eq 0 then eb.cas-wid = item.case-w.
                       if eb.cas-dep eq 0 then eb.cas-dep = item.case-d.
                       if eb.cas-wt  eq 0 then eb.cas-wt  = item.avg-w.
                       if eb.cas-pal eq 0 then eb.cas-pal = item.case-pall.
                       if eb.cas-cnt eq 0 then eb.cas-cnt =
                           if avail itemfg then itemfg.case-count else item.box-case.
                      end.
                     
 if not ll-copied-from-eb THEN DO:
  FIND CURRENT eb EXCLUSIVE-LOCK NO-ERROR.
  FIND CURRENT ef EXCLUSIVE-LOCK NO-ERROR.
find cust where cust.company = prmComp
                and cust.cust-no = eb.cust-no no-lock no-error.
 if avail cust and cust.pallet <> "" then vTr-no = cust.pallet . 

     eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
     eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.  
     eb.sman  = IF AVAIL cust THEN cust.sman ELSE "" .

    /* get default values from rm table */
     find item where item.company = eb.company and
                     item.i-no = eb.cas-no
              no-lock no-error.
     if avail item then assign eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)        .  
                             
    
         find item where item.company = prmComp and
                item.i-no = vTr-no no-lock no-error. 
         
            if avail item then 
                assign 
                eb.tr-len = item.case-l
                eb.tr-wid = item.case-w
                eb.tr-dep = item.case-d.
            
               find style where style.company = prmComp and
                   style.style = prmStyle no-lock no-error.
               if avail style then 
                   assign
                   eb.adhesive = style.material[7] 
                   eb.gluelap = style.dim-gl
                   eb.k-len = style.dim-dkl
                   eb.k-wid = style.dim-dkw 
                   eb.lock = style.dim-fit
                   eb.tuck = style.dim-tk.    
               find style where style.company = eb.company and
                   style.style = eb.style
                   no-lock no-error.
                    lv-foam = IF AVAIL style AND style.TYPE = "F" THEN YES ELSE NO .
                    ef.xgrain = "N" .
                
                    RELEASE bf-eb-comm.
                    IF est.est-type EQ 6 THEN
                        FIND FIRST bf-eb-comm
                        WHERE bf-eb-comm.company EQ eb.company
                        AND bf-eb-comm.est-no  EQ eb.est-no
                        AND bf-eb-comm.form-no EQ 0
                        AND bf-eb-comm.procat  NE ""
                        NO-LOCK NO-ERROR.
                    RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                    FIND FIRST sman
                        WHERE sman.company EQ cocode
                        AND sman.sman    EQ eb.sman
                        NO-LOCK NO-ERROR.
                    IF AVAIL cust THEN
                        FIND FIRST custype WHERE custype.custype EQ cust.type NO-LOCK NO-ERROR.
                    IF AVAIL sman THEN
                        RUN custom/com-comm.p (cocode, sman.sman, custype.custype, IF AVAIL bf-eb-comm THEN bf-eb-comm.procat ELSE eb.procat, ld-markup,
                            IF AVAIL cust THEN cust.cust-no ELSE "",
                                OUTPUT op-comm).
                    ELSE DO:
                        IF AVAIL custype AND custype.custype NE "" THEN op-comm = custype.commrate.
                        ELSE DO:
                          FIND FIRST ce-ctrl
                              WHERE ce-ctrl.company EQ cocode
                              AND ce-ctrl.loc     EQ locode
                              NO-LOCK NO-ERROR.
                          IF AVAIL ce-ctrl THEN op-comm = ce-ctrl.comm-mrkup.
                       END.
                  END.

                  ASSIGN eb.comm = op-comm .

                    if style.material[4] ne "" then do:  /* adder*/ 
                        find first item  where item.company eq cocode
                            and item.i-no    eq style.material[4]
                            no-lock no-error.
                        if avail item then
                            do i = 1 to 6:  
                            if ef.adder[i] = "" then do:
                                assign ef.adder[i]     = item.i-no
                                    ef.adder[i + 6] = item.est-dscr
                                    ef.weight       = ef.weight + item.basis-w.
                                leave.       
                            end.           
                          end.
                       end.

                    if style.material[5] ne "" then do:  /* leaf label */
                        find first item  where item.company eq cocode
                            and item.i-no    eq style.material[5]
                            no-lock no-error.
                        if avail item then /*leaf-block:
                        for each ef where ef.company eq xest.company and
                        ef.est-no = xest.est-no,
                        first eb of ef no-lock:  */
                            do i = 1 to 2:
                            if ef.leaf[i] = "" then do:
                                assign ef.leaf-snum[i] = ef.form-no
                                    ef.leaf-bnum[i] = 1
                                    ef.leaf[i]      = item.i-no
                                    ef.leaf-dscr[i] = item.est-dscr
                                    ef.leaf-l[i] = eb.t-len
                                    ef.leaf-w[i] = eb.t-wid.
                                leave.
                              end.   
                           END.
                      END.
                      
                      RUN calc-pass NO-ERROR.
                      RUN calc-blank-size NO-ERROR.
                      RUN calc-layout-corr NO-ERROR.
 END.

                      if lv-foam then assign
                                         eb.t-wid = eb.wid
                                         eb.t-len = eb.len
                                         eb.t-dep = eb.dep
                                         ef.cost-uom = "BF".

                    FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
                    IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
                        THEN eb.lin-in = eb.dep.
                    
                    RUN create-stack.

                      IF eb.est-type EQ 2 THEN DO:
                          DEF BUFFER bfx-eb FOR eb.
                              FIND FIRST bfx-eb
                                  WHERE bfx-eb.company  EQ eb.company
                                  AND bfx-eb.est-no   EQ eb.est-no 
                                  AND bfx-eb.form-no  EQ 0
                                  AND bfx-eb.blank-no EQ 0
                                  NO-LOCK NO-ERROR.
                              IF AVAIL bfx-eb THEN eb.procat = bfx-eb.procat.
                            END.

                            FIND FIRST shipto  WHERE shipto.company EQ prmComp
                                AND shipto.cust-no EQ prmCust
                                AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
                            IF AVAIL shipto THEN
                                ASSIGN
                                eb.ship-id      = shipto.ship-id
                                eb.ship-name    = shipto.ship-name
                                eb.ship-addr[1] = shipto.ship-addr[1]
                                eb.ship-addr[2] = shipto.ship-addr[2]
                                eb.ship-city    = shipto.ship-city
                                eb.ship-state   = shipto.ship-state
                                eb.ship-zip     = shipto.ship-zip.
                            IF CorType = 8 THEN DO:
                                ASSIGN
                                    eb.bl-qty      = prmEstQty
                                    eb.yld-qty     = prmEstQty .
                                END.

                                ASSIGN 
                                    lv-hld-eqty = est-qty.eqty  .
                                
                                RUN  qty-assign NO-ERROR.
                               
                                IF (CorType GE 5 AND CorType LE 6 AND eb.i-col + eb.i-coat EQ 0) OR
                                    (CorType GE 7 AND CorType LE 8 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
                                {ce/delplate.i}
                                END.
                                 
                              IF eb.pur-man THEN RUN create-e-itemfg-vend NO-ERROR.

                              IF CorType = 6 THEN DO:
                                  FIND xest WHERE RECID(xest) EQ RECID(est).
                                  FIND FIRST bf1-eb WHERE bf1-eb.company EQ prmComp
                                      AND bf1-eb.est-no  EQ xest.est-no
                                      AND bf1-eb.form-no EQ 0   NO-ERROR.
                                  IF NOT AVAIL bf1-eb THEN DO:
                                      {ce/set-info.a 6 "bf1-" "x"}
                                          ASSIGN
                                          bf1-eb.stock-no   = eb.stock-no
                                          bf1-eb.part-no    = eb.part-no
                                          bf1-eb.part-dscr1 = eb.part-dscr1
                                          bf1-eb.part-dscr2 = eb.part-dscr2
                                          bf1-eb.procat     = eb.procat
                                          bf1-eb.len        = eb.len
                                          bf1-eb.wid        = eb.wid
                                          bf1-eb.dep        = eb.dep.
                                      /*RUN cec/d-updset.w (RECID(b-eb),6).*/
                                      END.
                              END.
                              IF est.est-type NE 8 THEN
                                  FOR EACH bf-eb-ship
                                  WHERE bf-eb-ship.company EQ eb.company
                                  AND bf-eb-ship.est-no  EQ eb.est-no
                                  AND ROWID(bf-eb-ship)  NE ROWID(eb):
                                  ASSIGN
                                      bf-eb-ship.cust-no      = eb.cust-no
                                      bf-eb-ship.ship-id      = eb.ship-id
                                      bf-eb-ship.ship-no      = eb.ship-no
                                      bf-eb-ship.ship-name    = eb.ship-name
                                      bf-eb-ship.ship-addr[1] = eb.ship-addr[1]
                                      bf-eb-ship.ship-addr[2] = eb.ship-addr[2]
                                      bf-eb-ship.ship-city    = eb.ship-city
                                      bf-eb-ship.ship-state   = eb.ship-state
                                      bf-eb-ship.ship-zip     = eb.ship-zip
                                      bf-eb-ship.sman         = eb.sman
                                      bf-eb-ship.comm         = eb.comm.
                                  END.

                              
                               RUN  create-est-op NO-ERROR.
                               RUN create-prep NO-ERROR.
                               IF CorType GE 7 AND CorType LE 8 THEN
                                     IF lv-hld-fcol NE ef.f-col    OR
                                        lv-hld-fpas NE ef.f-pass   OR
                                        lv-hld-fcot NE ef.f-coat   OR 
                                        lv-hld-fctp NE ef.f-coat-p THEN DO:
                                         FOR EACH est-prep  WHERE est-prep.company   EQ est.company
                                             AND est-prep.est-no    EQ est.est-no
                                             AND est-prep.s-num     EQ ef.form-no
                                             AND (est-prep.mat-type EQ "F" OR est-prep.mat-type EQ "P"):
                                                RUN sys/inc/flm-prep.p(RECID(est), est-prep.s-num, OUTPUT estprpqty ).
                                                ASSIGN
                                                    est-prep.qty  =  estprpqty .
                                         END.
                                 END.
                             
                          ASSIGN 
                              prmEstimate = string(li-new-estnum,">>>>>>>9")
                              prmFrom     = 1
                              prmAction   = "Select"  .
                   
    END.
/********************Delete**************************************/
 
 IF prmAction = "Delete" THEN DO:
   
  FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND est.company = prmComp NO-LOCK NO-ERROR.
  FIND FIRST est-qty where est-qty.company = prmComp 
             and est-qty.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)  NO-LOCK NO-ERROR.
                   
      for each eb where eb.company = est.company 
                       and eb.est-no = est.est-no
                       and eb.eqty = est-qty.eqty AND eb.form-no = prmFrom AND eb.blank-no = prmBlank NO-LOCK:

                       for each ef where ef.company = est-qty.company 
                        and ef.est-no = est-qty.est-no
                        and ef.eqty = est-qty.eqty AND  ef.form-no = eb.form-no   NO-LOCK:

                    

    FIND bb WHERE ROWID(bb) EQ ROWID(eb) EXCLUSIVE-LOCK.
    FIND bf WHERE ROWID(bf) EQ ROWID(ef) EXCLUSIVE-LOCK.
    FIND bqty WHERE ROWID(bqty) EQ ROWID(est-qty) EXCLUSIVE-LOCK.
    FIND best WHERE ROWID(best) EQ ROWID(est) EXCLUSIVE-LOCK.

    ll-dum = YES.
    ll-mass-del = NO.
    
    IF ll-dum THEN DO:
        IF AVAIL bb THEN DELETE bb.

        IF AVAIL bf THEN DO:
            RUN est/blks-frm.p (ROWID(bf)).
            FIND CURRENT bf NO-ERROR.

            IF bf.blank-qty EQ 0 THEN DELETE bf.
        END.

        IF AVAIL best THEN DO:
            RUN est/frms-est.p (ROWID(best)).
            FIND CURRENT best NO-ERROR.

            IF best.form-qty EQ 0 THEN DO:
                ASSIGN
                    lv-ef-recid = ?
                    lv-eb-recid = ?.
                
          DELETE best.
        END.
      END.
    END.

    RELEASE bb.
    RELEASE bf.
    RELEASE bqty.
    RELEASE best.
  
  FIND CURRENT est NO-LOCK NO-ERROR.
  
  IF AVAIL est THEN DO:
    RUN est/resetf&b.p (ROWID(est), ll-mass-del).
  END.
    END.
      END.

      FIND FIRST est-qty where est-qty.company = prmComp 
             and est-qty.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)  NO-LOCK NO-ERROR.

      FIND FIRST  eb where eb.company = est-qty.company 
                           and eb.est-no = est-qty.est-no
                           and eb.eqty = est-qty.eqty  NO-LOCK NO-ERROR.
      IF AVAIL est-qty AND AVAIL eb  THEN DO:
            ASSIGN
            prmEstimate = eb.est-no
            prmFrom     = 1
            prmAction   = "Select" .
      END.

      ELSE DO:
          FIND LAST eb  WHERE eb.company = prmComp AND  (eb.est-type = 5 OR eb.est-type = 6 OR eb.est-type = 8) NO-LOCK NO-ERROR.
          ASSIGN
              prmEstimate = eb.est-no
              prmFrom     = 1
              prmAction   = "Select" .
       END.

   
 END.  /*  end of delete*/


 /***********************mass delete*************************/
 
 IF prmAction = "MassDelete" THEN DO:
   
      FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)    AND eb.company = prmComp AND eb.form-no NE 0 NO-LOCK NO-ERROR. 

     FOR EACH tt-eb:
    DELETE tt-eb.
  END.
  ASSIGN  ls-delete = prmMassType .
/*  ll-mass-del = NO.*/
 
  li-delete = LOOKUP(ls-delete,"est,form,blank").

  IF li-delete GT 0 THEN DO:
    /*ll-mass-del = YES.*/

    FOR EACH b-eb
        WHERE b-eb.company EQ eb.company
          AND b-eb.est-no  EQ eb.est-no
          AND (li-delete   EQ 1                                 OR
               (li-delete  EQ 2 AND b-eb.form-no EQ eb.form-no) OR
               (li-delete  EQ 3 AND ROWID(b-eb) EQ ROWID(eb)))
        NO-LOCK:
      CREATE tt-eb.
      BUFFER-COPY b-eb TO tt-eb
      ASSIGN
       tt-eb.row-id = ROWID(b-eb).
    END.

    FOR EACH tt-eb:
      IF AVAIL eb AND eb.est-no EQ tt-eb.est-no            AND
         (eb.form-no EQ tt-eb.form-no OR li-delete LT 2)   AND
         (eb.blank-no EQ tt-eb.blank-no OR li-delete LT 3) THEN
        /*RUN dispatch ("delete-record").*/
          
       IF eb.form-no = 1 THEN DO:

       END.
    END.  
  END.

  /*ll-mass-del = NO.*/
  FOR EACH tt-eb:
         DELETE tt-eb.
  END.

 END.

 /***********************end mass delete*********************/

IF prmAction = "Update" THEN DO:
   
    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    FOR EACH  est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp   EXCLUSIVE-LOCK :
    IF AVAIL est-qty  THEN DO:
        ASSIGN
            est.est-date = prmEstDate
            varEqty = est-qty.eqty 
            lv-hld-eqty = est-qty.eqty  .
        
    END.
    
    FIND FIRST  ef WHERE ef.company = est-qty.company AND ef.est-no = est.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom  EXCLUSIVE-LOCK NO-ERROR.
     
    FIND  FIRST  eb WHERE eb.company = ef.company AND eb.est-no = est.est-no 
                AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank EXCLUSIVE-LOCK  NO-ERROR.
     
     IF  AVAIL ef  THEN DO:
         ASSIGN
             lv-hld-fcol = ef.f-col    
             lv-hld-fpas = ef.f-pass   
             lv-hld-fcot = ef.f-coat   
             lv-hld-fctp = ef.f-coat-p .
           
        ASSIGN 
         ef.lsh-wid     = prmLength
         ef.lsh-len     = prmWidth
         ef.f-col       = prmColor 
         ef.f-pass      = prmPasses
         ef.f-coat      = prmCoating
         ef.f-coat-p    = prmCoatPasses 
         ef.board       = prmBoard
         ef.cal         = prmCalliper .
     END.

     IF  AVAIL eb THEN DO:
       ASSIGN
         eb.i-col       = prmColor
         eb.i-pass      = prmPasses
         eb.i-coat      = prmCoating 
         eb.i-coat-p    = prmCoatPasses
         eb.part-no     = prmCustPart
         eb.stock-no    = prmFgItem
         eb.cust-no     = prmCust
         eb.ship-id     = prmShipTo
         eb.style       = prmStyle
         eb.part-dscr1  = prmItemName
         eb.flute       = prmFlute
         eb.test        = prmTest
         eb.cust-%      = prmQtySet 
         eb.bl-qty      = prmEstQty
         
         eb.procat      = prmCategory
         eb.pur-man     = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
         eb.tab-in      = IF prmTab = "In" THEN TRUE ELSE FALSE 
         eb.len         = prmLength
         eb.wid         = prmWidth
         eb.dep         = prmDepth
         eb.yld-qty     = prmQtySet
              .
         
          END.  /**if avail of eb**/
         
  
         /*IF prmFrom = 5  THEN DO:*/
              ASSIGN
                  ef.eqty   = prmEstQty
                  eb.eqty   = prmEstQty
                  est-qty.eqty   = prmEstQty .
        /* END.
         ELSE DO:
             ASSIGN 
                 ef.eqty      = varEqty
                  eb.eqty     = varEqty.
         END.*/

         FIND FIRST shipto  WHERE shipto.company EQ prmComp
          AND shipto.cust-no EQ prmCust
             AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN
             ASSIGN
             eb.ship-id      = shipto.ship-id
             eb.ship-name    = shipto.ship-name
             eb.ship-addr[1] = shipto.ship-addr[1]
             eb.ship-addr[2] = shipto.ship-addr[2]
             eb.ship-city    = shipto.ship-city
             eb.ship-state   = shipto.ship-state
             eb.ship-zip     = shipto.ship-zip. 

         IF prmFrom = 8 THEN DO:
             ASSIGN
                 eb.bl-qty = prmEstQty
                 eb.yld-qty = prmEstQty .
         END.

         find first sys-ctrl where sys-ctrl.company eq prmComp
                        and sys-ctrl.name    eq "CADFILE"
                        no-lock no-error.
              IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
                  create sys-ctrl.
                  assign sys-ctrl.company = prmComp
                      sys-ctrl.name    = "CADFILE"
                      sys-ctrl.descrip = "Dictate the location of the cad image to search."
                      sys-ctrl.char-fld = "R:\rcode\cadimage\".      
                  END.
                  lv-cad-path = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".


               find xeb where recid(xeb) = recid(eb) no-lock.
               find xest where recid(xest) = recid(est) no-lock.  
                
                find item where item.company = prmComp and
                         item.i-no = vTr-no no-lock no-error. 
                     if avail item then 
                         assign 
                         eb.tr-len = item.case-l
                         eb.tr-wid = item.case-w
                         eb.tr-dep = item.case-d.

                find style where style.company = prmComp and
                   style.style = prmStyle no-lock no-error.
               if avail style then 
                   assign
                   eb.adhesive = style.material[7] 
                   eb.gluelap = style.dim-gl
                   eb.k-len = style.dim-dkl
                   eb.k-wid = style.dim-dkw 
                   eb.lock = style.dim-fit
                   eb.tuck = style.dim-tk.    
               find style where style.company = eb.company and
                   style.style = eb.style
                   no-lock no-error.
                    
                  
               FIND FIRST cust WHERE cust.company EQ cocode
                   AND cust.cust-no EQ eb.cust-no NO-LOCK NO-ERROR.
               RELEASE bf-eb-comm.
                    IF est.est-type EQ 6 THEN
                        FIND FIRST bf-eb-comm
                        WHERE bf-eb-comm.company EQ eb.company
                        AND bf-eb-comm.est-no  EQ eb.est-no
                        AND bf-eb-comm.form-no EQ 0
                        AND bf-eb-comm.procat  NE ""
                        NO-LOCK NO-ERROR.

                    RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                    
                    FIND FIRST sman
                        WHERE sman.company EQ cocode
                        AND sman.sman    EQ eb.sman
                        NO-LOCK NO-ERROR.
                    IF AVAIL cust THEN
                        FIND FIRST custype WHERE custype.custype EQ cust.type NO-LOCK NO-ERROR.
                    IF AVAIL sman THEN
                        RUN custom/com-comm.p (cocode, sman.sman, custype.custype, IF AVAIL bf-eb-comm THEN bf-eb-comm.procat ELSE eb.procat, ld-markup,
                            IF AVAIL cust THEN cust.cust-no ELSE "",
                                OUTPUT op-comm).
                    ELSE DO:
                        IF AVAIL custype AND custype.custype NE "" THEN op-comm = custype.commrate.
                        ELSE DO:
                          FIND FIRST ce-ctrl
                              WHERE ce-ctrl.company EQ cocode
                              AND ce-ctrl.loc     EQ locode
                              NO-LOCK NO-ERROR.
                          IF AVAIL ce-ctrl THEN op-comm = ce-ctrl.comm-mrkup.
                       END.
                  END.

                  ASSIGN eb.comm = op-comm .
                    

               IF (CorType GE 5 AND CorType LE 6 AND eb.i-col + eb.i-coat EQ 0) OR
                   (CorType GE 7 AND CorType LE 8 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
                    {ce/delplate.i}
                END.

               IF CorType GE 7 AND CorType LE 8 THEN
                   IF lv-hld-fcol NE ef.f-col    OR
                      lv-hld-fpas NE ef.f-pass   OR
                      lv-hld-fcot NE ef.f-coat   OR
                      lv-hld-fctp NE ef.f-coat-p THEN DO:
                        FOR EACH est-prep  WHERE est-prep.company   EQ est.company
                            AND est-prep.est-no    EQ est.est-no
                            AND est-prep.s-num     EQ ef.form-no
                            AND (est-prep.mat-type EQ "F" OR est-prep.mat-type EQ "P"):
                                RUN sys/inc/flm-prep.p(RECID(est), est-prep.s-num, OUTPUT estprpqty ).
                                ASSIGN
                                    est-prep.qty  =  estprpqty .
                         END.
                END.
         RUN  qty-assign NO-ERROR.
         RUN  create-est-op NO-ERROR.
         RUN calc-pass NO-ERROR.
         RUN   calc-blank-size NO-ERROR.

         IF est.est-type NE 8 THEN
             FOR EACH bf-eb-ship
             WHERE bf-eb-ship.company EQ eb.company
             AND bf-eb-ship.est-no  EQ eb.est-no
             AND ROWID(bf-eb-ship)  NE ROWID(eb):
             ASSIGN
                 bf-eb-ship.cust-no      = eb.cust-no
                 bf-eb-ship.ship-id      = eb.ship-id
                 bf-eb-ship.ship-no      = eb.ship-no
                 bf-eb-ship.ship-name    = eb.ship-name
                 bf-eb-ship.ship-addr[1] = eb.ship-addr[1]
                 bf-eb-ship.ship-addr[2] = eb.ship-addr[2]
                 bf-eb-ship.ship-city    = eb.ship-city
                 bf-eb-ship.ship-state   = eb.ship-state
                 bf-eb-ship.ship-zip     = eb.ship-zip
                 bf-eb-ship.sman         = eb.sman
                 bf-eb-ship.comm         = eb.comm.
             END.

         
 /*   END. /*for each of eb or ef **/    */

         END. /*end of for each est-qty */

FIND  FIRST  xeb WHERE xeb.company = prmComp AND xeb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate)
                AND xeb.form-no = prmFrom AND xeb.blank-no = prmBlank EXCLUSIVE-LOCK  NO-ERROR.
                ASSIGN
                    xeb.test = prmTest .
   
 ASSIGN  prmAction = "Select" .
               
END. /* update the record*/

/***  add a form ***************************/
 IF prmAction = "FormEstimate" THEN  DO:
        
        FIND FIRST est WHERE est.company = prmComp AND est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) EXCLUSIVE-LOCK NO-ERROR. 
        FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc  NO-LOCK NO-ERROR.
        FIND FIRST est-qty  WHERE est-qty.company EQ prmComp
            AND est-qty.est-no  EQ est.est-no  EXCLUSIVE-LOCK NO-ERROR. 
        ASSIGN
            est.est-date      = prmEstDate 
           /* est.updated-id    = VAR*/
            varEqty = est-qty.eqty       .

        FIND LAST ef  WHERE ef.company EQ est-qty.company
            AND ef.est-no  EQ est-qty.est-no 
            AND ef.eqty    EQ est-qty.eqty  USE-INDEX est-qty NO-LOCK NO-ERROR.
        li = (IF AVAIL ef THEN ef.form-no ELSE 0) + 1 .
         
              CREATE ef. 
              ASSIGN
                  ef.est-type   = est.est-type
                  ef.company    = est.company
                  ef.loc        = est.loc
                  ef.e-num      = est.e-num
                  ef.est-no     = est.est-no
                  ef.eqty       = est-qty.eqty
                  ef.form-no    = li 
                  ef.cust-seq   = 1
                 /* ef.lsh-wid    = ce-ctrl.ls-length
                  ef.lsh-len    = ce-ctrl.ls-width*/
                  ef.cost-uom  = "MSF"
                  ef.eqty      = varEqty
                  ef.lsh-wid   = prmLength
                  ef.lsh-len   = prmWidth
                  ef.f-col  =     prmColor 
                  ef.f-pass  =  prmPasses
                  ef.f-coat  = prmCoating
                  ef.f-coat-p = prmCoatPasses 
                  ef.flute    = prmFlute 
                  ef.test     = prmTest  
                  .
              

        FIND FIRST bb  WHERE bb.company  EQ ef.company 
            AND bb.est-no   EQ ef.est-no
            AND bb.form-no  EQ 0
            AND bb.blank-no EQ 0  NO-LOCK NO-ERROR.
        ls-part-no = IF AVAIL bb THEN bb.part-no ELSE "".

        FIND LAST bb NO-LOCK  WHERE bb.company EQ ef.company
            AND bb.est-no  EQ ef.est-no
            AND bb.form-no NE 0
            USE-INDEX est-qty NO-ERROR.
        IF AVAIL bb THEN
            ASSIGN
            prev-cust = bb.cust-no
            prev-ship = bb.ship-id.

        FIND LAST bb NO-LOCK  WHERE bb.company EQ ef.company
            AND bb.est-no  EQ ef.est-no
            AND bb.form-no EQ ef.form-no
             USE-INDEX est-qty NO-ERROR.
        li = IF AVAIL bb THEN bb.blank-no ELSE 0.

            CREATE eb. 
            ASSIGN
                eb.est-type  = ef.est-type
                eb.company   = ef.company
                eb.loc       = ef.loc
                eb.e-num     = ef.e-num
                eb.est-no    = ef.est-no
                eb.est-int   = INT(ef.est-no)
                eb.eqty      = ef.eqty
                eb.form-no   = ef.form-no
                eb.blank-no  = li + 1
                eb.cust-seq  = 1
                eb.cas-no    = ce-ctrl.def-case
                eb.tr-no     = ce-ctrl.def-pal  
                /*eb.cust-no   = prev-cust
                eb.ship-id   = prev-ship*/
                eb.tr-cas    = 1               
                eb.yld-qty = prmQtySet
                
                
                eb.part-no     = prmCustPart
                eb.stock-no    = prmFgItem
                eb.cust-no     = prmCust
                eb.ship-id     = prmShipTo
                eb.style       = prmStyle
                eb.part-dscr1  = prmItemName
                eb.flute       = prmFlute
                eb.test        = prmTest
                eb.cust-%      = prmQtySet 
                eb.bl-qty      = prmEstQty 
                ef.board       = prmBoard
                ef.cal         = prmCalliper
                eb.procat      = prmCategory
                eb.i-col       = prmColor
                eb.i-pass      = prmPasses
                eb.i-coat      = prmCoating 
                eb.i-coat-p    =  prmCoatPasses
                eb.pur-man     = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                eb.tab-in      = IF prmTab = "In" THEN TRUE ELSE FALSE 
                eb.len         = prmLength
                eb.wid         = prmWidth
                eb.dep         = prmDepth
                eb.chg-method  = "P"
                 .

           IF ef.est-type EQ 6 AND ls-part-no NE "" THEN DO:
               li = 1.
               FOR EACH bb WHERE bb.company  EQ ef.company
                   AND bb.est-no   EQ ef.est-no
                   AND bb.form-no  NE 0
                   AND bb.blank-no NE 0
                   AND ROWID(bb)   NE ROWID(eb) NO-LOCK
                   BY bb.form-no BY bb.blank-no:
                   li = li + 1.
                   END.
                   DO WHILE TRUE:
                       IF NOT CAN-FIND(FIRST bb WHERE bb.company EQ ef.company
                                       AND bb.est-no  EQ ef.est-no
                                       AND bb.part-no EQ ls-part-no + "-" + STRING(li)
                                       AND ROWID(bb)  NE ROWID(eb))
                           THEN DO:
                           eb.part-no = ls-part-no + "-" + STRING(li).
                           LEAVE.
                           END.
                           li = li + 1.
                           END.
                           END.
          FIND FIRST ITEM WHERE item.company  EQ eb.company
              AND item.mat-type EQ "C"  /* Case/Bundle */
              AND item.i-no     EQ eb.cas-no
              NO-LOCK NO-ERROR.
          IF AVAIL item THEN DO:
              FIND FIRST e-item
                  WHERE e-item.company EQ item.company
                  AND e-item.loc     EQ item.loc
                  AND e-item.i-no    EQ item.i-no
                  NO-LOCK NO-ERROR.
              FIND FIRST itemfg
                  WHERE itemfg.company EQ eb.company
                  AND itemfg.i-no    EQ eb.stock-no
                  NO-LOCK NO-ERROR.
              IF AVAIL e-item THEN
                  ASSIGN
                  eb.cas-len = e-item.case-l
                  eb.cas-wid = e-item.case-w
                  eb.cas-dep = e-item.case-d
                  eb.cas-wt  = e-item.avg-w
                  eb.cas-pal = e-item.case-pall
                  eb.cas-cnt = IF AVAIL itemfg THEN itemfg.case-count ELSE e-item.box-case 
                      .
                  IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
                  IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
                  IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
                  IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
                  IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
                  IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
                      IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
                      END.  /* avail item */
                 IF eb.est-type EQ 6 THEN DO:
                     FIND FIRST bb
                         WHERE bb.company  EQ eb.company
                         AND bb.est-no   EQ eb.est-no 
                         AND bb.form-no  EQ 0
                         AND bb.blank-no EQ 0
                         NO-LOCK NO-ERROR.
                     IF AVAIL bb THEN eb.procat = bb.procat.
                     END.
                     find cust where cust.company = prmComp
                         and cust.cust-no = eb.cust-no no-lock no-error.
                     if avail cust and cust.pallet <> "" then vTr-no = cust.pallet . 
                     eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
                     eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.
                     IF AVAIL cust  THEN DO:
                         ASSIGN
                             eb.sman = cust.sman 
                             eb.carrier = cust.carrier
                             eb.dest-code = cust.del-zone .
                         FIND FIRST carrier WHERE carrier.carrier = cust.carrier AND carrier.company = prmComp NO-LOCK NO-ERROR.
                         IF AVAIL carrier THEN ASSIGN  eb.carr-dscr = carrier.dscr .
                     END.
                     /* get default values from rm table */
                        find item where item.company = eb.company and
                            item.i-no = eb.cas-no
                            no-lock no-error.
                        if avail item then
                             assign 
                              eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)        .  


                     find item where item.company = prmComp and
                         item.i-no = vTr-no no-lock no-error. 
                     if avail item then 
                         assign 
                         eb.tr-len = item.case-l
                         eb.tr-wid = item.case-w
                         eb.tr-dep = item.case-d.

                     find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                         eb.adhesive = style.material[7] 
                         eb.gluelap = style.dim-gl
                         eb.k-len = style.dim-dkl
                         eb.k-wid = style.dim-dkw 
                         eb.lock = style.dim-fit
                         eb.tuck = style.dim-tk.    
                     find style where style.company = eb.company and
                         style.style = eb.style  no-lock no-error.
                      lv-foam = IF AVAIL style AND style.TYPE = "F" THEN YES ELSE NO .
                      ef.xgrain = "N" .

                    
                    IF est.est-type EQ 6 THEN
                        FIND FIRST bf-eb-comm
                        WHERE bf-eb-comm.company EQ eb.company
                        AND bf-eb-comm.est-no  EQ eb.est-no
                        AND bf-eb-comm.form-no EQ 0
                        AND bf-eb-comm.procat  NE ""
                        NO-LOCK NO-ERROR.

                    RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

                    RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                                            (IF AVAIL bf-eb-comm THEN bf-eb-comm.procat ELSE eb.procat),
                                            ld-markup,
                                            OUTPUT outcomm).
                     ASSIGN
                         eb.comm = outcomm .

                    if style.material[4] ne "" then do:  /* adder*/ 
                        find first item  where item.company eq cocode
                            and item.i-no    eq style.material[4]
                            no-lock no-error.
                        if avail item then
                            do i = 1 to 6:  
                            if ef.adder[i] = "" then do:
                                assign ef.adder[i]     = item.i-no
                                    ef.adder[i + 6] = item.est-dscr
                                    ef.weight       = ef.weight + item.basis-w.
                                leave.       
                            end.           
                          end.
                       end.

                    if style.material[5] ne "" then do:  /* leaf label */
                        find first item  where item.company eq cocode
                            and item.i-no    eq style.material[5]
                            no-lock no-error.
                        if avail item then /*leaf-block:
                        for each ef where ef.company eq xest.company and
                        ef.est-no = xest.est-no,
                        first eb of ef no-lock:  */
                            do i = 1 to 2:
                            if ef.leaf[i] = "" then do:
                                assign ef.leaf-snum[i] = ef.form-no
                                    ef.leaf-bnum[i] = 1
                                    ef.leaf[i]      = item.i-no
                                    ef.leaf-dscr[i] = item.est-dscr
                                    ef.leaf-l[i] = eb.t-len
                                    ef.leaf-w[i] = eb.t-wid.
                                leave.
                              end.   
                           END.
                      END.

                       FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
                    IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
                        THEN eb.lin-in = eb.dep.
                    
                    RUN create-stack.

                      FIND FIRST shipto  WHERE shipto.company EQ prmComp
          AND shipto.cust-no EQ prmCust
             AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN
             ASSIGN
             eb.ship-id      = shipto.ship-id
             eb.ship-name    = shipto.ship-name
             eb.ship-addr[1] = shipto.ship-addr[1]
             eb.ship-addr[2] = shipto.ship-addr[2]
             eb.ship-city    = shipto.ship-city
             eb.ship-state   = shipto.ship-state
             eb.ship-zip     = shipto.ship-zip.

         IF CorType = 8 THEN DO:
             ASSIGN
                 eb.bl-qty      = prmEstQty
                 eb.yld-qty     = prmEstQty .
             END.
             RUN calc-pass NO-ERROR.
             RUN   calc-blank-size NO-ERROR.
             RUN  create-prep NO-ERROR.
             RUN  qty-assign NO-ERROR.
             RUN calc-layout-corr NO-ERROR.
             IF eb.pur-man THEN RUN create-e-itemfg-vend NO-ERROR.

             IF (CorType GE 5 AND CorType LE 6 AND eb.i-col + eb.i-coat EQ 0) OR
                  (CorType GE 7 AND CorType LE 8 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
                      {ce/delplate.i}
              END.

              IF CorType GE 7 AND CorType LE 8 THEN
                    IF lv-hld-fcol NE ef.f-col    OR
                       lv-hld-fpas NE ef.f-pass   OR
                       lv-hld-fcot NE ef.f-coat   OR 
                       lv-hld-fctp NE ef.f-coat-p THEN DO:
                          FOR EACH est-prep  WHERE est-prep.company   EQ est.company
                              AND est-prep.est-no    EQ est.est-no
                              AND est-prep.s-num     EQ ef.form-no
                              AND (est-prep.mat-type EQ "F" OR est-prep.mat-type EQ "P"):
                                    RUN sys/inc/flm-prep.p(RECID(est), est-prep.s-num, OUTPUT estprpqty ).
                                    ASSIGN
                                        est-prep.qty  =  estprpqty .
                            END.
              END.

         ASSIGN 
             prmFrom   = ef.form-no
             prmAction = "Select".

    END.  /*formest*/



    IF prmAction = "BlankSave" THEN DO:
        FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
        FOR EACH  est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
            EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK:

            FIND FIRST ce-ctrl where (ce-ctrl.company = prmComp and ce-ctrl.loc     = prmLoc) NO-LOCK NO-ERROR.

           FIND FIRST bb
            WHERE bb.company  EQ ef.company 
            AND bb.est-no   EQ ef.est-no
            AND bb.form-no  EQ 0
            AND bb.blank-no EQ 0
            NO-LOCK NO-ERROR.
            ls-part-no = IF AVAIL bb THEN bb.part-no ELSE "".

            FIND LAST bb NO-LOCK
                WHERE bb.company EQ ef.company
                AND bb.est-no  EQ ef.est-no
                AND bb.form-no NE 0
                USE-INDEX est-qty NO-ERROR.
            IF AVAIL bb THEN
                ASSIGN
                    prev-cust = bb.cust-no
                    prev-ship = bb.ship-id.

            FIND LAST bb NO-LOCK
                WHERE bb.company EQ ef.company
                AND bb.est-no  EQ ef.est-no
                AND bb.form-no EQ ef.form-no
                USE-INDEX est-qty NO-ERROR.
                li = IF AVAIL bb THEN bb.blank-no ELSE 0.


            CREATE eb. 
            ASSIGN
                eb.est-type  = ef.est-type
                eb.company   = ef.company
                eb.loc       = ef.loc
                eb.e-num     = ef.e-num
                eb.est-no    = ef.est-no
                eb.est-int   = INT(ef.est-no)
                eb.eqty      = ef.eqty
                eb.form-no   = ef.form-no
                eb.blank-no  = li + 1
                eb.cust-seq  = 1
                eb.cas-no    = ce-ctrl.def-case
                eb.tr-no     = ce-ctrl.def-pal
                eb.cust-no   = prmCust    
                eb.ship-id   = prmShipTo  
                eb.tr-cas    = 1
                eb.i-pass    = 0
                eb.yld-qty   = 1
                eb.tab-in    = YES
                eb.len       = 0
                eb.wid       = 0
                eb.dep       = 0
                eb.procat    = prmCategory
                eb.flute     = prmFlute
                eb.test      = prmTest 
                
                eb.stock-no     = prmFgItem
                eb.style        = prmStyle
                eb.part-dscr1   = prmItemName 
                eb.yld-qty      = prmQtySet
                eb.bl-qty       = prmEstQty                
                eb.i-col        = prmColor                                       
                eb.i-pass       = prmPasses                                     
                eb.i-coat       = prmCoating                                    
                eb.i-coat-p     = prmCoatPasses                              
                eb.pur-man      = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                eb.tab-in       = IF prmTab = "In" THEN TRUE ELSE FALSE       
                eb.len          = prmLength                                      
                eb.wid          = prmWidth                                                
                eb.dep          = prmDepth 
                eb.cust-%       = INT(ef.est-type EQ 2)
                eb.yrprice      = prev-yrprice
                eb.chg-method   = "P"
                eb.part-no      = prmCustPart
                .     
                     
        IF ef.est-type EQ 6 AND ls-part-no NE "" THEN DO:
            li = 1.
            FOR EACH bb
                WHERE bb.company  EQ ef.company
                AND bb.est-no   EQ ef.est-no
                AND bb.form-no  NE 0
                AND bb.blank-no NE 0
                AND ROWID(bb)   NE ROWID(eb)
                NO-LOCK
            BY bb.form-no BY bb.blank-no:
                li = li + 1.
            END.
            DO WHILE TRUE:
            IF NOT CAN-FIND(FIRST bb
                    WHERE bb.company EQ ef.company
                      AND bb.est-no  EQ ef.est-no
                      AND bb.part-no EQ ls-part-no + "-" + STRING(li)
                      AND ROWID(bb)  NE ROWID(eb))
            THEN DO:
                eb.part-no = prmCustPart.
                LEAVE.
            END.
            li = li + 1.
            END. 
        END.



FIND FIRST item
    WHERE item.company  EQ eb.company
      AND item.mat-type EQ "C"  /* Case/Bundle */
      AND item.i-no     EQ eb.cas-no
    NO-LOCK NO-ERROR.
IF AVAIL item THEN DO:
  FIND FIRST e-item
      WHERE e-item.company EQ item.company
        AND e-item.loc     EQ item.loc
        AND e-item.i-no    EQ item.i-no
      NO-LOCK NO-ERROR.
  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.
  IF AVAIL e-item THEN
    ASSIGN
     eb.cas-len = e-item.case-l
     eb.cas-wid = e-item.case-w
     eb.cas-dep = e-item.case-d
     eb.cas-wt  = e-item.avg-w
     eb.cas-pal = e-item.case-pall
     eb.cas-cnt = IF AVAIL itemfg THEN itemfg.case-count ELSE e-item.box-case
              .
  IF eb.cas-len EQ 0 THEN eb.cas-len = item.case-l.
  IF eb.cas-wid EQ 0 THEN eb.cas-wid = item.case-w.
  IF eb.cas-dep EQ 0 THEN eb.cas-dep = item.case-d.
  IF eb.cas-wt  EQ 0 THEN eb.cas-wt  = item.avg-w.
  IF eb.cas-pal EQ 0 THEN eb.cas-pal = item.case-pall.
  IF eb.cas-cnt EQ 0 THEN eb.cas-cnt =
            IF AVAIL itemfg THEN itemfg.case-count ELSE item.box-case.
END.  /* avail item */
    
IF eb.est-type EQ 6 THEN DO:
  FIND FIRST bb
      WHERE bb.company  EQ eb.company
        AND bb.est-no   EQ eb.est-no 
        AND bb.form-no  EQ 0
        AND bb.blank-no EQ 0
      NO-LOCK NO-ERROR.
  IF AVAIL bb THEN eb.procat = bb.procat.
END.
find cust where cust.company = prmComp
                         and cust.cust-no = eb.cust-no no-lock no-error.
                     if avail cust and cust.pallet <> "" then vTr-no = cust.pallet . 
                     eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
                     eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no. 
                     IF AVAIL cust  THEN DO:
                         ASSIGN
                             eb.sman = cust.sman 
                             eb.carrier = cust.carrier
                             eb.dest-code = cust.del-zone .
                         FIND FIRST carrier WHERE carrier.carrier = cust.carrier AND carrier.company = prmComp NO-LOCK NO-ERROR.
                         IF AVAIL carrier THEN ASSIGN  eb.carr-dscr = carrier.dscr .
                     END.
                        /* get default values from rm table */
                        find item where item.company = eb.company and
                            item.i-no = eb.cas-no
                            no-lock no-error.
                        if avail item then 
                            assign
                              eb.cas-cnt = (item.box-case)
                              eb.cas-len = (item.case-l)
                              eb.cas-wid = (item.case-w)
                              eb.cas-dep = (item.case-d)
                              eb.cas-pal = (item.case-pall)
                              eb.cas-wt = (item.avg-w)        .  


                     find item where item.company = prmComp and
                         item.i-no = vTr-no no-lock no-error. 
                     if avail item then 
                         assign 
                         eb.tr-len = item.case-l
                         eb.tr-wid = item.case-w
                         eb.tr-dep = item.case-d.

                     find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                         eb.adhesive = style.material[7] 
                         eb.gluelap = style.dim-gl
                         eb.k-len = style.dim-dkl
                         eb.k-wid = style.dim-dkw 
                         eb.lock = style.dim-fit
                         eb.tuck = style.dim-tk.    
                     find style where style.company = eb.company and
                         style.style = eb.style  no-lock no-error.

                    
                    IF est.est-type EQ 6 THEN
                        FIND FIRST bf-eb-comm
                        WHERE bf-eb-comm.company EQ eb.company
                        AND bf-eb-comm.est-no  EQ eb.est-no
                        AND bf-eb-comm.form-no EQ 0
                        AND bf-eb-comm.procat  NE ""
                        NO-LOCK NO-ERROR.

                    RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).

                    RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman,
                                            (IF AVAIL bf-eb-comm THEN bf-eb-comm.procat ELSE eb.procat),
                                            ld-markup,
                                            OUTPUT outcomm).
                     ASSIGN
                         eb.comm = outcomm .

                       FIND FIRST ITEM WHERE ITEM.company = eb.company 
                          AND ITEM.i-no = eb.adhesive NO-LOCK NO-ERROR.
                    IF AVAIL ITEM AND index("G,S,T",ITEM.mat-type) > 0 AND ITEM.i-no <> "No Joint"
                        THEN eb.lin-in = eb.dep.
                    
                    RUN create-stack.
              

                      FIND FIRST shipto  WHERE shipto.company EQ prmComp
          AND shipto.cust-no EQ prmCust
             AND shipto.ship-id EQ prmShipTo NO-LOCK NO-ERROR.
         IF AVAIL shipto THEN
             ASSIGN
             eb.ship-id      = shipto.ship-id
             eb.ship-name    = shipto.ship-name
             eb.ship-addr[1] = shipto.ship-addr[1]
             eb.ship-addr[2] = shipto.ship-addr[2]
             eb.ship-city    = shipto.ship-city
             eb.ship-state   = shipto.ship-state
             eb.ship-zip     = shipto.ship-zip.

         IF CorType = 8 THEN DO:
             ASSIGN
                 eb.bl-qty      = prmEstQty
                 eb.yld-qty     = prmEstQty .
             END.
             RUN calc-pass NO-ERROR.
             RUN   calc-blank-size NO-ERROR.
             RUN  qty-assign NO-ERROR.
             IF eb.pur-man THEN RUN create-e-itemfg-vend NO-ERROR.

         ASSIGN 
             prmBlank = eb.blank-no
             prmFrom   = ef.form-no
             prmAction = "Select".
 END. 


    END.



    IF prmAction = "ChangeTandem"  THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
      FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  EXCLUSIVE-LOCK ,
       EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                    AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK,
        EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                  AND eb.form-no = ef.form-no  EXCLUSIVE-LOCK:
          IF est.ord-no EQ 0 THEN DO:
          IF AVAIL est  THEN  DO:
              ASSIGN
                  est.est-type = 8
                  ef.est-type = 8
                  eb.est-type = 8 .
          END.
          END.
          ELSE DO:
              ASSIGN
                  cError = "Type can not change...".
              RETURN.
          END.
          ASSIGN  prmAction = "Select" .
      END.
  END.

  IF prmAction = "ChangeSet"  THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
      FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  EXCLUSIVE-LOCK ,
       EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                    AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK,
        EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                  AND eb.form-no = ef.form-no  EXCLUSIVE-LOCK:
          IF est.ord-no EQ 0 THEN DO:
          IF AVAIL est  THEN  DO:
              ASSIGN
                  est.est-type = 6
                  ef.est-type = 6
                  eb.est-type = 6 .
          END.
          END.
          ELSE DO:
              ASSIGN
                  cError = "Type can not change...".
              RETURN.
          END.
          ASSIGN  prmAction = "Select" .
      END.
  END.



 /*****************select list**********************************************/

IF prmAction = "ListEst" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  NO-LOCK ,
     EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no NE 0  NO-LOCK,
      EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                AND eb.form-no = ef.form-no  NO-LOCK BY eb.form-no BY eb.blank-no :
          
         vQty = display-combo-qty ().           
        CREATE ttCorrugateEstimate.
        ASSIGN 
            ttCorrugateEstimate.vEst           = est.est-no
            ttCorrugateEstimate.vCust          = eb.cust-no
            ttCorrugateEstimate.vCustPart      = eb.part-no
            ttCorrugateEstimate.vShipTo        = eb.ship-id
            ttCorrugateEstimate.vItemName      = eb.part-dscr1
            ttCorrugateEstimate.vFgItem        = eb.stock-no
            ttCorrugateEstimate.vEstQty        = vQty
            ttCorrugateEstimate.vStyle         = eb.style
            ttCorrugateEstimate.vFlute         = eb.flute
            ttCorrugateEstimate.vTest          = eb.test
            ttCorrugateEstimate.vBoard         = ef.board
            ttCorrugateEstimate.vCaliper       = ef.cal
            ttCorrugateEstimate.vCategory      = eb.procat
            ttCorrugateEstimate.vLenght        = /*display-cw-dim(yes,eb.len)*/  eb.len
            ttCorrugateEstimate.vWidth         = /*display-cw-dim(yes,eb.wid)*/  eb.wid
            ttCorrugateEstimate.vDepth         = /*display-cw-dim(yes,eb.dep)*/  eb.dep
            ttCorrugateEstimate.vForm          = eb.form-no
            ttCorrugateEstimate.vBlank         = eb.blank-no
            ttCorrugateEstimate.vTab           =  IF eb.tab-in  = TRUE THEN "In" ELSE "Out"
            ttCorrugateEstimate.vColor         = eb.i-col
            ttCorrugateEstimate.vPasses        = eb.i-pass
            ttCorrugateEstimate.vCoating       = eb.i-coat
            ttCorrugateEstimate.vCoatPasses    = eb.i-coat-p
            ttCorrugateEstimate.vQtySet        = eb.yld-qty
            ttCorrugateEstimate.vInkFrom       = ef.f-col
            ttCorrugateEstimate.vPassesFrom    = ef.f-pass
            ttCorrugateEstimate.vCoatingFrom   = ef.f-coat
            ttCorrugateEstimate.vCoatPassesFrom = ef.f-coat-p
            ttCorrugateEstimate.vPurchManuf    = IF eb.pur-man = TRUE THEN "P" ELSE "M"
            ttCorrugateEstimate.vEstDate       = est.est-date
            ttCorrugateEstimate.vEstType       = eb.est-type

            .
    END.
END.  /* end of select*/

/*******************************end select list**********************************/



    IF prmAction = "Select" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  NO-LOCK ,
     EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom NO-LOCK,
      EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank NO-LOCK:
    
         vQty = display-combo-qty ().           
        CREATE ttCorrugateEstimate.
        ASSIGN 
            ttCorrugateEstimate.vEst           = est.est-no
            ttCorrugateEstimate.vCust          = eb.cust-no
            ttCorrugateEstimate.vCustPart      = eb.part-no
            ttCorrugateEstimate.vShipTo        = eb.ship-id
            ttCorrugateEstimate.vItemName      = eb.part-dscr1
            ttCorrugateEstimate.vFgItem        = eb.stock-no
            ttCorrugateEstimate.vEstQty        = vQty
            ttCorrugateEstimate.vStyle         = eb.style
            ttCorrugateEstimate.vFlute         = eb.flute
            ttCorrugateEstimate.vTest          = eb.test
            ttCorrugateEstimate.vBoard         = ef.board
            ttCorrugateEstimate.vCaliper       = ef.cal
            ttCorrugateEstimate.vCategory      = eb.procat
            ttCorrugateEstimate.vLenght        = /*display-cw-dim(yes,eb.len)*/  eb.len
            ttCorrugateEstimate.vWidth         = /*display-cw-dim(yes,eb.wid)*/  eb.wid
            ttCorrugateEstimate.vDepth         = /*display-cw-dim(yes,eb.dep)*/  eb.dep
            ttCorrugateEstimate.vForm          = eb.form-no
            ttCorrugateEstimate.vBlank         = eb.blank-no
            ttCorrugateEstimate.vTab           =  IF eb.tab-in  = TRUE THEN "In" ELSE "Out"
            ttCorrugateEstimate.vColor         = eb.i-col
            ttCorrugateEstimate.vPasses        = eb.i-pass
            ttCorrugateEstimate.vCoating       = eb.i-coat
            ttCorrugateEstimate.vCoatPasses    = eb.i-coat-p
            ttCorrugateEstimate.vQtySet        = eb.yld-qty
            ttCorrugateEstimate.vInkFrom       = ef.f-col
            ttCorrugateEstimate.vPassesFrom    = ef.f-pass
            ttCorrugateEstimate.vCoatingFrom   = ef.f-coat
            ttCorrugateEstimate.vCoatPassesFrom = ef.f-coat-p
            ttCorrugateEstimate.vPurchManuf    = IF eb.pur-man = TRUE THEN "P" ELSE "M"
            ttCorrugateEstimate.vEstDate       = est.est-date
            ttCorrugateEstimate.vEstType       = eb.est-type
            ttCorrugateEstimate.vReckey       = est.rec_key 
            ttCorrugateEstimate.vTlen       = eb.t-len  /*round(trunc((eb.t-len),0) + (((eb.t-len) - trunc((eb.t-len),0)) / K_FRAC),2)*/
            ttCorrugateEstimate.vTwid       = eb.t-wid /*round(trunc((eb.t-wid),0) + (((eb.t-wid) - trunc((eb.t-wid),0)) / K_FRAC),2)*/

             ttCorrugateEstimate.vQtyExtent[1]       = est-qty.qty[1]
             ttCorrugateEstimate.vQtyExtent[2]       = est-qty.qty[2]
           ttCorrugateEstimate.vQtyExtent[3]       = est-qty.qty[3]
           ttCorrugateEstimate.vQtyExtent[4]       = est-qty.qty[4]
           ttCorrugateEstimate.vQtyExtent[5]       = est-qty.qty[5]
           ttCorrugateEstimate.vQtyExtent[6]       = est-qty.qty[6]
           ttCorrugateEstimate.vQtyExtent[7]       = est-qty.qty[7]
           ttCorrugateEstimate.vQtyExtent[8]       = est-qty.qty[8]
           ttCorrugateEstimate.vQtyExtent[9]       = est-qty.qty[9]
           ttCorrugateEstimate.vQtyExtent[10]       = est-qty.qty[10]
           ttCorrugateEstimate.vQtyExtent[11]       = est-qty.qty[11]
           ttCorrugateEstimate.vQtyExtent[12]       = est-qty.qty[12]
           ttCorrugateEstimate.vQtyExtent[13]       = est-qty.qty[13]
           ttCorrugateEstimate.vQtyExtent[14]       = est-qty.qty[14]
           ttCorrugateEstimate.vQtyExtent[15]       = est-qty.qty[15]
           ttCorrugateEstimate.vQtyExtent[16]       = est-qty.qty[16]
           ttCorrugateEstimate.vQtyExtent[17]       = est-qty.qty[17]
           ttCorrugateEstimate.vQtyExtent[18]       = est-qty.qty[18]
           ttCorrugateEstimate.vQtyExtent[19]       = est-qty.qty[19]
           ttCorrugateEstimate.vQtyExtent[20]       = est-qty.qty[20] .
           
         
           v-count2 = 1.
           v-count = 21.
          DO WHILE v-count > 20 AND v-count < 41 :
              ASSIGN
              ttCorrugateEstimate.vRelQtyExtent[v-count2]   = est-qty.qty[v-count].
              v-count = v-count + 1.
              v-count2 = v-count2 + 1.
          END.  
          
    END.
END.  /* end of select*/




/**********************************add blank form*********************/

IF prmAction = "viewblank" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH  est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
    EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom NO-LOCK:
       
        
FIND FIRST bb
    WHERE bb.company  EQ ef.company 
      AND bb.est-no   EQ ef.est-no
      AND bb.form-no  EQ 0
      AND bb.blank-no EQ 0
    NO-LOCK NO-ERROR.
ls-part-no = IF AVAIL bb THEN bb.part-no ELSE "".           

FIND LAST bb NO-LOCK
    WHERE bb.company EQ ef.company
      AND bb.est-no  EQ ef.est-no
      AND bb.form-no NE 0
    USE-INDEX est-qty NO-ERROR.
IF AVAIL bb THEN
  ASSIGN
   prev-cust = bb.cust-no
   prev-ship = bb.ship-id.

FIND LAST bb NO-LOCK
    WHERE bb.company EQ ef.company
      AND bb.est-no  EQ ef.est-no
      AND bb.form-no EQ ef.form-no
    USE-INDEX est-qty NO-ERROR.
li = IF AVAIL bb THEN bb.blank-no ELSE 0.


CREATE ttCorrugateEstimate.
ASSIGN 
    ttCorrugateEstimate.vEst    = ef.est-no
    ttCorrugateEstimate.vCust   = prev-cust
    ttCorrugateEstimate.vShipTo = prev-ship
    ttCorrugateEstimate.vFlute = ef.flute
    ttCorrugateEstimate.vTest = ef.test
    ttCorrugateEstimate.vBoard = ef.board
    ttCorrugateEstimate.vCaliper = ef.cal
    ttCorrugateEstimate.vCategory = IF AVAIL bb THEN bb.procat ELSE ""
    ttCorrugateEstimate.vForm = ef.form-no
    ttCorrugateEstimate.vBlank = li + 1        
    ttCorrugateEstimate.vTab = "In"
    ttCorrugateEstimate.vInkFrom = ef.f-col
    ttCorrugateEstimate.vPassesFrom = ef.f-pass
    ttCorrugateEstimate.vCoatingFrom = ef.f-coat
    ttCorrugateEstimate.vCoatPassesFrom = ef.f-coat-p
    ttCorrugateEstimate.vReckey  = est.rec_key
    ttCorrugateEstimate.vEstDate  = est.est-date
    ttCorrugateEstimate.vEstType  = est.est-type
    /*ttCorrugateEstimate.vEstQty  = ef.eqty   */
    ttCorrugateEstimate.vQtySet  = 1
    ttCorrugateEstimate.vLenght  = 0
    ttCorrugateEstimate.vWidth  = 0
    ttCorrugateEstimate.vDepth  = 0
    ttCorrugateEstimate.vPurchManuf    =  "M" .

IF ef.est-type EQ 6 AND ls-part-no NE "" THEN DO:
  li = 1.
  FOR EACH bb
      WHERE bb.company  EQ ef.company
        AND bb.est-no   EQ ef.est-no
        AND bb.form-no  NE 0
        AND bb.blank-no NE 0
        AND ROWID(bb)   NE ROWID(eb)
      NO-LOCK
      BY bb.form-no BY bb.blank-no:
    li = li + 1.
  END.
  DO WHILE TRUE:
    IF NOT CAN-FIND(FIRST bb
                    WHERE bb.company EQ ef.company
                      AND bb.est-no  EQ ef.est-no
                      AND bb.part-no EQ ls-part-no + "-" + STRING(li)
                      AND ROWID(bb)  NE ROWID(eb))
    THEN DO:
      /*eb.part-no = ls-part-no + "-" + STRING(li).*/
        ttCorrugateEstimate.vCustPart = ls-part-no + "-" + STRING(li).
      LEAVE.
    END.
    li = li + 1.
  END.
END.

IF eb.est-type EQ 6 THEN DO:
  FIND FIRST bb
      WHERE bb.company  EQ eb.company
        AND bb.est-no   EQ eb.est-no 
        AND bb.form-no  EQ 0
        AND bb.blank-no EQ 0
      NO-LOCK NO-ERROR.
  IF AVAIL bb THEN ttCorrugateEstimate.vCategory = bb.procat.
END.
      
    END.

END.


/**********************************************************************/



/***************** procedure***********************************/
PROCEDURE create-est-op:

    find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).
   
     IF ceroute-log AND xest.est-type GE 5 AND xest.est-type LE 6 THEN DO:
         
     FOR EACH est-op
         WHERE est-op.company EQ xest.company
           AND est-op.est-no  EQ xest.est-no
           AND est-op.qty     EQ lv-hld-eqty:
       est-op.qty = est-qty.eqty.
     END.

     FOR EACH est-op
         WHERE est-op.company EQ xest.company
           AND est-op.est-no  EQ xest.est-no
           AND est-op.qty     EQ est-qty.eqty
           AND est-op.line    GE 500:
       DELETE est-op.
     END.

     IF CAN-FIND(FIRST est-op WHERE est-op.company EQ xest.company
                                AND est-op.est-no  EQ xest.est-no
                                AND est-op.qty     EQ est-qty.eqty
                                AND est-op.s-num   EQ xef.form-no) THEN do:
         
     FOR EACH est-op
         WHERE est-op.company EQ xest.company
           AND est-op.est-no  EQ xest.est-no
           AND est-op.qty     EQ lv-hld-eqty
           AND est-op.s-num   EQ xef.form-no
         NO-LOCK:
     END.
     END.
  
     ELSE DO:
       /* Protect existing est-op records */
       FOR EACH tt-est-op:
         DELETE tt-est-op.
       END.

       FOR EACH est-op
           WHERE est-op.company EQ xest.company
             AND est-op.est-no  EQ xest.est-no
             AND est-op.qty     EQ est-qty.eqty:
         CREATE tt-est-op.
         BUFFER-COPY est-op TO tt-est-op.
         DELETE est-op.
       END.
                                 
       xx = DEC(xef.form-no).

       RUN cec/mach-seq.p (xef.form-no, est-qty.eqty, NO).

       FOR EACH est-op
           WHERE est-op.company EQ xest.company
             AND est-op.est-no  EQ xest.est-no
             AND est-op.qty     EQ est-qty.eqty
             AND est-op.s-num   NE INT(xx):
         DELETE est-op.
       END.

       FOR EACH tt-est-op:
         CREATE est-op.
         BUFFER-COPY tt-est-op TO est-op.
         DELETE tt-est-op.
       END.

       li = 0.
       FOR EACH est-op
           WHERE est-op.company EQ est.company
             AND est-op.est-no  EQ est.est-no
             AND est-op.line    LT 500
           BY est-op.qty
           BY est-op.s-num
           BY est-op.b-num
           BY est-op.d-seq
           BY est-op.op-pass
           BY est-op.rec_key:
      
         /*{sys/inc/machpos.w est-op SHARE}  */
           find first reftable
               where reftable.reftable eq "MachinePosition"
               and reftable.company  eq string(est-op.company,"x(10)") +
               string(est-op.est-no,"x(10)")
               and reftable.loc      eq string(est-op.line - if est-op.line gt 500 then 500 else 0,"9999999999")
               and reftable.code     eq ""
               and reftable.code2    eq ""
               SHARE-LOCK no-error.
         ASSIGN
          li          = li + 1
          est-op.line = li.
     
         IF AVAIL reftable THEN reftable.loc = STRING(est-op.line,"9999999999"). 
       END.
     END.
   END.
     ELSE DO:
         ASSIGN
               eb.num-wid = 1
               eb.num-len = 1
               eb.num-up = 1.

            run cec/calc-dim.p.
     END.


  FOR EACH tt-eb-set:
    DELETE tt-eb-set.
  END.

 DEF VAR v-log AS LOG NO-UNDO.
  IF xest.est-type EQ 8 THEN do:
    IF AVAIL xest THEN
         v-log = xest.est-type EQ 8.
        IF prmAction = "Add" THEN do:
            RUN cec/mach-seq.p (xeb.form-no, 0, v-log).
        END.
 END.

END PROCEDURE.


PROCEDURE create-prep :
   def var i as int no-undo.
   DEFINE VAR estprpqty AS INT NO-UNDO.
  find last est-prep where est-prep.company = prmComp and
                           est-prep.est-no = est.est-no and
                           est-prep.eqty = est-qty.eqty 
                           no-lock no-error.
  i = if avail est-prep then est-prep.line + 1 else 1.
                             
  for each prep where prep.company = prmComp and prep.dfault eq yes no-lock:
      create est-prep.
      assign est-prep.e-num  = est.e-num
             est-prep.company = est.company
             est-prep.est-no = est.est-no
             est-prep.eqty = est-qty.eqty
             est-prep.line   = i
             est-prep.s-num  = eb.form-no
             est-prep.b-num  = 0 /*1 */
             est-prep.qty    = if prep.mat-type eq "r" and avail ef then ef.die-in
                               else if prep.mat-type eq "b" and  avail ef then ef.nsh-wid * ef.nsh-len /* ef.adh-sqin is 0 in Corrware - die inch */
                               else 1  /* mat-type eq "m" */
            est-prep.code   = prep.code
            est-prep.dscr   = prep.dscr
            est-prep.cost   = prep.cost
            est-prep.ml     = prep.ml
            est-prep.simon  = prep.simon
            est-prep.mkup   = prep.mkup
            est-prep.amtz   = prep.amtz
            est-prep.mat-type = prep.mat-type.
            if lookup(est-prep.mat-type, "p,f") gt 0 THEN DO:
               run sys/inc/flm-prep.p(recid(est), est-prep.s-num, OUTPUT estprpqty ).

               MESSAGE "kjdhfgkhdflkgh  " estprpqty . 
             ASSIGN 
                 est-prep.qty = estprpqty .
             END.
            i = i + 1.
/*message "prep crt: " i est-prep.qty prep.mat-type view-as alert-box.            
*/
  end.

END PROCEDURE.


PROCEDURE calc-pass :

      def var k as int no-undo.
      def var counter as int no-undo.
      def var i as int no-undo.
      def var j as int no-undo.
      def var save_id as recid no-undo.
      def var save_id2 as recid no-undo.
      def buffer alt-item for item .
      def var choice as log no-undo.
      def buffer bf-eb for eb.
      
      find first style where style.company = eb.company and
                 style.style = eb.style no-lock no-error.
      if avail style then do:
         if k = 0 then k = integer(style.material[3]).
         find first item where item.company = eb.company and
                    item.i-no = style.material[2] no-lock no-error.
         if avail item then k = integer(style.material[3]).
         find first alt-item where alt-item.company  = eb.company  and
                                   alt-item.mat-type = "V"     and
                                   alt-item.i-no     = style.material[6]
                                   no-lock no-error.
      end.
      if not avail item or not avail alt-item or (k = 0) then do:
         find first ce-ctrl where ce-ctrl.company = eb.company and
                                  ce-ctrl.loc = eb.loc
                                   no-lock no-error.
         if k = 0 then k = ce-ctrl.def-inkcov.
         if not avail item then do:
            find first item where item.company = eb.company and
                       item.i-no = ce-ctrl.def-ink no-lock no-error.
         end.
         if not avail alt-item then
            find first alt-item where alt-item.company  = eb.company  and
                                      alt-item.mat-type = "V"     and
                                      alt-item.i-no     = ce-ctrl.def-coat
                                      no-lock no-error.
      end.
 
      save_id = recid(item). save_id2 = recid(alt-item).
      j = (integer(prmColor)
          + integer(prmCoating)  ) 
          .
      {sys/inc/roundup.i j}
      counter = 1.
      choice = true.

/*    do i = 1 to 10:
       if eb.i-code[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
  
      find bf-eb of eb exclusive-lock.        
      if choice then do i = 1 to 10:
         if i le integer(eb.i-col) then do :
              find item where recid(item) = save_id no-lock no-error.
              assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = item.i-no
                     bf-eb.i-dscr[i] = item.i-name
                     bf-eb.i-%[i]    = k.
         end.
         else if (i > integer(eb.i-col)) and
                 (i <= (integer(eb.i-col) + 
                       integer(eb.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-eb.i-ps[i]   = counter
                     bf-eb.i-code[i] = if avail alt-item then alt-item.i-no else ""
                     bf-eb.i-dscr[i] = if avail alt-item then alt-item.i-name else ""
                     bf-eb.i-%[i]    = 100.
         end.
         else if (i >  (eb.i-col + eb.i-coat) )
         then do:
            assign bf-eb.i-ps[i]   = 0  
                     bf-eb.i-code[i] = ""
                     bf-eb.i-dscr[i] = "" 
                     bf-eb.i-%[i]    = 0.  
        
         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (eb.i-pass) then counter = eb.i-pass.         
      end. 
   
END PROCEDURE.


PROCEDURE box-design:

/*    IF cestyle-log  AND   lv-hld-style NE eb.style    THEN DO:
 
 find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CADFILE"
                        no-lock no-error.
    IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
      create sys-ctrl.
      assign sys-ctrl.company = cocode
             sys-ctrl.name    = "CADFILE"
             sys-ctrl.descrip = "Dictate the location of the cad image to search."
             sys-ctrl.char-fld = "R:\rcode\cadimage\".      

    END.
    lv-cad-path = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
    IF lv-cad-path <> "" THEN DO:
      IF lv-cad-ext = "" THEN lv-cad-ext = ".jpg".
      IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) = ? THEN lv-cad-ext = "".
    END.
    IF SEARCH(lv-cad-path + eb.cad-no + lv-cad-ext) <> ? THEN DO:
        FIND first box-design-hdr where box-design-hdr.design-no = 0 and
                                     box-design-hdr.company = eb.company 
                                 and box-design-hdr.est-no = eb.est-no     
                                 and box-design-hdr.form-no   eq eb.form-no
                                 and box-design-hdr.blank-no  eq eb.blank-no NO-ERROR.

        IF AVAIL box-design-hdr THEN 
           ASSIGN box-design-hdr.box-image = lv-cad-path + eb.cad-no + lv-cad-ext. /*".jpg"*/.
     END.
  END.*/

 END PROCEDURE.



PROCEDURE calc-blank-size :

   def buffer bf-eb for eb .
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var v-score-char like v-lscore-c extent 100.

   find first sys-ctrl  where sys-ctrl.company eq prmComp
                           and sys-ctrl.name    eq "PANELS"
        no-lock no-error.
   if not avail sys-ctrl then do:
      create sys-ctrl.
      assign  sys-ctrl.company = cocode
              sys-ctrl.name    = "PANELS"
              sys-ctrl.descrip = "CE Lock=Yes Panel Size Popup when Overriding W&L?"
              sys-ctrl.log-fld = yes.
      MESSAGE sys-ctrl.descrip
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE sys-ctrl.log-fld.
   end.
   lv-panels = sys-ctrl.log-fld.

   find xest where recid(xest) = recid(est) no-lock.
   find xef where recid(xef) = recid(ef) no-lock.
   find xeb where recid(xeb) = recid(eb) no-lock.
   
   find style where style.company = eb.company and
                    style.style = eb.style
                    no-lock no-error.
   
   if avail style then do:
       if style.type <> "F" then run calc-blank-size2. 

      {cec/msfcalc.i}
      run est/u2kinc1c.p (recid(eb)).
      run est/u2kinc2c.p (recid(eb)).
       find first formule no-lock no-error.
      find bf-eb of eb exclusive-lock. 
      assign bf-eb.t-wid = (formule.formule[1])
          bf-eb.t-len = (formule.formule[2])
          bf-eb.t-sqin = (formule.formule[7] * formule.formule[8])
          bf-eb.k-wid-array2 = 0
          bf-eb.k-len-array2 = 0.
      
      if not lv-panels or style.type = "F" then 
         assign bf-eb.k-wid-array2[1] = bf-eb.t-wid
                bf-eb.k-len-array2[1] = bf-eb.t-len
                .
      else do:
         run cec/descalc.p (recid(xest),recid(xeb)).

         DO i = 1 TO EXTENT(xeb.k-wid-scr-type2):
           ASSIGN
            xeb.k-wid-scr-type2[i] = lv-k-wid-scr-type[i]
            xeb.k-len-scr-type2[i] = lv-k-len-scr-type[i].
         END.

         if v-lscore-c begins "No" THEN
            assign  xeb.k-wid-array2[1] = xeb.t-wid
                    xeb.k-len-array2[1] = xeb.t-len.
         else do:
           i = 0.
           for each w-box-design-line:
             ASSIGN
                i = i + 1
                xeb.k-wid-array2[i] = w-box-design-line.wscore-d.

             {sys/inc/k16bb.i xeb.k-wid-array2[i]}
           end.

           assign  v-score-char    = ""
                   j               = 1.
           do i = 1 to 80:
             if substr(v-lscore-c,i,1) ne "" then do:
                v-score-char[j] = v-score-char[j] + substr(v-lscore-c,i,1).
                if substr(v-lscore-c,i + 1,1) eq "" then
                   assign  v-score-char[j] = trim(v-score-char[j])
                           j = j + 1.
             end.
             if j gt 12 then leave.
           end.
           do i = 1 to EXTENT(xeb.k-len-array2):
              xeb.k-len-array2[i] = dec(v-score-char[i]).
              {sys/inc/k16bb.i xeb.k-len-array2[i]}.
           end.
         end.  /* else v-lscore */
       end. /* panels or not foam */
       
   end.

END PROCEDURE.



PROCEDURE calc-blank-size2 :

   find xeb where recid(xeb) = recid(eb) no-lock.
   
   {est/u2estc.i eb.gluelap 1}
   {est/u2estc.i eb.k-wid 2}
       
   find first item where item.company = est.company
                    and item.i-no eq eb.adhesive
                  no-lock no-error.
   if avail item then do:
            
            if item.mat-type eq "G" then do:
                    if eb.tab-in  then do:
                       {est/u2estc.i eb.k-len 3}
                    end.
                    else do:
                       {est/u2estc.i eb.k-len 4}
                    end.
            end.
            else if item.mat-type eq "S" then do:
                    if eb.tab-in  then do:
                       {est/u2estc.i eb.k-len 5}
                    end.
                    else do:
                       {est/u2estc.i eb.k-len 6}
                    end.
            end.
            else if item.mat-type eq "T" then do:
                    eb.tab-in = no.
                    {est/u2estc.i eb.k-len 7}
            end. 
    end.
    else do:
           
                 eb.tab-in = no.
                 {est/u2estc.i eb.k-len 7}
                    
    end.

    if eb.len eq eb.wid
    then do:
                 {est/u2estc.i eb.k-wid 2 dim-fit}
    end.
    else do:
                 {est/u2estc.i eb.k-wid 2}
    end.




END PROCEDURE.

PROCEDURE qty-assign:
    IF est.est-type LE 6 THEN DO:
        
        assign
             est-qty.qty[1] = prmEstQty
            est-qty.qty[2] = prmEstQty2
            est-qty.qty[3] = prmEstQty3
            est-qty.qty[4] = prmEstQty4
            est-qty.qty[5] = prmEstQty5
            est-qty.qty[6] = prmEstQty6
            est-qty.qty[7] = prmEstQty7
            est-qty.qty[8] = prmEstQty8
            est-qty.qty[9] = prmEstQty9
            est-qty.qty[10] = prmEstQty10
            est-qty.qty[11] = prmEstQty11
            est-qty.qty[12] = prmEstQty12
            est-qty.qty[13] = prmEstQty13
            est-qty.qty[14] = prmEstQty14
            est-qty.qty[15] = prmEstQty15
           /* est-qty.qty[16] = prmEstQty16
            est-qty.qty[17] = prmEstQty17
            est-qty.qty[18] = prmEstQty18
            est-qty.qty[19] = prmEstQty19
            est-qty.qty[20] = prmEstQty20*/
            .
        assign
            est-qty.qty[21] = prmRelQty1
            est-qty.qty[22] = prmRelQty2
            est-qty.qty[23] = prmRelQty3
            est-qty.qty[24] = prmRelQty4
            est-qty.qty[25] = prmRelQty5
            est-qty.qty[26] = prmRelQty6
            est-qty.qty[27] = prmRelQty7
            est-qty.qty[28] = prmRelQty8
            est-qty.qty[29] = prmRelQty9
            est-qty.qty[30] = prmRelQty10
            est-qty.qty[31] = prmRelQty11
            est-qty.qty[32] = prmRelQty12
            est-qty.qty[33] = prmRelQty13
            est-qty.qty[34] = prmRelQty14
            est-qty.qty[35] = prmRelQty15
           /* est-qty.qty[36] = prmRelQty16
            est-qty.qty[37] = prmRelQty17
            est-qty.qty[38] = prmRelQty18
            est-qty.qty[39] = prmRelQty19
            est-qty.qty[40] = prmRelQty20*/
            .
        FIND FIRST est where est.company = est-qty.company and
                        est.est-no = est-qty.est-no EXCLUSIVE-LOCK.
      assign
          est.est-qty[1] = est-qty.eqty
          est.est-qty[2] = est-qty.qty[2]
          est.est-qty[3] = est-qty.qty[3]
          est.est-qty[4] = est-qty.qty[4] .

END.
                        
             
END PROCEDURE.


PROCEDURE create-e-itemfg-vend :

   DEF BUFFER bf-e-itemfg-vend FOR e-itemfg-vend.
   DEF BUFFER bf-e-itemfg FOR e-itemfg.

   IF eb.stock-no EQ "" AND
      NOT CAN-FIND(FIRST e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = eb.eqty
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no    EQ ""
                     AND e-itemfg-vend.vend-no EQ "") THEN DO TRANSACTION:
      
      create e-itemfg-vend.
      
      ASSIGN e-itemfg-vend.company = eb.company
             e-itemfg-vend.item-type = NO
             e-itemfg-vend.est-no = eb.est-no
             e-itemfg-vend.eqty = eb.eqty
             e-itemfg-vend.form-no = eb.form-no
             e-itemfg-vend.blank-no = eb.blank-no
             e-itemfg-vend.run-qty[1] = 99999999.
   END.
   ELSE
   IF eb.stock-no NE "" AND
      NOT CAN-FIND(FIRST e-itemfg-vend
                   WHERE e-itemfg-vend.company EQ eb.company
                     AND e-itemfg-vend.est-no = eb.est-no
                     AND e-itemfg-vend.eqty = eb.eqty
                     AND e-itemfg-vend.form-no = eb.form-no
                     AND e-itemfg-vend.blank-no = eb.blank-no
                     AND e-itemfg-vend.i-no = eb.stock-no) THEN DO:

      FOR EACH bf-e-itemfg-vend WHERE
          bf-e-itemfg-vend.company EQ eb.company AND
          bf-e-itemfg-vend.i-no = eb.stock-no AND
          bf-e-itemfg-vend.est-no EQ ""
          NO-LOCK:
          
          DO TRANSACTION:
            create e-itemfg-vend.
            BUFFER-COPY bf-e-itemfg-vend TO e-itemfg-vend
            ASSIGN e-itemfg-vend.est-no = eb.est-no
                 e-itemfg-vend.eqty = eb.eqty
                 e-itemfg-vend.form-no = eb.form-no
                 e-itemfg-vend.blank-no = eb.blank-no.

            CREATE reftable.
            ASSIGN
               reftable.reftable = "e-itemfg-vend.std-uom"
               reftable.company  = e-itemfg-vend.company
               reftable.loc      = ""
               reftable.code     = e-itemfg-vend.est-no
               reftable.val[1]   = e-itemfg-vend.form-no
               reftable.val[2]   = e-itemfg-vend.blank-no.

            FIND FIRST bf-e-itemfg WHERE
                 bf-e-itemfg.company EQ bf-e-itemfg-vend.company AND
                 bf-e-itemfg.i-no EQ bf-e-itemfg-vend.i-no
                 NO-LOCK NO-ERROR.

            IF AVAIL bf-e-itemfg THEN
            DO:
               reftable.code2 = bf-e-itemfg.std-uom.
               RELEASE bf-e-itemfg.
            END.

            RELEASE e-itemfg-vend.
          END.
      END.
   END.

END PROCEDURE.


PROCEDURE calc-layout-corr :

 /*DEF BUFFER bf-eb FOR eb.*/

  DEF VAR ll AS LOG NO-UNDO.

  FIND xest WHERE ROWID(xest) = ROWID(est) EXCLUSIVE-LOCK NO-ERROR.
  FIND xef  WHERE ROWID(xef)  = ROWID(ef) EXCLUSIVE-LOCK NO-ERROR.
  FIND xeb  WHERE ROWID(xeb)  = ROWID(eb) EXCLUSIVE-LOCK NO-ERROR.

  find style where style.company = cocode and
                      style.style = eb.style no-lock no-error.   
     lv-foam = if avail style and style.type = "F" then yes else no.


  /*ll = ip-new AND
       NOT CAN-FIND(FIRST bf-eb
                    WHERE bf-eb.company EQ eb.company
                      AND bf-eb.est-no  EQ eb.est-no
                      AND bf-eb.form-no EQ eb.form-no
                      AND ROWID(bf-eb)  NE ROWID(eb)).

  IF NOT ll THEN
    MESSAGE "Do you wish to reset layout screen?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE ll.

  IF ll THEN DO:*/
    IF NOT lv-foam THEN DO:
      {sys/inc/ceroute1.i w id l en}
    END.

    RUN cec/calc-dim.p.
  

  IF ceroute-chr NE "" THEN DO:
    FIND FIRST mach
        WHERE mach.company EQ est.company
          AND mach.loc     EQ est.loc
          AND mach.m-code  EQ ceroute-chr
          AND mach.dept[1] EQ "CR"
        NO-LOCK NO-ERROR.
    IF AVAIL mach THEN DO:
      ASSIGN
       xef.m-code   = ceroute-chr
       xef.lsh-lock = NO
       xeb.num-wid  = 1
       xeb.num-len  = 1.

      RUN cec/calc-dim1.p NO-ERROR.

      ASSIGN
       xef.gsh-len = xef.gsh-len - (xef.nsh-len * xef.n-out-l)
       xef.n-out-l = 1
       xef.gsh-len = xef.gsh-len + (xef.nsh-len * xef.n-out-l).

      IF ceroute-int NE 0 AND ceroute-int LT xef.gsh-wid THEN
        ASSIGN
         xef.n-out   = TRUNC(ceroute-int / xef.nsh-wid,0)
         xef.gsh-wid = xef.n-out * xef.nsh-wid + (mach.min-trimw * 2).
    END.
  END.  

END PROCEDURE.
 
PROCEDURE create-stack:

  FIND xest WHERE ROWID(xest) = ROWID(est) EXCLUSIVE-LOCK NO-ERROR.
  FIND xef  WHERE ROWID(xef)  = ROWID(ef) EXCLUSIVE-LOCK NO-ERROR.
  FIND xeb  WHERE ROWID(xeb)  = ROWID(eb) EXCLUSIVE-LOCK NO-ERROR.

    def var lv-cas-pal as dec no-undo.
     def var lv-tr-cnt as int no-undo.
     def var lv-numstack as int no-undo.
     def var lv-stackcode as cha no-undo.
         def var lv-error as log no-undo.
     run cec/kpallet.p (recid(xeb), output lv-cas-pal, output lv-tr-cnt,
                        output lv-numstack, output lv-stackcode, output lv-error).

     
       lv-layers = lv-cas-pal / lv-numstack.
       {sys/inc/roundup.i lv-layers}

       ASSIGN
        eb.cas-pal    = lv-cas-pal
        eb.tr-cnt     = lv-tr-cnt
        eb.tr-cas     = lv-layers
        eb.stacks     = lv-numstack
        eb.stack-code = lv-stackcode.

END PROCEDURE.
