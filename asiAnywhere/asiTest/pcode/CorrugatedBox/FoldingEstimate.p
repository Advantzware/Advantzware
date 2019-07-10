

/*------------------------------------------------------------------------
    File        : FoldingEstimate.p
    Purpose     :  Folding Estimate

    Syntax      :

    Description : Return a Dataset of Folding Estimates

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFoldingEstimate NO-UNDO
    FIELD vEst              AS CHAR FORMAT "x(8)"
    FIELD vCust             AS CHAR FORMAT "x(8)"
    FIELD vCustPart         AS CHAR FORMAT "x(15)"
    FIELD vShipTo           AS CHAR FORMAT "x(8)"
    FIELD vItemName         AS CHAR FORMAT "x(30)"
    FIELD vFgItem           AS CHAR FORMAT "x(15)"
    FIELD vEstQty           AS DECIMAL  FORMAT ">>>>>>>9"
    FIELD vStyle            AS CHAR FORMAT "x(6)"
    FIELD vPaper1            AS CHAR 
    FIELD vPaper2             AS CHAR 
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
    FIELD vNoW              AS DECIMAL
    FIELD vNoL              AS DECIMAL
    FIELD vUp               AS INTEGER 
    FIELD vDieIn            AS DECIMAL
    FIELD vhjk              AS CHAR   
    FIELD   kuh             AS CHAR 
    FIELD vEstType          AS INT
    FIELD vRec_key           AS CHAR  
    FIELD vQtyExtent  AS DECIMAL EXTENT 20
    FIELD vRelQtyExtent  AS DECIMAL EXTENT 20  
    FIELD vTlen             AS DECIMAL 
    FIELD vTwid             AS DECIMAL  
    FIELD vefbrowseval       AS CHAR 
    .

DEFINE DATASET dsFoldingEstimate FOR ttFoldingEstimate.

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
    DEFINE INPUT PARAMETER prmPaper1             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPaper2            AS CHAR NO-UNDO.
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
    DEFINE INPUT PARAMETER prmDiein             AS DECIMAL NO-UNDO.
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
   

    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.
    
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldingEstimate.

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
    IF   prmPaper1         = ?      THEN    prmPaper1       = "".       
    IF   prmPaper2         = ?      THEN    prmPaper2       = "".       
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
    IF   prmCoatPassesFrom = ?      THEN    prmCoatPassesFrom =1. 
    IF   prmDiein          = ?      THEN    prmDiein          =0.
    IF   prmPurchManuf     = ?      THEN    prmPurchManuf     = "".

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
{cec/descalc.i "new"}
    def buffer bb for eb.
    def buffer bf for ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-ef FOR ef.
  def buffer bf-eb for eb.
   def buffer bf-est for est.
  def buffer bf-est-qty for est-qty.
      DEF BUFFER bf1-eb FOR eb.
def buffer bqty for est-qty.
  def buffer best for est.
 def var ll-dum as log no-undo.
  def var lv-ef-recid as recid no-undo.
  def var lv-eb-recid as recid no-undo.
  DEF VAR ll-mass-del AS LOG NO-UNDO.
  def var li-est-type like est.est-type no-undo.
  DEFINE VAR outcomm AS DECIMAL NO-UNDO.
 DEF VAR ld-markup AS DEC NO-UNDO.
 def var uom-list as cha no-undo. 

  DEF VAR vTr-no AS CHAR NO-UNDO.
  DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-count2 AS INT NO-UNDO.

  def var char-val as cha no-undo.
def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xqty for est-qty.
DEF TEMP-TABLE tt-est-op LIKE est-op.
def NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def NEW SHARED var locode     as   char  format "x(5)"  no-undo.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.

def NEW SHARED var  x  as   int no-undo.
def NEW SHARED var  y  as   int no-undo.
DEF NEW SHARED  VAR  k  as   int no-undo.
def new shared var xcal as dec no-undo.
def new shared var sh-wid as dec no-undo.
def new shared var sh-len as dec no-undo.
def new shared temp-table formule field formule as dec extent 12.

def var i          as   int no-undo.
def var j          as   int no-undo.
/*{sys/inc/var.i SHARED} */
def var xx as dec no-undo.
    DEF BUFFER recalc-mr FOR reftable.
def var li-new-estnum as INT no-undo.
DEF VAR li AS INT NO-UNDO.
DEFINE VAR bi AS INT NO-UNDO.
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
    def var lv-estqty-recid as recid no-undo.
    def var char-val2 as cha no-undo.        
   def var date-val as cha no-undo.
     def var date-val2 as cha no-undo.
     def var lv-copy-qty as int extent 20 no-undo.
     DEF VAR v-side-count AS INT NO-UNDO.

    def var k_frac as dec init 6.25 no-undo.
    def var ls-add-what as cha no-undo.
    DEF VAR lv-hld-fcol LIKE ef.f-col NO-UNDO.
    DEF VAR lv-hld-fpas LIKE ef.f-pass NO-UNDO.
    DEF VAR lv-hld-fcot LIKE ef.f-coat NO-UNDO.
    DEF VAR lv-hld-fctp LIKE ef.f-coat-p NO-UNDO.

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

{sys/inc/ceroute.i F}
  /*{sys/inc/cestyle.i F}*/

  FUNCTION display-combo-qty RETURNS DECIMAL():
      DEF VAR lv-qty LIKE est-qty.eqty NO-UNDO.
      DEF BUFFER b-eb FOR eb.
          IF AVAIL est-qty AND AVAIL eb THEN DO:
             /* lv-qty = est-qty.eqty.*/
              FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) NO-LOCK NO-ERROR.
             IF AVAIL b-eb /*AND b-eb.est-type EQ 4*/ THEN lv-qty = b-eb.bl-qty.
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
          CorType = 1.
  END.
  IF prmType = "Set" THEN DO:
      ASSIGN 
          CorType = 2.
  END.
  IF prmType = "Tandem" THEN DO:
      ASSIGN 
          CorType = 4.
  END.
  FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc  EXCLUSIVE-LOCK NO-ERROR.
  
  li-new-estnum = INT( prmEstimate) .
  
  CREATE est.
  assign
      est.est-type = CorType
      est.company = prmComp
      est.loc     = prmLoc
      est.est-no = string(li-new-estnum,">>>>>>>9")
      est.form-qty = 1
      est.est-date = today
      est.mod-date = today
      est.entered-id   = prmUser
      est.updated-id   = prmUser
      .
  /*{sys/ref/est-add.i est F} */
  

     CREATE est-qty.
     ASSIGN 
         est-qty.company  = prmComp
         est-qty.est-no   = est.est-no
         est-qty.eqty     = prmEstQty
         est-qty.qty-date = est.est-date.

     FIND LAST ef
         WHERE ef.company EQ est-qty.company
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
             ef.lsh-wid   = prmWidth
             ef.lsh-len   = prmLength
             ef.cost-uom  = "MSF"

             ef.f-col    =  prmColor
             ef.f-pass   = IF prmColor > 0  THEN 1 ELSE 0
             ef.f-coat   = prmCoating
             ef.f-coat-p = IF prmCoating > 0 THEN 1   ELSE 0
             ef.flute    = prmPaper1
             
             .

         

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
                eb.part-no  = prmCustPart
                eb.cas-no   = ce-ctrl.def-case
                eb.tr-no    = ce-ctrl.def-pal
                eb.cust-%   = prmQtySet
                
                eb.test     = prmPaper2
                eb.cust-no  = prmCust
                eb.stock-no  = prmFgItem
                eb.ship-id  = prmShipTo
                eb.style    = prmStyle
                eb.part-dscr1 = prmItemName
               
                eb.yld-qty = prmQtySet 
                eb.bl-qty = prmEstQty 
                ef.board  = prmBoard
                ef.cal    = prmCalliper
                eb.procat = prmCategory
                eb.form-no = 1
                eb.blank-no = 1 
                eb.i-col = prmColor
                eb.i-pass = IF prmColor > 0 THEN  1 ELSE 0
                eb.i-coat =  prmCoating 
                eb.i-coat-p = IF prmCoating  > 0 THEN 1 ELSE 0
                eb.pur-man  = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                    /*eb.tab-in   = IF prmTab = "In" THEN TRUE ELSE FALSE */
                eb.len   = prmLength
                eb.wid      = prmWidth
                eb.dep      = prmDepth
                est.est-date = prmEstDate 

                    eb.yrprice  = prev-yrprice
                    eb.cust-%   = prmQtySet
                    
                    eb.num-up   =  1
                    eb.die-in  = prmDiein 
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
                        ASSIGN 
                           eb.cas-len = e-item.case-l
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

                  if eb.stock-no = "" then do:
                      find first ce-ctrl where ce-ctrl.company = cocode and
                          ce-ctrl.loc = locode
                          no-lock no-error.
                      eb.cas-no = ce-ctrl.def-case.
                      eb.tr-no = ce-ctrl.def-pal.      
                 end.

                  find cust where cust.company = prmComp
                      and cust.cust-no = eb.cust-no no-lock no-error.
                 
                  eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
                  eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no. 
                   eb.sman  = IF AVAIL cust THEN cust.sman ELSE "" .
                       /* get default values from rm table */
                       find item where item.company = eb.company and
                           item.i-no = eb.cas-no
                           no-lock no-error.
                       if avail item THEN assign 
                                            eb.cas-cnt = (item.box-case)
                                            eb.cas-len = (item.case-l)
                                            eb.cas-wid = (item.case-w)
                                            eb.cas-dep = (item.case-d)
                                            eb.cas-pal = (item.case-pall)
                                            eb.cas-wt = (item.avg-w)         .
                       find item where item.company = eb.company and
                           item.i-no = eb.tr-no
                           no-lock no-error.
                       if avail item then assign /*eb.cas-cost:Screen-value = */
                               eb.tr-len = (item.case-l)
                               eb.tr-wid = (item.case-w)
                               eb.tr-dep = (item.case-d)   .
                       ASSIGN
                           eb.tr-cnt = eb.cas-cnt * eb.cas-pal
                           eb.tr-cas = 1.
                       
                    find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                            eb.adhesive = style.material[7]
                            eb.gluelap = style.dim-gl
                            eb.k-len = style.dim-dkl
                            eb.k-wid = style.dim-dkw
                            eb.fpanel = style.dim-pan5
                            eb.lock = style.dim-fit
                            eb.tuck = style.dim-tk.

                     FIND FIRST ITEM WHERE item.company EQ prmComp AND item.i-no EQ eb.adhesive NO-LOCK NO-ERROR.
                        IF AVAIL ITEM AND INDEX("G,S,T",item.mat-type) GT 0 AND
                            item.i-no NE "No Joint" THEN 
                            ASSIGN
                                eb.lin-in = DECIMAL(eb.dep).
                        
                      RUN calc-pass NO-ERROR.
                      RUN calc-blank-size NO-ERROR.

                         if not avail cust then find cust where cust.company = eb.company and
                             cust.cust-no = eb.cust-no
                             no-lock no-error.

                         RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                                                 OUTPUT outcomm).
                         ASSIGN
                             eb.comm = outcomm .

                         FIND FIRST item NO-LOCK
                             WHERE item.company EQ cocode
                             AND item.i-no    EQ ef.board
                             NO-ERROR.
                         if avail item then do:
                             assign /*ef.board = item.i-no */
                                 ef.i-code = item.i-code
                                 ef.flute = item.flute
                                 ef.test = item.reg-no
                                 ef.weight = item.basis-w.
                             RUN sys/ref/uom-rm.p (item.mat-type, output uom-list).
                             IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
                             if item.i-code = "R" then assign ef.lsh-len = item.s-len
                                 ef.lsh-wid = item.s-wid
                                 ef.gsh-wid = item.s-wid.
                             if item.r-wid <> 0 then assign ef.roll = true
                                 ef.roll-wid = item.r-wid
                                 ef.lsh-wid = item.r-wid
                                 ef.gsh-wid = item.r-wid.   
                             FIND FIRST e-item OF item NO-LOCK NO-ERROR.
                             IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.
                       end.          
                       /*end.  /* avail reftable */*/

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
                                         ef.leaf-w[i] = eb.t-wid
                                         .
                                     leave.
                                end.   
                            end.
                         END.

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

                      IF CorType = 4 THEN DO:
                          ASSIGN
                              eb.bl-qty      = prmEstQty .
                      END.
                      
                     

                      RUN create-prep NO-ERROR.   
                      RUN qty-assign NO-ERROR.

                      IF (CorType GE 1 AND CorType LE 2 AND eb.i-col + eb.i-coat EQ 0) OR
                            (CorType GE 3 AND CorType LE 4 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
                        {ce/delplate.i}
                      END.

                        IF CorType GE 3 AND CorType LE 4 THEN
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
                      
                       RUN calc-layout NO-ERROR.
                       IF est.est-type GT 1  AND 
                           (eb.yld-qty LT eb.bl-qty) THEN
                           RUN set-yld-qty (ROWID(eb)).

                       if  ceroute-log AND CorType EQ 1 then do:
                        RUN creat_est-op NO-ERROR.
                       END.

                      IF CorType = 2 THEN DO:
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
            prmBlank    = 1
            prmAction   = "Select" .
      END.

      ELSE DO:
          FIND LAST eb  WHERE eb.company = prmComp AND  (eb.est-type = 1 OR eb.est-type = 2 OR eb.est-type = 4) NO-LOCK NO-ERROR.
          ASSIGN
              prmEstimate = eb.est-no
              prmFrom     = 1
              prmBlank    = 1
              prmAction   = "Select" .
       END.


 END.  /*  end of delete*/



IF prmAction = "Update" THEN DO:
     
    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
    FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = est.company EXCLUSIVE-LOCK :
    IF AVAIL est-qty  THEN DO:
            ASSIGN
            est.est-date = prmEstDate
            varEqty = est-qty.eqty. 
    END.
    FOR EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK ,
     EACH  eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no 
                AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank  EXCLUSIVE-LOCK:
     IF  AVAIL ef AND AVAIL eb THEN DO:

         ASSIGN
               lv-hld-fcol = ef.f-col    
               lv-hld-fpas = ef.f-pass   
               lv-hld-fcot = ef.f-coat   
               lv-hld-fctp = ef.f-coat-p .

       ASSIGN 
         ef.lsh-wid   = prmLength
         ef.lsh-len   = prmWidth
         ef.f-col  =   prmInkFrom 
         ef.f-pass  =  prmPassesFrom
         ef.f-coat  = prmCoatingFrom
         ef.f-coat-p = prmCoatPassesFrom 
         ef.medium    = prmPaper1
         ef.flute     = prmPaper2
         
         eb.part-no  = prmCustPart
         eb.stock-no   = prmFgItem
         eb.cust-no  = prmCust
         eb.ship-id  = prmShipTo
         eb.style    = prmStyle
         eb.part-dscr1 = prmItemName
         eb.cust-% = prmQtySet 
         eb.bl-qty = prmEstQty 
         ef.board  = prmBoard
         ef.cal    = prmCalliper
         eb.procat = prmCategory
         eb.i-col = prmColor
         eb.i-coat = prmCoating 
         eb.pur-man  = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
         /*eb.tab-in   = IF prmTab = "In" THEN TRUE ELSE FALSE */
         eb.len   = prmLength
         eb.wid      = prmWidth
         eb.dep      = prmDepth
         
         /*est.updated-id   = prmUser */
             
         eb.die-in  = prmDiein     .

         IF prmFrom = 1  THEN DO:
              ASSIGN
                  ef.eqty   = prmEstQty
                  eb.eqty   = prmEstQty
                  est-qty.eqty = prmEstQty
                   .
         END.
         ELSE DO:
             ASSIGN 
                 ef.eqty      = varEqty
                  eb.eqty     = varEqty.
         END.
         find cust where cust.company = prmComp
                      and cust.cust-no = eb.cust-no no-lock no-error.
                  
         eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
         eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.   
          eb.sman  = IF AVAIL cust THEN cust.sman ELSE "" .
                /* get default values from rm table */
                find item where item.company = eb.company and
                    item.i-no = eb.cas-no
                    no-lock no-error.
                if avail item THEN assign 
                                     eb.cas-cnt = (item.box-case)
                                     eb.cas-len = (item.case-l)
                                     eb.cas-wid = (item.case-w)
                                     eb.cas-dep = (item.case-d)
                                     eb.cas-pal = (item.case-pall)
                                     eb.cas-wt = (item.avg-w)         .
                find item where item.company = eb.company and
                    item.i-no = eb.tr-no
                    no-lock no-error.
                if avail item then assign /*eb.cas-cost:Screen-value = */
                        eb.tr-len = (item.case-l)
                        eb.tr-wid = (item.case-w)
                        eb.tr-dep = (item.case-d)   .
                ASSIGN
                    eb.tr-cnt = eb.cas-cnt * eb.cas-pal
                    eb.tr-cas = 1.

            
   find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                            eb.adhesive = style.material[7]
                            eb.gluelap = style.dim-gl
                            eb.k-len = style.dim-dkl
                            eb.k-wid = style.dim-dkw
                            eb.fpanel = style.dim-pan5
                            eb.lock = style.dim-fit
                            eb.tuck = style.dim-tk.  

                     FIND FIRST ITEM WHERE item.company EQ prmComp AND item.i-no EQ eb.adhesive NO-LOCK NO-ERROR.
                        IF AVAIL ITEM AND INDEX("G,S,T",item.mat-type) GT 0 AND
                            item.i-no NE "No Joint" THEN 
                            ASSIGN
                                eb.lin-in = DECIMAL(eb.dep).

                         if not avail cust then find cust where cust.company = eb.company and
                             cust.cust-no = eb.cust-no
                             no-lock no-error.

                         RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                                                 OUTPUT outcomm).
                         ASSIGN
                             eb.comm = outcomm .

                         
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
         
    END.
    RUN calc-pass NO-ERROR.
    RUN   calc-blank-size NO-ERROR.
    RUN qty-assign NO-ERROR.

    IF (CorType GE 1 AND CorType LE 2 AND eb.i-col + eb.i-coat EQ 0) OR
        (CorType GE 3 AND CorType LE 4 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
        {ce/delplate.i}
     END.

        IF CorType GE 3 AND CorType LE 4 THEN
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
    
    IF est.est-type GT 1  AND 
                           (eb.yld-qty LT eb.bl-qty) THEN
                           RUN set-yld-qty (ROWID(eb)).

    END. /* end of est-qty */
    END. /* end of for each eb and ef*/
 ASSIGN  prmAction = "Select" .

END. /*end of update*/

/*********add a form  *********/




IF prmAction = "FormEstimate" THEN  DO:
    FIND FIRST est WHERE est.company = prmComp AND est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) EXCLUSIVE-LOCK NO-ERROR. 

    FIND FIRST est-qty  WHERE est-qty.company EQ prmComp
        AND est-qty.est-no  EQ est.est-no  EXCLUSIVE-LOCK NO-ERROR. 
    ASSIGN
        est.est-date      = prmEstDate 
        /* est.updated-id    = VAR*/
        varEqty = est-qty.eqty       .
     FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc NO-LOCK NO-ERROR.
    
        FIND LAST ef WHERE ef.company EQ est-qty.company
            AND ef.est-no  EQ est-qty.est-no
            AND ef.eqty    EQ est-qty.eqty
            USE-INDEX est-qty NO-LOCK NO-ERROR.
        li = (IF AVAIL ef THEN ef.form-no ELSE 0) + 1 .
                 
            CREATE ef.
            ASSIGN
                ef.est-type  = est.est-type
                ef.company   = prmComp
                ef.loc       = prmLoc
                ef.e-num     = est.e-num
                ef.est-no    = est.est-no
                ef.eqty      = est-qty.eqty
                ef.form-no   = li 
                ef.cust-seq  = 1
                ef.blank-qty = 1
                ef.lsh-wid   = prmWidth
                ef.lsh-len   = prmLength
                ef.cost-uom  = "MSF"
                
                ef.f-col       = prmColor
                ef.f-pass      = IF prmColor > 0 THEN 1 ELSE 0
                ef.f-coat      = prmCoating 
                ef.f-coat-p    = IF prmCoating > 0 THEN 1  ELSE 0
                 ef.medium     = prmPaper1
                 ef.flute      = prmPaper2 .


       FOR EACH eb  WHERE eb.company EQ ef.company 
           AND eb.est-no  EQ ef.est-no
           AND eb.eqty    EQ ef.eqty NO-LOCK
           BY eb.form-no  DESC  BY eb.blank-no DESC:
           prev-yrprice = eb.yrprice.
           LEAVE.
           END.

           FIND LAST eb NO-LOCK  WHERE eb.company EQ ef.company
               AND eb.est-no  EQ ef.est-no
               AND eb.form-no NE 0
               USE-INDEX est-qty NO-ERROR.
           IF AVAIL eb THEN
               ASSIGN
               prev-cust  = eb.cust-no
               prev-ship  = eb.ship-id
               prev-style = eb.style.
           
           FIND LAST eb NO-LOCK WHERE eb.company EQ ef.company
               AND eb.est-no  EQ ef.est-no
               AND eb.form-no EQ ef.form-no
               USE-INDEX est-qty NO-ERROR.
           bi = IF AVAIL eb THEN eb.blank-no ELSE 0.

            FIND FIRST eb  WHERE eb.company  EQ ef.company 
                AND eb.est-no   EQ ef.est-no
                AND eb.form-no  EQ 0
                AND eb.blank-no EQ 0
                NO-LOCK NO-ERROR.
            ls-part-no = IF AVAIL eb THEN eb.part-no ELSE "".

             CREATE eb.
             ASSIGN
                 eb.eqty     = ef.eqty
                 eb.est-type = ef.est-type
                 eb.company  = prmComp
                 eb.loc      = ef.loc
                 eb.e-num    = ef.e-num
                 eb.est-no   = ef.est-no
                 eb.est-int  = INT(ef.est-no)
                 eb.eqty     = ef.eqty
                 eb.form-no  = li 
                 eb.cust-seq = 1
                 eb.blank-no = bi + 1
                 eb.part-no  = prmCustPart
                 eb.cas-no   = ce-ctrl.def-case
                 eb.tr-no    = ce-ctrl.def-pal
                 
                 eb.cust-no  = prmCust
                 eb.stock-no  = prmFgItem
                 eb.ship-id  = prmShipTo
                 eb.style    = prmStyle
                 eb.part-dscr1 = prmItemName
                 eb.yld-qty = prmQtySet 
                 eb.bl-qty = prmEstQty 
                 ef.board  = prmBoard
                 ef.cal    = prmCalliper
                 eb.procat = prmCategory
                 eb.i-col = prmColor
                 eb.i-pass = IF prmColor > 0 THEN  1 ELSE 0
                 eb.i-coat = prmCoating 
                 eb.i-coat-p = IF prmCoating > 0 THEN 1 ELSE 0
                 eb.pur-man  = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                 /*eb.tab-in   = IF prmTab = "In" THEN TRUE ELSE FALSE */
                 eb.len   = prmLength
                 eb.wid      = prmWidth
                 eb.dep      = prmDepth
                 est.est-date = prmEstDate 
                 eb.yrprice  = prev-yrprice
                 eb.i-pass   = 0
                 eb.cust-%   = prmQtySet
                 eb.num-up   =  1
                 eb.die-in  = prmDiein 
                 eb.chg-method = "P"  .




          FIND FIRST ITEM WHERE ITEM.company = ef.company
              AND ITEM.mat-type = "C"
              AND item.i-no eq eb.cas-no no-lock no-error.
          if avail item then do:
              find first e-item
                  where e-item.company eq item.company
                  and e-item.loc     eq item.loc
                  and e-item.i-no    eq item.i-no   no-lock no-error.
              find first itemfg
                  where itemfg.company eq prmComp
                  and itemfg.i-no    eq eb.stock-no   no-lock no-error.
              if avail e-item then
                  ASSIGN
                  eb.cas-len = e-item.case-l
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
         if eb.cas-cnt eq 0 then eb.cas-cnt = if avail itemfg then itemfg.case-count else item.box-case.
         end.

         find cust where cust.company = prmComp
                      and cust.cust-no = eb.cust-no no-lock no-error.
                  
         eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
         eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.   
          eb.sman  = IF AVAIL cust THEN cust.sman ELSE "" .
                /* get default values from rm table */
                find item where item.company = eb.company and
                    item.i-no = eb.cas-no
                    no-lock no-error.
                if avail item THEN assign 
                                     eb.cas-cnt = (item.box-case)
                                     eb.cas-len = (item.case-l)
                                     eb.cas-wid = (item.case-w)
                                     eb.cas-dep = (item.case-d)
                                     eb.cas-pal = (item.case-pall)
                                     eb.cas-wt = (item.avg-w)         .
                find item where item.company = eb.company and
                    item.i-no = eb.tr-no
                    no-lock no-error.
                if avail item then assign /*eb.cas-cost:Screen-value = */
                        eb.tr-len = (item.case-l)
                        eb.tr-wid = (item.case-w)
                        eb.tr-dep = (item.case-d)   .
                ASSIGN
                    eb.tr-cnt = eb.cas-cnt * eb.cas-pal
                    eb.tr-cas = 1.

            
   find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                            eb.adhesive = style.material[7]
                            eb.gluelap = style.dim-gl
                            eb.k-len = style.dim-dkl
                            eb.k-wid = style.dim-dkw
                            eb.fpanel = style.dim-pan5
                            eb.lock = style.dim-fit
                            eb.tuck = style.dim-tk.  

                     FIND FIRST ITEM WHERE item.company EQ prmComp AND item.i-no EQ eb.adhesive NO-LOCK NO-ERROR.
                        IF AVAIL ITEM AND INDEX("G,S,T",item.mat-type) GT 0 AND
                            item.i-no NE "No Joint" THEN 
                            ASSIGN
                                eb.lin-in = DECIMAL(eb.dep).

                         if not avail cust then find cust where cust.company = eb.company and
                             cust.cust-no = eb.cust-no
                             no-lock no-error.

                         RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                                                 OUTPUT outcomm).
                         ASSIGN
                             eb.comm = outcomm .

                         FIND FIRST item NO-LOCK
                             WHERE item.company EQ cocode
                             AND item.i-no    EQ ef.board
                             NO-ERROR.
                         if avail item then do:
                             assign /*ef.board = item.i-no */
                                 ef.i-code = item.i-code
                                 ef.flute = item.flute
                                 ef.test = item.reg-no
                                 ef.weight = item.basis-w.
                             RUN sys/ref/uom-rm.p (item.mat-type, output uom-list).
                             IF uom-list NE "" THEN ef.cost-uom = ENTRY(1,uom-list).
                             if item.i-code = "R" then assign ef.lsh-len = item.s-len
                                 ef.lsh-wid = item.s-wid
                                 ef.gsh-wid = item.s-wid.
                             if item.r-wid <> 0 then assign ef.roll = true
                                 ef.roll-wid = item.r-wid
                                 ef.lsh-wid = item.r-wid
                                 ef.gsh-wid = item.r-wid.   
                             FIND FIRST e-item OF item NO-LOCK NO-ERROR.
                             IF AVAIL e-item THEN ef.cost-uom = e-item.std-uom.
                       end.          
                       /*end.  /* avail reftable */*/

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
                                         ef.leaf-w[i] = eb.t-wid
                                         .
                                     leave.
                                end.   
                            end.
                         END.
                   

         IF eb.est-type EQ 2 THEN DO:
             
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
          
         IF CorType = 4 THEN DO:
             ASSIGN
                 eb.bl-qty      = prmEstQty .
                 
             END.  
        RUN calc-layout NO-ERROR.
        RUN calc-pass NO-ERROR.
        RUN   calc-blank-size NO-ERROR.
        RUN qty-assign NO-ERROR.
         RUN create-prep NO-ERROR.
                     

         IF (CorType GE 1 AND CorType LE 2 AND eb.i-col + eb.i-coat EQ 0) OR
             (CorType GE 3 AND CorType LE 4 AND ef.f-col + ef.f-coat EQ 0) THEN DO:
             {ce/delplate.i}
         END.

             IF CorType GE 3 AND CorType LE 4 THEN
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

        IF est.est-type GT 1  AND 
                           (eb.yld-qty LT eb.bl-qty) THEN
                           RUN set-yld-qty (ROWID(eb)).

         ASSIGN 
             prmFrom   = ef.form-no
             prmAction = "Select".

END. /* end of formEstimate*/

IF prmAction = "BlankSave" THEN DO:
     FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST  est-qty WHERE est-qty.est-no = est.est-no  EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK NO-ERROR.

        FIND FIRST ce-ctrl WHERE ce-ctrl.company = prmComp  AND ce-ctrl.loc = prmLoc NO-LOCK NO-ERROR.
        
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
                                              
        FIND FIRST eb
            WHERE eb.company  EQ ef.company 
            AND eb.est-no   EQ ef.est-no
            AND eb.form-no  EQ 0
            AND eb.blank-no EQ 0
            NO-LOCK NO-ERROR.
        ls-part-no = IF AVAIL eb THEN eb.part-no ELSE "".

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
                eb.part-no  = prmCustPart
                eb.cas-no   = ce-ctrl.def-case
                eb.tr-no    = ce-ctrl.def-pal
                eb.cust-no  = prmCust
                eb.ship-id  = prmShipTo
                eb.style    = prmStyle
                eb.yrprice  = prev-yrprice
                eb.i-pass   = 0
                eb.cust-%   = prmQtySet
                
                eb.stock-no  = prmFgItem
                eb.part-dscr1 = prmItemName
                eb.yld-qty = prmQtySet 
                eb.bl-qty = prmEstQty 
               /* ef.board  = prmBoard
                ef.cal    = prmCalliper*/
                eb.procat = prmCategory
                eb.i-col = prmColor
                eb.i-pass = IF prmColor > 0 THEN 1 ELSE 0
                eb.i-coat = prmCoating 
                eb.i-coat-p = IF prmCoating > 0 THEN 1 ELSE 0
                eb.pur-man  = IF prmPurchManuf = "P" THEN TRUE ELSE FALSE 
                 /*eb.tab-in   = IF prmTab = "In" THEN TRUE ELSE FALSE */
                eb.len   = prmLength
                eb.wid      = prmWidth
                eb.dep      = prmDepth
                /*est.est-date = prmEstDate */
                eb.yrprice  = prev-yrprice
                eb.i-pass   = 0
                eb.num-up   =  1
                eb.die-in  = prmDiein 
                eb.chg-method = "P"  .
    
    FIND FIRST ITEM WHERE ITEM.company = ef.company
        AND ITEM.mat-type = "C"
        AND item.i-no eq eb.cas-no no-lock no-error.
    if avail item then do:
        find first e-item
            where e-item.company eq item.company
            and e-item.loc     eq item.loc
            and e-item.i-no    eq item.i-no   no-lock no-error.
        
        find first itemfg
            where itemfg.company eq cocode
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

    find cust where cust.company = prmComp
                      and cust.cust-no = eb.cust-no no-lock no-error.
                  
         eb.cas-no = if avail cust and cust.case-bundle <> "" then cust.case-bundle else eb.cas-no.
         eb.tr-no = if avail cust and cust.pallet <> "" then cust.pallet else eb.tr-no.  
          eb.sman  = IF AVAIL cust THEN cust.sman ELSE "" .
                /* get default values from rm table */
                find item where item.company = eb.company and
                    item.i-no = eb.cas-no
                    no-lock no-error.
                if avail item THEN assign 
                                     eb.cas-cnt = (item.box-case)
                                     eb.cas-len = (item.case-l)
                                     eb.cas-wid = (item.case-w)
                                     eb.cas-dep = (item.case-d)
                                     eb.cas-pal = (item.case-pall)
                                     eb.cas-wt = (item.avg-w)         .
                find item where item.company = eb.company and
                    item.i-no = eb.tr-no
                    no-lock no-error.
                if avail item then assign /*eb.cas-cost:Screen-value = */
                        eb.tr-len = (item.case-l)
                        eb.tr-wid = (item.case-w)
                        eb.tr-dep = (item.case-d)   .
                ASSIGN
                    eb.tr-cnt = eb.cas-cnt * eb.cas-pal
                    eb.tr-cas = 1.


    find style where style.company = prmComp and
                         style.style = prmStyle no-lock no-error.
                     if avail style then 
                         assign
                            eb.adhesive = style.material[7]
                            eb.gluelap = style.dim-gl
                            eb.k-len = style.dim-dkl
                            eb.k-wid = style.dim-dkw
                            eb.fpanel = style.dim-pan5
                            eb.lock = style.dim-fit
                            eb.tuck = style.dim-tk.

                     FIND FIRST ITEM WHERE item.company EQ prmComp AND item.i-no EQ eb.adhesive NO-LOCK NO-ERROR.
                        IF AVAIL ITEM AND INDEX("G,S,T",item.mat-type) GT 0 AND
                            item.i-no NE "No Joint" THEN 
                            ASSIGN
                                eb.lin-in = DECIMAL(eb.dep).

                         if not avail cust then find cust where cust.company = eb.company and
                             cust.cust-no = eb.cust-no
                             no-lock no-error.

                         RUN ce/markup.p (eb.company, ROWID(eb), OUTPUT ld-markup).
                         RUN sys/inc/getsmncm.p (eb.cust-no, INPUT-OUTPUT eb.sman, eb.procat, ld-markup,
                                                 OUTPUT outcomm).
                         ASSIGN
                             eb.comm = outcomm .

                         
        IF eb.est-type EQ 2 THEN DO:
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
          
         IF CorType = 4 THEN DO:
             ASSIGN
                 eb.bl-qty      = prmEstQty .
                 
             END.  
        RUN calc-pass NO-ERROR.
        RUN   calc-blank-size NO-ERROR.
        RUN qty-assign NO-ERROR.
        IF est.est-type GT 1  AND 
                           (eb.yld-qty LT eb.bl-qty) THEN
                           RUN set-yld-qty (ROWID(eb)).
        
         ASSIGN 
             prmFrom   = ef.form-no
             prmBlank  = eb.blank-no 
             prmAction = "Select".
       
END. /****end of add blank*****/


IF prmAction = "ChangeTandem"  THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp EXCLUSIVE-LOCK NO-ERROR.
      FOR EACH est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = prmComp  EXCLUSIVE-LOCK ,
       EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                    AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom EXCLUSIVE-LOCK,
        EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no  
                  AND eb.form-no = ef.form-no  EXCLUSIVE-LOCK:
          IF est.ord-no = 0 THEN DO:
              IF AVAIL est  THEN  DO:
              ASSIGN
                  est.est-type = 4
                  ef.est-type = 4
                  eb.est-type = 4 .
              END.
           END.
            ELSE DO:
            ASSIGN cError = "Cannot change type" .
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
          IF est.ord-no = 0 THEN DO:
              IF AVAIL est  THEN  DO:
              ASSIGN
                  est.est-type = 2
                  ef.est-type = 2
                  eb.est-type = 2 .
                END.
            END.
            ELSE DO:
                ASSIGN cError = "Cannot change type" .
                RETURN.
            END.

          ASSIGN  prmAction = "Select" .
      END.
  END.

  IF prmAction = "ListSelect" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH  est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
    EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no NE 0 NO-LOCK,
      EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no 
                AND eb.form-no = ef.form-no  NO-LOCK BY eb.form-no BY eb.blank-no :
          
         vQty = display-combo-qty ().           
        CREATE ttFoldingEstimate.
        ASSIGN 
            ttFoldingEstimate.vEst           = est.est-no
            ttFoldingEstimate.vCust          = eb.cust-no
            ttFoldingEstimate.vCustPart      = eb.part-no
            ttFoldingEstimate.vShipTo        = eb.ship-id
            ttFoldingEstimate.vItemName      = eb.part-dscr1
            ttFoldingEstimate.vFgItem        = eb.stock-no
            ttFoldingEstimate.vEstQty        = vQty
            ttFoldingEstimate.vStyle         = eb.style
            ttFoldingEstimate.vPaper1         = ef.medium
            ttFoldingEstimate.vPaper2         = ef.flute
            ttFoldingEstimate.vBoard         = ef.board
            ttFoldingEstimate.vCaliper       = ef.cal
            ttFoldingEstimate.vCategory      = eb.procat
            ttFoldingEstimate.vLenght        = eb.len
            ttFoldingEstimate.vWidth         = eb.wid
            ttFoldingEstimate.vDepth         = eb.dep
            ttFoldingEstimate.vForm          = eb.form-no
            ttFoldingEstimate.vBlank         = eb.blank-no
            /*ttFoldingEstimate.vTab           =  IF eb.tab-in  = TRUE THEN "In" ELSE "Out"*/
            ttFoldingEstimate.vColor         = eb.i-col
            /*ttFoldingEstimate.vPasses        = eb.i-pass*/
            ttFoldingEstimate.vCoating       = eb.i-coat
           /* ttFoldingEstimate.vCoatPasses    = eb.i-coat-p*/
            ttFoldingEstimate.vQtySet        = eb.cust-%
            ttFoldingEstimate.vInkFrom       = ef.f-col
            ttFoldingEstimate.vPassesFrom    = ef.f-pass
            ttFoldingEstimate.vCoatingFrom   = ef.f-coat
            ttFoldingEstimate.vCoatPassesFrom = ef.f-coat-p
            ttFoldingEstimate.vPurchManuf    = IF eb.pur-man = TRUE THEN "P" ELSE "M"
            ttFoldingEstimate.vEstDate       = est.est-date

            ttFoldingEstimate.vNoW    = eb.num-wid
            ttFoldingEstimate.vNoL   = eb.num-len
            ttFoldingEstimate.vUp = eb.num-up
            ttFoldingEstimate.vDieIn    = eb.die-in 
            ttFoldingEstimate.vEstType    = est.est-type           
            .

            FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ prmComp
                                AND sys-ctrl.name    EQ 'EFBROWSE' NO-ERROR. 

           IF AVAIL sys-ctrl THEN DO:
            IF sys-ctrl.char-fld = "Std & Paper1/2" THEN
                ttFoldingEstimate.vefbrowseval = "Yes".
            ELSE
                ttFoldingEstimate.vefbrowseval = "No".
               
           END.

    END.
END.  /* end of listselect*/

IF prmAction = "Select" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH  est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
    EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom NO-LOCK,
      EACH eb WHERE eb.company = ef.company AND eb.est-no = ef.est-no 
                AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank NO-LOCK:
          
         vQty = display-combo-qty ().  
                   

        CREATE ttFoldingEstimate.
        ASSIGN 
            ttFoldingEstimate.vEst           = est.est-no
            ttFoldingEstimate.vCust          = eb.cust-no
            ttFoldingEstimate.vCustPart      = eb.part-no
            ttFoldingEstimate.vShipTo        = eb.ship-id
            ttFoldingEstimate.vItemName      = eb.part-dscr1
            ttFoldingEstimate.vFgItem        = eb.stock-no
            ttFoldingEstimate.vEstQty        = vQty
            ttFoldingEstimate.vStyle         = eb.style
            ttFoldingEstimate.vPaper1         = ef.medium
            ttFoldingEstimate.vPaper2          = ef.flute
            ttFoldingEstimate.vBoard         = ef.board
            ttFoldingEstimate.vCaliper       = ef.cal
            ttFoldingEstimate.vCategory      = eb.procat
            ttFoldingEstimate.vLenght        = eb.len
            ttFoldingEstimate.vWidth         = eb.wid
            ttFoldingEstimate.vDepth         = eb.dep
            ttFoldingEstimate.vForm          = eb.form-no
            ttFoldingEstimate.vBlank         = eb.blank-no
            /*ttFoldingEstimate.vTab           =  IF eb.tab-in  = TRUE THEN "In" ELSE "Out"*/
            ttFoldingEstimate.vColor         = eb.i-col
            /*ttFoldingEstimate.vPasses        = eb.i-pass*/
            ttFoldingEstimate.vCoating       = eb.i-coat
           /* ttFoldingEstimate.vCoatPasses    = eb.i-coat-p*/
            ttFoldingEstimate.vQtySet        = eb.cust-%
            ttFoldingEstimate.vInkFrom       = ef.f-col
            ttFoldingEstimate.vPassesFrom    = ef.f-pass
            ttFoldingEstimate.vCoatingFrom   = ef.f-coat
            ttFoldingEstimate.vCoatPassesFrom = ef.f-coat-p
            ttFoldingEstimate.vPurchManuf    = IF eb.pur-man = TRUE THEN "P" ELSE "M"
            ttFoldingEstimate.vEstDate       = est.est-date

            ttFoldingEstimate.vNoW    = eb.num-wid
            ttFoldingEstimate.vNoL   = eb.num-len
            ttFoldingEstimate.vUp = eb.num-up
            ttFoldingEstimate.vDieIn    = eb.die-in 
            ttFoldingEstimate.vEstType    = est.est-type
            ttFoldingEstimate.vRec_key    = est.rec_key 

            ttFoldingEstimate.vTlen       = eb.t-len  /*round(trunc((eb.t-len),0) + (((eb.t-len) - trunc((eb.t-len),0)) / K_FRAC),2)*/
            ttFoldingEstimate.vTwid       = eb.t-wid /*round(trunc((eb.t-wid),0) + (((eb.t-wid) - trunc((eb.t-wid),0)) / K_FRAC),2)*/
            ttFoldingEstimate.vQtyExtent[1]       = est-qty.qty[1]
            ttFoldingEstimate.vQtyExtent[2]       = est-qty.qty[2]
           ttFoldingEstimate.vQtyExtent[3]       = est-qty.qty[3]
           ttFoldingEstimate.vQtyExtent[4]       = est-qty.qty[4]
           ttFoldingEstimate.vQtyExtent[5]       = est-qty.qty[5]
           ttFoldingEstimate.vQtyExtent[6]       = est-qty.qty[6]
           ttFoldingEstimate.vQtyExtent[7]       = est-qty.qty[7]
           ttFoldingEstimate.vQtyExtent[8]       = est-qty.qty[8]
           ttFoldingEstimate.vQtyExtent[9]       = est-qty.qty[9]
           ttFoldingEstimate.vQtyExtent[10]       = est-qty.qty[10]
           ttFoldingEstimate.vQtyExtent[11]       = est-qty.qty[11]
           ttFoldingEstimate.vQtyExtent[12]       = est-qty.qty[12]
           ttFoldingEstimate.vQtyExtent[13]       = est-qty.qty[13]
           ttFoldingEstimate.vQtyExtent[14]       = est-qty.qty[14]
           ttFoldingEstimate.vQtyExtent[15]       = est-qty.qty[15]
           ttFoldingEstimate.vQtyExtent[16]       = est-qty.qty[16]
           ttFoldingEstimate.vQtyExtent[17]       = est-qty.qty[17]
           ttFoldingEstimate.vQtyExtent[18]       = est-qty.qty[18]
           ttFoldingEstimate.vQtyExtent[19]       = est-qty.qty[19]
           ttFoldingEstimate.vQtyExtent[20]       = est-qty.qty[20] .
           
           FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ prmComp
                                AND sys-ctrl.name    EQ 'EFBROWSE' NO-ERROR. 

           IF AVAIL sys-ctrl THEN DO:
            IF sys-ctrl.char-fld = "Std & Paper1/2" THEN
                ttFoldingEstimate.vefbrowseval = "Yes".
            ELSE
                ttFoldingEstimate.vefbrowseval = "No".
           END.
         
           v-count2 = 1.
           v-count = 21.
          DO WHILE v-count > 20 AND v-count < 41 :
              ASSIGN
              ttFoldingEstimate.vRelQtyExtent[v-count2]   = est-qty.qty[v-count].
              v-count = v-count + 1.
              v-count2 = v-count2 + 1.
          END.
           
            .
    END.
END.  /* end of select*/

/**********************************add blank form*********************/

IF prmAction = "viewblank" THEN DO:

    FIND FIRST est WHERE est.est-no =  FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
    FOR EACH  est-qty WHERE est-qty.est-no = est.est-no  NO-LOCK ,
    EACH ef WHERE ef.company = est-qty.company AND ef.est-no = est-qty.est-no 
                  AND ef.eqty = est-qty.eqty AND ef.form-no = prmFrom NO-LOCK:
       
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

        FIND FIRST eb
            WHERE eb.company  EQ ef.company 
            AND eb.est-no   EQ ef.est-no
            AND eb.form-no  EQ 0
            AND eb.blank-no EQ 0
            NO-LOCK NO-ERROR.
        ls-part-no = IF AVAIL eb THEN eb.part-no ELSE "".
         CREATE ttFoldingEstimate.
         
        ASSIGN
             ttFoldingEstimate.vEst   = ef.est-no
             ttFoldingEstimate.vForm  = ef.form-no
             ttFoldingEstimate.vBlank = li + 1
             ttFoldingEstimate.vCustPart  = ls-part-no + IF ls-part-no NE "" THEN ("-" + STRING(eb.form-no) + "-" + STRING(eb.blank-no)) ELSE ""
             ttFoldingEstimate.vCust  = prev-cust
             ttFoldingEstimate.vShipTo  = prev-ship
             ttFoldingEstimate.vStyle     = prev-style
             ttFoldingEstimate.vPaper1         = ef.medium
             ttFoldingEstimate.vPaper2          = ef.flute
             ttFoldingEstimate.vBoard         = ef.board
             ttFoldingEstimate.vCaliper       = ef.cal
             ttFoldingEstimate.vUp              = 1
             ttFoldingEstimate.vInkFrom       = ef.f-col
             ttFoldingEstimate.vPassesFrom    = ef.f-pass
             ttFoldingEstimate.vCoatingFrom   = ef.f-coat
             ttFoldingEstimate.vCoatPassesFrom = ef.f-coat-p
             ttFoldingEstimate.vEstDate       = est.est-date
             ttFoldingEstimate.vEstType    = est.est-type
             ttFoldingEstimate.vRec_key    = est.rec_key  
             ttFoldingEstimate.vPurchManuf    =  "M" .
    END.

END.






/**********************************************************************/


PROCEDURE create-prep :
def var i as int no-undo.

  FOR EACH est-prep
      WHERE est-prep.company EQ est.company 
        AND est-prep.est-no  EQ est.est-no                      
      USE-INDEX est-qty NO-LOCK
      BY est-prep.line DESC:
    LEAVE.
  END.
  i = (IF AVAIL est-prep THEN est-prep.line ELSE 0) + 1.
                             
  for each prep where prep.company = prmComp and prep.dfault eq yes no-lock:
      create est-prep.
      assign est-prep.e-num  = est.e-num
             est-prep.company = est.company
             est-prep.est-no = est.est-no
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
          ASSIGN
              est-prep.qty = estprpqty .
      END.
      i = i + 1.
  end.

END PROCEDURE.

PROCEDURE creat_est-op:

    find xest where recid(xest) = recid(est).
     find xef where recid(xef) = recid(ef).
     find xeb where recid(xeb) = recid(eb).

     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.line    ge 500:
       delete est-op.
     end.
    
     if can-find(first est-op where est-op.company eq xest.company
                                and est-op.est-no  eq xest.est-no
                                and est-op.s-num   eq xef.form-no) then
     for each est-op
         where est-op.company eq xest.company
           and est-op.est-no  eq xest.est-no
           and est-op.s-num   eq xef.form-no
         no-lock:
     end.
  
     else do:
       /* Protect existing est-op records */
       FOR EACH tt-est-op:
         DELETE tt-est-op.
       END.

       for each est-op
           where est-op.company eq xest.company
             and est-op.est-no  eq xest.est-no:
         CREATE tt-est-op.
         BUFFER-COPY est-op TO tt-est-op.
         DELETE est-op.
       end.
                                
       xx = dec(xef.form-no).       
       
       run ce/mach-seq.p (est-qty.eqty).

       for each est-op
           where est-op.company eq xest.company
             and est-op.est-no  eq xest.est-no
             and est-op.s-num   ne int(xx):
         delete est-op.
       end.       

       FOR EACH tt-est-op:
         CREATE est-op.
         BUFFER-COPY tt-est-op TO est-op.
         DELETE tt-est-op.
       END.
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

         RELEASE ITEM.

         IF style.material[2] NE "" THEN
            find first item where
                 item.company = eb.company and
                 item.i-no = style.material[2]
                 no-lock no-error.

         if avail item then k = integer(style.material[3]).

         RELEASE alt-item.

         IF style.material[6] NE "" THEN
            find first alt-item where
                 alt-item.company  = eb.company  and
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
 
      ASSIGN
      save_id = recid(item)
      save_id2 = recid(alt-item)
      j = (integer(prmColor)
          + integer(prmCoating))
      counter = 1
      choice = true.

      {sys/inc/roundup.i j}
      

/*    do i = 1 to 12:
       if eb.i-code2[i] ne "" then do:
          choice = no.
          leave.
       end.
      end.     
 commented to recalc every time */
  
      find bf-eb WHERE recid(bf-eb) = recid(eb) exclusive-lock.    
      if eb.i-col > 0 then assign bf-eb.i-pass = 1.
      if eb.i-coat > 0 then assign bf-eb.i-coat-p = 1.
      if choice then do i = 1 to 12:
         if i le integer(eb.i-col) then do :
              find item where recid(item) = save_id no-lock no-error.
              assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = item.i-no
                     bf-eb.i-dscr2[i] = item.i-name
                     bf-eb.i-%2[i]    = k.
         end.
         else if (i > integer(eb.i-col)) and
                 (i <= (integer(eb.i-col) + 
                       integer(eb.i-coat)) )
         then do:
              find alt-item where recid(alt-item) = save_id2 no-lock no-error.
              assign bf-eb.i-ps2[i]   = counter
                     bf-eb.i-code2[i] = if avail alt-item then alt-item.i-no else ""
                     bf-eb.i-dscr2[i] = if avail alt-item then alt-item.i-name else ""
                     bf-eb.i-%2[i]    = 100.
         end.
         else if (i >  (eb.i-col + eb.i-coat) )
         then do:
            assign bf-eb.i-ps2[i]   = 0  
                     bf-eb.i-code2[i] = ""
                     bf-eb.i-dscr2[i] = "" 
                     bf-eb.i-%2[i]    = 0.  
        
         end.
         if j <> 0 and i modulo j = 0 then counter = counter + 1.
         if counter > (eb.i-pass) then counter = eb.i-pass.         
      end. 

      {ce/updunit#.i bf-eb 0}
      {ce/updunit#.i bf-eb 1}

END PROCEDURE.


PROCEDURE calc-blank-size :

   def buffer bf-eb for eb .
   def var lv-panels as log no-undo.
   def var i as int no-undo.
   def var j as int no-undo.
   def var K_FRAC as dec init 6.25 no-undo.
   def var v-score-char like v-lscore-c  extent 12.

   find first sys-ctrl  where sys-ctrl.company eq cocode
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
      run est/u2kinc1.p (RECID(xeb)).
      run est/u2kinc2.p (RECID(xeb)).
      find bf-eb WHERE ROWID(bf-eb) EQ ROWID(eb) exclusive-lock.    
      FIND FIRST formule NO-ERROR.
      assign bf-eb.t-wid = (formule[1])
          bf-eb.t-len = (formule[2])
          bf-eb.t-sqin = (formule[7] * formule[8])
          .
      /*bf-eb.t-sqin = if v-corr then bf-eb.t-sqin * .007 else bf-eb.t-sqin / 144.
      */
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

/******************************* foldlayout***************************/

PROCEDURE calc-layout :
 
  DEF BUFFER bf-eb FOR eb.

  DEF VAR ll AS LOG NO-UNDO.


  FIND xest WHERE ROWID(xest) = ROWID(est) EXCLUSIVE-LOCK NO-ERROR.
  FIND xef  WHERE ROWID(xef)  = ROWID(ef)  EXCLUSIVE-LOCK NO-ERROR.
  FIND xeb  WHERE ROWID(xeb)  = ROWID(eb) EXCLUSIVE-LOCK NO-ERROR.

 /* ll = ip-new AND
       NOT CAN-FIND(FIRST bf-eb
                    WHERE bf-eb.company EQ eb.company
                      AND bf-eb.est-no  EQ eb.est-no
                      AND bf-eb.form-no EQ eb.form-no
                      AND ROWID(bf-eb)  NE ROWID(eb)).

  IF NOT ll THEN
    MESSAGE "Do you wish to reset layout screen?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
        UPDATE ll.*/

 /* IF ll THEN DO:
    IF NOT lv-foam THEN DO:*/
      /*{sys/inc/ceroute1.i w id l en}  */
      {ce/ceroute1.i w id l en} 
    /*END.*/
    RUN ce/calc-dim.p.
 /* END.*/

END PROCEDURE.

PROCEDURE set-yld-qty :

  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

  DEF VAR v-qty AS DEC NO-UNDO.

  DEF BUFFER b-eb  FOR eb.
  DEF BUFFER b-eb1 FOR eb.
  DEF BUFFER b-ef  FOR ef.


  FIND b-eb WHERE ROWID(b-eb) EQ ip-rowid EXCLUSIVE NO-ERROR.

  IF AVAIL b-eb THEN
  FIND FIRST b-ef OF b-eb EXCLUSIVE NO-ERROR.

  IF AVAIL b-ef THEN DO:
    IF b-eb.blank-no EQ 1 THEN
      ASSIGN
       b-eb.yld-qty = b-eb.bl-qty
       v-qty        = b-eb.yld-qty / b-eb.num-up.

    ELSE
    FOR EACH b-eb1 OF b-ef
        WHERE ROWID(b-eb1) NE ROWID(b-eb)
        BY b-eb1.yld-qty / b-eb1.num-up DESC:
      v-qty = b-eb1.yld-qty / b-eb1.num-up.
      LEAVE.
    END.

    {sys/inc/roundup.i v-qty}

    ASSIGN
     b-eb.die-in  = b-eb.die-in / b-eb.num-up
     b-eb.num-up  = TRUNC(b-eb.bl-qty / v-qty,0) +
                    INT(b-eb.bl-qty MODULO v-qty GT 0)
     b-eb.die-in  = b-eb.die-in * b-eb.num-up
     b-eb.yld-qty = v-qty * b-eb.num-up
     b-ef.die-in  = 0.

    FOR EACH b-eb1 OF b-ef NO-LOCK:
      b-ef.die-in = b-ef.die-in + b-eb1.die-in.
    END.

        
    IF b-eb.num-wid * b-eb.num-len NE b-eb.num-up THEN
      ASSIGN
       b-eb.num-wid = b-eb.num-up
       b-eb.num-len = 1.
  END.

END PROCEDURE.
