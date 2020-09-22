/* --------------------------------------------------- oe/ordlmisc.p 9/94 cd  */
/* order entry - Misc Items Update from Estimating                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER ip-qty AS INT NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest     for est.
def shared buffer xef      for ef.
def shared buffer xeb      for eb.

DEF BUFFER b-item FOR ITEM.

def new shared var uom-list as   char init ["M,EA,LOT,CASE,CS,C"].

def var v-misc-tot      as   dec format "->>>,>>>.99".
def var taxit           as   log init no.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEFINE VARIABLE dTaxAmount     AS DECIMAL format ">,>>9.99<<<" NO-UNDO.
DEFINE VARIABLE hOrderProcs AS HANDLE NO-UNDO.
RUN oe/OrderProcs.p PERSISTENT SET hOrderProcs.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/venditemcost.i}


/* ************************  Function Prototypes ********************** */
FUNCTION fGetTaxable RETURNS LOGICAL 
	( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER,
	 ipcPrepCode AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
find first ar-ctrl {ar/ar-ctrlW.i} no-lock no-error.

find oe-ordl where ROWID(oe-ordl) eq ip-rowid no-lock no-error.
IF AVAIL oe-ordl THEN
FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
IF NOT AVAIL oe-ord THEN LEAVE.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

find first cust of oe-ord no-lock.  

v-misc-tot = 0.

FOR EACH xeb
    WHERE xeb.company EQ oe-ordl.company
      AND xeb.est-no  EQ oe-ordl.est-no
      AND xeb.cust-no EQ oe-ord.cust-no
      AND xeb.form-no NE 0
      AND ((xeb.part-no EQ oe-ordl.part-no AND
            xeb.stock-no EQ oe-ordl.i-no OR xeb.stock-no EQ "") OR
           xeb.est-type EQ 2 OR xeb.est-type EQ 6)
    NO-LOCK:
    
for each est-prep
    where est-prep.company eq xeb.company
      and est-prep.est-no  eq xeb.est-no
      and est-prep.s-num   eq xeb.form-no
      and (est-prep.b-num  eq xeb.blank-no or est-prep.b-num eq 0)
      and est-prep.simon   eq "S" 
      AND est-prep.orderID EQ ""
    EXCLUSIVE-LOCK:

  FIND LAST oe-ordm NO-LOCK
      WHERE oe-ordm.company EQ oe-ord.company
        AND oe-ordm.ord-no  EQ oe-ord.ord-no
        AND oe-ordm.charge  EQ est-prep.code
        AND oe-ordm.est-no  EQ est-prep.est-no
        AND oe-ordm.estPrepEqty EQ est-prep.eqty
        AND oe-ordm.estPrepLine = est-prep.line
      NO-ERROR.
  IF AVAIL oe-ordm THEN NEXT.   
  
  find last oe-ordm of oe-ord no-lock no-error.
  z = (if avail oe-ordm then oe-ordm.line else 0) + 1.
   
  create oe-ordm.
  assign
   oe-ordm.company  = cocode
   oe-ordm.charge   = est-prep.code
   oe-ordm.ord-no   = oe-ord.ord-no
   oe-ordm.line     = z
   oe-ordm.est-no   = oe-ordl.est-no
   oe-ordm.bill     = "Y"
   oe-ordm.ord-i-no = oe-ordl.i-no
   oe-ordm.ord-line = oe-ordl.line
   oe-ordm.estPrepLine = est-prep.LINE 
   oe-ordm.form-no  = est-prep.s-num
   oe-ordm.blank-no = est-prep.b-num .
  ASSIGN 
             oe-ordm.miscType = 1
             oe-ordm.estPrepEqty   = est-prep.eqty
             oe-ordm.estPrepLine   = est-prep.line
             oe-ordm.est-no  = est-prep.est-no. 
      ASSIGN 
        oe-ordm.tax = fGetTaxable(cocode, oe-ord.cust-no, oe-ord.ship-id, oe-ordm.charge).
      IF oe-ordm.tax THEN   
        oe-ordm.spare-char-1 = oe-ord.tax-gr.
        
  assign
   oe-ordm.dscr = est-prep.dscr
   oe-ordm.cost = (est-prep.cost * est-prep.qty * (est-prep.amtz / 100)).

  IF ceprepprice-chr EQ "Profit" THEN
     oe-ordm.amt  = (est-prep.cost * est-prep.qty) / (1 - (est-prep.mkup / 100)) *
                    (est-prep.amtz / 100).
  ELSE
     oe-ordm.amt  = (est-prep.cost * est-prep.qty) * (1 + (est-prep.mkup / 100)) *
                    (est-prep.amtz / 100).

  IF ceprep-cha EQ "Dollar" THEN DO:
    {sys/inc/roundup.i oe-ordm.amt}
  END.
  IF ceprep-cha EQ "FiveDollar" THEN DO:
    {sys/inc/roundupfive.i oe-ordm.amt}
  END.

  RUN update-prep.
  est-prep.orderID = string(oe-ord.ord-no) .
end. /* each est-prep */
RELEASE est-prep .

for each ef OF xeb no-lock:
  do i = 1 to 5:
    if ef.mis-simon[i] eq "S"                                  and 
       (ef.mis-bnum[i] eq xeb.blank-no or ef.mis-bnum[i] eq 0) 
                     
                         then do:
                 
      FIND LAST oe-ordm NO-LOCK
          WHERE oe-ordm.company   EQ oe-ord.company
          AND oe-ordm.ord-no      EQ oe-ord.ord-no            
          AND oe-ordm.charge      EQ ef.mis-cost[i]
          AND oe-ordm.est-no      EQ oe-ordl.est-no
          AND oe-ordm.estPrepEqty EQ ef.eqty
          AND oe-ordm.estPrepLine = ef.form-no
          NO-ERROR.
      IF AVAIL oe-ordm THEN NEXT. 
        
                          
      z = (if avail oe-ordm then oe-ordm.line else 0) + 1.

      create oe-ordm.
      assign
       oe-ordm.company  = cocode
       oe-ordm.charge   = ef.mis-cost[i]
       oe-ordm.ord-no   = oe-ord.ord-no
       oe-ordm.line     = z
       oe-ordm.est-no   = oe-ordl.est-no
       oe-ordm.bill     = "Y"
       oe-ordm.ord-i-no = oe-ordl.i-no
       oe-ordm.ord-line = oe-ordl.line
       oe-ordm.dscr     = ""
       oe-ordm.form-no  = ef.form-no
       oe-ordm.blank-no = xeb.blank-no .

      
      ASSIGN 
             oe-ordm.miscType      = 2
             oe-ordm.estPrepEqty   = ef.eqty
             oe-ordm.estPrepLine   = ef.form-no
             oe-ordm.miscInd       = string(i)
             oe-ordm.est-no        = ef.est-no. 
      
      ASSIGN 
        oe-ordm.tax = fGetTaxable(cocode, oe-ord.cust-no, oe-ord.ship-id, oe-ordm.charge).
        IF oe-ordm.tax THEN 
            oe-ordm.spare-char-1 = oe-ord.tax-gr.

      IF ceprepprice-chr EQ "Profit" THEN
         oe-ordm.amt  = (ef.mis-labf[i] + ef.mis-matf[i] +
                        ((ef.mis-labm[i] + ef.mis-matm[i]) * (ip-qty / 1000))) /
                        (1 - (ef.mis-mkup[i] / 100)).
      ELSE
         oe-ordm.amt  = (ef.mis-labf[i] + ef.mis-matf[i] +
                        ((ef.mis-labm[i] + ef.mis-matm[i]) * (ip-qty / 1000))) *
                        (1 + (ef.mis-mkup[i] / 100)).

      IF ceprep-cha EQ "Dollar" THEN DO:
        {sys/inc/roundup.i oe-ordm.amt}
      END. 
      ELSE IF ceprep-cha EQ "FiveDollar" THEN DO:
        {sys/inc/roundupfive.i oe-ordm.amt}
      END.

      RUN update-prep.
    end.
  end.
end. /* each ef */
END. /* each xeb */

 IF VALID-HANDLE(hOrderProcs) THEN 
 DELETE OBJECT hOrderProcs.
 
RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE CreateVendItemCost:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendorUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS decimal NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS decimal NO-UNDO.
/*    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.    */
/*    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.*/

    FIND FIRST vendItemCost WHERE vendItemCost.company = ipcCompany
                              AND vendItemCost.itemID = ipcItemID
                              AND vendItemCost.itemType = ipcItemType
                              NO-ERROR.
    IF NOT AVAIL vendItemCost THEN DO:
       CREATE vendItemCost.
       ASSIGN vendItemCost.Company = ipcCompany
              vendItemCost.ItemID = ipcItemID
              vendItemCost.itemType = ipcItemType
              . 
    END.
    vendItemCost.VendorUOM = ipcVendorUOM .
    FIND FIRST vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID NO-ERROR.
    IF NOT AVAIL vendItemCostLevel THEN DO:
       CREATE vendItemCostLevel.
       ASSIGN vendItemCostLevel.vendItemCostId = vendItemCost.vendItemCostID
              . 
    END.    
    ASSIGN vendItemCostLevel.quantityBase = ipdQty
           vendItemCostLevel.costPerUOM = ipdCost
           .
                                                    

END PROCEDURE.

PROCEDURE update-prep.
  ASSIGN
   v-misc-tot = v-misc-tot + oe-ordm.amt
   oe-ordm.ord-i-no = oe-ordl.job-no
   oe-ordm.ord-line = oe-ordl.job-no2.


  IF AVAIL ar-ctrl THEN oe-ordm.actnum = ar-ctrl.sales.

  ASSIGN
   oe-ordm.s-man[1]  = xeb.sman
   oe-ordm.s-pct[1]  = 100
   oe-ordm.s-comm[1] = xeb.comm.

  FIND FIRST prep
      {ce/prepW.i}
        AND prep.code EQ oe-ordm.charge
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL prep THEN DO:
  
    RUN pDisplayPrepDisposedMessage IN hOrderProcs (ROWID(prep)).
    
    oe-ordm.charge = prep.code.
    IF oe-ordm.dscr EQ "" THEN oe-ordm.dscr  = prep.dscr.
    IF prep.actnum NE "" THEN oe-ordm.actnum = prep.actnum.

    IF oe-ordm.s-man[1] NE "" THEN
      RUN sys/inc/getsmncm.p (oe-ord.cust-no,
                              INPUT-OUTPUT oe-ordm.s-man[1],
                              prep.fgcat,
                              0,
                              OUTPUT oe-ordm.s-comm[1]).

    IF prep.i-no EQ "" THEN prep.i-no = prep.code.

    /* gdm - 06170905 */
    ASSIGN prep.last-date   = oe-ord.ord-date
           prep.last-order  = oe-ordm.ord-no
           prep.last-est-no = oe-ordm.est-no.

    /*FIND FIRST reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable EQ "PREPLASTJOB"
          AND reftable.company  EQ prep.company 
          AND reftable.loc      EQ prep.loc     
          AND reftable.code     EQ prep.CODE NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN reftable.reftable = "PREPLASTJOB"
                 reftable.company  = prep.company
                 reftable.loc      = prep.loc
                 reftable.code     = prep.CODE 
                 reftable.code2    = oe-ordl.job-no
                 reftable.val[1]   = oe-ordl.job-no2.
      END.
      ELSE
      IF AVAIL reftable THEN DO:

          ASSIGN reftable.code2    = oe-ordl.job-no
                 reftable.val[1]   = oe-ordl.job-no2.
      END.*/
    /* gdm - 06170905 end */

    IF NOT CAN-FIND(FIRST item
                    WHERE item.company EQ cocode
                      AND item.i-no    EQ prep.i-no) THEN DO:
      CREATE item.
      ASSIGN
       item.company   = cocode
       item.loc       = locode
       item.i-no      = prep.i-no
       item.i-name    = prep.dscr
       item.i-code    = "R"
       item.cost-type = prep.cost-type
       item.cons-uom  = prep.uom
       item.pur-uom   = prep.uom
       item.industry  = IF xeb.est-type LE 4 THEN "1" ELSE "2".

      IF lNewVendorItemCost THEN RUN CreateVendItemCost (ITEM.company, ITEM.i-no,"RM", ITEM.pur-uom,1, prep.cost ).
      ELSE DO:
          FIND FIRST e-item
              WHERE e-item.company EQ item.company
                AND e-item.i-no    EQ item.i-no
              NO-ERROR.
          IF NOT AVAIL e-item THEN DO:
            CREATE e-item.
            ASSIGN
             e-item.company = item.company
             e-item.i-no    = item.i-no.
          END.
          e-item.std-uom = item.pur-uom.
    
          FIND FIRST e-item-vend
              WHERE e-item-vend.company   EQ item.company
                AND e-item-vend.item-type EQ YES
                AND e-item-vend.i-no      EQ item.i-no
                AND e-item-vend.vend-no   EQ ""
              NO-ERROR.
          IF NOT AVAIL e-item-vend THEN DO:
            CREATE e-item-vend.
            ASSIGN
             e-item-vend.company   = item.company
             e-item-vend.item-type = YES
             e-item-vend.i-no      = item.i-no
             e-item-vend.vend-no   = "".
          END.
          ASSIGN
           e-item-vend.std-uom     = item.pur-uom
           e-item-vend.run-qty[1]  = 1
           e-item-vend.run-cost[1] = prep.cost.
      END.
      
      CASE prep.mat-type:
        WHEN 'B' THEN item.mat-type = 'O'.
        WHEN 'D' THEN item.mat-type = '8'.
        WHEN 'F' THEN item.mat-type = 'Y'.
        WHEN 'P' THEN item.mat-type = '7'.
        WHEN 'R' THEN item.mat-type = 'X'.
        OTHERWISE     item.mat-type = 'M'.
      END CASE.

      FOR EACH b-item NO-LOCK
          WHERE b-item.company  EQ item.company
            AND b-item.mat-type EQ item.mat-type
            AND b-item.procat   NE ""
            AND ROWID(b-item)   NE ROWID(item)
          USE-INDEX mat-type
          BY RECID(b-item):
        item.procat = b-item.procat.
        LEAVE.
      END.
             
      RELEASE item.
    END.
  END.

  IF NOT prep.commissionable THEN oe-ordm.s-comm[1] = 0.

  FIND FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq oe-ord.cust-no
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL cust THEN do:
    dTaxAmount = 0.     
    RUN Tax_Calculate(INPUT cocode,
                      INPUT oe-ord.tax-gr,
                      INPUT FALSE,
                      INPUT oe-ordm.amt,
                      INPUT "",
                      OUTPUT dTaxAmount).
    cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                   (IF oe-ordm.tax THEN (dTaxAmount) ELSE 0).
   END.                
  FIND CURRENT cust NO-LOCK.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetTaxable RETURNS LOGICAL 
	( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER,
    ipcPrepCode AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

    RUN Tax_GetTaxableMisc  (ipcCompany, ipcCust, ipcShipto, ipcPrepCode, OUTPUT lTaxable).  
    

    RETURN lTaxable.


		
END FUNCTION.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
