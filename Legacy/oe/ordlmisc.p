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
def var v-tax-rate      as   dec format ">,>>9.99<<<".
def var v-frt-tax-rate  like v-tax-rate.
DEF VAR v-tmp-int AS INT NO-UNDO.
DEFINE VARIABLE hdTaxProcs AS HANDLE NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

DO TRANSACTION:
  {sys/inc/OEPrepTaxCode.i}
END.



/* ************************  Function Prototypes ********************** */
FUNCTION fGetTaxable RETURNS LOGICAL 
	( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
RUN system/TaxProcs.p PERSISTENT SET hdTaxProcs.
find first ar-ctrl {ar/ar-ctrlW.i} no-lock no-error.

find oe-ordl where ROWID(oe-ordl) eq ip-rowid no-lock no-error.
IF AVAIL oe-ordl THEN
FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
IF NOT AVAIL oe-ord THEN LEAVE.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

find first cust of oe-ord no-lock.

run ar/cctaxrt.p (input cocode, oe-ord.tax-gr,
                  output v-tax-rate, output v-frt-tax-rate).
taxit = fGetTaxable(cocode, oe-ord.cust-no, oe-ord.ship-id).

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
                    
    no-lock:

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
   oe-ordm.ord-line = oe-ordl.line.

  ASSIGN 
             oe-ordm.miscType = 1
             oe-ordm.estPrepEqty   = est-prep.eqty
             oe-ordm.estPrepLine   = est-prep.line
             oe-ordm.est-no  = est-prep.est-no. 

  if taxit then oe-ordm.tax = true.
  IF PrepTax-log THEN 
     ASSIGN oe-ordm.spare-char-1 = IF cust.spare-char-1 <> "" THEN cust.spare-char-1 ELSE oe-ord.tax-gr.
            .
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
end. /* each est-prep */

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
       oe-ordm.dscr     = "".

      
      ASSIGN 
             oe-ordm.miscType      = 2
             oe-ordm.estPrepEqty   = ef.eqty
             oe-ordm.estPrepLine   = ef.form-no
             oe-ordm.miscInd       = string(i)
             oe-ordm.est-no        = ef.est-no. 
      
      

      if taxit then oe-ordm.tax = true.
      IF PrepTax-log THEN 
         ASSIGN oe-ordm.tax = TRUE
                oe-ordm.spare-char-1 = IF cust.spare-char-1 <> "" THEN cust.spare-char-1 ELSE oe-ord.tax-gr.
                .

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
DELETE OBJECT hdTaxProcs.
RETURN.

/* **********************  Internal Procedures  *********************** */

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

  IF NOT oe-ctrl.prep-comm THEN oe-ordm.s-comm[1] = 0.

  FIND FIRST cust
      WHERE cust.company eq cocode
        AND cust.cust-no eq oe-ord.cust-no
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL cust THEN
    cust.ord-bal = cust.ord-bal + oe-ordm.amt +
                   (IF oe-ordm.tax THEN (oe-ordm.amt * v-tax-rate / 100) ELSE 0).
  FIND CURRENT cust NO-LOCK.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetTaxable RETURNS LOGICAL 
	( ipcCompany AS CHARACTER,
    ipcCust AS CHARACTER,
    ipcShipto AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lTaxable AS LOGICAL NO-UNDO.

    RUN GetTaxableMisc IN hdTaxProcs (ipcCompany, ipcCust, ipcShipto, OUTPUT lTaxable).  
    RETURN lTaxable.


		
END FUNCTION.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
