
DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-ord FOR oe-ord.

DEFINE VARIABLE dTaxRate            AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTaxRateFreight     AS DECIMAL NO-UNDO.
DEFINE VARIABLE lTaxOnFreight       LIKE oe-ctrl.f-tax INIT NO NO-UNDO.
DEFINE VARIABLE lCalledFromJC       AS LOG     NO-UNDO.
DEFINE VARIABLE lLockFirst          AS LOG     INIT TRUE NO-UNDO.
DEFINE VARIABLE dTaxRatePrep        AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTaxRatePrepFreight AS DECIMAL NO-UNDO.
DEFINE VARIABLE dTaxCalculated      AS DECIMAL NO-UNDO INIT 0.
DEFINE VARIABLE dOrderCostNew       LIKE oe-ord.t-cost NO-UNDO.
DEFINE VARIABLE dOrderRevenueNew    LIKE oe-ord.t-revenue NO-UNDO.
DEFINE VARIABLE dOrderTaxNew        LIKE oe-ord.tax NO-UNDO.
DEFINE VARIABLE dOrderWeightNew     LIKE oe-ord.t-weight NO-UNDO.
DEFINE VARIABLE dOrderFreightNew    LIKE oe-ord.t-freight NO-UNDO.

FIND oe-ord WHERE ROWID(oe-ord) EQ ipriOeOrd NO-LOCK NO-ERROR.

/*so cust is not locked when it does not need to be since
  jc-calc only updates cost*/
IF INDEX(PROGRAM-NAME(1),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(2),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(3),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(4),"jc-calc") > 0 OR
   INDEX(PROGRAM-NAME(5),"jc-calc") > 0 THEN
    lCalledFromJC = YES.

IF AVAILABLE oe-ord THEN 
DO:

    lLockFirst = YES.

    FIND FIRST oe-ctrl NO-LOCK 
        WHERE oe-ctrl.company EQ oe-ord.company 
        NO-ERROR.
    IF AVAILABLE oe-ctrl THEN 
        lTaxOnFreight = oe-ctrl.f-tax.  

    RUN ar/cctaxrt.p (oe-ord.company, oe-ord.tax-gr, OUTPUT dTaxRate, OUTPUT dTaxRateFreight).

    ASSIGN
        dOrderCostNew    = 0
        dOrderRevenueNew = 0
        dOrderTaxNew     = 0
        dOrderWeightNew  = 0
        dOrderFreightNew = 0.

    /*Recalculate Order line cost*/
    FOR EACH bf-oe-ordl OF oe-ord NO-LOCK:

        IF bf-oe-ordl.t-cost NE bf-oe-ordl.cost * (bf-oe-ordl.qty / 1000) THEN  /*Order Line Cost needs recalculated*/ 
        DO:    
            FIND oe-ordl EXCLUSIVE-LOCK 
                WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
            NO-ERROR NO-WAIT.
      
            IF NOT AVAILABLE oe-ordl THEN NEXT.
  
            oe-ordl.t-cost = oe-ordl.cost * (oe-ordl.qty / 1000).
        END.
    END.  /*Each bf-oe-ordl for cost recalculation*/

    /*Calculate new order totals from standard order lines*/
    FOR EACH oe-ordl OF oe-ord NO-LOCK:
        ASSIGN
            dOrderCostNew    = dOrderCostNew    + oe-ordl.t-cost
            dOrderRevenueNew = dOrderRevenueNew + oe-ordl.t-price
            dOrderWeightNew  = dOrderWeightNew  + oe-ordl.t-weight
            dOrderFreightNew = dOrderFreightNew + oe-ordl.t-freight.

        IF oe-ordl.tax AND dTaxRate GT 0 THEN 
        DO:

            RUN ar/calctax2.p (oe-ord.tax-gr,
                NO,
                oe-ordl.t-price,
                oe-ord.company,
                oe-ordl.i-no,
                OUTPUT dTaxCalculated).

            ASSIGN 
                dOrderTaxNew = dOrderTaxNew + dTaxCalculated.
        END.
      
    END.  /*Each oe-ordl for Order Totals Recalculation*/

    /*Add billable misc charges to new order totals*/
    FOR EACH oe-ordm OF oe-ord NO-LOCK
        WHERE oe-ordm.bill NE "N":

        ASSIGN
            dOrderRevenueNew = dOrderRevenueNew + oe-ordm.amt
            dOrderCostNew    = dOrderCostNew + oe-ordm.cost.

        RUN ar/cctaxrt.p (oe-ord.company, oe-ordm.spare-char-1,
            OUTPUT dTaxRatePrep, OUTPUT dTaxRatePrepFreight).

        IF oe-ordm.tax AND (dTaxRate > 0 OR dTaxRatePrep > 0) THEN 
        DO:

            RUN ar/calctax2.p (oe-ord.tax-gr,
                NO,
                oe-ordm.amt,
                oe-ord.company,
                oe-ordm.ord-i-no,
                OUTPUT dTaxCalculated).

            ASSIGN 
                dOrderTaxNew = dOrderTaxNew + dTaxCalculated.
        END.
    END.  /*Each oe-ordm that are billable*/

    /*Add commission and freight to new order cost*/
    ASSIGN
        dOrderCostNew = dOrderCostNew + oe-ord.t-comm
        dOrderCostNew = dOrderCostNew + dOrderFreightNew
        .
    
    /*Add billable freight and corresponding tax to New Order Totals*/
    IF oe-ord.f-bill THEN 
    DO:
        dOrderRevenueNew = dOrderRevenueNew + dOrderFreightNew.

        IF lTaxOnFreight THEN
            dOrderTaxNew = dOrderTaxNew +
                ROUND((dOrderFreightNew * dTaxRateFreight) / 100,2).
    END.

    /*Assign new order totals*/    
    IF  oe-ord.t-cost     NE dOrderCostNew      OR
        oe-ord.t-revenue  NE dOrderRevenueNew   OR
        oe-ord.tax        NE dOrderTaxNew         OR
        oe-ord.t-weight   NE dOrderWeightNew    OR
        oe-ord.t-freight  NE dOrderFreightNew  THEN 
    DO:
        FIND bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord)
            EXCLUSIVE-LOCK.
        ASSIGN
            bf-oe-ord.t-cost    = dOrderCostNew  
            bf-oe-ord.t-revenue = dOrderRevenueNew  
            bf-oe-ord.tax       = dOrderTaxNew  
            bf-oe-ord.t-weight  = dOrderWeightNew  
            bf-oe-ord.t-freight = dOrderFreightNew  .
        RELEASE bf-oe-ord.
    END.


    /*Update customer order balance*/
    IF oe-ord.cust-no NE "" AND
        lCalledFromJC EQ NO AND
        AVAILABLE cust AND 
        (oe-ord.t-revenue NE dOrderRevenueNew OR oe-ord.tax NE dOrderTaxNew) THEN
    DO:
        REPEAT:
       
            FIND FIRST cust EXCLUSIVE-LOCK 
                WHERE cust.company EQ oe-ord.company
                AND cust.cust-no EQ oe-ord.cust-no
                NO-ERROR NO-WAIT.
       
            IF AVAILABLE cust THEN
            DO:
                cust.ord-bal = cust.ord-bal - oe-ord.t-revenue - oe-ord.tax.
                cust.ord-bal = cust.ord-bal + dOrderRevenueNew + dOrderTaxNew.
                FIND CURRENT cust NO-LOCK.
                LEAVE.
            END.
       
            IF lLockFirst THEN
            DO:
                /*MESSAGE "Customer record in use, waiting for release..."
                    VIEW-AS ALERT-BOX ERROR BUTTONS OK.*/
       
                SESSION:SET-WAIT-STATE("General").
                lLockFirst = NO.
            END.
        END. /* Repeat */

        SESSION:SET-WAIT-STATE("").
    END. /* avail cust */
END. /* if avail oe-ord */
