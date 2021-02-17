
/*------------------------------------------------------------------------
    File        : OrderProcs.p
    Purpose     : Centralization of numerous common functions in the import and manual entry of orders

    Syntax      :

    Description : Holds procedures for entering, editing and processing orders

    Author(s)   : BV
    Created     : Tue Jun 04 13:53:09 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
USING system.SharedConfig.

DEFINE VARIABLE hdOeValidate AS HANDLE.
{custom/globdefs.i}     /*Refactor - hate this*/
{sys/inc/var.i SHARED}  /*Refactor - hate this*/
{oe/relcheck.i NEW} /* temp-table definitions */
DEFINE VARIABLE glUseTransCust      AS LOGICAL   NO-UNDO. /* how to set cust no on release */
/*DEFINE VARIABLE glCheckCredit       AS LOGICAL   NO-UNDO. /* check credit hold */*/
DEFINE VARIABLE gcDefaultLocMethod  AS CHARACTER NO-UNDO. /* how to get default loc */
DEFINE VARIABLE glRecalcSkippedInTrigger   AS LOGICAL   NO-UNDO. /* not used */
DEFINE VARIABLE gcOnOrderQtyCode    LIKE sys-ctrl.char-fld NO-UNDO. /* determines if on order quantity used */  
DEFINE VARIABLE gcTagSelectionCode  AS CHARACTER NO-UNDO. /* Tag selection code */
DEFINE VARIABLE glUseItemfgLoc      AS LOGICAL   NO-UNDO. /* Get location from itemfg? */
DEFINE VARIABLE gcCompanyDefaultBin AS CHARACTER NO-UNDO.  /* default bin */
DEFINE VARIABLE cFreightCalculationValue AS CHARACTER NO-UNDO.

DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.

DEFINE VARIABLE gcCaseUOMList AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttUpdateOrderReleaseStatus NO-UNDO 
  FIELD ord-no AS INTEGER.

{oe/ttOrder.i}

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetOEAutoApprovalLog RETURNS LOGICAL 
	(ipcCompany AS CHARACTER, ipcCustomerID AS CHARACTER, ipcShipToID AS CHARACTER) FORWARD.

FUNCTION fGetSettingJobCreate RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetNextOrderNo RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetNextShipNo RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcCustNo AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
{cXML/cXMLDefs.i}
{oe/getprice.i}

PROCEDURE CalcOrderCommission:
    /*------------------------------------------------------------------------------
     Purpose: Given an order, calculates commission related information
     Notes: Replaces oe/oe-comm.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm FOR oe-ordm.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-prep    FOR prep.

    DEFINE VARIABLE iSman            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProductCat      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostExtended    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCommissionBasis AS CHARACTER NO-UNDO.

    FIND FIRST bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ipriRowid EXCLUSIVE-LOCK.


    FIND FIRST bf-cust NO-LOCK 
        WHERE bf-cust.company EQ bf-oe-ord.company
        AND bf-cust.cust-no EQ bf-oe-ord.cust-no
        NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN 
            cCustID   = bf-cust.cust-no
            cCustType = bf-cust.type
            . 
    
    bf-oe-ord.t-comm = 0.
    FOR EACH bf-oe-ordl OF bf-oe-ord NO-LOCK:
        dCostExtended = (bf-oe-ordl.qty / 1000) * bf-oe-ordl.cost.

        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ bf-oe-ordl.company
            AND bf-itemfg.i-no    EQ bf-oe-ordl.i-no
            NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                cProductCat = bf-itemfg.procat
                .
        DO iSman = 1 TO 3:
            IF bf-oe-ordl.s-man[iSman] NE "" THEN 
            DO:
                RUN custom/combasis.p (bf-oe-ord.company, bf-oe-ordl.s-man[iSman], cCustType, cProductCat, 0, cCustID, OUTPUT cCommissionBasis).

                bf-oe-ord.t-comm = bf-oe-ord.t-comm +
                    ROUND((bf-oe-ordl.t-price - IF cCommissionBasis EQ "G" THEN dCostExtended ELSE 0) *
                    (bf-oe-ordl.s-pct[iSman] / 100) * (bf-oe-ordl.s-comm[iSman] / 100),2).
            END.
        END.
    END.

    FOR EACH bf-oe-ordm OF bf-oe-ord NO-LOCK, 
        FIRST bf-prep NO-LOCK
        WHERE bf-prep.company EQ bf-oe-ordm.company
        AND bf-prep.code    EQ bf-oe-ordm.charge
        AND bf-prep.commissionable:
             
        DO iSman = 1 TO 3:
            IF bf-oe-ordm.s-man[iSman] NE "" THEN 
            DO:
                RUN custom/combasis.p (bf-oe-ord.company, bf-oe-ordm.s-man[iSman], cCustType, bf-prep.fgcat, 0, cCustID, OUTPUT cCommissionBasis).

                bf-oe-ord.t-comm = bf-oe-ord.t-comm +
                    ROUND((bf-oe-ordm.amt - IF cCommissionBasis EQ "G" THEN bf-oe-ordm.cost ELSE 0) *
                    (bf-oe-ordm.s-pct[iSman] / 100) * (bf-oe-ordm.s-comm[iSman] / 100),2).
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE GetCaseUOMList PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCaseUOMList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.

    IF gcCaseUOMList EQ "" THEN
        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany, 
            INPUT  "CaseUOMList", 
            INPUT  "C"              /* Logical */, 
            INPUT  NO               /* check by cust */, 
            INPUT  YES              /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
            OUTPUT gcCaseUomList, 
            OUTPUT lRecFound
            ).  
    
    opcCaseUOMList = gcCaseUOMList.
END PROCEDURE.

PROCEDURE GetOEImportConsol:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID       AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplOEImportConsol AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE cRtnChar  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany, 
        INPUT  "OEImportConsol", 
        INPUT  "L",  /* LOGICAL */
        INPUT  YES,  /* check by cust */
        INPUT  YES,  /* use cust not vendor */
        INPUT  ipcCustomerID, /* cust */
        INPUT  ipcShipToID, /* ship-to*/
        OUTPUT cRtnChar,
        OUTPUT lRecFound
        ).
    
    IF lRecFound THEN
        oplOEImportConsol = LOGICAL(cRtnChar).
END PROCEDURE.

PROCEDURE Order_CalculateOrderTotal:
/*------------------------------------------------------------------------------
 Purpose: Calculates the order balance change for a given order and updates 
          cust order balance
 Notes:   Orignal Source - oe/calordt.p, Deprecates - oe/calcordt.p
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd           AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdateOrdBalance AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-cust    FOR cust.
    
    DEFINE VARIABLE lCalledFromJC       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dTaxCalculated      AS DECIMAL NO-UNDO INIT 0.
    DEFINE VARIABLE dOrderCostNew       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderRevenueNew    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderTaxNew        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderWeightNew     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderFreightNew    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderRevenueOld    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderTaxOld        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOrderBalanceChange AS DECIMAL NO-UNDO.
    
    FIND FIRST oe-ord NO-LOCK
         WHERE ROWID(oe-ord) EQ ipriOeOrd 
         NO-ERROR.

    /*so cust is not locked when it does not need to be since
      jc-calc only updates cost*/
    IF INDEX(PROGRAM-NAME(1),"jc-calc") > 0 OR
       INDEX(PROGRAM-NAME(2),"jc-calc") > 0 OR
       INDEX(PROGRAM-NAME(3),"jc-calc") > 0 OR
       INDEX(PROGRAM-NAME(4),"jc-calc") > 0 OR
       INDEX(PROGRAM-NAME(5),"jc-calc") > 0 THEN
        lCalledFromJC = YES.

    IF AVAILABLE oe-ord THEN DO:
        ASSIGN
            dOrderCostNew    = 0
            dOrderRevenueNew = 0
            dOrderTaxNew     = 0
            dOrderWeightNew  = 0
            dOrderFreightNew = 0
            dOrderRevenueOld = 0
            dOrderTaxOld     = 0
            dOrderBalanceChange = 0.
    
        /*Recalculate Order line cost*/
        FOR EACH bf-oe-ordl OF oe-ord NO-LOCK:
    
            IF bf-oe-ordl.t-cost NE bf-oe-ordl.cost * (bf-oe-ordl.qty / 1000) THEN DO:  /*Order Line Cost needs recalculated*/    
                FIND oe-ordl EXCLUSIVE-LOCK 
                     WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
                     NO-ERROR NO-WAIT.
          
                IF NOT AVAILABLE oe-ordl OR LOCKED oe-ordl THEN 
                    NEXT.
      
                oe-ordl.t-cost = oe-ordl.cost * (oe-ordl.qty / 1000).
            END.
        END.  /*Each bf-oe-ordl for cost recalculation*/
    
        /*Calculate new order totals from standard order lines*/
        FOR EACH oe-ordl OF oe-ord NO-LOCK:
            ASSIGN
                dOrderCostNew    = dOrderCostNew    + oe-ordl.t-cost
                dOrderRevenueNew = dOrderRevenueNew + oe-ordl.t-price
                dOrderWeightNew  = dOrderWeightNew  + oe-ordl.t-weight
                dOrderFreightNew = dOrderFreightNew + oe-ordl.t-freight
                .
    
            IF oe-ordl.tax THEN DO:
                RUN Tax_Calculate  (
                    INPUT  oe-ord.company,
                    INPUT  oe-ord.tax-gr,
                    INPUT  FALSE,   /* Is this freight */
                    INPUT  oe-ordl.t-price,
                    INPUT  oe-ordl.i-no,
                    OUTPUT dTaxCalculated
                    ). 
                dOrderTaxNew = dOrderTaxNew + dTaxCalculated.
            END.   
        END.  /*Each oe-ordl for Order Totals Recalculation*/
    
        /*Add billable misc charges to new order totals*/
        FOR EACH oe-ordm OF oe-ord NO-LOCK
            WHERE oe-ordm.bill NE "N":
    
            ASSIGN
                dOrderRevenueNew = dOrderRevenueNew + oe-ordm.amt
                dOrderCostNew    = dOrderCostNew    + oe-ordm.cost
                .
    
            IF oe-ordm.tax THEN DO:
                RUN Tax_Calculate  (
                    INPUT  oe-ord.company,
                    INPUT  oe-ord.tax-gr,
                    INPUT  FALSE,   /* Is this freight */
                    INPUT  oe-ordm.amt,
                    INPUT  oe-ordm.ord-i-no,
                    OUTPUT dTaxCalculated
                    ). 
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
            dOrderRevenueNew = dOrderRevenueNew + dOrderFreightNew.
    
        /*Assign new order totals*/    
        IF  oe-ord.t-cost     NE dOrderCostNew    OR
            oe-ord.t-revenue  NE dOrderRevenueNew OR
            oe-ord.tax        NE dOrderTaxNew     OR
            oe-ord.t-weight   NE dOrderWeightNew  OR
            oe-ord.t-freight  NE dOrderFreightNew THEN DO:
                
            ASSIGN 
                dOrderRevenueOld = oe-ord.t-revenue
                dOrderTaxOld     = oe-ord.tax
                .
                
            FIND FIRST bf-oe-ord EXCLUSIVE-LOCK
                 WHERE ROWID(bf-oe-ord) EQ ROWID(oe-ord)
                 NO-ERROR.
                 
            ASSIGN
                bf-oe-ord.t-cost    = dOrderCostNew  
                bf-oe-ord.t-revenue = dOrderRevenueNew  
                bf-oe-ord.tax       = dOrderTaxNew  
                bf-oe-ord.t-weight  = dOrderWeightNew  
                bf-oe-ord.t-freight = dOrderFreightNew  
                .
                
            RELEASE bf-oe-ord.
            dOrderBalanceChange = dOrderRevenueNew - dOrderRevenueOld + dOrderTaxNew - dOrderTaxOld.
        END.
        
        /*Update customer order balance*/
        IF oe-ord.cust-no           NE "" 
            AND lCalledFromJC       EQ NO 
            AND dOrderBalanceChange NE 0 
            AND iplUpdateOrdBalance THEN DO:
                
            FIND FIRST bf-cust EXCLUSIVE-LOCK 
                 WHERE bf-cust.company EQ oe-ord.company
                   AND bf-cust.cust-no EQ oe-ord.cust-no
                 NO-ERROR.
       
            IF AVAILABLE bf-cust THEN DO:
                bf-cust.ord-bal = bf-cust.ord-bal + dOrderBalanceChange.
                RELEASE bf-cust.
            END.
        END. /* avail cust */
    END. /* if avail oe-ord */
END PROCEDURE.

PROCEDURE Order_CallCreateReleaseTrigger:
/*------------------------------------------------------------------------------
 Purpose: Generice procedure to call create release trigger for sendRelease,
          sendCustomer and sendFinishedGood API
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRNoValues   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iNumValues   AS INTEGER NO-UNDO.

    DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.

    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

    ASSIGN 
        scInstance = SharedConfig:instance. 
        cRNoValues= scInstance:ConsumeValue("RNoOERelh")
        .    
    DO iNumValues = 1 TO NUM-ENTRIES(cRNoValues,"|"):
        FIND FIRST bf-oe-relh NO-LOCK 
             WHERE bf-oe-relh.r-no EQ INTEGER(ENTRY(iNumValues,cRNoValues,"|"))
             NO-ERROR.
        IF AVAILABLE bf-oe-relh THEN DO: 
            FOR EACH bf-oe-rell NO-LOCK 
                WHERE bf-oe-rell.company EQ bf-oe-relh.company
                  AND bf-oe-rell.r-no    EQ bf-oe-relh.r-no,
                FIRST bf-itemfg NO-LOCK 
                WHERE bf-itemfg.company EQ bf-oe-rell.company
                  AND bf-itemfg.i-no    EQ bf-oe-rell.i-no  
                BREAK BY bf-oe-rell.r-no
                      BY bf-oe-rell.i-no:
                IF FIRST-OF(bf-oe-rell.r-no) THEN DO:
                    ASSIGN 
                        cAPIID       = "SendRelease"
                        cPrimaryID   = STRING(bf-oe-relh.release#)
                        cDescription = cAPIID + " triggered by CreateRelease"  
                                     + " from OrderProcs.p for Release: " + cPrimaryID
                        . 
                    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs  (
                        INPUT  bf-oe-relh.company,        /* Company Code (Mandatory) */
                        INPUT  bf-oe-rell.loc,            /* Location Code (Mandatory) */
                        INPUT  cAPIID,                    /* API ID (Mandatory) */
                        INPUT  bf-oe-relh.cust-no,        /* Scope ID*/
                        INPUT  "Customer",                /* Scope Type */
                        INPUT  "CreateRelease",           /* Trigger ID (Mandatory) */
                        INPUT  "oe-relh",                 /* Comma separated list of table names for which data being sent (Mandatory) */
                        INPUT  STRING(ROWID(bf-oe-relh)), /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                        INPUT  cPrimaryID,                /* Primary ID for which API is called for (Mandatory) */   
                        INPUT  cDescription,              /* Event's description (Optional) */
                        OUTPUT lSuccess,                  /* Success/Failure flag */
                        OUTPUT cMessage                   /* Status message */
                        ) NO-ERROR.
                    FIND FIRST bf-cust NO-LOCK
                         WHERE bf-cust.company EQ bf-oe-relh.company
                           AND bf-cust.cust-no EQ bf-oe-relh.cust-no
                         NO-ERROR.
                    IF AVAILABLE bf-cust THEN DO:
                        ASSIGN  
                            cAPIId       = "SendCustomer"
                            cPrimaryID   = bf-cust.cust-no
                            cDescription = cAPIID + " triggered by CreateRelease from OrderProcs.p for Customer: " + cPrimaryID
                            .
                        RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs  (
                            INPUT  bf-oe-relh.company,        /* Company Code (Mandatory) */
                            INPUT  bf-oe-rell.loc,            /* Location Code (Mandatory) */
                            INPUT  cAPIID,                    /* API ID (Mandatory) */
                            INPUT  bf-oe-relh.cust-no,        /* Scope ID*/
                            INPUT  "Customer",                /* Scope Type */
                            INPUT  "CreateRelease",           /* Trigger ID (Mandatory) */
                            INPUT  "cust",                    /* Comma separated list of table names for which data being sent (Mandatory) */
                            INPUT  STRING(ROWID(bf-cust)),    /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                            INPUT  cPrimaryID,                /* Primary ID for which API is called for (Mandatory) */   
                            INPUT  cDescription,              /* Event's description (Optional) */
                            OUTPUT lSuccess,                  /* Success/Failure flag */
                            OUTPUT cMessage                   /* Status message */
                            ) NO-ERROR.
                    END. /*avail bf-cust*/ 
                END. /* IF avail first-of(r-no) */
                IF FIRST-OF(bf-oe-rell.i-no) THEN DO:
                    ASSIGN 
                        cAPIId       = "SendFinishedGood"
                        cPrimaryID   = bf-itemfg.i-no
                        cDescription = cAPIID + " triggered by CreateRelease" 
                                     + " from OrderProcs.p for FG Item: " + cPrimaryID
                        .
                    RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs  (
                        INPUT  bf-oe-relh.company,        /* Company Code (Mandatory) */
                        INPUT  bf-oe-rell.loc,            /* Location Code (Mandatory) */
                        INPUT  cAPIID,                    /* API ID (Mandatory) */
                        INPUT  bf-oe-relh.cust-no,        /* Scope ID*/
                        INPUT  "Customer",                /* Scope Type */
                        INPUT  "CreateRelease",           /* Trigger ID (Mandatory) */
                        INPUT  "itemfg",                  /* Comma separated list of table names for which data being sent (Mandatory) */
                        INPUT  STRING(ROWID(bf-itemfg)),  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                        INPUT  cPrimaryID,                /* Primary ID for which API is called for (Mandatory) */   
                        INPUT  cDescription,              /* Event's description (Optional) */
                        OUTPUT lSuccess,                  /* Success/Failure flag */
                        OUTPUT cMessage                   /* Status message */
                        ) NO-ERROR.
                END. /*First bf-oe-rell.i-no*/                
            END.
           
            RELEASE bf-oe-relh.                                                                         
        END.  
        RUN Outbound_ResetContext IN hdOutboundProcs.  
    END.  
    
    FINALLY:
        IF VALID-HANDLE(hdOutboundProcs) THEN
            DELETE PROCEDURE hdOutboundProcs.
    END.
END PROCEDURE.

PROCEDURE Order_GetLinesTotal:
/*------------------------------------------------------------------------------
 Purpose: Returns Order lines total including freight and tax
 Notes:   Orignal Source - ar/updCust1.p
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderNo         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxGroup        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdOrderLinesTotal AS DECIMAL   NO-UNDO.
        
    DEFINE VARIABLE dLineQuantity AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLinePrice    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTaxAmount    AS DECIMAL NO-UNDO.
    
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ ipcCompany
          AND oe-ordl.ord-no  EQ ipiOrderNo:
    
        dLineQuantity = 0.
    
        IF oe-ordl.stat EQ "C" THEN 
            dLineQuantity = oe-ordl.qty.
    
        ELSE
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.company EQ oe-ordl.company
                  AND ar-invl.ord-no  EQ oe-ordl.ord-no
                  AND ar-invl.i-no    EQ oe-ordl.i-no
                USE-INDEX ord-no:
                  dLineQuantity = dLineQuantity + ar-invl.inv-qty.
            END.
    
        dLineQuantity = IF dLineQuantity LT oe-ordl.qty THEN dLineQuantity / oe-ordl.qty ELSE 1.
    
        IF dLineQuantity GT 0 THEN DO:
            ASSIGN
               dLinePrice         = oe-ordl.t-price * dLineQuantity
               opdOrderLinesTotal = opdOrderLinesTotal + dLinePrice
               dTaxAmount         = 0
               .
                           
            IF oe-ordl.tax THEN DO:
                RUN Tax_Calculate(
                    INPUT  ipcCompany, 
                    INPUT  ipcTaxGroup,
                    INPUT  FALSE,
                    INPUT  dLinePrice,
                    INPUT  oe-ordl.i-no, 
                    OUTPUT dTaxAmount
                    ).    
                opdOrderLinesTotal = opdOrderLinesTotal + ROUND(dTaxAmount,2).
            END. /* End of oe-ordl.tax */
            IF oe-ord.f-bill THEN
                opdOrderLinesTotal = opdOrderLinesTotal + (oe-ordl.t-freight * dLineQuantity).
        END. /* End of IF dLineQuantity GT 0  */
    END. /* End of For Each oe-ordl */
END PROCEDURE.

PROCEDURE Order_GetMiscAmountAndTax:
/*------------------------------------------------------------------------------
 Purpose: Returns Misc amount and Tax
 Notes:   Orignal Source - ar/updcust1.p
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTaxGroup   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMiscAmount AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdMiscTax    AS DECIMAL   NO-UNDO.
    
    DEFINE VARIABLE dTaxAmount AS DECIMAL NO-UNDO.
    
    FOR EACH oe-ordm NO-LOCK
        WHERE oe-ordm.company EQ ipcCompany
          AND oe-ordm.ord-no  EQ ipiOrderNo
          AND oe-ordm.bill    EQ "I":
    
        ASSIGN 
            opdMiscAmount = opdMiscAmount + oe-ordm.amt
            dTaxAmount    = 0
            .
        IF oe-ordm.tax THEN DO:    
            RUN Tax_Calculate(
                INPUT  ipcCompany, 
                INPUT  ipcTaxGroup,
                INPUT  FALSE,
                INPUT  oe-ordm.amt,
                INPUT  oe-ordm.charge, 
                OUTPUT dTaxAmount
                ).  
            opdMiscTax = opdMiscTax + ROUND(dTaxAmount,2).
        END.  /* End of IF oe-ordm.tax */
    END. /* End of oe-ordm */ 
END PROCEDURE.

PROCEDURE pConsolidateImportedOrderLines PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Consolidates the order lines if duplicates are found
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipiOrderSeqID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcShipToID   AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrderLine.
    
    DEFINE VARIABLE lOEImportConsol AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-ttOrderLine FOR ttOrderLine.
    
    RUN GetOEImportConsol (
        INPUT  ipcCompany,
        INPUT  ipcCustomerID,
        INPUT  ipcShipToID,
        OUTPUT lOEImportConsol    
        ).
    
    IF lOEImportConsol THEN DO:
        /* Consolidate and delete line only if action is create */
        FOR EACH ttOrderLine 
            WHERE ttOrderLine.orderSeqID EQ ipiOrderSeqID
              AND ttOrderLine.action     EQ cOrderActionCreate
            BY ttOrderLine.lineNo:
            FOR EACH bf-ttOrderLine
                WHERE bf-ttOrderLine.orderSeqID         EQ ttOrderLine.orderSeqID
                  AND bf-ttOrderLine.manufacturerPartID EQ ttOrderLine.manufacturerPartID
                  AND bf-ttOrderLine.lineNo             NE ttOrderLine.lineNo
                  AND bf-ttOrderLine.action             EQ cOrderActionCreate:
                ttOrderLine.quantity = ttOrderLine.quantity + bf-ttOrderLine.quantity.
                DELETE bf-ttOrderLine.
            END.
        END.        
    END.
END PROCEDURE.

PROCEDURE Order_CreateMiscChargeByDeliveryDate:
/*------------------------------------------------------------------------------
 Purpose: Creates a misc charge record for the order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID             AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDate          AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate  AS DATETIME  NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pCreateMiscChargeByDeliveryDate(
        INPUT  ipcCompany,
        INPUT  ipiOrderID,
        INPUT  ipdtOrderDate,
        INPUT  ipdtOrderDeliveryDate,
        OUTPUT lError,
        OUTPUT cMessage        
        ).
END.

PROCEDURE pCreateMiscChargeByDeliveryDate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates a misc charge record for the order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID             AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDate          AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate  AS DATETIME  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError               AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cSurchargeConfigList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSurchargeConfig     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrepCode            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound               AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    
    RUN pGetSurchargeConfig (
        INPUT  ipdtOrderDate,
        INPUT  ipdtOrderDeliveryDate, 
        OUTPUT cSurchargeConfigList
        ).
    
    cSurchargeConfig = ENTRY(1, cSurchargeConfigList).
    
    IF cSurchargeConfig EQ "" THEN
        RETURN.
    
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipcCompany
           AND bf-oe-ord.ord-no  EQ ipiOrderID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN DO:
        ASSIGN
            oplError   = TRUE 
            opcMessage = "Invalid Order #" + STRING(ipiOrderID)
            .     
        RETURN.
    END.

    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,
        INPUT  cSurchargeConfig,
        INPUT  "C",
        INPUT  YES,
        INPUT  YES,
        INPUT  bf-oe-ord.cust-no,
        INPUT  bf-oe-ord.ship-id,
        OUTPUT cPrepCode,
        OUTPUT lFound
        ).
    IF NOT lFound OR cPrepCode EQ "" THEN
        RETURN.

    RUN pCreateMiscSurcharge (
        INPUT  ipcCompany,
        INPUT  ipiOrderID,
        INPUT  cPrepCode,
        OUTPUT oplError,
        OUTPUT opcMessage
        ).
        
END PROCEDURE.

PROCEDURE pCreateMiscSurcharge PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates a oe-ordm record for a given order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrepCode AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplError    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iLineCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dMarkUp    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cPrepPrice AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm  FOR oe-ordm.
    DEFINE BUFFER bf-ar-ctrl  FOR ar-ctrl.
    DEFINE BUFFER bf-prep     FOR prep.
    DEFINE BUFFER bf-est-prep FOR est-prep.
    DEFINE BUFFER bf-account  FOR account. 

    MAIN-BLOCK:
    DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
        FIND FIRST bf-oe-ord NO-LOCK
             WHERE bf-oe-ord.company EQ ipcCompany
               AND bf-oe-ord.ord-no  EQ ipiOrderID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-ord THEN DO:
            ASSIGN
                oplError   = TRUE 
                opcMessage = "Invalid Order #" + STRING(ipiOrderID)
                .     
            RETURN.
        END.
        
        FIND FIRST bf-prep NO-LOCK
             WHERE bf-prep.company EQ bf-oe-ord.company
               AND bf-prep.code    EQ ipcPrepCode
             NO-ERROR.
        IF NOT AVAILABLE bf-prep THEN DO:
            ASSIGN
                oplError   = TRUE 
                opcMessage = "Invalid Prep code " + STRING(ipcPrepCode)
                .     
            RETURN.
        END.

        FIND FIRST bf-account
             WHERE bf-account.company EQ bf-oe-ord.company 
               AND bf-account.actnum  EQ bf-prep.actnum
             NO-ERROR.
        IF NOT AVAILABLE bf-account THEN DO:
            ASSIGN
                oplError   = TRUE 
                opcMessage = "Invalid Account Number " + bf-prep.actnum + " on Prep code " + STRING(ipcPrepCode)
                .     
            RETURN.        
        END.
        
        FIND LAST bf-oe-ordm NO-LOCK
            WHERE bf-oe-ordm.company EQ bf-oe-ord.company
              AND bf-oe-ordm.ord-no  EQ bf-oe-ord.ord-no
            USE-INDEX oe-misc NO-ERROR.
        IF AVAILABLE bf-oe-ordm THEN 
            iLineCount = bf-oe-ordm.line.

        RUN sys/ref/nk1look.p (
            INPUT  ipcCompany,
            INPUT  "CEPREPPRICE",
            INPUT  "C",
            INPUT  NO,
            INPUT  NO,
            INPUT  NO,
            INPUT  "",
            OUTPUT cPrepPrice,
            OUTPUT lFound
            ).

        IF cPrepPrice EQ "Profit" THEN
            dMarkUp = bf-prep.cost / (1 - (bf-prep.mkup / 100)).
        ELSE
            dMarkUp = bf-prep.cost * (1 + (bf-prep.mkup / 100)).
    
        IF dMarkUp EQ ? THEN
            dMarkUp = 0.

        CREATE bf-oe-ordm.
        ASSIGN
            bf-oe-ordm.company      = bf-oe-ord.company
            bf-oe-ordm.ord-no       = bf-oe-ord.ord-no
            bf-oe-ordm.charge       = bf-prep.code
            bf-oe-ordm.line         = iLineCount + 1
            bf-oe-ordm.dscr         = bf-prep.dscr
            bf-oe-ordm.cost         = bf-prep.cost
            bf-oe-ordm.actnum       = bf-prep.actnum
            bf-oe-ordm.amt          = dMarkUp
            bf-oe-ordm.est-no       = bf-oe-ord.est-no
            bf-oe-ordm.bill         = "Y"
            bf-oe-ordm.s-man[1]     = bf-oe-ord.sman[1]
            bf-oe-ordm.s-pct[1]     = bf-oe-ord.s-pct[1]
            bf-oe-ordm.s-comm[1]    = bf-oe-ord.s-comm[1]
            bf-oe-ordm.s-man[2]     = bf-oe-ord.sman[2]
            bf-oe-ordm.s-pct[2]     = bf-oe-ord.s-pct[2]
            bf-oe-ordm.s-comm[2]    = bf-oe-ord.s-comm[2]
            bf-oe-ordm.s-man[3]     = bf-oe-ord.sman[3]
            bf-oe-ordm.s-pct[3]     = bf-oe-ord.s-pct[3]
            bf-oe-ordm.s-comm[3]    = bf-oe-ord.s-comm[3]
            bf-oe-ordm.spare-char-1 = bf-oe-ord.tax-gr
            .    
    
        IF NOT bf-prep.commissionable THEN
            ASSIGN 
                bf-oe-ordm.s-comm[1] = 0 
                bf-oe-ordm.s-comm[2] = 0
                bf-oe-ordm.s-comm[3] = 0
                .
    
        RUN Tax_GetTaxableMisc (
            INPUT  ipcCompany, 
            INPUT  bf-oe-ord.cust-no, 
            INPUT  bf-oe-ord.ship-id, 
            INPUT  ipcPrepCode, 
            OUTPUT bf-oe-ordm.tax
            ).
        IF bf-oe-ordm.tax AND bf-oe-ord.tax-gr EQ "" THEN
            bf-oe-ordm.tax = FALSE.

        FIND FIRST bf-oe-ordl NO-LOCK
             WHERE bf-oe-ordl.company EQ bf-oe-ord.company
               AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
             NO-ERROR.
        IF AVAILABLE bf-oe-ordl THEN
            ASSIGN
                bf-oe-ordm.spare-char-2 = bf-oe-ordl.i-no 
                bf-oe-ordm.ord-i-no     = bf-oe-ordl.job-no
                bf-oe-ordm.ord-line     = bf-oe-ordl.job-no2
                bf-oe-ordm.spare-int-1  = bf-oe-ordl.line
                .

        FIND CURRENT bf-prep EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-prep THEN 
            ASSIGN 
                bf-prep.last-order  = bf-oe-ordm.ord-no
                bf-prep.last-est-no = bf-oe-ordm.est-no
                .
    END.    
END PROCEDURE.

PROCEDURE pCreateOrderHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an oe-ord (Order Header) record and returns the order no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiOrderID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    
    CREATE bf-oe-ord.
    ASSIGN
       bf-oe-ord.company  = ipcCompany
       bf-oe-ord.ord-no   = fGetNextOrderNo(ipcCompany)
       bf-oe-ord.user-id  = USERID('ASI')
       bf-oe-ord.type     = 'O'       
       ipiOrderID         = bf-oe-ord.ord-no
       .
END PROCEDURE.

PROCEDURE pCreateOrderLine PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates an order line
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLineNo  AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ ipcCompany
           AND bf-oe-ordl.ord-no  EQ ipiOrderID
           AND bf-oe-ordl.line    EQ ipiLineNo
         NO-ERROR.
    IF AVAILABLE bf-oe-ordl THEN
        RETURN.
    
    CREATE bf-oe-ordl.
    ASSIGN
        bf-oe-ordl.company = ipcCompany
        bf-oe-ordl.ord-no  = ipiOrderID
        bf-oe-ordl.line    = ipiLineNo
        .
END PROCEDURE.

PROCEDURE pCreateRelease:
/*------------------------------------------------------------------------------
 Purpose: Creates oe-rel records for the input order lines
 Notes: This procedure was copied from from oe/createRelease.i
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiLineNo   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipTo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipFrom AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dtDateRule AS DATE      NO-UNDO.
    DEFINE VARIABLE iNextRelNo AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOECARIER  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cOERELEAS  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord   FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf-shipto   FOR shipto.
    
    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipcCompany
           AND bf-oe-ord.ord-no  EQ ipiOrderID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Unable to find Order#: " + STRING(ipiOrderID) + ". Failed to create release."
            .
        RETURN.    
    END.

    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ ipcCompany
           AND bf-oe-ordl.ord-no  EQ ipiOrderID
           AND bf-oe-ordl.line    EQ ipiLineNo
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Unable to find Line#: " + STRING(ipiLineNo) + "for Order#: " + STRING(ipiOrderID) + ". Failed to create release."
            .
        RETURN.    
    END.
        
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,
        INPUT  "OECARIER",
        INPUT  "C",
        INPUT  NO,
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        OUTPUT cOECARIER,
        OUTPUT lFound
        ).

    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,
        INPUT  "OERELEAS",
        INPUT  "C",
        INPUT  NO,
        INPUT  NO,
        INPUT  NO,
        INPUT  "",
        OUTPUT cOERELEAS,
        OUTPUT lFound
        ).
        
    FIND FIRST bf-shipto NO-LOCK
         WHERE bf-shipto.company EQ bf-oe-ord.company
           AND bf-shipto.cust-no EQ bf-oe-ord.cust-no
           AND bf-shipto.ship-id EQ ipcShipTo
         NO-ERROR.
    IF NOT AVAILABLE bf-shipto THEN
        FIND FIRST bf-shipto NO-LOCK
             WHERE bf-shipto.company EQ bf-oe-ord.company
               AND bf-shipto.cust-no EQ bf-oe-ord.cust-no
             NO-ERROR.

    /* Get Next release no */
    RUN oe/getNextRelNo.p (
        INPUT  "oe-rel", 
        OUTPUT iNextRelNo
        ).

    CREATE bf-oe-rel.
    ASSIGN 
        bf-oe-rel.company   = bf-oe-ord.company
        bf-oe-rel.loc       = bf-oe-ord.loc
        bf-oe-rel.ord-no    = bf-oe-ordl.ord-no
        bf-oe-rel.i-no      = bf-oe-ordl.i-no
        bf-oe-rel.cust-no   = bf-oe-ord.cust-no
        bf-oe-rel.po-no     = IF bf-oe-ordl.po-no NE "" THEN 
                                  bf-oe-ordl.po-no 
                              ELSE 
                                  bf-oe-ord.po-no
        bf-oe-rel.qty       = bf-oe-ordl.qty
        bf-oe-rel.tot-qty   = bf-oe-ordl.qty
        bf-oe-rel.line      = bf-oe-ordl.line
        bf-oe-rel.s-comm[1] = bf-oe-ord.s-comm[1]
        bf-oe-rel.s-comm[2] = bf-oe-ord.s-comm[2]
        bf-oe-rel.s-comm[3] = bf-oe-ord.s-comm[3]
        bf-oe-rel.s-name[1] = bf-oe-ord.sname[1]
        bf-oe-rel.s-name[2] = bf-oe-ord.sname[2]
        bf-oe-rel.s-name[3] = bf-oe-ord.sname[3]
        bf-oe-rel.s-pct[1]  = bf-oe-ord.s-pct[1]
        bf-oe-rel.s-pct[2]  = bf-oe-ord.s-pct[2]
        bf-oe-rel.s-pct[3]  = bf-oe-ord.s-pct[3]
        bf-oe-rel.sman[1]   = bf-oe-ord.sman[1]
        bf-oe-rel.sman[2]   = bf-oe-ord.sman[2]
        bf-oe-rel.sman[3]   = bf-oe-ord.sman[3]
        bf-oe-rel.sold-no   = bf-oe-ord.sold-no
        bf-oe-rel.carrier   = IF cOECARIER = "Shipto" AND AVAILABLE bf-shipto THEN 
                                  bf-shipto.carrier
                              ELSE
                                  bf-oe-ord.carrier
        bf-oe-rel.r-no      = iNextRelNo
        bf-oe-rel.frt-pay   = SUBSTRING(bf-oe-ord.frt-pay,1,1)
        bf-oe-rel.fob-code  = bf-oe-ord.fob-code
        .

    IF cOERELEAS EQ "LastShip" THEN
        bf-oe-rel.rel-date = bf-oe-ord.last-date.
    ELSE IF cOERELEAS EQ "Due Date" THEN
        bf-oe-rel.rel-date = bf-oe-ordl.req-date.
    ELSE DO: /*DueDate+1Day*/ 
        RUN spCommon_DateRule (
            bf-oe-ord.company,
            ?,
            bf-oe-ord.cust-no,
            IF AVAILABLE bf-shipto THEN bf-shipto.ship-id ELSE "",
            bf-oe-ordl.req-date,
            ?,
            ?,
            OUTPUT dtDateRule
            ).
        IF dtDateRule NE ? THEN
            bf-oe-rel.rel-date = dtDateRule.
        ELSE DO:
            bf-oe-rel.rel-date = bf-oe-ordl.req-date + 1.
            IF WEEKDAY(bf-oe-rel.rel-date) EQ 7 THEN
                bf-oe-rel.rel-date = bf-oe-rel.rel-date + 2.
            ELSE IF WEEKDAY(oe-rel.rel-date) EQ 1 THEN
                bf-oe-rel.rel-date = bf-oe-rel.rel-date + 1.
        END. /* else */
    END.

    IF AVAILABLE bf-shipto THEN DO:
        ASSIGN 
            bf-oe-rel.ship-addr[1] = bf-shipto.ship-addr[1]
            bf-oe-rel.ship-city    = bf-shipto.ship-city
            bf-oe-rel.ship-state   = bf-shipto.ship-state
            bf-oe-rel.ship-zip     = bf-shipto.ship-zip
            bf-oe-rel.ship-no      = bf-shipto.ship-no
            bf-oe-rel.ship-id      = bf-shipto.ship-id
            bf-oe-rel.ship-i[1]    = bf-shipto.notes[1]
            bf-oe-rel.ship-i[2]    = bf-shipto.notes[2]
            bf-oe-rel.ship-i[3]    = bf-shipto.notes[3]
            bf-oe-rel.ship-i[4]    = bf-shipto.notes[4]
            bf-oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN 
                                         ipcShipFrom
                                     ELSE 
                                         bf-shipto.loc
            .
            
        RUN CopyShipNote (
            INPUT bf-shipto.rec_key, 
            INPUT bf-oe-rel.rec_key
            ).
    END.
    ELSE
        ASSIGN
            bf-oe-rel.ship-no      = bf-oe-ord.sold-no
            bf-oe-rel.ship-id      = bf-oe-ord.sold-id
            bf-oe-rel.ship-i[1]    = bf-oe-ord.ship-i[1]
            bf-oe-rel.ship-i[2]    = bf-oe-ord.ship-i[2]
            bf-oe-rel.ship-i[3]    = bf-oe-ord.ship-i[3]
            bf-oe-rel.ship-i[4]    = bf-oe-ord.ship-i[4]
            bf-oe-rel.spare-char-1 = IF ipcShipFrom NE "" THEN 
                                         ipcShipFrom                                    
                                     ELSE 
                                         bf-oe-ord.loc
            .
    /* Assign itemfg-loc values */
    RUN fg/fgitmloc.p (
        INPUT bf-oe-rel.i-no, 
        INPUT ROWID(bf-oe-rel)
        ).
    
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Release#: " + STRING(bf-oe-rel.rel-no) + " created succssfully"
        . 
END PROCEDURE.

PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes: This procedure was copied from from oe/createRelease.i
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.

PROCEDURE pGetcXMLShipToPrefix PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Returns the shipto id replacing the customer specific prefix from NK1
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcShipToID   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipToID   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cXMLShipToPrefix AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,
        INPUT  "cXMLShipToPrefix",
        INPUT  "C",
        INPUT  YES,
        INPUT  YES,
        INPUT  ipcCustomerID,
        INPUT  "",
        OUTPUT cXMLShipToPrefix,
        OUTPUT lFound
        ).

    IF lFound AND cXMLShipToPrefix NE '' THEN
        opcShipToID = REPLACE(ipcShipToID, cXMLShipToPrefix, "").
    ELSE
        opcShipToID = ipcShipToID.
END PROCEDURE.

PROCEDURE ProcessOrdersFromImport:
/*------------------------------------------------------------------------------
 Purpose: Procedure to process imported orders 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrder.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttOrderLine.
    DEFINE OUTPUT       PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE lOEAutoApproval    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidationError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cValidationMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    
    FOR EACH ttOrder:
        RUN pProcessImportedOrderHeader (
            BUFFER ttOrder,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
        IF NOT oplSuccess THEN
            RETURN.    
        
        RUN pConsolidateImportedOrderLines (
            INPUT        ttOrder.orderSeqID,
            INPUT        ttOrder.company,
            INPUT        ttOrder.customerID,
            INPUT        ttOrder.shipToID,
            INPUT-OUTPUT TABLE ttOrderLine
            ).
        
        FOR EACH ttOrderLine 
            WHERE ttOrderLine.orderSeqID EQ ttOrder.orderSeqID
               BY ttOrderLine.lineNo:
            RUN pProcessImportedOrderLine (
                INPUT  ttOrder.company,
                INPUT  ttOrder.orderID,
                BUFFER ttOrderLine,  
                OUTPUT opcMessage,
                OUTPUT oplSuccess
                ).
            IF NOT oplSuccess THEN
                RETURN.
        END.
        
        FIND FIRST bf-oe-ord NO-LOCK
             WHERE bf-oe-ord.company EQ ttOrder.company
               AND bf-oe-ord.ord-no  EQ ttOrder.orderID
             NO-ERROR.
        IF NOT AVAILABLE bf-oe-ord THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Order failed to create"
                .
            RETURN.    
        END.
         
        lOEAutoApproval = fGetOEAutoApprovalLog(INPUT ttOrder.company, INPUT ttOrder.customerID, INPUT ttOrder.shipToID).        

       RUN Order_CreateMiscChargeByDeliveryDate (
           INPUT bf-oe-ord.company,
           INPUT bf-oe-ord.ord-no,
           INPUT NOW,  /* Order Date and time */
           INPUT bf-oe-ord.promiseDate
           ).      
        
        IF lOeAutoApproval THEN 
            RUN ProcessImportedOrder (
                INPUT  ROWID(bf-oe-ord), 
                OUTPUT lValidationError, 
                OUTPUT cValidationMessage
                ).
        ELSE
            RUN CalcOrderCommission (
                INPUT  ROWID(bf-oe-ord), 
                OUTPUT lError, 
                OUTPUT cMessage
                ).
        
        /* Force re-calculate tax. oe/calcordt.p is not being triggered from write.trg/oe-ord.p
           on addition of each order line */
        RUN oe/calcordt.p (
            INPUT ROWID(bf-oe-ord)
            ).
    END.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Order(s) created successfully"
        .
END PROCEDURE.

PROCEDURE GetReleaseType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplIsAComponent AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcRelType AS CHARACTER NO-UNDO.

   FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ ipcCompany NO-LOCK NO-ERROR.
   
    IF ipcSCode <> "" THEN 
        opcRelType = ipcSCode.
    ELSE 
        opcRelType = "B". /*Default */
        
    
    IF iplIsAComponent THEN opcRelType = "S".
    
    IF AVAILABLE oe-ctrl THEN DO:        
      opcRelType = IF oe-ctrl.ship-from THEN "B" ELSE "I".
    END.

END PROCEDURE.

PROCEDURE pCreateActRelLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Replaced oe/cre-rell.p but with no tag selection which must be added
              - Note:  references locode
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE PARAMETER BUFFER ipbf-oe-relh FOR oe-relh.
    DEFINE INPUT PARAMETER iRelNo AS INTEGER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oprOeRellRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lError            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cErrMsg           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRelType AS CHARACTER NO-UNDO.

    IF NOT AVAIL(ipbf-oe-relh) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Release header record not found"
               .          
    IF NOT AVAIL(ipbf-oe-rel) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Scheduled release record not found"
               .
    IF oplError THEN
        RETURN.
        
    FIND FIRST bf-oe-ordl no-lock
        WHERE bf-oe-ordl.company  EQ ipbf-oe-rel.company
          AND bf-oe-ordl.ord-no   EQ ipbf-oe-rel.ord-no
          AND bf-oe-ordl.line EQ ipbf-oe-rel.line
        NO-ERROR.
        
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbf-oe-rel.company
          AND itemfg.i-no    EQ ipbf-oe-rel.i-no
        NO-ERROR.        
    // RUN get-next-r-no.
    RUN GetReleaseType (INPUT ipbf-oe-rel.company, "" /* Existing s-code */, bf-oe-ordl.is-a-component, OUTPUT cRelType).
    CREATE bf-oe-rell.
    ASSIGN
        oprOeRellRow       = ROWID(bf-oe-rell)
        bf-oe-rell.company    = ipbf-oe-rel.company
        bf-oe-rell.r-no       = ipbf-oe-relh.r-no
        bf-oe-rell.rel-no     = iRelNo
        bf-oe-rell.loc        = IF gcDefaultLocMethod NE "ShipFromWhse" THEN locode ELSE ipbf-oe-rel.spare-char-1
        bf-oe-rell.ord-no     = ipbf-oe-rel.ord-no
        bf-oe-rell.qty        = ipbf-oe-rel.tot-qty
        bf-oe-rell.i-no       = ipbf-oe-rel.i-no
        bf-oe-rell.job-no     = bf-oe-ordl.job-no
        bf-oe-rell.job-no2    = bf-oe-ordl.job-no2
        bf-oe-rell.po-no      = ipbf-oe-rel.po-no
        bf-oe-rell.line       = ipbf-oe-rel.line
        bf-oe-rell.lot-no     = ipbf-oe-rel.lot-no
        bf-oe-rell.frt-pay    = ipbf-oe-rel.frt-pay
        bf-oe-rell.fob-code   = ipbf-oe-rel.fob-code
        bf-oe-rell.sell-price = ipbf-oe-rel.sell-price
        bf-oe-rell.zeroPrice  = ipbf-oe-rel.zeroPrice
        bf-oe-rell.printed    = NO
        bf-oe-rell.posted     = NO
        bf-oe-rell.deleted    = NO
        /** Set link to the planned releases **/
        bf-oe-rell.link-no    = ipbf-oe-rel.r-no
        bf-oe-rell.s-code     = cRelType    
        bf-oe-rell.partial = IF bf-oe-rell.s-code EQ "I" THEN bf-oe-ordl.partial ELSE 0
        bf-oe-rell.qty-case = IF AVAILABLE itemfg AND itemfg.case-count GT 0
            THEN itemfg.case-count
            ELSE
            IF bf-oe-ordl.cas-cnt GT 0 THEN bf-oe-ordl.cas-cnt
            ELSE 1
        bf-oe-rell.cases   = TRUNC((bf-oe-rell.qty - bf-oe-rell.partial) /
                            bf-oe-rell.qty-case,0)
        bf-oe-rell.partial = bf-oe-rell.qty - (bf-oe-rell.cases * bf-oe-rell.qty-case)
        .                     

    RUN oe/rel-stat-upd.p (ROWID(bf-oe-rell)).

    /* Fill in correct bin/loc */
    IF AVAILABLE bf-oe-rell  THEN
        RUN pSetActualReleaseLocation (BUFFER bf-oe-rell, BUFFER ipbf-oe-rel, OUTPUT lError, OUTPUT cErrMsg).

    /* Set values for invoice only */
    FIND CURRENT bf-oe-rell EXCLUSIVE-LOCK.
    IF bf-oe-rell.s-code = "I" THEN
       bf-oe-rell.loc-bin = "".
       
     FIND CURRENT ipbf-oe-rel EXCLUSIVE-LOCK.
     ASSIGN 
        ipbf-oe-rel.rel-no   = iRelNo
        ipbf-oe-rel.b-ord-no = ipbf-oe-relh.b-ord-no
        ipbf-oe-rel.qty      = ipbf-oe-rel.tot-qty
        .       
    RUN oe/rel-stat.p (ROWID(ipbf-oe-rel), OUTPUT ipbf-oe-rel.stat).
    
    FIND CURRENT ipbf-oe-rel NO-LOCK NO-ERROR.
    FIND CURRENT bf-oe-rell NO-LOCK NO-ERROR.
    
    IF glRecalcSkippedInTrigger THEN DO:
        /* Corrects data integrity issue until auditing can identify problem */
        /* Run if recalc is skipped in oe-rell trigger */        
        FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE itemfg THEN DO:
            RUN fg/calcqa&b.p (ROWID(itemfg), 
                               OUTPUT itemfg.q-alloc,
                               OUTPUT itemfg.q-back
                               ).
            itemfg.q-avail = itemfg.q-onh +
                              (IF gcOnOrderQtyCode EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
                              itemfg.q-alloc.
            FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                                  AND itemfg-loc.i-no    EQ itemfg.i-no
                                EXCLUSIVE-LOCK:
                RUN fg/calcqabl.p (ROWID(itemfg), 
                                  itemfg-loc.loc, 
                                  OUTPUT itemfg-loc.q-alloc,
                                  OUTPUT itemfg-loc.q-back
                                  ).
                itemfg-loc.q-avail = itemfg-loc.q-onh +
                                 (IF gcOnOrderQtyCode EQ "XOnOrder" THEN 0 ELSE itemfg-loc.q-ono) -
                                 itemfg-loc.q-alloc.
            END.         
        END.                 
    END.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE pCopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.  

END PROCEDURE.

PROCEDURE pCreateActRelHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Replaces oe/cre-relh.p except for credit check
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE OUTPUT PARAMETER oprOeRelhRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iNextRNo        AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE iNextReleaseNum AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrigProgram    AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE lCreditHold     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustCode       AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust FOR cust.

    IF NOT AVAIL(ipbf-oe-rel) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Release record not found"
               .
    
/*    ASSIGN                                  */
/*        cOrigProgram = TRIM(PROGRAM-NAME(2))*/
/*        lCreditHold  = NO                   */
/*        .                                   */
/*    IF glCheckCredit THEN DO:                                     */
/*        FIND FIRST cust NO-LOCK                                   */
/*            WHERE cust.company EQ ipbf-oe-rel.company             */
/*              AND cust.cust-no EQ ipbf-oe-rel.cust-no             */
/*            NO-ERROR.                                             */
/*        IF AVAILABLE cust AND cOrigProgram NE "fg/invrecpt.p" THEN*/
/*                RUN oe/CRcheck.p ( INPUT ROWID(cust),             */
/*                                   INPUT YES,                     */
/*                                   OUTPUT lCreditHold ).          */
/*    END.                                                          */
    RUN oe/getNextRelNo.p (INPUT "oe-relh", 
                           OUTPUT iNextRNo).

    RUN oe/release#.p (ipbf-oe-rel.company, 
                       OUTPUT iNextReleaseNum).

    IF glUseTransCust = YES  AND ipbf-oe-rel.s-code EQ 'T' THEN 
    DO:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipbf-oe-rel.company 
              AND cust.active EQ 'X'  
            NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:
            IF CAN-FIND(FIRST shipto 
                WHERE shipto.company EQ ipbf-oe-rel.company 
                  AND shipto.cust-no EQ cust.cust-no 
                  AND shipto.ship-no EQ ipbf-oe-rel.ship-no 
                  AND shipto.ship-id EQ ipbf-oe-rel.ship-id) 
                THEN ASSIGN cCustCode = cust.cust-no.

            RELEASE cust.
        END.
    END.
    ELSE 
        cCustCode = ipbf-oe-rel.cust-no.
  
    CREATE oe-relh.  
    ASSIGN     
        oprOeRelhRow      = ROWID(oe-relh)
        oe-relh.cust-no   = cCustCode  
        oe-relh.r-no      = iNextRNo
        oe-relh.company   = ipbf-oe-rel.company
        oe-relh.ship-no   = ipbf-oe-rel.ship-no
        oe-relh.ship-id   = ipbf-oe-rel.ship-id
        oe-relh.ship-i[1] = ipbf-oe-rel.ship-i[1]
        oe-relh.ship-i[2] = ipbf-oe-rel.ship-i[2]
        oe-relh.ship-i[3] = ipbf-oe-rel.ship-i[3]
        oe-relh.ship-i[4] = ipbf-oe-rel.ship-i[4]
        oe-relh.carrier   = ipbf-oe-rel.carrier
        oe-relh.printed   = NO
        oe-relh.posted    = NO
        oe-relh.deleted   = NO
        oe-relh.rel-date  = ipbf-oe-rel.rel-date
        oe-relh.release#  = iNextReleaseNum
        oe-relh.user-id   = USERID("nosweat")
        oe-relh.upd-time  = TIME
        oe-relh.upd-date  = TODAY
        /* oe-relh.w-ord     = lCreditHold */
        .
       
    RUN pCopyShipNote (ipbf-oe-rel.rec_key, oe-relh.rec_key).

/*    IF lCreditHold THEN                                              */
/*    DO:                                                              */
/*        FIND FIRST bf-cust EXCLUSIVE-LOCK                            */
/*            WHERE bf-cust.company EQ ipbf-oe-rel.company             */
/*            AND bf-cust.cust-no EQ ipbf-oe-rel.cust-no USE-INDEX cust*/
/*            NO-WAIT NO-ERROR.                                        */
/*        IF AVAILABLE bf-cust THEN                                    */
/*            ASSIGN bf-cust.cr-hold = YES                             */
/*                .                                                    */
/*    END.                                                             */
    
END PROCEDURE.

PROCEDURE pApproveImportedOrder PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Moves a given order buffer from O-W to O-U-1 
     Notes: from oe/v-ord.w hold-approve
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    
    DEFINE VARIABLE iSman       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lHold       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cHoldReason AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE dMargin     AS DECIMAL   NO-UNDO.  /*Refactor?*/
    DEFINE VARIABLE cUOM        AS CHARACTER NO-UNDO.  /*Refactor?*/

    
   
    FIND CURRENT ipbf-oe-ord EXCLUSIVE-LOCK.
    ASSIGN
        ipbf-oe-ord.user-id     = USERID("nosweat")
        ipbf-oe-ord.approved-id = USERID("nosweat")
        ipbf-oe-ord.t-freight   = 0.
        
    RUN pSetGlobalSettings (INPUT ipbf-oe-ord.company).
    IF ipbf-oe-ord.type EQ "" THEN ipbf-oe-ord.type = "O".

    IF ipbf-oe-ord.sman[1] EQ "" THEN
        ASSIGN
            ipbf-oe-ord.sman    = ""
            ipbf-oe-ord.sman[1] = cust.sman.

    IF ipbf-oe-ord.sman[1] NE "" AND ipbf-oe-ord.s-pct[1] EQ 0 THEN
        ipbf-oe-ord.s-pct[1] = 100.00.

    DO iSman = 1 TO EXTENT(ipbf-oe-ord.sman):
        IF ipbf-oe-ord.s-comm[iSman] GE 100 THEN
            ASSIGN
                ipbf-oe-ord.s-comm = 0
                dMargin            = 0
                .

        FIND FIRST sman
            WHERE sman.company EQ ipbf-oe-ord.company
            AND sman.sman    EQ ipbf-oe-ord.sman[iSman]
            NO-LOCK NO-ERROR.
        IF AVAILABLE sman THEN 
        DO:
            ipbf-oe-ord.sname[1] = sman.sname.

            IF ipbf-oe-ord.s-comm[iSman] LE 0 THEN
            DO:
                ipbf-oe-ord.s-comm[iSman] = sman.scomm.
                IF iSman = 1 THEN
                    dMargin = 0.
            END.
        END.
    END. /* do iSman = 1 to sman */

    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
        AND bf-oe-ordl.job-no  EQ ""
        AND bf-oe-ordl.i-no    NE "",
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ bf-oe-ordl.company
        AND itemfg.i-no    EQ bf-oe-ordl.i-no:

        FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE oe-ordl THEN NEXT.

        ASSIGN
            oe-ordl.req-code  = ipbf-oe-ord.due-code
            oe-ordl.req-date  = ipbf-oe-ord.due-date
            oe-ordl.prom-code = ipbf-oe-ord.due-code
            oe-ordl.prom-date = ipbf-oe-ord.due-date.

        DO iSman = 1 TO MIN(EXTENT(oe-ordl.s-man),EXTENT(ipbf-oe-ord.sman)):
            ASSIGN
                oe-ordl.s-man[iSman]  = ipbf-oe-ord.sman[iSman]
                oe-ordl.s-pct[iSman]  = ipbf-oe-ord.s-pct[iSman]
                oe-ordl.s-comm[iSman] = ipbf-oe-ord.s-comm[iSman].

            IF iSman = 1 THEN
                ASSIGN
                    oe-ordl.q-qty = ipbf-oe-ord.t-fuel
                    dMargin       = ipbf-oe-ord.t-fuel.
        END.

        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company   EQ oe-ordl.company
            AND po-ordl.i-no      EQ oe-ordl.i-no
            AND po-ordl.po-no     EQ oe-ordl.po-no-po
            AND po-ordl.item-type EQ NO
            USE-INDEX item-ordno NO-ERROR.
        IF AVAILABLE po-ordl THEN
            ASSIGN
                cUOM         = po-ordl.cons-uom
                oe-ordl.cost = po-ordl.cons-cost.
        ELSE
            ASSIGN
                cUOM         = itemfg.prod-uom
                oe-ordl.cost = itemfg.total-std-cost.

        IF cUOM NE "M" THEN
            RUN sys/ref/convcuom.p(cUOM, "M", 0, 0, 0, 0,
                oe-ordl.cost, OUTPUT oe-ordl.cost).
        IF (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Order processing") THEN do:
            RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
            ipbf-oe-ord.t-freight = ipbf-oe-ord.t-freight + oe-ordl.t-freight.
        end.
        FIND CURRENT oe-ordl NO-LOCK.
        RELEASE oe-ordl.
    END. /* Each oe-ordl */
     
    IF (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Order processing") THEN 
      RUN oe/ordfrate.p (ROWID(ipbf-oe-ord)).
    
    RUN CalcOrderCommission(ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).
    
    RUN oe/calcordt.p (ROWID(ipbf-oe-ord)).

    RUN pValidateOrder(BUFFER ipbf-oe-ord, OUTPUT lHold, OUTPUT cHoldReason).    
    IF ipbf-oe-ord.job-no EQ "" THEN DO:
        IF lHold THEN 
        DO:
            ipbf-oe-ord.stat = "H".
            RUN oe/syncJobHold.p (INPUT ipbf-oe-ord.company, INPUT ipbf-oe-ord.ord-no, INPUT "Hold").
        END.
        ELSE 
        DO:
            ASSIGN
                ipbf-oe-ord.stat = "A"
                ipbf-oe-ord.approved-date = TODAY.
            RUN oe/syncJobHold.p (INPUT ipbf-oe-ord.company, INPUT ipbf-oe-ord.ord-no, INPUT "Approve").
            RUN ReleaseOrder(ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).          
        END.
    END.
        ELSE ipbf-oe-ord.stat = "W".  /*Refactor - if we add ability to build job, this can be removed*/
        
    FIND CURRENT ipbf-oe-ord NO-LOCK.
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
        AND bf-oe-ordl.job-no  EQ ""
        AND bf-oe-ordl.i-no    NE "":

        FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE oe-ordl THEN NEXT.
        oe-ordl.stat = (IF ipbf-oe-ord.stat EQ "W"  THEN ipbf-oe-ord.stat ELSE "").
    END.
/*    IF fGetSettingJobCreate(ipbf-oe-ord.company) AND ipbf-oe-ord.job-no NE "" THEN                          */
/*    DO TRANSACTION: /*create job*/                                                                          */
/*        FIND FIRST job NO-LOCK                                                                              */
/*            WHERE job.company EQ ipbf-oe-ord.company                                                        */
/*            AND job.job-no  EQ ipbf-oe-ord.job-no                                                           */
/*            AND job.job-no2 EQ ipbf-oe-ord.job-no2                                                          */
/*            NO-ERROR.                                                                                       */
/*                                                                                                            */
/*        IF AVAILABLE job AND TRIM(job.est-no) NE TRIM(ipbf-oe-ord.est-no) THEN                              */
/*            IF CAN-FIND(FIRST job-hdr                                                                       */
/*                WHERE job-hdr.company EQ job.company                                                        */
/*                AND job-hdr.job     EQ job.job                                                              */
/*                AND job-hdr.job-no  EQ job.job-no                                                           */
/*                AND job-hdr.job-no2 EQ job.job-no2                                                          */
/*                AND job-hdr.ord-no  NE ipbf-oe-ord.ord-no) OR                                               */
/*                CAN-FIND(FIRST bf-oe-ord                                                                    */
/*                WHERE bf-oe-ord.company EQ job.company                                                      */
/*                AND bf-oe-ord.job-no  EQ job.job-no                                                         */
/*                AND bf-oe-ord.job-no2 EQ job.job-no2                                                        */
/*                AND bf-oe-ord.est-no  EQ job.est-no)   OR                                                   */
/*                CAN-FIND(FIRST bf-oe-ordl                                                                   */
/*                WHERE bf-oe-ordl.company EQ job.company                                                     */
/*                AND bf-oe-ordl.job-no  EQ job.job-no                                                        */
/*                AND bf-oe-ordl.job-no2 EQ job.job-no2                                                       */
/*                AND bf-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.                                    */
/*            ELSE                                                                                            */
/*            DO:                                                                                             */
/*                FIND CURRENT job NO-ERROR.                                                                  */
/*                IF AVAILABLE job THEN DELETE job.                                                           */
/*            END.                                                                                            */
/*                                                                                                            */
/*        IF NOT AVAILABLE job THEN                                                                           */
/*        DO:                                                                                                 */
/*            RUN create-job (OUTPUT lv-job-recid).                                                           */
/*            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.                                               */
/*        END.                                                                                                */
/*                                                                                                            */
/*        v-qty-mod = YES.                                                                                    */
/*                                                                                                            */
/*        IF AVAILABLE job AND INDEX("HWPRL",job.stat) NE 0 THEN                                              */
/*        DO:                                                                                                 */
/*            /*IF NOT v-qty-mod THEN                                                                         */
/*               RUN oe/job-qty.p (ROWID(ipbf-oe-ord), OUTPUT v-qty-mod).*/                                   */
/*                                                                                                            */
/*            IF v-qty-mod OR job.stat EQ "P" THEN                                                            */
/*            DO:                                                                                             */
/*                RUN jc/chkrebld.p (RECID(job), OUTPUT choice).                                              */
/*                IF NOT choice THEN                                                                          */
/*                DO:                                                                                         */
/*                    ASSIGN                                                                                  */
/*                        hld-id     = fil_id                                                                 */
/*                        hld-nufile = nufile                                                                 */
/*                        hld-stat   = job.stat                                                               */
/*                        nufile     = YES.                                                                   */
/*                                                                                                            */
/*                    RUN jc/jc-calc.p(RECID(job), NO).                                                       */
/*                    ASSIGN                                                                                  */
/*                        fil_id = hld-id                                                                     */
/*                        nufile = hld-nufile.                                                                */
/*                                                                                                            */
/*                    IF hld-stat NE "P" THEN                                                                 */
/*                    DO:                                                                                     */
/*                        FIND CURRENT job EXCLUSIVE.                                                         */
/*                        job.stat = hld-stat.                                                                */
/*                        FIND CURRENT job NO-LOCK.                                                           */
/*                    END.                                                                                    */
/*                END.                                                                                        */
/*            END.                                                                                            */
/*        END.                                                                                                */
/*                                                                                                            */
/*        FIND FIRST sys-ctrl WHERE                                                                           */
/*            sys-ctrl.company EQ cocode AND                                                                  */
/*            sys-ctrl.name    EQ "SCHEDULE"                                                                  */
/*            NO-LOCK NO-ERROR.                                                                               */
/*                                                                                                            */
/*        v-run-schedule = IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO*/
/*        ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES         */
/*        ELSE NO.                                                                                            */
/*                                                                                                            */
/*        FOR EACH oe-ordl NO-LOCK                                                                            */
/*            WHERE oe-ordl.company EQ cocode                                                                 */
/*            AND oe-ordl.ord-no  EQ oe-ord.ord-no                                                            */
/*            AND oe-ordl.is-a-component EQ NO                                                                */
/*                                                                                                            */
/*            BREAK BY oe-ordl.job-no                                                                         */
/*            BY oe-ordl.job-no2:                                                                             */
/*                                                                                                            */
/*            IF LAST-OF(oe-ordl.job-no2) THEN                                                                */
/*            DO:                                                                                             */
/*                ASSIGN                                                                                      */
/*                    hld-id     = fil_id                                                                     */
/*                    hld-nufile = nufile                                                                     */
/*                    fil_id     = RECID(oe-ordl).                                                            */
/*                                                                                                            */
/*                RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.                                   */
/*                /* check oe-ordl.due-date and calc promised date and job's start-date */                    */
/*                                                                                                            */
/*                IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.                      */
/*                                                                                                            */
/*                ASSIGN                                                                                      */
/*                    fil_id = hld-id                                                                         */
/*                    nufile = hld-nufile.                                                                    */
/*            END.                                                                                            */
/*        END.                                                                                                */
/*    END.  /* transaction if v-create-job */                                                                 */


END PROCEDURE.


PROCEDURE pCreateFgBinForRelease PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Replaces create-fg-bin procedure in oe/cre-rell.p
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbfOeRell FOR oe-rell.
    DEFINE BUFFER bfOeRell FOR oe-rell.
    DEFINE BUFFER bfFgBin  FOR fg-bin.
    DEFINE VARIABLE xLoc LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE xBin LIKE fg-bin.loc-bin NO-UNDO.

    ASSIGN 
        xBin = ipbfOeRell.loc-bin
        xLoc = ipbfOeRell.loc
        .

    CREATE bfFgBin.
    ASSIGN
        bfFgBin.company = ipbfOeRell.company
        bfFgBin.i-no    = ipbfOeRell.i-no
        bfFgBin.job-no  = ipbfOeRell.job-no
        bfFgBin.job-no2 = ipbfOeRell.job-no2
        bfFgBin.loc     = xLoc
        bfFgBin.loc-bin = xBin
        bfFgBin.tag     = ipbfOeRell.tag
        bfFgBin.cust-no = ipbfOeRell.cust-no
        . 
    RELEASE bfFgBin.

    /*Create a bin so that is shows up in IF4 -FG Bin (blank i-no)*/
    FIND FIRST bfFgBin NO-LOCK
        WHERE bfFgBin.company EQ ipbfOeRell.company 
          AND bfFgBin.loc EQ xLoc
          AND bfFgBin.loc-bin EQ xBin
          AND bfFgBin.i-no = ""
        NO-ERROR.
    IF NOT AVAILABLE bfFgBin THEN 
    DO:
        CREATE bfFgBin.
        ASSIGN 
            bfFgBin.company = ipbfOeRell.company
            bfFgBin.i-no    = ""
            bfFgBin.loc     = xLoc
            bfFgBin.loc-bin = xBin
            .
    END.
    FIND CURRENT bfFgBin NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS LOGICAL NO-UNDO.
              
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "ADDXFER", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glUseTransCust = LOGICAL(cReturnChar) NO-ERROR.
    
/*    RUN sys/ref/nk1look.p (INPUT ipcCompany, "RELCREDT", "L" /* Logical */, NO /* check by cust */,*/
/*        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,                       */
/*        OUTPUT cReturnChar, OUTPUT lRecFound).                                                     */
/*    glCheckCredit = LOGICAL(cReturnChar) NO-ERROR.                                                 */
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "OEREORDR", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcOnOrderQtyCode = cReturnChar NO-ERROR.    
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "BOLWHSE", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcDefaultLocMethod = cReturnChar NO-ERROR.
         
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "RelSkipRecalc", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glRecalcSkippedInTrigger = LOGICAL(cReturnChar) NO-ERROR.

    RUN sys/ref/nk1look.p (INPUT ipcCompany, "AUTOPOST", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glUseItemfgLoc = cReturnChar EQ "FGFILE" NO-ERROR.    
                     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "addrelse", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcTagSelectionCode = cReturnChar NO-ERROR.
    
    /* Set gcCompanyDefaultBin in cre-rell */
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "BOLPRINT", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcCompanyDefaultBin = cReturnChar NO-ERROR. 
    RUN sys/ref/nk1look.p (INPUT g_company, "FreightCalculation", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    IF lRecFound THEN
     cFreightCalculationValue = cReturnChar NO-ERROR. 
      
END PROCEDURE.

PROCEDURE ReleaseOrder :
    /*------------------------------------------------------------------------------
     Purpose: Given a buffer oe-ord, release all lines at full quantity into 
     Actual Releases
     Notes:   Replaces oe/actRelMerge.p but does not contain any of the merge 
              logic - this needs to be added.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lAllOrdLinesReleased AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rOeRelh AS ROWID NO-UNDO.
    DEFINE VARIABLE rOeRell AS ROWID NO-UNDO.
    DEFINE VARIABLE iRelNo AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    
    ASSIGN lAllOrdLinesReleased = YES
           iRelNo = 0
           .
    FIND FIRST bf-oe-ord NO-LOCK
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.
    IF NOT AVAIL bf-oe-ord THEN DO:
        ASSIGN oplError = TRUE
               opcMessage = "Invalid order rowid passed in."
               .
        RETURN.
    END.        
    RUN pSetGlobalSettings (INPUT bf-oe-ord.company).
    
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ bf-oe-ord.company 
          AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
        :
        FIND FIRST bf-oe-rel NO-LOCK 
            WHERE bf-oe-rel.company EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no  EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.line    EQ bf-oe-ordl.line
            NO-ERROR.
        IF NOT AVAIL bf-oe-rel THEN  
            lAllOrdLinesReleased = NO.
     END.
     IF NOT lAllOrdLinesReleased THEN DO:
         ASSIGN oplError = TRUE
                opcMessage = "Some order lines do not have a scheduled release."
                .
         RETURN.
     END.
     REL-LINES:
     FOR EACH bf-oe-rel NO-LOCK
        WHERE bf-oe-rel.company EQ bf-oe-ord.company 
          AND bf-oe-rel.ord-no  EQ bf-oe-ord.ord-no
        BREAK BY bf-oe-rel.ord-no:
            
        IF FIRST-OF(bf-oe-rel.ord-no) THEN DO:
           RUN pCreateActRelHeader (BUFFER bf-oe-rel, OUTPUT rOeRelh, OUTPUT oplError, OUTPUT opcMessage).
             IF oplError THEN 
                 LEAVE REL-LINES.
           
           FIND FIRST bf-oe-relh NO-LOCK
              WHERE ROWID(bf-oe-relh) EQ rOeRelh
              NO-ERROR. 
           IF NOT AVAIL bf-oe-relh THEN DO:
                ASSIGN oplError   = TRUE
                       opcMessage = "Actual release header not created."
                       .
                LEAVE REL-LINES.
           END.
        END.     
        FIND FIRST bf-oe-relh NO-LOCK
            WHERE ROWID(bf-oe-relh) EQ rOeRelh
            NO-ERROR.
        iRelNo = iRelNo + 1.
        RUN pCreateActRelLine (BUFFER bf-oe-rel, BUFFER bf-oe-relh, iRelNo, OUTPUT rOeRell, OUTPUT oplError, OUTPUT opcMessage).
     END. 
     
     IF AVAILABLE bf-oe-relh THEN
        RUN pRunAPIOutboundTrigger (
            BUFFER bf-oe-relh,
            INPUT  "CreateRelease"
            ).
            
END PROCEDURE.

PROCEDURE ProcessImportedOrder:
    /*------------------------------------------------------------------------------
     Purpose: given a Rowid for an imported Order, process the order based on settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Order - Rowid Not found"
            .
    END.
    ELSE 
    DO:        
        RUN pApproveImportedOrder(BUFFER bf-oe-ord, OUTPUT oplError, OUTPUT opcMessage).

    END.

END PROCEDURE.

PROCEDURE pValidateOrder PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Runs the configured validation routines for a given order buffer, 
        creating hold tags as necessary.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    IF NOT AVAILABLE ipbf-oe-ord THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Order - Buffer not found"
            .
    END.
    ELSE 
    DO:
        IF NOT VALID-HANDLE(hdOeValidate) THEN
            RUN system/oeValidate.p PERSISTENT SET hdOeValidate.
        RUN ValidateOrder IN hdOeValidate (ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).
    
    END.

END PROCEDURE.

PROCEDURE pSetActualReleaseLocation PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose: Assign correct location and bin to oe-rell    
      Parameters:  <none>
      Notes: Replaces procedure find-bin-loc in oe/cre-rell.p
      
      Logic For Determining OT1 (Actual Release) WHSE, BIN
    
    1)  If the N-K BOLWHSE character value = "Shipto" then the WHSE and BIN
        are pulled from the customer shipto  (AF1, ShipTo tab) specified 
        on the OU1 Release.
    
    2)  If BOLWHSE character value = "ShipFromWhse", a default WHSE location 
        that matches the ShipFrom set in the OU1 Release will be defined.  
        The BIN location is then determined by finding a bin that matches 
        on the following:
        a.  Job, Item, Exact Qty ,  ShipFrom Whse
        b.  Job, Item, Any Qty > 0, ShipFrom Whse
        c.  Job, Item, ShipFrom Whse (any Qty)
        d.  Item, Order, Any Qty > 0, ShipFrom Whse
        e.  Item, Order, ShipFrom Whse (any Qty)
        f.  Item, Any Qty > 0, ShipFrom Whse (any Job, Order)
        g.  Item, ShipFrom Whse
        If the logic above doesn't locate a bin, then the Bin is set to the 
        character value of N-K BOLPRINT.  If this is not a valid bin for the 
        ShipFrom Whse, it will be created.
    
    3)  If BOLWHSE is not "ShipTo" or "ShipFromWhse" then the default WHSE 
        is set based on the global default warehouse for the company.  
        This can be overridden if a bin exists with the following hierarchical 
        matching criteria:
        a.  Job, Item,  Exact Qty,  Default Whse
        b.  Job, Item, Exact Qty  (any Whse)
        c.  Job, Item, Any Qty > 0, Default Whse
        d.  Job, Item, Any Qty>0  (any Whse)
        e.  Job, Item, Default Whse (any Qty)
        f.  Job, Item (any Qty, any Whse)
        g.  Item, Order, Any Qty > 0, Default Whse
        h.  Item, Order, Default Whse (any Qty)
        i.  Item, Order, Any Qty > 0 (any Whse)
        j.  Item, Order
        k.  Item, Any Qty > 0,Default Whse (any Job, Order)
        l.  Item, Default Whse
        m.  Item, Any Qty > 0
        n.  Item
        If the logic above doesn't locate a bin, then the default Whse and Bin 
        from the FG item file is used.  If those values are blank, the Whse and 
        Bin for the first shipto of the Internal Customer (Customer X) is used.
          
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rell FOR oe-rell.
    DEFINE PARAMETER BUFFER ipbf-oe-rel   FOR oe-rel.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cSelectedValue    AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    /* For premier, no tags are selected */
    cSelectedValue = "NoTag".
    
    FIND CURRENT ipbf-oe-rell EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-rel.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-rel.ord-no
          AND bf-oe-ordl.line    EQ ipbf-oe-rel.line
        NO-ERROR.
    IF NOT AVAIL bf-oe-ordl OR NOT AVAIL ipbf-oe-rel OR NOT AVAIL ipbf-oe-rell THEN 
    DO:
        ASSIGN oplError = true
               opcMessage = "Buffer not available"
               .
        RETURN. 
    END.
    
    IF ipbf-oe-rel.spare-char-1 GT "" 
       AND gcDefaultLocMethod EQ "ShipFromWhse" THEN
        ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
        
    /* gcDefaultLocMethod is an NK1 flag. ipbf-oe-rel.spare-char-1 is a ship-from */
    /* chosen by the user, so should try to find a bin for it       */
    IF gcDefaultLocMethod EQ "SHIPTO" THEN 
    DO:
        FIND FIRST bf-shipto NO-LOCK
            WHERE bf-shipto.company EQ ipbf-oe-rel.company
              AND bf-shipto.cust-no EQ ipbf-oe-rel.cust-no
              AND bf-shipto.ship-no EQ ipbf-oe-rel.ship-no
            USE-INDEX ship-no NO-ERROR.
        IF AVAILABLE bf-shipto THEN 
            ASSIGN
                ipbf-oe-rell.loc     = bf-shipto.loc
                ipbf-oe-rell.loc-bin = bf-shipto.loc-bin
                .
    END.  
    ELSE 
    DO:
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
              AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
              AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
              AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
              AND bf-fg-bin.qty     GE ipbf-oe-rell.qty
              AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
            USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GE ipbf-oe-rell.qty
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.
   
        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX co-ino NO-ERROR.

        IF AVAILABLE bf-fg-bin THEN 
        DO:        
            IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN
                ASSIGN
                    ipbf-oe-rell.loc     = bf-fg-bin.loc
                    ipbf-oe-rell.loc-bin = bf-fg-bin.loc-bin
                    .

            IF gcTagSelectionCode NE "No Tags" AND cSelectedValue NE "NoTag" THEN
                ipbf-oe-rell.tag      = bf-fg-bin.tag.
        
            ASSIGN
                ipbf-oe-rell.job-no   = bf-fg-bin.job-no
                ipbf-oe-rell.job-no2  = bf-fg-bin.job-no2
                ipbf-oe-rell.qty-case = bf-fg-bin.case-count
                .       
        END.                           
        ELSE 
            IF glUseItemfgLoc THEN 
            DO:
                FIND FIRST bf-itemfg NO-LOCK 
                    WHERE bf-itemfg.company EQ ipbf-oe-rell.company
                      AND bf-itemfg.i-no    EQ ipbf-oe-rell.i-no
                    NO-ERROR.
                IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
                DO:          
                    ASSIGN
                        ipbf-oe-rell.loc     = bf-itemfg.def-loc
                        ipbf-oe-rell.loc-bin = bf-itemfg.def-loc-bin
                        .
                END.
            END.
    END. /* gcDefaultLocMethod NE "ShipTo" */
  
    IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
    DO:
        FIND FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ ipbf-oe-rell.company
              AND bf-itemfg.i-no    EQ ipbf-oe-rell.i-no
            NO-ERROR.
        IF AVAILABLE bf-itemfg THEN
            ASSIGN
                ipbf-oe-rell.loc     = bf-itemfg.def-loc
                ipbf-oe-rell.loc-bin = bf-itemfg.def-loc-bin
                .
        IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-oe-rell.company
                  AND cust.active  EQ "X" 
                NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST bf-shipto WHERE bf-shipto.company EQ ipbf-oe-rell.company
                    AND bf-shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE bf-shipto THEN
                    ASSIGN   
                        ipbf-oe-rell.loc     = bf-shipto.loc
                        ipbf-oe-rell.loc-bin = bf-shipto.loc-bin.
            END.            
        END.
    END.

    /* gcCompanyDefaultBin is from an NK1 bolprint */
    IF (ipbf-oe-rell.loc-bin EQ "" 
        OR (ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1))
        AND gcDefaultLocMethod EQ "ShipFromWhse" THEN 
    DO:
        IF ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1 THEN
            ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
        ipbf-oe-rell.loc-bin = gcCompanyDefaultBin.
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
              AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
              AND bf-fg-bin.loc-bin EQ ipbf-oe-rell.loc-bin
            NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN 
        DO:
            RUN pCreateFgBinForRelease (BUFFER ipbf-oe-rell).        
        END.

    END.

    FIND CURRENT ipbf-oe-rell NO-LOCK.
    ASSIGN oplError = FALSE
           opcMessage = ""
           .
END PROCEDURE.

/* Post Releases */
PROCEDURE OrderProcsPostReleases: 
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLID     AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cRtnCharBol  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFoundBol AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnCharRel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFoundRel AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lException   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRelPost     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRelPost     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iRelPost     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLDate     AS CHARACTER NO-UNDO.
    
    ASSIGN
        oplSuccess = YES
        opiBOLID   = 0
        .
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "L" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vENDOr */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
    IF lRecFoundRel THEN
       lRelPost = LOGICAL(cRtnCharRel) NO-ERROR.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "C" /* Character */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
        
    IF lRecFoundRel THEN
        cRelPost = cRtnCharBol NO-ERROR. 
        
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "I" /* Integer */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vENDOr */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
            
    IF lRecFoundRel THEN
        iRelPost = INT(cRtnCharRel) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,
        INPUT "BOLDATE",
        INPUT "C" /* Character */,
        INPUT YES /* check by cust */,
        INPUT YES /* use cust NOT vENDOr */,
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharBol,
        OUTPUT lRecFoundBol
        ).
    IF lRecFoundBol THEN
       cBOLDate = sys-ctrl.char-fld NO-ERROR.
     
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company  EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
           AND NOT oe-relh.posted  
           AND oe-relh.printed
         NO-ERROR.
    IF AVAILABLE oe-relh AND CAN-FIND(FIRST oe-rell
                                      WHERE oe-rell.company EQ oe-relh.company
                                        AND oe-rell.r-no    EQ oe-relh.r-no
                                      USE-INDEX r-no) THEN DO:
     
        /* Validates releases data */
        RUN pOrderProcsRelCheck (
            INPUT  ipcCompany,
            INPUT  ipiReleaseID, 
            INPUT  iRelPost,
            OUTPUT lException,
            OUTPUT opcMessage
            ).
        IF lException THEN DO:
            oplSuccess = NO.
            RETURN.
        END.
        IF opiBOLID NE 0 THEN
            FIND FIRST oe-bolh NO-LOCK
                 WHERE oe-bolh.company EQ oe-relh.company
                   AND oe-bolh.bol-no  EQ  opiBOLID
                 NO-ERROR.
            IF NOT AVAILABLE oe-bolh   OR 
                cRelPost EQ "BOL/REL"  THEN
                
                DO TRANSACTION:
                    /* Creates BOL */
                    RUN pOrderProcsCreateBOL (
                        INPUT ipcCompany,
                        INPUT ipiReleaseID, 
                        INPUT cRelPost,
                        INPUT iRelPost,
                        INPUT lRelPost,
                        INPUT cBOLDate,
                        INPUT ipcUserName,
                        INPUT-OUTPUT opiBOLID
                        ).
                END.
        FIND FIRST oe-rell NO-LOCK
             WHERE oe-rell.company EQ oe-relh.company
               AND oe-rell.r-no    EQ oe-relh.r-no
               AND oe-rell.s-code  EQ "I"
             USE-INDEX r-no NO-ERROR.
    
        cRelPost = IF AVAILABLE oe-rell THEN 
                       "Invoice" 
                   ELSE 
                       cRelPost.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ opiBOLID
             NO-ERROR.
        
        IF NOT AVAILABLE oe-bolh THEN DO:
            ASSIGN 
                opcMessage = "BOL is not created"
                oplSuccess = NO
                .

            RETURN.
        END.
        
        /* Posts release */
        RUN pOrderProcsPostRelease (
            INPUT ipcCompany,
            INPUT ipiReleaseID
            ).
        
        /* Creates BOL Lines */
        RUN pOrderProcsCreateBOLLines (
            INPUT ipcCompany,
            INPUT ipiReleaseID,
            INPUT opiBOLID
            ).
            
        FOR EACH oe-rell WHERE oe-rell.company EQ oe-relh.company
                           AND oe-rell.r-no    EQ oe-relh.r-no
                         USE-INDEX r-no
                         NO-LOCK,
            EACH oe-ordl WHERE oe-ordl.company EQ oe-relh.company
                           AND oe-ordl.ord-no EQ oe-rell.ord-no
                         EXCLUSIVE-LOCK:
            
            RUN oe/ordlsqty.p (
                INPUT ROWID(oe-ordl),
                OUTPUT oe-ordl.inv-qty,
                OUTPUT oe-ordl.ship-qty
                ).
            
            IF oe-rell.link-no EQ 0 THEN DO:
                FIND FIRST oe-rel
                     WHERE oe-rel.company  EQ oe-rell.company
                       AND oe-rel.ord-no   EQ oe-rell.ord-no
                       AND oe-rel.line     EQ oe-rell.line
                       AND oe-rel.i-no     EQ oe-rell.i-no
                       AND oe-rel.ship-id  EQ oe-relh.ship-id
                       AND oe-rel.link-no  EQ 0
                     NO-ERROR.
            
                IF NOT AVAILABLE oe-rel THEN
                    FIND FIRST oe-rel
                         WHERE oe-rel.company  EQ oe-rell.company
                           AND oe-rel.ord-no   EQ oe-rell.ord-no
                           AND oe-rel.line     EQ oe-rell.line
                           AND oe-rel.i-no     EQ oe-rell.i-no
                           AND oe-rel.link-no  EQ 0
                         NO-ERROR.
            END.            
            ELSE
                FIND FIRST oe-rel
                     WHERE oe-rel.r-no EQ oe-rell.link-no
                     USE-INDEX seq-no NO-ERROR.
            
            IF AVAILABLE oe-rel THEN DO:
                RUN oe/custxship.p (
                    INPUT oe-relh.company,
                    INPUT oe-relh.cust-no,
                    INPUT oe-relh.ship-id,
                    BUFFER shipto
                    ).
            
                IF AVAILABLE shipto AND AVAILABLE oe-rel THEN
                    ASSIGN
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-addr[2] = shipto.ship-addr[2]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        .
            END.
        END.
        
    END.
    
END PROCEDURE.

PROCEDURE pOrderProcsRelCheck PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRelPost   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplException AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER buf-oe-relh FOR oe-relh.
    DEFINE BUFFER buf-oe-rell FOR oe-rell.
    
    FIND FIRST oe-relh NO-LOCK 
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-ERROR.
                         
    IF AVAILABLE oe-relh THEN DO:
        FIND FIRST buf-oe-relh EXCLUSIVE-LOCK
             WHERE ROWID(buf-oe-relh) EQ ROWID(oe-relh) 
             NO-WAIT NO-ERROR.
        
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no:
            
            FIND buf-oe-rell EXCLUSIVE-LOCK
                 WHERE ROWID(buf-oe-rell) EQ ROWID(oe-rell) 
                 NO-WAIT NO-ERROR.

            IF NOT AVAILABLE buf-oe-relh OR NOT AVAILABLE buf-oe-rell THEN DO:
                CREATE tt-except.
                BUFFER-COPY oe-rell TO tt-except.
                tt-except.reason = 1.
                LEAVE.
            END.
            FIND FIRST buf-oe-rell NO-LOCK 
                 WHERE ROWID(buf-oe-rell) EQ ROWID(oe-rell) 
                 NO-ERROR.
        END.
        FIND FIRST buf-oe-relh NO-LOCK
             WHERE ROWID(buf-oe-relh) EQ ROWID(oe-relh)  
             NO-ERROR.
        IF ipiRelPost EQ 1 AND
            NOT CAN-FIND(FIRST tt-except
                         WHERE tt-except.company EQ oe-relh.company
                           AND tt-except.r-no    EQ oe-relh.r-no) THEN DO:
                
            FOR EACH tt-fg-bin:
                tt-fg-bin.qty = 0.
            END.
            
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no,
            FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-rell.company
              AND oe-ord.ord-no  EQ oe-rell.ord-no
              AND oe-ord.type    NE "T"
            BREAK BY oe-rell.i-no
                  BY oe-rell.job-no
                  BY oe-rell.job-no2
                  BY oe-rell.loc
                  BY oe-rell.loc-bin
                  BY oe-rell.tag:
                    
                FIND FIRST tt-fg-bin
                     WHERE tt-fg-bin.company EQ oe-rell.company
                       AND tt-fg-bin.i-no    EQ oe-rell.i-no
                       AND tt-fg-bin.job-no  EQ oe-rell.job-no
                       AND tt-fg-bin.job-no2 EQ oe-rell.job-no2
                       AND tt-fg-bin.loc     EQ oe-rell.loc
                       AND tt-fg-bin.loc-bin EQ oe-rell.loc-bin
                       AND tt-fg-bin.tag     EQ oe-rell.tag
                    NO-ERROR.
                IF NOT AVAILABLE tt-fg-bin THEN DO:
                    CREATE tt-fg-bin.
                    ASSIGN
                        tt-fg-bin.company = oe-rell.company
                        tt-fg-bin.i-no    = oe-rell.i-no
                        tt-fg-bin.job-no  = oe-rell.job-no
                        tt-fg-bin.job-no2 = oe-rell.job-no2
                        tt-fg-bin.loc     = oe-rell.loc
                        tt-fg-bin.loc-bin = oe-rell.loc-bin
                        tt-fg-bin.tag     = oe-rell.tag
                        .
                    FOR EACH oe-bolh NO-LOCK
                        WHERE oe-bolh.company EQ tt-fg-bin.company
                          AND oe-bolh.posted  EQ NO
                          AND oe-bolh.deleted EQ NO
                        USE-INDEX post,
                        EACH oe-boll NO-LOCK
                        WHERE oe-boll.b-no  EQ oe-bolh.b-no
                          AND oe-boll.i-no    EQ tt-fg-bin.i-no
                          AND oe-boll.job-no  EQ tt-fg-bin.job-no
                          AND oe-boll.job-no2 EQ tt-fg-bin.job-no2
                          AND oe-boll.loc     EQ tt-fg-bin.loc
                          AND oe-boll.loc-bin EQ tt-fg-bin.loc-bin
                          AND oe-boll.tag     EQ tt-fg-bin.tag
                        USE-INDEX b-no2:
                            tt-fg-bin.qty = tt-fg-bin.qty + oe-boll.qty.
                    END.
                END.
                tt-fg-bin.qty = tt-fg-bin.qty + oe-rell.qty.
                        
                IF LAST-OF(oe-rell.tag) THEN DO:
                    FIND FIRST fg-bin NO-LOCK 
                        WHERE fg-bin.company EQ oe-rell.company
                          AND fg-bin.i-no    EQ oe-rell.i-no
                          AND fg-bin.job-no  EQ oe-rell.job-no
                          AND fg-bin.job-no2 EQ oe-rell.job-no2
                          AND fg-bin.loc     EQ oe-rell.loc
                          AND fg-bin.loc-bin EQ oe-rell.loc-bin
                          AND fg-bin.tag     EQ oe-rell.tag
                        NO-ERROR.
                
                    IF (NOT AVAILABLE fg-bin OR fg-bin.qty LT tt-fg-bin.qty) AND
                        oe-rell.tag EQ "" THEN DO:
                        CREATE tt-except.
                        BUFFER-COPY oe-rell TO tt-except.
                        tt-except.reason = 2.
                    END.
                END.
            END.
        END.
        FIND FIRST tt-except NO-LOCK
             WHERE tt-except.company EQ oe-relh.company
               AND tt-except.r-no    EQ oe-relh.r-no
             NO-ERROR.
        IF AVAILABLE tt-except THEN DO:
            ASSIGN
                opcMessage = IF tt-except.reason EQ 1 THEN
                                 "Release is not available"
                             ELSE IF tt-except.reason EQ 2 THEN
                                 "Quantity is not available"
                             ELSE
                                 "Exception occured"  
                oplException = YES
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pOrderProcsCreateBOL PRIVATE:
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRelpost   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiRelPost   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplRelPost   AS LOGICAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcBolPost   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcUserName  AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiBOLNo    AS INTEGER   NO-UNDO.  

    DEFINE VARIABLE cFRTPay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFRTList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRellctr AS INTEGER   NO-UNDO.

    ASSIGN 
        iRellctr = 0 
        cFRTList = "" 
        cFOBList = ""
        .
              
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company  EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-ERROR.
    
    IF AVAILABLE oe-relh THEN
        RUN pOrderProcsNewBOL (
            INPUT ipcCompany, 
            OUTPUT iopiBOLNo
            ).
    
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.r-no EQ oe-relh.r-no
        USE-INDEX r-no:
        IF oe-rell.lot-no <> "" THEN
            ASSIGN 
                cFRTList = (IF LOOKUP(oe-rell.frt-pay,cFRTList) = 0 THEN 
                                cFRTList + "," + oe-rell.frt-pay 
                            ELSE 
                                cFRTList)
                cFOBList = (IF LOOKUP(oe-rell.fob-code,cFOBList) = 0 THEN 
                                cFOBList + "," + oe-rell.fob-code 
                            ELSE 
                                cFOBList)
                .                
    END.    
     
    IF length(cFRTList) > 0 THEN
        cFRTList = SUBSTR(cFRTList,2).
    IF length(cFOBList) > 0 THEN
        cFOBList = SUBSTR(cFOBList,2).
    
    IF NUM-ENTRIES(cFRTList) > 1 THEN
       iRellctr = NUM-ENTRIES(cFRTList).
    ELSE
       IF NUM-ENTRIES(cFOBList) > 1 THEN
           iRellctr = NUM-ENTRIES(cFOBList).
    
    /* get first order fOR release */
    FIND FIRST oe-rell NO-LOCK
         WHERE oe-rell.r-no EQ oe-relh.r-no
         USE-INDEX r-no NO-ERROR.
    
    FIND FIRST oe-ord NO-LOCK 
         WHERE oe-ord.company EQ oe-relh.company
           AND oe-ord.ord-no  EQ oe-rell.ord-no
         NO-ERROR.
    
    ASSIGN 
        cFRTPay = (IF cFRTList <> "" THEN 
                       SUBSTR(cFRTList,1,1) 
                   ELSE 
                       "")
        cFOBCode = (IF cFOBList <> "" THEN 
                       SUBSTR(cFOBList,1,1) 
                    ELSE 
                        "")
        .
        
    IF oe-rell.lot-no <> "" THEN DO:   
        ASSIGN 
            cFRTPay = (IF cFRTPay = "" THEN 
                           oe-rell.frt-pay 
                       ELSE 
                           cFRTPay)
            cFOBCode = (IF cFOBCode = "" THEN 
                           oe-rell.fob-code 
                        ELSE 
                           cFOBCode)
            .
        
        IF cFRTPay = "P" THEN 
            cFRTPay = "Prepaid". 
        ELSE IF cFRTPay = "C" THEN 
            cFRTPay = "Collect".
        ELSE IF cFRTPay = "B" THEN 
            cFRTPay = "Bill". 
        ELSE IF cFRTPay = "T" THEN 
            cFRTPay = "ThirdParty".
    
        IF cFOBCode = "O" THEN 
            cFOBCode = "Origin". 
        ELSE IF cFOBCode = "D" THEN 
            cFOBCode = "Destination".
    
        IF iRellctr > 1 THEN DO:
            IF NOT CAN-DO("P,C,B,T",substr(cFRTPay,1,1)) THEN
                cFRTPay =  oe-rell.frt-pay.

            IF NOT CAN-DO("O,D",substr(cFOBCode,1,1)) THEN 
                cFOBCode = oe-rell.fob-code.
        END.
        IF length(cFRTPay) > 1 THEN
           cFRTPay =  SUBSTR(cFRTPay,1,1).
        IF length(cFOBCode) > 1 THEN
           cFOBCode =  SUBSTR(cFOBCode,1,1).
    END.
    RELEASE reftable.
    
    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ oe-relh.company
          AND oe-bolh.release# EQ oe-relh.release#
          AND NOT CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company 
                                           AND oe-boll.b-no    EQ oe-bolh.b-no):      
        DELETE oe-bolh.    
    END.
  
    x = 1.
    FIND LAST oe-bolh NO-LOCK
         USE-INDEX b-no NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
        x = oe-bolh.b-no + 1.
        
    FIND FIRST shipto NO-LOCK
     WHERE shipto.company EQ oe-relh.company
       AND shipto.cust-no EQ oe-relh.cust-no
       AND shipto.ship-id EQ oe-relh.ship-id
     NO-ERROR.
         
    IF NOT AVAILABLE shipto THEN 
        FIND FIRST loc NO-LOCK
             WHERE loc.company EQ ipcCompany
             NO-ERROR.
             
    CREATE oe-bolh.
    ASSIGN
        oe-bolh.company  = oe-relh.company
        oe-bolh.loc      = IF AVAILABLE shipto THEN
                               shipto.loc
                           ELSE IF AVAILABLE loc THEN
                               loc.loc
                           ELSE
                               ""
        oe-bolh.b-no     = x
        oe-bolh.bol-no   = iopiBOLNo
        oe-bolh.release# = oe-relh.release#
        oe-bolh.bol-date = IF ipcBolPost EQ "Current" THEN 
                               TODAY 
                           ELSE 
                               oe-relh.rel-date
        oe-bolh.cust-no  = oe-relh.cust-no
        oe-bolh.ship-no  = oe-relh.ship-no
        oe-bolh.ship-id  = oe-relh.ship-id
        oe-bolh.carrier  = oe-relh.carrier
        oe-bolh.stat     = STRING(iplRelPost AND ipcRelPost  BEGINS "BOL","H/R")
        oe-bolh.b-ord-no = oe-relh.b-ord-no
        oe-bolh.rel-date = oe-relh.rel-date
        oe-bolh.r-no     = oe-relh.r-no
        oe-bolh.posted   = no
        oe-bolh.printed  = no
        oe-bolh.deleted  = no
        oe-bolh.frt-pay  = IF cFRTPay <> "" THEN 
                               cFRTPay 
                           ELSE IF AVAILABLE oe-ord THEN 
                               oe-ord.frt-pay 
                           ELSE 
                               ""
        oe-bolh.trailer  = oe-relh.trailer
        oe-bolh.upd-date = TODAY
        oe-bolh.upd-time = TIME
        oe-bolh.user-id  = ipcUserName
        .
  
    IF cFOBCode <> "" THEN DO:
        FIND FIRST reftable NO-LOCK 
             WHERE reftable.reftable EQ "oe-bolh.lot-no" 
               AND reftable.rec_key  EQ oe-bolh.rec_key
             USE-INDEX rec_key
             NO-ERROR.
        IF NOT AVAILABLE reftable THEN DO:
            CREATE reftable.
            ASSIGN 
                reftable.reftable = "oe-bolh.lot-no" 
                reftable.rec_key  = oe-bolh.rec_key
                reftable.code     = cFOBCode
                .
        END.         
        RELEASE reftable.
    END.
  
    RUN oe/custxship.p (
        INPUT oe-bolh.company,
        INPUT oe-bolh.cust-no,
        INPUT oe-bolh.ship-id,
        BUFFER shipto
        ).
  
    IF NOT AVAILABLE shipto THEN
        FIND FIRST shipto
            WHERE shipto.company EQ ipcCompany
              AND shipto.cust-no EQ oe-bolh.cust-no
            NO-LOCK NO-ERROR.
  
    IF AVAILABLE shipto THEN DO:
        ASSIGN
            oe-bolh.ship-no      = shipto.ship-no
            oe-bolh.ship-id      = shipto.ship-id
            oe-bolh.ship-i[1]    = shipto.notes[1]
            oe-bolh.ship-i[2]    = shipto.notes[2]
            oe-bolh.ship-i[3]    = shipto.notes[3]
            oe-bolh.ship-i[4]    = shipto.notes[4].            
            oe-bolh.cust-no = IF shipto.bill THEN 
                                  shipto.ship-id
                              ELSE
                                  oe-bolh.cust-no
            .
    END.
    
    IF oe-relh.ship-i[1] NE "" OR oe-relh.ship-i[2] NE "" OR
       oe-relh.ship-i[3] NE "" OR oe-relh.ship-i[4] NE "" THEN DO:
        ASSIGN
            oe-bolh.ship-i[1] = oe-relh.ship-i[1]
            oe-bolh.ship-i[2] = oe-relh.ship-i[2]
            oe-bolh.ship-i[3] = oe-relh.ship-i[3]
            oe-bolh.ship-i[4] = oe-relh.ship-i[4]
            .
        RUN pCopyShipNote (
            INPUT oe-relh.rec_key, 
            INPUT oe-bolh.rec_key
            ).
    END.
END PROCEDURE.

PROCEDURE pOrderProcsNewBOL PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLNo   AS INTEGER   NO-UNDO.
   
    DEFINE VARIABLE cFRTPay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFRTList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRellctr AS INTEGER   NO-UNDO.
    
    FIND FIRST oe-ctrl EXCLUSIVE-LOCK
         WHERE oe-ctrl.company EQ ipcCompany 
         NO-ERROR.
    
    DO WHILE TRUE:
        ASSIGN
            opiBOLNo      = oe-ctrl.n-bol
            oe-ctrl.n-bol = opiBOLNo + 1
            .
       
        IF oe-ctrl.n-bol GT 99999999 THEN 
            oe-ctrl.n-bol = 1.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ opiBOLNo
             USE-INDEX bol-no NO-ERROR.
        IF NOT AVAILABLE oe-bolh THEN
            LEAVE.
    END.

END PROCEDURE.

PROCEDURE pOrderProcsCreateBOLLines PRIVATE:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBOLID     AS INTEGER   NO-UNDO.

    DO TRANSACTION:
        FIND FIRST oe-relh NO-LOCK
             WHERE oe-relh.company  EQ ipcCompany
               AND oe-relh.release# EQ ipiReleaseID
             NO-ERROR.
             
        /* get first Order for release */
        FIND FIRST oe-rell NO-LOCK
             WHERE oe-rell.r-no EQ oe-relh.r-no 
             USE-INDEX r-no 
             NO-ERROR.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-relh.company
               AND oe-ord.ord-no  EQ oe-rell.ord-no
             NO-ERROR.
        
        /* Creates records in oe-boll table */
        RUN pOrderProcsMakeBOLLs (
            INPUT ipcCompany,
            INPUT ipiReleaseID,
            INPUT ipiBOLID
            ).

        FIND FIRST oe-bolh 
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.

        FOR EACH  oe-boll EXCLUSIVE-LOCK 
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:
              
            RUN oe/calcBolWeight.p (
                INPUT ROWID(oe-boll), 
                OUTPUT oe-boll.weight
                ).
           
        END.
        
        RUN oe/palcal2.p (
            INPUT ROWID(oe-bolh), 
            OUTPUT oe-bolh.tot-pallets
            ).
    
        RUN oe/bolhtots.p (
            INPUT ROWID(oe-bolh)
            ).
        IF (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Bol Processing") THEN do:       
            RUN oe/calcBolFrt.p (
                INPUT ROWID(oe-bolh), 
                OUTPUT oe-bolh.freight
                ).
        END.
      
        IF oe-bolh.freight EQ ? THEN 
            oe-bolh.freight = 0.
      
        IF oe-rell.link-no NE 0 THEN
            FIND FIRST oe-rel 
                 WHERE oe-rel.r-no EQ oe-rell.link-no
                 USE-INDEX seq-no NO-ERROR.
      
        IF NOT AVAILABLE oe-rel THEN
            FIND FIRST oe-rel
                 WHERE oe-rel.company  EQ oe-relh.company
                   AND oe-rel.ord-no   EQ oe-rell.ord-no
                   AND oe-rel.liNE     EQ oe-rell.liNE
                   AND oe-rel.i-no     EQ oe-rell.i-no
                   AND oe-rel.ship-id  EQ oe-relh.ship-id
                   AND oe-rel.rel-date LE oe-relh.rel-date
                   AND oe-rel.rel-no   EQ 0
                   AND oe-rel.b-ord-no EQ 0
                   AND oe-rel.link-no  EQ 0
                 USE-INDEX ord-item NO-ERROR.
      
        IF NOT AVAILABLE oe-rel THEN
            FIND FIRST oe-rel
                 WHERE oe-rel.company  EQ oe-relh.company
                   AND oe-rel.ord-no   EQ oe-rell.ord-no
                   AND oe-rel.liNE     EQ oe-rell.liNE
                   AND oe-rel.i-no     EQ oe-rell.i-no
                   AND oe-rel.rel-date LE oe-relh.rel-date
                   AND oe-rel.rel-no   EQ 0
                   AND oe-rel.b-ord-no EQ 0
                   AND oe-rel.link-no  EQ 0
                 USE-INDEX ord-item NO-ERROR.
      
        IF AVAILABLE oe-rel THEN
            RUN oe/rel-stat.p (
                INPUT ROWID(oe-rel), 
                OUTPUT oe-rel.stat
                ).
    END.
END PROCEDURE.

PROCEDURE pOrderProcsMakeBOLLs PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID     AS INTEGER   NO-UNDO.
        
    DEFINE VARIABLE iBOLLine        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBOLQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLPrint       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lBOLWeight      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnBOLPrint    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFoundBOLPrint  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnBOLWeight   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFoundBOLWeight AS LOGICAL   NO-UNDO.

    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "BOLPRINT", 
        INPUT "C" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnBOLPrint, 
        OUTPUT lFoundBOLPrint
        ).
    IF lFoundBOLPrint THEN
        cBOLPrint = cRtnBOLPrint NO-ERROR. 
                
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "BOLWeight", 
        INPUT "L" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnBOLWeight, 
        OUTPUT lFoundBOLWeight
        ).
        
    IF lFoundBOLWeight THEN
        lBOLWeight = LOGICAL(cRtnBOLWeight) NO-ERROR.
    
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID 
         NO-ERROR.
         
    FIND FIRST oe-bolh NO-LOCK
         WHERE oe-bolh.company EQ ipcCompany
           AND oe-bolh.bol-no  EQ ipiBOLID
         NO-ERROR.
         
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK
        BREAK 
        BY oe-rell.ord-no
        BY oe-rell.i-no
        BY SUBSTR(oe-rell.rec_key,5,4)
        BY SUBSTR(oe-rell.rec_key,1,4)
        BY SUBSTR(oe-rell.rec_key,10,100):
    
        FIND FIRST oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
              AND oe-boll.i-no    EQ oe-rell.i-no
              AND oe-boll.ord-no  EQ oe-rell.ord-no
            USE-INDEX b-no NO-ERROR.
        
        IF AVAILABLE oe-boll THEN 
            iBOLLine = oe-boll.bol-line.        
        ELSE DO:
            FIND LAST oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no
                USE-INDEX bol-line NO-ERROR.
            iBOLLine  = (IF AVAILABLE oe-boll THEN 
                             oe-boll.bol-line 
                         ELSE 0) + 1.
        END.

        CREATE oe-boll.
        ASSIGN
            oe-boll.company  = oe-bolh.company
            oe-boll.b-no     = oe-bolh.b-no
            oe-boll.bol-no   = oe-bolh.bol-no
            oe-boll.bol-line = iBOLLine
            oe-boll.ord-no   = oe-rell.ord-no
            oe-boll.rel-no   = oe-rell.rel-no
            oe-boll.b-ord-no = oe-rell.b-ord-no
            oe-boll.po-no    = oe-rell.po-no
            oe-boll.loc-bin  = oe-rell.loc-bin
            oe-boll.loc      = oe-rell.loc
            oe-boll.r-no     = oe-rell.r-no
            oe-boll.i-no     = oe-rell.i-no
            oe-boll.line     = oe-rell.line
            oe-boll.tag      = oe-rell.tag
            oe-boll.job-no   = oe-rell.job-no
            oe-boll.job-no2  = oe-rell.job-no2
            oe-boll.cust-no  = oe-rell.cust-no
            oe-boll.cases    = oe-rell.cases
            oe-boll.qty-case = oe-rell.qty-case
            oe-boll.partial  = oe-rell.partial
            oe-boll.s-code   = oe-rell.s-code
            oe-boll.qty      = oe-rell.qty
            oe-boll.lot-no   = oe-rell.lot-no
            oe-boll.sell-price = oe-rell.sell-price
            oe-boll.zeroPrice = oe-rell.zeroPrice
            oe-boll.enteredBy = oe-rell.enteredBy
            oe-boll.enteredDT = oe-rell.enteredDT
            .
               
        IF oe-boll.loc-bin EQ "" THEN 
            oe-boll.loc-bin = cBOLPrint.
       
        iBOLQty  = iBOLQty  + oe-boll.qty.
        
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ oe-rell.i-no
            NO-ERROR.
        
        IF AVAILABLE itemfg  AND
           oe-boll.qty-case  EQ 0 AND
           itemfg.case-count NE 0 THEN DO:
           
            oe-boll.qty-case = itemfg.case-count.
        
            IF oe-boll.qty-case NE 0 THEN
                oe-boll.cases = TRUNC((oe-boll.qty - oe-boll.partial) / oe-boll.qty-case,0).
        END.

        IF LAST-OF(oe-rell.i-no) THEN DO: 
            {oe/oe-bolpc.i ALL}
        END.
        
        IF AVAILABLE itemfg THEN
            ASSIGN
                oe-boll.weight = ((((oe-boll.cases * oe-boll.qty-case) +
                                 oe-boll.partial) / 100) * itemfg.weight-100)
                oe-bolh.tot-wt = oe-bolh.tot-wt + oe-boll.weight
                .
 
        IF lBOLWeight AND
           TRIM(oe-rell.tag) NE "" THEN 
            FIND FIRST loadtag NO-LOCK 
                 WHERE loadtag.company   EQ ipcCompany
                   AND loadtag.item-type EQ NO
                   AND loadtag.tag-no    EQ oe-rell.tag 
                 NO-ERROR.
                 
        IF AVAILABLE loadtag THEN
            ASSIGN oe-boll.weight = ((oe-rell.cases * loadtag.misc-dec[1]) +
                                     loadtag.misc-dec[3]).
        IF oe-boll.qty LT 0 THEN 
            oe-boll.tag = "".
    END. /* each oe-rell */
    IF (cFreightCalculationValue EQ "ALL" OR cFreightCalculationValue EQ "Bol Processing") THEN do:
        RUN oe/calcBolFrt.p (
            INPUT ROWID(oe-bolh), 
            OUTPUT oe-bolh.freight
            ).
    END.
               
END PROCEDURE.


PROCEDURE pOrderProcsPostRelease PRIVATE:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseID AS INTEGER NO-UNDO.
    
    FIND FIRST oe-relh EXCLUSIVE-LOCK
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-WAIT NO-ERROR. 

    FOR EACH  oe-rell
        WHERE oe-rell.company EQ ipcCompany
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no:
        FIND FIRST itemfg
              WHERE itemfg.company EQ ipcCompany
                AND itemfg.i-no    EQ oe-rell.i-no
              USE-INDEX i-no NO-ERROR.
        
        IF NOT AVAILABLE itemfg THEN
            RETURN.

        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ ipcCompany
               AND oe-ord.ord-no  EQ oe-rell.ord-no  /* used be oe-relh.ord-no */
             NO-ERROR.

        IF NOT AVAILABLE oe-ord OR
           (AVAILABLE oe-ord AND oe-ord.inv-no NE 0 AND oe-ord.stat EQ "X") THEN
            RETURN.
        ELSE DO:
            ASSIGN
                oe-rell.printed = yes
                oe-rell.posted  = yes
                .
            
            IF (oe-rell.s-code EQ "S" OR oe-rell.s-code EQ "B") AND
                NOT oe-relh.deleted THEN DO:
                itemfg.q-rel = itemfg.q-rel + oe-rell.qty.
                RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
                FIND FIRST itemfg-loc EXCLUSIVE-LOCK 
                     WHERE itemfg-loc.company EQ ipcCompany
                       AND itemfg-loc.i-no    EQ oe-rell.i-no
                       AND itemfg-loc.loc     EQ oe-rell.loc
                     NO-ERROR.
                IF AVAILABLE itemfg-loc THEN
                    itemfg-loc.q-rel = itemfg-loc.q-rel + oe-rell.qty.            
            END.            
            
            RELEASE itemfg.
            FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
            
            IF AVAILABLE oe-ord THEN
                FIND FIRST oe-ordl
                     WHERE oe-ordl.company EQ ipcCompany
                       AND oe-ordl.ord-no  EQ oe-ord.ord-no
                       AND oe-ordl.i-no    EQ oe-rell.i-no
                       AND oe-ordl.line    EQ oe-rell.line
                     USE-INDEX ord-no NO-ERROR.
            ELSE 
                RETURN.
            
            IF AVAILABLE oe-ordl AND 
                     NOT oe-relh.deleted AND 
                         oe-rell.s-code NE "I" THEN
                oe-ordl.t-rel-qty = oe-ordl.t-rel-qty + oe-rell.qty.
            
            RELEASE oe-ordl.
            
            RUN oe/upschrel.p (
                INPUT ROWID(oe-rell)
                ).
        END. /* ELSE DO: */
    END. /* each oe-rell */

    IF AVAILABLE oe-relh THEN
        oe-relh.posted = YES.

    RELEASE oe-relh.
END PROCEDURE.

PROCEDURE Order_DeleteBOL:
    /*------------------------------------------------------------------------------
     Purpose: Deletes BOL and Unposts related release
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdRelLib AS HANDLE NO-UNDO.
    
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
    DEFINE BUFFER bf-oe-boll FOR oe-boll
    .
    DEFINE VARIABLE dOut AS DECIMAL NO-UNDO.
    
    RUN sbo/oerel-recalc-act.p PERSISTENT SET hdRelLib.
    
    oplSuccess = YES.
    
    EMPTY TEMP-TABLE ttUpdateOrderReleaseStatus.
    
    FIND FIRST bf-oe-bolh NO-LOCK
         WHERE bf-oe-bolh.company EQ ipcCompany
           AND bf-oe-bolh.bol-no  EQ ipiBOLID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-bolh THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "BOL header is not available"
            .
            
        RETURN.
    END.
    FOR EACH  bf-oe-boll NO-LOCK
        WHERE bf-oe-boll.company EQ bf-oe-bolh.company
          AND bf-oe-boll.b-no    EQ bf-oe-bolh.b-no: 
              
        FIND FIRST ttUpdateOrderReleaseStatus 
             WHERE ttUpdateOrderReleaseStatus.ord-no EQ bf-oe-boll.ord-no 
             NO-ERROR.
             
        IF NOT AVAILABLE ttUpdateOrderReleaseStatus THEN DO:
            CREATE ttUpdateOrderReleaseStatus.
            ttUpdateOrderReleaseStatus.ord-no = bf-oe-boll.ord-no.
        END.
        
        RUN pReleaseLineUpdateBOLStatusLinks (
            INPUT  ROWID(bf-oe-boll),
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                oplSuccess = NO
                opcMessage = ERROR-STATUS:GET-MESSAGE(1)
                .
            
            RETURN.
        END.
                
        FIND CURRENT bf-oe-boll EXCLUSIVE-LOCK NO-ERROR.
        DELETE bf-oe-boll. /* deletes BOL line */
        
    END. /* each oe-boll */  
    
    RUN pBOLReleaseHeaderUpdateStatus (
        INPUT  ROWID(bf-oe-bolh),
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = ERROR-STATUS:GET-MESSAGE(1)
            .
        
        RETURN.
    END.    
    FIND CURRENT bf-oe-bolh EXCLUSIVE-LOCK NO-ERROR.
    DELETE bf-oe-bolh. /* deletes BOL header */

    FOR EACH ttUpdateOrderReleaseStatus 
        WHERE ttUpdateOrderReleaseStatus.ord-no NE 0
        BREAK BY ttUpdateOrderReleaseStatus.ord-no:
    
        IF LAST-OF(ttUpdateOrderReleaseStatus.ord-no) THEN DO:
            FOR EACH oe-rel
                WHERE oe-rel.company EQ ipcCompany
                  AND oe-rel.ord-no  EQ ttUpdateOrderReleaseStatus.ord-no:
                RUN oe/rel-stat.p (
                    INPUT ROWID(oe-rel), 
                    OUTPUT oe-rel.stat
                    ).
 
                IF AVAILABLE oe-rel AND VALID-HANDLE(hdRelLib) THEN DO:
                    RUN recalc-act-qty IN hdRelLib (
                        INPUT ROWID(oe-rel), 
                        OUTPUT dOut
                        ).
                
                END.
            END.          
        END.
    
        DELETE ttUpdateOrderReleaseStatus.
    END.
        
    RELEASE bf-oe-bolh.
    RELEASE bf-oe-bolh.

    FINALLY:
        IF VALID-HANDLE(hdRelLib) THEN
            DELETE PROCEDURE hdRelLib.
    END.
END PROCEDURE.

PROCEDURE pReleaseLineUpdateBOLStatusLinks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates release line status,links,order quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprioeboll AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    DEFINE BUFFER bf-oe-rell  FOR oe-rell.
    DEFINE BUFFER bf-oe-rel   FOR oe-rel.
    DEFINE BUFFER bf-oe-ordl  FOR oe-ordl.
    DEFINE BUFFER bf1-oe-boll FOR oe-boll.
    
    DEFINE VARIABLE iOrdNo AS INTEGER NO-UNDO.
   
    DISABLE TRIGGERS FOR LOAD OF bf-oe-ordl.
    
    oplSuccess = YES.
    
    FIND FIRST bf1-oe-boll NO-LOCK
         WHERE ROWID(bf1-oe-boll) EQ iprioeboll
         NO-ERROR.
    IF NOT AVAILABLE bf1-oe-boll THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "BOL Line is not available"
            .
            
        RETURN.
    END.
    FIND FIRST bf-oe-boll NO-LOCK
         WHERE bf-oe-boll.company  EQ bf1-oe-boll.company
           AND bf-oe-boll.ord-no   EQ bf1-oe-boll.ord-no
           AND bf-oe-boll.line     EQ bf1-oe-boll.line
           AND bf-oe-boll.i-no     EQ bf1-oe-boll.i-no
           AND bf-oe-boll.r-no     EQ bf1-oe-boll.r-no
           AND bf-oe-boll.rel-no   EQ bf1-oe-boll.rel-no
           AND bf-oe-boll.b-ord-no EQ bf1-oe-boll.b-ord-no
           AND bf-oe-boll.po-no    EQ bf1-oe-boll.po-no
           AND ROWID(bf-oe-boll)   NE ROWID(bf1-oe-boll)
           AND CAN-FIND(FIRST bf-oe-bolh 
                        WHERE bf-oe-bolh.b-no   EQ bf-oe-boll.b-no
                          AND bf-oe-bolh.posted EQ NO)
           NO-ERROR.
    
     IF NOT AVAILABLE bf-oe-boll THEN DO:
         FOR EACH  oe-rell NO-LOCK
             WHERE oe-rell.company  EQ bf1-oe-boll.company
               AND oe-rell.ord-no   EQ bf1-oe-boll.ord-no
               AND oe-rell.line     EQ bf1-oe-boll.line
               AND oe-rell.i-no     EQ bf1-oe-boll.i-no
               AND oe-rell.rel-no   EQ bf1-oe-boll.rel-no
               AND oe-rell.b-ord-no EQ bf1-oe-boll.b-ord-no
               AND oe-rell.po-no    EQ bf1-oe-boll.po-no
               AND CAN-FIND(FIRST oe-relh 
                            WHERE oe-relh.r-no EQ oe-rell.r-no):

            FIND FIRST bf-oe-ordl EXCLUSIVE-LOCK
                 WHERE bf-oe-ordl.company EQ bf1-oe-boll.company
                   AND bf-oe-ordl.ord-no  EQ oe-rell.ord-no
                   AND bf-oe-ordl.i-no    EQ oe-rell.i-no
                   AND bf-oe-ordl.line    EQ oe-rell.line
                 NO-ERROR.
            IF oe-rell.s-code NE "I" THEN
                bf-oe-ordl.t-rel-qty = bf-oe-ordl.t-rel-qty - oe-rell.qty.
            IF bf-oe-ordl.t-rel-qty LT 0 THEN
                bf-oe-ordl.t-rel-qty = 0.
            
            iOrdNo = oe-rell.ord-no.
           
            IF oe-rell.posted EQ YES THEN DO:
                FIND FIRST bf-oe-rell EXCLUSIVE-LOCK
                     WHERE ROWID(bf-oe-rell) EQ ROWID(oe-rell)
                     NO-WAIT NO-ERROR.
                IF LOCKED bf-oe-rell THEN DO:
                    ASSIGN
                        oplSuccess = NO
                        opcMessage = "release line is locked and not available to update"
                        .
                       
                    RETURN.
                END. 
                bf-oe-rell.posted = NO.
                RELEASE bf-oe-rell.
            END.
            FOR EACH  oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-boll.company
                  AND oe-rel.ord-no  EQ iOrdNo
                  AND oe-rel.link-no NE 0:
        
                FOR EACH  bf-oe-rell EXCLUSIVE-LOCK
                    WHERE bf-oe-rell.company  EQ oe-rel.company
                      AND bf-oe-rell.r-no     EQ oe-rel.link-no
                      AND bf-oe-rell.ord-no   EQ oe-rel.ord-no
                      AND bf-oe-rell.i-no     EQ oe-rel.i-no
                      AND bf-oe-rell.line     EQ oe-rel.line
                      AND bf-oe-rell.rel-no   EQ oe-rel.rel-no
                      AND bf-oe-rell.b-ord-no EQ oe-rel.b-ord-no
                      AND bf-oe-rell.po-no    EQ oe-rel.po-no
                      AND bf-oe-rell.posted   EQ YES
                    USE-INDEX r-no
                    BREAK BY bf-oe-rell.r-no:
                    
                    bf-oe-rell.link-no = oe-rel.r-no.
                    
                    IF LAST(bf-oe-rell.r-no) THEN 
                        LEAVE.
                END.
                
                FIND FIRST bf-oe-rel EXCLUSIVE-LOCK
                     WHERE ROWID(bf-oe-rel) EQ ROWID(oe-rel)
                     NO-ERROR.
                IF AVAILABLE bf-oe-rel THEN
                    bf-oe-rel.link-no = 0.
                
            END.
    
            RUN oe/rel-stat-upd.p (
                INPUT ROWID(oe-rell)
                ).
        END. /* each oe-rell */
    END.
    
    RELEASE bf-oe-boll.
    RELEASE bf-oe-bolh.
    RELEASE bf-oe-rell.
    RELEASE bf-oe-rel.
    RELEASE bf-oe-ordl.
    RELEASE bf1-oe-boll.
END PROCEDURE.

PROCEDURE pBOLReleaseHeaderUpdateStatus PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Updates release header status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprioebolh AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
   
    oplSuccess = YES.
    
    FIND FIRST bf-oe-bolh NO-LOCK
         WHERE ROWID(bf-oe-bolh) EQ iprioebolh
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-bolh THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "BOL header is not available" 
            .
            
        RETURN.
    END.    
    
    FIND FIRST bf-oe-relh EXCLUSIVE-LOCK 
         WHERE bf-oe-relh.company  EQ bf-oe-bolh.company
           AND bf-oe-relh.release# EQ bf-oe-bolh.release#
         NO-WAIT NO-ERROR.
    IF LOCKED bf-oe-relh THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Release header is locked and not available to update" 
            .
            
        RETURN.
    END.
    IF NOT AVAILABLE bf-oe-relh THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Release header is not available" 
            .
            
        RETURN.
    END.

    bf-oe-relh.posted = NO.
    
    RELEASE bf-oe-relh.
    RELEASE bf-oe-bolh.
END PROCEDURE.

PROCEDURE pRunAPIOutboundTrigger :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-relh FOR oe-relh.
    
    DEFINE INPUT PARAMETER ipcTriggerID AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lSuccess     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAPIID       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimaryID   AS CHARACTER NO-UNDO.
       
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.

    DEFINE VARIABLE hdOutboundProcs AS HANDLE NO-UNDO.

    RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

    IF AVAILABLE ipbf-oe-relh THEN DO:

        FOR EACH bf-oe-rell NO-LOCK 
            WHERE bf-oe-rell.company EQ ipbf-oe-relh.company
            AND bf-oe-rell.r-no EQ ipbf-oe-relh.r-no,
            FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ bf-oe-rell.company
            AND bf-itemfg.i-no EQ bf-oe-rell.i-no
            BREAK BY bf-oe-rell.r-no  /*In order to get .loc from first oe-rell as "shipFrom"*/
            BY bf-oe-rell.i-no:
            
            IF FIRST-OF(bf-oe-rell.r-no) THEN DO:
                ASSIGN 
                    cAPIID       = "SendRelease"
                    cPrimaryID   = STRING(ipbf-oe-relh.release#)
                    cDescription = cAPIID + " triggered by " + ipcTriggerID 
                                 + " from OrderProcs for Release: " + cPrimaryID
                    . 

                RUN Outbound_PrepareAndExecute IN hdOutboundProcs (
                    INPUT  ipbf-oe-relh.company,                /* Company Code (Mandatory) */
                    INPUT  bf-oe-rell.loc,               /* Location Code (Mandatory) */
                    INPUT  cAPIID,                  /* API ID (Mandatory) */
                    INPUT  "",               /* Client ID (Optional) - Pass empty in case to make request for all clients */
                    INPUT  ipcTriggerID,              /* Trigger ID (Mandatory) */
                    INPUT  "oe-relh",               /* Comma separated list of table names for which data being sent (Mandatory) */
                    INPUT  STRING(ROWID(ipbf-oe-relh)),  /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
                    INPUT  cPrimaryID,              /* Primary ID for which API is called for (Mandatory) */   
                    INPUT  cDescription,       /* Event's description (Optional) */
                    OUTPUT lSuccess,                /* Success/Failure flag */
                    OUTPUT cMessage                 /* Status message */
                    ) NO-ERROR.
            END.
        END.               
        /* Reset context at the end of API calls to clear temp-table 
           data inside OutboundProcs */
        RUN Outbound_ResetContext IN hdOutboundProcs.
    END.
    
    FINALLY:
        DELETE PROCEDURE hdOutboundProcs.
    END.
END PROCEDURE.

PROCEDURE pProcessImportedOrderHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates an order from ttOrder temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttOrder FOR ttOrder.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord       FOR oe-ord.
    DEFINE BUFFER bf-cust         FOR cust.
    DEFINE BUFFER bf-sman         FOR sman.
    DEFINE BUFFER bf-terms        FOR terms.
    DEFINE BUFFER bf-soldto       FOR soldto.
    DEFINE BUFFER bf-shipto       FOR shipto.
    DEFINE BUFFER bf-new-shipto   FOR shipto.
    DEFINE BUFFER bf-state-shipto FOR shipto.
    
    IF NOT AVAILABLE ipbf-ttOrder THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Input Order not found"
            .
        RETURN.    
    END.
    
    IF ipbf-ttOrder.action EQ cOrderActionCreate THEN DO:
        RUN pCreateOrderHeader (
            INPUT  ipbf-ttOrder.company,
            OUTPUT ipbf-ttOrder.orderID
            ).        
    END.

    FIND FIRST bf-oe-ord EXCLUSIVE-LOCK
         WHERE bf-oe-ord.company EQ ipbf-ttOrder.company
           AND bf-oe-ord.ord-no  EQ ipbf-ttOrder.orderID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = 'Unable to find/create Order#: ' + STRING(ipbf-ttOrder.orderID)
            .                
        RETURN.
    END.
    
    ASSIGN
        bf-oe-ord.company       = ipbf-ttOrder.company
        bf-oe-ord.loc           = ipbf-ttOrder.wareHouseID
        bf-oe-ord.ord-date      = ipbf-ttOrder.orderDate
        bf-oe-ord.promiseDate   = ipbf-ttOrder.promiseDate
        bf-oe-ord.stat          = ipbf-ttOrder.stat
        bf-oe-ord.due-code      = 'ON'
        bf-oe-ord.cust-no       = ipbf-ttOrder.customerID
        bf-oe-ord.sold-id       = ipbf-ttOrder.billToID
        bf-oe-ord.po-no         = ipbf-ttOrder.poID
        bf-oe-ord.spare-char-3  = ipbf-ttOrder.payloadID
        bf-oe-ord.s-pct[1]      = 100.00
        bf-oe-ord.cc-num        = ipbf-ttOrder.cardNo
        bf-oe-ord.cc-type       = ipbf-ttOrder.cardType
        bf-oe-ord.cc-expiration = ipbf-ttOrder.cardExpiryDate
        .

    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ ipbf-ttOrder.company
           AND bf-cust.cust-no EQ ipbf-ttOrder.customerID
         NO-ERROR.
    IF AVAILABLE bf-cust THEN DO:
        ASSIGN
            bf-oe-ord.cust-name  = bf-cust.name
            bf-oe-ord.addr[1]    = bf-cust.addr[1]
            bf-oe-ord.addr[2]    = bf-cust.addr[2]
            bf-oe-ord.city       = bf-cust.city
            bf-oe-ord.state      = bf-cust.state
            bf-oe-ord.zip        = bf-cust.zip
            bf-oe-ord.terms      = bf-cust.terms
            bf-oe-ord.over-pct   = bf-cust.over-pct
            bf-oe-ord.under-pct  = bf-cust.under-pct
            bf-oe-ord.fob-code   = bf-cust.fob-code
            bf-oe-ord.frt-pay    = bf-cust.frt-pay
            bf-oe-ord.tax-gr     = bf-cust.tax-gr
            bf-oe-ord.sman[1]    = bf-cust.sman
            bf-oe-ord.carrier    = bf-cust.carrier
            bf-oe-ord.contact    = bf-cust.contact
            bf-oe-ord.last-date  = bf-oe-ord.ord-date + bf-cust.ship-days
            bf-oe-ord.due-date   = bf-oe-ord.last-date
            bf-oe-ord.csrUser_id = bf-cust.csrUser_id
            .

        FIND FIRST bf-sman NO-LOCK
             WHERE bf-sman.company EQ bf-oe-ord.company
               AND bf-sman.sman    EQ bf-cust.sman
             NO-ERROR.
        IF AVAILABLE bf-sman THEN
            ASSIGN
                bf-oe-ord.sname[1]  = bf-sman.sname
                bf-oe-ord.s-comm[1] = bf-sman.scomm
                .

        FIND FIRST bf-terms NO-LOCK
             WHERE bf-terms.company EQ bf-cust.company
               AND bf-terms.t-code  EQ bf-cust.terms 
             NO-ERROR.
        IF AVAILABLE bf-terms THEN 
            bf-oe-ord.terms-d = bf-terms.dscr.

        RUN pGetcXMLShipToPrefix (
            INPUT  bf-cust.company,
            INPUT  bf-cust.cust-no,
            INPUT  ipbf-ttOrder.shipToID,
            OUTPUT ipbf-ttOrder.shipToID
            ).        

        bf-oe-ord.ship-id = ipbf-ttOrder.shipToID.
       
        /* Update ship-to information. Create ship-to record if not available */
        IF NOT CAN-FIND(FIRST shipto
                        WHERE shipto.company EQ bf-oe-ord.company
                          AND shipto.cust-no EQ bf-oe-ord.cust-no
                          AND shipto.ship-id EQ bf-oe-ord.ship-id) THEN DO:
            FIND FIRST bf-shipto NO-LOCK
                 WHERE bf-shipto.company EQ bf-oe-ord.company
                   AND bf-shipto.cust-no EQ bf-oe-ord.cust-no
                   AND bf-shipto.ship-id EQ bf-oe-ord.cust-no
                 NO-ERROR.            
    
            CREATE bf-new-shipto.
            ASSIGN
                bf-new-shipto.company      = ipbf-ttOrder.company
                bf-new-shipto.loc          = ipbf-ttOrder.warehouseID
                bf-new-shipto.cust-no      = ipbf-ttOrder.customerID
                bf-new-shipto.ship-no      = fGetNextShipNo(ipbf-ttOrder.company, ipbf-ttOrder.customerID)
                bf-new-shipto.ship-id      = IF ipbf-ttOrder.shipToID EQ "" THEN
                                                 STRING(bf-new-shipto.ship-no)
                                             ELSE
                                                 ipbf-ttOrder.shipToID
                bf-new-shipto.contact      = ipbf-ttOrder.contactName
                bf-new-shipto.area-code    = ipbf-ttOrder.shipToAreaCode
                bf-new-shipto.phone        = ipbf-ttOrder.shipToPhone
                bf-new-shipto.ship-name    = ipbf-ttOrder.shipToName
                bf-new-shipto.ship-addr[1] = ipbf-ttOrder.shipToAddress1
                bf-new-shipto.ship-addr[2] = ipbf-ttOrder.shipToAddress2
                bf-new-shipto.ship-city    = ipbf-ttOrder.shipToCity
                bf-new-shipto.ship-state   = ipbf-ttOrder.shipToState
                bf-new-shipto.ship-zip     = ipbf-ttOrder.shipToZip
                bf-new-shipto.country      = ipbf-ttOrder.shipToCountry
                bf-oe-ord.ship-id          = bf-new-shipto.ship-id
                .
    
            IF AVAILABLE bf-shipto THEN
                ASSIGN
                    bf-new-shipto.carrier   = bf-shipto.carrier
                    bf-new-shipto.dest-code = bf-shipto.dest-code
                    bf-new-shipto.ship-id   = IF ipbf-ttOrder.shipToID NE "" THEN 
                                                  ipbf-ttOrder.shipToID 
                                              ELSE 
                                                  STRING(bf-shipto.ship-no)
                    bf-new-shipto.loc       = bf-shipto.loc
                    bf-new-shipto.tax-code  = bf-shipto.tax-code
                    .
            ELSE IF AVAILABLE bf-cust THEN 
                ASSIGN
                    bf-new-shipto.carrier  = bf-cust.carrier
                    bf-new-shipto.tax-code = bf-cust.tax-gr
                    .
      
            FIND FIRST bf-state-shipto
                 WHERE bf-state-shipto.company    EQ bf-new-shipto.company
                   AND bf-state-shipto.cust-no    EQ bf-new-shipto.cust-no
                   AND bf-state-shipto.ship-id    NE bf-new-shipto.cust-no
                   AND bf-state-shipto.ship-state EQ bf-new-shipto.ship-state
                 NO-LOCK NO-ERROR.
            IF AVAILABLE bf-state-shipto THEN 
                bf-new-shipto.tax-code = bf-state-shipto.tax-code.
    
            ipbf-ttOrder.shipToID = bf-new-shipto.ship-id.
        END.
        
        /* Update sold-to information */
        FIND FIRST bf-soldto NO-LOCK
             WHERE bf-soldto.company EQ bf-oe-ord.company
               AND bf-soldto.cust-no EQ bf-oe-ord.cust-no
               AND bf-soldto.sold-id EQ bf-oe-ord.sold-id
             NO-ERROR.
        IF NOT AVAILABLE bf-soldto THEN
            FIND FIRST bf-soldto NO-LOCK
                 WHERE bf-soldto.company EQ bf-oe-ord.company
                   AND bf-soldto.cust-no EQ bf-oe-ord.cust-no
                   AND bf-soldto.sold-id EQ bf-oe-ord.cust-no
                 NO-ERROR.
    
        IF AVAILABLE bf-soldto THEN
            ASSIGN
                bf-oe-ord.sold-id      = bf-soldto.sold-id
                bf-oe-ord.sold-no      = bf-soldto.sold-no
                bf-oe-ord.sold-name    = bf-soldto.sold-name
                bf-oe-ord.sold-addr[1] = bf-soldto.sold-addr[1]
                bf-oe-ord.sold-addr[2] = bf-soldto.sold-addr[2]
                bf-oe-ord.sold-city    = bf-soldto.sold-city
                bf-oe-ord.sold-state   = bf-soldto.sold-state
                bf-oe-ord.sold-zip     = bf-soldto.sold-zip
                .
    END.    

    IF bf-oe-ord.frt-pay = 'B' THEN 
        bf-oe-ord.f-bill = YES.
    ELSE 
        bf-oe-ord.f-bill = NO.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Order created successfully"
        .
END PROCEDURE.

PROCEDURE pProcessImportedOrderLine:
/*------------------------------------------------------------------------------
 Purpose: Creates an order line from ttOrderLines temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiOrderID AS INTEGER   NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttOrderLine FOR ttOrderLine.  
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
        
    DEFINE VARIABLE dCostPerUOMTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDL    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFO    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVO    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDM    AS DECIMAL   NO-UNDO.    
    DEFINE VARIABLE cCostUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE hdCostProcs      AS HANDLE    NO-UNDO.
    
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
        
    RUN system\CostProcs.p PERSISTENT SET hdCostProcs.  

    FIND FIRST bf-oe-ord NO-LOCK
         WHERE bf-oe-ord.company EQ ipcCompany
           AND bf-oe-ord.ord-no  EQ ipiOrderID
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Order header not found"
            .
        RETURN.
    END.
    
    IF NOT AVAILABLE ipbf-ttOrderLine THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Order line not found for Order#: " + STRING(bf-oe-ord.ord-no)
            .
        RETURN.        
    END.
    
    IF ipbf-ttOrderLine.action EQ cOrderActionCreate THEN DO:
        RUN pCreateOrderLine (
            INPUT bf-oe-ord.company,
            INPUT bf-oe-ord.ord-no,
            INPUT ipbf-ttOrderLine.lineNo
            ).        
    END.
    
    FIND FIRST bf-oe-ordl EXCLUSIVE-LOCK
         WHERE bf-oe-ordl.company EQ bf-oe-ord.company
           AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
           AND bf-oe-ordl.line    EQ ipbf-ttOrderLine.lineNo
         NO-ERROR.
    IF NOT AVAILABLE bf-oe-ordl THEN DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Unable to create/find line: (" + STRING(ipbf-ttOrderLine.lineNo) + ") for Order#: " + bf-oe-ord.po-no
            .
        RETURN.
    END.
    
    FIND FIRST bf-itemfg NO-LOCK 
         WHERE bf-itemfg.company EQ bf-oe-ord.company
           AND bf-itemfg.i-no    EQ ipbf-ttOrderLine.manufacturerPartID
         NO-ERROR.
    IF NOT AVAILABLE bf-itemfg THEN DO:                
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "ManufacturerPartID '" + ipbf-ttOrderLine.manufacturerPartID + "' not found for Order#: " + bf-oe-ord.po-no
            .     
        RETURN.
    END.

    RUN GetCaseUOMList (
        INPUT  bf-oe-ord.company,
        OUTPUT gcCaseUOMList
        ).

    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ bf-oe-ord.company 
           AND bf-cust.cust-no EQ bf-oe-ord.cust-no
           NO-ERROR.

    ASSIGN
        bf-oe-ordl.type-code  = bf-oe-ord.type
        bf-oe-ordl.cust-no    = bf-oe-ord.cust-no
        bf-oe-ordl.po-no      = bf-oe-ord.po-no
        bf-oe-ordl.req-code   = bf-oe-ord.due-code
        bf-oe-ordl.req-date   = ipbf-ttOrderLine.dueDate
        bf-oe-ordl.prom-code  = bf-oe-ord.due-code
        bf-oe-ordl.prom-date  = bf-oe-ord.due-date
        bf-oe-ordl.ship-id    = bf-oe-ord.ship-id
        bf-oe-ordl.over-pct   = bf-oe-ord.over-pct   
        bf-oe-ordl.under-pct  = bf-oe-ord.under-pct
        bf-oe-ordl.stat       = bf-oe-ord.stat
        bf-oe-ordl.part-no    = TRIM(ipbf-ttOrderLine.supplierPartID)
        bf-oe-ordl.qty        = ipbf-ttOrderLine.quantity
        bf-oe-ordl.pr-uom     = ipbf-ttOrderLine.uom
        bf-oe-ordl.price      = ipbf-ttOrderLine.unitPrice
        bf-oe-ordl.est-no     = bf-oe-ord.est-no
        bf-oe-ordl.q-qty      = bf-oe-ord.t-fuel
        bf-oe-ordl.whsed      = bf-oe-ordl.est-no NE ''
        bf-oe-ordl.q-no       = bf-oe-ord.q-no
        bf-oe-ordl.i-no       = bf-itemfg.i-no
        bf-oe-ordl.i-name     = bf-itemfg.i-name
        bf-oe-ordl.cases-unit = bf-itemfg.case-pall
        bf-oe-ordl.part-dscr1 = bf-itemfg.part-dscr1
        bf-oe-ordl.part-dscr2 = bf-itemfg.part-dscr2         
        .

    IF AVAILABLE bf-cust THEN
        ASSIGN
            bf-oe-ordl.disc = bf-cust.disc
            bf-oe-ordl.tax  = bf-cust.sort EQ 'Y' AND bf-oe-ord.tax-gr NE ''
            .
    
    IF bf-oe-ordl.price EQ 0 THEN DO:                      
        FIND FIRST xoe-ord OF bf-oe-ord NO-LOCK NO-ERROR.
        /* oe/getprice.i */
        RUN getPrice (
            INPUT ROWID(bf-oe-ordl)
            ).
    END.
    
    ASSIGN
        bf-oe-ordl.s-man  = bf-oe-ord.sman
        bf-oe-ordl.s-pct  = bf-oe-ord.s-pct
        bf-oe-ordl.s-comm = bf-oe-ord.s-comm        
        .
      
    RUN GetCostForFGItem IN hdCostProcs (
        INPUT  bf-oe-ordl.company,
        INPUT  bf-oe-ordl.i-no,
        OUTPUT dCostPerUOMTotal, 
        OUTPUT dCostPerUOMDL,
        OUTPUT dCostPerUOMFO,
        OUTPUT dCostPerUOMVO,
        OUTPUT dCostPerUOMDM,
        OUTPUT cCostUOM,
        OUTPUT lFound
        ) .
     
    ASSIGN
        bf-oe-ordl.cost   = dCostPerUOMTotal
        bf-oe-ordl.t-cost = bf-oe-ordl.cost * bf-oe-ordl.qty / 1000
        .
    
    IF bf-oe-ordl.pr-uom NE "EA" THEN DO:  /*This assumes the qty uom is the same as the price uom on imported orders*/
        ASSIGN 
            bf-oe-ordl.spare-dec-1  = bf-oe-ordl.qty
            bf-oe-ordl.spare-char-2 = bf-oe-ordl.pr-uom
            bf-oe-ordl.t-price      = bf-oe-ordl.spare-dec-1 * oe-ordl.price
            bf-oe-ordl.pr-uom       = IF LOOKUP(bf-oe-ordl.pr-uom, gcCaseUOMList) GT 0 THEN 
                                          "CS" 
                                      ELSE 
                                          bf-oe-ordl.pr-uom
            .
            
        RUN Conv_QtyToEA (
            INPUT  bf-oe-ordl.company,
            INPUT  bf-oe-ordl.i-no, 
            INPUT  bf-oe-ordl.qty, 
            INPUT  bf-oe-ordl.pr-uom, 
            INPUT  bf-itemfg.case-count, 
            OUTPUT bf-oe-ordl.qty
            ).
    END.
    ELSE 
        bf-oe-ordl.t-price = bf-oe-ordl.qty * bf-oe-ordl.price.
     
    bf-oe-ordl.cas-cnt = IF bf-oe-ordl.qty LT bf-itemfg.case-count THEN 
                             bf-oe-ordl.qty 
                         ELSE 
                             bf-itemfg.case-count.
    
    IF bf-oe-ordl.req-date EQ ? THEN 
        bf-oe-ordl.req-date = bf-oe-ord.ord-date + 10.
    
    bf-oe-ordl.promiseDate = bf-oe-ordl.req-date.
    
    IF bf-oe-ord.promiseDate EQ ? THEN DO:
        FIND CURRENT bf-oe-ord EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-oe-ord THEN
            bf-oe-ord.promiseDate = bf-oe-ordl.promiseDate.
    END.
    
    RUN pCreateRelease (
        INPUT  bf-oe-ordl.company,
        INPUT  bf-oe-ordl.ord-no,
        INPUT  bf-oe-ordl.line,
        INPUT  bf-oe-ord.ship-id,  /* ShipTo */
        INPUT  "",                 /* ShipFrom */
        OUTPUT oplSuccess,
        OUTPUT opcMessage       
        ).

    DELETE OBJECT hdCostProcs.
END PROCEDURE.

PROCEDURE Order_GetSurchargeConfig:
/*------------------------------------------------------------------------------
 Purpose: Returns the surcharge config
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipdtOrderDate          AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate  AS DATETIME  NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSurchargeConfigList AS CHARACTER NO-UNDO.
    
    RUN pGetSurchargeConfig (
        INPUT  ipdtOrderDate,
        INPUT  ipdtOrderDeliveryDate,
        OUTPUT opcSurchargeConfigList
        ).    
END PROCEDURE.

PROCEDURE Order_GetOrderEvaluationParams:
/*------------------------------------------------------------------------------
 Purpose: Returns the order evaluation parameters
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipdtOrderDate           AS DATETIME NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate   AS DATETIME NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtEvaluationOrderDate AS DATETIME NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsWeekendOrder       AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsHolidayOrder       AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsWeekendDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsSameDayDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsNextDayDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsHolidayDelivery    AS LOGICAL  NO-UNDO.
    
    RUN pGetOrderEvaluationParams (
        INPUT  ipdtOrderDate,
        INPUT  ipdtOrderDeliveryDate,
        OUTPUT opdtEvaluationOrderDate,
        OUTPUT oplIsWeekendOrder,
        OUTPUT oplIsHolidayOrder,
        OUTPUT oplIsWeekendDelivery,
        OUTPUT oplIsSameDayDelivery,
        OUTPUT oplIsNextDayDelivery,
        OUTPUT oplIsHolidayDelivery
        ).
END.

PROCEDURE pGetSurchargeConfig:
/*------------------------------------------------------------------------------
 Purpose: Returns the surcharge config
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipdtOrderDate          AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate  AS DATETIME  NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSurchargeConfigList AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dtEvaluationOrderDate    AS DATETIME NO-UNDO.
    DEFINE VARIABLE lIsWeekendOrder          AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lIsHolidayOrder          AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lIsWeekendDelivery       AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lIsSameDayDelivery       AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lIsNextDayDelivery       AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lIsHolidayDelivery       AS LOGICAL  NO-UNDO.
    DEFINE VARIABLE lAllowMultipleSurcharges AS LOGICAL  NO-UNDO.
    
    RUN pGetOrderEvaluationParams (
        INPUT  ipdtOrderDate,
        INPUT  ipdtOrderDeliveryDate,
        OUTPUT dtEvaluationOrderDate,
        OUTPUT lIsWeekendOrder,
        OUTPUT lIsHolidayOrder,
        OUTPUT lIsWeekendDelivery,
        OUTPUT lIsSameDayDelivery,
        OUTPUT lIsNextDayDelivery,
        OUTPUT lIsHolidayDelivery
        ).

    IF lIsSameDayDelivery THEN
        opcSurchargeConfigList = opcSurchargeConfigList + "," + "APIOrderSurchargeSameDay".
    
    IF lIsWeekendDelivery THEN
        opcSurchargeConfigList = opcSurchargeConfigList + "," + "APIOrderSurchargeWeekendDelivery".

    IF lIsNextDayDelivery THEN
        opcSurchargeConfigList = opcSurchargeConfigList + "," + "APIOrderSurchargeNextDay".
    
    IF lIsWeekendOrder AND lIsWeekendDelivery THEN
        opcSurchargeConfigList = opcSurchargeConfigList + "," + "APIOrderSurchargeWeekendOrder".
                
    opcSurchargeConfigList = TRIM(opcSurchargeConfigList,",").    
END PROCEDURE.

PROCEDURE pGetOrderEvaluationParams:
/*------------------------------------------------------------------------------
 Purpose: Returns the order evaluation parameters
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipdtOrderDate           AS DATETIME NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtOrderDeliveryDate   AS DATETIME NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtEvaluationOrderDate AS DATETIME NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsWeekendOrder       AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsHolidayOrder       AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsWeekendDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsSameDayDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsNextDayDelivery    AS LOGICAL  NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsHolidayDelivery    AS LOGICAL  NO-UNDO.
    
    RUN pGetEvaluationOrderDate (
        INPUT  ipdtOrderDate,
        OUTPUT opdtEvaluationOrderDate
        ).
        
    ASSIGN
        oplIsHolidayOrder    = FALSE   /* Can be used if necessary in future */
        oplIsHolidayDelivery = FALSE   /* Can be used if necessary in future */
        oplIsWeekendOrder    = DYNAMIC-FUNCTION("sfCommon_IsDateWeekend", ipdtOrderDate)   
        oplIsWeekendDelivery = DYNAMIC-FUNCTION("sfCommon_IsDateWeekend", ipdtOrderDeliveryDate)
        oplIsSameDayDelivery = DYNAMIC-FUNCTION("sfCommon_GetDifferenceDays", ipdtOrderDeliveryDate, opdtEvaluationOrderDate) EQ 0
        oplIsNextDayDelivery = DYNAMIC-FUNCTION("sfCommon_GetDifferenceDays", ipdtOrderDeliveryDate, opdtEvaluationOrderDate) EQ 1        
        . 
    
    /* Special Case */
    /* Consider same day evaluation order date if order day is Saturday and delivery date is Monday */
    IF (DYNAMIC-FUNCTION("sfCommon_GetWeekday", opdtEvaluationOrderDate) EQ 7 AND DYNAMIC-FUNCTION("sfCommon_GetWeekday", ipdtOrderDeliveryDate) EQ 2 AND DYNAMIC-FUNCTION("sfCommon_GetDifferenceDays", ipdtOrderDeliveryDate, opdtEvaluationOrderDate) EQ 2) OR
    /* Consider same day order if evaluation order date is Sunday and delivery date is Monday */ 
       (DYNAMIC-FUNCTION("sfCommon_GetWeekday", opdtEvaluationOrderDate) EQ 1 AND DYNAMIC-FUNCTION("sfCommon_GetWeekday", ipdtOrderDeliveryDate) EQ 2 AND DYNAMIC-FUNCTION("sfCommon_GetDifferenceDays", ipdtOrderDeliveryDate, opdtEvaluationOrderDate) EQ 1) THEN
        ASSIGN
            oplIsSameDayDelivery = TRUE
            oplIsNextDayDelivery = FALSE
            .    
END.

PROCEDURE pGetEvaluationOrderDate:
/*------------------------------------------------------------------------------
 Purpose: Returns the order evaluation date
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipdtOrderEnteredTime  AS DATETIME    NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtEvaluationOrdDate AS DATE        NO-UNDO.
    
    DEFINE VARIABLE iOrderDateHour AS INTEGER     NO-UNDO.
    
    opdtEvaluationOrdDate = ipdtOrderEnteredTime.
    
    RUN spCommon_GetHoursFromDateTime (
        INPUT  ipdtOrderEnteredTime, 
        OUTPUT iOrderDateHour
        ).

    /* 14 here is 2:00 PM */
    IF iOrderDateHour GE 14 THEN
        opdtEvaluationOrdDate = ADD-INTERVAL(opdtEvaluationOrdDate, 1, "days").
END PROCEDURE.

/* ************************  Function Implementations ***************** */


FUNCTION fGetOEAutoApprovalLog RETURNS LOGICAL 
	(INPUT ipcCompany AS CHARACTER, INPUT ipcCustomerID AS CHARACTER, INPUT ipcShipToID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the NK1 OEAutoApproval log field value
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lOEAutoApproval AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult         AS CHARACTER NO-UNDO.

    RUN sys/ref/nk1look.p (
        INPUT  ttOrder.company,
        INPUT  "OEAutoApproval", 
        INPUT  "L", 
        INPUT  YES,                 /* use shipto */ 
        INPUT  YES,                 /* use cust*/ 
        INPUT  ttOrder.customerID, 
        INPUT  ttOrder.shipToID, 
        OUTPUT cResult, 
        OUTPUT lFound
        ).
    IF lFound THEN
       lOEAutoApproval = LOGICAL(cResult) NO-ERROR.   
       
   RETURN lOEAutoApproval.     
END FUNCTION.

FUNCTION fGetSettingJobCreate RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the setting for NK1 Jobcreat
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "JobCreat", "L", NO, NO, "", "", 
        OUTPUT cReturn, OUTPUT lFound).
        
    RETURN lFound AND cReturn EQ "YES".
        
END FUNCTION.

FUNCTION fGetNextOrderNo RETURNS INTEGER PRIVATE 
    ( ipcCompany AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the next order number
 Notes:
------------------------------------------------------------------------------*/            
    DEFINE VARIABLE iNextOrderID AS INTEGER NO-UNDO.

    RUN sys/ref/asiseq.p (
        INPUT  ipcCompany,
        INPUT  'order_seq',
        OUTPUT iNextOrderID
        ) NO-ERROR.

    /* Supposed to be a new order number, so cannot be found on an existing order */
    DO WHILE CAN-FIND(FIRST oe-ord
                      WHERE oe-ord.company EQ ipcCompany
                        AND oe-ord.ord-no  EQ iNextOrderID):
        RUN sys/ref/asiseq.p (
            INPUT  ipcCompany,
            INPUT  'order_seq',
            OUTPUT iNextOrderID
            ) NO-ERROR.
    END.
    
    RETURN iNextOrderID.
END FUNCTION.

FUNCTION fGetNextShipNo RETURNS INTEGER PRIVATE
    (ipcCompany AS CHARACTER, ipcCustNo AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Returns the next order number
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE hdCustomerProcs AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iShipToNo       AS INTEGER NO-UNDO.
    
    RUN system/CustomerProcs.p PERSISTENT SET hdCustomerProcs.
    
    RUN Customer_GetNextShipToNo IN hdCustomerProcs (
        INPUT  ipcCompany,
        INPUT  ipcCustNo,
        OUTPUT iShipToNo
        ).
    
    DELETE PROCEDURE hdCustomerProcs.
    
    RETURN iShipToNo.
END FUNCTION.
