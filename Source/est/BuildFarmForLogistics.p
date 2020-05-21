
/*------------------------------------------------------------------------
    File        : BuildFarmForLogistics.p
    Purpose     : 

    Syntax      :

    Description : Given an Eb ROWID, build the VendItemCost information for 

    Author(s)   : 
    Created     : Sun Apr 26 21:45:36 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriEb AS ROWID.

DEFINE TEMP-TABLE ttQuantityCost
    FIELD iQty AS INTEGER
    FIELD dCost AS DECIMAL
    .
DEFINE VARIABLE cVendor AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorItem AS CHARACTER NO-UNDO INITIAL "In-house Manufacture".
DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO INITIAL "EA".
DEFINE VARIABLE ghVendorCost AS HANDLE.
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscEstimateSource AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  ********************   ******* */
RUN system\VendorCostProcs.p PERSISTENT SET ghVendorCost.

FIND FIRST eb NO-LOCK
    WHERE ROWID(eb) EQ ipriEb
    NO-ERROR.
IF NOT AVAILABLE eb THEN RETURN.
FIND FIRST cust NO-LOCK 
    WHERE cust.company EQ eb.company
    AND cust.active EQ "X"
    NO-ERROR.
IF AVAILABLE cust THEN 
    cVendor = cust.cust-no.
RUN pGetCostFrom(INPUT eb.company,OUTPUT cMiscEstimateSource). 
IF cMiscEstimateSource EQ "Estimate" THEN
   RUN pBuildQuantitiesAndCosts(BUFFER eb).
ELSE IF cMiscEstimateSource EQ "Quote" THEN
DO:
   RUN pBuildQuantitiesAndCostsFromQuote(BUFFER eb).    
END.

FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
    WHERE bf-vendItemCost.company EQ eb.company
    AND bf-vendItemCost.estimateNo EQ eb.est-no
    AND bf-vendItemCost.formNo EQ eb.form-no
    AND bf-vendItemCost.blankNo EQ eb.blank-no
    AND bf-vendItemCost.itemID EQ eb.stock-no
    AND bf-vendItemCost.itemType EQ "FG"
    AND bf-vendItemCost.vendorID NE ""
    NO-ERROR.
IF NOT AVAIL bf-vendItemCost THEN
    FIND FIRST bf-vendItemCost EXCLUSIVE-LOCK
        WHERE bf-vendItemCost.company EQ eb.company
        AND bf-vendItemCost.estimateNo EQ eb.est-no
        AND bf-vendItemCost.formNo EQ eb.form-no
        AND bf-vendItemCost.blankNo EQ eb.blank-no
        AND bf-vendItemCost.itemID EQ eb.stock-no
        AND bf-vendItemCost.itemType EQ "FG"
        AND bf-vendItemCost.vendorID EQ ""
        NO-ERROR.
IF NOT AVAILABLE bf-vendItemCost THEN DO:
    CREATE bf-vendItemCost.
    ASSIGN
        bf-vendItemCost.Company        = eb.company
        bf-vendItemCost.ItemID         = eb.stock-no
        bf-vendItemCost.itemType       = "FG"
        bf-vendItemCost.estimateNO     = eb.est-no
        bf-vendItemCost.formNo         = eb.form-no
        bf-vendItemCost.blankNo        = eb.blank-no
        bf-vendItemCost.vendorID       = cVendor
        bf-vendItemCost.VendorUOM      = cCostUOM
        bf-vendItemCost.vendorItemID   = cVendorItem
        bf-vendItemCost.effectiveDate  = TODAY 
        bf-vendItemCost.expirationDate = 12/31/2099
        .
END.
FOR EACH ttQuantityCost:
    FIND FIRST bf-vendItemCostLevel EXCLUSIVE-LOCK
        WHERE bf-vendItemCostLevel.vendItemCostID EQ bf-vendItemCost.vendItemCostID
        AND bf-vendItemCostLevel.quantityBase EQ ttQuantityCost.iQty
        NO-ERROR.
    IF NOT AVAILABLE bf-vendItemCostLevel THEN DO:
        CREATE bf-vendItemCostLevel.
        ASSIGN 
            bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID
            bf-vendItemCostLevel.quantityBase = ttQuantityCost.iQty
            .
    END.
    bf-vendItemCostLevel.costPerUOM = ttQuantityCost.dCost.
END.
RUN RecalculateFromAndTo IN ghVendorCost (bf-vendItemCost.vendItemCostID, OUTPUT lError, OUTPUT cMessage).

RELEASE bf-vendItemCost.
RELEASE bf-vendItemCostLevel.

DELETE OBJECT ghVendorCost.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildQuantitiesAndCosts PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE iCount AS INTEGER.
    DEFINE VARIABLE cEstNo AS CHARACTER.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
 
    FIND FIRST est NO-LOCK 
        WHERE est.company EQ ipbf-eb.company
        AND est.est-no EQ cEstNo
        NO-ERROR.
    FIND FIRST est-qty NO-LOCK
         WHERE est-qty.company EQ ipbf-eb.company
         AND est-qty.est-no EQ cEstNo
         NO-ERROR .    
    IF AVAILABLE est-qty THEN 
    DO iCount = 1 TO EXTENT(est-qty.qty):
        IF est-qty.qty[iCount] NE 0 THEN DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty = est-qty.qty[iCount].
        END.
    END.
    FOR EACH ttQuantityCost:
        FIND FIRST probe NO-LOCK 
            WHERE probe.company EQ ipbf-eb.company
            AND probe.est-no EQ cEstNo
            AND probe.est-qty EQ ttQuantityCost.iQty
            NO-ERROR.
        IF NOT AVAILABLE probe THEN 
            FIND FIRST probe NO-LOCK 
                WHERE probe.company EQ ipbf-eb.company
                AND probe.est-no EQ cEstNo
                AND probe.est-qty GT ttQuantityCost.iQty
                NO-ERROR.
        IF NOT AVAILABLE probe THEN 
            FIND FIRST probe NO-LOCK 
                WHERE probe.company EQ ipbf-eb.company
                AND probe.est-no EQ cEstNo
                NO-ERROR.
        IF AVAILABLE probe THEN 
            ttQuantityCost.dCost = probe.sell-price / 1000.
    END.

END PROCEDURE.  

PROCEDURE pBuildQuantitiesAndCostsFromQuote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE iCount AS INTEGER.
    DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO.
    DEFIN VARIABLE iQuoteNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
    
    DEFIN VARIABLE dQuotePrice AS DECIMAL .
    DEFIN VARIABLE cQuoteUom AS CHARACTER.
    DEFIN VARIABLE cChoice  AS CHARACTER .
    DEFIN VARIABLE iQuoteQty  AS INTEGER.
    DEFINE VARIABLE rwRowid AS ROWID NO-UNDO.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8). 
    
    FIND FIRST est NO-LOCK 
        WHERE est.company EQ ipbf-eb.company
        AND est.est-no EQ cEstNo
        NO-ERROR.
    FIND FIRST est-qty NO-LOCK
         WHERE est-qty.company EQ ipbf-eb.company
         AND est-qty.est-no EQ cEstNo
         NO-ERROR .
         
    IF AVAILABLE est-qty THEN 
    DO iCount = 1 TO EXTENT(est-qty.qty):   
        IF est-qty.qty[iCount] NE 0 THEN DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty = est-qty.qty[iCount].
        END.
    END.
    
    FOR EACH quotehd NO-LOCK
            WHERE quotehd.company  EQ ipbf-eb.company
            AND quotehd.est-no    EQ cEstNo :
            iCount = iCount + 1.
            iQuoteNo = quotehd.q-no.
    END.
    IF iCount GT 1 THEN
    DO:
       FIND FIRST est NO-LOCK
            WHERE est.company EQ ipbf-eb.company
            AND est.est-no EQ ipbf-eb.est-no NO-ERROR .        
        RUN oe/QuotePopup.w("",ipbf-eb.company,
                          ipbf-eb.loc,
                          cEstNo,
                          ipbf-eb.stock-no,
                          INPUT-OUTPUT dQuotePrice,
                          INPUT-OUTPUT cQuoteUom,
                          INPUT-OUTPUT iQuoteQty,
                          INPUT-OUTPUT iQuoteNo,
                          OUTPUT cChoice,
                          OUTPUT rwRowid).  
    END.
    
    FOR EACH ttQuantityCost:
    
       FIND FIRST quoteqty NO-LOCK
            WHERE quoteqty.company EQ ipbf-eb.company
              AND quoteqty.q-no EQ iQuoteNo
              AND quoteqty.qty  EQ ttQuantityCost.iQty
              AND rowid(quoteqty) EQ rwRowid NO-ERROR .
       IF NOT AVAIL quoteqty THEN
            FIND FIRST quoteqty NO-LOCK
                 WHERE quoteqty.company EQ ipbf-eb.company
                 AND quoteqty.q-no EQ iQuoteNo
                 AND quoteqty.qty  EQ ttQuantityCost.iQty NO-ERROR .
       IF NOT AVAIL quoteqty THEN
           FIND FIRST quoteqty NO-LOCK
            WHERE quoteqty.company EQ ipbf-eb.company
              AND quoteqty.q-no EQ iQuoteNo NO-ERROR .
        IF AVAIL quoteqty THEN      
        ttQuantityCost.dCost = quoteqty.price .
    END.       

END PROCEDURE. 
          
PROCEDURE pGetCostFrom PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcMiscEstimateSource AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound AS LOGICAL.
    DEFINE VARIABLE cRtnChar AS CHARACTER.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "MiscEstimateSource", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound). 
    opcMiscEstimateSource = STRING(cRtnChar) NO-ERROR .

END PROCEDURE.          
