
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
DEFINE INPUT PARAMETER iplPromptForQuotes AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttQuantityCost
    FIELD iQty       AS INTEGER
    FIELD dCostPerEA AS DECIMAL
    .
DEFINE VARIABLE cVendor             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVendorItem         AS CHARACTER NO-UNDO INITIAL "In-house Manufacture".
DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMiscEstimateSource AS CHARACTER NO-UNDO.

DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  ********************   ******* */

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
    RUN pBuildQuantitiesAndCostsFromProbe(BUFFER eb).
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
IF NOT AVAILABLE bf-vendItemCost THEN 
DO:
    CREATE bf-vendItemCost.
    ASSIGN
        bf-vendItemCost.Company        = eb.company
        bf-vendItemCost.ItemID         = eb.stock-no
        bf-vendItemCost.itemType       = "FG"
        bf-vendItemCost.estimateNO     = eb.est-no
        bf-vendItemCost.formNo         = eb.form-no
        bf-vendItemCost.blankNo        = eb.blank-no
        bf-vendItemCost.vendorID       = cVendor
        bf-vendItemCost.VendorUOM      = "EA"
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
    IF NOT AVAILABLE bf-vendItemCostLevel THEN 
    DO:
        CREATE bf-vendItemCostLevel.
        ASSIGN 
            bf-vendItemCostLevel.vendItemCostID = bf-vendItemCost.vendItemCostID
            bf-vendItemCostLevel.quantityBase   = ttQuantityCost.iQty
            .
    END.
    bf-vendItemCostLevel.costPerUOM = ttQuantityCost.dCostPerEA.
END.
RUN RecalculateFromAndTo (bf-vendItemCost.vendItemCostID, OUTPUT lError, OUTPUT cMessage).

RELEASE bf-vendItemCost.
RELEASE bf-vendItemCostLevel.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildQuantitiesAndCostsFromProbe PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Queries each probe for an estimate, building a tt of unique quantities, 
     starting with the latest probe for a given quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE cEstNo AS CHARACTER.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
    
    FOR EACH probe NO-LOCK 
        WHERE probe.company EQ ipbf-eb.company
        AND probe.est-no EQ cEstNo
        BY probe.probe-date DESCENDING
        BY probe.probe-time DESCENDING:
        FIND FIRST ttQuantityCost
            WHERE ttQuantityCost.iQty EQ probe.est-qty
            NO-ERROR.
        IF NOT AVAILABLE ttQuantityCost THEN 
        DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty       = probe.est-qty
                ttQuantityCost.dCostPerEA = probe.sell-price / 1000
                .  
        END.            
    END.        

END PROCEDURE.  

PROCEDURE pBuildQuantitiesAndCostsFromQuote PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Queries each valid/non-expired quote for an estimate 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE cEstNo      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iQuoteNo    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQuotePrice AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cQuoteUom   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChoice     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iQuoteQty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE rwRowid     AS ROWID     NO-UNDO.
    
    EMPTY TEMP-TABLE ttQuantityCost.
    cEstNo = ipbf-eb.sourceEstimate.
    RUN util/rjust.p (INPUT-OUTPUT cEstNo,8). 
    
    IF iplPromptForQuotes AND CAN-FIND(FIRST quotehd WHERE quotehd.company EQ ipbf-eb.company AND quotehd.est-no EQ cEstNo) THEN
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
            
    IF NOT iplPromptForQuotes THEN
     RUN pGetLastQuoteNO(INPUT ipbf-eb.company, INPUT ipbf-eb.loc, INPUT ipbf-eb.est-no, OUTPUT iQuoteNo) .
    
    FOR EACH quotehd NO-LOCK
        WHERE quotehd.company  EQ ipbf-eb.company
        AND (quotehd.q-no EQ iQuoteNo OR iQuoteNo EQ 0) 
        AND quotehd.loc EQ ipbf-eb.loc
        AND quotehd.est-no    EQ cEstNo 
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND quotehd.quo-date LE TODAY
        ,
        FIRST quoteitm NO-LOCK 
        WHERE quoteitm.company EQ quotehd.company
        AND quoteitm.loc EQ quotehd.loc
        AND quoteitm.q-no EQ quotehd.q-no
        AND quoteitm.i-no EQ ipbf-eb.stock-no
        ,
        EACH quoteqty NO-LOCK
        WHERE quoteqty.company EQ quoteitm.company
        AND quoteqty.loc EQ quoteitm.loc
        AND quoteqty.q-no EQ quoteitm.q-no
        AND quoteqty.line EQ quoteitm.line
        BY quotehd.quo-date DESCENDING:
            
        FIND FIRST ttQuantityCost 
            WHERE ttQuantityCost.iQty EQ quoteqty.qty
            NO-ERROR.
        IF NOT AVAILABLE ttQuantityCost THEN 
        DO:
            CREATE ttQuantityCost.
            ASSIGN 
                ttQuantityCost.iQty = quoteqty.qty
                ttQuantityCost.dCostPerEA = quoteqty.price .
            IF quoteqty.uom NE "EA" THEN 
                RUN Conv_ValueToEA(quoteqty.company, quoteitm.i-no, quoteqty.price, quoteqty.uom, 0, OUTPUT ttQuantityCost.dCostPerEA).
        END.               
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
    DEFINE VARIABLE cRtnChar  AS CHARACTER.
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "MiscEstimateSource", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound). 
    opcMiscEstimateSource = STRING(cRtnChar) NO-ERROR .

END PROCEDURE.     

PROCEDURE pGetLastQuoteNO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcloc      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimate AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opiQuoteNO AS INTEGER   NO-UNDO.    
        
    FOR EACH quotehd FIELDS(q-no) NO-LOCK  
        WHERE quotehd.company EQ ipcCompany 
        AND quotehd.loc EQ ipcloc
        AND quotehd.quo-date LE TODAY 
        AND (quotehd.expireDate GE TODAY OR quotehd.expireDate EQ ?)
        AND quotehd.est-no EQ ipcEstimate  
        BREAK BY quotehd.q-no DESC:
        opiQuoteNO = quotehd.q-no .
        LEAVE.
    END.

END PROCEDURE.   
