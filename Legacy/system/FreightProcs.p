
/*------------------------------------------------------------------------
    File        : FreightProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures and functions related to calculating estimated freight based on estRelease table or standard carrier and carrier matrix logic

    Author(s)   : BV
    Created     : Fri May 10 09:57:28 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE giTemp                   AS INTEGER NO-UNDO.
DEFINE VARIABLE giAllFormsIndicator      AS INTEGER INITIAL -1.
DEFINE VARIABLE giAllBlanksIndicator     AS INTEGER INITIAL -1.
DEFINE VARIABLE gdAllQuantitiesIndicator AS INTEGER INITIAL -1.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fCalcHandlingCostTotal RETURNS DECIMAL PRIVATE
    (ipdHandlingCostPerPallet AS DECIMAL,
    ipiPallets AS INTEGER) FORWARD.

FUNCTION fCalcStorageCostTotal RETURNS DECIMAL PRIVATE
    (ipdStorageCostPerPalletPerMonth AS DECIMAL,
    ipiPallets AS INTEGER,
    ipdPalletMultiplier AS DECIMAL,
    ipdMonthsAtShipFrom AS DECIMAL) FORWARD.

FUNCTION fGetNextEstReleaseRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextEstReleaseID RETURNS INTEGER PRIVATE
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CalcStorageAndHandlingForEstRelease:
    /*------------------------------------------------------------------------------
     Purpose: Calculates a given estReleaseID's Storage and Handling
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dPallets        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPalletsRounded AS DECIMAL NO-UNDO.

    FIND FIRST estRelease EXCLUSIVE-LOCK   
        WHERE estRelease.estReleaseID EQ ipiEstReleaseID
        NO-ERROR.
    IF AVAILABLE estRelease THEN 
    DO:
        ASSIGN 
            estRelease.quantityPerSubUnit      = MAXIMUM(estRelease.quantityPerSubUnit,1)
            estRelease.quantitySubUnitsPerUnit = MAXIMUM(estRelease.quantitySubUnitsPerUnit,1)
            .
        IF estRelease.quantityOfUnits EQ 0 THEN 
            ASSIGN /*calculate and round up*/ 
                dPallets                   = estRelease.quantityRelease / (estRelease.quantityPerSubUnit * estRelease.quantitySubUnitsPerUnit) 
                dPalletsRounded            = ROUND(dPallets,0)
                estRelease.quantityOfUnits = INTEGER(dPalletsRounded) + INTEGER(dPallets GT dPalletsRounded)
                .
        estRelease.storageCostTotal = fCalcStorageCostTotal(estRelease.storageCost,estRelease.quantityOfUnits,estRelease.palletMultiplier, estRelease.monthsAtShipFrom).
        estRelease.handlingCostTotal = fCalcHandlingCostTotal(estRelease.handlingCost, estRelease.quantityOfUnits).
    
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid estReleaseID " + STRING(ipiEstReleaseID)
            .
    RELEASE estRelease.

END PROCEDURE.

PROCEDURE CreateEstRelease:
    /*------------------------------------------------------------------------------
     Purpose: Creates an EstRelease Record based on required inputs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCreatedMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estRelease FOR estRelease.
    
    RUN pCreateEstReleaseBuffer(ipcCompany, ipcEstimateNo, ipiFormNo, ipiBlankNo, ipdQuantity, 
        OUTPUT opiEstReleaseID, OUTPUT oplCreated, OUTPUT opcCreatedMessage, BUFFER bf-estRelease).
        
END PROCEDURE.

PROCEDURE CreateReleaseForEstBlank:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstBlank AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estRelease FOR estRelease.
    DEFINE VARIABLE cCarrierDefault AS CHARACTER NO-UNDO.
      
    FIND FIRST eb NO-LOCK 
        WHERE ROWID(eb) EQ ipriEstBlank
        NO-ERROR.
    IF NOT AVAILABLE eb THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Estimate Blank RowID is Invalid"
            .
        RETURN.
    END.  
    RUN pCreateEstReleaseBuffer(eb.company, eb.est-no, eb.form-no, eb.blank-no, eb.eqty, 
        OUTPUT opiEstReleaseID, OUTPUT oplError, OUTPUT opcMessage, BUFFER bf-estRelease).
    IF AVAILABLE bf-estRelease THEN 
    DO:
        ASSIGN 
            bf-estRelease.customerID              = eb.cust-no
            bf-estRelease.shipToID                = eb.ship-id
            bf-estRelease.carrierID               = eb.carrier
            bf-estRelease.shipFromLocationID      = eb.loc    
            bf-estRelease.quantityPerSubUnit      = eb.cas-cnt
            bf-estRelease.quantitySubUnitsPerUnit = eb.cas-pal
            bf-estRelease.stackHeight             = eb.stackHeight
            bf-estRelease.quantityRelease         = bf-estRelease.quantity 
            .
        RUN GetStorageAndHandlingForLocation(eb.company, eb.loc, eb.stackHeight, 
            OUTPUT bf-estRelease.storageCost, OUTPUT bf-estRelease.handlingCost,
            OUTPUT oplError, OUTPUT opcMessage).
        RUN GetShipToCarrierAndZone(eb.company, eb.loc, eb.cust-no, eb.ship-id, 
            OUTPUT cCarrierDefault, OUTPUT bf-estRelease.carrierZone, 
            OUTPUT oplError, OUTPUT opcMessage).
    END. 
    
END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.

    RUN pDeleteEstReleases(ipcCompany, ipcEstimateNo, gdAllQuantitiesIndicator, giAllFormsIndicator, giAllBlanksIndicator).

END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimateBlank:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    
    RUN pDeleteEstReleases(ipcCompany, ipcEstimateNo, gdAllQuantitiesIndicator, ipiFormNo, ipiBlankNo).

END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimateForm:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    
    RUN pDeleteEstReleases(ipcCompany, ipcEstimateNo, gdAllQuantitiesIndicator, ipiFormNo, giAllBlanksIndicator).

END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimateQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.

    RUN pDeleteEstReleases(ipcCompany, ipcEstimateNo, ipdQuantity, giAllFormsIndicator, giAllBlanksIndicator).

END PROCEDURE.

PROCEDURE DeleteEstReleaseByID:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstReleaseID AS INTEGER NO-UNDO.
    
    FIND FIRST estRelease EXCLUSIVE-LOCK 
        WHERE estRelease.estReleaseID EQ ipiEstReleaseID
        NO-ERROR.
    IF AVAILABLE estRelease THEN 
        DELETE estRelease.
        
END PROCEDURE.

PROCEDURE GetStorageAndHandlingForLocation:
    /*------------------------------------------------------------------------------
     Purpose: Given a location and StackHeight, return the Storage and Handling Cost
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiStackHeight AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostStorage AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostHandling AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    ipiStackHeight = MAXIMUM(ipiStackHeight,1).
    FIND FIRST loc NO-LOCK 
        WHERE loc.company EQ ipcCompany
        AND loc.loc EQ ipcLocationID
        NO-ERROR.
    IF AVAILABLE loc THEN 
    DO:
        ASSIGN 
            opdCostStorage  = loc.storageCost[ipiStackHeight]
            opdCostHandling = loc.handlingCost
            opcMessage      = "Storage and Handling Costs returned for Location " + ipcLocationID
            .
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Location not valid: " + ipcLocationID
            .


END PROCEDURE.

PROCEDURE GetShipToCarrierAndZone:
    /*------------------------------------------------------------------------------
     Purpose: Returns the Carrier and Zone, for a given a shipto
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCarrier AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcZone AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipcCompany
        AND shipto.cust-no EQ ipcCustomerID
        AND shipto.ship-id EQ ipcShipToID
        NO-ERROR.
    IF AVAILABLE shipto THEN 
    DO:
        FIND FIRST carrier NO-LOCK 
            WHERE carrier.company EQ shipto.company
            AND carrier.carrier EQ shipto.carrier
            AND carrier.loc EQ ipcLocationID
            NO-ERROR.
        IF AVAILABLE carrier THEN 
        DO:
            FIND FIRST carr-mtx NO-LOCK 
                WHERE carr-mtx.company EQ carrier.company
                AND carr-mtx.loc EQ carrier.loc
                AND carr-mtx.carrier EQ carrier.carrier
                AND carr-mtx.del-zone EQ shipto.dest-code
                NO-ERROR.
            IF AVAILABLE carr-mtx THEN 
                ASSIGN 
                    opcCarrier = carr-mtx.carrier
                    opcZone    = carr-mtx.del-zone
                    opcMessage = "Valid Carrier and Zone found for shipping from " + ipcLocationID + " to " + ipcShipToID
                    .
            ELSE 
                ASSIGN 
                    oplError   = YES
                    opcMessage = "Invalid Zone " + shipto.del-zone + " for Carrier " + carrier.carrier + " for shipping from " + ipcLocationID + " to " + ipcShiptoID
                    .
        END.
        ELSE 
            ASSIGN 
                oplError   = YES
                opcMessage = "Invalid Carrier " + shipto.carrier + " for ship to " + shipto.ship-id + " and location " + ipcLocationID
                .
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Shipto " + ipcShipToID + " for customer " + ipcCustomerID
            . 

END PROCEDURE.

PROCEDURE pCreateEstReleaseBuffer PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-estRelease FOR estRelease.
    
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE company THEN 
    DO:
        ASSIGN 
            ipiFormNo  = MAXIMUM(ipiFormNo, 1)
            ipiBlankNo = MAXIMUM(ipiBlankNo, 1)
            .
        CREATE opbf-estRelease. 
        ASSIGN 
            opbf-estRelease.company          = ipcCompany
            opbf-estRelease.estimateNo       = ipcEstimateNo
            opbf-estRelease.formNo           = ipiFormNo
            opbf-estRelease.blankNo          = ipiBlankNo
            opbf-estRelease.quantity         = ipdQuantity
            opbf-estRelease.palletMultiplier = 1
            opbf-estRelease.monthsAtShipFrom = 0
            opbf-estRelease.estReleaseID     = fGetNextEstReleaseID()
            opbf-estRelease.rec_key          = fGetNextEstReleaseRecKey()
            opiEstReleaseID                  = opbf-estRelease.estReleaseID
            oplError                         = NO 
            opcMessage                       = "estRelease Created with ID: " + STRING(opbf-estRelease.estReleaseID).
    END.
    ELSE 
        ASSIGN 
            oplError   = YES 
            opcMessage = "Estimated Release Cannot be Created.  Invalid company: " + ipcCompany
            .


END PROCEDURE.

PROCEDURE pDeleteEstReleases PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Deletes Est Releases for a given estimate/form/blank scope
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.

    FOR EACH estRelease EXCLUSIVE-LOCK
        WHERE estRelease.company EQ ipcCompany
        AND estRelease.estimateNo EQ ipcEstimateNo
        AND (estRelease.formNo EQ ipiFormNo OR ipiFormNo EQ giAllFormsIndicator)
        AND (estRelease.blankNo EQ ipiBlankNo OR ipiBlankNo EQ giAllBlanksIndicator)
        AND (estRelease.quantity EQ ipdQuantity OR ipdQuantity EQ gdAllQuantitiesIndicator):
        DELETE estRelease.    
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fCalcStorageCostTotal RETURNS DECIMAL PRIVATE
    ( ipdStorageCostPerPalletPerMonth AS DECIMAL,
    ipiPallets AS INTEGER,
    ipdPalletMultiplier AS DECIMAL,
    ipdMonthsAtShipFrom AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Storage Cost Per Pallet Per Months x Pallets x Months At Ship From x Pallet Multiplier
     Notes:
    ------------------------------------------------------------------------------*/	
    
    ipdPalletMultiplier = MAXIMUM(ipdPalletMultiplier, 1).
    RETURN ipdStorageCostPerPalletPerMonth * ipiPallets * ipdPalletMultiplier * ipdMonthsAtShipFrom.
		
END FUNCTION.

FUNCTION fCalcHandlingCostTotal RETURNS DECIMAL PRIVATE
    (ipdHandlingCostPerPallet AS DECIMAL , 
    ipiPallets AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose: Handling Cost Per Pallet x Pallets
     Notes:
    ------------------------------------------------------------------------------*/	

    RETURN ipiPallets * ipdHandlingCostPerPallet.
	
END FUNCTION.

FUNCTION fGetNextEstReleaseRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Gets the next recKey for an estRelease 
     Notes:
    ------------------------------------------------------------------------------*/	
    giTemp = giTemp + 1.
    RETURN STRING(giTemp).
		
END FUNCTION.

FUNCTION fGetNextEstReleaseID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:Gets the next unique estReleaseID for an estRelease 
     Notes:
    ------------------------------------------------------------------------------*/	

    RETURN NEXT-VALUE(estReleaseID).
		
END FUNCTION.


