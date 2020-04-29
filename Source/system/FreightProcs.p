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
DEFINE VARIABLE ghInventoryProcs         AS HANDLE  NO-UNDO.

DEFINE TEMP-TABLE ttBOLLine
    FIELD riBOLLine AS ROWID
    FIELD dMSF AS DECIMAL 
    FIELD dLbs AS DECIMAL 
    FIELD dPallets AS DECIMAL
    .
DEFINE TEMP-TABLE ttFreightClass
    FIELD cFreightClass AS CHARACTER 
    FIELD dMSF AS DECIMAL 
    FIELD dLbs AS DECIMAL 
    FIELD dPallets AS DECIMAL
    . 
{Inventory/ttInventory.i "NEW SHARED"}
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

FUNCTION fGetMSF RETURNS DECIMAL PRIVATE
    (ipdLength AS DECIMAL,
    ipdWidth AS DECIMAL,
    ipcDimUOM AS CHARACTER) FORWARD.

FUNCTION fGetNextEstReleaseID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetTotalMSF RETURNS DECIMAL PRIVATE
    (ipdQuantity AS DECIMAL,
    ipdLength AS DECIMAL,
    ipdWidth AS DECIMAL,
    ipcDimUOM AS CHARACTER) FORWARD.

FUNCTION fUseFreightClassForDestination RETURNS LOGICAL PRIVATE
	(ipcCompany AS CHARACTER) FORWARD.

FUNCTION HasReleases RETURNS LOGICAL 
    (ipriEb AS ROWID) FORWARD.

FUNCTION UseReleasesForFreightAndWarehousing RETURNS LOGICAL 
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
PROCEDURE CalcFreightForEstRelease:
    /*------------------------------------------------------------------------------
     Purpose: Calculates a given estReleaseID's Freight Cost
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estRelease FOR estRelease.
    DEFINE BUFFER bf-eb         FOR eb.
    
    DEFINE VARIABLE dSubUnits    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartial     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dFreightMin  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMSF         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalWeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lValidEstRel AS LOGICAL NO-UNDO.
    
    RUN inventory\InventoryProcs.p PERSISTENT SET ghInventoryProcs.
    
    lValidEstRel = NO.
    FOR FIRST bf-estRelease EXCLUSIVE-LOCK   
        WHERE bf-estRelease.estReleaseID EQ ipiEstReleaseID
        ,
        FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company EQ bf-estRelease.company
        AND bf-eb.est-no EQ bf-estRelease.estimateNo
        AND bf-eb.form-no EQ bf-estRelease.formNo
        AND bf-eb.blank-no EQ bf-estRelease.blankNo:
        
        lValidEstRel = YES.
        RUN RecalcQuantityUnits IN ghInventoryProcs (bf-estRelease.quantityRelease, INPUT-OUTPUT bf-estRelease.quantityPerSubUnit, INPUT-OUTPUT bf-estRelease.quantitySubUnitsPerUnit, 
            OUTPUT dSubUnits, OUTPUT bf-estRelease.quantityOfUnits, OUTPUT dPartial).
        dMSF = fGetMSF(bf-estRelease.dimEachLen, bf-estRelease.dimEachWid, bf-estRelease.dimEachUOM).
        RUN pCalculateFreight(bf-estRelease.company, bf-estRelease.shipFromLocationID, bf-estRelease.carrierID, bf-estRelease.carrierZone,"", 
            bf-estRelease.quantityRelease, bf-estRelease.weightTotalPerEach, dMSF, bf-estRelease.quantityOfUnits, bf-eb.fr-out-c, bf-eb.fr-out-m,
            OUTPUT bf-estRelease.freightCost, OUTPUT dFreightMin, OUTPUT oplError, OUTPUT opcMessage).  
    END.
    IF NOT lValidEstRel THEN 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid estReleaseID " + STRING(ipiEstReleaseID)
            .
    DELETE OBJECT ghInventoryProcs.
END PROCEDURE.

PROCEDURE CalcStorageAndHandlingForEstRelease:
    /*------------------------------------------------------------------------------
     Purpose: Calculates a given estReleaseID's Storage and Handling
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstReleaseID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estRelease FOR estRelease.

    DEFINE VARIABLE dSubUnits AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartial  AS DECIMAL NO-UNDO.
    
    RUN inventory\InventoryProcs.p PERSISTENT SET ghInventoryProcs.

    FIND FIRST bf-estRelease EXCLUSIVE-LOCK   
        WHERE bf-estRelease.estReleaseID EQ ipiEstReleaseID
        NO-ERROR.
    IF AVAILABLE bf-estRelease THEN 
    DO:
        RUN RecalcQuantityUnits IN ghInventoryProcs (bf-estRelease.quantityRelease, INPUT-OUTPUT bf-estRelease.quantityPerSubUnit, INPUT-OUTPUT bf-estRelease.quantitySubUnitsPerUnit, 
            OUTPUT dSubUnits, OUTPUT bf-estRelease.quantityOfUnits, OUTPUT dPartial). 
        bf-estRelease.storageCostTotal = fCalcStorageCostTotal(bf-estRelease.storageCost,bf-estRelease.quantityOfUnits,bf-estRelease.palletMultiplier, bf-estRelease.monthsAtShipFrom).
        bf-estRelease.handlingCostTotal = fCalcHandlingCostTotal(bf-estRelease.handlingCost, bf-estRelease.quantityOfUnits).
    
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid estReleaseID " + STRING(ipiEstReleaseID)
            .
    DELETE OBJECT ghInventoryProcs.
END PROCEDURE.

PROCEDURE GetFreightForBOL:
    /*------------------------------------------------------------------------------
     Purpose:  Given a BOL ROWID, calculate the total freight.  Optionally, update the 
     freight field on the BOL header and pro-rated freight values on each bill of lading line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriBOL AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplUpdate AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreight AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-bolh  FOR oe-bolh.
    DEFINE BUFFER bf-oe-boll  FOR oe-boll.
    DEFINE BUFFER bf-carrier  FOR carrier.
    DEFINE BUFFER bf-carr-mtx FOR carr-mtx.
    DEFINE BUFFER bf-shipto   FOR shipto.
    
    DEFINE VARIABLE dTotPallets AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotMSF     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTotLbs     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMSF        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cShipFrom   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dFreight    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFreightMin AS DECIMAL   NO-UNDO.
    
    EMPTY TEMP-TABLE ttBOLLine.
    EMPTY TEMP-TABLE ttFreightClass.
    
    FIND FIRST oe-bolh NO-LOCK
        WHERE ROWID(oe-bolh) EQ ipriBOL
        NO-ERROR.
    IF NOT AVAILABLE oe-bolh THEN 
    DO: 
        ASSIGN
            oplError   = YES
            opcMessage = "Invalid ROWID for BOL"
            .
        RETURN.  
    END.
    /*Get shipto either for the BOL customer or customer X*/
    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER bf-shipto).
                        
    IF NOT AVAILABLE bf-shipto THEN 
    DO: 
        ASSIGN
            oplError   = YES
            opcMessage = "Invalid Ship To (" + oe-bolh.ship-id + ") for BOL"
            .
        RETURN.  
    END.
    cShipFrom = bf-shipto.loc.
    /*Process Each Line - Calculate Totals*/
    FOR EACH oe-boll NO-LOCK 
        WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no EQ oe-bolh.b-no,
        FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no EQ oe-boll.i-no
        BREAK BY itemfg.i-no
        BY oe-boll.line:
        
        IF FIRST(oe-boll.line) THEN 
            cShipFrom = oe-boll.loc.
        IF FIRST-OF(itemfg.i-no) THEN
            RUN fg/GetFGArea.p (ROWID(itemfg),"MSF", OUTPUT dMSF).    
        
        FIND FIRST ttFreightClass
            WHERE ttFreightClass.cFreightClass EQ itemfg.frt-class
            NO-ERROR.
        IF NOT AVAILABLE ttFreightClass THEN DO: 
            CREATE ttFreightClass.
            ASSIGN 
                ttFreightClass.cFreightClass = itemfg.frt-class
                .
        END.
        CREATE ttBOLLine.
        ASSIGN 
            ttBOLLine.riBOLLine   = ROWID(oe-boll)
            ttBOLLine.dLbs = oe-boll.weight
            ttBOLLine.dPallets = oe-boll.tot-pallets
            ttBOLLine.dMSF = dMSF * oe-boll.qty
            dTotPallets = dTotPallets + ttBOLLine.dPallets
            dTotMSF     = dTotMSF + ttBOLLine.dMSF
            dTotLbs     = dTotLbs + ttBOLLine.dLbs
            ttFreightClass.dLbs = ttFreightClass.dLbs + ttBOLLine.dLbs
            ttFreightClass.dMSF = ttFreightClass.dMSF + ttBOLLine.dMSF
            ttFreightClass.dPallets = ttFreightClass.dPallets + ttBOLLine.dPallets
            .
    END.  /*each oe-boll*/
    
    IF fUseFreightClassForDestination(oe-bolh.company) THEN DO:
        FOR EACH ttFreightClass:
            RUN GetFreightForCarrierZone (oe-bolh.company, cShipFrom, oe-bolh.carrier, ttFreightClass.cFreightClass, bf-shipto.ship-zip,
               ttFreightClass.dPallets, ttFreightClass.dLbs, ttFreightClass.dMSF, 
                OUTPUT dFreight, OUTPUT dFreightMin,
                OUTPUT oplError, OUTPUT opcMessage).
            IF oplError THEN 
                RUN GetFreightForCarrierZone (oe-bolh.company, cShipFrom, oe-bolh.carrier, bf-shipto.dest-code, bf-shipto.ship-zip,
                    ttFreightClass.dPallets, ttFreightClass.dLbs, ttFreightClass.dMSF,
                    OUTPUT dFreight, OUTPUT dFreightMin,
                    OUTPUT oplError, OUTPUT opcMessage).
            opdFreight = opdFreight + dFreight.
        END.
    END. 
    ELSE 
        RUN GetFreightForCarrierZone (oe-bolh.company, cShipFrom, oe-bolh.carrier, bf-shipto.dest-code, bf-shipto.ship-zip,
            dTotPallets, dTotLbs, dTotMSF, 
            OUTPUT opdFreight, OUTPUT dFreightMin,
            OUTPUT oplError, OUTPUT opcMessage).  
    
    IF iplUpdate THEN /*assign freight and prorated freight per line*/
    DO:
        RUN pGetCarrierBuffers(oe-bolh.company, cShipFrom, oe-bolh.carrier, bf-shipto.dest-code, bf-shipto.ship-zip, 
            BUFFER bf-carrier, BUFFER bf-carr-mtx, 
            OUTPUT oplError, OUTPUT opcMessage).
        IF AVAILABLE bf-carrier THEN DO:
            FOR EACH bf-oe-boll EXCLUSIVE-LOCK 
                WHERE bf-oe-boll.company EQ oe-bolh.company
                AND bf-oe-boll.b-no EQ oe-bolh.b-no, 
                FIRST ttBOLLine
                WHERE ttBOLLine.riBOLLine EQ ROWID(bf-oe-boll): 
                    CASE bf-carrier.chg-method:
                        WHEN "P" THEN 
                            bf-oe-boll.freight = ttBOLLine.dPallets / dTotPallets * opdFreight.
                        WHEN "W" THEN 
                            bf-oe-boll.freight = ttBOLLine.dLbs / dTotLbs * opdFreight.
                        OTHERWISE 
                            bf-oe-boll.freight = ttBOLLine.dMSF / dTotMSF * opdFreight.
                    END CASE.
            END. /*Each bf-oe-boll - exclusive*/
            
            /*update bol-header*/
            FIND FIRST bf-oe-bolh EXCLUSIVE-LOCK 
                WHERE ROWID(bf-oe-bolh) EQ ROWID(oe-bolh)
                NO-ERROR.
            IF AVAILABLE bf-oe-bolh THEN 
                bf-oe-bolh.freight = opdFreight.
            
            RELEASE bf-oe-bolh.
            RELEASE bf-oe-boll.
        END. /*available bf-carrier*/
    END. /*iplUpdate = YES*/
    
END PROCEDURE.

PROCEDURE GetFreightForCarrierZone:
    /*------------------------------------------------------------------------------
     Purpose: Given a Company, Loc, Carrier, Zone, Zip (optional), and lookup quantities
     return the total calculated freight and freight min (to apply min x releases if necessary)
     Notes: 
     Syntax: RUN GetFreightForCarrierZone IN hFreightProcs (eb.company, eb.loc, eb.carrier, eb.dest-code, eb.ship-zip,
                dPalletCount, dTotalWeight, dTotalMSF, 
                OUTPUT dFreightTotal, OUTPUT dFreightMin,
                OUTPUT lError, OUTPUT cMessage).  
         
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrier AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZone AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalPallets AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalMSF AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightMin AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-carrier  FOR carrier.
    DEFINE BUFFER bf-carr-mtx FOR carr-mtx.

    RUN pGetCarrierBuffers(ipcCompany, ipcLocationID, ipcCarrier, ipcZone, ipcZip, 
        BUFFER bf-carrier, BUFFER bf-carr-mtx, 
        OUTPUT oplError, OUTPUT opcMessage).
    IF AVAILABLE bf-carrier AND AVAILABLE bf-carr-mtx THEN 
    DO:
        RUN pGetFreightForCarrierBuffers(BUFFER bf-carrier, BUFFER bf-carr-mtx, 
            ipdTotalPallets, ipdTotalWeight, ipdTotalMSF, 
            OUTPUT opdFreightTotal, OUTPUT opdFreightMin, OUTPUT oplError, OUTPUT opcMessage).
        
    END.

END PROCEDURE.

PROCEDURE CreateEstRelease:
    /*------------------------------------------------------------------------------
     Purpose: Creates an EstRelease Record based on required inputs
     Notes:
     Sample Syntax: RUN CreateEstRelease IN hFreightProcs (gcCompany, gcEstimate2, 1, 1, gdQty1,
        OUTPUT iEstReleaseID, 
        OUTPUT lError, OUTPUT cMessage ).
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

PROCEDURE CreateEstReleaseForEstBlank:
    /*------------------------------------------------------------------------------
     Purpose:  Creates EstRelease based on Rowid of eb (EstBlank)
     Notes:
     Sample Syntax: RUN CreateEstReleaseForEstBlank IN hFreightProcs (ROWID(eb), 
        OUTPUT iEstReleaseID, OUTPUT lError, OUTPUT cMessage).
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
        RUN pUpdateEstReleaseFromEstBlank(BUFFER bf-estRelease, BUFFER eb, OUTPUT oplError, OUTPUT opcMessage).
    END. 
    
END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
     Sample Syntax: RUN DeleteAllEstReleasesForEstimate IN hFreightProcs (gcCompany, gcEstimate2).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.

    RUN pDeleteEstReleases(ipcCompany, ipcEstimateNo, gdAllQuantitiesIndicator, giAllFormsIndicator, giAllBlanksIndicator).

END PROCEDURE.

PROCEDURE DeleteAllEstReleasesForEstimateBlank:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all estRelease records for a given estimate
     Notes:
     Sample Syntax: RUN DeleteAllEstReleasesForEstimateBlank IN hFreightProcs (gcCompany, gcEstimate1, giDeleteBlankForm, giDeleteBlank).
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
     Sample Syntax: RUN DeleteAllEstReleasesForEstimateForm IN hFreightProcs (gcCompany, gcEstimate1, giDeleteForm).
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
     Sample Syntax: RUN DeleteAllEstReleasesForEstimateQuantity IN hFreightProcs (gcCompany, gcEstimate1, gdQty1).
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

    DEFINE BUFFER bf-estRelease FOR estRelease.
        
    FIND FIRST bf-estRelease EXCLUSIVE-LOCK 
        WHERE bf-estRelease.estReleaseID EQ ipiEstReleaseID
        NO-ERROR.
    IF AVAILABLE bf-estRelease THEN 
        DELETE bf-estRelease.
        
END PROCEDURE.

PROCEDURE GetFreightForEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Given company estimate and calculation quantity, return the total freight cost
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightTotal AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pGetFreightForEstReleases(ipcCompany, ipcEstimateNo, ipdQuantity, giAllFormsIndicator, giAllBlanksIndicator, 
        OUTPUT opdFreightTotal,
        OUTPUT lError, OUTPUT cMessage ).

END PROCEDURE.

PROCEDURE GetFreightForEstimateBlank:
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid for estBlank (eb) quantity return the freight cost
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightTotal AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    
    RUN pGetFreightForEstReleases(ipcCompany, ipcEstimateNo, ipdQuantity, ipiFormNo, ipiBlankNo, 
        OUTPUT opdFreightTotal,
        OUTPUT lError, OUTPUT cMessage ).

END PROCEDURE.

PROCEDURE GetFreight:
    /*------------------------------------------------------------------------------
     Purpose: Given all required inputs, return the total freight 
     Notes:
     Syntax:
           RUN GetFreight(ipcCompany, ipcShipFromLocationID, ipcCarrierID, ipcCarrierZone, ipcZip, 
                ipdQuantityInEA, ipdLBsofEA, ipdMSFofEA, ipdTotalUnits, ipdFreightPer100LbsOverride, ipdFreightPerMOverride,
                OUTPUT opdFreightCost, OUTPUT oplError, OUTPUT opcMessage).
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipFromLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrierID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrierZone AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityInEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLBsOfEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdMSFOfEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalUnits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdFreightPer100LbsOverride AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdFreightPerMOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightMin AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN pCalculateFreight(ipcCompany, ipcShipFromLocationID, ipcCarrierID, ipcCarrierZone, ipcZip, 
        ipdQuantityInEA, ipdLBsofEA, ipdMSFofEA, ipdTotalUnits, ipdFreightPer100LbsOverride, ipdFreightPerMOverride,
        OUTPUT opdFreightCost, OUTPUT opdFreightMin, OUTPUT oplError, OUTPUT opcMessage).

END PROCEDURE.

PROCEDURE GetStorageAndHandlingForEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate/quantity return the total Handling and Storage costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdStorageCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdHandlingCostTotal AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pGetStorageAndHandling(ipcCompany, ipcEstimateNo, ipdQuantity, giAllFormsIndicator, giAllBlanksIndicator, 
        OUTPUT opdStorageCostTotal, OUTPUT opdHandlingCostTotal,
        OUTPUT lError, OUTPUT cMessage ).
        
END PROCEDURE.

PROCEDURE GetStorageAndHandlingForEstimateBlank:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate/quantity and form-blank return the total Handling and Storage costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdStorageCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdHandlingCostTotal AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN pGetStorageAndHandling(ipcCompany, ipcEstimateNo, ipdQuantity, ipiFormNo, ipiBlankNo, 
        OUTPUT opdStorageCostTotal, OUTPUT opdHandlingCostTotal,
        OUTPUT lError, OUTPUT cMessage ).
        
END PROCEDURE.

PROCEDURE GetStorageAndHandlingForLocation:
    /*------------------------------------------------------------------------------
     Purpose: Given a location and StackHeight, return the Storage and Handling Cost
     Notes:
     Sample Syntax: RUN GetStorageAndHandlingForLocation IN hFreightProcs (estRelease.company, estRelease.shipFromLocationID, estRelease.stackHeight,
                OUTPUT estRelease.storageCost, OUTPUT estRelease.handlingCost, 
                OUTPUT lError, OUTPUT cMessage). 
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
    
    DEFINE BUFFER bf-carrier  FOR carrier.
    DEFINE BUFFER bf-carr-mtx FOR carr-mtx.
    
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipcCompany
        AND shipto.cust-no EQ ipcCustomerID
        AND shipto.ship-id EQ ipcShipToID
        NO-ERROR.
    IF AVAILABLE shipto THEN 
    DO:
        RUN pGetCarrierBuffers(shipto.company, ipcLocationID, shipto.carrier,shipto.dest-code, shipto.ship-zip,
            BUFFER bf-carrier, BUFFER bf-carr-mtx,
            OUTPUT oplError, OUTPUT opcMessage).
        IF AVAILABLE bf-carr-mtx THEN 
            ASSIGN 
                opcCarrier = bf-carr-mtx.carrier
                opcZone    = bf-carr-mtx.del-zone
                .
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Shipto " + ipcShipToID + " for customer " + ipcCustomerID
            . 

END PROCEDURE.

PROCEDURE pCalculateFreight PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculates the freight
     Notes:
     Syntax:
         RUN pCalculateFreight(ipcCompany, ipcShipFromLocationID, ipcCarrierID, ipcCarrierZone, ipcZip, 
            ipdQuantityInEA, ipdLBsofEA, ipdMSFofEA, ipdTotalUnits, ipdFreightPer100LbsOverride, ipdFreightPerMOverride,
            OUTPUT opdFreightCost, OUTPUT oplError, OUTPUT opcMessage).
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcShipFromLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrierID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrierZone AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityInEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLBsOfEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdMSFOfEA AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalUnits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdFreightPer100LbsOverride AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdFreightPerMOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightCostMin AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dTotalWeight AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalMSF    AS DECIMAL NO-UNDO.

    ASSIGN 
        dTotalMSF    = ipdQuantityInEA * ipdMSFOfEA
        dTotalWeight = ipdQuantityInEA * ipdLBsOfEA
        .
    IF ipdFreightPer100LbsOverride NE 0 THEN 
        ASSIGN 
            opdFreightCost = ipdFreightPer100LbsOverride * dTotalWeight / 100
            opcMessage     = "Freight Calculated from Override of cost per 100 Lbs"
            .    
    ELSE IF ipdFreightPerMOverride NE 0 THEN
            ASSIGN 
                opdFreightCost = ipdFreightPerMOverride * ipdQuantityInEA / 1000
                opcMessage     = "Freight Calculated from Override of cost per 1000"
                .
        ELSE 
            RUN GetFreightForCarrierZone (ipcCompany, ipcShipFromLocationID, ipcCarrierID, ipcCarrierZone, ipcZip,
                ipdTotalUnits, dTotalWeight, dTotalMSF, 
                OUTPUT opdFreightCost, OUTPUT opdFreightCostMin,
                OUTPUT oplError, OUTPUT opcMessage).  

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
            opbf-estRelease.stackHeight      = 1
            opbf-estRelease.palletMultiplier = 1
            opbf-estRelease.monthsAtShipFrom = 0
            opbf-estRelease.estReleaseID     = fGetNextEstReleaseID()
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

    DEFINE BUFFER bf-estRelease FOR estRelease.
    
    FOR EACH bf-estRelease EXCLUSIVE-LOCK
        WHERE bf-estRelease.company EQ ipcCompany
        AND bf-estRelease.estimateNo EQ ipcEstimateNo
        AND (bf-estRelease.formNo EQ ipiFormNo OR ipiFormNo EQ giAllFormsIndicator)
        AND (bf-estRelease.blankNo EQ ipiBlankNo OR ipiBlankNo EQ giAllBlanksIndicator)
        AND (bf-estRelease.quantity EQ ipdQuantity OR ipdQuantity EQ gdAllQuantitiesIndicator):
        DELETE bf-estRelease.    
    END.

END PROCEDURE.

PROCEDURE pGetCarrierBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company, location, carrier, zone and zip (optional), 
     return a carrier and carr-mtx buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCarrier AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZone AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcZip AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-carrier  FOR carrier.
    DEFINE PARAMETER BUFFER opbf-carr-mtx FOR carr-mtx.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    FIND FIRST opbf-carrier NO-LOCK 
        WHERE opbf-carrier.company EQ ipcCompany
        AND opbf-carrier.carrier EQ ipcCarrier
        AND opbf-carrier.loc EQ ipcLocationID
        NO-ERROR.
    IF AVAILABLE opbf-carrier THEN 
    DO:
        FIND FIRST opbf-carr-mtx NO-LOCK 
            WHERE opbf-carr-mtx.company EQ opbf-carrier.company
            AND opbf-carr-mtx.loc EQ opbf-carrier.loc
            AND opbf-carr-mtx.carrier EQ opbf-carrier.carrier
            AND opbf-carr-mtx.del-zone EQ ipcZone
            AND opbf-carr-mtx.del-zip EQ ipcZip
            NO-ERROR.
        IF NOT AVAILABLE opbf-carr-mtx THEN  
            FIND FIRST opbf-carr-mtx NO-LOCK 
                WHERE opbf-carr-mtx.company EQ opbf-carrier.company
                AND opbf-carr-mtx.loc EQ opbf-carrier.loc
                AND opbf-carr-mtx.carrier EQ opbf-carrier.carrier
                AND opbf-carr-mtx.del-zone EQ ipcZone
                AND opbf-carr-mtx.del-zip EQ ""
                NO-ERROR.
        IF NOT AVAILABLE opbf-carr-mtx THEN  
            FIND FIRST opbf-carr-mtx NO-LOCK 
                WHERE opbf-carr-mtx.company EQ opbf-carrier.company
                AND opbf-carr-mtx.loc EQ opbf-carrier.loc
                AND opbf-carr-mtx.carrier EQ opbf-carrier.carrier
                AND opbf-carr-mtx.del-zone EQ ipcZone
                NO-ERROR.
        IF AVAILABLE opbf-carr-mtx THEN 
            ASSIGN 
                oplError   = NO
                opcMessage = "Carrier and matrix found for carrier " + ipcCarrier + " for location " + ipcLocationID
                .
        ELSE 
            ASSIGN
                oplError   = YES 
                opcMessage = "Invalid Zone " + ipcZone + " for Carrier " + ipcCarrier + " for shipping from " + ipcLocationID
                .
                        
    END.
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Carrier " + ipcCarrier + " for location " + ipcLocationID
            . 

END PROCEDURE.

PROCEDURE pGetEffectiveQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate and quantity, search the estRelease table for the
     effective quantity to establish the pro rata costs for freight and handling
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityTarget AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityEffective AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-estRelease FOR estRelease.

    FOR EACH bf-estRelease NO-LOCK 
        WHERE bf-estRelease.company EQ ipcCompany
        AND bf-estRelease.formNo EQ ipiFormNo
        AND bf-estRelease.blankNo EQ ipiBlankNo
        BREAK BY bf-estRelease.quantity:
        
        IF FIRST-OF(bf-estRelease.quantity) THEN 
        DO:
            IF FIRST(bf-estRelease.quantity) OR bf-estRelease.quantity LE ipdQuantityTarget THEN 
                opdQuantityEffective = bf-estRelease.quantity.
            IF bf-estRelease.quantity GE ipdQuantityTarget THEN LEAVE.
        END.
    END.

END PROCEDURE.

PROCEDURE pGetFreightForEstReleases PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an estimate, qty, form, and blank, return the total freight cost
    costs
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estRelease FOR estRelease.
        
    DEFINE VARIABLE dQuantityEffective       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dFreightEach             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityReleaseProRated AS DECIMAL NO-UNDO.
    
    RUN pGetEffectiveQuantity (ipcCompany, ipcEstimateNo, ipiFormNo, ipiBlankNo, ipdQuantity, OUTPUT dQuantityEffective).
    FOR EACH bf-estRelease NO-LOCK
        WHERE bf-estRelease.company EQ ipcCompany
        AND bf-estRelease.estimateNo EQ ipcEstimateNo
        AND bf-estRelease.formNo EQ ipiFormNo
        AND bf-estRelease.blankNo EQ ipiBlankNo
        AND bf-estRelease.quantity EQ dQuantityEffective:

        RUN CalcFreightForEstRelease(bf-estRelease.estReleaseID, OUTPUT oplError, OUTPUT opcMessage).
            
        IF NOT oplError THEN 
        DO:
            IF ipdQuantity EQ dQuantityEffective THEN 
                ASSIGN 
                    opdFreightCostTotal = opdFreightCostTotal + bf-estRelease.freightCost
                    .
            ELSE /*Prorate the Freight based on the relative release to master quantity*/
                ASSIGN 
                    dFreightEach             = bf-estRelease.freightCost / bf-estRelease.quantityRelease
                    dQuantityReleaseProRated = ipdQuantity * bf-estRelease.quantityRelease / bf-estRelease.quantity
                    opdFreightCostTotal      = opdFreightCostTotal + dFreightEach * dQuantityReleaseProRated
                    . 
        END. /*No error*/
    END. /*Each estRelease for blank*/
    
END PROCEDURE.

PROCEDURE pGetFreightForCarrierBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-carrier  FOR carrier.
    DEFINE PARAMETER BUFFER ipbf-carr-mtx FOR carr-mtx.
    DEFINE INPUT PARAMETER ipdTotalPallets AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalWeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdTotalMSF AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdFreightMin AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iLevel          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCostMultiplier AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyToLookup    AS DECIMAL NO-UNDO.
    
    CASE ipbf-carrier.chg-method:
        WHEN "W" THEN
            ASSIGN 
                dCostMultiplier = ipdTotalWeight / 100
                dQtyToLookup    = ipdTotalWeight
                opcMessage      = "Freight Calculated from carrier charge based on total weight of " + STRING(ipdTotalWeight,"->>,>>>,>>9.99")
                .
                
        WHEN "P" THEN 
            ASSIGN 
                dCostMultiplier = ipdTotalPallets
                dQtyToLookup    = ipdTotalPallets
                opcMessage      = "Freight Calculated from carrier charge based on total pallets of " + STRING(ipdTotalPallets,"->>,>>>,>>9")
                .
        OTHERWISE /*MSF*/
        ASSIGN 
            dCostMultiplier = ipdTotalMSF
            dQtyToLookup    = ipdTotalMSF
            opcMessage      = "Freight Calculated from carrier charge based on total MSF of " + STRING(ipdTotalMSF,"->>,>>>,>>9.99")
            .
    END CASE. 
    IF dCostMultiplier EQ ? THEN dCostMultiplier = 0.
    DO iLevel = 1 TO 10:
        IF ipbf-carr-mtx.weight[iLevel] GE dQtyToLookup THEN LEAVE.
    END.
    IF iLevel LE 10 THEN 
        ASSIGN 
            opdFreightMin   = ipbf-carr-mtx.min-rate
            opdFreightTotal = ipbf-carr-mtx.rate[iLevel] * dCostMultiplier
            .
    IF opdFreightTotal LT opdFreightMin THEN 
        ASSIGN 
            opdFreightTotal = opdFreightMin
            opcMessage      = opcMessage + " - minimum not exceeded".

END PROCEDURE.

PROCEDURE pGetStorageAndHandling PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an estimate, qty, form, and blank, return the total storage and handling
    costs
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdStorageCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdHandlingCostTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estRelease FOR estRelease.
    DEFINE VARIABLE dQuantityEffective       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dStorageEach             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dHandlingEach            AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityReleaseProRated AS DECIMAL NO-UNDO.
    
    RUN pGetEffectiveQuantity (ipcCompany, ipcEstimateNo, ipiFormNo, ipiBlankNo, ipdQuantity, OUTPUT dQuantityEffective).
    IF dQuantityEffective EQ 0 THEN 
        dQuantityEffective = ipdQuantity.
    FOR EACH bf-estRelease NO-LOCK
        WHERE bf-estRelease.company EQ ipcCompany
        AND bf-estRelease.estimateNo EQ ipcEstimateNo
        AND bf-estRelease.formNo EQ ipiFormNo
        AND bf-estRelease.blankNo EQ ipiBlankNo
        AND bf-estRelease.quantity EQ dQuantityEffective:
            
        RUN CalcStorageAndHandlingForEstRelease(bf-estRelease.estReleaseID, OUTPUT oplError, OUTPUT opcMessage).
        
        IF NOT oplError THEN 
        DO:
            IF ipdQuantity EQ dQuantityEffective THEN 
                ASSIGN 
                    opdStorageCostTotal  = opdStorageCostTotal + bf-estRelease.storageCostTotal
                    opdHandlingCostTotal = opdHandlingCostTotal + bf-estRelease.handlingCostTotal
                    .
            ELSE /*Prorate the Costs based on the relative release to master quantity*/
                ASSIGN 
                    dQuantityReleaseProRated = ipdQuantity * bf-estRelease.quantityRelease / bf-estRelease.quantity
                    dStorageEach             = bf-estRelease.storageCostTotal / bf-estRelease.quantityRelease
                    dHandlingEach            = bf-estRelease.handlingCostTotal / bf-estRelease.quantityRelease
                    opdStorageCostTotal      = opdStorageCostTotal + dStorageEach * dQuantityReleaseProRated
                    opdHandlingCostTotal     = opdHandlingCostTotal + dHandlingEach * dQuantityReleaseProRated
                    . 
        END.
    END.
    
END PROCEDURE.

PROCEDURE pUpdateEstReleaseFromEstBlank PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given 2 buffers, update the estRelease from the eb and recalculate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estRelease FOR estRelease.
    DEFINE PARAMETER BUFFER ipbf-eb         FOR eb.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    

    DEFINE BUFFER bf-ef      FOR ef.
    DEFINE BUFFER bf-item    FOR ITEM.
    DEFINE BUFFER bf-ce-ctrl FOR ce-ctrl.
    DEFINE VARIABLE cCarrierDefault AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dTotalWeight    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBasisWeight    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnitWeight  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dUnitWeight     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dTareWeight     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMSF            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dSubUnits       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPartial        AS DECIMAL   NO-UNDO.
    
    RUN inventory\InventoryProcs.p PERSISTENT SET ghInventoryProcs.
    
    ASSIGN 
        ipbf-estRelease.customerID              = ipbf-eb.cust-no
        ipbf-estRelease.shipToID                = ipbf-eb.ship-id
        ipbf-estRelease.carrierID               = ipbf-eb.carrier
        ipbf-estRelease.shipFromLocationID      = ipbf-eb.loc    
        ipbf-estRelease.quantityPerSubUnit      = ipbf-eb.cas-cnt
        ipbf-estRelease.quantitySubUnitsPerUnit = ipbf-eb.cas-pal
        ipbf-estRelease.stackHeight             = MAXIMUM(ipbf-eb.stackHeight,1)
        ipbf-estRelease.quantityRelease         = ipbf-estRelease.quantity 
        ipbf-estRelease.dimEachLen              = ipbf-eb.t-len
        ipbf-estRelease.dimEachWid              = ipbf-eb.t-wid
        ipbf-estRelease.dimEachDep              = ipbf-eb.t-dep
        ipbf-estRelease.dimEachUOM              = "IN"
        .

    RUN GetStorageAndHandlingForLocation(ipbf-eb.company, ipbf-eb.loc, ipbf-eb.stackHeight, 
        OUTPUT ipbf-estRelease.storageCost, OUTPUT ipbf-estRelease.handlingCost,
        OUTPUT oplError, OUTPUT opcMessage).
    RUN GetShipToCarrierAndZone(ipbf-eb.company, ipbf-eb.loc, ipbf-eb.cust-no, ipbf-eb.ship-id, 
        OUTPUT cCarrierDefault, OUTPUT ipbf-estRelease.carrierZone, 
        OUTPUT oplError, OUTPUT opcMessage).
    RUN RecalcQuantityUnits IN ghInventoryProcs (ipbf-estRelease.quantityRelease, INPUT-OUTPUT ipbf-estRelease.quantityPerSubUnit, 
        INPUT-OUTPUT ipbf-estRelease.quantitySubUnitsPerUnit, 
        OUTPUT dSubUnits, OUTPUT ipbf-estRelease.quantityOfUnits, OUTPUT dPartial).
        
    IF ipbf-eb.weight-m NE 0 THEN 
        ipbf-estRelease.weightTotalPerEach = ipbf-eb.weight-m / 1000.
    ELSE 
    DO:
        FIND FIRST bf-ef NO-LOCK 
            WHERE bf-ef.company EQ ipbf-eb.company
            AND bf-ef.est-no EQ ipbf-eb.est-no
            AND bf-ef.form-no EQ ipbf-eb.form-no 
            NO-ERROR.
        FIND FIRST bf-ce-ctrl NO-LOCK 
            WHERE bf-ce-ctrl.company EQ ipbf-eb.company
            NO-ERROR.
        IF AVAILABLE bf-ef THEN 
        DO: 
            /*Calculate Weight of Board to get Net Weight of Each*/
            ASSIGN 
                dMSF         = (ipbf-eb.t-sqin - ipbf-eb.t-win) / 144000
                dBasisWeight = bf-ef.weight
                .
            IF bf-ef.medium NE "" THEN 
            DO:
                FIND FIRST bf-item NO-LOCK 
                    WHERE bf-item.company EQ bf-ef.company
                    AND bf-item.i-no EQ bf-ef.medium
                    NO-ERROR.
                IF AVAILABLE bf-item THEN 
                    dBasisWeight = dBasisWeight + bf-item.basis-w * (1 - (bf-item.shrink / 100)).
            END.
            IF bf-ef.flute NE "" THEN                                                     
            DO:
                RELEASE bf-item.
                FIND FIRST bf-item NO-LOCK 
                    WHERE bf-item.company EQ bf-ef.company
                    AND bf-item.i-no EQ bf-ef.flute
                    NO-ERROR.
                IF AVAILABLE bf-item THEN 
                    dBasisWeight = dBasisWeight + bf-item.basis-w.
            END.
            /*Calculate Net Weight Per Each*/
            ipbf-estRelease.weightNetPerEach = dBasisWeight * dMSF.
            
        END.
        /*Calculate Tare Weight Per Each*/ 
        IF ipbf-eb.cas-no NE "" THEN   /*Cases/Sub-units*/
            FIND FIRST bf-item NO-LOCK 
                WHERE bf-item.company EQ ipbf-eb.company
                AND bf-item.i-no EQ ipbf-eb.cas-no
                NO-ERROR.
        IF AVAILABLE bf-item THEN 
            dSubUnitWeight = bf-item.basis-w.
        ELSE 
            dSubUnitWeight = IF AVAILABLE bf-ce-ctrl THEN bf-ce-ctrl.def-cas-w ELSE 0.
        dTareWeight = dTareWeight + dSubUnits * dSubUnitWeight.
            
        IF ipbf-eb.tr-no NE "" THEN
            FIND FIRST bf-item NO-LOCK 
                WHERE bf-item.company EQ ipbf-eb.company
                AND bf-item.i-no EQ ipbf-eb.tr-no
                NO-ERROR. 
        IF AVAILABLE bf-item THEN 
            dUnitWeight = bf-item.basis-w.
        ELSE 
            dUnitWeight = IF AVAILABLE bf-ce-ctrl THEN bf-ce-ctrl.def-pal-w ELSE 0.
        dTareWeight = dTareWeight + ipbf-estRelease.quantityOfUnits * dUnitWeight. 
            
        IF ipbf-estRelease.quantityRelease GT 0 THEN 
            ipbf-estRelease.weightTarePerEach = dTareWeight / ipbf-estRelease.quantityRelease.
        
        ipbf-estRelease.weightTotalPerEach = ipbf-estRelease.weightNetPerEach + ipbf-estRelease.weightTarePerEach.
    END.    
    DELETE OBJECT ghInventoryProcs.
        
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

FUNCTION fGetMSF RETURNS DECIMAL PRIVATE
    ( ipdLength AS DECIMAL, ipdWidth AS DECIMAL, ipcDimUOM AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Given LxW dimensions return the MSF for Ea
     Notes:
    ------------------------------------------------------------------------------*/	
    IF ipcDimUOM EQ "IN" OR ipcDimUOM EQ "" THEN 
        RETURN (ipdLength * ipdWidth / 144000).    
    ELSE 
        RETURN 1.

		
END FUNCTION.

FUNCTION fGetNextEstReleaseID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:Gets the next unique estReleaseID for an estRelease 
     Notes:
    ------------------------------------------------------------------------------*/	

    RETURN NEXT-VALUE(estReleaseID).
		
END FUNCTION.

FUNCTION fGetTotalMSF RETURNS DECIMAL PRIVATE
    (ipdQuantity AS DECIMAL, ipdLength AS DECIMAL, ipdWidth AS DECIMAL, ipcDimUOM AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: given a quantity and LxW dimensions, return the total MSF calculation
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN ipdQuantity * fGetMSF(ipdLength, ipdWidth, ipcDimUOM).    
		
END FUNCTION.

FUNCTION fUseFreightClassForDestination RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns NK1 setting indicating that Freight Class should be
     used for destination code instead of shipto dest code
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    RUN sys\ref\nk1look.p (ipcCompany,'BOLFreight','C',NO,NO,'','', 
        OUTPUT cReturn, OUTPUT lFound).
    
    RETURN lFound AND cReturn EQ "FGFreightClass".    
             
END FUNCTION.

FUNCTION HasReleases RETURNS LOGICAL 
    (ipriEb AS ROWID):
    /*------------------------------------------------------------------------------
     Purpose: Given a rowid for eb, determine if there are estReleases
     Notes:
    ------------------------------------------------------------------------------*/	
    
    FIND FIRST eb NO-LOCK 
        WHERE ROWID(eb) EQ ipriEb
        NO-ERROR.
    RETURN AVAILABLE eb 
        AND CAN-FIND(FIRST estRelease 
        WHERE estRelease.company EQ eb.company
        AND estRelease.estimateNo EQ eb.est-no
        AND estRelease.formNo EQ eb.form-no
        AND estRelease.blankNo EQ eb.blank-no).  
			
END FUNCTION.

FUNCTION UseReleasesForFreightAndWarehousing RETURNS LOGICAL 
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns NK1 setting indicating that new estRelease table should be used
     to calculate Freight and Warehousing costs
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    RUN sys\ref\nk1look.p (ipcCompany,'CEReleases','L',NO,NO,'','', 
        OUTPUT cReturn, OUTPUT lFound).
    
    RETURN lFound AND cReturn EQ "YES". 			
END FUNCTION.

