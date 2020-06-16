
/*------------------------------------------------------------------------
    File        : VendorCostConverter.p
    Purpose     : 

    Syntax      :

    Description : Procedures for the VendorCostConversion		

    Author(s)   : BV
    Created     : Mon Sep 30 12:39:18 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*Temp-table Definitions*/
DEFINE TEMP-TABLE ttProcessed 
    FIELD imported   AS LOGICAL
    FIELD note       AS CHARACTER 
    FIELD rec_key LIKE vendItemCost.rec_key 
    FIELD vendItemCostID LIKE vendItemCost.vendItemCostID 
    FIELD vendorItemID LIKE vendItemCost.vendorItemID 
    FIELD company LIKE vendItemCost.company 
    FIELD itemID LIKE vendItemCost.itemID 
    FIELD itemType LIKE vendItemCost.itemType 
    FIELD vendorID LIKE vendItemCost.vendorID 
    FIELD customerID LIKE vendItemCost.customerID 
    FIELD estimateNo LIKE vendItemCost.estimateNo 
    FIELD formNo LIKE vendItemCost.formNo 
    FIELD blankNo LIKE vendItemCost.blankNo
    FIELD eQty       AS DECIMAL
    FIELD vendorUOM LIKE vendItemCost.vendorUOM
    FIELD effectiveDate LIKE vendItemCost.effectiveDate 
    FIELD expirationDate LIKE vendItemCost.expirationDate 
    FIELD createdDate LIKE vendItemCost.createdDate 
    FIELD createdID LIKE vendItemCost.createdID 
    FIELD updatedDate LIKE vendItemCost.updatedDate 
    FIELD updatedID LIKE vendItemCost.updatedID 
    FIELD dimLengthMaximum LIKE vendItemCost.dimLengthMaximum 
    FIELD dimLengthMinimum LIKE vendItemCost.dimLengthMinimum 
    FIELD dimLengthOver LIKE vendItemCost.dimLengthOver 
    FIELD dimLengthOverCharge LIKE vendItemCost.dimLengthOverCharge
    FIELD dimLengthUnder LIKE vendItemCost.dimLengthUnder 
    FIELD dimLengthUnderCharge LIKE vendItemCost.dimLengthUnderCharge 
    FIELD dimUom LIKE vendItemCost.dimUOM
    FIELD dimWidthMaximum LIKE vendItemCost.dimWidthMaximum 
    FIELD dimWidthMinimum LIKE vendItemCost.dimWidthMinimum 
    FIELD dimWidthOver LIKE vendItemCost.dimWidthOver 
    FIELD dimWidthOverCharge LIKE vendItemCost.dimWidthOverCharge 
    FIELD dimWidthUnder LIKE vendItemCost.dimWidthUnder 
    FIELD dimWidthUnderCharge LIKE vendItemCost.dimWidthUnderCharge 
    FIELD leadDays LIKE vendItemCost.leadDays 
    FIELD overPercentAllowed LIKE vendItemCost.overPercentAllowed 
    FIELD quantityMaximumOrder LIKE vendItemCost.quantityMaximumOrder 
    FIELD quantityMinimumOrder LIKE vendItemCost.quantityMinimumOrder 
    FIELD underPercentAllowed LIKE vendItemCost.underPercentAllowed 
    FIELD useQuantityFromBase LIKE vendItemCost.useQuantityFromBase 
    FIELD validLengthIncrement LIKE vendItemCost.validLengthIncrement 
    FIELD validLength LIKE vendItemCost.validLength
    FIELD validWidth LIKE vendItemCost.validWidth
    .

/*Persistent Handles*/
DEFINE VARIABLE ghOutput          AS HANDLE.
DEFINE VARIABLE ghVendorCost      AS HANDLE.        

/*Setting variables*/
DEFINE VARIABLE glUseQtyFrom      AS LOGICAL   NO-UNDO.

/*Globals*/
DEFINE VARIABLE giCountProcessed  AS INTEGER   NO-UNDO.
DEFINE VARIABLE giCountConverted  AS INTEGER   NO-UNDO.
DEFINE VARIABLE glConvertAllCompanies AS LOGICAL NO-UNDO.
/*Constants*/
DEFINE VARIABLE gdDefaultEffective AS DATE NO-UNDO INITIAL 01/01/1900.
DEFINE VARIABLE gdDefaultExpiration AS DATE NO-UNDO INITIAL 12/31/2099.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
      


/* **********************  Internal Procedures  *********************** */
PROCEDURE pAddConvertedProcessed PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Copies the converted 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttProcessed FOR ttProcessed.
    DEFINE PARAMETER BUFFER ipbf-vendItemCost FOR vendItemCost.
    DEFINE INPUT PARAMETER ipcAppendNote AS CHARACTER NO-UNDO.
    
    BUFFER-COPY ipbf-vendItemCost TO ipbf-ttProcessed.
    ASSIGN 
        ipbf-ttProcessed.Note = "Successfully converted" + ipcAppendNote
        ipbf-ttProcessed.imported = YES
        giCountConverted = giCountConverted + 1
        .

END PROCEDURE.

PROCEDURE pAddErrorProcessedRM PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttProcessed FOR ttProcessed.
    DEFINE PARAMETER BUFFER ipbf-e-item-vend FOR e-item-vend.
    DEFINE PARAMETER BUFFER ipbf-e-item FOR e-item.
    DEFINE INPUT PARAMETER ipcNote AS CHARACTER NO-UNDO.
    
    ASSIGN 
        ipbf-ttProcessed.itemID = ipbf-e-item.i-no
        ipbf-ttProcessed.itemType = "RM"
        ipbf-ttProcessed.company = ipbf-e-item.company
        ipbf-ttProcessed.vendorID = ipbf-e-item-vend.vend-no
        ipbf-ttProcessed.Note = ipcNote
        ipbf-ttProcessed.imported = NO
        .

END PROCEDURE.

PROCEDURE pAddErrorProcessedFG PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttProcessed FOR ttProcessed.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg FOR e-itemfg.
    DEFINE INPUT PARAMETER ipcNote AS CHARACTER NO-UNDO.
    
    
    ASSIGN 
        ipbf-ttProcessed.itemID = ipbf-e-itemfg.i-no
        ipbf-ttProcessed.itemType = "FG"
        ipbf-ttProcessed.company = ipbf-e-itemfg.company
        ipbf-ttProcessed.vendorID = ipbf-e-itemfg-vend.vend-no
        ipbf-ttProcessed.customerID = ipbf-e-itemfg-vend.cust-no
        ipbf-ttProcessed.estimateNo = ipbf-e-itemfg-vend.est-no
        ipbf-ttProcessed.formNo = ipbf-e-itemfg-vend.form-no
        ipbf-ttProcessed.blankNo = ipbf-e-itemfg-vend.blank-no
        ipbf-ttProcessed.EQty = ipbf-e-itemfg-vend.eqty
        ipbf-ttProcessed.Note = ipcNote
        ipbf-ttProcessed.imported = NO
        .

END PROCEDURE.

PROCEDURE ConvertLegacyToNew:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplConvertFarm AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplConvertRM AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplConvertFG AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveFG AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveVend AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveCust AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dTimer   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttProcessed.
    ASSIGN 
        giCountProcessed = 0
        giCountConverted = 0
        dTimer = TIME
        .        

    RUN system\OutputProcs.p PERSISTENT SET ghOutput.
    RUN system\VendorCostProcs.p PERSISTENT SET ghVendorCost.

    RUN pSetGlobalSettings(ipcCompany).
    IF iplConvertRM THEN 
        RUN pProcessRM(ipcCompany, iplIncludeInactiveVend).
    IF iplConvertFG OR iplConvertFarm THEN 
        RUN pProcessFG(ipcCompany, iplConvertFarm, iplConvertFG, iplIncludeInactiveFG, iplIncludeInactiveVend, iplIncludeInactiveCust).
    
    ASSIGN 
        oplError = NO 
        opcMessage = "Processed: " + STRING(giCountProcessed) + CHR(13) +
                    "Converted: " + STRING(giCountConverted) + CHR(13) +
                    "Time: " + STRING(TIME - dTimer)
            .
  

    RUN TempTableToCSV IN ghOutput (TEMP-TABLE ttProcessed:HANDLE, 
                                    ipcOutputFile, 
                                    TRUE /* Export Header */,
                                    OUTPUT lSuccess,
                                    OUTPUT cMessage).
    DELETE OBJECT ghOutput.
    DELETE OBJECT ghVendorCost.

END PROCEDURE.

PROCEDURE pCreateVendItemCostFromEItemfgVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttProcessed FOR ttProcessed.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg      FOR e-itemfg.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAppendNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iFormNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBlankNo AS INTEGER NO-UNDO.
    
    ASSIGN 
        iFormNo = MAX(ipbf-e-itemfg-vend.form-no,1)
        iBlankNo = MAX(ipbf-e-itemfg-vend.blank-no,1)
        .
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-itemfg-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-itemfg-vend.i-no
        AND bf-vendItemCost.itemType EQ "FG"
        AND bf-vendItemCost.vendorID EQ ipbf-e-itemfg-vend.vend-no
        AND bf-vendItemCost.customerID EQ ipbf-e-itemfg-vend.cust-no
        AND bf-vendItemCost.estimateNo EQ ipbf-e-itemfg-vend.est-no
        AND bf-vendItemCost.formNo EQ iFormNo
        AND bf-vendItemCost.blankNo EQ iBlankNo)
        THEN 
    DO:
        RUN pAddErrorProcessedFG(BUFFER ipbf-ttProcessed, BUFFER ipbf-e-itemfg-vend, BUFFER ipbf-e-itemfg, "Duplicate already converted").
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            opiVendItemCostID                = bf-vendItemCost.vendItemCostID
            bf-vendItemCost.company          = ipbf-e-itemfg-vend.company
            bf-vendItemCost.itemID           = ipbf-e-itemfg-vend.i-no
            bf-vendItemCost.itemType         = "FG"
            bf-vendItemCost.vendorID         = ipbf-e-itemfg-vend.vend-no
            bf-vendItemCost.customerID       = ipbf-e-itemfg-vend.cust-no
            bf-vendItemCost.estimateNo       = ipbf-e-itemfg-vend.est-no
            bf-vendItemCost.formNo           = iFormNo
            bf-vendItemCost.blankNo          = iBlankNo
            bf-vendItemCost.dimWidthMinimum  = ipbf-e-itemfg-vend.roll-w[27]
            bf-vendItemCost.dimWidthMaximum  = ipbf-e-itemfg-vend.roll-w[28]
            bf-vendItemCost.dimLengthMinimum = ipbf-e-itemfg-vend.roll-w[29]
            bf-vendItemCost.dimLengthMaximum = ipbf-e-itemfg-vend.roll-w[30]
            bf-vendItemCost.dimUOM           = "IN"
            bf-vendItemCost.vendorItemID     = ipbf-e-itemfg-vend.vend-item
            bf-vendItemCost.useQuantityFrom  = glUseQtyFrom
            bf-vendItemCost.effectiveDate    = gdDefaultEffective
            bf-vendItemCost.expirationDate   = gdDefaultExpiration
            .
        IF ipbf-e-itemfg.std-uom NE "" THEN                         
            cUOM = CAPS(ipbf-e-itemfg.std-uom).
        ELSE IF ipbf-e-itemfg-vend.std-uom NE "" THEN 
            cUOM = CAPS(ipbf-e-itemfg-vend.std-uom).
        ELSE
            ASSIGN 
                cUOM = "EA"
                cAppendNote = " but blank UOM entered as default of EA"
                .
        bf-vendItemCost.vendorUOM = cUOM.
        
        DO iIndex = 1 TO 26:
            bf-vendItemCost.validWidth[iIndex] = IF ipbf-e-itemfg-vend.roll-w[iIndex] NE 0 
                                                    THEN ipbf-e-itemfg-vend.roll-w[iIndex] 
                                                    ELSE ipbf-e-itemfg.roll-w[iIndex].
        END.
        DO iIndex = 1 TO 10:
            IF ipbf-e-itemfg-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-itemfg-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-itemfg-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-itemfg-vend.setups[iIndex]
                    .
            END. /*run-qty ne 0*/
        END.  /*Do loop 1*/      
        RUN RecalculateFromAndTo IN ghVendorCost (bf-vendItemCost.vendItemCostID, OUTPUT lError, OUTPUT cMessage).    
        RUN pAddConvertedProcessed(BUFFER ipbf-ttProcessed, BUFFER bf-vendItemCost, cAppendNote).    
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

PROCEDURE pCreateVendItemCostFromEItemVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttProcessed FOR ttProcessed.
    DEFINE PARAMETER BUFFER ipbf-e-item-vend FOR e-item-vend.
    DEFINE PARAMETER BUFFER ipbf-e-item      FOR e-item.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAppendNote AS CHARACTER NO-UNDO.
        
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-item-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-item-vend.i-no
        AND bf-vendItemCost.itemType EQ "RM"
        AND bf-vendItemCost.vendorID EQ ipbf-e-item-vend.vend-no)
        THEN 
    DO:
        RUN pAddErrorProcessedRM(BUFFER ipbf-ttProcessed, BUFFER ipbf-e-item-vend, BUFFER ipbf-e-item,"Duplicate already converted").
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            opiVendItemCostID                    = bf-vendItemCost.vendItemCostID
            bf-vendItemCost.company              = ipbf-e-item-vend.company
            bf-vendItemCost.itemID               = ipbf-e-item-vend.i-no
            bf-vendItemCost.itemType             = "RM"
            bf-vendItemCost.vendorID             = ipbf-e-item-vend.vend-no
            bf-vendItemCost.dimWidthMinimum      = ipbf-e-item-vend.roll-w[27]
            bf-vendItemCost.dimWidthMaximum      = ipbf-e-item-vend.roll-w[28]
            bf-vendItemCost.dimLengthMinimum     = ipbf-e-item-vend.roll-w[29]
            bf-vendItemCost.dimLengthMaximum     = ipbf-e-item-vend.roll-w[30]
            bf-vendItemCost.dimWidthUnder        = ipbf-e-item-vend.underWidth
            bf-vendItemCost.dimWidthUnderCharge  = ipbf-e-item-vend.underWidthCost
            bf-vendItemCost.dimLengthUnder       = ipbf-e-item-vend.underLength
            bf-vendItemCost.dimLengthUnderCharge = ipbf-e-item-vend.underLengthCost
            bf-vendItemCost.dimUOM               = "IN"
            bf-vendItemCost.vendorItemID         = ipbf-e-item-vend.vend-item
            bf-vendItemCost.effectiveDate        = gdDefaultEffective
            bf-vendItemCost.expirationDate       = gdDefaultExpiration
            .
        IF ipbf-e-item.std-uom NE "" THEN                         
            cUOM = CAPS(ipbf-e-item.std-uom).
        ELSE IF ipbf-e-item-vend.std-uom NE "" THEN 
            cUOM = CAPS(ipbf-e-item-vend.std-uom).
        ELSE
            ASSIGN 
                cUOM = "EA"
                cAppendNote = " but blank UOM entered as default of EA"
                .
        bf-vendItemCost.vendorUOM = cUOM.
        
        DO iIndex = 1 TO 26:
            bf-vendItemCost.validWidth[iIndex] = IF ipbf-e-item-vend.roll-w[iIndex] NE 0 
                                                    THEN ipbf-e-item-vend.roll-w[iIndex] 
                                                    ELSE ipbf-e-item.roll-w[iIndex].
        END.
        DO iIndex = 1 TO 10:
            IF ipbf-e-item-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-item-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-item-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-item-vend.setups[iIndex]
                    .
            END. /*run-qty ne 0*/
        END.  /*Do loop 1*/              
        DO iIndex = 1 TO 10:
            IF ipbf-e-item-vend.runQtyXtra[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-item-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-item-vend.runCostXtra[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-item-vend.setupsXtra[iIndex]
                    .
            END. /*runQtyExtra ne 0*/
        END.  /*Do loop 2*/
        RUN RecalculateFromAndTo IN ghVendorCost (bf-vendItemCost.vendItemCostID, OUTPUT lError, OUTPUT cMessage).    
        RUN pAddConvertedProcessed(BUFFER ipbf-ttProcessed, BUFFER bf-vendItemCost, cAppendNote). 
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

PROCEDURE pProcessFG PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and range of items, process Fg e-itemfg-vend records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplConvertFarm AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplConvertFG AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveFG AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveVend AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveCust AS LOGICAL NO-UNDO.
        
    DEFINE VARIABLE iVendCostItemID AS INT64   NO-UNDO.
    
    FOR EACH e-itemfg NO-LOCK 
        WHERE (e-itemfg.company EQ ipcCompany OR glConvertAllCompanies)
        ,
        EACH e-itemfg-vend NO-LOCK 
        WHERE e-itemfg-vend.company EQ e-itemfg.company
        AND e-itemfg-vend.i-no EQ e-itemfg.i-no:
        
        IF NOT iplConvertFG AND e-itemfg-vend.est-no EQ "" THEN NEXT.
       
        IF NOT iplConvertFarm AND e-itemfg-vend.est-no NE "" THEN NEXT.
        
        CREATE ttProcessed.
        giCountProcessed = giCountProcessed + 1.
        IF e-itemfg-vend.i-no EQ "" AND e-itemfg-vend.est-no EQ "" THEN DO:
            RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Blank FG Item ID").
            NEXT.
        END.
        ELSE IF e-itemfg-vend.i-no NE "" THEN DO:
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ e-itemfg-vend.company
                AND itemfg.i-no EQ e-itemfg-vend.i-no
                NO-ERROR.
            IF NOT AVAILABLE itemfg THEN DO:
                RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Invalid FG Item ID: " + e-itemfg-vend.i-no).                
            END.
        END.            
        IF e-itemfg-vend.vend-no NE "" THEN DO:
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ e-itemfg-vend.company
                AND vend.vend-no EQ e-itemfg-vend.vend-no
                NO-ERROR.
            IF AVAILABLE vend THEN DO:
                IF NOT iplIncludeInactiveVend AND vend.active EQ "I" THEN DO: 
                    RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Vendor Inactive: " + vend.vend-no).
                    NEXT.
                END.  
            END.
            ELSE DO: 
                RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Vendor Invalid: " + TRIM(e-itemfg-vend.vend-no)).
                NEXT.
            END.
                
        END.
        IF e-itemfg-vend.cust-no NE "" THEN DO:
            FIND FIRST cust NO-LOCK 
                WHERE cust.company EQ e-itemfg-vend.company
                AND cust.cust-no EQ e-itemfg-vend.cust-no
                NO-ERROR.
            IF AVAILABLE cust THEN DO:
                IF NOT iplIncludeInactiveVend AND cust.active EQ "I" THEN DO: 
                    RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Customer Inactive: " + cust.cust-no).
                    NEXT.
                END.  
            END.
            ELSE DO: 
                RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "Customer Invalid: " + e-itemfg-vend.cust-no).
                NEXT.
            END.                
        END.
        IF e-itemfg-vend.run-qty[1] EQ 0 THEN DO:
            RUN pAddErrorProcessedFG(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, "No initial cost level").
            NEXT.
        END.
        
        RUN pCreateVendItemCostFromEItemFgVend(BUFFER ttProcessed, BUFFER e-itemfg-vend, BUFFER e-itemfg, OUTPUT iVendCostItemID).
        
    END. /*each e-item-vend*/


END PROCEDURE.

PROCEDURE pProcessRM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and range of items, process RM e-item-vend records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeInactiveVend AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iVendCostItemID AS INT64 NO-UNDO.

    FOR EACH e-item-vend NO-LOCK 
        WHERE (e-item-vend.company EQ ipcCompany OR glConvertAllCompanies),
        FIRST e-item NO-LOCK 
        WHERE e-item.company EQ e-item-vend.company
        AND e-item.i-no EQ e-item-vend.i-no:
        
        CREATE ttProcessed.
        giCountProcessed = giCountProcessed + 1.
        IF e-item-vend.i-no EQ "" THEN DO:
            RUN pAddErrorProcessedRM(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, "Blank RM Item ID").
            NEXT.
        END.
        ELSE DO:
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ e-item-vend.company
                AND item.i-no EQ e-item-vend.i-no
                NO-ERROR.
            IF NOT AVAILABLE ITEM THEN DO:
                RUN pAddErrorProcessedRM(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, "Invalid RM Item ID: " + e-item-vend.i-no).
            END.
        END.
        IF e-item-vend.vend-no NE "" THEN DO:
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ e-item-vend.company
                AND vend.vend-no EQ e-item-vend.vend-no
                NO-ERROR.
            IF AVAILABLE vend THEN DO:
                IF NOT iplIncludeInactiveVend AND vend.active EQ "I" THEN DO:
                    RUN pAddErrorProcessedRM(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, "Vendor Inactive: " + vend.vend-no).
                    NEXT.
                END.  
            END.
            ELSE DO:
                RUN pAddErrorProcessedRM(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, "Vendor Invalid: " + TRIM(e-item-vend.vend-no)).
                NEXT.
            END.
                
        END.
        IF e-item-vend.run-qty[1] EQ 0 THEN DO:
            RUN pAddErrorProcessedRM(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, "No initial cost level").
            NEXT.
        END.
        RUN pCreateVendItemCostFromEItemVend(BUFFER ttProcessed, BUFFER e-item-vend, BUFFER e-item, OUTPUT iVendCostItemID).
        
    END. /*each e-item-vend*/


END PROCEDURE.

PROCEDURE PurgeAllNewForCompany:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all Test Data Loaded to reset
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER iplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FOR EACH bf-vendItemCost EXCLUSIVE-LOCK
        WHERE bf-vendItemCost.company EQ ipcCompany OR glConvertAllCompanies:
        iCount = iCount + 1.
        DELETE bf-vendItemCost.
    END.
    RELEASE bf-vendItemCost.
    ASSIGN 
        iplError = NO
        ipcMessage = "Successfully deleted " + STRING(iCount) + " records for company " + ipcCompany
        .        
    
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "VendCostMatrix", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glUseQtyFrom = cReturn EQ "YES".
    
END PROCEDURE.
