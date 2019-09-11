
/*------------------------------------------------------------------------
    File        : VendorCostTester.p
    Purpose     : 

    Syntax      :

    Description : Tester/Loader for VendorCostProcs.p Testing

    Author(s)   : BV
    Created     : Wed Sep 04 21:12:54 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttDuplicates
    FIELD cItem       AS CHARACTER
    FIELD cVendor     AS CHARACTER
    FIELD cCustomer   AS CHARACTER 
    FIELD cEstimateNo AS CHARACTER
    FIELD iForm       AS INTEGER
    FIELD iBlank      AS INTEGER
    FIELD dEQty       AS DECIMAL
    FIELD cCompany    AS CHARACTER
    FIELD cItemType   AS CHARACTER
    .
        
/*Persistent Handles*/
DEFINE VARIABLE ghSession        AS HANDLE.
DEFINE VARIABLE ghOutput         AS HANDLE.
DEFINE VARIABLE ghVendorCost     AS HANDLE.

/*Constants*/
DEFINE VARIABLE gcCompany        AS CHARACTER NO-UNDO INITIAL '001'.
DEFINE VARIABLE gcItemIDRM      AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE gcItemIDFG      AS CHARACTER NO-UNDO INITIAL ''.
DEFINE VARIABLE glRunAllCompanies AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE glRunAllFG       AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE glRunAllRM       AS LOGICAL NO-UNDO INITIAL NO. 
DEFINE VARIABLE glPurge          AS LOGICAL   NO-UNDO INITIAL NO.
DEFINE VARIABLE glDisplayDetail  AS LOGICAL   NO-UNDO INITIAL NO.

/*Setting variables*/
DEFINE VARIABLE glUseQtyFrom     AS LOGICAL   NO-UNDO.

/*Globals*/
DEFINE VARIABLE giCountCreated   AS INTEGER   NO-UNDO.
DEFINE VARIABLE giCountDuplicate AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN system\OutputProcs.p PERSISTENT SET ghOutput.
SESSION:ADD-SUPER-PROCEDURE (ghOutput).
RUN system\VendorCostProcs.p PERSISTENT SET ghVendorCost.
SESSION:ADD-SUPER-PROCEDURE (ghVendorCost).

RUN pSetGlobalSettings(gcCompany). 
RUN pConvertLegacyToNew.

//RUN pTestSampleFG.
RUN pTestSampleRM.

DELETE OBJECT ghSession.
DELETE OBJECT ghOutput.
DELETE OBJECT ghVendorCost.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pConvertLegacyToNew PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE dTimer AS DECIMAL NO-UNDO.
dTimer = TIME.
IF glPurge THEN RUN pPurgeAll.
RUN pProcessRM(gcCompany, gcItemIDRM).
RUN pProcessFG(gcCompany, gcItemIDFG).
MESSAGE "Created " giCountCreated SKIP
    "Duplicate " giCountDuplicate
    "Time " TIME - dTimer
    VIEW-AS ALERT-BOX.

RUN TempTableToCSV(TEMP-TABLE ttDuplicates:HANDLE, "C:\tmp\Duplicates.csv", TRUE /* Export Header */).

END PROCEDURE.

PROCEDURE pCreateVendItemCostFromEItemfgVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-e-itemfg-vend FOR e-itemfg-vend.
    DEFINE PARAMETER BUFFER ipbf-e-itemfg      FOR e-itemfg.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyFrom AS DECIMAL NO-UNDO.
    
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-itemfg-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-itemfg-vend.i-no
        AND bf-vendItemCost.itemType EQ "FG"
        AND bf-vendItemCost.vendorID EQ ipbf-e-itemfg-vend.vend-no
        AND bf-vendItemCost.customerID EQ ipbf-e-itemfg-vend.cust-no
        AND bf-vendItemCost.estimateNo EQ ipbf-e-itemfg-vend.est-no
        AND bf-vendItemCost.formNo EQ ipbf-e-itemfg-vend.form-no
        AND bf-vendItemCost.blankNo EQ ipbf-e-itemfg-vend.blank-no)
        THEN 
    DO:
        giCountDuplicate = giCountDuplicate + 1.
        CREATE ttDuplicates.
        ASSIGN 
            ttDuplicates.cCompany    = ipbf-e-itemfg-vend.company
            ttDuplicates.cItem       = ipbf-e-itemfg-vend.i-no
            ttDuplicates.cItemType   = "FG"
            ttDuplicates.cVendor     = ipbf-e-itemfg-vend.vend-no
            ttDuplicates.cEstimateNo = ipbf-e-itemfg-vend.est-no
            ttDuplicates.iForm       = ipbf-e-itemfg-vend.form-no
            ttDuplicates.iBlank      = ipbf-e-itemfg-vend.blank-no
            ttDuplicates.cCustomer   = ipbf-e-itemfg-vend.cust-no
            ttDuplicates.dEQty       = ipbf-e-itemfg-vend.eqty
            .
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            giCountCreated                   = giCountCreated + 1
            opiVendItemCostID                = bf-vendItemCost.vendItemCostID
            bf-vendItemCost.company          = ipbf-e-itemfg-vend.company
            bf-vendItemCost.itemID           = ipbf-e-itemfg-vend.i-no
            bf-vendItemCost.itemType         = "FG"
            bf-vendItemCost.vendorID         = ipbf-e-itemfg-vend.vend-no
            bf-vendItemCost.customerID       = ipbf-e-itemfg-vend.cust-no
            bf-vendItemCost.estimateNo       = ipbf-e-itemfg-vend.est-no
            bf-vendItemCost.formNo           = ipbf-e-itemfg-vend.form-no
            bf-vendItemCost.blankNo          = ipbf-e-itemfg-vend.blank-no
            bf-vendItemCost.dimWidthMinimum  = ipbf-e-itemfg-vend.roll-w[27]
            bf-vendItemCost.dimWidthMaximum  = ipbf-e-itemfg-vend.roll-w[28]
            bf-vendItemCost.dimLengthMinimum = ipbf-e-itemfg-vend.roll-w[29]
            bf-vendItemCost.dimLengthMaximum = ipbf-e-itemfg-vend.roll-w[30]
            bf-vendItemCost.dimUOM           = "IN"
            bf-vendItemCost.vendorItemID     = ipbf-e-itemfg-vend.vend-item
            bf-vendItemCost.vendorUOM        = CAPS(ipbf-e-itemfg.std-uom) 
            bf-vendItemCost.useQuantityFrom  = glUseQtyFrom
            .
        DO iIndex = 1 TO 10:
            IF ipbf-e-itemfg-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-itemfg-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-itemfg-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-itemfg-vend.setups[iIndex]
                    dQtyFrom                            = bf-vendItemCostLevel.quantityTo + 0.000001
                    .
            END. /*run-qty ne 0*/
        END.  /*Do loop 1*/              
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

PROCEDURE pCreateVendItemCostFromEItemVend PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an e-item-vend buffer, create vendItemCost record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-e-item-vend FOR e-item-vend.
    DEFINE PARAMETER BUFFER ipbf-e-item      FOR e-item.
    DEFINE OUTPUT PARAMETER opiVendItemCostID AS INT64.
    
    DEFINE BUFFER bf-vendItemCost      FOR vendItemCost.
    DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
    
    DEFINE VARIABLE iIndex   AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyFrom AS DECIMAL NO-UNDO.
    
    IF CAN-FIND(FIRST bf-vendItemCost
        WHERE bf-vendItemCost.company EQ ipbf-e-item-vend.company
        AND bf-vendItemCost.itemID EQ ipbf-e-item-vend.i-no
        AND bf-vendItemCost.itemType EQ "RM"
        AND bf-vendItemCost.vendorID EQ ipbf-e-item-vend.vend-no)
        THEN 
    DO:
        giCountDuplicate = giCountDuplicate + 1.
        CREATE ttDuplicates.
        ASSIGN 
            ttDuplicates.cCompany  = ipbf-e-item-vend.company
            ttDuplicates.cItem     = ipbf-e-item-vend.i-no
            ttDuplicates.cItemType = "RM"
            ttDuplicates.cVendor   = ipbf-e-item-vend.vend-no
            .
    END.
    ELSE 
    DO:
        CREATE bf-vendItemCost.
        ASSIGN  
            giCountCreated                       = giCountCreated + 1
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
            bf-vendItemCost.vendorUOM            = CAPS(ipbf-e-item.std-uom) 
            .
        DO iIndex = 1 TO 10:
            IF ipbf-e-item-vend.run-qty[iIndex] NE 0 THEN 
            DO:
                CREATE bf-vendItemCostLevel.
                ASSIGN 
                    bf-vendItemCostLevel.vendItemCostID = opiVendItemCostID
                    bf-vendItemCostLevel.quantityBase   = ipbf-e-item-vend.run-qty[iIndex]
                    bf-vendItemCostLevel.costPerUOM     = ipbf-e-item-vend.run-cost[iIndex]
                    bf-vendItemCostLevel.costSetup      = ipbf-e-item-vend.setups[iIndex]
                    dQtyFrom                            = bf-vendItemCostLevel.quantityTo + 0.000001
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
                    dQtyFrom                            = bf-vendItemCostLevel.quantityTo + 0.000001
                    .
            END. /*runQtyExtra ne 0*/
        END.  /*Do loop 2*/              
    END. /*Not duplicate*/
    RELEASE bf-vendItemCost.

END PROCEDURE.

PROCEDURE pGetSettings PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Sets Settings (NK1s) for Proc
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.



END PROCEDURE.

PROCEDURE pProcessFG PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and range of items, process Fg e-itemfg-vend records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.
        
    DEFINE VARIABLE lUseQtyFrom AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iVendCostItemID AS INT64 NO-UNDO.
    
    FOR EACH e-itemfg NO-LOCK 
        WHERE (e-itemfg.company EQ ipcCompany OR glRunAllCompanies)
        AND (e-itemfg.i-no EQ ipcINo AND e-itemfg.i-no NE '' OR glRunAllFG),
        EACH e-itemfg-vend NO-LOCK 
        WHERE e-itemfg-vend.company EQ e-itemfg.company
        AND e-itemfg-vend.i-no EQ e-itemfg.i-no:
    
        RUN pCreateVendItemCostFromEItemFgVend(BUFFER e-itemfg-vend, BUFFER e-itemfg, OUTPUT iVendCostItemID).
        IF iVendCostItemID NE 0 AND glDisplayDetail THEN DO:
            FIND FIRST vendItemCost NO-LOCK
                WHERE vendItemCost.vendItemCostID EQ iVendCostItemID
                NO-ERROR.
            IF AVAILABLE vendItemCost THEN DO:
                DISPLAY vendItemCost.company vendItemCost.itemID vendItemCost.vendorID.
                FOR EACH vendItemCostLevel NO-LOCK
                    WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID
                    BY vendItemCostLevel.quantityFrom:
                    DISPLAY vendItemCostLevel.quantityFrom FORMAT ">>>>>>9.999999" vendItemCostLevel.quantityTo FORMAT ">>>>>>9.999999" vendItemCostLevel.costPerUOM vendItemCostLevel.costSetup.
                END.
            END. /*avail vendItemCost*/
        END. /*Vend iteCostID ne 0*/
    END. /*each e-item-vend*/


END PROCEDURE.

PROCEDURE pProcessRM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and range of items, process RM e-item-vend records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcINo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iVendCostItemID AS INT64 NO-UNDO.

    FOR EACH e-item-vend NO-LOCK 
        WHERE (e-item-vend.company EQ ipcCompany OR glRunAllCompanies)
        AND (e-item-vend.i-no EQ ipcINo AND e-item-vend.i-no NE '' OR glRunAllRM)
        OR ipcINo EQ 'ALL',
        FIRST e-item NO-LOCK 
        WHERE e-item.company EQ e-item-vend.company
        AND e-item.i-no EQ e-item-vend.i-no:
        
        RUN pCreateVendItemCostFromEItemVend(BUFFER e-item-vend, BUFFER e-item, OUTPUT iVendCostItemID).
        IF iVendCostItemID NE 0 AND glDisplayDetail THEN DO:
            FIND FIRST vendItemCost NO-LOCK
                WHERE vendItemCost.vendItemCostID EQ iVendCostItemID
                NO-ERROR.
            IF AVAILABLE vendItemCost THEN DO:
                DISPLAY vendItemCost.company vendItemCost.itemID vendItemCost.vendorID.
                FOR EACH vendItemCostLevel NO-LOCK
                    WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID
                    BY vendItemCostLevel.quantityFrom:
                    DISPLAY vendItemCostLevel.quantityFrom FORMAT ">>>>>>9.999999" vendItemCostLevel.quantityTo FORMAT ">>>>>>9.999999" vendItemCostLevel.costPerUOM vendItemCostLevel.costSetup.
                END.
            END. /*avail vendItemCost*/
        END. /*Vend iteCostID ne 0*/
    END. /*each e-item-vend*/


END PROCEDURE.

PROCEDURE pPurgeAll PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Deletes all Test Data Loaded to reset
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-vendItemCost FOR vendItemCost.
    
    FOR EACH bf-vendItemCost EXCLUSIVE-LOCK:
        DELETE bf-vendItemCost.
    END.
    
    IF CAN-FIND(FIRST vendItemCost) OR CAN-FIND(FIRST vendItemCostLevel) THEN 
        MESSAGE "Purge Incomplete"
            VIEW-AS ALERT-BOX.
    ELSE 
        MESSAGE "Purge Complete"
            VIEW-AS ALERT-BOX.
    
    RELEASE bf-vendItemCost.
    
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

PROCEDURE pTestSampleFG PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCostTotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostSetup AS DECIMAL NO-UNDO.
DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.

DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

FIND FIRST itemfg NO-LOCK 
    WHERE itemfg.company EQ gcCompany
    AND itemfg.i-no EQ "ZOVPURCH"
    NO-ERROR.
FIND FIRST vendItemCost EXCLUSIVE-LOCK
    WHERE vendItemCost.company EQ itemfg.company
    AND vendItemCost.itemID EQ itemfg.i-no
    AND vendItemCost.itemType EQ "FG"
    NO-ERROR.
ASSIGN 
    vendItemCost.effectiveDate = 1/1/2019
    vendItemCost.expirationDate = 1/1/2020
    .



RUN GetVendorCost(vendItemCost.company, 
                  vendItemCost.ItemID, 
                  vendItemCost.itemType, 
                  "STAPLES", 
                  vendItemCost.customerID, 
                  "", 
                  0, 
                  0,
                  5, 
                  "MSF",
                  itemfg.t-len, 
                  itemfg.t-wid, 
                  0, 
                  "IN", 
                  itemfg.weight-100 / 100, 
                  "LB/EA", 
                  NO,
                  OUTPUT dCostPerUOM, 
                  OUTPUT dCostSetup, 
                  OUTPUT cCostUOM,
                  OUTPUT dCostTotal, 
                  OUTPUT lError, 
                  OUTPUT cMessage).  
        
MESSAGE 
    "Total Cost: " dCostTotal SKIP
    "Cost Per UOM: " dCostPerUOM SKIP 
    "Cost UOM: " cCostUOM SKIP 
    "Cost Setup: " dCostSetup SKIP
    "Error: " lError SKIP 
    "Message: " cMessage SKIP
    VIEW-AS ALERT-BOX.

END PROCEDURE.

PROCEDURE pTestSampleRM PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE dCostTotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL NO-UNDO.
DEFINE VARIABLE dCostSetup AS DECIMAL NO-UNDO.
DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.

DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

FIND FIRST item NO-LOCK 
    WHERE item.company EQ gcCompany
    AND item.i-no EQ "SBS014"
    NO-ERROR.
FIND FIRST vendItemCost EXCLUSIVE-LOCK
    WHERE vendItemCost.company EQ item.company
    AND vendItemCost.itemID EQ item.i-no
    AND vendItemCost.itemType EQ "RM"
    NO-ERROR.
ASSIGN 
    vendItemCost.effectiveDate = 1/1/2019
    vendItemCost.expirationDate = 1/1/2020
    .



RUN GetVendorCost(vendItemCost.company, 
                  vendItemCost.ItemID, 
                  vendItemCost.itemType, 
                  "STAPLES", 
                  vendItemCost.customerID, 
                  "", 
                  0, 
                  0,
                  41, 
                  "MSF",
                  36, 
                  36, 
                  0, 
                  "IN", 
                  item.basis-w, 
                  "LB/MSF", 
                  NO,
                  OUTPUT dCostPerUOM, 
                  OUTPUT dCostSetup, 
                  OUTPUT cCostUOM,
                  OUTPUT dCostTotal, 
                  OUTPUT lError, 
                  OUTPUT cMessage).  
        
MESSAGE 
    "Total Cost: " dCostTotal SKIP
    "Cost Per UOM: " dCostPerUOM SKIP 
    "Cost UOM: " cCostUOM SKIP 
    "Cost Setup: " dCostSetup SKIP
    "Error: " lError SKIP 
    "Message: " cMessage SKIP
    VIEW-AS ALERT-BOX.

END PROCEDURE.
