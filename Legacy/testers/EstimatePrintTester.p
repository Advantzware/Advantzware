
/*------------------------------------------------------------------------
    File        : EstimatePrintTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE STREAM sImport.
{est/ttEstPrint.i "NEW SHARED"}

DEFINE VARIABLE gcOutputFile  AS CHARACTER INITIAL "C:\temp\estPrintOut.txt".
DEFINE VARIABLE gcTestDataDir AS CHARACTER INITIAL "C:\Users\brad.vigrass\Documents\Testing\EstimateData\".
DEFINE VARIABLE giRecKey      AS INTEGER   NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */

RUN pBuildTestData.
FIND FIRST ttEstHeader NO-LOCK.
RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Item Summary","Comic Sans Serif").

/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildSummaryFromDetail:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.

    FOR EACH ttEstCostDetail NO-LOCK
        WHERE ttEstCostDetail.rec_keyHeader EQ ipbf-ttEstHeader.rec_KeyHeader 
        :
    END.

END PROCEDURE.

PROCEDURE pAddRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Add a record for specified type with array of data
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcData AS CHARACTER NO-UNDO EXTENT 100.

    CASE ipcType:
        WHEN "EstHeader" THEN 
            DO:
                CREATE ttEstHeader.
                ASSIGN 
                    ttEstHeader.rec_KeyHeader  = ipcData[1]
                    ttEstHeader.cCompany       = ipcData[2]
                    ttEstHeader.cEstNo         = ipcData[3]
                    ttEstHeader.cEstType       = ipcData[4]
                    ttEstHeader.dQtyMaster     = DECIMAL(ipcData[5])
                    ttEstHeader.cCalculator    = ipcData[6]
                    ttEstHeader.dtCalcDateTime = DATETIME(ipcData[7])
                    .
            END.       
        WHEN "CostGroup" THEN 
            DO:
                CREATE ttEstCostGroup.
                ASSIGN
                    ttEstCostGroup.rec_keyCostGroup  = ipcData[1]
                    ttEstCostGroup.iSequence         = INTEGER(ipcData[2])
                    ttEstCostGroup.cGroupLabel       = ipcData[3]
                    ttEstCostGroup.cGroupType        = ipcData[4]
                    ttEstCostGroup.cGroupDescription = ipcData[5]
                    ttEstCostGroup.iCostGroupLevel   = INTEGER(ipcData[6])
                    .
            END.
        WHEN "CostGroupLevel" THEN 
            DO:
                CREATE ttEstCostGroupLevel.
                ASSIGN
                    ttEstCostGroupLevel.iCostGroupLevel            = INTEGER(ipcData[1])
                    ttEstCostGroupLevel.cCostGroupLevelDescription = ipcData[2]
                    .
            END.
    END CASE.
END PROCEDURE.

PROCEDURE pBuildTestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the temptables to test with
     Notes:
    ------------------------------------------------------------------------------*/
    
    RUN pLoadData("EstHeader").
    RUN pLoadData("CostGroupLevel").
    RUN pLoadData("CostGroup").
    
    
    FIND FIRST ttEstHeader NO-LOCK.        
    CREATE ttEstItem.
    ASSIGN 
        ttEstItem.rec_keyItem       = fGetNextRecKey()
        ttEstItem.rec_keyHeader     = ttEstHeader.rec_KeyHeader
        ttEstItem.dQtyPerParent     = 1
        ttEstItem.dQtyRequired      = 15000
        ttEstItem.dQtyYielded       = 15000
        ttEstItem.cItemName         = "Long Item Name"
        ttEstItem.cItemDescription1 = "Long Description 1"
        ttEstItem.cItemDescription2 = "Long Description 2"
        ttEstItem.cStyle            = "Reverse Tuck"
        ttEstItem.lIsSet            = NO
        ttEstItem.cCustomerName     = "Customer Name"
        ttEstItem.cCustomerAddress1 = "Customer Address 1"
        ttEstItem.cCustomerAddress2 = "Customer Address 2"
        ttEstItem.cCustomerAddress3 = "Customer City, ST Zipcode"
        ttEstItem.cShipToName       = "Shipto Name"
        ttEstItem.cShipToAddress1   = "Shipto Address 1"
        ttEstItem.cShipToAddress2   = "Shipto Address 2"
        ttEstItem.cShipToAddress3   = "Shipto City, ST Zipcode"
        ttEstItem.cSalesgroupName   = "Salesgroup Name"
        ttEstItem.cCustomerID       = "CUSTID"
        ttEstItem.cShipToID         = "SHIPID"
        ttEstItem.cColor            = "Color Description"
        .
    CREATE ttEstForm.
    ASSIGN 
        ttEstForm.rec_KeyForm               = fGetNextRecKey()
        ttEstForm.rec_keyHeader             = ttEstHeader.rec_KeyHeader
        ttEstForm.iFormNo                   = 1
        ttEstForm.iNumOutLength             = 1
        ttEstForm.iNumOutWidth              = 2
        ttEstForm.iNumOut                   = MAX(ttEstForm.iNumOutLength, 1) * MAX(ttEstForm.iNumOutWidth, 1) * MAX(ttEstForm.iNumOutDepth, 1)
        ttEstForm.dGrossWidth               = 48 
        ttEstForm.dGrossLength              = 74 
        ttEstForm.dGrossArea                = ttEstForm.dGrossWidth * ttEstForm.dGrossLength / 144
        ttEstForm.dNetWidth                 = ttEstForm.dGrossWidth / iNumOutWidth - 0.25
        ttEstForm.dNetLength                = ttEstForm.dGrossLength / iNumOutLength - 0.25
        ttEstForm.dNetArea                  = ttEstForm.dNetWidth * ttEstForm.dGrossLength / 144
        ttEstForm.dDieWidth                 = ttEstForm.dNetWidth - .625
        ttEstForm.dDieLength                = ttEstForm.dNetLength - .625
        ttEstForm.dDieArea                  = ttEstForm.dDieWidth * ttEstForm.dDieLength / 144
        ttEstForm.cUOMDimension             = "In"
        ttEstForm.cUOMArea                  = "SF"
        ttEstForm.dGrossQtyRequiredWasteMR  = 20
        ttEstForm.dGrossQtyRequiredWasteRun = 6
        ttEstForm.dBasisWeightInLbsPerMSF   = 123 
        ttEstForm.dWeightDie                = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dDieArea 
        ttEstForm.cUOMWeightDie             = "LB/MSHT"
        ttEstForm.dWeightNet                = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dNetArea 
        ttEstForm.cUOMWeightNet             = "LB/MSHT"
        ttEstForm.dWeightGross              = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dGrossArea
        ttEstForm.cUOMWeightGross           = "LB/MSHT"
        ttEstForm.dQtyFGOnForm              = ttEstHeader.dQtyMaster
        .
    CREATE ttEstBlank.
    ASSIGN 
        ttEstBlank.rec_keyBlank                   = fGetNextRecKey()
        ttEstBlank.rec_keyForm                    = ttEstForm.rec_KeyForm
        ttEstBlank.rec_keyItem                    = ttEstItem.rec_keyItem
        ttEstBlank.iBlankNo                       = 1
        ttEstBlank.iNumOutLength                  = 2
        ttEstBlank.iNumOutWidth                   = 1
        ttEstBlank.iNumOut                        = MAX(ttEstBlank.iNumOutWidth, 1) * MAX(ttEstBlank.iNumOutLength, 1) * MAX(ttEstBlank.iNumOutDepth, 1)
        ttEstBlank.dBlankWidth                    = ttEstForm.dDieWidth / ttEstBlank.iNumOutWidth
        ttEstBlank.dBlankLength                   = ttEstForm.dDieLength / ttEstBlank.iNumOutLength
        ttEstBlank.dBlankArea                     = ttEstBlank.dBlankWidth * ttEstBlank.dBlankLength / 144
        ttEstBlank.cUOMArea                       = "SF"
        ttEstBlank.cUOMDimension                  = "In"
        ttEstBlank.dLength                        = 11
        ttEstBlank.dWidth                         = 11
        ttEstBlank.dDepth                         = 11
    
        ttEstForm.dGrossQtyRequiredNoWaste        = ttEstItem.dQtyRequired / (ttEstBlank.iNumOut * ttEstForm.iNumOut)
        ttEstForm.dGrossQtyRequiredTotal          = ttEstForm.dGrossQtyRequiredNoWaste + ttEstForm.dGrossQtyRequiredWasteMR + ttEstForm.dGrossQtyRequiredWasteRun
        ttEstForm.dGrossQtyRequiredTotalArea      = ttEstForm.dGrossQtyRequiredTotal * ttEstForm.dGrossArea / 1000
        ttEstForm.cUOMGrossQtyRequiredTotalArea   = "MSF"
        ttEstForm.dGrossQtyRequiredTotalWeight    = ttEstForm.dGrossQtyRequiredTotalArea * ttEstForm.dBasisWeightInLbsPerMSF
        ttEstForm.cUOMGrossQtyRequiredTotalWeight = "LBS"
    
        ttEstItem.cSize                           = TRIM(STRING(ttEstBlank.dLength,">>>9.99")) + " x " + TRIM(STRING(ttEstBlank.dWidth,">>>9.99"))
        .
    IF ttEstBlank.dDepth NE 0 THEN 
        ttEstItem.cSize = ttEstItem.cSize + " x " + TRIM(STRING(ttEstBlank.dDepth,">>>9.99")).
    
    ASSIGN 
        ttEstBlank.dWeight    = ttEstForm.dBasisWeightInLbsPerMSF * ttEstBlank.dBlankArea * 144 / 1000 
        ttEstBlank.cUOMWeight = "LB/M"
        .
    
    FIND FIRST ITEM NO-LOCK 
        WHERE item.company EQ  ttEstHeader.cCompany
        AND item.i-no EQ "200 C"
        NO-ERROR.
    CREATE ttEstMaterial.
    ASSIGN 
        ttEstMaterial.rec_keyMaterial      = fGetNextRecKey()
        ttEstMaterial.rec_keyForm          = ttEstForm.rec_KeyForm
        ttEstMaterial.rec_keyBlank         = ttEstBlank.rec_keyBlank
        ttEstMaterial.cItemID              = item.i-no 
        ttEstMaterial.cItemName            = item.i-name 
        ttEstMaterial.cQtyUOM              = "EA"
        ttEstMaterial.dCostMR              = 124.34
        ttEstMaterial.dCostPerUOM          = .15
        ttEstMaterial.cCostUOM             = "EA"
        ttEstMaterial.dQtyRequiredNoWaste  = ttEstForm.dGrossQtyRequiredNoWaste
        ttEstMaterial.dQtyRequiredWasteMR  = ttEstForm.dGrossQtyRequiredWasteMR
        ttEstMaterial.dQtyRequiredWasteRun = ttEstForm.dGrossQtyRequiredWasteRun
        ttEstMaterial.dQtyRequiredTotal    = ttEstForm.dGrossQtyRequiredTotal
        ttEstMaterial.dCostTotalNoWaste    = ttEstMaterial.dQtyRequiredNoWaste * ttEstMaterial.dCostPerUOM
        ttEstMaterial.dCostTotalWasteMR    = ttEstMaterial.dQtyRequiredWasteMR * ttEstMaterial.dCostPerUOM + ttEstMaterial.dCostMR
        ttEstMaterial.dCostTotalWasteRun   = ttEstMaterial.dQtyRequiredWasteRun * ttEstMaterial.dCostPerUOM
        ttEstMaterial.dCostTotal           = ttEstMaterial.dCostTotalNoWaste + ttEstMaterial.dCostTotalWasteMR + ttEstMaterial.dCostTotalWasteRun
        ttEstMaterial.lIsPrimarySubstrate  = YES
        ttEstMaterial.lAddToWeightFG       = YES
        ttEstMaterial.lAddToWeightTare     = NO
        .
    FIND FIRST ITEM NO-LOCK 
        WHERE item.company EQ  ttEstHeader.cCompany
        AND item.i-no EQ "GLUE"
        NO-ERROR.
    CREATE ttEstMaterial.
    ASSIGN 
        ttEstMaterial.rec_keyMaterial      = fGetNextRecKey()
        ttEstMaterial.rec_keyForm          = ttEstForm.rec_KeyForm
        ttEstMaterial.rec_keyBlank         = ttEstBlank.rec_keyBlank
        ttEstMaterial.cItemID              = item.i-no 
        ttEstMaterial.cItemName            = item.i-name 
        ttEstMaterial.cQtyUOM              = "LB"
        ttEstMaterial.dCostMR              = 3
        ttEstMaterial.dCostPerUOM          = .90
        ttEstMaterial.cCostUOM             = "LB"
        ttEstMaterial.dQtyRequiredNoWaste  = 82.02
        ttEstMaterial.dQtyRequiredWasteMR  = 0
        ttEstMaterial.dQtyRequiredWasteRun = 0
        ttEstMaterial.dQtyRequiredTotal    = ttEstMaterial.dQtyRequiredNoWaste + ttEstMaterial.dQtyRequiredWasteMR + ttEstMaterial.dQtyRequiredWasteRun
        ttEstMaterial.dCostTotalNoWaste    = ttEstMaterial.dQtyRequiredNoWaste * ttEstMaterial.dCostPerUOM
        ttEstMaterial.dCostTotalWasteMR    = ttEstMaterial.dQtyRequiredWasteMR * ttEstMaterial.dCostPerUOM + ttEstMaterial.dCostMR
        ttEstMaterial.dCostTotalWasteRun   = ttEstMaterial.dQtyRequiredWasteRun * ttEstMaterial.dCostPerUOM
        ttEstMaterial.dCostTotal           = ttEstMaterial.dCostTotalNoWaste + ttEstMaterial.dCostTotalWasteMR + ttEstMaterial.dCostTotalWasteRun
        .
END PROCEDURE.


PROCEDURE pLoadData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Loads EstHeader table from Excel
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cImportFile AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE iCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cData       AS CHARACTER NO-UNDO EXTENT 100 .
    
    cImportFile = gcTestDataDir + ipcType + ".csv".
    IF SEARCH(cImportFile) NE ? THEN 
    DO:
        INPUT STREAM sImport FROM VALUE(cImportFile).
        REPEAT:
            iCount = iCount + 1.
            IMPORT STREAM sImport DELIMITER ','
                cData
                .
            IF iCount GT 1 AND cData[1] NE "" THEN 
                RUN pAddRecord(ipcType, cData).
                 
        END.
    END.
    OUTPUT STREAM sImport CLOSE.
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    giRecKey = giRecKey + 1.
    RETURN STRING(giRecKey).


		
END FUNCTION.
