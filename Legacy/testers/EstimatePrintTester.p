
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

DEFINE VARIABLE gcOutputFile          AS CHARACTER INITIAL "C:\temp\estPrintOut.txt".
DEFINE VARIABLE gcTestDataDir         AS CHARACTER INITIAL "C:\Users\brad.vigrass\Documents\Testing\EstimateData\".
DEFINE VARIABLE giRecKey              AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcSourceTypeOperation AS CHARACTER NO-UNDO INITIAL "Operation".
DEFINE VARIABLE gcSourceTypeMaterial  AS CHARACTER NO-UNDO INITIAL "Material".
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextID RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

/* ***************************  Main Block  *************************** */

RUN pBuildTestData2.
RUN pBuildDetail.
FIND FIRST ttEstHeader NO-LOCK 
    WHERE ttEstHeader.estHeaderID = "header1".
RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Item Summary","Courier New").

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdMarkup AS DECIMAL NO-UNDO.
    
    CREATE ttEstCostDetail.
    ASSIGN 
        ttEstCostDetail.rec_key            = fGetNextRecKey()
        ttEstCostDetail.estCostDetailID    = fGetNextID()
        ttEstCostDetail.estHeaderID        = ipbf-ttEstOperation.estHeaderID
        ttEstCostDetail.estFormID          = ipbf-ttEstOperation.estFormID
        ttEstCostDetail.estBlankID         = ipbf-ttEstOperation.estBlankID
        ttEstCostDetail.estSourceID        = ipbf-ttEstOperation.estOperationID 
        ttEstCostDetail.cSourceType        = gcSourceTypeOperation
        ttEstCostDetail.estCostCategoryID  = ipcEstCostCategoryID
        ttEstCostDetail.cDetailDescription = ipcDescription
        ttEstCostDetail.dCost              = ipdCost
        ttEstCostDetail.dMarkup            = ipdMarkup
        .

END PROCEDURE.

PROCEDURE pAddCostDetailForMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMaterial FOR ttEstMaterial.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdMarkup AS DECIMAL NO-UNDO.
     
    CREATE ttEstCostDetail.
    ASSIGN 
        ttEstCostDetail.rec_key            = fGetNextRecKey()
        ttEstCostDetail.estCostDetailID    = fGetNextID()
        ttEstCostDetail.estHeaderID        = ipbf-ttEstMaterial.estHeaderID
        ttEstCostDetail.estFormID          = ipbf-ttEstMaterial.estFormID
        ttEstCostDetail.estBlankID         = ipbf-ttEstMaterial.estBlankID
        ttEstCostDetail.estSourceID        = ipbf-ttEstMaterial.estMaterialID 
        ttEstCostDetail.cSourceType        = gcSourceTypeMaterial
        ttEstCostDetail.estCostCategoryID  = ipcEstCostCategoryID
        ttEstCostDetail.cDetailDescription = ipcDescription
        ttEstCostDetail.dCost              = ipdCost
        ttEstCostDetail.dMarkup            = ipdMarkup.
    

END PROCEDURE.

PROCEDURE pBuildCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.

    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory1","Operation Setup DL",
        ipbf-ttEstOperation.dCostTotalDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory2","Operation Setup VOH",
        ipbf-ttEstOperation.dCostTotalVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory3","Operation Setup FOH",
        ipbf-ttEstOperation.dCostTotalFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory4","Operation Run DL",
        ipbf-ttEstOperation.dCostTotalDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory5","Operation Run VOH",
        ipbf-ttEstOperation.dCostTotalVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory6","Operation Run FOH",
        ipbf-ttEstOperation.dCostTotalFORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory7","Operation Setup DL - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory8","Operation Setup VOH - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory9","Operation Setup FOH - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory10","Operation Run DL - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory11","Operation Run VOH - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "CostCategory12","Operation Run FOH - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffFORun,0).
        
END PROCEDURE.

PROCEDURE pBuildDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the cost detail from the Test Data
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttEstCostDetail.
    FOR EACH ttEstHeader NO-LOCK:
        /*Process Operations*/
        FOR EACH ttEstOperation NO-LOCK 
            WHERE ttEstOperation.estHeaderID EQ ttEstHeader.estHeaderID:
            RUN pBuildCostDetailForOperation(BUFFER ttEstOperation).
        END. /*Each ttEstOperation for estHeader*/    
        /*Process Materials*/
        FOR EACH ttEstMaterial NO-LOCK 
            WHERE ttEstMaterial.estHeaderID EQ ttEstHeader.estHeaderID:

                    
        END. /*Each ttEstMaterial for Form*/
    
    END. /*Each ttEstHeader*/    

END PROCEDURE.

PROCEDURE pBuildSummaryFromDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    
    EMPTY TEMP-TABLE ttEstCostSummary.
    FOR EACH ttEstCostDetail NO-LOCK
        WHERE ttEstCostDetail.estHeaderID EQ ipbf-ttEstHeader.estHeaderID 
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
                    ttEstHeader.estHeaderID    = ipcData[1]
                    ttEstHeader.company        = ipcData[2]
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
                    ttEstCostGroup.estCostGroupID    = ipcData[1]
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
        WHEN "CostCategory" THEN 
            DO:
                CREATE ttEstCostCategory.
                ASSIGN
                    ttEstCostCategory.estCostCategoryID    = ipcData[1]
                    ttEstCostCategory.cCategoryLabel       = ipcData[2]
                    ttEstCostCategory.cCategoryDescription = ipcData[3]
                    ttEstCostCategory.cBasis               = ipcData[4]
                    ttEstCostCategory.estCostGroupID       = ipcData[5]
                    ttEstCostCategory.cCostModel           = ipcData[6]
                    ttEstCostCategory.cIncludeIn           = ipcData[7]
                    .
            END.
        WHEN "Item" THEN 
            DO:
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.estItemID         = ipcData[1]
                    ttEstItem.estHeaderID       = ipcData[2]
                    ttEstItem.cCustomerPart     = ipcData[3]
                    ttEstItem.dQtyPerParent     = DECIMAL(ipcData[4])
                    ttEstItem.dQtyRequired      = DECIMAL(ipcData[5])
                    ttEstItem.dQtyYielded       = DECIMAL(ipcData[6])
                    ttEstItem.cItemName         = ipcData[7]
                    ttEstItem.cItemDescription1 = ipcData[8]
                    ttEstItem.cItemDescription2 = ipcData[9]
                    ttEstItem.cStyleID          = ipcData[10]
                    ttEstItem.cStyle            = ipcData[11]
                    ttEstItem.lIsSet            = LOGICAL(ipcData[12])
                    ttEstItem.cCustomerName     = ipcData[13]
                    ttEstItem.cCustomerAddress1 = ipcData[14]
                    ttEstItem.cCustomerAddress2 = ipcData[15]
                    ttEstItem.cCustomerAddress3 = ipcData[16]
                    ttEstItem.cShipToName       = ipcData[17]
                    ttEstItem.cShipToAddress1   = ipcData[18]
                    ttEstItem.cShipToAddress2   = ipcData[19]
                    ttEstItem.cShipToAddress3   = ipcData[20]
                    ttEstItem.cSalesgroupName   = ipcData[21]
                    ttEstItem.cCustomerID       = ipcData[22]
                    ttEstItem.cShipToID         = ipcData[23]
                    ttEstItem.cSalesgroupID     = ipcData[24]
                    ttEstItem.cColor            = ipcData[25]
                    .
                    
            END.
    END CASE.
END PROCEDURE.

PROCEDURE pBuildSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details for a given header and creates summary
              records for each
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.

    FOR EACH ttEstCostDetail NO-LOCK 
        WHERE ttEstCostDetail.estHeaderID EQ ipbf-ttEstHeader.estHeaderID, 
        FIRST ttEstCostCategory NO-LOCK 
        WHERE ttEstCostCategory.estCostCategoryID EQ ttEstCostDetail.estCostCategoryID,
        FIRST ttEstForm NO-LOCK 
        WHERE ttEstForm.estFormID EQ ttEstCostDetail.estFormID    
        :
        FIND FIRST ttEstCostSummary EXCLUSIVE-LOCK
            WHERE ttEstCostSummary.cScope EQ ttEstCostDetail.estFormID
            AND ttEstCostSummary.estCostGroupID EQ ttEstCostCategory.estCostGroupID
            NO-ERROR.
        IF NOT AVAILABLE ttEstCostSummary THEN 
        DO:
            CREATE ttEstCostSummary.
            ASSIGN 
                ttEstCostSummary.estCostSummaryID = fGetNextID()
                ttEstCostSummary.estCostGroupID   = ttEstCostCategory.estCostGroupID
                ttEstCostSummary.cScope           = ttEstCostDetail.estFormID
                .
        END.
        ASSIGN 
            ttEstCostSummary.dCostTotal = ttEstCostSummary.dCostTotal + ttEstCostDetail.dCost
            ttEstCostSummary.dCostPerM  = ttEstCostSummary.dCostPerM + ttEstCostDetail.dCost / (ttEstForm.dQtyFGOnForm / 1000)
            .
    END.        


END PROCEDURE.


PROCEDURE pBuildTestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the temptables to test with
     Notes:
    ------------------------------------------------------------------------------*/
    
    RUN pLoadData("EstHeader").
    RUN pLoadData("CostGroupLevel").
    RUN pLoadData("CostGroup").
    RUN pLoadData("CostCategory").
    RUN pLoadData("Item").
    
    FOR EACH ttEstHeader NO-LOCK:        
        CREATE ttEstForm.
        ASSIGN 
            ttEstForm.estFormID                 = fGetNextID()
            ttEstForm.estHeaderID               = ttEstHeader.estHeaderID
            ttEstForm.iFormNo                   = 1
            ttEstForm.iNumOutLength             = 1
            ttEstForm.iNumOutWidth              = 2
            ttEstForm.iNumOut                   = MAX(ttEstForm.iNumOutLength, 1) * MAX(ttEstForm.iNumOutWidth, 1) * MAX(ttEstForm.iNumOutDepth, 1)
            ttEstForm.dGrossWidth               = 48 
            ttEstForm.dGrossLength              = 74 
            ttEstForm.dGrossArea                = ttEstForm.dGrossWidth * ttEstForm.dGrossLength / 144
            ttEstForm.dNetWidth                 = ttEstForm.dGrossWidth / ttEstForm.iNumOutWidth - 0.25
            ttEstForm.dNetLength                = ttEstForm.dGrossLength / ttEstForm.iNumOutLength - 0.25
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
            ttEstBlank.estBlankID                     = fGetNextID()
            ttEstBlank.estFormID                      = ttEstForm.estFormID
            ttEstBlank.estItemID                      = ttEstItem.estItemID
            ttEstBlank.estHeaderID                    = ttEstForm.estHeaderID
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
            WHERE item.company EQ  ttEstHeader.company
            AND item.i-no EQ "200 C"
            NO-ERROR.
        CREATE ttEstMaterial.
        ASSIGN 
            ttEstMaterial.estMaterialID        = fGetNextID()
            ttEstMaterial.estFormID            = ttEstForm.estFormID
            ttEstMaterial.estBlankID           = ttEstBlank.estBlankID
            ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
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
            WHERE item.company EQ  ttEstHeader.company
            AND item.i-no EQ "GLUE"
            NO-ERROR.
        CREATE ttEstMaterial.
        ASSIGN 
            ttEstMaterial.estMaterialID        = fGetNextID()
            ttEstMaterial.estFormID            = ttEstForm.estFormID
            ttEstMaterial.estBlankID           = ttEstBlank.estBlankID
            ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
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
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "1"
            ttEstOperation.cOperationName         = "Laminate Litho"
            ttEstOperation.dHoursSetup            = 1.5
            ttEstOperation.dHoursRun              = 2.81
            ttEstOperation.dSpeed                 = 1000
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 125
            ttEstOperation.dCostPerHourFORun      = 100
            ttEstOperation.dCostPerHourVORun      = 100
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "2"
            ttEstOperation.cOperationName         = "Bobst (Quality)"
            ttEstOperation.dHoursSetup            = 2
            ttEstOperation.dHoursRun              = 1.77
            ttEstOperation.dSpeed                 = 1500
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 124
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "3"
            ttEstOperation.cOperationName         = "Emmeci Cosmetic"
            ttEstOperation.dHoursSetup            = 3
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 207
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "4"
            ttEstOperation.cOperationName         = " 1 - Additional Labor"
            ttEstOperation.dHoursSetup            = 0
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 30
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "5"
            ttEstOperation.cOperationName         = " 4 - Additional Labor"
            ttEstOperation.dHoursSetup            = 0
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 120
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstForm.
        ASSIGN 
            ttEstForm.estFormID                 = fGetNextID()
            ttEstForm.estHeaderID               = ttEstHeader.estHeaderID
            ttEstForm.iFormNo                   = 2
            ttEstForm.iNumOutLength             = 1
            ttEstForm.iNumOutWidth              = 2
            ttEstForm.iNumOut                   = MAX(ttEstForm.iNumOutLength, 1) * MAX(ttEstForm.iNumOutWidth, 1) * MAX(ttEstForm.iNumOutDepth, 1)
            ttEstForm.dGrossWidth               = 32 
            ttEstForm.dGrossLength              = 54 
            ttEstForm.dGrossArea                = ttEstForm.dGrossWidth * ttEstForm.dGrossLength / 144
            ttEstForm.dNetWidth                 = ttEstForm.dGrossWidth / ttEstForm.iNumOutWidth - 0.25
            ttEstForm.dNetLength                = ttEstForm.dGrossLength / ttEstForm.iNumOutLength - 0.25
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
            ttEstBlank.estBlankID                     = fGetNextID()
            ttEstBlank.estFormID                      = ttEstForm.estFormID
            ttEstBlank.estItemID                      = ttEstItem.estItemID
            ttEstBlank.estHeaderID                    = ttEstForm.estHeaderID
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
            WHERE item.company EQ  ttEstHeader.company
            AND item.i-no EQ "200 C"
            NO-ERROR.
        CREATE ttEstMaterial.
        ASSIGN 
            ttEstMaterial.estMaterialID        = fGetNextID()
            ttEstMaterial.estFormID            = ttEstForm.estFormID
            ttEstMaterial.estBlankID           = ttEstBlank.estBlankID
            ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
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
            WHERE item.company EQ  ttEstHeader.company
            AND item.i-no EQ "GLUE"
            NO-ERROR.
        CREATE ttEstMaterial.
        ASSIGN 
            ttEstMaterial.estMaterialID        = fGetNextID()
            ttEstMaterial.estFormID            = ttEstForm.estFormID
            ttEstMaterial.estBlankID           = ttEstBlank.estBlankID
            ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
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
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "1"
            ttEstOperation.cOperationName         = "Laminate Litho"
            ttEstOperation.dHoursSetup            = 1.5
            ttEstOperation.dHoursRun              = 2.81
            ttEstOperation.dSpeed                 = 1000
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 125
            ttEstOperation.dCostPerHourFORun      = 100
            ttEstOperation.dCostPerHourVORun      = 100
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "2"
            ttEstOperation.cOperationName         = "Bobst (Quality)"
            ttEstOperation.dHoursSetup            = 2
            ttEstOperation.dHoursRun              = 1.77
            ttEstOperation.dSpeed                 = 1500
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 124
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "3"
            ttEstOperation.cOperationName         = "Emmeci Cosmetic"
            ttEstOperation.dHoursSetup            = 3
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 207
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "4"
            ttEstOperation.cOperationName         = " 1 - Additional Labor"
            ttEstOperation.dHoursSetup            = 0
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 30
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).
            
        CREATE ttEstOperation.
        ASSIGN 
            ttEstOperation.estOperationID         = fGetNextID()
            ttEstOperation.estFormID              = ttEstForm.estFormID
            ttEstOperation.estBlankID             = ttEstBlank.estBlankID
            ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
            ttEstOperation.cOperationID           = "5"
            ttEstOperation.cOperationName         = " 4 - Additional Labor"
            ttEstOperation.dHoursSetup            = 0
            ttEstOperation.dHoursRun              = 12.5
            ttEstOperation.dSpeed                 = 1200
            ttEstOperation.dCrewSizeRun           = 1
            ttEstOperation.dCrewSizeSetup         = 1
            ttEstOperation.dCostPerManHourDLRun   = 120
            ttEstOperation.dCostPerHourFORun      = 0
            ttEstOperation.dCostPerHourVORun      = 0
            ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
            ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
            ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
            .
        RUN pCalcEstOperation(BUFFER ttEstOperation).

    END.
        
END PROCEDURE.

PROCEDURE pBuildTestData2 PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the temptables to test with
     Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE iOutTotal AS INTEGER NO-UNDO.
    
    RUN pLoadData("EstHeader").
    RUN pLoadData("CostGroupLevel").
    RUN pLoadData("CostGroup").
    RUN pLoadData("CostCategory").
    
    FOR EACH ttEstHeader NO-LOCK:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ ttEstHeader.company
            AND est.est-no EQ FILL(" ",8 - LENGTH(ttEstHeader.cEstNo)) + ttEstHeader.cEstNo
            NO-ERROR.
        IF NOT AVAILABLE est THEN RETURN.
        /*Build Items*/
        FOR EACH eb NO-LOCK 
            WHERE eb.company EQ est.company 
            AND eb.est-no EQ est.est-no: 
            FIND FIRST ttEstItem NO-LOCK
                WHERE ttEstItem.estHeaderID = ttEstHeader.estHeaderID
                AND ttEstItem.cCustomerPart = eb.part-no
                NO-ERROR.
            IF NOT AVAILABLE ttEstItem THEN 
            DO:
                CREATE ttEstItem.
                ASSIGN 
                    ttEstItem.rec_key           = fGetNextRecKey()
                    ttEstItem.estItemID         = fGetNextID()
                    ttEstItem.estHeaderID       = ttEstHeader.estHeaderID
                    ttEstItem.cCustomerPart     = eb.part-no
                    ttEstItem.dQtyPerParent     = eb.quantityPerSet
                    ttEstItem.dQtyRequired      = ttEstHeader.dQtyMaster * ttEstItem.dQtyPerParent
                    ttEstItem.dQtyPerParent     = ttEstItem.dQtyRequired
                    ttEstItem.cColor            = eb.i-coldscr
                    ttEstItem.cCustomerID       = eb.cust-no
                    ttEstItem.cShipToID         = eb.ship-id
                    ttEstItem.cItemName         = eb.part-dscr1
                    ttEstItem.cItemDescription1 = eb.part-dscr2
                    ttEstItem.cSalesgroupID     = eb.sman
                    ttEstItem.cStyleID          = eb.style
                    ttEstItem.lIsSet            = eb.is-a-set
                    .
                FIND FIRST cust NO-LOCK 
                    WHERE cust.company EQ eb.company
                    AND cust.cust-no EQ eb.cust-no
                    NO-ERROR.
                IF AVAILABLE cust THEN 
                    ASSIGN 
                        ttEstItem.cCustomerName     = cust.name
                        ttEstItem.cCustomerAddress1 = cust.addr[1]
                        ttEstItem.cCustomerAddress2 = cust.addr[2]
                        ttEstItem.cCustomerAddress3 = cust.city + ", " + cust.state + " " + cust.zip
                        .
                FIND FIRST shipto NO-LOCK 
                    WHERE shipto.company EQ eb.company
                    AND shipto.cust-no EQ eb.cust-no
                    AND shipto.ship-id EQ eb.ship-id
                    NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN 
                        ttEstItem.cShipToName     = shipto.ship-name
                        ttEstItem.cShipToAddress1 = shipto.ship-addr[1]
                        ttEstItem.cShipToAddress2 = shipto.ship-addr[2]
                        ttEstItem.cShipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
                        .
                FIND FIRST sman NO-LOCK 
                    WHERE sman.company EQ eb.company
                    AND sman.sman EQ eb.sman
                    NO-ERROR.
                IF AVAILABLE sman THEN 
                    ASSIGN 
                        ttEstItem.cSalesgroupName = sman.sname        
                        .
                FIND FIRST style NO-LOCK 
                    WHERE style.company EQ eb.company
                    AND style.style EQ eb.style
                    NO-ERROR.
                IF AVAILABLE style THEN 
                    ASSIGN 
                        ttEstItem.cStyle = style.dscr
                        .
            END. /*Create ttEstitem*/
               
        END. /*Build EstItems*/
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no:
            CREATE ttEstForm.
            ASSIGN 
                ttEstForm.rec_key                         = fGetNextRecKey()
                ttEstForm.estFormID                       = fGetNextID()
                ttEstForm.estHeaderID                     = ttEstHeader.estHeaderID
                ttEstForm.iFormNo                         = ef.form-no
                ttEstForm.iNumOutLength                   = ef.n-out-l
                ttEstForm.iNumOutWidth                    = ef.n-out
                ttEstForm.iNumOutDepth                    = ef.n-out-d
                ttEstForm.iNumOut                         = MAX(ttEstForm.iNumOutLength, 1) * MAX(ttEstForm.iNumOutWidth, 1) * MAX(ttEstForm.iNumOutDepth, 1)
                ttEstForm.dGrossWidth                     = ef.gsh-wid 
                ttEstForm.dGrossLength                    = ef.gsh-len
                ttEstForm.dGrossDepth                     = ef.gsh-dep 
                ttEstForm.dNetWidth                       = ef.nsh-wid
                ttEstForm.dNetLength                      = ef.nsh-len
                ttEstform.dNetDepth                       = ef.nsh-dep
            
                /*Refactor- Hard-codes*/
                ttEstForm.cUOMDimension                   = "In"
                ttEstForm.cUOMArea                        = "SF"
                ttEstForm.cUOMWeightDie                   = "LB/MSHT"
                ttEstForm.cUOMWeightNet                   = "LB/MSHT"
                ttEstForm.cUOMWeightGross                 = "LB/MSHT"
                ttEstForm.cUOMGrossQtyRequiredTotalWeight = "LBS"
                ttEstForm.cUOMGrossQtyRequiredTotalArea   = "MSF"
            
            
                /*Refactor - Source?*/
                ttEstForm.dDieWidth                       = ttEstForm.dNetWidth - .625
                ttEstForm.dDieLength                      = ttEstForm.dNetLength - .625
            
                /*Refactor - Formulas/Conversions - don't assume SF and inches*/
                ttEstForm.dGrossArea                      = ttEstForm.dGrossWidth * ttEstForm.dGrossLength / 144
                ttEstForm.dNetArea                        = ttEstForm.dNetWidth * ttEstForm.dNetLength / 144
                ttEstForm.dDieArea                        = ttEstForm.dDieWidth * ttEstForm.dDieLength / 144
                                    
                /*Refactor - Calculate from Operations*/
                ttEstForm.dGrossQtyRequiredWasteMR        = 20
                ttEstForm.dGrossQtyRequiredWasteRun       = 6
            
            
                /*Refactor - pull from Board mat*/
                ttEstForm.dBasisWeightInLbsPerMSF         = 123 
            
                ttEstForm.dWeightDie                      = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dDieArea 
                ttEstForm.dWeightNet                      = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dNetArea 
                ttEstForm.dWeightGross                    = ttEstForm.dBasisWeightInLbsPerMSF * ttEstForm.dGrossArea
            
                /*Refactor - Calculate Combo products*/
                ttEstForm.dQtyFGOnForm                    = ttEstHeader.dQtyMaster
                .
            iOutTotal = 0.
            FOR EACH eb NO-LOCK 
                OF ef:
                CREATE ttEstBlank.
                ASSIGN 
                    ttEstBlank.rec_key       = fGetNextRecKey()
                    ttEstBlank.estBlankID    = fGetNextID()
                    ttEstBlank.estFormID     = ttEstForm.estFormID
                    ttEstBlank.estHeaderID   = ttEstForm.estHeaderID
                    ttEstBlank.iBlankNo      = eb.blank-no
                    ttEstBlank.iNumOutLength = eb.num-len
                    ttEstBlank.iNumOutWidth  = eb.num-wid
                    ttEstBlank.iNumOutDepth  = eb.num-dep
                    ttEstBlank.iNumOut       = MAX(ttEstBlank.iNumOutWidth, 1) * MAX(ttEstBlank.iNumOutLength, 1) * MAX(ttEstBlank.iNumOutDepth, 1)
                    iOutTotal                = iOutTotal + ttEstBlank.iNumOut
                    ttEstBlank.dBlankWidth   = eb.t-wid
                    ttEstBlank.dBlankLength  = eb.t-len
                    ttEstBlank.dBlankDepth   = eb.t-dep
                    ttEstBlank.dBlankArea    = eb.t-sqft
                    ttEstBlank.dLength       = eb.len
                    ttEstBlank.dWidth        = eb.wid
                    ttEstBlank.dDepth        = eb.dep
                    
                    /*Refactor - apply area UOM conversion*/
                    ttEstBlank.dWeight       = ttEstForm.dBasisWeightInLbsPerMSF * ttEstBlank.dBlankArea * 144 / 1000 
                    
                    /*Refactor - Hardcoded*/
                    ttEstBlank.cUOMArea      = "SF"
                    ttEstBlank.cUOMDimension = "In"
                    ttEstBlank.cUOMWeight    = "LB/M"
                    .
                    
                FIND FIRST ttEstItem EXCLUSIVE-LOCK 
                    WHERE ttEstItem.estHeaderID EQ ttEstBlank.estHeaderID
                    AND ttEstItem.cCustomerPart EQ eb.part-no
                    NO-ERROR 
                    .
                IF AVAILABLE ttEstItem THEN 
                DO:
                    ASSIGN 
                        ttEstBlank.estItemID = ttEstItem.estItemID
                        ttEstItem.cSize      = TRIM(STRING(ttEstBlank.dLength,">>>9.99")) + " x " + TRIM(STRING(ttEstBlank.dWidth,">>>9.99"))
                        .
                    IF ttEstBlank.dDepth NE 0 THEN 
                        ttEstItem.cSize = ttEstItem.cSize + " x " + TRIM(STRING(ttEstBlank.dDepth,">>>9.99")).
                END.
                FIND FIRST ITEM NO-LOCK 
                    WHERE item.company EQ  eb.company
                    AND item.i-no EQ eb.adhesive
                    NO-ERROR.
                CREATE ttEstMaterial.
                ASSIGN 
                    ttEstMaterial.rec_key              = fGetNextRecKey()
                    ttEstMaterial.estMaterialID        = fGetNextID()
                    ttEstMaterial.estFormID            = ttEstForm.estFormID
                    ttEstMaterial.estBlankID           = ttEstBlank.estBlankID
                    ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
                    ttEstMaterial.iFormNo              = eb.form-no
                    ttEstMaterial.iBlankNo             = eb.blank-no
                    ttEstMaterial.cItemID              = item.i-no 
                    ttEstMaterial.cItemName            = item.i-name 
                    ttEstMaterial.cQtyUOM              = ITEM.cons-uom
            
                    /*Refactor - pull from vend-cost table*/
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
            END. /*Each eb of ef*/
            /*Refactor - Need True calculations*/
            ASSIGN 
                ttEstForm.dGrossQtyRequiredNoWaste     = ttEstHeader.dQtyMaster / (iOutTotal * ttEstForm.iNumOut)
                ttEstForm.dGrossQtyRequiredTotal       = ttEstForm.dGrossQtyRequiredNoWaste + ttEstForm.dGrossQtyRequiredWasteMR + ttEstForm.dGrossQtyRequiredWasteRun
                ttEstForm.dGrossQtyRequiredTotalArea   = ttEstForm.dGrossQtyRequiredTotal * ttEstForm.dGrossArea / 1000
                ttEstForm.dGrossQtyRequiredTotalWeight = ttEstForm.dGrossQtyRequiredTotalArea * ttEstForm.dBasisWeightInLbsPerMSF
                .
            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ  ef.company
                AND item.i-no EQ ef.board
                NO-ERROR.
            IF AVAILABLE ITEM THEN 
            DO:
                CREATE ttEstMaterial.
                ASSIGN 
                    ttEstMaterial.rec_key              = fGetNextRecKey()
                    ttEstMaterial.estMaterialID        = fGetNextID()
                    ttEstMaterial.estFormID            = ttEstForm.estFormID
                    ttEstMaterial.estHeaderID          = ttEstForm.estHeaderID
                    ttEstMaterial.iFormNo              = ef.form-no
                    ttEstMaterial.iBlankNo             = 0
                    ttEstMaterial.cItemID              = item.i-no 
                    ttEstMaterial.cItemName            = item.i-name 
                    ttEstMaterial.lIsPrimarySubstrate  = YES
                    ttEstMaterial.lAddToWeightFG       = YES
                    ttEstMaterial.lAddToWeightTare     = NO
                    ttEstMaterial.cQtyUOM              = ITEM.cons-uom
                    
                    /*Refactor - pull from vend-cost table*/
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
                    ttEstMaterial.dCostTotal           = ttEstMaterial.dCostTotalNoWaste + ttEstMaterial.dCostTotalWasteMR + ttEstMaterial.dCostTotalWasteRu   
                    .
            END.
            FOR EACH est-op NO-LOCK 
                WHERE est-op.company EQ est.company
                AND est-op.est-no EQ ef.est-no
                AND est-op.s-num EQ ef.form-no
                AND est-op.line LT 500
                :
                CREATE ttEstOperation.
                ASSIGN 
                    ttEstOperation.rec_key                = fGetNextRecKey()
                    ttEstOperation.estOperationID         = fGetNextID()
                    ttEstOperation.estFormID              = ttEstForm.estFormID
                    ttEstoperation.estHeaderID            = ttEstForm.estHeaderID
                    /*refactor - pull from est-op b-num*/
                    ttEstOperation.estBlankID             = "1"
            
                    ttEstOperation.iFormNo                = est-op.s-num
                    ttEstOperation.iBlankNo               = est-op.b-num
                    ttEstOperation.estHeaderID            = ttEstForm.estHeaderID
                    ttEstOperation.cOperationID           = est-op.m-code
            
                    ttEstOperation.dHoursSetup            = 1.5
                    ttEstOperation.dHoursRun              = 2.81
                    ttEstOperation.dSpeed                 = 1000
            
                    ttEstOperation.dCrewSizeRun           = 1
                    ttEstOperation.dCrewSizeSetup         = 1
                    ttEstOperation.dCostPerManHourDLRun   = 125
                    ttEstOperation.dCostPerHourFORun      = 100
                    ttEstOperation.dCostPerHourVORun      = 100
                    ttEstOperation.dCostPerManHourDLSetup = ttEstOperation.dCostPerManHourDLRun
                    ttEstOperation.dCostPerHourFOSetup    = ttEstOperation.dCostPerHourFORun
                    ttEstOperation.dCostPerHourVOSetup    = ttEstOperation.dCostPerHourVORun
                    .
                FIND FIRST mach NO-LOCK 
                    WHERE mach.company EQ est-op.company
                    AND mach.m-code EQ est-op.m-code
                    NO-ERROR.
                IF AVAILABLE mach THEN 
                    ASSIGN 
                        ttEstOperation.cOperationName     = mach.m-dscr
                        ttEstOperation.cOperationFeedType = mach.p-type
                        ttEstOperation.cDepartmentID      = mach.dept[1]
                        ttEstOperation.cAlt1DepartmentID  = mach.dept[2]
                        ttEstOperation.cAlt2DepartmentID  = mach.dept[3]
                        ttEstOperation.cAlt3DepartmentID  = mach.dept[4]
                        .
                RUN pCalcEstOperation(BUFFER ttEstOperation).
            END.    
        END.  /*Each ef of est*/  
             
    END. /*each ttEstHeader*/
        
END PROCEDURE.


PROCEDURE pCalcEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a ttEstOperation buffer, calculate simple calculated fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    
    DEFINE VARIABLE lApplyMinChargeOnRun   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lApplyMinChargeOnSetup AS LOGICAL NO-UNDO.
    
    ASSIGN 
        ipbf-ttEstOperation.dCostPerHourTotalRun   = ipbf-ttEstOperation.dCostPerManHourDLRun * ipbf-ttEstOperation.dCrewSizeRun + 
                                                     ipbf-ttEstOperation.dCostPerHourFORun + ipbf-ttEstOperation.dCostPerHourVORun
        ipbf-ttEstOperation.dCostPerHourTotalSetup = ipbf-ttEstOperation.dCostPerManHourDLSetup * ipbf-ttEstOperation.dCrewSizeSetup + 
                                                     ipbf-ttEstOperation.dCostPerHourFOSetup + 
                                                     ipbf-ttEstOperation.dCostPerHourVOSetup
        ipbf-ttEstOperation.dCostTotalDLSetup      = ipbf-ttEstOperation.dHoursSetup * ipbf-ttEstOperation.dCrewSizeSetup * ipbf-ttEstOperation.dCostPerManHourDLSetup
        ipbf-ttEstOperation.dCostTotalVOSetup      = ipbf-ttEstOperation.dHoursSetup * ipbf-ttEstOperation.dCostPerHourVOSetup
        ipbf-ttEstOperation.dCostTotalFOSetup      = ipbf-ttEstOperation.dHoursSetup * ipbf-ttEstOperation.dCostPerHourFOSetup
        ipbf-ttEstOperation.dCostTotalDLRun        = ipbf-ttEstOperation.dHoursRun * ipbf-ttEstOperation.dCrewSizeRun * ipbf-ttEstOperation.dCostPerManHourDLRun
        ipbf-ttEstOperation.dCostTotalVORun        = ipbf-ttEstOperation.dHoursRun * ipbf-ttEstOperation.dCostPerHourVORun
        ipbf-ttEstOperation.dCostTotalFORun        = ipbf-ttEstOperation.dHoursRun * ipbf-ttEstOperation.dCostPerHourFORun
        ipbf-ttEstOperation.dCostTotalSetup        = ipbf-ttEstOperation.dHoursSetup * ipbf-ttEstOperation.dCostPerHourTotalSetup
        ipbf-ttEstOperation.dCostTotalRun          = ipbf-ttEstOperation.dHoursRun * ipbf-ttEstOperation.dCostPerHourTotalRun
        ipbf-ttEstOperation.dCostTotal             = ipbf-ttEstOperation.dCostTotalRun + ipbf-ttEstOperation.dCostTotalSetup 
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

FUNCTION fGetNextID RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    giRecKey = giRecKey + 1.
    RETURN STRING(giRecKey).


		
END FUNCTION.

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	


    giRecKey = giRecKey + 1.
    RETURN STRING(giRecKey).

		
END FUNCTION.
