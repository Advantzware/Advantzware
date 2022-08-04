  /*--------------------------------------------------------------------------
    File        : EstimateCalcProcs.p
    
    Purpose     : New code for EstimateCalcProcs.p for Performace improvement. It being updated to use Dataset/temp-tables instead of DB tables
    
    Syntax      :      
    
    Description :        

    Author(s)   : BV
                  
    
    Created     : Tue Feb 08 17:17:51 EST 2022
    
    Notes       : sakshi.singh- Refactor for performance optimization. 
                  Dataset will be written into DB in the end of processing
                  Original copy of code is available in EstimateCalcProcsAlt.p
    ------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{est/ttEstCost.i}
{est/ttEstSysConfig.i}
{est\RecostBoardEst.i}
{est/ttEstimateCalc.i}
   
DEFINE VARIABLE giID                                  AS INT64 NO-UNDO.
DEFINE VARIABLE ghFreight                             AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghFormula                             AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghOperation                           AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghRecostBoardEst                      AS HANDLE    NO-UNDO.

DEFINE VARIABLE gcSourceTypeOperation                 AS CHARACTER NO-UNDO INITIAL "Operation".
DEFINE VARIABLE gcSourceTypeMaterial                  AS CHARACTER NO-UNDO INITIAL "Material".
DEFINE VARIABLE gcSourceTypeMisc                      AS CHARACTER NO-UNDO INITIAL "Miscellaneous".
DEFINE VARIABLE gcSourceTypeNonFactory                AS CHARACTER NO-UNDO INITIAL "NonFactory".
DEFINE VARIABLE gcSourceTypeProfit                    AS CHARACTER NO-UNDO INITIAL "Profit".


DEFINE VARIABLE gcDeptsForPrinters                    AS CHARACTER NO-UNDO INITIAL "PR".
DEFINE VARIABLE gcDeptsForGluers                      AS CHARACTER NO-UNDO INITIAL "GL,QS".
DEFINE VARIABLE gcDeptsForLeafers                     AS CHARACTER NO-UNDO INITIAL "WN,WS,FB,FS".
DEFINE VARIABLE gcDeptsForSheeters                    AS CHARACTER NO-UNDO INITIAL "RC,RS,CR".
DEFINE VARIABLE gcDeptsForCoaters                     AS CHARACTER NO-UNDO INITIAL "PR,CT".
DEFINE VARIABLE gcDeptsForCorrugators                  AS CHARACTER NO-UNDO INITIAL "CR,LM".

DEFINE VARIABLE gcIndustryFolding                     AS CHARACTER NO-UNDO INITIAL "Folding".
DEFINE VARIABLE gcIndustryCorrugated                  AS CHARACTER NO-UNDO INITIAL "Corrugated".

DEFINE VARIABLE giErrorWarning                        AS INTEGER   NO-UNDO INITIAL 3.
DEFINE VARIABLE giErrorImportant                      AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE giErrorCritical                       AS INTEGER   NO-UNDO INITIAL 1.

DEFINE VARIABLE gcDefaultWeightUOM                    AS CHARACTER NO-UNDO INITIAL "LB".
DEFINE VARIABLE gcDefaultAreaUOM                      AS CHARACTER NO-UNDO INITIAL "SQIN".
DEFINE VARIABLE gcDefaultBasisWeightUOM               AS CHARACTER NO-UNDO INITIAL "LB/MSF".

DEFINE VARIABLE gdWindowDimOverlap                    AS DECIMAL   NO-UNDO INITIAL 0.5.

DEFINE VARIABLE gcMaterialTypeCalcDefault             AS CHARACTER NO-UNDO INITIAL "ByDefault".

/*Settings Globals*/
DEFINE VARIABLE gcPrepRoundTo                         AS CHARACTER NO-UNDO.  /*CEPREP - char val - potentially deprecate*/
DEFINE VARIABLE gcPrepMarkupOrMargin                  AS CHARACTER NO-UNDO.  /*CEPrepPrice - char val*/
DEFINE VARIABLE gdMaterialMarkup                      AS DECIMAL   NO-UNDO.    /*CEMatl - Dec val*/
DEFINE VARIABLE gcMarginMatrixLookup                  AS CHARACTER NO-UNDO.    /*CEMatl - Dec val*/
DEFINE VARIABLE glOpRatesSeparate                     AS LOGICAL   NO-UNDO INITIAL YES.    /*CEOpRates - log val*/

DEFINE VARIABLE glUsePlateChangesAsColorForSetupWaste AS LOGICAL   NO-UNDO INITIAL NO.  /*Defect in EstOperation Calc of applying the MR Waste Sheets Per Color?*/
DEFINE VARIABLE glVendItemCost                        AS LOGICAL   NO-UNDO INITIAL YES.    /*VendItemCost - log val*/
DEFINE VARIABLE glApplyOperationMinimumCharge         AS LOGICAL   NO-UNDO. /*CEPRICE Logical*/
DEFINE VARIABLE glApplyOperationMinimumChargeRunOnly  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glRoundPriceToDollar                  AS LOGICAL   NO-UNDO.  /*CEROUND*/
DEFINE VARIABLE glPromptForMaterialVendor             AS LOGICAL   NO-UNDO.  /*CEVENDOR*/
DEFINE VARIABLE glUseBlankVendor                      AS LOGICAL   NO-UNDO.  /*CEVendorDefault*/
DEFINE VARIABLE glCalcSourceForMachineStd             AS LOGICAL   NO-UNDO.  /*CEOpStandards*/
DEFINE VARIABLE glUseGrossWeight                      AS LOGICAL   NO-UNDO.  /*CEShipWeight*/
DEFINE VARIABLE glCalcFoamCostFromBlank               AS LOGICAL   NO-UNDO.  /*FOAMCOST*/
DEFINE VARIABLE gcCECostSourceLookup                  AS CHARACTER NO-UNDO.  /*CECostSource*/
DEFINE VARIABLE giPromptForErrorLevel                 AS INTEGER   NO-UNDO.  /*CEShowErrorsAndWarnings*/
DEFINE VARIABLE glAutoRecostBoard                     AS LOGICAL   NO-UNDO.  /*CEAutoRecostBoard*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64,
    ipiEstFormID AS INT64,
    ipiBlankNo AS INTEGER) FORWARD.

FUNCTION fGetMatTypeCalc RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
     ipcMaterialType AS CHARACTER) FORWARD.

FUNCTION fGetMSF RETURNS DECIMAL PRIVATE
    (ipdArea AS DECIMAL,
     ipcUOM AS CHARACTER) FORWARD.

FUNCTION fGetNetSheetOut RETURNS INTEGER PRIVATE
    (ipiEstCostOperationID AS INT64,
    ipiDefaultOut AS INTEGER,
    INPUT ipdEstOPQty AS DECIMAL) FORWARD.

FUNCTION fGetNextID RETURNS INT64 PRIVATE
    ( ipcTableName AS CHARACTER ) FORWARD.

FUNCTION fGetPartCount RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER,
     ipcEstimateID AS CHARACTER) FORWARD.

FUNCTION fGetProfit RETURNS DECIMAL PRIVATE
    (ipdCost AS DECIMAL,
    ipdProfitPercent AS DECIMAL,
    ipcPercentType AS CHARACTER) FORWARD.

FUNCTION fGetQuantityPerSet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD.

FUNCTION fIsAdderMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsBoardMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER,
    ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fIsGlueMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsInkMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsLeafMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsPackingMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsWaxMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fIsWindowMaterial RETURNS LOGICAL PRIVATE
    (ipcMaterialTypeID AS CHARACTER) FORWARD.

FUNCTION fRoundUP RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL) FORWARD.

FUNCTION fIsComboType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fIsFoamStyle RETURNS LOGICAL PRIVATE 
    (ipcCompany AS CHARACTER, ipcEstNo AS CHARACTER, INPUT ipiFormNo AS INTEGER) FORWARD.

FUNCTION fIsMiscType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fIsSetType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fIsSingleType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fIsWoodType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
RUN system\FreightProcs.p PERSISTENT SET ghFreight.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghFreight).
RUN system\FormulaProcs.p PERSISTENT SET ghFormula.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghFormula).

/* **********************  Internal Procedures  *********************** */

PROCEDURE CalculateEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Primary Public Procedure for calculating the estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iEstCostHeaderID AS INT64 NO-UNDO.
    
    RUN pCalcEstimate(ipcCompany, ipcEstimateNo, "", 0, 0, iplPurge, NO, OUTPUT iEstCostHeaderID).
    
END PROCEDURE.

PROCEDURE CalculateEstimateWithPrompts:
    /*------------------------------------------------------------------------------
    Purpose:  Public Procedure that calculates estimate and may include UI prompts
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iEstCostHeaderID AS INT64 NO-UNDO.
    
    RUN pCalcEstimate(ipcCompany, ipcEstimateNo, "", 0, 0, iplPurge, YES, OUTPUT iEstCostHeaderID).
    
END PROCEDURE.

PROCEDURE CalculateJob:
    /*------------------------------------------------------------------------------
        Purpose: Primary Public Procedure for calculating the estimate
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantity AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INT64 NO-UNDO.

    RUN pCalcEstimate(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2, ipiQuantity, iplPurge, NO, OUTPUT opiEstCostHeaderID).
    
END PROCEDURE.

PROCEDURE CalculateJobWithPrompts:
    /*------------------------------------------------------------------------------
        Purpose: Primary Public Procedure for calculating the estimate
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantity AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INT64 NO-UNDO.

    RUN pCalcEstimate(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2, ipiQuantity, iplPurge, YES, OUTPUT opiEstCostHeaderID).
    
END PROCEDURE.

PROCEDURE ChangeSellPrice:
    /*------------------------------------------------------------------------------
     Purpose: Given a probe, check to see if sell prices have changed and apply that 
     to the header, forms and items and recalculate commissions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriProbe AS ROWID NO-UNDO.
    
    DEFINE VARIABLE dQtyInM         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPriceDiffRatio AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dNewPrice       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dProfitChange   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCommission     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iEstCostHeaderID AS INT64  NO-UNDO.
    
    FIND probe NO-LOCK 
        WHERE ROWID(probe) EQ ipriProbe
        NO-ERROR.
    IF AVAILABLE probe THEN
        FIND FIRST EstCostHeader NO-LOCK 
            WHERE EstCostHeader.estCostHeaderID EQ INT64(probe.spare-char-2)
            NO-ERROR.
    IF AVAILABLE EstCostHeader THEN 
    DO: 
        RUN pSetGlobalSettings(EstCostHeader.company). 
        RUN pBuildSystemData(EstCostHeader.company). 
        ASSIGN iEstCostHeaderID = EstCostHeader.estCostHeaderID.
    END.    
    /* Copy DB record to Temp-Table */
    RUN pCopyDBToTempTables(iEstCostHeaderID).
    
    FIND FIRST ttEstCostHeader NO-LOCK 
         WHERE ttEstCostHeader.estCostHeaderID EQ iEstCostHeaderID. 
    IF AVAILABLE ttEstCostHeader THEN  
    DO:    
        ASSIGN 
            dQtyInM         = ttEstCostHeader.quantityMaster / 1000
            dPriceDiffRatio = ROUND(probe.sell-price,2) / ROUND(ttEstCostHeader.sellPrice / dQtyInM, 2)
            .
        IF dPriceDiffRatio NE 1 THEN 
        DO:            
            /*Remove all existing estCostDetails for commission and profit*/
            RUN pPurgeCostDetail(ttEstCostHeader.estCostHeaderID, "commission").
            RUN pPurgeCostDetail(ttEstCostHeader.estCostHeaderID, "pProfit").
            /*Reset the summary totals on the header, form, and items*/
            RUN pResetCostTotals(ttEstCostHeader.estCostHeaderID).          
            FOR EACH ttEstCostForm NO-LOCK
                WHERE ttEstCostForm.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID,
                FIRST ttEstCostBlank NO-LOCK 
                WHERE ttEstCostBlank.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID
                  AND ttEstCostBlank.estCostFormID EQ ttEstCostForm.estCostFormID,
                FIRST ttEstCostItem NO-LOCK 
                WHERE ttEstCostItem.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID
                  AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID:
                
                /*Calculate new Price for form and commisson*/
                ASSIGN 
                    dNewPrice   = MAXIMUM (ROUND(ttEstCostForm.sellPrice * dPriceDiffRatio, 2),ROUND(ttEstCostForm.sellPrice * dPriceDiffRatio, 0))
                    dCommission = dNewPrice * ttEstCostItem.commissionPct / 100
                    .   
                
                /*Recalculate Totals for Form*/    
                RUN pCalcCostTotals(ttEstCostHeader.estCostHeaderID, ttEstCostForm.estCostFormID, YES). 
                
                /*Add New Commission Cost*/
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "" , ttEstCostForm.estCostFormID,
                    gcSourceTypeNonFactory,"commission","Commission", dCommission, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER ttEstCostDetail).
                /*Recalculate Cost Totals to get a new TotalFullCost*/
                RUN pCalcCostTotals(ttEstCostHeader.estCostHeaderID, ttEstCostForm.estCostFormID, NO).
                
                /*Add New Profit based on new Full Cost and New Price*/
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "" , ttEstCostForm.estCostFormID,
                    gcSourceTypeNonFactory,"pProfit","Profit After Price Change", dNewPrice - ttEstCostForm.costTotalFull, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER ttEstCostDetail).
                /*Recalculate Sell Price*/
                RUN pCalcCostTotals(ttEstCostHeader.estCostHeaderID, ttEstCostForm.estCostFormID, NO).
            END. 
            
            /*Reset and calculate all cost summaries*/
            RUN pPurgeCostSummary(ttEstCostHeader.estCostHeaderID).
            RUN pBuildCostSummary(ttEstCostHeader.estCostHeaderID).
            RUN pBuildProbe(BUFFER ttEstCostHeader).
            /* To update DB tables with updated Temp-Table */
            RUN pWriteToDBTables(iEstCostHeaderID).
        END.          
    END.

END PROCEDURE.



PROCEDURE pAddCostDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstFormID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstBlankID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstSourceID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcSourceType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstCostDetail FOR ttEstCostDetail.
    
    CREATE opbf-ttEstCostDetail.
    ASSIGN 
        opbf-ttEstCostDetail.estCostHeaderID   = ipiEstHeaderID
        opbf-ttEstCostDetail.estCostFormID     = ipiEstFormID
        opbf-ttEstCostDetail.estCostBlankID    = ipiEstBlankID
        opbf-ttEstCostDetail.company           = ipcCompany
        opbf-ttEstCostDetail.estimateNo        = ipcEstimateNo
        opbf-ttEstCostDetail.sourceID          = ipiEstSourceID
        opbf-ttEstCostDetail.sourceType        = ipcSourceType
        opbf-ttEstCostDetail.estCostCategoryID = ipcEstCostCategoryID
        opbf-ttEstCostDetail.estCostDetailDesc = ipcDescription
        opbf-ttEstCostDetail.costTotal         = ipdCost
        opbf-ttEstCostDetail.profitPercent     = ipdProfitPercent
        opbf-ttEstCostDetail.profitPercentType = "Margin"
        .
        
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostDetail.estCostDetailID, INPUT-OUTPUT opbf-ttEstCostDetail.rec_key, "estCostDetail").

END PROCEDURE.

PROCEDURE pAddCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstCostOperation.estCostHeaderID, ipbf-ttEstCostOperation.estCostFormID, ipbf-ttEstCostOperation.estCostBlankID, ipbf-ttEstCostOperation.estCostOperationID, 
        gcSourceTypeOperation, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-ttEstCostOperation.company, ipbf-ttEstCostOperation.estimateNo, BUFFER bf-ttEstCostDetail).

END PROCEDURE.

PROCEDURE pAddCostDetailForMaterial PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstCostMaterial.estCostHeaderID, ipbf-ttEstCostMaterial.estCostFormID, ipbf-ttEstCostMaterial.estCostBlankID, ipbf-ttEstCostMaterial.estCostMaterialID, 
        gcSourceTypeMaterial, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.estimateNo, BUFFER bf-ttEstCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMisc FOR ttEstCostMisc.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstCostMisc.estCostHeaderID, ipbf-ttEstCostMisc.estCostFormID, ipbf-ttEstCostMisc.estCostBlankID, ipbf-ttEstCostMisc.estCostMiscID, 
        gcSourceTypeMisc, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-ttEstCostMisc.company, ipbf-ttEstCostMisc.estimateNo, BUFFER bf-ttEstCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a scopeID, GroupID, Cost and Quantity, adds or increments a cost summary
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcScopeRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcGroupID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER  ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER  ipdQtyPerM AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcScopeType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostSummary FOR ttEstCostSummary.
    
    FIND FIRST bf-ttEstCostSummary EXCLUSIVE-LOCK
        WHERE bf-ttEstCostSummary.scopeRecKey EQ ipcScopeRecKey
        AND bf-ttEstCostSummary.estCostGroupID EQ ipcGroupID
        NO-ERROR.
    IF NOT AVAILABLE bf-ttEstCostSummary THEN 
    DO:
        CREATE bf-ttEstCostSummary.
        ASSIGN 
            bf-ttEstCostSummary.estCostGroupID  = ipcGroupID
            bf-ttEstCostSummary.scopeRecKey     = ipcScopeRecKey
            bf-ttEstCostSummary.estCostHeaderID = ipiEstCostHeaderID
            bf-ttEstCostSummary.scopeType       = ipcScopeType
            .
            
        RUN pSetKeyFields(INPUT-OUTPUT bf-ttEstCostSummary.estCostSummaryID, INPUT-OUTPUT bf-ttEstCostSummary.rec_key, "estCostSummary").
        
    END.
    bf-ttEstCostSummary.costTotal = bf-ttEstCostSummary.costTotal + ipdCost.
    IF ipdQtyPerM GT 0 THEN 
        bf-ttEstCostSummary.costTotalPerMFinished  = bf-ttEstCostSummary.costTotalPerMFinished + ipdCost / ipdQtyPerM.
                
    RELEASE bf-ttEstCostSummary.
    
END PROCEDURE.


PROCEDURE pAddECostDetailEstMisc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE INPUT  PARAMETER ipcDescription       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdSourceTotalCost   AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
     
    CREATE bf-ttEstCostDetail.
    BUFFER-COPY ipbf-ttEstCostDetail EXCEPT estCostDetailDesc estCostDetailID rec_key costTotal hasBeenProcessed TO bf-ttEstCostDetail.

    RUN pSetKeyFields(INPUT-OUTPUT bf-ttEstCostDetail.estCostDetailID, INPUT-OUTPUT bf-ttEstCostDetail.rec_key, "estCostDetail").
    
    ASSIGN
        bf-ttEstCostDetail.costTotal         = ipdSourceTotalCost
        bf-ttEstCostDetail.estCostDetailDesc = ipcDescription
        bf-ttEstCostDetail.hasBeenProcessed  = FALSE
        .
END PROCEDURE.

PROCEDURE pAddError PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: Registers an error in the ttEstError table for display
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiErrorLevel AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.

    CREATE ttEstError.
    ASSIGN 
        ttEstError.cError          = ipcError
        ttEstError.iErrorLevel     = ipiErrorLevel
        ttEstError.estHeaderID     = ipiEstHeaderID
        ttEstError.iFormNo         = ipiFormNo
        ttEstError.iBlankNo        = ipiBlankNo
        ttEstError.dQuantityMaster = ipdQuantityMaster
        .
END PROCEDURE.

PROCEDURE pAddEstBlank PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: given an eb buffer, create the EstBlank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb                   FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader      FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm        FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostBlank       FOR ttEstCostBlank.

    
    DEFINE           BUFFER bf-ttEstCostItem            FOR ttEstCostItem.
    DEFINE           BUFFER bf-SetHeader-ttEstCostBlank FOR ttEstCostBlank.
    
    CREATE opbf-ttEstCostBlank.
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostBlank.estCostBlankID, INPUT-OUTPUT opbf-ttEstCostBlank.rec_key, "estCostBlank").
    
    ASSIGN 
        opbf-ttEstCostBlank.company                 = ipbf-ttEstCostForm.company
        opbf-ttEstCostBlank.estCostFormID           = ipbf-ttEstCostForm.estCostFormID
        opbf-ttEstCostBlank.estCostHeaderID         = ipbf-ttEstCostForm.estCostHeaderID
        opbf-ttEstCostBlank.estimateNo              = ipbf-eb.est-no
        opbf-ttEstCostBlank.formNo                  = ipbf-eb.form-no
        opbf-ttEstCostBlank.blankNo                 = ipbf-eb.blank-no
        opbf-ttEstCostBlank.numOutLength            = ipbf-eb.num-len
        opbf-ttEstCostBlank.numOutWidth             = ipbf-eb.num-wid
        opbf-ttEstCostBlank.numOutDepth             = ipbf-eb.num-dep
        opbf-ttEstCostBlank.numOut                  = MAX(opbf-ttEstCostBlank.numOutWidth, 1) * MAX(opbf-ttEstCostBlank.numOutLength, 1) * MAX(opbf-ttEstCostBlank.numOutDepth, 1)
        opbf-ttEstCostBlank.blankWidth              = IF ipbf-eb.t-wid EQ 0 THEN ipbf-eb.wid ELSE ipbf-eb.t-wid
        opbf-ttEstCostBlank.blankLength             = IF ipbf-eb.t-len EQ 0 THEN ipbf-eb.len ELSE ipbf-eb.t-len
        opbf-ttEstCostBlank.blankDepth              = IF ipbf-eb.t-dep EQ 0 THEN ipbf-eb.dep ELSE ipbf-eb.t-dep
        opbf-ttEstCostBlank.blankArea               = IF ipbf-eb.t-sqin EQ 0 THEN opbf-ttEstCostBlank.blankLength * opbf-ttEstCostBlank.blankWidth ELSE ipbf-eb.t-sqin
        opbf-ttEstCostBlank.dimLength               = ipbf-eb.len
        opbf-ttEstCostBlank.dimWidth                = ipbf-eb.wid
        opbf-ttEstCostBlank.dimDepth                = ipbf-eb.dep
        opbf-ttEstCostBlank.quantityPerSubUnit      = ipbf-eb.cas-cnt
        opbf-ttEstCostBlank.quantitySubUnitsPerUnit = ipbf-eb.cas-pal
        opbf-ttEstCostBlank.isPurchased             = ipbf-eb.pur-man
                                                        
        /*Refactor - Hardcoded*/
        opbf-ttEstCostBlank.areaUOM                 = "SQIN"
        opbf-ttEstCostBlank.dimUOM                  = "IN"
        opbf-ttEstCostBlank.weightUOM               = gcDefaultWeightUOM
                    
                            
        /*Refactor - Calculate Windowing*/
        opbf-ttEstCostBlank.blankAreaNetWindow      = opbf-ttEstCostBlank.blankArea

        opbf-ttEstCostBlank.weightPerBlank          = ipbf-ttEstCostForm.basisWeight * fGetMSF(opbf-ttEstCostBlank.blankAreaNetWindow, opbf-ttEstCostBlank.areaUOM)
    
        opbf-ttEstCostBlank.quantityPerSet          = fGetQuantityPerSet(BUFFER ipbf-eb)
        opbf-ttEstCostBlank.quantityRequired        = (IF fIsComboType(ipbf-ttEstCostHeader.estType) THEN ipbf-eb.bl-qty ELSE ipbf-ttEstCostHeader.quantityMaster) * opbf-ttEstCostBlank.quantityPerSet 
        opbf-ttEstCostBlank.quantityYielded         = (IF fIsComboType(ipbf-ttEstCostHeader.estType)THEN ipbf-eb.yld-qty ELSE ipbf-ttEstCostHeader.quantityMaster) * opbf-ttEstCostBlank.quantityPerSet
        
        opbf-ttEstCostBlank.priceBasedOnYield       = ipbf-eb.yrprice AND fIsComboType(ipbf-ttEstCostHeader.estType)
        
        .
        
    
    FIND FIRST bf-ttEstCostItem EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostItem.estCostHeaderID EQ opbf-ttEstCostBlank.estCostHeaderID
        AND bf-ttEstCostItem.customerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE bf-ttEstCostItem THEN 
    DO:
        ASSIGN 
            opbf-ttEstCostBlank.estCostItemID = bf-ttEstCostItem.estCostItemID
            bf-ttEstCostItem.sizeDesc       = TRIM(STRING(opbf-ttEstCostBlank.dimLength,">>>9.99")) + " x " + TRIM(STRING(opbf-ttEstCostBlank.dimWidth,">>>9.99"))
            .
        IF opbf-ttEstCostBlank.dimDepth NE 0 THEN 
            bf-ttEstCostItem.sizeDesc = bf-ttEstCostItem.sizeDesc + " x " + TRIM(STRING(opbf-ttEstCostBlank.dimDepth,">>>9.99")).
            
        RELEASE bf-ttEstCostItem.
    END.
    ASSIGN 
        ipbf-ttEstCostForm.numOutBlanksOnNet = ipbf-ttEstCostForm.numOutBlanksOnNet + opbf-ttEstCostBlank.numOut
        ipbf-ttEstCostForm.blankArea         = ipbf-ttEstCostForm.blankArea + opbf-ttEstCostBlank.blankArea * opbf-ttEstCostBlank.numOut
        . 
    
    IF fIsSetType(ipbf-ttEstCostHeader.estType) AND opbf-ttEstCostBlank.formNo NE 0 THEN 
    DO:
        FIND FIRST bf-SetHeader-ttEstCostBlank EXCLUSIVE-LOCK 
            WHERE bf-SetHeader-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND bf-SetHeader-ttEstCostBlank.formNo EQ 0
            NO-ERROR.
        IF AVAILABLE bf-SetHeader-ttEstCostBlank THEN 
            ASSIGN 
                bf-SetHeader-ttEstCostBlank.weightPerBlank = bf-SetHeader-ttEstCostBlank.weightPerBlank + 
                    opbf-ttEstCostBlank.weightPerBlank * opbf-ttEstCostBlank.quantityPerSet.
                    
    END.

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstCostForm FOR ttEstCostForm.
    
    CREATE opbf-ttEstCostForm.
    ASSIGN 
        opbf-ttEstCostForm.estCostHeaderID = ipbf-ttEstCostHeader.estCostHeaderID
        opbf-ttEstCostForm.estimateNo      = ipbf-ttEstCostHeader.estimateNo
        opbf-ttEstCostForm.company         = ipbf-ttEstCostHeader.company
        opbf-ttEstCostForm.formNo          = ipiFormNo
        .
        
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostForm.estCostFormID, INPUT-OUTPUT opbf-ttEstCostForm.rec_key, "estCostForm").
        
END PROCEDURE.

PROCEDURE pAddEstFormFromEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef              FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader. 
    DEFINE PARAMETER BUFFER opbf-ttEstCostForm   FOR ttEstCostForm.

    RUN pAddEstForm(BUFFER ipbf-ttEstCostHeader, ipbf-ef.form-no, BUFFER opbf-ttEstCostForm).
    
    ASSIGN 
        opbf-ttEstCostForm.numOutNetLength                = MAX(ipbf-ef.n-out-l, 1)
        opbf-ttEstCostForm.numOutNetWidth                 = MAX(ipbf-ef.n-out, 1)
        opbf-ttEstCostForm.numOutNetDepth                 = MAX(ipbf-ef.n-out-d, 1)
        opbf-ttEstCostForm.numOutNet                      = opbf-ttEstCostForm.numOutNetLength * opbf-ttEstCostForm.numOutNetWidth * opbf-ttEstCostForm.numOutNetDepth
        opbf-ttEstCostForm.grossWidth                     = ipbf-ef.gsh-wid 
        opbf-ttEstCostForm.grossLength                    = ipbf-ef.gsh-len
        opbf-ttEstCostForm.grossDepth                     = ipbf-ef.gsh-dep 
        opbf-ttEstCostForm.netWidth                       = ipbf-ef.nsh-wid
        opbf-ttEstCostForm.netLength                      = ipbf-ef.nsh-len
        opbf-ttEstCostForm.netDepth                       = ipbf-ef.nsh-dep
        opbf-ttEstCostForm.dieWidth                       = ipbf-ef.trim-w
        opbf-ttEstCostForm.dieLength                      = ipbf-ef.trim-l
        opbf-ttEstCostForm.basisWeight                    = ipbf-ef.weight
        opbf-ttEstCostForm.company                        = ipbf-ef.company     
        opbf-ttEstCostForm.costOverridePerUOM             = ipbf-ef.cost-msh
        opbf-ttEstCostForm.costOverrideUOM                = ipbf-ef.cost-uom  
        opbf-ttEstCostForm.noCost                         = NOT ipbf-ef.nc
        /*Refactor - handle when ef.roll is yes see ce/print4p.i*/
                
        /*Refactor- Hard-codes*/
        opbf-ttEstCostForm.dimUOM                         = "IN"
        opbf-ttEstCostForm.areaUOM                        = "SF"
        opbf-ttEstCostForm.weightDieUOM                   = gcDefaultWeightUOM + "/MSHT"
        opbf-ttEstCostForm.weightNetUOM                   = gcDefaultWeightUOM + "/MSHT"
        opbf-ttEstCostForm.weightGrossUOM                 = gcDefaultWeightUOM + "/MSHT"
        opbf-ttEstCostForm.grossQtyRequiredTotalWeightUOM = gcDefaultWeightUOM
        opbf-ttEstCostForm.grossQtyRequiredTotalAreaUOM   = "MSF"
            
               
        /*Refactor - Formulas/Conversions - don't assume SF and inches*/
        opbf-ttEstCostForm.grossArea                      = opbf-ttEstCostForm.grossWidth * opbf-ttEstCostForm.grossLength / 144
        opbf-ttEstCostForm.netArea                        = opbf-ttEstCostForm.netWidth * opbf-ttEstCostForm.netLength / 144
        opbf-ttEstCostForm.dieArea                        = opbf-ttEstCostForm.dieWidth * opbf-ttEstCostForm.dieLength / 144
        
        opbf-ttEstCostForm.weightDieSheet                 = opbf-ttEstCostForm.basisWeight * opbf-ttEstCostForm.dieArea 
        opbf-ttEstCostForm.weightNetSheet                 = opbf-ttEstCostForm.basisWeight * opbf-ttEstCostForm.netArea 
        opbf-ttEstCostForm.weightGrossSheet               = opbf-ttEstCostForm.basisWeight * opbf-ttEstCostForm.grossArea
        .

END PROCEDURE.

PROCEDURE pAddEstItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create an ttEstCostItem given eb and other key ids
     Notes: ce/print4p.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb              FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER opbf-ttEstCostItem   FOR ttEstCostItem.

    CREATE opbf-ttEstCostItem.
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostItem.estCostItemID, INPUT-OUTPUT opbf-ttEstCostItem.rec_key, "estCostItem").
    
    ASSIGN 
        opbf-ttEstCostItem.estCostHeaderID           = ipbf-ttEstCostHeader.estCostHeaderID
        opbf-ttEstCostItem.company                   = ipbf-eb.company
        opbf-ttEstCostItem.estimateNo                = ipbf-eb.est-no
        opbf-ttEstCostItem.customerPart              = ipbf-eb.part-no
        opbf-ttEstCostItem.colorDesc                 = ipbf-eb.i-coldscr
        opbf-ttEstCostItem.customerID                = ipbf-eb.cust-no
        opbf-ttEstCostItem.shipToID                  = ipbf-eb.ship-id
        opbf-ttEstCostItem.itemName                  = ipbf-eb.part-dscr1
        opbf-ttEstCostItem.itemDescription1          = ipbf-eb.part-dscr2
        opbf-ttEstCostItem.salesgroupID              = ipbf-eb.sman
        opbf-ttEstCostItem.styleID                   = ipbf-eb.style
        opbf-ttEstCostItem.isSet                     = ipbf-eb.is-a-set
        opbf-ttEstCostItem.itemID                    = ipbf-eb.stock-no
        opbf-ttEstCostItem.company                   = ipbf-eb.company
        opbf-ttEstCostItem.productCategory           = ipbf-eb.procat
        opbf-ttEstCostItem.commissionPct             = ipbf-eb.comm
        opbf-ttEstCostItem.carrierID                 = ipbf-eb.carrier
        opbf-ttEstCostItem.carrierZone               = ipbf-eb.dest-code
        opbf-ttEstCostItem.freightChargeMethod       = ipbf-eb.chg-method
        opbf-ttEstCostItem.freightWeightPerMOverride = ipbf-eb.weight-m
        opbf-ttEstCostItem.freightCostOverridePerCWT = ipbf-eb.fr-out-c
        opbf-ttEstCostItem.freightCostOverridePerM   = ipbf-eb.fr-out-m
        opbf-ttEstCostItem.isPurchased               = ipbf-eb.pur-man
        opbf-ttEstCostItem.blankWidth                = IF ipbf-eb.t-wid EQ 0 THEN ipbf-eb.wid ELSE ipbf-eb.t-wid
        opbf-ttEstCostItem.blankLength               = IF ipbf-eb.t-len EQ 0 THEN ipbf-eb.len ELSE ipbf-eb.t-len
        opbf-ttEstCostItem.blankDepth                = IF ipbf-eb.t-dep EQ 0 THEN ipbf-eb.dep ELSE ipbf-eb.t-dep
        opbf-ttEstCostItem.blankArea                 = IF ipbf-eb.t-sqin EQ 0 THEN opbf-ttEstCostItem.blankLength * opbf-ttEstCostItem.blankWidth ELSE ipbf-eb.t-sqin
        opbf-ttEstCostItem.dimLength                 = ipbf-eb.len
        opbf-ttEstCostItem.dimWidth                  = ipbf-eb.wid
        opbf-ttEstCostItem.dimDepth                  = ipbf-eb.dep
        opbf-ttEstCostItem.quantityPerSubUnit        = ipbf-eb.cas-cnt
        opbf-ttEstCostItem.quantitySubUnitsPerUnit   = ipbf-eb.cas-pal
        /*Refactor - Calculate Windowing*/
        opbf-ttEstCostItem.blankAreaNetWindow        = opbf-ttEstCostItem.blankArea                
        /*Refactor - Hardcoded*/
        opbf-ttEstCostItem.areaUOM                   = "SQIN"
        opbf-ttEstCostItem.dimUOM                    = "IN"
        opbf-ttEstCostItem.quantityPerSet            = fGetQuantityPerSet(BUFFER ipbf-eb)
        opbf-ttEstCostItem.formNo                    = ipbf-eb.form-no
        opbf-ttEstCostItem.blankNo                   = ipbf-eb.blank-no
        
        .
        
    IF fIsComboType(ipbf-ttEstCostHeader.estType) THEN 
        ASSIGN 
            opbf-ttEstCostItem.quantityRequired = ipbf-eb.bl-qty
            opbf-ttEstCostItem.quantityYielded  = ipbf-eb.yld-qty
            .
    ELSE 
        ASSIGN 
            opbf-ttEstCostItem.quantityRequired = ipbf-ttEstCostHeader.quantityMaster * opbf-ttEstCostItem.quantityPerSet
            opbf-ttEstCostItem.quantityYielded  = opbf-ttEstCostItem.quantityRequired
            .
            
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-eb.company
        AND cust.cust-no EQ ipbf-eb.cust-no
        NO-ERROR.
  
    /*Refactor - hardcoded temp?  Consider just using eb fields*/
    IF AVAILABLE cust AND cust.cust-no NE "Temp" THEN 
        ASSIGN 
            opbf-ttEstCostItem.customerName     = cust.name
            opbf-ttEstCostItem.customerAddress1 = cust.addr[1]
            opbf-ttEstCostItem.customerAddress2 = cust.addr[2]
            opbf-ttEstCostItem.customerAddress3 = cust.city + ", " + cust.state + " " + cust.zip
            .
    ELSE 
        ASSIGN  
            opbf-ttEstCostItem.customerName     = ipbf-eb.ship-name
            opbf-ttEstCostItem.customerAddress1 = ipbf-eb.ship-addr[1]
            opbf-ttEstCostItem.customerAddress2 = ipbf-eb.ship-addr[2]
            opbf-ttEstCostItem.customerAddress3 = ipbf-eb.ship-city + ", " + ipbf-eb.ship-state + " " + ipbf-eb.ship-zip
            .
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipbf-eb.company
        AND shipto.cust-no EQ ipbf-eb.cust-no
        AND shipto.ship-id EQ ipbf-eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN 
            opbf-ttEstCostItem.shipToName     = shipto.ship-name
            opbf-ttEstCostItem.shipToAddress1 = shipto.ship-addr[1]
            opbf-ttEstCostItem.shipToAddress2 = shipto.ship-addr[2]
            opbf-ttEstCostItem.shipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
            .
    FIND FIRST sman NO-LOCK 
        WHERE sman.company EQ ipbf-eb.company
        AND sman.sman EQ ipbf-eb.sman
        NO-ERROR.
    IF AVAILABLE sman THEN 
        ASSIGN 
            opbf-ttEstCostItem.salesgroupName  = sman.sname
            opbf-ttEstCostItem.commissionBasis = sman.commbasis        
            .
    FIND FIRST style NO-LOCK 
        WHERE style.company EQ ipbf-eb.company
        AND style.style EQ ipbf-eb.style
        NO-ERROR.
    IF AVAILABLE style THEN 
        ASSIGN 
            opbf-ttEstCostItem.styleDesc = style.dscr
            .

END PROCEDURE.

PROCEDURE pAddEstFarm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create ttEstCostMaterial for the Farm out for a estimate and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader   FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank    FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER opbf-ttEstCostMaterial FOR ttEstCostMaterial.
    
    DEFINE           BUFFER bf-ttEstCostItem       FOR ttEstCostItem.
    
    FIND FIRST bf-ttEstCostItem NO-LOCK 
        WHERE bf-ttEstCostItem.estCostHeaderID EQ ipbf-ttEstCostBlank.estCostHeaderID
          AND bf-ttEstCostItem.estCostItemID   EQ ipbf-ttEstCostBlank.estCostItemID
        NO-ERROR.
    
    IF AVAILABLE bf-ttEstCostItem THEN 
    DO:
        
        CREATE opbf-ttEstCostMaterial.
        RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostMaterial.estCostMaterialID, INPUT-OUTPUT opbf-ttEstCostMaterial.rec_key, "estCostMaterial").
        ASSIGN 
            opbf-ttEstCostMaterial.estCostFormID      = ipbf-ttEstCostBlank.estCostFormID
            opbf-ttEstCostMaterial.estCostHeaderID    = ipbf-ttEstCostBlank.estCostHeaderID
            opbf-ttEstCostMaterial.estCostBlankID     = ipbf-ttEstCostBlank.estCostBlankID
            opbf-ttEstCostMaterial.company            = ipbf-ttEstCostHeader.company
            opbf-ttEstCostMaterial.estimateNo         = ipbf-ttEstCostHeader.estimateNo
            opbf-ttEstCostMaterial.formNo             = ipbf-ttEstCostBlank.formNo
            opbf-ttEstCostMaterial.blankNo            = ipbf-ttEstCostBlank.blankNo
            opbf-ttEstCostMaterial.itemID             = bf-ttEstCostItem.itemID
            opbf-ttEstCostMaterial.itemName           = bf-ttEstCostItem.itemName
            opbf-ttEstCostMaterial.quantityUOM        = "EA"
            opbf-ttEstCostMaterial.quantityUOMWaste   = opbf-ttEstCostMaterial.quantityUOM
            opbf-ttEstCostMaterial.sequenceOfMaterial = 1
            opbf-ttEstCostMaterial.dimLength          = ipbf-ttEstCostBlank.blankLength
            opbf-ttEstCostMaterial.dimWidth           = ipbf-ttEstCostBlank.blankWidth
            opbf-ttEstCostMaterial.dimDepth           = ipbf-ttEstCostBlank.blankDepth
            opbf-ttEstCostMaterial.dimUOM             = ipbf-ttEstCostBlank.dimUOM
            .
            
    END. /*available ttEstCostItem*/        
    
END PROCEDURE.

PROCEDURE pAddEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create ttEstCostMaterial from an item buffer and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcRMItemID AS CHARACTER.
    DEFINE INPUT PARAMETER ipiEstCostBlankID AS INT64.
    DEFINE PARAMETER BUFFER opbf-ttEstCostMaterial FOR ttEstCostMaterial.
 
    DEFINE           BUFFER bf-item              FOR ITEM.

    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ  ipbf-ttEstCostHeader.company
        AND bf-item.i-no EQ ipcRMItemID
        NO-ERROR.
    IF AVAILABLE bf-item THEN 
    DO:
        CREATE opbf-ttEstCostMaterial.
        RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostMaterial.estCostMaterialID, INPUT-OUTPUT opbf-ttEstCostMaterial.rec_key, "estCostMaterial").
        
        ASSIGN 
            opbf-ttEstCostMaterial.estCostFormID    = ipbf-ttEstCostForm.estCostFormID
            opbf-ttEstCostMaterial.estCostHeaderID  = ipbf-ttEstCostForm.estCostHeaderID
            opbf-ttEstCostMaterial.estCostBlankID   = ipiEstCostBlankID
            opbf-ttEstCostMaterial.company          = bf-item.company
            opbf-ttEstCostMaterial.estimateNo       = ipbf-ttEstCostForm.estimateNo
            opbf-ttEstCostMaterial.formNo           = ipbf-ttEstCostForm.formNo
            opbf-ttEstCostMaterial.itemID           = bf-item.i-no 
            opbf-ttEstCostMaterial.itemName         = IF bf-item.i-name NE "" THEN bf-item.i-name ELSE bf-item.est-dscr 
            opbf-ttEstCostMaterial.quantityUOM      = CAPS(bf-item.cons-uom)
            opbf-ttEstCostMaterial.quantityUOMWaste = opbf-ttEstCostMaterial.quantityUOM
            opbf-ttEstCostMaterial.basisWeight      = bf-item.basis-w
            opbf-ttEstCostMaterial.basisWeightUOM   = gcDefaultBasisWeightUOM
            opbf-ttEstCostMaterial.dimLength        = bf-item.s-len
            opbf-ttEstCostMaterial.dimWidth         = bf-item.s-wid
            opbf-ttEstCostMaterial.dimDepth         = bf-item.s-dep
            opbf-ttEstCostMaterial.dimUOM           = "IN"
            opbf-ttEstCostMaterial.costPerUOMAvg    = bf-item.avg-cost
            opbf-ttEstCostMaterial.costPerUOMLast   = bf-item.last-cost
            opbf-ttEstCostMaterial.isRealMaterial   = bf-item.i-code EQ "R"
            opbf-ttEstCostMaterial.materialType     = bf-item.mat-type
            opbf-ttEstCostMaterial.weightUOM        = gcDefaultWeightUOM
            .
        
        IF fIsBoardMaterial(opbf-ttEstCostMaterial.materialType) THEN 
            opbf-ttEstCostMaterial.sequenceOfMaterial = 1.
        ELSE IF fIsInkMaterial(opbf-ttEstCostMaterial.materialType) THEN 
                opbf-ttEstCostMaterial.sequenceOfMaterial = 2.
            ELSE IF fIsGlueMaterial(opbf-ttEstCostMaterial.materialType) THEN 
                    opbf-ttEstCostMaterial.sequenceOfMaterial = 3.
                ELSE IF fIsLeafMaterial(opbf-ttEstCostMaterial.materialType) THEN 
                        opbf-ttEstCostMaterial.sequenceOfMaterial = 4.
                    ELSE IF fIsPackingMaterial(opbf-ttEstCostMaterial.materialType) THEN  
                            opbf-ttEstCostMaterial.sequenceOfMaterial = 5.
                        ELSE  
                            opbf-ttEstCostMaterial.sequenceOfMaterial = 6.
            
        IF opbf-ttEstCostMaterial.estCostBlankID NE 0 THEN 
        DO:
            FIND FIRST ttEstCostBlank NO-LOCK 
                WHERE ttEstCostBlank.estCostBlankID EQ opbf-ttEstCostMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE ttEstCostBlank THEN 
            DO:
                IF NOT opbf-ttEstCostMaterial.isRealMaterial AND fIsBoardMaterial(opbf-ttEstCostMaterial.materialType) THEN 
                    ASSIGN 
                        opbf-ttEstCostMaterial.dimLength = ttEstCostBlank.blankLength
                        opbf-ttEstCostMaterial.dimWidth  = ttEstCostBlank.blankWidth
                        opbf-ttEstCostMaterial.dimLength = ttEstCostBlank.blankLength
                        .
                ASSIGN 
                    opbf-ttEstCostMaterial.blankNo = ttEstCostBlank.blankNo
                    .
            END.
        END.
        ELSE IF NOT opbf-ttEstCostMaterial.isRealMaterial AND fIsBoardMaterial(opbf-ttEstCostMaterial.materialType) THEN 
                ASSIGN 
                    opbf-ttEstCostMaterial.dimLength = ipbf-ttEstCostForm.grossLength
                    opbf-ttEstCostMaterial.dimWidth  = ipbf-ttEstCostForm.grossWidth
                    opbf-ttEstCostMaterial.dimDepth  = ipbf-ttEstCostForm.grossDepth
                    .
        
    END.
    
END PROCEDURE.

PROCEDURE pAddEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstCostMisc and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostMisc   FOR ttEstCostMisc.

    CREATE opbf-ttEstCostMisc.
    ASSIGN 
        opbf-ttEstCostMisc.estCostHeaderID = ipbf-ttEstCostForm.estCostHeaderID
        opbf-ttEstCostMisc.estCostFormID   = ipbf-ttEstCostForm.estCostFormID
        opbf-ttEstCostMisc.company         = ipbf-ttEstCostForm.company
        opbf-ttEstCostMisc.estimateNo      = ipbf-ttEstCostForm.estimateNo
        .
    
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostMisc.estCostMiscID, INPUT-OUTPUT opbf-ttEstCostMisc.rec_key, "estCostMisc").
    
END PROCEDURE.

PROCEDURE pAddEstMiscForBoardFreight PRIVATE:

 /*------------------------------------------------------------------------------
     Purpose: given a freight charge on Board, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT  PARAMETER ipcCostType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostDesc      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCostperUOM    AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUOM       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCostTotal     AS DECIMAL NO-UNDO.

    DEFINE           BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    DEFINE           BUFFER bf-prep          FOR prep.

    IF ipdCostTotal GT 0 THEN 
    DO:
        RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).

        ASSIGN 
            bf-ttEstCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
            bf-ttEstCostMisc.formNo                = ipbf-ttEstCostForm.formNo  
            bf-ttEstCostMisc.blankNo               = 0  
            bf-ttEstCostMisc.costDescription       = ipcCostDesc
            bf-ttEstCostMisc.costType              = ipcCostType
            bf-ttEstCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-ttEstCostMisc.SIMON                 = "I"
            bf-ttEstCostMisc.costUOM               = ipcCostUOM
            bf-ttEstCostMisc.costPerUOM            = ipdCostperUOM
            bf-ttEstCostMisc.costSetup             = 0
            bf-ttEstCostMisc.costTotalBeforeProfit = ipdCostTotal
            .
        RUN pCalcEstMisc(BUFFER bf-ttEstCostMisc, BUFFER ipbf-ttEstCostForm).
        
    END.

END PROCEDURE.

PROCEDURE pAddEstMiscForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a form buffer and other key fields, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostMisc FOR ttEstCostMisc.
    
    DEFINE VARIABLE cCostType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostPerM  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQty       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iBlankNo   AS INTEGER   NO-UNDO.
    
    ASSIGN 
        iBlankNo = ipbf-ef.mis-bnum[ipiIndex]
        .
    IF ipcType EQ "Material" THEN 
        ASSIGN 
            cCostType  = "Mat"
            dCostSetup = ipbf-ef.mis-matf[ipiIndex]
            .
    ELSE 
        ASSIGN 
            cCostType  = "Lab" 
            dCostSetup = ipbf-ef.mis-labf[ipiIndex]
            .
    
    IF iBlankNo NE 0 THEN 
    DO:
        FIND FIRST ttEstCostBlank NO-LOCK 
            WHERE ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
            AND ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
            AND ttEstCostBlank.blankNo EQ iBlankNo
            NO-ERROR.
        IF AVAILABLE ttEstCostBlank THEN 
            dQty = ttEstCostBlank.quantityRequired. 
    END.
    IF dQty EQ 0 THEN 
        dQty = ipbf-ttEstCostForm.quantityFGOnForm. 
        
    RUN pGetMiscCostPerM(BUFFER ipbf-ef, dQty, ipiIndex, UPPER(cCostType), OUTPUT dCostPerM).
    
    IF dCostPerM GT 0 OR dCostSetup GT 0 THEN 
    DO:
        RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
       
        ASSIGN 
            bf-ttEstCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
            bf-ttEstCostMisc.formNo                = ipbf-ef.mis-snum[ipiIndex]  
            bf-ttEstCostMisc.blankNo               = iBlankNo
            bf-ttEstCostMisc.costDescription       = ipbf-ef.mis-cost[ipiIndex]
            bf-ttEstCostMisc.costType              = cCostType
            bf-ttEstCostMisc.costUOM               = "M"
            bf-ttEstCostMisc.costPerUOM            = dCostPerM
            bf-ttEstCostMisc.costSetup             = dCostSetup
            bf-ttEstCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-ttEstCostMisc.SIMON                 = ipbf-ef.mis-simon[ipiIndex]
            bf-ttEstCostMisc.profitPercent         = ipbf-ef.mis-mkup[ipiIndex]
            bf-ttEstCostMisc.sourcequantity        = dQty
            bf-ttEstCostMisc.quantityPerSourceQty  = 1
            bf-ttEstCostMisc.quantityRequiredTotal = dQty
            bf-ttEstCostMisc.quantityUOM           = "EA"
            bf-ttEstCostMisc.costTotalBeforeProfit = dCostPerM * dQty / 1000 + dCostSetup
            .
        RUN pCalcEstMisc(BUFFER bf-ttEstCostMisc, BUFFER ipbf-ttEstCostForm).
        
    END.
END PROCEDURE.

PROCEDURE pAddEstMiscForHandling PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given the RM or FG Hanling charges, add EstMisc records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT  PARAMETER ipcCostType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostDesc      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCostperUOM    AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostUOM       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdCostTotal     AS DECIMAL NO-UNDO.
    
    DEFINE           BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    
    IF ipdCostTotal GT 0 THEN 
    DO:
        RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
       
        ASSIGN 
            bf-ttEstCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
            bf-ttEstCostMisc.formNo                = ipbf-ttEstCostForm.formNo  
            bf-ttEstCostMisc.blankNo               = 0  
            bf-ttEstCostMisc.costDescription       = ipcCostDesc
            bf-ttEstCostMisc.costType              = ipcCostType
            bf-ttEstCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-ttEstCostMisc.SIMON                 = "I"
            bf-ttEstCostMisc.costUOM               = ipcCostUOM
            bf-ttEstCostMisc.costPerUOM            = ipdCostperUOM
            bf-ttEstCostMisc.costSetup             = 0
            bf-ttEstCostMisc.costTotalBeforeProfit = ipdCostTotal
            .
            
        RUN pCalcEstMisc(BUFFER bf-ttEstCostMisc, BUFFER ipbf-ttEstCostForm).
    END.

END PROCEDURE.


PROCEDURE pAddEstMiscForPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a est-prep buffer and quantity, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-prep      FOR est-prep.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT  PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
    
    DEFINE           BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    DEFINE           BUFFER bf-prep          FOR prep.
    
    RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
    FIND FIRST bf-prep NO-LOCK 
        WHERE bf-prep.company EQ ipbf-est-prep.company
        AND bf-prep.code EQ ipbf-est-prep.code
        NO-ERROR.
    IF NOT AVAILABLE bf-prep THEN 
    DO: 
        RUN pAddError("Prep '" + ipbf-est-prep.code + "' is not valid", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipbf-est-prep.b-num, ipdQuantityMaster).
        RETURN.    
    END.
        
    ASSIGN 
        bf-ttEstCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
        bf-ttEstCostMisc.formNo                = ipbf-est-prep.s-num  
        bf-ttEstCostMisc.blankNo               = ipbf-est-prep.b-num
        bf-ttEstCostMisc.prepID                = ipbf-est-prep.code
        bf-ttEstCostMisc.itemID                = IF AVAILABLE bf-prep THEN bf-prep.i-no ELSE ipbf-est-prep.i-no
        bf-ttEstCostMisc.costDescription       = ipbf-est-prep.dscr
        bf-ttEstCostMisc.costType              = IF ipbf-est-prep.ml THEN "Mat" ELSE "Lab"
        bf-ttEstCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
        bf-ttEstCostMisc.SIMON                 = ipbf-est-prep.simon
        bf-ttEstCostMisc.profitPercent         = ipbf-est-prep.mkup
        bf-ttEstCostMisc.sourcequantity        = ipbf-est-prep.qty
        bf-ttEstCostMisc.quantityPerSourceQty  = 1
        bf-ttEstCostMisc.quantityRequiredTotal = bf-ttEstCostMisc.sourceQuantity * bf-ttEstCostMisc.quantityPerSourceQty
        bf-ttEstCostMisc.quantityUOM           = "EA"
        bf-ttEstCostMisc.costUOM               = "EA"
        bf-ttEstCostMisc.costPerUOM            = ipbf-est-prep.cost
        bf-ttEstCostMisc.costSetup             = 0
        bf-ttEstCostMisc.costTotalBeforeProfit = bf-ttEstCostMisc.costPerUOM * bf-ttEstCostMisc.quantityRequiredTotal + bf-ttEstCostMisc.costSetup
        bf-ttEstCostMisc.isPrep                = YES
        .
    
    RUN pCalcEstMisc(BUFFER bf-ttEstCostMisc, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pAddEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Create ttEstCostOperation for ttEstCostForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostOperation FOR ttEstCostOperation.
    
    CREATE opbf-ttEstCostOperation.
    ASSIGN 
        opbf-ttEstCostOperation.estCostFormID   = ipbf-ttEstCostForm.estCostFormID
        opbf-ttEstCostOperation.estCostHeaderID = ipbf-ttEstCostForm.estCostHeaderID
        opbf-ttEstCostOperation.company         = ipbf-ttEstCostForm.company
        opbf-ttEstCostOperation.estimateNo      = ipbf-ttEstCostForm.estimateNo
        opbf-ttEstCostOperation.formNo          = ipbf-ttEstCostForm.formNo
        .
    
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostOperation.estCostOperationID, INPUT-OUTPUT opbf-ttEstCostOperation.rec_key, "estCostOperation").
        
END PROCEDURE.

PROCEDURE pAddEstOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstCostOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op           FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.

    DEFINE           BUFFER bf-mach               FOR mach.
    DEFINE           BUFFER bf-est-op             FOR est-op.

    DEFINE VARIABLE cOutputType AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF NOT AVAILABLE bf-mach THEN 
    DO:
        RUN pAddError("Machine '" + ipbf-est-op.m-code + "' is not valid", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipbf-est-op.b-num, ipdQuantityMaster).
        RETURN.
    END.    
    ELSE 
    DO:     
        RUN pAddEstOperation(BUFFER ipbf-ttEstCostForm, BUFFER opbf-ttEstCostOperation).
        ASSIGN 
            opbf-ttEstCostOperation.blankNo                      = ipbf-est-op.b-num
            opbf-ttEstCostOperation.estCostBlankID               = fGetEstBlankID(opbf-ttEstCostOperation.estCostBlankID, opbf-ttEstCostOperation.estCostFormID, opbf-ttEstCostOperation.blankNo)
            opbf-ttEstCostOperation.operationID                  = ipbf-est-op.m-code
            opbf-ttEstCostOperation.pass                         = MAX(ipbf-est-op.op-pass, 1)
            opbf-ttEstCostOperation.sequenceOfOperation          = ipbf-est-op.line
            opbf-ttEstCostOperation.numOutDivisor                = ipbf-est-op.n_out_div
                       
            opbf-ttEstCostOperation.quantityInSetupWaste         = ipbf-est-op.op-waste
            opbf-ttEstCostOperation.hoursSetup                   = ipbf-est-op.op-mr
            opbf-ttEstCostOperation.speed                        = ipbf-est-op.op-speed
            opbf-ttEstCostOperation.quantityInRunWastePercent    = ipbf-est-op.op-spoil
            opbf-ttEstCostOperation.isLocked                     = ipbf-est-op.isLocked
            opbf-ttEstCostOperation.crewSizeSetup                = ipbf-est-op.op-crew[1]
            opbf-ttEstCostOperation.crewSizeRun                  = ipbf-est-op.op-crew[2]
            opbf-ttEstCostOperation.countInks                    = ipbf-est-op.num-col
            opbf-ttEstCostOperation.countCoats                   = ipbf-est-op.num-coat
            opbf-ttEstCostOperation.countFountainChanges         = ipbf-est-op.fountains
            opbf-ttEstCostOperation.countPlateChanges            = ipbf-est-op.plates
            
            opbf-ttEstCostOperation.isSpeedInLF                  = bf-mach.therm
            opbf-ttEstCostOperation.operationName                = bf-mach.m-dscr
            opbf-ttEstCostOperation.feedType                     = bf-mach.p-type
            opbf-ttEstCostOperation.outputType                   = opbf-ttEstCostOperation.feedType
            opbf-ttEstCostOperation.departmentIDPrimary          = bf-mach.dept[1]
            opbf-ttEstCostOperation.departmentID                 = bf-mach.dept
            opbf-ttEstCostOperation.quantityInSetupWastePerColor = bf-mach.col-wastesh
            opbf-ttEstCostOperation.costPerHourFOSetup           = bf-mach.mr-fixoh
            opbf-ttEstCostOperation.costPerHourFORun             = bf-mach.run-fixoh
            opbf-ttEstCostOperation.costPerHourVOSetup           = bf-mach.mr-varoh
            opbf-ttEstCostOperation.costPerHourVORun             = bf-mach.run-varoh
            opbf-ttEstCostOperation.quantityInkLbsWastedPerSetup = bf-mach.ink-waste
            opbf-ttEstCostOperation.quantityInkLbsWastedPerColor = bf-mach.col-wastelb
            opbf-ttEstCostOperation.hoursRunMinimum              = bf-mach.minRunHours
            opbf-ttEstCostOperation.costMinimum                  = bf-mach.mrk-rate
            .

        IF glOpRatesSeparate THEN 
            ASSIGN 
                opbf-ttEstCostOperation.costPerManHourDLSetup = bf-mach.lab-rate[1]
                opbf-ttEstCostOperation.costPerManHourDLRun   = bf-mach.lab-rate[2]
                .
        ELSE 
            ASSIGN 
                opbf-ttEstCostOperation.costPerManHourDLSetup = bf-mach.lab-rate[bf-mach.lab-drate]
                opbf-ttEstCostOperation.costPerManHourDLRun   = bf-mach.lab-rate[bf-mach.lab-drate]
                .
            
       
        IF fIsDepartment(gcDeptsForPrinters, opbf-ttEstCostOperation.departmentID) THEN  
            opbf-ttEstCostOperation.isPrinter = YES.
        IF fIsDepartment(gcDeptsForCoaters, opbf-ttEstCostOperation.departmentID) THEN  
            opbf-ttEstCostOperation.isCoater = YES.
        
        IF fIsDepartment(gcDeptsForGluers, opbf-ttEstCostOperation.departmentID)  THEN 
            opbf-ttEstCostOperation.isGluer = YES.
        IF fIsDepartment(gcDeptsForLeafers, opbf-ttEstCostOperation.departmentID)  THEN 
            opbf-ttEstCostOperation.isLeafer = YES.
        IF fIsDepartment(gcDeptsForSheeters, opbf-ttEstCostOperation.departmentID)  THEN 
            opbf-ttEstCostOperation.isNetSheetMaker = YES.
        IF fIsDepartment(gcDeptsForCorrugators, opbf-ttEstCostOperation.departmentID)  THEN 
            opbf-ttEstCostOperation.isCorrugator = YES.
        
        IF VALID-HANDLE(ghOperation) THEN
            RUN Operations_GetOutputType IN ghOperation( INPUT ipbf-est-op.company, 
                INPUT ipbf-est-op.est-no, 
                INPUT ipbf-est-op.s-num, 
                INPUT ipbf-est-op.m-code,
                INPUT ipbf-est-op.qty,
                INPUT ipbf-est-op.line,
                OUTPUT cOutputType).
        
        IF cOutputType = "B" THEN
            ASSIGN 
                opbf-ttEstCostOperation.isBlankMaker = YES
                opbf-ttEstCostOperation.outputType   = "B"
                .
                
        ELSE IF cOutputType = "S" THEN
            ASSIGN
                opbf-ttEstCostOperation.isNetSheetMaker = YES
                opbf-ttEstCostOperation.outputType      = "S".
                
        
        opbf-ttEstCostOperation.numOutForOperation = 1.
        IF opbf-ttEstCostOperation.isNetSheetMaker THEN 
            ASSIGN 
                opbf-ttEstCostOperation.numOutForOperation = fGetNetSheetOut(opbf-ttEstCostOperation.estCostOperationID,ipbf-ttEstCostForm.numOutNet, ipbf-est-op.qty)
                .
        IF opbf-ttEstCostOperation.isBlankMaker THEN 
            ASSIGN 
                opbf-ttEstCostOperation.numOutForOperation = opbf-ttEstCostOperation.numOutForOperation * ipbf-ttEstCostForm.numOutBlanksOnNet.
                
        IF opbf-ttEstCostOperation.blankNo NE 0 THEN 
        DO:
            FIND FIRST ttEstCostBlank NO-LOCK 
                WHERE ttEstCostBlank.estCostHeaderID EQ opbf-ttEstCostOperation.estCostHeaderID
                AND ttEstCostBlank.estCostFormID EQ opbf-ttEstCostOperation.estCostFormID
                AND ttEstCostBlank.blankNo EQ opbf-ttEstCostOperation.blankNo
                NO-ERROR.
            IF AVAILABLE ttEstCostBlank THEN 
                opbf-ttEstCostOperation.estCostBlankID = ttEstCostBlank.estCostBlankID. 
        END.
    END.
    
END PROCEDURE.

PROCEDURE pAddGlue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a glue
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank  FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER ipbf-eb              FOR eb.
  
    DEFINE           BUFFER bf-item            FOR item.
    DEFINE VARIABLE dQtyRequiredPerBlank AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyMultiplier       AS DECIMAL NO-UNDO.
    
    IF ipbf-eb.adhesive NE "" THEN 
    DO:
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ ipbf-eb.company
            AND bf-item.i-no EQ ipbf-eb.adhesive
            NO-ERROR.
        IF NOT AVAILABLE bf-item THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is not valid", giErrorImportant, ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo,ipbf-ttEstCostBlank.blankNo, ipbf-ttEstCostHeader.quantityMaster).
            RETURN.
        END.
        IF NOT fIsGlueMaterial(bf-item.mat-type) THEN  
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid material but not a glue material type", giErrorImportant, ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo,ipbf-ttEstCostBlank.blankNo, ipbf-ttEstCostHeader.quantityMaster).
            RETURN.
        END.
        IF bf-item.sqin-lb EQ 0 AND bf-item.linin-lb EQ 0 THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid glue material but no coverage rate configured", giErrorWarning, ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo,ipbf-ttEstCostBlank.blankNo, ipbf-ttEstCostHeader.quantityMaster).
            RETURN.
        END.
        
        FIND FIRST ttGlue EXCLUSIVE-LOCK 
            WHERE ttGlue.estHeaderID EQ ipbf-ttEstCostBlank.estCostHeaderID
            AND ttGlue.estFormID EQ ipbf-ttEstCostBlank.estCostFormID
            AND ttGlue.estBlankID EQ ipbf-ttEstCostBlank.estCostBlankID
            AND ttGlue.iFormNo EQ ipbf-ttEstCostBlank.formNo
            AND ttGlue.iBlankNo EQ ipbf-ttEstCostBlank.blankNo
            AND ttGlue.cItemID EQ ipbf-eb.adhesive
            NO-ERROR.
        IF NOT AVAILABLE ttGlue THEN 
        DO:
            CREATE ttGlue.
            ASSIGN 
                ttGlue.company       = ipbf-ttEstCostBlank.company
                ttGlue.estHeaderID   = ipbf-ttEstCostBlank.estCostHeaderID
                ttGlue.estFormID     = ipbf-ttEstCostBlank.estCostFormID
                ttGlue.estBlankID    = ipbf-ttEstCostBlank.estCostBlankID
                ttGlue.iFormNo       = ipbf-ttEstCostBlank.formNo
                ttGlue.iBlankNo      = ipbf-ttEstCostBlank.blankNo
                ttGlue.cItemID       = bf-item.i-no
                ttGlue.cDescription  = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name
                ttGlue.cMaterialType = bf-item.mat-type
                ttGlue.cQtyUOM       = bf-item.cons-uom   
                ttGlue.dMinLbsPerJob = bf-item.min-lbs
                .
        END.
        
        IF bf-item.linin-lb NE 0 AND ipbf-eb.lin-in NE 0 THEN 
        DO:
            IF bf-item.mat-type EQ 'T' THEN 
                ASSIGN 
                    dQtyMultiplier          = 4 /*Quad Stay tape assumed*/
                    ttGlue.cCoverageRateUOM = "LIN/" + ttGlue.cQtyUOM
                    .
            ELSE 
                ASSIGN 
                    dQtyMultiplier          = 1
                    ttGlue.cCoverageRateUOM = "LIN/LB"
                    .
            ASSIGN 
                
                ttGlue.dCoverageRate = bf-item.linin-lb
                dQtyRequiredPerBlank = ipbf-eb.lin-in * dQtyMultiplier / ttGlue.dCoverageRate
                .
        END.
        ELSE IF bf-item.sqin-lb NE 0 THEN 
                ASSIGN 
                    ttGlue.dCoverageRate    = bf-item.sqin-lb
                    ttGlue.cCoverageRateUOM = "SQIN/LB"
                    dQtyRequiredPerBlank    = ipbf-ttEstCostBlank.blankArea / ttGlue.dCoverageRate
                    .
        ASSIGN
            ttGlue.dQtyRequiredPerBlank = ttGlue.dQtyRequiredPerBlank + dQtyRequiredPerBlank.
        .             
            
    END.

END PROCEDURE.

PROCEDURE pAddHeader PRIVATE:
    /*------------------------------------------------------------------------------
         Purpose: given an eb buffer, create the EstBlank
         Notes:
        ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est FOR est.
    DEFINE INPUT PARAMETER ipdQtyMaster AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdReleases AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INT64 NO-UNDO.
       
    CREATE ttEstCostHeader.
    RUN pSetKeyFields(INPUT-OUTPUT ttEstCostHeader.estCostHeaderID, INPUT-OUTPUT ttEstCostHeader.rec_key, "estCostHeader").
    ASSIGN 
        ttEstCostHeader.calcDateTime   = NOW
        ttEstCostHeader.calculatedBy   = USERID("ASI")
        ttEstCostHeader.company        = ipbf-est.company
        ttEstCostHeader.estimateNo     = ipbf-est.est-no
        ttEstCostHeader.quantityMaster = ipdQtyMaster
        ttEstCostHeader.releaseCount   = ipdReleases
        ttEstCostHeader.jobID          = ipcJobID
        ttEstCostHeader.jobID2         = ipiJobID2
        opiEstCostHeaderID           = ttEstCostHeader.estCostHeaderID
        .
    
        
    RUN pBuildHeader(BUFFER ttEstCostHeader).

END PROCEDURE.

PROCEDURE pAddInk PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add an ink
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank FOR ttEstCostBlank.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCoveragePercent AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplNoCharge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
        
    DEFINE BUFFER bf-item FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstCostBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Ink RM Code '" + ipcItemCode + "'", giErrorImportant, ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo, ipdQuantityMaster).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT fIsInkMaterial(bf-item.mat-type) THEN  
        DO: 
            RUN pAddError("Ink '" + ipcItemCode + "' is valid material but not an ink material type.", giErrorImportant,ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo, ipdQuantityMaster).
            RETURN.
        END.
        IF bf-item.yield EQ 0 OR bf-item.yield EQ ? THEN 
        DO: 
            RUN pAddError("Ink '" + ipcItemCode + "' has an invalid coverage rate.", giErrorImportant,ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo, ipdQuantityMaster).
            RETURN.
        END.
        FIND FIRST ttInk EXCLUSIVE-LOCK 
            WHERE ttInk.estHeaderID EQ ipbf-ttEstCostBlank.estCostHeaderID
            AND ttInk.estFormID EQ ipbf-ttEstCostBlank.estCostFormID
            AND ttInk.estBlankID EQ ipbf-ttEstCostBlank.estCostBlankID
            AND ttInk.iFormNo EQ ipbf-ttEstCostBlank.formNo
            AND ttInk.iBlankNo EQ ipbf-ttEstCostBlank.blankNo
            AND ttInk.cItemID EQ ipcItemCode
            AND ttInk.iPass EQ ipiPass
            NO-ERROR.
        IF NOT AVAILABLE ttInk THEN 
        DO:
            CREATE ttInk.
            ASSIGN 
                ttInk.company          = ipbf-ttEstCostBlank.company
                ttInk.estHeaderID      = ipbf-ttEstCostBlank.estCostHeaderID
                ttInk.estFormID        = ipbf-ttEstCostBlank.estCostFormID
                ttInk.estBlankID       = ipbf-ttEstCostBlank.estCostBlankID
                ttInk.iFormNo          = ipbf-ttEstCostBlank.formNo
                ttInk.iBlankNo         = ipbf-ttEstCostBlank.blankNo
                ttInk.iPass            = ipiPass
                ttInk.cItemID          = ipcItemCode
                ttInk.cDescription     = ipcDescription
                ttInk.lNoCharge        = iplNoCharge
                ttInk.cMaterialType    = bf-item.mat-type
                ttInk.dCoverageRate    = bf-item.yield
                ttInk.cCoverageRateUOM = "LBS/SQIN"
                ttInk.cPressType       = bf-item.press-type
                ttInk.cQtyUOM          = "LB"   
                ttInk.dMinLbsPerJob    = bf-item.min-lbs         
                .
        END.
        ASSIGN 
            ttInk.dCoveragePercent = ttInk.dCoveragePercent + (ipdCoveragePercent / 100)
            .             
        IF ttInk.cMaterialType EQ "I" THEN 
            ttInk.iCountInks = ttInk.iCountInks + 1.
        ELSE 
            ttInk.iCountCoatings = ttInk.iCountCoatings + 1.
    
    END.

END PROCEDURE.

PROCEDURE pAddLeaf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a leaf/film/window/label
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
            
    DEFINE BUFFER bf-item         FOR item.
    DEFINE BUFFER bf-ttEstCostBlank FOR ttEstCostBlank.
    
    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Leaf/Film RM Code '" + ipcItemCode + "'", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipiBlankNo, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT fIsLeafMaterial(bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Leaf/Film '" + ipcItemCode + "' is valid but not a leaf/film material type.", giErrorImportant,ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipiBlankNo, ipbf-ttEstCostHeader.quantityMaster).
            RETURN.
        END.
        FIND FIRST ttLeaf EXCLUSIVE-LOCK 
            WHERE ttLeaf.estHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
            AND ttLeaf.estFormID EQ ipbf-ttEstCostForm.estCostFormID
            AND ttLeaf.iFormNo EQ ipbf-ttEstCostForm.formNo
            AND ttLeaf.iBlankNo EQ ipiBlankNo
            AND ttLeaf.cItemID EQ ipcItemCode
            AND ttLeaf.iIndex EQ ipiIndex
            NO-ERROR.
        IF NOT AVAILABLE ttLeaf THEN 
        DO:
            CREATE ttLeaf.
            ASSIGN 
                ttLeaf.company             = ipbf-ttEstCostForm.company
                ttLeaf.estHeaderID         = ipbf-ttEstCostForm.estCostHeaderID
                ttLeaf.estFormID           = ipbf-ttEstCostForm.estCostFormID
                ttLeaf.iFormNo             = ipbf-ttEstCostForm.formNo
                ttLeaf.iBlankNo            = ipiBlankNo
                ttLeaf.cItemID             = ipcItemCode
                ttLeaf.cDescription        = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name
                ttLeaf.cMaterialType       = bf-item.mat-type
                ttLeaf.cQtyUOM             = IF bf-item.cons-uom EQ "" THEN "LB" ELSE bf-item.cons-uom    
                ttLeaf.dDimLength          = ipdLength
                ttLeaf.dDimWidth           = ipdWidth    
                ttLeaf.cDimUOM             = "IN"
                ttLeaf.dAreaInSqInAperture = ipdLength * ipdWidth 
                ttLeaf.cDescription        = IF ipcDescription NE "" THEN ipcDescription ELSE ttLeaf.cDescription
                ttLeaf.dCoverageRate       = bf-item.sqin-lb
                ttLeaf.cCoverageRateUOM    = "SQIN/LB"
                ttLeaf.lIsSheetFed         = ipiBlankNo EQ 0
                ttLeaf.lIsWindow           = fIsWindowMaterial(bf-item.mat-type) AND bf-item.industry EQ "1"
                ttLeaf.lIsWax              = fIsWaxMaterial(bf-item.mat-type) AND bf-item.industry EQ "2"
                .
            IF ttLeaf.lIsWindow THEN 
                ASSIGN 
                    ttLeaf.dDimLength = ipdLength + gdWindowDimOverlap * 2
                    ttLeaf.dDimWidth  = ipdWidth + gdWindowDimOverlap * 2
                    .
            ttLeaf.dAreaInSQIn         = ttLeaf.dDimLength * ttLeaf.dDimWidth.
            
            IF ttLeaf.lIsWax AND bf-item.shrink NE 0 THEN 
                ASSIGN 
                    ttLeaf.dAreaInSqIn  = ((ttLeaf.dAreaInSQIn / 144000) * ipbf-ttEstCostForm.basisWeight) * bf-item.shrink
                    ttLeaf.dQtyRequiredPerLeaf = ttLeaf.dAreaInSqIn
                    ttLeaf.dCoverageRate = 1                    
                    .
            ELSE 
                RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostForm.company, ttLeaf.cItemID, "RM", "EA", ttLeaf.cQtyUOM, 
                144000 / ttLeaf.dCoverageRate, ttLeaf.dDimLength, ttLeaf.dDimWidth, 0, 
                1, OUTPUT ttLeaf.dQtyRequiredPerLeaf).                
                
            FIND FIRST bf-ttEstCostBlank EXCLUSIVE-LOCK 
                WHERE bf-ttEstCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND bf-ttEstCostBlank.estCostFormID EQ ttLeaf.estFormID
                AND bf-ttEstCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE bf-ttEstCostBlank THEN 
                ASSIGN
                    /*Only add Window Area if material is a Window - i.e. cut out*/ 
                    bf-ttEstCostBlank.blankAreaWindow    = bf-ttEstCostBlank.blankAreaWindow + IF ttLeaf.lIsWindow THEN ttLeaf.dAreaInSQInAperture ELSE 0
                    bf-ttEstCostBlank.blankAreaNetWindow = bf-ttEstCostBlank.blankArea - bf-ttEstCostBlank.blankAreaWindow
                    ttLeaf.estBlankID                  = bf-ttEstCostBlank.estCostBlankID
                    .
            RELEASE bf-ttEstCostBlank.
        END.
        
    END.

END PROCEDURE.

PROCEDURE pAddPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a packing material
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank FOR ttEstCostBlank.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttPack FOR ttPack.
    DEFINE INPUT PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
        
    DEFINE           BUFFER bf-item     FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstCostBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Pack RM Code '" + ipcItemCode + "'", giErrorImportant, ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo, ipdQuantityMaster).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT fIsPackingMaterial(bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Packing '" + ipcItemCode + "' is valid but not a packing material type.", giErrorImportant,ipbf-ttEstCostBlank.estCostHeaderID, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo, ipdQuantityMaster).
            RETURN.
        END.
        FIND FIRST opbf-ttPack EXCLUSIVE-LOCK 
            WHERE opbf-ttPack.estHeaderID EQ ipbf-ttEstCostBlank.estCostHeaderID
            AND opbf-ttPack.estFormID EQ ipbf-ttEstCostBlank.estCostFormID
            AND opbf-ttPack.estBlankID EQ ipbf-ttEstCostBlank.estCostBlankID
            AND opbf-ttPack.iFormNo EQ ipbf-ttEstCostBlank.formNo
            AND opbf-ttPack.iBlankNo EQ ipbf-ttEstCostBlank.blankNo
            AND opbf-ttPack.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE opbf-ttPack THEN 
        DO:
            CREATE opbf-ttPack.
            ASSIGN 
                opbf-ttPack.company               = ipbf-ttEstCostBlank.company
                opbf-ttPack.estHeaderID           = ipbf-ttEstCostBlank.estCostHeaderID
                opbf-ttPack.estFormID             = ipbf-ttEstCostBlank.estCostFormID
                opbf-ttPack.estBlankID            = ipbf-ttEstCostBlank.estCostBlankID
                opbf-ttPack.iFormNo               = ipbf-ttEstCostBlank.formNo
                opbf-ttPack.iBlankNo              = ipbf-ttEstCostBlank.blankNo
                opbf-ttPack.cItemID               = ipcItemCode
                opbf-ttPack.cDescription          = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name
                opbf-ttPack.cMaterialType         = bf-item.mat-type
                opbf-ttPack.cQtyUOM               = "EA"   
                opbf-ttPack.cDimUOM               = "IN"
                opbf-ttPack.dDimLength            = bf-item.case-l
                opbf-ttPack.dDimWidth             = bf-item.case-w
                opbf-ttPack.dDimDepth             = bf-item.case-d
                opbf-ttPack.dWeightTare           = IF bf-item.basis-w NE 0 THEN bf-item.basis-w 
                                                        ELSE IF bf-item.weight-100 NE 0 THEN bf-item.weight-100 / 100 ELSE 0
                opbf-ttPack.iCountPerSubUnit      = bf-item.box-case
                opbf-ttPack.iCountSubUnitsPerUnit = bf-item.case-pall
                opbf-ttPack.dWeightCapacity       = bf-item.avg-w
                .
        END.
    
    END.

END PROCEDURE.

PROCEDURE pBuildCostDetailForFreight PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given buffers, calculate and process freight
     Notes:
     Syntax:
            RUN pBuildCostDetailForFreight(BUFFER estCostHeader, BUFFER ttEstCostForm, 
                BUFFER ttEstCostBlank, BUFFER ttEstCostItem, BUFFER eb, iEstCostFormID, iEstCostBlankID).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank  FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostItem   FOR ttEstCostItem.
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.
    DEFINE INPUT PARAMETER ipiEstCostFormIDForCost AS INT64.
    DEFINE INPUT PARAMETER ipiEstCostBlankIDForCost AS INT64.
    
    DEFINE VARIABLE dQtyShipped   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMSFforEA     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLBSforEA     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFreightTotal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFreightMin   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE dFreightTotalInternal AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFreightTotalExternal AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    
        
    ASSIGN 
        dQtyShipped = ipbf-ttEstCostBlank.quantityRequired
                
        /*REFACTOR - MSF assumed use blankAreaUOM*/
        dMSFforEA   = ipbf-ttEstCostItem.blankArea / 144000  
                
        /*REFACTOR - LBs assumed use weightUOM */
        dLBsforEA   = IF ipbf-ttEstCostItem.freightWeightPerMOverride NE 0 
                                THEN ipbf-ttEstCostItem.freightWeightPerMOverride / 1000 
                                ELSE ipbf-ttEstCostItem.weightTotal / ipbf-ttEstCostItem.quantityRequired
        .
            
    /*Get Total Freight from Freight Procs*/
    IF DYNAMIC-FUNCTION("HasReleases", ROWID(ipbf-eb)) THEN DO: /*Run through estReleases Calc*/
        RUN GetFreightForEstimateBlank (ipbf-ttEstCostBlank.company, ipbf-ttEstCostBlank.estimateNo, dQtyShipped, ipbf-ttEstCostBlank.formNo, ipbf-ttEstCostBlank.blankNo,
            OUTPUT dFreightTotal, OUTPUT dFreightTotalInternal, OUTPUT dFreightTotalExternal).
    END.
    ELSE 
    DO: /*Calc Freight based on basic inputs*/
        RUN GetFreight(ipbf-ttEstCostBlank.company, ipbf-ttEstCostHeader.warehouseID, ipbf-ttEstCostItem.carrierID, ipbf-ttEstCostItem.carrierZone, "", 
            dQtyShipped, dLBSforEA, dMSFforEA, ipbf-ttEstCostBlank.quantityOfUnits, ipbf-ttEstCostItem.freightCostOverridePerCWT, ipbf-ttEstCostItem.freightCostOverridePerM,
            OUTPUT dFreightTotal, OUTPUT dFreightMin, OUTPUT lError, OUTPUT cMessage).
        IF ipbf-ttEstCostHeader.releaseCount GT 1 AND dFreightMin GT 0 THEN 
            dFreightTotal = dFreightTotal + (ipbf-ttEstCostHeader.releaseCount - 1) * dFreightMin.
    END.
    IF dFreightTotal NE 0 THEN 
    DO:
        IF ipbf-ttEstCostItem.freightChargeMethod EQ "P" THEN 
        DO: /*Integrate Freight Cost for Prepaid*/
            RUN pAddCostDetail(ipbf-ttEstCostBlank.estCostHeaderID, ipiEstCostFormIDForCost, ipiEstCostBlankIDForCost, ipbf-ttEstCostBlank.estCostBlankID, 
                gcSourceTypeNonFactory, "nfFreightInternal", "Freight Internal", dFreightTotalInternal, 0, ipbf-ttEstCostBlank.company, ipbf-ttEstCostBlank.estimateNo, BUFFER bf-ttEstCostDetail).

            RUN pAddCostDetail(ipbf-ttEstCostBlank.estCostHeaderID, ipiEstCostFormIDForCost, ipiEstCostBlankIDForCost, ipbf-ttEstCostBlank.estCostBlankID, 
                gcSourceTypeNonFactory, "nfFreight", "Freight", dFreightTotalExternal, 0, ipbf-ttEstCostBlank.company, ipbf-ttEstCostBlank.estimateNo, BUFFER bf-ttEstCostDetail).
        END. /*Prepaid Freight*/
        ELSE 
        DO: /*Separate Billed Freight*/
            RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
            IF AVAILABLE bf-ttEstCostMisc THEN 
            DO:
                ASSIGN 
                    bf-ttEstCostMisc.estCostFormID         = ipiEstCostFormIDForCost
                    bf-ttEstCostMisc.estCostBlankID        = ipiEstCostBlankIDForCost
                    bf-ttEstCostMisc.formNo                = ipbf-ttEstCostBlank.formNo  
                    bf-ttEstCostMisc.blankNo               = ipbf-ttEstCostBlank.blankNo
                    bf-ttEstCostMisc.prepID                = "Freight"
                    bf-ttEstCostMisc.costDescription       = "Freight - Billed"
                    bf-ttEstCostMisc.costType              = "Frt"
                    bf-ttEstCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
                    bf-ttEstCostMisc.SIMON                 = "S"
                    bf-ttEstCostMisc.profitPercent         = 0
                    bf-ttEstCostMisc.sourcequantity        = 1
                    bf-ttEstCostMisc.quantityPerSourceQty  = 1
                    bf-ttEstCostMisc.quantityRequiredTotal = bf-ttEstCostMisc.sourceQuantity * bf-ttEstCostMisc.quantityPerSourceQty
                    bf-ttEstCostMisc.quantityUOM           = "EA"
                    bf-ttEstCostMisc.costUOM               = "EA"
                    bf-ttEstCostMisc.costPerUOM            = 0
                    bf-ttEstCostMisc.costSetup             = dFreightTotal
                    bf-ttEstCostMisc.costTotalBeforeProfit = bf-ttEstCostMisc.costPerUOM * bf-ttEstCostMisc.quantityRequiredTotal + bf-ttEstCostMisc.costSetup
                    bf-ttEstCostMisc.isPrep                = NO
                    .
                RUN pCalcEstMisc(BUFFER bf-ttEstCostMisc, BUFFER ipbf-ttEstCostForm).
            END.
        END. /*Separate Billed Freight*/
    END.    /*Freight not 0*/
    

END PROCEDURE.

PROCEDURE pBuildCostDetailForMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
        
    IF fIsBoardMaterial(ipbf-ttEstCostMaterial.materialType) THEN  
    DO:
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "boardNoWaste","Board Cost - No Waste",
            ipbf-ttEstCostMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "boardSetupWaste","Board Cost - Setup Waste",
            ipbf-ttEstCostMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "boardRunWaste","Board Cost - Run Waste",
            ipbf-ttEstCostMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "boardSetupVend","Board Cost - Vendor Setup",
            ipbf-ttEstCostMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "boardMinDiff","Board Cost - Minimum Diff",
            ipbf-ttEstCostMaterial.costTotalMinDiff,0).
    END.
    ELSE 
    DO:        
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "matNoWaste","Material Cost - No Waste",
            ipbf-ttEstCostMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "matSetupWaste","Material Cost - Setup Waste",
            ipbf-ttEstCostMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "matRunWaste","Material Cost - Run Waste",
            ipbf-ttEstCostMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "matSetupVend","Material Cost - Vendor Setup",
            ipbf-ttEstCostMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "matMinDiff","Material Cost - Minimum Diff",
            ipbf-ttEstCostMaterial.costTotalMinDiff,0).
    END.    
    IF ipbf-ttEstCostMaterial.costTotalDeviation NE 0 THEN 
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstCostMaterial, "deviation","Deviation Cost",
            ipbf-ttEstCostMaterial.costTotalDeviation,0).
        
END PROCEDURE.

PROCEDURE pBuildCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMisc FOR ttEstCostMisc.
    
    DEFINE VARIABLE cCostBin AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cCostBin = IF ipbf-ttEstCostMisc.isPrep THEN "P" ELSE "M"
        cCostBin = cCostBin + ipbf-ttEstCostMisc.costType + ipbf-ttEstCostMisc.SIMON.
    
    CASE cCostBin:
        WHEN "PLabI" THEN 
            DO:  /*PrepLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabProfit","Prep Labor - Profit",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatI" THEN  
            DO:  /*PrepMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatProfit","Prep Material - Profit",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END. 
        WHEN "PLabM" THEN 
            DO:  /*PrepLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabPrice","Prep Labor - Profit - Price",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatM" THEN  
            DO:  /*PrepMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatPrice","Prep Material - Profit - Price",
                    ipbf-ttEstCostMisc.profitTotal,0).   
            END.                 
        WHEN "PLabS" OR 
        WHEN "PLabO" THEN 
            DO:  /*PrepLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabCostSep","Prep Labor - Cost - Separate",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pLabProfitSep","Prep Labor - Profit - Separate",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatS" OR 
        WHEN "PMatO" THEN  
            DO:  /*PrepMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatCostSep","Prep Material - Cost - Separate",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "pMatProfitSep","Prep Material - Profit - Separate",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END. 
        WHEN "MLabI" THEN 
            DO:  /*MiscLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabProfit","Misc Labor - Profit - COGS",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatI" OR
        WHEN "MFrtBrdI" OR
        WHEN "MRMI" OR
        WHEN "MFGI" THEN  
            DO:  /*MiscMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatProfit","Misc Material - Profit - COGS",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END. 
        WHEN "MLabM" THEN 
            DO:  /*MiscLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabPrice","Misc Labor - Profit - Price",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatM" THEN  
            DO:  /*MiscMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatPrice","Misc Material - Profit - Price",
                    ipbf-ttEstCostMisc.profitTotal,0).   
            END.                 
        WHEN "MLabS" OR 
        WHEN "MLabO" THEN 
            DO:  /*MiscLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabCostSep","Misc Labor - Cost - Separate",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mLabProfitSep","Misc Labor - Profit - Separate",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatS" OR 
        WHEN "MMatO" THEN  
            DO:  /*MiscMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatCostSep","Misc Material - Cost - Separate",
                    ipbf-ttEstCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstCostMisc, "mMatProfitSep","Misc Material - Profit - Separate",
                    ipbf-ttEstCostMisc.profitTotal,0).                    
            END. 
    END.
   
        
END PROCEDURE.

PROCEDURE pBuildEstHandlingCharges PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Calculate the RM and FG handling chg per cwt
    ------------------------------------------------------------------------------*/
  
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostForm        FOR ttEstCostForm.
    DEFINE BUFFER bf-ttEstCostHeader      FOR ttEstCostHeader.
    DEFINE BUFFER bf-ef                   FOR ef.
    DEFINE BUFFER bf-ttEstCostBlank       FOR ttEstCostBlank.
    DEFINE BUFFER bf-ttEstCostItem        FOR ttEstCostItem.
    DEFINE BUFFER bf-ttEstCostMaterial    FOR ttEstCostMaterial.
    DEFINE BUFFER bf-ttEstCostMisc        FOR ttEstCostMisc.
    
    
    DEFINE VARIABLE dRMHandlingCost           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dFGHandlingCost           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dHandlingCost             AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dApplicableRMHandlingRate AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dApplicableFGHandlingRate AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dApplicableHandlingPct    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBoardWeightInCUOM        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightInSrcUOM           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostTotalMaterial        AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cSrcUOM                   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dNetWeightInCUOM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFirstBlank               AS LOGICAL   NO-UNDO.
    
    
    FOR FIRST bf-ttEstCostHeader NO-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH bf-ttEstCostForm NO-LOCK 
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-ef NO-LOCK
        WHERE bf-ef.company = bf-ttEstCostForm.company
        AND bf-ef.est-no  = bf-ttEstCostForm.estimateNo
        AND bf-ef.form-no = bf-ttEstCostForm.formNo:
        
        ASSIGN
            dFGHandlingCost           = 0
            dRMHandlingCost           = 0 
            dHandlingCost             = 0 
            dCostTotalMaterial        = 0
            lFirstBlank               = Yes
            dApplicableFGHandlingRate = 0
            dApplicableRMHandlingRate = 0
            dApplicableHandlingPct    = 0.
            
        
        FOR EACH bf-ttEstCostBlank NO-LOCK 
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
            FIRST bf-ttEstCostItem NO-LOCK 
            WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID:
                
            IF bf-ttEstCostBlank.isPurchased THEN
                dApplicableFGHandlingRate = bf-ttEstCostHeader.handlingRateFGFarmPerCWT.
            ELSE
                dApplicableFGHandlingRate = bf-ttEstCostHeader.handlingRateFGPerCWT.
                
            
            ASSIGN
                dWeightInSrcUOM = bf-ttEstCostItem.weightTotal
                cSrcUOM         = bf-ttEstCostItem.weightUOM.
                
            RUN pConvertQuantityFromUOMToUOM(bf-ttEstCostItem.company, bf-ttEstCostItem.itemID, "RM", cSrcUOM, "CWT", 
                0, bf-ttEstCostItem.dimLength, bf-ttEstCostItem.dimWidth, bf-ttEstCostItem.dimDepth, 
                dWeightInSrcUOM, OUTPUT dNetWeightInCUOM).
                
            
            /* calculate the FG handling charge using Net weight. From the blank item, we can get FG Net weights by form. */
            IF dApplicableFGHandlingRate NE 0 THEN 
                dFGHandlingCost = dFGHandlingCost + (dNetWeightInCUOM * dApplicableFGHandlingRate).    
            
            /* Set Form Level fields based upon first blank values. is there a more accurate way to find find primary Blank */
            IF lFirstBlank = YES THEN
            DO:
                lFirstBlank = NO.
                
                IF bf-ttEstCostBlank.isPurchased THEN
                    ASSIGN 
                        dApplicableRMHandlingRate = bf-ttEstCostHeader.handlingRateRMFarmPerCWT 
                        dApplicableHandlingPct    = bf-ttEstCostHeader.handlingChargeFarmPct
                        .
                ELSE
                    ASSIGN 
                        dApplicableRMHandlingRate = bf-ttEstCostHeader.handlingRateRMPerCWT
                        dApplicableHandlingPct    = bf-ttEstCostHeader.handlingChargePct.
            END.
                
        END. /* FOR EACH bf-ttEstCostBlank NO-LOCK */ 
        
        ASSIGN
            dWeightInSrcUOM = bf-ttEstCostForm.grossQtyRequiredTotalWeight 
            cSrcUOM     = bf-ttEstCostForm.grossQtyRequiredTotalWeightUOM.
                
            
        RUN pConvertQuantityFromUOMToUOM(bf-ttEstCostForm.company, bf-ef.board, "RM", cSrcUOM, "CWT", 0, 0, 0, 0, 
            dWeightInSrcUOM, OUTPUT dBoardWeightInCUOM).
         
        /* calculate the RM handling charge, per form based on form Gross Weight */
        IF dApplicableRMHandlingRate NE 0 THEN
        DO:    
            dRMHandlingCost = (dBoardWeightInCUOM * dApplicableRMHandlingRate).
            
            IF dRMHandlingCost NE 0 THEN
                RUN pAddEstMiscForHandling (BUFFER bf-ttEstCostForm, "RM", "Raw Mat'l Handling",dApplicableRMHandlingRate,"CWT", dRMHandlingCost).
        END.
        
        /* Calculate the Handling percentage as a percentage of all material based costs and Prep and Misc */
        FOR EACH bf-ttEstCostMaterial NO-LOCK 
            WHERE bf-ttEstCostMaterial.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostMaterial.estCostFormID   EQ bf-ttEstCostForm.estCostFormID:
                
                dCostTotalMaterial = dCostTotalMaterial + bf-ttEstCostMaterial.costTotal.
                 
        END.
        
        /* Include cost for Prep and any Material misc cost which is not Labour */
        FOR EACH bf-ttEstCostMisc NO-LOCK 
            WHERE bf-ttEstCostMisc.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostMisc.estCostFormID   EQ bf-ttEstCostForm.estCostFormID:
                
            IF bf-ttEstCostMisc.isPrep = YES 
            OR (bf-ttEstCostMisc.isPrep = NO AND bf-ttEstCostMisc.costType = "Mat") THEN
                dCostTotalMaterial = dCostTotalMaterial + bf-ttEstCostMisc.costTotal.
                
        END. /*Each ttEstCostMaterial for estHeader*/
        
        IF dApplicableHandlingPct NE 0 THEN
        DO:
            dHandlingCost = (dCostTotalMaterial * dApplicableHandlingPct).
            
            IF dHandlingCost NE 0 THEN
                RUN pAddEstMiscForHandling (BUFFER bf-ttEstCostForm, "RM", "Handling Charge",dApplicableHandlingPct,"%", dHandlingCost). 
        END.
        
        IF dFGHandlingCost NE 0 THEN 
            RUN pAddEstMiscForHandling (BUFFER bf-ttEstCostForm, "FG", "Finished Goods Handling",dApplicableFGHandlingRate,"CWT", dFGHandlingCost).
        
        
    END.
        
END PROCEDURE.

PROCEDURE pBuildHeadersToProcess PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate, build the headers and temp-table to process
     calculation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantityOverride AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-est             FOR est.
    DEFINE BUFFER bf-est-qty         FOR est-qty.
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQty AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstCostHeaderToCalc.
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipcCompany
        AND bf-est.est-no EQ ipcEstimateNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN RETURN.
    IF ipiQuantityOverride NE 0 THEN 
    DO:
        RUN pAddHeader(BUFFER bf-est, ipiQuantityOverride, 1, ipcJobID, ipiJobID2, OUTPUT opiEstCostHeaderID).
        CREATE ttEstCostHeaderToCalc.
        ASSIGN 
            ttEstCostHeaderToCalc.iEstCostHeaderID = opiEstCostHeaderID.
    END.
    ELSE 
    DO:
        FIND FIRST bf-est-qty
            WHERE bf-est-qty.company EQ bf-est.company
              AND bf-est-qty.est-no  EQ bf-est.est-no
              AND bf-est-qty.qty[1]  NE 0
             NO-LOCK NO-ERROR.
        IF AVAILABLE bf-est-qty THEN 
            DO iQtyCount = 1 TO 20:

                iQty = IF bf-est-qty.qty[iQtyCount] EQ 0 AND iQtyCount EQ 1 THEN bf-est-qty.eqty ELSE bf-est-qty.qty[iQtyCount].
                IF iQty NE 0 THEN 
                DO:
                    RUN pAddHeader(BUFFER bf-est, iQty, MAX(bf-est-qty.qty[iQtyCount + 20], 1), 
                        ipcJobID, ipiJobID2, OUTPUT opiEstCostHeaderID).
                    CREATE ttEstCostHeaderToCalc.
                    ASSIGN 
                        ttEstCostHeaderToCalc.iEstCostHeaderID = opiEstCostHeaderID.
                    FIND FIRST bf-ttEstCostHeader NO-LOCK 
                        WHERE bf-ttEstCostHeader.estCostHeaderID EQ opiEstCostHeaderID
                        NO-ERROR.
                    IF AVAILABLE bf-ttEstCostHeader AND fIsComboType(bf-ttEstCostHeader.estType) THEN LEAVE.
                END.
            END.
    END.

END PROCEDURE.

PROCEDURE pBuildProbe PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostHeader, make or update a probe record for display in Print tab
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    
    DEFINE           BUFFER bf-probe           FOR probe.
    DEFINE           BUFFER bf-probeit         FOR probeit.
    DEFINE           BUFFER bf-estimate-probe  FOR probe.
    
    DEFINE VARIABLE iProbeLine     AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyInM        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInMForItem AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPricePerM     AS DECIMAL NO-UNDO.
       
    DISABLE TRIGGERS FOR LOAD OF probe.
    
    IF ipbf-ttEstCostHeader.jobID NE "" THEN RETURN.
    
    dQtyInM    = ipbf-ttEstCostHeader.quantityMaster / 1000.
    
    FIND FIRST bf-probe EXCLUSIVE-LOCK 
        WHERE bf-probe.company EQ ipbf-ttEstCostHeader.company
        AND bf-probe.est-no EQ ipbf-ttEstCostHeader.estimateNo
        AND bf-probe.spare-char-2 EQ STRING(ipbf-ttEstCostHeader.estCostHeaderID)
        NO-ERROR.
    
    IF NOT AVAILABLE bf-probe THEN 
    DO:        
        FOR EACH bf-estimate-probe NO-LOCK 
            WHERE bf-estimate-probe.company EQ ipbf-ttEstCostHeader.company
            AND bf-estimate-probe.est-no EQ ipbf-ttEstCostHeader.estimateNo
            AND bf-estimate-probe.probe-date <> ?
            BY bf-estimate-probe.LINE DESCENDING:
            iProbeLine = bf-estimate-probe.LINE.
            LEAVE.
        END.
        iProbeLine = iProbeLine + 1.
        CREATE bf-probe.
        ASSIGN 
            bf-probe.company      = ipbf-ttEstCostHeader.company
            bf-probe.est-no       = ipbf-ttEstCostHeader.estimateNo
            bf-probe.line         = iProbeLine
            bf-probe.spare-char-2 = STRING(ipbf-ttEstCostHeader.estCostHeaderID)
            bf-probe.est-qty      = ipbf-ttEstCostHeader.quantityMaster        
            bf-probe.probe-date   = DATE(ipbf-ttEstCostHeader.calcDateTime)
            bf-probe.probe-time   = INTEGER(MTIME(ipbf-ttEstCostHeader.calcDateTime) / 1000)
            bf-probe.probe-user   = ipbf-ttEstCostHeader.calculatedBy
            bf-probe.freight      = ipbf-ttEstCostHeader.releaseCount             /* Holds Number of Releases */
            bf-probe.tot-lbs      = IF glUseGrossWeight THEN ipbf-ttEstCostHeader.weightTotal ELSE ipbf-ttEstCostHeader.weightNet
            .
            
        FOR EACH ttEstCostForm NO-LOCK 
            WHERE ttEstCostForm.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            :
            
            ASSIGN 
                bf-probe.gsh-qty    = bf-probe.gsh-qty + ttEstCostForm.grossQtyRequiredTotal
                bf-probe.gshQtyInSF = bf-probe.gshQtyInSF + (ttEstCostForm.grossQtyRequiredTotalArea * 1000 )
                .                  
        END.    
        FOR EACH ttEstCostBlank NO-LOCK 
            WHERE ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND ttEstCostBlank.formNo NE 0
            :
            ASSIGN 
                bf-probe.bsf = bf-probe.bsf + ttEstCostBlank.quantityPerSet * ttEstCostBlank.blankArea  / 144 //Refactor - assumes area is SQIN.
                .
        END.
    END.
    ASSIGN 
        bf-probe.fact-cost      = ipbf-ttEstCostHeader.costTotalFactory / dQtyInM
        bf-probe.full-cost      = ipbf-ttEstCostHeader.costTotalFull / dQtyInM
        bf-probe.gross-profit   = ipbf-ttEstCostHeader.profitPctGross
        bf-probe.net-profit     = ipbf-ttEstCostHeader.profitPctNet 
        dPricePerM              = ipbf-ttEstCostHeader.sellPrice / dQtyInM
        bf-probe.sell-price     = IF glRoundPriceToDollar THEN ROUND(dPricePerM, 0) ELSE ROUND(dPricePerM, 2)
        bf-probe.spare-dec-1    = ipbf-ttEstCostHeader.costTotalMaterial / dQtyInM
        bf-probe.boardCostTotal = ipbf-ttEstCostHeader.costTotalBoard
        bf-probe.boardCostPerM  = ipbf-ttEstCostHeader.costTotalBoard / dQtyInM
        .
    IF ipbf-ttEstCostHeader.sellPrice GT 0 THEN 
        bf-probe.boardCostPct  = ipbf-ttEstCostHeader.costTotalBoard / ipbf-ttEstCostHeader.sellPrice * 100.                
    FOR EACH ttEstCostItem NO-LOCK
        WHERE ttEstCostItem.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
        AND NOT ttEstCostItem.isSet:
            
        ASSIGN 
            dQtyInMForItem = ttEstCostItem.quantityRequired / 1000
            .
        FIND FIRST bf-probeit EXCLUSIVE-LOCK 
            WHERE bf-probeit.company EQ ipbf-ttEstCostHeader.company
            AND bf-probeit.est-no EQ ipbf-ttEstCostHeader.estimateNo
            AND bf-probeit.part-no EQ ttEstCostItem.customerPart
            AND bf-probeit.line EQ bf-probe.line
            NO-ERROR.            
        IF NOT AVAILABLE bf-probeit THEN 
        DO:
            CREATE bf-probeit.
            ASSIGN 
                bf-probeit.line    = bf-probe.line
                bf-probeit.bl-qty  = ttEstCostItem.quantityRequired
                bf-probeit.yld-qty = ttEstCostItem.quantityYielded
                bf-probeit.company = ttEstCostItem.company
                bf-probeit.cust-no = ttEstCostItem.customerID
                bf-probeit.est-no  = ttEstCostItem.estimateNo
                bf-probeit.part-no = ttEstCostItem.customerPart
                .
        END.
        ASSIGN
            dPricePerM            = ttEstCostItem.sellPrice / dQtyInMForItem
            bf-probeit.fact-cost  = ttEstCostItem.costTotalFactory / dQtyInMForItem
            bf-probeit.full-cost  = ttEstCostItem.costTotalFull / dQtyInMForItem
            bf-probeit.sell-price = IF glRoundPriceToDollar THEN ROUND(dPricePerM, 0) ELSE ROUND(dPricePerM, 2)
            bf-probeit.brd-cost   = ttEstCostItem.costTotalBoard / dQtyInMForItem
            .
        RELEASE bf-probeit.
            
    END.
    RELEASE bf-probe.
    
END PROCEDURE.

PROCEDURE pBuildFreightCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculate Freight Cost Details to add to Non Factory Costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostHeader          FOR ttEstCostHeader.
    DEFINE BUFFER bf-ttEstCostBlank           FOR ttEstCostBlank.
    DEFINE BUFFER bf-ttEstCostForm          FOR ttEstCostForm.
    DEFINE BUFFER bf-ttEstCostItem            FOR ttEstCostItem.
    DEFINE BUFFER bf-eb                     FOR eb.
    
    DEFINE BUFFER bfFirstBlank-ttEstCostBlank FOR ttEstCostBlank.
    DEFINE BUFFER bfComp-ttEstCostBlank       FOR ttEstCostBlank.
    
    FIND FIRST bf-ttEstCostHeader NO-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-ttEstCostHeader THEN RETURN.
    IF bf-ttEstCostHeader.isUnitizedSet THEN 
    DO:
        FOR FIRST bf-ttEstCostBlank NO-LOCK
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
            AND bf-ttEstCostBlank.formNo EQ 0
            AND bf-ttEstCostBlank.blankNo EQ 0,
            FIRST bf-ttEstCostForm NO-LOCK 
            WHERE bf-ttEstCostForm.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostForm.estCostFormID EQ bf-ttEstCostBlank.estCostFormID,
            FIRST bf-ttEstCostItem NO-LOCK 
            WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID,
            FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-ttEstCostBlank.company
            AND bf-eb.est-no EQ bf-ttEstCostBlank.estimateNo
            AND bf-eb.form-no EQ bf-ttEstCostBlank.formNo
            AND bf-eb.blank-no EQ bf-ttEstCostBlank.blankNo,
            FIRST bfFirstBlank-ttEstCostBlank NO-LOCK
            WHERE bfFirstBlank-ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
            AND bfFirstBlank-ttEstCostBlank.formNo EQ 1
            AND bfFirstBlank-ttEstCostBlank.blankNo EQ 1
            :
            RUN pBuildCostDetailForFreight(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, 
                BUFFER bf-ttEstCostBlank, BUFFER bf-ttEstCostItem, BUFFER bf-eb, 
                bfFirstBlank-ttEstCostBlank.estCostFormID, bfFirstBlank-ttEstCostBlank.estCostBlankID).
        END. /*Set Header Blank*/
    END.
    ELSE 
    DO:    
        FOR EACH bf-ttEstCostForm NO-LOCK
            WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
            FIRST bf-ttEstCostHeader NO-LOCK 
            WHERE bf-ttEstCostHeader.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID,
            EACH bf-ttEstCostBlank NO-LOCK 
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
            FIRST bf-ttEstCostItem NO-LOCK 
            WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID,
            FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-ttEstCostBlank.company
            AND bf-eb.est-no EQ bf-ttEstCostBlank.estimateNo
            AND bf-eb.form-no EQ bf-ttEstCostBlank.formNo
            AND bf-eb.blank-no EQ bf-ttEstCostBlank.blankNo
            :
            RUN pBuildCostDetailForFreight(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, 
                BUFFER bf-ttEstCostBlank, BUFFER bf-ttEstCostItem, BUFFER bf-eb, 
                bf-ttEstCostBlank.estCostFormID, bf-ttEstCostBlank.estCostBlankID).        
        END. /*Each blank*/
    END. /*Not unitized*/ 
    RUN pCalcCostTotals(ipiEstCostHeaderID, 0, NO).
        
END PROCEDURE.

PROCEDURE pBuildPriceRelatedCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes margin and profit and then calculates commission
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

    DEFINE VARIABLE dNetProfitForForm AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPriceForForm     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCommission       AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ttEstCostForm FOR ttEstCostForm.
    
    FOR EACH bf-ttEstCostForm NO-LOCK
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
          AND ttEstCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
        FIRST ttEstCostItem NO-LOCK 
        WHERE ttEstCostItem.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID 
          AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID:
        
        RUN pGetPriceProfitAndCommissionForForm(BUFFER bf-ttEstCostForm, BUFFER ttEstCostItem, OUTPUT dNetProfitForForm, OUTPUT dCommission, OUTPUT dPriceForForm).
        RUN pAddCostDetail(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, "", bf-ttEstCostForm.estCostFormID, 
            gcSourceTypeProfit, "pProfit", "Profit", dNetProfitForForm, 0, bf-ttEstCostForm.company, bf-ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, "" , bf-ttEstCostForm.estCostFormID,
            gcSourceTypeNonFactory,"commission","Commission", dCommission, 0, bf-ttEstCostForm.company, bf-ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail ).
        RUN pCalcCostTotals(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, NO).
        
    END.
     
 
END PROCEDURE.

PROCEDURE pCalcBoardCostFromBlank PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose: Calculate material dimension from blank
      Notes: If N-K-1 FoamCost = Blank then calculate Total Cost using eb dimenstions
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostMaterial FOR ttEstCostMaterial.
    
    DEFINE VARIABLE lFoam  AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostBlank    FOR ttEstCostBlank.
    
    IF NOT glCalcFoamCostFromBlank  THEN
        RETURN.
        
    lFoam = fIsFoamStyle (ipbf-ttEstCostForm.company, ipbf-ttEstCostForm.estimateNo, ipbf-ttEstCostForm.formNo).
        
    IF lFoam THEN
    DO:
        FIND FIRST bf-ttEstCostBlank NO-LOCK 
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostBlank.estCostFormID   EQ ipbf-ttEstCostForm.estCostFormID NO-ERROR.
        
        IF AVAILABLE bf-ttEstCostBlank THEN
            ASSIGN 
                opbf-ttEstCostMaterial.dimLength               = bf-ttEstCostBlank.blankLength
                opbf-ttEstCostMaterial.dimWidth                = bf-ttEstCostBlank.blankWidth
                opbf-ttEstCostMaterial.dimDepth                = bf-ttEstCostBlank.blankDepth
                opbf-ttEstCostMaterial.quantityRequiredNoWaste = bf-ttEstCostBlank.quantityRequired
                .
    END.  

END PROCEDURE.

PROCEDURE pBuildFreightForBoardCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Determine the Freight charges for Board Material
     Notes: This cost is processing for Single/combo estimates but not being included in case of Item-BOM setup.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ef                     FOR ef. 
    DEFINE BUFFER bf-ttEstCostForm          FOR ttEstCostForm. 
    DEFINE BUFFER bf-ttEstCostBlank         FOR ttEstCostBlank. 
    DEFINE BUFFER bf-ttEstCostHeader        FOR ttEstCostHeader.
    DEFINE BUFFER bf-ttEstCostItem          FOR ttEstCostItem. 
    
    
    DEFINE VARIABLE dBoardFreight   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInCUOM   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInSrcUOM AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cSrcUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPerMSheetUOM   AS CHARACTER NO-UNDO INIT "MSH".
    
   
    FOR FIRST bf-ttEstCostHeader NO-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH bf-ttEstCostForm NO-LOCK
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-ef NO-LOCK
        WHERE bf-ef.company = bf-ttEstCostForm.company
        AND bf-ef.est-no  = bf-ttEstCostForm.estimateNo
        AND bf-ef.form-no = bf-ttEstCostForm.formNo 
        AND bf-ef.fr-msh  NE 0:
            
        ASSIGN
            dQuantityInCUOM   = 0
            dBoardFreight     = 0
            dQuantityInSrcUOM = (IF bf-ef.fr-uom = cPerMSheetUOM THEN bf-ttEstCostForm.grossQtyRequiredTotal ELSE bf-ttEstCostForm.grossQtyRequiredTotalArea)
            cSrcUOM           = (IF bf-ef.fr-uom = cPerMSheetUOM THEN "EA" ELSE bf-ttEstCostForm.grossQtyRequiredTotalAreaUOM).
            
        
        IF bf-ef.fr-uom NE "" THEN
            RUN pConvertQuantityFromUOMToUOM(bf-ttEstCostForm.company, bf-ef.board, "RM", cSrcUOM, bf-ef.fr-uom, 
                bf-ttEstCostForm.basisWeight, bf-ttEstCostForm.grossLength, bf-ttEstCostForm.grossWidth, bf-ttEstCostForm.grossDepth, 
                dQuantityInSrcUOM, OUTPUT dQuantityInCUOM).
        
        IF dQuantityInCUOM NE 0 THEN
            dBoardFreight = dQuantityInCUOM *  bf-ef.fr-msh. 
                    
        IF dBoardFreight NE 0 THEN
           RUN pAddEstMiscForBoardFreight (BUFFER bf-ttEstCostForm, "FrtBrd", "Freight in Board Cost",bf-ef.fr-msh, bf-ef.fr-uom, dBoardFreight).
    
    END.
    
END PROCEDURE.

PROCEDURE pCalcHeaderCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculates all costs for the cost header based on operations and materials
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader.
    
    RUN pBuildFactoryCostDetails(ipiEstCostHeaderID).
    RUN pBuildNonFactoryCostDetails(ipiEstCostHeaderID).
    RUN pBuildFreightCostDetails(ipiEstCostHeaderID).
    RUN pBuildPriceRelatedCostDetails(ipiEstCostHeaderID).
    RUN pBuildCostSummary(ipiEstCostHeaderID).
    
    FIND FIRST bf-ttEstCostHeader NO-LOCK
        WHERE bf-ttEstCostHeader.estCostHeaderID = ipiEstCostHeaderID NO-ERROR. 
        
    IF AVAILABLE bf-ttEstCostHeader THEN
    DO:
        RUN pCopyHeaderCostsToSetItem(BUFFER bf-ttEstCostHeader).
        RUN pBuildProbe(BUFFER bf-ttEstCostHeader).
    END.

END PROCEDURE.

PROCEDURE pCalcEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Internal master procedure for calculating an estimate/job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantity AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPrompt AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INT64 NO-UNDO.
    
    RUN pSetGlobalSettings(ipcCompany). 
    RUN pBuildSystemData(ipcCompany). 
    RUN est\OperationProcs.p PERSISTENT SET ghOperation.
    RUN est\RecostBoardEst.p PERSISTENT SET ghRecostBoardEst.
     
    IF iplPurge THEN 
        RUN pPurgeCalculation(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2).
        
    RUN pBuildHeadersToProcess(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2, ipiQuantity, OUTPUT opiEstCostHeaderID).
    
    FOR EACH ttEstCostHeaderToCalc: 
        RUN pCalcHeader(ttEstCostHeaderToCalc.iEstCostHeaderID).
    END.
    
    IF iplPrompt THEN 
        RUN pPromptForCalculationChanges.
        
    IF iplPrompt AND giPromptForErrorLevel GT 0 
    AND CAN-FIND(FIRST ttEstError WHERE ttEstError.iErrorLevel LE giPromptForErrorLevel) THEN 
        RUN est/estCalcErrorList.w(INPUT TABLE ttEstError,giPromptForErrorLevel).     
    
    RUN pWriteDatasetIntoDB.
    
    IF VALID-HANDLE(ghOperation) THEN
        DELETE PROCEDURE ghOperation.
        
    IF VALID-HANDLE(ghRecostBoardEst) THEN
        DELETE PROCEDURE ghRecostBoardEst.

END PROCEDURE.

PROCEDURE pCalcHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Main Build of data from Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostForm       FOR ttEstCostForm.
    DEFINE BUFFER bf-ttEstCostBlank      FOR ttEstCostBlank.
    DEFINE BUFFER bf-ttEstCostMaterial   FOR ttEstCostMaterial.
    DEFINE BUFFER bf-ttEstCostOperation  FOR ttEstCostOperation.
    DEFINE BUFFER bf-ttEstCostHeader     FOR ttEstCostHeader.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyOnForm          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyOnFormRequired  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyOnFormYielded   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyMaster          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE chMessage           AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
         
    FOR EACH bf-ttEstCostHeader NO-LOCK
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ bf-ttEstCostHeader.company
            AND est.est-no EQ bf-ttEstCostHeader.estimateNo
            NO-ERROR.
        
        IF NOT AVAILABLE est THEN RETURN.

        RUN pBuildItems(BUFFER bf-ttEstCostHeader).
        dQtyMaster          = 0.
        
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no
            BY ef.form-no DESCENDING:
            
            RUN pAddEstFormFromEf(BUFFER ef, BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).
            
            ASSIGN 
                iNumOutBlanksOnForm = 0
                dQtyOnForm          = 0
                dQtyOnFormRequired  = 0
                dQtyOnFormYielded   = 0
                .
            
            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-ttEstCostBlank).
                ASSIGN 
                    iNumOutBlanksOnForm = iNumOutBlanksOnForm + bf-ttEstCostBlank.numOut
                    dQtyOnForm          = dQtyOnForm + 
                                        (IF bf-ttEstCostBlank.priceBasedOnYield THEN bf-ttEstCostBlank.quantityYielded ELSE bf-ttEstCostBlank.quantityRequired)
                    dQtyOnFormRequired  = dQtyOnFormRequired + bf-ttEstCostBlank.quantityRequired
                    dQtyOnFormYielded   = dQtyOnFormYielded + bf-ttEstCostBlank.quantityYielded
                    .
                RUN pBuildInksForEb(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostBlank, BUFFER eb).
                RUN pAddGlue(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostBlank, BUFFER eb).
                RUN pBuildPackingForEb(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostBlank, BUFFER eb).
                
            END. /*Each eb of ef*/
            
            ASSIGN 
                bf-ttEstCostForm.numOut                   = iNumOutBlanksOnForm * bf-ttEstCostForm.numOutNet
                bf-ttEstCostForm.quantityFGOnFormRequired = dQtyOnFormRequired
                bf-ttEstCostForm.quantityFGOnFormYielded  = dQtyOnFormYielded
                bf-ttEstCostForm.quantityFGOnForm         = dQtyOnForm
                dQtyMaster                              = dQtyMaster + dQtyOnForm
                bf-ttEstCostForm.grossQtyRequiredNoWaste  = fRoundUp(bf-ttEstCostForm.quantityFGOnForm / bf-ttEstCostForm.numOut)
                .
            
            RUN pCalcBlankPct(BUFFER bf-ttEstCostForm).                
            RUN pProcessOperations(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).
            IF AVAILABLE bf-ttEstCostBlank AND bf-ttEstCostBlank.isPurchased THEN 
                RUN pProcessFarm(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-ttEstCostBlank ).
            ELSE DO: 
                RUN pProcessLeafs(BUFFER ef, BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).
                RUN pProcessBoard(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, BUFFER ef).      
                RUN pProcessAdders(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm, ef.adder).   
                RUN pProcessInks(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).
                RUN pProcessGlues(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).
            END.
            RUN pProcessSpecialMaterials(BUFFER ef, BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostForm).  
            
            RUN pProcessMiscPrep(BUFFER ef, BUFFER bf-ttEstCostForm, bf-ttEstCostHeader.quantityMaster).
            RUN pProcessMiscNonPrep(BUFFER ef, BUFFER bf-ttEstCostForm).
                      
        END.  /*Each ef of est*/  
        /* if combo, update the master quantity for per M calculations*/
        IF fIsComboType(bf-ttEstCostHeader.estType) THEN 
        DO:
            FIND CURRENT bf-ttEstCostHeader EXCLUSIVE-LOCK.
            bf-ttEstCostHeader.quantityMaster = dQtyMaster.
            FIND CURRENT bf-ttEstCostHeader NO-LOCK.
        END.                            
       
        IF glAutoRecostBoard = TRUE THEN
        DO:
            RUN RecostBoardEst_RecostBoard IN ghRecostBoardEst(INPUT TABLE ttEstCostHeaderToCalc,
                INPUT TABLE ttEstCostMaterial,
                INPUT TABLE ttEstCostHeader,
                OUTPUT chMessage,
                OUTPUT TABLE ttRecostBoardGroups).
                
            IF chMessage = "" THEN
            DO:  
                RUN RecostBoardEst_UpdateEstCostMaterial IN ghRecostBoardEst(INPUT NO,
                    INPUT-OUTPUT TABLE ttEstCostMaterial BY-REFERENCE,
                    INPUT TABLE ttRecostBoardGroups). /*Update the EstCostMaterial costs with better costs*/
            END.
        END.
        
        RUN pProcessPacking(BUFFER bf-ttEstCostHeader).
        RUN pProcessEstMaterial(BUFFER bf-ttEstCostHeader).
        RUN pCalcWeightsAndSizes(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildFreightForBoardCost(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildEstHandlingCharges(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildFactoryCostDetails(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildNonFactoryCostDetails(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildFreightCostDetails(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildPriceRelatedCostDetails(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildCostSummary(bf-ttEstCostHeader.estCostHeaderID).
        RUN pCopyHeaderCostsToSetItem(BUFFER bf-ttEstCostHeader).
        RUN pUpdateCostDetails(bf-ttEstCostHeader.estCostHeaderID).
        RUN pBuildProbe(BUFFER bf-ttEstCostHeader).
    END. /*each bf-ttEstCostHeader*/

END PROCEDURE.

PROCEDURE pBuildNonFactoryCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds "cost" detail for all markups/non-factory costs
     Notes:  Existing functionality "builds" up the totals and continues to take 
     percentages of the built up total (Warehousing & Folding include Direct Material Markup and GS&A totals).  
     I don't think this is right so only taking
     totals of static values.  The order in which % calculate should not affect the 
     % markups
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ce-ctrl         FOR ce-ctrl.
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    
    DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCostStorage  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostHandling AS DECIMAL NO-UNDO.
    
    FOR EACH bf-ttEstCostHeader EXCLUSIVE-LOCK
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID, 
        FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ bf-ttEstCostHeader.company
        AND bf-ce-ctrl.loc EQ bf-ttEstCostHeader.warehouseID:
            
        
        IF bf-ttEstCostHeader.directMaterialPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfMatMarkup", "Direct Material Markup", ttEstCostForm.costTotalMaterial * bf-ttEstCostHeader.directMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        DO iIndex = 1 TO 6:
            bf-ttEstCostHeader.gsaMaterialPct = bf-ce-ctrl.mat-pct[iIndex] / 100.
            IF bf-ce-ctrl.mat-cost[iIndex] GT ttEstCostForm.costTotalMaterial THEN LEAVE.
        END. 
        IF bf-ttEstCostHeader.gsaMaterialPct NE 0 THEN 
        DO:
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSAMat", "GSA Material", (ttEstCostForm.costTotalMaterial - ttEstCostForm.costTotalBoard) * bf-ttEstCostHeader.gsaMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSABoard", "GSA Board", ttEstCostForm.costTotalBoard * bf-ttEstCostHeader.gsaMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        END.
        DO iIndex = 1 TO 6:
            bf-ttEstCostHeader.gsaLaborPct = bf-ce-ctrl.lab-pct[iIndex] / 100.
            IF bf-ce-ctrl.lab-cost[iIndex] GT ttEstCostForm.costTotalLabor THEN LEAVE.
        END. 
        IF bf-ttEstCostHeader.gsaLaborPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSALab", "GSA Labor", ttEstCostForm.costTotalLabor * bf-ttEstCostHeader.gsaLaborPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        
        IF bf-ttEstCostHeader.warehouseMarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", ttEstCostForm.costTotalFactory * bf-ttEstCostHeader.warehouseMarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        
        FOR EACH ttEstCostBlank NO-LOCK 
            WHERE ttEstCostBlank.estCostHeaderID EQ ttEstCostForm.estCostHeaderID
            AND ttEstCostBlank.estCostFormID EQ ttEstCostForm.estCostFormID:
            RUN GetStorageAndHandlingForEstimateBlank(ttEstCostBlank.company, ttEstCostBlank.estimateNo, ttEstCostBlank.quantityRequired, 
                ttEstCostBlank.formNo, ttEstCostBlank.blankNo,
                OUTPUT dCostStorage, OUTPUT dCostHandling).
            IF dCostStorage NE 0 THEN 
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, ttEstCostBlank.estCostBlankID, ttEstCostBlank.estCostBlankID, 
                    gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", dCostStorage, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
            IF dCostHandling NE 0 THEN 
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, ttEstCostBlank.estCostBlankID, ttEstCostBlank.estCostBlankID, 
                    gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", dCostHandling, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        END.
        
        /*Note - currently a defect with the Folding % is zeroed out during the calc process - this is fix*/
        IF bf-ttEstCostHeader.foldPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfFolding", "Folding", ttEstCostForm.costTotalFactory * bf-ttEstCostHeader.foldPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).      
        
                                
        IF bf-ttEstCostHeader.special1MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", ttEstCostForm.costTotalFactory * bf-ttEstCostHeader.special1MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).    
        IF bf-ttEstCostHeader.special2MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", ttEstCostForm.costTotalFactory * bf-ttEstCostHeader.special2MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        IF bf-ttEstCostHeader.special3MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", ttEstCostForm.costTotalFactory * bf-ttEstCostHeader.special3MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", bf-ttEstCostHeader.special1FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).    
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", bf-ttEstCostHeader.special2FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", bf-ttEstCostHeader.special3FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).                                                                                                   
    END. /*Each bf-ttEstCostHeader*/    
    RUN pCalcCostTotals(ipiEstCostHeaderID, 0, NO).
    RELEASE bf-ttEstCostHeader.
    
END PROCEDURE.

PROCEDURE pBuildCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.

    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opSetupDL","Operation Setup DL",
        ipbf-ttEstCostOperation.costTotalDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opSetupVO","Operation Setup VOH",
        ipbf-ttEstCostOperation.costTotalVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opSetupFO","Operation Setup FOH",
        ipbf-ttEstCostOperation.costTotalFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opRunDL","Operation Run DL",
        ipbf-ttEstCostOperation.costTotalDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opRunVO","Operation Run VOH",
        ipbf-ttEstCostOperation.costTotalVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opRunFO","Operation Run FOH",
        ipbf-ttEstCostOperation.costTotalFORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opSetupMinDiff","Operation Setup - Min Charge Diff",
        ipbf-ttEstCostOperation.costTotalMinDiffSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstCostOperation, "opRunMinDiff","Operation Run - Min Charge Diff",
        ipbf-ttEstCostOperation.costTotalMinDiffRun,0).
            
END PROCEDURE.

PROCEDURE pBuildFactoryCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the cost detail for all Factory Costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    /*Process Operations*/
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForOperation(BUFFER ttEstCostOperation).
    END. /*Each ttEstCostOperation for estHeader*/    
    /*Process Materials*/
    FOR EACH ttEstCostMaterial NO-LOCK 
        WHERE ttEstCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMaterial(BUFFER ttEstCostMaterial).                  
                    
    END. /*Each ttEstCostMaterial for estHeader*/
    
    FOR EACH ttEstCostMisc NO-LOCK 
        WHERE ttEstCostMisc.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMisc(BUFFER ttEstCostMisc).                  
    END. /*Each ttEstCostMaterial for estHeader*/
    RUN pCalcCostTotals(ipiEstCostHeaderID, 0, NO).
    
END PROCEDURE.

PROCEDURE pBuildPackingForEb PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all packing infor for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank  FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.
    
    DEFINE           BUFFER bf-ttPack          FOR ttPack.
    DEFINE           BUFFER bf-ce-ctrl         FOR ce-ctrl.
    
    DEFINE VARIABLE dLayerDepth   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDividerDepth AS DECIMAL   NO-UNDO.        
    DEFINE VARIABLE dPackQty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrapID      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dStrapQty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrapQtyUOM  AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ipbf-ttEstCostHeader.company
        NO-ERROR.
    
    IF ipbf-ttEstCostHeader.isUnitizedSet AND ipbf-ttEstCostBlank.formNo NE 0 THEN 
        RETURN.   /*Ignore non-form 0 packing for unitized set*/
    
    /*Case*/
    RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, ipbf-eb.cas-no, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
    IF AVAILABLE bf-ttPack THEN 
        ASSIGN 
            bf-ttPack.dDimLength          = IF ipbf-eb.cas-len NE 0 THEN ipbf-eb.cas-len ELSE bf-ttPack.dDimLength
            bf-ttPack.dDimWidth           = IF ipbf-eb.cas-wid NE 0 THEN ipbf-eb.cas-wid ELSE bf-ttPack.dDimWidth
            bf-ttPack.dDimDepth           = IF ipbf-eb.cas-dep NE 0 THEN ipbf-eb.cas-dep ELSE bf-ttPack.dDimDepth
            bf-ttPack.iCountPerSubUnit    = IF ipbf-eb.cas-cnt EQ 0 AND ipbf-eb.cas-wt EQ 0 THEN bf-ttPack.iCountPerSubUnit ELSE ipbf-eb.cas-cnt
            bf-ttPack.dWeightCapacity     = IF ipbf-eb.cas-cnt EQ 0 AND ipbf-eb.cas-wt EQ 0 THEN bf-ttPack.dWeightCapacity ELSE ipbf-eb.cas-wt
            bf-ttPack.dCostPerUOMOverride = ipbf-eb.cas-cost
            bf-ttPack.dQtyMultiplier      = MAX(ipbf-eb.spare-int-3, 1)
            bf-ttPack.lIsCase             = YES
            bf-ttPack.lNoCharge           = ipbf-eb.casNoCharge
            bf-ttPack.dWeightTare         = IF bf-ttPack.dWeightTare EQ 0 AND AVAILABLE bf-ce-ctrl THEN bf-ce-ctrl.def-cas-w ELSE bf-ttPack.dWeightTare
            .
            
    RELEASE bf-ttPack.
     
    /*Pallet*/
    RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, ipbf-eb.tr-no, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
    IF AVAILABLE bf-ttPack THEN 
        ASSIGN 
            bf-ttPack.dDimLength            = IF ipbf-eb.tr-len NE 0 THEN ipbf-eb.tr-len ELSE bf-ttPack.dDimLength
            bf-ttPack.dDimWidth             = IF ipbf-eb.tr-wid NE 0 THEN ipbf-eb.tr-wid ELSE bf-ttPack.dDimWidth
            bf-ttPack.dDimDepth             = IF ipbf-eb.tr-dep NE 0 THEN ipbf-eb.tr-dep ELSE bf-ttPack.dDimDepth
            bf-ttPack.iCountPerUnit         = IF ipbf-eb.tr-cnt NE 0 THEN ipbf-eb.tr-cnt ELSE bf-ttPack.iCountPerUnit
            bf-ttPack.iCountSubUnitsPerUnit = ipbf-eb.cas-pal
            bf-ttPack.dCostPerUOMOverride   = ipbf-eb.tr-cost
            bf-ttPack.dQtyMultiplier        = 1
            bf-ttPack.lIsPallet             = YES
            bf-ttPack.lNoCharge             = ipbf-eb.trNoCharge
            bf-ttPack.dWeightTare           = IF bf-ttPack.dWeightTare EQ 0 AND AVAILABLE bf-ce-ctrl THEN bf-ce-ctrl.def-pal-w ELSE bf-ttPack.dWeightTare
            .
            
    RELEASE bf-ttPack.       
     
    RUN pGetLayerDividerDepth(ipbf-eb.company, ipbf-eb.est-no, ipbf-eb.form-no, ipbf-eb.blank-no,
        OUTPUT dLayerDepth, OUTPUT dDividerDepth).
     
    /*LayerPad*/
    RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, ipbf-eb.layer-pad, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
    IF AVAILABLE bf-ttPack THEN 
        ASSIGN 
            bf-ttPack.dDimLength        = IF ipbf-eb.lp-len NE 0 THEN ipbf-eb.lp-len ELSE bf-ttPack.dDimLength
            bf-ttPack.dDimWidth         = IF ipbf-eb.lp-wid NE 0 THEN ipbf-eb.lp-wid ELSE bf-ttPack.dDimWidth
            bf-ttPack.dDimDepth         = IF dLayerDepth NE 0 THEN dLayerDepth ELSE bf-ttPack.dDimDepth
            bf-ttPack.dQtyMultiplier    = MAX(ipbf-eb.lp-up, 1)
            bf-ttPack.cQtyMultiplierPer = ipbf-eb.spare-char-3
            .      
    RELEASE bf-ttPack.
    
    /*Divider*/
    RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, ipbf-eb.divider, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
    IF AVAILABLE bf-ttPack THEN 
        ASSIGN 
            bf-ttPack.dDimLength        = IF ipbf-eb.div-len NE 0 THEN ipbf-eb.div-len ELSE bf-ttPack.dDimLength
            bf-ttPack.dDimWidth         = IF ipbf-eb.div-wid NE 0 THEN ipbf-eb.div-wid ELSE bf-ttPack.dDimWidth
            bf-ttPack.dDimDepth         = IF dDividerDepth NE 0 THEN dDividerDepth ELSE bf-ttPack.dDimDepth
            bf-ttPack.dQtyMultiplier    = MAX(ipbf-eb.div-up, 1)
            bf-ttPack.cQtyMultiplierPer = ipbf-eb.spare-char-4
            .      
    RELEASE bf-ttPack.           
    
    RUN pGetStrapping(BUFFER ipbf-eb, OUTPUT cStrapID, OUTPUT dStrapQty, OUTPUT cStrapQtyUOM).
    IF cStrapID NE "" THEN 
    DO:
        /*Strapping*/
        RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, cStrapID, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
        IF AVAILABLE bf-ttPack THEN 
            ASSIGN 
                bf-ttPack.dQtyMultiplier    = dStrapQty
                bf-ttPack.cQtyUOM           = cStrapQtyUOM
                bf-ttPack.cQtyMultiplierPer = "P"
                bf-ttPack.lNoCharge         = ipbf-eb.trNoCharge
                .      
        RELEASE bf-ttPack.           
    END.
    
    FOR EACH estPacking NO-LOCK 
        WHERE estPacking.company EQ ipbf-eb.company
        AND estPacking.estimateNo EQ ipbf-eb.est-no
        AND estPacking.formNo EQ ipbf-eb.form-no
        AND estPacking.blankNo EQ ipbf-eb.blank-no:
        
        RUN pAddPacking(BUFFER ipbf-ttEstCostBlank, estPacking.rmItemID, BUFFER bf-ttPack, ipbf-ttEstCostHeader.quantityMaster).
        IF AVAILABLE bf-ttPack THEN 
            ASSIGN 
                bf-ttPack.dDimLength          = IF estPacking.dimLength NE 0 THEN estPacking.dimLength ELSE bf-ttPack.dDimLength
                bf-ttPack.dDimWidth           = IF estPacking.dimWidth NE 0 THEN estPacking.dimWidth ELSE bf-ttPack.dDimWidth
                bf-ttPack.dDimDepth           = IF estPacking.dimDepth NE 0 THEN estPacking.dimDepth ELSE bf-ttPack.dDimDepth
                bf-ttPack.cDimUOM             = estPacking.dimUOM
                bf-ttPack.dQtyMultiplier      = MAX(estPacking.quantity, 1)
                bf-ttPack.cQtyMultiplierPer   = estPacking.quantityPer
                bf-ttPack.dCostPerUOMOverride = estPacking.costOverridePerUOM
                bf-ttPack.lNoCharge           = estPacking.noCharge
                .      
        RELEASE bf-ttPack.           
    END.
/*Add corrugated banding calculation here*/
    
END PROCEDURE.

PROCEDURE pBuildInksForEb PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all inks for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank  FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    IF ipbf-ttEstCostHeader.industry EQ gcIndustryFolding THEN
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code2):
        IF ipbf-eb.i-code2[iIndex] GT "" THEN
            RUN pAddInk(BUFFER ipbf-ttEstCostBlank, ipbf-eb.i-ps2[iIndex], ipbf-eb.i-code2[iIndex], ipbf-eb.i-dscr2[iIndex], ipbf-eb.i-%2[iIndex], ipbf-eb.inkNoCharge, ipbf-ttEstCostHeader.quantityMaster).
    END.
    ELSE
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code):
        IF ipbf-eb.i-code[iIndex] GT "" THEN    
            RUN pAddInk(BUFFER ipbf-ttEstCostBlank, ipbf-eb.i-ps[iIndex], ipbf-eb.i-code[iIndex], ipbf-eb.i-dscr[iIndex], ipbf-eb.i-%[iIndex], ipbf-eb.inkNoCharge, ipbf-ttEstCostHeader.quantityMaster).
    END.

END PROCEDURE.

PROCEDURE pBuildLeafForEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all inks for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef              FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.
    
    DEFINE BUFFER bf-est-flm FOR est-flm.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    IF CAN-FIND(FIRST est-flm WHERE est-flm.company EQ ef.company AND est-flm.est-no EQ ef.est-no AND est-flm.snum EQ ef.form-no) THEN 
    DO:
        iIndex = 0.
        FOR EACH bf-est-flm NO-LOCK 
            WHERE bf-est-flm.company EQ ef.company
            AND bf-est-flm.est-no EQ ef.est-no
            AND bf-est-flm.snum EQ ef.form-no
            BY bf-est-flm.bnum:
            iIndex = iIndex + 1.
            RUN pAddLeaf(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, bf-est-flm.i-no, bf-est-flm.dscr, bf-est-flm.bnum, iIndex, bf-est-flm.len, bf-est-flm.wid).
        END.    
    END.
    ELSE 
    DO iIndex = 1 TO 4:
        IF ipbf-ef.leaf[iIndex] NE "" THEN
            RUN pAddLeaf(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.leaf[iIndex], ipbf-ef.leaf-dscr[iIndex], ipbf-ef.leaf-bnum[iIndex], iIndex, ipbf-ef.leaf-l[iIndex], ipbf-ef.leaf-w[iIndex]).
    END.
    

END PROCEDURE.

PROCEDURE pBuildItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.

    DEFINE           BUFFER bf-ttEstCostItem     FOR ttEstCostItem.
    DEFINE           BUFFER bf-ttEstCostForm     FOR ttEstCostForm.
    DEFINE           BUFFER bf-ttEstCostBlank    FOR ttEstCostBlank.
    
    DEFINE VARIABLE iEstItemIDSetHeader AS INT64 NO-UNDO.
    
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-ttEstCostHeader.company
        AND eb.est-no EQ ipbf-ttEstCostHeader.estimateNo
        BY eb.form-no DESCENDING: 
        FIND FIRST bf-ttEstCostItem EXCLUSIVE-LOCK
            WHERE bf-ttEstCostItem.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND bf-ttEstCostItem.customerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstCostItem OR eb.form-no EQ 0 THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostItem).
            IF eb.form-no EQ 0 THEN 
            DO:
                RUN pAddEstForm(BUFFER ipbf-ttEstCostHeader, 0, BUFFER bf-ttEstCostForm).
                RUN pAddEstBlank(BUFFER eb, BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-ttEstCostBlank).
                
                IF AVAILABLE bf-ttEstCostForm THEN 
                    bf-ttEstCostForm.quantityFGOnForm = ipbf-ttEstCostHeader.quantityMaster.
                IF eb.pur-man THEN /*Refactor - this should be .unitized*/
                DO:
                    FIND CURRENT ipbf-ttEstCostHeader EXCLUSIVE-LOCK.
                    ipbf-ttEstCostHeader.isUnitizedSet = YES.
                    FIND CURRENT ipbf-ttEstCostHeader NO-LOCK. 
                    RUN pBuildPackingForEb(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostBlank, BUFFER eb).
                END.
                ASSIGN 
                    bf-ttEstCostItem.isSet              = YES
                    bf-ttEstCostItem.quantityPerSet     = 0
                    bf-ttEstCostItem.blankArea          = 0
                    bf-ttEstCostItem.blankAreaNetWindow = 0
                    bf-ttEstCostItem.blankAreaWindow    = 0
                    iEstItemIDSetHeader               = bf-ttEstCostItem.estCostItemID
                    .
                RELEASE bf-ttEstCostForm.
                RELEASE bf-ttEstCostBlank.
            END.     
            ELSE 
                bf-ttEstCostItem.estCostItemIDParent = iEstItemIDSetHeader.           
        END. /*Create ttEstCostItem*/
        ELSE 
            ASSIGN 
                bf-ttEstCostItem.quantityRequired = bf-ttEstCostItem.quantityRequired + eb.bl-qty
                bf-ttEstCostItem.quantityYielded  = bf-ttEstCostItem.quantityYielded + eb.yld-qty
                bf-ttEstCostItem.quantityPerSet   = IF fIsSetType(ipbf-ttEstCostHeader.estType) THEN bf-ttEstCostItem.quantityPerSet + fGetQuantityPerSet(BUFFER eb) ELSE 1.
            .      
        RELEASE bf-ttEstCostItem.
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pCalcBlankPct PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculates the share of that the blank will have on the form to 
     proportinately allocate form costs to each blank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-ttEstCostBlank  FOR ttEstCostBlank.
    
    DEFINE VARIABLE dTotalBlankAreaOnForm AS DECIMAL.
    
    IF ipbf-ttEstCostForm.blankArea GT 0 THEN 
        FOR EACH bf-ttEstCostBlank EXCLUSIVE-LOCK 
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
            AND bf-ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID:
   
            bf-ttEstCostBlank.pctOfForm = bf-ttEstCostBlank.blankArea * bf-ttEstCostBlank.numOut / ipbf-ttEstCostForm.blankArea. 
        END. 
    RELEASE bf-ttEstCostBlank.
        
END PROCEDURE.

PROCEDURE pCalcCostTotalsForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and category buffer, increment the appropriate fields
     on the form for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotals.i &TableName=ttEstCostForm}
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a header and category buffer, increment the appropriate fields
     on the form for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotals.i &TableName=ttEstCostHeader}
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a item and category buffer, increment the appropriate fields
     on the item for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotals.i &TableName=ttEstCostItem}

END PROCEDURE.

PROCEDURE pCalcWeightsAndSizes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estCostHeaderID, calculate weight for all blanks, items, forms
     and header based on weight of materials flagged for inclusion
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostBlank    FOR ttEstCostBlank.
    DEFINE BUFFER bf-ttEstCostItem     FOR ttEstCostItem.
    DEFINE BUFFER bfSet-ttEstCostItem  FOR ttEstCostItem.
    DEFINE BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE BUFFER bf-ttEstCostHeader   FOR ttEstCostHeader.
    
    DEFINE VARIABLE dWeightInDefaultUOM         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cMaterialItemType           AS CHARACTER NO-UNDO.
    
    FOR EACH bf-ttEstCostBlank NO-LOCK 
        WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-ttEstCostItem EXCLUSIVE-LOCK
        WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostBlank.estCostHeaderID
        AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID:
                
                       
        FOR EACH bf-ttEstCostMaterial EXCLUSIVE-LOCK 
            WHERE bf-ttEstCostMaterial.estCostHeaderID EQ bf-ttEstCostBlank.estCostHeaderID
            AND bf-ttEstCostMaterial.estCostFormID EQ bf-ttEstCostBlank.estCostFormID
            AND (bf-ttEstCostMaterial.addToWeightNet OR bf-ttEstCostMaterial.addToWeightTare)
            AND (bf-ttEstCostMaterial.estCostBlankID EQ bf-ttEstCostBlank.estCostBlankID OR bf-ttEstCostMaterial.estCostBlankID EQ 0):
            
            IF bf-ttEstCostMaterial.weightTotal = 0 THEN DO:  //Weight may have been calculated already
                IF bf-ttEstCostMaterial.isPurchasedFG THEN 
                    cMaterialItemType = "FG".
                ELSE
                    cMaterialItemTYpe = "RM".
                RUN pConvertQuantityFromUOMToUOM(bf-ttEstCostMaterial.company, bf-ttEstCostMaterial.itemID, cMaterialItemType, bf-ttEstCostMaterial.quantityUOM, bf-ttEstCostMaterial.weightUOM, 
                    bf-ttEstCostMaterial.basisWeight, bf-ttEstCostMaterial.dimLength, bf-ttEstCostMaterial.dimWidth, bf-ttEstCostMaterial.dimDepth, bf-ttEstCostMaterial.quantityRequiredNoWaste, 
                    OUTPUT bf-ttEstCostMaterial.weightTotal).                                 
            END.
            
            dWeightInDefaultUOM = 0.            
            IF bf-ttEstCostMaterial.estCostBlankID EQ 0 AND NOT bf-ttEstCostMaterial.isPrimarySubstrate THEN /*Pro-rate Weight based on pctOfForm - board already done by blank above*/
                dWeightInDefaultUOM = bf-ttEstCostMaterial.weightTotal * bf-ttEstCostBlank.pctOfForm.
            ELSE IF bf-ttEstCostMaterial.isPrimarySubstrate THEN //Calculate based on formula square inches (net) per blank
                dWeightInDefaultUOM = fGetMSF(bf-ttEstCostBlank.blankAreaNetWindow, bf-ttEstCostBlank.areaUOM) * bf-ttEstCostMaterial.basisWeight * bf-ttEstCostBlank.quantityRequired.  
            ELSE /*Blank specific material - get all weight*/
                dWeightInDefaultUOM = bf-ttEstCostMaterial.weightTotal.

            IF bf-ttEstCostMaterial.weightUOM NE gcDefaultWeightUOM THEN 
            DO:
                    //REFACTOR: Convert to default weight UOM
            END.
            IF bf-ttEstCostMaterial.addToWeightNet OR bf-ttEstCostMaterial.addToWeightTare THEN 
            DO:
                IF bf-ttEstCostMaterial.addToWeightNet THEN 
                    bf-ttEstCostItem.weightNet   = bf-ttEstCostItem.weightNet + dWeightInDefaultUOM.
                IF bf-ttEstCostMaterial.addToWeightTare THEN 
                    bf-ttEstCostItem.weightTare  = bf-ttEstCostItem.weightTare + dWeightInDefaultUOM.
                bf-ttEstCostItem.weightTotal = bf-ttEstCostItem.weightTotal + dWeightInDefaultUOM.
            END.
        END.  /*Each material for blank*/       
    END. /*each blank for header*/
    
    /*Calculate Header Weight*/
    FOR FIRST bf-ttEstCostHeader EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH bf-ttEstCostItem EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID:

        ASSIGN 
            bf-ttEstCostHeader.weightTotal = bf-ttEstCostHeader.weightTotal + bf-ttEstCostItem.weightTotal
            bf-ttEstCostHeader.weightNet   = bf-ttEstCostHeader.weightNet + bf-ttEstCostItem.weightNet
            bf-ttEstCostHeader.weightTare  = bf-ttEstCostHeader.weightTare + bf-ttEstCostItem.weightTare
            .
    END.
    /*Apply Header Weight to Set Header Item*/
    FOR FIRST bf-ttEstCostHeader NO-LOCK
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        AND bf-ttEstCostHeader.isUnitizedSet,
        FIRST bf-ttEstCostItem EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
        AND bf-ttEstCostItem.isSet:

        ASSIGN 
            bf-ttEstCostItem.weightTotal = bf-ttEstCostHeader.weightTotal
            bf-ttEstCostItem.weightNet   = bf-ttEstCostHeader.weightNet
            bf-ttEstCostItem.weightTare  = bf-ttEstCostHeader.weightTare
            .
    END. 
    /*Calc Total MSF for a Set*/
    FOR FIRST bf-ttEstCostHeader NO-LOCK
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        AND bf-ttEstCostHeader.isUnitizedSet,
        FIRST bfSet-ttEstCostItem EXCLUSIVE-LOCK 
        WHERE bfSet-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
        AND bfSet-ttEstCostItem.isSet,
        EACH bf-ttEstCostItem NO-LOCK
        WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID:
        ASSIGN 
            bfSet-ttEstCostItem.blankArea          = bfSet-ttEstCostItem.blankArea + bf-ttEstCostItem.blankArea * bf-ttEstCostItem.quantityPerSet
            bfSet-ttEstCostItem.blankAreaNetWindow = bfSet-ttEstCostItem.blankAreaNetWindow + bf-ttEstCostItem.blankAreaNetWindow * bf-ttEstCostItem.quantityPerSet
            bfSet-ttEstCostItem.blankAreaWindow    = bfSet-ttEstCostItem.blankAreaWindow + bf-ttEstCostItem.blankAreaWindow * bf-ttEstCostItem.quantityPerSet
            .
    END.
    RELEASE bfSet-ttEstCostItem.
    RELEASE bf-ttEstCostItem.
    RELEASE bf-ttEstCostMaterial.
    RELEASE bf-ttEstCostHeader.
    
END PROCEDURE.


PROCEDURE pConvertQuantityFromUOMToUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Wrapper procedure for conversion programs.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFromUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcToUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdBasisWeightInLbsPerMSF AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdDepth AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityInFromUOM AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityInToUOM AS DECIMAL NO-UNDO.

    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.


    RUN Conv_QuantityFromUOMtoUOM(ipcCompany, ipcItemID, ipcItemType,
        ipdQuantityInFromUOM, ipcFromUOM, ipcToUOM,
        ipdBasisWeightInLbsPerMSF, ipdLength, ipdWidth, ipdDepth, 0,
        OUTPUT opdQuantityInToUOM, OUTPUT lError, OUTPUT cMessage).
    
END PROCEDURE.

PROCEDURE pCopyHeaderCostsToSetItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a Cost Header, transfers the header costs to the 
     Set Header Item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.

    DEFINE           BUFFER bf-ttEstCostBlank    FOR ttEstCostBlank. 
    DEFINE           BUFFER bf-ttEstCostItem     FOR ttEstCostItem.

    IF fIsSetType(ipbf-ttEstCostHeader.estType) THEN 
    DO:
        FOR EACH bf-ttEstCostBlank NO-LOCK
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND bf-ttEstCostBlank.formNo EQ 0
            AND bf-ttEstCostBlank.blankNo EQ 0,
            FIRST bf-ttEstCostItem EXCLUSIVE-LOCK
            WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostBlank.estCostHeaderID
              AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID:
            ASSIGN 
                bf-ttEstCostItem.costTotalBoard            = ipbf-ttEstCostHeader.costTotalBoard
                bf-ttEstCostItem.costTotalFactory          = ipbf-ttEstCostHeader.costTotalFactory
                bf-ttEstCostItem.costTotalFixedOverhead    = ipbf-ttEstCostHeader.costTotalFixedOverhead
                bf-ttEstCostItem.costTotalFull             = ipbf-ttEstCostHeader.costTotalFull
                bf-ttEstCostItem.costTotalGroupLevel       = ipbf-ttEstCostHeader.costTotalGroupLevel
                bf-ttEstCostItem.costTotalLabor            = ipbf-ttEstCostHeader.costTotalLabor
                bf-ttEstCostItem.costTotalMaterial         = ipbf-ttEstCostHeader.costTotalMaterial
                bf-ttEstCostItem.costTotalNonFactory       = ipbf-ttEstCostHeader.costTotalNonFactory
                bf-ttEstCostItem.costTotalVariableOverhead = ipbf-ttEstCostHeader.costTotalVariableOverhead
                bf-ttEstCostItem.profitPctGross            = ipbf-ttEstCostHeader.profitPctGross
                bf-ttEstCostItem.profitPctNet              = ipbf-ttEstCostHeader.profitPctNet
                bf-ttEstCostItem.sellPrice                 = ipbf-ttEstCostHeader.sellPrice
                .
        END.
    END.
    RELEASE bf-ttEstCostItem.

END PROCEDURE.

PROCEDURE pCopyDBToTempTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Write DB table records to Temp Table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstCostForm.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
    EMPTY TEMP-TABLE ttEstCostOperation.
    EMPTY TEMP-TABLE ttEstCostMisc.
    EMPTY TEMP-TABLE ttEstCostMaterial.
    EMPTY TEMP-TABLE ttEstCostBlank.
    EMPTY TEMP-TABLE ttEstCostItem.
    EMPTY TEMP-TABLE ttEstCostHeader.
        
    FOR FIRST EstCostHeader NO-LOCK
        WHERE EstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:

        CREATE ttEstCostHeader.
        BUFFER-COPY EstCostHeader TO ttEstCostHeader.      
    END.
    
    FOR EACH EstCostForm NO-LOCK
        WHERE EstCostForm.estCostHeaderID EQ ipiEstCostHeaderID:

        CREATE ttEstCostForm.
        BUFFER-COPY EstCostForm TO ttEstCostForm. 
             
    END.                
        
    FOR EACH EstCostBlank NO-LOCK 
        WHERE EstCostBlank.estCostHeaderID EQ ipiEstCostHeaderID:
                      
        CREATE ttEstCostBlank.
        BUFFER-COPY EstCostBlank TO ttEstCostBlank.
    END.    
                      
    FOR EACH EstCostItem NO-LOCK 
        WHERE EstCostItem.estCostHeaderID EQ ipiEstCostHeaderID:
                      
        CREATE ttEstCostItem.
        BUFFER-COPY EstCostItem TO ttEstCostItem.                  
    END.
            
    FOR EACH EstCostDetail NO-LOCK 
        WHERE EstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID:

        CREATE ttEstCostDetail.
        BUFFER-COPY EstCostDetail TO ttEstCostDetail.
    END. 
             
    FOR EACH EstCostOperation NO-LOCK 
        WHERE EstCostOperation.estCostHeaderID EQ ipiEstCostHeaderID:
             
        CREATE ttEstCostOperation.
        BUFFER-COPY EstCostOperation TO ttEstCostOperation.
    END. 
            
    FOR EACH EstCostMaterial NO-LOCK 
        WHERE EstCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID:
                      
        CREATE ttEstCostMaterial.
        BUFFER-COPY EstCostMaterial TO ttEstCostMaterial.
    END.
            
    FOR EACH EstCostMisc NO-LOCK 
        WHERE EstCostMisc.estCostHeaderID EQ ipiEstCostHeaderID:

        CREATE ttEstCostMisc.
        BUFFER-COPY EstCostMisc TO ttEstCostMisc.
    END. 
        
    FOR EACH EstCostSummary NO-LOCK
        WHERE EstCostSummary.estCostHeaderID EQ ipiEstCostHeaderID
        USE-INDEX estHeader:
            
        CREATE ttEstCostSummary.
        BUFFER-COPY EstCostSummary TO ttEstCostSummary.     
    END.
    
END PROCEDURE.

PROCEDURE pUpdateCostDetails PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INTEGER NO-UNDO.

    DEFINE VARIABLE lEstMiscAvailable AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dSourceTotalCost  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cDescription      AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estMisc                FOR estMisc.
    DEFINE BUFFER bf-ttEstCostHeader        FOR ttEstCostHeader.
    DEFINE BUFFER bf-ttEstCostSummary       FOR ttEstCostSummary.
    DEFINE BUFFER bf-ttEstCostDetail        FOR ttEstCostDetail.
    DEFINE BUFFER bf-source-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ttEstCostForm          FOR ttEstCostForm.
    DEFINE BUFFER bf-ttEstCostItem          FOR ttEstCostItem.
    DEFINE BUFFER bf-ttEstCostBlank         FOR ttEstCostBlank.
    
    FIND FIRST bf-ttEstCostHeader NO-LOCK
         WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
         NO-ERROR.
    IF NOT AVAILABLE bf-ttEstCostHeader THEN
        RETURN.
    
    FOR EACH bf-estMisc NO-LOCK
        WHERE bf-estMisc.company       EQ bf-ttEstCostHeader.company
          AND bf-estMisc.estimateNo    EQ bf-ttEstCostHeader.estimateNo
        BY bf-estMisc.sequenceID:
        lEstMiscAvailable = TRUE.
        
        FOR EACH bf-ttEstCostDetail
            WHERE bf-ttEstCostDetail.estCostHeaderID   EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostDetail.estCostCategoryID EQ bf-estMisc.estCostCategoryID
              AND bf-ttEstCostDetail.hasBeenProcessed  EQ TRUE:
            IF bf-estMisc.flatFeeCharge NE 0 THEN DO:
                cDescription = bf-estMisc.costDescription 
                             + " (" + "Flat Fee " + STRING(bf-estMisc.flatFeeCharge) + ")".
                             
                RUN pAddECostDetailEstMisc(BUFFER bf-ttEstCostDetail, cDescription, bf-estMisc.flatFeeCharge).
            END.
            ELSE IF bf-estMisc.chargePercent NE 0 THEN DO:
                dSourceTotalCost = 0.
                
                IF bf-estMisc.estCostCalcBy EQ "Category" THEN DO:
                    FOR EACH bf-source-ttEstCostDetail
                        WHERE bf-source-ttEstCostDetail.estCostHeaderID   EQ bf-ttEstCostDetail.estCostHeaderID
                          AND bf-source-ttEstCostDetail.estCostCategoryID EQ bf-estMisc.estCostCalcSource:
                        dSourceTotalCost = dSourceTotalCost + bf-source-ttEstCostDetail.costTotal.
                    END.
                    
                    cDescription = bf-estMisc.estCostCalcSource.
                    FIND FIRST ttEstCostCategory
                         WHERE ttEstCostCategory.estCostCategoryID EQ bf-estMisc.estCostCalcSource
                         NO-ERROR.
                    IF AVAILABLE ttEstCostCategory THEN
                        cDescription = ttEstCostCategory.estCostCategoryDesc.
                    
                    cDescription = bf-estMisc.costDescription 
                                 + "(" + STRING(bf-estMisc.chargePercent) + "% of Category - " 
                                 + cDescription + " - "  + STRING(dSourceTotalCost) + ")".

                    RUN pAddECostDetailEstMisc(BUFFER bf-ttEstCostDetail, cDescription, dSourceTotalCost * bf-estMisc.chargePercent * 0.01).
                END.
                ELSE IF bf-estMisc.estCostCalcBy EQ "Group" THEN DO:
                    FOR EACH bf-ttEstCostSummary
                        WHERE bf-ttEstCostSummary.estcostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID
                          AND bf-ttEstCostSummary.estCostGroupID  EQ bf-estMisc.estCostCalcSource
                          AND bf-ttEstCostSummary.scopeRecKey     EQ bf-ttEstCostHeader.rec_key:
                        dSourceTotalCost = dSourceTotalCost + bf-ttEstCostSummary.costTotal.
                    END.
                    
                    cDescription = bf-estMisc.estCostCalcSource.
                    FIND FIRST ttEstCostGroup
                         WHERE ttEstCostGroup.estCostGroupID EQ bf-estMisc.estCostCalcSource
                         NO-ERROR.
                    IF AVAILABLE ttEstCostGroup THEN
                        cDescription = ttEstCostGroup.estCostGroupDesc.

                    cDescription = bf-estMisc.costDescription 
                                 + "(" + STRING(bf-estMisc.chargePercent) + "% of Group - " 
                                 + cDescription + " - "  + STRING(dSourceTotalCost) + ")".

                    RUN pAddECostDetailEstMisc(BUFFER bf-ttEstCostDetail, cDescription, dSourceTotalCost * bf-estMisc.chargePercent * 0.01).
                END.
                ELSE IF bf-estMisc.estCostCalcBy EQ "Level" THEN DO:
                    FOR EACH ttEstCostGroup
                        WHERE ttEstCostGroup.estCostGroupLevelID LE INTEGER(bf-estMisc.estCostCalcSource):
                        FOR EACH bf-ttEstCostSummary
                            WHERE bf-ttEstCostSummary.estcostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID
                              AND bf-ttEstCostSummary.estCostGroupID  EQ ttEstCostGroup.estCostGroupID
                              AND bf-ttEstCostSummary.scopeRecKey     EQ bf-ttEstCostHeader.rec_key:
                            dSourceTotalCost = dSourceTotalCost + bf-ttEstCostSummary.costTotal.
                        END.
                    END.

                    cDescription = bf-estMisc.estCostCalcSource.
                    FIND FIRST ttEstCostGroupLevel
                         WHERE ttEstCostGroupLevel.estCostGroupLevelID EQ INTEGER(bf-estMisc.estCostCalcSource)
                         NO-ERROR.
                    IF AVAILABLE ttEstCostGroupLevel THEN
                        cDescription = ttEstCostGroupLevel.estCostGroupLevelDesc.

                    cDescription = bf-estMisc.costDescription 
                                 + "(" + STRING(bf-estMisc.chargePercent) + "% of Level - " 
                                 + cDescription + " - "  + STRING(dSourceTotalCost) + ")".

                    RUN pAddECostDetailEstMisc(BUFFER bf-ttEstCostDetail, cDescription, dSourceTotalCost * bf-estMisc.chargePercent * 0.01).
                END.
                ELSE IF bf-estMisc.estCostCalcBy EQ "Custom" THEN DO:
                    CASE bf-estMisc.estCostCalcSource:
                        WHEN "costTotalBoard" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalBoard.
                        WHEN "costTotalLabor" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalLabor.
                        WHEN "costTotalVariableOverhead" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalVariableOverhead.
                        WHEN "costTotalFixedOverhead" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalFixedOverhead.
                        WHEN "costTotalMaterial" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalMaterial.
                        WHEN "costTotalFactory" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalFactory.
                        WHEN "costTotalNonFactory" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalNonFactory.
                        WHEN "netProfit" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.netProfit.
                        WHEN "costTotalFull" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.costTotalFull.
                        WHEN "sellPrice" THEN
                            dSourceTotalCost = bf-ttEstCostHeader.sellPrice.
                        WHEN "grossProfit" THEN                        
                            dSourceTotalCost = bf-ttEstCostHeader.sellPrice - bf-ttEstCostHeader.costTotalFactory.
                    END CASE.
                    
                    cDescription = bf-estMisc.costDescription 
                                 + "(" + STRING(bf-estMisc.chargePercent) + "% of " + bf-estMisc.estCostCalcSource  
                                 + " - "  + STRING(dSourceTotalCost) + ")".

                    RUN pAddECostDetailEstMisc(BUFFER bf-ttEstCostDetail, cDescription, dSourceTotalCost * bf-estMisc.chargePercent * 0.01).                    
                END.
            END.
        END.
    END.

    IF lEstMiscAvailable THEN DO:
        RUN pPurgeCostSummary(ipiEstCostHeaderID).
        RUN pBuildCostSummary(ipiEstCostHeaderID).

        FOR EACH bf-ttEstCostForm NO-LOCK
            WHERE bf-ttEstCostForm.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID,
            FIRST bf-ttEstCostBlank NO-LOCK 
            WHERE bf-ttEstCostBlank.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostBlank.estCostFormID   EQ bf-ttEstCostForm.estCostFormID,
            FIRST bf-ttEstCostItem NO-LOCK 
            WHERE bf-ttEstCostItem.estCostHeaderID EQ bf-ttEstCostHeader.estCostHeaderID
              AND bf-ttEstCostItem.estCostItemID   EQ bf-ttEstCostBlank.estCostItemID:
            RUN pCalcCostTotals(bf-ttEstCostHeader.estCostHeaderID, bf-ttEstCostForm.estCostFormID, NO).
        END. 
    END.
END PROCEDURE.

PROCEDURE pWriteToDBTables PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    FOR EACH ttEstCostHeader NO-LOCK
        WHERE ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostHeader EXCLUSIVE-LOCK  
        WHERE EstCostHeader.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID NO-ERROR.
        IF AVAILABLE EstCostHeader THEN 
        DO:
            BUFFER-COPY ttEstCostHeader TO EstCostHeader.  
        END. 
        ELSE 
        DO: 
            CREATE EstCostHeader.
            BUFFER-COPY ttEstCostHeader TO EstCostHeader.
        END.         
    END.
    
    FOR EACH ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostForm EXCLUSIVE-LOCK 
        WHERE EstCostForm.estCostHeaderID EQ ttEstCostForm.estCostHeaderID
          AND EstCostForm.estCostFormID   EQ ttEstCostForm.estCostFormID  NO-ERROR.
        
        IF AVAILABLE EstCostForm THEN 
        DO: 
            BUFFER-COPY ttEstCostForm TO EstCostForm.
        END. 
        ELSE 
        DO:
            CREATE EstCostForm.
            BUFFER-COPY ttEstCostForm TO EstCostForm.  
        END.       
    END.                
        
    FOR EACH ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostBlank EXCLUSIVE-LOCK 
        WHERE EstCostBlank.estCostHeaderID EQ ttEstCostBlank.estCostHeaderID 
          AND EstCostBlank.estCostFormID EQ ttEstCostBlank.estCostFormID
          AND EstCostBlank.estCostBlankID EQ ttEstCostBlank.estCostBlankID NO-ERROR.
        
        IF AVAILABLE EstCostBlank THEN
        DO:      
           BUFFER-COPY ttEstCostBlank TO EstCostBlank.
        END.
        ELSE 
        DO:
            CREATE EstCostBlank.
            BUFFER-COPY ttEstCostBlank TO EstCostBlank.
        END.       
    END.    
                      
    FOR EACH ttEstCostItem NO-LOCK 
        WHERE ttEstCostItem.estCostHeaderID EQ ipiEstCostHeaderID :
            
        FIND FIRST EstCostItem EXCLUSIVE-LOCK 
        WHERE EstCostItem.estCostHeaderID EQ ttEstCostItem.estCostHeaderID
          AND EstCostItem.estCostItemID EQ ttEstCostItem.estCostItemID NO-ERROR.
          
        IF AVAILABLE EstCostItem THEN 
        DO:
            BUFFER-COPY ttEstCostItem TO EstCostItem.
        END.
        ELSE 
        DO: 
            CREATE EstCostItem.
            BUFFER-COPY ttEstCostItem TO EstCostItem.
        END.                             
    END.
     
    /*Remove all existing estCostDetails for commission and profit*/
    RUN pPurgeCostDetailDB(ipiEstCostHeaderID, "commission").
    RUN pPurgeCostDetailDB(ipiEstCostHeaderID, "pProfit"). 
           
    FOR EACH ttEstCostDetail NO-LOCK 
        WHERE ttEstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID:
        
        FIND FIRST EstCostDetail EXCLUSIVE-LOCK 
        WHERE EstCostDetail.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
          AND EstCostDetail.estCostDetailID EQ ttEstCostDetail.estCostDetailID NO-ERROR.
        IF AVAILABLE EstCostDetail THEN
        DO:   
            BUFFER-COPY ttEstCostDetail TO EstCostDetail.
        END.    
        ELSE 
        DO:
            CREATE EstCostDetail.
            BUFFER-COPY ttEstCostDetail TO EstCostDetail. 
        END.       
    END. 
             
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostOperation EXCLUSIVE-LOCK 
        WHERE EstCostOperation.estCostHeaderID EQ ttEstCostOperation.estCostHeaderID
          AND EstCostOperation.estCostOperationID EQ ttEstCostOperation.estCostOperationID NO-ERROR.
        
        IF AVAILABLE EstCostOperation THEN 
        DO: 
            BUFFER-COPY ttEstCostOperation TO EstCostOperation.
        END.    
        ELSE 
        DO:
            CREATE EstCostOperation.
            BUFFER-COPY ttEstCostOperation TO EstCostOperation.
        END.       
    END. 
            
    FOR EACH ttEstCostMaterial NO-LOCK 
        WHERE ttEstCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostMaterial EXCLUSIVE-LOCK 
        WHERE EstCostMaterial.estCostHeaderID EQ ttEstCostMaterial.estCostHeaderID
          AND EstCostMaterial.estCostMaterialID EQ ttEstCostMaterial.estCostMaterialID NO-ERROR.
        
        IF AVAILABLE EstCostMaterial THEN 
        DO:          
            BUFFER-COPY ttEstCostMaterial TO EstCostMaterial.
        END.    
        ELSE 
        DO: 
            CREATE EstCostMaterial.
            BUFFER-COPY ttEstCostMaterial TO EstCostMaterial. 
        END.     
    END.
            
    FOR EACH ttEstCostMisc NO-LOCK 
        WHERE ttEstCostMisc.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostMisc EXCLUSIVE-LOCK 
        WHERE EstCostMisc.estCostHeaderID EQ ttEstCostMisc.estCostHeaderID
          AND EstCostMisc.estCostMiscID EQ ttEstCostMisc.estCostMiscID NO-ERROR. 
        
        IF AVAILABLE EstCostMisc THEN 
        DO:    
           BUFFER-COPY ttEstCostMisc TO EstCostMisc.
        END. 
        ELSE 
        DO: 
            CREATE EstCostMisc.
            BUFFER-COPY ttEstCostMisc TO EstCostMisc.
        END.       
    END. 
    
    RUN pPurgeCostSummaryDB(ipiEstCostHeaderID).    
    FOR EACH ttEstCostSummary NO-LOCK
        WHERE ttEstCostSummary.estCostHeaderID EQ ipiEstCostHeaderID:
            
        FIND FIRST EstCostSummary EXCLUSIVE-LOCK 
        WHERE EstCostSummary.estCostHeaderID EQ ttEstCostSummary.estCostHeaderID
          AND EstCostSummary.estCostSummaryID EQ ttEstCostSummary.estCostSummaryID 
          AND EstCostSummary.estCostGroupID EQ ttEstCostSummary.estCostGroupID
          AND EstCostSummary.scopeRecKey EQ ttEstCostSummary.scopeRecKey NO-ERROR.   
        
        IF AVAILABLE EstCostSummary THEN 
        DO: 
            BUFFER-COPY ttEstCostSummary TO EstCostSummary.
        END. 
        ELSE 
        DO: 
            CREATE EstCostSummary.
            BUFFER-COPY ttEstCostSummary TO EstCostSummary.   
        END.         
    END.
    
    EMPTY TEMP-TABLE ttEstCostForm.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
    EMPTY TEMP-TABLE ttEstCostOperation.
    EMPTY TEMP-TABLE ttEstCostMisc.
    EMPTY TEMP-TABLE ttEstCostMaterial.
    EMPTY TEMP-TABLE ttEstCostBlank.
    EMPTY TEMP-TABLE ttEstCostItem.
    EMPTY TEMP-TABLE ttEstCostHeader.
    
END PROCEDURE.

PROCEDURE pGetEstCostCategoryTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Return the temp-table for EstCostCategory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostCategory. 

END PROCEDURE.

PROCEDURE pGetLayerDividerDepth PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper around reftable for retrieving the depth for layer pad
     and divider - REFACTOR!!
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdLayerDepth AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDividerDepth AS DECIMAL NO-UNDO.

    FIND FIRST reftable NO-LOCK 
        WHERE reftable.reftable EQ "cedepth"
        AND reftable.company  EQ ipcCompany
        AND reftable.loc      EQ ipcEstNo
        AND reftable.code     EQ STRING(ipiFormNo,"9999999999")
        AND reftable.code2    EQ STRING(ipiBlankNo,"9999999999")
        NO-ERROR.
    IF AVAILABLE reftable THEN 
        ASSIGN 
            opdLayerDepth   = reftable.val[1]
            opdDividerDepth = reftable.val[2]
            .
            
END PROCEDURE.

PROCEDURE pCalcTotalsForCostDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a cost Total
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostDetail     FOR ttEstCostDetail.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostCategory   FOR ttEstCostCategory.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm       FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader     FOR ttEstCostHeader.
    
    RUN pCalcCostTotalsHeader(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
    RUN pCalcCostTotalsForm(BUFFER ipbf-ttEstCostForm, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
        
    FIND FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostDetail.estCostHeaderID
        AND ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostDetail.estCostFormID
        AND ttEstCostBlank.estCostBlankID EQ ipbf-ttEstCostDetail.estCostBlankID
        NO-ERROR.
    IF AVAILABLE ttEstCostBlank THEN 
    DO:  /*Blank-specific cost*/
        FIND FIRST ttEstCostItem NO-LOCK 
            WHERE ttEstCostItem.estCostHeaderID EQ ttEstCostBlank.estCostHeaderID
            AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID
            NO-ERROR.
        IF AVAILABLE ttEstCostItem THEN 
            RUN pCalcCostTotalsItem(BUFFER ttEstCostItem, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
    END.
    ELSE /*Divide up the Form-level Costs into each item*/
        FOR EACH ttEstCostBlank NO-LOCK
            WHERE ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostDetail.estCostHeaderID
            AND ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostDetail.estCostFormID, 
            FIRST ttEstCostItem NO-LOCK  
            WHERE ttEstCostItem.estCostHeaderID EQ ttEstCostBlank.estCostHeaderID
            AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID :
            RUN pCalcCostTotalsItem(BUFFER ttEstCostItem, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal * ttEstCostBlank.pctOfForm).
        END.

END PROCEDURE.

PROCEDURE pGetPriceProfitAndCommissionForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and blank, return the target margin percentage
     Notes: Should replace "CalcSellPrice.p and markup.p"
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostItem   FOR ttEstCostItem.
    DEFINE OUTPUT PARAMETER opdNetProfit AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCommission AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dCommPct  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dLookup   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBoardPct AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMargin   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMarginOn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dProfit   AS DECIMAL   NO-UNDO.

    CASE gcMarginMatrixLookup:
        WHEN "Board Cost" THEN 
            dLookup = ipbf-ttEstCostForm.costTotalBoard.
        WHEN "Factory Cost" THEN 
            dLookup = ipbf-ttEstCostForm.costTotalFactory.
        WHEN "Full Cost" THEN 
            dlookup = ipbf-ttEstCostForm.costTotalFull.
        OTHERWISE /*Square Feet*/ 
        dLookup   = ipbf-ttEstCostForm.grossQtyRequiredTotalArea.
    END CASE. 
    IF ipbf-ttEstCostForm.costTotalFactory NE 0 THEN 
        dBoardPct = ipbf-ttEstCostForm.costTotalBoard / ipbf-ttEstCostForm.costTotalFactory * 100.
    RUN est/GetMarkup.p (ipbf-ttEstCostItem.company, 
        ipbf-ttEstCostItem.customerID, ipbf-ttEstCostItem.productCategory, ipbf-ttEstCostItem.styleID,
        dLookup, dBoardPct, INPUT-OUTPUT dMargin, INPUT-OUTPUT cMarginOn).
    IF cMarginOn EQ "" THEN cMarginOn = "G".
    
    IF dMargin EQ 0 THEN 
    DO: 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-ttEstCostItem.company
            AND cust.cust-no EQ ipbf-ttEstCostItem.customerID
            NO-ERROR.
        IF AVAILABLE cust THEN 
            ASSIGN 
                dMargin   = cust.markup
                cMarginOn = "N"
                .
    END.
    IF dMargin EQ 0 THEN 
    DO:
        FIND FIRST ttEstCostHeader NO-LOCK 
            WHERE ttEstCostHeader.company EQ ipbf-ttEstCostItem.company
            AND ttEstCostHeader.estCostHeaderID EQ ipbf-ttEstCostItem.estCostHeaderID
            NO-ERROR.
        IF AVAILABLE ttEstCostHeader THEN 
            ASSIGN 
                dMargin   = ttEstCostHeader.marginPct
                cMarginOn = ttEstCostHeader.marginOn
                .
    END.
    dCommPct = ipbf-ttEstCostItem.commissionPct.
    CASE cMarginOn:
        WHEN "B" THEN 
            dCost = ipbf-ttEstCostForm.costTotalBoard.
        WHEN "N" THEN
            dCost = ipbf-ttEstCostForm.costTotalFull.
        OTHERWISE /*G*/
        dCost = ipbf-ttEstCostForm.costTotalFactory.
    END CASE.    
    IF cMarginOn EQ "N" THEN
        ASSIGN 
            dProfit       = fGetProfit(dCost, dMargin + dCommPct, "Margin")
            opdPrice      = dProfit + dCost
            opdCommission = opdPrice * dCommPct / 100
            opdNetProfit  = opdPrice - opdCommission - ipbf-ttEstCostForm.costTotalFull
            .     
    ELSE 
        ASSIGN        
            dProfit       = fGetProfit(dCost, dMargin, "Margin")
            opdPrice      = dProfit + dCost
            opdCommission = opdPrice * dCommPct / 100
            opdNetProfit  = opdPrice - ipbf-ttEstCostForm.costTotalFull
            .

END PROCEDURE.

PROCEDURE pGetStrapping PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a blank, output the strap code and quantity required per pallet
     Notes:  should replace pr4-str.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    DEFINE OUTPUT PARAMETER opcItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityRequiredPerPallet AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcQuantityUOM AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFormula    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStrapCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dStrapQty   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrapUOM   AS CHARACTER NO-UNDO.
    

    FIND FIRST stackPattern NO-LOCK
        WHERE stackPattern.stackCode     EQ ipbf-eb.stack-code
        NO-ERROR.

    IF AVAILABLE stackPattern THEN 
    DO:
        ASSIGN 
            opcItemID   = stackPattern.strapCode
            cFormula    = stackPattern.strapFormula
            iStrapCount = MAXIMUM(stackPattern.strapCount, 1)
            cStrapUOM   = IF stackPattern.strapUOM NE "" THEN stackPattern.strapUOM ELSE "MLI" /*Refactor to use stackPattern.strapUOM*/
            .
            
        RUN ProcessStyleFormula (ipbf-eb.company,  /*Company*/
            cFormula,  /*Formula*/
            ipbf-eb.tr-len,  /*Length*/
            ipbf-eb.tr-wid, /*Width*/
            ipbf-eb.tr-dep, /*Depth*/
            0, /*G*/
            0, /*T*/
            0, /*K*/
            0, /*F*/
            0, /*B*/
            0, /*O*/
            0, /*I*/
            OUTPUT dStrapQty /*Result*/).
       
        
        IF cStrapUOM = "MLI" THEN dStrapQty = dStrapQty / 1000.
        ASSIGN 
            opdQuantityRequiredPerPallet = dStrapQty /* * iStrapCount*/
            opcQuantityUOM               = cStrapUOM.
        
            
    END.

END PROCEDURE.

PROCEDURE pBuildSystemData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Populates the system data in Temp-tables
     Notes: If No data is setup in user specific tables then use system tables 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    
    RUN Estimate_GetSystemDataForEstimate(INPUT ipcCompany,
        OUTPUT TABLE ttEstCostCategory,
        OUTPUT TABLE ttEstCostGroup,
        OUTPUT TABLE ttEstCostGroupLevel). 
END PROCEDURE.

       
PROCEDURE pImportMachineStandards PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Import Machine Standards on each operation that isn't flagged as locked
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-est-op             FOR est-op.
    DEFINE PARAMETER BUFFER opbf-ttEstCostOperation FOR ttEstCostOperation.
    
    DEFINE VARIABLE dMRWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMRHrs     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpeed     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpoilPrct AS DECIMAL NO-UNDO.
       
    RUN Operations_ImportMachineStandards IN ghOperation
        (ipbf-est-op.company, ipbf-est-op.est-no, ipbf-est-op.s-num, ipbf-est-op.b-num, ipbf-est-op.op-pass,ipbf-est-op.qty, ipbf-ttEstCostHeader.quantityMaster, ipbf-est-op.m-code, OUTPUT dSpeed, OUTPUT dMRHrs, OUTPUT dMRWaste, OUTPUT dSpoilPrct).
    
    IF AVAILABLE opbf-ttEstCostOperation THEN
        ASSIGN
            opbf-ttEstCostOperation.quantityInSetupWaste      = dMRWaste
            opbf-ttEstCostOperation.hoursSetup                = dMRHrs
            opbf-ttEstCostOperation.speed                     = dSpeed
            opbf-ttEstCostOperation.quantityInRunWastePercent = dSpoilPrct
            . 
        
END PROCEDURE.

PROCEDURE pProcessAdders PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for adders
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcAdders LIKE ef.adder NO-UNDO.

    DEFINE BUFFER bf-ttEstCostMaterial      FOR ttEstCostMaterial.
    DEFINE BUFFER bf-item                 FOR ITEM.
    DEFINE BUFFER bfBoard-ttEstCostMaterial FOR ttEstCostMaterial.
    
    DEFINE VARIABLE iCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAdder AS CHARACTER NO-UNDO.
    
    DO iCount = 1 TO 6 :  //EXTENT(ipcAdders) - can't be used because elements 7 and up are the descriptions
        cAdder = ipcAdders[iCount].
        IF cAdder NE "" THEN 
        DO:
            FIND FIRST bf-item NO-LOCK 
                WHERE bf-item.company EQ ipbf-ttEstCostForm.company
                AND bf-item.i-no EQ cAdder
                NO-ERROR.
            IF NOT AVAILABLE bf-item THEN 
            DO:
                RUN pAddError("Adder '" + cAdder + "' is not valid", giErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
                RETURN.
            END.
            IF NOT fIsAdderMaterial(bf-item.mat-type) THEN 
            DO:
                RUN pAddError("Adder '" + cAdder + "' is valid material but not an adder material type.", giErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
                RETURN.
            END.      
            RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, cAdder, 0, BUFFER bf-ttEstCostMaterial).
            ASSIGN 
                bf-ttEstCostMaterial.isPrimarySubstrate         = NO
                bf-ttEstCostMaterial.addToWeightNet             = YES
                bf-ttEstCostMaterial.addToWeightTare            = NO
                                       
                bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
                bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
                bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
                bf-ttEstCostMaterial.quantityUOMWaste           = "EA"
                bf-ttEstCostMaterial.quantityUOM                = "EA"
                bf-ttEstCostMaterial.basisWeight                = bf-item.basis-w
                bf-ttEstCostMaterial.dimWidth                   = ipbf-ttEstCostForm.grossWidth
                bf-ttEstCostMaterial.dimLength                  = ipbf-ttEstCostForm.grossLength
                bf-ttEstCostMaterial.dimDepth                   = ipbf-ttEstCostForm.grossDepth
                bf-ttEstCostMaterial.noCharge                   = ipbf-ttEstCostForm.noCost
                .
            FIND FIRST bfBoard-ttEstCostMaterial NO-LOCK 
                WHERE bfBoard-ttEstCostMaterial.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
                AND bfBoard-ttEstCostMaterial.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
                AND bfBoard-ttEstCostMaterial.isPrimarySubstrate
                NO-ERROR.
            IF AVAILABLE bfBoard-ttEstCostMaterial THEN 
                bf-ttEstCostMaterial.vendorID = bfBoard-ttEstCostMaterial.vendorID.
            
            RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).   
    
        END.
    END.

END PROCEDURE.

PROCEDURE pProcessBoardBOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-item-bom           FOR item-bom.
    DEFINE OUTPUT PARAMETER oplValidBom             AS LOGICAL NO-UNDO.
    
    DEFINE           BUFFER bf-ttEstCostMaterial      FOR ttEstCostMaterial.
    DEFINE           BUFFER bfBoard-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE           BUFFER bf-ttEstCostOperation      FOR ttEstCostOperation.
    DEFINE           BUFFER bf-item                 FOR ITEM.
    
    DEFINE VARIABLE dShrinkPct AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidth AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDepth AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipbf-item-bom.i-no
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Component '" + ipbf-item-bom.i-no + "' is not valid", giErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    ELSE IF AVAILABLE bf-item THEN 
    DO: 
        IF NOT CAN-FIND (FIRST bf-ttEstCostOperation
            WHERE bf-ttEstCostOperation.company     EQ ipbf-ttEstCostForm.company
            AND bf-ttEstCostOperation.estimateNo    EQ ipbf-ttEstCostForm.estimateNo
            AND bf-ttEstCostOperation.isCorrugator) THEN 
        DO:
            RUN pAddError("BOM Component '" + ipbf-item-bom.i-no + "' but there no corrugator/laminator in routing", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
            RETURN.
        END.    
    END. /* ELSE IF AVAILABLE bf-item THEN */   
    oplValidBom = YES.

    RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-item-bom.i-no, 0, BUFFER bf-ttEstCostMaterial).
    
    //Override Form Dimensions for real material.  Length needs to vary depending on Shrink
    IF bf-item.i-code EQ 'R' THEN 
        ASSIGN 
            dWidth = IF bf-item.r-wid NE 0 THEN bf-item.r-wid ELSE bf-item.s-wid
            dDepth = bf-item.s-dep
            .
    ELSE 
        ASSIGN 
            dWidth = ipbf-ttEstCostForm.grossWidth
            dDepth = ipbf-ttEstCostForm.grossDepth
            .
        
    ASSIGN 
        bf-ttEstCostMaterial.isPrimarySubstrate         = YES
        bf-ttEstCostMaterial.addToWeightNet             = YES
        bf-ttEstCostMaterial.addToWeightTare            = NO                                       
        bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
        bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
        bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
        bf-ttEstCostMaterial.quantityUOMWaste           = "EA"
        bf-ttEstCostMaterial.quantityUOM                = "EA"
        bf-ttEstCostMaterial.basisWeight                = bf-item.basis-w
        dShrinkPct                                      = (IF ipbf-item-bom.shrink NE 100 THEN ipbf-item-bom.shrink ELSE 0) / 100
        bf-ttEstCostMaterial.dimWidth                     = dWidth
        bf-ttEstCostMaterial.dimLength                    = ipbf-ttEstCostForm.grossLength / ( 1 - dShrinkPct)
        bf-ttEstCostMaterial.dimDepth                     = dDepth
        bf-ttEstCostMaterial.noCharge                     = ipbf-ttEstCostForm.noCost
        .
    FIND FIRST bfBoard-ttEstCostMaterial NO-LOCK 
        WHERE bfBoard-ttEstCostMaterial.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
        AND bfBoard-ttEstCostMaterial.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND bfBoard-ttEstCostMaterial.isPrimarySubstrate
        NO-ERROR.
    IF AVAILABLE bfBoard-ttEstCostMaterial THEN 
        bf-ttEstCostMaterial.vendorID = bfBoard-ttEstCostMaterial.vendorID.
            
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).   
    
END PROCEDURE.

PROCEDURE pProcessBoardBOMAdhesive PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds the adhesive based on the pre-calculated square inches
     of total bom layers.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdAreaInSqIn AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE BUFFER bf-item            FOR ITEM.
    
    DEFINE VARIABLE dCoverageRateInSqInPerLb AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Adhesive '" + ipcItemID + "' is not valid", giErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    
    RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipcItemID, 0, BUFFER bf-ttEstCostMaterial).
    ASSIGN 
        bf-ttEstCostMaterial.isPrimarySubstrate = NO
        bf-ttEstCostMaterial.addToWeightNet     = YES
        bf-ttEstCostMaterial.addToWeightTare    = NO
        dCoverageRateInSqInPerLb              = bf-item.sqin-lb                                
        bf-ttEstCostMaterial.quantityUOMWaste   = "LB"
        bf-ttEstCostMaterial.quantityUOM        = "LB"
        bf-ttEstCostMaterial.basisWeight        = bf-item.basis-w
        bf-ttEstCostMaterial.noCharge           = ipbf-ttEstCostForm.noCost
        .
    IF dCoverageRateInSqInPerLb GT 0 THEN 
        ASSIGN 
            bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            .    
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessBoardBOMLaminate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds the laminate if there is BOM based on total
     Gross Sq Ins of the Sheet
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE BUFFER bf-item            FOR ITEM.
    
    DEFINE VARIABLE dCoverageRateInSqInPerLb AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dGrossSheetAreaInSqIn    AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Laminate '" + ipcItemID + "' is not valid", giErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    
    RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipcItemID, 0, BUFFER bf-ttEstCostMaterial).
    ASSIGN 
        bf-ttEstCostMaterial.isPrimarySubstrate = NO
        bf-ttEstCostMaterial.addToWeightNet     = YES
        bf-ttEstCostMaterial.addToWeightTare    = NO
        dCoverageRateInSqInPerLb              = bf-item.sqin-lb                                
        bf-ttEstCostMaterial.quantityUOMWaste   = "LB"
        bf-ttEstCostMaterial.quantityUOM        = "LB"
        bf-ttEstCostMaterial.basisWeight        = bf-item.basis-w
        bf-ttEstCostMaterial.noCharge           = ipbf-ttEstCostForm.noCost
        dGrossSheetAreaInSqIn                 = ipbf-ttEstCostForm.grossWidth * ipbf-ttEstCostForm.grossLength
        .
    IF dCoverageRateInSqInPerLb GT 0 THEN 
        ASSIGN 
            bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            .    
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessEstMaterial PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Processes Additional Materials listing in Misc / Sub tab for 
     given form 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    
    DEFINE BUFFER bf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE BUFFER bf-estMaterial        FOR estMaterial.
    DEFINE BUFFER bf-ttEstCostMaterial    FOR ttEstCostMaterial.
    DEFINE BUFFER bf-ttEstCostBlank       FOR ttEstCostBlank.
    DEFINE BUFFER bfUnitize-ttEstCostForm FOR ttEstCostForm.
    
    DEFINE VARIABLE dSqFtPerEA AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCases AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPallets AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInEA AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMultiplier AS INTEGER NO-UNDO.

    IF ipbf-ttEstCostHeader.isUnitizedSet  THEN 
    DO:
        /*Establish unitization form (Form 1)*/
        FIND FIRST bfUnitize-ttEstCostForm NO-LOCK 
            WHERE bfUnitize-ttEstCostForm.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND bfUnitize-ttEstCostForm.formNo EQ 1
            NO-ERROR.
    END.
    
    FOR EACH bf-estMaterial NO-LOCK 
        WHERE bf-estMaterial.company EQ ipbf-ttEstCostHeader.company
        AND bf-estMaterial.estimateNo EQ ipbf-ttEstCostHeader.estimateNo,
        FIRST bf-ttEstCostForm NO-LOCK 
            WHERE bf-ttEstCostForm.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostheaderID
            AND bf-ttEstCostForm.formNo EQ bf-estMaterial.formNo:
        ASSIGN 
            iCases = 0
            iPallets = 0
            iQuantityInEA = 0
            .
        IF bf-estMaterial.blankNo NE 0 THEN
            FIND FIRST bf-ttEstCostBlank NO-LOCK
                WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
                AND bf-ttEstCostBlank.formNo EQ bf-ttEstCostForm.formNo
                AND bf-ttEstCostBlank.blankNo EQ bf-estMaterial.blankNo
                NO-ERROR.
        IF AVAILABLE bf-ttEstCostBlank THEN DO: 
            RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostForm, bf-estMaterial.itemID, bf-ttEstCostBlank.estCostBlankID, BUFFER bf-ttEstCostMaterial).
            ASSIGN 
                iCases = bf-ttEstCostBlank.quantityOfSubUnits
                iPallets = bf-ttEstCostBlank.quantityOfUnits
                iQuantityInEA = bf-ttEstCostBlank.quantityRequired
                .
        END.
        ELSE 
        DO:
            RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostForm, bf-estMaterial.itemID, 0, BUFFER bf-ttEstCostMaterial).
            FOR EACH bf-ttEstCostBlank NO-LOCK
                WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
                AND bf-ttEstCostBlank.formNo EQ bf-ttEstCostForm.formNo:
                ASSIGN 
                    iCases = iCases + bf-ttEstCostBlank.quantityOfSubUnits
                    iPallets = iPallets + bf-ttEstCostBlank.quantityOfUnits
                    iQuantityInEA = iQuantityInEA + bf-ttEstCostBlank.quantityRequired. 
            END.            
        END.
        ASSIGN  
            bf-ttEstCostMaterial.addToWeightNET          = NOT fIsPackingMaterial(bf-estMaterial.materialTypeID)
            bf-ttEstCostMaterial.addToWeightTare         = fIsPackingMaterial(bf-estMaterial.materialTypeID) 
            bf-ttEstCostMaterial.quantityUOM             = bf-estMaterial.quantityUOM
            bf-ttEstCostMaterial.dimDepth                = bf-estMaterial.dimDepth
            bf-ttEstCostMaterial.dimWidth                = bf-estMaterial.dimWidth
            bf-ttEstCostMaterial.dimLength               = bf-estMaterial.dimLength
            bf-ttEstCostMaterial.dimUOM                  = "IN"  //Refactor - add dimUOM to estMaterial 
            bf-ttEstCostMaterial.weightUOM               = bf-estMaterial.weightUOM
            bf-ttEstCostMaterial.costOverridePerUOM      = bf-estMaterial.costOverridePerUOM
            bf-ttEstCostMaterial.noCharge                = bf-estMaterial.noCharge
            dSqFtPerEa                                 = DYNAMIC-FUNCTION("Conv_GetSqft",bf-ttEstCostMaterial.dimLength, bf-ttEstCostMaterial.dimWidth, bf-ttEstCostMaterial.dimUOM).
            
            .            
        IF dSqFtPerEA NE 0 AND bf-estMaterial.weightPerEA NE 0 THEN 
            bf-ttEstCostMaterial.basisWeight = bf-estMaterial.weightPerEA / (dSqFtPerEA / 1000).
            
        CASE bf-estMaterial.quantityPer:
            WHEN "L" THEN 
                iMultiplier = 1.
            WHEN "C" THEN 
                iMultiplier = iCases.
            WHEN "P" THEN 
                iMultiplier = iPallets.
            WHEN "E" THEN 
                iMultiplier = iQuantityInEA.
            WHEN "S" THEN 
                iMultiplier = ipbf-ttEstCostHeader.quantityMaster.
        END CASE.
        ASSIGN 
            bf-ttEstCostMaterial.quantityRequiredNoWaste = bf-estMaterial.quantity * iMultiplier
            bf-ttEstCostMaterial.quantityRequiredSetupWaste = (bf-estMaterial.wastePercent / 100) * bf-ttEstCostMaterial.quantityRequiredNoWaste
            bf-ttEstCostMaterial.weightTotal = bf-estMaterial.weightPerEA * bf-ttEstCostMaterial.quantityRequiredNoWaste
            .
        IF bf-ttEstCostForm.formNo EQ 0 AND AVAILABLE bfUnitize-ttEstCostForm THEN 
        DO:
            /*Associate Form 0 materials to the unitize form (Form 1)*/
            ASSIGN 
                bf-ttEstCostMaterial.estCostFormID  = bfUnitize-ttEstCostForm.estCostFormID
                bf-ttEstCostMaterial.estCostBlankID = 0
                .
            RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER bfUnitize-ttEstCostForm).
        END. 
        ELSE     
            RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER bf-ttEstCostForm).
        
    END.
    
END PROCEDURE.    

PROCEDURE pProcessMaterialTypeCalc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  For special material type calculations (non default)
 Notes:  Will process the ttEstCostMaterial buffer accordingly.
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER ipcCalculationType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostBlank FOR ttEstCostBlank.
    DEFINE BUFFER bf-ttEstCostItem FOR ttEstCostItem.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    DEFINE VARIABLE dTotWeightPerForm AS DECIMAL NO-UNDO.
    
    CASE ipcCalculationType:
        WHEN "ByFGWeight" THEN DO:
            FOR EACH bf-ttEstCostBlank NO-LOCK 
                WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostMaterial.estCostHeaderID
                AND bf-ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostMaterial.estCostFormID,
                FIRST bf-ttEstCostItem NO-LOCK
                WHERE bf-ttEstCostItem.estCostHeaderID EQ ipbf-ttEstCostMaterial.estCostHeaderID
                  AND bf-ttEstCostItem.estCostItemID EQ bf-ttEstCostBlank.estCostItemID,
                FIRST bf-itemfg NO-LOCK 
                WHERE bf-itemfg.company EQ bf-ttEstCostItem.company
                AND bf-itemfg.i-no EQ bf-ttEstCostItem.itemID:
                dTotWeightPerForm = dTotWeightPerForm + bf-itemfg.weightPerEA * bf-ttEstCostBlank.numOut.
            END.        
            IF dTotWeightPerForm GT 0 THEN 
                ASSIGN 
                    ipbf-ttEstCostMaterial.quantityRequiredNoWaste = ipbf-ttEstCostMaterial.quantityRequiredNoWaste * dTotWeightPerForm
                    ipbf-ttEstCostMaterial.quantityRequiredRunWaste = ipbf-ttEstCostMaterial.quantityRequiredRunWaste * dTotWeightPerForm
                    ipbf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostMaterial.quantityRequiredSetupWaste * dTotWeightPerForm
                    ipbf-ttEstCostMaterial.quantityUOM = "LB"
                    ipbf-ttEstCostMaterial.quantityUOMWaste = "LB"
                    . 
        END.
    END CASE.
    
END PROCEDURE.

PROCEDURE pProcessMiscNonPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the ttEstCostMisc for non-prep mis items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DO iIndex = 1 TO 6:
        IF ipbf-ef.mis-cost[iIndex] EQ "" THEN NEXT.
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-ttEstCostForm, iIndex, "Material").
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-ttEstCostForm, iIndex, "Labor").
    END.

END PROCEDURE.

PROCEDURE pProcessMiscPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the ttEstCostMisc for prep misc items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT  PARAMETER ipdQuantityMaster AS DECIMAL NO-UNDO.
    
    FOR EACH est-prep NO-LOCK 
        WHERE est-prep.company EQ ipbf-ef.company
        AND est-prep.est-no EQ ipbf-ef.est-no
        AND est-prep.s-num EQ ipbf-ef.form-no
        AND est-prep.code NE "":
        RUN pAddEstMiscForPrep(BUFFER est-prep, BUFFER ipbf-ttEstCostForm, ipdQuantityMaster).
    END.    

END PROCEDURE.

PROCEDURE pProcessOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a estCostHeader, build all estCostOperations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader  FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE VARIABLE dQtyFormsRequiredForBlanks    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFormsRequiredForBlanksMax AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOut                     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget                    AS DECIMAL NO-UNDO.
    

    
    /*Get the effective Est-op quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstCostHeader.company 
        AND est-op.est-no  EQ ipbf-ttEstCostHeader.estimateNo 
        AND est-op.line    LT 500
        BREAK BY est-op.qty:
    
        IF FIRST-OF(est-op.qty) THEN 
        DO:
            /*Refactor - Qty in Ops must match one of the est-qty?*/
            IF FIRST(est-op.qty) OR
                CAN-FIND(FIRST est-qty
                WHERE est-qty.company EQ est-op.company
                AND est-qty.est-no  EQ est-op.est-no
                AND est-qty.eqty    EQ est-op.qty)
                THEN dQtyTarget = est-op.qty.
            IF est-op.qty GE ipbf-ttEstCostHeader.quantityMaster THEN LEAVE.
        END.
    END.
    
    EMPTY TEMP-TABLE ttEstBlank.
    
    FOR EACH ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
        AND ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID:
            
            
        IF NOT CAN-FIND (FIRST ttEstBlank
            WHERE ttEstBlank.estCostBlankID EQ ttEstCostBlank.estCostBlankID) THEN
        DO:
            CREATE ttEstBlank.
            ASSIGN 
                ttEstBlank.estCostBlankID   = ttEstCostBlank.estCostBlankID
                ttEstBlank.estCostFormID    = ttEstCostBlank.estCostFormID
                ttEstBlank.dQtyInOut        = IF ttEstCostBlank.priceBasedOnYield THEN ttEstCostBlank.quantityYielded ELSE ttEstCostBlank.quantityRequired
                ttEstBlank.iOut             = ttEstCostBlank.numOut
                .
        END.
        
    END.
    
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstCostHeader.company
        AND est-op.est-no EQ ipbf-ttEstCostHeader.estimateNo
        AND est-op.s-num EQ ipbf-ttEstCostForm.formNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:

    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation,ipbf-ttEstCostHeader.quantityMaster).                    
    IF AVAILABLE bf-ttEstCostOperation THEN 

    DO:
        /* Import Machine Standards */
        IF glCalcSourceForMachineStd AND NOT est-op.isLocked THEN
            RUN pImportMachineStandards (BUFFER ipbf-ttEstCostHeader, BUFFER est-op, BUFFER bf-ttEstCostOperation).
        
        /*REFACTOR to calculate quantities for combos*/        
        IF est-op.b-num NE 0 AND bf-ttEstCostOperation.feedType EQ "B" THEN
        DO:  
            /*Calculate for Combo*/
            FIND FIRST ttEstBlank
                WHERE ttEstBlank.estCostBlankID EQ bf-ttEstCostOperation.estCostBlankID
                NO-ERROR.
                
            IF AVAILABLE ttEstBlank AND NOT ttEstBlank.lOutputInitialized THEN 
                ASSIGN 
                    ttEstBlank.lOutputInitialized = YES
                    .
            
            RUN pProcessOperation(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation, INPUT-OUTPUT ttEstBlank.dQtyInOut, 
                INPUT-OUTPUT ttEstBlank.dQtyInOutSetupWaste, INPUT-OUTPUT ttEstBlank.dQtyInOutRunWaste).
        END. /*BlankNo not 0*/
        ELSE 
        DO:                  
            IF bf-ttEstCostOperation.isBlankMaker AND NOT bf-ttEstCostOperation.isNetSheetMaker THEN 
            DO:
                /*Find the most forms required to support each blank operations*/
                FOR EACH ttEstBlank NO-LOCK 
                    WHERE ttEstBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID:
                    dQtyFormsRequiredForBlanks = fRoundUp(ttEstBlank.dQtyInOut / MAX(ttEstBlank.iOut,1)).
                    IF dQtyFormsRequiredForBlanksMax LT dQtyFormsRequiredForBlanks THEN 
                        ASSIGN 
                            dQtyFormsRequiredForBlanksMax = dQtyFormsRequiredForBlanks
                            dQtyInOutSetupWaste           = fRoundUp(ttEstBlank.dQtyInOutSetupWaste / MAX(ttEstBlank.iOut,1))
                            dQtyInOutRunWaste             = fRoundUp(ttEstBlank.dQtyInOutRunWaste / MAX(ttEstBlank.iOut,1))
                            .
                END.
                
                /*Convert the forms for the most wasteful blank into what is required out of the blank maker as a total for all blanks*/
                ASSIGN 
                    dQtyInOut           = dQtyFormsRequiredForBlanksMax * bf-ttEstCostOperation.numOutForOperation
                    dQtyInOutSetupWaste = dQtyInOutSetupWaste * bf-ttEstCostOperation.numOutForOperation
                    dQtyInOutRunWaste   = dQtyInOutRunWaste * bf-ttEstCostOperation.numOutForOperation.
            END.
            IF dQtyInOut EQ 0 THEN 
                dQtyInOut = ipbf-ttEstCostForm.quantityFGOnFormYielded.
            RUN pProcessOperation(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation, INPUT-OUTPUT dQtyInOut, 
                INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
                
        END.
        RUN pCalcEstOperation(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostOperation, BUFFER ipbf-ttEstCostForm).                    
    END.
                    
END. /*Each est-op*/

IF dQtyInOut NE 0 THEN
    ASSIGN 
        ipbf-ttEstCostForm.grossQtyRequiredSetupWaste = dQtyInOutSetupWaste
        ipbf-ttEstCostForm.grossQtyRequiredRunWaste   = dQtyInOutRunWaste
        ipbf-ttEstCostForm.grossQtyRequiredNoWaste    = dQtyInOut - (dQtyInOutSetupWaste + dQtyInOutRunWaste)
        ipbf-ttEstCostForm.grossQtyRequiredTotal      = dQtyInOut
        .
            
END PROCEDURE.

PROCEDURE pProcessSpecialMaterials PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an EF, processes the special materials (Misc/Sub) tab
     Notes: replaces ce/pr4-spe.p, ce/pr4-spe.i
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ef              FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.

    DEFINE           BUFFER bf-item            FOR ITEM.
    DEFINE           BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE           BUFFER bf-ttEstCostBlank    FOR ttEstCostBlank.
    
    DEFINE VARIABLE iIndex          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEstCostBlankID AS INT64   NO-UNDO.
    
    DO iIndex = 1 TO 8:
        IF ipbf-ef.spec-no[iIndex] NE "" THEN 
        DO:
            FIND FIRST bf-ttEstCostBlank NO-LOCK 
                WHERE bf-ttEstCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
                AND bf-ttEstCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
                AND bf-ttEstCostBlank.blankNo EQ 1  /*REFACTOR - What is blank number???*/
                NO-ERROR.
            IF AVAILABLE bf-ttEstCostBlank THEN 
                iEstCostBlankID = bf-ttEstCostBlank.estCostBlankID.
            RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.spec-no[iIndex], iEstCostBlankID, BUFFER bf-ttEstCostMaterial).
            IF AVAILABLE bf-ttEstCostMaterial THEN 
            DO: 
                
                /*REFACTOR - ugly.  work around to support more than 2 decimals as stored in db*/
                RUN custom/extradec.p (.0001, ipbf-ef.spec-qty[iIndex] * ipbf-ttEstCostForm.quantityFGOnForm,
                    OUTPUT bf-ttEstCostMaterial.quantityRequiredNoWaste).
                    
                ASSIGN            
                    bf-ttEstCostMaterial.addToWeightNet = YES
                    bf-ttEstCostMaterial.itemName       = ipbf-ef.spec-dscr[iIndex]
                    bf-ttEstCostMaterial.quantityUOM    = "EA"
                    .
                RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).
            END.
        END.         
    END.
END PROCEDURE.

PROCEDURE pCalcCostTotals PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Factory Cost Details and totals Forms and Items for 
     use in % Total Calculations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstCostFormID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER iplFullReset AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    
        
    RUN pGetEstCostCategoryTT(OUTPUT TABLE ttEstCostCategory).
    
    FOR EACH bf-ttEstCostDetail EXCLUSIVE-LOCK
        WHERE bf-ttEstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID
        AND (iplFullReset OR NOT bf-ttEstCostDetail.hasBeenProcessed)
        AND (ipiEstCostFormID EQ 0 OR bf-ttEstCostDetail.estCostFormID EQ ipiEstCostFormID), 
        FIRST ttEstCostHeader NO-LOCK 
        WHERE ttEstCostHeader.estCostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID,
        FIRST ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID
        AND ttEstCostForm.estCostFormID EQ bf-ttEstCostDetail.estCostFormID, 
        FIRST ttEstCostCategory NO-LOCK 
        WHERE ttEstCostCategory.estCostCategoryID EQ bf-ttEstCostDetail.estCostCategoryID 
        :
        RUN pCalcTotalsForCostDetail(BUFFER bf-ttEstCostDetail, BUFFER ttEstCostCategory, BUFFER ttEstCostForm, BUFFER ttEstCostHeader).
        bf-ttEstCostDetail.hasBeenProcessed = YES.
    END.        
    RELEASE bf-ttEstCostDetail.
            
END PROCEDURE.


PROCEDURE pBuildCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details for a given header and creates summary
              records for each
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

    RUN pGetEstCostCategoryTT(OUTPUT TABLE ttEstCostCategory).
    
    FOR EACH ttEstCostDetail NO-LOCK
        WHERE ttEstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID, 
        FIRST ttEstCostHeader NO-LOCK 
        WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID,
        FIRST ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
        AND ttEstCostForm.estCostFormID EQ ttEstCostDetail.estCostFormID, 
        FIRST ttEstCostCategory NO-LOCK 
        WHERE ttEstCostCategory.estCostCategoryID EQ ttEstCostDetail.estCostCategoryID
        :
        RUN pAddCostSummary(ttEstCostHeader.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, ttEstCostHeader.quantityMaster / 1000, "Header").
        RUN pAddCostSummary(ttEstCostForm.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, ttEstCostForm.quantityFGOnForm / 1000, "Form").
        
        FIND FIRST ttEstCostBlank NO-LOCK 
            WHERE ttEstCostBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
            AND ttEstCostBlank.estCostFormID EQ ttEstCostDetail.estCostFormID
            AND ttEstCostBlank.estCostBlankID EQ ttEstCostDetail.estCostBlankID
            NO-ERROR.
        IF AVAILABLE ttEstCostBlank THEN 
        DO:  /*Blank-specific cost*/
            FIND FIRST ttEstCostItem NO-LOCK 
                WHERE ttEstCostItem.estCostHeaderID EQ ttEstCostBlank.estCostHeaderID
                AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID
                NO-ERROR.
            IF AVAILABLE ttEstCostItem THEN 
                RUN pAddCostSummary(ttEstCostItem.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, ttEstCostItem.quantityRequired / 1000, "Item").
           
        END.
        ELSE /*Divide up the Form-level Costs into each item*/
            FOR EACH ttEstCostBlank NO-LOCK
                WHERE ttEstCostBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
                AND ttEstCostBlank.estCostFormID EQ ttEstCostDetail.estCostFormID, 
                FIRST ttEstCostItem NO-LOCK  
                WHERE ttEstCostItem.estCostHeaderID EQ ttEstCostBlank.estCostHeaderID
                AND ttEstCostItem.estCostItemID EQ ttEstCostBlank.estCostItemID :
                RUN pAddCostSummary(ttEstCostItem.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal * ttEstCostBlank.pctOfForm, ttEstCostItem.quantityRequired / 1000, "Item").
           
            END.
        
    END.        


END PROCEDURE.

PROCEDURE pGetRequiredTotal PRIVATE:
    DEFINE INPUT PARAMETER dQuantityRequiredNoWaste    AS DECIMAL NO-UNDO. 
    DEFINE INPUT PARAMETER dQuantityRequiredSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER dQuantityRequiredRunWaste   AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER dQuantityRequiredMinDiff    AS DECIMAL NO-UNDO.
    
    DEFINE OUTPUT PARAMETER dQuantityRequiredTotal AS DECIMAL NO-UNDO.
    
    dQuantityRequiredTotal = dQuantityRequiredNoWaste + dQuantityRequiredSetupWaste +
                             dQuantityRequiredRunWaste + dQuantityRequiredMinDiff.
END PROCEDURE.

PROCEDURE pCalcEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a ttEstCostMaterial buffer, calculate simple calculated fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader   FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm     FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-ttEstCostBlank      FOR ttEstCostBlank.
    
    DEFINE VARIABLE dCostDeviation AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInM   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dtotal AS DECIMAL.
    
    RUN pGetRequiredTotal(ipbf-ttEstCostMaterial.quantityRequiredNoWaste,
                          ipbf-ttEstCostMaterial.quantityRequiredSetupWaste,
                          ipbf-ttEstCostMaterial.quantityRequiredRunWaste,
                          ipbf-ttEstCostMaterial.quantityRequiredMinDiff,
                          OUTPUT ipbf-ttEstCostMaterial.quantityRequiredTotal).
            
    IF ipbf-ttEstCostMaterial.costOverridePerUOM EQ 0 THEN 
    DO:
        IF ipbf-ttEstCostMaterial.isPurchasedFG THEN
            RUN pGetEstFarmCosts(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostMaterial, ipbf-ttEstCostMaterial.quantityRequiredTotal, ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.vendorID, 
                OUTPUT ipbf-ttEstCostMaterial.costPerUOM, OUTPUT ipbf-ttEstCostMaterial.costUOM,  OUTPUT ipbf-ttEstCostMaterial.costSetup, OUTPUT ipbf-ttEstCostMaterial.vendorID, OUTPUT ipbf-ttEstCostMaterial.costPerUOMDeviation).
        ELSE 
            RUN pGetEstMaterialCosts(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostMaterial,ipbf-ttEstCostMaterial.quantityRequiredTotal,ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.vendorID, 
                OUTPUT ipbf-ttEstCostMaterial.costPerUOM, OUTPUT ipbf-ttEstCostMaterial.costUOM,  OUTPUT ipbf-ttEstCostMaterial.costSetup, OUTPUT ipbf-ttEstCostMaterial.vendorID, OUTPUT ipbf-ttEstCostMaterial.costPerUOMDeviation).
            
    END.
    ELSE 
        ipbf-ttEstCostMaterial.costPerUOM = ipbf-ttEstCostMaterial.costOverridePerUOM.
    
    IF ipbf-ttEstCostMaterial.costUOM EQ "" THEN 
        ipbf-ttEstCostMaterial.costUOM = "EA".
    IF ipbf-ttEstCostMaterial.costUOM NE ipbf-ttEstCostMaterial.quantityUOM THEN 
    DO:
        RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.costUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, 
            ipbf-ttEstCostMaterial.quantityRequiredNoWaste, OUTPUT ipbf-ttEstCostMaterial.quantityRequiredNoWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.costUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, 
            ipbf-ttEstCostMaterial.quantityRequiredSetupWaste, OUTPUT ipbf-ttEstCostMaterial.quantityRequiredSetupWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.costUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, 
            ipbf-ttEstCostMaterial.quantityRequiredRunWaste, OUTPUT ipbf-ttEstCostMaterial.quantityRequiredRunWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.costUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, 
            ipbf-ttEstCostMaterial.quantityRequiredMinDiff, OUTPUT ipbf-ttEstCostMaterial.quantityRequiredMinDiffInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", ipbf-ttEstCostMaterial.quantityUOM, ipbf-ttEstCostMaterial.costUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, 
            ipbf-ttEstCostMaterial.quantityRequiredTotal, OUTPUT ipbf-ttEstCostMaterial.quantityRequiredTotalInCUOM).
    END.
    ELSE 
        ASSIGN 
            ipbf-ttEstCostMaterial.quantityRequiredNoWasteInCUOM    = ipbf-ttEstCostMaterial.quantityRequiredNoWaste
            ipbf-ttEstCostMaterial.quantityRequiredSetupWasteInCUOM = ipbf-ttEstCostMaterial.quantityRequiredSetupWaste
            ipbf-ttEstCostMaterial.quantityRequiredRunWasteInCUOM   = ipbf-ttEstCostMaterial.quantityRequiredRunWaste
            ipbf-ttEstCostMaterial.quantityRequiredMinDiffInCUOM    = ipbf-ttEstCostMaterial.quantityRequiredMinDiff
            ipbf-ttEstCostMaterial.quantityRequiredTotalInCUOM      = ipbf-ttEstCostMaterial.quantityRequiredTotal
            .
             
    IF NOT ipbf-ttEstCostMaterial.noCharge THEN 
    DO:
        ASSIGN 
            ipbf-ttEstCostMaterial.costTotalNoWaste    = ipbf-ttEstCostMaterial.quantityRequiredNoWasteInCUOM * ipbf-ttEstCostMaterial.costPerUOM
            ipbf-ttEstCostMaterial.costTotalDeviation  = ipbf-ttEstCostMaterial.quantityRequiredTotalInCUOM * ipbf-ttEstCostMaterial.costPerUOMDeviation
            ipbf-ttEstCostMaterial.costTotalSetupWaste = ipbf-ttEstCostMaterial.quantityRequiredSetupWasteInCUOM * ipbf-ttEstCostMaterial.costPerUOM
            ipbf-ttEstCostMaterial.costTotalRunWaste   = ipbf-ttEstCostMaterial.quantityRequiredRunWasteInCUOM * ipbf-ttEstCostMaterial.costPerUOM
            ipbf-ttEstCostMaterial.costTotalMinDiff    = ipbf-ttEstCostMaterial.quantityRequiredMinDiffInCUOM * ipbf-ttEstCostMaterial.costPerUOM
            ipbf-ttEstCostMaterial.costTotal           = ipbf-ttEstCostMaterial.costTotalNoWaste + ipbf-ttEstCostMaterial.costTotalSetupWaste + 
                                                                 ipbf-ttEstCostMaterial.costTotalRunWaste + ipbf-ttEstCostMaterial.costTotalMinDiff + 
                                                                 ipbf-ttEstCostMaterial.costSetup
            .
        dQuantityInM = 0.
        IF ipbf-ttEstCostMaterial.blankNo NE 0 THEN 
        DO:
            FIND FIRST bf-ttEstCostBlank NO-LOCK 
                WHERE bf-ttEstCostBlank.estCostBlankID EQ ipbf-ttEstCostMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE bf-ttEstCostBlank THEN 
                dQuantityInM = bf-ttEstCostBlank.quantityRequired / 1000.
        END.
        IF dQuantityInM EQ 0 THEN 
            dQuantityInM = ipbf-ttEstCostForm.quantityFGOnForm / 1000. 
        IF dQuantityInM EQ 0 THEN 
            dQuantityInM = 1.
        ASSIGN  
            ipbf-ttEstCostMaterial.costTotalPerMFinished           = ipbf-ttEstCostMaterial.costTotal / dQuantityInM
            ipbf-ttEstCostMaterial.costTotalPerMFinishedNoWaste    = ipbf-ttEstCostMaterial.costTotalNoWaste / dQuantityInM
            ipbf-ttEstCostMaterial.costTotalPerMFinishedSetupWaste = ipbf-ttEstCostMaterial.costTotalSetupWaste  / dQuantityInM
            ipbf-ttEstCostMaterial.costTotalPerMFinishedRunWaste   = ipbf-ttEstCostMaterial.costTotalRunWaste / dQuantityInM
            .
    END.
    ELSE 
        ASSIGN 
            ipbf-ttEstCostMaterial.costPerUOM = 0
            ipbf-ttEstCostMaterial.costSetup  = 0
            .        
    
END PROCEDURE.

PROCEDURE pCalcEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a ttEstCostMisc buffer, calculate common fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMisc   FOR ttEstCostMisc.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    ipbf-ttEstCostMisc.profitTotal = fGetProfit(ipbf-ttEstCostMisc.costTotalBeforeProfit, ipbf-ttEstCostMisc.profitPercent, ipbf-ttEstCostMisc.profitPercentType).
    IF ipbf-ttEstCostMisc.SIMON EQ "M" THEN 
        ipbf-ttEstCostMisc.costTotal = ipbf-ttEstCostMisc.costTotalBeforeProfit.
    ELSE 
        ipbf-ttEstCostMisc.costTotal = ipbf-ttEstCostMisc.costTotalBeforeProfit + ipbf-ttEstCostMisc.profitTotal.
    ipbf-ttEstCostMisc.costTotalPerMFinished = ipbf-ttEstCostMisc.costTotal / (ipbf-ttEstCostForm.quantityFGOnForm / 1000).

END PROCEDURE.

PROCEDURE pCalcEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostOperation buffer, calculate simple calculated fields
     Notes: Should replace end if pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    
    DEFINE VARIABLE dCostMinimumDiff       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffFactor AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffSetup  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostMinimumDiffRun    AS DECIMAL NO-UNDO.
    
        
    IF ipbf-ttEstCostOperation.speed NE 0 THEN
        IF ipbf-ttEstCostOperation.isSpeedInLF THEN
            ipbf-ttEstCostOperation.hoursRun = ipbf-ttEstCostOperation.quantityInAfterSetupWasteLF / ipbf-ttEstCostOperation.speed. 
        ELSE 
            ipbf-ttEstCostOperation.hoursRun = ipbf-ttEstCostOperation.quantityInAfterSetupWaste / ipbf-ttEstCostOperation.speed.
    ELSE 
        ipbf-ttEstCostOperation.hoursRun = 0.
    
    IF ipbf-ttEstCostOperation.numOutDivisor GT 0 THEN
        ipbf-ttEstCostOperation.hoursRun = ipbf-ttEstCostOperation.hoursRun / ipbf-ttEstCostOperation.numOutDivisor.
        
    IF ipbf-ttEstCostOperation.hoursRun LT ipbf-ttEstCostOperation.hoursRunMinimum THEN 
        ipbf-ttEstCostOperation.hoursRun = ipbf-ttEstCostOperation.hoursRunMinimum.
    
    ASSIGN    
        ipbf-ttEstCostOperation.costPerHourTotalRun   = ipbf-ttEstCostOperation.costPerManHourDLRun * ipbf-ttEstCostOperation.crewSizeRun + 
                                                     ipbf-ttEstCostOperation.costPerHourFORun + ipbf-ttEstCostOperation.costPerHourVORun
        ipbf-ttEstCostOperation.costPerHourTotalSetup = ipbf-ttEstCostOperation.costPerManHourDLSetup * ipbf-ttEstCostOperation.crewSizeSetup + 
                                                     ipbf-ttEstCostOperation.costPerHourFOSetup + ipbf-ttEstCostOperation.costPerHourVOSetup
        ipbf-ttEstCostOperation.costTotalDLSetup      = ipbf-ttEstCostOperation.hoursSetup * ipbf-ttEstCostOperation.crewSizeSetup * ipbf-ttEstCostOperation.costPerManHourDLSetup
        ipbf-ttEstCostOperation.costTotalVOSetup      = ipbf-ttEstCostOperation.hoursSetup * ipbf-ttEstCostOperation.costPerHourVOSetup
        ipbf-ttEstCostOperation.costTotalFOSetup      = ipbf-ttEstCostOperation.hoursSetup * ipbf-ttEstCostOperation.costPerHourFOSetup
        ipbf-ttEstCostOperation.costTotalDLRun        = ipbf-ttEstCostOperation.hoursRun * ipbf-ttEstCostOperation.crewSizeRun * ipbf-ttEstCostOperation.costPerManHourDLRun
        ipbf-ttEstCostOperation.costTotalVORun        = ipbf-ttEstCostOperation.hoursRun * ipbf-ttEstCostOperation.costPerHourVORun
        ipbf-ttEstCostOperation.costTotalFORun        = ipbf-ttEstCostOperation.hoursRun * ipbf-ttEstCostOperation.costPerHourFORun
        ipbf-ttEstCostOperation.costTotalSetup        = ipbf-ttEstCostOperation.costTotalDLSetup + ipbf-ttEstCostOperation.costTotalVOSetup + ipbf-ttEstCostOperation.costTotalFOSetup
        ipbf-ttEstCostOperation.costTotalRun          = ipbf-ttEstCostOperation.costTotalDLRun + ipbf-ttEstCostOperation.costTotalVORun + ipbf-ttEstCostOperation.costTotalFORun
        ipbf-ttEstCostOperation.costTotal             = ipbf-ttEstCostOperation.costTotalRun + ipbf-ttEstCostOperation.costTotalSetup
        .
    
    /*Apply minimum Charge*/
    IF glApplyOperationMinimumCharge AND ipbf-ttEstCostOperation.costTotal GT 0 THEN 
    DO:
        IF glApplyOperationMinimumChargeRunOnly THEN 
        DO:
            IF ipbf-ttEstCostOperation.costMinimum GT ipbf-ttEstCostOperation.costTotalRun THEN 
                ASSIGN 
                    dCostMinimumDiffRun = ipbf-ttEstCostOperation.costMinimum - ipbf-ttEstCostOperation.costTotalRun.
        END.
        ELSE 
        DO:
            IF ipbf-ttEstCostOperation.costMinimum GT ipbf-ttEstCostOperation.costTotal THEN 
                ASSIGN 
                    dCostMinimumDiff       = ipbf-ttEstCostOperation.costMinimum - ipbf-ttEstCostOperation.costTotal
                    dCostMinimumDiffFactor = ipbf-ttEstCostOperation.costTotalSetup / ipbf-ttEstCostOperation.costTotal
                    dCostMinimumDiffSetup  = dCostMinimumDiff * dCostMinimumDiffFactor
                    dCostMinimumDiffRun    = dCostMinimumDiff - dCostMinimumDiffSetup
                    .
        END.
        ASSIGN 
            ipbf-ttEstCostOperation.costTotalDlRun   = ipbf-ttEstCostOperation.costTotalDLRun + dCostMinimumDiffRun
            ipbf-ttEstCostOperation.costTotalDlSetup = ipbf-ttEstCostOperation.costTotalDLSetup + dCostMinimumDiffSetup
            ipbf-ttEstCostOperation.costTotalSetup   = ipbf-ttEstCostOperation.costTotalDLSetup + ipbf-ttEstCostOperation.costTotalVOSetup + ipbf-ttEstCostOperation.costTotalFOSetup
            ipbf-ttEstCostOperation.costTotalRun     = ipbf-ttEstCostOperation.costTotalDLRun + ipbf-ttEstCostOperation.costTotalVORun + ipbf-ttEstCostOperation.costTotalFORun
            ipbf-ttEstCostOperation.costTotal        = ipbf-ttEstCostOperation.costTotalRun + ipbf-ttEstCostOperation.costTotalSetup
            .
                    
    END. 
                        
END PROCEDURE.

PROCEDURE pProcessBoard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ef              FOR ef.
    
    DEFINE           BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE           BUFFER bf-item            FOR ITEM.
    DEFINE           BUFFER bf-item-bom        FOR item-bom.
    
    DEFINE VARIABLE lFoundBOM AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidBOM AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMaterialTypeCalculation AS CHARACTER NO-UNDO.
    /*Get best vendor*/
    DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cScope              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lAvailAdder         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iCount              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQtyTotal           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cAdderList          AS CHARACTER EXTENT 6 NO-UNDO.
    
    DO iCount = 1 TO 6:
        IF ipbf-ef.adder[iCount] <> "" THEN
            lAvailAdder = TRUE.
        cAdderList[iCount] = ipbf-ef.adder[iCount].
    END.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipbf-ef.board
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Board '" + ipbf-ef.board + "' is not valid", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    IF NOT fIsBoardMaterial(bf-item.mat-type) THEN  
    DO:
        RUN pAddError("Board '" + ipbf-ef.board + "' is valid material but not a board material type.", giErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.   
    FOR EACH bf-item-bom NO-LOCK
        WHERE bf-item-bom.company EQ bf-item.company
        AND bf-item-bom.parent-i EQ bf-item.i-no
        AND bf-item-bom.i-no NE ""
        AND bf-item-bom.i-no NE "0"
        AND bf-item-bom.line# LT 9:    
        RUN pProcessBoardBOM(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-item-bom, OUTPUT lValidBom).
        IF NOT lFoundBom AND lValidBom THEN 
            lFoundBom = YES.
    END.
    IF lFoundBom THEN 
    DO:
        IF ipbf-ef.adh-code NE "" THEN 
            RUN pProcessBoardBOMAdhesive(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.adh-code, ipbf-ef.adh-sqin).
        IF ipbf-ef.lam-code NE "" THEN 
            RUN pProcessBoardBOMLaminate(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.lam-code).
    END. 
    ELSE 
    DO: 
        RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.board, 0, BUFFER bf-ttEstCostMaterial).
        ASSIGN 
            bf-ttEstCostMaterial.isPrimarySubstrate         = YES
            bf-ttEstCostMaterial.addToWeightNet             = YES
            bf-ttEstCostMaterial.addToWeightTare            = NO
                                           
            bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
            bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
            bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
            bf-ttEstCostMaterial.quantityUOMWaste           = "EA"
            bf-ttEstCostMaterial.quantityUOM                = "EA"
            bf-ttEstCostMaterial.basisWeight                = bf-item.basis-w
            bf-ttEstCostMaterial.dimWidth                   = ipbf-ttEstCostForm.grossWidth
            bf-ttEstCostMaterial.dimLength                  = ipbf-ttEstCostForm.grossLength
            bf-ttEstCostMaterial.dimDepth                   = ipbf-ttEstCostForm.grossDepth
            bf-ttEstCostMaterial.noCharge                   = ipbf-ttEstCostForm.noCost
            .
        
        RUN pCalcBoardCostFromBlank (BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMaterial).
            
        cMaterialTypeCalculation = fGetMatTypeCalc(bf-ttEstCostMaterial.company, bf-ttEstCostMaterial.materialType).
        IF cMaterialTypeCalculation NE gcMaterialTypeCalcDefault THEN
            RUN pProcessMaterialTypeCalc(BUFFER bf-ttEstCostMaterial, cMaterialTypeCalculation).
            
        IF ipbf-ttEstCostForm.costOverridePerUOM NE 0 THEN 
            ASSIGN 
                bf-ttEstCostMaterial.costOverridePerUOM = ipbf-ttEstCostForm.costOverridePerUOM
                bf-ttEstCostMaterial.costUOM            = ipbf-ttEstCostForm.costOverrideUOM
                .
                
        IF glVendItemCost THEN
            ASSIGN 
                cScope              = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-RM-Over")
                lIncludeBlankVendor = YES.
        RUN pGetRequiredTotal(bf-ttEstCostMaterial.quantityRequiredNoWaste,
            bf-ttEstCostMaterial.quantityRequiredSetupWaste,
            bf-ttEstCostMaterial.quantityRequiredRunWaste,
            bf-ttEstCostMaterial.quantityRequiredMinDiff,
            OUTPUT dQtyTotal).
        /*Get Best Vendor*/
        IF lAvailAdder AND NOT glUseBlankVendor THEN
            RUN VendCost_GetBestVendorWithAdders(bf-ttEstCostMaterial.Company,
                                                 bf-ttEstCostMaterial.ItemID,
                                                 "RM", 
                                                cScope, 
                                                lIncludeBlankVendor, 
                                                bf-ttEstCostMaterial.estimateNo, 
                                                bf-ttEstCostMaterial.formNo, 
                                                bf-ttEstCostMaterial.blankNo,
                                                dQtyTotal,
                                                bf-ttEstCostMaterial.quantityUOM, 
                                                bf-ttEstCostMaterial.dimLength,  
                                                bf-ttEstCostMaterial.dimWidth, 
                                                bf-ttEstCostMaterial.dimDepth, 
                                                bf-ttEstCostMaterial.dimUOM, 
                                                bf-ttEstCostMaterial.basisWeight, 
                                                bf-ttEstCostMaterial.basisWeightUOM,
                                                cAdderList,
                                                OUTPUT bf-ttEstCostMaterial.VendorID,
                                                OUTPUT lError,
                                                OUTPUT cMessage).
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).
        RUN pConvertQuantityFromUOMToUOM(bf-ttEstCostMaterial.company, bf-ttEstCostMaterial.itemID, "RM", bf-ttEstCostMaterial.quantityUOM, bf-ttEstCostMaterial.weightUOM, 
                    bf-ttEstCostMaterial.basisWeight, bf-ttEstCostMaterial.dimLength, bf-ttEstCostMaterial.dimWidth, bf-ttEstCostMaterial.dimDepth, bf-ttEstCostMaterial.quantityRequiredTotal, 
                    OUTPUT bf-ttEstCostMaterial.weightTotal).   
    END.
    
    ASSIGN 
        ipbf-ttEstCostForm.grossQtyRequiredTotal       = ipbf-ttEstCostForm.grossQtyRequiredNoWaste + ipbf-ttEstCostForm.grossQtyRequiredSetupWaste + ipbf-ttEstCostForm.grossQtyRequiredRunWaste
        ipbf-ttEstCostForm.grossQtyRequiredTotalArea   = ipbf-ttEstCostForm.grossQtyRequiredTotal * ipbf-ttEstCostForm.grossArea / 1000
        ipbf-ttEstCostForm.grossQtyRequiredTotalWeight = ipbf-ttEstCostForm.grossQtyRequiredTotalArea * ipbf-ttEstCostForm.basisWeight
        .
    
            
END PROCEDURE.

PROCEDURE pProcessFarm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given blank, create the "Materials" for farm out
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank  FOR ttEstCostBlank.
    
    DEFINE           BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
        
    RUN pAddEstFarm(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostBlank, BUFFER bf-ttEstCostMaterial).
    IF AVAILABLE bf-ttEstCostMaterial THEN 
    DO:
        ASSIGN 
            bf-ttEstCostMaterial.isPrimarySubstrate      = NO
            bf-ttEstCostMaterial.addToWeightNet          = YES
            bf-ttEstCostMaterial.addToWeightTare         = NO
            bf-ttEstCostMaterial.isPurchasedFG           = YES                               
            bf-ttEstCostMaterial.quantityRequiredNoWaste = ipbf-ttEstCostBlank.quantityRequired
            .
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).
    END.
    
    ASSIGN 
        ipbf-ttEstCostForm.grossQtyRequiredTotal       = ipbf-ttEstCostForm.grossQtyRequiredNoWaste + ipbf-ttEstCostForm.grossQtyRequiredSetupWaste + ipbf-ttEstCostForm.grossQtyRequiredRunWaste
        ipbf-ttEstCostForm.grossQtyRequiredTotalArea   = ipbf-ttEstCostForm.grossQtyRequiredTotal * ipbf-ttEstCostForm.grossArea / 1000
        ipbf-ttEstCostForm.grossQtyRequiredTotalWeight = ipbf-ttEstCostForm.grossQtyRequiredTotalArea * ipbf-ttEstCostForm.basisWeight
        .
    
    
END PROCEDURE.

PROCEDURE pProcessGlues PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE           BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    
    DEFINE VARIABLE dQtyRequiredMinDiff AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iNumOut             AS INTEGER NO-UNDO.
        
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttEstCostOperation.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND ttEstCostOperation.isGluer, 
        EACH ttGlue NO-LOCK
        WHERE ttGlue.estHeaderID EQ ttEstCostOperation.estCostHeaderID
        AND ttGlue.estFormID EQ ttEstCostOperation.estCostFormID
        AND (ttGlue.estBlankID EQ ttEstCostOperation.estCostBlankID 
        OR (ttEstCostOperation.estCostBlankID EQ ? 
        AND (ttEstCostOperation.FeedType EQ "S" OR ttEstCostOperation.FeedType EQ "R")))
        ,
        FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ttGlue.estHeaderID
        AND ttEstCostBlank.estCostFormID EQ ttGlue.estFormID 
        AND ttEstCostBlank.estCostBlankID EQ ttGlue.estBlankID 
        BY ttEstCostOperation.sequenceOfOperation DESCENDING: 

        RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ttGlue.cItemID, ttEstCostBlank.estCostBlankID, BUFFER bf-ttEstCostMaterial).
        
        iNumOut = IF ttEstCostOperation.FeedType EQ "R" THEN ipbf-ttEstCostForm.numOutNet * ttEstCostBlank.numOut 
                  ELSE IF ttEstCostOperation.FeedType EQ "S" THEN ttEstCostBlank.numOut
                  ELSE 1.

        ASSIGN    
            bf-ttEstCostMaterial.addToWeightNet             = YES
            bf-ttEstCostMaterial.quantityRequiredNoWaste    = ttEstCostOperation.quantityInNoWaste * ttGlue.dQtyRequiredPerBlank * iNumOut
            bf-ttEstCostMaterial.quantityRequiredRunWaste   = ttEstCostOperation.quantityInRunWaste * ttGlue.dQtyRequiredPerBlank * iNumOut
            bf-ttEstCostMaterial.quantityRequiredSetupWaste = ttEstCostOperation.quantityInSetupWaste * ttGlue.dQtyRequiredPerBlank * iNumOut
            dQtyRequiredMinDiff                           = ttGlue.dMinLbsPerJob - 
                                                    (bf-ttEstCostMaterial.quantityRequiredNoWaste + bf-ttEstCostMaterial.quantityRequiredRunWaste + bf-ttEstCostMaterial.quantityRequiredSetupWaste)
            bf-ttEstCostMaterial.quantityUOM                = ttGlue.cQtyUOM
            .             
        IF dQtyRequiredMinDiff GT 0 THEN 
            bf-ttEstCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).
        
    END.

END PROCEDURE.

PROCEDURE pBuildHeader PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Builds all fields on the estCostHeader record
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    
    DEFINE           BUFFER bf-est             FOR est.
    DEFINE           BUFFER bf-ce-ctrl         FOR ce-ctrl.
     
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipbf-ttEstCostHeader.company
        AND bf-est.est-no EQ ipbf-ttEstCostHeader.estimateNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Estimate '" + ipbf-ttEstCostHeader.estimateNo + "' not valid", giErrorCritical, ipbf-ttEstCostHeader.estCostHeaderID, 0,0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END. 
    FIND FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ipbf-ttEstCostHeader.company
        AND bf-ce-ctrl.loc EQ bf-est.loc
        NO-ERROR.
    IF NOT AVAILABLE bf-ce-ctrl THEN 
        FIND FIRST bf-ce-ctrl NO-LOCK 
            WHERE bf-ce-ctrl.company EQ ipbf-ttEstCostHeader.company
            NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Control File not found for company '" + ipbf-ttEstCostHeader.company + "'", giErrorCritical, ipbf-ttEstCostHeader.estCostHeaderID, 0,0, ipbf-ttEstCostHeader.quantityMaster).
        RETURN.
    END.
    ASSIGN 
        ipbf-ttEstCostHeader.industry                    = IF bf-est.est-type LE 4 THEN gcIndustryFolding ELSE gcIndustryCorrugated
        ipbf-ttEstCostHeader.estType                     = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", bf-est.est-type, bf-est.estimateTypeID)
        ipbf-ttEstCostHeader.warehouseID                 = bf-est.loc
        ipbf-ttEstCostHeader.marginOn                    = bf-ce-ctrl.sell-by
        ipbf-ttEstCostHeader.marginPct                   = bf-ce-ctrl.prof-mrkup
        ipbf-ttEstCostHeader.warehouseMarkupPct          = bf-ce-ctrl.whse-mrkup / 100 /*ctrl[1]*/
        ipbf-ttEstCostHeader.handlingChargePct           = bf-ce-ctrl.hand-pct / 100 /*ctrl[2]*/
        ipbf-ttEstCostHeader.handlingRateRMPerCWT        = bf-ce-ctrl.rm-rate /*ctrl[3]*/ /*NOTE CHANGED to be /100 */
        
        
        
        ipbf-ttEstCostHeader.showCommissions             = bf-ce-ctrl.comm-add /*ctrl[5]*/
        ipbf-ttEstCostHeader.showLaborRates              = bf-ce-ctrl.sho-labor /*ctrl[7]*/
        /*        ipbf-ttEstCostHeader.addToFactCostFreight        = bf-ce-ctrl.shp-add /*ctrl[6]*/     */
        /*        ipbf-ttEstCostHeader.addToFactCostSpecial1       = bf-ce-ctrl.spec-add[1] /*ctrl[13]*/*/
        /*        ipbf-ttEstCostHeader.addToFactCostSpecial2       = bf-ce-ctrl.spec-add[2] /*ctrl[14]*/*/
        /*        ipbf-ttEstCostHeader.addToFactCostSpecial3       = bf-ce-ctrl.spec-add[3] /*ctrl[15]*/*/
        /*        ipbf-ttEstCostHeader.addToFactCostGSA            = bf-ce-ctrl.spec-add[6] /*ctrl[16]*/*/
        /*        ipbf-ttEstCostHeader.addToFactCostRoyalty        = bf-ce-ctrl.spec-add[8] /*ctrl[18]*/*/
        /*        ipbf-ttEstCostHeader.addToFactCostComm           = bf-ce-ctrl.spec-add[7] /*ctrl[17]*/*/
        ipbf-ttEstCostHeader.foldPct                     = bf-ce-ctrl.fold-pct / 100 /*ctrl[19]*/ /*NOTE CHANGED to be /100 */            
        ipbf-ttEstCostHeader.handlingRateFGPerCWT        = bf-ce-ctrl.fg-rate   /*ld-fg-rate*/
        ipbf-ttEstCostHeader.handlingRateRMFarmPerCWT    = bf-ce-ctrl.rm-rate-farm 
        ipbf-ttEstCostHeader.handlingRateFGFarmPerCWT    = bf-ce-ctrl.fg-rate-farm 
        ipbf-ttEstCostHeader.handlingChargeFarmPct       = bf-ce-ctrl.hand-pct-farm / 100
        ipbf-ttEstCostHeader.directMaterialPct           = gdMaterialMarkup / 100           
        ipbf-ttEstCostHeader.weightUOM                   = gcDefaultWeightUOM     
        
        ipbf-ttEstCostHeader.special1MarkupPct           = IF bf-ce-ctrl.spec-%[1] < 1 THEN bf-ce-ctrl.spec-%[1] ELSE 0 /*ctrl[4] - already a fraction?*/ 
        ipbf-ttEstCostHeader.special1FlatValue           = IF bf-ce-ctrl.spec-%[1] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[1] /*REFACTOR - treatment of Special Costs*/            
        ipbf-ttEstCostHeader.special2MarkupPct           = IF bf-ce-ctrl.spec-%[2] < 1 THEN bf-ce-ctrl.spec-%[2] ELSE 0 /*ctrl[4] - already a fraction?*/     
        ipbf-ttEstCostHeader.special2FlatValue           = IF bf-ce-ctrl.spec-%[2] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[2] /*REFACTOR - treatment of Special Costs*/
        ipbf-ttEstCostHeader.special3MarkupPct           = IF bf-ce-ctrl.spec-%[3] < 1 THEN bf-ce-ctrl.spec-%[3] ELSE 0 /*ctrl[4] - already a fraction?*/ 
        ipbf-ttEstCostHeader.special3FlatValue           = IF bf-ce-ctrl.spec-%[3] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[3] /*REFACTOR - treatment of Special Costs*/
        .
    
END PROCEDURE.

PROCEDURE pProcessInk PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttInk/ttEstCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostBlank     FOR ttEstCostBlank.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttInk            FOR ttInk.
    
 
    DEFINE           BUFFER bf-ttEstCostMaterial    FOR ttEstCostMaterial.
    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL.
    DEFINE VARIABLE dqtyRequiredMinDiff AS DECIMAL. 
        
    RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ttInk.cItemID, ipbf-ttEstCostBlank.estCostBlankID, BUFFER bf-ttEstCostMaterial).
        
    ASSIGN    
        bf-ttEstCostMaterial.addToWeightNet             = YES
        ipbf-ttInk.dQtyRequiredPerBlank                 = ipbf-ttInk.dCoveragePercent * ipbf-ttEstCostBlank.blankAreaNetWindow / ipbf-ttInk.dCoverageRate
        dQtyRequiredPerForm                             = ipbf-ttEstCostBlank.numOut * ipbf-ttInk.dQtyRequiredPerBlank
        bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostOperation.quantityInNoWaste * dQtyRequiredPerForm
        bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostOperation.quantityInRunWaste * dQtyRequiredPerForm + (ipbf-ttEstCostOperation.quantityInkLbsWastedPerColor * (ipbf-ttInk.iCountInks + ipbf-ttInk.iCountCoatings))
        bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostOperation.quantityInSetupWaste * dQtyRequiredPerForm + ipbf-ttEstCostOperation.quantityInkLbsWastedPerSetup
        dQtyRequiredMinDiff                             = ipbf-ttInk.dMinLbsPerJob - (bf-ttEstCostMaterial.quantityRequiredNoWaste + bf-ttEstCostMaterial.quantityRequiredRunWaste + bf-ttEstCostMaterial.quantityRequiredSetupWaste)
        bf-ttEstCostMaterial.quantityUOM                = ipbf-ttInk.cQtyUOM
        bf-ttEstCostMaterial.noCharge                   = ipbf-ttInk.lNoCharge
        .            
    IF dQtyRequiredMinDiff GT 0 THEN 
        bf-ttEstCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
    
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessLeaf PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttLeaf/estCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttLeaf           FOR ttLeaf.
    DEFINE INPUT PARAMETER ipdQtyRequiredPerFeed AS DECIMAL NO-UNDO.    
 
    DEFINE BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
        
    RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ttLeaf.cItemID, ipbf-ttLeaf.estBlankID, BUFFER bf-ttEstCostMaterial).
        
    ASSIGN    
        bf-ttEstCostMaterial.addToWeightNet             = YES
        bf-ttEstCostMaterial.dimLength                  = ipbf-ttLeaf.dDimLength
        bf-ttEstCostMaterial.dimWidth                   = ipbf-ttLeaf.dDimWidth
        bf-ttEstCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostOperation.quantityInNoWaste * ipdQtyRequiredPerFeed
        bf-ttEstCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostOperation.quantityInRunWaste * ipdQtyRequiredPerFeed
        bf-ttEstCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostOperation.quantityInSetupWaste * ipdQtyRequiredPerFeed
        bf-ttEstCostMaterial.quantityUOM                = ipbf-ttLeaf.cQtyUOM
        bf-ttEstCostMaterial.basisWeight                = 144000 / ipbf-ttLeaf.dCoverageRate
        .             

    RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessInks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for inks with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    /*Inks*/
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttEstCostOperation.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND ttEstCostOperation.isPrinter, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstCostOperation.estCostHeaderID
        AND ttInk.estFormID EQ ttEstCostOperation.estCostFormID
        AND ttInk.iPass EQ ttEstCostOperation.pass
        AND ttInk.iCountInks GT 0,
        FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND ttEstCostBlank.estCostFormID EQ ttInk.estFormID 
        AND ttEstCostBlank.estCostBlankID EQ ttInk.estBlankID :
        
        RUN pProcessInk(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ttEstCostBlank, BUFFER ttEstCostOperation, BUFFER ttInk).    
        
    END.
    /*Coatings*/
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttEstCostOperation.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND ttEstCostOperation.isCoater, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstCostOperation.estCostHeaderID
        AND ttInk.estFormID EQ ttEstCostOperation.estCostFormID
        AND ttInk.iPass EQ ttEstCostOperation.pass
        AND ttInk.iCountCoatings GT 0,
        FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND ttEstCostBlank.estCostFormID EQ ttInk.estFormID 
        AND ttEstCostBlank.estCostBlankID EQ ttInk.estBlankID :
            
        RUN pProcessInk(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ttEstCostBlank, BUFFER ttEstCostOperation, BUFFER ttInk).    
    END.

END PROCEDURE.

PROCEDURE pProcessLeafs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for leafs with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE VARIABLE dQtyRequiredPerFeed AS DECIMAL NO-UNDO.
    
    RUN pBuildLeafForEf(BUFFER ipbf-ef, BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm).
    
    FOR EACH ttLeaf NO-LOCK
        WHERE ttLeaf.estHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttLeaf.estFormID EQ ipbf-ttEstCostForm.estCostFormID
        ,
        FIRST ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttEstCostOperation.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND ttEstCostOperation.isLeafer
        AND (ttEstCostOperation.feedType EQ "S" AND ttLeaf.lIsSheetFed OR NOT ttLeaf.lIsSheetFed)  /*If leaf is not for a specific blank, must have a sheet fed leafer*/
        BY ttEstCostOperation.sequenceOfOperation DESCENDING:
        
        IF ttEstCostOperation.feedType NE "B" AND ttLeaf.iBlankNo NE 0 THEN  /*Allow blank specific leaf/window to be consumed by sheet fed machines*/
        DO:            
            FIND FIRST ttEstCostBlank NO-LOCK 
                WHERE ttEstCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND ttEstCostBlank.estCostFormID EQ ttLeaf.estFormID 
                AND ttEstCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstCostBlank THEN 
                dQtyRequiredPerFeed = ttEstCostBlank.numOut * ttLeaf.dQtyRequiredPerLeaf.
        END.
        ELSE 
            dQtyRequiredPerFeed = ttLeaf.dQtyRequiredPerLeaf.
            
        RUN pProcessLeaf(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ttEstCostOperation, BUFFER ttLeaf, dQtyRequiredPerFeed).    
        
    END.

   
END PROCEDURE.

PROCEDURE pProcessPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstCostMaterial for packing material with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.

    DEFINE           BUFFER bf-ttEstCostMaterial    FOR ttEstCostMaterial.
    DEFINE           BUFFER bf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE           BUFFER bf-ttEstCostBlank       FOR ttEstCostBlank.
    DEFINE           BUFFER bfUnitize-ttEstCostform FOR ttEstCostForm.
    
    DEFINE VARIABLE iCaseCount      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCases          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCasesProRata   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPalletCount    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPallets        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dPalletsProRata AS DECIMAL NO-UNDO.
        
    
    ASSIGN 
        iCaseCount = 0
        iCases     = 0
        iPallets   = 0
        .
        
    IF ipbf-ttEstCostHeader.isUnitizedSet  THEN 
    DO:
        /*Establish unitization form (Form 1)*/
        FIND FIRST bfUnitize-ttEstCostForm NO-LOCK 
            WHERE bfUnitize-ttEstCostForm.estCostHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID
            AND bfUnitize-ttEstCostForm.formNo EQ 1
            NO-ERROR.
    END.

    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstCostHeader.estCostHeaderID,
        FIRST bf-ttEstCostBlank EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-ttEstCostBlank.estCostFormID EQ ttPack.estFormID
        AND bf-ttEstCostBlank.estCostBlankID EQ ttPack.estBlankID,
        FIRST bf-ttEstCostForm NO-LOCK 
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-ttEstCostForm.estCostFormID EQ ttPack.estFormID
        BY lIsCase DESCENDING 
        BY lIsPallet DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostForm, ttPack.cItemID, bf-ttEstCostBlank.estCostBlankID, BUFFER bf-ttEstCostMaterial).
        bf-ttEstCostMaterial.basisWeight = 0.  //For Case material, the basis weight is not weight per MSF
        
        IF ttPack.lIsCase THEN 
        DO:
            IF ttPack.iCountPerSubUnit NE 0 THEN
                ASSIGN
                    iCaseCount    = ttPack.iCountPerSubUnit  
                    dCasesProRata = bf-ttEstCostBlank.quantityRequired / ttPack.iCountPerSubUnit
                    iCases        = fRoundUp(dCasesProRata * ttPack.dQtyMultiplier).
            ELSE 
            DO:
                /*Calc cases based on weight - REFACTOR since assumes weight is in LB/M */
                iCases     = fRoundUp(bf-ttEstCostBlank.quantityRequired * (bf-ttEstCostBlank.weightPerBlank) / ttPack.dWeightCapacity) * ttPack.dQtyMultiplier
                    .
                IF iCases GT 0 THEN 
                    iCaseCount = fRoundUp(bf-ttEstCostBlank.quantityRequired /  iCases)
                        .
            END.
            ASSIGN  
                bf-ttEstCostMaterial.addToWeightTare         = YES 
                bf-ttEstCostMaterial.quantityRequiredNoWaste = iCases
                bf-ttEstCostMaterial.quantityUOM             = ttPack.cQtyUOM
                bf-ttEstCostBlank.quantityPerSubUnit         = iCaseCount
                bf-ttEstCostBlank.quantityOfSubUnits         = iCases
                bf-ttEstCostMaterial.costOverridePerUOM      = ttPack.dCostPerUOMOverride
                bf-ttEstCostMaterial.noCharge                = ttPack.lNoCharge
                .            
            
            IF iCaseCount NE 0 THEN
                bf-ttEstCostMaterial.itemName = bf-ttEstCostMaterial.itemName + " (" + TRIM(STRING(iCaseCount,">>>>>9")) + ")".   
            bf-ttEstCostMaterial.weightTotal             = ttPack.dWeightTare * iCases.
        END.
        ELSE IF ttPack.lIsPallet THEN 
            DO:
                ASSIGN  
                    iPalletCount                               = IF ttPack.iCountPerUnit EQ 0 THEN ttPack.iCountSubUnitsPerUnit * iCaseCount ELSE ttPack.iCountPerUnit
                    dPalletsProRata                            = IF iPalletCount NE 0 THEN bf-ttEstCostBlank.quantityRequired / iPalletCount ELSE iPalletCount
                    iPallets                                   = fRoundUp(dPalletsProRata * ttPack.dQtyMultiplier) 
                    bf-ttEstCostMaterial.addToWeightTare         = YES
                    bf-ttEstCostMaterial.quantityRequiredNoWaste = iPallets
                    bf-ttEstCostMaterial.quantityUOM             = ttPack.cQtyUOM
                    bf-ttEstCostBlank.quantityOfUnits            = iPallets
                    bf-ttEstCostMaterial.costOverridePerUOM      = ttPack.dCostPerUOMOverride   
                    bf-ttEstCostMaterial.noCharge                = ttPack.lNoCharge
                    .            
        
                IF iPalletCount NE 0 THEN 
                    bf-ttEstCostMaterial.itemName = bf-ttEstCostMaterial.itemName + " (" + TRIM(STRING(iPalletCount,">>>>>9")) + ")".
                bf-ttEstCostMaterial.weightTotal             = ttPack.dWeightTare * iPallets.
            END.        
            ELSE 
            DO:
                IF ttPack.dQtyMultiplier GT 1 THEN /*If there are multiple packers per case/pallet, only add a packer if necessary*/
                    ASSIGN 
                        dPalletsProRata = bf-ttEstCostBlank.quantityRequired / MAX(1, bf-ttEstCostBlank.quantityPerSubUnit * bf-ttEstCostBlank.quantitySubUnitsPerUnit)
                        dCasesProRata   = bf-ttEstCostBlank.quantityRequired / MAX(1, bf-ttEstCostBlank.quantityPerSubUnit)
                        .
                ELSE 
                    ASSIGN 
                        dPalletsProRata = bf-ttEstCostBlank.quantityOfUnits
                        dCasesProRata   = bf-ttEstCostBlank.quantityOfSubUnits
                        .
         
                CASE ttPack.cQtyMultiplier:
                    WHEN "P" THEN 
                        bf-ttEstCostMaterial.quantityRequiredNoWaste = dPalletsProRata * ttPack.dQtyMultiplier.
                    WHEN "C" THEN 
                        bf-ttEstCostMaterial.quantityRequiredNoWaste = dCasesProRata * ttPack.dQtyMultiplier.
                    OTHERWISE 
                    bf-ttEstCostMaterial.quantityRequiredNoWaste = ttPack.dQtyMultiplier.
                END CASE.
                ASSIGN                                                 
                    bf-ttEstCostMaterial.addToWeightTare    = YES
                    bf-ttEstCostMaterial.quantityUOM        = ttPack.cQtyUOM
                    bf-ttEstCostMaterial.costOverridePerUOM = ttPack.dCostPerUOMOverride
                    bf-ttEstCostMaterial.noCharge           = ttPack.lNoCharge
                    .    
                IF bf-ttEstCostMaterial.quantityUOM EQ "EA" THEN 
                    bf-ttEstCostMaterial.quantityRequiredNoWaste = fRoundUp(bf-ttEstCostMaterial.quantityRequiredNoWaste).            
                bf-ttEstCostMaterial.weightTotal = ttPack.dWeightTare * bf-ttEstCostMaterial.quantityRequiredNoWaste.
            END.
        
        IF bf-ttEstCostForm.formNo EQ 0 AND AVAILABLE bfUnitize-ttEstCostForm THEN 
        DO:
            /*Associate Form 0 materials to the unitize form (Form 1)*/
            ASSIGN 
                bf-ttEstCostMaterial.estCostFormID  = bfUnitize-ttEstCostForm.estCostFormID
                bf-ttEstCostMaterial.estCostBlankID = 0
                .
            RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER bfUnitize-ttEstCostForm).
        END. 
        ELSE     
            RUN pCalcEstMaterial(BUFFER ipbf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER bf-ttEstCostForm).
        
    END.
    RELEASE bf-ttEstCostBlank.
       
    
END PROCEDURE.



PROCEDURE pGetEstFarmCosts PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an ttEstCostMaterial, fill in cost per UOM and Setup for 
    given vendor - Farm Tab version.
    
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader   FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostDeviation AS DECIMAL NO-UNDO.
       
    DEFINE VARIABLE cScope              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iIndex              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCostFound          AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE dRunQty             AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dRunCost            AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dSetups             AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dQtyInCUOM          AS DECIMAL.
    DEFINE VARIABLE dCostTotal          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.

    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.
    
    IF glVendItemCost THEN 
    DO:
        ASSIGN 
            cScope              = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-FG")
            lIncludeBlankVendor = YES
            .

        RUN VendCost_GetBestCost(ipbf-ttEstCostMaterial.company, 
            ipbf-ttEstCostMaterial.itemID, "FG", 
            cScope, lIncludeBlankVendor, 
            ipbf-ttEstCostMaterial.estimateNo, ipbf-ttEstCostMaterial.formNo, ipbf-ttEstCostMaterial.blankNo,
            ipdQty, ipcQtyUOM, 
            ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, ipbf-ttEstCostMaterial.dimUOM, 
            ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.basisWeightUOM,
            OUTPUT opdCost, OUTPUT opcCostUOM, OUTPUT opdSetup, OUTPUT opcVendorID, OUTPUT opdCostDeviation, OUTPUT dCostTotal,
            OUTPUT lError, OUTPUT cMessage).
    END.
    ELSE 
    DO:
        FIND FIRST e-itemfg NO-LOCK
            WHERE e-itemfg.company EQ ipbf-ttEstCostHeader.company
            AND e-itemfg.i-no EQ ipbf-ttEstCostMaterial.itemID
            NO-ERROR.
        IF AVAILABLE e-itemfg THEN
        DO:
            opcCostUom = e-itemfg.std-uom.
            RELEASE e-itemfg-vend.
            IF ipcVendNo NE "" THEN
                FIND FIRST e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-ttEstCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-ttEstCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-ttEstCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-ttEstCostMaterial.blankNo
                    AND e-itemfg-vend.vend-no EQ ipcVendNo
                    NO-ERROR.
            IF NOT AVAILABLE e-itemfg-vend THEN
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-ttEstCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-ttEstCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-ttEstCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-ttEstCostMaterial.blankNo
                    AND e-itemfg-vend.vend-no EQ ""
                    BY e-itemfg-vend.vend-no:
                    LEAVE.
                END.    
            IF NOT AVAILABLE e-itemfg-vend THEN
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-ttEstCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-ttEstCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-ttEstCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-ttEstCostMaterial.blankNo
                    BY e-itemfg-vend.vend-no:
                    LEAVE.
                END.    
    
            IF AVAILABLE e-itemfg-vend THEN
            DO: 
                IF e-itemfg-vend.std-uom NE "" THEN
                    opcCostUom = e-itemfg-vend.std-uom.
    
                DO iIndex = 1 TO 10:
                    ASSIGN
                        dRunQty[iIndex]  = e-itemfg-vend.run-qty[iIndex]
                        dRunCost[iIndex] = e-itemfg-vend.run-cost[iIndex]
                        dSetups[iIndex]  = e-itemfg-vend.setups[iIndex].
                END.
                IF opcCostUOM NE ipcQtyUOM THEN
                    RUN pConvertQuantityFromUOMToUOM(e-itemfg-vend.company, ipbf-ttEstCostMaterial.itemID, "FG", ipcQtyUOM,opcCostUOM,
                        ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth,
                        ipdQty, OUTPUT dQtyInCUOM).
                ELSE
                    dQtyInCUOM = ipdQty.
                DO iIndex = 1 TO 10:
                    IF dRunQty[iIndex] NE 0   AND
                        dRunQty[iIndex] GE dQtyInCUOM THEN
                    DO:
                        ASSIGN
                            lCostFound = YES
                            opdCost    = dRunCost[iIndex]
                            opdSetup   = dSetups[iIndex]
                            .
                        LEAVE.
                    END.
                END.
            END.
        END.
    END.                
END PROCEDURE.

PROCEDURE pGetEstMaterialCosts PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an ttEstCostMaterial, fill in cost per UOM and Setup for 
    given vendor.
     Notes:  replaces est/matcost.i (should move to costProcs)
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader   FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendorID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostDeviation AS DECIMAL NO-UNDO.
           
    DEFINE VARIABLE cScope              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludeBlankVendor AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iIndex              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lCostFound          AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE dRunQty             AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dRunCost            AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dSetups             AS DECIMAL   EXTENT 20.
    DEFINE VARIABLE dQtyInCUOM          AS DECIMAL.
    DEFINE VARIABLE dCostTotal          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUseBlank           AS LOGICAL   NO-UNDO.


    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.    
     
    IF glVendItemCost THEN 
    DO:
        ASSIGN 
            cScope              = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-RM-Over")
            lIncludeBlankVendor = YES
            .
        IF ipbf-ttEstCostMaterial.vendorID NE "" OR glUseBlankVendor THEN 
        DO:
            opcVendorID = ipbf-ttEstCostMaterial.vendorID.
            RUN GetVendorCost(ipbf-ttEstCostMaterial.company, ipbf-ttEstCostMaterial.itemID, "RM", 
                opcVendorID, "", ipbf-ttEstCostMaterial.estimateNo, ipbf-ttEstCostMaterial.formNo, ipbf-ttEstCostMaterial.blankNo, 
                ipdQty, ipcQtyUOM, 
                ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth, ipbf-ttEstCostMaterial.dimUOM, 
                ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.basisWeightUOM, 
                NO,
                OUTPUT opdCost, OUTPUT opdSetup, OUTPUT opcCostUOM, OUTPUT dCostTotal, OUTPUT lError, OUTPUT cMessage).
        END.
        ELSE 
        DO:                        
            RUN VendCost_GetBestCost(ipbf-ttEstCostMaterial.company, 
                ipbf-ttEstCostMaterial.itemID, 
                "RM", 
                cScope, 
                lIncludeBlankVendor, 
                ipbf-ttEstCostMaterial.estimateNo, 
                ipbf-ttEstCostMaterial.formNo, 
                ipbf-ttEstCostMaterial.blankNo,
                ipdQty, 
                ipcQtyUOM, 
                ipbf-ttEstCostMaterial.dimLength,  
                ipbf-ttEstCostMaterial.dimWidth, 
                ipbf-ttEstCostMaterial.dimDepth, 
                ipbf-ttEstCostMaterial.dimUOM, 
                ipbf-ttEstCostMaterial.basisWeight, 
                ipbf-ttEstCostMaterial.basisWeightUOM,
                OUTPUT opdCost, 
                OUTPUT opcCostUOM, 
                OUTPUT opdSetup, 
                OUTPUT opcVendorID, 
                OUTPUT opdCostDeviation, 
                OUTPUT dCostTotal,
                OUTPUT lError, 
                OUTPUT cMessage).
        END.
    END.
    ELSE 
    DO:         
        FIND FIRST e-item NO-LOCK
            WHERE e-item.company EQ ipbf-ttEstCostMaterial.company
            AND e-item.i-no EQ ipbf-ttEstCostMaterial.itemID
            NO-ERROR.
        IF AVAILABLE e-item THEN
        DO:
            opcCostUom = e-item.std-uom.
            RELEASE e-item-vend.
            IF ipcVendNo NE "" THEN
                FIND FIRST e-item-vend OF e-item NO-LOCK
                    WHERE e-item-vend.item-type EQ YES
                    AND e-item-vend.vend-no EQ ipcVendNo
                    NO-ERROR.
            IF NOT AVAILABLE e-item-vend THEN
                FOR EACH e-item-vend OF e-item NO-LOCK
                    WHERE e-item-vend.item-type EQ YES
                    AND e-item-vend.vend-no EQ ""
                    BY e-item-vend.vend-no:
                    LEAVE.
                END.
            IF NOT AVAILABLE e-item-vend THEN
                FOR EACH e-item-vend OF e-item NO-LOCK
                    WHERE e-item-vend.item-type EQ YES
                    BY e-item-vend.vend-no:
                    LEAVE.
                END.

            IF AVAILABLE e-item-vend THEN
            DO:
                IF e-item-vend.std-uom NE "" THEN
                    opcCostUom = e-item-vend.std-uom.

                DO iIndex = 1 TO 10:
                    ASSIGN
                        dRunQty[iIndex]  = e-item-vend.run-qty[iIndex]
                        dRunCost[iIndex] = e-item-vend.run-cost[iIndex]
                        dSetups[iIndex]  = e-item-vend.setups[iIndex].
                END.
                DO iIndex = 1 TO 10:
                    ASSIGN
                        dRunQty[iIndex + 10]  = e-item-vend.runQtyXtra[iIndex]
                        dRunCost[iIndex + 10] = e-item-vend.runCostXtra[iIndex]
                        dSetups[iIndex + 10]  = e-item-vend.setupsXtra[iIndex].
                END.
                IF opcCostUOM NE ipcQtyUOM THEN
                    RUN pConvertQuantityFromUOMToUOM(e-item-vend.company, ipbf-ttEstCostMaterial.itemID, "RM", ipcQtyUOM,opcCostUOM,
                        ipbf-ttEstCostMaterial.basisWeight, ipbf-ttEstCostMaterial.dimLength, ipbf-ttEstCostMaterial.dimWidth, ipbf-ttEstCostMaterial.dimDepth,
                        ipdQty, OUTPUT dQtyInCUOM).
                ELSE
                    dQtyInCUOM = ipdQty.
                DO iIndex = 1 TO 20:
                    IF dRunQty[iIndex] NE 0   AND
                        dRunQty[iIndex] GE dQtyInCUOM THEN
                    DO:
                        ASSIGN
                            lCostFound = YES
                            opdCost    = dRunCost[iIndex]
                            opdSetup   = dSetups[iIndex]
                            .
                        LEAVE.
                    END.
                END.
            END.
        END.
    END.    
    IF gcCECostSourceLookup NE "VendorCostOnly" AND ipbf-ttEstCostMaterial.isRealMaterial AND (opdCost EQ 0 OR lError) THEN
    DO:
        IF gcCECostSourceLookup EQ "VendorCostThenLast" THEN
            ASSIGN opdCost = ipbf-ttEstCostMaterial.costPerUOMLast.
        ELSE IF gcCECostSourceLookup EQ "VendorCostThenAverage" THEN
                ASSIGN opdCost = ipbf-ttEstCostMaterial.costPerUOMAvg.                    
        ASSIGN 
            opcCostUOM = ipbf-ttEstCostMaterial.quantityUOM.  /*REFACTOR? - What uom is avg and last cost in*/            
    END.        

END PROCEDURE.

PROCEDURE pGetMiscCostPerM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, master quantity and cost type, return the cost
     Notes: replaces ce/refest5a.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCostType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerM AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iLookupIndex AS INTEGER NO-UNDO.

    FIND FIRST reftable NO-LOCK 
        WHERE reftable.reftable EQ "EST-MISC"
        AND reftable.company  EQ ipbf-ef.company
        AND reftable.loc      EQ ipbf-ef.loc
        AND reftable.code     EQ TRIM(ipbf-ef.est-no) + STRING(ipbf-ef.form-no,"/99")
        AND reftable.code2    EQ ipcCostType + "-QTY" + STRING(ipiIndex,"99")
        NO-ERROR.
    IF AVAILABLE reftable THEN 
    DO iLookupIndex = 1 TO EXTENT(reftable.val):
        IF ipdQuantity LE reftable.val[iLookupIndex] THEN LEAVE.
        IF iLookupIndex = EXTENT(reftable.val) THEN 
        DO:
            iLookupIndex = 0.
            RELEASE reftable.
            LEAVE.
        END.
    END.
    IF AVAILABLE reftable THEN
        FIND FIRST reftable NO-LOCK
            WHERE reftable.reftable EQ "EST-MISC"
            AND reftable.company  EQ ipbf-ef.company
            AND reftable.loc      EQ ipbf-ef.loc
            AND reftable.code     EQ TRIM(ipbf-ef.est-no) + STRING(ipbf-ef.form-no,"/99")
            AND reftable.code2    EQ ipcCostType + "-CST" + STRING(ipiIndex,"99")
            NO-ERROR.

    opdCostPerM = IF AVAILABLE reftable THEN reftable.val[iLookupIndex] ELSE 0.

END PROCEDURE.

PROCEDURE pProcessOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: should replace ce/prokalk.i and ce/pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOut AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutRunWaste AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-mach FOR mach.

    DEFINE VARIABLE iInkCoatCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO. 
    DEFINE VARIABLE dLFPerFeed    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPartCount    AS DECIMAL NO-UNDO.
    
    ASSIGN 
        ipbf-ttEstCostOperation.quantityOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-ttEstCostOperation.quantityInNoWaste = ipbf-ttEstCostOperation.quantityOut / ipbf-ttEstCostOperation.numOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste                  = iopdQtyInOutSetupWaste / ipbf-ttEstCostOperation.numOutForOperation
        iopdQtyInOutRunWaste                    = iopdQtyInOutRunWaste / ipbf-ttEstCostOperation.numOutForOperation
        ipbf-ttEstCostOperation.quantityIn        = fRoundUp(ipbf-ttEstCostOperation.quantityIn)
        iopdQtyInOutSetupWaste                  = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste                    = fRoundUp(iopdQtyInOutRunWaste)
        .
    
    /*Recalc from standards off right now*/
    IF NOT ipbf-ttEstCostOperation.isLocked THEN 
    DO:
        FIND FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ ipbf-ttEstCostHeader.company
            AND bf-mach.m-code EQ ipbf-ttEstCostOperation.operationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-ttEstCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ipbf-ttEstCostOperation, BUFFER bf-mach).
       
    END.
    
    ASSIGN 
        ipbf-ttEstCostOperation.quantityInRunWaste        = (ipbf-ttEstCostOperation.quantityInNoWaste / 
                                                    (1 - (ipbf-ttEstCostOperation.quantityInRunWastePercent / 100))) 
                                                    - ipbf-ttEstCostOperation.quantityInNoWaste
        ipbf-ttEstCostOperation.quantityInRunWaste        = fRoundUp(ipbf-ttEstCostOperation.quantityInRunWaste)
        ipbf-ttEstCostOperation.quantityInAfterSetupWaste = ipbf-ttEstCostOperation.quantityInNoWaste + ipbf-ttEstCostOperation.quantityInRunWaste
        ipbf-ttEstCostOperation.quantityIn                = ipbf-ttEstCostOperation.quantityInAfterSetupWaste + ipbf-ttEstCostOperation.quantityInSetupWaste
        iopdQtyInOutRunWaste                            = iopdQtyInOutRunWaste + ipbf-ttEstCostOperation.quantityInRunWaste
        iopdQtyInOutSetupWaste                          = iopdQtyInOutSetupWaste + ipbf-ttEstCostOperation.quantityInSetupWaste
        ipbf-ttEstCostOperation.quantityIn                = fRoundUp(ipbf-ttEstCostOperation.quantityIn)
        iopdQtyInOut                                    = ipbf-ttEstCostOperation.quantityIn
        .
    IF ipbf-ttEstCostOperation.isSpeedInLF THEN 
    DO:
        /*Refactor - assumes dim in inches*/
        CASE ipbf-ttEstCostOperation.feedType:
            WHEN "R" THEN 
                dLFPerFeed = ipbf-ttEstCostForm.grossLength / 12.
            WHEN "S" THEN 
                DO:
                    IF ipbf-ttEstCostOperation.isNetSheetMaker THEN 
                        dLFPerFeed = ipbf-ttEstCostForm.grossLength / 12.
                    ELSE
                        dLFPerFeed = ipbf-ttEstCostForm.netLength / 12.
                END.
        END CASE.
        ipbf-ttEstCostOperation.quantityInAfterSetupWasteLF = ipbf-ttEstCostOperation.quantityInAfterSetupWaste * dLFPerFeed.
    END.
    
    //Apply feed types A and P after base in-out calculation performed.  These will only affect the run hrs
    IF fIsSetType(ipbf-ttEstCostHeader.estType) AND (ipbf-ttEstCostOperation.feedType EQ "A" OR  ipbf-ttEstCostOperation.feedType EQ "P") THEN 
    DO: 
        IF ipbf-ttEstCostOperation.feedType EQ "P" THEN 
            dPartCount = fGetPartCount(ipbf-ttEstCostHeader.company, ipbf-ttEstCostHeader.estimateNo).
        ELSE 
            dPartCount = 1.
        ASSIGN 
            ipbf-ttEstCostOperation.quantityInNoWaste         = ipbf-ttEstCostHeader.quantityMaster * dPartCount
            ipbf-ttEstCostOperation.quantityOut               = ipbf-ttEstCostHeader.quantityMaster
            ipbf-ttEstCostOperation.quantityInRunWaste        = (ipbf-ttEstCostOperation.quantityInNoWaste / 
                                                    (1 - (ipbf-ttEstCostOperation.quantityInRunWastePercent / 100))) 
                                                    - ipbf-ttEstCostOperation.quantityInNoWaste
            ipbf-ttEstCostOperation.quantityInRunWaste        = fRoundUp(ipbf-ttEstCostOperation.quantityInRunWaste)
            ipbf-ttEstCostOperation.quantityInAfterSetupWaste = ipbf-ttEstCostOperation.quantityInNoWaste + ipbf-ttEstCostOperation.quantityInRunWaste
            ipbf-ttEstCostOperation.quantityIn                = ipbf-ttEstCostOperation.quantityInAfterSetupWaste + ipbf-ttEstCostOperation.quantityInSetupWaste
            ipbf-ttEstCostOperation.quantityIn                = fRoundUp(ipbf-ttEstCostOperation.quantityIn)            
            .
    END.
    
END PROCEDURE.

PROCEDURE pPromptForCalculationChanges PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create Prompt for Calculation Changes
     Notes:
    ------------------------------------------------------------------------------*/
    
    IF glPromptForMaterialVendor THEN 
    DO:
        //run prompt for vendors
        RUN est/estCostMaterialList.w (INPUT-OUTPUT TABLE ttEstCostHeaderToCalc BY-REFERENCE, 
                                       INPUT-OUTPUT TABLE ttEstCostMaterial BY-REFERENCE,
                                       INPUT TABLE ttEstCostHeader BY-REFERENCE ).
        
        FOR EACH ttEstCostHeaderToCalc
            WHERE ttEstCostHeaderToCalc.lRecalcRequired EQ YES:
                
            RUN pRecalcMaterials(ttEstCostHeaderToCalc.iEstCostHeaderID).
            RUN pCalcHeaderCosts(ttEstCostHeaderToCalc.iEstCostHeaderID).
        END.
    END.
    
END PROCEDURE.

PROCEDURE pPurgeCalculation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and estimate number, purges all related data for 
     cost estimate calculation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.

    DEFINE BUFFER bf-probe         FOR probe.
    DEFINE BUFFER bf-probeit       FOR probeit.
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader.
    DEFINE BUFFER bf-estCostHeader   FOR estCostHeader.
    
    IF ipcJobNo EQ "" THEN DO:
        FOR EACH bf-probe EXCLUSIVE-LOCK 
            WHERE bf-probe.company EQ ipcCompany
            AND bf-probe.est-no  EQ ipcEstimateNo:
            DELETE bf-probe.                 
        END.
        FOR EACH bf-probeit EXCLUSIVE-LOCK 
            WHERE bf-probeit.company EQ ipcCompany
            AND bf-probeit.est-no  EQ ipcEstimateNo:
            DELETE bf-probeit.                 
        END.
    END.
    FOR EACH bf-ttEstCostHeader EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostHeader.company EQ ipcCompany
        AND bf-ttEstCostHeader.estimateNo EQ ipcEstimateNo
        AND bf-ttEstCostHeader.jobID EQ ipcJobNo
        AND bf-ttEstCostHeader.jobID2 EQ ipiJobNo2:
            
        DELETE bf-ttEstCostHeader.    
    END.
    
    FOR EACH estCostHeader NO-LOCK 
        WHERE estCostHeader.company    EQ ipcCompany
          AND estCostHeader.estimateNo EQ ipcEstimateNo
          AND estCostHeader.jobID      EQ ipcJobNo
          AND estCostHeader.jobID2     EQ ipiJobNo2:
        FIND FIRST bf-estCostHeader EXCLUSIVE-LOCK
             WHERE bf-estCostHeader.estCostHeaderID EQ estCostHeader.estCostHeaderID
             NO-ERROR.
        IF AVAILABLE bf-estCostHeader THEN
            DELETE bf-estCostHeader.
    END.
   
    RELEASE bf-probe.
    RELEASE bf-probeit.
    RELEASE bf-ttEstCostHeader.
    
END PROCEDURE.

PROCEDURE pPurgeCostDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Purges all EstCostDetail data for a given headerID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    FOR EACH bf-ttEstCostDetail EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID
        AND (ipcCategory EQ "" OR bf-ttEstCostDetail.estCostCategoryID EQ ipcCategory)
        USE-INDEX estHeader:
        DELETE bf-ttEstCostDetail.
    END.
    
    RELEASE bf-ttEstCostDetail.
    
END PROCEDURE.

PROCEDURE pPurgeCostDetailDB PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Purges all EstCostDetail data for a given headerID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcCategory AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-EstCostDetail FOR EstCostDetail.
    
    FOR EACH bf-EstCostDetail EXCLUSIVE-LOCK 
        WHERE bf-EstCostDetail.estCostHeaderID EQ ipiEstCostHeaderID
        AND (ipcCategory EQ "" OR bf-EstCostDetail.estCostCategoryID EQ ipcCategory)
        USE-INDEX estHeader:
        DELETE bf-EstCostDetail.
    END.
    
    RELEASE bf-EstCostDetail.
    
END PROCEDURE.

PROCEDURE pPurgeCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Purges all ttEstCostSummary data for a given headerID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

    DEFINE BUFFER bf-ttEstCostSummary FOR ttEstCostSummary.
    FOR EACH bf-ttEstCostSummary EXCLUSIVE-LOCK 
        WHERE bf-ttEstCostSummary.estCostHeaderID EQ ipiEstCostHeaderID
        USE-INDEX estHeader:
        DELETE bf-ttEstCostSummary.
    END.
    
    RELEASE bf-ttEstCostSummary.
    
END PROCEDURE.
PROCEDURE pPurgeCostSummaryDB PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Purges all ttEstCostSummary data for a given headerID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

    DEFINE BUFFER bf-EstCostSummary FOR EstCostSummary.
    FOR EACH bf-EstCostSummary EXCLUSIVE-LOCK 
        WHERE bf-EstCostSummary.estCostHeaderID EQ ipiEstCostHeaderID
        USE-INDEX estHeader:
        DELETE bf-EstCostSummary.
    END.
    
    RELEASE bf-EstCostSummary.
    
END PROCEDURE.

PROCEDURE pPurgeJobCalculation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Removes any recalculation records for a specific job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader. 
    
    FOR EACH bf-ttEstCostHeader
        WHERE bf-ttEstCostHeader.company EQ ipcCompany
        AND bf-ttEstCostHeader.estimateNo EQ ipcEstimateNo
        AND bf-ttEstCostHeader.jobID EQ ipcJobNo
        AND bf-ttEstCostHeader.jobID2 EQ ipiJobNo2:
        DELETE bf-ttEstCostHeader.    
    END.
    
    RELEASE bf-ttEstCostHeader.
    
END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated Run Speeds for estCostOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.

   

END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunWastePercent PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated RunWastePercent for estCostOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.




END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsSetupWaste PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated MR Waste for estCostOperation
     based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostHeader    FOR ttEstCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.

    ipbf-ttEstCostOperation.quantityInSetupWaste = ipbf-mach.mr-waste.
    IF ipbf-ttEstCostOperation.isPrinter OR ipbf-ttEstCostOperation.isCoater THEN 
        ipbf-ttEstCostOperation.quantityInSetupWaste = ipbf-ttEstCostOperation.quantityInSetupWaste + 
            (ipbf-ttEstCostOperation.quantityInSetupWastePerColor * 
            IF glUsePlateChangesAsColorForSetupWaste AND ipbf-ttEstCostOperation.countPlateChanges NE 0 THEN ipbf-ttEstCostOperation.countPlateChanges
            ELSE (ipbf-ttEstCostOperation.countCoats + ipbf-ttEstCostOperation.countInks)).

/*REFACTOR To handle the "Plain Jobs only* function in EB1*/
/*        RUN est/diewaste.p (BUFFER est-op).*/


END PROCEDURE.

PROCEDURE pRecalcMaterials PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Given an estCostHeaderID, recalculate all estCostMaterials (includes vendor selection)
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64.
    
    DEFINE BUFFER bf-ttEstCostHeader   FOR ttEstCostHeader.
    DEFINE BUFFER bf-ttEstCostMaterial FOR ttEstCostMaterial.
    DEFINE BUFFER bf-ttEstCostForm   FOR ttEstCostForm.
    
    RUN pResetCostTotals(ipiEstCostHeaderID). 
    RUN pPurgeCostDetail(ipiEstCostHeaderID, "").
    RUN pPurgeCostSummary(ipiEstCostHeaderID).
    FOR EACH bf-ttEstCostForm NO-LOCK
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-ttEstCostHeader NO-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID:
        
        FOR EACH bf-ttEstCostMaterial EXCLUSIVE-LOCK
            WHERE bf-ttEstCostMaterial.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
            AND bf-ttEstCostMaterial.estCostFormID EQ bf-ttEstCostForm.estCostFormID:
                    
            RUN pCalcEstMaterial(BUFFER bf-ttEstCostHeader, BUFFER bf-ttEstCostMaterial, BUFFER bf-ttEstCostForm).
        END.
       // RUN pCalcCostTotals(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, YES).
    END.

END PROCEDURE.

PROCEDURE pResetCostTotals PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Resets Cost Totals for a given estCostHeaderID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64.
    
    DEFINE BUFFER bf-ttEstCostForm FOR ttEstCostForm.
    DEFINE BUFFER bf-ttEstCostItem   FOR ttEstCostItem.
    DEFINE BUFFER bf-ttEstCostHeader FOR ttEstCostHeader.
    
    FOR EACH bf-ttEstCostForm NO-LOCK 
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsForm(BUFFER bf-ttEstCostForm).
    END.
    FOR EACH bf-ttEstCostItem NO-LOCK 
        WHERE bf-ttEstCostItem.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsItem(BUFFER bf-ttEstCostItem).
    END.
    FOR EACH bf-ttEstCostHeader NO-LOCK 
        WHERE bf-ttEstCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsHeader(BUFFER bf-ttEstCostHeader).
    END.
    
     
END PROCEDURE.

PROCEDURE pResetCostTotalsForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Resets Cost Totals for a given ttEstCostForm
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotalReset.i &TableName=ttEstCostForm}
     
END PROCEDURE.

PROCEDURE pResetCostTotalsHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Resets Cost Totals for a given ttEstCostForm
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotalReset.i &TableName=ttEstCostHeader}
     
END PROCEDURE.

PROCEDURE pResetCostTotalsItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Resets Cost Totals for a given ttEstCostForm
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotalReset.i &TableName=ttEstCostItem}
     
END PROCEDURE.


PROCEDURE pSetGlobalSettings PRIVATE:
    
    /*------------------------------------------------------------------------------
     Purpose: Sets the NK1 setting global variables that are pertinent to th
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (ipcCompany, "CEPREP", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN gcPrepRoundTo = cReturn.

    RUN sys/ref/nk1look.p (ipcCompany, "CEPREPPRICE", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN gcPrepMarkupOrMargin = cReturn.
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEMATL", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound AND cReturn EQ "YES" THEN  
    DO:
        RUN sys/ref/nk1look.p (ipcCompany, "CEMATL", "D", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
        IF lFound THEN gdMaterialMarkup = DECIMAL(cReturn).
    END.
    RUN sys/ref/nk1look.p (ipcCompany,"CEMarkupMatrixLookup","C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF NOT lFound OR cReturn EQ "" THEN 
        gcMarginMatrixLookup = "Square Feet".
    ELSE 
        gcMarginMatrixLookup = cReturn.
        
    RUN sys/ref/nk1look.p (ipcCompany,"CEOpRates","C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glOpRatesSeparate = lFound AND cReturn EQ "MR/Run Separate".
    
    RUN sys/ref/nk1look.p (ipcCompany, "VendItemCost", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glVendItemCost = cReturn EQ "Yes".
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEPrice", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glApplyOperationMinimumCharge = cReturn EQ "YES".
    IF lFound AND glApplyOperationMinimumCharge THEN 
    DO: 
        RUN sys/ref/nk1look.p (ipcCompany, "CEPrice", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
        IF lFound THEN glApplyOperationMinimumChargeRunOnly = cReturn EQ "RunOnly".
    END.
    
    RUN sys/ref/nk1look.p (ipcCompany, "CERound", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glRoundPriceToDollar = cReturn EQ "Dollar".
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEWindow", "D", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN gdWindowDimOverlap = DECIMAL(cReturn).
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEPrompt", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN glPromptForMaterialVendor = cReturn EQ "YES".
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEVendorDefault", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    glUseBlankVendor = lFound AND cReturn EQ "Blank Vendor".
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEShipWeight", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    glUseGrossWeight = lFound AND cReturn EQ "Gross".
        
    RUN sys/ref/nk1look.p (ipcCompany, "FOAMCOST", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    glCalcFoamCostFromBlank = lFound AND cReturn EQ "Blank".

    RUN sys/ref/nk1look.p (ipcCompany, "CEAutoRecostBoard", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glAutoRecostBoard = lFound AND cReturn EQ "YES".    

    RUN sys/ref/nk1look.p (ipcCompany, "CEOpStandards", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    glCalcSourceForMachineStd = lFound AND cReturn EQ "Machine if Not Locked".
    
    RUN sys/ref/nk1look.p (ipcCompany,"CECostSource","C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    IF NOT lFound OR cReturn EQ "" THEN 
        gcCECostSourceLookup = "VendorCostOnly".
    ELSE 
        gcCECostSourceLookup = cReturn.    
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEShowErrorsAndWarnings", "I" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
        giPromptForErrorLevel = IF lFound THEN INTEGER (cReturn) ELSE 0.
       
END PROCEDURE.

PROCEDURE pSetKeyFields PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Assign UNique INdex fields of Temp-table
     Notes: Assign a temporary value to Key fields. It will be calculated by Triggers while DB record creation.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER ipiKeyField   AS INT64 NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER ipcRecKeyField AS CHARACTER NO-UNDO.
    DEFINE INPUT         PARAMETER ipcTableName AS CHARACTER NO-UNDO.

    ASSIGN
        ipiKeyField    = fGetNextID(ipcTableName) 
        ipcRecKeyField = STRING(YEAR(TODAY),"9999") + 
                         STRING(MONTH(TODAY),"99") + 
                         STRING(DAY(TODAY),"99") + 
                         STRING(TIME,"99999") + 
                         STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999").
                         
END PROCEDURE.

PROCEDURE pWriteDatasetIntoDB PRIVATE:
/*------------------------------------------------------------------------------
        Purpose:This procedure will create records in DB tables
        Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bfEx-estCostForm         FOR estCostForm.
    DEFINE BUFFER bfEx-EstCostDetail       FOR EstCostDetail.
    DEFINE BUFFER bfEx-estCostSummary      FOR estCostSummary.
    DEFINE BUFFER bfEx-estCostOperation    FOR estCostOperation.
    DEFINE BUFFER bfEx-estCostMisc         FOR estCostMisc.
    DEFINE BUFFER bfEx-estCostMaterial     FOR estCostMaterial.
    DEFINE BUFFER bfEx-estCostItem         FOR estCostItem.
    DEFINE BUFFER bfEx-estCostBlank        FOR estCostBlank.
    DEFINE BUFFER bfEx-estCostHeader       FOR estCostHeader.
    
    DEFINE BUFFER bfSetHeader-ttEstCostItem FOR ttEstCostItem.
    
    FOR EACH ttEstCostHeader
        TRANSACTION:
        CREATE bfEx-estCostHeader.
        BUFFER-COPY ttEstCostHeader TO bfEx-estCostHeader.
                
    END. /* ttEstCostHeader */
    
    FOR EACH ttEstCostForm
        TRANSACTION:
        CREATE bfEx-estCostForm.
        BUFFER-COPY ttEstCostForm TO bfEx-estCostForm.
            
    END. /* ttEstCostForm */
    
    FOR EACH ttEstCostItem
        TRANSACTION:
        CREATE bfEx-estCostItem.
        BUFFER-COPY ttEstCostItem TO bfEx-estCostItem.
            
        ASSIGN
            ttEstCostItem.DBEstCostItemID = bfEx-estCostItem.estCostItemID
            ttEstCostItem.DBRec_key       = bfEx-estCostItem.rec_key.
        
    END. /* ttEstCostItem */
    
    /* Process Est Cost Item one more time to align parent key */
    FOR EACH ttEstCostItem
        WHERE ttEstCostItem.estCostItemIDParent NE 0
        TRANSACTION:
            
        /* Find and Link with appropriate Parent EstCost Item Id */       
        FIND FIRST bfSetHeader-ttEstCostItem
            WHERE bfSetHeader-ttEstCostItem.estCostHeaderID  = ttEstCostItem.estCostHeaderID
            AND bfSetHeader-ttEstCostItem.estCostItemID    = ttEstCostItem.estCostItemIDParent NO-ERROR.
            
        IF AVAILABLE bfSetHeader-ttEstCostItem THEN
        DO:
            FIND FIRST bfEx-estCostItem EXCLUSIVE-LOCK
                WHERE bfEx-estCostItem.estCostHeaderID = ttEstCostItem.estCostHeaderID
                AND bfEx-estCostItem.Rec_key         = ttEstCostItem.DBRec_key NO-ERROR.
            IF AVAILABLE bfEx-estCostItem THEN    
                bfEx-estCostItem.estCostItemIDParent = bfSetHeader-ttEstCostItem.DBEstCostItemID.
        END.
    END.
    
    FOR EACH ttEstCostBlank
        TRANSACTION:
        CREATE bfEx-estCostBlank.
        BUFFER-COPY ttEstCostBlank TO bfEx-estCostBlank.        
    END. /* ttEstCostBlank */
        
    FOR EACH ttEstCostSummary
        TRANSACTION:
        CREATE bfEx-estCostSummary.
        BUFFER-COPY ttEstCostSummary TO bfEx-estCostSummary.        
    END. /* ttEstCostSummary */
     
    FOR EACH ttEstCostOperation
        TRANSACTION:
        CREATE bfEx-estCostOperation.
        BUFFER-COPY ttEstCostOperation TO bfEx-estCostOperation.
    END. /* ttEstCostOperation */    
        
    FOR EACH ttEstCostMisc
        TRANSACTION:
        CREATE bfEx-estCostMisc.
        BUFFER-COPY ttEstCostMisc TO bfEx-estCostMisc.
    END. /* ttEstCostMisc */    
    
    FOR EACH ttEstCostMaterial
        TRANSACTION:
        CREATE bfEx-estCostMaterial.
        BUFFER-COPY ttEstCostMaterial TO bfEx-estCostMaterial.
    END. /* ttEstCostMaterial */ 
     
    /* process this one in the end because its linked table from many tables */
    FOR EACH ttEstCostDetail
        TRANSACTION:
        CREATE bfEx-estCostDetail.
        BUFFER-COPY ttEstCostDetail TO bfEx-estCostDetail.     
    END. /* ttEstCostDetail */ 
    
    RELEASE bfEx-estCostForm.
    RELEASE bfEx-estCostDetail.
    RELEASE bfEx-estCostSummary.
    RELEASE bfEx-estCostOperation.
    RELEASE bfEx-estCostMisc.
    RELEASE bfEx-estCostMaterial.
    RELEASE bfEx-estCostBlank.
    RELEASE bfEx-estCostItem.
    RELEASE bfEx-estCostHeader.
    
    EMPTY TEMP-TABLE ttEstCostForm.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
    EMPTY TEMP-TABLE ttEstCostOperation.
    EMPTY TEMP-TABLE ttEstCostMisc.
    EMPTY TEMP-TABLE ttEstCostMaterial.
    EMPTY TEMP-TABLE ttEstCostBlank.
    EMPTY TEMP-TABLE ttEstCostItem.
    EMPTY TEMP-TABLE ttEstCostHeader.
    
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64 , ipiEstFormID AS INT64 , ipiBlankNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the Blank ID given header, form id and blank #
     Notes:
    ------------------------------------------------------------------------------*/    
    FIND FIRST ttEstCostBlank NO-LOCK 
        WHERE ttEstCostBlank.estCostHeaderID EQ ipiEstHeaderID
        AND ttEstCostBlank.estCostFormID EQ ipiEstFormID
        AND ttEstCostBlank.blankNo EQ ipiBlankNo
        NO-ERROR.
    IF AVAILABLE ttEstCostBlank THEN 
        RETURN ttEstCostBlank.estCostBlankID.

END FUNCTION.

FUNCTION fGetMatTypeCalc RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcMaterialType AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cCalculationType AS CHARACTER NO-UNDO.
    
    FIND FIRST materialType NO-LOCK 
        WHERE materialType.company EQ ipcCompany
        AND materialType.materialType EQ ipcMaterialType
        NO-ERROR.
    IF AVAILABLE materialType THEN 
        cCalculationType = materialType.calculationType.
    ELSE 
        cCalculationType = gcMaterialTypeCalcDefault.
    
    RETURN cCalculationType.
    
END FUNCTION.

FUNCTION fGetMSF RETURNS DECIMAL PRIVATE
    (ipdArea AS DECIMAL, ipcUOM AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Get area in MSF given area and UOM      
 Notes:
------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fConv_GetAreaSqFeet", ipdArea, ipcUOM) / 1000.

END FUNCTION.

FUNCTION fGetNetSheetOut RETURNS INTEGER PRIVATE
    (ipiEstCostOperationID AS INT64, ipiDefaultOut AS INTEGER, INPUT ipdEstOPQty AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose:  Given an operation buffer, return the # out based on the 
     specific net sheet pass of the operation
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE BUFFER bf-ef-nsh           FOR ef-nsh.
    DEFINE BUFFER bf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE BUFFER bf-est-op            FOR est-op.
    
    DEFINE VARIABLE iOut AS INTEGER NO-UNDO.
    DEFINE VARIABLE lFoam AS LOGICAL NO-UNDO.
    
    FIND FIRST bf-ttEstCostOperation NO-LOCK 
        WHERE bf-ttEstCostOperation.estCostOperationID EQ ipiEstCostOperationID
        NO-ERROR.
    IF AVAILABLE bf-ttEstCostOperation THEN
    DO:
         lFoam = fIsFoamStyle (bf-ttEstCostOperation.company, bf-ttEstCostOperation.estimateNo, bf-ttEstCostOperation.formNo).
         
        IF lFoam THEN
        DO:
            FIND FIRST bf-ef-nsh NO-LOCK    
                WHERE bf-ef-nsh.company EQ bf-ttEstCostOperation.company
                AND bf-ef-nsh.est-no EQ bf-ttEstCostOperation.estimateNo
                AND bf-ef-nsh.form-no EQ bf-ttEstCostOperation.formNo
                AND bf-ef-nsh.pass EQ bf-ttEstCostOperation.pass
                NO-ERROR.
            
            IF AVAILABLE bf-ef-nsh THEN 
                iOut = bf-ef-nsh.n-out-d * bf-ef-nsh.n-out-l * bf-ef-nsh.n-out-w.
        END.
        ELSE
        DO: 
            /* pull Numout for sheeters */
            FIND FIRST bf-est-op NO-LOCK
                WHERE bf-est-op.company EQ bf-ttEstCostOperation.company
                AND bf-est-op.est-no    EQ bf-ttEstCostOperation.estimateNo
                AND bf-est-op.s-num     EQ bf-ttEstCostOperation.formNo
                AND bf-est-op.b-num     EQ bf-ttEstCostOperation.blankNo
                AND bf-est-op.op-Pass   EQ bf-ttEstCostOperation.pass
                AND bf-est-op.line      LT 500
                AND bf-est-op.qty       EQ ipdEstOPQty NO-ERROR.
                
           IF AVAILABLE bf-est-op THEN     
                iOut = bf-est-op.n-out.
           
        END.
    END.
    
    IF iOut LE 0 THEN 
        iOut = ipiDefaultOut.
    
    RETURN iOut.
        
END FUNCTION.

FUNCTION fGetNextID RETURNS INT64 PRIVATE
    ( ipcTableName AS CHARACTER  ):
    /*------------------------------------------------------------------------------
         Purpose: increment the ID for a unique id
         Notes:
        ------------------------------------------------------------------------------*/  
    CASE (ipcTableName) :  
        
    WHEN ("estCostForm") THEN     
    giID = NEXT-VALUE(estCostFormID_seq,ASI). 
        
    WHEN ("estCostMaterial") THEN     
    giID = NEXT-VALUE(estCostMaterialID_seq,ASI).
    
    WHEN ("estCostBlank") THEN     
    giID = NEXT-VALUE(estCostBlankID_seq,ASI).
    
    WHEN ("estCostHeader") THEN     
    giID = NEXT-VALUE(estCostHeaderID_seq,ASI).
    
    WHEN ("estCostSummary") THEN     
    giID = NEXT-VALUE(estCostSummaryID_seq,ASI).
    
    WHEN ("estCostItem") THEN     
    giID = NEXT-VALUE(estCostItemID_seq,ASI).
    
    WHEN ("estCostDetail") THEN     
    giID = NEXT-VALUE(estCostDetailID_seq,ASI).
    
    WHEN ("estCostMisc") THEN     
    giID = NEXT-VALUE(estCostMiscID_seq,ASI).
    
    WHEN ("estCostOperation") THEN     
    giID = NEXT-VALUE(estCostOperationID_seq,ASI).
    
    END.
    RETURN giID.
    
            
END FUNCTION.

FUNCTION fGetPartCount RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER, ipcEstimateID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Gets the part count for a set for partition feed type calculation
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE BUFFER bf-eb FOR eb.
    
    DEFINE VARIABLE dParts AS DECIMAL NO-UNDO.
    
    FOR EACH bf-eb NO-LOCK 
        WHERE bf-eb.company EQ ipcCompany
        AND bf-eb.est-no EQ ipcEstimateID
        AND bf-eb.form-no NE 0:
        
        dParts = dParts + fGetQuantityPerSet(BUFFER bf-eb).
    END.
    
    RETURN dParts.


        
END FUNCTION.

FUNCTION fGetProfit RETURNS DECIMAL PRIVATE
    ( ipdCost AS DECIMAL, ipdProfitPercent AS DECIMAL, ipcPercentType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Calculates profit based on margin or markup method
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE dProfit AS DECIMAL NO-UNDO.
    
    IF ipcPercentType EQ "Margin" THEN
        dProfit = ipdCost * (1 / (1 - ipdProfitPercent / 100) - 1).
    ELSE 
        dProfit = ipdCost * (ipdProfitPercent / 100).

    RETURN dProfit.
    
END FUNCTION.

FUNCTION fGetQuantityPerSet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb):
    /*------------------------------------------------------------------------------
     Purpose: Returns the quantity per set in decimal form for an eb 
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_GetQuantityPerSet", BUFFER ipbf-eb).
        
END FUNCTION.

FUNCTION fIsAdderMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for board
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsAdderMaterial", ipcMaterialTypeID).
        
END FUNCTION.

FUNCTION fIsBoardMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for board
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsBoardMaterial", ipcMaterialTypeID).
        
END FUNCTION.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER, ipcDepartmentList AS CHARACTER EXTENT 4):
    /*------------------------------------------------------------------------------
     Purpose: determine if provided department is in department list
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsDepartment", ipcDepartment, ipcDepartmentList).
        
END FUNCTION.


FUNCTION fIsGlueMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for Glue
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsGlueMaterial", ipcMaterialTypeID).
        
        
END FUNCTION.

FUNCTION fIsInkMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for ink
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsInkMaterial", ipcMaterialTypeID).
        
        
END FUNCTION.

FUNCTION fIsLeafMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for leaf/film
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsLeafMaterial", ipcMaterialTypeID).
    
END FUNCTION.

FUNCTION fIsPackingMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for packing
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsPackingMaterial", ipcMaterialTypeID).
        
END FUNCTION.

FUNCTION fIsWaxMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for packing
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsWaxMaterial", ipcMaterialTypeID).
        
END FUNCTION.

FUNCTION fIsWindowMaterial RETURNS LOGICAL PRIVATE
    ( ipcMaterialTypeID AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose:  Given a material type, return if the material type is for packing
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsWindowMaterial", ipcMaterialTypeID).
        
END FUNCTION.



FUNCTION fRoundUP RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Given a value, rounds up to next integer
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("sfCommon_RoundUp", ipdValue).
        
END FUNCTION.


FUNCTION fIsComboType RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsComboType", ipcEstType).
    
END FUNCTION.

FUNCTION fIsFoamStyle RETURNS LOGICAL 
    (ipcCompany AS CHARACTER, ipcEstNo AS CHARACTER, INPUT ipiFormNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if any Item is Foam type in the Estimate
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-Style FOR Style.
    DEFINE BUFFER bf-eb    FOR eb.
    
    FIND FIRST bf-eb
        WHERE bf-eb.company EQ ipcCompany
        AND bf-eb.est-no  EQ ipcEstNo
        AND bf-eb.form-no EQ ipiFormNo
        AND bf-eb.pur-man EQ NO
        AND CAN-FIND(FIRST bf-Style
        WHERE bf-Style.company EQ bf-eb.company
        AND bf-Style.style   EQ bf-eb.style
        AND bf-Style.type    EQ "F")
        NO-LOCK NO-ERROR.
    
    IF AVAILABLE bf-eb THEN
        lResult = YES.

    RETURN lResult.
END FUNCTION.


FUNCTION fIsMiscType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsMiscType", ipcEstType).
        
END FUNCTION.

FUNCTION fIsSetType RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Set Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsSetType", ipcEstType).
        
END FUNCTION.

FUNCTION fIsSingleType RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Single Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsSingleType", ipcEstType).
    
END FUNCTION.

FUNCTION fIsWoodType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Single Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsWoodType", ipcEstType).
    
END FUNCTION.
  
