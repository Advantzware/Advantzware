  /*--------------------------------------------------------------------------
    File        : EstimateCalcProcsAlt.p
    
    Purpose     : New code for EstimateCalcProcs.p for Performace improvement. It being updated to use Dataset/temp-tables instead of DB tables
    
    Syntax      :      
    
    Description :        

    Author(s)   : BV
                  sakshi.singh
    
    Created     : Tue Feb 08 17:17:51 EST 2022
    
    Notes       : Dataset will be written into DB in the end of processing
    ------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{est/ttEstCost.i}
{est/ttEstSysConfig.i}
{est/dsEstimate.i}

   
DEFINE VARIABLE giID                                  AS INT64 NO-UNDO.
DEFINE VARIABLE ghFreight                             AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghFormula                             AS HANDLE    NO-UNDO.
DEFINE VARIABLE ghOperation                           AS HANDLE    NO-UNDO.

DEFINE VARIABLE gcSourceTypeOperation                 AS CHARACTER NO-UNDO INITIAL "Operation".
DEFINE VARIABLE gcSourceTypeMaterial                  AS CHARACTER NO-UNDO INITIAL "Material".
DEFINE VARIABLE gcSourceTypeMisc                      AS CHARACTER NO-UNDO INITIAL "Miscellaneous".
DEFINE VARIABLE gcSourceTypeNonFactory                AS CHARACTER NO-UNDO INITIAL "NonFactory".
DEFINE VARIABLE gcSourceTypeProfit                    AS CHARACTER NO-UNDO INITIAL "Profit".

DEFINE VARIABLE gcBoardMatTypes                       AS CHARACTER NO-UNDO INITIAL "1,2,3,4,A,B,P,R".
DEFINE VARIABLE gcGlueMatTypes                        AS CHARACTER NO-UNDO INITIAL "G,S,T".
DEFINE VARIABLE gcInkMatTypes                         AS CHARACTER NO-UNDO INITIAL "I,V".
DEFINE VARIABLE gcPackMatTypes                        AS CHARACTER NO-UNDO INITIAL "5,6,C,D,J,M".
DEFINE VARIABLE gcLeafMatTypes                        AS CHARACTER NO-UNDO INITIAL "F,W".
DEFINE VARIABLE gcWindowMatTypes                      AS CHARACTER NO-UNDO INITIAL "W".
DEFINE VARIABLE gcWaxMatTypes                         AS CHARACTER NO-UNDO INITIAL "W".
DEFINE VARIABLE gcAdderMatTypes                       AS CHARACTER NO-UNDO INITIAL "A".

DEFINE VARIABLE gcDeptsForPrinters                    AS CHARACTER NO-UNDO INITIAL "PR".
DEFINE VARIABLE gcDeptsForGluers                      AS CHARACTER NO-UNDO INITIAL "GL,QS".
DEFINE VARIABLE gcDeptsForLeafers                     AS CHARACTER NO-UNDO INITIAL "WN,WS,FB,FS".
DEFINE VARIABLE gcDeptsForSheeters                    AS CHARACTER NO-UNDO INITIAL "RC,RS,CR".
DEFINE VARIABLE gcDeptsForCoaters                     AS CHARACTER NO-UNDO INITIAL "PR,CT".

DEFINE VARIABLE gcIndustryFolding                     AS CHARACTER NO-UNDO INITIAL "Folding".
DEFINE VARIABLE gcIndustryCorrugated                  AS CHARACTER NO-UNDO INITIAL "Corrugated".

DEFINE VARIABLE gcErrorWarning                        AS CHARACTER NO-UNDO INITIAL "Warning".
DEFINE VARIABLE gcErrorImportant                      AS CHARACTER NO-UNDO INITIAL "Important".
DEFINE VARIABLE gcErrorCritical                       AS CHARACTER NO-UNDO INITIAL "Critical".

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
DEFINE VARIABLE glCalcFoamCostFromBlank               AS LOGICAL   NO-UNDO.  /*FOAMCOST*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64,
    ipiEstFormID AS INT64,
    ipiBlankNo AS INTEGER) FORWARD.

FUNCTION fGetMatTypeCalc RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
     ipcMaterialType AS CHARACTER) FORWARD.

FUNCTION fGetNetSheetOut RETURNS INTEGER PRIVATE
    (ipiEstCostOperationID AS INT64,
    ipiDefaultOut AS INTEGER,
    INPUT ipdEstOPQty AS DECIMAL) FORWARD.

FUNCTION fGetNextID RETURNS INT64 PRIVATE
    (  ) FORWARD.

FUNCTION fGetPartCount RETURNS DECIMAL PRIVATE
    (ipcCompany AS CHARACTER,
     ipcEstimateID AS CHARACTER) FORWARD.

FUNCTION fGetProfit RETURNS DECIMAL PRIVATE
    (ipdCost AS DECIMAL,
    ipdProfitPercent AS DECIMAL,
    ipcPercentType AS CHARACTER) FORWARD.

FUNCTION fGetQuantityPerSet RETURNS DECIMAL PRIVATE
    (BUFFER ipbf-eb FOR eb) FORWARD.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER,
    ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fRoundUP RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL) FORWARD.

FUNCTION fIsComboType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION IsFoamStyle RETURNS LOGICAL 
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
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ttEstCostForm FOR ttEstCostForm.

    DEFINE VARIABLE dQtyInM         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPriceDiffRatio AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dNewPrice       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dProfitChange   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCommission     AS DECIMAL NO-UNDO.
    
    FIND probe NO-LOCK 
        WHERE ROWID(probe) EQ ipriProbe
        NO-ERROR.

    IF AVAILABLE probe THEN
        FIND FIRST estCostHeader NO-LOCK 
            WHERE estCostHeader.estCostHeaderID EQ INT64(probe.spare-char-2)
            NO-ERROR.
    IF AVAILABLE estCostHeader THEN 
    DO:
        ASSIGN 
            dQtyInM         = estCostHeader.quantityMaster / 1000
            dPriceDiffRatio = ROUND(probe.sell-price,2) / ROUND(estCostHeader.sellPrice / dQtyInM, 2)
            .
        IF dPriceDiffRatio NE 1 THEN 
        DO:
            /*Remove all existing estCostDetails for commission and profit*/
            RUN pPurgeCostDetail(estCostHeader.estCostHeaderID, "commission").
            RUN pPurgeCostDetail(estCostHeader.estCostHeaderID, "pProfit").
            /*Reset the summary totals on the header, form, and items*/
            RUN pResetCostTotals(estCostHeader.estCostHeaderID).          
            FOR EACH bf-ttEstCostForm NO-LOCK
                WHERE bf-ttEstCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID,
                FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ estCostHeader.estCostHeaderID
                  AND estCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
                FIRST estCostItem NO-LOCK 
                WHERE estCostItem.estCostHeaderID EQ estCostHeader.estCostHeaderID
                  AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
                
                /*Calculate new Price for form and commisson*/
                ASSIGN 
                    dNewPrice   = ROUND(bf-ttEstCostForm.sellPrice * dPriceDiffRatio, 2)
                    dCommission = dNewPrice * estCostItem.commissionPct / 100
                    .
                
                /*Recalculate Totals for Form*/    
                RUN pCalcCostTotals(estCostHeader.estCostHeaderID, bf-ttEstCostForm.estCostFormID, YES).  
                
                /*Add New Commission Cost*/
                RUN pAddCostDetail(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, "" , bf-ttEstCostForm.estCostFormID,
                    gcSourceTypeNonFactory,"commission","Commission", dCommission, 0, bf-ttEstCostForm.company, bf-ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
                /*Recalculate Cost Totals to get a new TotalFullCost*/
                RUN pCalcCostTotals(estCostHeader.estCostHeaderID, bf-ttEstCostForm.estCostFormID, NO).
                
                /*Add New Profit based on new Full Cost and New Price*/
                RUN pAddCostDetail(bf-ttEstCostForm.estCostHeaderID, bf-ttEstCostForm.estCostFormID, "" , bf-ttEstCostForm.estCostFormID,
                    gcSourceTypeNonFactory,"pProfit","Profit After Price Change", dNewPrice - bf-ttEstCostForm.costTotalFull, 0, bf-ttEstCostForm.company, bf-ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
                /*Recalculate Sell Price*/
                RUN pCalcCostTotals(estCostHeader.estCostHeaderID, bf-ttEstCostForm.estCostFormID, NO).
            END. 
            
            /*Reset and calculate all cost summaries*/
            RUN pPurgeCostSummary(estCostHeader.estCostHeaderID).
            RUN pBuildCostSummary(estCostHeader.estCostHeaderID).
            RUN pBuildProbe(BUFFER estCostHeader).
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
        
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostDetail.estCostDetailID, INPUT-OUTPUT opbf-ttEstCostDetail.rec_key).

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
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-estCostMaterial.estCostHeaderID, ipbf-estCostMaterial.estCostFormID, ipbf-estCostMaterial.estCostBlankID, ipbf-estCostMaterial.estCostMaterialID, 
        gcSourceTypeMaterial, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-estCostMaterial.company, ipbf-estCostMaterial.estimateNo, BUFFER bf-ttEstCostDetail). 
    

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
            .
            
        RUN pSetKeyFields(INPUT-OUTPUT bf-ttEstCostSummary.estCostSummaryID, INPUT-OUTPUT bf-ttEstCostSummary.rec_key).
        
    END.
    bf-ttEstCostSummary.costTotal = bf-ttEstCostSummary.costTotal + ipdCost.
    IF ipdQtyPerM GT 0 THEN 
        bf-ttEstCostSummary.costTotalPerMFinished  = bf-ttEstCostSummary.costTotalPerMFinished + ipdCost / ipdQtyPerM.
        
    RELEASE bf-ttEstCostSummary.
    
END PROCEDURE.


PROCEDURE pAddError PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: Registers an error in the ttEstError table for display
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.

    CREATE ttEstError.
    ASSIGN 
        ttEstError.cError      = ipcError
        ttEstError.cErrorType  = ipcErrorType
        ttEstError.estHeaderID = ipiEstHeaderID
        ttEstError.iFormNo     = ipiFormNo
        ttEstError.iBlankNo    = ipiBlankNo
        .
END PROCEDURE.

PROCEDURE pAddEstBlank PRIVATE:

    /*------------------------------------------------------------------------------
     Purpose: given an eb buffer, create the EstBlank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb                   FOR eb.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader        FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm        FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-estCostBlank         FOR estCostBlank.
    
    DEFINE           BUFFER bf-estCostItem            FOR estCostItem.
    DEFINE           BUFFER bf-SetHeader-estCostBlank FOR estCostBlank.
    
    CREATE opbf-estCostBlank.
    ASSIGN 
        opbf-estCostBlank.company                 = ipbf-ttEstCostForm.company
        opbf-estCostBlank.estCostFormID           = ipbf-ttEstCostForm.estCostFormID
        opbf-estCostBlank.estCostHeaderID         = ipbf-ttEstCostForm.estCostHeaderID
        opbf-estCostBlank.estimateNo              = ipbf-eb.est-no
        opbf-estCostBlank.formNo                  = ipbf-eb.form-no
        opbf-estCostBlank.blankNo                 = ipbf-eb.blank-no
        opbf-estCostBlank.numOutLength            = ipbf-eb.num-len
        opbf-estCostBlank.numOutWidth             = ipbf-eb.num-wid
        opbf-estCostBlank.numOutDepth             = ipbf-eb.num-dep
        opbf-estCostBlank.numOut                  = MAX(opbf-estCostBlank.numOutWidth, 1) * MAX(opbf-estCostBlank.numOutLength, 1) * MAX(opbf-estCostBlank.numOutDepth, 1)
        opbf-estCostBlank.blankWidth              = IF ipbf-eb.t-wid EQ 0 THEN ipbf-eb.wid ELSE ipbf-eb.t-wid
        opbf-estCostBlank.blankLength             = IF ipbf-eb.t-len EQ 0 THEN ipbf-eb.len ELSE ipbf-eb.t-len
        opbf-estCostBlank.blankDepth              = IF ipbf-eb.t-dep EQ 0 THEN ipbf-eb.dep ELSE ipbf-eb.t-dep
        opbf-estCostBlank.blankArea               = IF ipbf-eb.t-sqin EQ 0 THEN opbf-estCostBlank.blankLength * opbf-estCostBlank.blankWidth ELSE ipbf-eb.t-sqin
        opbf-estCostBlank.dimLength               = ipbf-eb.len
        opbf-estCostBlank.dimWidth                = ipbf-eb.wid
        opbf-estCostBlank.dimDepth                = ipbf-eb.dep
        opbf-estCostBlank.quantityPerSubUnit      = ipbf-eb.cas-cnt
        opbf-estCostBlank.quantitySubUnitsPerUnit = ipbf-eb.cas-pal
        opbf-estCostBlank.isPurchased             = ipbf-eb.pur-man
                                                        
        /*Refactor - Hardcoded*/
        opbf-estCostBlank.areaUOM                 = "SQIN"
        opbf-estCostBlank.dimUOM                  = "IN"
        opbf-estCostBlank.weightUOM               = gcDefaultWeightUOM
                    
                            
        /*Refactor - Calculate Windowing*/
        opbf-estCostBlank.blankAreaNetWindow      = opbf-estCostBlank.blankArea

        /*Refactor - apply area UOM conversion*/
        opbf-estCostBlank.weightPerBlank          = ipbf-ttEstCostForm.basisWeight * opbf-estCostBlank.blankAreaNetWindow / 144000 
    
        opbf-estCostBlank.quantityPerSet          = fGetQuantityPerSet(BUFFER ipbf-eb)
        opbf-estCostBlank.quantityRequired        = (IF fIsComboType(ipbf-estCostHeader.estType) THEN ipbf-eb.bl-qty ELSE ipbf-estCostHeader.quantityMaster) * opbf-estCostBlank.quantityPerSet 
        opbf-estCostBlank.quantityYielded         = (IF fIsComboType(ipbf-estCostHeader.estType)THEN ipbf-eb.yld-qty ELSE ipbf-estCostHeader.quantityMaster) * opbf-estCostBlank.quantityPerSet
        
        opbf-estCostBlank.priceBasedOnYield       = ipbf-eb.yrprice AND fIsComboType(ipbf-estCostHeader.estType)
        
        .
        
    
    FIND FIRST bf-estCostItem EXCLUSIVE-LOCK 
        WHERE bf-estCostItem.estCostHeaderID EQ opbf-estCostBlank.estCostHeaderID
        AND bf-estCostItem.customerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE bf-estCostItem THEN 
    DO:
        ASSIGN 
            opbf-estCostBlank.estCostItemID = bf-estCostItem.estCostItemID
            bf-estCostItem.sizeDesc            = TRIM(STRING(opbf-estCostBlank.dimLength,">>>9.99")) + " x " + TRIM(STRING(opbf-estCostBlank.dimWidth,">>>9.99"))
            .
        IF opbf-estCostBlank.dimDepth NE 0 THEN 
            bf-estCostItem.sizeDesc = bf-estCostItem.sizeDesc + " x " + TRIM(STRING(opbf-estCostBlank.dimDepth,">>>9.99")).
            
        RELEASE bf-estCostItem.
    END.
    ASSIGN 
        ipbf-ttEstCostForm.numOutBlanksOnNet = ipbf-ttEstCostForm.numOutBlanksOnNet + opbf-estCostBlank.numOut
        ipbf-ttEstCostForm.blankArea         = ipbf-ttEstCostForm.blankArea + opbf-estCostBlank.blankArea * opbf-estCostBlank.numOut
        . 
    
    IF fIsSetType(ipbf-estCostHeader.estType) AND opbf-estCostBlank.formNo NE 0 THEN 
    DO:
        FIND FIRST bf-SetHeader-estCostBlank EXCLUSIVE-LOCK 
            WHERE bf-SetHeader-estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bf-SetHeader-estCostBlank.formNo EQ 0
            NO-ERROR.
        IF AVAILABLE bf-SetHeader-estCostBlank THEN 
            ASSIGN 
                bf-SetHeader-estCostBlank.weightPerBlank = bf-SetHeader-estCostBlank.weightPerBlank + 
                    opbf-estCostBlank.weightPerBlank * opbf-estCostBlank.quantityPerSet.
                    
    END.

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstCostForm FOR ttEstCostForm.
    
    CREATE opbf-ttEstCostForm.
    ASSIGN 
        opbf-ttEstCostForm.estCostHeaderID = ipbf-estCostHeader.estCostHeaderID
        opbf-ttEstCostForm.estimateNo      = ipbf-estCostHeader.estimateNo
        opbf-ttEstCostForm.company         = ipbf-estCostHeader.company
        opbf-ttEstCostForm.formNo          = ipiFormNo
        .
        
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostForm.estCostFormID, INPUT-OUTPUT opbf-ttEstCostForm.rec_key).
        
END PROCEDURE.

PROCEDURE pAddEstFormFromEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader. 
    DEFINE PARAMETER BUFFER opbf-ttEstCostForm FOR ttEstCostForm.

    RUN pAddEstForm(BUFFER ipbf-estCostHeader, ipbf-ef.form-no, BUFFER opbf-ttEstCostForm).
    
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
     Purpose: Create an estCostItem given eb and other key ids
     Notes: ce/print4p.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER opbf-estCostItem   FOR estCostItem.

    CREATE opbf-estCostItem.
    ASSIGN 
        opbf-estCostItem.estCostHeaderID           = ipbf-estCostHeader.estCostHeaderID
        opbf-estCostItem.company                   = ipbf-eb.company
        opbf-estCostItem.estimateNo                = ipbf-eb.est-no
        opbf-estCostItem.customerPart              = ipbf-eb.part-no
        opbf-estCostItem.colorDesc                 = ipbf-eb.i-coldscr
        opbf-estCostItem.customerID                = ipbf-eb.cust-no
        opbf-estCostItem.shipToID                  = ipbf-eb.ship-id
        opbf-estCostItem.itemName                  = ipbf-eb.part-dscr1
        opbf-estCostItem.itemDescription1          = ipbf-eb.part-dscr2
        opbf-estCostItem.salesgroupID              = ipbf-eb.sman
        opbf-estCostItem.styleID                   = ipbf-eb.style
        opbf-estCostItem.isSet                     = ipbf-eb.is-a-set
        opbf-estCostItem.itemID                    = ipbf-eb.stock-no
        opbf-estCostItem.company                   = ipbf-eb.company
        opbf-estCostItem.productCategory           = ipbf-eb.procat
        opbf-estCostItem.commissionPct             = ipbf-eb.comm
        opbf-estCostItem.carrierID                 = ipbf-eb.carrier
        opbf-estCostItem.carrierZone               = ipbf-eb.dest-code
        opbf-estCostItem.freightChargeMethod       = ipbf-eb.chg-method
        opbf-estCostItem.freightWeightPerMOverride = ipbf-eb.weight-m
        opbf-estCostItem.freightCostOverridePerCWT = ipbf-eb.fr-out-c
        opbf-estCostItem.freightCostOverridePerM   = ipbf-eb.fr-out-m
        opbf-estCostItem.isPurchased               = ipbf-eb.pur-man
        opbf-estCostItem.blankWidth                = IF ipbf-eb.t-wid EQ 0 THEN ipbf-eb.wid ELSE ipbf-eb.t-wid
        opbf-estCostItem.blankLength               = IF ipbf-eb.t-len EQ 0 THEN ipbf-eb.len ELSE ipbf-eb.t-len
        opbf-estCostItem.blankDepth                = IF ipbf-eb.t-dep EQ 0 THEN ipbf-eb.dep ELSE ipbf-eb.t-dep
        opbf-estCostItem.blankArea                 = IF ipbf-eb.t-sqin EQ 0 THEN opbf-estCostItem.blankLength * opbf-estCostItem.blankWidth ELSE ipbf-eb.t-sqin
        opbf-estCostItem.dimLength                 = ipbf-eb.len
        opbf-estCostItem.dimWidth                  = ipbf-eb.wid
        opbf-estCostItem.dimDepth                  = ipbf-eb.dep
        opbf-estCostItem.quantityPerSubUnit        = ipbf-eb.cas-cnt
        opbf-estCostItem.quantitySubUnitsPerUnit   = ipbf-eb.cas-pal
        /*Refactor - Calculate Windowing*/
        opbf-estCostItem.blankAreaNetWindow        = opbf-estCostItem.blankArea                
        /*Refactor - Hardcoded*/
        opbf-estCostItem.areaUOM                   = "SQIN"
        opbf-estCostItem.dimUOM                    = "IN"
        opbf-estCostItem.quantityPerSet            = fGetQuantityPerSet(BUFFER ipbf-eb)
        opbf-estCostItem.formNo                    = ipbf-eb.form-no
        opbf-estCostItem.blankNo                   = ipbf-eb.blank-no
        
        .
        
    IF fIsComboType(ipbf-estCostHeader.estType) THEN 
        ASSIGN 
            opbf-estCostItem.quantityRequired = ipbf-eb.bl-qty
            opbf-estCostItem.quantityYielded  = ipbf-eb.yld-qty
            .
    ELSE 
        ASSIGN 
            opbf-estCostItem.quantityRequired = ipbf-estCostHeader.quantityMaster * opbf-estCostItem.quantityPerSet
            opbf-estCostItem.quantityYielded  = opbf-estCostItem.quantityRequired
            .
            
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-eb.company
        AND cust.cust-no EQ ipbf-eb.cust-no
        NO-ERROR.
  
    /*Refactor - hardcoded temp?  Consider just using eb fields*/
    IF AVAILABLE cust AND cust.cust-no NE "Temp" THEN 
        ASSIGN 
            opbf-estCostItem.customerName     = cust.name
            opbf-estCostItem.customerAddress1 = cust.addr[1]
            opbf-estCostItem.customerAddress2 = cust.addr[2]
            opbf-estCostItem.customerAddress3 = cust.city + ", " + cust.state + " " + cust.zip
            .
    ELSE 
        ASSIGN  
            opbf-estCostItem.customerName     = ipbf-eb.ship-name
            opbf-estCostItem.customerAddress1 = ipbf-eb.ship-addr[1]
            opbf-estCostItem.customerAddress2 = ipbf-eb.ship-addr[2]
            opbf-estCostItem.customerAddress3 = ipbf-eb.ship-city + ", " + ipbf-eb.ship-state + " " + ipbf-eb.ship-zip
            .
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipbf-eb.company
        AND shipto.cust-no EQ ipbf-eb.cust-no
        AND shipto.ship-id EQ ipbf-eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN 
            opbf-estCostItem.shipToName     = shipto.ship-name
            opbf-estCostItem.shipToAddress1 = shipto.ship-addr[1]
            opbf-estCostItem.shipToAddress2 = shipto.ship-addr[2]
            opbf-estCostItem.shipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
            .
    FIND FIRST sman NO-LOCK 
        WHERE sman.company EQ ipbf-eb.company
        AND sman.sman EQ ipbf-eb.sman
        NO-ERROR.
    IF AVAILABLE sman THEN 
        ASSIGN 
            opbf-estCostItem.salesgroupName  = sman.sname
            opbf-estCostItem.commissionBasis = sman.commbasis        
            .
    FIND FIRST style NO-LOCK 
        WHERE style.company EQ ipbf-eb.company
        AND style.style EQ ipbf-eb.style
        NO-ERROR.
    IF AVAILABLE style THEN 
        ASSIGN 
            opbf-estCostItem.styleDesc = style.dscr
            .

END PROCEDURE.

PROCEDURE pAddEstFarm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create estCostMaterial for the Farm out for a estimate and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank    FOR estCostBlank.
    DEFINE PARAMETER BUFFER opbf-estCostMaterial FOR estCostMaterial.
    
    DEFINE           BUFFER bf-estCostItem       FOR estCostItem.
    
    FIND FIRST bf-estCostItem NO-LOCK 
        WHERE bf-estCostItem.estCostItemID EQ ipbf-estCostBlank.estCostItemID
        NO-ERROR.
    
    IF AVAILABLE bf-estCostItem THEN 
    DO:
        
        CREATE opbf-estCostMaterial.
        ASSIGN 
            opbf-estCostMaterial.estCostFormID      = ipbf-estCostBlank.estCostFormID
            opbf-estCostMaterial.estCostHeaderID    = ipbf-estCostBlank.estCostHeaderID
            opbf-estCostMaterial.estCostBlankID     = ipbf-estCostBlank.estCostBlankID
            opbf-estCostMaterial.company            = ipbf-estCostHeader.company
            opbf-estCostMaterial.estimateNo         = ipbf-estCostHeader.estimateNo
            opbf-estCostMaterial.formNo             = ipbf-estCostBlank.formNo
            opbf-estCostMaterial.blankNo            = ipbf-estCostBlank.blankNo
            opbf-estCostMaterial.itemID             = bf-estCostItem.itemID
            opbf-estCostMaterial.itemName           = bf-estCostItem.itemName
            opbf-estCostMaterial.quantityUOM        = "EA"
            opbf-estCostMaterial.quantityUOMWaste   = opbf-estCostMaterial.quantityUOM
            opbf-estCostMaterial.sequenceOfMaterial = 1
            opbf-estCostMaterial.dimLength          = ipbf-estCostBlank.blankLength
            opbf-estCostMaterial.dimWidth           = ipbf-estCostBlank.blankWidth
            opbf-estCostMaterial.dimDepth           = ipbf-estCostBlank.blankDepth
            opbf-estCostMaterial.dimUOM             = ipbf-estCostBlank.dimUOM
            .
            
    END. /*available estCostItem*/        
    
END PROCEDURE.

PROCEDURE pAddEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create estCostMaterial from an item buffer and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcRMItemID AS CHARACTER.
    DEFINE INPUT PARAMETER ipiEstCostBlankID AS INT64.
    DEFINE PARAMETER BUFFER opbf-estCostMaterial FOR estCostMaterial.
 
    DEFINE           BUFFER bf-item              FOR ITEM.

    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ  ipbf-estCostHeader.company
        AND bf-item.i-no EQ ipcRMItemID
        NO-ERROR.
    IF AVAILABLE bf-item THEN 
    DO:
        CREATE opbf-estCostMaterial.
        ASSIGN 
            opbf-estCostMaterial.estCostFormID    = ipbf-ttEstCostForm.estCostFormID
            opbf-estCostMaterial.estCostHeaderID  = ipbf-ttEstCostForm.estCostHeaderID
            opbf-estCostMaterial.estCostBlankID   = ipiEstCostBlankID
            opbf-estCostMaterial.company          = bf-item.company
            opbf-estCostMaterial.estimateNo       = ipbf-ttEstCostForm.estimateNo
            opbf-estCostMaterial.formNo           = ipbf-ttEstCostForm.formNo
            opbf-estCostMaterial.itemID           = bf-item.i-no 
            opbf-estCostMaterial.itemName         = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name 
            opbf-estCostMaterial.quantityUOM      = CAPS(bf-item.cons-uom)
            opbf-estCostMaterial.quantityUOMWaste = opbf-estCostMaterial.quantityUOM
            opbf-estCostMaterial.basisWeight      = bf-item.basis-w
            opbf-estCostMaterial.basisWeightUOM   = gcDefaultBasisWeightUOM
            opbf-estCostMaterial.dimLength        = bf-item.s-len
            opbf-estCostMaterial.dimWidth         = bf-item.s-wid
            opbf-estCostMaterial.dimDepth         = bf-item.s-dep
            opbf-estCostMaterial.dimUOM           = "IN"
            opbf-estCostMaterial.costPerUOMAvg    = bf-item.avg-cost
            opbf-estCostMaterial.costPerUOMLast   = bf-item.last-cost
            opbf-estCostMaterial.isRealMaterial   = bf-item.i-code EQ "R"
            opbf-estCostMaterial.materialType     = bf-item.mat-type
            opbf-estCostMaterial.weightUOM        = gcDefaultWeightUOM
            .
        
        IF CAN-DO(gcBoardMatTypes, opbf-estCostMaterial.materialType) THEN 
            opbf-estCostMaterial.sequenceOfMaterial = 1.
        ELSE IF CAN-DO(gcInkMatTypes,opbf-estCostMaterial.materialType) THEN 
                opbf-estCostMaterial.sequenceOfMaterial = 2.
            ELSE IF CAN-DO(gcGlueMatTypes,opbf-estCostMaterial.materialType) THEN 
                    opbf-estCostMaterial.sequenceOfMaterial = 3.
                ELSE IF CAN-DO(gcLeafMatTypes,opbf-estCostMaterial.materialType) THEN 
                        opbf-estCostMaterial.sequenceOfMaterial = 4.
                    ELSE IF CAN-DO(gcPackMatTypes,opbf-estCostMaterial.materialType) THEN 
                            opbf-estCostMaterial.sequenceOfMaterial = 5.
                        ELSE  
                            opbf-estCostMaterial.sequenceOfMaterial = 6.
            
        IF opbf-estCostMaterial.estCostBlankID NE 0 THEN 
        DO:
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostBlankID EQ opbf-estCostMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
            DO:
                IF NOT opbf-estCostMaterial.isRealMaterial THEN 
                    ASSIGN 
                        opbf-estCostMaterial.dimLength = estCostBlank.blankLength
                        opbf-estCostMaterial.dimWidth  = estCostBlank.blankWidth
                        opbf-estCostMaterial.dimLength = estCostBlank.blankLength
                        .
                ASSIGN 
                    opbf-estCostMaterial.blankNo = estCostBlank.blankNo
                    .
            END.
        END.
        ELSE IF NOT opbf-estCostMaterial.isRealMaterial THEN 
                ASSIGN 
                    opbf-estCostMaterial.dimLength = ipbf-ttEstCostForm.grossLength
                    opbf-estCostMaterial.dimWidth  = ipbf-ttEstCostForm.grossWidth
                    opbf-estCostMaterial.dimDepth  = ipbf-ttEstCostForm.grossDepth
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
    
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostMisc.estCostMiscID, INPUT-OUTPUT opbf-ttEstCostMisc.rec_key).
    
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
        FIND FIRST estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
            AND estCostBlank.blankNo EQ iBlankNo
            NO-ERROR.
        IF AVAILABLE estCostBlank THEN 
            dQty = estCostBlank.quantityRequired. 
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
    
    DEFINE           BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    DEFINE           BUFFER bf-prep          FOR prep.
    
    RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
    FIND FIRST bf-prep NO-LOCK 
        WHERE bf-prep.company EQ ipbf-est-prep.company
        AND bf-prep.code EQ ipbf-est-prep.code
        NO-ERROR.
        
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
    
    RUN pSetKeyFields(INPUT-OUTPUT opbf-ttEstCostOperation.estCostOperationID, INPUT-OUTPUT opbf-ttEstCostOperation.rec_key).
        
END PROCEDURE.

PROCEDURE pAddEstOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstCostOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op           FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER opbf-ttEstCostOperation FOR ttEstCostOperation.

    DEFINE           BUFFER bf-mach               FOR mach.
    DEFINE           BUFFER bf-est-op             FOR est-op.

    DEFINE VARIABLE cOutputType AS CHARACTER NO-UNDO.
    
    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
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
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ opbf-ttEstCostOperation.estCostHeaderID
                AND estCostBlank.estCostFormID EQ opbf-ttEstCostOperation.estCostFormID
                AND estCostBlank.blankNo EQ opbf-ttEstCostOperation.blankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                opbf-ttEstCostOperation.estCostBlankID = estCostBlank.estCostBlankID. 
        END.
    END.
    
END PROCEDURE.

PROCEDURE pAddGlue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a glue
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank  FOR estCostBlank.
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.
  
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
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is not valid", gcErrorImportant, ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo,ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        IF NOT CAN-DO(gcGlueMatTypes,bf-item.mat-type) THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid material but not a material type of " + gcGlueMatTypes, gcErrorImportant, ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo,ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        IF bf-item.sqin-lb EQ 0 AND bf-item.linin-lb EQ 0 THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid glue material but no coverage rate configured", gcErrorWarning, ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo,ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        
        FIND FIRST ttGlue EXCLUSIVE-LOCK 
            WHERE ttGlue.estHeaderID EQ ipbf-estCostBlank.estCostHeaderID
            AND ttGlue.estFormID EQ ipbf-estCostBlank.estCostFormID
            AND ttGlue.estBlankID EQ ipbf-estCostBlank.estCostBlankID
            AND ttGlue.iFormNo EQ ipbf-estCostBlank.formNo
            AND ttGlue.iBlankNo EQ ipbf-estCostBlank.blankNo
            AND ttGlue.cItemID EQ ipbf-eb.adhesive
            NO-ERROR.
        IF NOT AVAILABLE ttGlue THEN 
        DO:
            CREATE ttGlue.
            ASSIGN 
                ttGlue.company       = ipbf-estCostBlank.company
                ttGlue.estHeaderID   = ipbf-estCostBlank.estCostHeaderID
                ttGlue.estFormID     = ipbf-estCostBlank.estCostFormID
                ttGlue.estBlankID    = ipbf-estCostBlank.estCostBlankID
                ttGlue.iFormNo       = ipbf-estCostBlank.formNo
                ttGlue.iBlankNo      = ipbf-estCostBlank.blankNo
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
                    dQtyRequiredPerBlank    = ipbf-estCostBlank.blankArea / ttGlue.dCoverageRate
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
       
    CREATE estCostHeader.
    ASSIGN 
        estCostHeader.calcDateTime   = NOW
        estCostHeader.calculatedBy   = USERID("ASI")
        estCostHeader.company        = ipbf-est.company
        estCostHeader.estimateNo     = ipbf-est.est-no
        estCostHeader.quantityMaster = ipdQtyMaster
        estCostHeader.releaseCount   = ipdReleases
        estCostHeader.jobID          = ipcJobID
        estCostHeader.jobID2         = ipiJobID2
        opiEstCostHeaderID           = estCostHeader.estCostHeaderID
        .
        
    RUN pBuildHeader(BUFFER estCostHeader).

END PROCEDURE.

PROCEDURE pAddInk PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add an ink
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCoveragePercent AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER iplNoCharge AS LOGICAL NO-UNDO.
        
    DEFINE BUFFER bf-item FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-estCostBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Ink RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcInkMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Ink RM Code '" + ipcItemCode + "' is not one of '" + gcInkMatTypes + "'", gcErrorImportant,ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        IF bf-item.yield EQ 0 OR bf-item.yield EQ ? THEN 
        DO: 
            RUN pAddError("Ink RM Code '" + ipcItemCode + "' has an invalid coverage rate.", gcErrorImportant,ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        FIND FIRST ttInk EXCLUSIVE-LOCK 
            WHERE ttInk.estHeaderID EQ ipbf-estCostBlank.estCostHeaderID
            AND ttInk.estFormID EQ ipbf-estCostBlank.estCostFormID
            AND ttInk.estBlankID EQ ipbf-estCostBlank.estCostBlankID
            AND ttInk.iFormNo EQ ipbf-estCostBlank.formNo
            AND ttInk.iBlankNo EQ ipbf-estCostBlank.blankNo
            AND ttInk.cItemID EQ ipcItemCode
            AND ttInk.iPass EQ ipiPass
            NO-ERROR.
        IF NOT AVAILABLE ttInk THEN 
        DO:
            CREATE ttInk.
            ASSIGN 
                ttInk.company          = ipbf-estCostBlank.company
                ttInk.estHeaderID      = ipbf-estCostBlank.estCostHeaderID
                ttInk.estFormID        = ipbf-estCostBlank.estCostFormID
                ttInk.estBlankID       = ipbf-estCostBlank.estCostBlankID
                ttInk.iFormNo          = ipbf-estCostBlank.formNo
                ttInk.iBlankNo         = ipbf-estCostBlank.blankNo
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

    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
            
    DEFINE BUFFER bf-item         FOR item.
    DEFINE BUFFER bf-estCostBlank FOR estCostBlank.
    
    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Leaf/Film RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipiBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcLeafMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Leaf/Film RM Code '" + ipcItemCode + "' is not one of '" + gcLeafMatTypes + "'", gcErrorImportant,ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, ipiBlankNo).
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
                ttLeaf.lIsWindow           = CAN-DO(gcWindowMatTypes, bf-item.mat-type) AND bf-item.industry EQ "1"
                ttLeaf.lIsWax              = CAN-DO(gcWaxMatTypes, bf-item.mat-type) AND bf-item.industry EQ "2"
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
                
            FIND FIRST bf-estCostBlank EXCLUSIVE-LOCK 
                WHERE bf-estCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND bf-estCostBlank.estCostFormID EQ ttLeaf.estFormID
                AND bf-estCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE bf-estCostBlank THEN 
                ASSIGN
                    /*Only add Window Area if material is a Window - i.e. cut out*/ 
                    bf-estCostBlank.blankAreaWindow    = bf-estCostBlank.blankAreaWindow + IF ttLeaf.lIsWindow THEN ttLeaf.dAreaInSQInAperture ELSE 0
                    bf-estCostBlank.blankAreaNetWindow = bf-estCostBlank.blankArea - bf-estCostBlank.blankAreaWindow
                    ttLeaf.estBlankID                  = bf-estCostBlank.estCostBlankID
                    .
            RELEASE bf-estCostBlank.
        END.
        
    END.

END PROCEDURE.

PROCEDURE pAddPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a packing material
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttPack FOR ttPack.
        
    DEFINE           BUFFER bf-item     FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-estCostBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Pack RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcPackMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Packing RM Code '" + ipcItemCode + "' is not one of '" + gcPackMatTypes + "'", gcErrorImportant,ipbf-estCostBlank.estCostHeaderID, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo).
            RETURN.
        END.
        FIND FIRST opbf-ttPack EXCLUSIVE-LOCK 
            WHERE opbf-ttPack.estHeaderID EQ ipbf-estCostBlank.estCostHeaderID
            AND opbf-ttPack.estFormID EQ ipbf-estCostBlank.estCostFormID
            AND opbf-ttPack.estBlankID EQ ipbf-estCostBlank.estCostBlankID
            AND opbf-ttPack.iFormNo EQ ipbf-estCostBlank.formNo
            AND opbf-ttPack.iBlankNo EQ ipbf-estCostBlank.blankNo
            AND opbf-ttPack.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE opbf-ttPack THEN 
        DO:
            CREATE opbf-ttPack.
            ASSIGN 
                opbf-ttPack.company               = ipbf-estCostBlank.company
                opbf-ttPack.estHeaderID           = ipbf-estCostBlank.estCostHeaderID
                opbf-ttPack.estFormID             = ipbf-estCostBlank.estCostFormID
                opbf-ttPack.estBlankID            = ipbf-estCostBlank.estCostBlankID
                opbf-ttPack.iFormNo               = ipbf-estCostBlank.formNo
                opbf-ttPack.iBlankNo              = ipbf-estCostBlank.blankNo
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
                BUFFER estCostBlank, BUFFER estCostItem, BUFFER eb, iEstCostFormID, iEstCostBlankID).
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank  FOR estCostBlank.
    DEFINE PARAMETER BUFFER ipbf-estCostItem   FOR estCostItem.
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
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    DEFINE BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    
        
    ASSIGN 
        dQtyShipped = ipbf-estCostBlank.quantityRequired
                
        /*REFACTOR - MSF assumed use blankAreaUOM*/
        dMSFforEA   = ipbf-estCostItem.blankArea / 144000  
                
        /*REFACTOR - LBs assumed use weightUOM */
        dLBsforEA   = IF ipbf-estCostItem.freightWeightPerMOverride NE 0 
                                THEN ipbf-estCostItem.freightWeightPerMOverride / 1000 
                                ELSE ipbf-estCostItem.weightTotal / ipbf-estCostItem.quantityRequired
        .
            
    /*Get Total Freight from Freight Procs*/
    IF DYNAMIC-FUNCTION("HasReleases", ROWID(ipbf-eb)) THEN  /*Run through estReleases Calc*/
        RUN GetFreightForEstimateBlank (ipbf-estCostBlank.company, ipbf-estCostBlank.estimateNo, dQtyShipped, ipbf-estCostBlank.formNo, ipbf-estCostBlank.blankNo,
            OUTPUT dFreightTotal).
    ELSE 
    DO: /*Calc Freight based on basic inputs*/
        RUN GetFreight(ipbf-estCostBlank.company, ipbf-estCostHeader.warehouseID, ipbf-estCostItem.carrierID, ipbf-estCostItem.carrierZone, "", 
            dQtyShipped, dLBSforEA, dMSFforEA, ipbf-estCostBlank.quantityOfUnits, ipbf-estCostItem.freightCostOverridePerCWT, ipbf-estCostItem.freightCostOverridePerM,
            OUTPUT dFreightTotal, OUTPUT dFreightMin, OUTPUT lError, OUTPUT cMessage).
        IF ipbf-estCostHeader.releaseCount GT 1 AND dFreightMin GT 0 THEN 
            dFreightTotal = dFreightTotal + (ipbf-estCostHeader.releaseCount - 1) * dFreightMin.
    END.
    IF dFreightTotal NE 0 THEN 
    DO:
        IF ipbf-estCostItem.freightChargeMethod EQ "P" THEN 
        DO: /*Integrate Freight Cost for Prepaid*/
            RUN pAddCostDetail(ipbf-estCostBlank.estCostHeaderID, ipiEstCostFormIDForCost, ipiEstCostBlankIDForCost, ipbf-estCostBlank.estCostBlankID, 
                gcSourceTypeNonFactory, "nfFreight", "Freight", dFreightTotal, 0, ipbf-estCostBlank.company, ipbf-estCostBlank.estimateNo, BUFFER bf-ttEstCostDetail).
        END. /*Prepaid Freight*/
        ELSE 
        DO: /*Separate Billed Freight*/
            RUN pAddEstMisc(BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostMisc).
            IF AVAILABLE bf-ttEstCostMisc THEN 
            DO:
                ASSIGN 
                    bf-ttEstCostMisc.estCostFormID         = ipiEstCostFormIDForCost
                    bf-ttEstCostMisc.estCostBlankID        = ipiEstCostBlankIDForCost
                    bf-ttEstCostMisc.formNo                = ipbf-estCostBlank.formNo  
                    bf-ttEstCostMisc.blankNo               = ipbf-estCostBlank.blankNo
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
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.

    IF CAN-DO(gcBoardMatTypes,ipbf-estCostMaterial.materialType) THEN 
    DO:
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "boardNoWaste","Board Cost - No Waste",
            ipbf-estCostMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "boardSetupWaste","Board Cost - Setup Waste",
            ipbf-estCostMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "boardRunWaste","Board Cost - Run Waste",
            ipbf-estCostMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "boardSetupVend","Board Cost - Vendor Setup",
            ipbf-estCostMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "boardMinDiff","Board Cost - Minimum Diff",
            ipbf-estCostMaterial.costTotalMinDiff,0).
    END.
    ELSE 
    DO:        
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matNoWaste","Material Cost - No Waste",
            ipbf-estCostMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matSetupWaste","Material Cost - Setup Waste",
            ipbf-estCostMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matRunWaste","Material Cost - Run Waste",
            ipbf-estCostMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matSetupVend","Material Cost - Vendor Setup",
            ipbf-estCostMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matMinDiff","Material Cost - Minimum Diff",
            ipbf-estCostMaterial.costTotalMinDiff,0).
    END.    
    IF ipbf-estCostMaterial.costTotalDeviation NE 0 THEN 
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "deviation","Deviation Cost",
            ipbf-estCostMaterial.costTotalDeviation,0).
        
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
    DEFINE BUFFER bf-estCostHeader        FOR estCostHeader.
    DEFINE BUFFER bf-ef                   FOR ef.
    DEFINE BUFFER bf-estCostBlank         FOR estCostBlank.
    DEFINE BUFFER bf-estCostItem          FOR estCostItem.
    DEFINE BUFFER bf-estCostMaterial      FOR estCostMaterial.
    DEFINE BUFFER bf-ttEstCostMisc          FOR ttEstCostMisc.
    
    
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
    
    
    FOR FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
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
            
        
        FOR EACH bf-estCostBlank NO-LOCK 
            WHERE bf-estCostBlank.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-estCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
            FIRST bf-estCostItem NO-LOCK 
            WHERE bf-estCostItem.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID:
                
            IF bf-estCostBlank.isPurchased THEN
                dApplicableFGHandlingRate = bf-estCostHeader.handlingRateFGFarmPerCWT.
            ELSE
                dApplicableFGHandlingRate = bf-estCostHeader.handlingRateFGPerCWT.
                
            
            ASSIGN
                dWeightInSrcUOM = bf-estCostItem.weightTotal
                cSrcUOM         = bf-estCostItem.weightUOM.
                
            RUN pConvertQuantityFromUOMToUOM(bf-estCostItem.company, bf-estCostItem.itemID, "RM", cSrcUOM, "CWT", 
                0, bf-estCostItem.dimLength, bf-estCostItem.dimWidth, bf-estCostItem.dimDepth, 
                dWeightInSrcUOM, OUTPUT dNetWeightInCUOM).
                
            
            /* calculate the FG handling charge using Net weight. From the blank item, we can get FG Net weights by form. */
            IF dApplicableFGHandlingRate NE 0 THEN 
                dFGHandlingCost = dFGHandlingCost + (dNetWeightInCUOM * dApplicableFGHandlingRate).    
            
            /* Set Form Level fields based upon first blank values. is there a more accurate way to find find primary Blank */
            IF lFirstBlank = YES THEN
            DO:
                lFirstBlank = NO.
                
                IF bf-estCostBlank.isPurchased THEN
                    ASSIGN 
                        dApplicableRMHandlingRate = bf-estCostHeader.handlingRateRMFarmPerCWT 
                        dApplicableHandlingPct    = bf-estCostHeader.handlingChargeFarmPct
                        .
                ELSE
                    ASSIGN 
                        dApplicableRMHandlingRate = bf-estCostHeader.handlingRateRMPerCWT
                        dApplicableHandlingPct    = bf-estCostHeader.handlingChargePct.
            END.
                
        END. /* FOR EACH bf-estCostBlank NO-LOCK */ 
        
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
        FOR EACH bf-estCostMaterial NO-LOCK 
            WHERE bf-estCostMaterial.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-estCostMaterial.estCostFormID   EQ bf-ttEstCostForm.estCostFormID:
                
                dCostTotalMaterial = dCostTotalMaterial + bf-estCostMaterial.costTotal.
                 
        END.
        
        /* Include cost for Prep and any Material misc cost which is not Labour */
        FOR EACH bf-ttEstCostMisc NO-LOCK 
            WHERE bf-ttEstCostMisc.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
              AND bf-ttEstCostMisc.estCostFormID   EQ bf-ttEstCostForm.estCostFormID:
                
            IF bf-ttEstCostMisc.isPrep = YES 
            OR (bf-ttEstCostMisc.isPrep = NO AND bf-ttEstCostMisc.costType = "Mat") THEN
                dCostTotalMaterial = dCostTotalMaterial + bf-ttEstCostMisc.costTotal.
                
        END. /*Each estCostMaterial for estHeader*/
        
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

    DEFINE BUFFER bf-est     FOR est.
    DEFINE BUFFER bf-est-qty FOR est-qty.
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
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
                    FIND FIRST bf-estCostHeader NO-LOCK 
                        WHERE bf-estCostHeader.estCostHeaderID EQ opiEstCostHeaderID
                        NO-ERROR.
                    IF AVAILABLE bf-estCostHeader AND fIsComboType(bf-estCostHeader.estType) THEN LEAVE.
                END.
            END.
    END.

END PROCEDURE.

PROCEDURE pBuildProbe PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostHeader, make or update a probe record for display in Print tab
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    
    DEFINE           BUFFER bf-probe           FOR probe.
    DEFINE           BUFFER bf-probeit         FOR probeit.
    DEFINE           BUFFER bf-estimate-probe  FOR probe.
    
    DEFINE VARIABLE iProbeLine     AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyInM        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInMForItem AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPricePerM     AS DECIMAL NO-UNDO.
       
    DISABLE TRIGGERS FOR LOAD OF probe.
    
    IF ipbf-estCostHeader.jobID NE "" THEN RETURN.
    
    dQtyInM    = ipbf-estCostHeader.quantityMaster / 1000.
    
    FIND FIRST bf-probe EXCLUSIVE-LOCK 
        WHERE bf-probe.company EQ ipbf-estCostHeader.company
        AND bf-probe.est-no EQ ipbf-estCostHeader.estimateNo
        AND bf-probe.spare-char-2 EQ STRING(ipbf-estCostHeader.estCostHeaderID)
        NO-ERROR.
    
    IF NOT AVAILABLE bf-probe THEN 
    DO:        
        FOR EACH bf-estimate-probe NO-LOCK 
            WHERE bf-estimate-probe.company EQ ipbf-estCostHeader.company
            AND bf-estimate-probe.est-no EQ ipbf-estCostHeader.estimateNo
            AND bf-estimate-probe.probe-date <> ?
            BY bf-estimate-probe.LINE DESCENDING:
            iProbeLine = bf-estimate-probe.LINE.
            LEAVE.
        END.
        iProbeLine = iProbeLine + 1.
        CREATE bf-probe.
        ASSIGN 
            bf-probe.company      = ipbf-estCostHeader.company
            bf-probe.est-no       = ipbf-estCostHeader.estimateNo
            bf-probe.line         = iProbeLine
            bf-probe.spare-char-2 = STRING(ipbf-estCostHeader.estCostHeaderID)
            bf-probe.est-qty      = ipbf-estCostHeader.quantityMaster        
            bf-probe.probe-date   = DATE(ipbf-estCostHeader.calcDateTime)
            bf-probe.probe-time   = INTEGER(MTIME(ipbf-estCostHeader.calcDateTime) / 1000)
            bf-probe.probe-user   = ipbf-estCostHeader.calculatedBy
            bf-probe.freight      = ipbf-estCostHeader.releaseCount             /* Holds Number of Releases */
            bf-probe.tot-lbs      = ipbf-estCostHeader.weightTotal
            .
            
        FOR EACH ttEstCostForm NO-LOCK 
            WHERE ttEstCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            :
            
            ASSIGN 
                bf-probe.gsh-qty    = bf-probe.gsh-qty + ttEstCostForm.grossQtyRequiredTotal
                bf-probe.gshQtyInSF = bf-probe.gshQtyInSF + (ttEstCostForm.grossQtyRequiredTotalArea * 1000 )
                .                  
        END.    
        FOR EACH estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND estCostBlank.formNo NE 0
            :
            ASSIGN 
                bf-probe.bsf = bf-probe.bsf + estCostBlank.quantityPerSet * estCostBlank.blankArea  / 144 //Refactor - assumes area is SQIN.
                .
        END.
    END.
    ASSIGN 
        bf-probe.fact-cost      = ipbf-estCostHeader.costTotalFactory / dQtyInM
        bf-probe.full-cost      = ipbf-estCostHeader.costTotalFull / dQtyInM
        bf-probe.gross-profit   = ipbf-estCostHeader.profitPctGross
        bf-probe.net-profit     = ipbf-estCostHeader.profitPctNet 
        dPricePerM              = ipbf-estCostHeader.sellPrice / dQtyInM
        bf-probe.sell-price     = IF glRoundPriceToDollar THEN ROUND(dPricePerM, 0) ELSE ROUND(dPricePerM, 2)
        bf-probe.spare-dec-1    = ipbf-estCostHeader.costTotalMaterial / dQtyInM
        bf-probe.boardCostTotal = ipbf-estCostHeader.costTotalBoard
        bf-probe.boardCostPerM  = ipbf-estCostHeader.costTotalBoard / dQtyInM
        .
    IF ipbf-estCostHeader.sellPrice GT 0 THEN 
        bf-probe.boardCostPct  = ipbf-estCostHeader.costTotalBoard / ipbf-estCostHeader.sellPrice * 100.
                    
    FOR EACH estCostItem NO-LOCK
        WHERE estCostItem.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        AND NOT estCostItem.isSet:
            
        ASSIGN 
            dQtyInMForItem = estCostItem.quantityRequired / 1000
            .
        FIND FIRST bf-probeit EXCLUSIVE-LOCK 
            WHERE bf-probeit.company EQ ipbf-estCostHeader.company
            AND bf-probeit.est-no EQ ipbf-estCostHeader.estimateNo
            AND bf-probeit.part-no EQ estCostItem.customerPart
            AND bf-probeit.line EQ bf-probe.line
            NO-ERROR.            
        IF NOT AVAILABLE bf-probeit THEN 
        DO:
            CREATE bf-probeit.
            ASSIGN 
                bf-probeit.line    = bf-probe.line
                bf-probeit.bl-qty  = estCostItem.quantityRequired
                bf-probeit.yld-qty = estCostItem.quantityYielded
                bf-probeit.company = estCostItem.company
                bf-probeit.cust-no = estCostItem.customerID
                bf-probeit.est-no  = estCostItem.estimateNo
                bf-probeit.part-no = estCostItem.customerPart
                .
        END.
        ASSIGN
            dPricePerM            = estCostItem.sellPrice / dQtyInMForItem
            bf-probeit.fact-cost  = estCostItem.costTotalFactory / dQtyInMForItem
            bf-probeit.full-cost  = estCostItem.costTotalFull / dQtyInMForItem
            bf-probeit.sell-price = IF glRoundPriceToDollar THEN ROUND(dPricePerM, 0) ELSE ROUND(dPricePerM, 2)
            bf-probeit.brd-cost   = estCostItem.costTotalBoard / dQtyInMForItem
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
    
    DEFINE BUFFER bf-estCostHeader          FOR estCostHeader.
    DEFINE BUFFER bf-estCostBlank           FOR estCostBlank.
    DEFINE BUFFER bf-ttEstCostForm          FOR ttEstCostForm.
    DEFINE BUFFER bf-estCostItem            FOR estCostItem.
    DEFINE BUFFER bf-eb                     FOR eb.
    
    DEFINE BUFFER bfFirstBlank-estCostBlank FOR estCostBlank.
    DEFINE BUFFER bfComp-estCostBlank       FOR estCostBlank.
    
    FIND FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-estCostHeader THEN RETURN.
    IF bf-estCostHeader.isUnitizedSet THEN 
    DO:
        FOR FIRST bf-estCostBlank NO-LOCK
            WHERE bf-estCostBlank.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            AND bf-estCostBlank.formNo EQ 0
            AND bf-estCostBlank.blankNo EQ 0,
            FIRST bf-ttEstCostForm NO-LOCK 
            WHERE bf-ttEstCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
              AND bf-ttEstCostForm.estCostFormID EQ bf-estCostBlank.estCostFormID,
            FIRST bf-estCostItem NO-LOCK 
            WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
              AND bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID,
            FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-estCostBlank.company
            AND bf-eb.est-no EQ bf-estCostBlank.estimateNo
            AND bf-eb.form-no EQ bf-estCostBlank.formNo
            AND bf-eb.blank-no EQ bf-estCostBlank.blankNo,
            FIRST bfFirstBlank-estCostBlank NO-LOCK
            WHERE bfFirstBlank-estCostBlank.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            AND bfFirstBlank-estCostBlank.formNo EQ 1
            AND bfFirstBlank-estCostBlank.blankNo EQ 1
            :
            RUN pBuildCostDetailForFreight(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, 
                BUFFER bf-estCostBlank, BUFFER bf-estCostItem, BUFFER bf-eb, 
                bfFirstBlank-estCostBlank.estCostFormID, bfFirstBlank-estCostBlank.estCostBlankID).
        END. /*Set Header Blank*/
    END.
    ELSE 
    DO:    
        FOR EACH bf-ttEstCostForm NO-LOCK
            WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
            FIRST bf-estCostHeader NO-LOCK 
            WHERE bf-estCostHeader.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID,
            EACH bf-estCostBlank NO-LOCK 
            WHERE bf-estCostBlank.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
              AND bf-estCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
            FIRST bf-estCostItem NO-LOCK 
            WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
              AND bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID,
            FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-estCostBlank.company
            AND bf-eb.est-no EQ bf-estCostBlank.estimateNo
            AND bf-eb.form-no EQ bf-estCostBlank.formNo
            AND bf-eb.blank-no EQ bf-estCostBlank.blankNo
            :
            RUN pBuildCostDetailForFreight(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, 
                BUFFER bf-estCostBlank, BUFFER bf-estCostItem, BUFFER bf-eb, 
                bf-estCostBlank.estCostFormID, bf-estCostBlank.estCostBlankID).        
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
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
          AND estCostBlank.estCostFormID EQ bf-ttEstCostForm.estCostFormID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID 
          AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
        
        RUN pGetPriceProfitAndCommissionForForm(BUFFER bf-ttEstCostForm, BUFFER estCostItem, OUTPUT dNetProfitForForm, OUTPUT dCommission, OUTPUT dPriceForForm).
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
    DEFINE PARAMETER BUFFER opbf-estCostMaterial FOR estCostMaterial.
    
    DEFINE VARIABLE lFoam  AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostBlank    FOR estCostBlank.
    
    IF NOT glCalcFoamCostFromBlank  THEN
        RETURN.
        
    lFoam = IsFoamStyle (ipbf-ttEstCostForm.company, ipbf-ttEstCostForm.estimateNo, ipbf-ttEstCostForm.formNo).
        
    IF lFoam THEN
    DO:
        FIND FIRST bf-estCostBlank NO-LOCK 
            WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
              AND bf-estCostBlank.estCostFormID   EQ ipbf-ttEstCostForm.estCostFormID NO-ERROR.
        
        IF AVAILABLE bf-estCostBlank THEN
            ASSIGN 
                opbf-estCostMaterial.dimLength               = bf-estCostBlank.blankLength
                opbf-estCostMaterial.dimWidth                = bf-estCostBlank.blankWidth
                opbf-estCostMaterial.dimDepth                = bf-estCostBlank.blankDepth
                opbf-estCostMaterial.quantityRequiredNoWaste = bf-estCostBlank.quantityRequired
                .
    END.  

END PROCEDURE.

PROCEDURE pBuildFreightForBoardCost PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Determine the Freight charges for Board Material
     Notes: This cost is processing for Single/combo estimates but not being included in case of Item-BOM setup.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ef                   FOR ef. 
    DEFINE BUFFER bf-ttEstCostForm        FOR ttEstCostForm. 
    DEFINE BUFFER bf-estCostBlank         FOR estCostBlank. 
    DEFINE BUFFER bf-estCostHeader        FOR estCostHeader.
    DEFINE BUFFER bf-estCostItem          FOR estCostItem. 
    
    
    DEFINE VARIABLE dBoardFreight   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInCUOM   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityInSrcUOM AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE cSrcUOM         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPerMSheetUOM   AS CHARACTER NO-UNDO INIT "MSH".
    
   
    FOR FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
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
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    RUN pBuildFactoryCostDetails(ipiEstCostHeaderID).
    RUN pBuildNonFactoryCostDetails(ipiEstCostHeaderID).

    RUN pBuildPriceRelatedCostDetails(ipiEstCostHeaderID).
    RUN pBuildCostSummary(ipiEstCostHeaderID).
    
    FIND FIRST bf-estCostHeader NO-LOCK
        WHERE bf-estCostHeader.estCostHeaderID = ipiEstCostHeaderID NO-ERROR. 
        
    IF AVAILABLE bf-estCostHeader THEN
    DO:
        RUN pCopyHeaderCostsToSetItem(BUFFER bf-estCostHeader).
        RUN pBuildProbe(BUFFER bf-estCostHeader).
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
     
    IF iplPurge THEN 
        RUN pPurgeCalculation(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2).
        
    RUN pBuildHeadersToProcess(ipcCompany, ipcEstimateNo, ipcJobNo, ipiJobNo2, ipiQuantity, OUTPUT opiEstCostHeaderID).
    FOR EACH ttEstCostHeaderToCalc: 
        RUN pCalcHeader(ttEstCostHeaderToCalc.iEstCostHeaderID).
    END.
    IF iplPrompt THEN 
        RUN pPromptForCalculationChanges.
    
    RUN pWriteDatasetIntoDB.
    
    IF VALID-HANDLE(ghOperation) THEN
        DELETE PROCEDURE ghOperation.

END PROCEDURE.

PROCEDURE pCalcHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Main Build of data from Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostForm     FOR ttEstCostForm.
    DEFINE BUFFER bf-estCostBlank      FOR estCostBlank.
    DEFINE BUFFER bf-estCostMaterial   FOR estCostMaterial.
    DEFINE BUFFER bf-ttEstCostOperation  FOR ttEstCostOperation.
    DEFINE BUFFER bf-estCostHeader     FOR estCostHeader.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyOnForm          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyOnFormRequired  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyOnFormYielded   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyMaster          AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
         
    FOR EACH bf-estCostHeader NO-LOCK
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ bf-estCostHeader.company
            AND est.est-no EQ bf-estCostHeader.estimateNo
            NO-ERROR.
        
        IF NOT AVAILABLE est THEN RETURN.

        RUN pBuildItems(BUFFER bf-estCostHeader).
        dQtyMaster          = 0.
        
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no
            BY ef.form-no DESCENDING:
            
            RUN pAddEstFormFromEf(BUFFER ef, BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).
            
            ASSIGN 
                iNumOutBlanksOnForm = 0
                dQtyOnForm          = 0
                dQtyOnFormRequired  = 0
                dQtyOnFormYielded   = 0
                .
            
            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-estCostBlank).
                ASSIGN 
                    iNumOutBlanksOnForm = iNumOutBlanksOnForm + bf-estCostBlank.numOut
                    dQtyOnForm          = dQtyOnForm + 
                                        (IF bf-estCostBlank.priceBasedOnYield THEN bf-estCostBlank.quantityYielded ELSE bf-estCostBlank.quantityRequired)
                    dQtyOnFormRequired  = dQtyOnFormRequired + bf-estCostBlank.quantityRequired
                    dQtyOnFormYielded   = dQtyOnFormYielded + bf-estCostBlank.quantityYielded
                    .
                RUN pBuildInksForEb(BUFFER bf-estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                RUN pAddGlue(BUFFER bf-estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                RUN pBuildPackingForEb(BUFFER bf-estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                
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
            RUN pProcessOperations(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).
            IF AVAILABLE bf-estCostBlank AND bf-estCostBlank.isPurchased THEN 
                RUN pProcessFarm(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-estCostBlank ).
            ELSE DO: 
                RUN pProcessLeafs(BUFFER ef, BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).
                RUN pProcessBoard(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, BUFFER ef).      
                RUN pProcessAdders(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm, ef.adder).   
                RUN pProcessInks(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).
                RUN pProcessGlues(BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).
            END.
            RUN pProcessSpecialMaterials(BUFFER ef, BUFFER bf-estCostHeader, BUFFER bf-ttEstCostForm).  
            
            RUN pProcessMiscPrep(BUFFER ef, BUFFER bf-ttEstCostForm).
            RUN pProcessMiscNonPrep(BUFFER ef, BUFFER bf-ttEstCostForm).
                      
        END.  /*Each ef of est*/  
        /* if combo, update the master quantity for per M calculations*/
        IF fIsComboType(bf-estCostHeader.estType) THEN 
        DO:
            FIND CURRENT bf-estCostHeader EXCLUSIVE-LOCK.
            bf-estCostHeader.quantityMaster = dQtyMaster.
            FIND CURRENT bf-estCostHeader NO-LOCK.
        END.                            
        RUN pCalculateWeightsAndSizes(bf-estCostHeader.estCostHeaderID).
        RUN pProcessPacking(BUFFER bf-estCostHeader).
        RUN pProcessEstMaterial(BUFFER bf-estCostHeader).
        RUN pBuildFreightForBoardCost(bf-estCostHeader.estCostHeaderID).
        RUN pBuildEstHandlingCharges(bf-estCostHeader.estCostHeaderID).
        RUN pBuildFactoryCostDetails(bf-estCostHeader.estCostHeaderID).
        RUN pBuildNonFactoryCostDetails(bf-estCostHeader.estCostHeaderID).
        RUN pBuildFreightCostDetails(bf-estCostHeader.estCostHeaderID).
        RUN pBuildPriceRelatedCostDetails(bf-estCostHeader.estCostHeaderID).
        RUN pBuildCostSummary(bf-estCostHeader.estCostHeaderID).
        RUN pCopyHeaderCostsToSetItem(BUFFER bf-estCostHeader).
        RUN pBuildProbe(BUFFER bf-estCostHeader).
        
    END. /*each bf-estCostHeader*/

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
    DEFINE BUFFER bf-ce-ctrl       FOR ce-ctrl.
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-ttEstCostMisc   FOR ttEstCostMisc.
    
    DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCostStorage  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostHandling AS DECIMAL NO-UNDO.
    
    FOR EACH bf-estCostHeader EXCLUSIVE-LOCK
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID, 
        FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ bf-estCostHeader.company
        AND bf-ce-ctrl.loc EQ bf-estCostHeader.warehouseID:
            
        
        IF bf-estCostHeader.directMaterialPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfMatMarkup", "Direct Material Markup", ttEstCostForm.costTotalMaterial * bf-estCostHeader.directMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        DO iIndex = 1 TO 6:
            bf-estCostHeader.gsaMaterialPct = bf-ce-ctrl.mat-pct[iIndex] / 100.
            IF bf-ce-ctrl.mat-cost[iIndex] GT ttEstCostForm.costTotalMaterial THEN LEAVE.
        END. 
        IF bf-estCostHeader.gsaMaterialPct NE 0 THEN 
        DO:
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSAMat", "GSA Material", (ttEstCostForm.costTotalMaterial - ttEstCostForm.costTotalBoard) * bf-estCostHeader.gsaMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSABoard", "GSA Board", ttEstCostForm.costTotalBoard * bf-estCostHeader.gsaMaterialPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        END.
        DO iIndex = 1 TO 6:
            bf-estCostHeader.gsaLaborPct = bf-ce-ctrl.lab-pct[iIndex] / 100.
            IF bf-ce-ctrl.lab-cost[iIndex] GT ttEstCostForm.costTotalLabor THEN LEAVE.
        END. 
        IF bf-estCostHeader.gsaLaborPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSALab", "GSA Labor", ttEstCostForm.costTotalLabor * bf-estCostHeader.gsaLaborPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        
        IF bf-estCostHeader.warehouseMarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", ttEstCostForm.costTotalFactory * bf-estCostHeader.warehouseMarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        
        FOR EACH estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ttEstCostForm.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ttEstCostForm.estCostFormID:
            RUN GetStorageAndHandlingForEstimateBlank(estCostBlank.company, estCostBlank.estimateNo, estCostBlank.quantityRequired, 
                estCostBlank.formNo, estCostBlank.blankNo,
                OUTPUT dCostStorage, OUTPUT dCostHandling).
            IF dCostStorage NE 0 THEN 
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, estCostBlank.estCostBlankID, estCostBlank.estCostBlankID, 
                    gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", dCostStorage, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
            IF dCostHandling NE 0 THEN 
                RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, estCostBlank.estCostBlankID, estCostBlank.estCostBlankID, 
                    gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", dCostHandling, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        END.
        
        /*Note - currently a defect with the Folding % is zeroed out during the calc process - this is fix*/
        IF bf-estCostHeader.foldPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfFolding", "Folding", ttEstCostForm.costTotalFactory * bf-estCostHeader.foldPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).      
        
                                
        IF bf-estCostHeader.special1MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", ttEstCostForm.costTotalFactory * bf-estCostHeader.special1MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).    
        IF bf-estCostHeader.special2MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", ttEstCostForm.costTotalFactory * bf-estCostHeader.special2MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        IF bf-estCostHeader.special3MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", ttEstCostForm.costTotalFactory * bf-estCostHeader.special3MarkupPct, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", bf-estCostHeader.special1FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).    
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", bf-estCostHeader.special2FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstCostForm.estCostHeaderID, ttEstCostForm.estCostFormID, "", ttEstCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", bf-estCostHeader.special3FlatValue, 0, ttEstCostForm.company, ttEstCostForm.estimateNo, BUFFER bf-ttEstCostDetail).                                                                                                   
    END. /*Each bf-estCostHeader*/    
    RUN pCalcCostTotals(ipiEstCostHeaderID, 0, NO).
    RELEASE bf-estCostHeader.
    
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
    FOR EACH estCostMaterial NO-LOCK 
        WHERE estCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMaterial(BUFFER estCostMaterial).                  
                    
    END. /*Each estCostMaterial for estHeader*/
    
    FOR EACH ttEstCostMisc NO-LOCK 
        WHERE ttEstCostMisc.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMisc(BUFFER ttEstCostMisc).                  
    END. /*Each estCostMaterial for estHeader*/
    RUN pCalcCostTotals(ipiEstCostHeaderID, 0, NO).
    
END PROCEDURE.

PROCEDURE pBuildPackingForEb PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all packing infor for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank  FOR estCostBlank.
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
        WHERE bf-ce-ctrl.company EQ ipbf-estCostheader.company
        NO-ERROR.
    
    IF ipbf-estCostHeader.isUnitizedSet AND ipbf-estCostBlank.formNo NE 0 THEN 
        RETURN.   /*Ignore non-form 0 packing for unitized set*/
    
    /*Case*/
    RUN pAddPacking(BUFFER ipbf-estCostBlank, ipbf-eb.cas-no, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-estCostBlank, ipbf-eb.tr-no, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-estCostBlank, ipbf-eb.layer-pad, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-estCostBlank, ipbf-eb.divider, BUFFER bf-ttPack).
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
        RUN pAddPacking(BUFFER ipbf-estCostBlank, cStrapID, BUFFER bf-ttPack).
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
        
        RUN pAddPacking(BUFFER ipbf-estCostBlank, estPacking.rmItemID, BUFFER bf-ttPack).
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank  FOR estCostBlank.
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    IF ipbf-estCostHeader.industry EQ gcIndustryFolding THEN
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code2):
        IF ipbf-eb.i-code2[iIndex] GT "" THEN
            RUN pAddInk(BUFFER ipbf-estCostBlank, ipbf-eb.i-ps2[iIndex], ipbf-eb.i-code2[iIndex], ipbf-eb.i-dscr2[iIndex], ipbf-eb.i-%2[iIndex], ipbf-eb.inkNoCharge).
    END.
    ELSE
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code):
        IF ipbf-eb.i-code[iIndex] GT "" THEN    
            RUN pAddInk(BUFFER ipbf-estCostBlank, ipbf-eb.i-ps[iIndex], ipbf-eb.i-code[iIndex], ipbf-eb.i-dscr[iIndex], ipbf-eb.i-%[iIndex], ipbf-eb.inkNoCharge).
    END.

END PROCEDURE.

PROCEDURE pBuildLeafForEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all inks for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    
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
            RUN pAddLeaf(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, bf-est-flm.i-no, bf-est-flm.dscr, bf-est-flm.bnum, iIndex, bf-est-flm.len, bf-est-flm.wid).
        END.    
    END.
    ELSE 
    DO iIndex = 1 TO 4:
        IF ipbf-ef.leaf[iIndex] NE "" THEN
            RUN pAddLeaf(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.leaf[iIndex], ipbf-ef.leaf-dscr[iIndex], ipbf-ef.leaf-bnum[iIndex], iIndex, ipbf-ef.leaf-l[iIndex], ipbf-ef.leaf-w[iIndex]).
    END.
    

END PROCEDURE.

PROCEDURE pBuildItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.

    DEFINE           BUFFER bf-estCostItem     FOR estCostItem.
    DEFINE           BUFFER bf-ttEstCostForm   FOR ttEstCostForm.
    DEFINE           BUFFER bf-estCostBlank    FOR estCostBlank.
    
    DEFINE VARIABLE iEstItemIDSetHeader AS INT64 NO-UNDO.
    
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-estCostHeader.company
        AND eb.est-no EQ ipbf-estCostHeader.estimateNo
        BY eb.form-no DESCENDING: 
        FIND FIRST bf-estCostItem EXCLUSIVE-LOCK
            WHERE bf-estCostItem.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bf-estCostItem.customerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-estCostItem OR eb.form-no EQ 0 THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-estCostHeader, BUFFER bf-estCostItem).
            IF eb.form-no EQ 0 THEN 
            DO:
                RUN pAddEstForm(BUFFER ipbf-estCostHeader, 0, BUFFER bf-ttEstCostForm).
                RUN pAddEstBlank(BUFFER eb, BUFFER ipbf-estCostHeader, BUFFER bf-ttEstCostForm, BUFFER bf-estCostBlank).
                
                IF AVAILABLE bf-ttEstCostForm THEN 
                    bf-ttEstCostForm.quantityFGOnForm = ipbf-estCostHeader.quantityMaster.
                IF eb.pur-man THEN /*Refactor - this should be .unitized*/
                DO:
                    FIND CURRENT ipbf-estCostHeader EXCLUSIVE-LOCK.
                    ipbf-estCostHeader.isUnitizedSet = YES.
                    FIND CURRENT ipbf-estCostHeader NO-LOCK. 
                    RUN pBuildPackingForEb(BUFFER ipbf-estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                END.
                ASSIGN 
                    bf-estCostItem.isSet              = YES
                    bf-estCostItem.quantityPerSet     = 0
                    bf-estCostItem.blankArea          = 0
                    bf-estCostItem.blankAreaNetWindow = 0
                    bf-estCostItem.blankAreaWindow    = 0
                    iEstItemIDSetHeader               = bf-estCostItem.estCostItemID
                    .
                RELEASE bf-ttEstCostForm.
                RELEASE bf-estCostBlank.
            END.     
            ELSE 
                bf-estCostItem.estCostItemIDParent = iEstItemIDSetHeader.           
        END. /*Create estCostItem*/
        ELSE 
            ASSIGN 
                bf-estCostItem.quantityRequired = bf-estCostItem.quantityRequired + eb.bl-qty
                bf-estCostItem.quantityYielded  = bf-estCostItem.quantityYielded + eb.yld-qty
                bf-estCostItem.quantityPerSet   = IF fIsSetType(ipbf-estCostHeader.estType) THEN bf-estCostItem.quantityPerSet + fGetQuantityPerSet(BUFFER eb) ELSE 1.
            .      
        RELEASE bf-estCostItem.
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pCalcBlankPct PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculates the share of that the blank will have on the form to 
     proportinately allocate form costs to each blank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-estCostBlank  FOR estCostBlank.
    
    DEFINE VARIABLE dTotalBlankAreaOnForm AS DECIMAL.
    
    IF ipbf-ttEstCostForm.blankArea GT 0 THEN 
        FOR EACH bf-estCostBlank EXCLUSIVE-LOCK 
            WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
            AND bf-estCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID:
   
            bf-estCostBlank.pctOfForm = bf-estCostBlank.blankArea * bf-estCostBlank.numOut / ipbf-ttEstCostForm.blankArea. 
        END. 
    RELEASE bf-estCostBlank.
        
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
    {est\EstimateCostTotals.i &TableName=estCostHeader}
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a item and category buffer, increment the appropriate fields
     on the item for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotals.i &TableName=estCostItem}

END PROCEDURE.

PROCEDURE pCalculateWeightsAndSizes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estCostHeaderID, calculate weight for all blanks, items, forms
     and header based on weight of materials flagged for inclusion
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    DEFINE BUFFER bf-estCostBlank    FOR estCostBlank.
    DEFINE BUFFER bf-estCostItem     FOR estCostItem.
    DEFINE BUFFER bfSet-estCostItem  FOR estCostItem.
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-estCostHeader   FOR estCostHeader.
    
    DEFINE VARIABLE dWeightInDefaultUOM         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBasisWeightInDefaultUOM    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBlankAreaTotalInDefaultUOM AS DECIMAL NO-UNDO.
    
    
    FOR EACH bf-estCostBlank NO-LOCK 
        WHERE bf-estCostBlank.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-estCostItem EXCLUSIVE-LOCK
        WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostBlank.estCostHeaderID
        AND bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID:
        
        dBlankAreaTotalInDefaultUOM = bf-estCostBlank.blankAreaNetWindow / 144000 * bf-estCostBlank.quantityRequired.
        IF bf-estCostBlank.areaUOM NE gcDefaultAreaUOM THEN 
        DO:
            //REFACTOR: convert blankarea
        END.
                
        FOR EACH bf-estCostMaterial NO-LOCK 
            WHERE bf-estCostMaterial.estCostHeaderID EQ bf-estCostBlank.estCostHeaderID
            AND bf-estCostMaterial.estCostFormID EQ bf-estCostBlank.estCostFormID
            AND (bf-estCostMaterial.addToWeightNet OR bf-estCostMaterial.addToWeightTare)
            AND (bf-estCostMaterial.estCostBlankID EQ bf-estCostBlank.estCostBlankID OR bf-estCostMaterial.estCostBlankID EQ 0):
            
            dWeightInDefaultUOM = 0.
            
            IF bf-estCostMaterial.estCostBlankID EQ 0 THEN 
            DO: /*Form level material - calc based on basis-weight and blank area*/
                IF bf-estCostMaterial.isPrimarySubstrate THEN 
                DO:   /*Board and adders*/
                    /*refactor - basis assumed to be LBs/MSF*/
                    dWeightInDefaultUOM = bf-estCostMaterial.basisWeight * dBlankAreaTotalInDefaultUOM.
                    
                END. /*Primary substrate calculations - board and adders*/
                ELSE /*non-board/substrate form level material - pro-rate weight*/
                    dWeightInDefaultUOM = bf-estCostMaterial.weightTotal * bf-estCostBlank.pctOfForm.
            END.
            ELSE /*Blank specific material - get all weight*/
                dWeightInDefaultUOM = bf-estCostMaterial.weightTotal.

            IF bf-estCostMaterial.weightUOM NE gcDefaultWeightUOM THEN 
            DO:
                    //REFACTOR: Convert to default weight UOM
            END.
            IF bf-estCostMaterial.addToWeightNet OR bf-estCostMaterial.addToWeightTare THEN 
            DO:
                IF bf-estCostMaterial.addToWeightNet THEN 
                    bf-estCostItem.weightNet   = bf-estCostItem.weightNet + dWeightInDefaultUOM.
                IF bf-estCostMaterial.addToWeightTare THEN 
                    bf-estCostItem.weightTare  = bf-estCostItem.weightTare + dWeightInDefaultUOM.
                bf-estCostItem.weightTotal = bf-estCostItem.weightTotal + dWeightInDefaultUOM.
            END.
        END.  /*Each material for blank*/       
    END. /*each blank for header*/
    
    /*Calculate Header Weight*/
    FOR FIRST bf-estCostHeader EXCLUSIVE-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH bf-estCostItem EXCLUSIVE-LOCK 
        WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:

        ASSIGN 
            bf-estCostHeader.weightTotal = bf-estCostHeader.weightTotal + bf-estCostItem.weightTotal
            bf-estCostHeader.weightNet   = bf-estCostHeader.weightNet + bf-estCostItem.weightNet
            bf-estCostHeader.weightTare  = bf-estCostHeader.weightTare + bf-estCostItem.weightTare
            .
    END.
    /*Apply Header Weight to Set Header Item*/
    FOR FIRST bf-estCostHeader NO-LOCK
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        AND bf-estCostHeader.isUnitizedSet,
        FIRST bf-estCostItem EXCLUSIVE-LOCK 
        WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
        AND bf-estCostItem.isSet:

        ASSIGN 
            bf-estCostItem.weightTotal = bf-estCostHeader.weightTotal
            bf-estCostItem.weightNet   = bf-estCostHeader.weightNet
            bf-estCostItem.weightTare  = bf-estCostHeader.weightTare
            .
    END. 
    /*Calc Total MSF for a Set*/
    FOR FIRST bf-estCostHeader NO-LOCK
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        AND bf-estCostHeader.isUnitizedSet,
        FIRST bfSet-estCostItem EXCLUSIVE-LOCK 
        WHERE bfSet-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
        AND bfSet-estCostItem.isSet,
        EACH bf-estCostItem NO-LOCK
        WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHEader.estCostHeaderID:
        ASSIGN 
            bfSet-estCostItem.blankArea          = bfSet-estCostItem.blankArea + bf-estCostItem.blankArea * bf-estCostItem.quantityPerSet
            bfSet-estCostItem.blankAreaNetWindow = bfSet-estCostItem.blankAreaNetWindow + bf-estCostItem.blankAreaNetWindow * bf-estCostItem.quantityPerSet
            bfSet-estCostItem.blankAreaWindow    = bfSet-estCostItem.blankAreaWindow + bf-estCostItem.blankAreaWindow * bf-estCostItem.quantityPerSet
            .
    END.
    RELEASE bfSet-estCostItem.
    RELEASE bf-estCostItem.
    RELEASE bf-estCostHeader.
    
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
    IF lError THEN
        RUN custom/convquom.p (ipcCompany, ipcFromUOM, ipcToUOM, ipdBasisWeightInLbsPerMSF, ipdLength, ipdWidth, ipdDepth, ipdQuantityInFromUOM, OUTPUT opdQuantityInToUOM).
    
END PROCEDURE.

PROCEDURE pCopyHeaderCostsToSetItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a Cost Header, transfers the header costs to the 
     Set Header Item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.

    DEFINE           BUFFER bf-estCostBlank    FOR estCostBlank. 
    DEFINE           BUFFER bf-estCostItem     FOR estCostItem.

    IF fIsSetType(ipbf-estCostHeader.estType) THEN 
    DO:
        FOR EACH bf-estCostBlank NO-LOCK
            WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bf-estCostBlank.formNo EQ 0
            AND bf-estCostBlank.blankNo EQ 0,
            FIRST bf-estCostItem EXCLUSIVE-LOCK
            WHERE bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID:
            ASSIGN 
                bf-estCostItem.costTotalBoard            = ipbf-estCostHeader.costTotalBoard
                bf-estCostItem.costTotalFactory          = ipbf-estCostheader.costTotalFactory
                bf-estCostItem.costTotalFixedOverhead    = ipbf-estCostHeader.costTotalFixedOverhead
                bf-estCostItem.costTotalFull             = ipbf-estCostHeader.costTotalFull
                bf-estCostItem.costTotalGroupLevel       = ipbf-estCostHeader.costTotalGroupLevel
                bf-estCostItem.costTotalLabor            = ipbf-estCostHeader.costTotalLabor
                bf-estCostItem.costTotalMaterial         = ipbf-estCostHeader.costTotalMaterial
                bf-estCostItem.costTotalNonFactory       = ipbf-estCostHeader.costTotalNonFactory
                bf-estCostItem.costTotalVariableOverhead = ipbf-estCostHeader.costTotalVariableOverhead
                bf-estCostItem.profitPctGross            = ipbf-estCostHeader.profitPctGross
                bf-estCostItem.profitPctNet              = ipbf-estCostHeader.profitPctNet
                bf-estCostItem.sellPrice                 = ipbf-estCostHeader.sellPrice
                .
        END.
    END.
    RELEASE bf-estCostItem.

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
    DEFINE PARAMETER BUFFER ipbf-ttEstCostCategory FOR ttEstCostCategory.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm     FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader     FOR estCostHeader.
    
    RUN pCalcCostTotalsHeader(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
    RUN pCalcCostTotalsForm(BUFFER ipbf-ttEstCostForm, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
        
    FIND FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ipbf-ttEstCostDetail.estCostHeaderID
        AND estCostBlank.estCostFormID EQ ipbf-ttEstCostDetail.estCostFormID
        AND estCostBlank.estCostBlankID EQ ipbf-ttEstCostDetail.estCostBlankID
        NO-ERROR.
    IF AVAILABLE estCostBlank THEN 
    DO:  /*Blank-specific cost*/
        FIND FIRST estCostItem NO-LOCK 
            WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
            AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID
            NO-ERROR.
        IF AVAILABLE estCostItem THEN 
            RUN pCalcCostTotalsItem(BUFFER estCostItem, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal).
    END.
    ELSE /*Divide up the Form-level Costs into each item*/
        FOR EACH estCostBlank NO-LOCK
            WHERE estCostBlank.estCostHeaderID EQ ipbf-ttEstCostDetail.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ipbf-ttEstCostDetail.estCostFormID, 
            FIRST estCostItem NO-LOCK  
            WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
            AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID :
            RUN pCalcCostTotalsItem(BUFFER estCostItem, BUFFER ipbf-ttEstCostCategory, ipbf-ttEstCostDetail.costTotal * estCostBlank.pctOfForm).
        END.

END PROCEDURE.

PROCEDURE pGetPriceProfitAndCommissionForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and blank, return the target margin percentage
     Notes: Should replace "CalcSellPrice.p and markup.p"
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostItem   FOR estCostItem.
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
    RUN est/GetMarkup.p (ipbf-estCostItem.company, 
        ipbf-estCostItem.customerID, ipbf-estCostItem.productCategory, ipbf-estCostItem.styleID,
        dLookup, dBoardPct, INPUT-OUTPUT dMargin, INPUT-OUTPUT cMarginOn).
    IF cMarginOn EQ "" THEN cMarginOn = "G".
    
    IF dMargin EQ 0 THEN 
    DO: 
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ ipbf-estCostItem.company
            AND cust.cust-no EQ ipbf-estCostItem.customerID
            NO-ERROR.
        IF AVAILABLE cust THEN 
            ASSIGN 
                dMargin   = cust.markup
                cMarginOn = "N"
                .
    END.
    IF dMargin EQ 0 THEN 
    DO:
        FIND FIRST estCostHeader NO-LOCK 
            WHERE estCostHeader.company EQ ipbf-estCostItem.company
            AND estCostHeader.estCostHeaderID EQ ipbf-estCostItem.estCostHeaderID
            NO-ERROR.
        IF AVAILABLE estCostHeader THEN 
            ASSIGN 
                dMargin   = estCostHeader.marginPct
                cMarginOn = estCostHeader.marginOn
                .
    END.
    dCommPct = ipbf-estCostItem.commissionPct.
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

PROCEDURE pProcessAdders PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for adders
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcAdders LIKE ef.adder NO-UNDO.

    DEFINE BUFFER bf-estCostMaterial      FOR estCostMaterial.
    DEFINE BUFFER bf-item                 FOR ITEM.
    DEFINE BUFFER bfBoard-estCostMaterial FOR estCostMaterial.
    
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
                RUN pAddError("Adder '" + cAdder + "' is not valid", gcErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
                RETURN.
            END.
            IF NOT CAN-DO(gcAdderMatTypes,bf-item.mat-type) THEN 
            DO:
                RUN pAddError("Adder '" + cAdder + "' is valid material but not a material type of " + gcAdderMatTypes, gcErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
                RETURN.
            END.      
            RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, cAdder, 0, BUFFER bf-estCostMaterial).
            ASSIGN 
                bf-estCostMaterial.isPrimarySubstrate         = NO
                bf-estCostMaterial.addToWeightNet             = YES
                bf-estCostMaterial.addToWeightTare            = NO
                                       
                bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
                bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
                bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
                bf-estCostMaterial.quantityUOMWaste           = "EA"
                bf-estCostMaterial.quantityUOM                = "EA"
                bf-estCostMaterial.basisWeight                = bf-item.basis-w
                bf-estCostMaterial.dimWidth                   = ipbf-ttEstCostForm.grossWidth
                bf-estCostMaterial.dimLength                  = ipbf-ttEstCostForm.grossLength
                bf-estCostMaterial.dimDepth                   = ipbf-ttEstCostForm.grossDepth
                bf-estCostMaterial.noCharge                   = ipbf-ttEstCostForm.noCost
                .
            FIND FIRST bfBoard-estCostMaterial NO-LOCK 
                WHERE bfBoard-estCostMaterial.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
                AND bfBoard-estCostMaterial.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
                AND bfBoard-estCostMaterial.isPrimarySubstrate
                NO-ERROR.
            IF AVAILABLE bfBoard-estCostMaterial THEN 
                bf-estCostMaterial.vendorID = bfBoard-estCostMaterial.vendorID.
            
            RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).   
    
        END.
    END.

END PROCEDURE.

PROCEDURE pProcessBoardBOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader      FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-item-bom           FOR item-bom.
    DEFINE OUTPUT PARAMETER oplValidBom             AS LOGICAL NO-UNDO.
    
    DEFINE           BUFFER bf-estCostMaterial      FOR estCostMaterial.
    DEFINE           BUFFER bfBoard-estCostMaterial FOR estCostMaterial.
    DEFINE           BUFFER bf-item                 FOR ITEM.
    
    DEFINE VARIABLE dShrinkPct AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipbf-item-bom.i-no
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Component '" + ipbf-item-bom.i-no + "' is not valid", gcErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
        RETURN.
    END.
    oplValidBom = YES.
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-item-bom.i-no, 0, BUFFER bf-estCostMaterial).
    ASSIGN 
        bf-estCostMaterial.isPrimarySubstrate         = YES
        bf-estCostMaterial.addToWeightNet             = YES
        bf-estCostMaterial.addToWeightTare            = NO
                                       
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
        bf-estCostMaterial.quantityUOMWaste           = "EA"
        bf-estCostMaterial.quantityUOM                = "EA"
        bf-estCostMaterial.basisWeight                = bf-item.basis-w
        dShrinkPct                                    = (IF ipbf-item-bom.shrink NE 100 THEN ipbf-item-bom.shrink ELSE 0) / 100
        bf-estCostMaterial.dimWidth                   = ipbf-ttEstCostForm.grossWidth
        bf-estCostMaterial.dimLength                  = ipbf-ttEstCostForm.grossLength / ( 1 - dShrinkPct)
        bf-estCostMaterial.dimDepth                   = ipbf-ttEstCostForm.grossDepth
        bf-estCostMaterial.noCharge                   = ipbf-ttEstCostForm.noCost
        .
    FIND FIRST bfBoard-estCostMaterial NO-LOCK 
        WHERE bfBoard-estCostMaterial.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        AND bfBoard-estCostMaterial.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND bfBoard-estCostMaterial.isPrimarySubstrate
        NO-ERROR.
    IF AVAILABLE bfBoard-estCostMaterial THEN 
        bf-estCostMaterial.vendorID = bfBoard-estCostMaterial.vendorID.
            
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).   
    
END PROCEDURE.

PROCEDURE pProcessBoardBOMAdhesive PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds the adhesive based on the pre-calculated square inches
     of total bom layers.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdAreaInSqIn AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-item            FOR ITEM.
    
    DEFINE VARIABLE dCoverageRateInSqInPerLb AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Adhesive '" + ipcItemID + "' is not valid", gcErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
        RETURN.
    END.
    
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipcItemID, 0, BUFFER bf-estCostMaterial).
    ASSIGN 
        bf-estCostMaterial.isPrimarySubstrate = NO
        bf-estCostMaterial.addToWeightNet     = YES
        bf-estCostMaterial.addToWeightTare    = NO
        dCoverageRateInSqInPerLb              = bf-item.sqin-lb                                
        bf-estCostMaterial.quantityUOMWaste   = "LB"
        bf-estCostMaterial.quantityUOM        = "LB"
        bf-estCostMaterial.basisWeight        = bf-item.basis-w
        bf-estCostMaterial.noCharge           = ipbf-ttEstCostForm.noCost
        .
    IF dCoverageRateInSqInPerLb GT 0 THEN 
        ASSIGN 
            bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste * ipdAreaInSqIn / dCoverageRateInSqInPerLb
            .    
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessBoardBOMLaminate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds the laminate if there is BOM based on total
     Gross Sq Ins of the Sheet
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-item            FOR ITEM.
    
    DEFINE VARIABLE dCoverageRateInSqInPerLb AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dGrossSheetAreaInSqIn    AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstCostForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("BOM Laminate '" + ipcItemID + "' is not valid", gcErrorWarning, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
        RETURN.
    END.
    
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipcItemID, 0, BUFFER bf-estCostMaterial).
    ASSIGN 
        bf-estCostMaterial.isPrimarySubstrate = NO
        bf-estCostMaterial.addToWeightNet     = YES
        bf-estCostMaterial.addToWeightTare    = NO
        dCoverageRateInSqInPerLb              = bf-item.sqin-lb                                
        bf-estCostMaterial.quantityUOMWaste   = "LB"
        bf-estCostMaterial.quantityUOM        = "LB"
        bf-estCostMaterial.basisWeight        = bf-item.basis-w
        bf-estCostMaterial.noCharge           = ipbf-ttEstCostForm.noCost
        dGrossSheetAreaInSqIn                 = ipbf-ttEstCostForm.grossWidth * ipbf-ttEstCostForm.grossLength
        .
    IF dCoverageRateInSqInPerLb GT 0 THEN 
        ASSIGN 
            bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste * dGrossSheetAreaInSqIn / dCoverageRateInSqInPerLb
            .    
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessEstMaterial PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Processes Additional Materials listing in Misc / Sub tab for 
     given form 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    
    DEFINE BUFFER bf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE BUFFER bf-estMaterial        FOR estMaterial.
    DEFINE BUFFER bf-estCostMaterial    FOR estCostMaterial.
    DEFINE BUFFER bf-estCostBlank       FOR estCostBlank.
    DEFINE BUFFER bfUnitize-estCostForm FOR ttEstCostForm.
    
    DEFINE VARIABLE dSqFtPerEA AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCases AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPallets AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQuantityInEA AS INTEGER NO-UNDO.
    DEFINE VARIABLE iMultiplier AS INTEGER NO-UNDO.

    IF ipbf-estCostHeader.isUnitizedSet  THEN 
    DO:
        /*Establish unitization form (Form 1)*/
        FIND FIRST bfUnitize-estCostForm NO-LOCK 
            WHERE bfUnitize-estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bfUnitize-estCostForm.formNo EQ 1
            NO-ERROR.
    END.
    
    FOR EACH bf-estMaterial NO-LOCK 
        WHERE bf-estMaterial.company EQ ipbf-estCostHeader.company
        AND bf-estMaterial.estimateNo EQ ipbf-estCostHeader.estimateNo,
        FIRST bf-ttEstCostForm NO-LOCK 
            WHERE bf-ttEstCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostheaderID
            AND bf-ttEstCostForm.formNo EQ bf-estMaterial.formNo:
        ASSIGN 
            iCases = 0
            iPallets = 0
            iQuantityInEA = 0
            .
        IF bf-estMaterial.blankNo NE 0 THEN
            FIND FIRST bf-estCostBlank NO-LOCK
                WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
                AND bf-estCostBlank.formNo EQ bf-ttEstCostForm.formNo
                AND bf-estCostBlank.blankNo EQ bf-estMaterial.blankNo
                NO-ERROR.
        IF AVAILABLE bf-estCostBlank THEN DO: 
            RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-ttEstCostForm, bf-estMaterial.itemID, bf-estCostBlank.estCostBlankID, BUFFER bf-estCostMaterial).
            ASSIGN 
                iCases = bf-estCostBlank.quantityOfSubUnits
                iPallets = bf-estCostBlank.quantityOfUnits
                iQuantityInEA = bf-estCostBlank.quantityRequired
                .
        END.
        ELSE 
        DO:
            RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-ttEstCostForm, bf-estMaterial.itemID, 0, BUFFER bf-estCostMaterial).
            FOR EACH bf-estCostBlank NO-LOCK
                WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
                AND bf-estCostBlank.formNo EQ bf-ttEstCostForm.formNo:
                ASSIGN 
                    iCases = iCases + bf-estCostBlank.quantityOfSubUnits
                    iPallets = iPallets + bf-estCostBlank.quantityOfUnits
                    iQuantityInEA = iQuantityInEA + bf-estCostBlank.quantityRequired. 
            END.            
        END.
        ASSIGN  
            bf-estCostMaterial.addToWeightNET          = YES 
            bf-estCostMaterial.quantityUOM             = bf-estMaterial.quantityUOM
            bf-estCostMaterial.dimDepth                = bf-estMaterial.dimDepth
            bf-estCostMaterial.dimWidth                = bf-estMaterial.dimWidth
            bf-estCostMaterial.dimLength               = bf-estMaterial.dimLength
            bf-estCostMaterial.dimUOM                  = "IN"  //Refactor - add dimUOM to estMaterial 
            bf-estCostMaterial.weightUOM               = bf-estMaterial.weightUOM
            bf-estCostMaterial.costOverridePerUOM      = bf-estMaterial.costOverridePerUOM
            bf-estCostMaterial.noCharge                = bf-estMaterial.noCharge
            dSqFtPerEa                                 = DYNAMIC-FUNCTION("Conv_GetSqft",bf-estCostMaterial.dimLength, bf-estCostMaterial.dimWidth, bf-estCostMaterial.dimUOM).
            
            .            
        IF dSqFtPerEA NE 0 AND bf-estMaterial.weightPerEA NE 0 THEN 
            bf-estCostMaterial.basisWeight = bf-estMaterial.weightPerEA / (dSqFtPerEA / 1000).
            
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
                iMultiplier = ipbf-estCostHeader.quantityMaster.
        END CASE.
        ASSIGN 
            bf-estCostMaterial.quantityRequiredNoWaste = bf-estMaterial.quantity * iMultiplier
            bf-estCostMaterial.quantityRequiredSetupWaste = (bf-estMaterial.wastePercent / 100) * bf-estCostMaterial.quantityRequiredNoWaste
            bf-estCostMaterial.weightTotal = bf-estMaterial.weightPerEA * bf-estCostMaterial.quantityRequiredNoWaste
            .
        IF bf-ttEstCostForm.formNo EQ 0 AND AVAILABLE bfUnitize-estCostForm THEN 
        DO:
            /*Associate Form 0 materials to the unitize form (Form 1)*/
            ASSIGN 
                bf-estCostMaterial.estCostFormID  = bfUnitize-estCostForm.estCostFormID
                bf-estCostMaterial.estCostBlankID = 0
                .
            RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER bfUnitize-estCostForm).
        END. 
        ELSE     
            RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER bf-ttEstCostForm).
        
    END.
    
END PROCEDURE.    

PROCEDURE pProcessMaterialTypeCalc PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  For special material type calculations (non default)
 Notes:  Will process the estCostMaterial buffer accordingly.
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
    DEFINE INPUT PARAMETER ipcCalculationType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostBlank FOR estCostBlank.
    DEFINE BUFFER bf-estCostItem FOR estCostItem.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    DEFINE VARIABLE dTotWeightPerForm AS DECIMAL NO-UNDO.
    
    CASE ipcCalculationType:
        WHEN "ByFGWeight" THEN DO:
            FOR EACH bf-estCostBlank NO-LOCK 
                WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-estCostMaterial.estCostHeaderID
                AND bf-estCostBlank.estCostFormID EQ ipbf-estCostMaterial.estCostFormID,
                FIRST bf-estCostItem NO-LOCK
                WHERE bf-estCostItem.estCostHeaderID EQ ipbf-estCostMaterial.estCostHeaderID
                  AND bf-estCostItem.estCostItemID EQ bf-estCostBlank.estCostItemID,
                FIRST bf-itemfg NO-LOCK 
                WHERE bf-itemfg.company EQ bf-estCostItem.company
                AND bf-itemfg.i-no EQ bf-estCostItem.itemID:
                dTotWeightPerForm = dTotWeightPerForm + bf-itemfg.weightPerEA * bf-estCostBlank.numOut.
            END.        
            IF dTotWeightPerForm GT 0 THEN 
                ASSIGN 
                    ipbf-estCostMaterial.quantityRequiredNoWaste = ipbf-estCostMaterial.quantityRequiredNoWaste * dTotWeightPerForm
                    ipbf-estCostMaterial.quantityRequiredRunWaste = ipbf-estCostMaterial.quantityRequiredRunWaste * dTotWeightPerForm
                    ipbf-estCostMaterial.quantityRequiredSetupWaste = ipbf-estCostMaterial.quantityRequiredSetupWaste * dTotWeightPerForm
                    ipbf-estCostMaterial.quantityUOM = "LB"
                    ipbf-estCostMaterial.quantityUOMWaste = "LB"
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
    
    FOR EACH est-prep NO-LOCK 
        WHERE est-prep.company EQ ipbf-ef.company
        AND est-prep.est-no EQ ipbf-ef.est-no
        AND est-prep.s-num EQ ipbf-ef.form-no
        AND est-prep.code NE "":
        RUN pAddEstMiscForPrep(BUFFER est-prep, BUFFER ipbf-ttEstCostForm).
    END.    

END PROCEDURE.

PROCEDURE pProcessOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a estCostHeader, build all estCostOperations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader  FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm  FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE VARIABLE dQtyFormsRequiredForBlanks    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyFormsRequiredForBlanksMax AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOut                     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget                    AS DECIMAL NO-UNDO.
    

    
    /*Get the effective Est-op quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-estCostHeader.company 
        AND est-op.est-no  EQ ipbf-estCostHeader.estimateNo 
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
            IF est-op.qty GE ipbf-estCostHeader.quantityMaster THEN LEAVE.
        END.
    END.
    EMPTY TEMP-TABLE ttEstBlank.
    
    FOR EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        AND estCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID:
            
            
        IF NOT CAN-FIND (FIRST ttEstBlank
            WHERE ttEstBlank.estCostBlankID EQ estCostBlank.estCostBlankID) THEN
        DO:
            CREATE ttEstBlank.
            ASSIGN 
                ttEstBlank.estCostBlankID   = estCostBlank.estCostBlankID
                ttEstBlank.estCostFormID    = estCostBlank.estCostFormID
                ttEstBlank.dQtyInOut        = IF estCostBlank.priceBasedOnYield THEN estCostBlank.quantityYielded ELSE estCostBlank.quantityRequired
                ttEstBlank.iOut             = estCostBlank.numOut
                .
        END.
        
    END.
    
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-estCostHeader.company
        AND est-op.est-no EQ ipbf-estCostHeader.estimateNo
        AND est-op.s-num EQ ipbf-ttEstCostForm.formNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:

    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation).                    
    IF AVAILABLE bf-ttEstCostOperation THEN 
    DO:
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
            
            RUN pProcessOperation(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation, INPUT-OUTPUT ttEstBlank.dQtyInOut, 
                INPUT-OUTPUT ttEstBlank.dQtyInOutSetupWaste, INPUT-OUTPUT ttEstBlank.dQtyInOutRunWaste).
        END. /*BlankNo not 0*/
        ELSE 
        DO:                  
            IF bf-ttEstCostOperation.isBlankMaker THEN 
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
            RUN pProcessOperation(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-ttEstCostOperation, INPUT-OUTPUT dQtyInOut, 
                INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
                
        END.
        RUN pCalcEstOperation(BUFFER ipbf-estCostHeader, BUFFER bf-ttEstCostOperation, BUFFER ipbf-ttEstCostForm).                    
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

    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE           BUFFER bf-item            FOR ITEM.
    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE           BUFFER bf-estCostBlank    FOR estCostBlank.
    
    DEFINE VARIABLE iIndex          AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEstCostBlankID AS INT64   NO-UNDO.
    
    DO iIndex = 1 TO 8:
        IF ipbf-ef.spec-no[iIndex] NE "" THEN 
        DO:
            FIND FIRST bf-estCostBlank NO-LOCK 
                WHERE bf-estCostBlank.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
                AND bf-estCostBlank.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
                AND bf-estCostBlank.blankNo EQ 1  /*REFACTOR - What is blank number???*/
                NO-ERROR.
            IF AVAILABLE bf-estCostBlank THEN 
                iEstCostBlankID = bf-estCostBlank.estCostBlankID.
            RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.spec-no[iIndex], iEstCostBlankID, BUFFER bf-estCostMaterial).
            IF AVAILABLE bf-estCostMaterial THEN 
            DO: 
                
                /*REFACTOR - ugly.  work around to support more than 2 decimals as stored in db*/
                RUN custom/extradec.p (.0001, ipbf-ef.spec-qty[iIndex] * ipbf-ttEstCostForm.quantityFGOnForm,
                    OUTPUT bf-estCostMaterial.quantityRequiredNoWaste).
                    
                ASSIGN            
                    bf-estCostMaterial.addToWeightNet = YES
                    bf-estCostMaterial.itemName       = ipbf-ef.spec-dscr[iIndex]
                    bf-estCostMaterial.quantityUOM    = "EA"
                    .
                RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).
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
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.estCostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID,
        FIRST ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ bf-ttEstCostDetail.estCostHeaderID
        AND ttEstCostForm.estCostFormID EQ bf-ttEstCostDetail.estCostFormID, 
        FIRST ttEstCostCategory NO-LOCK 
        WHERE ttEstCostCategory.estCostCategoryID EQ bf-ttEstCostDetail.estCostCategoryID 
        :
        RUN pCalcTotalsForCostDetail(BUFFER bf-ttEstCostDetail, BUFFER ttEstCostCategory, BUFFER ttEstCostForm, BUFFER estCostHeader).
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
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID,
        FIRST ttEstCostForm NO-LOCK
        WHERE ttEstCostForm.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
        AND ttEstCostForm.estCostFormID EQ ttEstCostDetail.estCostFormID, 
        FIRST ttEstCostCategory NO-LOCK 
        WHERE ttEstCostCategory.estCostCategoryID EQ ttEstCostDetail.estCostCategoryID
        :
        RUN pAddCostSummary(estCostHeader.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, estCostHeader.quantityMaster / 1000).
        RUN pAddCostSummary(ttEstCostForm.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, ttEstCostForm.quantityFGOnForm / 1000).
        
        FIND FIRST estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ttEstCostDetail.estCostFormID
            AND estCostBlank.estCostBlankID EQ ttEstCostDetail.estCostBlankID
            NO-ERROR.
        IF AVAILABLE estCostBlank THEN 
        DO:  /*Blank-specific cost*/
            FIND FIRST estCostItem NO-LOCK 
                WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID
                NO-ERROR.
            IF AVAILABLE estCostItem THEN 
                RUN pAddCostSummary(estCostItem.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal, estCostItem.quantityRequired / 1000).
           
        END.
        ELSE /*Divide up the Form-level Costs into each item*/
            FOR EACH estCostBlank NO-LOCK
                WHERE estCostBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
                AND estCostBlank.estCostFormID EQ ttEstCostDetail.estCostFormID, 
                FIRST estCostItem NO-LOCK  
                WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID :
                RUN pAddCostSummary(estCostItem.rec_key, ttEstCostCategory.estCostGroupID, ttEstCostDetail.estCostHeaderID, ttEstCostDetail.costTotal * estCostBlank.pctOfForm, estCostItem.quantityRequired / 1000).
           
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
     Purpose: Given a estCostMaterial buffer, calculate simple calculated fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm   FOR ttEstCostForm.
    
    DEFINE           BUFFER bf-estCostBlank      FOR estCostBlank.
    
    DEFINE VARIABLE dCostDeviation AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInM   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dtotal AS DECIMAL.
    
    RUN pGetRequiredTotal(ipbf-estCostMaterial.quantityRequiredNoWaste,
                          ipbf-estCostMaterial.quantityRequiredSetupWaste,
                          ipbf-estCostMaterial.quantityRequiredRunWaste,
                          ipbf-estCostMaterial.quantityRequiredMinDiff,
                          OUTPUT ipbf-estCostMaterial.quantityRequiredTotal).
            
    IF ipbf-estCostMaterial.costOverridePerUOM EQ 0 THEN 
    DO:
        IF ipbf-estCostMaterial.isPurchasedFG THEN
            RUN pGetEstFarmCosts(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostMaterial, ipbf-estCostMaterial.quantityRequiredTotal, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.vendorID, 
                OUTPUT ipbf-estCostMaterial.costPerUOM, OUTPUT ipbf-estCostMaterial.costUOM,  OUTPUT ipbf-estCostMaterial.costSetup, OUTPUT ipbf-estCostMaterial.vendorID, OUTPUT ipbf-estCostMaterial.costPerUOMDeviation).
        ELSE 
            RUN pGetEstMaterialCosts(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostMaterial,ipbf-estCostMaterial.quantityRequiredTotal,ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.vendorID, 
                OUTPUT ipbf-estCostMaterial.costPerUOM, OUTPUT ipbf-estCostMaterial.costUOM,  OUTPUT ipbf-estCostMaterial.costSetup, OUTPUT ipbf-estCostMaterial.vendorID, OUTPUT ipbf-estCostMaterial.costPerUOMDeviation).
            
    END.
    ELSE 
        ipbf-estCostMaterial.costPerUOM = ipbf-estCostMaterial.costOverridePerUOM.
    
    IF ipbf-estCostMaterial.costUOM EQ "" THEN 
        ipbf-estCostMaterial.costUOM = "EA".
    IF ipbf-estCostMaterial.costUOM NE ipbf-estCostMaterial.quantityUOM THEN 
    DO:
        RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredNoWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredSetupWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredSetupWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredRunWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredRunWasteInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredMinDiff, OUTPUT ipbf-estCostMaterial.quantityRequiredMinDiffInCUOM).
        RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredTotal, OUTPUT ipbf-estCostMaterial.quantityRequiredTotalInCUOM).
    END.
    ELSE 
        ASSIGN 
            ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM    = ipbf-estCostMaterial.quantityRequiredNoWaste
            ipbf-estCostMaterial.quantityRequiredSetupWasteInCUOM = ipbf-estCostMaterial.quantityRequiredSetupWaste
            ipbf-estCostMaterial.quantityRequiredRunWasteInCUOM   = ipbf-estCostMaterial.quantityRequiredRunWaste
            ipbf-estCostMaterial.quantityRequiredMinDiffInCUOM    = ipbf-estCostMaterial.quantityRequiredMinDiff
            ipbf-estCostMaterial.quantityRequiredTotalInCUOM      = ipbf-estCostMaterial.quantityRequiredTotal
            .
    RUN pConvertQuantityFromUOMToUOM(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.weightUOM, 
        ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
        ipbf-estCostMaterial.quantityRequiredNoWaste, OUTPUT ipbf-estCostMaterial.weightTotal).
               
    IF NOT ipbf-estCostMaterial.noCharge THEN 
    DO:
        ASSIGN 
            ipbf-estCostMaterial.costTotalNoWaste    = ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM * ipbf-estCostMaterial.costPerUOM
            ipbf-estCostMaterial.costTotalDeviation  = ipbf-estCostMaterial.quantityRequiredTotalInCUOM * ipbf-estCostMaterial.costPerUOMDeviation
            ipbf-estCostMaterial.costTotalSetupWaste = ipbf-estCostMaterial.quantityRequiredSetupWasteInCUOM * ipbf-estCostMaterial.costPerUOM
            ipbf-estCostMaterial.costTotalRunWaste   = ipbf-estCostMaterial.quantityRequiredRunWasteInCUOM * ipbf-estCostMaterial.costPerUOM
            ipbf-estCostMaterial.costTotalMinDiff    = ipbf-estCostMaterial.quantityRequiredMinDiffInCUOM * ipbf-estCostMaterial.costPerUOM
            ipbf-estCostMaterial.costTotal           = ipbf-estCostMaterial.costTotalNoWaste + ipbf-estCostMaterial.costTotalSetupWaste + 
                                                                 ipbf-estCostMaterial.costTotalRunWaste + ipbf-estCostMaterial.costTotalMinDiff + 
                                                                 ipbf-estCostMaterial.costSetup
            .
        dQuantityInM = 0.
        IF ipbf-estCostMaterial.blankNo NE 0 THEN 
        DO:
            FIND FIRST bf-estCostBlank NO-LOCK 
                WHERE bf-estCostBlank.estCostBlankID EQ ipbf-estCostMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE bf-estCostBlank THEN 
                dQuantityInM = bf-estCostBlank.quantityRequired / 1000.
        END.
        IF dQuantityInM EQ 0 THEN 
            dQuantityInM = ipbf-ttEstCostForm.quantityFGOnForm / 1000. 
        IF dQuantityInM EQ 0 THEN 
            dQuantityInM = 1.
        ASSIGN  
            ipbf-estCostMaterial.costTotalPerMFinished           = ipbf-estCostMaterial.costTotal / dQuantityInM
            ipbf-estCostMaterial.costTotalPerMFinishedNoWaste    = ipbf-estCostMaterial.costTotalNoWaste / dQuantityInM
            ipbf-estCostMaterial.costTotalPerMFinishedSetupWaste = ipbf-estCostMaterial.costTotalSetupWaste  / dQuantityInM
            ipbf-estCostMaterial.costTotalPerMFinishedRunWaste   = ipbf-estCostMaterial.costTotalRunWaste / dQuantityInM
            .
    END.
    ELSE 
        ASSIGN 
            ipbf-estCostMaterial.costPerUOM = 0
            ipbf-estCostMaterial.costSetup  = 0
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
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
     Purpose: for a given form, build the estCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    
    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
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
        RUN pAddError("Board '" + ipbf-ef.board + "' is not valid", gcErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
        RETURN.
    END.
    IF NOT CAN-DO(gcBoardMatTypes,bf-item.mat-type) THEN 
    DO:
        RUN pAddError("Board '" + ipbf-ef.board + "' is valid material but not a material type of " + gcBoardMatTypes, gcErrorImportant, ipbf-ttEstCostForm.estCostHeaderID, ipbf-ttEstCostForm.formNo, 0).
        RETURN.
    END.   
    FOR EACH bf-item-bom NO-LOCK
        WHERE bf-item-bom.company EQ bf-item.company
        AND bf-item-bom.parent-i EQ bf-item.i-no
        AND bf-item-bom.i-no NE ""
        AND bf-item-bom.line# LT 9:    
        RUN pProcessBoardBOM(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER bf-item-bom, OUTPUT lValidBom).
        IF NOT lFoundBom AND lValidBom THEN 
            lFoundBom = YES.
    END.
    IF lFoundBom THEN 
    DO:
        IF ipbf-ef.adh-code NE "" THEN 
            RUN pProcessBoardBOMAdhesive(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.adh-code, ipbf-ef.adh-sqin).
        IF ipbf-ef.lam-code NE "" THEN 
            RUN pProcessBoardBOMLaminate(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.lam-code).
    END. 
    ELSE 
    DO: 
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ef.board, 0, BUFFER bf-estCostMaterial).
        ASSIGN 
            bf-estCostMaterial.isPrimarySubstrate         = YES
            bf-estCostMaterial.addToWeightNet             = YES
            bf-estCostMaterial.addToWeightTare            = NO
                                           
            bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostForm.grossQtyRequiredNoWaste
            bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostForm.grossQtyRequiredSetupWaste
            bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostForm.grossQtyRequiredRunWaste
            bf-estCostMaterial.quantityUOMWaste           = "EA"
            bf-estCostMaterial.quantityUOM                = "EA"
            bf-estCostMaterial.basisWeight                = bf-item.basis-w
            bf-estCostMaterial.dimWidth                   = ipbf-ttEstCostForm.grossWidth
            bf-estCostMaterial.dimLength                  = ipbf-ttEstCostForm.grossLength
            bf-estCostMaterial.dimDepth                   = ipbf-ttEstCostForm.grossDepth
            bf-estCostMaterial.noCharge                   = ipbf-ttEstCostForm.noCost
            .
        
        RUN pCalcBoardCostFromBlank (BUFFER ipbf-ttEstCostForm, BUFFER bf-estCostMaterial).
            
        cMaterialTypeCalculation = fGetMatTypeCalc(bf-estCostMaterial.company, bf-estCostMaterial.materialType).
        IF cMaterialTypeCalculation NE gcMaterialTypeCalcDefault THEN
            RUN pProcessMaterialTypeCalc(BUFFER bf-estCostMaterial, cMaterialTypeCalculation).
            
        IF ipbf-ttEstCostForm.costOverridePerUOM NE 0 THEN 
            ASSIGN 
                bf-estCostMaterial.costOverridePerUOM = ipbf-ttEstCostForm.costOverridePerUOM
                bf-estCostMaterial.costUOM            = ipbf-ttEstCostForm.costOverrideUOM
                .
                
        IF glVendItemCost THEN
            ASSIGN 
                cScope              = DYNAMIC-FUNCTION("VendCost_GetValidScopes","Est-RM-Over")
                lIncludeBlankVendor = YES.
        RUN pGetRequiredTotal(bf-estCostMaterial.quantityRequiredNoWaste,
            bf-estCostMaterial.quantityRequiredSetupWaste,
            bf-estCostMaterial.quantityRequiredRunWaste,
            bf-estCostMaterial.quantityRequiredMinDiff,
            OUTPUT dQtyTotal).
        /*Get Best Vendor*/
        IF lAvailAdder AND NOT glUseBlankVendor THEN
            RUN VendCost_GetBestVendorWithAdders(bf-estCostMaterial.Company,
                                                 bf-estCostMaterial.ItemID,
                                                 "RM", 
                                                cScope, 
                                                lIncludeBlankVendor, 
                                                bf-estCostMaterial.estimateNo, 
                                                bf-estCostMaterial.formNo, 
                                                bf-estCostMaterial.blankNo,
                                                dQtyTotal,
                                                bf-estCostMaterial.quantityUOM, 
                                                bf-estCostMaterial.dimLength,  
                                                bf-estCostMaterial.dimWidth, 
                                                bf-estCostMaterial.dimDepth, 
                                                bf-estCostMaterial.dimUOM, 
                                                bf-estCostMaterial.basisWeight, 
                                                bf-estCostMaterial.basisWeightUOM,
                                                cAdderList,
                                                OUTPUT bf-estCostMaterial.VendorID,
                                                OUTPUT lError,
                                                OUTPUT cMessage).
        
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).   
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank  FOR estCostBlank.
    
    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
        
    RUN pAddEstFarm(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostBlank, BUFFER bf-estCostMaterial).
    IF AVAILABLE bf-estCostMaterial THEN 
    DO:
        ASSIGN 
            bf-estCostMaterial.isPrimarySubstrate      = NO
            bf-estCostMaterial.addToWeightNet          = YES
            bf-estCostMaterial.addToWeightTare         = NO
            bf-estCostMaterial.isPurchasedFG           = YES                               
            bf-estCostMaterial.quantityRequiredNoWaste = ipbf-estCostBlank.quantityRequired
            .
        
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).
    END.
    
    ASSIGN 
        ipbf-ttEstCostForm.grossQtyRequiredTotal       = ipbf-ttEstCostForm.grossQtyRequiredNoWaste + ipbf-ttEstCostForm.grossQtyRequiredSetupWaste + ipbf-ttEstCostForm.grossQtyRequiredRunWaste
        ipbf-ttEstCostForm.grossQtyRequiredTotalArea   = ipbf-ttEstCostForm.grossQtyRequiredTotal * ipbf-ttEstCostForm.grossArea / 1000
        ipbf-ttEstCostForm.grossQtyRequiredTotalWeight = ipbf-ttEstCostForm.grossQtyRequiredTotalArea * ipbf-ttEstCostForm.basisWeight
        .
    
    
END PROCEDURE.

PROCEDURE pProcessGlues PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE VARIABLE dQtyRequiredMinDiff AS DECIMAL NO-UNDO.
    
    FOR EACH ttEstCostOperation NO-LOCK 
        WHERE ttEstCostOperation.estCostHeaderID EQ ipbf-ttEstCostForm.estCostHeaderID
        AND ttEstCostOperation.estCostFormID EQ ipbf-ttEstCostForm.estCostFormID
        AND ttEstCostOperation.isGluer, 
        EACH ttGlue NO-LOCK
        WHERE ttGlue.estHeaderID EQ ttEstCostOperation.estCostHeaderID
        AND ttGlue.estFormID EQ ttEstCostOperation.estCostFormID
        AND ttGlue.estBlankID EQ ttEstCostOperation.estCostBlankID
        ,
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttGlue.estHeaderID
        AND estCostBlank.estCostFormID EQ ttGlue.estFormID 
        AND estCostBlank.estCostBlankID EQ ttGlue.estBlankID 
        BY ttEstCostOperation.sequenceOfOperation DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ttGlue.cItemID, estCostBlank.estCostBlankID, BUFFER bf-estCostMaterial).
        
        ASSIGN    
            bf-estCostMaterial.addToWeightNet             = YES
            bf-estCostMaterial.quantityRequiredNoWaste    = ttEstCostOperation.quantityInNoWaste * ttGlue.dQtyRequiredPerBlank
            bf-estCostMaterial.quantityRequiredRunWaste   = ttEstCostOperation.quantityInRunWaste * ttGlue.dQtyRequiredPerBlank
            bf-estCostMaterial.quantityRequiredSetupWaste = ttEstCostOperation.quantityInSetupWaste * ttGlue.dQtyRequiredPerBlank
            dQtyRequiredMinDiff                           = ttGlue.dMinLbsPerJob - 
                                                    (bf-estCostMaterial.quantityRequiredNoWaste + bf-estCostMaterial.quantityRequiredRunWaste + bf-estCostMaterial.quantityRequiredSetupWaste)
            bf-estCostMaterial.quantityUOM                = ttGlue.cQtyUOM
            .             
        IF dQtyRequiredMinDiff GT 0 THEN 
            bf-estCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).
        
    END.

END PROCEDURE.

PROCEDURE pBuildHeader PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Builds all fields on the estCostHeader record
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    
    DEFINE           BUFFER bf-est             FOR est.
    DEFINE           BUFFER bf-ce-ctrl         FOR ce-ctrl.
     
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipbf-estCostHeader.company
        AND bf-est.est-no EQ ipbf-estCostHeader.estimateNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Estimate '" + ipbf-estCostHeader.estimateNo + "' not valid", gcErrorCritical, ipbf-estCostHeader.estCostHeaderID, 0,0).
        RETURN.
    END. 
    FIND FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ipbf-estCostHeader.company
        AND bf-ce-ctrl.loc EQ bf-est.loc
        NO-ERROR.
    IF NOT AVAILABLE bf-ce-ctrl THEN 
        FIND FIRST bf-ce-ctrl NO-LOCK 
            WHERE bf-ce-ctrl.company EQ ipbf-estCostHeader.company
            NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Control File not found for company '" + ipbf-estCostHeader.company + "'", gcErrorCritical, ipbf-estCostHeader.estCostHeaderID, 0,0).
        RETURN.
    END.
    ASSIGN 
        ipbf-estCostHeader.industry                    = IF bf-est.est-type LE 4 THEN gcIndustryFolding ELSE gcIndustryCorrugated
        ipbf-estCostHeader.estType                     = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", bf-est.est-type, bf-est.estimateTypeID)
        ipbf-estCostHeader.warehouseID                 = bf-est.loc
        ipbf-estCostHeader.marginOn                    = bf-ce-ctrl.sell-by
        ipbf-estCostHeader.marginPct                   = bf-ce-ctrl.prof-mrkup
        ipbf-estCostHeader.warehouseMarkupPct          = bf-ce-ctrl.whse-mrkup / 100 /*ctrl[1]*/
        ipbf-estCostHeader.handlingChargePct           = bf-ce-ctrl.hand-pct / 100 /*ctrl[2]*/
        ipbf-estCostHeader.handlingRateRMPerCWT        = bf-ce-ctrl.rm-rate /*ctrl[3]*/ /*NOTE CHANGED to be /100 */
        
        
        
        ipbf-estCostHeader.showCommissions             = bf-ce-ctrl.comm-add /*ctrl[5]*/
        ipbf-estCostHeader.showLaborRates              = bf-ce-ctrl.sho-labor /*ctrl[7]*/
        /*        ipbf-estCostHeader.addToFactCostFreight        = bf-ce-ctrl.shp-add /*ctrl[6]*/     */
        /*        ipbf-estCostHeader.addToFactCostSpecial1       = bf-ce-ctrl.spec-add[1] /*ctrl[13]*/*/
        /*        ipbf-estCostHeader.addToFactCostSpecial2       = bf-ce-ctrl.spec-add[2] /*ctrl[14]*/*/
        /*        ipbf-estCostHeader.addToFactCostSpecial3       = bf-ce-ctrl.spec-add[3] /*ctrl[15]*/*/
        /*        ipbf-estCostHeader.addToFactCostGSA            = bf-ce-ctrl.spec-add[6] /*ctrl[16]*/*/
        /*        ipbf-estCostHeader.addToFactCostRoyalty        = bf-ce-ctrl.spec-add[8] /*ctrl[18]*/*/
        /*        ipbf-estCostHeader.addToFactCostComm           = bf-ce-ctrl.spec-add[7] /*ctrl[17]*/*/
        ipbf-estCostHeader.foldPct                     = bf-ce-ctrl.fold-pct / 100 /*ctrl[19]*/ /*NOTE CHANGED to be /100 */            
        ipbf-estCostHeader.handlingRateFGPerCWT        = bf-ce-ctrl.fg-rate   /*ld-fg-rate*/
        ipbf-estCostHeader.handlingRateRMFarmPerCWT    = bf-ce-ctrl.rm-rate-farm 
        ipbf-estCostHeader.handlingRateFGFarmPerCWT    = bf-ce-ctrl.fg-rate-farm 
        ipbf-estCostHeader.handlingChargeFarmPct       = bf-ce-ctrl.hand-pct-farm / 100
        ipbf-estCostHeader.directMaterialPct           = gdMaterialMarkup / 100           
        ipbf-estCostHeader.weightUOM                   = gcDefaultWeightUOM     
        
        ipbf-estCostHeader.special1MarkupPct           = IF bf-ce-ctrl.spec-%[1] < 1 THEN bf-ce-ctrl.spec-%[1] ELSE 0 /*ctrl[4] - already a fraction?*/ 
        ipbf-estCostHeader.special1FlatValue           = IF bf-ce-ctrl.spec-%[1] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[1] /*REFACTOR - treatment of Special Costs*/            
        ipbf-estCostHeader.special2MarkupPct           = IF bf-ce-ctrl.spec-%[2] < 1 THEN bf-ce-ctrl.spec-%[2] ELSE 0 /*ctrl[4] - already a fraction?*/     
        ipbf-estCostHeader.special2FlatValue           = IF bf-ce-ctrl.spec-%[2] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[2] /*REFACTOR - treatment of Special Costs*/
        ipbf-estCostHeader.special3MarkupPct           = IF bf-ce-ctrl.spec-%[3] < 1 THEN bf-ce-ctrl.spec-%[3] ELSE 0 /*ctrl[4] - already a fraction?*/ 
        ipbf-estCostHeader.special3FlatValue           = IF bf-ce-ctrl.spec-%[3] < 1 THEN 0 ELSE bf-ce-ctrl.spec-%[3] /*REFACTOR - treatment of Special Costs*/
        .
    
END PROCEDURE.

PROCEDURE pProcessInk PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttInk/ttEstCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank     FOR estCostBlank.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttInk            FOR ttInk.
    
 
    DEFINE           BUFFER bf-estCostMaterial    FOR estCostMaterial.
    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL.
    DEFINE VARIABLE dqtyRequiredMinDiff AS DECIMAL. 
        
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ttInk.cItemID, ipbf-estCostBlank.estCostBlankID, BUFFER bf-estCostMaterial).
        
    ASSIGN    
        bf-estCostMaterial.addToWeightNet             = YES
        ipbf-ttInk.dQtyRequiredPerBlank               = ipbf-ttInk.dCoveragePercent * ipbf-estCostBlank.blankAreaNetWindow / ipbf-ttInk.dCoverageRate
        dQtyRequiredPerForm                           = ipbf-estCostBlank.numOut * ipbf-ttInk.dQtyRequiredPerBlank
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostOperation.quantityInNoWaste * dQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostOperation.quantityInRunWaste * dQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostOperation.quantityInSetupWaste * dQtyRequiredPerForm + ipbf-ttEstCostOperation.quantityInkLbsWastedPerSetup
        dQtyRequiredMinDiff                           = ipbf-ttInk.dMinLbsPerJob - (bf-estCostMaterial.quantityRequiredNoWaste + bf-estCostMaterial.quantityRequiredRunWaste + bf-estCostMaterial.quantityRequiredSetupWaste)
        bf-estCostMaterial.quantityUOM                = ipbf-ttInk.cQtyUOM
        bf-estCostMaterial.noCharge                   = ipbf-ttInk.lNoCharge
        .             
    IF dQtyRequiredMinDiff GT 0 THEN 
        bf-estCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
    
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessLeaf PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttLeaf/estCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm    FOR ttEstCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttLeaf           FOR ttLeaf.
    DEFINE INPUT PARAMETER ipdQtyRequiredPerFeed AS DECIMAL NO-UNDO.    
 
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
        
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, ipbf-ttLeaf.cItemID, ipbf-ttLeaf.estBlankID, BUFFER bf-estCostMaterial).
        
    ASSIGN    
        bf-estCostMaterial.addToWeightNet             = YES
        bf-estCostMaterial.dimLength                  = ipbf-ttLeaf.dDimLength
        bf-estCostMaterial.dimWidth                   = ipbf-ttLeaf.dDimWidth
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-ttEstCostOperation.quantityInNoWaste * ipdQtyRequiredPerFeed
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-ttEstCostOperation.quantityInRunWaste * ipdQtyRequiredPerFeed
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-ttEstCostOperation.quantityInSetupWaste * ipdQtyRequiredPerFeed
        bf-estCostMaterial.quantityUOM                = ipbf-ttLeaf.cQtyUOM
        bf-estCostMaterial.basisWeight                = 144000 / ipbf-ttLeaf.dCoverageRate
        .             

    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-ttEstCostForm).

END PROCEDURE.

PROCEDURE pProcessInks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for inks with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
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
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND estCostBlank.estCostFormID EQ ttInk.estFormID 
        AND estCostBlank.estCostBlankID EQ ttInk.estBlankID :
        
        RUN pProcessInk(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER estCostBlank, BUFFER ttEstCostOperation, BUFFER ttInk).    
        
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
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND estCostBlank.estCostFormID EQ ttInk.estFormID 
        AND estCostBlank.estCostBlankID EQ ttInk.estBlankID :
            
        RUN pProcessInk(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER estCostBlank, BUFFER ttEstCostOperation, BUFFER ttInk).    
    END.

END PROCEDURE.

PROCEDURE pProcessLeafs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for leafs with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstCostForm FOR ttEstCostForm.

    DEFINE VARIABLE dQtyRequiredPerFeed AS DECIMAL NO-UNDO.
    
    RUN pBuildLeafForEf(BUFFER ipbf-ef, BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm).
    
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
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND estCostBlank.estCostFormID EQ ttLeaf.estFormID 
                AND estCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                dQtyRequiredPerFeed = estCostBlank.numOut * ttLeaf.dQtyRequiredPerLeaf.
        END.
        ELSE 
            dQtyRequiredPerFeed = ttLeaf.dQtyRequiredPerLeaf.
            
        RUN pProcessLeaf(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ttEstCostOperation, BUFFER ttLeaf, dQtyRequiredPerFeed).    
        
    END.

   
END PROCEDURE.

PROCEDURE pProcessPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for packing material with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.

    DEFINE           BUFFER bf-estCostMaterial    FOR estCostMaterial.
    DEFINE           BUFFER bf-ttEstCostForm      FOR ttEstCostForm.
    DEFINE           BUFFER bf-estCostBlank       FOR estCostBlank.
    DEFINE           BUFFER bfUnitize-estCostform FOR ttEstCostForm.
    
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
        
    IF ipbf-estCostHeader.isUnitizedSet  THEN 
    DO:
        /*Establish unitization form (Form 1)*/
        FIND FIRST bfUnitize-estCostForm NO-LOCK 
            WHERE bfUnitize-estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bfUnitize-estCostForm.formNo EQ 1
            NO-ERROR.
    END.

    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-estCostHeader.estCostHeaderID,
        FIRST bf-estCostBlank EXCLUSIVE-LOCK 
        WHERE bf-estCostBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-estCostBlank.estCostFormID EQ ttPack.estFormID
        AND bf-estCostBlank.estCostBlankID EQ ttPack.estBlankID,
        FIRST bf-ttEstCostForm NO-LOCK 
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-ttEstCostForm.estCostFormID EQ ttPack.estFormID
        BY lIsCase DESCENDING 
        BY lIsPallet DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-ttEstCostForm, ttPack.cItemID, bf-estCostBlank.estCostBlankID, BUFFER bf-estCostMaterial).
        
        IF ttPack.lIsCase THEN 
        DO:
            IF ttPack.iCountPerSubUnit NE 0 THEN
                ASSIGN
                    iCaseCount    = ttPack.iCountPerSubUnit  
                    dCasesProRata = bf-estCostBlank.quantityRequired / ttPack.iCountPerSubUnit
                    iCases        = fRoundUp(dCasesProRata * ttPack.dQtyMultiplier).
            ELSE 
            DO:
                /*Calc cases based on weight - REFACTOR since assumes weight is in LB/M */
                iCases     = fRoundUp(bf-estCostBlank.quantityRequired * (bf-estCostBlank.weightPerBlank) / ttPack.dWeightCapacity) * ttPack.dQtyMultiplier
                    .
                IF iCases GT 0 THEN 
                    iCaseCount = fRoundUp(bf-estCostBlank.quantityRequired /  iCases)
                        .
            END.
            ASSIGN  
                bf-estCostMaterial.addToWeightTare         = YES 
                bf-estCostMaterial.quantityRequiredNoWaste = iCases
                bf-estCostMaterial.quantityUOM             = ttPack.cQtyUOM
                bf-estCostBlank.quantityPerSubUnit         = iCaseCount
                bf-estCostBlank.quantityOfSubUnits         = iCases
                bf-estCostMaterial.costOverridePerUOM      = ttPack.dCostPerUOMOverride
                bf-estCostMaterial.noCharge                = ttPack.lNoCharge
                .            
            
            IF iCaseCount NE 0 THEN
                bf-estCostMaterial.itemName = bf-estCostMaterial.itemName + " (" + TRIM(STRING(iCaseCount,">>>>>9")) + ")".   
            bf-estCostMaterial.weightTotal             = ttPack.dWeightTare * iCases.
        END.
        ELSE IF ttPack.lIsPallet THEN 
            DO:
                ASSIGN  
                    iPalletCount                               = IF ttPack.iCountPerUnit EQ 0 THEN ttPack.iCountSubUnitsPerUnit * iCaseCount ELSE ttPack.iCountPerUnit
                    dPalletsProRata                            = IF iPalletCount NE 0 THEN bf-estCostBlank.quantityRequired / iPalletCount ELSE iPalletCount
                    iPallets                                   = fRoundUp(dPalletsProRata * ttPack.dQtyMultiplier) 
                    bf-estCostMaterial.addToWeightTare         = YES
                    bf-estCostMaterial.quantityRequiredNoWaste = iPallets
                    bf-estCostMaterial.quantityUOM             = ttPack.cQtyUOM
                    bf-estCostBlank.quantityOfUnits            = iPallets
                    bf-estCostMaterial.costOverridePerUOM      = ttPack.dCostPerUOMOverride   
                    bf-estCostMaterial.noCharge                = ttPack.lNoCharge
                    .            
        
                IF iPalletCount NE 0 THEN 
                    bf-estCostMaterial.itemName = bf-estCostMaterial.itemName + " (" + TRIM(STRING(iPalletCount,">>>>>9")) + ")".
                bf-estCostMaterial.weightTotal             = ttPack.dWeightTare * iPallets.
            END.        
            ELSE 
            DO:
                IF ttPack.dQtyMultiplier GT 1 THEN /*If there are multiple packers per case/pallet, only add a packer if necessary*/
                    ASSIGN 
                        dPalletsProRata = bf-estCostBlank.quantityRequired / MAX(1, bf-estCostBlank.quantityPerSubUnit * bf-estCostBlank.quantitySubUnitsPerUnit)
                        dCasesProRata   = bf-estCostBlank.quantityRequired / MAX(1, bf-estCostBlank.quantityPerSubUnit)
                        .
                ELSE 
                    ASSIGN 
                        dPalletsProRata = bf-estCostBlank.quantityOfUnits
                        dCasesProRata   = bf-estCostBlank.quantityOfSubUnits
                        .
         
                CASE ttPack.cQtyMultiplier:
                    WHEN "P" THEN 
                        bf-estCostMaterial.quantityRequiredNoWaste = dPalletsProRata * ttPack.dQtyMultiplier.
                    WHEN "C" THEN 
                        bf-estCostMaterial.quantityRequiredNoWaste = dCasesProRata * ttPack.dQtyMultiplier.
                    OTHERWISE 
                    bf-estCostMaterial.quantityRequiredNoWaste = ttPack.dQtyMultiplier.
                END CASE.
                ASSIGN                                                 
                    bf-estCostMaterial.addToWeightTare    = NO
                    bf-estCostMaterial.quantityUOM        = ttPack.cQtyUOM
                    bf-estCostMaterial.costOverridePerUOM = ttPack.dCostPerUOMOverride
                    bf-estCostMaterial.noCharge           = ttPack.lNoCharge
                    .    
                IF bf-estCostMaterial.quantityUOM EQ "EA" THEN 
                    bf-estCostMaterial.quantityRequiredNoWaste = fRoundUp(bf-estCostMaterial.quantityRequiredNoWaste).            
                bf-estCostMaterial.weightTotal = ttPack.dWeightTare * bf-estCostMaterial.quantityRequiredNoWaste.
            END.
        
        IF bf-ttEstCostForm.formNo EQ 0 AND AVAILABLE bfUnitize-estCostForm THEN 
        DO:
            /*Associate Form 0 materials to the unitize form (Form 1)*/
            ASSIGN 
                bf-estCostMaterial.estCostFormID  = bfUnitize-estCostForm.estCostFormID
                bf-estCostMaterial.estCostBlankID = 0
                .
            RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER bfUnitize-estCostForm).
        END. 
        ELSE     
            RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER bf-ttEstCostForm).
        
    END.
    RELEASE bf-estCostBlank.
       
    
END PROCEDURE.



PROCEDURE pGetEstFarmCosts PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an estCostMaterial, fill in cost per UOM and Setup for 
    given vendor - Farm Tab version.
    
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
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

        RUN VendCost_GetBestCost(ipbf-estCostMaterial.company, 
            ipbf-estCostMaterial.itemID, "FG", 
            cScope, lIncludeBlankVendor, 
            ipbf-estCostMaterial.estimateNo, ipbf-estCostMaterial.formNo, ipbf-estCostMaterial.blankNo,
            ipdQty, ipcQtyUOM, 
            ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, ipbf-estCostMaterial.dimUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.basisWeightUOM,
            OUTPUT opdCost, OUTPUT opcCostUOM, OUTPUT opdSetup, OUTPUT opcVendorID, OUTPUT opdCostDeviation, OUTPUT dCostTotal,
            OUTPUT lError, OUTPUT cMessage).
    END.
    ELSE 
    DO:
        FIND FIRST e-itemfg NO-LOCK
            WHERE e-itemfg.company EQ ipbf-estCostHeader.company
            AND e-itemfg.i-no EQ ipbf-estCostMaterial.itemID
            NO-ERROR.
        IF AVAILABLE e-itemfg THEN
        DO:
            opcCostUom = e-itemfg.std-uom.
            RELEASE e-itemfg-vend.
            IF ipcVendNo NE "" THEN
                FIND FIRST e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-estCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-estCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-estCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-estCostMaterial.blankNo
                    AND e-itemfg-vend.vend-no EQ ipcVendNo
                    NO-ERROR.
            IF NOT AVAILABLE e-itemfg-vend THEN
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-estCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-estCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-estCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-estCostMaterial.blankNo
                    AND e-itemfg-vend.vend-no EQ ""
                    BY e-itemfg-vend.vend-no:
                    LEAVE.
                END.    
            IF NOT AVAILABLE e-itemfg-vend THEN
                FOR EACH e-itemfg-vend NO-LOCK
                    WHERE e-itemfg-vend.company EQ ipbf-estCostHeader.company 
                    AND e-itemfg-vend.est-no EQ ipbf-estCostHeader.estimateNo
                    AND e-itemfg-vend.form-no EQ ipbf-estCostMaterial.formNo
                    AND e-itemfg-vend.blank-no EQ ipbf-estCostMaterial.blankNo
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
                    RUN pConvertQuantityFromUOMToUOM(e-itemfg-vend.company, ipbf-estCostMaterial.itemID, "FG", ipcQtyUOM,opcCostUOM,
                        ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth,
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
    Purpose: Given an estCostMaterial, fill in cost per UOM and Setup for 
    given vendor.
     Notes:  replaces est/matcost.i (should move to costProcs)
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
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
        IF ipbf-estCostMaterial.vendorID NE "" OR glUseBlankVendor THEN 
        DO:
            opcVendorID = ipbf-estCostMaterial.vendorID.
            RUN GetVendorCost(ipbf-estCostMaterial.company, ipbf-estCostMaterial.itemID, "RM", 
                opcVendorID, "", ipbf-estCostMaterial.estimateNo, ipbf-estCostMaterial.formNo, ipbf-estCostMaterial.blankNo, 
                ipdQty, ipcQtyUOM, 
                ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, ipbf-estCostMaterial.dimUOM, 
                ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.basisWeightUOM, 
                NO,
                OUTPUT opdCost, OUTPUT opdSetup, OUTPUT opcCostUOM, OUTPUT dCostTotal, OUTPUT lError, OUTPUT cMessage).
        END.
        ELSE 
        DO:                        
            RUN VendCost_GetBestCost(ipbf-estCostMaterial.company, 
                    ipbf-estCostMaterial.itemID, 
                    "RM", 
                    cScope, 
                    lIncludeBlankVendor, 
                    ipbf-estCostMaterial.estimateNo, 
                    ipbf-estCostMaterial.formNo, 
                    ipbf-estCostMaterial.blankNo,
                    ipdQty, 
                    ipcQtyUOM, 
                    ipbf-estCostMaterial.dimLength,  
                    ipbf-estCostMaterial.dimWidth, 
                    ipbf-estCostMaterial.dimDepth, 
                    ipbf-estCostMaterial.dimUOM, 
                    ipbf-estCostMaterial.basisWeight, 
                    ipbf-estCostMaterial.basisWeightUOM,
                    OUTPUT opdCost, 
                    OUTPUT opcCostUOM, 
                    OUTPUT opdSetup, 
                    OUTPUT opcVendorID, 
                    OUTPUT opdCostDeviation, 
                    OUTPUT dCostTotal,
                    OUTPUT lError, 
                    OUTPUT cMessage).
        END.
        RETURN.
    
    END.
    
    FIND FIRST e-item NO-LOCK
        WHERE e-item.company EQ ipbf-estCostMaterial.company
        AND e-item.i-no EQ ipbf-estCostMaterial.itemID
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
                RUN pConvertQuantityFromUOMToUOM(e-item-vend.company, ipbf-estCostMaterial.itemID, "RM", ipcQtyUOM,opcCostUOM,
                    ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth,
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

    IF ipbf-estCostMaterial.isRealMaterial AND (opdCost EQ 0 OR lError) THEN
        ASSIGN 
            opdCost    = IF ipbf-estCostHeader.forRealItemsUseAvgCost THEN ipbf-estCostMaterial.costPerUOMAvg ELSE ipbf-estCostMaterial.costPerUOMLast
            opcCostUOM = ipbf-estCostMaterial.quantityUOM  /*REFACTOR? - What uom is avg and last cost in*/
            .
            
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
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
            WHERE bf-mach.company EQ ipbf-estCostHeader.company
            AND bf-mach.m-code EQ ipbf-ttEstCostOperation.operationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-estCostHeader, BUFFER ipbf-ttEstCostForm, BUFFER ipbf-estCostOperation, BUFFER bf-mach).
       
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
    IF fIsSetType(ipbf-estCostHeader.estType) AND (ipbf-ttEstCostOperation.feedType EQ "A" OR  ipbf-ttEstCostOperation.feedType EQ "P") THEN 
    DO: 
        IF ipbf-ttEstCostOperation.feedType EQ "P" THEN 
            dPartCount = fGetPartCount(ipbf-estCostHeader.company, ipbf-estCostHeader.estimateNo).
        ELSE 
            dPartCount = 1.
        ASSIGN 
            ipbf-ttEstCostOperation.quantityInNoWaste         = ipbf-estCostHeader.quantityMaster * dPartCount
            ipbf-ttEstCostOperation.quantityOut               = ipbf-estCostHeader.quantityMaster
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
        RUN est/estCostMaterialList.w (INPUT-OUTPUT TABLE ttEstCostHeaderToCalc BY-REFERENCE).
        
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
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
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
    FOR EACH bf-estCostHeader EXCLUSIVE-LOCK 
        WHERE bf-estCostHeader.company EQ ipcCompany
        AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
        AND bf-estCostHeader.jobID EQ ipcJobNo
        AND bf-estCostHeader.jobID2 EQ ipiJobNo2:
            
        DELETE bf-estCostHeader.    
    END.
    RELEASE bf-probe.
    RELEASE bf-probeit.
    RELEASE bf-estCostHeader.
    
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

PROCEDURE pPurgeJobCalculation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Removes any recalculation records for a specific job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader. 
    
    FOR EACH bf-estCostHeader
        WHERE bf-estCostHeader.company EQ ipcCompany
        AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
        AND bf-estCostHeader.jobID EQ ipcJobNo
        AND bf-estCostHeader.jobID2 EQ ipiJobNo2:
        DELETE bf-estCostHeader.    
    END.
    
    RELEASE bf-estCostHeader.
    
END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated Run Speeds for estCostOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
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
    
    DEFINE BUFFER bf-estCostHeader   FOR estCostHeader.
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-ttEstCostForm   FOR ttEstCostForm.
    
    RUN pResetCostTotals(ipiEstCostHeaderID). 
    RUN pPurgeCostDetail(ipiEstCostHeaderID, "").
    RUN pPurgeCostSummary(ipiEstCostHeaderID).
    FOR EACH bf-ttEstCostForm NO-LOCK
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID:
        
        FOR EACH bf-estCostMaterial EXCLUSIVE-LOCK
            WHERE bf-estCostMaterial.estCostHeaderID EQ bf-ttEstCostForm.estCostHeaderID
            AND bf-estCostMaterial.estCostFormID EQ bf-ttEstCostForm.estCostFormID:
                    
            RUN pCalcEstMaterial(BUFFER bf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER bf-ttEstCostForm).
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
    DEFINE BUFFER bf-estCostItem   FOR estCostItem.
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    FOR EACH bf-ttEstCostForm NO-LOCK 
        WHERE bf-ttEstCostForm.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsForm(BUFFER bf-ttEstCostForm).
    END.
    FOR EACH bf-estCostItem NO-LOCK 
        WHERE bf-estCostItem.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsItem(BUFFER bf-estCostItem).
    END.
    FOR EACH bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pResetCostTotalsHeader(BUFFER bf-estCostHeader).
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
    {est\EstimateCostTotalReset.i &TableName=estCostHeader}
     
END PROCEDURE.

PROCEDURE pResetCostTotalsItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Resets Cost Totals for a given ttEstCostForm
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotalReset.i &TableName=estCostItem}
     
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
    
    RUN sys/ref/nk1look.p (ipcCompany, "FOAMCOST", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    glCalcFoamCostFromBlank = lFound AND cReturn EQ "Blank".
    
END PROCEDURE.

PROCEDURE pSetKeyFields PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Assign UNique INdex fields of Temp-table
     Notes: Assign a temporary value to Key fields. It will be calculated by Triggers while DB record creation.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER ipiKeyField   AS INT64 NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER ipcRecKeyField AS CHARACTER NO-UNDO.


    ASSIGN
        ipiKeyField    = fGetNextID() 
        ipcRecKeyField = "00000000" + STRING(ipiKeyField).

END PROCEDURE.

PROCEDURE pUpdateChildTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Assign Parent Key in Child Tables
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcParentTable      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiEstCostHeaderID  AS INT64 NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSourceKey        AS INT64 NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTargetKey        AS INT64 NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSourceRecKey     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTargetRecKey     AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bfChild-estCostBlank     FOR estCostBlank.
    DEFINE BUFFER bfChild-ttEstCostOperation FOR ttEstCostOperation.
    DEFINE BUFFER bfChild-estCostMaterial  FOR estCostMaterial.
    DEFINE BUFFER bfChild-ttEstCostMisc      FOR ttEstCostMisc.
    DEFINE BUFFER bfChild-ttEstCostDetail  FOR ttEstCostDetail.
    DEFINE BUFFER bfChild-ttEstCostSummary FOR ttEstCostSummary.
    
    
    CASE(ipcParentTable):
        WHEN "estCostForm" THEN
        DO:
            FOR EACH bfChild-estCostBlank EXCLUSIVE-LOCK
                WHERE bfChild-estCostBlank.estCostHeaderID = ipiEstCostHeaderID:
              
                IF bfChild-estCostBlank.estCostFormID = ipiSourceKey THEN
                    bfChild-estCostBlank.estCostFormID = ipiTargetKey.
            END.
        
            FOR EACH bfChild-ttEstCostDetail EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostDetail.estCostHeaderID = ipiEstCostHeaderID:              
                IF bfChild-ttEstCostDetail.estCostFormID = ipiSourceKey THEN
                    bfChild-ttEstCostDetail.estCostFormID = ipiTargetKey.
                    
                IF bfChild-ttEstCostDetail.sourceID = ipiSourceKey THEN
                    bfChild-ttEstCostDetail.sourceID = ipiTargetKey.
            END.
        
            FOR EACH bfChild-ttEstCostOperation EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostOperation.estCostHeaderID = ipiEstCostHeaderID:
                
                IF bfChild-ttEstCostOperation.estCostFormID = ipiSourceKey THEN  
                    bfChild-ttEstCostOperation.estCostFormID = ipiTargetKey.
            END.
        
            FOR EACH bfChild-estCostMaterial EXCLUSIVE-LOCK
                WHERE bfChild-estCostMaterial.estCostHeaderID = ipiEstCostHeaderID:
                IF bfChild-estCostMaterial.estCostFormID = ipiSourceKey THEN  
                    bfChild-estCostMaterial.estCostFormID = ipiTargetKey.
            END.
        
            FOR EACH bfChild-ttEstCostMisc EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostMisc.estCostHeaderID = ipiEstCostHeaderID:
                
                IF bfChild-ttEstCostMisc.estCostFormID = ipiSourceKey THEN  
                    bfChild-ttEstCostMisc.estCostFormID = ipiTargetKey.
            END.
        
            FOR EACH bfChild-ttEstCostSummary EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostSummary.estCostHeaderID = ipiEstCostHeaderID:
            
                IF bfChild-ttEstCostSummary.scopeRecKey = ipcSourceRecKey THEN      
                    bfChild-ttEstCostSummary.scopeRecKey = ipcTargetRecKey.
            END.
    
        END.
        WHEN "estCostOperation" THEN
        DO:
            FOR EACH bfChild-ttEstCostDetail EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostDetail.estCostHeaderID = ipiEstCostHeaderID:              
                IF bfChild-ttEstCostDetail.sourceID = ipiSourceKey THEN
                    bfChild-ttEstCostDetail.sourceID = ipiTargetKey.
            END.
        END.
        WHEN "estCostMisc" THEN
        DO:
            FOR EACH bfChild-ttEstCostDetail EXCLUSIVE-LOCK
                WHERE bfChild-ttEstCostDetail.estCostHeaderID = ipiEstCostHeaderID              
                  AND bfChild-ttEstCostDetail.sourceType = gcSourceTypeMisc:
                      
                IF bfChild-ttEstCostDetail.sourceID = ipiSourceKey THEN
                    bfChild-ttEstCostDetail.sourceID = ipiTargetKey.
            END.
        END.
        
    END CASE.

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
    

    FOR EACH ttEstCostForm
        TRANSACTION:
        CREATE bfEx-estCostForm.
        BUFFER-COPY ttEstCostForm EXCEPT rec_key estCostFormID TO bfEx-estCostForm.
        
        /* Update parent key in child Tables */
        RUN pUpdateChildTables ("estCostForm", 
            ttEstCostForm.estCostHeaderID, 
            ttEstCostForm.estCostFormID, 
            bfEx-estCostForm.estCostFormID,
            ttEstCostForm.rec_key,
            bfEx-estCostForm.rec_key).
            
    END. /* ttEstCostForm */
    
    FOR EACH ttEstCostDetail
        TRANSACTION:
        CREATE bfEx-estCostDetail.
        BUFFER-COPY ttEstCostDetail EXCEPT rec_key estCostDetailID TO bfEx-estCostDetail.
        
    END. /* ttEstCostDetail */
    
    FOR EACH ttEstCostSummary
        TRANSACTION:
        CREATE bfEx-estCostSummary.
        BUFFER-COPY ttEstCostSummary EXCEPT rec_key estCostSummaryID TO bfEx-estCostSummary.
        
    END. /* ttEstCostSummary */
     
    FOR EACH ttEstCostOperation
        TRANSACTION:
        CREATE bfEx-estCostOperation.
        BUFFER-COPY ttEstCostOperation EXCEPT rec_key estCostOperationID TO bfEx-estCostOperation.
        
        RUN pUpdateChildTables ("estCostOperation", 
            ttEstCostOperation.estCostHeaderID, 
            ttEstCostOperation.estCostOperationID, 
            bfEx-estCostOperation.estCostOperationID,
            ttEstCostOperation.rec_key,
            bfEx-estCostOperation.rec_key).
    END. /* ttEstCostOperation */    
        
    FOR EACH ttEstCostMisc
        TRANSACTION:
        CREATE bfEx-estCostMisc.
        BUFFER-COPY ttEstCostMisc EXCEPT rec_key estCostMiscID TO bfEx-estCostMisc.
        
        RUN pUpdateChildTables ("estCostMisc", 
            ttEstCostMisc.estCostHeaderID, 
            ttEstCostMisc.estCostMiscID, 
            bfEx-estCostMisc.estCostMiscID,
            ttEstCostMisc.rec_key,
            bfEx-estCostMisc.rec_key).
    END. /* ttEstCostOperation */    
     
        
    RELEASE bfEx-estCostForm.
    RELEASE bfEx-estCostDetail.
    RELEASE bfEx-estCostSummary.
    RELEASE bfEx-estCostOperation.
    RELEASE bfEx-estCostMisc.
    
    EMPTY TEMP-TABLE ttEstCostForm.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
    EMPTY TEMP-TABLE ttEstCostOperation.
    EMPTY TEMP-TABLE ttEstCostMisc.
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64 , ipiEstFormID AS INT64 , ipiBlankNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the Blank ID given header, form id and blank #
     Notes:
    ------------------------------------------------------------------------------*/    
    FIND FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ipiEstHeaderID
        AND estCostBlank.estCostFormID EQ ipiEstFormID
        AND estCostBlank.blankNo EQ ipiBlankNo
        NO-ERROR.
    IF AVAILABLE estCostBlank THEN 
        RETURN estCostBlank.estCostBlankID.

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
         lFoam = IsFoamStyle (bf-ttEstCostOperation.company, bf-ttEstCostOperation.estimateNo, bf-ttEstCostOperation.formNo).
         
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
    (  ):
    /*------------------------------------------------------------------------------
         Purpose: increment the ID for a unique id
         Notes:
        ------------------------------------------------------------------------------*/    
    
    giID = giID + 1.
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

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER, ipcDepartmentList AS CHARACTER EXTENT 4):
    /*------------------------------------------------------------------------------
     Purpose: determine if provided department is in department list
     Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("fEstimate_IsDepartment", ipcDepartment, ipcDepartmentList).
        
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

FUNCTION IsFoamStyle RETURNS LOGICAL 
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
  