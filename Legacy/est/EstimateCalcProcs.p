
/*------------------------------------------------------------------------
    File        : EstimateCalcProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.

{est/ttEstCost.i}
DEFINE VARIABLE giProbeLine                           AS INTEGER   NO-UNDO.

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

DEFINE VARIABLE gcIndustryFolding                     AS CHARACTER NO-UNDO INITIAL "Folding".
DEFINE VARIABLE gcIndustryCorrugated                  AS CHARACTER NO-UNDO INITIAL "Corrugated".

DEFINE VARIABLE gcTypeSingle                          AS CHARACTER NO-UNDO INITIAL "Single".
DEFINE VARIABLE gcTypeSet                             AS CHARACTER NO-UNDO INITIAL "Set".
DEFINE VARIABLE gcTypeCombo                           AS CHARACTER NO-UNDO INITIAL "Combo/Tandem".
DEFINE VARIABLE gcTypeList                            AS CHARACTER NO-UNDO. 

DEFINE VARIABLE gcErrorWarning                        AS CHARACTER NO-UNDO INITIAL "Warning".
DEFINE VARIABLE gcErrorImportant                      AS CHARACTER NO-UNDO INITIAL "Important".
DEFINE VARIABLE gcErrorCritical                       AS CHARACTER NO-UNDO INITIAL "Critical".

/*Settings Globals*/
DEFINE VARIABLE gcPrepRoundTo                         AS CHARACTER NO-UNDO.  /*CEPREP - char val - potentially deprecate*/
DEFINE VARIABLE gcPrepMarkupOrMargin                  AS CHARACTER NO-UNDO.  /*CEPrepPrice - char val*/
DEFINE VARIABLE gdMaterialMarkup                      AS DECIMAL   NO-UNDO.    /*CEMatl - Dec val*/
DEFINE VARIABLE gcMarginMatrixLookup                  AS CHARACTER NO-UNDO.    /*CEMatl - Dec val*/

DEFINE VARIABLE glUsePlateChangesAsColorForSetupWaste AS LOGICAL   NO-UNDO INITIAL NO.  /*Defect in EstOperation Calc of applying the MR Waste Sheets Per Color?*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64,
    ipiEstFormID AS INT64,
    ipiBlankNo AS INTEGER) FORWARD.

FUNCTION fGetProfit RETURNS DECIMAL PRIVATE
    (ipdCost AS DECIMAL,
    ipdProfitPercent AS DECIMAL,
    ipcPercentType AS CHARACTER) FORWARD.

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER,
    ipcDepartmentList AS CHARACTER EXTENT 4) FORWARD.

FUNCTION fRoundUP RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL) FORWARD.

/* ***************************  Main Block  *************************** */
ASSIGN 
    /*Build mapping from estimate type # to descriptive type*/ 
    gcTypeList = gcTypeSingle + "," + gcTypeSet + ","  + gcTypeCombo + "," + gcTypeCombo + "," + gcTypeSingle + "," + gcTypeSet + ","  + gcTypeCombo + "," + gcTypeCombo
    .

RUN pSetGlobalSettings(ipcCompany).  
IF iplPurge THEN 
    RUN pPurgeCalculation(ipcCompany, ipcEstimateNo).
RUN pBuildHeadersToProcess(ipcCompany, ipcEstimateNo).
FOR EACH ttEstHeaderToCalc: 
    RUN pCalculateHeader(ttEstHeaderToCalc.iEstCostHeaderID, INPUT-OUTPUT giProbeLine).
END.
FOR EACH ttEstError NO-LOCK:
    DISPLAY ttEstError.iFormNo ttEstError.iBlankNo ttEstError.cErrorType ttEstError.cError FORMAT "x(60)" ttEstError.iFormNo.
END.


/* **********************  Internal Procedures  *********************** */

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
    DEFINE PARAMETER BUFFER opbf-estCostDetail FOR estCostDetail.
    
    CREATE opbf-estCostDetail.
    ASSIGN 
        opbf-estCostDetail.estCostHeaderID   = ipiEstHeaderID
        opbf-estCostDetail.estCostFormID     = ipiEstFormID
        opbf-estCostDetail.estCostBlankID    = ipiEstBlankID
        opbf-estCostDetail.company           = ipcCompany
        opbf-estCostDetail.estimateNo        = ipcEstimateNo
        opbf-estCostDetail.sourceID          = ipiEstSourceID
        opbf-estCostDetail.sourceType        = ipcSourceType
        opbf-estCostDetail.estCostCategoryID = ipcEstCostCategoryID
        opbf-estCostDetail.estCostDetailDesc = ipcDescription
        opbf-estCostDetail.costTotal         = ipdCost
        opbf-estCostDetail.profitPercent     = ipdProfitPercent
        opbf-estCostDetail.profitPercentType = "Margin"
        .

END PROCEDURE.

PROCEDURE pAddCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    
    RUN pAddCostDetail(ipbf-estCostOperation.estCostHeaderID, ipbf-estCostOperation.estCostFormID, ipbf-estCostOperation.estCostBlankID, ipbf-estCostOperation.estCostOperationID, 
        gcSourceTypeOperation, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-estCostOperation.company, ipbf-estCostOperation.estimateNo, BUFFER bf-estCostDetail).

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
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    
    RUN pAddCostDetail(ipbf-estCostMaterial.estCostHeaderID, ipbf-estCostMaterial.estCostFormID, ipbf-estCostMaterial.estCostBlankID, ipbf-estCostMaterial.estCostMaterialID, 
        gcSourceTypeMaterial, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-estCostMaterial.company, ipbf-estCostMaterial.estimateNo, BUFFER bf-estCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostMisc FOR estCostMisc.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    
    RUN pAddCostDetail(ipbf-estCostMisc.estCostHeaderID, ipbf-estCostMisc.estCostFormID, ipbf-estCostMisc.estCostBlankID, ipbf-estCostMisc.estCostMiscID, 
        gcSourceTypeMisc, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, ipbf-estCostMisc.company, ipbf-estCostMisc.estimateNo, BUFFER bf-estCostDetail). 
    

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
    
    FIND FIRST estCostSummary EXCLUSIVE-LOCK
        WHERE estCostSummary.scopeRecKey EQ ipcScopeRecKey
        AND estCostSummary.estCostGroupID EQ ipcGroupID
        NO-ERROR.
    IF NOT AVAILABLE estCostSummary THEN 
    DO:
        CREATE estCostSummary.
        ASSIGN 
            estCostSummary.estCostGroupID  = ipcGroupID
            estCostSummary.scopeRecKey     = ipcScopeRecKey
            estCostSummary.estCostHeaderID = ipiEstCostHeaderID
            .

    END.
    estCostSummary.costTotal = estCostSummary.costTotal + ipdCost.
    IF ipdQtyPerM GT 0 THEN 
        estCostSummary.costTotalPerMFinished  = estCostSummary.costTotalPerMFinished + ipdCost / ipdQtyPerM.

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
    DEFINE PARAMETER BUFFER ipbf-eb            FOR eb.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE PARAMETER BUFFER opbf-estCostBlank  FOR estCostBlank.
 
    CREATE opbf-estCostBlank.
    ASSIGN 
        opbf-estCostBlank.company            = ipbf-estCostForm.company
        opbf-estCostBlank.estCostFormID      = ipbf-estCostForm.estCostFormID
        opbf-estCostBlank.estCostHeaderID    = ipbf-estCostForm.estCostHeaderID
        opbf-estCostBlank.formNo             = ipbf-eb.form-no
        opbf-estCostBlank.blankNo            = ipbf-eb.blank-no
        opbf-estCostBlank.numOutLength       = ipbf-eb.num-len
        opbf-estCostBlank.numOutWidth        = ipbf-eb.num-wid
        opbf-estCostBlank.numOutDepth        = ipbf-eb.num-dep
        opbf-estCostBlank.numOut             = MAX(opbf-estCostBlank.numOutWidth, 1) * MAX(opbf-estCostBlank.numOutLength, 1) * MAX(opbf-estCostBlank.numOutDepth, 1)
        opbf-estCostBlank.blankWidth         = ipbf-eb.t-wid
        opbf-estCostBlank.blankLength        = ipbf-eb.t-len
        opbf-estCostBlank.blankDepth         = ipbf-eb.t-dep
        opbf-estCostBlank.blankArea          = ipbf-eb.t-sqin
        opbf-estCostBlank.dimLength          = ipbf-eb.len
        opbf-estCostBlank.dimWidth           = ipbf-eb.wid
        opbf-estCostBlank.dimDepth           = ipbf-eb.dep
                                                
        /*Refactor - Hardcoded*/
        opbf-estCostBlank.areaUOM            = "SQIN"
        opbf-estCostBlank.dimUOM             = "IN"
        opbf-estCostBlank.weightUOM          = "LB"
                    
        /*Refactor - apply area UOM conversion*/
        opbf-estCostBlank.weight             = ipbf-estCostForm.basisWeight * opbf-estCostBlank.blankArea / 144000 
                            
        /*Refactor - Calculate Windowing*/
        opbf-estCostBlank.blankAreaNetWindow = opbf-estCostBlank.blankArea
        
        opbf-estCostBlank.quantityRequired   = IF ipbf-estCostHeader.estType EQ gcTypeCombo THEN ipbf-eb.bl-qty ELSE ipbf-estCostHeader.quantityMaster
        opbf-estCostBlank.quantityYielded    = ipbf-eb.yld-qty
        .
        
    FIND FIRST estCostItem EXCLUSIVE-LOCK 
        WHERE estCostItem.estCostHeaderID EQ opbf-estCostBlank.estCostHeaderID
        AND estCostItem.customerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE estCostItem THEN 
    DO:
        ASSIGN 
            opbf-estCostBlank.estCostItemID    = estCostItem.estCostItemID
            estCostItem.sizeDesc               = TRIM(STRING(opbf-estCostBlank.dimLength,">>>9.99")) + " x " + TRIM(STRING(opbf-estCostBlank.dimWidth,">>>9.99"))
            opbf-estCostBlank.quantityPerSet   = MAX(estCostItem.quantityPerSet, 1)
            opbf-estCostBlank.quantityRequired = opbf-estCostBlank.quantityRequired * opbf-estCostBlank.quantityPerSet
            opbf-estCostBlank.quantityYielded  = opbf-estCostBlank.quantityYielded * opbf-estCostBlank.quantityPerSet
            .
        IF opbf-estCostBlank.dimDepth NE 0 THEN 
            estCostItem.sizeDesc = estCostItem.sizeDesc + " x " + TRIM(STRING(opbf-estCostBlank.dimDepth,">>>9.99")).
    END.
    ASSIGN 
        ipbf-estCostForm.numOutBlanksOnNet = ipbf-estCostForm.numOutBlanksOnNet + opbf-estCostBlank.numOut
        ipbf-estCostForm.blankArea         = ipbf-estCostForm.blankArea + opbf-estCostBlank.blankArea * opbf-estCostBlank.numOut
        . 

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-estCostForm FOR estCostForm.
    
    CREATE opbf-estCostForm.
    ASSIGN 
        opbf-estCostForm.estCostHeaderID = ipbf-estCostHeader.estCostHeaderID
        opbf-estCostForm.estimateNo      = ipbf-estCostHeader.estimateNo
        opbf-estCostForm.company         = ipbf-estCostHeader.company
        opbf-estCostForm.formNo          = ipiFormNo
        .
END PROCEDURE.

PROCEDURE pAddEstFormFromEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader. 
    DEFINE PARAMETER BUFFER opbf-estCostForm   FOR estCostForm.

    RUN pAddEstForm(BUFFER ipbf-estCostHeader, ipbf-ef.form-no, BUFFER opbf-estCostForm).
    
    ASSIGN 
        opbf-estCostForm.numOutNetLength                = MAX(ipbf-ef.n-out-l, 1)
        opbf-estCostForm.numOutNetWidth                 = MAX(ipbf-ef.n-out, 1)
        opbf-estCostForm.numOutNetDepth                 = MAX(ipbf-ef.n-out-d, 1)
        opbf-estCostForm.numOutNet                      = opbf-estCostForm.numOutNetLength * opbf-estCostForm.numOutNetWidth * opbf-estCostForm.numOutNetDepth
        opbf-estCostForm.grossWidth                     = ipbf-ef.gsh-wid 
        opbf-estCostForm.grossLength                    = ipbf-ef.gsh-len
        opbf-estCostForm.grossDepth                     = ipbf-ef.gsh-dep 
        opbf-estCostForm.netWidth                       = ipbf-ef.nsh-wid
        opbf-estCostForm.netLength                      = ipbf-ef.nsh-len
        opbf-estCostForm.netDepth                       = ipbf-ef.nsh-dep
        opbf-estCostForm.dieWidth                       = ipbf-ef.trim-w
        opbf-estCostForm.dieLength                      = ipbf-ef.trim-l
        opbf-estCostForm.basisWeight                    = ipbf-ef.weight
        opbf-estCostForm.company                        = ipbf-ef.company     
        opbf-estCostForm.costOverridePerUOM             = ipbf-ef.cost-msh
        opbf-estCostForm.costOverrideUOM                = ipbf-ef.cost-uom  
        opbf-estCostForm.noCost                         = NOT ipbf-ef.nc
        /*Refactor - handle when ef.roll is yes see ce/print4p.i*/
                
        /*Refactor- Hard-codes*/
        opbf-estCostForm.dimUOM                         = "IN"
        opbf-estCostForm.areaUOM                        = "SF"
        opbf-estCostForm.weightDieUOM                   = "LB/MSHT"
        opbf-estCostForm.weightNetUOM                   = "LB/MSHT"
        opbf-estCostForm.weightGrossUOM                 = "LB/MSHT"
        opbf-estCostForm.grossQtyRequiredTotalWeightUOM = "LBS"
        opbf-estCostForm.grossQtyRequiredTotalAreaUOM   = "MSF"
            
               
        /*Refactor - Formulas/Conversions - don't assume SF and inches*/
        opbf-estCostForm.grossArea                      = opbf-estCostForm.grossWidth * opbf-estCostForm.grossLength / 144
        opbf-estCostForm.netArea                        = opbf-estCostForm.netWidth * opbf-estCostForm.netLength / 144
        opbf-estCostForm.dieArea                        = opbf-estCostForm.dieWidth * opbf-estCostForm.dieLength / 144
        
        opbf-estCostForm.weightDie                      = opbf-estCostForm.basisWeight * opbf-estCostForm.dieArea 
        opbf-estCostForm.weightNet                      = opbf-estCostForm.basisWeight * opbf-estCostForm.netArea 
        opbf-estCostForm.weightGross                    = opbf-estCostForm.basisWeight * opbf-estCostForm.grossArea
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
        .
    
    IF ipbf-eb.est-type LT 5 THEN
        opbf-estCostItem.quantityPerSet     = MAX(ipbf-eb.cust-%, 1). 
    ELSE         
        opbf-estCostItem.quantityPerSet     = ipbf-eb.quantityPerSet.
    IF opbf-estCostItem.quantityPerSet LT 0 THEN 
        opbf-estCostItem.quantityPerSet     = 1 / opbf-estCostItem.quantityPerSet.
    IF opbf-estCostItem.quantityPerSet EQ 0 THEN 
        opbf-estCostItem.quantityPerSet     = 1 .
     
    opbf-estCostItem.quantityRequired      = ipbf-estCostHeader.quantityMaster * opbf-estCostItem.quantityPerSet.
            
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
            opbf-estCostItem.salesgroupName = sman.sname
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

PROCEDURE pAddEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create estCostMaterial from an item buffer and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT PARAMETER ipcRMItemID AS CHARACTER.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER.
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
            opbf-estCostMaterial.estCostFormID    = ipbf-estCostForm.estCostFormID
            opbf-estCostMaterial.estCostHeaderID  = ipbf-estCostForm.estCostHeaderID
            opbf-estCostMaterial.company          = bf-item.company
            opbf-estCostMaterial.estimateNo       = ipbf-estCostForm.estimateNo
            opbf-estCostMaterial.formNo           = ipbf-estCostForm.formNo
            opbf-estCostMaterial.blankNo          = ipiBlankNo
            opbf-estCostMaterial.itemID           = bf-item.i-no 
            opbf-estCostMaterial.itemName         = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name 
            opbf-estCostMaterial.quantityUOM      = CAPS(bf-item.cons-uom)
            opbf-estCostMaterial.quantityUOMWaste = opbf-estCostMaterial.quantityUOM
            opbf-estCostMaterial.basisWeight      = bf-item.basis-w
            opbf-estCostMaterial.basisWeightUOM   = "LB/MSF"
            opbf-estCostMaterial.dimLength        = bf-item.s-len
            opbf-estCostMaterial.dimWidth         = bf-item.s-wid
            opbf-estCostMaterial.dimDepth         = bf-item.s-dep
            opbf-estCostMaterial.dimUOM           = "IN"
            opbf-estCostMaterial.costPerUOMAvg    = bf-item.avg-cost
            opbf-estCostMaterial.costPerUOMLast   = bf-item.last-cost
            opbf-estCostMaterial.isRealMaterial   = bf-item.i-code EQ "R"
            opbf-estCostMaterial.materialType     = bf-item.mat-type
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
            
        IF opbf-estCostMaterial.blankNo NE 0 THEN 
        DO:
            opbf-estCostMaterial.estCostBlankID = fGetEstBlankID(opbf-estCostMaterial.estCostHeaderID,opbf-estCostMaterial.estCostFormID, opbf-estCostMaterial.blankNo).
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostBlankID EQ opbf-estCostMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE estCostBlank AND NOT opbf-estCostMaterial.isRealMaterial THEN 
                ASSIGN 
                    opbf-estCostMaterial.dimLength = estCostBlank.blankLength
                    opbf-estCostMaterial.dimWidth  = estCostBlank.blankWidth
                    opbf-estCostMaterial.dimLength = estCostBlank.blankLength
                    .
        END.
        ELSE IF NOT opbf-estCostMaterial.isRealMaterial THEN 
                ASSIGN 
                    opbf-estCostMaterial.dimLength = ipbf-estCostForm.grossLength
                    opbf-estCostMaterial.dimWidth  = ipbf-estCostForm.grossWidth
                    opbf-estCostMaterial.dimDepth  = ipbf-estCostForm.grossDepth
                    .
        
    END.
    
END PROCEDURE.

PROCEDURE pAddEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an estCostMisc and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    DEFINE PARAMETER BUFFER opbf-estCostMisc FOR estCostMisc.

    CREATE opbf-estCostMisc.
    ASSIGN 
        opbf-estCostMisc.estCostHeaderID = ipbf-estCostForm.estCostHeaderID
        opbf-estCostMisc.estCostFormID   = ipbf-estCostForm.estCostFormID
        opbf-estCostMisc.company         = ipbf-estCostForm.company
        opbf-estCostMisc.estimateNo      = ipbf-estCostForm.estimateNo
        .
END PROCEDURE.

PROCEDURE pAddEstMiscForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a form buffer and other key fields, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostMisc FOR estCostMisc.
    
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
    
    IF iBlankNo NE 0 THEN DO:
        FIND FIRST estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID
            AND estCostBlank.blankNo EQ iBlankNo
            NO-ERROR.
        IF AVAILABLE estCostBlank THEN 
            dQty = estCostBlank.quantityRequired. 
    END.
    IF dQty EQ 0 THEN 
        dQty = ipbf-estCostForm.quantityFGOnForm. 
        
    RUN pGetMiscCostPerM(BUFFER ipbf-ef, dQty, ipiIndex, UPPER(cCostType), OUTPUT dCostPerM).
    
    IF dCostPerM GT 0 OR dCostSetup GT 0 THEN 
    DO:
        RUN pAddEstMisc(BUFFER ipbf-estCostForm, BUFFER bf-estCostMisc).
       
        ASSIGN 
            bf-estCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
            bf-estCostMisc.formNo                = ipbf-ef.mis-snum[ipiIndex]  
            bf-estCostMisc.blankNo               = iBlankNo
            bf-estCostMisc.costDescription       = ipbf-ef.mis-cost[ipiIndex]
            bf-estCostMisc.costType              = cCostType
            bf-estCostMisc.costUOM               = "M"
            bf-estCostMisc.costPerUOM            = dCostPerM
            bf-estCostMisc.costSetup             = dCostSetup
            bf-estCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-estCostMisc.SIMON                 = ipbf-ef.mis-simon[ipiIndex]
            bf-estCostMisc.profitPercent         = ipbf-ef.mis-mkup[ipiIndex]
            bf-estCostMisc.sourcequantity        = dQty
            bf-estCostMisc.quantityPerSourceQty  = 1
            bf-estCostMisc.quantityRequiredTotal = dQty
            bf-estCostMisc.quantityUOM           = "EA"
            bf-estCostMisc.costTotalBeforeProfit = dCostPerM * dQty / 1000 + dCostSetup
            .
        RUN pCalcEstMisc(BUFFER bf-estCostMisc, BUFFER ipbf-estCostForm).
        
    END.
END PROCEDURE.

PROCEDURE pAddEstMiscForPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a est-prep buffer and quantity, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-prep    FOR est-prep.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    
    DEFINE BUFFER bf-estCostMisc FOR estCostMisc.
    
    RUN pAddEstMisc(BUFFER ipbf-estCostForm, BUFFER bf-estCostMisc).
    
    ASSIGN 
        bf-estCostMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
        bf-estCostMisc.formNo                = ipbf-est-prep.s-num  
        bf-estCostMisc.blankNo               = ipbf-est-prep.b-num
        bf-estCostMisc.prepID                = ipbf-est-prep.code
        bf-estCostMisc.costDescription       = ipbf-est-prep.dscr
        bf-estCostMisc.costType              = IF ipbf-est-prep.ml THEN "Mat" ELSE "Lab"
        bf-estCostMisc.profitPercentType     = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
        bf-estCostMisc.SIMON                 = ipbf-est-prep.simon
        bf-estCostMisc.profitPercent         = ipbf-est-prep.mkup
        bf-estCostMisc.sourcequantity        = ipbf-est-prep.qty
        bf-estCostMisc.quantityPerSourceQty  = 1
        bf-estCostMisc.quantityRequiredTotal = bf-estCostMisc.sourceQuantity * bf-estCostMisc.quantityPerSourceQty
        bf-estCostMisc.quantityUOM           = "EA"
        bf-estCostMisc.costUOM               = "EA"
        bf-estCostMisc.costPerUOM            = ipbf-est-prep.cost
        bf-estCostMisc.costSetup             = 0
        bf-estCostMisc.costTotalBeforeProfit = bf-estCostMisc.costPerUOM * bf-estCostMisc.quantityRequiredTotal + bf-estCostMisc.costSetup
        bf-estCostMisc.isPrep                = YES
        .
    
    RUN pCalcEstMisc(BUFFER bf-estCostMisc, BUFFER ipbf-estCostForm).

END PROCEDURE.

PROCEDURE pAddEstOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an estCostOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op           FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER opbf-estCostOperation FOR estCostOperation.

    DEFINE           BUFFER bf-mach               FOR mach.
    DEFINE           BUFFER bf-est-op             FOR est-op.

    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
    DO:
        CREATE opbf-estCostOperation.
        ASSIGN 
            opbf-estCostOperation.estCostFormID                = ipbf-estCostForm.estCostFormID
            opbf-estCostOperation.estCostHeaderID              = ipbf-estCostForm.estCostHeaderID
            opbf-estCostOperation.company                      = ipbf-est-op.company
            opbf-estCostOperation.estimateNo                   = ipbf-est-op.est-no
            opbf-estCostOperation.formNo                       = ipbf-est-op.s-num
            opbf-estCostOperation.blankNo                      = ipbf-est-op.b-num
            opbf-estCostOperation.estCostBlankID               = fGetEstBlankID(opbf-estCostOperation.estCostBlankID, opbf-estCostOperation.estCostFormID, opbf-estCostOperation.blankNo)
            opbf-estCostOperation.operationID                  = ipbf-est-op.m-code
            opbf-estCostOperation.pass                         = MAX(ipbf-est-op.op-pass, 1)
            opbf-estCostOperation.sequenceOfOperation          = ipbf-est-op.line
            opbf-estCostOperation.numOutDivisor                = ipbf-est-op.n_out_div
                       
            opbf-estCostOperation.quantityInSetupWaste         = ipbf-est-op.op-waste
            opbf-estCostOperation.hoursSetup                   = ipbf-est-op.op-mr
            opbf-estCostOperation.speed                        = ipbf-est-op.op-speed
            opbf-estCostOperation.quantityInRunWastePercent    = ipbf-est-op.op-spoil
            opbf-estCostOperation.isLocked                     = ipbf-est-op.isLocked
            opbf-estCostOperation.crewSizeSetup                = ipbf-est-op.op-crew[1]
            opbf-estCostOperation.crewSizeRun                  = ipbf-est-op.op-crew[2]
            opbf-estCostOperation.countInks                    = ipbf-est-op.num-col
            opbf-estCostOperation.countCoats                   = ipbf-est-op.num-coat
            opbf-estCostOperation.countFountainChanges         = ipbf-est-op.fountains
            opbf-estCostOperation.countPlateChanges            = ipbf-est-op.plates
            
            opbf-estCostOperation.isSpeedInLF                  = bf-mach.therm
            opbf-estCostOperation.operationName                = bf-mach.m-dscr
            opbf-estCostOperation.feedType                     = bf-mach.p-type
            opbf-estCostOperation.outputType                   = opbf-estCostOperation.feedType
            opbf-estCostOperation.departmentIDPrimary          = bf-mach.dept[1]
            opbf-estCostOperation.departmentID                 = bf-mach.dept
            opbf-estCostOperation.quantityInSetupWastePerColor = bf-mach.col-wastesh
          
            /*Refactor - this is where we can have a different rate for setup vs. run*/
            opbf-estCostOperation.costPerManHourDLRun          = bf-mach.lab-rate[bf-mach.lab-drate]
            opbf-estCostOperation.costPerManHourDLSetup        = bf-mach.lab-rate[bf-mach.lab-drate]
            
            opbf-estCostOperation.costPerHourFOSetup           = bf-mach.mr-fixoh
            opbf-estCostOperation.costPerHourFORun             = bf-mach.run-fixoh
            opbf-estCostOperation.costPerHourVOSetup           = bf-mach.mr-varoh
            opbf-estCostOperation.costPerHourVORun             = bf-mach.run-varoh
            opbf-estCostOperation.quantityInkLbsWastedPerSetup = bf-mach.ink-waste
            opbf-estCostOperation.quantityInkLbsWastedPerColor = bf-mach.col-wastelb
            .
       
        IF fIsDepartment("PR",opbf-estCostOperation.departmentID) THEN  
            opbf-estCostOperation.isPrinter = YES.
        IF fIsDepartment("CT",opbf-estCostOperation.departmentID) THEN  
            opbf-estCostOperation.isCoater = YES.
        IF fIsDepartment("RC,RS",opbf-estCostOperation.departmentID)  THEN 
            ASSIGN 
                opbf-estCostOperation.isNetSheetMaker = YES
                opbf-estCostOperation.outputType      = "S"
                .
        IF fIsDepartment("GL",opbf-estCostOperation.departmentID)  THEN 
            opbf-estCostOperation.isGluer = YES.
        IF fIsDepartment("WN,WS,FB,FS",opbf-estCostOperation.departmentID)  THEN 
            opbf-estCostOperation.isLeafer = YES.
        
        IF CAN-DO("R,S",opbf-estCostOperation.feedType) THEN 
        DO:
            FOR EACH bf-est-op NO-LOCK 
                WHERE bf-est-op.company EQ ipbf-est-op.company
                AND bf-est-op.est-no EQ ipbf-est-op.est-no
                AND bf-est-op.s-num EQ ipbf-est-op.s-num
                AND bf-est-op.qty EQ ipbf-est-op.qty
                AND bf-est-op.line GT ipbf-est-op.line
                AND bf-est-op.line LT 500,
                FIRST bf-mach NO-LOCK 
                WHERE bf-mach.company EQ bf-est-op.company
                AND bf-mach.m-code EQ bf-est-op.m-code 
                BY bf-est-op.line:
                IF bf-mach.p-type EQ "B" THEN  /*Last machine before a blank fed*/
                    ASSIGN 
                        opbf-estCostOperation.isBlankMaker = YES
                        opbf-estCostOperation.outputType   = "B"
                        .
                LEAVE.
            END.
            IF NOT AVAILABLE bf-est-op THEN /*Last Machine*/  
                ASSIGN 
                    opbf-estCostOperation.isBlankMaker = YES
                    opbf-estCostOperation.outputType   = "B"
                    .
        END.
        
        
        IF opbf-estCostOperation.isNetSheetMaker THEN 
            ASSIGN 
                opbf-estCostOperation.numOutForOperation = ipbf-estCostForm.numOutNet
                .
        ELSE IF opbf-estCostOperation.isBlankMaker THEN 
                ASSIGN 
                    opbf-estCostOperation.numOutForOperation = ipbf-estCostForm.numOutBlanksOnNet
                    .
            ELSE 
                ASSIGN 
                    opbf-estCostOperation.numOutForOperation = 1
                    .
        IF opbf-estCostOperation.blankNo NE 0 THEN 
        DO:
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ opbf-estCostOperation.estCostHeaderID
                AND estCostBlank.estCostFormID EQ opbf-estCostOperation.estCostFormID
                AND estCostBlank.blankNo EQ opbf-estCostOperation.blankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                opbf-estCostOperation.estCostBlankID = estCostBlank.estCostBlankID. 
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
    DEFINE OUTPUT PARAMETER opiEstCostHeaderID AS INT64 NO-UNDO.
       
    CREATE estCostHeader.
    ASSIGN 
        estCostHeader.calcDateTime   = NOW
        estCostHeader.calculatedBy   = USERID("ASI")
        estCostHeader.company        = ipbf-est.company
        estCostHeader.estimateNo     = ipbf-est.est-no
        estCostHeader.quantityMaster = ipdQtyMaster
        estCostHeader.releaseCount   = ipdReleases
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
                ttInk.cMaterialType    = bf-item.mat-type
                ttInk.dCoverageRate    = bf-item.yield
                ttInk.cCoverageRateUOM = "LBS/SQIN"
                ttInk.cPressType       = bf-item.press-type
                ttInk.cQtyUOM          = "LB"   
                ttInk.dMinLbsPerJob    = bf-item.min-lbs         
                .
        END.
        ASSIGN 
            ttInk.dCoveragePercent     = ttInk.dCoveragePercent + (ipdCoveragePercent / 100)
            ttInk.dQtyRequiredPerBlank = ttInk.dCoveragePercent * ipbf-estCostBlank.blankAreaNetWindow / ttInk.dCoverageRate
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

    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
            
    DEFINE BUFFER bf-item FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-estCostForm.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Leaf/Film RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-estCostForm.estCostHeaderID, ipbf-estCostForm.formNo, ipiBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcLeafMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Leaf/Film RM Code '" + ipcItemCode + "' is not one of '" + gcLeafMatTypes + "'", gcErrorImportant,ipbf-estCostForm.estCostHeaderID, ipbf-estCostForm.formNo, ipiBlankNo).
            RETURN.
        END.
        FIND FIRST ttLeaf EXCLUSIVE-LOCK 
            WHERE ttLeaf.estHeaderID EQ ipbf-estCostForm.estCostHeaderID
            AND ttLeaf.estFormID EQ ipbf-estCostForm.estCostFormID
            AND ttLeaf.iFormNo EQ ipbf-estCostForm.formNo
            AND ttLeaf.iBlankNo EQ ipiBlankNo
            AND ttLeaf.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE ttLeaf THEN 
        DO:
            CREATE ttLeaf.
            ASSIGN 
                ttLeaf.company             = ipbf-estCostForm.company
                ttLeaf.estHeaderID         = ipbf-estCostForm.estCostHeaderID
                ttLeaf.estFormID           = ipbf-estCostForm.estCostFormID
                ttLeaf.iFormNo             = ipbf-estCostForm.formNo
                ttLeaf.iBlankNo            = ipiBlankNo
                ttLeaf.cItemID             = ipcItemCode
                ttLeaf.cDescription        = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name
                ttLeaf.cMaterialType       = bf-item.mat-type
                ttLeaf.cQtyUOM             = IF bf-item.cons-uom EQ "" THEN "LB" ELSE bf-item.cons-uom    
                ttLeaf.dDimLength          = ipdLength
                ttLeaf.dDimWidth           = ipdWidth     
                ttLeaf.dAreaInSQIn         = ipdLength * ipdWidth
                ttLeaf.cDescription        = IF ipcDescription NE "" THEN ipcDescription ELSE ttLeaf.cDescription
                ttLeaf.dCoverageRate       = bf-item.sqin-lb
                ttLeaf.cCoverageRateUOM    = "SQIN/LB"
                ttLeaf.dQtyRequiredPerLeaf = ttLeaf.dAreaInSQIn / ttLeaf.dCoverageRate
                .
            
            FIND FIRST estCostBlank EXCLUSIVE-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND estCostBlank.estCostFormID EQ ttLeaf.estFormID
                AND estCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                ASSIGN 
                    estCostBlank.blankAreaWindow    = estCostBlank.blankAreaWindow + ttLeaf.dAreaInSQIn
                    estCostBlank.blankAreaNetWindow = estCostBlank.blankArea - estCostBlank.blankAreaWindow
                    .
            
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
                opbf-ttPack.dWeightTare           = bf-item.basis-w
                opbf-ttPack.iCountPerSubUnit      = bf-item.box-case
                opbf-ttPack.iCountSubUnitsPerUnit = bf-item.case-pall
                opbf-ttPack.dWeightCapacity       = bf-item.avg-w
                .
        END.
    
    END.

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
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matNoWaste","Board Cost - No Waste",
            ipbf-estCostMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matSetupWaste","Board Cost - Setup Waste",
            ipbf-estCostMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matRunWaste","Board Cost - Run Waste",
            ipbf-estCostMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matSetupVend","Board Cost - Vendor Setup",
            ipbf-estCostMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-estCostMaterial, "matMinDiff","Board Cost - Minimum Diff",
            ipbf-estCostMaterial.costTotalMinDiff,0).
    END.    
    
        
END PROCEDURE.

PROCEDURE pBuildCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostMisc FOR estCostMisc.
    
    DEFINE VARIABLE cCostBin AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cCostBin = IF ipbf-estCostMisc.isPrep THEN "P" ELSE "M"
        cCostBin = cCostBin + ipbf-estCostMisc.costType + ipbf-estCostMisc.SIMON.
    
    CASE cCostBin:
        WHEN "PLabI" THEN 
            DO:  /*PrepLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabProfit","Prep Labor - Profit",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatI" THEN  
            DO:  /*PrepMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatCost","Prep Material - Cost",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatProfit","Prep Material - Profit",
                    ipbf-estCostMisc.profitTotal,0).                    
            END. 
        WHEN "PLabM" THEN 
            DO:  /*PrepLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabPrice","Prep Labor - Profit - Price",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatM" THEN  
            DO:  /*PrepMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatCost","Prep Material - Cost",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatPrice","Prep Material - Profit - Price",
                    ipbf-estCostMisc.profitTotal,0).   
            END.                 
        WHEN "PLabS" OR 
        WHEN "PLabO" THEN 
            DO:  /*PrepLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabCostSep","Prep Labor - Cost - Separate",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pLabProfitSep","Prep Labor - Profit - Separate",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "PMatS" OR 
        WHEN "PMatO" THEN  
            DO:  /*PrepMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatCostSep","Prep Material - Cost - Separate",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "pMatProfitSep","Prep Material - Profit - Separate",
                    ipbf-estCostMisc.profitTotal,0).                    
            END. 
        WHEN "MLabI" THEN 
            DO:  /*MiscLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabProfit","Misc Labor - Profit - COGS",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatI" THEN  
            DO:  /*MiscMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatProfit","Misc Material - Profit - COGS",
                    ipbf-estCostMisc.profitTotal,0).                    
            END. 
        WHEN "MLabM" THEN 
            DO:  /*MiscLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabPrice","Misc Labor - Profit - Price",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatM" THEN  
            DO:  /*MiscMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatPrice","Misc Material - Profit - Price",
                    ipbf-estCostMisc.profitTotal,0).   
            END.                 
        WHEN "MLabS" OR 
        WHEN "MLabO" THEN 
            DO:  /*MiscLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabCostSep","Misc Labor - Cost - Separate",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mLabProfitSep","Misc Labor - Profit - Separate",
                    ipbf-estCostMisc.profitTotal,0).                    
            END.
        WHEN "MMatS" OR 
        WHEN "MMatO" THEN  
            DO:  /*MiscMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatCostSep","Misc Material - Cost - Separate",
                    ipbf-estCostMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-estCostMisc, "mMatProfitSep","Misc Material - Profit - Separate",
                    ipbf-estCostMisc.profitTotal,0).                    
            END. 
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

    DEFINE BUFFER bf-est     FOR est.
    DEFINE BUFFER bf-est-qty FOR est-qty.
    DEFINE VARIABLE iEstCostHeaderID AS INT64   NO-UNDO.
    DEFINE VARIABLE iQtyCount        AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstHeaderToCalc.
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipcCompany
        AND bf-est.est-no EQ ipcEstimateNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN RETURN.
    FIND FIRST bf-est-qty
        WHERE bf-est-qty.company EQ bf-est.company
        AND bf-est-qty.est-no  EQ bf-est.est-no
        NO-LOCK NO-ERROR.
    IF AVAIL bf-est-qty THEN 
    DO iQtyCount = 1 TO 20:
        IF bf-est-qty.qty[iQtyCount] NE 0 THEN 
        DO:
            RUN pAddHeader(BUFFER bf-est, bf-est-qty.qty[iQtyCount], bf-est-qty.qty[iQtyCount + 20], OUTPUT iEstCostHeaderID).
            CREATE ttEstHeaderToCalc.
            ASSIGN 
                ttEstHeaderToCalc.iEstCostHeaderID = iEstCostHeaderID.
        END.
    END.

END PROCEDURE.

PROCEDURE pBuildProbe PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostHeader, make a probe record for display in Print tab
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER ipdQuantity AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiLine AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dQtyInM AS DECIMAL NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF probe.
    
    ASSIGN 
        iopiLine = iopiLine + 1
        dQtyInM  = ipdQuantity / 1000
        .
    CREATE probe.

    ASSIGN
        probe.company      = ipbf-estCostHeader.company
        probe.est-no       = ipbf-estCostHeader.estimateNo
        probe.probe-date   = DATE(ipbf-estCostHeader.calcDateTime)
        probe.probe-time   = INTEGER(MTIME(ipbf-estCostHeader.calcDateTime) / 1000)
        probe.line         = iopiLine
        probe.probe-user   = ipbf-estCostHeader.calculatedBy
        probe.freight      = ipbf-estCostHeader.releaseCount             /* Holds Number of Releases */
        probe.est-qty      = ipdQuantity
        probe.fact-cost    = ipbf-estCostHeader.costTotalFactory / dQtyInM
        probe.full-cost    = ipbf-estCostHeader.costTotalFull / dQtyInM
        probe.gross-profit = ipbf-estCostHeader.profitPctGross
        probe.net-profit   = ipbf-estCostHeader.profitPctNet 
        probe.sell-price   = ipbf-estCostHeader.sellPrice / dQtyInM
        probe.spare-char-2 = STRING(ipbf-estCostHeader.estCostHeaderID)         
        probe.spare-dec-1  = ipbf-estCostHeader.costTotalMaterial / dQtyInM           
        .
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        :
        probe.gsh-qty  = probe.gsh-qty + estCostForm.grossQtyRequiredTotal.
    END.
        
        
END PROCEDURE.

PROCEDURE pBuildFreightCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Calculate Freight Cost Details to add to Non Factory Costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.

    DEFINE VARIABLE dNetProfitForForm AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dPriceForForm     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCommission       AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    DEFINE BUFFER bf-estCostForm   FOR estCostForm.
    
    FOR EACH bf-estCostForm EXCLUSIVE-LOCK
        WHERE bf-estCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostFormID EQ bf-estCostForm.estCostFormID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
        /*Calculate Freight*/

    END. 
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
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    DEFINE BUFFER bf-estCostForm   FOR estCostForm.
    
    FOR EACH bf-estCostForm EXCLUSIVE-LOCK
        WHERE bf-estCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostFormID EQ bf-estCostForm.estCostFormID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
        RUN pGetPriceAndProfitForForm(BUFFER bf-estCostForm, BUFFER estCostItem, OUTPUT dNetProfitForForm, OUTPUT dPriceForForm).
        RUN pAddCostDetail(bf-estCostForm.estCostHeaderID, bf-estCostForm.estCostFormID, "", bf-estCostForm.estCostFormID, 
            gcSourceTypeProfit, "pProfit", "Profit", dNetProfitForForm, 0, bf-estCostForm.company, bf-estCostForm.estimateNo, BUFFER bf-estCostDetail).
        RUN pCalcCostTotals(ipiEstCostHeaderID).

    END. 
    /*Calculate commission on sell price*/
    FOR EACH bf-estCostForm EXCLUSIVE-LOCK
        WHERE bf-estCostForm.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostFormID EQ bf-estCostForm.estCostFormID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
        RUN pAddCostDetail(bf-estCostForm.estCostHeaderID, bf-estCostForm.estCostFormID, estCostBlank.estCostBlankID, estCostItem.estCostItemID,
            gcSourceTypeNonFactory,"commission","Commission", (estCostItem.commissionPct / 100) * estCostItem.sellPrice, 0,
            bf-estCostForm.company, bf-estCostForm.estimateNo, BUFFER bf-estCostDetail ).
        RUN pCalcCostTotals(ipiEstCostHeaderID).    

    END. 
END PROCEDURE.

PROCEDURE pCalculateHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Main Build of data from Estimate
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiProbeLine AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-estCostForm       FOR estCostForm.
    DEFINE BUFFER bf-estCostBlank      FOR estCostBlank.
    DEFINE BUFFER bf-estCostMaterial   FOR estCostMaterial.
    DEFINE BUFFER bf-estCostOperation  FOR estCostOperation.
    DEFINE BUFFER bf-estCostGroupLevel FOR estCostGroupLevel.
    DEFINE BUFFER bf-estCostGroup      FOR estCostGroup.
    DEFINE BUFFER bf-estCostCategory   FOR estCostCategory.
    DEFINE BUFFER bf-estCostHeader     FOR estCostHeader.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyOnForm          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyMaster          AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
         
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ estCostHeader.company
            AND est.est-no EQ estCostHeader.estimateNo
            NO-ERROR.
        
        IF NOT AVAILABLE est THEN RETURN.

        RUN pBuildItems(BUFFER estCostHeader).
        dQtyMaster          = 0.
        
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no:
            
            RUN pAddEstFormFromEf(BUFFER ef, BUFFER estCostHeader, BUFFER bf-estCostForm).
            
            ASSIGN 
                iNumOutBlanksOnForm = 0
                dQtyOnForm          = 0
                .
            
            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER estCostHeader, BUFFER bf-estCostForm, BUFFER bf-estCostBlank).
                ASSIGN 
                    iNumOutBlanksOnForm = iNumOutBlanksOnForm + bf-estCostBlank.numOut
                    dQtyOnForm          = dQtyOnForm + bf-estCostBlank.quantityRequired
                    .
                RUN pBuildInksForEb(BUFFER estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                RUN pAddGlue(BUFFER estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                RUN pBuildPackingForEb(BUFFER estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                
            END. /*Each eb of ef*/
            
            ASSIGN 
                bf-estCostForm.numOut                  = iNumOutBlanksOnForm * bf-estCostForm.numOutNet
                bf-estCostForm.quantityFGOnForm        = dQtyOnForm
                dQtyMaster                             = dQtyMaster + dQtyOnForm
                bf-estCostForm.grossQtyRequiredNoWaste = fRoundUp(bf-estCostForm.quantityFGOnForm / bf-estCostForm.numOut)
                .
            /* if Combo, update the master quantity for per M calculations*/
            IF estCostHeader.estType EQ gcTypeCombo THEN DO:
                 
            END.                            
            RUN pCalcBlankPct(BUFFER bf-estCostForm).                
            RUN pProcessOperations(BUFFER estCostHeader, BUFFER bf-estCostForm).
            RUN pProcessLeafs(BUFFER ef, BUFFER estCostHeader, BUFFER bf-estCostForm).
            RUN pProcessBoard(BUFFER estCostHeader, BUFFER bf-estCostForm, ef.board).           
            RUN pProcessInks(BUFFER estCostHeader, BUFFER bf-estCostForm).
            RUN pProcessPacking(BUFFER estCostHeader, BUFFER bf-estCostForm).
            RUN pProcessGlues(BUFFER estCostHeader, BUFFER bf-estCostForm).
            RUN pProcessSpecialMaterials(BUFFER ef, BUFFER estCostHeader, BUFFER bf-estCostForm).  
            RUN pProcessMiscPrep(BUFFER ef, BUFFER bf-estCostForm).
            RUN pProcessMiscNonPrep(BUFFER ef, BUFFER bf-estCostForm).          
        END.  /*Each ef of est*/  
        
        RUN pBuildFactoryCostDetails(estCostHeader.estCostHeaderID).
        RUN pBuildNonFactoryCostDetails(estCostHeader.estCostHeaderID).
        RUN pBuildPriceRelatedCostDetails(estCostHeader.estCostHeaderID).
        RUN pBuildCostSummary(estCostHeader.estCostHeaderID).
        RUN pBuildProbe(BUFFER estCostHeader, dQtyMaster, INPUT-OUTPUT iopiProbeLine).
        
    END. /*each estCostHeader*/

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
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    DEFINE BUFFER bf-ce-ctrl       FOR ce-ctrl.
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    FOR EACH bf-estCostHeader EXCLUSIVE-LOCK
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID,
        EACH estCostForm NO-LOCK
        WHERE estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID, 
        FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ bf-estCostHeader.company
        AND bf-ce-ctrl.loc EQ bf-estCostHeader.warehouseID:
        IF bf-estCostHeader.directMaterialPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfMatMarkup", "Direct Material Markup", estCostForm.costTotalMaterial * bf-estCostHeader.directMaterialPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        DO iIndex = 1 TO 6:
            bf-estCostHeader.gsaMaterialPct = bf-ce-ctrl.mat-pct[iIndex] / 100.
            IF bf-ce-ctrl.mat-cost[iIndex] GT estCostForm.costTotalMaterial THEN LEAVE.
        END. 
        IF bf-estCostHeader.gsaMaterialPct NE 0 THEN 
        DO:
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSAMat", "GSA Material", (estCostForm.costTotalMaterial - estCostForm.costTotalBoard) * bf-estCostHeader.gsaMaterialPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSABoard", "GSA Board", estCostForm.costTotalBoard * bf-estCostHeader.gsaMaterialPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        END.
        DO iIndex = 1 TO 6:
            bf-estCostHeader.gsaLaborPct = bf-ce-ctrl.lab-pct[iIndex] / 100.
            IF bf-ce-ctrl.lab-cost[iIndex] GT estCostForm.costTotalLabor THEN LEAVE.
        END. 
        IF bf-estCostHeader.gsaLaborPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSALab", "GSA Labor", estCostForm.costTotalLabor * bf-estCostHeader.gsaLaborPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        
        
        IF bf-estCostHeader.warehouseMarkupPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", estCostForm.costTotalFactory * bf-estCostHeader.warehouseMarkupPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        
        /*Note - currently a defect with the Folding % is zeroed out during the calc process - this is fix*/
        IF bf-estCostHeader.foldPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfFolding", "Folding", estCostForm.costTotalFactory * bf-estCostHeader.foldPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).      
        
                                
        IF bf-estCostHeader.special1MarkupPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", estCostForm.costTotalFactory * bf-estCostHeader.special1MarkupPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).    
        IF bf-estCostHeader.special2MarkupPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", estCostForm.costTotalFactory * bf-estCostHeader.special2MarkupPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        IF bf-estCostHeader.special3MarkupPct NE 0 THEN 
            RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", estCostForm.costTotalFactory * bf-estCostHeader.special3MarkupPct, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", bf-estCostHeader.special1FlatValue, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).    
        RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", bf-estCostHeader.special2FlatValue, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).
        RUN pAddCostDetail(estCostForm.estCostHeaderID, estCostForm.estCostFormID, "", estCostForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", bf-estCostHeader.special3FlatValue, 0, estCostForm.company, estCostForm.estimateNo, BUFFER bf-estCostDetail).                                                                                                   
    END. /*Each bf-estCostHeader*/    
    RUN pCalcCostTotals(ipiEstCostHeaderID).
END PROCEDURE.

PROCEDURE pBuildCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.

    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opSetupDL","Operation Setup DL",
        ipbf-estCostOperation.costTotalDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opSetupVO","Operation Setup VOH",
        ipbf-estCostOperation.costTotalVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opSetupFO","Operation Setup FOH",
        ipbf-estCostOperation.costTotalFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opRunDL","Operation Run DL",
        ipbf-estCostOperation.costTotalDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opRunVO","Operation Run VOH",
        ipbf-estCostOperation.costTotalVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opRunFO","Operation Run FOH",
        ipbf-estCostOperation.costTotalFORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opSetupMinDiff","Operation Setup - Min Charge Diff",
        ipbf-estCostOperation.costTotalMinDiffSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-estCostOperation, "opRunMinDiff","Operation Run - Min Charge Diff",
        ipbf-estCostOperation.costTotalMinDiffRun,0).
            
END PROCEDURE.

PROCEDURE pBuildFactoryCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the cost detail for all Factory Costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    /*Process Operations*/
    FOR EACH estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForOperation(BUFFER estCostOperation).
    END. /*Each estCostOperation for estHeader*/    
    /*Process Materials*/
    FOR EACH estCostMaterial NO-LOCK 
        WHERE estCostMaterial.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMaterial(BUFFER estCostMaterial).                  
                    
    END. /*Each estCostMaterial for estHeader*/
    FOR EACH estCostMisc NO-LOCK 
        WHERE estCostMisc.estCostHeaderID EQ ipiEstCostHeaderID:
        RUN pBuildCostDetailForMisc(BUFFER estCostMisc).                  
    END. /*Each estCostMaterial for estHeader*/
    RUN pCalcCostTotals(ipiEstCostHeaderID).
    
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
    
    DEFINE VARIABLE dLayerDepth   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDividerDepth AS DECIMAL NO-UNDO.        

    IF ipbf-estCostHeader.isUnitizedSet AND NOT ipbf-estCostBlank.formNo EQ 0 THEN 
        RETURN.   /*Ignore non-form 0 packing for unitized set*/
    
    /*Case*/
    RUN pAddPacking(BUFFER ipbf-estCostBlank, ipbf-eb.cas-no, BUFFER bf-ttPack).
    IF AVAILABLE bf-ttPack THEN 
        ASSIGN 
            bf-ttPack.dDimLength          = IF ipbf-eb.cas-len NE 0 THEN ipbf-eb.cas-len ELSE bf-ttPack.dDimLength
            bf-ttPack.dDimWidth           = IF ipbf-eb.cas-wid NE 0 THEN ipbf-eb.cas-wid ELSE bf-ttPack.dDimWidth
            bf-ttPack.dDimDepth           = IF ipbf-eb.cas-dep NE 0 THEN ipbf-eb.cas-dep ELSE bf-ttPack.dDimDepth
            bf-ttPack.iCountPerSubUnit    = IF ipbf-eb.cas-cnt NE 0 THEN ipbf-eb.cas-cnt ELSE bf-ttPack.iCountPerSubUnit
            bf-ttPack.dWeightCapacity     = IF ipbf-eb.cas-wt NE 0 THEN ipbf-eb.cas-wt ELSE bf-ttPack.dWeightCapacity
            bf-ttPack.dCostPerUOMOverride = ipbf-eb.cas-cost
            bf-ttPack.dQtyMultiplier      = MAX(ipbf-eb.spare-int-3, 1)
            bf-ttPack.lIsCase             = YES
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
    
/*Add additional packing processing here*/
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
            RUN pAddInk(BUFFER ipbf-estCostBlank, ipbf-eb.i-ps2[iIndex], ipbf-eb.i-code2[iIndex], ipbf-eb.i-dscr2[iIndex], ipbf-eb.i-%2[iIndex]).
    END.
    ELSE
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code):
        IF ipbf-eb.i-code[iIndex] GT "" THEN    
            RUN pAddInk(BUFFER ipbf-estCostBlank, ipbf-eb.i-ps[iIndex], ipbf-eb.i-code[iIndex], ipbf-eb.i-dscr[iIndex], ipbf-eb.i-%[iIndex]).
    END.

END PROCEDURE.

PROCEDURE pBuildLeafForEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all inks for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    DO iIndex = 1 TO 4:
        IF ipbf-ef.leaf[iIndex] NE "" THEN
            RUN pAddLeaf(BUFFER ipbf-estCostForm, ipbf-ef.leaf[iIndex], ipbf-ef.leaf-dscr[iIndex], ipbf-ef.leaf-bnum[iIndex], ipbf-ef.leaf-l[iIndex], ipbf-ef.leaf-w[iIndex]).
    END.
    

END PROCEDURE.

PROCEDURE pBuildItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.

    DEFINE           BUFFER bf-estCostItem     FOR estCostItem.
    DEFINE           BUFFER bf-estCostForm     FOR estCostForm.
    DEFINE           BUFFER bf-estCostBlank    FOR estCostBlank.
    
    DEFINE VARIABLE iEstItemIDSetHeader AS INT64 NO-UNDO.
    
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-estCostHeader.company
        AND eb.est-no EQ ipbf-estCostHeader.estimateNo
        BY eb.form-no: 
        FIND FIRST bf-estCostItem NO-LOCK
            WHERE bf-estCostItem.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            AND bf-estCostItem.customerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-estCostItem THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-estCostHeader, BUFFER bf-estCostItem).
            IF eb.form-no EQ 0 THEN 
            DO:
                RUN pAddEstForm(BUFFER ipbf-estCostHeader, 0, BUFFER bf-estCostForm).
                RUN pAddEstBlank(BUFFER eb, BUFFER ipbf-estCostHeader, BUFFER bf-estCostForm, BUFFER bf-estCostBlank).
                IF eb.unitized THEN 
                DO:
                    ipbf-estCostHeader.isUnitizedSet = YES. 
                    RUN pBuildPackingForEb(BUFFER ipbf-estCostHeader, BUFFER bf-estCostBlank, BUFFER eb).
                END.
                ASSIGN 
                    bf-estCostItem.isSet = YES
                    iEstItemIDSetHeader  = bf-estCostItem.estCostItemID
                    .
            END.     
            ELSE 
                bf-estCostItem.estCostItemIDParent = iEstItemIDSetHeader.           
        END. /*Create estCostItem*/
               
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pCalcBlankPct PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculates the share of that the blank will have on the form to 
     proportinately allocate form costs to each blank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    
    DEFINE VARIABLE dTotalBlankAreaOnForm AS DECIMAL.
    
    IF ipbf-estCostForm.blankArea GT 0 THEN 
        FOR EACH estCostBlank EXCLUSIVE-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID:
   
            estCostBlank.pctOfForm = estCostBlank.blankArea * estCostBlank.numOut / ipbf-estCostForm.blankArea. 
        END. 
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and category buffer, increment the appropriate fields
     on the form for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    {est\EstimateCostTotals.i &TableName=estCostForm}
    
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
    DEFINE PARAMETER BUFFER ipbf-estCostDetail   FOR estCostDetail.
    DEFINE PARAMETER BUFFER ipbf-estCostCategory FOR estCostCategory.
    DEFINE PARAMETER BUFFER ipbf-estCostForm     FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    
    RUN pCalcCostTotalsHeader(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostCategory, ipbf-estCostDetail.costTotal).
    RUN pCalcCostTotalsForm(BUFFER ipbf-estCostForm, BUFFER ipbf-estCostCategory, ipbf-estCostDetail.costTotal).
        
    FIND FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostDetail.estCostHeaderID
        AND estCostBlank.estCostFormID EQ ipbf-estCostDetail.estCostFormID
        AND estCostBlank.estCostBlankID EQ ipbf-estCostDetail.estCostBlankID
        NO-ERROR.
    IF AVAILABLE estCostBlank THEN 
    DO:  /*Blank-specific cost*/
        FIND FIRST estCostItem NO-LOCK 
            WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
            AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID
            NO-ERROR.
        IF AVAILABLE estCostItem THEN 
            RUN pCalcCostTotalsItem(BUFFER estCostItem, BUFFER ipbf-estCostCategory, ipbf-estCostDetail.costTotal).
    END.
    ELSE /*Divide up the Form-level Costs into each item*/
        FOR EACH estCostBlank NO-LOCK
            WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostDetail.estCostHeaderID
            AND estCostBlank.estCostFormID EQ ipbf-estCostDetail.estCostFormID, 
            FIRST estCostItem NO-LOCK  
            WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
            AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID :
            RUN pCalcCostTotalsItem(BUFFER estCostItem, BUFFER ipbf-estCostCategory, ipbf-estCostDetail.costTotal * estCostBlank.pctOfForm).
        END.

END PROCEDURE.

PROCEDURE pGetPriceAndProfitForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and blank, return the target margin percentage
     Notes: Should replace "CalcSellPrice.p and markup.p"
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostItem FOR estCostItem.
    DEFINE OUTPUT PARAMETER opdNetProfit AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdPrice AS DECIMAL NO-UNDO.

    DEFINE VARIABLE dLookup   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBoardPct AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dMargin   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cMarginOn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dProfit   AS DECIMAL   NO-UNDO.

    CASE gcMarginMatrixLookup:
        WHEN "Board Cost" THEN 
            dLookup = ipbf-estCostForm.costTotalBoard.
        WHEN "Factory Cost" THEN 
            dLookup = ipbf-estCostForm.costTotalFactory.
        WHEN "Full Cost" THEN 
            dlookup = ipbf-estCostForm.costTotalFull.
        OTHERWISE /*Square Feet*/ 
        dLookup   = ipbf-estCostForm.grossQtyRequiredTotalArea.
    END CASE. 
    IF ipbf-estCostForm.costTotalFactory NE 0 THEN 
        dBoardPct = ipbf-estCostForm.costTotalBoard / ipbf-estCostForm.costTotalFactory * 100.
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
            dMargin = cust.markup.
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
    CASE cMarginOn:
        WHEN "B" THEN 
            dCost = ipbf-estCostForm.costTotalBoard.
        WHEN "N" THEN
            dCost = ipbf-estCostForm.costTotalFull.
        OTHERWISE /*G*/
        dCost = ipbf-estCostForm.costTotalFactory.
    END CASE.
    ASSIGN        
        dProfit      = fGetProfit(dCost, dMargin, "Margin")
        opdPrice     = dProfit + dCost
        opdNetProfit = opdPrice - ipbf-estCostForm.costTotalFull
        .

END PROCEDURE.

PROCEDURE pProcessMiscNonPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the estCostMisc for non-prep mis items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DO iIndex = 1 TO 6:
        IF ipbf-ef.mis-cost[iIndex] EQ "" THEN NEXT.
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-estCostForm, iIndex, "Material").
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-estCostForm, iIndex, "Labor").
    END.

END PROCEDURE.

PROCEDURE pProcessMiscPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the estCostMisc for prep misc items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    
    FOR EACH est-prep NO-LOCK 
        WHERE est-prep.company EQ ipbf-ef.company
        AND est-prep.est-no EQ ipbf-ef.est-no
        AND est-prep.s-num EQ ipbf-ef.form-no
        AND est-prep.code NE "":
        RUN pAddEstMiscForPrep(BUFFER est-prep, BUFFER ipbf-estCostForm).
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
                FIND FIRST estCostHeader EXCLUSIVE-LOCK 
                    WHERE estCostHeader.estCostHeaderID EQ INT64(ipcData[1])
                    NO-ERROR.
                IF NOT AVAILABLE estCostHeader THEN CREATE estCostHeader.
                ASSIGN 
                    estCostHeader.estCostHeaderID = INT64(ipcData[1])
                    estCostHeader.company         = FILL("0",3 - LENGTH(ipcData[2])) + ipcData[2]
                    estCostHeader.estimateNo      = FILL(" ",8 - LENGTH(ipcData[3])) + ipcData[3]
                    estCostHeader.estType         = ipcData[4]
                    estCostHeader.quantityMaster  = DECIMAL(ipcData[5])
                    estCostHeader.calculatedBy    = ipcData[6]
                    estCostHeader.calcDateTime    = DATETIME(ipcData[7])
                    .
                RUN pBuildHeader(BUFFER estCostHeader).
            END.       
        WHEN "CostGroup" THEN 
            DO:
                CREATE estCostGroup.
                ASSIGN
                    estCostGroup.estCostGroupID      = ipcData[1]
                    estCostGroup.costGroupSequence   = INTEGER(ipcData[2])
                    estCostGroup.costGroupLabel      = ipcData[3]
                    estCostGroup.estCostGroupDesc    = ipcData[5]
                    estCostGroup.estCostGroupLevelID = INTEGER(ipcData[6])
//                    estCostGroup.rec_key             = fGetNextRecKey()
                    .
            END.
        WHEN "CostGroupLevel" THEN 
            DO:
                CREATE estCostGroupLevel.
                ASSIGN
                    estCostGroupLevel.estCostGroupLevelID   = INTEGER(ipcData[1])
                    estCostGroupLevel.estCostGroupLevelDesc = ipcData[2]
//                    estCostGroupLevel.rec_key               = fGetNextRecKey()
                    .
            END.
        WHEN "CostCategory" THEN 
            DO:
                CREATE estCostCategory.
                ASSIGN
                    estCostCategory.estCostCategoryID       = ipcData[1]
                    estCostCategory.costCategoryLabel       = ipcData[2]
                    estCostCategory.estCostCategoryDesc     = ipcData[3]
//                    ttEstCostCategory.cBasis                   = ipcData[4]
                    estCostCategory.estCostGroupID          = ipcData[5]
//                    ttEstCostCategory.cCostModel               = ipcData[6]
                    estCostCategory.includeInBoardCost      = ipcData[7] EQ "YES"
                    estCostCategory.includeInMaterialCost   = ipcData[8] EQ "YES"
                    estCostCategory.includeInLaborCost      = ipcData[9] EQ "YES"
                    estCostCategory.includeInFactoryCost    = ipcData[10] EQ "YES"
                    estCostCategory.includeInNonFactoryCost = ipcData[11] EQ "YES"
                    
                    .
            END.
        WHEN "Item" THEN 
            DO:
                CREATE estCostItem.
                ASSIGN
                    estCostItem.estCostItemID    = INT64(ipcData[1])
                    estCostItem.estCostHeaderID  = INT64(ipcData[2])
                    estCostItem.customerPart     = ipcData[3]
                    estCostItem.quantityPerSet   = DECIMAL(ipcData[4])
                    estCostItem.quantityRequired = DECIMAL(ipcData[5])
                    estCostItem.quantityYielded  = DECIMAL(ipcData[6])
                    estCostItem.itemName         = ipcData[7]
                    estCostItem.itemDescription1 = ipcData[8]
                    estCostItem.itemDescription2 = ipcData[9]
                    estCostItem.styleID          = ipcData[10]
                    estCostItem.styleDesc        = ipcData[11]
                    estCostItem.isSet            = LOGICAL(ipcData[12])
                    estCostItem.customerName     = ipcData[13]
                    estCostItem.customerAddress1 = ipcData[14]
                    estCostItem.customerAddress2 = ipcData[15]
                    estCostItem.customerAddress3 = ipcData[16]
                    estCostItem.shipToName       = ipcData[17]
                    estCostItem.shipToAddress1   = ipcData[18]
                    estCostItem.shipToAddress2   = ipcData[19]
                    estCostItem.shipToAddress3   = ipcData[20]
                    estCostItem.salesgroupName   = ipcData[21]
                    estCostItem.customerID       = ipcData[22]
                    estCostItem.shipToID         = ipcData[23]
                    estCostItem.salesgroupID     = ipcData[24]
                    estCostItem.colorDesc        = ipcData[25]
                    .
                    
            END.
    END CASE.
END PROCEDURE.

PROCEDURE pProcessOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a estCostHeader, build all estCostOperations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader  FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm    FOR estCostForm.
    
    DEFINE           BUFFER bf-estCostOperation FOR estCostOperation.
    DEFINE VARIABLE dQtyInOut           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget          AS DECIMAL NO-UNDO.
    
    dQtyInOut = ipbf-estCostForm.quantityFGOnForm.
    
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
    
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-estCostHeader.company
        AND est-op.est-no EQ ipbf-estCostHeader.estimateNo
        AND est-op.s-num EQ ipbf-estCostForm.formNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:

    /*REFACTOR to calculate quantities for combos*/        
    /*    IF est-op.b-num NE 0 THEN                                                     */
    /*    DO:  /*Calculate for Combo*/                                                  */
    /*        FIND FIRST estCostBlank NO-LOCK                                           */
    /*            WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID*/
    /*            AND estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID      */
    /*            AND estCostBlank.blankNo EQ est-op.b-num                              */
    /*            AND NOT estCostBlank.lOutputInitialized                               */
    /*            NO-ERROR.                                                             */
    /*        IF AVAILABLE estCostBlank THEN                                            */
    /*            ASSIGN                                                                */
    /*                estCostBlank.lOutputInitialized = YES                             */
    /*                dQtyInOut                     = estCostBlank.qtyRequired          */
    /*                .                                                                 */
    /*    END.                                                                          */
           
    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-estCostForm, BUFFER bf-estCostOperation).
                
    IF AVAILABLE bf-estCostOperation THEN 
    DO: 
        RUN pProcessOperation(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-estCostOperation, INPUT-OUTPUT dQtyInOut, 
            INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
        RUN pCalcEstOperation(BUFFER ipbf-estCostHeader, BUFFER bf-estCostOperation, BUFFER ipbf-estCostForm).                    
    END.
        
END.
ASSIGN 
    ipbf-estCostForm.grossQtyRequiredSetupWaste = dQtyInOutSetupWaste
    ipbf-estCostForm.grossQtyRequiredRunWaste   = dQtyInOutRunWaste
    ipbf-estCostForm.grossQtyRequiredTotal      = ipbf-estCostForm.grossQtyRequiredNoWaste + ipbf-estCostForm.grossQtyRequiredRunWaste + ipbf-estCostForm.grossQtyRequiredSetupWaste
    .
        
END PROCEDURE.

PROCEDURE pProcessSpecialMaterials PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an EF, processes the special materials (Misc/Sub) tab
     Notes: replaces ce/pr4-spe.p, ce/pr4-spe.i
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ef            FOR ef.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.

    DEFINE           BUFFER bf-item            FOR ITEM.
    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    DO iIndex = 1 TO 8:
        IF ipbf-ef.spec-no[iIndex] NE "" THEN 
        DO:
            RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ipbf-ef.spec-no[iIndex], 1, BUFFER bf-estCostMaterial).
            IF AVAILABLE bf-estCostMaterial THEN 
            DO: 
                
                /*REFACTOR - ugly.  work around to support more than 2 decimals as stored in db*/
                RUN custom/extradec.p (.0001, ipbf-ef.spec-qty[iIndex] * ipbf-estCostForm.quantityFGOnForm,
                    OUTPUT bf-estCostMaterial.quantityRequiredNoWaste).
                    
                ASSIGN            
                    bf-estCostMaterial.itemName    = ipbf-ef.spec-dscr[iIndex]
                    bf-estCostMaterial.quantityUOM = "EA"
                    .
                RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).
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
    
    DEFINE BUFFER bf-estCostDetail FOR estCostDetail.
    
    FOR EACH bf-estCostDetail EXCLUSIVE-LOCK
        WHERE bf-estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID
        AND NOT bf-estCostDetail.hasBeenProcessed, 
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.estCostHeaderID EQ bf-estCostDetail.estCostHeaderID,
        FIRST estCostForm NO-LOCK
        WHERE estCostForm.estCostHeaderID EQ bf-estCostDetail.estCostHeaderID
        AND estCostForm.estCostFormID EQ bf-estCostDetail.estCostFormID, 
        FIRST estCostCategory NO-LOCK 
        WHERE estCostCategory.estCostCategoryID EQ bf-estCostDetail.estCostCategoryID 
        :
        RUN pCalcTotalsForCostDetail(BUFFER bf-estCostDetail, BUFFER estCostCategory, BUFFER estCostForm, BUFFER estCostHeader).
        bf-estCostDetail.hasBeenProcessed = YES.
    END.        
        
END PROCEDURE.


PROCEDURE pBuildCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details for a given header and creates summary
              records for each
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    
    FOR EACH estCostDetail NO-LOCK
        WHERE estCostDetail.estCostHeaderID EQ ipiEstCostHeaderID, 
        FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.estCostHeaderID EQ estCostDetail.estCostHeaderID,
        FIRST estCostForm NO-LOCK
        WHERE estCostForm.estCostHeaderID EQ estCostDetail.estCostHeaderID
        AND estCostForm.estCostFormID EQ estCostDetail.estCostFormID, 
        FIRST estCostCategory NO-LOCK 
        WHERE estCostCategory.estCostCategoryID EQ estCostDetail.estCostCategoryID
        :
        
        RUN pAddCostSummary(estCostForm.rec_key, estCostCategory.estCostGroupID, estCostDetail.estCostHeaderID, estCostDetail.costTotal, estCostForm.quantityFGOnForm / 1000).
        
        FIND FIRST estCostBlank NO-LOCK 
            WHERE estCostBlank.estCostHeaderID EQ estCostDetail.estCostHeaderID
            AND estCostBlank.estCostFormID EQ estCostDetail.estCostFormID
            AND estCostBlank.estCostBlankID EQ estCostDetail.estCostBlankID
            NO-ERROR.
        IF AVAILABLE estCostBlank THEN 
        DO:  /*Blank-specific cost*/
            FIND FIRST estCostItem NO-LOCK 
                WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID
                NO-ERROR.
            IF AVAILABLE estCostItem THEN 
                RUN pAddCostSummary(estCostItem.rec_key, estCostCategory.estCostGroupID, estCostDetail.estCostHeaderID, estCostDetail.costTotal, estCostItem.quantityRequired / 1000).
           
        END.
        ELSE /*Divide up the Form-level Costs into each item*/
            FOR EACH estCostBlank NO-LOCK
                WHERE estCostBlank.estCostHeaderID EQ estCostDetail.estCostHeaderID
                AND estCostBlank.estCostFormID EQ estCostDetail.estCostFormID, 
                FIRST estCostItem NO-LOCK  
                WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                AND estCostItem.estCostItemID EQ estCostBlank.estCostItemID :
                RUN pAddCostSummary(estCostItem.rec_key, estCostCategory.estCostGroupID, estCostDetail.estCostHeaderID, estCostDetail.costTotal * estCostBlank.pctOfForm, estCostItem.quantityRequired / 1000).
           
            END.
        
    END.        


END PROCEDURE.


PROCEDURE pBuildTestData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the temptables to test with
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplReset AS LOGICAL NO-UNDO.
        
    DEFINE BUFFER bf-estCostForm       FOR estCostForm.
    DEFINE BUFFER bf-estCostBlank      FOR estCostBlank.
    DEFINE BUFFER bf-estCostMaterial   FOR estCostMaterial.
    DEFINE BUFFER bf-estCostOperation  FOR estCostOperation.
    DEFINE BUFFER bf-estCostGroupLevel FOR estCostGroupLevel.
    DEFINE BUFFER bf-estCostGroup      FOR estCostGroup.
    DEFINE BUFFER bf-estCostCategory   FOR estCostCategory.
    DEFINE BUFFER bf-estCostHeader     FOR estCostHeader.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyOnForm          AS DECIMAL NO-UNDO.
    

    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
    
    RUN pLoadData("EstHeader").
 
    IF NOT CAN-FIND(FIRST bf-estCostGroupLevel) OR iplReset THEN 
    DO: 
        FOR EACH bf-estCostGroupLevel:
            DELETE bf-estCostGroupLevel.
        END.
        RELEASE bf-estCostGroupLevel.
        RUN pLoadData("CostGroupLevel").
    END. 
    IF NOT CAN-FIND(FIRST bf-estCostGroup) OR iplReset THEN 
    DO: 
        FOR EACH bf-estCostGroup:
            DELETE bf-estCostGroup.
        END.
        RELEASE bf-estCostGroup.    
        RUN pLoadData("CostGroup").
    END.
    IF NOT CAN-FIND(FIRST bf-estCostCategory) OR iplReset THEN 
    DO: 
        FOR EACH bf-estCostCategory:
            DELETE bf-estCostCategory.
        END.
        RELEASE bf-estCostCategory.    
        RUN pLoadData("CostCategory").
    END.
   

    FIND FIRST estCostHeader NO-LOCK. 
    IF AVAILABLE estCostHeader THEN 
    DO:
        RUN pPurgeCalculation(estCostHeader.company, estCostHeader.estimateNo).
        RUN pCalculateEstimate(estCostHeader.company, estCostHeader.estimateNo).
    END.
        
END PROCEDURE.


PROCEDURE pCalcEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostMaterial buffer, calculate simple calculated fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
    DEFINE PARAMETER BUFFER ipbf-estCostForm     FOR estCostForm.

    ipbf-estCostMaterial.quantityRequiredTotal    = ipbf-estCostMaterial.quantityRequiredNoWaste + ipbf-estCostMaterial.quantityRequiredSetupWaste + 
        ipbf-estCostMaterial.quantityRequiredRunWaste + ipbf-estCostMaterial.quantityRequiredMinDiff.    
    IF ipbf-estCostMaterial.costOverridePerUOM EQ 0 THEN 
        RUN pGetEstMaterialCosts(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostMaterial,ipbf-estCostMaterial.quantityRequiredTotal,ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.vendorID, 
            OUTPUT ipbf-estCostMaterial.costPerUOM, OUTPUT ipbf-estCostMaterial.costUOM,  OUTPUT ipbf-estCostMaterial.costSetup ).
    ELSE 
        ipbf-estCostMaterial.costPerUOM = ipbf-estCostMaterial.costOverridePerUOM.
    IF ipbf-estCostMaterial.costUOM EQ "" THEN 
        ipbf-estCostMaterial.costUOM = "EA".
    IF ipbf-estCostMaterial.costUOM NE ipbf-estCostMaterial.quantityUOM THEN 
    DO:
        RUN custom/convquom.p(ipbf-estCostMaterial.company, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredNoWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM).
        RUN custom/convquom.p(ipbf-estCostMaterial.company, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredSetupWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredSetupWasteInCUOM).
        RUN custom/convquom.p(ipbf-estCostMaterial.company, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredRunWaste, OUTPUT ipbf-estCostMaterial.quantityRequiredRunWasteInCUOM).
        RUN custom/convquom.p(ipbf-estCostMaterial.company, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
            ipbf-estCostMaterial.basisWeight, ipbf-estCostMaterial.dimLength, ipbf-estCostMaterial.dimWidth, ipbf-estCostMaterial.dimDepth, 
            ipbf-estCostMaterial.quantityRequiredMinDiff, OUTPUT ipbf-estCostMaterial.quantityRequiredMinDiffInCUOM).
        RUN custom/convquom.p(ipbf-estCostMaterial.company, ipbf-estCostMaterial.quantityUOM, ipbf-estCostMaterial.costUOM, 
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
    ASSIGN 
        ipbf-estCostMaterial.costTotalNoWaste                = ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM * ipbf-estCostMaterial.costPerUOM
        ipbf-estCostMaterial.costTotalSetupWaste             = ipbf-estCostMaterial.quantityRequiredSetupWasteInCUOM * ipbf-estCostMaterial.costPerUOM + 
                                                             ipbf-estCostMaterial.costSetup
        ipbf-estCostMaterial.costTotalRunWaste               = ipbf-estCostMaterial.quantityRequiredRunWasteInCUOM * ipbf-estCostMaterial.costPerUOM
        ipbf-estCostMaterial.costTotalMinDiff                = ipbf-estCostMaterial.quantityRequiredMinDiffInCUOM * ipbf-estCostMaterial.costPerUOM
        ipbf-estCostMaterial.costTotal                       = ipbf-estCostMaterial.costTotalNoWaste + ipbf-estCostMaterial.costTotalSetupWaste + 
                                                             ipbf-estCostMaterial.costTotalRunWaste + ipbf-estCostMaterial.costTotalMinDiff
        ipbf-estCostMaterial.costTotalPerMFinished           = ipbf-estCostMaterial.costTotal / (ipbf-estCostForm.quantityFGOnForm / 1000)
        ipbf-estCostMaterial.costTotalPerMFinishedNoWaste    = ipbf-estCostMaterial.costTotalNoWaste / (ipbf-estCostForm.quantityFGOnForm / 1000)
        ipbf-estCostMaterial.costTotalPerMFinishedSetupWaste = ipbf-estCostMaterial.costTotalSetupWaste  / (ipbf-estCostForm.quantityFGOnForm / 1000)
        ipbf-estCostMaterial.costTotalPerMFinishedRunWaste   = ipbf-estCostMaterial.costTotalRunWaste / (ipbf-estCostForm.quantityFGOnForm / 1000)
        .        
    
END PROCEDURE.

PROCEDURE pCalcEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a estCostMisc buffer, calculate common fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostMisc FOR estCostMisc.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.

    ipbf-estCostMisc.profitTotal = fGetProfit(ipbf-estCostMisc.costTotalBeforeProfit, ipbf-estCostMisc.profitPercent, ipbf-estCostMisc.profitPercentType).
    IF ipbf-estCostMisc.SIMON EQ "M" THEN 
        ipbf-estCostMisc.costTotal = ipbf-estCostMisc.costTotalBeforeProfit.
    ELSE 
        ipbf-estCostMisc.costTotal = ipbf-estCostMisc.costTotalBeforeProfit + ipbf-estCostMisc.profitTotal.
    ipbf-estCostMisc.costTotalPerMFinished = ipbf-estCostMisc.costTotal / (ipbf-estCostForm.quantityFGOnForm / 1000).

END PROCEDURE.

PROCEDURE pCalcEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostOperation buffer, calculate simple calculated fields
     Notes: Should replace end if pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    
    DEFINE VARIABLE lApplyMinChargeOnRun   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lApplyMinChargeOnSetup AS LOGICAL NO-UNDO.
    
    IF ipbf-estCostOperation.speed NE 0 THEN
        IF ipbf-estCostOperation.isSpeedInLF THEN
            ipbf-estCostOperation.hoursRun = ipbf-estCostOperation.quantityInAfterSetupWasteLF / ipbf-estCostOperation.speed. 
        ELSE 
            ipbf-estCostOperation.hoursRun = ipbf-estCostOperation.quantityInAfterSetupWaste / ipbf-estCostOperation.speed.
    ELSE 
        ipbf-estCostOperation.hoursRun = 0.
    
    ASSIGN    
        ipbf-estCostOperation.costPerHourTotalRun   = ipbf-estCostOperation.costPerManHourDLRun * ipbf-estCostOperation.crewSizeRun + 
                                                     ipbf-estCostOperation.costPerHourFORun + ipbf-estCostOperation.costPerHourVORun
        ipbf-estCostOperation.costPerHourTotalSetup = ipbf-estCostOperation.costPerManHourDLSetup * ipbf-estCostOperation.crewSizeSetup + 
                                                     ipbf-estCostOperation.costPerHourFOSetup + ipbf-estCostOperation.costPerHourVOSetup
        ipbf-estCostOperation.costTotalDLSetup      = ipbf-estCostOperation.hoursSetup * ipbf-estCostOperation.crewSizeSetup * ipbf-estCostOperation.costPerManHourDLSetup
        ipbf-estCostOperation.costTotalVOSetup      = ipbf-estCostOperation.hoursSetup * ipbf-estCostOperation.costPerHourVOSetup
        ipbf-estCostOperation.costTotalFOSetup      = ipbf-estCostOperation.hoursSetup * ipbf-estCostOperation.costPerHourFOSetup
        ipbf-estCostOperation.costTotalDLRun        = ipbf-estCostOperation.hoursRun * ipbf-estCostOperation.crewSizeRun * ipbf-estCostOperation.costPerManHourDLRun
        ipbf-estCostOperation.costTotalVORun        = ipbf-estCostOperation.hoursRun * ipbf-estCostOperation.costPerHourVORun
        ipbf-estCostOperation.costTotalFORun        = ipbf-estCostOperation.hoursRun * ipbf-estCostOperation.costPerHourFORun
        ipbf-estCostOperation.costTotalSetup        = ipbf-estCostOperation.hoursSetup * ipbf-estCostOperation.costPerHourTotalSetup
        ipbf-estCostOperation.costTotalRun          = ipbf-estCostOperation.hoursRun * ipbf-estCostOperation.costPerHourTotalRun
        ipbf-estCostOperation.costTotal             = ipbf-estCostOperation.costTotalRun + ipbf-estCostOperation.costTotalSetup 
        .

END PROCEDURE.

PROCEDURE pProcessBoard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-item            FOR ITEM.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-estCostForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Board '" + ipcITemID + "' is not valid", gcErrorImportant, ipbf-estCostForm.estCostHeaderID, ipbf-estCostForm.formNo, 0).
        RETURN.
    END.
    IF NOT CAN-DO(gcBoardMatTypes,bf-item.mat-type) THEN 
    DO:
        RUN pAddError("Board '" + ipcITemID + "' is valid material but not a material type of " + gcBoardMatTypes, gcErrorImportant, ipbf-estCostForm.estCostHeaderID, ipbf-estCostForm.formNo, 0).
        RETURN.
    END.
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ipcItemID, 0, BUFFER bf-estCostMaterial).
    ASSIGN 
        bf-estCostMaterial.isPrimarySubstrate         = YES
        bf-estCostMaterial.addToWeightFG              = YES
        bf-estCostMaterial.addToWeightTare            = NO
                                       
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-estCostForm.grossQtyRequiredNoWaste
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-estCostForm.grossQtyRequiredSetupWaste
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-estCostForm.grossQtyRequiredRunWaste
        bf-estCostMaterial.quantityUOMWaste           = "EA"
        bf-estCostMaterial.quantityUOM                = "EA"
        bf-estCostMaterial.basisWeight                = bf-item.basis-w
        bf-estCostMaterial.dimWidth                   = ipbf-estCostForm.grossWidth
        bf-estCostMaterial.dimLength                  = ipbf-estCostForm.grossLength
        bf-estCostMaterial.dimDepth                   = ipbf-estCostForm.grossDepth
        .
    IF ipbf-estCostForm.costOverridePerUOM NE 0 THEN 
        ASSIGN 
            bf-estCostMaterial.costOverridePerUOM = ipbf-estCostForm.costOverridePerUOM
            bf-estCostMaterial.costUOM            = ipbf-estCostForm.costOverrideUOM
            .
        
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).   
    
    ASSIGN 
        ipbf-estCostForm.grossQtyRequiredTotal       = ipbf-estCostForm.grossQtyRequiredNoWaste + ipbf-estCostForm.grossQtyRequiredSetupWaste + ipbf-estCostForm.grossQtyRequiredRunWaste
        ipbf-estCostForm.grossQtyRequiredTotalArea   = ipbf-estCostForm.grossQtyRequiredTotal * ipbf-estCostForm.grossArea / 1000
        ipbf-estCostForm.grossQtyRequiredTotalWeight = ipbf-estCostForm.grossQtyRequiredTotalArea * ipbf-estCostForm.basisWeight
        .
    
    IF ipbf-estCostForm.noCost THEN 
        ASSIGN 
            bf-estCostMaterial.costPerUOM                      = 0
            bf-estCostMaterial.costSetup                       = 0
            bf-estCostMaterial.costTotal                       = 0
            bf-estCostMaterial.costTotalMinDiff                = 0
            bf-estCostMaterial.costTotalNoWaste                = 0
            bf-estCostMaterial.costTotalSetupWaste             = 0
            bf-estCostMaterial.costTotalRunWaste               = 0
            bf-estCostMaterial.costTotalPerMFinished           = 0
            bf-estCostMaterial.costTotalPerMFinishedNoWaste    = 0
            bf-estCostMaterial.costTotalPerMFinishedRunWaste   = 0
            bf-estCostMaterial.costTotalPerMFinishedSetupWaste = 0
            .
            
END PROCEDURE.

PROCEDURE pProcessGlues PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.

    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE VARIABLE dQtyRequiredMinDiff AS DECIMAL NO-UNDO.
    
    FOR FIRST estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND estCostOperation.isGluer, 
        EACH ttGlue NO-LOCK
        WHERE ttGlue.estHeaderID EQ estCostOperation.estCostHeaderID
        AND ttGlue.estFormID EQ estCostOperation.estCostFormID
        AND ttGlue.estBlankID EQ estCostOperation.estCostBlankID
        ,
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttGlue.estHeaderID
        AND estCostBlank.estCostFormID EQ ttGlue.estFormID 
        AND estCostBlank.estCostBlankID EQ ttGlue.estBlankID 
        BY estCostOperation.sequenceOfOperation DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ttGlue.cItemID, ttGlue.iBlankNo, BUFFER bf-estCostMaterial).
        
        ASSIGN    
            bf-estCostMaterial.quantityRequiredNoWaste    = estCostOperation.quantityInNoWaste * ttGlue.dQtyRequiredPerBlank
            bf-estCostMaterial.quantityRequiredRunWaste   = estCostOperation.quantityInRunWaste * ttGlue.dQtyRequiredPerBlank
            bf-estCostMaterial.quantityRequiredSetupWaste = estCostOperation.quantityInSetupWaste * ttGlue.dQtyRequiredPerBlank
            dQtyRequiredMinDiff                           = ttGlue.dMinLbsPerJob - 
                                                    (bf-estCostMaterial.quantityRequiredNoWaste + bf-estCostMaterial.quantityRequiredRunWaste + bf-estCostMaterial.quantityRequiredSetupWaste)
            bf-estCostMaterial.quantityUOM                = ttGlue.cQtyUOM
            .             
        IF dQtyRequiredMinDiff GT 0 THEN 
            bf-estCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).
        
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
        ipbf-estCostHeader.estType                     = ENTRY(bf-est.est-type, gcTypeList)
        ipbf-estCostHeader.warehouseID                 = bf-est.loc
        ipbf-estCostHeader.marginOn                    = bf-ce-ctrl.sell-by
        ipbf-estCostHeader.marginPct                   = bf-ce-ctrl.prof-mrkup
        ipbf-estCostHeader.warehouseMarkupPct          = bf-ce-ctrl.whse-mrkup / 100 /*ctrl[1]*/
        ipbf-estCostHeader.handlingChargePct           = bf-ce-ctrl.hand-pct / 100 /*ctrl[2]*/
        ipbf-estCostHeader.handlingRatePerCWTRMPct     = bf-ce-ctrl.rm-rate / 100 /*ctrl[3]*/ /*NOTE CHANGED to be /100 */
        
        ipbf-estCostHeader.special1MarkupPct           = bf-ce-ctrl.spec-%[1] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-estCostHeader.special1FlatValue           = 0 /*REFACTOR - treatment of Special Costs*/
        
        ipbf-estCostHeader.special2MarkupPct           = bf-ce-ctrl.spec-%[2] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-estCostHeader.special2FlatValue           = 0
        
        ipbf-estCostHeader.special3MarkupPct           = bf-ce-ctrl.spec-%[3] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-estCostHeader.special3FlatValue           = 0
        
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
        ipbf-estCostHeader.handlingRatePerCWTFGPct     = bf-ce-ctrl.fg-rate / 100  /*ld-fg-rate*/
        ipbf-estCostHeader.handlingRatePerCWTRMFarmPct = bf-ce-ctrl.rm-rate-farm / 100
        ipbf-estCostHeader.handlingRatePerCWTFGFarmPct = bf-ce-ctrl.fg-rate-farm / 100
        ipbf-estCostHeader.handlingChargeFarmPct       = bf-ce-ctrl.hand-pct-farm / 100
        ipbf-estCostHeader.directMaterialPct           = gdMaterialMarkup / 100                  
        .
        
END PROCEDURE.

PROCEDURE pProcessInk PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttInk/estCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank     FOR estCostBlank.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttInk            FOR ttInk.
    
 
    DEFINE           BUFFER bf-estCostMaterial    FOR estCostMaterial.
    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL.
    DEFINE VARIABLE dqtyRequiredMinDiff AS DECIMAL. 
        
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ipbf-ttInk.cItemID, ipbf-ttInk.iBlankNo, BUFFER bf-estCostMaterial).
        
    ASSIGN    
        dQtyRequiredPerForm                           = ipbf-estCostBlank.numOut * ipbf-ttInk.dQtyRequiredPerBlank
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-estCostOperation.quantityInNoWaste * dQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-estCostOperation.quantityInRunWaste * dQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-estCostOperation.quantityInSetupWaste * dQtyRequiredPerForm + ipbf-estCostOperation.quantityInkLbsWastedPerSetup
        dQtyRequiredMinDiff                           = ipbf-ttInk.dMinLbsPerJob - (bf-estCostMaterial.quantityRequiredNoWaste + bf-estCostMaterial.quantityRequiredRunWaste + bf-estCostMaterial.quantityRequiredSetupWaste)
        bf-estCostMaterial.quantityUOM                = ipbf-ttInk.cQtyUOM
        .             
    IF dQtyRequiredMinDiff GT 0 THEN 
        bf-estCostMaterial.quantityRequiredMinDiff = dQtyRequiredMinDiff.
    
    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).

END PROCEDURE.

PROCEDURE pProcessLeaf PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttLeaf/estCostOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-ttLeaf           FOR ttLeaf.
    DEFINE INPUT PARAMETER ipdQtyRequiredPerForm AS DECIMAL NO-UNDO.    
 
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
        
    RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ipbf-ttLeaf.cItemID, ipbf-ttLeaf.iBlankNo, BUFFER bf-estCostMaterial).
        
    ASSIGN    
        bf-estCostMaterial.dimLength                  = ipbf-ttLeaf.dDimLength
        bf-estCostMaterial.dimWidth                   = ipbf-ttLeaf.dDimWidth
        bf-estCostMaterial.quantityRequiredNoWaste    = ipbf-estCostOperation.quantityInNoWaste * ipdQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredRunWaste   = ipbf-estCostOperation.quantityInRunWaste * ipdQtyRequiredPerForm
        bf-estCostMaterial.quantityRequiredSetupWaste = ipbf-estCostOperation.quantityInSetupWaste * ipdQtyRequiredPerForm
        bf-estCostMaterial.quantityUOM                = ipbf-ttLeaf.cQtyUOM
        .             

    RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).

END PROCEDURE.

PROCEDURE pProcessInks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for inks with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.

    /*Inks*/
    FOR EACH estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND estCostOperation.isPrinter, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ estCostOperation.estCostHeaderID
        AND ttInk.estFormID EQ estCostOperation.estCostFormID
        AND ttInk.iPass EQ estCostOperation.pass
        AND ttInk.iCountInks GT 0,
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND estCostBlank.estCostFormID EQ ttInk.estFormID 
        AND estCostBlank.estCostBlankID EQ ttInk.estBlankID :
        
        RUN pProcessInk(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER estCostBlank, BUFFER estCostOperation, BUFFER ttInk).    
        
    END.
    /*Coatings*/
    FOR EACH estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND estCostOperation.isCoater, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ estCostOperation.estCostHeaderID
        AND ttInk.estFormID EQ estCostOperation.estCostFormID
        AND ttInk.iPass EQ estCostOperation.pass
        AND ttInk.iCountCoatings GT 0,
        FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND estCostBlank.estCostFormID EQ ttInk.estFormID 
        AND estCostBlank.estCostBlankID EQ ttInk.estBlankID :
            
        RUN pProcessInk(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER estCostBlank, BUFFER estCostOperation, BUFFER ttInk).    
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
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.

    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL NO-UNDO.
    
    RUN pBuildLeafForEf(BUFFER ipbf-ef, BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm).
    
    FOR FIRST estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND estCostOperation.isLeafer
        , 
        EACH ttLeaf NO-LOCK
        WHERE ttLeaf.estHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND ttLeaf.estFormID EQ ipbf-estCostForm.estCostFormID
        BY estCostOperation.sequenceOfOperation DESCENDING:
        
        IF estCostOperation.feedType EQ "B" THEN 
        DO:            
            FIND FIRST estCostBlank NO-LOCK 
                WHERE estCostBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND estCostBlank.estCostFormID EQ ttLeaf.estFormID 
                AND estCostBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE estCostBlank THEN 
                dQtyRequiredPerForm = estCostBlank.numOut * ttLeaf.dQtyRequiredPerLeaf.
        END.
        ELSE 
            dQtyRequiredPerForm = ttLeaf.dQtyRequiredPerLeaf.
            
        RUN pProcessLeaf(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER estCostOperation, BUFFER ttLeaf, dQtyRequiredPerForm).    
        
    END.

   
END PROCEDURE.

PROCEDURE pProcessPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the estCostMaterial for packing material with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.

    DEFINE           BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE VARIABLE iCaseCount   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCases       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPalletCount AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPallets     AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-estCostBlank FOR estCostBlank.
    
    ASSIGN 
        iCaseCount = 0
        iCases     = 0
        iPallets   = 0
        .
    
    /*Case*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-estCostForm.estCostFormID
        AND ttPack.lIsCase,
        FIRST bf-estCostBlank EXCLUSIVE-LOCK 
        WHERE bf-estCostBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-estCostBlank.estCostFormID EQ ttPack.estFormID
        AND bf-estCostBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-estCostMaterial).
        
        IF ttPack.iCountPerSubUnit NE 0 THEN
            ASSIGN
                iCaseCount = ttPack.iCountPerSubUnit  
                iCases     = fRoundUp(bf-estCostBlank.quantityRequired / ttPack.iCountPerSubUnit) * ttPack.dQtyMultiplier.
        ELSE 
            ASSIGN /*Calc cases based on weight - REFACTOR since assumes weight is in LB/M */
                iCases     = fRoundUp(bf-estCostBlank.quantityRequired * (bf-estCostBlank.weight) / ttPack.dWeightCapacity) * ttPack.dQtyMultiplier
                iCaseCount = fRoundUp(bf-estCostBlank.quantityRequired /  iCases)
                .
        ASSIGN  
            bf-estCostMaterial.quantityRequiredNoWaste = iCases
            bf-estCostBlank.quantityOfSubUnits         = iCases
            bf-estCostMaterial.quantityUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-estCostMaterial.itemName = bf-estCostMaterial.itemName + " (" + TRIM(STRING(iCaseCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).
    END.
    
    /*Pallet*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-estCostForm.estCostFormID
        AND ttPack.lIsPallet,
        FIRST bf-estCostBlank EXCLUSIVE-LOCK 
        WHERE bf-estCostBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-estCostBlank.estCostFormID EQ ttPack.estFormID
        AND bf-estCostBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-estCostMaterial).
        
        ASSIGN  
            iPalletCount                               = IF ttPack.iCountPerUnit EQ 0 THEN ttPack.iCountSubUnitsPerUnit * iCaseCount ELSE ttPack.iCountPerUnit
            iPallets                                   = fRoundUp(bf-estCostBlank.quantityRequired / iPalletCount) * ttPack.dQtyMultiplier 
            bf-estCostMaterial.quantityRequiredNoWaste = iPallets
            bf-estCostMaterial.quantityUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-estCostMaterial.itemName = bf-estCostMaterial.itemName + " (" + TRIM(STRING(iPalletCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).
    END.
    
    /*Other Pack*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-estCostForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-estCostForm.estCostFormID
        AND NOT ttPack.lIsPallet AND NOT ttPack.lIsCase,
        FIRST bf-estCostBlank EXCLUSIVE-LOCK 
        WHERE bf-estCostBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND bf-estCostBlank.estCostFormID EQ ttPack.estFormID
        AND bf-estCostBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-estCostMaterial).
        
        
        ASSIGN 
            bf-estCostMaterial.quantityRequiredNoWaste = IF ttPack.cQtyMultiplierPer EQ "P" THEN iPallets * ttPack.dQtyMultiplier ELSE iCases * ttPack.dQtyMultiplier
            bf-estCostMaterial.quantityUOM             = ttPack.cQtyUOM
            .                    
        RUN pCalcEstMaterial(BUFFER ipbf-estCostHeader, BUFFER bf-estCostMaterial, BUFFER ipbf-estCostForm).
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
       
    DEFINE VARIABLE iIndex     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCostFound AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE dRunQty    AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dRunCost   AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dSetups    AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dQtyInCUOM AS DECIMAL.
    

    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.

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
                RUN custom/convquom.p(e-item-vend.company,ipcQtyUOM,opcCostUOM, 
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

    IF ipbf-estCostMaterial.isRealMaterial AND NOT lCostFound THEN
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
        FIND FIRST reftable
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
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOut AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutRunWaste AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-mach FOR mach.

    DEFINE VARIABLE iInkCoatCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO. 
    
    
    ASSIGN 
        ipbf-estCostOperation.quantityOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-estCostOperation.quantityInNoWaste = ipbf-estCostOperation.quantityOut / ipbf-estCostOperation.numOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste                  = iopdQtyInOutSetupWaste / ipbf-estCostOperation.numOutForOperation
        iopdQtyInOutRunWaste                    = iopdQtyInOutRunWaste / ipbf-estCostOperation.numOutForOperation
        ipbf-estCostOperation.quantityIn        = fRoundUp(ipbf-estCostOperation.quantityIn)
        iopdQtyInOutSetupWaste                  = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste                    = fRoundUp(iopdQtyInOutRunWaste)
        .
    
    /*Recalc from standards off right now*/
    IF NOT ipbf-estCostOperation.isLocked THEN 
    DO:
        FIND FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ ipbf-estCostHeader.company
            AND bf-mach.m-code EQ ipbf-estCostOperation.operationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER ipbf-estCostOperation, BUFFER bf-mach).
       
    END.
    
    ASSIGN 
        ipbf-estCostOperation.quantityInRunWaste        = (ipbf-estCostOperation.quantityInNoWaste / 
                                                    (1 - (ipbf-estCostOperation.quantityInRunWastePercent / 100))) 
                                                    - ipbf-estCostOperation.quantityInNoWaste
        ipbf-estCostOperation.quantityInRunWaste        = fRoundUp(ipbf-estCostOperation.quantityInRunWaste)
        ipbf-estCostOperation.quantityInAfterSetupWaste = ipbf-estCostOperation.quantityInNoWaste + ipbf-estCostOperation.quantityInRunWaste
        ipbf-estCostOperation.quantityIn                = ipbf-estCostOperation.quantityInAfterSetupWaste + ipbf-estCostOperation.quantityInSetupWaste
        iopdQtyInOutRunWaste                            = iopdQtyInOutRunWaste + ipbf-estCostOperation.quantityInRunWaste
        iopdQtyInOutSetupWaste                          = iopdQtyInOutSetupWaste + ipbf-estCostOperation.quantityInSetupWaste
        ipbf-estCostOperation.quantityIn                = fRoundUp(ipbf-estCostOperation.quantityIn)
        iopdQtyInOut                                    = ipbf-estCostOperation.quantityIn
        .


END PROCEDURE.

PROCEDURE pPurgeCalculation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a company and estimate number, purges all related data for 
     cost estimate calculation
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estCostHeader    FOR estCostHeader.
    DEFINE BUFFER bf-estCostMisc      FOR estCostMisc.
    DEFINE BUFFER bf-estCostOperation FOR estCostOperation.
    DEFINE BUFFER bf-estCostMaterial  FOR estCostMaterial.
    DEFINE BUFFER bf-estCostForm      FOR estCostForm.
    DEFINE BUFFER bf-estCostItem      FOR estCostItem.
    DEFINE BUFFER bf-estCostBlank     FOR estCostBlank.
    DEFINE BUFFER bf-estCostSummary   FOR estCostSummary.
    DEFINE BUFFER bf-estCostDetail    FOR estCostDetail.
    DEFINE BUFFER bf-probe            FOR probe.
    
    DISABLE TRIGGERS FOR LOAD OF estCostHeader.
    DISABLE TRIGGERS FOR LOAD OF estCostMisc.
    DISABLE TRIGGERS FOR LOAD OF estCostOperation.
    DISABLE TRIGGERS FOR LOAD OF estCostMaterial.
    DISABLE TRIGGERS FOR LOAD OF estCostForm.
    DISABLE TRIGGERS FOR LOAD OF estCostItem.
    DISABLE TRIGGERS FOR LOAD OF estCostBlank.
    DISABLE TRIGGERS FOR LOAD OF estCostSummary.
    DISABLE TRIGGERS FOR LOAD OF estCostDetail.
    
    FOR EACH bf-probe EXCLUSIVE-LOCK 
        WHERE bf-probe.company EQ ipcCompany
        AND bf-probe.est-no  EQ ipcEstimateNo:
        DELETE bf-probe.                 
    END.
    FOR EACH bf-estCostHeader EXCLUSIVE-LOCK 
        WHERE bf-estCostHeader.company EQ ipcCompany
        AND bf-estCostHeader.estimateNo EQ ipcEstimateNo
        USE-INDEX estimate:
        FOR EACH bf-estCostMisc EXCLUSIVE-LOCK
            WHERE bf-estCostMisc.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            /*            USE-INDEX estHeader*/
            :
            DELETE bf-estCostMisc.
        END. 
        FOR EACH bf-estCostOperation EXCLUSIVE-LOCK
            WHERE bf-estCostOperation.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
            DELETE bf-estCostOperation.
        END. 
        FOR EACH bf-estCostMaterial EXCLUSIVE-LOCK
            WHERE bf-estCostMaterial.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
            DELETE bf-estCostMaterial.
        END.
        FOR EACH bf-estCostForm EXCLUSIVE-LOCK 
            WHERE bf-estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            USE-INDEX estHeader:
            DELETE bf-estCostForm.
        END.    
        FOR EACH bf-estCostBlank EXCLUSIVE-LOCK 
            WHERE bf-estCostBlank.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
            DELETE bf-estCostBlank.
        END.
        FOR EACH bf-estCostItem EXCLUSIVE-LOCK 
            WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            USE-INDEX estHeader:
            DELETE bf-estCostItem.
        END.
        FOR EACH bf-estCostDetail EXCLUSIVE-LOCK 
            WHERE bf-estCostDetail.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            USE-INDEX estHeader:
            DELETE bf-estCostDetail.
        END.
        FOR EACH bf-estCostSummary EXCLUSIVE-LOCK 
            WHERE bf-estCostSummary.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
            USE-INDEX estHeader:
            DELETE bf-estCostSummary.
        END.
        DELETE bf-estCostHeader.
    END. /*Each bf-estCostHeader for estimate*/
END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated Run Speeds for estCostOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.

   

END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunWastePercent PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated RunWastePercent for estCostOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.




END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsSetupWaste PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated MR Waste for estCostOperation
     based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostOperation FOR estCostOperation.
    DEFINE PARAMETER BUFFER ipbf-mach             FOR mach.

    ipbf-estCostOperation.quantityInSetupWaste = ipbf-mach.mr-waste.
    IF ipbf-estCostOperation.isPrinter OR ipbf-estCostOperation.isCoater THEN 
        ipbf-estCostOperation.quantityInSetupWaste = ipbf-estCostOperation.quantityInSetupWaste + 
            (ipbf-estCostOperation.quantityInSetupWastePerColor * 
            IF glUsePlateChangesAsColorForSetupWaste AND ipbf-estCostOperation.countPlateChanges NE 0 THEN ipbf-estCostOperation.countPlateChanges
            ELSE (ipbf-estCostOperation.countCoats + ipbf-estCostOperation.countInks)).

/*REFACTOR To handle the "Plain Jobs only* function in EB1*/
/*        RUN est/diewaste.p (BUFFER est-op).*/


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

FUNCTION fIsDepartment RETURNS LOGICAL PRIVATE
    (ipcDepartment AS CHARACTER, ipcDepartmentList AS CHARACTER EXTENT 4):
    /*------------------------------------------------------------------------------
     Purpose: determine if provided department is in department list
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lIsDepartment AS LOGICAL NO-UNDO. 
    
    DO iIndex = 1 TO 4:
        IF CAN-DO(ipcDepartment,ipcDepartmentList[iIndex]) THEN 
        DO:
            lIsDepartment = YES.
            LEAVE.
        END.
    END.
    RETURN lIsDepartment.
        
END FUNCTION.

FUNCTION fRoundUP RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Given a value, rounds up to next integer
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dValueRounded AS DECIMAL NO-UNDO.

    IF (ipdValue - INTEGER(ipdValue)) > 0 THEN 
        dValueRounded = INTEGER(ipdValue) + 1.
    ELSE dValueRounded = INTEGER (ipdValue).
    RETURN dValueRounded.
		
END FUNCTION.


