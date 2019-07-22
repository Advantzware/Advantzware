
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

USING System.Buffer FROM ASSEMBLY.

DEFINE STREAM sImport.
{est/ttEstPrint.i "NEW SHARED"}

DEFINE VARIABLE ghSession AS HANDLE.

DEFINE VARIABLE gcOutputFile                          AS CHARACTER INITIAL "C:\temp\estPrintOut.txt".
DEFINE VARIABLE gcTestDataDir                         AS CHARACTER INITIAL "C:\Users\brad.vigrass\Documents\Testing\EstimateData\".
//DEFINE VARIABLE gcTestDataDir                         AS CHARACTER INITIAL "C:\asigui\Backups\Import\".
DEFINE VARIABLE giRecKey                              AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcSourceTypeOperation                 AS CHARACTER NO-UNDO INITIAL "Operation".
DEFINE VARIABLE gcSourceTypeMaterial                  AS CHARACTER NO-UNDO INITIAL "Material".
DEFINE VARIABLE gcSourceTypeMisc                      AS CHARACTER NO-UNDO INITIAL "Miscellaneous".
DEFINE VARIABLE gcSourceTypeNonFactory                AS CHARACTER NO-UNDO INITIAL "NonFactory".

DEFINE VARIABLE gcBoardMatTypes                       AS CHARACTER NO-UNDO INITIAL "1,2,3,4,A,B,P,R".
DEFINE VARIABLE gcGlueMatTypes                        AS CHARACTER NO-UNDO INITIAL "G,S,T".
DEFINE VARIABLE gcInkMatTypes                         AS CHARACTER NO-UNDO INITIAL "I,V".
DEFINE VARIABLE gcPackMatTypes                        AS CHARACTER NO-UNDO INITIAL "5,6,C,D,J,M".
DEFINE VARIABLE gcLeafMatTypes                        AS CHARACTER NO-UNDO INITIAL "F,W".

DEFINE VARIABLE gcIndustryFolding                     AS CHARACTER NO-UNDO INITIAL "Folding".
DEFINE VARIABLE gcIndustryCorrugated                  AS CHARACTER NO-UNDO INITIAL "Corrugated".

DEFINE VARIABLE gcErrorWarning                        AS CHARACTER NO-UNDO INITIAL "Warning".
DEFINE VARIABLE gcErrorImportant                      AS CHARACTER NO-UNDO INITIAL "Important".
DEFINE VARIABLE gcErrorCritical                       AS CHARACTER NO-UNDO INITIAL "Critical".

/*Settings Globals*/
DEFINE VARIABLE gcPrepRoundTo                         AS CHARACTER NO-UNDO.  /*CEPREP - char val - potentially deprecate*/
DEFINE VARIABLE gcPrepMarkupOrMargin                  AS CHARACTER NO-UNDO.  /*CEPrepPrice - char val*/
DEFINE VARIABLE gdMaterialMarkup                      AS DECIMAL   NO-UNDO.    /*CEMatl - Dec val*/

DEFINE VARIABLE glUsePlateChangesAsColorForSetupWaste AS LOGICAL   NO-UNDO INITIAL NO.  /*Defect in EstOperation Calc of applying the MR Waste Sheets Per Color?*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64,
    ipiEstFormID AS INT64,
    ipiBlankNo AS INTEGER) FORWARD.

FUNCTION fGetNextID RETURNS INT64 PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.

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
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

RUN pBuildTestData("001",YES).
RUN pBuildFactoryCostDetails.
RUN pCalcCostTotals.
RUN pBuildNonFactoryCostDetails.
RUN pBuildCostSummary.

FOR EACH ttEstError NO-LOCK:
    DISPLAY ttEstError.iFormNo ttEstError.iBlankNo ttEstError.cErrorType ttEstError.cError FORMAT "x(60)" ttEstError.iFormNo.
END.
FIND FIRST ttEstHeader NO-LOCK 
    WHERE ttEstHeader.estCostHeaderID = 2.

RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Set Summary First","Calibri").


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
    DEFINE PARAMETER BUFFER opbf-ttEstCostDetail FOR ttEstCostDetail.
    
    CREATE opbf-ttEstCostDetail.
    ASSIGN 
        opbf-ttEstCostDetail.rec_key            = fGetNextRecKey()
        opbf-ttEstCostDetail.estCostDetailID    = fGetNextID()
        opbf-ttEstCostDetail.estCostHeaderID    = ipiEstHeaderID
        opbf-ttEstCostDetail.estCostFormID      = ipiEstFormID
        opbf-ttEstCostDetail.estCostBlankID     = ipiEstBlankID
        opbf-ttEstCostDetail.sourceID           = ipiEstSourceID
        opbf-ttEstCostDetail.sourceType         = ipcSourceType
        opbf-ttEstCostDetail.estCostCategoryID  = ipcEstCostCategoryID
        opbf-ttEstCostDetail.estCostDetailDesc  = ipcDescription
        opbf-ttEstCostDetail.costTotal          = ipdCost
        opbf-ttEstCostDetail.profitPercent      = ipdProfitPercent
        opbf-ttEstCostDetail.profitPercentType  = "Margin"
        .

END PROCEDURE.

PROCEDURE pAddCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstOperation.estCostHeaderID, ipbf-ttEstOperation.estCostFormID, ipbf-ttEstOperation.estCostBlankID, ipbf-ttEstOperation.estCostOperationID, 
        gcSourceTypeOperation, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, BUFFER bf-ttEstCostDetail).

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
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstMaterial.estCostHeaderID, ipbf-ttEstMaterial.estCostFormID, ipbf-ttEstMaterial.estCostBlankID, ipbf-ttEstMaterial.estCostMaterialID, 
        gcSourceTypeMaterial, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, BUFFER bf-ttEstCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMisc FOR ttEstMisc.
    DEFINE INPUT PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdProfitPercent AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    RUN pAddCostDetail(ipbf-ttEstMisc.estCostHeaderID, ipbf-ttEstMisc.estCostFormID, ipbf-ttEstMisc.estCostBlankID, ipbf-ttEstMisc.estCostMiscID, 
        gcSourceTypeMisc, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, BUFFER bf-ttEstCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a scopeID, GroupID, Cost and Quantity, adds or increments a cost summary
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcScopeRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipcGroupID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER  ipdCost AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER  ipdQtyPerM AS DECIMAL NO-UNDO.
    
    FIND FIRST ttEstCostSummary EXCLUSIVE-LOCK
        WHERE ttEstCostSummary.scopeRecKey EQ ipcScopeRecKey
        AND ttEstCostSummary.estCostGroupID EQ ipcGroupID
        NO-ERROR.
    IF NOT AVAILABLE ttEstCostSummary THEN 
    DO:
        CREATE ttEstCostSummary.
        ASSIGN 
            ttEstCostSummary.estCostSummaryID = fGetNextID()
            ttEstCostSummary.estCostGroupID   = ipcGroupID
            ttEstCostSummary.scopeRecKey      = ipcScopeRecKey
            .

    END.
    ttEstCostSummary.costTotal = ttEstCostSummary.costTotal + ipdCost.
    IF ipdQtyPerM GT 0 THEN 
        ttEstCostSummary.costTotalPerMFinished  = ttEstCostSummary.costTotalPerMFinished + ipdCost / ipdQtyPerM.

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
    DEFINE PARAMETER BUFFER ipbf-eb         FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm  FOR ttEstForm.
    DEFINE PARAMETER BUFFER opbf-ttEstBlank FOR ttEstBlank.
 
    CREATE opbf-ttEstBlank.
    ASSIGN 
        opbf-ttEstBlank.rec_key             = fGetNextRecKey()
        opbf-ttEstBlank.estCostBlankID          = fGetNextID()
        opbf-ttEstBlank.company             = ipbf-ttEstForm.company
        opbf-ttEstBlank.estCostFormID           = ipbf-ttEstForm.estCostFormID
        opbf-ttEstBlank.estCostHeaderID         = ipbf-ttEstForm.estCostHeaderID
        opbf-ttEstBlank.formNo             = ipbf-eb.form-no
        opbf-ttEstBlank.blankNo            = ipbf-eb.blank-no
        opbf-ttEstBlank.numOutLength       = ipbf-eb.num-len
        opbf-ttEstBlank.numOutWidth        = ipbf-eb.num-wid
        opbf-ttEstBlank.numOutDepth        = ipbf-eb.num-dep
        opbf-ttEstBlank.numOut             = MAX(opbf-ttEstBlank.numOutWidth, 1) * MAX(opbf-ttEstBlank.numOutLength, 1) * MAX(opbf-ttEstBlank.numOutDepth, 1)
        opbf-ttEstBlank.blankWidth         = ipbf-eb.t-wid
        opbf-ttEstBlank.blankLength        = ipbf-eb.t-len
        opbf-ttEstBlank.blankDepth         = ipbf-eb.t-dep
        opbf-ttEstBlank.blankArea          = ipbf-eb.t-sqin
        opbf-ttEstBlank.dimLength             = ipbf-eb.len
        opbf-ttEstBlank.dimWidth              = ipbf-eb.wid
        opbf-ttEstBlank.dimDepth              = ipbf-eb.dep
                                                
        /*Refactor - Hardcoded*/
        opbf-ttEstBlank.areaUOM            = "SQIN"
        opbf-ttEstBlank.dimUOM       = "IN"
        opbf-ttEstBlank.weightUOM          = "LB/M"
                    
        /*Refactor - apply area UOM conversion*/
        opbf-ttEstBlank.weight             = ipbf-ttEstForm.basisWeight * opbf-ttEstBlank.blankArea / 144000 
                            
        /*Refactor - Calculate Windowing*/
        opbf-ttEstBlank.blankAreaNetWindow = opbf-ttEstBlank.blankArea
        opbf-ttEstBlank.qtyRequired        = ipbf-eb.bl-qty
        opbf-ttEstBlank.qtyYielded         = ipbf-eb.yld-qty
        .
        
    FIND FIRST ttEstItem EXCLUSIVE-LOCK 
        WHERE ttEstItem.estCostHeaderID EQ opbf-ttEstBlank.estCostHeaderID
        AND ttEstItem.customerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE ttEstItem THEN 
    DO:
        ASSIGN 
            opbf-ttEstBlank.estCostItemID    = ttEstItem.estCostItemID
            ttEstItem.sizeDesc           = TRIM(STRING(opbf-ttEstBlank.dimLength,">>>9.99")) + " x " + TRIM(STRING(opbf-ttEstBlank.dimWidth,">>>9.99"))
            opbf-ttEstBlank.qtyPerSet   = MAX(ttEstItem.qtyPerSet, 1)
            opbf-ttEstBlank.qtyRequired = ipbf-eb.bl-qty * opbf-ttEstBlank.qtyPerSet
            opbf-ttEstBlank.qtyYielded  = ipbf-eb.yld-qty * opbf-ttEstBlank.qtyPerSet
            .
        IF opbf-ttEstBlank.dimDepth NE 0 THEN 
            ttEstItem.sizeDesc = ttEstItem.sizeDesc + " x " + TRIM(STRING(opbf-ttEstBlank.dimDepth,">>>9.99")).
    END.
    ASSIGN 
        ipbf-ttEstForm.numOutBlanksOnNet = ipbf-ttEstForm.numOutBlanksOnNet + opbf-ttEstBlank.numOut
        ipbf-ttEstForm.blankArea         = ipbf-ttEstForm.blankArea + opbf-ttEstBlank.blankArea * opbf-ttEstBlank.numOut
        . 

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstForm FOR ttEstForm.
    
    CREATE opbf-ttEstForm.
    ASSIGN 
        opbf-ttEstForm.rec_key     = fGetNextRecKey()
        opbf-ttEstForm.estCostFormID   = fGetNextID()
        opbf-ttEstForm.estCostHeaderID = ipiEstHeaderID
        opbf-ttEstForm.formNo     = ipiFormNo
        .
END PROCEDURE.

PROCEDURE pAddEstFormFromEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader. 
    DEFINE PARAMETER BUFFER opbf-ttEstForm   FOR ttEstForm.

    RUN pAddEstForm(ipbf-ttEstHeader.estCostHeaderID, ipbf-ef.form-no, BUFFER opbf-ttEstForm).
    
    ASSIGN 
        opbf-ttEstForm.numOutNetLength                = MAX(ipbf-ef.n-out-l, 1)
        opbf-ttEstForm.numOutNetWidth                 = MAX(ipbf-ef.n-out, 1)
        opbf-ttEstForm.numOutNetDepth                 = MAX(ipbf-ef.n-out-d, 1)
        opbf-ttEstForm.numOutNet                      = opbf-ttEstForm.numOutNetLength * opbf-ttEstForm.numOutNetWidth * opbf-ttEstForm.numOutNetDepth
        opbf-ttEstForm.grossWidth                     = ipbf-ef.gsh-wid 
        opbf-ttEstForm.grossLength                    = ipbf-ef.gsh-len
        opbf-ttEstForm.grossDepth                     = ipbf-ef.gsh-dep 
        opbf-ttEstForm.netWidth                       = ipbf-ef.nsh-wid
        opbf-ttEstForm.netLength                      = ipbf-ef.nsh-len
        opbf-ttEstForm.netDepth                       = ipbf-ef.nsh-dep
        opbf-ttEstForm.dieWidth                       = ipbf-ef.trim-w
        opbf-ttEstForm.dieLength                      = ipbf-ef.trim-l
        opbf-ttEstForm.basisWeight                    = ipbf-ef.weight
        opbf-ttEstForm.company                        = ipbf-ef.company     
        opbf-ttEstForm.costOverridePerUOM             = ipbf-ef.cost-msh
        opbf-ttEstForm.costOverrideUOM                = ipbf-ef.cost-uom  
        opbf-ttEstForm.noCost                         = NOT ipbf-ef.nc
        /*Refactor - handle when ef.roll is yes see ce/print4p.i*/
                
        /*Refactor- Hard-codes*/
        opbf-ttEstForm.dimUOM                          = "IN"
        opbf-ttEstForm.areaUOM                         = "SF"
        opbf-ttEstForm.weightDieUOM                    = "LB/MSHT"
        opbf-ttEstForm.weightNetUOM                    = "LB/MSHT"
        opbf-ttEstForm.weightGrossUOM                  = "LB/MSHT"
        opbf-ttEstForm.grossQtyRequiredTotalWeightUOM  = "LBS"
        opbf-ttEstForm.grossQtyRequiredTotalAreaUOM    = "MSF"
            
               
        /*Refactor - Formulas/Conversions - don't assume SF and inches*/
        opbf-ttEstForm.grossArea                       = opbf-ttEstForm.grossWidth * opbf-ttEstForm.grossLength / 144
        opbf-ttEstForm.netArea                         = opbf-ttEstForm.netWidth * opbf-ttEstForm.netLength / 144
        opbf-ttEstForm.dieArea                         = opbf-ttEstForm.dieWidth * opbf-ttEstForm.dieLength / 144
        
        opbf-ttEstForm.weightDie                       = opbf-ttEstForm.basisWeight * opbf-ttEstForm.dieArea 
        opbf-ttEstForm.weightNet                       = opbf-ttEstForm.basisWeight * opbf-ttEstForm.netArea 
        opbf-ttEstForm.weightGross                     = opbf-ttEstForm.basisWeight * opbf-ttEstForm.grossArea
            
        /*Refactor - Calculate Combo products*/
        opbf-ttEstForm.qtyFGOnForm                    = ipbf-ttEstHeader.qtyMaster
        .

END PROCEDURE.

PROCEDURE pAddEstItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create an ttEstItem given eb and other key ids
     Notes: ce/print4p.i
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER opbf-ttEstItem   FOR ttEstItem.

    CREATE opbf-ttEstItem.
    ASSIGN 
        opbf-ttEstItem.rec_key           = fGetNextRecKey()
        opbf-ttEstItem.estCostItemID     = fGetNextID()
        opbf-ttEstItem.estCostHeaderID   = ipbf-ttEstHeader.estCostHeaderID
        opbf-ttEstItem.customerPart      = ipbf-eb.part-no
        opbf-ttEstItem.colorDesc         = ipbf-eb.i-coldscr
        opbf-ttEstItem.customerID        = ipbf-eb.cust-no
        opbf-ttEstItem.shipToID          = ipbf-eb.ship-id
        opbf-ttEstItem.itemName          = ipbf-eb.part-dscr1
        opbf-ttEstItem.itemDescription1  = ipbf-eb.part-dscr2
        opbf-ttEstItem.salesgroupID      = ipbf-eb.sman
        opbf-ttEstItem.styleID           = ipbf-eb.style
        opbf-ttEstItem.isSet             = ipbf-eb.is-a-set
        opbf-ttEstItem.itemID          = ipbf-eb.stock-no
        opbf-ttEstItem.company           = ipbf-eb.company
        .
    
    IF ipbf-eb.est-type LT 5 THEN
        opbf-ttEstItem.qtyPerSet     = MAX(ipbf-eb.cust-%, 1). 
    ELSE         
        opbf-ttEstItem.qtyPerSet     = ipbf-eb.quantityPerSet.
    IF opbf-ttEstItem.qtyPerSet LT 0 THEN 
        opbf-ttEstItem.qtyPerSet     = 1 / opbf-ttEstItem.qtyPerSet.
    IF opbf-ttEstItem.qtyPerSet EQ 0 THEN 
        opbf-ttEstItem.qtyPerSet     = 1 .
     
    opbf-ttEstItem.qtyRequired      = ipbf-eb.bl-qty * opbf-ttEstItem.qtyPerSet.
            
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-eb.company
        AND cust.cust-no EQ ipbf-eb.cust-no
        NO-ERROR.
  
    /*Refactor - hardcoded temp?  Consider just using eb fields*/
    IF AVAILABLE cust AND cust.cust-no NE "Temp" THEN 
        ASSIGN 
            opbf-ttEstItem.customerName     = cust.name
            opbf-ttEstItem.customerAddress1 = cust.addr[1]
            opbf-ttEstItem.customerAddress2 = cust.addr[2]
            opbf-ttEstItem.customerAddress3 = cust.city + ", " + cust.state + " " + cust.zip
            .
    ELSE 
        ASSIGN  
            opbf-ttEstItem.customerName     = ipbf-eb.ship-name
            opbf-ttEstItem.customerAddress1 = ipbf-eb.ship-addr[1]
            opbf-ttEstItem.customerAddress2 = ipbf-eb.ship-addr[2]
            opbf-ttEstItem.customerAddress3 = ipbf-eb.ship-city + ", " + ipbf-eb.ship-state + " " + ipbf-eb.ship-zip
            .
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipbf-eb.company
        AND shipto.cust-no EQ ipbf-eb.cust-no
        AND shipto.ship-id EQ ipbf-eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN 
            opbf-ttEstItem.shipToName     = shipto.ship-name
            opbf-ttEstItem.shipToAddress1 = shipto.ship-addr[1]
            opbf-ttEstItem.shipToAddress2 = shipto.ship-addr[2]
            opbf-ttEstItem.shipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
            .
    FIND FIRST sman NO-LOCK 
        WHERE sman.company EQ ipbf-eb.company
        AND sman.sman EQ ipbf-eb.sman
        NO-ERROR.
    IF AVAILABLE sman THEN 
        ASSIGN 
            opbf-ttEstItem.salesgroupName = sman.sname        
            .
    FIND FIRST style NO-LOCK 
        WHERE style.company EQ ipbf-eb.company
        AND style.style EQ ipbf-eb.style
        NO-ERROR.
    IF AVAILABLE style THEN 
        ASSIGN 
            opbf-ttEstItem.styleDesc = style.dscr
            .

END PROCEDURE.

PROCEDURE pAddEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Create ttEstMaterial from an item buffer and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT PARAMETER ipcRMItemID AS CHARACTER.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER.
    DEFINE PARAMETER BUFFER opbf-ttEstMaterial FOR ttEstMaterial.
 
    DEFINE           BUFFER bf-item            FOR ITEM.

    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ  ipbf-ttEstHeader.company
        AND bf-item.i-no EQ ipcRMItemID
        NO-ERROR.
    IF AVAILABLE bf-item THEN 
    DO:
        CREATE opbf-ttEstMaterial.
        ASSIGN 
            opbf-ttEstMaterial.rec_key         = fGetNextRecKey()
            opbf-ttEstMaterial.estCostMaterialID   = fGetNextID()
            opbf-ttEstMaterial.estCostFormID       = ipbf-ttEstForm.estCostFormID
            opbf-ttEstMaterial.estCostHeaderID     = ipbf-ttEstForm.estCostHeaderID
            opbf-ttEstMaterial.company         = bf-item.company
            opbf-ttEstMaterial.formNo         = ipbf-ttEstForm.formNo
            opbf-ttEstMaterial.blankNo        = ipiBlankNo
            opbf-ttEstMaterial.itemID         = bf-item.i-no 
            opbf-ttEstMaterial.itemName       = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name 
            opbf-ttEstMaterial.qtyUOM         = CAPS(bf-item.cons-uom)
            opbf-ttEstMaterial.qtyUOMWaste    = opbf-ttEstMaterial.qtyUOM
            opbf-ttEstMaterial.basisWeight    = bf-item.basis-w
            opbf-ttEstMaterial.basisWeightUOM = "LB/MSF"
            opbf-ttEstMaterial.dimLength      = bf-item.s-len
            opbf-ttEstMaterial.dimWidth       = bf-item.s-wid
            opbf-ttEstMaterial.dimDepth       = bf-item.s-dep
            opbf-ttEstMaterial.dimUOM         = "IN"
            opbf-ttEstMaterial.costPerUOMAvg  = bf-item.avg-cost
            opbf-ttEstMaterial.costPerUOMLast = bf-item.last-cost
            opbf-ttEstMaterial.isRealMaterial = bf-item.i-code EQ "R"
            opbf-ttEstMaterial.materialType   = bf-item.mat-type
            .
        
        IF CAN-DO(gcBoardMatTypes, opbf-ttEstMaterial.materialType) THEN 
            opbf-ttEstMaterial.sequenceOfMaterial = 1.
        ELSE IF CAN-DO(gcInkMatTypes,opbf-ttEstMaterial.materialType) THEN 
                opbf-ttEstMaterial.sequenceOfMaterial = 2.
            ELSE IF CAN-DO(gcGlueMatTypes,opbf-ttEstMaterial.materialType) THEN 
                    opbf-ttEstMaterial.sequenceOfMaterial = 3.
                ELSE IF CAN-DO(gcLeafMatTypes,opbf-ttEstMaterial.materialType) THEN 
                        opbf-ttEstMaterial.sequenceOfMaterial = 4.
                    ELSE IF CAN-DO(gcPackMatTypes,opbf-ttEstMaterial.materialType) THEN 
                            opbf-ttEstMaterial.sequenceOfMaterial = 5.
                        ELSE  
                            opbf-ttEstMaterial.sequenceOfMaterial = 6.
            
        IF opbf-ttEstMaterial.blankNo NE 0 THEN 
        DO:
            opbf-ttEstMaterial.estCostBlankID = fGetEstBlankID(opbf-ttEstMaterial.estCostHeaderID,opbf-ttEstMaterial.estCostFormID, opbf-ttEstMaterial.blankNo).
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estCostBlankID EQ opbf-ttEstMaterial.estCostBlankID
                NO-ERROR.
            IF AVAILABLE ttEstBlank AND NOT opbf-ttEstMaterial.isRealMaterial THEN 
                ASSIGN 
                    opbf-ttEstMaterial.dimLength = ttEstBlank.blankLength
                    opbf-ttEstMaterial.dimWidth  = ttEstBlank.blankWidth
                    opbf-ttEstMaterial.dimLength = ttEstBlank.blankLength
                    .
        END.
        ELSE IF NOT opbf-ttEstMaterial.isRealMaterial THEN 
                ASSIGN 
                    opbf-ttEstMaterial.dimLength = ipbf-ttEstForm.grossLength
                    opbf-ttEstMaterial.dimWidth  = ipbf-ttEstForm.grossWidth
                    opbf-ttEstMaterial.dimDepth  = ipbf-ttEstForm.grossDepth
                    .
        
    END.
    
END PROCEDURE.

PROCEDURE pAddEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstMisc and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipiEstFormID AS INT64 NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstMisc FOR ttEstMisc.

    CREATE opbf-ttEstMisc.
    ASSIGN 
        opbf-ttEstMisc.rec_key     = fGetNextRecKey()
        opbf-ttEstMisc.estCostMiscID   = fGetNextID()
        opbf-ttEstMisc.estCostHeaderID = ipiEstHeaderID
        opbf-ttEstMisc.estCostFormID   = ipiEstFormID
        .
END PROCEDURE.

PROCEDURE pAddEstMiscForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a form buffer and other key fields, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef        FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiIndex AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-ttEstMisc FOR ttEstMisc.
    
    DEFINE VARIABLE cCostType  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostPerM  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup AS DECIMAL   NO-UNDO.
    
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
    
    RUN pGetMiscCostPerM(BUFFER ipbf-ef, ipdQuantity, ipiIndex, UPPER(cCostType), OUTPUT dCostPerM).
    
    IF dCostPerM GT 0 OR dCostSetup GT 0 THEN 
    DO:
        RUN pAddEstMisc(ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.estCostFormID, BUFFER bf-ttEstMisc).
       
        ASSIGN 
            bf-ttEstMisc.estCostBlankID        = 0 /*REFACTOR - Get blank ID from form #?*/
            bf-ttEstMisc.formNo                = ipbf-ef.mis-snum[ipiIndex]  
            bf-ttEstMisc.blankNo               = ipbf-ef.mis-bnum[ipiIndex]
            bf-ttEstMisc.costDescription       = ipbf-ef.mis-cost[ipiIndex]
            bf-ttEstMisc.costType              = cCostType
            bf-ttEstMisc.costUOM               = "M"
            bf-ttEstMisc.costPerUOM            = dCostPerM
            bf-ttEstMisc.costSetup             = dCostSetup
            bf-ttEstMisc.profitPercentType           = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-ttEstMisc.SIMON                 = ipbf-ef.mis-simon[ipiIndex]
            bf-ttEstMisc.profitPercent         = ipbf-ef.mis-mkup[ipiIndex]
            bf-ttEstMisc.sourceQty             = ipdQuantity
            bf-ttEstMisc.qtyPerSourceQty       = 1
            bf-ttEstMisc.qtyRequiredTotal      = ipdQuantity
            bf-ttEstMisc.qtyUOM                = "EA"
            bf-ttEstMisc.costTotalBeforeProfit = dCostPerM * ipdQuantity / 1000 + dCostSetup
            .
        RUN pCalcEstMisc(BUFFER bf-ttEstMisc, BUFFER ipbf-ttEstForm).
        
    END.
END PROCEDURE.

PROCEDURE pAddEstMiscForPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a est-prep buffer and quantity, add an EstMisc record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-prep  FOR est-prep.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-ttEstMisc FOR ttEstMisc.
    
    RUN pAddEstMisc(ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.estCostFormID, BUFFER bf-ttEstMisc).
    
    ASSIGN 
        bf-ttEstMisc.estCostBlankID             = 0 /*REFACTOR - Get blank ID from form #?*/
        bf-ttEstMisc.formNo                = ipbf-est-prep.s-num  
        bf-ttEstMisc.blankNo               = ipbf-est-prep.b-num
        bf-ttEstMisc.prepID                = ipbf-est-prep.code
        bf-ttEstMisc.costDescription       = ipbf-est-prep.dscr
        bf-ttEstMisc.costType              = IF ipbf-est-prep.ml THEN "Mat" ELSE "Lab"
        bf-ttEstMisc.profitPercentType           = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
        bf-ttEstMisc.SIMON                 = ipbf-est-prep.simon
        bf-ttEstMisc.profitPercent         = ipbf-est-prep.mkup
        bf-ttEstMisc.sourceQty             = ipbf-est-prep.qty
        bf-ttEstMisc.qtyPerSourceQty       = 1
        bf-ttEstMisc.qtyRequiredTotal      = bf-ttEstMisc.sourceQty * bf-ttEstMisc.qtyPerSourceQty
        bf-ttEstMisc.qtyUOM                = "EA"
        bf-ttEstMisc.costUOM               = "EA"
        bf-ttEstMisc.costPerUOM            = ipbf-est-prep.cost
        bf-ttEstMisc.costSetup             = 0
        bf-ttEstMisc.costTotalBeforeProfit = bf-ttEstMisc.costPerUOM * bf-ttEstMisc.qtyRequiredTotal + bf-ttEstMisc.costSetup
        bf-ttEstMisc.isPrep                = YES
        .
    
    RUN pCalcEstMisc(BUFFER bf-ttEstMisc, BUFFER ipbf-ttEstForm).

END PROCEDURE.

PROCEDURE pAddEstOperationFromEstOp PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstOperation based on est-op and form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-op         FOR est-op.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER opbf-ttEstOperation FOR ttEstOperation.

    DEFINE           BUFFER bf-mach             FOR mach.
    DEFINE           BUFFER bf-est-op           FOR est-op.

    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
    DO:
        CREATE opbf-ttEstOperation.
        ASSIGN 
            opbf-ttEstOperation.rec_key                         = fGetNextRecKey()
            opbf-ttEstOperation.estCostOperationID                  = fGetNextID()
            opbf-ttEstOperation.estCostFormID                       = ipbf-ttEstForm.estCostFormID
            opbf-ttEstoperation.estCostHeaderID                     = ipbf-ttEstForm.estCostHeaderID
            opbf-ttEstOperation.company                         = ipbf-est-op.company
            opbf-ttEstOperation.formNo                         = ipbf-est-op.s-num
            opbf-ttEstOperation.blankNo                        = ipbf-est-op.b-num
            opbf-ttEstOperation.estCostBlankID                      = fGetEstBlankID(opbf-ttEstOperation.estCostBlankID, opbf-ttEstOperation.estCostFormID, opbf-ttEstOperation.blankNo)
            opbf-ttEstOperation.operationID                    = ipbf-est-op.m-code
            opbf-ttEstOperation.pass                           = MAX(ipbf-est-op.op-pass, 1)
            opbf-ttEstOperation.sequenceOfOperation                       = ipbf-est-op.line
            opbf-ttEstOperation.numOutDivisor                  = ipbf-est-op.n_out_div
                       
            opbf-ttEstOperation.qtyInSetupWaste                = ipbf-est-op.op-waste
            opbf-ttEstOperation.hoursSetup                     = ipbf-est-op.op-mr
            opbf-ttEstOperation.speed                          = ipbf-est-op.op-speed
            opbf-ttEstOperation.qtyInRunWastePercent           = ipbf-est-op.op-spoil
            opbf-ttEstOperation.isLocked                       = ipbf-est-op.isLocked
            opbf-ttEstOperation.crewSizeSetup                  = ipbf-est-op.op-crew[1]
            opbf-ttEstOperation.crewSizeRun                    = ipbf-est-op.op-crew[2]
            opbf-ttEstOperation.countInks                      = ipbf-est-op.num-col
            opbf-ttEstOperation.countCoats                     = ipbf-est-op.num-coat
            opbf-ttEstOperation.countFountainChanges           = ipbf-est-op.fountains
            opbf-ttEstOperation.countPlateChanges              = ipbf-est-op.plates
            
            opbf-ttEstOperation.isSpeedInLF                      = bf-mach.therm
            opbf-ttEstOperation.operationName                  = bf-mach.m-dscr
            opbf-ttEstOperation.feedType              = bf-mach.p-type
            opbf-ttEstOperation.outputType            = opbf-ttEstOperation.feedType
            opbf-ttEstOperation.departmentIDPrimary            = bf-mach.dept[1]
            opbf-ttEstOperation.departmentID                  = bf-mach.dept
            opbf-ttEstOperation.qtyInSetupWastePerColor        = bf-mach.col-wastesh
          
            /*Refactor - this is where we can have a different rate for setup vs. run*/
            opbf-ttEstOperation.costPerManHourDLRun            = bf-mach.lab-rate[bf-mach.lab-drate]
            opbf-ttEstOperation.costPerManHourDLSetup          = bf-mach.lab-rate[bf-mach.lab-drate]
            
            opbf-ttEstOperation.costPerHourFOSetup             = bf-mach.mr-fixoh
            opbf-ttEstOperation.costPerHourFORun               = bf-mach.run-fixoh
            opbf-ttEstOperation.costPerHourVOSetup             = bf-mach.mr-varoh
            opbf-ttEstOperation.costPerHourVORun               = bf-mach.run-varoh
            opbf-ttEstOperation.qtyInkLbsWastedPerSetup        = bf-mach.ink-waste
            opbf-ttEstOperation.qtyInkLbsWastedPerColorInSetup = bf-mach.col-wastelb
            .
       
        IF fIsDepartment("PR",opbf-ttEstOperation.departmentID) THEN  
            opbf-ttEstOperation.isPrinter = YES.
        IF fIsDepartment("CT",opbf-ttEstOperation.departmentID) THEN  
            opbf-ttEstOperation.isCoater = YES.
        IF fIsDepartment("RC,RS",opbf-ttEstOperation.departmentID)  THEN 
            ASSIGN 
                opbf-ttEstOperation.isNetSheetMaker = YES
                opbf-ttEstOperation.outputType = "S"
                .
        IF fIsDepartment("GL",opbf-ttEstOperation.departmentID)  THEN 
            opbf-ttEstOperation.isGluer = YES.
        IF fIsDepartment("WN,WS,FB,FS",opbf-ttEstOperation.departmentID)  THEN 
            opbf-ttEstOperation.isLeafer = YES.
        
        IF CAN-DO("R,S",opbf-ttEstOperation.feedType) THEN 
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
                        opbf-ttEstOperation.isBlankMaker = YES
                        opbf-ttEstOperation.outputType = "B"
                        .
                LEAVE.
            END.
            IF NOT AVAILABLE bf-est-op THEN /*Last Machine*/  
                ASSIGN 
                    opbf-ttEstOperation.isBlankMaker = YES
                    opbf-ttEstOperation.outputType = "B"
                    .
        END.
        
        
        IF opbf-ttEstOperation.isNetSheetMaker THEN 
            ASSIGN 
                opbf-ttEstOperation.numOutForOperation = ipbf-ttEstForm.numOutNet
                .
        ELSE IF opbf-ttEstOperation.isBlankMaker THEN 
                ASSIGN 
                    opbf-ttEstOperation.numOutForOperation = ipbf-ttEstForm.numOutBlanksOnNet
                    .
            ELSE 
                ASSIGN 
                    opbf-ttEstOperation.numOutForOperation = 1
                    .
        IF opbf-ttEstOperation.blankNo NE 0 THEN 
        DO:
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estCostHeaderID EQ opbf-ttEstOperation.estCostHeaderID
                AND ttEstBlank.estCostFormID EQ opbf-ttEstOperation.estCostFormID
                AND ttEstBlank.blankNo EQ opbf-ttEstOperation.blankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                opbf-ttEstOperation.estCostBlankID = ttEstBlank.estCostBlankID. 
        END.
    END.
    
END PROCEDURE.

PROCEDURE pAddGlue PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a glue
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstBlank  FOR ttEstBlank.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
  
    DEFINE           BUFFER bf-item          FOR item.
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
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is not valid", gcErrorImportant, ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo,ipbf-ttEstBlank.blankNo).
            RETURN.
        END.
        IF NOT CAN-DO(gcGlueMatTypes,bf-item.mat-type) THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid material but not a material type of " + gcGlueMatTypes, gcErrorImportant, ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo,ipbf-ttEstBlank.blankNo).
            RETURN.
        END.
        IF bf-item.sqin-lb EQ 0 AND bf-item.linin-lb EQ 0 THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid glue material but no coverage rate configured", gcErrorWarning, ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo,ipbf-ttEstBlank.blankNo).
            RETURN.
        END.
        
        FIND FIRST ttGlue EXCLUSIVE-LOCK 
            WHERE ttGlue.estHeaderID EQ ipbf-ttEstBlank.estCostHeaderID
            AND ttGlue.estFormID EQ ipbf-ttEstBlank.estCostFormID
            AND ttGlue.estBlankID EQ ipbf-ttEstBlank.estCostBlankID
            AND ttGlue.iFormNo EQ ipbf-ttEstBlank.formNo
            AND ttGlue.iBlankNo EQ ipbf-ttEstBlank.blankNo
            AND ttGlue.cItemID EQ ipbf-eb.adhesive
            NO-ERROR.
        IF NOT AVAILABLE ttGlue THEN 
        DO:
            CREATE ttGlue.
            ASSIGN 
                ttGlue.company       = ipbf-ttEstBlank.company
                ttGlue.estHeaderID   = ipbf-ttEstBlank.estCostHeaderID
                ttGlue.estFormID     = ipbf-ttEstBlank.estCostFormID
                ttGlue.estBlankID    = ipbf-ttEstBlank.estCostBlankID
                ttGlue.iFormNo       = ipbf-ttEstBlank.formNo
                ttGlue.iBlankNo      = ipbf-ttEstBlank.blankNo
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
                    dQtyRequiredPerBlank    = ipbf-ttEstBlank.blankArea / ttGlue.dCoverageRate
                    .
        ASSIGN
            ttGlue.dQtyRequiredPerBlank = ttGlue.dQtyRequiredPerBlank + dQtyRequiredPerBlank.
        .             
            
    END.

END PROCEDURE.

PROCEDURE pAddInk PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add an ink
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstBlank FOR ttEstBlank.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCoveragePercent AS DECIMAL NO-UNDO.
        
    DEFINE BUFFER bf-item FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Ink RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo, ipbf-ttEstBlank.blankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcInkMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Ink RM Code '" + ipcItemCode + "' is not one of '" + gcInkMatTypes + "'", gcErrorImportant,ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo, ipbf-ttEstBlank.blankNo).
            RETURN.
        END.
        FIND FIRST ttInk EXCLUSIVE-LOCK 
            WHERE ttInk.estHeaderID EQ ipbf-ttEstBlank.estCostHeaderID
            AND ttInk.estFormID EQ ipbf-ttEstBlank.estCostFormID
            AND ttInk.estBlankID EQ ipbf-ttEstBlank.estCostBlankID
            AND ttInk.iFormNo EQ ipbf-ttEstBlank.formNo
            AND ttInk.iBlankNo EQ ipbf-ttEstBlank.blankNo
            AND ttInk.cItemID EQ ipcItemCode
            AND ttInk.iPass EQ ipiPass
            NO-ERROR.
        IF NOT AVAILABLE ttInk THEN 
        DO:
            CREATE ttInk.
            ASSIGN 
                ttInk.company          = ipbf-ttEstBlank.company
                ttInk.estHeaderID      = ipbf-ttEstBlank.estCostHeaderID
                ttInk.estFormID        = ipbf-ttEstBlank.estCostFormID
                ttInk.estBlankID       = ipbf-ttEstBlank.estCostBlankID
                ttInk.iFormNo          = ipbf-ttEstBlank.formNo
                ttInk.iBlankNo         = ipbf-ttEstBlank.blankNo
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
            ttInk.dQtyRequiredPerBlank = ttInk.dCoveragePercent * ipbf-ttEstBlank.blankAreaNetWindow / ttInk.dCoverageRate
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

    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdLength AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth AS DECIMAL NO-UNDO.
            
    DEFINE BUFFER bf-item FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstForm.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Leaf/Film RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.formNo, ipiBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcLeafMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Leaf/Film RM Code '" + ipcItemCode + "' is not one of '" + gcLeafMatTypes + "'", gcErrorImportant,ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.formNo, ipiBlankNo).
            RETURN.
        END.
        FIND FIRST ttLeaf EXCLUSIVE-LOCK 
            WHERE ttLeaf.estHeaderID EQ ipbf-ttEstForm.estCostHeaderID
            AND ttLeaf.estFormID EQ ipbf-ttEstForm.estCostFormID
            AND ttLeaf.iFormNo EQ ipbf-ttEstForm.formNo
            AND ttLeaf.iBlankNo EQ ipiBlankNo
            AND ttLeaf.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE ttLeaf THEN 
        DO:
            CREATE ttLeaf.
            ASSIGN 
                ttLeaf.company             = ipbf-ttEstForm.company
                ttLeaf.estHeaderID         = ipbf-ttEstForm.estCostHeaderID
                ttLeaf.estFormID           = ipbf-ttEstForm.estCostFormID
                ttLeaf.iFormNo             = ipbf-ttEstForm.formNo
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
            
            FIND FIRST ttEstBlank EXCLUSIVE-LOCK 
                WHERE ttEstBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND ttEstBlank.estCostFormID EQ ttLeaf.estFormID
                AND ttEstBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                ASSIGN 
                    ttEstBlank.blankAreaWindow    = ttEstBlank.blankAreaWindow + ttLeaf.dAreaInSQIn
                    ttEstBlank.blankAreaNetWindow = ttEstBlank.blankArea - ttEstBlank.blankAreaWindow
                    .
            
        END.
        
    END.

END PROCEDURE.

PROCEDURE pAddPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given header, form, blank, and eb buffer, add a packing material
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ttEstBlank FOR ttEstBlank.
    DEFINE INPUT PARAMETER ipcItemCode AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttPack FOR ttPack.
        
    DEFINE           BUFFER bf-item     FOR item.

    IF ipcItemCode EQ "" THEN RETURN.
    FIND FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ ipbf-ttEstBlank.company
        AND bf-item.i-no EQ ipcItemCode
        NO-ERROR.

    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Invalid Pack RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo, ipbf-ttEstBlank.blankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcPackMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Packing RM Code '" + ipcItemCode + "' is not one of '" + gcPackMatTypes + "'", gcErrorImportant,ipbf-ttEstBlank.estCostHeaderID, ipbf-ttEstBlank.formNo, ipbf-ttEstBlank.blankNo).
            RETURN.
        END.
        FIND FIRST opbf-ttPack EXCLUSIVE-LOCK 
            WHERE opbf-ttPack.estHeaderID EQ ipbf-ttEstBlank.estCostHeaderID
            AND opbf-ttPack.estFormID EQ ipbf-ttEstBlank.estCostFormID
            AND opbf-ttPack.estBlankID EQ ipbf-ttEstBlank.estCostBlankID
            AND opbf-ttPack.iFormNo EQ ipbf-ttEstBlank.formNo
            AND opbf-ttPack.iBlankNo EQ ipbf-ttEstBlank.blankNo
            AND opbf-ttPack.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE opbf-ttPack THEN 
        DO:
            CREATE opbf-ttPack.
            ASSIGN 
                opbf-ttPack.company               = ipbf-ttEstBlank.company
                opbf-ttPack.estHeaderID           = ipbf-ttEstBlank.estCostHeaderID
                opbf-ttPack.estFormID             = ipbf-ttEstBlank.estCostFormID
                opbf-ttPack.estBlankID            = ipbf-ttEstBlank.estCostBlankID
                opbf-ttPack.iFormNo               = ipbf-ttEstBlank.formNo
                opbf-ttPack.iBlankNo              = ipbf-ttEstBlank.blankNo
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
    DEFINE PARAMETER BUFFER ipbf-ttEstMaterial FOR ttEstMaterial.

    IF CAN-DO(gcBoardMatTypes,ipbf-ttEstMaterial.materialType) THEN 
    DO:
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardNoWaste","Board Cost - No Waste",
            ipbf-ttEstMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardSetupWaste","Board Cost - Setup Waste",
            ipbf-ttEstMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardRunWaste","Board Cost - Run Waste",
            ipbf-ttEstMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardSetupVend","Board Cost - Vendor Setup",
            ipbf-ttEstMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardMinDiff","Board Cost - Minimum Diff",
            ipbf-ttEstMaterial.costTotalMinDiff,0).
    END.
    ELSE 
    DO:        
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matNoWaste","Board Cost - No Waste",
            ipbf-ttEstMaterial.costTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matSetupWaste","Board Cost - Setup Waste",
            ipbf-ttEstMaterial.costTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matRunWaste","Board Cost - Run Waste",
            ipbf-ttEstMaterial.costTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matSetupVend","Board Cost - Vendor Setup",
            ipbf-ttEstMaterial.costSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matMinDiff","Board Cost - Minimum Diff",
            ipbf-ttEstMaterial.costTotalMinDiff,0).
    END.    
    
        
END PROCEDURE.

PROCEDURE pBuildCostDetailForMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMisc FOR ttEstMisc.
    
    DEFINE VARIABLE cCostBin AS CHARACTER NO-UNDO.
    
    ASSIGN 
        cCostBin = IF ipbf-ttEstMisc.isPrep THEN "P" ELSE "M"
        cCostBin = cCostBin + ipbf-ttEstMisc.costType + ipbf-ttEstMisc.SIMON.
    
    CASE cCostBin:
        WHEN "PLabI" THEN 
            DO:  /*PrepLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabProfit","Prep Labor - Profit",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "PMatI" THEN  
            DO:  /*PrepMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatProfit","Prep Material - Profit",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END. 
        WHEN "PLabM" THEN 
            DO:  /*PrepLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabPrice","Prep Labor - Profit - Price",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "PMatM" THEN  
            DO:  /*PrepMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatPrice","Prep Material - Profit - Price",
                    ipbf-ttEstMisc.profitTotal,0).   
            END.                 
        WHEN "PLabS" OR 
        WHEN "PLabO" THEN 
            DO:  /*PrepLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCostSep","Prep Labor - Cost - Separate",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabProfitSep","Prep Labor - Profit - Separate",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "PMatS" OR 
        WHEN "PMatO" THEN  
            DO:  /*PrepMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCostSep","Prep Material - Cost - Separate",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatProfitSep","Prep Material - Profit - Separate",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END. 
        WHEN "MLabI" THEN 
            DO:  /*MiscLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabProfit","Misc Labor - Profit - COGS",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "MMatI" THEN  
            DO:  /*MiscMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatProfit","Misc Material - Profit - COGS",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END. 
        WHEN "MLabM" THEN 
            DO:  /*MiscLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabPrice","Misc Labor - Profit - Price",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "MMatM" THEN  
            DO:  /*MiscMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatPrice","Misc Material - Profit - Price",
                    ipbf-ttEstMisc.profitTotal,0).   
            END.                 
        WHEN "MLabS" OR 
        WHEN "MLabO" THEN 
            DO:  /*MiscLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCostSep","Misc Labor - Cost - Separate",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabProfitSep","Misc Labor - Profit - Separate",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END.
        WHEN "MMatS" OR 
        WHEN "MMatO" THEN  
            DO:  /*MiscMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCostSep","Misc Material - Cost - Separate",
                    ipbf-ttEstMisc.costTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatProfitSep","Misc Material - Profit - Separate",
                    ipbf-ttEstMisc.profitTotal,0).                    
            END. 
    END.
   
        
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
    DEFINE BUFFER bf-ttEstCostDetail FOR ttEstCostDetail.
    
    DEFINE BUFFER bf-ce-ctrl         FOR ce-ctrl.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    FOR EACH ttEstHeader EXCLUSIVE-LOCK,
        EACH ttEstForm NO-LOCK
        WHERE ttEstForm.estCostHeaderID EQ ttEstHeader.estCostHeaderID, 
        FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ttEstHeader.company
        AND bf-ce-ctrl.loc EQ ttEstHeader.warehouseID:
        IF ttEstHeader.directMaterialPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfMatMarkup", "Direct Material Markup", ttEstForm.costTotalMaterial * ttEstHeader.directMaterialPct, 0, BUFFER bf-ttEstCostDetail).
        DO iIndex = 1 TO 6:
            ttEstHeader.gsaMaterialPct = bf-ce-ctrl.mat-pct[iIndex] / 100.
            IF bf-ce-ctrl.mat-cost[iIndex] GT ttEstForm.costTotalMaterial THEN LEAVE.
        END. 
        IF ttEstHeader.gsaMaterialPct NE 0 THEN 
        DO:
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSAMat", "GSA Material", (ttEstForm.costTotalMaterial - ttEstForm.costTotalBoard) * ttEstHeader.gsaMaterialPct, 0, BUFFER bf-ttEstCostDetail).
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSABoard", "GSA Board", ttEstForm.costTotalBoard * ttEstHeader.gsaMaterialPct, 0, BUFFER bf-ttEstCostDetail).
        END.
        DO iIndex = 1 TO 6:
            ttEstHeader.gsaLaborPct = bf-ce-ctrl.lab-pct[iIndex] / 100.
            IF bf-ce-ctrl.lab-cost[iIndex] GT ttEstForm.costTotalLabor THEN LEAVE.
        END. 
        IF ttEstHeader.gsaLaborPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfGSALab", "GSA Labor", ttEstForm.costTotalLabor * ttEstHeader.gsaLaborPct, 0, BUFFER bf-ttEstCostDetail).
        
        
        IF ttEstHeader.warehouseMarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfWarehouse", "Warehousing", ttEstForm.costTotalFactory * ttEstHeader.warehouseMarkupPct, 0, BUFFER bf-ttEstCostDetail).
        
        /*Note - currently a defect with the Folding % is zeroed out during the calc process - this is fix*/
        IF ttEstHeader.foldPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfFolding", "Folding", ttEstForm.costTotalFactory * ttEstHeader.foldPct, 0, BUFFER bf-ttEstCostDetail).      
        
                                
        IF ttEstHeader.special1MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", ttEstForm.costTotalFactory * ttEstHeader.special1MarkupPct, 0, BUFFER bf-ttEstCostDetail).    
        IF ttEstHeader.special2MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", ttEstForm.costTotalFactory * ttEstHeader.special2MarkupPct, 0, BUFFER bf-ttEstCostDetail).
        IF ttEstHeader.special3MarkupPct NE 0 THEN 
            RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
                gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", ttEstForm.costTotalFactory * ttEstHeader.special3MarkupPct, 0, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef1", "Special Markup 1", ttEstHeader.special1FlatValue, 0, BUFFER bf-ttEstCostDetail).    
        RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef2", "Special Markup 2", ttEstHeader.special2FlatValue, 0, BUFFER bf-ttEstCostDetail).
        RUN pAddCostDetail(ttEstForm.estCostHeaderID, ttEstForm.estCostFormID, "", ttEstForm.estCostFormID, 
            gcSourceTypeNonFactory, "nfUserDef3", "Special Markup 3", ttEstHeader.special3FlatValue, 0, BUFFER bf-ttEstCostDetail).                                                                                                   
    END. /*Each ttEstHeader*/    

END PROCEDURE.

PROCEDURE pBuildCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.

    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupDL","Operation Setup DL",
        ipbf-ttEstOperation.costTotalDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupVO","Operation Setup VOH",
        ipbf-ttEstOperation.costTotalVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupFO","Operation Setup FOH",
        ipbf-ttEstOperation.costTotalFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunDL","Operation Run DL",
        ipbf-ttEstOperation.costTotalDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunVO","Operation Run VOH",
        ipbf-ttEstOperation.costTotalVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunFO","Operation Run FOH",
        ipbf-ttEstOperation.costTotalFORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupMinDiff","Operation Setup - Min Charge Diff",
        ipbf-ttEstOperation.costTotalMinDiffSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunMinDiff","Operation Run - Min Charge Diff",
        ipbf-ttEstOperation.costTotalMinDiffRun,0).
            
END PROCEDURE.

PROCEDURE pBuildFactoryCostDetails PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds the cost detail for all Factory Costs
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttEstHeader NO-LOCK:
        /*Process Operations*/
        FOR EACH ttEstOperation NO-LOCK 
            WHERE ttEstOperation.estCostHeaderID EQ ttEstHeader.estCostHeaderID:
            RUN pBuildCostDetailForOperation(BUFFER ttEstOperation).
        END. /*Each ttEstOperation for estHeader*/    
        /*Process Materials*/
        FOR EACH ttEstMaterial NO-LOCK 
            WHERE ttEstMaterial.estCostHeaderID EQ ttEstHeader.estCostHeaderID:
            RUN pBuildCostDetailForMaterial(BUFFER ttEstMaterial).                  
                    
        END. /*Each ttEstMaterial for estHeader*/
        FOR EACH ttEstMisc NO-LOCK 
            WHERE ttEstMisc.estCostHeaderID EQ ttEstHeader.estCostHeaderID:
            RUN pBuildCostDetailForMisc(BUFFER ttEstMisc).                  
                    
        END. /*Each ttEstMaterial for estHeader*/
    END. /*Each ttEstHeader*/    

END PROCEDURE.

PROCEDURE pBuildPackingForEb PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all packing infor for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstBlank  FOR ttEstBlank.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.
    
    DEFINE           BUFFER bf-ttPack        FOR ttPack.
    
    DEFINE VARIABLE dLayerDepth   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDividerDepth AS DECIMAL NO-UNDO.        

    IF ipbf-ttEstHeader.isUnitizedSet AND NOT ipbf-ttEstBlank.formNo EQ 0 THEN 
        RETURN.   /*Ignore non-form 0 packing for unitized set*/
    
    /*Case*/
    RUN pAddPacking(BUFFER ipbf-ttEstBlank, ipbf-eb.cas-no, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-ttEstBlank, ipbf-eb.tr-no, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-ttEstBlank, ipbf-eb.layer-pad, BUFFER bf-ttPack).
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
    RUN pAddPacking(BUFFER ipbf-ttEstBlank, ipbf-eb.divider, BUFFER bf-ttPack).
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
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstBlank  FOR ttEstBlank.
    DEFINE PARAMETER BUFFER ipbf-eb          FOR eb.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    IF ipbf-ttEstHeader.industry EQ gcIndustryFolding THEN
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code2):
        IF ipbf-eb.i-code2[iIndex] GT "" THEN
            RUN pAddInk(BUFFER ipbf-ttEstBlank, ipbf-eb.i-ps2[iIndex], ipbf-eb.i-code2[iIndex], ipbf-eb.i-dscr2[iIndex], ipbf-eb.i-%2[iIndex]).
    END.
    ELSE
    DO iIndex = 1 TO EXTENT(ipbf-eb.i-code):
        IF ipbf-eb.i-code[iIndex] GT "" THEN    
            RUN pAddInk(BUFFER ipbf-ttEstBlank, ipbf-eb.i-ps[iIndex], ipbf-eb.i-code[iIndex], ipbf-eb.i-dscr[iIndex], ipbf-eb.i-%[iIndex]).
    END.

END PROCEDURE.

PROCEDURE pBuildLeafForEf PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Process all inks for a given blank - does not calculate cost
     Notes:  REplaces ce/mach-ink1.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    DO iIndex = 1 TO 4:
        IF ipbf-ef.leaf[iIndex] NE "" THEN
            RUN pAddLeaf(BUFFER ipbf-ttEstForm, ipbf-ef.leaf[iIndex], ipbf-ef.leaf-dscr[iIndex], ipbf-ef.leaf-bnum[iIndex], ipbf-ef.leaf-l[iIndex], ipbf-ef.leaf-w[iIndex]).
    END.
    

END PROCEDURE.

PROCEDURE pBuildItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.

    DEFINE           BUFFER bf-ttEstItem     FOR ttEstItem.
    DEFINE           BUFFER bf-ttEstForm     FOR ttEstForm.
    DEFINE           BUFFER bf-ttEstBlank    FOR ttEstBlank.
    
    DEFINE VARIABLE iEstItemIDSetHeader AS INT64 NO-UNDO.
    
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-ttEstHeader.company
        AND eb.est-no EQ ipbf-ttEstHeader.estNo
        BY eb.form-no: 
        FIND FIRST bf-ttEstItem NO-LOCK
            WHERE bf-ttEstItem.estCostHeaderID EQ ipbf-ttEstHeader.estCostHeaderID
            AND bf-ttEstItem.customerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstItem THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstItem).
            IF eb.form-no EQ 0 THEN 
            DO:
                RUN pAddEstForm(ipbf-ttEstHeader.estCostHeaderID, 0, BUFFER bf-ttEstForm).
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstForm, BUFFER bf-ttEstBlank).
                IF eb.unitized THEN 
                DO:
                    ipbf-ttEstHeader.isUnitizedSet = YES. 
                    RUN pBuildPackingForEb(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                END.
                ASSIGN 
                    bf-ttEstItem.isSet = YES
                    iEstItemIDSetHeader = bf-ttEstItem.estCostItemID
                    .
            END.     
            ELSE 
                bf-ttEstItem.estCostItemIDParent = iEstItemIDSetHeader.           
        END. /*Create ttEstitem*/
               
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pCalcBlankPct PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Calculates the share of that the blank will have on the form to 
     proportinately allocate form costs to each blank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    
    DEFINE VARIABLE dTotalBlankAreaOnForm AS DECIMAL.
    
    IF ipbf-ttEstForm.blankArea GT 0 THEN 
        FOR EACH ttEstBlank EXCLUSIVE-LOCK 
            WHERE ttEstBlank.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
            AND ttEstBlank.estCostFormID EQ ipbf-ttEstForm.estCostFormID:
   
            ttEstBlank.pctOfForm = ttEstBlank.blankArea * ttEstBlank.numOut / ipbf-ttEstForm.blankArea. 
        END. 
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and category buffer, increment the appropriate fields
     on the form for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm         FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-estCostCategory FOR estCostCategory.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.

    FIND CURRENT ipbf-ttEstForm EXCLUSIVE-LOCK.
    IF ipbf-estCostCategory.includeInBoardCost THEN 
        ipbf-ttEstForm.costTotalBoard = ipbf-ttEstForm.costTotalBoard + ipdCost.
    IF ipbf-estCostCategory.includeInLaborCost THEN 
        ipbf-ttEstForm.costTotalLabor = ipbf-ttEstForm.costTotalLabor + ipdCost.
    IF ipbf-estCostCategory.includeInMaterialCost THEN        
        ipbf-ttEstForm.costTotalMaterial = ipbf-ttEstForm.costTotalMaterial + ipdCost.
    IF ipbf-estCostCategory.includeInFactoryCost THEN 
        ipbf-ttEstForm.costTotalFactory = ipbf-ttEstForm.costTotalFactory + ipdCost.
    IF ipbf-estCostCategory.includeInNonFactoryCost THEN 
        ipbf-ttEstForm.costTotalNonFactory = ipbf-ttEstForm.costTotalNonFactory + ipdCost.
                  
    FIND CURRENT ipbf-ttEstForm NO-LOCK.
    
END PROCEDURE.

PROCEDURE pCalcCostTotalsItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a item and category buffer, increment the appropriate fields
     on the item for cost totals.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstItem         FOR ttEstItem.
    DEFINE PARAMETER BUFFER ipbf-estCostCategory FOR estCostCategory.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.

    FIND CURRENT ipbf-ttEstItem EXCLUSIVE-LOCK.
    IF ipbf-estCostCategory.includeInBoardCost THEN 
        ipbf-ttEstItem.costTotalBoard = ipbf-ttEstItem.costTotalBoard + ipdCost.
    IF ipbf-estCostCategory.includeInLaborCost THEN 
        ipbf-ttEstItem.costTotalLabor = ipbf-ttEstItem.costTotalLabor + ipdCost.
    IF ipbf-estCostCategory.includeInMaterialCost THEN 
        ipbf-ttEstItem.costTotalMaterial = ipbf-ttEstItem.costTotalMaterial + ipdCost.
    IF ipbf-estCostCategory.includeInFactoryCost THEN 
        ipbf-ttEstItem.costTotalFactory = ipbf-ttEstItem.costTotalFactory + ipdCost.
    IF ipbf-estCostCategory.includeInNonFactoryCost THEN 
        ipbf-ttEstItem.costTotalFull = ipbf-ttEstItem.costTotalFull + ipdCost.          
    FIND CURRENT ipbf-ttEstItem NO-LOCK.
    
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

PROCEDURE pProcessMiscNonPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the ttEstMisc for non-prep mis items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef        FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DO iIndex = 1 TO 6:
        IF INDEX("IM",ipbf-ef.mis-simon[iIndex]) EQ 0 OR ipbf-ef.mis-cost[iIndex] EQ "" THEN NEXT.
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-ttEstForm, ipdQuantity, iIndex, "Material").
        RUN pAddEstMiscForForm(BUFFER ipbf-ef, BUFFER ipbf-ttEstForm, ipdQuantity, iIndex, "Labor").
    END.

END PROCEDURE.

PROCEDURE pProcessMiscPrep PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an ef buffer, build the ttEstMisc for prep misc items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef        FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.

    FOR EACH est-prep NO-LOCK 
        WHERE est-prep.company EQ ipbf-ef.company
        AND est-prep.est-no EQ ipbf-ef.est-no
        AND est-prep.s-num EQ ipbf-ef.form-no
        AND est-prep.code NE "":
        RUN pAddEstMiscForPrep(BUFFER est-prep, BUFFER ipbf-ttEstForm, ipdQuantity).
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
                    ttEstHeader.estCostHeaderID = INT64(ipcData[1])
                    ttEstHeader.company         = FILL("0",3 - LENGTH(ipcData[2])) + ipcData[2]
                    ttEstHeader.estNo           = FILL(" ",8 - LENGTH(ipcData[3])) + ipcData[3]
                    ttEstHeader.estType         = ipcData[4]
                    ttEstHeader.qtyMaster       = DECIMAL(ipcData[5])
                    ttEstHeader.calculatedBy    = ipcData[6]
                    ttEstHeader.calcDateTime    = DATETIME(ipcData[7])
                    .
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
                    estCostCategory.estCostCategoryID        = ipcData[1]
                    estCostCategory.costCategoryLabel           = ipcData[2]
                    estCostCategory.estCostCategoryDesc     = ipcData[3]
//                    ttEstCostCategory.cBasis                   = ipcData[4]
                    estCostCategory.estCostGroupID           = ipcData[5]
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
                CREATE ttEstItem.
                ASSIGN
                    ttEstItem.estCostItemID     = INT64(ipcData[1])
                    ttEstItem.estCostHeaderID   = INT64(ipcData[2])
                    ttEstItem.customerPart      = ipcData[3]
                    ttEstItem.qtyPerSet         = DECIMAL(ipcData[4])
                    ttEstItem.qtyRequired       = DECIMAL(ipcData[5])
                    ttEstItem.qtyYielded        = DECIMAL(ipcData[6])
                    ttEstItem.itemName          = ipcData[7]
                    ttEstItem.itemDescription1  = ipcData[8]
                    ttEstItem.itemDescription2  = ipcData[9]
                    ttEstItem.styleID           = ipcData[10]
                    ttEstItem.styleDesc         = ipcData[11]
                    ttEstItem.isSet             = LOGICAL(ipcData[12])
                    ttEstItem.customerName      = ipcData[13]
                    ttEstItem.customerAddress1  = ipcData[14]
                    ttEstItem.customerAddress2  = ipcData[15]
                    ttEstItem.customerAddress3  = ipcData[16]
                    ttEstItem.shipToName        = ipcData[17]
                    ttEstItem.shipToAddress1    = ipcData[18]
                    ttEstItem.shipToAddress2    = ipcData[19]
                    ttEstItem.shipToAddress3    = ipcData[20]
                    ttEstItem.salesgroupName    = ipcData[21]
                    ttEstItem.customerID        = ipcData[22]
                    ttEstItem.shipToID          = ipcData[23]
                    ttEstItem.salesgroupID      = ipcData[24]
                    ttEstItem.colorDesc         = ipcData[25]
                    .
                    
            END.
    END CASE.
END PROCEDURE.

PROCEDURE pProcessOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a ttEstHeader, build all ttEstOperations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader  FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm    FOR ttEstForm.
    
    DEFINE           BUFFER bf-ttEstOperation FOR ttEstOperation.
    DEFINE VARIABLE dQtyInOut           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutRunWaste   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQtyTarget          AS DECIMAL NO-UNDO.
    
    dQtyInOut = ipbf-ttEstForm.qtyFGOnForm.
    
    /*Get the effective Est-op quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstHeader.company 
        AND est-op.est-no  EQ ipbf-ttEstHeader.estNo 
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
            IF est-op.qty GE ipbf-ttEstHeader.qtyMaster THEN LEAVE.
        END.
    END.
    
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstHeader.company
        AND est-op.est-no EQ ipbf-ttEstHeader.estNo
        AND est-op.s-num EQ ipbf-ttEstForm.formNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:
        
    IF est-op.b-num NE 0 THEN 
    DO:  /*Calculate for Combo*/    
        FIND FIRST ttEstBlank NO-LOCK 
            WHERE ttEstBlank.estCostHeaderID EQ ipbf-ttEstHeader.estCostHeaderID
            AND ttEstBlank.estCostFormID EQ ipbf-ttEstForm.estCostFormID
            AND ttEstBlank.blankNo EQ est-op.b-num
            AND NOT ttEstBlank.lOutputInitialized
            NO-ERROR.
        IF AVAILABLE ttEstBlank THEN 
            ASSIGN 
                ttEstBlank.lOutputInitialized = YES
                dQtyInOut                     = ttEstBlank.qtyRequired
                .
    END.
           
    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-ttEstForm, BUFFER bf-ttEstOperation).
                
    IF AVAILABLE bf-ttEstOperation THEN 
    DO: 
        RUN pProcessOperation(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER bf-ttEstOperation, INPUT-OUTPUT dQtyInOut, 
            INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
        RUN pCalcEstOperation(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstOperation, BUFFER ipbf-ttEstForm).                    
    END.
        
END.
ASSIGN 
    ipbf-ttEstForm.grossQtyRequiredSetupWaste = dQtyInOutSetupWaste
    ipbf-ttEstForm.grossQtyRequiredRunWaste   = dQtyInOutRunWaste
    ipbf-ttEstForm.grossQtyRequiredTotal      = ipbf-ttEstForm.grossQtyRequiredNoWaste + ipbf-ttEstForm.grossQtyRequiredRunWaste + ipbf-ttEstForm.grossQtyRequiredSetupWaste
    .
        
END PROCEDURE.

PROCEDURE pProcessSpecialMaterials PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an EF, processes the special materials (Misc/Sub) tab
     Notes: replaces ce/pr4-spe.p, ce/pr4-spe.i
    ------------------------------------------------------------------------------*/

    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    DEFINE           BUFFER bf-item          FOR ITEM.
    DEFINE           BUFFER bf-ttEstMaterial FOR ttEstMaterial.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    DO iIndex = 1 TO 8:
        IF ipbf-ef.spec-no[iIndex] NE "" THEN 
        DO:
            RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipbf-ef.spec-no[iIndex], 1, BUFFER bf-ttEstMaterial).
            IF AVAILABLE bf-ttEstMaterial THEN 
            DO: 
                
                /*REFACTOR - ugly.  work around to support more than 2 decimals as stored in db*/
                RUN custom/extradec.p (.0001, ipbf-ef.spec-qty[iIndex] * ipbf-ttEstForm.qtyFGOnForm,
                    OUTPUT bf-ttEstMaterial.qtyRequiredNoWaste).
                    
                ASSIGN            
                    bf-ttEstMaterial.itemName = ipbf-ef.spec-dscr[iIndex]
                    bf-ttEstMaterial.qtyUOM   = "EA"
                    .
                RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
            END.
        END.         
    END.
END PROCEDURE.

PROCEDURE pCalcCostTotals PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details and totals Forms and Items for 
     use in % Total Calculations
     Notes:
    ------------------------------------------------------------------------------*/
    
    FOR EACH ttEstCostDetail NO-LOCK, 
        FIRST ttEstHeader NO-LOCK 
        WHERE ttEstHeader.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID,
        FIRST ttEstForm NO-LOCK
        WHERE ttEstForm.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
        AND ttEstForm.estCostFormID EQ ttEstCostDetail.estCostFormID, 
        FIRST estCostCategory NO-LOCK 
        WHERE estCostCategory.estCostCategoryID EQ ttEstCostDetail.estCostCategoryID 
        :
        
        RUN pCalcCostTotalsForm(BUFFER ttEstForm, BUFFER estCostCategory,ttEstCostDetail.costTotal).
        
        FIND FIRST ttEstBlank NO-LOCK 
            WHERE ttEstBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
            AND ttEstBlank.estCostFormID EQ ttEstCostDetail.estCostFormID
            AND ttEstBlank.estCostBlankID EQ ttEstCostDetail.estCostBlankID
            NO-ERROR.
        IF AVAILABLE ttEstBlank THEN 
        DO:  /*Blank-specific cost*/
            FIND FIRST ttEstItem NO-LOCK 
                WHERE ttEstItem.estCostHeaderID EQ ttEstBlank.estCostHeaderID
                AND ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID
                NO-ERROR.
            IF AVAILABLE ttEstItem THEN 
                RUN pCalcCostTotalsItem(BUFFER ttEstItem, BUFFER estCostCategory, ttEstCostDetail.costTotal).
        END.
        ELSE /*Divide up the Form-level Costs into each item*/
            FOR EACH ttEstBlank NO-LOCK
                WHERE ttEstBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
                AND ttEstBlank.estCostFormID EQ ttEstCostDetail.estCostFormID, 
                FIRST ttEstItem NO-LOCK  
                WHERE ttEstItem.estCostHeaderID EQ ttEstBlank.estCostHeaderID
                AND ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID :
                RUN pCalcCostTotalsItem(BUFFER ttEstItem, BUFFER estCostCategory, ttEstCostDetail.costTotal * ttEstBlank.pctOfForm).
            END.
    END.        
        
END PROCEDURE.

PROCEDURE pBuildCostSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details for a given header and creates summary
              records for each
     Notes:
    ------------------------------------------------------------------------------*/
    
    FOR EACH ttEstCostDetail NO-LOCK, 
        FIRST ttEstHeader NO-LOCK 
        WHERE ttEstHeader.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID,
        FIRST ttEstForm NO-LOCK
        WHERE ttEstForm.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
        AND ttEstForm.estCostFormID EQ ttEstCostDetail.estCostFormID, 
        FIRST estCostCategory NO-LOCK 
        WHERE estCostCategory.estCostCategoryID EQ ttEstCostDetail.estCostCategoryID 
        :
        
        RUN pAddCostSummary(ttEstForm.rec_key, estCostCategory.estCostGroupID, ttEstCostDetail.costTotal, ttEstForm.qtyFGOnForm / 1000).
        
        FIND FIRST ttEstBlank NO-LOCK 
            WHERE ttEstBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
            AND ttEstBlank.estCostFormID EQ ttEstCostDetail.estCostFormID
            AND ttEstBlank.estCostBlankID EQ ttEstCostDetail.estCostBlankID
            NO-ERROR.
        IF AVAILABLE ttEstBlank THEN 
        DO:  /*Blank-specific cost*/
            FIND FIRST ttEstItem NO-LOCK 
                WHERE ttEstItem.estCostHeaderID EQ ttEstBlank.estCostHeaderID
                AND ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID
                NO-ERROR.
            IF AVAILABLE ttEstItem THEN 
                RUN pAddCostSummary(ttEstItem.rec_key, estCostCategory.estCostGroupID, ttEstCostDetail.costTotal, ttEstItem.qtyRequired / 1000).
           
        END.
        ELSE /*Divide up the Form-level Costs into each item*/
            FOR EACH ttEstBlank NO-LOCK
                WHERE ttEstBlank.estCostHeaderID EQ ttEstCostDetail.estCostHeaderID
                AND ttEstBlank.estCostFormID EQ ttEstCostDetail.estCostFormID, 
                FIRST ttEstItem NO-LOCK  
                WHERE ttEstItem.estCostHeaderID EQ ttEstBlank.estCostHeaderID
                AND ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID :
                RUN pAddCostSummary(ttEstItem.rec_key, estCostCategory.estCostGroupID, ttEstCostDetail.costTotal * ttEstBlank.pctOfForm, ttEstItem.qtyRequired / 1000).
           
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
        
    DEFINE BUFFER bf-ttEstForm      FOR ttEstForm.
    DEFINE BUFFER bf-ttEstBlank     FOR ttEstBlank.
    DEFINE BUFFER bf-ttEstMaterial  FOR ttEstMaterial.
    DEFINE BUFFER bf-ttEstOperation FOR ttEstOperation.
    DEFINE BUFFER bf-estCostGroupLevel FOR estCostGroupLevel.
    DEFINE BUFFER bf-estCostGroup FOR estCostGroup.
    DEFINE BUFFER bf-estCostCategory FOR estCostCategory.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyOnForm          AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttEstHeader.
    EMPTY TEMP-TABLE ttEstItem.
    EMPTY TEMP-TABLE ttEstForm.
    EMPTY TEMP-TABLE ttEstBlank.
    EMPTY TEMP-TABLE ttEstMisc.
    EMPTY TEMP-TABLE ttEstMaterial.
    EMPTY TEMP-TABLE ttEstOperation.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
 //   EMPTY TEMP-TABLestCostCategoryry.
 //   EMPTY TEMP-TABLE ttEstCostGroup.
 //   EMPTY TEMP-TABLE ttEstCostGroupLevel.
    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
    RUN pLoadData("EstHeader").

    IF NOT CAN-FIND(FIRST bf-estCostGroupLevel) OR iplReset THEN DO: 
        FOR EACH bf-estCostGroupLevel:
            DELETE bf-estCostGroupLevel.
        END.
        RELEASE bf-estCostGroupLevel.
        RUN pLoadData("CostGroupLevel").
    END. 
    IF NOT CAN-FIND(FIRST bf-estCostGroup) OR iplReset THEN DO: 
        FOR EACH bf-estCostGroup:
            DELETE bf-estCostGroup.
        END.
        RELEASE bf-estCostGroup.    
        RUN pLoadData("CostGroup").
    END.
    IF NOT CAN-FIND(FIRST bf-estCostCategory) OR iplReset THEN DO: 
        FOR EACH bf-estCostCategory:
            DELETE bf-estCostCategory.
        END.
        RELEASE bf-estCostCategory.    
        RUN pLoadData("CostCategory").
    END.
   
    
    RUN pSetGlobalSettings(ipcCompany).
    
    FOR EACH ttEstHeader NO-LOCK:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ ttEstHeader.company
            AND est.est-no EQ ttEstHeader.estNo
            NO-ERROR.
        IF NOT AVAILABLE est THEN RETURN.
        RUN pBuildHeader(BUFFER ttEstHeader).
        RUN pBuildItems(BUFFER ttEstHeader).
       
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no:
            
            RUN pAddEstFormFromEf(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            
            ASSIGN 
                iNumOutBlanksOnForm = 0
                dQtyOnForm          = 0
                .
            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstForm, BUFFER bf-ttEstBlank).
                ASSIGN 
                    iNumOutBlanksOnForm = iNumOutBlanksOnForm + bf-ttEstBlank.numOut
                    dQtyOnForm          = dQtyOnForm + bf-ttEstBlank.qtyRequired
                    .
                RUN pBuildInksForEb(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                RUN pAddGlue(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                RUN pBuildPackingForEb(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                
            END. /*Each eb of ef*/
            
            ASSIGN 
                bf-ttEstForm.numOut                  = iNumOutBlanksOnForm * bf-ttEstForm.numOutNet
                bf-ttEstForm.qtyFGOnForm             = dQtyOnForm
                bf-ttEstForm.grossQtyRequiredNoWaste = bf-ttEstForm.qtyFGOnForm / bf-ttEstForm.numOut
                .
                
            RUN pCalcBlankPct(BUFFER bf-ttEstForm).                
            RUN pProcessOperations(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessLeafs(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessBoard(BUFFER ttEstHeader, BUFFER bf-ttEstForm, ef.board).           
            RUN pProcessInks(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessPacking(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessGlues(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessSpecialMaterials(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).  
            RUN pProcessMiscPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.qtyMaster).
            RUN pProcessMiscNonPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.qtyMaster).          
        END.  /*Each ef of est*/  
             
    END. /*each ttEstHeader*/
        
END PROCEDURE.


PROCEDURE pCalcEstMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a ttEstMaterial buffer, calculate simple calculated fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader   FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstMaterial FOR ttEstMaterial.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm     FOR ttEstForm.

    ipbf-ttEstMaterial.qtyRequiredTotal    = ipbf-ttEstMaterial.qtyRequiredNoWaste + ipbf-ttEstMaterial.qtyRequiredSetupWaste + 
        ipbf-ttEstMaterial.qtyRequiredRunWaste + ipbf-ttEstMaterial.qtyRequiredMinDiff.    
    IF ipbf-ttEstMaterial.costOverridePerUOM EQ 0 THEN 
        RUN pGetEstMaterialCosts(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstMaterial,ipbf-ttEstMaterial.qtyRequiredTotal,ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.vendorID, 
            OUTPUT ipbf-ttEstMaterial.costPerUOM, OUTPUT ipbf-ttEstMaterial.costUOM,  OUTPUT ipbf-ttEstMaterial.costSetup ).
    ELSE 
        ipbf-ttEstMaterial.costPerUOM = ipbf-ttEstMaterial.costOverridePerUOM.
    IF ipbf-ttEstMaterial.costUOM EQ "" THEN 
        ipbf-ttEstMaterial.costUOM = "EA".
    IF ipbf-ttEstMaterial.costUOM NE ipbf-ttEstMaterial.qtyUOM THEN 
    DO:
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.costUOM, 
            ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
            ipbf-ttEstMaterial.qtyRequiredNoWaste, OUTPUT ipbf-ttEstMaterial.qtyRequiredNoWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.costUOM, 
            ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
            ipbf-ttEstMaterial.qtyRequiredSetupWaste, OUTPUT ipbf-ttEstMaterial.qtyRequiredSetupWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.costUOM, 
            ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
            ipbf-ttEstMaterial.qtyRequiredRunWaste, OUTPUT ipbf-ttEstMaterial.qtyRequiredRunWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.costUOM, 
            ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
            ipbf-ttEstMaterial.qtyRequiredMinDiff, OUTPUT ipbf-ttEstMaterial.qtyRequiredMinDiffInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.qtyUOM, ipbf-ttEstMaterial.costUOM, 
            ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
            ipbf-ttEstMaterial.qtyRequiredTotal, OUTPUT ipbf-ttEstMaterial.qtyRequiredTotalInCostUOM).
    END.
    ELSE 
        ASSIGN 
            ipbf-ttEstMaterial.qtyRequiredNoWasteInCostUOM    = ipbf-ttEstMaterial.qtyRequiredNoWaste
            ipbf-ttEstMaterial.qtyRequiredSetupWasteInCostUOM = ipbf-ttEstMaterial.qtyRequiredSetupWaste
            ipbf-ttEstMaterial.qtyRequiredRunWasteInCostUOM   = ipbf-ttEstMaterial.qtyRequiredRunWaste
            ipbf-ttEstMaterial.qtyRequiredMinDiffInCostUOM    = ipbf-ttEstMaterial.qtyRequiredMinDiff
            ipbf-ttEstMaterial.qtyRequiredTotalInCostUOM      = ipbf-ttEstMaterial.qtyRequiredTotal
            .
    ASSIGN 
        ipbf-ttEstMaterial.costTotalNoWaste                = ipbf-ttEstMaterial.qtyRequiredNoWasteInCostUOM * ipbf-ttEstMaterial.costPerUOM
        ipbf-ttEstMaterial.costTotalSetupWaste             = ipbf-ttEstMaterial.qtyRequiredSetupWasteInCostUOM * ipbf-ttEstMaterial.costPerUOM + 
                                                             ipbf-ttEstMaterial.costSetup
        ipbf-ttEstMaterial.costTotalRunWaste               = ipbf-ttEstMaterial.qtyRequiredRunWasteInCostUOM * ipbf-ttEstMaterial.costPerUOM
        ipbf-ttEstMaterial.costTotalMinDiff                = ipbf-ttEstMaterial.qtyRequiredMinDiffInCostUOM * ipbf-ttEstMaterial.costPerUOM
        ipbf-ttEstMaterial.costTotal                       = ipbf-ttEstMaterial.costTotalNoWaste + ipbf-ttEstMaterial.costTotalSetupWaste + 
                                                             ipbf-ttEstMaterial.costTotalRunWaste + ipbf-ttEstMaterial.costTotalMinDiff
        ipbf-ttEstMaterial.costTotalPerMFinished           = ipbf-ttEstMaterial.costTotal / (ipbf-ttEstForm.qtyFGOnForm / 1000)
        ipbf-ttEstMaterial.costTotalPerMFinishedNoWaste    = ipbf-ttEstMaterial.costTotalNoWaste / (ipbf-ttEstForm.qtyFGOnForm / 1000)
        ipbf-ttEstMaterial.costTotalPerMFinishedSetupWaste = ipbf-ttEstMaterial.costTotalSetupWaste  / (ipbf-ttEstForm.qtyFGOnForm / 1000)
        ipbf-ttEstMaterial.costTotalPerMFinishedRunWaste   = ipbf-ttEstMaterial.costTotalRunWaste / (ipbf-ttEstForm.qtyFGOnForm / 1000)
        .        
    
END PROCEDURE.

PROCEDURE pCalcEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a ttEstMisc buffer, calculate common fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMisc FOR ttEstMisc.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.

    ipbf-ttEstMisc.profitTotal = fGetProfit(ipbf-ttEstMisc.costTotalBeforeProfit, ipbf-ttEstMisc.profitPercent, ipbf-ttEstMisc.profitPercentType).
    IF ipbf-ttEstMisc.SIMON EQ "M" THEN 
        ipbf-ttEstMisc.costTotal = ipbf-ttEstMisc.costTotalBeforeProfit.
    ELSE 
        ipbf-ttEstMisc.costTotal = ipbf-ttEstMisc.costTotalBeforeProfit + ipbf-ttEstMisc.profitTotal.
    ipbf-ttEstMisc.costTotalPerMFinished = ipbf-ttEstMisc.costTotal / (ipbf-ttEstForm.qtyFGOnForm / 1000).

END PROCEDURE.

PROCEDURE pCalcEstOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a ttEstOperation buffer, calculate simple calculated fields
     Notes: Should replace end if pr4-mch.p
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    
    DEFINE VARIABLE lApplyMinChargeOnRun   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lApplyMinChargeOnSetup AS LOGICAL NO-UNDO.
    
    IF ipbf-ttEstOperation.speed NE 0 THEN
        IF ipbf-ttEstOperation.isSpeedInLF THEN
            ipbf-ttEstOperation.hoursRun = ipbf-ttEstOperation.qtyInAfterSetupWasteLF / ipbf-ttEstOperation.speed. 
        ELSE 
            ipbf-ttEstOperation.hoursRun = ipbf-ttEstOperation.qtyInAfterSetupWaste / ipbf-ttEstOperation.speed.
    ELSE 
        ipbf-ttEstOperation.hoursRun = 0.
    
    ASSIGN    
        ipbf-ttEstOperation.costPerHourTotalRun   = ipbf-ttEstOperation.costPerManHourDLRun * ipbf-ttEstOperation.crewSizeRun + 
                                                     ipbf-ttEstOperation.costPerHourFORun + ipbf-ttEstOperation.costPerHourVORun
        ipbf-ttEstOperation.costPerHourTotalSetup = ipbf-ttEstOperation.costPerManHourDLSetup * ipbf-ttEstOperation.crewSizeSetup + 
                                                     ipbf-ttEstOperation.costPerHourFOSetup + ipbf-ttEstOperation.costPerHourVOSetup
        ipbf-ttEstOperation.costTotalDLSetup      = ipbf-ttEstOperation.hoursSetup * ipbf-ttEstOperation.crewSizeSetup * ipbf-ttEstOperation.costPerManHourDLSetup
        ipbf-ttEstOperation.costTotalVOSetup      = ipbf-ttEstOperation.hoursSetup * ipbf-ttEstOperation.costPerHourVOSetup
        ipbf-ttEstOperation.costTotalFOSetup      = ipbf-ttEstOperation.hoursSetup * ipbf-ttEstOperation.costPerHourFOSetup
        ipbf-ttEstOperation.costTotalDLRun        = ipbf-ttEstOperation.hoursRun * ipbf-ttEstOperation.crewSizeRun * ipbf-ttEstOperation.costPerManHourDLRun
        ipbf-ttEstOperation.costTotalVORun        = ipbf-ttEstOperation.hoursRun * ipbf-ttEstOperation.costPerHourVORun
        ipbf-ttEstOperation.costTotalFORun        = ipbf-ttEstOperation.hoursRun * ipbf-ttEstOperation.costPerHourFORun
        ipbf-ttEstOperation.costTotalSetup        = ipbf-ttEstOperation.hoursSetup * ipbf-ttEstOperation.costPerHourTotalSetup
        ipbf-ttEstOperation.costTotalRun          = ipbf-ttEstOperation.hoursRun * ipbf-ttEstOperation.costPerHourTotalRun
        ipbf-ttEstOperation.costTotal             = ipbf-ttEstOperation.costTotalRun + ipbf-ttEstOperation.costTotalSetup 
        .

END PROCEDURE.

PROCEDURE pProcessBoard PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ttEstMaterial FOR ttEstMaterial.
    DEFINE BUFFER bf-item          FOR ITEM.
    
    FIND FIRST bf-item NO-LOCK 
        WHERE bf-item.company EQ ipbf-ttEstForm.company
        AND bf-item.i-no EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-item THEN 
    DO:
        RUN pAddError("Board '" + ipcITemID + "' is not valid", gcErrorImportant, ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.formNo, 0).
        RETURN.
    END.
    IF NOT CAN-DO(gcBoardMatTypes,bf-item.mat-type) THEN 
    DO:
        RUN pAddError("Board '" + ipcITemID + "' is valid material but not a material type of " + gcBoardMatTypes, gcErrorImportant, ipbf-ttEstForm.estCostHeaderID, ipbf-ttEstForm.formNo, 0).
        RETURN.
    END.
    RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipcItemID, 0, BUFFER bf-ttEstMaterial).
    ASSIGN 
        bf-ttEstMaterial.isPrimarySubstrate    = YES
        bf-ttEstMaterial.addToWeightFG         = YES
        bf-ttEstMaterial.addToWeightTare       = NO
                                       
        bf-ttEstMaterial.qtyRequiredNoWaste    = ipbf-ttEstForm.grossQtyRequiredNoWaste
        bf-ttEstMaterial.qtyRequiredSetupWaste = ipbf-ttEstForm.grossQtyRequiredSetupWaste
        bf-ttEstMaterial.qtyRequiredRunWaste   = ipbf-ttEstForm.grossQtyRequiredRunWaste
        bf-ttEstMaterial.qtyUOMWaste           = "EA"
        bf-ttEstMaterial.qtyUOM                = "EA"
        bf-ttEstMaterial.basisWeight           = bf-item.basis-w
        bf-ttEstMaterial.dimWidth              = ipbf-ttEstForm.grossWidth
        bf-ttEstMaterial.dimLength             = ipbf-ttEstForm.grossLength
        bf-ttEstMaterial.dimDepth              = ipbf-ttEstForm.grossDepth
        .
    IF ipbf-ttEstForm.costOverridePerUOM NE 0 THEN 
        ASSIGN 
            bf-ttEstMaterial.costOverridePerUOM = ipbf-ttEstForm.costOverridePerUOM
            bf-ttEstMaterial.costUOM            = ipbf-ttEstForm.costOverrideUOM
            .
        
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).   
    
    ASSIGN 
        ipbf-ttEstForm.grossQtyRequiredTotal       = ipbf-ttEstForm.grossQtyRequiredNoWaste + ipbf-ttEstForm.grossQtyRequiredSetupWaste + ipbf-ttEstForm.grossQtyRequiredRunWaste
        ipbf-ttEstForm.grossQtyRequiredTotalArea   = ipbf-ttEstForm.grossQtyRequiredTotal * ipbf-ttEstForm.grossArea / 1000
        ipbf-ttEstForm.grossQtyRequiredTotalWeight = ipbf-ttEstForm.grossQtyRequiredTotalArea * ipbf-ttEstForm.basisWeight
        .
    
    IF ipbf-ttEstForm.noCost THEN 
        ASSIGN 
            bf-ttEstMaterial.costPerUOM                      = 0
            bf-ttEstMaterial.costSetup                       = 0
            bf-ttEstMaterial.costTotal                       = 0
            bf-ttEstMaterial.costTotalMinDiff                = 0
            bf-ttEstMaterial.costTotalNoWaste                = 0
            bf-ttEstMaterial.costTotalSetupWaste             = 0
            bf-ttEstMaterial.costTotalRunWaste               = 0
            bf-ttEstMaterial.costTotalPerMFinished           = 0
            bf-ttEstMaterial.costTotalPerMFinishedNoWaste    = 0
            bf-ttEstMaterial.costTotalPerMFinishedRunWaste   = 0
            bf-ttEstMaterial.costTotalPerMFinishedSetupWaste = 0
            .
            
END PROCEDURE.

PROCEDURE pProcessGlues PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for glues with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    DEFINE           BUFFER bf-ttEstMaterial FOR ttEstMaterial.
    DEFINE VARIABLE dQtyRequiredMinDiff AS DECIMAL NO-UNDO.
    
    FOR FIRST ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttEstOperation.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttEstOperation.isGluer, 
        EACH ttGlue NO-LOCK
        WHERE ttGlue.estHeaderID EQ ttEstOperation.estCostHeaderID
        AND ttGlue.estFormID EQ ttEstOperation.estCostFormID
        AND ttGlue.estBlankID EQ ttEstOperation.estCostBlankID
        ,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttGlue.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttGlue.estFormID 
        AND ttEstBlank.estCostBlankID EQ ttGlue.estBlankID 
        BY ttEstOperation.sequenceOfOperation DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttGlue.cItemID, ttGlue.iBlankNo, BUFFER bf-ttEstMaterial).
        
        ASSIGN    
            bf-ttEstMaterial.qtyRequiredNoWaste    = ttEstOperation.qtyInNoWaste * ttGlue.dQtyRequiredPerBlank
            bf-ttEstMaterial.qtyRequiredRunWaste   = ttEstOperation.qtyInRunWaste * ttGlue.dQtyRequiredPerBlank
            bf-ttEstMaterial.qtyRequiredSetupWaste = ttEstOperation.qtyInSetupWaste * ttGlue.dQtyRequiredPerBlank
            dQtyRequiredMinDiff                     = ttGlue.dMinLbsPerJob - 
                                                    (bf-ttEstMaterial.qtyRequiredNoWaste + bf-ttEstMaterial.qtyRequiredRunWaste + bf-ttEstMaterial.qtyRequiredSetupWaste)
            bf-ttEstMaterial.qtyUOM                = ttGlue.cQtyUOM
            .             
        IF dQtyRequiredMinDiff GT 0 THEN 
            bf-ttEstMaterial.qtyRequiredMinDiff = dQtyRequiredMinDiff.
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
        
    END.

END PROCEDURE.

PROCEDURE pBuildHeader PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Builds all fields on the ttEstHeader record
    Notes:
    ------------------------------------------------------------------------------*/
    
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    
    DEFINE           BUFFER bf-est           FOR est.
    DEFINE           BUFFER bf-ce-ctrl       FOR ce-ctrl.
     
    FIND FIRST bf-est NO-LOCK 
        WHERE bf-est.company EQ ipbf-ttEstHeader.company
        AND bf-est.est-no EQ ipbf-ttEstHeader.estNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Estimate '" + ipbf-ttEstHeader.estNo + "' not valid", gcErrorCritical, ipbf-ttEstHeader.estCostHeaderID, 0,0).
        RETURN.
    END. 
    FIND FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ipbf-ttEstHeader.company
        AND bf-ce-ctrl.loc EQ bf-est.loc
        NO-ERROR.
    IF NOT AVAILABLE bf-ce-ctrl THEN 
        FIND FIRST bf-ce-ctrl NO-LOCK 
            WHERE bf-ce-ctrl.company EQ ipbf-ttEstHeader.company
            NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Control File not found for company '" + ipbf-ttEstHeader.company + "'", gcErrorCritical, ipbf-ttEstHeader.estCostHeaderID, 0,0).
        RETURN.
    END.
    ASSIGN 
        ipbf-ttEstHeader.industry                    = IF bf-est.est-type LE 4 THEN gcIndustryFolding ELSE gcIndustryCorrugated
        ipbf-ttEstHeader.warehouseID                 = bf-est.loc
        ipbf-ttEstHeader.marginOn                    = bf-ce-ctrl.sell-by
        ipbf-ttEstHeader.marginPct                   = bf-ce-ctrl.prof-mrkup
        ipbf-ttEstHeader.warehouseMarkupPct          = bf-ce-ctrl.whse-mrkup / 100 /*ctrl[1]*/
        ipbf-ttEstHeader.handlingChargePct           = bf-ce-ctrl.hand-pct / 100 /*ctrl[2]*/
        ipbf-ttEstHeader.handlingRatePerCWTRMPct     = bf-ce-ctrl.rm-rate / 100 /*ctrl[3]*/ /*NOTE CHANGED to be /100 */
        
        ipbf-ttEstHeader.special1MarkupPct           = bf-ce-ctrl.spec-%[1] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-ttEstHeader.special1FlatValue           = 0 /*REFACTOR - treatment of Special Costs*/
        
        ipbf-ttEstHeader.special2MarkupPct           = bf-ce-ctrl.spec-%[2] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-ttEstHeader.special2FlatValue           = 0
        
        ipbf-ttEstHeader.special3MarkupPct           = bf-ce-ctrl.spec-%[3] / 100 /*ctrl[4]*/ /*NOTE CHANGED to be /100 */
        ipbf-ttEstHeader.special3FlatValue           = 0
        
        ipbf-ttEstHeader.showCommissions             = bf-ce-ctrl.comm-add /*ctrl[5]*/
        ipbf-ttEstHeader.showLaborRates              = bf-ce-ctrl.sho-labor /*ctrl[7]*/
/*        ipbf-ttEstHeader.addToFactCostFreight        = bf-ce-ctrl.shp-add /*ctrl[6]*/     */
/*        ipbf-ttEstHeader.addToFactCostSpecial1       = bf-ce-ctrl.spec-add[1] /*ctrl[13]*/*/
/*        ipbf-ttEstHeader.addToFactCostSpecial2       = bf-ce-ctrl.spec-add[2] /*ctrl[14]*/*/
/*        ipbf-ttEstHeader.addToFactCostSpecial3       = bf-ce-ctrl.spec-add[3] /*ctrl[15]*/*/
/*        ipbf-ttEstHeader.addToFactCostGSA            = bf-ce-ctrl.spec-add[6] /*ctrl[16]*/*/
/*        ipbf-ttEstHeader.addToFactCostRoyalty        = bf-ce-ctrl.spec-add[8] /*ctrl[18]*/*/
/*        ipbf-ttEstHeader.addToFactCostComm           = bf-ce-ctrl.spec-add[7] /*ctrl[17]*/*/
        ipbf-ttEstHeader.foldPct                     = bf-ce-ctrl.fold-pct / 100 /*ctrl[19]*/ /*NOTE CHANGED to be /100 */            
        ipbf-ttEstHeader.handlingRatePerCWTFGPct     = bf-ce-ctrl.fg-rate / 100  /*ld-fg-rate*/
        ipbf-ttEstHeader.handlingRatePerCWTRMFarmPct = bf-ce-ctrl.rm-rate-farm / 100
        ipbf-ttEstHeader.handlingRatePerCWTFGFarmPct = bf-ce-ctrl.fg-rate-farm / 100
        ipbf-ttEstHeader.handlingChargeFarmPct       = bf-ce-ctrl.hand-pct-farm / 100
        ipbf-ttEstHeader.directMaterialPct           = gdMaterialMarkup / 100                  
        .
        
END PROCEDURE.

PROCEDURE pProcessInk PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttInk/ttEstOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstBlank     FOR ttEstBlank.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-ttInk          FOR ttInk.
    
 
    DEFINE           BUFFER bf-ttEstMaterial    FOR ttEstMaterial.
    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL.
    DEFINE VARIABLE dqtyRequiredMinDiff AS DECIMAL. 
        
    RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipbf-ttInk.cItemID, ipbf-ttInk.iBlankNo, BUFFER bf-ttEstMaterial).
        
    ASSIGN    
        dQtyRequiredPerForm                     = ipbf-ttEstBlank.numOut * ipbf-ttInk.dQtyRequiredPerBlank
        bf-ttEstMaterial.qtyRequiredNoWaste    = ipbf-ttEstOperation.qtyInNoWaste * dQtyRequiredPerForm
        bf-ttEstMaterial.qtyRequiredRunWaste   = ipbf-ttEstOperation.qtyInRunWaste * dQtyRequiredPerForm
        bf-ttEstMaterial.qtyRequiredSetupWaste = ipbf-ttEstOperation.qtyInSetupWaste * dQtyRequiredPerForm + ipbf-ttEstOperation.qtyInkLbsWastedPerSetup
        dQtyRequiredMinDiff                     = ipbf-ttInk.dMinLbsPerJob - (bf-ttEstMaterial.qtyRequiredNoWaste + bf-ttEstMaterial.qtyRequiredRunWaste + bf-ttEstMaterial.qtyRequiredSetupWaste)
        bf-ttEstMaterial.qtyUOM                = ipbf-ttInk.cQtyUOM
        .             
    IF dQtyRequiredMinDiff GT 0 THEN 
        bf-ttEstMaterial.qtyRequiredMinDiff = dQtyRequiredMinDiff.
    
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).

END PROCEDURE.

PROCEDURE pProcessLeaf PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Processes a single ttLeaf/ttEstOperation
    Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-ttLeaf         FOR ttLeaf.
    DEFINE INPUT PARAMETER ipdQtyRequiredPerForm AS DECIMAL NO-UNDO.    
 
    DEFINE BUFFER bf-ttEstMaterial FOR ttEstMaterial.
        
    RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipbf-ttLeaf.cItemID, ipbf-ttLeaf.iBlankNo, BUFFER bf-ttEstMaterial).
        
    ASSIGN    
        bf-ttEstMaterial.dimLength             = ipbf-ttLeaf.dDimLength
        bf-ttEstMaterial.dimWidth              = ipbf-ttLeaf.dDimWidth
        bf-ttEstMaterial.qtyRequiredNoWaste    = ipbf-ttEstOperation.qtyInNoWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.qtyRequiredRunWaste   = ipbf-ttEstOperation.qtyInRunWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.qtyRequiredSetupWaste = ipbf-ttEstOperation.qtyInSetupWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.qtyUOM                = ipbf-ttLeaf.cQtyUOM
        .             

    RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).

END PROCEDURE.

PROCEDURE pProcessInks PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for inks with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    /*Inks*/
    FOR EACH ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttEstOperation.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttEstOperation.isPrinter, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstOperation.estCostHeaderID
        AND ttInk.estFormID EQ ttEstOperation.estCostFormID
        AND ttInk.iPass EQ ttEstOperation.pass
        AND ttInk.iCountInks GT 0,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttInk.estFormID 
        AND ttEstBlank.estCostBlankID EQ ttInk.estBlankID :
        
        RUN pProcessInk(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ttEstBlank, BUFFER ttEstOperation, BUFFER ttInk).    
        
    END.
    /*Coatings*/
    FOR EACH ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttEstOperation.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttEstOperation.isCoater, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstOperation.estCostHeaderID
        AND ttInk.estFormID EQ ttEstOperation.estCostFormID
        AND ttInk.iPass EQ ttEstOperation.pass
        AND ttInk.iCountCoatings GT 0,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttInk.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttInk.estFormID 
        AND ttEstBlank.estCostBlankID EQ ttInk.estBlankID :
            
        RUN pProcessInk(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ttEstBlank, BUFFER ttEstOperation, BUFFER ttInk).    
    END.

END PROCEDURE.
PROCEDURE pProcessLeafs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for leafs with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL NO-UNDO.
    
    RUN pBuildLeafForEf(BUFFER ipbf-ef, BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm).
    
    FOR FIRST ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttEstOperation.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttEstOperation.isLeafer
        , 
        EACH ttLeaf NO-LOCK
        WHERE ttLeaf.estHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttLeaf.estFormID EQ ipbf-ttEstForm.estCostFormID
        BY ttEstOperation.sequenceOfOperation DESCENDING:
        
        IF ttEstOperation.feedType EQ "B" THEN 
        DO:            
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estCostHeaderID EQ ttLeaf.estHeaderID
                AND ttEstBlank.estCostFormID EQ ttLeaf.estFormID 
                AND ttEstBlank.blankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                dQtyRequiredPerForm = ttEstBlank.numOut * ttLeaf.dQtyRequiredPerLeaf.
        END.
        ELSE 
            dQtyRequiredPerForm = ttLeaf.dQtyRequiredPerLeaf.
            
        RUN pProcessLeaf(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ttEstOperation, BUFFER ttLeaf, dQtyRequiredPerForm).    
        
    END.

   
END PROCEDURE.

PROCEDURE pProcessPacking PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for packing material with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    DEFINE           BUFFER bf-ttEstMaterial FOR ttEstMaterial.
    DEFINE VARIABLE iCaseCount   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCases       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPalletCount AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iPallets     AS DECIMAL NO-UNDO.
    
    ASSIGN 
        iCaseCount = 0
        iCases     = 0
        iPallets   = 0
        .
    
    /*Case*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttPack.lIsCase,
        FIRST ttEstBlank EXCLUSIVE-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttPack.estFormID
        AND ttEstBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        IF ttPack.iCountPerSubUnit NE 0 THEN
            ASSIGN
                iCaseCount = ttPack.iCountPerSubUnit  
                iCases     = fRoundUp(ttEstBlank.qtyRequired / ttPack.iCountPerSubUnit) * ttPack.dQtyMultiplier.
        ELSE 
            ASSIGN 
                iCases     = fRoundUp(ttEstBlank.qtyRequired * ttEstBlank.weight / ttPack.dWeightCapacity) * ttPack.dQtyMultiplier
                iCaseCount = TRUNCATE(ttEstBlank.qtyRequired /  iCases, 0)
                .
        ASSIGN  
            bf-ttEstMaterial.qtyRequiredNoWaste = iCases
            ttEstBlank.quantityOfSubUnits                 = iCases
            bf-ttEstMaterial.qtyUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-ttEstMaterial.itemName = bf-ttEstMaterial.itemName + " (" + TRIM(STRING(iCaseCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
    END.
    
    /*Pallet*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estCostFormID
        AND ttPack.lIsPallet,
        FIRST ttEstBlank EXCLUSIVE-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttPack.estFormID
        AND ttEstBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        ASSIGN  
            iPalletCount                         = IF ttPack.iCountPerUnit EQ 0 THEN ttPack.iCountSubUnitsPerUnit * iCaseCount ELSE ttPack.iCountPerUnit
            iPallets                             = fRoundUp(ttEstBlank.qtyRequired / iPalletCount) * ttPack.dQtyMultiplier 
            bf-ttEstMaterial.qtyRequiredNoWaste = iPallets
            bf-ttEstMaterial.qtyUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-ttEstMaterial.itemName = bf-ttEstMaterial.itemName + " (" + TRIM(STRING(iPalletCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
    END.
    
    /*Other Pack*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estCostFormID
        AND NOT ttPack.lIsPallet AND NOT ttPack.lIsCase,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estCostFormID EQ ttPack.estFormID
        AND ttEstBlank.estCostBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        
        ASSIGN 
            bf-ttEstMaterial.qtyRequiredNoWaste = IF ttPack.cQtyMultiplierPer EQ "P" THEN iPallets * ttPack.dQtyMultiplier ELSE iCases * ttPack.dQtyMultiplier
            bf-ttEstMaterial.qtyUOM             = ttPack.cQtyUOM
            .                    
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
    END.
END PROCEDURE.

PROCEDURE pGetEstMaterialCosts PRIVATE:
    /*------------------------------------------------------------------------------
    Purpose: Given an ttEstMaterial, fill in cost per UOM and Setup for 
    given vendor.
     Notes:  replaces est/matcost.i (should move to costProcs)
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader   FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstMaterial FOR ttEstMaterial.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQtyUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
       
    DEFINE VARIABLE iIndex        AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCostFound    AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE dRunQty       AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dRunCost      AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dSetups       AS DECIMAL EXTENT 20.
    DEFINE VARIABLE dQtyInCostUOM AS DECIMAL.
    

    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.

    FIND FIRST e-item NO-LOCK 
        WHERE e-item.company EQ ipbf-ttEstMaterial.company 
        AND e-item.i-no EQ ipbf-ttEstMaterial.itemID
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
                    ipbf-ttEstMaterial.basisWeight, ipbf-ttEstMaterial.dimLength, ipbf-ttEstMaterial.dimWidth, ipbf-ttEstMaterial.dimDepth, 
                    ipdQty, OUTPUT dQtyInCostUOM).
            ELSE 
                dQtyInCostUOM = ipdQty. 
            DO iIndex = 1 TO 20:
                IF dRunQty[iIndex] NE 0   AND
                    dRunQty[iIndex] GE dQtyInCostUOM THEN 
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

    IF ipbf-ttEstMaterial.isRealMaterial AND NOT lCostFound THEN
        ASSIGN 
            opdCost    = IF ipbf-ttEstHeader.forRealItemsUseAvgCost THEN ipbf-ttEstMaterial.costPerUOMAvg ELSE ipbf-ttEstMaterial.costPerUOMLast
            opcCostUOM = ipbf-ttEstMaterial.qtyUOM  /*REFACTOR? - What uom is avg and last cost in*/
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
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOut AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutSetupWaste AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQtyInOutRunWaste AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-mach FOR mach.

    DEFINE VARIABLE iInkCoatCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL NO-UNDO. 
    
    
    ASSIGN 
        ipbf-ttEstOperation.qtyOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-ttEstOperation.qtyInNoWaste = ipbf-ttEstOperation.qtyOut / ipbf-ttEstOperation.numOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste            = iopdQtyInOutSetupWaste / ipbf-ttEstOperation.numOutForOperation
        iopdQtyInOutRunWaste              = iopdQtyInOutRunWaste / ipbf-ttEstOperation.numOutForOperation
        ipbf-ttEstOperation.qtyIn        = fRoundUp(ipbf-ttEstOperation.qtyIn)
        iopdQtyInOutSetupWaste            = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste              = fRoundUp(iopdQtyInOutRunWaste)
        .
    
    /*Recalc from standards off right now*/
    IF NOT ipbf-ttEstOperation.isLocked THEN 
    DO:
        FIND FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ ipbf-ttEstHeader.company
            AND bf-mach.m-code EQ ipbf-ttEstOperation.operationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ipbf-ttEstOperation, BUFFER bf-mach).
       
    END.
    
    ASSIGN 
        ipbf-ttEstOperation.qtyInRunWaste        = (ipbf-ttEstOperation.qtyInNoWaste / 
                                                    (1 - (ipbf-ttEstOperation.qtyInRunWastePercent / 100))) 
                                                    - ipbf-ttEstOperation.qtyInNoWaste
        ipbf-ttEstOperation.qtyInRunWaste        = fRoundUp(ipbf-ttEstOperation.qtyInRunWaste)
        ipbf-ttEstOperation.qtyInAfterSetupWaste = ipbf-ttEstOperation.qtyInNoWaste + ipbf-ttEstOperation.qtyInRunWaste
        ipbf-ttEstOperation.qtyIn                = ipbf-ttEstOperation.qtyInAfterSetupWaste + ipbf-ttEstOperation.qtyInSetupWaste
        iopdQtyInOutRunWaste                      = iopdQtyInOutRunWaste + ipbf-ttEstOperation.qtyInRunWaste
        iopdQtyInOutSetupWaste                    = iopdQtyInOutSetupWaste + ipbf-ttEstOperation.qtyInSetupWaste
        ipbf-ttEstOperation.qtyIn                = fRoundUp(ipbf-ttEstOperation.qtyIn)
        iopdQtyInOut                              = ipbf-ttEstOperation.qtyIn
        .


END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunSpeed PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated Run Speeds for ttEstOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-mach           FOR mach.

   

END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsRunWastePercent PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated RunWastePercent for ttEstOperation
         based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-mach           FOR mach.




END PROCEDURE.

PROCEDURE pRecalcEstOperationFromStandardsSetupWaste PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Fetches updated MR Waste for ttEstOperation
     based on machine buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader    FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm      FOR ttEstForm.
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.
    DEFINE PARAMETER BUFFER ipbf-mach           FOR mach.

    ipbf-ttEstOperation.qtyInSetupWaste = ipbf-mach.mr-waste.
    IF ipbf-ttEstOperation.isPrinter OR ipbf-ttEstOperation.isCoater THEN 
        ipbf-ttEstOperation.qtyInSetupWaste = ipbf-ttEstOperation.qtyInSetupWaste + 
            (ipbf-ttEstOperation.qtyInSetupWastePerColor * 
            IF glUsePlateChangesAsColorForSetupWaste AND ipbf-ttEstOperation.countPlateChanges NE 0 THEN ipbf-ttEstOperation.countPlateChanges
            ELSE (ipbf-ttEstOperation.countCoats + ipbf-ttEstOperation.countInks)).

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

FUNCTION fGetEstBlankID RETURNS INT64 PRIVATE
    (ipiEstHeaderID AS INT64 , ipiEstFormID AS INT64 , ipiBlankNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the Blank ID given header, form id and blank #
     Notes:
    ------------------------------------------------------------------------------*/    
    FIND FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ipiEstHeaderID
        AND ttEstBlank.estCostFormID EQ ipiEstFormID
        AND ttEstBlank.blankNo EQ ipiBlankNo
        NO-ERROR.
    IF AVAILABLE ttEstBlank THEN 
        RETURN ttEstBlank.estCostBlankID.

END FUNCTION.

FUNCTION fGetNextID RETURNS INT64 PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    giRecKey = giRecKey + 1.
    RETURN giRecKey.


        
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


