
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

DEFINE VARIABLE gcOutputFile                          AS CHARACTER INITIAL "C:\temp\estPrintOut.txt".
DEFINE VARIABLE gcTestDataDir                         AS CHARACTER INITIAL "C:\Users\brad.vigrass\Documents\Testing\EstimateData\".
DEFINE VARIABLE giRecKey                              AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcSourceTypeOperation                 AS CHARACTER NO-UNDO INITIAL "Operation".
DEFINE VARIABLE gcSourceTypeMaterial                  AS CHARACTER NO-UNDO INITIAL "Material".
DEFINE VARIABLE gcSourceTypeMisc                      AS CHARACTER NO-UNDO INITIAL "Miscellaneous".

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

DEFINE VARIABLE glUsePlateChangesAsColorForSetupWaste AS LOGICAL   NO-UNDO INITIAL NO.  /*Defect in EstOperation Calc of applying the MR Waste Sheets Per Color?*/


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetEstBlankID RETURNS CHARACTER PRIVATE
    (ipcEstHeaderID AS CHARACTER,
    ipcEstFormID AS CHARACTER,
    ipiBlankNo AS INTEGER) FORWARD.

FUNCTION fGetNextID RETURNS CHARACTER PRIVATE
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

RUN pBuildTestData("001").
RUN pBuildDetail.
RUN pBuildSummary.
FOR EACH ttEstError NO-LOCK:
    DISPLAY ttEstError.cErrorType ttEstError.cError FORMAT "x(60)".
END.
FIND FIRST ttEstHeader NO-LOCK 
    WHERE ttEstHeader.estHeaderID = "header2".

RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Set Summary First","Calibri").

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddCostDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an EstOperation buffer, create a unique cost detail record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstFormID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstBlankID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstSourceID AS CHARACTER NO-UNDO.
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
        opbf-ttEstCostDetail.estHeaderID        = ipcEstHeaderID
        opbf-ttEstCostDetail.estFormID          = ipcEstFormID
        opbf-ttEstCostDetail.estBlankID         = ipcEstBlankID
        opbf-ttEstCostDetail.estSourceID        = ipcEstSourceID 
        opbf-ttEstCostDetail.cSourceType        = ipcSourceType
        opbf-ttEstCostDetail.estCostCategoryID  = ipcEstCostCategoryID
        opbf-ttEstCostDetail.cDetailDescription = ipcDescription
        opbf-ttEstCostDetail.dCost              = ipdCost
        opbf-ttEstCostDetail.dProfitPercent     = ipdProfitPercent
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
    
    RUN pAddCostDetail(ipbf-ttEstOperation.estHeaderID, ipbf-ttEstOperation.estFormID, ipbf-ttEstOperation.estBlankID, ipbf-ttEstOperation.estOperationID, 
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
    
    RUN pAddCostDetail(ipbf-ttEstMaterial.estHeaderID, ipbf-ttEstMaterial.estFormID, ipbf-ttEstMaterial.estBlankID, ipbf-ttEstMaterial.estMaterialID, 
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
    
    RUN pAddCostDetail(ipbf-ttEstMisc.estHeaderID, ipbf-ttEstMisc.estFormID, ipbf-ttEstMisc.estBlankID, ipbf-ttEstMisc.estMiscID, 
        gcSourceTypeMisc, ipcEstCostCategoryID, ipcDescription, ipdCost, ipdProfitPercent, BUFFER bf-ttEstCostDetail). 
    

END PROCEDURE.

PROCEDURE pAddError PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Registers an error in the ttEstError table for display
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcErrorType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstHeaderID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.

    CREATE ttEstError.
    ASSIGN 
        ttEstError.cError      = ipcError
        ttEstError.cErrorType  = ipcErrorType
        ttEstError.estHeaderID = ipcEstHeaderID
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
        opbf-ttEstBlank.estBlankID          = fGetNextID()
        opbf-ttEstBlank.company             = ipbf-ttEstForm.company
        opbf-ttEstBlank.estFormID           = ipbf-ttEstForm.estFormID
        opbf-ttEstBlank.estHeaderID         = ipbf-ttEstForm.estHeaderID
        opbf-ttEstBLank.riEbRowID           = ROWID(ipbf-eb)
        opbf-ttEstBlank.iFormNo             = ipbf-eb.form-no
        opbf-ttEstBlank.iBlankNo            = ipbf-eb.blank-no
        opbf-ttEstBlank.iNumOutLength       = ipbf-eb.num-len
        opbf-ttEstBlank.iNumOutWidth        = ipbf-eb.num-wid
        opbf-ttEstBlank.iNumOutDepth        = ipbf-eb.num-dep
        opbf-ttEstBlank.iNumOut             = MAX(opbf-ttEstBlank.iNumOutWidth, 1) * MAX(opbf-ttEstBlank.iNumOutLength, 1) * MAX(opbf-ttEstBlank.iNumOutDepth, 1)
        opbf-ttEstBlank.dBlankWidth         = ipbf-eb.t-wid
        opbf-ttEstBlank.dBlankLength        = ipbf-eb.t-len
        opbf-ttEstBlank.dBlankDepth         = ipbf-eb.t-dep
        opbf-ttEstBlank.dBlankArea          = ipbf-eb.t-sqin
        opbf-ttEstBlank.dLength             = ipbf-eb.len
        opbf-ttEstBlank.dWidth              = ipbf-eb.wid
        opbf-ttEstBlank.dDepth              = ipbf-eb.dep
                                                
        /*Refactor - Hardcoded*/
        opbf-ttEstBlank.cUOMArea            = "SQIN"
        opbf-ttEstBlank.cUOMDimension       = "IN"
        opbf-ttEstBlank.cUOMWeight          = "LB/M"
                    
        /*Refactor - apply area UOM conversion*/
        opbf-ttEstBlank.dWeight             = ipbf-ttEstForm.dBasisWeightInLbsPerMSF * opbf-ttEstBlank.dBlankArea / 144000 
                            
        /*Refactor - Calculate Windowing*/
        opbf-ttEstBlank.dBlankAreaNetWindow = opbf-ttEstBlank.dBlankArea
        .
        
    FIND FIRST ttEstItem EXCLUSIVE-LOCK 
        WHERE ttEstItem.estHeaderID EQ opbf-ttEstBlank.estHeaderID
        AND ttEstItem.cCustomerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE ttEstItem THEN 
    DO:
        ASSIGN 
            opbf-ttEstBlank.estItemID    = ttEstItem.estItemID
            ttEstItem.cSize              = TRIM(STRING(opbf-ttEstBlank.dLength,">>>9.99")) + " x " + TRIM(STRING(opbf-ttEstBlank.dWidth,">>>9.99"))
            opbf-ttEstBlank.dQtyPerSet   = ttEstItem.dQtyPerParent
            opbf-ttEstBlank.dQtyRequired = ttEstItem.dQtyRequired
            opbf-ttEstBlank.dQtyYielded  = ttEstItem.dQtyYielded
            .
        IF opbf-ttEstBlank.dDepth NE 0 THEN 
            ttEstItem.cSize = ttEstItem.cSize + " x " + TRIM(STRING(opbf-ttEstBlank.dDepth,">>>9.99")).
    END.
    ipbf-ttEstForm.iNumOutBlanksOnNet = ipbf-ttEstForm.iNumOutBlanksOnNet + opbf-ttEstBlank.iNumOut. 

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: create the EstForm for an est header and form no
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstForm FOR ttEstForm.
    
    CREATE opbf-ttEstForm.
    ASSIGN 
        opbf-ttEstForm.rec_key     = fGetNextRecKey()
        opbf-ttEstForm.estFormID   = fGetNextID()
        opbf-ttEstForm.estHeaderID = ipcEstHeaderID
        opbf-ttEstForm.iFormNo     = ipiFormNo
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

    RUN pAddEstForm(ipbf-ttEstHeader.estHeaderID, ipbf-ef.form-no, BUFFER opbf-ttEstForm).
    
    ASSIGN 
        opbf-ttEstForm.iNumOutNetLength                = MAX(ipbf-ef.n-out-l, 1)
        opbf-ttEstForm.iNumOutNetWidth                 = MAX(ipbf-ef.n-out, 1)
        opbf-ttEstForm.iNumOutNetDepth                 = MAX(ipbf-ef.n-out-d, 1)
        opbf-ttEstForm.iNumOutNet                      = opbf-ttEstForm.iNumOutNetLength * opbf-ttEstForm.iNumOutNetWidth * opbf-ttEstForm.iNumOutNetDepth
        opbf-ttEstForm.dGrossWidth                     = ipbf-ef.gsh-wid 
        opbf-ttEstForm.dGrossLength                    = ipbf-ef.gsh-len
        opbf-ttEstForm.dGrossDepth                     = ipbf-ef.gsh-dep 
        opbf-ttEstForm.dNetWidth                       = ipbf-ef.nsh-wid
        opbf-ttEstForm.dNetLength                      = ipbf-ef.nsh-len
        opbf-ttEstForm.dNetDepth                       = ipbf-ef.nsh-dep
        opbf-ttEstForm.dDieWidth                       = ipbf-ef.trim-w
        opbf-ttEstForm.dDieLength                      = ipbf-ef.trim-l
        opbf-ttEstForm.dBasisWeightInLbsPerMSF         = ipbf-ef.weight
        opbf-ttEstForm.riEfRowID                       = ROWID(ipbf-ef)
        opbf-ttEstForm.company                         = ipbf-ef.company     
        opbf-ttEstForm.dCostPerUOMOverride             = ipbf-ef.cost-msh
        opbf-ttEstForm.cCostUOMOverride                = ipbf-ef.cost-uom  
        opbf-ttEstForm.lNoCharge                       = NOT ipbf-ef.nc
        /*Refactor - handle when ef.roll is yes see ce/print4p.i*/
                
        /*Refactor- Hard-codes*/
        opbf-ttEstForm.cUOMDimension                   = "IN"
        opbf-ttEstForm.cUOMArea                        = "SF"
        opbf-ttEstForm.cUOMWeightDie                   = "LB/MSHT"
        opbf-ttEstForm.cUOMWeightNet                   = "LB/MSHT"
        opbf-ttEstForm.cUOMWeightGross                 = "LB/MSHT"
        opbf-ttEstForm.cUOMGrossQtyRequiredTotalWeight = "LBS"
        opbf-ttEstForm.cUOMGrossQtyRequiredTotalArea   = "MSF"
            
               
        /*Refactor - Formulas/Conversions - don't assume SF and inches*/
        opbf-ttEstForm.dGrossArea                      = opbf-ttEstForm.dGrossWidth * opbf-ttEstForm.dGrossLength / 144
        opbf-ttEstForm.dNetArea                        = opbf-ttEstForm.dNetWidth * opbf-ttEstForm.dNetLength / 144
        opbf-ttEstForm.dDieArea                        = opbf-ttEstForm.dDieWidth * opbf-ttEstForm.dDieLength / 144
        
        opbf-ttEstForm.dWeightDie                      = opbf-ttEstForm.dBasisWeightInLbsPerMSF * opbf-ttEstForm.dDieArea 
        opbf-ttEstForm.dWeightNet                      = opbf-ttEstForm.dBasisWeightInLbsPerMSF * opbf-ttEstForm.dNetArea 
        opbf-ttEstForm.dWeightGross                    = opbf-ttEstForm.dBasisWeightInLbsPerMSF * opbf-ttEstForm.dGrossArea
            
        /*Refactor - Calculate Combo products*/
        opbf-ttEstForm.dQtyFGOnForm                    = ipbf-ttEstHeader.dQtyMaster
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
        opbf-ttEstItem.estItemID         = fGetNextID()
        opbf-ttEstItem.estHeaderID       = ipbf-ttEstHeader.estHeaderID
        opbf-ttEstItem.cCustomerPart     = ipbf-eb.part-no
        opbf-ttEstItem.cColor            = ipbf-eb.i-coldscr
        opbf-ttEstItem.cCustomerID       = ipbf-eb.cust-no
        opbf-ttEstItem.cShipToID         = ipbf-eb.ship-id
        opbf-ttEstItem.cItemName         = ipbf-eb.part-dscr1
        opbf-ttEstItem.cItemDescription1 = ipbf-eb.part-dscr2
        opbf-ttEstItem.cSalesgroupID     = ipbf-eb.sman
        opbf-ttEstItem.cStyleID          = ipbf-eb.style
        opbf-ttEstItem.lIsSet            = ipbf-eb.is-a-set
        opbf-ttEstItem.cFGItemID         = ipbf-eb.stock-no
        opbf-ttEstItem.company           = ipbf-eb.company
        .
    
    IF ipbf-eb.est-type LT 5 THEN
        opbf-ttEstItem.dQtyPerParent     = MAX(ipbf-eb.cust-%, 1). 
    ELSE         
        opbf-ttEstItem.dQtyPerParent     = ipbf-eb.quantityPerSet.
    IF opbf-ttEstItem.dQtyPerParent LT 0 THEN 
        opbf-ttEstItem.dQtyPerParent     = 1 / opbf-ttEstItem.dQtyPerParent.
    IF opbf-ttEstItem.dQtyPerParent EQ 0 THEN 
        opbf-ttEstItem.dQtyPerParent     = 1 .
     
    opbf-ttEstItem.dQtyRequired      = ipbf-ttEstHeader.dQtyMaster * opbf-ttEstItem.dQtyPerParent.
            
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-eb.company
        AND cust.cust-no EQ ipbf-eb.cust-no
        NO-ERROR.
  
    /*Refactor - hardcoded temp?  Consider just using eb fields*/
    IF AVAILABLE cust AND cust.cust-no NE "Temp" THEN 
        ASSIGN 
            opbf-ttEstItem.cCustomerName     = cust.name
            opbf-ttEstItem.cCustomerAddress1 = cust.addr[1]
            opbf-ttEstItem.cCustomerAddress2 = cust.addr[2]
            opbf-ttEstItem.cCustomerAddress3 = cust.city + ", " + cust.state + " " + cust.zip
            .
    ELSE 
        ASSIGN  
            opbf-ttEstItem.cCustomerName     = ipbf-eb.ship-name
            opbf-ttEstItem.cCustomerAddress1 = ipbf-eb.ship-addr[1]
            opbf-ttEstItem.cCustomerAddress2 = ipbf-eb.ship-addr[2]
            opbf-ttEstItem.cCustomerAddress3 = ipbf-eb.ship-city + ", " + ipbf-eb.ship-state + " " + ipbf-eb.ship-zip
            .
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ ipbf-eb.company
        AND shipto.cust-no EQ ipbf-eb.cust-no
        AND shipto.ship-id EQ ipbf-eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN 
            opbf-ttEstItem.cShipToName     = shipto.ship-name
            opbf-ttEstItem.cShipToAddress1 = shipto.ship-addr[1]
            opbf-ttEstItem.cShipToAddress2 = shipto.ship-addr[2]
            opbf-ttEstItem.cShipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
            .
    FIND FIRST sman NO-LOCK 
        WHERE sman.company EQ ipbf-eb.company
        AND sman.sman EQ ipbf-eb.sman
        NO-ERROR.
    IF AVAILABLE sman THEN 
        ASSIGN 
            opbf-ttEstItem.cSalesgroupName = sman.sname        
            .
    FIND FIRST style NO-LOCK 
        WHERE style.company EQ ipbf-eb.company
        AND style.style EQ ipbf-eb.style
        NO-ERROR.
    IF AVAILABLE style THEN 
        ASSIGN 
            opbf-ttEstItem.cStyle = style.dscr
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
            opbf-ttEstMaterial.estMaterialID   = fGetNextID()
            opbf-ttEstMaterial.estFormID       = ipbf-ttEstForm.estFormID
            opbf-ttEstMaterial.estHeaderID     = ipbf-ttEstForm.estHeaderID
            opbf-ttEstMaterial.riItemRowID     = ROWID(bf-item)
            opbf-ttEstMaterial.company         = bf-item.company
            opbf-ttEstMaterial.iFormNo         = ipbf-ttEstForm.iFormNo
            opbf-ttEstMaterial.iBlankNo        = ipiBlankNo
            opbf-ttEstMaterial.cItemID         = bf-item.i-no 
            opbf-ttEstMaterial.cItemName       = IF bf-item.est-dscr NE "" THEN bf-item.est-dscr ELSE bf-item.i-name 
            opbf-ttEstMaterial.cQtyUOM         = CAPS(bf-item.cons-uom)
            opbf-ttEstMaterial.cQtyUOMWaste    = opbf-ttEstMaterial.cQtyUOM
            opbf-ttEstMaterial.dBasisWeight    = bf-item.basis-w
            opbf-ttEstMaterial.cBasisWeightUOM = "LB/MSF"
            opbf-ttEstMaterial.dDimLength      = bf-item.s-len
            opbf-ttEstMaterial.dDimWidth       = bf-item.s-wid
            opbf-ttEstMaterial.dDimDepth       = bf-item.s-dep
            opbf-ttEstMaterial.cDimUOM         = "IN"
            opbf-ttEstMaterial.dCostPerUOMAvg  = bf-item.avg-cost
            opbf-ttEstMaterial.dCostPerUOMLast = bf-item.last-cost
            opbf-ttEstMaterial.lIsRealMaterial = bf-item.i-code EQ "R"
            opbf-ttEstMaterial.cMaterialType   = bf-item.mat-type
            .
        
        IF CAN-DO(gcBoardMatTypes, opbf-ttEstMaterial.cMaterialType) THEN 
            opbf-ttEstMaterial.iSequence = 1.
        ELSE IF CAN-DO(gcInkMatTypes,opbf-ttEstMaterial.cMaterialType) THEN 
            opbf-ttEstMaterial.iSequence = 2.
        ELSE IF CAN-DO(gcGlueMatTypes,opbf-ttEstMaterial.cMaterialType) THEN 
            opbf-ttEstMaterial.iSequence = 3.
        ELSE IF CAN-DO(gcLeafMatTypes,opbf-ttEstMaterial.cMaterialType) THEN 
            opbf-ttEstMaterial.iSequence = 4.
        ELSE IF CAN-DO(gcPackMatTypes,opbf-ttEstMaterial.cMaterialType) THEN 
            opbf-ttEstMaterial.iSequence = 5.
        ELSE  
            opbf-ttEstMaterial.iSequence = 6.
            
        IF opbf-ttEstMaterial.iBlankNo NE 0 THEN 
        DO:
            opbf-ttEstMaterial.estBlankID = fGetEstBlankID(opbf-ttEstMaterial.estHeaderID,opbf-ttEstMaterial.estFormID, opbf-ttEstMaterial.iBlankNo).
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estBlankID EQ opbf-ttEstMaterial.estBlankID
                NO-ERROR.
            IF AVAILABLE ttEstBlank AND NOT opbf-ttEstMaterial.lIsRealMaterial THEN 
                ASSIGN 
                    opbf-ttEstMaterial.dDimLength = ttEstBlank.dBlankLength
                    opbf-ttEstMaterial.dDimWidth  = ttEstBlank.dBlankWidth
                    opbf-ttEstMaterial.dDimLength = ttEstBlank.dBlankLength
                    .
        END.
        ELSE IF NOT opbf-ttEstMaterial.lIsRealMaterial THEN 
                ASSIGN 
                    opbf-ttEstMaterial.dDimLength = ipbf-ttEstForm.dGrossLength
                    opbf-ttEstMaterial.dDimWidth  = ipbf-ttEstForm.dGrossWidth
                    opbf-ttEstMaterial.dDimDepth  = ipbf-ttEstForm.dGrossDepth
                    .
        
    END.
    
END PROCEDURE.

PROCEDURE pAddEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Creates an ttEstMisc and returns the buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstFormID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttEstMisc FOR ttEstMisc.

    CREATE opbf-ttEstMisc.
    ASSIGN 
        opbf-ttEstMisc.rec_key     = fGetNextRecKey()
        opbf-ttEstMisc.estMiscID   = fGetNextID()
        opbf-ttEstMisc.estHeaderID = ipcEstHeaderID
        opbf-ttEstMisc.estFormID   = ipcEstFormID
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
        RUN pAddEstMisc(ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.estFormID, BUFFER bf-ttEstMisc).
       
        ASSIGN 
            bf-ttEstMisc.estBlankID             = "" /*REFACTOR - Get blank ID from form #?*/
            bf-ttEstMisc.iFormNo                = ipbf-ef.mis-snum[ipiIndex]  
            bf-ttEstMisc.iBlankNo               = ipbf-ef.mis-bnum[ipiIndex]
            bf-ttEstMisc.cCostDescription       = ipbf-ef.mis-cost[ipiIndex]
            bf-ttEstMisc.cCostType              = cCostType
            bf-ttEstMisc.cCostUOM               = "M"
            bf-ttEstMisc.dCostPerUOM            = dCostPerM
            bf-ttEstMisc.dCostSetup             = dCostSetup
            bf-ttEstMisc.cPercentType           = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
            bf-ttEstMisc.cSIMON                 = ipbf-ef.mis-simon[ipiIndex]
            bf-ttEstMisc.dProfitPercent         = ipbf-ef.mis-mkup[ipiIndex]
            bf-ttEstMisc.dSourceQty             = ipdQuantity
            bf-ttEstMisc.dQtyPerSourceQty       = 1
            bf-ttEstMisc.dQtyRequiredTotal      = ipdQuantity
            bf-ttEstMisc.cQtyUOM                = "EA"
            bf-ttEstMisc.dCostTotalBeforeProfit = dCostPerM * ipdQuantity / 1000 + dCostSetup
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
    
    RUN pAddEstMisc(ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.estFormID, BUFFER bf-ttEstMisc).
    
    ASSIGN 
        bf-ttEstMisc.estBlankID             = "" /*REFACTOR - Get blank ID from form #?*/
        bf-ttEstMisc.iFormNo                = ipbf-est-prep.s-num  
        bf-ttEstMisc.iBlankNo               = ipbf-est-prep.b-num
        bf-ttEstMisc.cPrepID                = ipbf-est-prep.code
        bf-ttEstMisc.cCostDescription       = ipbf-est-prep.dscr
        bf-ttEstMisc.cCostType              = IF ipbf-est-prep.ml THEN "Mat" ELSE "Lab"
        bf-ttEstMisc.cPercentType           = (IF gcPrepMarkupOrMargin EQ "Profit" THEN "Margin" ELSE "Markup")
        bf-ttEstMisc.cSIMON                 = ipbf-est-prep.simon
        bf-ttEstMisc.dProfitPercent         = ipbf-est-prep.mkup
        bf-ttEstMisc.dSourceQty             = ipbf-est-prep.qty
        bf-ttEstMisc.dQtyPerSourceQty       = 1
        bf-ttEstMisc.dQtyRequiredTotal      = bf-ttEstMisc.dSourceQty * bf-ttEstMisc.dQtyPerSourceQty
        bf-ttEstMisc.cQtyUOM                = "EA"
        bf-ttEstMisc.cCostUOM               = "EA"
        bf-ttEstMisc.dCostPerUOM            = ipbf-est-prep.cost
        bf-ttEstMisc.dCostSetup             = 0
        bf-ttEstMisc.dCostTotalBeforeProfit = bf-ttEstMisc.dCostPerUOM * bf-ttEstMisc.dQtyRequiredTotal + dCostSetup
        bf-ttEstMisc.lIsPrep                = YES
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

    FIND FIRST bf-mach NO-LOCK 
        WHERE bf-mach.company EQ ipbf-est-op.company
        AND bf-mach.m-code EQ ipbf-est-op.m-code
        NO-ERROR.
    IF AVAILABLE bf-mach THEN 
    DO:
        CREATE opbf-ttEstOperation.
        ASSIGN 
            opbf-ttEstOperation.rec_key                         = fGetNextRecKey()
            opbf-ttEstOperation.estOperationID                  = fGetNextID()
            opbf-ttEstOperation.estFormID                       = ipbf-ttEstForm.estFormID
            opbf-ttEstoperation.estHeaderID                     = ipbf-ttEstForm.estHeaderID
            opbf-ttEstOperation.company                         = ipbf-est-op.company
            opbf-ttEstOperation.iFormNo                         = ipbf-est-op.s-num
            opbf-ttEstOperation.iBlankNo                        = ipbf-est-op.b-num
            opbf-ttEstOperation.estBlankID                      = fGetEstBlankID(opbf-ttEstOperation.estBlankID, opbf-ttEstOperation.estFormID, opbf-ttEstOperation.iBlankNo)
            opbf-ttEstOperation.cOperationID                    = ipbf-est-op.m-code
            opbf-ttEstOperation.iPass                           = MAX(ipbf-est-op.op-pass, 1)
            opbf-ttEstOperation.iSequence                       = ipbf-est-op.line
            opbf-ttEstOperation.iNumOutDivisor                  = ipbf-est-op.n_out_div
                       
            opbf-ttEstOperation.dQtyInSetupWaste                = ipbf-est-op.op-waste
            opbf-ttEstOperation.dHoursSetup                     = ipbf-est-op.op-mr
            opbf-ttEstOperation.dSpeed                          = ipbf-est-op.op-speed
            opbf-ttEstOperation.dQtyInRunWastePercent           = ipbf-est-op.op-spoil
            opbf-ttEstOperation.lIsLocked                       = ipbf-est-op.isLocked
            opbf-ttEstOperation.dCrewSizeSetup                  = ipbf-est-op.op-crew[1]
            opbf-ttEstOperation.dCrewSizeRun                    = ipbf-est-op.op-crew[2]
            opbf-ttEstOperation.iCountInks                      = ipbf-est-op.num-col
            opbf-ttEstOperation.iCountCoats                     = ipbf-est-op.num-coat
            opbf-ttEstOperation.iCountFountainChanges           = ipbf-est-op.fountains
            opbf-ttEstOperation.iCountPlateChanges              = ipbf-est-op.plates
            
            opbf-ttEstOperation.lSpeedInLF                      = bf-mach.therm
            opbf-ttEstOperation.cOperationName                  = bf-mach.m-dscr
            opbf-ttEstOperation.cOperationFeedType              = bf-mach.p-type
            opbf-ttEstOperation.cDepartmentIDPrimary            = bf-mach.dept[1]
            opbf-ttEstOperation.cDepartmentIDs                  = bf-mach.dept
            opbf-ttEstOperation.dQtyInSetupWastePerColor        = bf-mach.col-wastesh
          
            
            opbf-ttEstOperation.dCostPerManHourDLRun            = bf-mach.lab-rate[1]
            opbf-ttEstOperation.dCostPerManHourDLSetup          = bf-mach.lab-rate[1]
            opbf-ttEstOperation.dCostPerHourFOSetup             = bf-mach.mr-fixoh
            opbf-ttEstOperation.dCostPerHourFORun               = bf-mach.run-fixoh
            opbf-ttEstOperation.dCostPerHourVOSetup             = bf-mach.mr-varoh
            opbf-ttEstOperation.dCostPerHourVORun               = bf-mach.run-varoh
            opbf-ttEstOperation.dQtyInkLbsWastedPerSetup        = bf-mach.ink-waste
            opbf-ttEstOperation.dQtyInkLbsWastedPerColorInSetup = bf-mach.col-wastelb
            .
       
        IF fIsDepartment("PR",opbf-ttEstOperation.cDepartmentIDs) THEN  
            opbf-ttEstOperation.lIsPrinter = YES.
        IF fIsDepartment("CT",opbf-ttEstOperation.cDepartmentIDs) THEN  
            opbf-ttEstOperation.lIsCoater = YES.
        IF fIsDepartment("RC,RS",opbf-ttEstOperation.cDepartmentIDs)  THEN 
            opbf-ttEstOperation.lIsNetSheetMaker = YES.
        IF fIsDepartment("GL",opbf-ttEstOperation.cDepartmentIDs)  THEN 
            opbf-ttEstOperation.lIsGluer = YES.
        IF fIsDepartment("WN,WS,FB,FS",opbf-ttEstOperation.cDepartmentIDs)  THEN 
            opbf-ttEstOperation.lIsLeafer = YES.
        IF fIsDepartment("DC",opbf-ttEstOperation.cDepartmentIDs) /*refactor - next machine is blank fed*/ THEN 
            opbf-ttEstOperation.lIsBlankMaker = YES.
        
        IF opbf-ttEstOperation.lIsNetSheetMaker THEN 
            ASSIGN 
                opbf-ttEstOperation.iNumOutForOperation = ipbf-ttEstForm.iNumOutNet
                .
        ELSE IF opbf-ttEstOperation.lIsBlankMaker THEN 
                ASSIGN 
                    opbf-ttEstOperation.iNumOutForOperation = ipbf-ttEstForm.iNumOutBlanksOnNet
                    .
            ELSE 
                ASSIGN 
                    opbf-ttEstOperation.iNumOutForOperation = 1
                    .
        IF opbf-ttEstOperation.iBlankNo NE 0 THEN DO:
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estHeaderID EQ opbf-ttEstOperation.estHeaderID
                AND ttEstBlank.estFormID EQ opbf-ttEstOperation.estFormID
                AND ttEstBlank.iBlankNo EQ opbf-ttEstOperation.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                opbf-ttEstOperation.estBlankID = ttEstBlank.estBlankID. 
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
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is not valid", gcErrorImportant, ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo,ipbf-ttEstBlank.iBlankNo).
            RETURN.
        END.
        IF NOT CAN-DO(gcGlueMatTypes,bf-item.mat-type) THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid material but not a material type of " + gcGlueMatTypes, gcErrorImportant, ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo,ipbf-ttEstBlank.iBlankNo).
            RETURN.
        END.
        IF bf-item.sqin-lb EQ 0 AND bf-item.linin-lb EQ 0 THEN 
        DO:
            RUN pAddError("Glue/Adhesive '" + ipbf-eb.adhesive + "' is valid glue material but no coverage rate configured", gcErrorWarning, ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo,ipbf-ttEstBlank.iBlankNo).
            RETURN.
        END.
        
        FIND FIRST ttGlue EXCLUSIVE-LOCK 
            WHERE ttGlue.estHeaderID EQ ipbf-ttEstBlank.estHeaderID
            AND ttGlue.estFormID EQ ipbf-ttEstBlank.estFormID
            AND ttGlue.estBlankID EQ ipbf-ttEstBlank.estBlankID
            AND ttGlue.iFormNo EQ ipbf-ttEstBlank.iFormNo
            AND ttGlue.iBlankNo EQ ipbf-ttEstBlank.iBlankNo
            AND ttGlue.cItemID EQ ipbf-eb.adhesive
            NO-ERROR.
        IF NOT AVAILABLE ttGlue THEN 
        DO:
            CREATE ttGlue.
            ASSIGN 
                ttGlue.company       = ipbf-ttEstBlank.company
                ttGlue.estHeaderID   = ipbf-ttEstBlank.estHeaderID
                ttGlue.estFormID     = ipbf-ttEstBlank.estFormID
                ttGlue.estBlankID    = ipbf-ttEstBlank.estBlankID
                ttGlue.iFormNo       = ipbf-ttEstBlank.iFormNo
                ttGlue.iBlankNo      = ipbf-ttEstBlank.iBlankNo
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
                    dQtyRequiredPerBlank    = ipbf-ttEstBlank.dBlankArea / ttGlue.dCoverageRate
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
        RUN pAddError("Invalid Ink RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo, ipbf-ttEstBlank.iBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcInkMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Ink RM Code '" + ipcItemCode + "' is not one of '" + gcInkMatTypes + "'", gcErrorImportant,ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo, ipbf-ttEstBlank.iBlankNo).
            RETURN.
        END.
        FIND FIRST ttInk EXCLUSIVE-LOCK 
            WHERE ttInk.estHeaderID EQ ipbf-ttEstBlank.estHeaderID
            AND ttInk.estFormID EQ ipbf-ttEstBlank.estFormID
            AND ttInk.estBlankID EQ ipbf-ttEstBlank.estBlankID
            AND ttInk.iFormNo EQ ipbf-ttEstBlank.iFormNo
            AND ttInk.iBlankNo EQ ipbf-ttEstBlank.iBlankNo
            AND ttInk.cItemID EQ ipcItemCode
            AND ttInk.iPass EQ ipiPass
            NO-ERROR.
        IF NOT AVAILABLE ttInk THEN 
        DO:
            CREATE ttInk.
            ASSIGN 
                ttInk.company          = ipbf-ttEstBlank.company
                ttInk.estHeaderID      = ipbf-ttEstBlank.estHeaderID
                ttInk.estFormID        = ipbf-ttEstBlank.estFormID
                ttInk.estBlankID       = ipbf-ttEstBlank.estBlankID
                ttInk.iFormNo          = ipbf-ttEstBlank.iFormNo
                ttInk.iBlankNo         = ipbf-ttEstBlank.iBlankNo
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
            ttInk.dQtyRequiredPerBlank = ttInk.dCoveragePercent * ipbf-ttEstBlank.dBlankAreaNetWindow / ttInk.dCoverageRate
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
        RUN pAddError("Invalid Leaf/Film RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.iFormNo, ipiBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcLeafMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Leaf/Film RM Code '" + ipcItemCode + "' is not one of '" + gcLeafMatTypes + "'", gcErrorImportant,ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.iFormNo, ipiBlankNo).
            RETURN.
        END.
        FIND FIRST ttLeaf EXCLUSIVE-LOCK 
            WHERE ttLeaf.estHeaderID EQ ipbf-ttEstForm.estHeaderID
            AND ttLeaf.estFormID EQ ipbf-ttEstForm.estFormID
            AND ttLeaf.iFormNo EQ ipbf-ttEstForm.iFormNo
            AND ttLeaf.iBlankNo EQ ipiBlankNo
            AND ttLeaf.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE ttLeaf THEN 
        DO:
            CREATE ttLeaf.
            ASSIGN 
                ttLeaf.company             = ipbf-ttEstForm.company
                ttLeaf.estHeaderID         = ipbf-ttEstForm.estHeaderID
                ttLeaf.estFormID           = ipbf-ttEstForm.estFormID
                ttLeaf.iFormNo             = ipbf-ttEstForm.iFormNo
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
                WHERE ttEstBlank.estHeaderID EQ ttLeaf.estHeaderID
                AND ttEstBlank.estFormID EQ ttLeaf.estFormID
                AND ttEstBlank.iBlankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                ASSIGN 
                    ttEstBlank.dBlankAreaWindow    = ttEstBlank.dBlankAreaWindow + ttLeaf.dAreaInSQIn
                    ttEstBlank.dBlankAreaNetWindow = ttEstBlank.dBlankArea - ttEstBlank.dBlankAreaWindow
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
        RUN pAddError("Invalid Pack RM Code '" + ipcItemCode + "'", gcErrorImportant, ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo, ipbf-ttEstBlank.iBlankNo).
        RETURN.
    END.
    ELSE 
    DO:
        IF NOT CAN-DO(gcPackMatTypes, bf-item.mat-type) THEN 
        DO: 
            RUN pAddError("Material Type for Packing RM Code '" + ipcItemCode + "' is not one of '" + gcPackMatTypes + "'", gcErrorImportant,ipbf-ttEstBlank.estHeaderID, ipbf-ttEstBlank.iFormNo, ipbf-ttEstBlank.iBlankNo).
            RETURN.
        END.
        FIND FIRST opbf-ttPack EXCLUSIVE-LOCK 
            WHERE opbf-ttPack.estHeaderID EQ ipbf-ttEstBlank.estHeaderID
            AND opbf-ttPack.estFormID EQ ipbf-ttEstBlank.estFormID
            AND opbf-ttPack.estBlankID EQ ipbf-ttEstBlank.estBlankID
            AND opbf-ttPack.iFormNo EQ ipbf-ttEstBlank.iFormNo
            AND opbf-ttPack.iBlankNo EQ ipbf-ttEstBlank.iBlankNo
            AND opbf-ttPack.cItemID EQ ipcItemCode
            NO-ERROR.
        IF NOT AVAILABLE opbf-ttPack THEN 
        DO:
            CREATE opbf-ttPack.
            ASSIGN 
                opbf-ttPack.company               = ipbf-ttEstBlank.company
                opbf-ttPack.estHeaderID           = ipbf-ttEstBlank.estHeaderID
                opbf-ttPack.estFormID             = ipbf-ttEstBlank.estFormID
                opbf-ttPack.estBlankID            = ipbf-ttEstBlank.estBlankID
                opbf-ttPack.iFormNo               = ipbf-ttEstBlank.iFormNo
                opbf-ttPack.iBlankNo              = ipbf-ttEstBlank.iBlankNo
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

    IF CAN-DO(gcBoardMatTypes,ipbf-ttEstMaterial.cMaterialType) THEN 
    DO:
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardNoWaste","Board Cost - No Waste",
            ipbf-ttEstMaterial.dCostTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardSetupWaste","Board Cost - Setup Waste",
            ipbf-ttEstMaterial.dCostTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardRunWaste","Board Cost - Run Waste",
            ipbf-ttEstMaterial.dCostTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardSetupVend","Board Cost - Vendor Setup",
            ipbf-ttEstMaterial.dCostSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "boardMinDiff","Board Cost - Minimum Diff",
            ipbf-ttEstMaterial.dCostTotalMinDiff,0).
    END.
    ELSE 
    DO:        
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matNoWaste","Board Cost - No Waste",
            ipbf-ttEstMaterial.dCostTotalNoWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matSetupWaste","Board Cost - Setup Waste",
            ipbf-ttEstMaterial.dCostTotalSetupWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matRunWaste","Board Cost - Run Waste",
            ipbf-ttEstMaterial.dCostTotalRunWaste,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matSetupVend","Board Cost - Vendor Setup",
            ipbf-ttEstMaterial.dCostSetup,0).
        RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "matMinDiff","Board Cost - Minimum Diff",
            ipbf-ttEstMaterial.dCostTotalMinDiff,0).
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
        cCostBin = IF ipbf-ttEstMisc.lIsPrep THEN "P" ELSE "M"
        cCostBin = cCostBin + ipbf-ttEstMisc.cCostType + ipbf-ttEstMisc.cSIMON.
    
    CASE cCostBin:
        WHEN "PLabI" THEN 
            DO:  /*PrepLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabProfit","Prep Labor - Profit",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatI" THEN  
            DO:  /*PrepMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatProfit","Prep Material - Profit",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "PLabM" THEN 
            DO:  /*PrepLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCost","Prep Labor - Cost",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabPrice","Prep Labor - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatM" THEN  
            DO:  /*PrepMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCost","Prep Material - Cost",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatPrice","Prep Material - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).   
            END.                 
        WHEN "PLabS" OR 
        WHEN "PLabO" THEN 
            DO:  /*PrepLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabCostSep","Prep Labor - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pLabProfitSep","Prep Labor - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatS" OR 
        WHEN "PMatO" THEN  
            DO:  /*PrepMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatCostSep","Prep Material - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "pMatProfitSep","Prep Material - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "MLabI" THEN 
            DO:  /*MiscLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabProfit","Misc Labor - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatI" THEN  
            DO:  /*MiscMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatProfit","Misc Material - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "MLabM" THEN 
            DO:  /*MiscLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCost","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabPrice","Misc Labor - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatM" THEN  
            DO:  /*MiscMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCost","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatPrice","Misc Material - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).   
            END.                 
        WHEN "MLabS" OR 
        WHEN "MLabO" THEN 
            DO:  /*MiscLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabCostSep","Misc Labor - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mLabProfitSep","Misc Labor - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatS" OR 
        WHEN "MMatO" THEN  
            DO:  /*MiscMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatCostSep","Misc Material - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "mMatProfitSep","Misc Material - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
    END.
   
        
END PROCEDURE.

PROCEDURE pBuildCostDetailForOperation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstOperation FOR ttEstOperation.

    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupDL","Operation Setup DL",
        ipbf-ttEstOperation.dCostTotalDLSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupVO","Operation Setup VOH",
        ipbf-ttEstOperation.dCostTotalVOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupFO","Operation Setup FOH",
        ipbf-ttEstOperation.dCostTotalFOSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunDL","Operation Run DL",
        ipbf-ttEstOperation.dCostTotalDLRun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunVO","Operation Run VOH",
        ipbf-ttEstOperation.dCostTotalVORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunFO","Operation Run FOH",
        ipbf-ttEstOperation.dCostTotalFORun,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opSetupMinDiff","Operation Setup - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffSetup,0).
    RUN pAddCostDetailForOperation(BUFFER ipbf-ttEstOperation, "opRunMinDiff","Operation Run - Min Charge Diff",
        ipbf-ttEstOperation.dCostTotalMinDiffRun,0).
            
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
            RUN pBuildCostDetailForMaterial(BUFFER ttEstMaterial).                  
                    
        END. /*Each ttEstMaterial for estHeader*/
        FOR EACH ttEstMisc NO-LOCK 
            WHERE ttEstMisc.estHeaderID EQ ttEstHeader.estHeaderID:
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

        
    IF ipbf-ttEstHeader.lIsUnitizedSet AND NOT ipbf-ttEstBlank.iFormNo EQ 0 THEN 
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

    IF ipbf-ttEstHeader.cIndustry EQ gcIndustryFolding THEN
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
    
    DEFINE VARIABLE cEstItemIDSetHeader AS CHARACTER NO-UNDO.
    
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-ttEstHeader.company
        AND eb.est-no EQ ipbf-ttEstHeader.cEstNo
        BY eb.form-no: 
        FIND FIRST bf-ttEstItem NO-LOCK
            WHERE bf-ttEstItem.estHeaderID EQ ipbf-ttEstHeader.estHeaderID
            AND bf-ttEstItem.cCustomerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstItem THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstItem).
            IF eb.form-no EQ 0 THEN 
            DO:
                RUN pAddEstForm(ipbf-ttEstHeader.estHeaderID, 0, BUFFER bf-ttEstForm).
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstForm, BUFFER bf-ttEstBlank).
                IF eb.unitized THEN 
                DO:
                    ipbf-ttEstHeader.lIsUnitizedSet = YES. 
                    RUN pBuildPackingForEb(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                END.
                ASSIGN 
                    bf-ttEstItem.lIsSet = YES
                    cEstItemIDSetHeader = bf-ttEstItem.estItemID
                    .
            END.     
            ELSE 
                bf-ttEstItem.estItemIDParent = cEstItemIDSetHeader.           
        END. /*Create ttEstitem*/
               
    END. /*Build EstItems*/

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
                    ttEstHeader.estHeaderID    = ipcData[1]
                    ttEstHeader.company        = FILL("0",3 - LENGTH(ipcData[2])) + ipcData[2]
                    ttEstHeader.cEstNo         = FILL(" ",8 - LENGTH(ipcData[3])) + ipcData[3]
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
    
    dQtyInOut = ipbf-ttEstHeader.dQtyMaster.
    
    /*Get the effective Est-op quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstHeader.company 
        AND est-op.est-no  EQ ipbf-ttEstHeader.cEstNo 
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
            IF est-op.qty GE ipbf-ttEstHeader.dQtyMaster THEN LEAVE.
        END.
    END.
    
    /*Process each est-op for the right quantity*/
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstHeader.company
        AND est-op.est-no EQ ipbf-ttEstHeader.cEstNo
        AND est-op.s-num EQ ipbf-ttEstForm.iFormNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQtyTarget
        GROUP BY est-op.line DESCENDING:
        
    IF est-op.b-num NE 0 THEN 
    DO:  /*Calculate for Combo*/    
        FIND FIRST ttEstBlank NO-LOCK 
            WHERE ttEstBlank.estHeaderID EQ ipbf-ttEstHeader.estHeaderID
            AND ttEstBlank.estFormID EQ ipbf-ttEstForm.estFormID
            AND ttEstBlank.iBlankNo EQ est-op.b-num
            AND NOT ttEstBlank.lOutputInitialized
            NO-ERROR.
        IF AVAILABLE ttEstBlank THEN 
            ASSIGN 
                ttEstBlank.lOutputInitialized = YES
                dQtyInOut                     = ttEstBlank.iNumOut * ipbf-ttEstForm.iNumOutNet * ipbf-ttEstForm.dGrossQtyRequiredNoWaste
                .
    END.
           
    RUN pAddEstOperationFromEstOp(BUFFER est-op, BUFFER ipbf-ttEstForm, BUFFER bf-ttEstOperation).
                
    IF AVAILABLE bf-ttEstOperation THEN 
    DO: 
        RUN pProcessEstOperation(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER bf-ttEstOperation, INPUT-OUTPUT dQtyInOut, 
            INPUT-OUTPUT dQtyInOutSetupWaste, INPUT-OUTPUT dQtyInOutRunWaste).
        RUN pCalcEstOperation(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstOperation, BUFFER ipbf-ttEstForm).                    
    END.
        
END.
ASSIGN 
    ipbf-ttEstForm.dGrossQtyRequiredSetupWaste = dQtyInOutSetupWaste
    ipbf-ttEstForm.dGrossQtyRequiredRunWaste   = dQtyInOutRunWaste
    ipbf-ttEstForm.dGrossQtyRequiredTotal      = ipbf-ttEstForm.dGrossQtyRequiredNoWaste + ipbf-ttEstForm.dGrossQtyRequiredRunWaste + ipbf-ttEstForm.dGrossQtyRequiredSetupWaste
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
                RUN custom/extradec.p (.0001, ipbf-ef.spec-qty[iIndex] * ipbf-ttEstForm.dQtyFGOnForm,
                    OUTPUT bf-ttEstMaterial.dQtyRequiredNoWaste).
                    
                ASSIGN            
                    bf-ttEstMaterial.cItemName = ipbf-ef.spec-dscr[iIndex]
                    bf-ttEstMaterial.cQtyUOM   = "EA"
                    .
                RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
            END.
        END.         
    END.
END PROCEDURE.

PROCEDURE pBuildSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes all Cost Details for a given header and creates summary
              records for each
     Notes:
    ------------------------------------------------------------------------------*/
    
    FOR EACH ttEstHeader NO-LOCK, 
        EACH ttEstCostDetail NO-LOCK 
        WHERE ttEstCostDetail.estHeaderID EQ ttEstHeader.estHeaderID, 
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
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-ttEstForm      FOR ttEstForm.
    DEFINE BUFFER bf-ttEstBlank     FOR ttEstBlank.
    DEFINE BUFFER bf-ttEstMaterial  FOR ttEstMaterial.
    DEFINE BUFFER bf-ttEstOperation FOR ttEstOperation.
    
    DEFINE VARIABLE iNumOutBlanksOnForm AS INTEGER.
    
    EMPTY TEMP-TABLE ttEstHeader.
    EMPTY TEMP-TABLE ttEstItem.
    EMPTY TEMP-TABLE ttEstForm.
    EMPTY TEMP-TABLE ttEstBlank.
    EMPTY TEMP-TABLE ttEstMisc.
    EMPTY TEMP-TABLE ttEstMaterial.
    EMPTY TEMP-TABLE ttEstOperation.
    EMPTY TEMP-TABLE ttEstCostDetail.
    EMPTY TEMP-TABLE ttEstCostSummary.
    EMPTY TEMP-TABLE ttEstCostCategory.
    EMPTY TEMP-TABLE ttEstCostGroup.
    EMPTY TEMP-TABLE ttEstCostGroupLevel.
    EMPTY TEMP-TABLE ttEstError.
    EMPTY TEMP-TABLE ttInk.
    EMPTY TEMP-TABLE ttGlue.
    
    RUN pLoadData("EstHeader").

    RUN pLoadData("CostGroupLevel").
    RUN pLoadData("CostGroup").
    RUN pLoadData("CostCategory").
    
    RUN pSetGlobalSettings(ipcCompany).
    
    FOR EACH ttEstHeader NO-LOCK:        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ ttEstHeader.company
            AND est.est-no EQ ttEstHeader.cEstNo
            NO-ERROR.
        IF NOT AVAILABLE est THEN RETURN.
        RUN pBuildHeader(BUFFER ttEstHeader).
        RUN pBuildItems(BUFFER ttEstHeader).
       
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no:
            
            RUN pAddEstFormFromEf(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).

            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstForm, BUFFER bf-ttEstBlank).
                iNumOutBlanksOnForm = iNumOutBlanksOnForm + bf-ttEstBlank.iNumOut.
                RUN pBuildInksForEb(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                RUN pAddGlue(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                RUN pBuildPackingForEb(BUFFER ttEstHeader, BUFFER bf-ttEstBlank, BUFFER eb).
                
            END. /*Each eb of ef*/
            
            ASSIGN 
                bf-ttEstForm.iNumOut                  = iNumOutBlanksOnForm * bf-ttEstForm.iNumOutNet
                bf-ttEstForm.dGrossQtyRequiredNoWaste = ttEstHeader.dQtyMaster / bf-ttEstForm.iNumOut
                .
            RUN pProcessOperations(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessLeafs(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessBoard(BUFFER ttEstHeader, BUFFER bf-ttEstForm, ef.board).           
            RUN pProcessInks(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessPacking(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessGlues(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
            RUN pProcessSpecialMaterials(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).  
            RUN pProcessMiscPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.dQtyMaster).
            RUN pProcessMiscNonPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.dQtyMaster).          
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

    ipbf-ttEstMaterial.dQtyRequiredTotal    = ipbf-ttEstMaterial.dQtyRequiredNoWaste + ipbf-ttEstMaterial.dQtyRequiredSetupWaste + 
        ipbf-ttEstMaterial.dQtyRequiredRunWaste + ipbf-ttEstMaterial.dQtyRequiredMinDiff.    
    IF ipbf-ttEstMaterial.dCostPerUOMOverride EQ 0 THEN 
        RUN pGetEstMaterialCosts(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstMaterial,ipbf-ttEstMaterial.dQtyRequiredTotal,ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cVendorID, 
            OUTPUT ipbf-ttEstMaterial.dCostPerUOM, OUTPUT ipbf-ttEstMaterial.cCostUOM,  OUTPUT ipbf-ttEstMaterial.dCostSetup ).
    ELSE 
        ipbf-ttEstMaterial.dCostPerUOM = ipbf-ttEstMaterial.dCostPerUOMOverride.
    IF ipbf-ttEstMaterial.cCostUOM EQ "" THEN 
        ipbf-ttEstMaterial.cCostUOM = "EA".
    IF ipbf-ttEstMaterial.cCostUOM NE ipbf-ttEstMaterial.cQtyUOM THEN 
    DO:
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cCostUOM, 
            ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
            ipbf-ttEstMaterial.dQtyRequiredNoWaste, OUTPUT ipbf-ttEstMaterial.dQtyRequiredNoWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cCostUOM, 
            ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
            ipbf-ttEstMaterial.dQtyRequiredSetupWaste, OUTPUT ipbf-ttEstMaterial.dQtyRequiredSetupWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cCostUOM, 
            ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
            ipbf-ttEstMaterial.dQtyRequiredRunWaste, OUTPUT ipbf-ttEstMaterial.dQtyRequiredRunWasteInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cCostUOM, 
            ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
            ipbf-ttEstMaterial.dQtyRequiredMinDiff, OUTPUT ipbf-ttEstMaterial.dQtyRequiredMinDiffInCostUOM).
        RUN custom/convquom.p(ipbf-ttEstMaterial.company, ipbf-ttEstMaterial.cQtyUOM, ipbf-ttEstMaterial.cCostUOM, 
            ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
            ipbf-ttEstMaterial.dQtyRequiredTotal, OUTPUT ipbf-ttEstMaterial.dQtyRequiredTotalInCostUOM).
    END.
    ELSE 
        ASSIGN 
            ipbf-ttEstMaterial.dQtyRequiredNoWasteInCostUOM    = ipbf-ttEstMaterial.dQtyRequiredNoWaste
            ipbf-ttEstMaterial.dQtyRequiredSetupWasteInCostUOM = ipbf-ttEstMaterial.dQtyRequiredSetupWaste
            ipbf-ttEstMaterial.dQtyRequiredRunWasteInCostUOM   = ipbf-ttEstMaterial.dQtyRequiredRunWaste
            ipbf-ttEstMaterial.dQtyRequiredMinDiffInCostUOM    = ipbf-ttEstMaterial.dQtyRequiredMinDiff
            ipbf-ttEstMaterial.dQtyRequiredTotalInCostUOM      = ipbf-ttEstMaterial.dQtyRequiredTotal
            .
    ASSIGN 
        ipbf-ttEstMaterial.dCostTotalNoWaste                = ipbf-ttEstMaterial.dQtyRequiredNoWasteInCostUOM * ipbf-ttEstMaterial.dCostPerUOM
        ipbf-ttEstMaterial.dCostTotalSetupWaste             = ipbf-ttEstMaterial.dQtyRequiredSetupWasteInCostUOM * ipbf-ttEstMaterial.dCostPerUOM + ipbf-ttEstMaterial.dCostSetup
        ipbf-ttEstMaterial.dCostTotalRunWaste               = ipbf-ttEstMaterial.dQtyRequiredRunWasteInCostUOM * ipbf-ttEstMaterial.dCostPerUOM
        ipbf-ttEstMaterial.dCostTotalMinDiff                = ipbf-ttEstMaterial.dQtyRequiredMinDiffInCostUOM * ipbf-ttEstMaterial.dCostPerUOM
        ipbf-ttEstMaterial.dCostTotal                       = ipbf-ttEstMaterial.dCostTotalNoWaste + ipbf-ttEstMaterial.dCostTotalSetupWaste + 
                                                              ipbf-ttEstMaterial.dCostTotalRunWaste + ipbf-ttEstMaterial.dCostTotalMinDiff
        ipbf-ttEstMaterial.dCostTotalPerMFinished           = ipbf-ttEstMaterial.dCostTotal / (ipbf-ttEstForm.dQtyFGOnForm / 1000)
        ipbf-ttEstMaterial.dCostTotalPerMFinishedNoWaste    = ipbf-ttEstMaterial.dCostTotalNoWaste / (ipbf-ttEstForm.dQtyFGOnForm / 1000)
        ipbf-ttEstMaterial.dCostTotalPerMFinishedSetupWaste = ipbf-ttEstMaterial.dCostTotalSetupWaste  / (ipbf-ttEstForm.dQtyFGOnForm / 1000)
        ipbf-ttEstMaterial.dCostTotalPerMFinishedRunWaste   = ipbf-ttEstMaterial.dCostTotalRunWaste / (ipbf-ttEstForm.dQtyFGOnForm / 1000)
        .        
    
END PROCEDURE.

PROCEDURE pCalcEstMisc PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a ttEstMisc buffer, calculate common fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMisc FOR ttEstMisc.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.

    ipbf-ttEstMisc.dProfit = fGetProfit(ipbf-ttEstMisc.dCostTotalBeforeProfit, ipbf-ttEstMisc.dProfitPercent, ipbf-ttEstMisc.cPercentType).
    IF ipbf-ttEstMisc.cSIMON EQ "M" THEN 
        ipbf-ttEstMisc.dCostTotal = ipbf-ttEstMisc.dCostTotalBeforeProfit.
    ELSE 
        ipbf-ttEstMisc.dCostTotal = ipbf-ttEstMisc.dCostTotalBeforeProfit + ipbf-ttEstMisc.dProfit.
    ipbf-ttEstMisc.dCostTotalPerMFinished = ipbf-ttEstMisc.dCostTotal / (ipbf-ttEstForm.dQtyFGOnForm / 1000).

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
    
    IF ipbf-ttEstOperation.dSpeed NE 0 THEN
        IF ipbf-ttEstOperation.lSpeedInLF THEN
            ipbf-ttEstOperation.dHoursRun = ipbf-ttEstOperation.dQtyInAfterSetupWasteLF / ipbf-ttEstOperation.dSpeed. 
        ELSE 
            ipbf-ttEstOperation.dHoursRun = ipbf-ttEstOperation.dQtyInAfterSetupWaste / ipbf-ttEstOperation.dSpeed.
    ELSE 
        ipbf-ttEstOperation.dHoursRun = 0.
    
    ASSIGN    
        ipbf-ttEstOperation.dCostPerHourTotalRun   = ipbf-ttEstOperation.dCostPerManHourDLRun * ipbf-ttEstOperation.dCrewSizeRun + 
                                                     ipbf-ttEstOperation.dCostPerHourFORun + ipbf-ttEstOperation.dCostPerHourVORun
        ipbf-ttEstOperation.dCostPerHourTotalSetup = ipbf-ttEstOperation.dCostPerManHourDLSetup * ipbf-ttEstOperation.dCrewSizeSetup + 
                                                     ipbf-ttEstOperation.dCostPerHourFOSetup + ipbf-ttEstOperation.dCostPerHourVOSetup
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
        RUN pAddError("Board '" + ipcITemID + "' is not valid", gcErrorImportant, ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.iFormNo, 0).
        RETURN.
    END.
    IF NOT CAN-DO(gcBoardMatTypes,bf-item.mat-type) THEN 
    DO:
        RUN pAddError("Board '" + ipcITemID + "' is valid material but not a material type of " + gcBoardMatTypes, gcErrorImportant, ipbf-ttEstForm.estHeaderID, ipbf-ttEstForm.iFormNo, 0).
        RETURN.
    END.
    RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipcItemID, 0, BUFFER bf-ttEstMaterial).
    ASSIGN 
        bf-ttEstMaterial.lIsPrimarySubstrate    = YES
        bf-ttEstMaterial.lAddToWeightFG         = YES
        bf-ttEstMaterial.lAddToWeightTare       = NO
                                       
        bf-ttEstMaterial.dQtyRequiredNoWaste    = ipbf-ttEstForm.dGrossQtyRequiredNoWaste
        bf-ttEstMaterial.dQtyRequiredSetupWaste = ipbf-ttEstForm.dGrossQtyRequiredSetupWaste
        bf-ttEstMaterial.dQtyRequiredRunWaste   = ipbf-ttEstForm.dGrossQtyRequiredRunWaste
        bf-ttEstMaterial.cQtyUOMWaste           = "EA"
        bf-ttEstMaterial.cQtyUOM                = "EA"
        bf-ttEstMaterial.dBasisWeight           = bf-item.basis-w
        bf-ttEstMaterial.dDimWidth              = ipbf-ttEstForm.dGrossWidth
        bf-ttEstMaterial.dDimLength             = ipbf-ttEstForm.dGrossLength
        bf-ttEstMaterial.dDimDepth              = ipbf-ttEstForm.dGrossDepth
        .
    IF ipbf-ttEstForm.dCostPerUOMOverride NE 0 THEN 
        ASSIGN 
            bf-ttEstMaterial.dCostPerUOMOverride = ipbf-ttEstForm.dCostPerUOMOverride
            bf-ttEstMaterial.cCostUOM            = ipbf-ttEstForm.cCostUOMOverride
            .
        
    RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).   
    
    ASSIGN 
        ipbf-ttEstForm.dGrossQtyRequiredTotal       = ipbf-ttEstForm.dGrossQtyRequiredNoWaste + ipbf-ttEstForm.dGrossQtyRequiredSetupWaste + ipbf-ttEstForm.dGrossQtyRequiredRunWaste
        ipbf-ttEstForm.dGrossQtyRequiredTotalArea   = ipbf-ttEstForm.dGrossQtyRequiredTotal * ipbf-ttEstForm.dGrossArea / 1000
        ipbf-ttEstForm.dGrossQtyRequiredTotalWeight = ipbf-ttEstForm.dGrossQtyRequiredTotalArea * ipbf-ttEstForm.dBasisWeightInLbsPerMSF
        .
    
    IF ipbf-ttEstForm.lNoCharge THEN 
        ASSIGN 
            bf-ttEstMaterial.dCostPerUOM                      = 0
            bf-ttEstMaterial.dCostSetup                       = 0
            bf-ttEstMaterial.dCostTotal                       = 0
            bf-ttEstMaterial.dCostTotalMinDiff                = 0
            bf-ttEstMaterial.dCostTotalNoWaste                = 0
            bf-ttEstMaterial.dCostTotalSetupWaste             = 0
            bf-ttEstMaterial.dCostTotalRunWaste               = 0
            bf-ttEstMaterial.dCostTotalPerMFinished           = 0
            bf-ttEstMaterial.dCostTotalPerMFinishedNoWaste    = 0
            bf-ttEstMaterial.dCostTotalPerMFinishedRunWaste   = 0
            bf-ttEstMaterial.dCostTotalPerMFinishedSetupWaste = 0
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
        WHERE ttEstOperation.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttEstOperation.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttEstOperation.lIsGluer, 
        EACH ttGlue NO-LOCK
        WHERE ttGlue.estHeaderID EQ ttEstOperation.estHeaderID
        AND ttGlue.estFormID EQ ttEstOperation.estFormID
        AND ttGlue.estBlankID EQ ttEstOperation.estBlankID
        ,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttGlue.estHeaderID
        AND ttEstBlank.estFormID EQ ttGlue.estFormID 
        AND ttEstBlank.estBlankID EQ ttGlue.estBlankID 
        BY ttEstOperation.iSequence DESCENDING:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttGlue.cItemID, ttGlue.iBlankNo, BUFFER bf-ttEstMaterial).
        
        ASSIGN    
            bf-ttEstMaterial.dQtyRequiredNoWaste    = ttEstOperation.dQtyInNoWaste * ttGlue.dQtyRequiredPerBlank
            bf-ttEstMaterial.dQtyRequiredRunWaste   = ttEstOperation.dQtyInRunWaste * ttGlue.dQtyRequiredPerBlank
            bf-ttEstMaterial.dQtyRequiredSetupWaste = ttEstOperation.dQtyInSetupWaste * ttGlue.dQtyRequiredPerBlank
            dQtyRequiredMinDiff                     = ttGlue.dMinLbsPerJob - (bf-ttEstMaterial.dQtyRequiredNoWaste + bf-ttEstMaterial.dQtyRequiredRunWaste + bf-ttEstMaterial.dQtyRequiredSetupWaste)
            bf-ttEstMaterial.cQtyUOM                = ttGlue.cQtyUOM
            .             
        IF dQtyRequiredMinDiff GT 0 THEN 
            bf-ttEstMaterial.dQtyRequiredMinDiff = dQtyRequiredMinDiff.
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
        AND bf-est.est-no EQ ipbf-ttEstHeader.cEstNo
        NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Estimate '" + ipbf-ttEstHeader.cEstNo + "' not valid", gcErrorCritical, ipbf-ttEstHeader.estHeaderID, 0,0).
        RETURN.
    END. 
    FIND FIRST bf-ce-ctrl NO-LOCK 
        WHERE bf-ce-ctrl.company EQ ipbf-ttEstHeader.company
        AND bf-ce-ctrl.loc EQ ipbf-ttEstHeader.locationID
        NO-ERROR.
    IF NOT AVAILABLE bf-ce-ctrl THEN 
        FIND FIRST bf-ce-ctrl NO-LOCK 
            WHERE bf-ce-ctrl.company EQ ipbf-ttEstHeader.company
            NO-ERROR.
    IF NOT AVAILABLE bf-est THEN 
    DO:
        RUN pAddError("Control File not found for company '" + ipbf-ttEstHeader.company + "'", gcErrorCritical, ipbf-ttEstHeader.estHeaderID, 0,0).
        RETURN.
    END.
    ASSIGN 
        ipbf-ttEstHeader.cIndustry = IF bf-est.est-type LE 4 THEN gcIndustryFolding ELSE gcIndustryCorrugated
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
        dQtyRequiredPerForm                     = ipbf-ttEstBlank.iNumOut * ipbf-ttInk.dQtyRequiredPerBlank
        bf-ttEstMaterial.dQtyRequiredNoWaste    = ipbf-ttEstOperation.dQtyInNoWaste * dQtyRequiredPerForm
        bf-ttEstMaterial.dQtyRequiredRunWaste   = ipbf-ttEstOperation.dQtyInRunWaste * dQtyRequiredPerForm
        bf-ttEstMaterial.dQtyRequiredSetupWaste = ipbf-ttEstOperation.dQtyInSetupWaste * dQtyRequiredPerForm + ipbf-ttEstOperation.dQtyInkLbsWastedPerSetup
        dQtyRequiredMinDiff                     = ipbf-ttInk.dMinLbsPerJob - (bf-ttEstMaterial.dQtyRequiredNoWaste + bf-ttEstMaterial.dQtyRequiredRunWaste + bf-ttEstMaterial.dQtyRequiredSetupWaste)
        bf-ttEstMaterial.cQtyUOM                = ipbf-ttInk.cQtyUOM
        .             
    IF dQtyRequiredMinDiff GT 0 THEN 
        bf-ttEstMaterial.dQtyRequiredMinDiff = dQtyRequiredMinDiff.
    
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
    DEFINE PARAMETER BUFFER ipbf-ttLeaf          FOR ttLeaf.
    DEFINE INPUT PARAMETER ipdQtyRequiredPerForm AS DECIMAL NO-UNDO.    
 
    DEFINE           BUFFER bf-ttEstMaterial    FOR ttEstMaterial.
        
    RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ipbf-ttLeaf.cItemID, ipbf-ttLeaf.iBlankNo, BUFFER bf-ttEstMaterial).
        
    ASSIGN    
        bf-ttEstMaterial.dDimLength             = ipbf-ttLeaf.dDimLength
        bf-ttEstMaterial.dDimWidth              = ipbf-ttLeaf.dDimWidth
        bf-ttEstMaterial.dQtyRequiredNoWaste    = ipbf-ttEstOperation.dQtyInNoWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.dQtyRequiredRunWaste   = ipbf-ttEstOperation.dQtyInRunWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.dQtyRequiredSetupWaste = ipbf-ttEstOperation.dQtyInSetupWaste * ipdQtyRequiredPerForm
        bf-ttEstMaterial.cQtyUOM                = ipbf-ttLeaf.cQtyUOM
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
        WHERE ttEstOperation.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttEstOperation.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttEstOperation.lIsPrinter, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstOperation.estHeaderID
        AND ttInk.estFormID EQ ttEstOperation.estFormID
        AND ttInk.iPass EQ ttEstOperation.iPass
        AND ttInk.iCountInks GT 0,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttInk.estHeaderID
        AND ttEstBlank.estFormID EQ ttInk.estFormID 
        AND ttEstBlank.estBlankID EQ ttInk.estBlankID :
        
        RUN pProcessInk(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ttEstBlank, BUFFER ttEstOperation, BUFFER ttInk).    
        
    END.
    /*Coatings*/
    FOR EACH ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttEstOperation.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttEstOperation.lIsCoater, 
        EACH ttInk NO-LOCK
        WHERE ttInk.estHeaderID EQ ttEstOperation.estHeaderID
        AND ttInk.estFormID EQ ttEstOperation.estFormID
        AND ttInk.iPass EQ ttEstOperation.iPass
        AND ttInk.iCountCoatings GT 0,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttInk.estHeaderID
        AND ttEstBlank.estFormID EQ ttInk.estFormID 
        AND ttEstBlank.estBlankID EQ ttInk.estBlankID :
            
        RUN pProcessInk(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ttEstBlank, BUFFER ttEstOperation, BUFFER ttInk).    
    END.

END PROCEDURE.
PROCEDURE pProcessLeafs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: for a given form, build the ttEstMaterial for leafs with the 
     quantity required
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.

    DEFINE VARIABLE dQtyRequiredPerForm AS DECIMAL NO-UNDO.
    
    RUN pBuildLeafForEf(BUFFER ipbf-ef, BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm).
    
    FOR FIRST ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttEstOperation.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttEstOperation.lIsLeafer
        , 
        EACH ttLeaf NO-LOCK
        WHERE ttLeaf.estHeaderID EQ ttEstOperation.estHeaderID
        AND ttLeaf.estFormID EQ ttEstOperation.estFormID
        BY ttEstOperation.iSequence DESCENDING:
        
        IF ttEstOperation.cOperationFeedType EQ "B" THEN DO:            
            FIND FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.estHeaderID EQ ttLeaf.estHeaderID
                AND ttEstBlank.estFormID EQ ttLeaf.estFormID 
                AND ttEstBlank.iBlankNo EQ ttLeaf.iBlankNo
                NO-ERROR.
            IF AVAILABLE ttEstBlank THEN 
                dQtyRequiredPerForm = ttEstBlank.iNumOut * ttLeaf.dQtyRequiredPerLeaf.
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
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttPack.lIsCase,
        FIRST ttEstBlank EXCLUSIVE-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estFormID EQ ttPack.estFormID
        AND ttEstBlank.estBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        IF ttPack.iCountPerSubUnit NE 0 THEN
            ASSIGN
                iCaseCount = ttPack.iCountPerSubUnit  
                iCases     = fRoundUp(ttEstBlank.dQtyRequired / ttPack.iCountPerSubUnit) * ttPack.dQtyMultiplier.
        ELSE 
            ASSIGN 
                iCases     = fRoundUp(ttEstBlank.dQtyRequired * ttEstBlank.dWeight / ttPack.dWeightCapacity) * ttPack.dQtyMultiplier
                iCaseCount = TRUNCATE(ttEstBlank.dQtyRequired /  iCases, 0)
                .
        ASSIGN  
            bf-ttEstMaterial.dQtyRequiredNoWaste = iCases
            ttEstBlank.iSubUnits                 = iCases
            bf-ttEstMaterial.cQtyUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-ttEstMaterial.cItemName = bf-ttEstMaterial.cItemName + " (" + TRIM(STRING(iCaseCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
    END.
    
    /*Pallet*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estFormID
        AND ttPack.lIsPallet,
        FIRST ttEstBlank EXCLUSIVE-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estFormID EQ ttPack.estFormID
        AND ttEstBlank.estBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        ASSIGN  
            iPalletCount                         = IF ttPack.iCountPerUnit EQ 0 THEN ttPack.iCountSubUnitsPerUnit * iCaseCount ELSE ttPack.iCountPerUnit
            iPallets                             = fRoundUp(ttEstBlank.dQtyRequired / iPalletCount) * ttPack.dQtyMultiplier 
            bf-ttEstMaterial.dQtyRequiredNoWaste = iPallets
            bf-ttEstMaterial.cQtyUOM             = ttPack.cQtyUOM
            .            
        
        IF iCaseCount NE 0 THEN 
            bf-ttEstMaterial.cItemName = bf-ttEstMaterial.cItemName + " (" + TRIM(STRING(iPalletCount,">>>>>9")) + ")".   
        
        RUN pCalcEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstMaterial, BUFFER ipbf-ttEstForm).
    END.
    
    /*Other Pack*/
    FOR EACH ttPack NO-LOCK 
        WHERE ttPack.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        AND ttPack.estFormID EQ ipbf-ttEstForm.estFormID
        AND NOT ttPack.lIsPallet AND NOT ttPack.lIsCase,
        FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttPack.estHeaderID
        AND ttEstBlank.estFormID EQ ttPack.estFormID
        AND ttEstBlank.estBlankID EQ ttPack.estBlankID:
        
        RUN pAddEstMaterial(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, ttPack.cItemID, ttPack.iBlankNo, BUFFER bf-ttEstMaterial).
        
        
        ASSIGN 
            bf-ttEstMaterial.dQtyRequiredNoWaste = IF ttPack.cQtyMultiplierPer EQ "P" THEN iPallets * ttPack.dQtyMultiplier ELSE iCases * ttPack.dQtyMultiplier
            bf-ttEstMaterial.cQtyUOM             = ttPack.cQtyUOM
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
        AND e-item.i-no EQ ipbf-ttEstMaterial.cItemID
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
                    ipbf-ttEstMaterial.dBasisWeight, ipbf-ttEstMaterial.dDimLength, ipbf-ttEstMaterial.dDimWidth, ipbf-ttEstMaterial.dDimDepth, 
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

    IF ipbf-ttEstMaterial.lIsRealMaterial AND NOT lCostFound THEN
        ASSIGN 
            opdCost    = IF ipbf-ttEstHeader.lForRealItemsUseAvgCost THEN ipbf-ttEstMaterial.dCostPerUOMAvg ELSE ipbf-ttEstMaterial.dCostPerUOMLast
            opcCostUOM = ipbf-ttEstMaterial.cQtyUOM  /*REFACTOR? - What uom is avg and last cost in*/
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

PROCEDURE pProcessEstOperation PRIVATE:
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
        ipbf-ttEstOperation.dQtyOut       = iopdQtyInOut  /*This machines out is last machines in*/  
        ipbf-ttEstOperation.dQtyInNoWaste = ipbf-ttEstOperation.dQtyOut / ipbf-ttEstOperation.iNumOutForOperation  /*Get QtyIn in Feed units*/
        iopdQtyInOutSetupWaste            = iopdQtyInOutSetupWaste / ipbf-ttEstOperation.iNumOutForOperation
        iopdQtyInOutRunWaste              = iopdQtyInOutRunWaste / ipbf-ttEstOperation.iNumOutForOperation
        ipbf-ttEstOperation.dQtyIn        = fRoundUp(ipbf-ttEstOperation.dQtyIn)
        iopdQtyInOutSetupWaste            = fRoundUp(iopdQtyInOutSetupWaste)
        iopdQtyInOutRunWaste              = fRoundUp(iopdQtyInOutRunWaste)
        .
    
    /*Recalc from standards off right now*/
    IF NOT ipbf-ttEstOperation.lIsLocked THEN 
    DO:
        FIND FIRST bf-mach NO-LOCK 
            WHERE bf-mach.company EQ ipbf-ttEstHeader.company
            AND bf-mach.m-code EQ ipbf-ttEstOperation.cOperationID
            NO-ERROR.
        //RUN pRecalcEstOperationFromStandardsSetupWaste(BUFFER ipbf-ttEstHeader, BUFFER ipbf-ttEstForm, BUFFER ipbf-ttEstOperation, BUFFER bf-mach).
       
    END.
    
    ASSIGN 
        ipbf-ttEstOperation.dQtyInRunWaste        = (ipbf-ttEstOperation.dQtyInNoWaste / 
                                                    (1 - (ipbf-ttEstOperation.dQtyInRunWastePercent / 100))) 
                                                    - ipbf-ttEstOperation.dQtyInNoWaste
        ipbf-ttEstOperation.dQtyInRunWaste        = fRoundUp(ipbf-ttEstOperation.dQtyInRunWaste)
        ipbf-ttEstOperation.dQtyInAfterSetupWaste = ipbf-ttEstOperation.dQtyInNoWaste + ipbf-ttEstOperation.dQtyInRunWaste
        ipbf-ttEstOperation.dQtyIn                = ipbf-ttEstOperation.dQtyInAfterSetupWaste + ipbf-ttEstOperation.dQtyInSetupWaste
        iopdQtyInOutRunWaste                      = iopdQtyInOutRunWaste + ipbf-ttEstOperation.dQtyInRunWaste
        iopdQtyInOutSetupWaste                    = iopdQtyInOutSetupWaste + ipbf-ttEstOperation.dQtyInSetupWaste
        ipbf-ttEstOperation.dQtyIn                = fRoundUp(ipbf-ttEstOperation.dQtyIn)
        iopdQtyInOut                              = ipbf-ttEstOperation.dQtyIn
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

    ipbf-ttEstOperation.dQtyInSetupWaste = ipbf-mach.mr-waste.
    IF ipbf-ttEstOperation.lIsPrinter OR ipbf-ttEstOperation.lIsCoater THEN 
        ipbf-ttEstOperation.dQtyInSetupWaste = ipbf-ttEstOperation.dQtyInSetupWaste + 
            (ipbf-ttEstOperation.dQtyInSetupWastePerColor * 
            IF glUsePlateChangesAsColorForSetupWaste AND ipbf-ttEstOperation.iCountPlateChanges NE 0 THEN ipbf-ttEstOperation.iCountPlateChanges
            ELSE (ipbf-ttEstOperation.iCountCoats + ipbf-ttEstOperation.iCountInks)).

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

FUNCTION fGetEstBlankID RETURNS CHARACTER PRIVATE
    (ipcEstHeaderID AS CHARACTER , ipcEstFormID AS CHARACTER , ipiBlankNo AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the Blank ID given header, form id and blank #
     Notes:
    ------------------------------------------------------------------------------*/    
    FIND FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ipcEstHeaderID
        AND ttEstBlank.estFormID EQ ipcEstFormID
        AND ttEstBlank.iBlankNo EQ ipiBlankNo
        NO-ERROR.
    IF AVAILABLE ttEstBlank THEN 
        RETURN ttEstBlank.estBlankID.
        
END FUNCTION.

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
