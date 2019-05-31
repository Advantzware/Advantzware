
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
DEFINE VARIABLE gcSourceTypeMisc      AS CHARACTER NO-UNDO INITIAL "Miscellaneous".

/*Settings Globals*/
DEFINE VARIABLE gcPrepRoundTo         AS CHARACTER NO-UNDO.  /*CEPREP - char val - potentially deprecate*/
DEFINE VARIABLE gcPrepMarkupOrMargin  AS CHARACTER NO-UNDO.  /*CEPrepPrice - char val*/
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

/* ***************************  Main Block  *************************** */

RUN pBuildTestData("001").
RUN pBuildDetail.
RUN pBuildSummary.

FIND FIRST ttEstHeader NO-LOCK 
    WHERE ttEstHeader.estHeaderID = "header2".

RUN est\EstimatePrint.p (ROWID(ttEstHeader), gcOutputFile, "By Form with Item Summary","Baskerville Old Face").

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
        opbf-ttEstBlank.rec_key       = fGetNextRecKey()
        opbf-ttEstBlank.estBlankID    = fGetNextID()
        opbf-ttEstBlank.estFormID     = ipbf-ttEstForm.estFormID
        opbf-ttEstBlank.estHeaderID   = ipbf-ttEstForm.estHeaderID
        opbf-ttEstBlank.iBlankNo      = ipbf-eb.blank-no
        opbf-ttEstBlank.iNumOutLength = ipbf-eb.num-len
        opbf-ttEstBlank.iNumOutWidth  = ipbf-eb.num-wid
        opbf-ttEstBlank.iNumOutDepth  = ipbf-eb.num-dep
        opbf-ttEstBlank.iNumOut       = MAX(opbf-ttEstBlank.iNumOutWidth, 1) * MAX(opbf-ttEstBlank.iNumOutLength, 1) * MAX(opbf-ttEstBlank.iNumOutDepth, 1)
        /*                    opbf-iOutTotal                = iOutTotal + opbf-ttEstBlank.iNumOut*/
        opbf-ttEstBlank.dBlankWidth   = ipbf-eb.t-wid
        opbf-ttEstBlank.dBlankLength  = ipbf-eb.t-len
        opbf-ttEstBlank.dBlankDepth   = ipbf-eb.t-dep
        opbf-ttEstBlank.dBlankArea    = ipbf-eb.t-sqin
        opbf-ttEstBlank.dLength       = ipbf-eb.len
        opbf-ttEstBlank.dWidth        = ipbf-eb.wid
        opbf-ttEstBlank.dDepth        = ipbf-eb.dep
                                        
        /*Refactor - Hardcoded*/
        opbf-ttEstBlank.cUOMArea      = "SQIN"
        opbf-ttEstBlank.cUOMDimension = "In"
        opbf-ttEstBlank.cUOMWeight    = "LB/M"
                    
        /*Refactor - apply area UOM conversion*/
        opbf-ttEstBlank.dWeight       = ipbf-ttEstForm.dBasisWeightInLbsPerMSF * opbf-ttEstBlank.dBlankArea / 144 
        .
                    
    FIND FIRST ttEstItem EXCLUSIVE-LOCK 
        WHERE ttEstItem.estHeaderID EQ opbf-ttEstBlank.estHeaderID
        AND ttEstItem.cCustomerPart EQ ipbf-eb.part-no
        NO-ERROR 
        .
    IF AVAILABLE ttEstItem THEN 
    DO:
        ASSIGN 
            opbf-ttEstBlank.estItemID = ttEstItem.estItemID
            ttEstItem.cSize           = TRIM(STRING(opbf-ttEstBlank.dLength,">>>9.99")) + " x " + TRIM(STRING(opbf-ttEstBlank.dWidth,">>>9.99"))
            .
        IF opbf-ttEstBlank.dDepth NE 0 THEN 
            ttEstItem.cSize = ttEstItem.cSize + " x " + TRIM(STRING(opbf-ttEstBlank.dDepth,">>>9.99")).
    END.

END PROCEDURE.

PROCEDURE pAddEstForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given an ef buffer, create the EstForm
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader. 
    DEFINE PARAMETER BUFFER opbf-ttEstForm   FOR ttEstForm.

    CREATE opbf-ttEstForm.
    ASSIGN 
        opbf-ttEstForm.rec_key                         = fGetNextRecKey()
        opbf-ttEstForm.estFormID                       = fGetNextID()
        opbf-ttEstForm.estHeaderID                     = ipbf-ttEstHeader.estHeaderID
        opbf-ttEstForm.iFormNo                         = ipbf-ef.form-no
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
                                    
        /*Refactor - Calculate from Operations*/
        opbf-ttEstForm.dGrossQtyRequiredWasteSetup     = 800
        opbf-ttEstForm.dGrossQtyRequiredWasteRun       = 121
            
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
        opbf-ttEstItem.cCustomerPart     = eb.part-no
        opbf-ttEstItem.dQtyPerParent     = eb.quantityPerSet
        opbf-ttEstItem.dQtyRequired      = ipbf-ttEstHeader.dQtyMaster * opbf-ttEstItem.dQtyPerParent
        opbf-ttEstItem.dQtyPerParent     = opbf-ttEstItem.dQtyRequired
        opbf-ttEstItem.cColor            = eb.i-coldscr
        opbf-ttEstItem.cCustomerID       = eb.cust-no
        opbf-ttEstItem.cShipToID         = eb.ship-id
        opbf-ttEstItem.cItemName         = eb.part-dscr1
        opbf-ttEstItem.cItemDescription1 = eb.part-dscr2
        opbf-ttEstItem.cSalesgroupID     = eb.sman
        opbf-ttEstItem.cStyleID          = eb.style
        opbf-ttEstItem.lIsSet            = eb.is-a-set
        opbf-ttEstItem.cFGItemID         = eb.stock-no
        .
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ eb.company
        AND cust.cust-no EQ eb.cust-no
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
            opbf-ttEstItem.cCustomerName     = eb.ship-name
            opbf-ttEstItem.cCustomerAddress1 = eb.ship-addr[1]
            opbf-ttEstItem.cCustomerAddress2 = eb.ship-addr[2]
            opbf-ttEstItem.cCustomerAddress3 = eb.ship-city + ", " + eb.ship-state + " " + eb.ship-zip
            .
    FIND FIRST shipto NO-LOCK 
        WHERE shipto.company EQ eb.company
        AND shipto.cust-no EQ eb.cust-no
        AND shipto.ship-id EQ eb.ship-id
        NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN 
            opbf-ttEstItem.cShipToName     = shipto.ship-name
            opbf-ttEstItem.cShipToAddress1 = shipto.ship-addr[1]
            opbf-ttEstItem.cShipToAddress2 = shipto.ship-addr[2]
            opbf-ttEstItem.cShipToAddress3 = shipto.ship-city + ", " + shipto.ship-state + " " + shipto.ship-zip
            .
    FIND FIRST sman NO-LOCK 
        WHERE sman.company EQ eb.company
        AND sman.sman EQ eb.sman
        NO-ERROR.
    IF AVAILABLE sman THEN 
        ASSIGN 
            opbf-ttEstItem.cSalesgroupName = sman.sname        
            .
    FIND FIRST style NO-LOCK 
        WHERE style.company EQ eb.company
        AND style.style EQ eb.style
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
            opbf-ttEstMaterial.rec_key       = fGetNextRecKey()
            opbf-ttEstMaterial.estMaterialID = fGetNextID()
            opbf-ttEstMaterial.estFormID     = ipbf-ttEstForm.estFormID
            opbf-ttEstMaterial.estHeaderID   = ipbf-ttEstForm.estHeaderID
            opbf-ttEstMaterial.iFormNo       = ipbf-ttEstForm.iFormNo
            opbf-ttEstMaterial.iBlankNo      = ipiBlankNo
            opbf-ttEstMaterial.cItemID       = bf-item.i-no 
            opbf-ttEstMaterial.cItemName     = bf-item.i-name 
            opbf-ttEstMaterial.cQtyUOM       = bf-item.cons-uom
            .
        IF opbf-ttEstMaterial.iBlankNo NE 0 THEN 
            opbf-ttEstMaterial.estBlankID = fGetEstBlankID(opbf-ttEstMaterial.estHeaderID,opbf-ttEstMaterial.estFormID, opbf-ttEstMaterial.iBlankNo).
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

PROCEDURE pAddEstOperation PRIVATE:
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
            opbf-ttEstOperation.rec_key             = fGetNextRecKey()
            opbf-ttEstOperation.estOperationID      = fGetNextID()
            opbf-ttEstOperation.estFormID           = ipbf-ttEstForm.estFormID
            opbf-ttEstoperation.estHeaderID         = ipbf-ttEstForm.estHeaderID
            opbf-ttEstOperation.iFormNo             = ipbf-est-op.s-num
            opbf-ttEstOperation.iBlankNo            = ipbf-est-op.b-num
            opbf-ttEstOperation.estBlankID          = fGetEstBlankID(opbf-ttEstOperation.estBlankID, opbf-ttEstOperation.estFormID, opbf-ttEstOperation.iBlankNo)
            opbf-ttEstOperation.cOperationID        = ipbf-est-op.m-code
            opbf-ttEstOperation.iPass               = ipbf-est-op.op-pass
            opbf-ttEstOperation.iSequence           = ipbf-est-op.d-seq
            opbf-ttEstOperation.iNumOutDivisor      = ipbf-est-op.n_out_div
                       
            opbf-ttEstOperation.dQtyWasteSetup      = ipbf-est-op.op-waste
            opbf-ttEstOperation.dHoursSetup         = ipbf-est-op.op-mr
            opbf-ttEstOperation.dSpeed              = ipbf-est-op.op-speed
            opbf-ttEstOperation.lIsLocked           = ipbf-est-op.isLocked
            opbf-ttEstOperation.dCrewSizeSetup      = ipbf-est-op.op-crew[1]
            opbf-ttEstOperation.dCrewSizeRun        = ipbf-est-op.op-crew[2]
            
            opbf-ttEstOperation.lSpeedInLF          = bf-mach.therm
            opbf-ttEstOperation.cOperationName      = bf-mach.m-dscr
            opbf-ttEstOperation.cOperationFeedType  = bf-mach.p-type
            opbf-ttEstOperation.cDepartmentID       = bf-mach.dept[1]
            opbf-ttEstOperation.cAlt1DepartmentID   = bf-mach.dept[2]
            opbf-ttEstOperation.cAlt2DepartmentID   = bf-mach.dept[3]
            opbf-ttEstOperation.cAlt3DepartmentID   = bf-mach.dept[4]
            
            opbf-ttEstOperation.dCostPerHourFOSetup = bf-mach.mr-fixoh
            opbf-ttEstOperation.dCostPerHourFORun   = bf-mach.run-fixoh
            opbf-ttEstOperation.dCostPerHourVOSetup = bf-mach.mr-varoh
            opbf-ttEstOperation.dCostPerHourVORun   = bf-mach.run-varoh
            
            .
    END.
    
END PROCEDURE.
PROCEDURE pBuildCostDetailForMaterial PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an operation buffer, build all costDetail records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstMaterial FOR ttEstMaterial.

    RUN pAddCostDetailForMaterial(BUFFER ipbf-ttEstMaterial, "CostCategory22","Material Cost Setup",
        ipbf-ttEstMaterial.dCostSetup,0).
   
        
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
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory33","Prep Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory36","Prep Labor - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatI" THEN  
            DO:  /*PrepMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory40","Prep Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory43","Prep Material - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "PLabM" THEN 
            DO:  /*PrepLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory33","Prep Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory38","Prep Labor - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatM" THEN  
            DO:  /*PrepMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory40","Prep Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory45","Prep Material - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).   
            END.                 
        WHEN "PLabS" OR 
        WHEN "PLabO" THEN 
            DO:  /*PrepLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory35","Prep Labor - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory39","Prep Labor - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "PMatS" OR 
        WHEN "PMatO" THEN  
            DO:  /*PrepMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory42","Prep Material - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory46","Prep Material - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "MLabI" THEN 
            DO:  /*MiscLabIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory47","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory50","Misc Labor - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatI" THEN  
            DO:  /*MiscMatIncluded*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory54","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory57","Misc Material - Profit - COGS",
                    ipbf-ttEstMisc.dProfit,0).                    
            END. 
        WHEN "MLabM" THEN 
            DO:  /*MiscLabMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory47","Misc Labor - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory52","Misc Labor - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatM" THEN  
            DO:  /*MiscMatMarginSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory54","Misc Material - Cost - COGS",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory59","Misc Material - Profit - Price",
                    ipbf-ttEstMisc.dProfit,0).   
            END.                 
        WHEN "MLabS" OR 
        WHEN "MLabO" THEN 
            DO:  /*MiscLabSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory49","Misc Labor - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory53","Misc Labor - Profit - Separate",
                    ipbf-ttEstMisc.dProfit,0).                    
            END.
        WHEN "MMatS" OR 
        WHEN "MMatO" THEN  
            DO:  /*MiscMatSeparate*/
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory56","Misc Material - Cost - Separate",
                    ipbf-ttEstMisc.dCostTotalBeforeProfit,0).
                RUN pAddCostDetailForMisc(BUFFER ipbf-ttEstMisc, "CostCategory60","Misc Material - Profit - Separate",
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
            RUN pBuildCostDetailForMaterial(BUFFER ttEstMaterial).                  
                    
        END. /*Each ttEstMaterial for estHeader*/
        FOR EACH ttEstMisc NO-LOCK 
            WHERE ttEstMisc.estHeaderID EQ ttEstHeader.estHeaderID:
            RUN pBuildCostDetailForMisc(BUFFER ttEstMisc).                  
                    
        END. /*Each ttEstMaterial for estHeader*/
            
    END. /*Each ttEstHeader*/    

END PROCEDURE.

PROCEDURE pBuildItems PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given company and estimate, build the items
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.

    DEFINE           BUFFER bf-ttEstItem     FOR ttEstItem.
    /*Build Items*/
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ ipbf-ttEstHeader.company
        AND eb.est-no EQ ipbf-ttEstHeader.cEstNo: 
        FIND FIRST bf-ttEstItem NO-LOCK
            WHERE bf-ttEstItem.estHeaderID EQ ipbf-ttEstHeader.estHeaderID
            AND bf-ttEstItem.cCustomerPart EQ eb.part-no
            NO-ERROR.
        IF NOT AVAILABLE bf-ttEstItem THEN 
        DO:
            RUN pAddEstItem(BUFFER eb, BUFFER ipbf-ttEstHeader, BUFFER bf-ttEstItem).
        END. /*Create ttEstitem*/
               
    END. /*Build EstItems*/

END PROCEDURE.

PROCEDURE pBuildMiscNonPrep PRIVATE:
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

PROCEDURE pBuildMiscPrep PRIVATE:
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

PROCEDURE pBuildOperations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: given a ttEstHeader, build all ttEstOperations
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader  FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm    FOR ttEstForm.
    
    DEFINE           BUFFER bf-ttEstOperation FOR ttEstOperation.
    DEFINE VARIABLE dQtyInSheetsRunningTotal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityTarget          AS DECIMAL NO-UNDO.
    
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
                THEN dQuantityTarget = est-op.qty.
            IF est-op.qty GE ipbf-ttEstHeader.dQtyMaster THEN LEAVE.
        END.
    END.
    FOR EACH est-op NO-LOCK 
        WHERE est-op.company EQ ipbf-ttEstHeader.company
        AND est-op.est-no EQ ipbf-ttEstHeader.cEstNo
        AND est-op.line LT 500
        AND est-op.qty EQ dQuantityTarget
        BY est-op.line DESCENDING:
    
        RUN pAddEstOperation(BUFFER est-op, BUFFER ipbf-ttEstForm, BUFFER bf-ttEstOperation).
                
        ASSIGN             
            bf-ttEstOperation.
        dQtyInSheets
        bf-ttEstOperation.dHoursRun = 1                  
            .
       
        RUN pCalcEstOperation(BUFFER bf-ttEstOperation).
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
        
        RUN pBuildItems(BUFFER ttEstHeader).
       
        /*Process Forms and Blanks*/
        FOR EACH ef NO-LOCK 
            WHERE ef.company EQ est.company
            AND ef.est-no EQ est.est-no:
            
            RUN pAddEstForm(BUFFER ef, BUFFER ttEstHeader, BUFFER bf-ttEstForm).

            FOR EACH eb NO-LOCK 
                OF ef:
                
                RUN pAddEstBlank(BUFFER eb, BUFFER bf-ttEstForm, BUFFER bf-ttEstBlank).
                RUN pAddEstMaterial(BUFFER ttEstHeader, BUFFER bf-ttEstForm, eb.adhesive, bf-ttEstBlank.iBlankNo, BUFFER bf-ttEstMaterial).
                    
                /*Refactor - pull from vend-cost table*/
                ASSIGN 
                    bf-ttEstMaterial.dCostSetup             = 3
                    bf-ttEstMaterial.dCostPerUOM            = .90
                    bf-ttEstMaterial.cCostUOM               = "LB"
                    bf-ttEstMaterial.dQtyRequiredNoWaste    = 82.02
            
                    bf-ttEstMaterial.dQtyRequiredWasteSetup = 0
                    bf-ttEstMaterial.dQtyRequiredWasteRun   = 0
                    bf-ttEstMaterial.dQtyRequiredTotal      = bf-ttEstMaterial.dQtyRequiredNoWaste + bf-ttEstMaterial.dQtyRequiredWasteSetup + bf-ttEstMaterial.dQtyRequiredWasteRun
                    bf-ttEstMaterial.dCostTotalNoWaste      = bf-ttEstMaterial.dQtyRequiredNoWaste * bf-ttEstMaterial.dCostPerUOM
                    bf-ttEstMaterial.dCostTotalWasteSetup   = bf-ttEstMaterial.dQtyRequiredWasteSetup * bf-ttEstMaterial.dCostPerUOM + bf-ttEstMaterial.dCostSetup
                    bf-ttEstMaterial.dCostTotalWasteRun     = bf-ttEstMaterial.dQtyRequiredWasteRun * bf-ttEstMaterial.dCostPerUOM
                    bf-ttEstMaterial.dCostTotal             = bf-ttEstMaterial.dCostTotalNoWaste + bf-ttEstMaterial.dCostTotalWasteSetup + bf-ttEstMaterial.dCostTotalWasteRun
                    .
                
            END. /*Each eb of ef*/
            
            /*Refactor - Need True calculations - bf-ttEstBlank will only be last blank*/
            ASSIGN 
                bf-ttEstForm.iNumOut                      = bf-ttEstBlank.iNumOut * bf-ttEstForm.iNumOutNet
                bf-ttEstForm.dGrossQtyRequiredNoWaste     = ttEstHeader.dQtyMaster / bf-ttEstForm.iNumOut
                bf-ttEstForm.dGrossQtyRequiredTotal       = bf-ttEstForm.dGrossQtyRequiredNoWaste + bf-ttEstForm.dGrossQtyRequiredWasteSetup + bf-ttEstForm.dGrossQtyRequiredWasteRun
                bf-ttEstForm.dGrossQtyRequiredTotalArea   = bf-ttEstForm.dGrossQtyRequiredTotal * bf-ttEstForm.dGrossArea / 1000
                bf-ttEstForm.dGrossQtyRequiredTotalWeight = bf-ttEstForm.dGrossQtyRequiredTotalArea * bf-ttEstForm.dBasisWeightInLbsPerMSF
                .
            
            RUN pAddEstMaterial(BUFFER ttEstHeader, BUFFER bf-ttEstForm, ef.board, 0, BUFFER bf-ttEstMaterial).
            ASSIGN 
                bf-ttEstMaterial.lIsPrimarySubstrate    = YES
                bf-ttEstMaterial.lAddToWeightFG         = YES
                bf-ttEstMaterial.lAddToWeightTare       = NO
                    
                /*Refactor - pull from vend-cost table*/
                bf-ttEstMaterial.dCostSetup             = 484.07
                bf-ttEstMaterial.dCostPerUOM            = 1000
                bf-ttEstMaterial.cCostUOM               = "TON"
                    
                bf-ttEstMaterial.dQtyRequiredNoWaste    = bf-ttEstForm.dGrossQtyRequiredNoWaste
                bf-ttEstMaterial.dQtyRequiredWasteSetup = bf-ttEstForm.dGrossQtyRequiredWasteSetup
                bf-ttEstMaterial.dQtyRequiredWasteRun   = bf-ttEstForm.dGrossQtyRequiredWasteRun
                bf-ttEstMaterial.dQtyRequiredTotal      = bf-ttEstForm.dGrossQtyRequiredTotal
                bf-ttEstMaterial.dCostTotalNoWaste      = bf-ttEstMaterial.dQtyRequiredNoWaste * bf-ttEstMaterial.dCostPerUOM
                bf-ttEstMaterial.dCostTotalWasteSetup   = bf-ttEstMaterial.dQtyRequiredWasteSetup * bf-ttEstMaterial.dCostPerUOM + bf-ttEstMaterial.dCostSetup
                bf-ttEstMaterial.dCostTotalWasteRun     = bf-ttEstMaterial.dQtyRequiredWasteRun * bf-ttEstMaterial.dCostPerUOM
                bf-ttEstMaterial.dCostTotal             = bf-ttEstMaterial.dCostTotalNoWaste + bf-ttEstMaterial.dCostTotalWasteSetup + bf-ttEstMaterial.dCostTotalWasteRu   
                .
            /*Build Misc Charges - prep*/
            RUN pBuildMiscPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.dQtyMaster).
            
            /*Build Misc Charges - non-prep*/
            RUN pBuildMiscNonPrep(BUFFER ef, BUFFER bf-ttEstForm, ttEstHeader.dQtyMaster).          
            
            RUN pBuildOperations(BUFFER ttEstHeader, BUFFER bf-ttEstForm).
        END.  /*Each ef of est*/  
             
    END. /*each ttEstHeader*/
        
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
