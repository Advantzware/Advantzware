
/*------------------------------------------------------------------------
    File        : CostProcs.p
    Purpose     : 

    Syntax      :

    Description : Stores all Cost Retrieval and Calculation Procedures

    Author(s)   : BV
    Created     : Mon Jan 21 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE gdMultiplierForSquareFoot AS DECIMAL NO-UNDO.
DEFINE VARIABLE glIncludeFreight AS LOGICAL NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fConvert RETURNS DECIMAL 
    (ipcFromUOM AS CHARACTER,
    ipcToUOM AS CHARACTER,
    ipdBasisWeightInPoundsPerSqInch AS DECIMAL,
    ipdLengthInInches AS DECIMAL,
    ipdWidthInInches AS DECIMAL,
    ipdDepthInInches AS DECIMAL,
    ipdValueToConvert AS DECIMAL) FORWARD.

FUNCTION fConvertCurrency RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL,
    ipcCompany AS CHARACTER,
    ipcCurrCode AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
    
    

/* **********************  Internal Procedures  *********************** */

PROCEDURE GetCostForFGItemHist:
/*------------------------------------------------------------------------------
 Purpose: Returns costs for a given FGHist rowid 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNo                AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2               AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoNo                 AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPoLine               AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag                  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRitaCode             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal      AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFreight    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostSource           AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSourceFound          AS LOGICAL   NO-UNDO.
         
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iPoLine AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCostPerUOMTotalDef AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDLDef AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFODef AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVODef AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDMDef AS DECIMAL NO-UNDO.
    DEFINE VARIABLE cCostUOMDef      AS CHARACTER NO-UNDO.
            
    RUN GetCostForFGItem(ipcCompany, ipcFGItemID,
        OUTPUT dCostPerUOMTotalDef, OUTPUT dCostPerUOMDLDef, OUTPUT dCostPerUOMFODef, OUTPUT dCostPerUOMVODef, OUTPUT dCostPerUOMDMDef, OUTPUT cCostUOMDef, OUTPUT lFound).
    IF NOT lFound THEN DO:
        opcCostSource = "Invalid Item: " + ipcFGItemID.
        RETURN.
    END. 
    IF ipcJobNo NE "" THEN DO:
        RUN GetCostForJob(ipcCompany, ipcFGItemID, ipcJobNo, ipiJobNo2, 
            OUTPUT opdCostPerUOMTotal, OUTPUT opdCostPerUOMDL, OUTPUT opdCostPerUOMFO, OUTPUT opdCostPerUOMVO, OUTPUT opdCostPerUOMDM, OUTPUT opcCostUOM, OUTPUT oplSourceFound).
        opcCostSource = "Job: " + ipcJobNo + "-" + STRING(ipiJobNo2,"99").
        IF NOT oplSourceFound OR opdCostPerUOMTotal EQ 0 THEN 
            ASSIGN 
                opcCostSource = opcCostSource + " not found or 0. "
                oplSourceFound = NO.
    END.
    IF opdCostPerUOMTotal EQ 0 AND ipiPONo GT 0 THEN DO:
        iPoLine = MAXIMUM(ipiPoLine, 1).
        RUN GetCostForPOLine(ipcCompany, ipiPoNo, iPoLine, ipcFGItemID,
            OUTPUT opdCostPerUOMTotal, OUTPUT opcCostUOM, OUTPUT opdCostPerUOMFreight, OUTPUT oplSourceFound).
        ASSIGN 
            opdCostPerUOMDM = opdCostPerUOMTotal            
            opcCostSource = "PO: " + STRING(ipiPoNo,"999999") + "-" + STRING(iPoLine)
            .
        IF NOT oplSourceFound OR opdCostPerUOMTotal EQ 0 THEN 
            ASSIGN 
                opcCostSource = opcCostSource + " not found or 0. "
                oplSourceFound = NO.
    END.
    IF opdCostPerUOMTotal EQ 0 AND ipcTag NE "" THEN DO:
        RUN GetCostForReceipt(ipcCompany, ipcFGItemID, ipcTag, ipcJobNo, ipiJobNo2, 
            OUTPUT opdCostPerUOMTotal, OUTPUT opdCostPerUOMDL, OUTPUT opdCostPerUOMFO, OUTPUT opdCostPerUOMVO, OUTPUT opdCostPerUOMDM, OUTPUT opcCostUOM, OUTPUT oplSourceFound).
         opcCostSource = "Receipt for tag: " + ipcTag.
         IF NOT lFound OR opdCostPerUOMTotal EQ 0 THEN 
            ASSIGN 
                opcCostSource = opcCostSource + " not found or 0. "
                oplSourceFound = NO.
    END.
    IF opdCostPerUOMTotal EQ 0 THEN DO:
        ASSIGN 
            opcCostSource = opcCostSource + "Item Fallback: " + ipcFGItemID
            opdCostPerUOMTotal = dCostPerUOMTotalDef
            opdCostPerUOMDL = dCostPerUOMDLDef
            opdCostPerUOMFO = dCostPerUOMFODef
            opdCostPerUOMVO = dCostPerUOMVODef
            opdCostPerUOMDM = dCostPerUOMDMDef
            opcCostUOM = cCostUOMDef
            .
    END.
    IF opcCostUOM NE cCostUOMDef THEN DO: 
        opcCostUOM = cCostUOMDef.
        opdCostPerUOMTotal = fConvert(opcCostUOM, cCostUOMDef, 0, 0, 0, 0, opdCostPerUOMTotal).
        opdCostPerUOMDL = fConvert(opcCostUOM, cCostUOMDef, 0, 0, 0, 0, opdCostPerUOMDL).
        opdCostPerUOMFO = fConvert(opcCostUOM, cCostUOMDef, 0, 0, 0, 0, opdCostPerUOMFO).
        opdCostPerUOMVO = fConvert(opcCostUOM, cCostUOMDef, 0, 0, 0, 0, opdCostPerUOMVO).
        opdCostPerUOMDM = fConvert(opcCostUOM, cCostUOMDef, 0, 0, 0, 0, opdCostPerUOMDM).
    END.
    
END PROCEDURE.

PROCEDURE GetCostForPOLine:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Per UOM Cost based on total cost of the po.
     Includes Setup and Discounts in EffectiveCost.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMExFreight AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFreight AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-item    FOR ITEM.
    DEFINE BUFFER bf-vend    FOR vend.
    
    DEFINE VARIABLE lIncludeFreight AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dLengthInInches AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidthInInches  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDepthInInches  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBasisWeight    AS DECIMAL NO-UNDO.

    RUN pSetGlobalSettings(ipcCompany).
    RUN pSetBuffersPO(ipcCompany, ipiPONumber, ipiPOLine, ipcItemID,
        BUFFER bf-po-ord, BUFFER bf-po-ordl, 
        BUFFER bf-itemfg, BUFFER bf-item, BUFFER bf-vend).

    IF AVAILABLE bf-itemfg THEN 
    DO: 
        ASSIGN  
            dBasisWeight    = 0
            opcCostUOM      = bf-itemfg.prod-uom
            dLengthInInches = bf-itemfg.t-len
            dWidthInInches  = bf-itemfg.t-wid
            dDepthInInches  = bf-itemfg.t-dep
            .
    END.
    ELSE IF AVAILABLE bf-item THEN 
        DO:
            ASSIGN  
                dBasisWeight    = bf-item.basis-w
                opcCostUOM      = bf-item.cons-uom
                dLengthInInches = bf-item.s-len
                dWidthInInches  = bf-item.s-wid
                dDepthInInches  = bf-item.s-dep
                .
            
        END.
    IF AVAILABLE bf-po-ordl THEN DO:
        ASSIGN 
            oplFound = YES
            dLengthInInches = bf-po-ordl.s-len
            dWidthInInches  = bf-po-ordl.s-wid
            dDepthInInches  = bf-po-ordl.s-dep
            .
            
        RUN pGetCostForPOLineInUOM(BUFFER bf-po-ord, BUFFER bf-po-ordl, opcCostUOM,
            dLengthInInches, dWidthInInches, dDepthInInches, dBasisWeight,
            OUTPUT opdCostPerUOMExFreight, OUTPUT opdCostPerUOMFreight).
    
        /*Apply Vendor Currency Conversion*/
        IF AVAILABLE bf-vend THEN DO:
            opdCostPerUOMExFreight = fConvertCurrency(opdCostPerUOMExFreight, bf-vend.company, bf-vend.curr-code).
            opdCostPerUOMFreight = fConvertCurrency(opdCostPerUOMFreight, bf-vend.company, bf-vend.curr-code).
        END.
    END.
END PROCEDURE.

PROCEDURE pGetCostForPOLineInUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given inputs, calculates an effective per UOM cost for a PO Line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE INPUT PARAMETER ipcTargetUOM AS CHARACTER.
    DEFINE INPUT PARAMETER ipdLengthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdWidthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdDepthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdBasisWeightInPoundsPerSqInch AS DECIMAL.
    DEFINE OUTPUT PARAMETER opdCostPerUOMExFreight AS DECIMAL.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFreight AS DECIMAL.

    DEFINE VARIABLE dCostTotal          AS DECIMAL.
    DEFINE VARIABLE dOrderQty           AS DECIMAL.
    DEFINE VARIABLE cOrderQtyUOM        AS CHARACTER.
    DEFINE VARIABLE dCostPerUOM         AS DECIMAL.
    DEFINE VARIABLE cCostUOM            AS CHARACTER. 
    DEFINE VARIABLE dCostInOrderQtyUOM  AS DECIMAL.
    DEFINE VARIABLE dCostSetup          AS DECIMAL.
    DEFINE VARIABLE dDiscountPercentage AS DECIMAL.
    DEFINE VARIABLE dFreightPortion     AS DECIMAL.


    IF AVAILABLE ipbf-po-ordl THEN 
        ASSIGN
            dOrderQty           = ipbf-po-ordl.ord-qty
            cOrderQtyUOM        = ipbf-po-ordl.pr-qty-uom 
            dCostPerUOM         = ipbf-po-ordl.cost
            cCostUOM            = ipbf-po-ordl.pr-uom
            dCostSetup          = ipbf-po-ordl.setup
            dDiscountPercentage = ipbf-po-ordl.disc
            .
        
    IF dOrderQty EQ 0 THEN RETURN.
    dCostInOrderQtyUOM = fConvert(cCostUOM, cOrderQtyUOM,
        ipdBasisWeightInPoundsPerSqInch,
        ipdLengthInInches, ipdWidthInInches, ipdDepthInInches,
        dCostPerUOM).

    /*Apply discount to Per UOM Cost Only*/
    dCostInOrderQtyUOM = dCostInOrderQtyUOM * (1 - dDiscountPercentage / 100).
                                  
    /*Calculate Total Cost with OrderQty Times Unit Cost and add setup*/
    dCostTotal = dCostInOrderQtyUOM * dOrderQty + dCostSetup.

    /*Calculate Freight Costs*/
    RUN pGetFreightPortion(BUFFER ipbf-po-ord, BUFFER ipbf-po-ordl, OUTPUT dFreightPortion).
        
    /*Convert Cost from Order UOM into Target UOM*/
    opdCostPerUOMExFreight = fConvert(cOrderQtyUOM, ipcTargetUOM,
        ipdBasisWeightInPoundsPerSqInch,
        ipdLengthInInches, ipdWidthInInches, ipdDepthInInches,
        dCostTotal / dOrderQty).
    
    opdCostPerUOMFreight = fConvert(cOrderQtyUOM, ipcTargetUOM,
        ipdBasisWeightInPoundsPerSqInch,
        ipdLengthInInches, ipdWidthInInches, ipdDepthInInches,
        dFreightPortion / dOrderQty).
    
END PROCEDURE.

PROCEDURE pGetFreightPortion PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a PO Line, determine the proportionate freight to allocate 
     for that line vs. the total freight PO.  Must convert all lines to EA quantity
     and use the Weight/100 measure to determine freight [REFACTOR]
     Notes: Should replace getfrtcs.p (Private since this should only be called when 
     retrieving the cost)
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE OUTPUT PARAMETER opdCostFreightPortion  AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE VARIABLE dQtyInEach          AS DECIMAL.
    DEFINE VARIABLE dWeightTargetPOLine AS DECIMAL.
    DEFINE VARIABLE dWeightTotal        AS DECIMAL.
    DEFINE VARIABLE lIsEachUOM          AS LOGICAL.
    DEFINE VARIABLE cEachUOMListFG      AS CHARACTER.
    DEFINE VARIABLE cEachUOMListRM      AS CHARACTER.

    IF AVAILABLE ipbf-po-ordl THEN 
    DO:

        IF AVAILABLE ipbf-po-ord AND ipbf-po-ord.t-freight GT 0 THEN 
        DO:
            FOR EACH bf-po-ordl WHERE
                bf-po-ordl.company EQ ipbf-po-ord.company AND
                bf-po-ordl.po-no EQ ipbf-po-ord.po-no NO-LOCK:
                IF NOT bf-po-ordl.item-type THEN 
                DO:
                    
                    FIND FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ bf-po-ordl.company
                        AND itemfg.i-no    EQ bf-po-ordl.i-no
                        NO-ERROR.

                    dQtyInEach = bf-po-ordl.ord-qty.
                    
                    /*Get List of UOMs equivalent to EA and convert to EA if not on list*/
                    RUN sys/ref/uom-fg.p (?, OUTPUT cEachUOMListFG).
                    IF LOOKUP(bf-po-ordl.pr-qty-uom,cEachUOMListFG) EQ 0 THEN
                        dQtyInEach = fConvert(bf-po-ordl.pr-qty-uom, "EA",
                            0, bf-po-ordl.s-len, bf-po-ordl.s-wid, bf-po-ordl.s-dep,
                            dQtyInEach).
                                  
                    dWeightTotal = dWeightTotal + (dQtyInEach / 100 * itemfg.weight-100).

                    IF ROWID(bf-po-ordl) EQ ROWID(ipbf-po-ordl) THEN
                        dWeightTargetPOLine = dQtyInEach / 100 * itemfg.weight-100.
                END.
                ELSE 
                DO:

                    FIND FIRST ITEM NO-LOCK
                        WHERE item.company EQ bf-po-ordl.company
                        AND item.i-no    EQ bf-po-ordl.i-no
                        NO-ERROR.
                    
                    dQtyInEach = bf-po-ordl.ord-qty.
                    
                    /*Get List of UOMs equivalent to EA and convert to EA if not on list*/
                    RUN sys/ref/uom-rm.p  (item.mat-type, OUTPUT cEachUOMListRM).
                    IF LOOKUP(bf-po-ordl.pr-qty-uom,cEachUOMListRM) EQ 0 THEN
                        dQtyInEach = fConvert(bf-po-ordl.pr-qty-uom, "EA",
                            ITEM.basis-w, bf-po-ordl.s-len, bf-po-ordl.s-wid, bf-po-ordl.s-dep,
                            dQtyInEach).
      
                    dWeightTotal = dWeightTotal + (dQtyInEach / 100 * item.weight-100).

                    IF ROWID(bf-po-ordl) EQ ROWID(ipbf-po-ordl) THEN
                        dWeightTargetPOLine = dQtyInEach / 100 * item.weight-100.
                END.
            END.

            IF dWeightTargetPOLine NE 0 AND dWeightTargetPOLine NE ? AND
                dWeightTotal NE 0 AND dWeightTotal NE ? THEN
                opdCostFreightPortion = ipbf-po-ord.t-freight * (dWeightTargetPOLine / dWeightTotal).
        END.
    END.


END PROCEDURE.

PROCEDURE pSetBuffersPO PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Sets Buffers for FG Item and Customers
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER opbf-po-ordl FOR po-ordl.
    DEFINE PARAMETER BUFFER opbf-itemfg  FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-item    FOR ITEM.
    DEFINE PARAMETER BUFFER opbf-vend    FOR vend.
    
    FIND FIRST opbf-po-ord NO-LOCK 
        WHERE opbf-po-ord.company EQ ipcCompany
        AND opbf-po-ord.po-no EQ ipiPONumber
        NO-ERROR.
    IF AVAILABLE opbf-po-ord THEN 
        FIND FIRST opbf-vend NO-LOCK 
            WHERE opbf-vend.company EQ opbf-po-ord.company
            AND opbf-vend.vend-no EQ opbf-po-ord.vend-no
            NO-ERROR. 
    IF AVAILABLE opbf-po-ord THEN 
        FIND FIRST opbf-po-ordl NO-LOCK 
            WHERE opbf-po-ordl.company EQ opbf-po-ord.company
            AND opbf-po-ordl.po-no EQ opbf-po-ord.po-no
            AND opbf-po-ordl.line EQ ipiPOLine
            AND opbf-po-ordl.i-no EQ ipcItemID
            NO-ERROR.
    /*Po-ordl.item-type = NO-> FG, YES->RM*/
    IF AVAILABLE opbf-po-ordl AND opbf-po-ordl.item-type THEN 
        FIND FIRST opbf-item NO-LOCK 
            WHERE opbf-item.company EQ opbf-po-ordl.company
            AND opbf-item.i-no EQ opbf-po-ordl.i-no
            NO-ERROR.
    ELSE 
        FIND FIRST opbf-itemfg NO-LOCK 
            WHERE opbf-itemfg.company EQ opbf-po-ordl.company
            AND opbf-itemfg.i-no EQ opbf-po-ordl.i-no
            NO-ERROR.
    
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Sets the NK1 setting global variables that are pertinent to th
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (ipcCompany, "FGPOFRT", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
glIncludeFreight = lFound AND cReturn EQ "YES".

RUN sys/ref/nk1look.p (ipcCompany, "MSFCALC", "C", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
gdMultiplierForSquareFoot = IF lFound AND cReturn EQ "Corrware" THEN .007 ELSE 1 / 144.  

END PROCEDURE.

PROCEDURE pCalculateCostPerUOMForBOL PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given criteria to find BOL lines, calculate the total cost of goods
            shipped, and the total quantity shipped
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBNo               AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiOrderNo           AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCustomerPONo      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSource           AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dQtyShippedInM          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQtyShippedLineInM      AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerUOMTotalLine    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDLLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFOLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVOLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDMLine       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOMLine            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostPerUOMTotalLineDef AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDLLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMFOLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMVOLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostPerUOMDMLineDef    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOMLineDef         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostTotal              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostDL                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostFO                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostVO                 AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostDM                 AS DECIMAL   NO-UNDO.    
    
    DEFINE VARIABLE lFound                  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBOLLine                AS CHARACTER NO-UNDO.

    FOR EACH oe-boll NO-LOCK  
        WHERE oe-boll.company EQ ipcCompany
        AND oe-boll.b-no    EQ ipiBNo
        AND oe-boll.ord-no  EQ ipiOrderNo
        AND oe-boll.i-no    EQ ipcFGItemID
        AND oe-boll.po-no   EQ ipcCustomerPONo
        AND oe-boll.qty     NE 0
        USE-INDEX b-no :
            
        ASSIGN 
            dCostPerUOMTotalLine = 0
            dCostPerUOMDLLine    = 0
            dCostPerUOMFOLine    = 0
            dCostPerUOMVOLine    = 0
            dCostPerUOMDMLine    = 0
            cBOLLine             = " for BOL Line:" + STRING(oe-boll.bol-no,">>>>>>>") + "-" + STRING(oe-boll.bol-line,">>>")
            dQtyShippedLineInM   = oe-boll.qty / 1000
            dQtyShippedInM       = dQtyShippedInM + dQtyShippedLineInM
            cCostUOMLine         = ""
            .
        RUN GetCostForFGItem(oe-boll.company,
            oe-boll.i-no,
            OUTPUT dCostPerUOMTotalLineDef,
            OUTPUT dCostPerUOMDLLineDef,
            OUTPUT dCostPerUOMFOLineDef,
            OUTPUT dCostPerUOMVOLineDef,
            OUTPUT dCostPerUOMDMLineDef,
            OUTPUT cCostUOMLineDef,
            OUTPUT lFound).
        IF cCostUOMLineDef EQ "" THEN cCostUOMLineDef = "M".
        /*Find matching bin first*/
        RUN GetCostForBin(oe-boll.company, 
            oe-boll.i-no,
            oe-boll.tag,
            oe-boll.loc,
            oe-boll.loc-bin,
            oe-boll.job-no,
            oe-boll.job-no2,
            OUTPUT dCostPerUOMTotalLine,
            OUTPUT dCostPerUOMDLLine,
            OUTPUT dCostPerUOMFOLine,
            OUTPUT dCostPerUOMVOLine,
            OUTPUT dCostPerUOMDMLine,
            OUTPUT cCostUOMLine,
            OUTPUT lFound).
        IF lFound THEN 
            opcSource = opcSource + "FGBin" + cBOLLine + "," .
        ELSE 
        DO:
            /*Find matching receipt*/
            RUN GetCostForReceipt(oe-boll.company,
                oe-boll.i-no,
                oe-boll.tag,
                oe-boll.job-no,
                oe-boll.job-no2,
                OUTPUT dCostPerUOMTotalLine,
                OUTPUT dCostPerUOMDLLine,
                OUTPUT dCostPerUOMFOLine,
                OUTPUT dCostPerUOMVOLine,
                OUTPUT dCostPerUOMDMLine,
                OUTPUT cCostUOMLine,
                OUTPUT lFound).
            IF lFound THEN 
                opcSource = opcSource + "Rcpt" + cBOLLine + "," .
            ELSE 
            DO:
                /*Get the cost For Job*/
                RUN GetCostForJob(oe-boll.company,
                    oe-boll.i-no,
                    oe-boll.job-no,
                    oe-boll.job-no2,
                    OUTPUT dCostPerUOMTotalLine,
                    OUTPUT dCostPerUOMDLLine,
                    OUTPUT dCostPerUOMFOLine,
                    OUTPUT dCostPerUOMVOLine,
                    OUTPUT dCostPerUOMDMLine,
                    OUTPUT cCostUOMLine,
                    OUTPUT lFound).
                IF lFound THEN 
                    opcSource = opcSource + "Job" + cBOLLine + ",".
            END.    
        END.                
        IF NOT lFound THEN 
        DO:
            /*if no matches found, use standard costs For item*/
            ASSIGN 
                dCostPerUOMTotalLine = dCostPerUOMTotalLineDef
                dCostPerUOMDLLine    = dCostPerUOMDLLineDef
                dCostPerUOMFOLine    = dCostPerUOMFOLineDef
                dCostPerUOMVOLine    = dCostPerUOMVOLineDef
                dCostPerUOMDMLine    = dCostPerUOMDMLineDef
                cCostUOMLine         = cCostUOMLineDef
                opcSource            = opcSource + "Item" + cBOLLine + ",".
        END.
        IF cCostUOMLine EQ "" THEN cCostUOMLine = cCostUOMLineDef.
        IF cCostUOMLine NE "M" THEN /*convert all to per M*/
        DO:
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMTotalLine, OUTPUT dCostPerUOMTotalLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMDLLine, OUTPUT dCostPerUOMDLLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMFOLine, OUTPUT dCostPerUOMFOLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMVOLine, OUTPUT dCostPerUOMVOLine).
            RUN pConvertCostToM(cCostUOMLine, dCostPerUOMDMLine, OUTPUT dCostPerUOMDMLine).
        END.         
        
        /*sum the total costs in order to calculate average cost per M*/
        ASSIGN 
            dCostTotal = dCostTotal + dCostPerUOMTotalLine * dQtyShippedLineInM  
            dCostDL    = dCostDL + dCostPerUOMDLLine * dQtyShippedLineInM
            dCostFO    = dCostFO + dCostPerUOMFOLine * dQtyShippedLineInM
            dCostVO    = dCostVO + dCostPerUOMVOLine * dQtyShippedLineInM
            dCostDM    = dCostDM + dCostPerUOMDMLine * dQtyShippedLineInM 
            .
            
    END.  /*Each oe-boll*/
    
    /*Calculate Average Costs for the total BOL qty*/
    ASSIGN 
        opcCostUOM         = "M"
        opdCostPerUOMDL    = dCostDL / dQtyShippedInM
        opdCostPerUOMFO    = dCostFO / dQtyShippedInM
        opdCostPerUOMVO    = dCostVO / dQtyShippedInM
        opdCostPerUOMDM    = dCostDM / dQtyShippedInM
        opdCostPerUOMTotal = dCostTotal / dQtyShippedInM
        .
     
    
END PROCEDURE.

PROCEDURE pConvertCostToM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Converts cost to UOM
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    
    opdCost = fConvert(ipcUOM, "M", 0, 0, 0, 0, ipdCost).

END PROCEDURE.

PROCEDURE GetCostForBin:
    /*------------------------------------------------------------------------------
     Purpose: Given bin inputs, finds a mathing bin and returns costs
     Notes: Could add additional "loose" matches like not requiring loc and loc-bin
     or just match on job
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhs AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcBin AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
            

    FIND FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ ipcCompany
        AND fg-bin.i-no EQ ipcFGItemID
        AND fg-bin.tag EQ ipcTag
        AND fg-bin.loc EQ ipcWhs
        AND fg-bin.loc-bin EQ ipcBin
        AND fg-bin.job-no EQ ipcJobNo
        AND fg-bin.job-no2 EQ ipiJobNo2
        NO-ERROR.
    IF AVAILABLE fg-bin AND fg-bin.std-tot-cost NE 0 THEN 
        ASSIGN 
            oplFound           = YES
            opdCostPerUOMTotal = fg-bin.std-tot-cost
            opdCostPerUOMDL    = fg-bin.std-lab-cost
            opdCostPerUOMFO    = fg-bin.std-fix-cost
            opdCostPerUOMVO    = fg-bin.std-var-cost
            opdCostPerUOMDM    = fg-bin.std-mat-cost
            opcCostUOM         = fg-bin.pur-uom
            .

END PROCEDURE.

PROCEDURE GetCostForFGItem:
    /*------------------------------------------------------------------------------
         Purpose: Given bin inputs, finds a mathing bin and returns costs
         Notes: 
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
    
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no EQ ipcFGItemID
        NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        ASSIGN 
            oplFound   = YES
            opcCostUOM = itemfg.prod-uom
            .
        IF itemfg.std-tot-cost NE 0 THEN 
            ASSIGN 
                opdCostPerUOMTotal = itemfg.std-tot-cost
                opdCostPerUOMDL    = itemfg.std-lab-cost
                opdCostPerUOMFO    = itemfg.std-fix-cost
                opdCostPerUOMVO    = itemfg.std-var-cost
                opdCostPerUOMDM    = itemfg.std-mat-cost
                .
        ELSE 
        DO:
            FIND FIRST fg-ctrl NO-LOCK 
                WHERE fg-ctrl.company EQ itemfg.company
                NO-ERROR.
            IF AVAILABLE fg-ctrl AND fg-ctrl.inv-meth EQ "A" THEN 
                opdCostPerUOMTotal = itemfg.avg-cost.
            ELSE 
                opdCostPerUOMTotal = itemfg.last-cost.
        END.
    END.

END PROCEDURE.

PROCEDURE GetCostForJob:
    /*------------------------------------------------------------------------------
     Purpose: Given Job inputs, finds matching job and returns costs
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo             AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2            AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal  AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO     AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM     AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound            AS LOGICAL NO-UNDO.

    FIND FIRST job-hdr NO-LOCK 
        WHERE job-hdr.company EQ ipcCompany
        AND job-hdr.job-no EQ ipcJobNo
        AND job-hdr.job-no2 EQ ipiJobNo2      
        AND job-hdr.i-no EQ ipcFGItemID
        NO-ERROR.            
    IF AVAILABLE job-hdr AND job-hdr.std-tot-cost NE 0 THEN
        ASSIGN
            oplFound           = YES
            opcCostUOM         = "M"
            opdCostPerUOMTotal = job-hdr.std-tot-cost
            opdCostPerUOMDL    = job-hdr.std-lab-cost
            opdCostPerUOMFO    = job-hdr.std-fix-cost
            opdCostPerUOMVO    = job-hdr.std-var-cost
            opdCostPerUOMDM    = job-hdr.std-mat-cost
            .
    ELSE 
    DO:  /*Set Component*/
        FIND FIRST job NO-LOCK 
            WHERE job.company EQ ipcCompany
            AND job.job-no  EQ ipcJobNo
            AND job.job-no2 EQ ipiJobNo2
            NO-ERROR.
        IF AVAIL job THEN
            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job.job,"999999999")
                AND reftable.code2    EQ ipcFGItemID
                NO-ERROR.
        IF AVAIL reftable AND reftable.val[5] GT 0 THEN
            ASSIGN
                oplFound            = YES
                opcCostUOM          = "M"
                opdCostPerUOMTotal  = reftable.val[5]   
                opdCostPerUOMDL = reftable.val[1]
                opdCostPerUOMDM = reftable.val[2]
                opdCostPerUOMVO = reftable.val[3]
                opdCostPerUOMFO = reftable.val[4].
    END.

END PROCEDURE.

PROCEDURE GetCostForReceipt:
    /*------------------------------------------------------------------------------
        Purpose: Given bin inputs, finds a mathing receipt tag and returns costs
        Notes: Could add additional "loose" matches like not requiring loc and loc-bin
        or just match on job
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM AS DECIMAL NO-UNDO.  
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER b-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER b-fg-rdtlh FOR fg-rdtlh.
        
    
    IF ipcTag NE "" THEN 
    DO:
        each-fg:
        FOR EACH b-fg-rcpth WHERE b-fg-rcpth.company   EQ ipcCompany
            AND b-fg-rcpth.i-no      EQ ipcFGItemID
            AND b-fg-rcpth.rita-code EQ "R"
            USE-INDEX tran NO-LOCK  ,
            
            FIRST b-fg-rdtlh WHERE b-fg-rdtlh.r-no    EQ b-fg-rcpth.r-no 
            AND b-fg-rdtlh.rita-code EQ b-fg-rcpth.rita-code
            AND b-fg-rdtlh.tag EQ ipcTag
            NO-LOCK
            BY b-fg-rcpth.trans-date DESCENDING:            
            
            IF b-fg-rdtlh.std-tot-cost NE 0 THEN         
                ASSIGN
                    opdCostPerUOMFO    = b-fg-rdtlh.std-fix-cost   
                    opdCostPerUOMDL    = b-fg-rdtlh.std-lab-cost   
                    opdCostPerUOMDM    = b-fg-rdtlh.std-mat-cost    
                    opdCostPerUOMTotal = b-fg-rdtlh.std-tot-cost    
                    opdCostPerUOMVO    = b-fg-rdtlh.std-var-cost    
                    .
            ELSE 
                ASSIGN 
                    opdCostPerUOMTotal = b-fg-rdtlh.cost.    
            ASSIGN 
                opcCostUOM         = b-fg-rcpth.pur-uom
                oplFound           = YES
                .
            LEAVE each-fg. 
        END. /* each fg-rcp */
    END.
    
END PROCEDURE.

PROCEDURE GetCostForInvoiceLine:
    /*------------------------------------------------------------------------------
     Purpose: Main Wrapper Program for Main Block
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriInvl                AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDL         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMVO         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMDM         AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMTotal      AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalExtended    AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostSource           AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBNo          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOrderNo      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFGItemID     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustomerPONo AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cJobNo        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQtyInvoiced  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lFound        AS LOGICAL   NO-UNDO.


    RUN pGetKeyCriteriaForInvl(ipriInvl, 
        OUTPUT cCompany, 
        OUTPUT iBNo, 
        OUTPUT iOrderNo,
        OUTPUT cFGItemID,
        OUTPUT cCustomerPONo,
        OUTPUT cJobNo,
        OUTPUT iJobNo2,
        OUTPUT dQtyInvoiced,
        OUTPUT lFound).
    IF lFound THEN 
    DO:        
        RUN pCalculateCostPerUOMForBOL(cCompany,
            iBNo,
            iOrderNo,
            cFGItemID,
            cCustomerPONo,
            OUTPUT opdCostPerUOMTotal,
            OUTPUT opdCostPerUOMDL,
            OUTPUT opdCostPerUOMFO,
            OUTPUT opdCostPerUOMVO,
            OUTPUT opdCostPerUOMDM,
            OUTPUT opcCostUOM,
            OUTPUT opcCostSource).
        IF opdCostPerUOMTotal EQ 0 OR opdCostPerUOMTotal EQ ? THEN 
        DO:
            RUN GetCostForFGItem(cCompany,
                cFGItemID,
                OUTPUT opdCostPerUOMTotal,
                OUTPUT opdCostPerUOMDL,
                OUTPUT opdCostPerUOMFO,
                OUTPUT opdCostPerUOMVO,
                OUTPUT opdCostPerUOMDM,
                OUTPUT opcCostUOM,
                OUTPUT lFound).
            IF lFound THEN
                opcCostSource = "ItemFallback".
        END.
        IF opcCostUOM NE "M" THEN 
        DO: 
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMTotal, OUTPUT opdCostPerUOMTotal).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMDL, OUTPUT opdCostPerUOMDL).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMFO, OUTPUT opdCostPerUOMFO).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMVO, OUTPUT opdCostPerUOMVO).
            RUN pConvertCostToM(opcCostUOM, opdCostPerUOMDM, OUTPUT opdCostPerUOMDM).
            opcCostUOM = "M".
        END.
        IF opdCostPerUOMTotal EQ ? THEN opdCostPerUOMTotal = 0.
        IF opdCostPerUOMDL EQ ? THEN opdCostPerUOMDL = 0.
        IF opdCostPerUOMFO EQ ? THEN opdCostPerUOMFO = 0.
        IF opdCostPerUOMVO EQ ? THEN opdCostPerUOMVO = 0.
        IF opdCostPerUOMDM EQ ? THEN opdCostPerUOMDM = 0.
        opdCostTotalExtended = opdCostPerUOMTotal * dQtyInvoiced / 1000.
    END.
    ELSE 
        opcCostSource = "Invalid Inv Line".
        
END PROCEDURE.

PROCEDURE pGetKeyCriteriaForInvl PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriInvl AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOrderNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustomerPONo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcJobNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJobNo2 AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQtyInvoiced AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.

    FIND inv-line NO-LOCK 
        WHERE ROWID(inv-line) EQ ipriInvl 
        NO-ERROR.
    IF NOT AVAILABLE inv-line THEN
        FIND ar-invl NO-LOCK 
            WHERE ROWID(ar-invl) EQ ipriInvl
            NO-ERROR.

    IF AVAILABLE inv-line OR AVAILABLE ar-invl THEN 
    DO:
        oplFound = YES.
        IF AVAILABLE inv-line THEN
            ASSIGN
                opcCompany      = inv-line.company
                opiBNo          = inv-line.b-no
                opiOrderNo      = inv-line.ord-no
                opcFGItemID     = inv-line.i-no
                opcCustomerPONo = inv-line.po-no
                opcJobNo        = inv-line.job-no
                opiJobNo2       = inv-line.job-no2
                opdQtyInvoiced  = inv-line.inv-qty
                .
        ELSE
            ASSIGN
                opcCompany      = ar-invl.company
                opiBNo          = ar-invl.b-no
                opiOrderNo      = ar-invl.ord-no
                opcFGItemID     = ar-invl.i-no
                opcCustomerPONo = ar-invl.po-no
                opcJobNo        = ar-invl.job-no
                opiJobNo2       = ar-invl.job-no2
                opdQtyInvoiced  = ar-invl.inv-qty
                .

        RELEASE inv-line.
        RELEASE ar-invl.
    END.
END PROCEDURE.



/* ************************  Function Implementations ***************** */

FUNCTION fConvert RETURNS DECIMAL 
    (ipcFromUOM AS CHARACTER , ipcToUOM AS CHARACTER, 
    ipdBasisWeightInPoundsPerSqInch AS DECIMAL, 
    ipdLengthInInches AS DECIMAL, ipdWidthInInches AS DECIMAL, ipdDepthInInches AS DECIMAL, 
    ipdValueToConvert AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Replaces all conversion programs
     Notes:  modelled after rm\convcuom.p - should be able to replace all conversion programs
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE dValueConverted   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSquareFootOfEach AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLengthInFeet     AS DECIMAL NO-UNDO.
    
    IF ipdValueToConvert EQ 0 THEN 
    DO:
        RETURN dValueConverted.
    END.

    IF ipdLengthInInches EQ 0 AND
        (LOOKUP(ipcFromUOM,"LF,LI,MLF,MLI") NE 0  OR
        LOOKUP(ipcToUOM,"LF,LI,MLF,MLI") NE 0) THEN ipdLengthInInches = 12.

    IF ipdDepthInInches EQ 0 THEN ipdDepthInInches = 1.
    
    dSquareFootOfEach = ipdLengthInInches * ipdWidthInInches * gdMultiplierForSquareFoot.
    dLengthInFeet = ipdLengthInInches / 12.
    /*Convert FromUOM To Each*/
    CASE ipcFromUOM:
        WHEN "MSH" OR 
        WHEN "M" THEN 
            ipdValueToConvert = ipdValueToConvert / 1000.
        WHEN "MSF" THEN
            ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000.
        WHEN "TON" THEN
            IF ipdWidthInInches NE 0 AND ipdLengthInInches NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN
                ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000 * ipdBasisWeightInPoundsPerSqInch / 2000.
        WHEN "LB" THEN
            IF ipdWidthInInches NE 0 AND ipdLengthInInches NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN
                ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert / 1000 * ipdBasisWeightInPoundsPerSqInch.
        WHEN "SF" THEN
            ipdValueToConvert = dSquareFootOfEach * ipdValueToConvert.
        WHEN "MLF" THEN
            ipdValueToConvert = (dLengthInFeet * ipdValueToConvert) / 1000.
        WHEN "LF" THEN
            ipdValueToConvert = dLengthInFeet * ipdValueToConvert.
        WHEN "MLI" THEN
            ipdValueToConvert = (ipdLengthInInches  * ipdValueToConvert) / 1000.
        WHEN "LI" THEN
            ipdValueToConvert = ipdLengthInInches * ipdValueToConvert.
        WHEN "BF" OR 
        WHEN "BSF" THEN
            ipdValueToConvert = ((ipdLengthInInches * ipdWidthInInches * ipdDepthInInches) / 144) * ipdValueToConvert.
        OTHERWISE 
        DO:
            fromuom:
            REPEAT:
                /* put cost into an EA uom */
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom  EQ ipcFromUOM
                    AND uom.mult NE 0
                    NO-ERROR.
                IF AVAILABLE uom THEN 
                DO:
                    ipdValueToConvert = (IF ipdValueToConvert EQ 0 THEN 1 ELSE ipdValueToConvert / uom.mult).
     
                    IF uom.other NE "" AND uom.other NE uom.uom THEN 
                    DO:
                        ipcFromUOM = uom.other.
                        NEXT fromuom.
                    END.
                END.
    
                ELSE ipdValueToConvert = (IF ipdValueToConvert EQ 0 THEN 1 ELSE ipdValueToConvert).
    
                LEAVE fromuom.
            END.
        END.
    END CASE.
    
    /*Convert from Each to ToUOM*/
    CASE ipcToUOM:
        WHEN "MSH" OR 
        WHEN "M" THEN 
            dValueConverted = ipdValueToConvert * 1000.
        WHEN "MSF" THEN 
            IF dSquareFootOfEach NE 0 THEN 
                dValueConverted = (1000 * ipdValueToConvert) / (dSquareFootOfEach).
        WHEN "TON" THEN
            IF dSquareFootOfEach NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN 
                dValueConverted = (2000 * 1000 * ipdValueToConvert) / (ipdBasisWeightInPoundsPerSqInch * dSquareFootOfEach).
        WHEN "LB" THEN
            IF dSquareFootOfEach NE 0 AND ipdBasisWeightInPoundsPerSqInch NE 0 THEN
                dValueConverted = (1000 * ipdValueToConvert) / (ipdBasisWeightInPoundsPerSqInch * dSquareFootOfEach).
        WHEN "SF" THEN
            IF dSquareFootOfEach NE 0 THEN
                dValueConverted = ipdValueToConvert / dSquareFootOfEach.
        WHEN "MLF" THEN
            IF dLengthInFeet NE 0 THEN 
                dValueConverted = (1000 * ipdValueToConvert) / dLengthInFeet.
        WHEN "LF" THEN 
            IF dLengthInFeet NE 0 THEN 
                dValueConverted = ipdValueToConvert / dLengthInFeet.
        WHEN "MLI" THEN
            IF ipdLengthInInches NE 0 THEN 
                dValueConverted = (1000 * ipdValueToConvert) / ipdLengthInInches.
        WHEN "LI" THEN
            IF ipdLengthInInches NE 0 THEN 
                dValueConverted = ipdValueToConvert / ipdLengthInInches.
        WHEN "BF" OR 
        WHEN "BSF" THEN 
            IF ipdLengthInInches NE 0 AND ipdWidthInInches NE 0 AND ipdDepthInInches NE 0 THEN 
                dValueConverted = ipdValueToConvert / ((ipdLengthInInches * ipdWidthInInches * ipdDepthInInches) / 144).
        OTHERWISE 
        DO:
            touom:
            REPEAT:
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom  EQ ipcToUOM
                    AND uom.mult NE 0
                    NO-ERROR.
                IF AVAILABLE uom THEN 
                DO:
                    ipdValueToConvert = (IF ipdValueToConvert NE 0 THEN (ipdValueToConvert * uom.mult) ELSE 0).
                    IF uom.other NE "" AND uom.other NE uom.uom THEN 
                    DO:
                        ipcToUOM = uom.other.
                        NEXT touom.
                    END.
                END.
    
                dValueConverted = ipdValueToConvert.
    
                LEAVE touom.
            END.
        END.
    END CASE.

    RETURN dValueConverted.
	
END FUNCTION.

FUNCTION fConvertCurrency RETURNS DECIMAL PRIVATE
    (ipdValue AS DECIMAL, ipcCompany AS CHARACTER, ipcCurrCode AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Given a value and currency code, return value converted to 
     currency exchange rate.
     Notes: Should replace CalcRcptCostFromPO-conver-vendor-comp-curr
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dValueConverted AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-company  FOR company.
    DEFINE BUFFER bf-currency FOR currency.
	 
    dValueConverted = ipdValue.
    FIND FIRST bf-company NO-LOCK 
        WHERE bf-company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE bf-company AND ipcCurrCode NE "" AND ipcCurrCode NE bf-company.curr-code THEN 
    DO:
        FIND FIRST bf-currency NO-LOCK 
            WHERE bf-currency.company EQ bf-company.company 
            AND bf-currency.c-code EQ ipcCurrCode
            NO-ERROR.

        IF AVAIL bf-currency THEN
        DO:
            dValueConverted = ipdValue * bf-currency.ex-rate.
            RELEASE bf-currency.
        END.
    END.

    RELEASE bf-company.
        	
    RETURN dValueConverted.
		
END FUNCTION.

