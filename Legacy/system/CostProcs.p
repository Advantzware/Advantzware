
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

PROCEDURE GetCostForPOLine:
    /*------------------------------------------------------------------------------
     Purpose: Returns a Per UOM Cost based on total cost of the po.
     Includes Setup and Discounts in EffectiveCost.
     Will include freight in the per UOM cost based on FGPOFRT settings 
     one 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMExFreight AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMFreight AS DECIMAL NO-UNDO.

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
    RUN pSetBuffersPO(ipcCompany, ipiPONumber, ipiPOLine, 
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
    IF AVAILABLE bf-po-ordl THEN 
        ASSIGN 
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

