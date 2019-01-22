
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
DEFINE VARIABLE gdMultiplierForSquareInch AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fConvert RETURNS DECIMAL 
    (ipcFromUOM AS CHARACTER,
    ipcToUOM AS CHARACTER,
    ipdBasisWeightInPoundsPerSqInch AS DECIMAL,
    ipipdLengthInInches AS DECIMAL,
    ipdWidthInInches AS DECIMAL,
    ipdDepthInInches AS DECIMAL,
    ipdValueToConvert AS DECIMAL) FORWARD.

FUNCTION fGetIncludeFreight RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */

gdMultiplierForSquareInch = 1 / 144.


/* **********************  Internal Procedures  *********************** */

PROCEDURE GetCostForPOLine:
    /*------------------------------------------------------------------------------
     Purpose: REturns a Per UOM Cost based on total cost of the po.
     Includes Setup and Discounts in EffectiveCost.
     Will include freight in the per UOM cost based on FGPOFRT settings 
     one 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipiPONumber AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPOLine AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipdFreightOverride AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOMEffective AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdLineFreight AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-po-ord  FOR po-ord.
    DEFINE BUFFER bf-po-ordl FOR po-ordl.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-item    FOR ITEM.
    DEFINE BUFFER bf-vend    FOR vend.
    
    DEFINE VARIABLE lIncludeFreight   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ipdLengthInInches AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWidthInInches    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDepthInInches    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBasisWeight      AS DECIMAL NO-UNDO.
    
    RUN pSetBuffersPO(ipcCompany, ipiPONumber, ipiPOLine, 
        BUFFER bf-po-ord, BUFFER bf-po-ordl, 
        BUFFER bf-itemfg, BUFFER bf-item, BUFFER bf-vend).

    IF AVAILABLE bf-itemfg THEN 
    DO: 
        lIncludeFreight = fGetIncludeFreight(bf-itemfg.company).
        ASSIGN 
            
            opcCostUOM = bf-itemfg.prod-uom
            .
        
    END.
    ELSE 
    DO: 
        lIncludeFreight = NO.
        ASSIGN 
            opcCostUOM = bf-item.cons-uom
            .
    /*Run pGetCostForPOLineInUOM for item*/
    END.
    RUN pGetCostForPOLineInUOM(BUFFER bf-po-ord, BUFFER bf-po-ordl, opcCostUOM,
        lIncludeFreight, ipdFreightOverride, 
        ipdLengthInInches, dWidthInInches, dDepthInInches, dBasisWeight,
        OUTPUT opdCostPerUOMEffective).
                               
END PROCEDURE.

PROCEDURE pCalculateCostUOMEffective PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given value inputs for cost, cost uom, 
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pGetCostForPOLineInUOM PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given inputs, calculates an effective per UOM cost for a PO Line
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER ipbf-po-ordl FOR po-ordl.
    DEFINE INPUT PARAMETER ipcTargetUOM AS CHARACTER.
    DEFINE INPUT PARAMETER iplIncludeFreight AS LOGICAL.
    DEFINE INPUT PARAMETER ipdFreightOverride AS DECIMAL.
    DEFINE INPUT PARAMETER ipdLengthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdWidthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdDepthInInches AS DECIMAL.
    DEFINE INPUT PARAMETER ipdBasisWeightInPoundsPerSqInch AS DECIMAL.
    DEFINE OUTPUT PARAMETER opdCostInTargetUOM AS DECIMAL.

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
                              
    /*Calculate Total Cost with OrderQty Times Unit Cost and add setup*/
    dCostTotal = dCostInOrderQtyUOM * dOrderQty + dCostSetup.

    /*Apply Freight Costs if necessary*/
    IF iplIncludeFreight THEN 
    DO:
        IF ipdFreightOverride NE 0 THEN 
            dCostTotal = dCostTotal + ipdFreightOverride.
        ELSE DO:
            RUN pGetFreightPortion(BUFFER ipbf-po-ord, BUFFER ipbf-po-ordl, OUTPUT dFreightPortion).
            dCostTotal = dCostTotal + dFreightPortion.
        END.
    END. 

    /*Convert Cost from Order UOM into Target UOM*/
    opdCostInTargetUOM = fConvert(cOrderQtyUOM, ipcTargetUOM,
        ipdBasisWeightInPoundsPerSqInch,
        ipdLengthInInches, ipdWidthInInches, ipdDepthInInches,
        dCostTotal / dOrderQty).

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
    IF AVAILABLE opbf-po-ordl AND opbf-po-ordl.item-type THEN 
        FIND FIRST opbf-itemfg NO-LOCK 
            WHERE opbf-itemfg.company EQ opbf-po-ordl.company
            AND opbf-itemfg.i-no EQ opbf-po-ordl.i-no
            NO-ERROR.
    ELSE 
        FIND FIRST opbf-item NO-LOCK 
            WHERE opbf-item.company EQ opbf-po-ordl.company
            AND opbf-item.i-no EQ opbf-po-ordl.i-no
            NO-ERROR.
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fConvert RETURNS DECIMAL 
    (ipcFromUOM AS CHARACTER , ipcToUOM AS CHARACTER, 
    ipdBasisWeightInPoundsPerSqInch AS DECIMAL, 
    ipdLengthInInches AS DECIMAL, ipdWidthInInches AS DECIMAL, ipdDepthInInches AS DECIMAL, 
    ipdValueToConvert AS DECIMAL):
    /*------------------------------------------------------------------------------
     Purpose: Replaces all conversion programs
     Notes:
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
    
    dSquareFootOfEach = ipdLengthInInches * ipdWidthInInches * gdMultiplierForSquareInch.
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

FUNCTION fGetIncludeFreight RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Given company, get the NK1 FGPOFRT logical which should include freight
     in cost
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE lIncludeFreight AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn         AS CHARACTER NO-UNDO. 
    
    RUN sys/ref/nk1look.p (ipcCompany,
        "FGPOFRT",
        "L",
        NO,
        NO,
        "",
        "",
        OUTPUT cReturn,
        OUTPUT lFound).

    lIncludeFreight = lFound AND cReturn EQ "YES".
    RETURN lIncludeFreight.
		
END FUNCTION.

