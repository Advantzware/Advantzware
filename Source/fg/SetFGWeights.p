
/*------------------------------------------------------------------------
    File        : SetFGWeights.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Oct 15 14:38:59 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipriItemfg AS ROWID NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fUseTotalWeight RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
RUN pSetFGWeights(ipriItemfg).


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetBuffers PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets the main itemfg buffer and related buffers if necessary
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItemfg AS ROWID NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-itemfg      FOR itemfg.
    DEFINE PARAMETER BUFFER opbf-ef          FOR ef.
    DEFINE PARAMETER BUFFER opbf-estCostItem FOR estCostItem.
    
    FIND opbf-itemfg NO-LOCK 
        WHERE ROWID(opbf-itemfg) EQ ipriItemfg NO-ERROR.
    IF AVAILABLE opbf-itemfg THEN 
    DO:
        FIND FIRST eb NO-LOCK
            WHERE eb.company EQ opbf-itemfg.company
            AND eb.est-no EQ opbf-itemfg.est-no
            AND eb.stock-no EQ opbf-itemfg.i-no
            NO-ERROR.
        IF NOT AVAILABLE eb THEN 
            FIND FIRST eb NO-LOCK
                WHERE eb.company EQ opbf-itemfg.company
                AND eb.est-no EQ opbf-itemfg.est-no
                AND eb.part-no EQ opbf-itemfg.part-no
                NO-ERROR.
        IF AVAILABLE eb THEN 
            FIND FIRST opbf-ef NO-LOCK
                WHERE opbf-ef.company EQ eb.company
                AND opbf-ef.est-no EQ eb.est-no
                AND opbf-ef.form-no EQ eb.form-no
                NO-ERROR 
                .
        FOR FIRST estCostHeader NO-LOCK
            WHERE estCostHeader.company EQ opbf-itemfg.company
            AND estCostHeader.estimateNo EQ opbf-itemfg.est-no
            BY estCostHeader.calcDateTime DESCENDING:
            FIND FIRST opbf-estCostItem NO-LOCK 
                WHERE opbf-estCostItem.estCostHeaderID EQ estCostHeader.estCostHeaderID
                AND opbf-estCostItem.itemID EQ opbf-itemfg.i-no
                NO-ERROR.
            IF NOT AVAILABLE opbf-estCostItem THEN 
                FIND FIRST opbf-estCostItem NO-LOCK 
                    WHERE opbf-estCostItem.estCostHeaderID EQ estCostHeader.estCostHeaderID
                    AND opbf-estCostItem.customerPart EQ opbf-itemfg.part-no
                    NO-ERROR.
        END.
    END.  

END PROCEDURE.

PROCEDURE pSetFGWeights PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Main processing      
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItemFG AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-itemfg      FOR itemfg.
    DEFINE BUFFER bf-ef          FOR ef.
    DEFINE BUFFER bf-estCostItem FOR estCostItem.
    
    DEFINE VARIABLE dWeightPer100         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightPerEANet       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightPerEATare      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightPerTareSubUnit AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightPerTareUnit    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dWeightPerEAGross     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cWeightUOM            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantityOfSubUnits   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityOfUnits      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQuantityOfEA         AS DECIMAL   NO-UNDO.
    
    RUN pGetBuffers(ipriItemfg, BUFFER bf-itemfg, BUFFER bf-ef, BUFFER bf-estCostItem).
    IF AVAILABLE bf-itemfg THEN 
    DO:
        
        IF AVAILABLE bf-estCostItem THEN 
        DO:
            dQuantityOfEA = bf-estCostItem.quantityRequired.
            IF dQuantityOfEA GT 0 THEN
                ASSIGN 
                    dWeightPerEANet       = bf-estCostItem.weightNet / dQuantityOfEA
                    dWeightPerEATare      = bf-estCostItem.weightTare / dQuantityOfEA
                    dWeightPerEAGross     = bf-estCostItem.weightTotal / dQuantityOfEA
                    dQuantityOfSubUnits   = IF bf-estCostItem.quantityPerSubUnit GT 0 THEN dQuantityOfEA / bf-estCostItem.quantityPerSubUnit ELSE 0 
                    dQuantityOfUnits      = IF dQuantityOfSubUnits GT 0 AND bf-estCostItem.quantitySubUnitsPerUnit GT 0 THEN (dQuantityOfEA / dQuantityofSubUnits) / bf-estCostItem.quantitySubUnitsPerUnit ELSE 0 
                    dWeightPerTareSubUnit = IF dQuantityOfSubUnits GT 0 THEN bf-estCostItem.weightTare / dQuantityOfSubUnits ELSE 0
                    dWeightPerTareUnit    = IF dQuantityOfUnits GT 0 THEN bf-estCostItem.weightTare / dQuantityOfUnits ELSE 0
                    cWeightUOM            = bf-estCostItem.weightUOM
                    .
            
            IF fUseTotalWeight(bf-estCostItem.company) THEN 
                dWeightPer100 = dWeightPerEAGross * 100.
            ELSE 
                dWeightPer100 = dWeightPerEANet * 100.
        END.
        ELSE IF AVAILABLE bf-ef THEN 
            DO:  //legacy rules from updfgdim.i
                ASSIGN 
                    cWeightUOM        = "LB"
                    dWeightPer100     = bf-ef.weight * bf-itemfg.t-sqft * 0.1  //weight in LBs by MSF - weight / 1000 * 100 = Weight / 10                
                    dWeightPerEANet   = dWeightPer100 / 100
                    dWeightPerEAGross = dWeightPerEANet
                    .
            END.
        IF cWeightUOM NE "LB" THEN 
        DO:
                //Refactor - convert to LBs
        END.        
        IF dWeightPer100 NE 0 THEN 
        DO:    
            FIND CURRENT bf-itemfg EXCLUSIVE-LOCK.
            ASSIGN 
                bf-itemfg.weightNetPerEA       = dWeightPerEANet
                bf-itemfg.weightPerEA          = dWeightPerEAGross
                bf-itemfg.weight-100           = dWeightPer100
                bf-itemfg.weightUOM            = cWeightUOM
                bf-itemfg.weightTarePerSubunit = dWeightPerTareSubUnit
                bf-itemfg.weightTarePerUnit    = dWeightPerTareUnit
                .
            FIND CURRENT bf-itemfg NO-LOCK. 
        END.    
    END.    
    
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fUseTotalWeight RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the NK1 value of CEShipWeight
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "CEShipWeight", "C" , NO, YES, "","", OUTPUT cReturn, OUTPUT lFound).
    RETURN lFound AND cReturn EQ "Gross".
		
END FUNCTION.

