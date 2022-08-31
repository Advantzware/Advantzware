
/*------------------------------------------------------------------------
    File        : EstMiscProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu Aug 04 00:45:40 IST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */
{est/ttEstMisc.i}
/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE EstMisc_Delete:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriSourceRowID      AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estMiscControl     FOR estMiscControl.
    DEFINE BUFFER bf-estMisc            FOR estMisc.
    
    IF ipriSourceRowID NE ? THEN DO:
        FIND FIRST bf-estMiscControl EXCLUSIVE-LOCK
             WHERE ROWID(bf-estMiscControl) EQ ipriSourceRowID
             NO-ERROR.
        IF AVAILABLE bf-estMiscControl THEN
            DELETE bf-estMiscControl.
        ELSE DO:
            FIND FIRST bf-estMisc EXCLUSIVE-LOCK
                 WHERE ROWID(bf-estMisc) EQ ipriSourceRowID
                 NO-ERROR.
            IF AVAILABLE bf-estMisc THEN
                DELETE bf-estMisc.           
       END.
    END.


END PROCEDURE.

PROCEDURE EstMisc_GetCustomList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustomList AS CHARACTER NO-UNDO.
    
    opcCustomList = "Board Cost"             + "," + "costTotalBoard" + ","
                  + "Labor Cost"             + "," + "costTotalLabor" + ","
                  + "Variable Overhead Cost" + "," + "costTotalVariableOverhead" + ","
                  + "Fixed Overhead Cost"    + "," + "costTotalFixedOverhead" + ","
                  + "Material Cost"          + "," + "costTotalMaterial" + ","
                  + "Factory Cost"           + "," + "costTotalFactory" + ","
                  + "Non Factory Cost"       + "," + "costTotalNonFactory" + ","
                  + "Net Profit"             + "," + "netProfit" + ","
                  + "Full Cost"              + "," + "costTotalFull" + ","
                  + "Sell Price"             + "," + "sellPrice" + ","
                  + "Gross Profit"           + "," + "grossProfit".
END PROCEDURE.

PROCEDURE EstMisc_GetCategoryList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCategoryList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostCategorySystem FOR estCostCategorySystem.
    DEFINE BUFFER bf-estCostCategory       FOR estCostCategory.

    FOR EACH bf-estCostCategory NO-LOCK
        WHERE bf-estCostCategory.company EQ ipcCompany:
        IF CAN-FIND(FIRST estCostCategorySystem 
                    WHERE estCostCategorySystem.estCostCategoryID EQ bf-estCostCategory.estCostCategoryID) THEN
            NEXT.
             
        opcCategoryList = opcCategoryList + "," + bf-estCostCategory.estCostCategoryDesc + "," + STRING(bf-estCostCategory.estCostCategoryID).
    END.  
        
    FOR EACH bf-estCostCategorySystem NO-LOCK:
        opcCategoryList = opcCategoryList + "," + bf-estCostCategorySystem.estCostCategoryDesc + "," + STRING(bf-estCostCategorySystem.estCostCategoryID).
    END.    
    
    opcCategoryList = TRIM(opcCategoryList, ",").
END PROCEDURE.


PROCEDURE EstMisc_GetGroupList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcGroupList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostGroupSystem FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroup       FOR estCostGroup.

    FOR EACH bf-estCostGroup NO-LOCK
        WHERE bf-estCostGroup.company EQ ipcCompany:
        IF CAN-FIND(FIRST estCostGroupSystem 
                    WHERE estCostGroupSystem.estCostGroupID EQ bf-estCostGroup.estCostGroupID) THEN
            NEXT.
             
        opcGroupList = opcGroupList + "," + bf-estCostGroup.estCostGroupDesc + "," + STRING(bf-estCostGroup.estCostGroupID).
    END.  
        
    FOR EACH bf-estCostGroupSystem NO-LOCK:
        opcGroupList = opcGroupList + "," + bf-estCostGroupSystem.estCostGroupDesc + "," + STRING(bf-estCostGroupSystem.estCostGroupID).
    END.    
    
    opcGroupList = TRIM(opcGroupList, ",").
END PROCEDURE.

PROCEDURE EstMisc_GetGroupLevelList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcGroupLevelList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    DEFINE BUFFER bf-estCostGroupLevel       FOR estCostGroupLevel.

    FOR EACH bf-estCostGroupLevel NO-LOCK
        WHERE bf-estCostGroupLevel.company EQ ipcCompany:
        IF CAN-FIND(FIRST estCostGroupLevelSystem 
                    WHERE estCostGroupLevelSystem.estCostGroupLevelID EQ bf-estCostGroupLevel.estCostGroupLevelID) THEN
            NEXT.
             
        opcGroupLevelList = opcGroupLevelList + "," + bf-estCostGroupLevel.estCostGroupLevelDesc + "," + STRING(bf-estCostGroupLevel.estCostGroupLevelID).
    END.  
        
    FOR EACH bf-estCostGroupLevelSystem NO-LOCK:
        opcGroupLevelList = opcGroupLevelList + "," + bf-estCostGroupLevelSystem.estCostGroupLevelDesc + "," + STRING(bf-estCostGroupLevelSystem.estCostGroupLevelID).
    END.    
    
    opcGroupLevelList = TRIM(opcGroupLevelList, ",").
END PROCEDURE.

PROCEDURE EstMisc_GetEstMisc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcSourceType AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstMisc.
    
    DEFINE BUFFER bf-estMiscControl FOR estMiscControl.
    DEFINE BUFFER bf-estMisc        FOR estMisc.
    
    EMPTY TEMP-TABLE ttEstMisc.
    
    IF ipcSourceType EQ "estMiscControl" THEN DO:        
        FOR EACH bf-estMiscControl NO-LOCK
            WHERE bf-estMiscControl.company EQ ipcCompany:
            CREATE ttEstMisc.
            BUFFER-COPY bf-estMiscControl TO ttEstMisc.
            
            ASSIGN
                ttEstMisc.sourceType  = "estMiscControl"
                ttEstMisc.sourceRowID = ROWID(bf-estMiscControl)
                ttEstMisc.isFlatFee   = bf-estMiscControl.flatFeeCharge NE 0
                .
        END.
    END.
    ELSE IF ipcSourceType EQ "estMisc" THEN DO:
        FOR EACH bf-estMisc NO-LOCK
            WHERE bf-estMisc.company    EQ ipcCompany
              AND bf-estMisc.estimateNO EQ ipcEstimateNo:
            CREATE ttEstMisc.
            BUFFER-COPY bf-estMisc TO ttEstMisc.
            
            ASSIGN
                ttEstMisc.sourceType  = "estMisc"
                ttEstMisc.sourceRowID = ROWID(bf-estMisc)
                ttEstMisc.isFlatFee   = bf-estMisc.flatFeeCharge NE 0
                .
        END.
    END.    
END PROCEDURE.

PROCEDURE EstMisc_ResetEstMiscForEstimate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-estMisc        FOR estMisc.
    DEFINE BUFFER bf-estMiscControl FOR estMiscControl.
    
    IF ipcEstimateNo EQ "" THEN
        RETURN.
        
    DO TRANSACTION:
        FOR EACH bf-estMisc EXCLUSIVE-LOCK
            WHERE bf-estMisc.company    EQ ipcCompany
              AND bf-estMisc.estimateNo EQ ipcEstimateNo:
            DELETE bf-estMisc.
        END.
        
        FOR EACH bf-estMiscControl NO-LOCK
            WHERE bf-estMiscControl.company EQ ipcCompany:
            CREATE bf-estMisc.
            BUFFER-COPY bf-estMiscControl EXCEPT rec_key TO bf-estMisc.

            bf-estMisc.estimateNo = ipcEstimateNo.
        END.
    END.
END PROCEDURE.

PROCEDURE EstMisc_Update:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriSourceRowID      AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSourceType        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostDescripton    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstCostCategoryID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsFlatFee         AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstCostCalcBy     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstCostCalcSource AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdFlatFeeCharge     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdChargePercent     AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSequenceID        AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oprwRowid            AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER NO-UNDO.
                    
    DEFINE BUFFER bf-estMiscControl     FOR estMiscControl.
    DEFINE BUFFER bf-estMisc            FOR estMisc.
    DEFINE BUFFER bf-dup-estMiscControl FOR estMiscControl.
    DEFINE BUFFER bf-dup-estMisc        FOR estMisc.
    
    IF ipriSourceRowID NE ? THEN DO:
        IF ipcSourceType EQ "estMiscControl" THEN
            FIND FIRST bf-estMiscControl NO-LOCK
                 WHERE ROWID(bf-estMiscControl) EQ ipriSourceRowID
                 NO-ERROR.
        ELSE IF ipcSourceType EQ "estMisc" THEN
            FIND FIRST bf-estMisc NO-LOCK
                 WHERE ROWID(bf-estMisc) EQ ipriSourceRowID
                 NO-ERROR.
    END.
    
    IF ipcCompany EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Company cannot be empty"
            .
        
        RETURN.            
    END.
    
    IF ipcCostDescripton EQ "" THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Cost Description cannot be empty"
            .
        
        RETURN.            
    END.

    IF ipcEstCostCategoryID EQ "" OR ipcEstCostCategoryID EQ ? THEN DO:
        ASSIGN
            oplError   = TRUE
            opcMessage = "Category cannot be empty"
            .
        
        RETURN.            
    END.
    
    IF iplIsFlatFee THEN DO:
        IF ipdFlatFeeCharge EQ 0 THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Flat Fee cannot be 0".
                .
            
            RETURN.
        END.            

    END.
    ELSE DO:
        IF ipdChargePercent EQ 0 THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Charge Percent cannot be 0".
                .
            
            RETURN.
        END.            

        IF ipcEstCostCalcBy EQ "" OR ipcEstCostCalcBy EQ ? THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Calculate Cost By cannot be empty"
                .
            
            RETURN.            
        END.            

        IF ipcEstCostCalcSource EQ "" OR ipcEstCostCalcSource EQ ? THEN DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Cost Source cannot be empty"
                .
            
            RETURN.
        END.

    END.
    
    IF ipcSourceType EQ "estMiscControl" THEN DO:
        IF iplIsFlatFee AND AVAILABLE bf-estMiscControl THEN DO:
            FIND FIRST bf-dup-estMiscControl NO-LOCK
                 WHERE bf-dup-estMiscControl.company           EQ ipcCompany
                   AND bf-dup-estMiscControl.estCostCategoryID EQ ipcEstCostCategoryID
                   AND ROWID(bf-dup-estMiscControl)            NE ROWID(bf-estMiscControl)
                 NO-ERROR.
            IF AVAILABLE bf-dup-estMiscControl THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.        
        END.
        ELSE IF NOT iplIsFlatFee AND AVAILABLE bf-estMiscControl THEN DO:
            FIND FIRST bf-dup-estMiscControl NO-LOCK
                 WHERE bf-dup-estMiscControl.company           EQ ipcCompany
                   AND bf-dup-estMiscControl.estCostCalcBy     EQ ipcEstCostCalcBy
                   AND bf-dup-estMiscControl.estCostCalcSource EQ ipcEstCostCalcSource
                   AND bf-dup-estMiscControl.estCostCategoryID EQ ipcEstCostCategoryID
                   AND ROWID(bf-dup-estMiscControl)            NE ROWID(bf-estMiscControl)
                 NO-ERROR.
            IF AVAILABLE bf-dup-estMiscControl THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.
        END.
        
        IF AVAILABLE bf-estMiscControl THEN
            FIND CURRENT bf-estMiscControl EXCLUSIVE-LOCK NO-ERROR.
        ELSE DO:
            CREATE bf-estMiscControl.

            IF ipiSequenceID EQ 0 THEN DO:
                FOR EACH bf-dup-estMiscControl NO-LOCK
                    WHERE bf-dup-estMiscControl.company EQ ipcCompany
                    BY bf-dup-estMiscControl.sequenceID DESCENDING:
                    ipiSequenceID = bf-estMiscControl.sequenceID.
                    LEAVE.
                END.
                
                ipiSequenceID = ipiSequenceID + 1.
            END.        
        END.
            
        IF AVAILABLE bf-estMiscControl THEN DO:
            ASSIGN
                bf-estMiscControl.company           = ipcCompany
                bf-estMiscControl.estCostCategoryID = ipcEstCostCategoryID
                bf-estMiscControl.costDescription   = ipcCostDescripton
                bf-estMiscControl.sequenceID        = ipiSequenceID
                . 
            
            IF iplIsFlatFee THEN
                ASSIGN
                    bf-estMiscControl.flatFeeCharge     = ipdFlatFeeCharge
                    bf-estMiscControl.estCostCalcBy     = ""
                    bf-estMiscControl.estCostCalcSource = ""
                    bf-estMiscControl.chargePercent     = 0
                    .
            ELSE                
                ASSIGN
                    bf-estMiscControl.flatFeeCharge     = 0
                    bf-estMiscControl.estCostCalcBy     = ipcEstCostCalcBy
                    bf-estMiscControl.estCostCalcSource = ipcEstCostCalcSource
                    bf-estMiscControl.chargePercent     = ipdChargePercent
                    .
            oprwRowid = ROWID(bf-estMiscControl).        
        END.
        ELSE DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Error updating record"
                .
            
            RETURN.        
        END.
    END.
    ELSE IF ipcSourceType EQ "estMisc" THEN DO:
        IF iplIsFlatFee AND AVAILABLE bf-estMisc THEN DO:
            FIND FIRST bf-dup-estMisc NO-LOCK
                 WHERE bf-dup-estMisc.company           EQ ipcCompany
                   AND bf-dup-estMisc.estimateNo        EQ ipcEstimateNo
                   AND bf-dup-estMisc.estCostCategoryID EQ ipcEstCostCategoryID
                   AND bf-dup-estMisc.estCostCalcBy     EQ ""
                   AND bf-dup-estMisc.estCostCalcSource EQ ""
                   AND ROWID(bf-dup-estMisc)            NE ROWID(bf-estMisc)
                 NO-ERROR.
            IF AVAILABLE bf-dup-estMisc THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.        
        END.
        ELSE IF NOT iplIsFlatFee AND AVAILABLE bf-estMisc THEN DO:
            FIND FIRST bf-dup-estMisc NO-LOCK
                 WHERE bf-dup-estMisc.company           EQ ipcCompany
                   AND bf-dup-estMisc.estimateNo        EQ ipcEstimateNo
                   AND bf-dup-estMisc.estCostCalcBy     EQ ipcEstCostCalcBy
                   AND bf-dup-estMisc.estCostCalcSource EQ ipcEstCostCalcSource
                   AND bf-dup-estMisc.estCostCategoryID EQ ipcEstCostCategoryID
                   AND ROWID(bf-dup-estMisc)            NE ROWID(bf-estMisc)
                 NO-ERROR.
            IF AVAILABLE bf-dup-estMisc THEN DO:
                ASSIGN
                    oplError   = TRUE
                    opcMessage = "A record already exists for same configuration"
                    .
                
                RETURN.
            END.
        END.

        IF AVAILABLE bf-estMisc THEN
            FIND CURRENT bf-estMisc EXCLUSIVE-LOCK NO-ERROR.
        ELSE DO:
            CREATE bf-estMisc.
            
            IF ipiSequenceID EQ 0 THEN DO:
                FOR EACH bf-dup-estMisc NO-LOCK
                    WHERE bf-dup-estMisc.company    EQ ipcCompany
                      AND bf-dup-estMisc.estimateNO EQ ipcEstimateNo
                    BY bf-dup-estMisc.sequenceID DESCENDING:
                    ipiSequenceID = bf-estMisc.sequenceID.
                    LEAVE.
                END.
                
                ipiSequenceID = ipiSequenceID + 1.
            END.        
        END.
            
        IF AVAILABLE bf-estMisc THEN DO:
            ASSIGN
                bf-estMisc.company           = ipcCompany
                bf-estMisc.estimateNo        = ipcEstimateNo
                bf-estMisc.formNo            = ipiFormNo
                bf-estMisc.estCostCategoryID = ipcEstCostCategoryID
                bf-estMisc.costDescription   = ipcCostDescripton
                bf-estMisc.sequenceID        = ipiSequenceID
                . 
            
            IF iplIsFlatFee THEN
                ASSIGN
                    bf-estMisc.flatFeeCharge     = ipdFlatFeeCharge
                    bf-estMisc.estCostCalcBy     = ""
                    bf-estMisc.estCostCalcSource = ""
                    bf-estMisc.chargePercent     = 0
                    .
            ELSE                
                ASSIGN
                    bf-estMisc.flatFeeCharge     = 0
                    bf-estMisc.estCostCalcBy     = ipcEstCostCalcBy
                    bf-estMisc.estCostCalcSource = ipcEstCostCalcSource
                    bf-estMisc.chargePercent     = ipdChargePercent
                    .
            oprwRowid = ROWID(bf-estMisc).        
        END.
        ELSE DO:
            ASSIGN
                oplError   = TRUE
                opcMessage = "Error updating record"
                .
            
            RETURN.        
        END.        
    END.
END PROCEDURE.
