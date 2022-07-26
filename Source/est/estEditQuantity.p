
/*------------------------------------------------------------------------
    File        : estEditQuantity.p
    Purpose     : To edit the estimate quantities before calculation

    Syntax      :

    Description : Estimate Edit Quantity.

    Author(s)   : 
    Created     : 05/25/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***********************Parameter Definitions  ********************* */
DEFINE INPUT PARAMETER ipriEB AS RECID NO-UNDO.

/* ***************************  Definitions  ************************** */
{est/ttEstimateQuantity.i}

DEFINE VARIABLE glCorrware AS LOGICAL NO-UNDO INITIAL NO.
DEFINE VARIABLE glopError AS LOGICAL NO-UNDO.
DEFINE VARIABLE glCERunAvailable AS LOGICAL NO-UNDO.
DEFINE VARIABLE glComboType AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdVendorCostProcs AS HANDLE NO-UNDO.

DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf-est FOR est.

RUN system/VendorCostProcs.p PERSISTENT SET hdVendorCostProcs.
/* ********************  Preprocessor Definitions  ******************** */

/* ********************  FunctionPrototypes  ************************** */

FUNCTION fIsComboType RETURNS LOGICAL PRIVATE 
    (ipcEstType AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
RUN pCheckCriteria.

/* **********************  Internal Procedures  *********************** */
PROCEDURE pCheckCriteria PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/  
   FIND FIRST buf-eb NO-LOCK 
    WHERE RECID(buf-eb) EQ ipriEB NO-ERROR.
   
   RUN pSetGlobalSettings(buf-eb.company, buf-eb.est-no).
   IF glCERunAvailable THEN  
   DO: 
       /* not applicable for combo type estimates */
       IF AVAILABLE buf-eb
       AND NOT glComboType THEN  
           RUN pBuildTempTable(BUFFER buf-eb).
       ELSE 
           MESSAGE "Edit-Quantity is not applicable for combo estimates"
               VIEW-AS ALERT-BOX. 
   END.
   ELSE 
       MESSAGE "NK1 CeRUN Value is not 1 or 2"
       VIEW-AS ALERT-BOX.            
       
END PROCEDURE.          
 
PROCEDURE pBuildTempTable PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/ 
    DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
    
    DEFINE VARIABLE dQuantityPerSet AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE opdCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE opdSetup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE VARIABLE opdCostNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE VARIABLE opdQuantityNextPriceBreak AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
      
    EMPTY TEMP-TABLE ttEstimateQuantity.
             
    FIND FIRST est-qty NO-LOCK 
         WHERE est-qty.company EQ ipbf-eb.company  
           AND est-qty.est-no EQ  ipbf-eb.est-no NO-ERROR.
   
    IF AVAILABLE est-qty THEN
    DO: 
        FIND FIRST est NO-LOCK
             WHERE est.company EQ est-qty.company 
               AND est.est-no EQ est-qty.est-no NO-ERROR.
                      
        CREATE ttEstimateQuantity.
        ASSIGN ttEstimateQuantity.EstQuantity[1] = INTEGER(est-qty.eqty)
               ttEstimateQuantity.EstRelease[1]  = est-qty.qty[21]
               ttEstimateQuantity.EstRunship[1]  = est-qty.whsed[1].
             
        DO iCount = 2 TO 20:
            ASSIGN ttEstimateQuantity.EstQuantity[iCount] = est-qty.qty[iCount]
                   ttEstimateQuantity.EstRelease[iCount]  = est-qty.qty[iCount + 20]
                   ttEstimateQuantity.EstRunship[iCount]  = est-qty.whsed[iCount].
        END.                      
           
        ASSIGN dQuantityPerSet  = IF NOT glCorrware THEN 1
                                  ELSE (IF ipbf-eb.quantityPerSet < 0 THEN -1 / ipbf-eb.quantityPerSet ELSE ipbf-eb.quantityPerSet).
                                  
        FIND FIRST ttEstimateQuantity NO-LOCK NO-ERROR.
        
        IF ttEstimateQuantity.EstQuantity[1] > 0 THEN 
            ttEstimateQuantity.EstMSF[1] = IF glCorrware THEN 
                                              ((ttEstimateQuantity.EstQuantity[1] * ipbf-eb.t-len * ipbf-eb.t-wid * .007)
                                               * dQuantityPerSet
                                               / 1000 )
                                            ELSE 
                                              ((ttEstimateQuantity.EstQuantity[1] * ipbf-eb.t-len * ipbf-eb.t-wid / 144)
                                               * dQuantityPerSet
                                               / 1000 ).                                     

        DO iCount = 2 TO 20:
            IF est-qty.qty[iCount] > 0 THEN
                ttEstimateQuantity.EstMSF[iCount] = IF glCorrware THEN
                                                    ( (est-qty.qty[iCount] * ipbf-eb.t-len * ipbf-eb.t-wid * .007)
                                                    * dQuantityPerSet
                                                    / 1000 )
                                                    ELSE  ( (est-qty.qty[iCount] * ipbf-eb.t-len * ipbf-eb.t-wid / 144)
                                                    * dQuantityPerSet
                                                    / 1000 ).
        END.
        FIND FIRST ef NO-LOCK 
            WHERE ef.company EQ ipbf-eb.company 
              AND ef.est-no EQ ipbf-eb.est-no
              AND ef.form-no EQ ipbf-eb.form-no NO-ERROR.
                   
        IF ipbf-eb.pur-man THEN 
        DO:
        RUN GetNextPriceBreak IN hdVendorCostProcs (INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE, ipbf-eb.company, "FG", ipbf-eb.stock-no, ipbf-eb.est-no, ipbf-eb.form-no, ipbf-eb.blank-no, ef.gsh-len, ef.gsh-wid, ef.gsh-dep, ROWID(ef), ipbf-eb.num-up, "", OUTPUT opdCostNextPriceBreak).                                .
        END.
        ELSE 
        DO:
        RUN GetNextPriceBreak IN hdVendorCostProcs (INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE,  ipbf-eb.company, "RM", ef.board, ipbf-eb.est-no, ipbf-eb.form-no, ipbf-eb.blank-no, ef.gsh-len, ef.gsh-wid, ef.gsh-dep, ROWID(ef), ipbf-eb.num-up, "", OUTPUT opdCostNextPriceBreak).
        END.

        RUN pCallUIToUpdate(BUFFER est-qty, BUFFER est).
    END.
END PROCEDURE.

PROCEDURE pCallUIToUpdate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Call UI for updating quantities for estimate
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-qty FOR est-qty.
    DEFINE PARAMETER BUFFER ipbf-est     FOR est.
        
    RUN est/Updestqtyd.w (ipriEB, glCorrware, OUTPUT glopError, INPUT-OUTPUT TABLE ttEstimateQuantity BY-REFERENCE).
    
    IF NOT glopError THEN
       RUN pProcessUpdatedQuantity(BUFFER ipbf-est-qty, BUFFER ipbf-est). 
          
END PROCEDURE.

PROCEDURE pProcessUpdatedQuantity PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-est-qty FOR est-qty.
    DEFINE PARAMETER BUFFER ipbf-est     FOR est. 
    
    DEFINE VARIABLE iEQtyBeforeEdit LIKE est-qty.eqty NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    
    FIND CURRENT ipbf-est NO-ERROR.
    ASSIGN ipbf-est.est-qty[1] = INTEGER (ttEstimateQuantity.EstQuantity [1])
           ipbf-est.est-qty[2] = INTEGER (ttEstimateQuantity.EstQuantity [2])
           ipbf-est.est-qty[3] = INTEGER (ttEstimateQuantity.EstQuantity [3])
           ipbf-est.est-qty[4] = INTEGER (ttEstimateQuantity.EstQuantity [4]).
            
    FIND CURRENT ipbf-est NO-LOCK NO-ERROR.

    ASSIGN iEQtyBeforeEdit = ipbf-est-qty.eqty.
    FIND CURRENT ipbf-est-qty NO-ERROR.
    ipbf-est-qty.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    DO iCount = 1 TO 20:
        ASSIGN est-qty.qty[iCount] = ttEstimateQuantity.EstQuantity[iCount] 
               est-qty.qty[iCount + 20] = ttEstimateQuantity.EstRelease[iCount] 
               est-qty.whsed[iCount] = ttEstimateQuantity.EstRunship[iCount].
    END.
    
    FIND CURRENT ipbf-est-qty NO-LOCK NO-ERROR.
    
    FOR EACH buf-eb
       WHERE buf-eb.company EQ ipbf-est.company
         AND buf-eb.est-no  EQ ipbf-est.est-no
         AND buf-eb.eqty    EQ iEQtyBeforeEdit
         AND buf-eb.form-no NE 0:  
                       
        buf-eb.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    END.
     
    FOR EACH buf-ef
       WHERE buf-ef.company EQ ipbf-est.company
         AND buf-ef.est-no  EQ ipbf-est.est-no
         AND buf-ef.eqty    EQ iEQtyBeforeEdit:  
                
        buf-ef.eqty = INTEGER (ttEstimateQuantity.EstQuantity [1]).
    END.
    
    FOR EACH est-op
         WHERE est-op.company EQ ipbf-est.company
           AND est-op.est-no  EQ ipbf-est.est-no
           AND est-op.qty     EQ iEQtyBeforeEdit:
       est-op.qty = ipbf-est-qty.eqty.
     END.
    
     FOR EACH est-op
         WHERE est-op.company EQ ipbf-est.company
           AND est-op.est-no  EQ ipbf-est.est-no
           AND est-op.qty     EQ ipbf-est-qty.eqty
           AND est-op.line    GE 500:
       DELETE est-op.
     END.

END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cEstType AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER buf-est FOR est.
    
    FIND FIRST buf-est NO-LOCK 
         WHERE buf-est.company EQ ipcCompany
           AND buf-est.est-no  EQ ipcEstimateNo NO-ERROR.
    IF AVAILABLE buf-est THEN   
    ASSIGN cEstType    = DYNAMIC-FUNCTION("fEstimate_GetEstimateType", buf-est.est-type, buf-est.estimateTypeID)
           glComboType = fIsComboType(cEstType).
     
    
    FIND FIRST sys-ctrl NO-LOCK 
         WHERE sys-ctrl.company EQ ipcCompany
           AND sys-ctrl.name    EQ "MSFCALC" NO-ERROR.
             
    glCorrware = (NOT AVAILABLE sys-ctrl) OR sys-ctrl.char-fld EQ "Corrware".
       
    RUN sys/ref/nk1look.p (ipcCompany, "CERUN", "I", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
    glCERunAvailable = lFound AND (INTEGER (cReturn) EQ 1 OR INTEGER (cReturn) EQ 2).    
       
END PROCEDURE.

FUNCTION fIsComboType RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Returns the constant value for Combo Estimate Type
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsComboType", ipcEstType).
    
END FUNCTION.
