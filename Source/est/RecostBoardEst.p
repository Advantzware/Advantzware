/*------------------------------------------------------------------------
    File        : RecostBoardEst.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Varun
    Created     : Fri Feb 04 02:36:58 EST 2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */
                                         
{est\RecostBoardEst.i}
{system\FormulaProcs.i}

DEFINE VARIABLE loUpdate AS LOG     NO-UNDO INITIAL FALSE.

/* **********************  Internal Procedures  *********************** */
PROCEDURE RecostBoardEst_RecostBoard:
    DEFINE INPUT  PARAMETER ipinEstCostHeaderID AS INT64.
    DEFINE OUTPUT PARAMETER opchErrorMessage    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRecostBoardGroups.    

    RUN pProcessEstCostMaterial(INPUT ipinEstCostHeaderID). /*Build internal temp-tables*/

    FIND FIRST ttRecostBoardGroups WHERE ttRecostBoardGroups.Multi EQ YES NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE ttRecostBoardGroups THEN 
    DO:  
        ASSIGN opchErrorMessage = "No purchase order items can be grouped for new costs.".
        RETURN.  
    END. 
        
    /*more than one EstCostMaterial line with matching item, vendor, & size*/
    RUN pGetNewCosts.
    
    RUN RecostBoardEst_UpdateEstCostMaterial(INPUT YES,
                              INPUT TABLE ttRecostBoardGroups).  /*See if cost is better than current EstCostMaterial, YES = Compare only*/             
            
    IF NOT loUpdate THEN
        ASSIGN opchErrorMessage = "No board cost or setup improvements available after grouping purchase order items.". 
            
END PROCEDURE.

PROCEDURE pGetNewCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE oploError   AS LOGICAL NO-UNDO.    
    DEFINE VARIABLE opchMessage AS CHARACTER NO-UNDO.
   
    DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.    
    
    FOR EACH ttRecostBoardGroups 
        WHERE ttRecostBoardGroups.Multi:
        
        RUN GetVendorCost(ttRecostBoardGroups.CompanyId, 
            ttRecostBoardGroups.INo, 
            ttRecostBoardGroups.itemType, 
            ttRecostBoardGroups.VendNo, 
            ttRecostBoardGroups.customerID, 
            "", 
            0, 
            0,
            ttRecostBoardGroups.TotalQty, 
            ttRecostBoardGroups.TotalQtyUOM,
            ttRecostBoardGroups.Len, 
            ttRecostBoardGroups.Wid, 
            ttRecostBoardGroups.Dep, 
            ttRecostBoardGroups.UOM, 
            ttRecostBoardGroups.BasisWeight, 
            ttRecostBoardGroups.BasisWeightUOM, 
            NO,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT oploError, 
            OUTPUT opchMessage).

            ASSIGN 
                ttRecostBoardGroups.NewCost    = dCostPerUOM + ttRecostBoardGroups.AdderCost
                ttRecostBoardGroups.NewCostUOM = cCostUOM
                ttRecostBoardGroups.NewSetup   = dCostSetup.

    END. /*each ttRecostBoardGroups*/

END PROCEDURE.

PROCEDURE pProcessEstCostMaterial PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose: Build Temp tables for processing    
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipinEstCostHeaderID AS INT64 NO-UNDO.    
    
    DEFINE BUFFER bf-Item FOR ITEM.
    DEFINE BUFFER bf-eb   FOR eb.        

    DEFINE VARIABLE cScoreList        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdders           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAdderCost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQty              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hdFormulaProcs    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdConversionProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE loError           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE chMessage         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dScores           AS DECIMAL   NO-UNDO EXTENT 20.
    DEFINE VARIABLE cScoreTypes       AS CHARACTER NO-UNDO EXTENT 20.
    DEFINE VARIABLE iCount            AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttRecostBoardGroups.
    EMPTY TEMP-TABLE ttRecostBoardLineXRef.
    
    IF VALID-HANDLE(hdFormulaProcs) = FALSE THEN
        RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
    
    IF VALID-HANDLE(hdConversionProcs) = FALSE THEN
        RUN system/ConversionProcs.p   PERSISTENT SET hdConversionProcs.   
    
    FOR EACH estCostHeader NO-LOCK     
        WHERE estCostHeader.estCostHeaderID EQ ipinEstCostHeaderID ,
        EACH estCostMaterial NO-LOCK     
        WHERE estCostMaterial.estCostHeaderID EQ estCostHeader.estCostHeaderID     
        AND estCostMaterial.isPrimarySubstrate
         BY estCostHeader.quantityMaster BY estCostMaterial.Formno:
            
        ASSIGN 
            cScoreList = ""
            iCount     = 0.           
                    
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ estCostMaterial.company
            AND bf-eb.est-no    EQ estCostMaterial.estimateNo
            AND bf-eb.form-no   EQ estCostMaterial.Formno NO-ERROR.
            
        IF AVAILABLE bf-eb THEN
        DO:           
            
            RUN GetPanelScoreAndTypeForEstimate IN hdFormulaProcs (
                INPUT bf-eb.company,
                INPUT bf-eb.est-no,
                INPUT bf-eb.form-no,
                INPUT bf-eb.blank-no,
                INPUT "W",
                OUTPUT dScores,                
                OUTPUT cScoreTypes).
                
            DO iCount = 1 TO 20:                
                ASSIGN 
                    cScoreList = cScoreList + " " + string(dScores[iCount])
                    iCount     = iCount + 1.
            END.
            
            ASSIGN 
                cScoreList = TRIM(cScoreList," ").            
        END.
                        
        RUN pGetAdders(INPUT estCostMaterial.company,
            INPUT estCostMaterial.estimateNo,
            INPUT estCostMaterial.formNo,
            OUTPUT cAdders,
            OUTPUT dAdderCost).
           
        FIND FIRST bf-Item NO-LOCK
            WHERE bf-Item.company EQ estCostHeader.company
            AND bf-Item.i-no    EQ estCostMaterial.itemID NO-ERROR.
                   
        IF NOT AVAILABLE bf-Item THEN 
            NEXT.          
                       
        FIND FIRST ttRecostBoardGroups 
            WHERE ttRecostBoardGroups.INo  EQ estCostMaterial.itemID
            AND ttRecostBoardGroups.VendNo EQ estCostMaterial.vendorID
            AND ttRecostBoardGroups.Len    EQ estCostMaterial.dimLength
            AND ttRecostBoardGroups.Wid    EQ estCostMaterial.dimWidth
            AND ttRecostBoardGroups.Dep    EQ estCostMaterial.dimdepth
            AND ttRecostBoardGroups.Scores EQ cScoreList
            AND ttRecostBoardGroups.Adders EQ cAdders NO-ERROR.
            
        IF AVAILABLE ttRecostBoardGroups THEN 
        DO:                   
            dQty = estCostMaterial.quantityRequiredTotal.                        
                        
            IF estCostMaterial.quantityUOM NE ttRecostBoardGroups.TotalQtyUOM THEN 
            DO:     
                RUN Conv_QuantityFromUOMtoUOM IN hdConversionProcs(
                    INPUT estCostMaterial.company,
                    INPUT estCostMaterial.itemID,
                    INPUT ttRecostBoardGroups.itemType,
                    INPUT estCostMaterial.quantityRequiredTotal,
                    INPUT estCostMaterial.quantityUOM,
                    INPUT ttRecostBoardGroups.TotalQtyUOM,
                    INPUT estCostMaterial.basisWeight,
                    INPUT estCostMaterial.dimLength,
                    INPUT estCostMaterial.dimWidth,
                    INPUT estCostMaterial.dimdepth,
                    INPUT 0,
                    OUTPUT dQty,
                    OUTPUT loError,
                    OUTPUT chMessage).
            END.
            
            ASSIGN 
                ttRecostBoardGroups.Multi      = YES
                ttRecostBoardGroups.TotalQty   = ttRecostBoardGroups.TotalQty + dQty
                ttRecostBoardGroups.LineCount  = ttRecostBoardGroups.LineCount + 1
                ttRecostBoardGroups.FormIdList = ttRecostBoardGroups.FormIdList + "," + string(estCostMaterial.formNo).            
        END.
        ELSE 
        DO:
            CREATE ttRecostBoardGroups.
            ASSIGN
                ttRecostBoardGroups.CompanyId      = estCostMaterial.company
                ttRecostBoardGroups.INo            = estCostMaterial.itemID
                ttRecostBoardGroups.ItemName       = bf-Item.i-name
                ttRecostBoardGroups.VendNo         = estCostMaterial.vendorID
                ttRecostBoardGroups.Len            = estCostMaterial.dimLength
                ttRecostBoardGroups.Wid            = estCostMaterial.dimWidth
                ttRecostBoardGroups.Dep            = estCostMaterial.dimDepth 
                ttRecostBoardGroups.UOM            = estCostMaterial.dimUOM                
                ttRecostBoardGroups.Scores         = cScoreList
                ttRecostBoardGroups.Adders         = cAdders
                ttRecostBoardGroups.AdderCost      = dAdderCost
                ttRecostBoardGroups.TotalQty       = estCostMaterial.quantityRequiredTotal
                ttRecostBoardGroups.TotalQtyUOM    = estCostMaterial.quantityUOM                
                ttRecostBoardGroups.Multi          = NO
                ttRecostBoardGroups.BasisWeight    = estCostMaterial.basisWeight
                ttRecostBoardGroups.BasisWeightUOM = estCostMaterial.basisWeightUOM                
                ttRecostBoardGroups.LineCount      = 1
                ttRecostBoardGroups.itemType       = IF estCostMaterial.isPurchasedFG THEN "FG" ELSE "RM" 
                ttRecostBoardGroups.quantityMaster = estCostHeader.quantityMaster
                ttRecostBoardGroups.customerID     = bf-Item.cust-no
                ttRecostBoardGroups.FormIdList     = STRING(estCostMaterial.formNo).
        END.
        
        CREATE ttRecostBoardLineXRef.
        ASSIGN        
            ttRecostBoardLineXRef.RecostBoardGroupRowId = ROWID(ttRecostBoardGroups)
            ttRecostBoardLineXRef.EstCostMaterialRowID     = ROWID(estCostMaterial).             
    
    END.
    IF VALID-HANDLE(hdFormulaProcs) THEN
        DELETE PROCEDURE hdFormulaProcs.
    
    IF VALID-HANDLE(hdConversionProcs) THEN
        DELETE PROCEDURE hdConversionProcs.
    
    
END PROCEDURE.

PROCEDURE RecostBoardEst_UpdateEstCostMaterial:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplCompareOnly AS LOG NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttRecostBoardGroups.

    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE VARIABLE dEstGroupCost     LIKE estCostMaterial.costTotal NO-UNDO.
    DEFINE VARIABLE dNewEstLineSetup  LIKE estCostMaterial.costSetup NO-UNDO.
    DEFINE VARIABLE cAdders           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdConversionProcs AS HANDLE    NO-UNDO.
    DEFINE VARIABLE loError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE chMessage         AS CHARACTER NO-UNDO.
    
    IF VALID-HANDLE(hdConversionProcs) = FALSE THEN
        RUN system/ConversionProcs.p   PERSISTENT SET hdConversionProcs.    

    FOR EACH ttRecostBoardGroups
        WHERE ttRecostBoardGroups.Multi
        AND (iplCompareOnly OR ttRecostBoardGroups.UpdateCost),
        EACH ttRecostBoardLineXRef
        WHERE ttRecostBoardLineXRef.RecostBoardGroupRowId EQ ROWID(ttRecostBoardGroups) NO-LOCK:
        FIND FIRST estCostMaterial 
            WHERE ROWID(estCostMaterial) EQ ttRecostBoardLineXRef.EstCostMaterialRowID NO-LOCK NO-ERROR.
        
        IF AVAILABLE estCostMaterial THEN 
        DO:
            ASSIGN 
                dEstGroupCost    = ttRecostBoardGroups.NewCost
                dNewEstLineSetup = ttRecostBoardGroups.NewSetup / ttRecostBoardGroups.LineCount.
             
           IF estCostMaterial.costUOM NE ttRecostBoardGroups.NewCostUom THEN       
            RUN Conv_QuantityFromUOMtoUOM IN hdConversionProcs(
                INPUT ttRecostBoardGroups.company,
                INPUT ttRecostBoardGroups.INO,
                INPUT ttRecostBoardGroups.itemType,
                INPUT estCostMaterial.quantityRequiredTotal,
                INPUT ttRecostBoardGroups.NewCostUom,
                INPUT estCostMaterial.quantityUOM,
                INPUT ttRecostBoardGroups.BasisWeight,
                INPUT ttRecostBoardGroups.len,
                INPUT ttRecostBoardGroups.wid,
                INPUT ttRecostBoardGroups.dep,
                INPUT 0,
                OUTPUT dEstGroupCost,
                OUTPUT loError,
                OUTPUT chMessage).  
        
            IF dEstGroupCost LT estCostMaterial.costTotal OR dNewEstLineSetup LT estCostMaterial.costSetup THEN 
            DO:
                ASSIGN 
                    loUpdate               = YES 
                    ttRecostBoardGroups.UpdateCost = YES.
                    
                IF NOT iplCompareOnly THEN 
                DO:  /*Update estCostMaterial*/
                    FIND CURRENT estCostMaterial EXCLUSIVE-LOCK.
                    IF  estCostMaterial.costPerUOM > dEstGroupCost THEN  
                        ASSIGN estCostMaterial.costPerUOM = dEstGroupCost
                            estCostMaterial.costTotal  = estCostMaterial.quantityRequiredTotal * estCostMaterial.costPerUOM.
                    IF estCostMaterial.costSetup > dNewEstLineSetup THEN 
                        ASSIGN estCostMaterial.costSetup = dNewEstLineSetup.
                    FIND CURRENT estCostMaterial NO-LOCK.                    
                END.
            END.
        END. /*avail bf-estCostMaterial*/
    END. /*Each ttRecostBoardGroups*/
    
    IF VALID-HANDLE(hdConversionProcs) THEN
        DELETE PROCEDURE hdConversionProcs.
        
END PROCEDURE.

PROCEDURE pGetAdders PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipchCompanyId  LIKE  estCostMaterial.company.
    DEFINE INPUT  PARAMETER ipchestimateNo LIKE  estCostMaterial.estimateNo.
    DEFINE INPUT  PARAMETER ipchformNo     LIKE  estCostMaterial.formNo.
    DEFINE OUTPUT PARAMETER opcAdders      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost        AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-ef FOR ef.
        
    DEFINE VARIABLE iCount      AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAvailAdder AS LOGICAL NO-UNDO.

    FIND FIRST bf-ef NO-LOCK
        WHERE bf-ef.company = ipchCompanyId
        AND bf-ef.est-no    = ipchestimateNo
        AND bf-ef.form-no   = ipchformNo NO-ERROR.
        
    IF AVAILABLE bf-ef THEN 
    DO:
        DO iCount = 1 TO 6:
            IF bf-ef.adder[iCount] <> "" THEN
                ASSIGN
                    lAvailAdder = TRUE
                    opcAdders   = opcAdders + "," + bf-ef.adder[iCount].        
        END.
        
        ASSIGN 
            opcAdders = TRIM(opcAdders,",").        
    
    END. /*avail bf-ef*/
END PROCEDURE.