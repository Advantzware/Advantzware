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
{est\ttEstimateCalc.i}
{est\ttEstCostHeaderToCalc.i}

DEFINE VARIABLE loUpdate AS LOG     NO-UNDO INITIAL FALSE.

/* **********************  Internal Procedures  *********************** */
PROCEDURE RecostBoardEst_RecostBoard:
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostHeaderToCalc. 
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostHeader.        
    DEFINE OUTPUT PARAMETER opchErrorMessage    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttRecostBoardGroups.    

    RUN pProcessEstCostMaterial(INPUT TABLE ttEstCostHeaderToCalc, 
                                INPUT TABLE ttEstCostMaterial,
                                INPUT TABLE ttEstCostHeader). /*Build internal temp-tables*/

    FIND FIRST ttRecostBoardGroups WHERE ttRecostBoardGroups.Multi EQ YES NO-LOCK NO-ERROR.
    
    IF NOT AVAILABLE ttRecostBoardGroups THEN 
    DO:  
        ASSIGN opchErrorMessage = "No purchase order items can be grouped for new costs.".
        RETURN.  
    END. 
        
    /*more than one EstCostMaterial line with matching item, vendor, & size*/
    RUN pGetNewCosts.
    
    RUN RecostBoardEst_UpdateEstCostMaterial(INPUT YES,
        INPUT-OUTPUT TABLE ttEstCostMaterial BY-REFERENCE,
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
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostHeaderToCalc. 
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER TABLE FOR ttEstCostHeader.    
    
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
    
    FOR EACH ttEstCostHeaderToCalc,
        EACH ttEstCostHeader NO-LOCK     
        WHERE ttEstCostHeader.estCostHeaderID EQ ttEstCostHeaderToCalc.iEstCostHeaderID,
        EACH ttestCostMaterial NO-LOCK     
        WHERE ttEstCostMaterial.estCostHeaderID EQ ttEstCostHeader.estCostHeaderID     
        AND ttEstCostMaterial.isPrimarySubstrate
         BY ttEstCostHeader.quantityMaster BY ttEstCostMaterial.Formno:
           
        ASSIGN 
            cScoreList = ""
            iCount     = 0.           
                    
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ ttEstCostMaterial.company
            AND bf-eb.est-no    EQ ttEstCostMaterial.estimateNo
            AND bf-eb.form-no   EQ ttEstCostMaterial.Formno NO-ERROR.
            
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
                IF dScores[iCount] NE 0 THEN                
                    ASSIGN 
                        cScoreList = cScoreList + " " + string(dScores[iCount]).                    
            END.
            
            ASSIGN 
                cScoreList = TRIM(cScoreList," ").            
        END.
                        
        RUN pGetAdders(INPUT ttEstCostMaterial.company,
            INPUT ttEstCostMaterial.estimateNo,
            INPUT ttEstCostMaterial.formNo,
            OUTPUT cAdders,
            OUTPUT dAdderCost).
           
        FIND FIRST bf-Item NO-LOCK
            WHERE bf-Item.company EQ ttEstCostHeader.company
            AND bf-Item.i-no    EQ ttEstCostMaterial.itemID NO-ERROR.
                   
        IF NOT AVAILABLE bf-Item THEN 
            NEXT.          
                       
        FIND FIRST ttRecostBoardGroups 
            WHERE ttRecostBoardGroups.INo  EQ ttEstCostMaterial.itemID
            AND ttRecostBoardGroups.VendNo EQ ttEstCostMaterial.vendorID
            AND ttRecostBoardGroups.Len    EQ ttEstCostMaterial.dimLength
            AND ttRecostBoardGroups.Wid    EQ ttEstCostMaterial.dimWidth
            AND ttRecostBoardGroups.Dep    EQ ttEstCostMaterial.dimdepth
            AND ttRecostBoardGroups.Scores EQ cScoreList
            AND ttRecostBoardGroups.Adders EQ cAdders NO-ERROR.
            
        IF AVAILABLE ttRecostBoardGroups THEN 
        DO:                   
            dQty = ttEstCostMaterial.quantityRequiredTotal.                        
                        
            IF ttEstCostMaterial.quantityUOM NE ttRecostBoardGroups.TotalQtyUOM THEN 
            DO:     
                RUN Conv_QuantityFromUOMtoUOM IN hdConversionProcs(
                    INPUT ttEstCostMaterial.company,
                    INPUT ttEstCostMaterial.itemID,
                    INPUT ttRecostBoardGroups.itemType,
                    INPUT ttEstCostMaterial.quantityRequiredTotal,
                    INPUT ttEstCostMaterial.quantityUOM,
                    INPUT ttRecostBoardGroups.TotalQtyUOM,
                    INPUT ttEstCostMaterial.basisWeight,
                    INPUT ttEstCostMaterial.dimLength,
                    INPUT ttEstCostMaterial.dimWidth,
                    INPUT ttEstCostMaterial.dimdepth,
                    INPUT 0,
                    OUTPUT dQty,
                    OUTPUT loError,
                    OUTPUT chMessage).
            END.
            
            ASSIGN 
                ttRecostBoardGroups.Multi      = YES
                ttRecostBoardGroups.TotalQty   = ttRecostBoardGroups.TotalQty + dQty
                ttRecostBoardGroups.LineCount  = ttRecostBoardGroups.LineCount + 1
                ttRecostBoardGroups.FormIdList = ttRecostBoardGroups.FormIdList + "," + string(ttEstCostMaterial.formNo).                
        END.
        ELSE 
        DO:            
            CREATE ttRecostBoardGroups.
            ASSIGN
                ttRecostBoardGroups.CompanyId      = ttEstCostMaterial.company
                ttRecostBoardGroups.INo            = ttEstCostMaterial.itemID
                ttRecostBoardGroups.ItemName       = bf-Item.i-name
                ttRecostBoardGroups.VendNo         = ttEstCostMaterial.vendorID
                ttRecostBoardGroups.Len            = ttEstCostMaterial.dimLength
                ttRecostBoardGroups.Wid            = ttEstCostMaterial.dimWidth
                ttRecostBoardGroups.Dep            = ttEstCostMaterial.dimDepth 
                ttRecostBoardGroups.UOM            = ttEstCostMaterial.dimUOM                
                ttRecostBoardGroups.Scores         = cScoreList
                ttRecostBoardGroups.Adders         = cAdders
                ttRecostBoardGroups.AdderCost      = dAdderCost
                ttRecostBoardGroups.TotalQty       = ttEstCostMaterial.quantityRequiredTotal
                ttRecostBoardGroups.TotalQtyUOM    = ttEstCostMaterial.quantityUOM                
                ttRecostBoardGroups.Multi          = NO
                ttRecostBoardGroups.BasisWeight    = ttEstCostMaterial.basisWeight
                ttRecostBoardGroups.BasisWeightUOM = ttEstCostMaterial.basisWeightUOM                
                ttRecostBoardGroups.LineCount      = 1
                ttRecostBoardGroups.itemType       = IF ttEstCostMaterial.isPurchasedFG THEN "FG" ELSE "RM" 
                ttRecostBoardGroups.quantityMaster = ttEstCostHeader.quantityMaster
                ttRecostBoardGroups.customerID     = bf-Item.cust-no
                ttRecostBoardGroups.FormIdList     = STRING(ttEstCostMaterial.formNo).
        END.
        
        CREATE ttRecostBoardLineXRef.
        ASSIGN        
            ttRecostBoardLineXRef.RecostBoardGroupRowId = ROWID(ttRecostBoardGroups)
            ttRecostBoardLineXRef.EstCostMaterialID     = ttEstCostMaterial.EstCostMaterialID.     
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
    DEFINE INPUT PARAMETER iplCompareOnly AS LOGICAL NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttEstCostMaterial.
    DEFINE INPUT PARAMETER TABLE FOR ttRecostBoardGroups.

    DEFINE BUFFER bf-estCostMaterial  FOR ttEstCostMaterial.
    DEFINE VARIABLE dEstGroupCost     LIKE ttEstCostMaterial.costTotal NO-UNDO.
    DEFINE VARIABLE dNewEstLineSetup  LIKE ttEstCostMaterial.costSetup NO-UNDO.
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
        
        FIND FIRST ttestCostMaterial 
            WHERE ttestCostMaterial.EstCostMaterialID EQ ttRecostBoardLineXRef.EstCostMaterialID
              AND ttestCostMaterial.isPrimarySubstrate NO-LOCK NO-ERROR.
        
        IF AVAILABLE ttestCostMaterial THEN 
        DO:            
            ASSIGN 
                dEstGroupCost    = ttRecostBoardGroups.NewCost
                dNewEstLineSetup = ttRecostBoardGroups.NewSetup / ttRecostBoardGroups.LineCount.
             
           IF ttEstCostMaterial.costUOM NE ttRecostBoardGroups.NewCostUom THEN       
            RUN Conv_QuantityFromUOMtoUOM IN hdConversionProcs(
                INPUT ttRecostBoardGroups.company,
                INPUT ttRecostBoardGroups.INO,
                INPUT ttRecostBoardGroups.itemType,
                INPUT ttEstCostMaterial.quantityRequiredTotal,
                INPUT ttRecostBoardGroups.NewCostUom,
                INPUT ttEstCostMaterial.quantityUOM,
                INPUT ttRecostBoardGroups.BasisWeight,
                INPUT ttRecostBoardGroups.len,
                INPUT ttRecostBoardGroups.wid,
                INPUT ttRecostBoardGroups.dep,
                INPUT 0,
                OUTPUT dEstGroupCost,
                OUTPUT loError,
                OUTPUT chMessage).  
        
            IF dEstGroupCost LT ttEstCostMaterial.costTotal OR dNewEstLineSetup LT ttEstCostMaterial.costSetup THEN 
            DO:                
                ASSIGN 
                    loUpdate               = YES 
                    ttRecostBoardGroups.UpdateCost = YES.
                    
                IF NOT iplCompareOnly THEN 
                DO:  /*Update estCostMaterial*/
                    FIND CURRENT ttestCostMaterial NO-ERROR.
                    IF  ttEstCostMaterial.costPerUOM > dEstGroupCost THEN  
                        ASSIGN ttEstCostMaterial.costPerUOM = dEstGroupCost
                            ttEstCostMaterial.costTotal  = ttEstCostMaterial.quantityRequiredTotal * ttEstCostMaterial.costPerUOM.
                    IF ttEstCostMaterial.costSetup > dNewEstLineSetup THEN 
                        ASSIGN ttEstCostMaterial.costSetup = dNewEstLineSetup.                    
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
    DEFINE INPUT  PARAMETER ipchCompanyId  LIKE  ttEstCostMaterial.company.
    DEFINE INPUT  PARAMETER ipchestimateNo LIKE  ttEstCostMaterial.estimateNo.
    DEFINE INPUT  PARAMETER ipchformNo     LIKE  ttEstCostMaterial.formNo.
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