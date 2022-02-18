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
                                         
{est\ttEstCostHeaderToCalc.i}
{est\RecostBoardEst.i}
{system\VendorCostProcs.i}
{system\FormulaProcs.i}

DEFINE INPUT PARAMETER ipinEstCostHeaderID LIKE ttEstCostHeaderToCalc.iEstCostHeaderID.
DEFINE INPUT PARAMETER iplMessage AS LOG NO-UNDO. /* YES will prompt a summary message*/

{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEFINE VARIABLE glUpdate AS LOG     NO-UNDO.
DEFINE VARIABLE k_frac   AS DECIMAL INIT "6.25" NO-UNDO.

ASSIGN 
    cocode = g_company
    locode = g_loc.
{sys/inc/f16to32.i}

/* ***************************  Main Block  *************************** */

glUpdate = NO.

RUN ProcessEstCostMaterial. /*Build internal temp-tables*/

FIND FIRST ttEstGroups WHERE ttEstGroups.Multi EQ YES NO-LOCK NO-ERROR.
IF AVAILABLE ttEstGroups THEN 
DO:     
    /*more than one EstCostMaterial line with matching item, vendor, & size*/
    RUN GetNewCostsN.    
    
    RUN UpdateEstCostMaterial(YES).  /*See if cost is better than current EstCostMaterial, YES = Compare only*/
    
    IF glUpdate THEN 
    DO: /*If Better costs were found*/
        IF iplMessage THEN RUN ShowCostUpdates. /*Present updates to user and get confirmation*/
        RUN UpdateEstCostMaterial(NO).  /*Update the EstCostMaterial costs with better costs*/
    END.    
    ELSE
        IF iplMessage THEN 
            MESSAGE "No board cost or setup improvements available after grouping purchase order items."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.  /*Avail ttEstGroups*/
ELSE
    IF iplMessage THEN 
        MESSAGE "No purchase order items can be grouped for new costs."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.


/* **********************  Internal Procedures  *********************** */

PROCEDURE GetNewCostsN:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
   
    DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    
    FOR EACH ttEstGroups 
        WHERE ttEstGroups.Multi:
        
        RUN GetVendorCost(cocode, 
            ttEstGroups.INo, 
            ttEstGroups.itemType, 
            ttEstGroups.VendNo, 
            ttEstGroups.customerID, 
            "", 
            0, 
            0,
            ttEstGroups.TotalQty, 
            ttEstGroups.TotalQtyUOM,
            ttEstGroups.Len, 
            ttEstGroups.Wid, 
            ttEstGroups.Dep, 
            ttEstGroups.UOM, 
            ttEstGroups.BasisWeight, 
            ttEstGroups.BasisWeightUOM, 
            NO,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT lError, 
            OUTPUT cMessage).
   
        ASSIGN 
            ttEstGroups.NewCost    = dCostPerUOM + ttEstGroups.AdderCost
            ttEstGroups.NewCostUOM = cCostUOM
            ttEstGroups.NewSetup   = dCostSetup.

    END. /*each ttEstGroups*/

END PROCEDURE.



PROCEDURE ProcessEstCostMaterial :
    /*------------------------------------------------------------------------------
      Purpose: Build Temp tables for processing    
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-Item FOR ITEM.
    DEFINE BUFFER bf-eb   FOR eb.        

    DEFINE VARIABLE dQty           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cScores        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cAdders        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dAdderCost     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hdFormulaProcs AS HANDLE    NO-UNDO.

    FOR EACH ttEstGroups:
        DELETE ttEstGroups.
    END.
    FOR EACH ttEstLineXref:
        DELETE ttEstLineXref.
    END.    
    
    FOR EACH estCostHeader NO-LOCK     
        WHERE estCostHeader.estCostHeaderID EQ ipinEstCostHeaderID,
        EACH estCostMaterial NO-LOCK     
        WHERE estCostMaterial.estCostHeaderID EQ estCostHeader.estCostHeaderID     
        AND estCostMaterial.isPrimarySubstrate:
            
        ASSIGN 
            cScores = "".
            
        IF VALID-HANDLE(hdFormulaProcs) = FALSE THEN
            RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
            
        FIND FIRST bf-eb NO-LOCK
            WHERE bf-eb.company EQ estCostMaterial.company
            AND bf-eb.est-no    EQ estCostMaterial.estimateNo
            AND bf-eb.form-no   EQ estCostMaterial.Formno NO-ERROR.
        
        IF AVAILABLE bf-eb THEN
            FIND FIRST style NO-LOCK
                WHERE style.company EQ bf-eb.company
                AND style.style   EQ bf-eb.style
                NO-ERROR.           
            
        IF AVAILABLE bf-eb AND AVAILABLE style THEN
        DO:
            RUN Formula_ParseDesignScores IN hdFormulaProcs (
                INPUT bf-eb.company,
                INPUT bf-eb.est-no,
                INPUT bf-eb.form-no,
                INPUT bf-eb.blank-no,
                INPUT style.designIDAlt,
                INPUT NO,
                OUTPUT TABLE ttScoreLine).
            
            FOR EACH ttScoreLine 
                WHERE ttScoreLine.PanelType = "W" BY ttScoreLine.LineNum:
                
                ASSIGN 
                    cScores = cScores + "," + string(ttScoreLine.ScoreLine).                
            END.
            
            ASSIGN 
                cScores = TRIM(cScores,",").
        END.
                        
        RUN GetAdders(INPUT estCostMaterial.company,
            INPUT estCostMaterial.estimateNo,
            INPUT estCostMaterial.formNo,
            OUTPUT cAdders,
            OUTPUT dAdderCost).
           
        FIND FIRST bf-Item NO-LOCK
            WHERE bf-Item.company EQ estCostHeader.company
            AND bf-Item.i-no    EQ estCostMaterial.itemID NO-ERROR.
                   
        IF NOT AVAILABLE bf-Item THEN 
            NEXT.          
                       
        FIND FIRST ttEstGroups 
            WHERE ttEstGroups.INo          EQ estCostMaterial.itemID
            AND ttEstGroups.VendNo         EQ estCostMaterial.vendorID
            AND ttEstGroups.Len            EQ bf-eb.len
            AND ttEstGroups.Wid            EQ bf-eb.wid
            AND ttEstGroups.Dep            EQ bf-eb.dep
            AND ttEstGroups.Scores         EQ cScores
            AND ttEstGroups.Adders         EQ cAdders
            AND ttEstGroups.quantityMaster EQ estCostHeader.quantityMaster NO-ERROR.
            
        IF AVAILABLE ttEstGroups THEN 
        DO:                   
            dQty = estCostMaterial.quantityRequiredTotal.
                        
            IF estCostMaterial.quantityUOM NE ttEstGroups.TotalQtyUOM THEN 
            DO:
                /*convert uom so that it can be summed with existing qty*/
                RUN sys/ref/convquom.p (INPUT estCostMaterial.quantityUOM,
                    INPUT ttEstGroups.TotalQtyUOM,
                    INPUT estCostMaterial.basisWeight,
                    INPUT bf-eb.len,
                    INPUT bf-eb.wid,
                    INPUT bf-eb.dep,
                    INPUT estCostMaterial.quantityRequiredTotal,
                    OUTPUT dQty).
            END.
            ASSIGN 
                ttEstGroups.Multi      = YES
                ttEstGroups.TotalQty   = ttEstGroups.TotalQty + dQty
                ttEstGroups.LineCount  = ttEstGroups.LineCount + 1
                ttEstGroups.FormIdList = ttEstGroups.FormIdList + "," + string(estCostMaterial.formNo).            
        END.
        ELSE 
        DO:
            CREATE ttEstGroups.
            ASSIGN
                ttEstGroups.INo            = estCostMaterial.itemID
                ttEstGroups.VendNo         = estCostMaterial.vendorID
                ttEstGroups.Len            = bf-eb.len
                ttEstGroups.Wid            = bf-eb.wid
                ttEstGroups.Dep            = bf-eb.dep 
                ttEstGroups.UOM            = estCostMaterial.dimUOM                
                ttEstGroups.Scores         = cScores
                ttEstGroups.Adders         = cAdders
                ttEstGroups.AdderCost      = dAdderCost
                ttEstGroups.TotalQty       = estCostMaterial.quantityRequiredTotal
                ttEstGroups.TotalQtyUOM    = estCostMaterial.quantityUOM                
                ttEstGroups.Multi          = NO
                ttEstGroups.BasisWeight    = estCostMaterial.basisWeight
                ttEstGroups.BasisWeightUOM = estCostMaterial.basisWeightUOM                
                ttEstGroups.LineCount      = 1
                ttEstGroups.itemType       = IF estCostMaterial.isPurchasedFG THEN "FG" ELSE "RM" 
                ttEstGroups.quantityMaster = estCostHeader.quantityMaster
                ttEstGroups.customerID     = bf-Item.cust-no
                ttEstGroups.FormIdList     = STRING(estCostMaterial.formNo).
        END.
        
        CREATE ttEstLineXRef.
        ASSIGN        
            ttEstLineXRef.EstGroupRowId        = ROWID(ttEstGroups)
            ttEstLineXRef.estCostMaterialRowID = ROWID(estCostMaterial).             
    
    END.
    DELETE PROCEDURE hdFormulaProcs.
    
END PROCEDURE.

PROCEDURE ShowCostUpdates :
    /*------------------------------------------------------------------------------
      Purpose: Present Results to the user
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lUpdateCost AS LOG NO-UNDO.

    FOR EACH ttEstGroups
        WHERE ttEstGroups.Multi
        AND ttEstGroups.UpdateCost:

        MESSAGE "A reduced cost of " ttEstGroups.NewCost " " ttEstGroups.NewCostUOM " was found." SKIP
            "Total Qty: " ttEstGroups.TotalQty " " ttEstGroups.TotalQtyUom SKIP
            "Item #: " ttEstGroups.INo SKIP
            "Vendor: " ttEstGroups.VendNo SKIP
            "Width: " ttEstGroups.Wid SKIP
            "Length: " ttEstGroups.Len SKIP
            "Scores: " ttEstGroups.Scores SKIP 
            "Adders: " ttEstGroups.Adders SKIP(1)
            "Do you want to apply this cost to the order lines?"
            VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE lUpdateCost.
        ttEstGroups.UpdateCost = lUpdateCost.

    END. /*each ttEstGroups*/
END PROCEDURE.


PROCEDURE UpdateEstCostMaterial :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER iplCompareOnly AS LOG NO-UNDO.

    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE VARIABLE xEstGroupCost    LIKE estCostMaterial.costTotal NO-UNDO.
    DEFINE VARIABLE xNewEstLineSetup LIKE estCostMaterial.costSetup NO-UNDO.
    DEFINE VARIABLE cAdders          AS CHARACTER NO-UNDO.

    FOR EACH ttEstGroups
        WHERE ttEstGroups.Multi
        AND (iplCompareOnly OR ttEstGroups.UpdateCost),
        EACH ttEstLineXref
        WHERE ttEstLineXref.EstGroupRowId EQ ROWID(ttEstGroups) NO-LOCK:
        FIND FIRST estCostMaterial 
            WHERE ROWID(estCostMaterial) EQ ttEstLineXref.estCostMaterialRowID NO-LOCK NO-ERROR.
        
        IF AVAILABLE estCostMaterial THEN 
        DO:
            ASSIGN 
                xEstGroupCost    = ttEstGroups.NewCost
                xNewEstLineSetup = ttEstGroups.NewSetup / ttEstGroups.LineCount.                
                       
            IF estCostMaterial.costUOM NE ttEstGroups.NewCostUom THEN
                RUN sys/ref/convcuom.p(INPUT ttEstGroups.NewCostUom,
                    INPUT estCostMaterial.quantityUOM,
                    INPUT ttEstGroups.BasisWeight,
                    INPUT ttEstGroups.Len,
                    INPUT ttEstGroups.Wid,
                    INPUT 0,
                    INPUT xEstGroupCost, 
                    OUTPUT xEstGroupCost).
        
            IF xEstGroupCost LT estCostMaterial.costTotal OR xNewEstLineSetup LT estCostMaterial.costSetup THEN 
            DO:
                ASSIGN 
                    glUpdate               = YES 
                    ttEstGroups.UpdateCost = YES.
                    
                IF NOT iplCompareOnly THEN 
                DO:  /*Update estCostMaterial*/
                    FIND CURRENT estCostMaterial EXCLUSIVE-LOCK.
                    IF  estCostMaterial.costPerUOM > xEstGroupCost THEN  
                        ASSIGN estCostMaterial.costPerUOM = xEstGroupCost
                            estCostMaterial.costTotal  = estCostMaterial.quantityRequiredTotal * estCostMaterial.costPerUOM.
                    IF estCostMaterial.costSetup > xNewEstLineSetup THEN 
                        ASSIGN estCostMaterial.costSetup = xNewEstLineSetup.
                    FIND CURRENT estCostMaterial NO-LOCK.
                    
                END.
            END.
        END. /*avail bf-estCostMaterial*/
    END. /*Each tt*/
    
END PROCEDURE.

PROCEDURE GetAdders :
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
                lAvailAdder = TRUE.
            opcAdders = opcAdders + "," + bf-ef.adder[iCount].        
        END.
        
        ASSIGN 
            opcAdders = TRIM(opcAdders,",").        
    
    END. /*avail bf-ef*/
END PROCEDURE.