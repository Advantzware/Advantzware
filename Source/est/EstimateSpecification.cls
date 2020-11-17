 
 /*------------------------------------------------------------------------
    File        : EstimateSpecification
    Purpose     : 
    Syntax      : 
    Description : Class for modeling all elements of an estimate
    Author(s)   : BradVigrass
    Created     : Fri Sep 25 14:15:34 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS est.EstimateSpecification: 
    
    DEFINE TEMP-TABLE ttEstimate NO-UNDO 
        LIKE est
        FIELD estimateID AS CHARACTER   
        FIELD estimateType AS CHARACTER
        .
        
    DEFINE TEMP-TABLE ttEstimateBlank NO-UNDO
        LIKE eb
        FIELD estimateID AS CHARACTER 
        FIELD formNo AS INTEGER
        FIELD blankNo AS INTEGER
        FIELD dimLength AS DECIMAL 
        FIELD dimWidth AS DECIMAL 
        FIELD dimDepth AS DECIMAL
        FIELD dimUOM AS CHARACTER 
        FIELD blankLength AS DECIMAL 
        FIELD blankWidth AS DECIMAL 
        FIELD styleID AS CHARACTER 
        FIELD partID AS CHARACTER 
        FIELD quantityOnLength AS INTEGER  
        FIELD quantityOnWidth AS INTEGER 
        .
        
    DEFINE TEMP-TABLE ttEstimateForm NO-UNDO 
        LIKE ef  
        FIELD estimateID AS CHARACTER 
        FIELD formNo AS INTEGER
        FIELD boardID AS CHARACTER 
        FIELD grossSheetLength AS DECIMAL 
        FIELD grossSheetWidth AS DECIMAL
        FIELD grossSheetDepth AS DECIMAL
        FIELD netSheetLength AS DECIMAL 
        FIELD netSheetWidth AS DECIMAL
        FIELD netSheetDepth AS DECIMAL  
        FIELD dieLength AS DECIMAL 
        FIELD dieWidth AS DECIMAL
        FIELD dieDepth AS DECIMAL
        FIELD caliper AS DECIMAL 
        .  
    
    DEFINE TEMP-TABLE ttEstimateItem NO-UNDO 
        FIELD company AS CHARACTER 
        FIELD estimateID AS CHARACTER 
        FIELD partID AS CHARACTER
        FIELD partName AS CHARACTER 
        FIELD partDescription1 AS CHARACTER
        FIELD partDescription2 AS CHARACTER
        FIELD partDescription3 AS CHARACTER
        FIELD customerID AS CHARACTER
        FIELD shipToID AS CHARACTER
        .
        
    DEFINE DATA-SOURCE srcEst FOR est.
    
    DEFINE QUERY qEstimate FOR ttEstimate.
    DEFINE QUERY qEstimateBlank FOR ttEstimateBlank.
    DEFINE QUERY qEstimateForm FOR ttEstimateForm.
    DEFINE QUERY qEstimateItem FOR ttEstimateItem.
    
    DEFINE PUBLIC PROPERTY BlankCount AS INTEGER 
        GET():
            RETURN QUERY qEstimateBlank:NUM-RESULTS.
        END GET.
        PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY FormCount AS INTEGER 
        GET():
            RETURN QUERY qEstimateForm:NUM-RESULTS.
        END GET.
        PRIVATE SET.
    
    DEFINE PUBLIC PROPERTY ItemCount AS INTEGER 
        GET():
            RETURN QUERY qEstimateItem:NUM-RESULTS.
        END GET.
        PRIVATE SET.
    
    CONSTRUCTOR PUBLIC EstimateSpecification():
        
    END.
    

    METHOD PUBLIC VOID InitializeEstimate(INPUT ipcCompany AS CHARACTER, INPUT ipcEstimateID AS CHARACTER):        
         EMPTY TEMP-TABLE ttEstimate.
        
        FIND FIRST est NO-LOCK 
            WHERE est.company EQ ipcCompany
            AND est.est-no EQ ipcEstimateID
            NO-ERROR.
        IF AVAILABLE est THEN 
        DO:
            CREATE ttEstimate.
            BUFFER-COPY est TO ttEstimate.
            OPEN QUERY qEstimate PRESELECT EACH ttEstimate.
            
            FOR EACH ef OF est NO-LOCK:
                CREATE ttEstimateForm.
                BUFFER-COPY ef TO ttEstimateForm.
            END.
            OPEN QUERY qEstimateForm PRESELECT EACH ttEstimateForm.
            
            FOR EACH eb OF est NO-LOCK:
                CREATE ttEstimateBlank.
                BUFFER-COPY eb TO ttEstimateBlank.
            END.
            OPEN QUERY qEstimateBlank PRESELECT EACH ttEstimateBlank.
            
        END.
                 
    END.

    DESTRUCTOR PUBLIC EstimateSpecification():
        CLOSE QUERY qEstimate.
        CLOSE QUERY qEstimateBlank.
        CLOSE QUERY qEstimateForm.
        CLOSE QUERY qEstimateItem.
        EMPTY TEMP-TABLE ttEstimate.
        EMPTY TEMP-TABLE ttEstimateBlank.
        EMPTY TEMP-TABLE ttEstimateForm.
        EMPTY TEMP-TABLE ttEstimateItem.
    END.
    
END CLASS.