/*------------------------------------------------------------------------
    File        : api/JobCard.p
    Purpose     : Returns the request data for receipt

    Syntax      :

    Description : Returns the request data for receipt

    Author(s)   : DEVA$!
    Created     : Fri Jun 04 07:04:19 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    
    DEFINE TEMP-TABLE ttEstCostForm
        LIKE EstCostForm
        BEFORE-TABLE bttEstCostForm.
        
    DEFINE TEMP-TABLE ttEstCostBlank
        LIKE EstCostBlank
        BEFORE-TABLE bttEstCostBlank.

    DEFINE TEMP-TABLE ttEstCostMaterial
        LIKE EstCostMaterial
        BEFORE-TABLE bttEstCostMaterial.
        
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iEstCostHeaderID      AS INT64    NO-UNDO.
    DEFINE VARIABLE lcEstCostHeaderData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostFormData     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostBlankData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostMaterialData AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcConcatEstCostFormData     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostBlankData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostMaterialData AS LONGCHAR NO-UNDO.
        
    DEFINE VARIABLE oEstCostHeader   AS entities.EstCostHeader   NO-UNDO.
    DEFINE VARIABLE oEstCostForm     AS entities.EstCostForm     NO-UNDO.
    DEFINE VARIABLE oEstCostBlank    AS entities.EstCostBlank    NO-UNDO.
    DEFINE VARIABLE oEstCostMaterial AS entities.EstCostMaterial NO-UNDO.
    
    DEFINE BUFFER bf-header-APIOutboundDetail   FOR APIOutboundDetail.
    DEFINE BUFFER bf-form-APIOutboundDetail     FOR APIOutboundDetail.
    DEFINE BUFFER bf-blank-APIOutboundDetail    FOR APIOutboundDetail.
    DEFINE BUFFER bf-material-APIOutboundDetail FOR APIOutboundDetail.
    
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
                 
    IF ipcRequestHandler NE "" THEN 
        RUN VALUE(ipcRequestHandler) (
            INPUT  TABLE ttArgs,
            INPUT  ipiAPIOutboundID,
            INPUT  ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType  = "ROWID"
               AND ttArgs.argKey   = "EstCostHeaderID"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid Estimate Header ID"
                .
            
            RETURN.
        END.                    
        
        iEstCostHeaderID = INT64(ttArgs.argValue).
        
        RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).

        FIND FIRST bf-header-APIOutboundDetail NO-LOCK
             WHERE bf-header-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-header-APIOutboundDetail.detailID      EQ "EstCostHeader"
               AND bf-header-APIOutboundDetail.parentID      EQ "JobCard"
             NO-ERROR.
                     
        FIND FIRST bf-form-APIOutboundDetail NO-LOCK
             WHERE bf-form-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-form-APIOutboundDetail.detailID      EQ "EstCostForm"
               AND bf-form-APIOutboundDetail.parentID      EQ "EstCostHeader"
             NO-ERROR.

        FIND FIRST bf-blank-APIOutboundDetail NO-LOCK
             WHERE bf-blank-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-blank-APIOutboundDetail.detailID      EQ "EstCostBlank"
               AND bf-blank-APIOutboundDetail.parentID      EQ "EstCostForm"
             NO-ERROR.

        FIND FIRST bf-material-APIOutboundDetail NO-LOCK
             WHERE bf-material-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
               AND bf-material-APIOutboundDetail.detailID      EQ "EstCostMaterial"
               AND bf-material-APIOutboundDetail.parentID      EQ "EstCostBlank"
             NO-ERROR.
                                  
        oEstCostHeader = NEW entities.EstCostHeader(iEstCostHeaderID).
        
        IF AVAILABLE bf-header-APIOutboundDetail THEN
            lcEstCostHeaderData = bf-header-APIOutboundDetail.data.

        IF AVAILABLE bf-form-APIOutboundDetail THEN
            lcEstCostFormData = bf-form-APIOutboundDetail.data.

        IF AVAILABLE bf-blank-APIOutboundDetail THEN
            lcEstCostBlankData = bf-blank-APIOutboundDetail.data.

        IF AVAILABLE bf-material-APIOutboundDetail THEN
            lcEstCostMaterialData = bf-material-APIOutboundDetail.data.
        
        oEstCostHeader:LoadForms(iEstCostHeaderID).
        
        oEstCostForm = oEstCostHeader:GetFormObject().
        oEstCostBlank = oEstCostForm:GetBlankObject().
        oEstCostMaterial = oEstCostBlank:GetMaterialObject().
        
        oEstCostForm:SetRequestDataAndType (lcEstCostFormData, gcRequestDataType).
        oEstCostBlank:SetRequestDataAndType (lcEstCostBlankData, gcRequestDataType).
        oEstCostMaterial:SetRequestDataAndType (lcEstCostMaterialData, gcRequestDataType).

        oEstCostForm:GetForms (OUTPUT TABLE ttEstCostForm).
        
        FOR EACH ttEstCostForm:
            lcEstCostFormData = oEstCostForm:ReplaceEntityAttributes(BUFFER ttEstCostForm:HANDLE).
            
            oEstCostForm:LoadBlanks(ttEstCostForm.estCostFormID).

            oEstCostBlank:GetBlanks (OUTPUT TABLE ttEstCostBlank).
            
            FOR EACH ttEstCostBlank:
                lcConcatEstCostBlankData = oEstCostBlank:ReplaceEntityAttributes(BUFFER ttEstCostBlank:HANDLE).

                oEstCostBlank:LoadMaterials(ttEstCostBlank.estCostBlankID).

                lcConcatEstCostMaterialData = oEstCostMaterial:UpdateRequestData( ).
    
                lcEstCostBlankData = REPLACE(lcConcatEstCostBlankData, "$EstCostMaterial$", lcConcatEstCostMaterialData).
                
                lcConcatEstCostBlankData = lcConcatEstCostBlankData + lcEstCostBlankData.    
            END.
            
            lcEstCostFormData = REPLACE(lcEstCostFormData, "$EstCostBlank$", lcConcatEstCostBlankData).
            
            lcConcatEstCostFormData = lcConcatEstCostFormData + lcEstCostFormData.    
        END.
        
        oEstCostHeader:SetRequestDataAndType(lcEstCostHeaderData, gcRequestDataType).
        
        lcEstCostHeaderData = oEstCostHeader:UpdateRequestData().
        
        lcEstCostHeaderData = REPLACE(lcEstCostHeaderData, "$EstCostForm$", lcConcatEstCostFormData).
        
        ioplcRequestData = REPLACE(ioplcRequestData, "$EstCostHeader$", lcEstCostHeaderData).

        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.

    FINALLY:
        IF VALID-OBJECT (oEstCostHeader) THEN
            DELETE OBJECT oEstCostHeader.
            	
    END FINALLY.
    