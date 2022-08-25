/*------------------------------------------------------------------------
    File        : api/SendEstimate.p
    Purpose     : Returns the request data for an estimate

    Syntax      :

    Description : Returns the request data for an estimate

    Author(s)   : DEVA$!
    Created     : Thu Jul 7 01:40:32 EDT 2022
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    {api/CommonAPIProcs.i}
    {est/ttEstimateCalc.i}
    
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.    
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.

    DEFINE VARIABLE lcEstCostHeader    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostForm      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostDetail    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostSummary   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostOperation AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostMisc      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostMaterial  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostItem      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostBlank     AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcEstCostHeaderData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostFormData      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostDetailData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostSummaryData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostOperationData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostMiscData      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostMaterialData  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostItemData      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcEstCostBlankData     AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcConcatEstCostHeader    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostForm      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostDetail    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostSummary   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostOperation AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostMisc      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostMaterial  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostItem      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatEstCostBlank     AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcHeader          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcForm            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlank           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcQuantity        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPrep            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperation       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignLine   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcProbe           AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcHeaderData          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormData            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankData           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcQuantityData        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPrepData            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignHeaderData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignLineData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcProbeData           AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcConcatHeader          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatForm            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBlank           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatQuantity        AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatPrep            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatOperation       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBoxDesignHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBoxDesignLine   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatProbe           AS LONGCHAR NO-UNDO.

    DEFINE BUFFER bf-estCostForm      FOR estCostForm.
    DEFINE BUFFER bf-estCostDetail    FOR estCostDetail.
    DEFINE BUFFER bf-estCostSummary   FOR estCostSummary.
    DEFINE BUFFER bf-estCostOperation FOR estCostOperation.
    DEFINE BUFFER bf-estCostMisc      FOR estCostMisc.
    DEFINE BUFFER bf-estCostMaterial  FOR estCostMaterial.
    DEFINE BUFFER bf-estCostItem      FOR estCostItem.
    DEFINE BUFFER bf-estCostBlank     FOR estCostBlank.
    DEFINE BUFFER bf-estCostHeader    FOR estCostHeader.

    DEFINE BUFFER bf-est              FOR est.
    DEFINE BUFFER bf-ef               FOR ef.
    DEFINE BUFFER bf-eb               FOR eb.
    DEFINE BUFFER bf-est-qty          FOR est-qty.
    DEFINE BUFFER bf-est-prep         FOR est-prep.
    DEFINE BUFFER bf-est-op           FOR est-op.
    DEFINE BUFFER bf-box-design-hdr   FOR box-design-hdr.
    DEFINE BUFFER bf-box-design-line  FOR box-design-line.
    DEFINE BUFFER bf-probe            FOR probe.
           
    RUN pUpdateRequestDataType(INPUT ipiAPIOutboundID).
        
    IF ipcRequestHandler NE "" THEN
        RUN VALUE(ipcRequestHandler) (
            INPUT TABLE  ttArgs,
            INPUT ipiAPIOutboundID,
            INPUT ipiAPIOutboundTriggerID,
            INPUT-OUTPUT ioplcRequestData,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).
    ELSE DO:
        FIND FIRST ttArgs
             WHERE ttArgs.argType EQ "ROWID"
               AND ttArgs.argKey  EQ "est"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                opcMessage = "No valid est record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.
        
        FIND FIRST bf-est NO-LOCK
            WHERE ROWID(bf-est) EQ TO-ROWID(ttArgs.argValue) 
            NO-ERROR.
        IF NOT AVAILABLE bf-est THEN DO:
            ASSIGN
                opcMessage = "No valid est record passed to handler"
                oplSuccess = FALSE
                .
            RETURN.
        END.         

        oAttribute = NEW system.Attribute().
        oAttribute:RequestDataType = gcRequestDataType.

        RUN pGetRequestData ("SendEstimate", "Header", OUTPUT lcHeaderData).
        RUN pGetRequestData ("Header", "Form", OUTPUT lcFormData).
        RUN pGetRequestData ("Header", "Blank", OUTPUT lcBlankData).
        RUN pGetRequestData ("Header", "Prep", OUTPUT lcPrepData).
        RUN pGetRequestData ("Header", "BoxDesignHeader", OUTPUT lcBoxDesignHeaderData).
        RUN pGetRequestData ("Header", "BoxDesignLine", OUTPUT lcBoxDesignLineData).
        RUN pGetRequestData ("Header", "Quantity", OUTPUT lcQuantityData).
        RUN pGetRequestData ("Header", "Probe", OUTPUT lcProbeData).
        RUN pGetRequestData ("Header", "Operation", OUTPUT lcOperationData).
        
        
        FOR EACH bf-estCostHeader NO-LOCK
             WHERE bf-estCostHeader.company    EQ bf-est.company
               AND bf-estCostHeader.estimateNo EQ bf-est.est-no
             BY bf-estCostHeader.calcDateTime DESCENDING:
            LEAVE.
        END.

        FOR EACH bf-ef NO-LOCK
            WHERE bf-ef.company EQ bf-est.company
              AND bf-ef.est-no  EQ bf-est.est-no:
            ASSIGN
                lcForm = lcFormData
                .
            
            IF lcForm NE "" THEN    
                lcForm = oAttribute:ReplaceAttributes(lcForm, BUFFER bf-ef:HANDLE).
            
            lcConcatForm = lcConcatForm + lcForm.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatForm, gcRequestDataType).

        FOR EACH bf-eb NO-LOCK
            WHERE bf-eb.company EQ bf-est.company
              AND bf-eb.est-no  EQ bf-est.est-no:
            ASSIGN
                lcBlank = lcBlankData
                .
            
            IF lcBlank NE "" THEN    
                lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER bf-eb:HANDLE).
            
            lcConcatBlank = lcConcatBlank + lcBlank.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatBlank, gcRequestDataType).

        FOR EACH bf-est-qty NO-LOCK
            WHERE bf-est-qty.company EQ bf-est.company
              AND bf-est-qty.est-no  EQ bf-est.est-no:
            ASSIGN
                lcQuantity = lcQuantityData
                .
            
            IF lcQuantity NE "" THEN    
                lcQuantity = oAttribute:ReplaceAttributes(lcQuantity, BUFFER bf-est-qty:HANDLE).
            
            lcConcatQuantity = lcConcatQuantity + lcQuantity.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatQuantity, gcRequestDataType).

        FOR EACH bf-est-prep NO-LOCK
            WHERE bf-est-prep.company EQ bf-est.company
              AND bf-est-prep.est-no  EQ bf-est.est-no:
            ASSIGN
                lcPrep = lcPrepData
                .
            
            IF lcPrep NE "" THEN    
                lcPrep = oAttribute:ReplaceAttributes(lcPrep, BUFFER bf-est-prep:HANDLE).
            
            lcConcatPrep = lcConcatPrep + lcPrep.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatPrep, gcRequestDataType).

        FOR EACH bf-est-op NO-LOCK
            WHERE bf-est-op.company EQ bf-est.company
              AND bf-est-op.est-no  EQ bf-est.est-no:
            ASSIGN
                lcOperation = lcOperationData
                .
            
            IF lcOperation NE "" THEN    
                lcOperation = oAttribute:ReplaceAttributes(lcOperation, BUFFER bf-est-op:HANDLE).
            
            lcConcatOperation = lcConcatOperation + lcOperation.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatOperation, gcRequestDataType).

        FOR EACH bf-probe NO-LOCK
            WHERE bf-probe.company EQ bf-est.company
              AND bf-probe.est-no  EQ bf-est.est-no:
            ASSIGN
                lcProbe = lcProbeData
                .
            
            IF lcProbe NE "" THEN    
                lcProbe = oAttribute:ReplaceAttributes(lcProbe, BUFFER bf-probe:HANDLE).
            
            lcConcatProbe = lcConcatProbe + lcProbe.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatProbe, gcRequestDataType).

        FOR EACH bf-box-design-hdr NO-LOCK
            WHERE bf-box-design-hdr.company EQ bf-est.company
              AND bf-box-design-hdr.est-no  EQ bf-est.est-no:
            ASSIGN
                lcBoxDesignHeader = lcBoxDesignHeaderData
                .
            
            IF lcBoxDesignHeader NE "" THEN    
                lcBoxDesignHeader = oAttribute:ReplaceAttributes(lcBoxDesignHeader, BUFFER bf-box-design-hdr:HANDLE).
            
            lcConcatBoxDesignHeader = lcConcatBoxDesignHeader + lcBoxDesignHeader.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatBoxDesignHeader, gcRequestDataType).

        FOR EACH bf-box-design-line NO-LOCK
            WHERE bf-box-design-line.company EQ bf-est.company
              AND bf-box-design-line.est-no  EQ bf-est.est-no:
            ASSIGN
                lcBoxDesignLine = lcBoxDesignLineData
                .
            
            IF lcBoxDesignLine NE "" THEN    
                lcBoxDesignLine = oAttribute:ReplaceAttributes(lcBoxDesignLine, BUFFER bf-box-design-line:HANDLE).
            
            lcConcatBoxDesignLine = lcConcatBoxDesignLine + lcBoxDesignLine.                                 
        END.                
        
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatBoxDesignLine, gcRequestDataType).

        ASSIGN
            lcHeader = lcHeaderData
            .
        
        IF lcHeader NE "" THEN    
            lcHeader = oAttribute:ReplaceAttributes(lcHeader, BUFFER bf-est:HANDLE).
        
        lcHeader = REPLACE(lcHeader, "$Forms$", lcConcatForm).
        lcHeader = REPLACE(lcHeader, "$Blanks$", lcConcatBlank).
        lcHeader = REPLACE(lcHeader, "$Preps$", lcConcatPrep).
        lcHeader = REPLACE(lcHeader, "$Operations$", lcConcatOperation).
        lcHeader = REPLACE(lcHeader, "$Quantities$", lcConcatQuantity).
        lcHeader = REPLACE(lcHeader, "$Probes$", lcConcatProbe).
        lcHeader = REPLACE(lcHeader, "$BoxDesignHeaders$", lcConcatBoxDesignHeader).
        lcHeader = REPLACE(lcHeader, "$BoxDesignLines$", lcConcatBoxDesignLine).

        lcConcatHeader = lcConcatHeader + lcHeader.                                 

        IF AVAILABLE bf-estCostHeader THEN DO:
            RUN pGetRequestData ("SendEstimate", "EstCostHeader", OUTPUT lcEstCostHeaderData).
            RUN pGetRequestData ("EstCostHeader", "EstCostForm", OUTPUT lcEstCostFormData).
            RUN pGetRequestData ("EstCostHeader", "EstCostDetail", OUTPUT lcEstCostDetailData).
            RUN pGetRequestData ("EstCostHeader", "EstCostSummary", OUTPUT lcEstCostSummaryData).
            RUN pGetRequestData ("EstCostHeader", "EstCostOperation", OUTPUT lcEstCostOperationData).
            RUN pGetRequestData ("EstCostHeader", "EstCostMisc", OUTPUT lcEstCostMiscData).
            RUN pGetRequestData ("EstCostHeader", "EstCostMaterial", OUTPUT lcEstCostMaterialData).
            RUN pGetRequestData ("EstCostHeader", "EstCostItem", OUTPUT lcEstCostItemData).
            RUN pGetRequestData ("EstCostHeader", "EstCostBlank", OUTPUT lcEstCostBlankData).

            FOR EACH bf-estCostForm NO-LOCK
                WHERE bf-estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostForm = lcEstCostFormData
                    .
                
                IF lcEstCostForm NE "" THEN    
                    lcEstCostForm = oAttribute:ReplaceAttributes(lcEstCostForm, BUFFER bf-estCostForm:HANDLE).
                
                lcConcatEstCostForm = lcConcatEstCostForm + lcEstCostForm.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostForm, gcRequestDataType).
                        
            FOR EACH bf-estCostBlank NO-LOCK
                WHERE bf-estCostBlank.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostBlank = lcEstCostBlankData
                    .
                
                IF lcEstCostBlank NE "" THEN 
                    lcEstCostBlank = oAttribute:ReplaceAttributes(lcEstCostBlank, BUFFER bf-estCostBlank:HANDLE).
                
                lcConcatEstCostBlank = lcConcatEstCostBlank + lcEstCostBlank.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostBlank, gcRequestDataType).
            
            FOR EACH bf-estCostItem NO-LOCK
                WHERE bf-estCostItem.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostItem = lcEstCostItemData
                    .
                    
                IF lcEstCostItem NE "" THEN
                    lcEstCostItem = oAttribute:ReplaceAttributes(lcEstCostItem, BUFFER bf-estCostItem:HANDLE).
                
                lcConcatEstCostItem = lcConcatEstCostItem + lcEstCostItem.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostItem, gcRequestDataType).
                    
            FOR EACH bf-estCostDetail NO-LOCK
                WHERE bf-estCostDetail.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostDetail = lcEstCostDetailData
                    .
                
                IF lcEstCostDetail NE "" THEN
                    lcEstCostDetail = oAttribute:ReplaceAttributes(lcEstCostDetail, BUFFER bf-estCostDetail:HANDLE).
                
                lcConcatEstCostDetail = lcConcatEstCostDetail + lcEstCostDetail.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostDetail, gcRequestDataType).
                     
            FOR EACH bf-estCostOperation NO-LOCK
                WHERE bf-estCostOperation.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostOperation = lcEstCostOperationData
                    .
                
                IF lcEstCostOperation NE "" THEN
                    lcEstCostOperation = oAttribute:ReplaceAttributes(lcEstCostOperation, BUFFER bf-estCostOperation:HANDLE).
                
                lcConcatEstCostOperation = lcConcatEstCostOperation + lcEstCostOperation.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostOperation, gcRequestDataType).
                    
            FOR EACH bf-estCostMaterial NO-LOCK
                WHERE bf-estCostMaterial.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostMaterial = lcEstCostMaterialData
                    .
                
                IF lcEstCostMaterial NE "" THEN
                    lcEstCostMaterial = oAttribute:ReplaceAttributes(lcEstCostMaterial, BUFFER bf-estCostMaterial:HANDLE).
                
                lcConcatEstCostMaterial = lcConcatEstCostMaterial + lcEstCostMaterial.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostMaterial, gcRequestDataType).
                    
            FOR EACH bf-estCostMisc NO-LOCK
                WHERE bf-estCostMisc.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
                ASSIGN
                    lcEstCostMisc = lcEstCostMiscData
                    .
                
                IF lcEstCostMisc NE "" THEN
                    lcEstCostMisc = oAttribute:ReplaceAttributes(lcEstCostMisc, BUFFER bf-estCostMisc:HANDLE).
                
                lcConcatEstCostMisc = lcConcatEstCostMisc + lcEstCostMisc.                                 
            END.                
    
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostMisc, gcRequestDataType).
            
            FOR EACH bf-estCostSummary NO-LOCK
                WHERE bf-estCostSummary.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                USE-INDEX estHeader:
                ASSIGN
                    lcEstCostSummary = lcEstCostSummaryData
                    .
                
                IF lcEstCostSummary NE "" THEN
                    lcEstCostSummary = oAttribute:ReplaceAttributes(lcEstCostSummary, BUFFER bf-estCostSummary:HANDLE).
                
                lcConcatEstCostSummary = lcConcatEstCostSummary + lcEstCostSummary.                                 
            END.                
            
            RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostSummary, gcRequestDataType).

            ASSIGN
                lcEstCostHeader = lcEstCostHeaderData
                .
            
            IF lcEstCostHeader NE "" THEN    
                lcEstCostHeader = oAttribute:ReplaceAttributes(lcEstCostHeader, BUFFER bf-estCostHeader:HANDLE).
            
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostForms$", lcConcatEstCostForm).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostDetails$", lcConcatEstCostDetail).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostSummaries$", lcConcatEstCostSummary).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostOperations$", lcConcatEstCostOperation).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostMiscs$", lcConcatEstCostMisc).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostMaterials$", lcConcatEstCostMaterial).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostItems$", lcConcatEstCostItem).
            lcEstCostHeader = REPLACE(lcEstCostHeader, "$EstCostBlanks$", lcConcatEstCostBlank).

            lcConcatEstCostHeader = lcConcatEstCostHeader + lcEstCostHeader.                                 
        END.

        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatEstCostHeader, gcRequestDataType).
        RUN pUpdateDelimiter(INPUT-OUTPUT lcConcatHeader, gcRequestDataType).
                    
        ioplcRequestData = oAttribute:ReplaceAttributes(ioplcRequestData, BUFFER bf-estCostHeader:HANDLE).

        ioplcRequestData = REPLACE(ioplcRequestData, "$EstCostHeaders$", lcConcatEstCostHeader).

        ioplcRequestData = REPLACE(ioplcRequestData, "$Headers$", lcConcatHeader).
        
        ASSIGN   
            opcMessage       = ""
            oplSuccess       = TRUE
            .
    END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcParentID     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.
        
    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ ipcParentID
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

END PROCEDURE.
