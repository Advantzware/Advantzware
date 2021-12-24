/*------------------------------------------------------------------------
    File        : api/JobTicket.p
    Purpose     : Returns the request data for receipt

    Syntax      :

    Description : Returns the request data for receipt

    Author(s)   : DEVA$!
    Created     : Fri Jun 04 07:04:19 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

    DEFINE TEMP-TABLE ttEstCostHeaderID NO-UNDO
        FIELD estCostHeaderID AS INT64
        FIELD riJobHeader     AS ROWID
        .
        
    DEFINE TEMP-TABLE ttEstCostHeader 
        LIKE EstCostheader
        BEFORE-TABLE bttEstCostHeader.
            
    DEFINE TEMP-TABLE ttEstCostForm
        LIKE EstCostForm
        BEFORE-TABLE bttEstCostForm.
        
    DEFINE TEMP-TABLE ttEstCostBlank
        LIKE EstCostBlank
        BEFORE-TABLE bttEstCostBlank.

    DEFINE TEMP-TABLE ttEstCostMaterial
        LIKE EstCostMaterial
        BEFORE-TABLE bttEstCostMaterial.
    
    DEFINE TEMP-TABLE ttEstCostOperation
        LIKE EstCostOperation
        BEFORE-TABLE bttEstCostOperation.

    {api/ttArgs.i}
            
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    {api/CommonAPIProcs.i}
    
    DEFINE VARIABLE mptrTTReceipt          AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE hdTTHandle             AS HANDLE   NO-UNDO.
    DEFINE VARIABLE lcReportHeader         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPageHeader           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPageFooter           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJob                  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcForm                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterial             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperation            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlank                AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatJob            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatForm           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatMaterial       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatOperation      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBlank          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcReportFooter         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobHeader            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobGroupHeader       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobGroupFooter       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormGroupHeader      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormGroupFooter      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterialGroupHeader  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterialGroupFooter  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationGroupHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationGroupFooter AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankGroupHeader     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankGroupFooter     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcData                 AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcJobData       AS LONGCHAR.
    DEFINE VARIABLE lcFormData      AS LONGCHAR.
    DEFINE VARIABLE lcBlankData     AS LONGCHAR.
    DEFINE VARIABLE lcMaterialData  AS LONGCHAR.
    DEFINE VARIABLE lcOperationData AS LONGCHAR.
    DEFINE VARIABLE lcItemData      AS LONGCHAR.
    
    DEFINE VARIABLE lJobAvailable       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOperationAvailable AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFormAvailable      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBlankAvailable     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lMaterialAvailable  AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
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
               AND ttArgs.argKey   = "TTEstCostHeaderIDHandle"
             NO-ERROR.
        IF NOT AVAILABLE ttArgs THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid temp-table handle"
                .
            
            RETURN.
        END.
        
        hdTTHandle = HANDLE(ttArgs.argValue).
        
        IF NOT VALID-HANDLE (hdTTHandle) THEN DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Invalid temp-table handle"
                .
            
            RETURN.        
        END.

        /* Code to send data from dynamic temp-table handle to static temp-table */
        hdTTHandle:WRITE-XML("MEMPTR", mptrTTReceipt).
        
        TEMP-TABLE ttEstCostHeaderID:READ-XML("MEMPTR", mptrTTReceipt, "EMPTY", ?, FALSE).
        
        SET-SIZE(mptrTTReceipt) = 0.                  

        oAttribute = NEW system.Attribute().
        oAttribute:RequestDataType = gcRequestDataType.
        
        RUN pGetRequestData ("Job", OUTPUT lcJobData).
        RUN pGetRequestData ("Form", OUTPUT lcFormData).
        RUN pGetRequestData ("Blank", OUTPUT lcBlankData).
        RUN pGetRequestData ("Material", OUTPUT lcMaterialData).
        RUN pGetRequestData ("Operation", OUTPUT lcOperationData).
        RUN pGetRequestData ("Item", OUTPUT lcItemData).
        
        RUN pGetRequestData ("PageHeader", OUTPUT lcPageHeader).
        RUN pGetRequestData ("PageFooter", OUTPUT lcPageFooter).
        RUN pGetRequestData ("JobGroupHeader", OUTPUT lcJobGroupHeader).
        RUN pGetRequestData ("JobGroupFooter", OUTPUT lcJobGroupFooter).
        RUN pGetRequestData ("FormGroupHeader", OUTPUT lcFormGroupHeader).
        RUN pGetRequestData ("FormGroupFooter", OUTPUT lcFormGroupFooter).
        RUN pGetRequestData ("MaterialGroupHeader", OUTPUT lcMaterialGroupHeader).
        RUN pGetRequestData ("MaterialGroupFooter", OUTPUT lcMaterialGroupFooter).
        RUN pGetRequestData ("OperationGroupHeader", OUTPUT lcOperationGroupHeader).
        RUN pGetRequestData ("OperationGroupFooter", OUTPUT lcOperationGroupFooter).
        RUN pGetRequestData ("BlankGroupHeader", OUTPUT lcBlankGroupHeader).
        RUN pGetRequestData ("BlankGroupFooter", OUTPUT lcBlankGroupFooter).
        
        FOR EACH ttEstCostHeaderID:
            FIND FIRST estCostHeader NO-LOCK
                 WHERE estCostHeader.estCostHeaderID EQ ttEstCostHeaderID.estCostHeaderID
                 NO-ERROR.
            IF NOT AVAILABLE estCostHeader THEN
                NEXT.
            
            FIND FIRST job-hdr NO-LOCK
                 WHERE ROWID(job-hdr) EQ ttEstCostHeaderID.riJobHeader
                 NO-ERROR.
                 
            lJobAvailable = TRUE.
            
            lFormAvailable = FALSE.
            
            lcConcatForm = "".
            
            FOR EACH estCostForm NO-LOCK
                WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID
                BY estCostForm.formNo:

                FIND FIRST ef NO-LOCK
                     WHERE ef.company  EQ estCostForm.company
                       AND ef.est-no   EQ estCostForm.estimateNo
                       AND ef.form-no  EQ estCostForm.formNo
                     NO-ERROR.            

                ASSIGN
                    lFormAvailable      = TRUE
                    lBlankAvailable     = FALSE
                    lOperationAvailable = FALSE
                    .
                
                lcConcatBlank = "".
                
                FOR EACH estCostBlank NO-LOCK
                    WHERE estCostBlank.estCostHeaderID EQ estCostForm.estCostHeaderID
                      AND estCostBlank.estCostFormID   EQ estCostForm.estCostFormID:
                    FIND FIRST eb NO-LOCK
                         WHERE eb.company  EQ estCostBlank.company
                           AND eb.est-no   EQ estCostBlank.estimateNo
                           AND eb.form-no  EQ estCostBlank.formNo
                           AND eb.blank-no EQ estCostBlank.blankNo
                         NO-ERROR.

                    FIND FIRST bf-job-hdr NO-LOCK
                         WHERE bf-job-hdr.company  EQ estCostBlank.company 
                           AND bf-job-hdr.job-no   EQ estCostHeader.jobID
                           AND bf-job-hdr.job-no2  EQ estCostHeader.jobID2
                           AND bf-job-hdr.frm      EQ estCostBlank.formNo
                           AND bf-job-hdr.blank-no EQ estCostBlank.blankNo
                           NO-ERROR.
                          
                    ASSIGN
                        lBlankAvailable    = TRUE
                        lMaterialAvailable = FALSE
                        .
                    
                    lcConcatMaterial = "".
                    
                    FOR EACH estCostMaterial NO-LOCK
                        WHERE estCostMaterial.estCostHeaderID EQ estCostBlank.estCostHeaderID
                          AND estCostMaterial.estCostBlankID  EQ estCostBlank.estCostBlankID:
                              
                        lMaterialAvailable = TRUE.

                        lcMaterial = lcMaterialData.
                        
                        lcMaterial = oAttribute:ReplaceAttributes(lcMaterial, BUFFER estCostMaterial:HANDLE).
                        lcConcatMaterial = lcConcatMaterial + lcMaterial.
                    END.
                    
                    lcBlank = lcBlankData.
                    
                    FIND FIRST estCostItem NO-LOCK
                         WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                           AND estCostItem.estCostItemID   EQ estCostBlank.estCostItemID
                         NO-ERROR.
                    lcBlank = oAttribute:ReplaceAttributes(INPUT lcBlank, BUFFER estCostItem:HANDLE).

                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBlank, "MaterialAvailable", STRING(lMaterialAvailable)).

                    lcBlank = REPLACE(lcBlank, "$Materials$", lcConcatMaterial).
                    lcBlank = REPLACE(lcBlank, "$MaterialGroupHeader$", lcMaterialGroupHeader).
                    lcBlank = REPLACE(lcBlank, "$MaterialGroupFooter$", lcMaterialGroupFooter).

                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER estCostBlank:HANDLE).
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER eb:HANDLE).
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER bf-job-hdr:HANDLE).
                    
                    lcConcatBlank = lcConcatBlank + lcBlank.
                END.
                
                lcConcatOperation = "".
                
                FOR EACH estCostOperation NO-LOCK
                    WHERE estCostOperation.estCostHeaderID EQ estCostForm.estCostHeaderID
                      AND estCostOperation.estCostFormID   EQ estCostForm.estCostFormID
                      BY estCostOperation.sequence:
                    
                    lOperationAvailable = TRUE.
    
                    lcOperation = lcOperationData.
                    
                    lcOperation = oAttribute:ReplaceAttributes(lcOperation, BUFFER estCostOperation:HANDLE).
                    lcConcatOperation = lcConcatOperation + lcOperation.
                END.
                
                lcForm = lcFormData.
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcForm, "OperationAvailable",STRING(lOperationAvailable)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcForm, "BlankAvailable",STRING(lBlankAvailable)).
                
                lcForm = REPLACE(lcForm, "$Blanks$", lcConcatBlank).
                lcForm = REPLACE(lcForm, "$BlankGroupHeader$", lcBlankGroupHeader).
                lcForm = REPLACE(lcForm, "$BlankGroupFooter$", lcBlankGroupFooter).
                lcForm = REPLACE(lcForm, "$Operations$", lcConcatOperation).
                lcForm = REPLACE(lcForm, "$OperationGroupHeader$", lcOperationGroupHeader).
                lcForm = REPLACE(lcForm, "$OperationGroupFooter$", lcOperationGroupFooter).

                lcForm = oAttribute:ReplaceAttributes(lcForm, BUFFER estCostForm:HANDLE).
                lcForm = oAttribute:ReplaceAttributes(lcForm, BUFFER ef:HANDLE).

                lcConcatForm = lcConcatForm + lcForm.
            END.  
            
            lcJob = lcJobData.
            
            lcJob = REPLACE(lcJob, "$Forms$", lcConcatForm).
            lcJob = REPLACE(lcJob, "$FormGroupHeader$", lcFormGroupHeader).
            lcJob = REPLACE(lcJob, "$FormGroupFooter$", lcFormGroupFooter).

            lcJob = oAttribute:ReplaceAttributes(lcJob, BUFFER estCostHeader:HANDLE).
            
            RUN pGetRequestData ("JobHeader", OUTPUT lcJobHeader).
            
            lcJobHeader = oAttribute:ReplaceAttributes(lcJobHeader, BUFFER estCostHeader:HANDLE).

            RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
            
            RUN pInsertPageHeaderFooter (INPUT-OUTPUT lcJob, INPUT lcJobHeader, INPUT lcPageFooter).

            lcJob = oAttribute:ReplaceAttributes(lcJob, BUFFER job-hdr:HANDLE).
            
            lcConcatJob = lcConcatJob + lcJob.
        END.
        
        RUN pGetRequestData(INPUT "JobGroupHeader", OUTPUT lcJobGroupHeader).
        RUN pGetRequestData(INPUT "JobGroupFooter", OUTPUT lcJobGroupFooter).
        RUN pGetRequestData(INPUT "ReportGroupHeader", OUTPUT lcReportHeader).
        RUN pGetRequestData(INPUT "ReportGroupFooter", OUTPUT lcReportFooter).
        
        ioplcRequestData = REPLACE(ioplcRequestData, "$Jobs$", lcConcatJob).
        ioplcRequestData = REPLACE(ioplcRequestData, "$JobGroupHeader$", lcJobGroupHeader).
        ioplcRequestData = REPLACE(ioplcRequestData, "$JobGroupFooter$", lcJobGroupFooter).
        ioplcRequestData = REPLACE(ioplcRequestData, "$ReportHeader$", lcReportHeader).
        ioplcRequestData = REPLACE(ioplcRequestData, "$ReportFooter$", lcReportFooter).
        
        RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
        
        ASSIGN
            opcMessage = ""
            oplSuccess = TRUE
            .
    END.

    FINALLY:
        IF VALID-OBJECT (oAttribute) THEN
            DELETE OBJECT oAttribute.

    END FINALLY.
    

/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetRequestData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcDetailID     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR  NO-UNDO.

    DEFINE VARIABLE lcHeader AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFooter AS LONGCHAR NO-UNDO.
        
    DEFINE BUFFER bf-APIOutboundDetail FOR apiOutboundDetail.
        
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID
           AND bf-APIOutboundDetail.parentID      EQ "JobTicket"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN
        oplcRequestData = bf-APIOutboundDetail.data.

    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Header"
           AND bf-APIOutboundDetail.parentID      EQ "JobTicket"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcHeader = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Header" + "$", lcHeader).
    END.
    
    FIND FIRST bf-APIOutboundDetail NO-LOCK
         WHERE bf-APIOutboundDetail.apiOutboundID EQ ipiAPIOutboundID
           AND bf-APIOutboundDetail.detailID      EQ ipcDetailID + "Footer"
           AND bf-APIOutboundDetail.parentID      EQ "JobTicket"
         NO-ERROR.
    IF AVAILABLE bf-APIOutboundDetail THEN DO:
        lcFooter = bf-APIOutboundDetail.data.
        oplcRequestData = REPLACE(oplcRequestData, "$" + ipcDetailID + "Footer" + "$", lcFooter).
    END.    
END PROCEDURE.

PROCEDURE pInsertPageHeaderFooter:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT  PARAMETER ioplcRequestData AS LONGCHAR NO-UNDO.
    DEFINE INPUT         PARAMETER iplcHeader       AS LONGCHAR NO-UNDO.
    DEFINE INPUT         PARAMETER iplcFooter       AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE iLineCount     AS INTEGER  NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER  NO-UNDO.
    DEFINE VARIABLE lcData         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE iLastIndex     AS INTEGER  NO-UNDO INITIAL 1.
    DEFINE VARIABLE lcRequestData1 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcRequestData2 AS LONGCHAR NO-UNDO.
    
    DO WHILE ioplcRequestData MATCHES "*" + "$PageSeparator$" + "*":
        iIndex = INDEX (ioplcRequestData, "$PageSeparator$").
        IF iIndex LE 0 THEN
            LEAVE.
            
        lcData = SUBSTRING(ioplcRequestData, iLastIndex, iIndex - iLastIndex - 1).
        
        lcData = ENTRY(NUM-ENTRIES(lcData, CHR(12)), lcData, CHR(12)).
        
        iLineCount = NUM-ENTRIES (lcData, CHR(10)).

        lcRequestData1 = SUBSTRING(ioplcRequestData, 1, iIndex - 1).
        lcRequestData2 = SUBSTRING(ioplcRequestData, iIndex + LENGTH("$PageSeparator$")).
        
        IF iLineCount GT 60 THEN
            ASSIGN
                ioplcRequestData = lcRequestData1 + iplcFooter + iplcHeader + lcRequestData2
                iLastIndex       = iIndex + LENGTH("$PageSeparator$")
                .
        ELSE
            ioplcRequestData = lcRequestData1 + lcRequestData2.
        
        RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
    END. 
END PROCEDURE.

