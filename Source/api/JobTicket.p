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

    {api/ttArgs.i}
            
    DEFINE INPUT        PARAMETER TABLE                   FOR ttArgs.
    DEFINE INPUT        PARAMETER ipiAPIOutboundID        AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiAPIOutboundTriggerID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRequestHandler       AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData        AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess              AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage              AS CHARACTER NO-UNDO.

    {api/CommonAPIProcs.i}
    
    DEFINE VARIABLE mptrTTReceipt              AS MEMPTR   NO-UNDO.
    DEFINE VARIABLE hdTTHandle                 AS HANDLE   NO-UNDO.
    DEFINE VARIABLE lcReportHeader             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPageHeader               AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPageFooter               AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJob                      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcForm                     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterial                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormMaterial             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperation                AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlank                    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcSpecInstrctn             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDeptInstrctn             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignImage           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDieImage                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFGItemImage              AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPlateImage               AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcCADImage                 AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatJob                AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatForm               AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatMaterial           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatFormMaterial       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatOperation          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBlank              AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatSpecInstrctn       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatDeptInstrctn       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatImageFiles         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatBlankImageFiles    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatFormImageFiles     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcConcatJobImageFiles      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcReportFooter             AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobHeader                AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobGroupHeader           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcJobGroupFooter           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormGroupHeader          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormGroupFooter          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterialGroupHeader      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterialGroupFooter      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormMaterialGroupHeader  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormMaterialGroupFooter  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationGroupHeader     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationGroupFooter     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankGroupHeader         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankGroupFooter         AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcSpecInstrctnGroupHeader  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcSpecInstrctnGroupFooter  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDeptInstrctnGroupHeader  AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDeptInstrctnGroupFooter  AS LONGCHAR NO-UNDO.    
    DEFINE VARIABLE lcData                     AS LONGCHAR NO-UNDO.

    DEFINE VARIABLE lcJobData            AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormData           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBlankData          AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcMaterialData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFormMaterialData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcOperationData      AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcItemData           AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcSpecInstrctnData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDeptInstrctnData   AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcBoxDesignImageData AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcDieImageData       AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcFGItemImageData    AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcPlateImageData     AS LONGCHAR NO-UNDO.
    DEFINE VARIABLE lcCADImageData       AS LONGCHAR NO-UNDO.
    
    DEFINE VARIABLE lJobAvailable            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOperationAvailable      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFormAvailable           AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBlankAvailable          AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lMaterialAvailable       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFormMaterialAvailable   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSpecInstrctnAvailable   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lDeptInstrctnAvailable   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lDieImageAvailable       AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lFGItemImageAvailable    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPlateImageAvailable     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCADImageAvailable       AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cCompany            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNoteText           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSpecList           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDeptExceptionCodes AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cPlateFolder        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDieFolder          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCadFolder          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPlateFolder   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidDieFolder     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidCadFolder     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cImageFile          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintBoxDesign     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lPrintFGItemImage   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValid              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFirstForm          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLastForm           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFirstBlank         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLastBlank          AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE iNumLinesInPage     AS INTEGER   NO-UNDO INITIAL 62.
        
    DEFINE VARIABLE oAttribute AS system.Attribute NO-UNDO.
    
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-cust    FOR cust.
    
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    RUN pGetNumLinesInPage(ipiAPIOutboundID, OUTPUT iNumLinesInPage).
    
    /* ************************  Function Prototypes ********************** */
    FUNCTION fGetImageFile RETURNS CHARACTER 
        ( ipcImageFile AS CHARACTER ) FORWARD.
    
    
    ASSIGN
        cSpecList         = system.SharedConfig:Instance:GetValue("JobTicket_SpecList")
        lPrintBoxDesign   = LOGICAL(system.SharedConfig:Instance:GetValue("JobTicket_PrintBoxDesign")) 
        lPrintFGItemImage = LOGICAL(system.SharedConfig:Instance:GetValue("JobTicket_PrintFGItemImage"))
        NO-ERROR.
    
    RUN sys/ref/nk1look.p (
        INPUT  cCompany, 
        INPUT  "NOTES", 
        INPUT  "C", 
        INPUT  NO, 
        INPUT  NO, 
        INPUT  "", 
        INPUT  "", 
        OUTPUT cDeptExceptionCodes, 
        OUTPUT lFound
        ).

    IF lPrintBoxDesign THEN DO:
        RUN sys/ref/nk1look.p(
            INPUT cCompany,
            INPUT "PlateFile",
            INPUT "C",
            INPUT NO,
            INPUT NO,
            INPUT "",
            INPUT "",
            OUTPUT cPlateFolder,
            OUTPUT lFound
            ).       
        RUN FileSys_GetFilePath(cPlateFolder, OUTPUT cPlateFolder, OUTPUT lValidPlateFolder, OUTPUT cMessage).
        IF SUBSTRING(cPlateFolder, LENGTH(cPlateFolder), 1) NE "\" THEN
            cPlateFolder = cPlateFolder + "\".                         
    
        RUN sys/ref/nk1look.p(
            INPUT cCompany,
            INPUT "DieFile",
            INPUT "C",
            INPUT NO,
            INPUT NO,
            INPUT "",
            INPUT "",
            OUTPUT cDieFolder,
            OUTPUT lFound
            ).
        RUN FileSys_GetFilePath(cDieFolder, OUTPUT cDieFolder, OUTPUT lValidDieFolder, OUTPUT cMessage).
        IF SUBSTRING(cDieFolder, LENGTH(cDieFolder), 1) NE "\" THEN
            cDieFolder = cDieFolder + "\".                         
    
        RUN sys/ref/nk1look.p(
            INPUT cCompany,
            INPUT "CadFile",
            INPUT "C",
            INPUT NO,
            INPUT NO,
            INPUT "",
            INPUT "",
            OUTPUT cCadFolder,
            OUTPUT lFound
            ).
        RUN FileSys_GetFilePath(cCADFolder, OUTPUT cCADFolder, OUTPUT lValidCADFolder, OUTPUT cMessage).    
        IF SUBSTRING(cCadFolder, LENGTH(cCadFolder), 1) NE "\" THEN
            cCadFolder = cCadFolder + "\".                         
    END.
    
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
        RUN pGetRequestData ("FormMaterial", OUTPUT lcFormMaterialData).
        RUN pGetRequestData ("Operation", OUTPUT lcOperationData).
        RUN pGetRequestData ("Item", OUTPUT lcItemData).
        RUN pGetRequestData ("SpecialInstruction", OUTPUT lcSpecInstrctnData).
        RUN pGetRequestData ("DeptInstruction", OUTPUT lcDeptInstrctnData).
        RUN pGetRequestData ("DieImage", OUTPUT lcDieImageData).
        RUN pGetRequestData ("BoxDesignImage", OUTPUT lcBoxDesignImageData).
        RUN pGetRequestData ("FGItemImage", OUTPUT lcFGItemImageData).
        RUN pGetRequestData ("PlateImage", OUTPUT lcPlateImageData).
        RUN pGetRequestData ("CADImage", OUTPUT lcCADImageData).
        
        RUN pGetRequestData ("PageHeader", OUTPUT lcPageHeader).
        RUN pGetRequestData ("PageFooter", OUTPUT lcPageFooter).
        RUN pGetRequestData ("JobGroupHeader", OUTPUT lcJobGroupHeader).
        RUN pGetRequestData ("JobGroupFooter", OUTPUT lcJobGroupFooter).
        RUN pGetRequestData ("FormGroupHeader", OUTPUT lcFormGroupHeader).
        RUN pGetRequestData ("FormGroupFooter", OUTPUT lcFormGroupFooter).
        RUN pGetRequestData ("MaterialGroupHeader", OUTPUT lcMaterialGroupHeader).
        RUN pGetRequestData ("MaterialGroupFooter", OUTPUT lcMaterialGroupFooter).
        RUN pGetRequestData ("FormMaterialGroupHeader", OUTPUT lcFormMaterialGroupHeader).
        RUN pGetRequestData ("FormMaterialGroupFooter", OUTPUT lcFormMaterialGroupFooter).
        RUN pGetRequestData ("OperationGroupHeader", OUTPUT lcOperationGroupHeader).
        RUN pGetRequestData ("OperationGroupFooter", OUTPUT lcOperationGroupFooter).
        RUN pGetRequestData ("BlankGroupHeader", OUTPUT lcBlankGroupHeader).
        RUN pGetRequestData ("BlankGroupFooter", OUTPUT lcBlankGroupFooter).
        RUN pGetRequestData ("SpecialInstructionGroupHeader", OUTPUT lcSpecInstrctnGroupHeader).
        RUN pGetRequestData ("SpecialInstructionGroupFooter", OUTPUT lcSpecInstrctnGroupFooter).
        RUN pGetRequestData ("DeptInstructionGroupHeader", OUTPUT lcDeptInstrctnGroupHeader).
        RUN pGetRequestData ("DeptInstructionGroupFooter", OUTPUT lcDeptInstrctnGroupFooter).
        
        FOR EACH ttEstCostHeaderID:
            FIND FIRST estCostHeader NO-LOCK
                 WHERE estCostHeader.estCostHeaderID EQ ttEstCostHeaderID.estCostHeaderID
                 NO-ERROR.
            IF NOT AVAILABLE estCostHeader THEN
                NEXT.
            
            FIND FIRST job-hdr NO-LOCK
                 WHERE ROWID(job-hdr) EQ ttEstCostHeaderID.riJobHeader
                 NO-ERROR.
            
            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST job NO-LOCK
                     WHERE job.job EQ job-hdr.job
                     NO-ERROR.     

                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ job-hdr.company
                       AND cust.cust-no EQ job-hdr.cust-no
                     NO-ERROR.
            END.
            
            ASSIGN
                lJobAvailable          = TRUE
                lFormAvailable         = FALSE
                lSpecInstrctnAvailable = FALSE
                lcConcatForm           = ""
                lcConcatSpecInstrctn   = ""
                lcConcatJobImageFiles  = ""
                .
            
            FOR EACH estCostForm NO-LOCK
                WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID
                BREAK BY estCostForm.formNo:

                FIND FIRST ef NO-LOCK
                     WHERE ef.company  EQ estCostForm.company
                       AND ef.est-no   EQ estCostForm.estimateNo
                       AND ef.form-no  EQ estCostForm.formNo
                     NO-ERROR.            

                ASSIGN
                    lFormAvailable      = TRUE
                    lBlankAvailable     = FALSE
                    lOperationAvailable = FALSE
                    lFirstForm          = FIRST(estCostForm.formNo)
                    lLastForm           = LAST(estCostForm.formNo)
                    .
                
                ASSIGN
                    lcConcatBlank           = ""
                    lcConcatFormImageFiles  = ""
                    .
                
                FOR EACH estCostBlank NO-LOCK
                    WHERE estCostBlank.estCostHeaderID EQ estCostForm.estCostHeaderID
                      AND estCostBlank.estCostFormID   EQ estCostForm.estCostFormID
                    BREAK BY estCostBlank.blankNo:
                    ASSIGN
                        lcConcatBlankImageFiles = ""
                        lFirstBlank             = FIRST(estCostBlank.blankNo)
                        lLastBlank              = LAST(estCostBlank.blankNo)
                        .
                          
                    FIND FIRST eb NO-LOCK
                         WHERE eb.company  EQ estCostBlank.company
                           AND eb.est-no   EQ estCostBlank.estimateNo
                           AND eb.form-no  EQ estCostBlank.formNo
                           AND eb.blank-no EQ estCostBlank.blankNo
                         NO-ERROR.
                    IF AVAILABLE eb AND lPrintBoxDesign THEN DO:
                        FIND FIRST box-design-hdr NO-LOCK
                             WHERE box-design-hdr.design-no EQ 0
                               AND box-design-hdr.company   EQ eb.company
                               AND box-design-hdr.est-no    EQ eb.est-no
                               AND box-design-hdr.form-no   EQ eb.form-no
                               AND box-design-hdr.blank-no  EQ eb.blank-no
                             NO-ERROR.
                        IF AVAILABLE box-design-hdr AND box-design-hdr.box-image NE "" THEN DO:
                            cImageFile = box-design-hdr.box-image.
                            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
                            IF lValid THEN DO:
                                lcBoxDesignImage = lcBoxDesignImageData.
                                oAttribute:UpdateRequestData(INPUT-OUTPUT lcBoxDesignImage, "ImageFile", cImageFile).
                                lcConcatBlankImageFiles = lcConcatBlankImageFiles + lcBoxDesignImage.
                            END.                        
                        END.
                        
                        IF lValidCadFolder AND eb.cad-no NE "" THEN DO:
                            cImageFile = fGetImageFile(cCadFolder + eb.cad-no).
                            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
                            IF lValid THEN DO:
                                lcCADImage = lcCADImageData.
                                oAttribute:UpdateRequestData(INPUT-OUTPUT lcCADImage, "ImageFile", cImageFile).
                                lcConcatBlankImageFiles = lcConcatBlankImageFiles + lcCADImage.
                            END.
                        END.
                        IF lValidDieFolder AND eb.die-no NE "" THEN DO:
                            cImageFile = fGetImageFile(cDieFolder + eb.die-no).
                            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
                            IF lValid THEN DO:
                                lcDieImage = lcDieImageData.
                                oAttribute:UpdateRequestData(INPUT-OUTPUT lcDieImage, "ImageFile", cImageFile).
                                lcConcatBlankImageFiles = lcConcatBlankImageFiles + lcDieImage.
                            END.
                        END.
                        IF lValidPlateFolder AND eb.plate-no NE "" THEN DO:
                            cImageFile = fGetImageFile(cPlateFolder + eb.plate-no).
                            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
                            IF lValid THEN DO:
                                lcPlateImage = lcPlateImageData.
                                oAttribute:UpdateRequestData(INPUT-OUTPUT lcPlateImage, "ImageFile", cImageFile).
                                lcConcatBlankImageFiles = lcConcatBlankImageFiles + lcPlateImage.
                            END.
                        END.                        
                    END.
                    
                    FIND FIRST bf-job-hdr NO-LOCK
                         WHERE bf-job-hdr.company  EQ estCostBlank.company 
                           AND bf-job-hdr.job-no   EQ estCostHeader.jobID
                           AND bf-job-hdr.job-no2  EQ estCostHeader.jobID2
                           AND bf-job-hdr.frm      EQ estCostBlank.formNo
                           AND bf-job-hdr.blank-no EQ estCostBlank.blankNo
                           NO-ERROR.
                    
                    IF AVAILABLE bf-job-hdr THEN
                        FIND FIRST bf-cust NO-LOCK
                             WHERE bf-cust.company EQ bf-job-hdr.company
                               AND bf-cust.cust-no EQ bf-job-hdr.cust-no
                             NO-ERROR.
                    
                    ASSIGN
                        lBlankAvailable    = TRUE
                        lMaterialAvailable = FALSE
                        .
                    
                    lcConcatMaterial = "".
                    
                    FOR EACH estCostMaterial NO-LOCK
                        WHERE estCostMaterial.estCostHeaderID EQ estCostBlank.estCostHeaderID
                          AND estCostMaterial.estCostBlankID  EQ estCostBlank.estCostBlankID:
                        FIND FIRST item NO-LOCK
                             WHERE item.company EQ estCostMaterial.company
                               AND item.i-no    EQ estCostMaterial.itemID
                             NO-ERROR.

                        IF AVAILABLE job-hdr THEN DO:
                            FIND FIRST job-mat NO-LOCK
                                 WHERE job-mat.company  EQ job-hdr.company
                                   AND job-mat.job      EQ job-hdr.job
                                   AND job-mat.job-no   EQ job-hdr.job-no
                                   AND job-mat.job-no2  EQ job-hdr.job-no2
                                   AND job-mat.frm      EQ estCostMaterial.formNo
                                   AND job-mat.blank-no EQ estCostMaterial.blankNo
                                 NO-ERROR.

                            IF AVAILABLE job-mat THEN
                                FIND FIRST po-ordl NO-LOCK
                                     WHERE po-ordl.company   EQ job-mat.company
                                       AND po-ordl.job-no    EQ job-mat.job-no
                                       AND po-ordl.job-no2   EQ job-mat.job-no2
                                       AND po-ordl.s-num     EQ job-mat.frm
                                       AND po-ordl.b-num     EQ job-mat.blank-no
                                       AND po-ordl.i-no      EQ job-mat.i-no
                                       AND po-ordl.item-type EQ TRUE
                                     NO-ERROR.
                        END.
                                                        
                        lMaterialAvailable = TRUE.

                        lcMaterial = lcMaterialData.
                        
                        lcMaterial = oAttribute:ReplaceAttributes(lcMaterial, BUFFER estCostMaterial:HANDLE).
                        lcMaterial = oAttribute:ReplaceAttributes(lcMaterial, BUFFER item:HANDLE).
                        lcMaterial = oAttribute:ReplaceAttributes(lcMaterial, BUFFER job-mat:HANDLE).
                        lcMaterial = oAttribute:ReplaceAttributes(lcMaterial, BUFFER po-ordl:HANDLE).
                        
                        lcConcatMaterial = lcConcatMaterial + lcMaterial.
                    END.
                    
                    lcBlank = lcBlankData.
                    
                    FIND FIRST estCostItem NO-LOCK
                         WHERE estCostItem.estCostHeaderID EQ estCostBlank.estCostHeaderID
                           AND estCostItem.estCostItemID   EQ estCostBlank.estCostItemID
                         NO-ERROR.
                    lcBlank = oAttribute:ReplaceAttributes(INPUT lcBlank, BUFFER estCostItem:HANDLE).

                    lcSpecInstrctn = "".
                    
                    IF AVAILABLE estCostItem THEN DO:
                        FIND FIRST itemfg NO-LOCK
                             WHERE itemfg.company EQ estCostItem.company
                               AND itemfg.i-no    EQ estCostItem.itemID
                               NO-ERROR.
                        IF AVAILABLE itemfg THEN DO:
                            IF lPrintFGItemImage AND itemfg.box-image NE "" THEN DO:
                                RUN FileSys_ValidateFile(itemfg.box-image, OUTPUT lValid, OUTPUT cMessage).
                                IF lValid THEN DO:
                                    lcFGItemImage = lcFGItemImageData.
                                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcFGItemImage, "ImageFile", itemfg.box-image).
                                    
                                    lcConcatBlankImageFiles = lcConcatBlankImageFiles + lcFGItemImage.
                                END.
                            END.                            
                            FOR EACH notes NO-LOCK 
                                WHERE notes.rec_key   EQ itemfg.rec_key
                                  AND notes.note_type NE ""
                                  AND CAN-DO(cSpecList, notes.note_code),
                                FIRST item-spec NO-LOCK
                                WHERE item-spec.company EQ itemfg.company
                                  AND item-spec.i-no    EQ ""
                                  AND item-spec.code    EQ notes.note_code
                                BY item-spec.code
                                BY notes.note_date 
                                BY notes.note_time:
                                ASSIGN
                                    lSpecInstrctnAvailable = TRUE
                                    cNoteText              = notes.note_text
                                    .
            
                                lcSpecInstrctn = lcSpecInstrctnData.
                                
                                RUN pReplaceNotesText(INPUT-OUTPUT cNoteText).
                                
                                oAttribute:UpdateRequestData(INPUT-OUTPUT lcSpecInstrctn, "NoteText", cNoteText).
                                
                                lcSpecInstrctn = oAttribute:ReplaceAttributes(lcSpecInstrctn, BUFFER notes:HANDLE).
                                lcSpecInstrctn = oAttribute:ReplaceAttributes(lcSpecInstrctn, BUFFER item-spec:HANDLE).
                                
                                lcConcatSpecInstrctn = lcConcatSpecInstrctn + lcSpecInstrctn.
                            END.
                        END.
                    END.

                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBlank, "MaterialAvailable", STRING(lMaterialAvailable)).

                    lcBlank = REPLACE(lcBlank, "$FGItemImage$", lcFGItemImage).
                    lcBlank = REPLACE(lcBlank, "$BoxDesignImage$", lcBoxDesignImage).
                    lcBlank = REPLACE(lcBlank, "$DieImage$", lcDieImage).
                    lcBlank = REPLACE(lcBlank, "$CADImage$", lcCADImage).
                    lcBlank = REPLACE(lcBlank, "$PlateImage$", lcPlateImage).
                    lcBlank = REPLACE(lcBlank, "$BlankImages$", lcConcatBlankImageFiles).

                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBlank, "FirstBlank", STRING(lFirstBlank)).
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcBlank, "LastBlank", STRING(lLastBlank)).
                    
                    lcConcatBlankImageFiles = oAttribute:ReplaceAttributes(lcConcatBlankImageFiles, BUFFER eb:HANDLE).

                    lcBlank = REPLACE(lcBlank, "$Materials$", lcConcatMaterial).
                    lcBlank = REPLACE(lcBlank, "$MaterialGroupHeader$", lcMaterialGroupHeader).
                    lcBlank = REPLACE(lcBlank, "$MaterialGroupFooter$", lcMaterialGroupFooter).
                    
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER estCostBlank:HANDLE).
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER eb:HANDLE).
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER cust:HANDLE).
                    lcBlank = oAttribute:ReplaceAttributes(lcBlank, BUFFER bf-job-hdr:HANDLE).

                    lcConcatFormImageFiles = lcConcatFormImageFiles + lcConcatBlankImageFiles.                    
                    lcConcatBlank = lcConcatBlank + lcBlank.
                END.

                lFormMaterialAvailable = FALSE.
                
                lcConcatFormMaterial = "".
                
                FOR EACH estCostMaterial NO-LOCK
                    WHERE estCostMaterial.estCostHeaderID EQ estCostForm.estCostHeaderID
                      AND estCostMaterial.formNo          EQ estCostForm.formNo:
                    FIND FIRST item NO-LOCK
                         WHERE item.company EQ estCostMaterial.company
                           AND item.i-no    EQ estCostMaterial.itemID
                         NO-ERROR.
                    
                    IF AVAILABLE job-hdr THEN DO:
                        FIND FIRST job-mat NO-LOCK
                             WHERE job-mat.company  EQ job-hdr.company
                               AND job-mat.job      EQ job-hdr.job
                               AND job-mat.job-no   EQ job-hdr.job-no
                               AND job-mat.job-no2  EQ job-hdr.job-no2
                               AND job-mat.frm      EQ estCostMaterial.formNo
                               AND job-mat.blank-no EQ estCostMaterial.blankNo
                             NO-ERROR.

                        IF AVAILABLE job-mat THEN
                            FIND FIRST po-ordl NO-LOCK
                                 WHERE po-ordl.company   EQ job-mat.company
                                   AND po-ordl.job-no    EQ job-mat.job-no
                                   AND po-ordl.job-no2   EQ job-mat.job-no2
                                   AND po-ordl.s-num     EQ job-mat.frm
                                   AND po-ordl.b-num     EQ job-mat.blank-no
                                   AND po-ordl.i-no      EQ job-mat.i-no
                                   AND po-ordl.item-type EQ TRUE
                                 NO-ERROR.
                    END.
                    
                    lFormMaterialAvailable = TRUE.

                    lcFormMaterial = lcFormMaterialData.
                    
                    lcFormMaterial = oAttribute:ReplaceAttributes(lcFormMaterial, BUFFER estCostMaterial:HANDLE).
                    lcFormMaterial = oAttribute:ReplaceAttributes(lcFormMaterial, BUFFER item:HANDLE).
                    lcFormMaterial = oAttribute:ReplaceAttributes(lcFormMaterial, BUFFER job-mat:HANDLE).
                    lcFormMaterial = oAttribute:ReplaceAttributes(lcFormMaterial, BUFFER po-ordl:HANDLE).

                    lcConcatFormMaterial = lcConcatFormMaterial + lcFormMaterial.
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
                lcForm = REPLACE(lcForm, "$FormImages$", lcConcatFormImageFiles).
                
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcForm, "FormMaterialAvailable",STRING(lFormMaterialAvailable)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcForm, "FirstForm", STRING(lFirstForm)).
                oAttribute:UpdateRequestData(INPUT-OUTPUT lcForm, "LastForm", STRING(lLastForm)).

                lcForm = REPLACE(lcForm, "$FormMaterials$", lcConcatFormMaterial).
                lcForm = REPLACE(lcForm, "$FormMaterialGroupHeader$", lcFormMaterialGroupHeader).
                lcForm = REPLACE(lcForm, "$FormMaterialGroupFooter$", lcFormMaterialGroupFooter).

                lcForm = oAttribute:ReplaceAttributes(lcForm, BUFFER estCostForm:HANDLE).
                lcForm = oAttribute:ReplaceAttributes(lcForm, BUFFER ef:HANDLE).

                lcConcatJobImageFiles = lcConcatJobImageFiles + lcConcatFormImageFiles.
                
                lcConcatForm = lcConcatForm + lcForm.
            END.  
            
            ASSIGN
                lcConcatDeptInstrctn   = "".
                lDeptInstrctnAvailable = FALSE
                .
                
            IF AVAILABLE job THEN DO:
                FOR EACH notes NO-LOCK 
                    WHERE notes.rec_key   EQ job.rec_key
                      AND LOOKUP(notes.note_code, cDeptExceptionCodes) EQ 0
                    BY notes.note_date 
                    BY notes.note_time:
                    FIND FIRST dept NO-LOCK
                         WHERE dept.code EQ notes.note_code
                         NO-ERROR.
                         
                    ASSIGN
                        lDeptInstrctnAvailable = TRUE
                        cNoteText              = notes.note_text
                        .
    
                    lcDeptInstrctn = lcDeptInstrctnData.
                    
                    RUN pReplaceNotesText(INPUT-OUTPUT cNoteText).
                    
                    oAttribute:UpdateRequestData(INPUT-OUTPUT lcDeptInstrctn, "NoteText", cNoteText).
                    
                    lcDeptInstrctn = oAttribute:ReplaceAttributes(lcDeptInstrctn, BUFFER notes:HANDLE).
                    lcDeptInstrctn = oAttribute:ReplaceAttributes(lcDeptInstrctn, BUFFER item-spec:HANDLE).
                    lcDeptInstrctn = oAttribute:ReplaceAttributes(lcDeptInstrctn, BUFFER dept:HANDLE).
                    
                    lcConcatDeptInstrctn = lcConcatDeptInstrctn + lcDeptInstrctn.
                END.
            END.
            
            lcJob = lcJobData.

            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJob, "SpecialInstructionAvailable", STRING(lSpecInstrctnAvailable)).
            oAttribute:UpdateRequestData(INPUT-OUTPUT lcJob, "DeptInstructionAvailable", STRING(lDeptInstrctnAvailable)).
            
            lcJob = REPLACE(lcJob, "$Forms$", lcConcatForm).
            lcJob = REPLACE(lcJob, "$FormGroupHeader$", lcFormGroupHeader).
            lcJob = REPLACE(lcJob, "$FormGroupFooter$", lcFormGroupFooter).
            lcJob = REPLACE(lcJob, "$SpecialInstructions$", lcConcatSpecInstrctn).
            lcJob = REPLACE(lcJob, "$SpecialInstructionGroupHeader$", lcSpecInstrctnGroupHeader).
            lcJob = REPLACE(lcJob, "$SpecialInstructionGroupFooter$", lcSpecInstrctnGroupFooter).
            lcJob = REPLACE(lcJob, "$DeptInstructions$", lcConcatDeptInstrctn).
            lcJob = REPLACE(lcJob, "$DeptInstructionGroupHeader$", lcDeptInstrctnGroupHeader).
            lcJob = REPLACE(lcJob, "$DeptInstructionGroupFooter$", lcDeptInstrctnGroupFooter).
            
            lcJob = oAttribute:ReplaceAttributes(lcJob, BUFFER estCostHeader:HANDLE).
            
            RUN pGetRequestData ("JobHeader", OUTPUT lcJobHeader).
            
            lcJobHeader = oAttribute:ReplaceAttributes(lcJobHeader, BUFFER estCostHeader:HANDLE).

            lcConcatJobImageFiles = REPLACE(lcConcatJobImageFiles, "$JobHeader$", lcJobHeader).            
            lcJob = REPLACE(lcJob, "$JobImages$", lcConcatJobImageFiles).

            RUN pUpdateDelimiterWithoutTrim (INPUT-OUTPUT ioplcRequestData, "").
            
            RUN pInsertPageHeaderFooter (INPUT iNumLinesInPage, INPUT-OUTPUT lcJob, INPUT lcJobHeader, INPUT lcPageFooter).

            lcJob = oAttribute:ReplaceAttributes(lcJob, BUFFER job-hdr:HANDLE).
            lcJob = oAttribute:ReplaceAttributes(lcJob, BUFFER cust:HANDLE).
            
            lcConcatJob = lcConcatJob + lcJob.
        END.
        
        RUN pGetRequestData(INPUT "JobGroupHeader", OUTPUT lcJobGroupHeader).
        RUN pGetRequestData(INPUT "JobGroupFooter", OUTPUT lcJobGroupFooter).
        RUN pGetRequestData(INPUT "ReportHeader", OUTPUT lcReportHeader).
        RUN pGetRequestData(INPUT "ReportFooter", OUTPUT lcReportFooter).
        
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

PROCEDURE pReplaceNotesText:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopcNoteText AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTempNote       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIterations     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTextLineLength AS INTEGER   NO-UNDO INIT 70.
    ASSIGN
        iopcNoteText = REPLACE(iopcNoteText, CHR(10) + CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(10), " ")
        iopcNoteText = REPLACE(iopcNoteText, CHR(13), " ")
        iopcNoteText = REPLACE(iopcNoteText, "  ", " ")
        .
    
    IF LENGTH(iopcNoteText) GT iTextLineLength THEN DO:
        iIterations = (LENGTH(iopcNoteText) / iTextLineLength) + INT(LENGTH(iopcNoteText) MOD iTextLineLength GT 0).
        DO iIndex = 1 TO iIterations:
            cTempNote = cTempNote + CHR(10) + "linefeed" + SUBSTRING(iopcNoteText, iIndex * iTextLineLength - (iTextLineLength - 1), iTextLineLength). 
        END.
    END.
    ELSE
        cTempNote = iopcNoteText.
    
    cTempNote = TRIM(cTempNote, CHR(10)).
    
    iopcNoteText = cTempNote.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetImageFile RETURNS CHARACTER 
	( ipcImageFile AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid     AS LOGICAL   NO-UNDO.
    
    cImageFile = ipcImageFile + ".pdf".
    
    RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
    
    IF NOT lValid THEN DO:
        cImageFile = ipcImageFile + ".jpg".
        RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
    END.
    
    IF NOT lValid THEN 
    DO:
        cImageFile = ipcImageFile + ".png".
        RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
    END.
    
    RETURN cImageFile.
END FUNCTION.

