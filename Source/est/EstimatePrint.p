
/*------------------------------------------------------------------------
    File        : EstimatePrint.p
    Purpose     : 

    Syntax      :

    Description : Builds the Estimate Printout report		

    Author(s)   : BV
    Created     : Wed Jan 23 15:41:37 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.

DEFINE VARIABLE glShowAllQuantities AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iSequence     AS INTEGER
    FIELD cType         AS CHARACTER 
    .
    
DEFINE TEMP-TABLE ttCEFormatConfig NO-UNDO
    FIELD outputFile                  AS CHARACTER 
    FIELD previewFile                 AS LOGICAL INITIAL YES
    FIELD printInPDF                  AS LOGICAL
    FIELD formatMaster                AS CHARACTER
    FIELD formatFont                  AS CHARACTER INITIAL "Calibri"
    FIELD formatFontSize              AS INTEGER INITIAL 11
    FIELD xprintTags                  AS CHARACTER 
    FIELD isClassic                   AS LOGICAL 
    FIELD maxColumnsForQuantity       AS INTEGER INITIAL 99
    FIELD characterNumberError        AS CHARACTER INITIAL "#"
    FIELD characterContinue           AS CHARACTER INITIAL ">>"
    FIELD characterMasterQuantity     AS CHARACTER INITIAL "*"
    FIELD rowsPerPage                 AS INTEGER INITIAL 64
    FIELD showAllQuantities           AS LOGICAL INITIAL YES
    FIELD showProfitPercent           AS LOGICAL INITIAL YES
    FIELD SIMONListInclude            AS CHARACTER INITIAL "I,M"
    FIELD SIMONListSeparate           AS CHARACTER INITIAL "S,O,N"
    FIELD useReferenceQuantity        AS LOGICAL INITIAL NO
    FIELD printByForm                 AS LOGICAL INITIAL YES
    FIELD printSummary                AS LOGICAL INITIAL YES
    FIELD printSummaryFirst           AS LOGICAL INITIAL YES
    FIELD printAnalysis               AS LOGICAL INITIAL YES
    FIELD printNotes                  AS LOGICAL INITIAL YES
    FIELD operationTimeInHHMM         AS LOGICAL 
    FIELD summColQuantityShow         AS LOGICAL INITIAL NO
    FIELD summColQuantityLabel        AS CHARACTER INITIAL "Quantity"
    FIELD summColQuantityCol          AS DECIMAL INITIAL 36
    FIELD summColQuantityWidth        AS INTEGER INITIAL 9
    FIELD summColQuantityRequestShow  AS LOGICAL INITIAL YES
    FIELD summColQuantityRequestLabel AS CHARACTER INITIAL "Requested"
    FIELD summColQuantityRequestCol   AS DECIMAL INITIAL 26
    FIELD summColQuantityRequestWidth AS INTEGER INITIAL 9
    FIELD summColQuantityYieldShow    AS LOGICAL INITIAL YES
    FIELD summColQuantityYieldLabel   AS CHARACTER INITIAL "Yielded" 
    FIELD summColQuantityYieldCol     AS DECIMAL INITIAL 36
    FIELD summColQuantityYieldWidth   AS INTEGER INITIAL 9
    FIELD summColWeightShow           AS LOGICAL INITIAL YES
    FIELD summColWeightLabel          AS CHARACTER INITIAL "Weight"
    FIELD summColWeightCol            AS DECIMAL INITIAL 46
    FIELD summColWeightWidth          AS INTEGER INITIAL 9
    FIELD summColItemNameShow         AS LOGICAL INITIAL YES
    FIELD summColItemNameLabel        AS CHARACTER INITIAL "Item Name"
    FIELD summColItemNameCol          AS DECIMAL INITIAL 2
    FIELD summColItemNameWidth        AS INTEGER INITIAL 30
    FIELD summColDirectCostShow       AS LOGICAL INITIAL NO
    FIELD summColDirectCostCol        AS DECIMAL INITIAL 46
    FIELD summColDirectCostWidth      AS INTEGER INITIAL 6
    FIELD summColFactoryCostShow      AS LOGICAL INITIAL YES
    FIELD summColFactoryCostCol       AS DECIMAL INITIAL 56
    FIELD summColFactoryCostWidth     AS INTEGER INITIAL 6
    FIELD summColFullCostShow         AS LOGICAL INITIAL YES
    FIELD summColFullCostCol          AS DECIMAL INITIAL 66
    FIELD summColFullCostWidth        AS INTEGER INITIAL 6
    FIELD summColSellPriceShow        AS LOGICAL INITIAL YES
    FIELD summColSellPriceCol         AS DECIMAL INITIAL 76
    FIELD summColSellPriceWidth       AS INTEGER INITIAL 6
    .

{system\NotesProcs.i}

DEFINE STREAM sEstOutput.
DEFINE VARIABLE hdOutputProcs       AS HANDLE.
DEFINE VARIABLE hdNotesProcs        AS HANDLE.
DEFINE VARIABLE hdEstimateCalcProcs AS HANDLE.
RUN sys/NotesProcs.p PERSISTENT SET hdNotesProcs.
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
RUN est/EstimateCalcProcs.p PERSISTENT SET hdEstimateCalcProcs.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    (ipdNumber AS DECIMAL,
    ipiLeftDigits AS INTEGER,
    ipiRightDigits AS INTEGER,
    iplComma AS LOGICAL,
    iplAllowNegatives AS LOGICAL) FORWARD.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    (ipcString AS CHARACTER,
    ipiCharacters AS INTEGER) FORWARD.

FUNCTION fFormatTime RETURNS CHARACTER PRIVATE
    (ipdTimeInDecimal AS DECIMAL) FORWARD.

FUNCTION fFormIsPurchasedFG RETURNS LOGICAL PRIVATE
    (ipiFormID AS INT64) FORWARD.

FUNCTION fGetCostGroupLabel RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER,
	 ipcLocation AS CHARACTER,
	 ipcCostGroupID AS CHARACTER,
	 ipcCostGroupLabel AS CHARACTER) FORWARD.

FUNCTION fGetMaterialDescription RETURNS CHARACTER PRIVATE
	(BUFFER ipbf-item FOR item) FORWARD.

FUNCTION fTypeAllowsMult RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fTypePrintsLayout RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fTypePrintsBoard RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.
    
FUNCTION fTypeIsWood RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.    
    
/* ***************************  Main Block  *************************** */
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutputProcs).
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdNotesProcs).
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdEstimateCalcProcs).
RUN pBuildSections(ipiEstCostHeaderID, ipcOutputFile, BUFFER ttCeFormatConfig).
IF CAN-FIND(FIRST ttSection) THEN 
DO: 
    RUN Output_InitializeXprint(ttCEFormatConfig.outputFile, 
        ttCEFormatConfig.previewFile, 
        ttCEFormatConfig.printInPDF, 
        ttCEFormatConfig.formatFont, 
        ttCEFormatConfig.formatFontSize,
        ttCEFormatConfig.xprintTags) .
    RUN pProcessSections(BUFFER ttCEFormatConfig).
    RUN Output_Close.
    RUN Output_PrintXprintFile(ttCEFormatConfig.outputFile).
END.
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutputProcs).
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdNotesProcs).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildConfigFromTemplate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormatMaster AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormatFont AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttCEFormatConfig FOR ttCEFormatConfig.

    
    CREATE opbf-ttCEFormatConfig.
    ASSIGN 
        opbf-ttCEFormatConfig.formatMaster                = ipcFormatMaster
        opbf-ttCEFormatConfig.formatFont                  = ipcFormatFont
        opbf-ttCEFormatConfig.formatFontSize              = 11
        opbf-ttCEFormatConfig.previewFile                 = YES
        opbf-ttCEFormatConfig.printInPDF                  = NO        
        opbf-ttCEFormatConfig.printSummary                = INDEX(ipcFormatMaster, "Summary") GT 0
        opbf-ttCEFormatConfig.printSummaryFirst           = INDEX(ipcFormatMaster, "First") GT 0
        opbf-ttCEFormatConfig.showAllQuantities           = INDEX(ipcFormatMaster, "Mult Qty") GT 0
        opbf-ttCEFormatConfig.printByForm                 = INDEX(ipcFormatMaster, "By Form") GT 0
        opbf-ttCEFormatConfig.printAnalysis               = INDEX(ipcFormatMaster, "Analysis") GT 0
        opbf-ttCEFormatConfig.printNotes                  = INDEX(ipcFormatMaster, "No Notes") EQ 0
        opbf-ttCEFormatConfig.showProfitPercent           = YES
        opbf-ttCEFormatConfig.summColItemNameShow         = YES
        opbf-ttCEFormatConfig.summColItemNameCol          = 2
        opbf-ttCEFormatConfig.summColItemNameWidth        = 30
        opbf-ttCEFormatConfig.summColItemNameLabel        = "Item Name"
        opbf-ttCEFormatConfig.summColQuantityShow         = NO
        opbf-ttCEFormatConfig.summColQuantityCol          = 36
        opbf-ttCEFormatConfig.summColQuantityWidth        = 9
        opbf-ttCEFormatConfig.summColQuantityLabel        = "Quantity"
        opbf-ttCEFormatConfig.summColQuantityRequestShow  = YES
        opbf-ttCEFormatConfig.summColQuantityRequestCol   = 26
        opbf-ttCEFormatConfig.summColQuantityRequestWidth = 9
        opbf-ttCEFormatConfig.summColQuantityRequestLabel = "Requested"
        opbf-ttCEFormatConfig.summColQuantityYieldShow    = YES
        opbf-ttCEFormatConfig.summColQuantityYieldCol     = 36
        opbf-ttCEFormatConfig.summColQuantityYieldWidth   = 9
        opbf-ttCEFormatConfig.summColQuantityYieldLabel   = "Yielded"
        opbf-ttCEFormatConfig.summColWeightShow           = YES
        opbf-ttCEFormatConfig.summColWeightCol            = 46
        opbf-ttCEFormatConfig.summColWeightWidth          = 9
        opbf-ttCEFormatConfig.summColWeightLabel          = "Weight"
        opbf-ttCEFormatConfig.summColDirectCostShow       = NO
        opbf-ttCEFormatConfig.summColDirectCostCol        = 46
        opbf-ttCEFormatConfig.summColDirectCostWidth      = 6
        opbf-ttCEFormatConfig.summColFactoryCostShow      = YES
        opbf-ttCEFormatConfig.summColFactoryCostCol       = 56
        opbf-ttCEFormatConfig.summColFactoryCostWidth     = 6
        opbf-ttCEFormatConfig.summColFullCostShow         = YES
        opbf-ttCEFormatConfig.summColFullCostCol          = 66
        opbf-ttCEFormatConfig.summColFullCostWidth        = 6
        opbf-ttCEFormatConfig.summColSellPriceShow        = YES
        opbf-ttCEFormatConfig.summColSellPriceCol         = 76
        opbf-ttCEFormatConfig.summColSellPriceWidth       = 6
        .
    
    CASE ipcFormatFont:
        WHEN "Classic" THEN 
            ASSIGN 
                opbf-ttCEFormatConfig.isClassic  = YES
                opbf-ttCEFormatConfig.formatFont = "Courier New"
                .
        OTHERWISE 
        ASSIGN 
            opbf-ttCeFormatConfig.isClassic = NO
            .
    END.
        
    CASE ipcFormatMaster:
        WHEN "Standard" OR 
        WHEN "Config" THEN
            ASSIGN
                opbf-ttCEFormatConfig.showProfitPercent          = NO
                opbf-ttCEFormatConfig.printByForm                = YES
                opbf-ttCEFormatConfig.printSummary               = YES
                opbf-ttCEFormatConfig.printSummaryFirst          = YES
                opbf-ttCEFormatConfig.showAllQuantities          = NO
                opbf-ttCEFormatConfig.summColQuantityShow        = YES
                opbf-ttCEFormatConfig.summColQuantityRequestShow = NO
                opbf-ttCEFormatConfig.summColQuantityYieldShow   = NO
                opbf-ttCEFormatConfig.summColWeightShow          = NO
                opbf-ttCEFormatConfig.summColDirectCostShow      = YES
                opbf-ttCEFormatConfig.summColFactoryCostShow     = YES
                opbf-ttCEFormatConfig.summColFullCostShow        = NO
                opbf-ttCEFormatConfig.summColSellPriceShow       = YES
                opbf-ttCEFormatConfig.summColSellPriceCol        = 66
                opbf-ttCEFormatConfig.useReferenceQuantity       = YES
                .
    END.
    
END PROCEDURE.

PROCEDURE pBuildSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a format, processes temp-table to build paging structure
     Notes: Options are Consolidated or by Form (by Item -TBD or by Blank -TBD)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttCeFormatConfig FOR ttCEFormatConfig.
    
    DEFINE VARIABLE iSectionCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cSectionBy    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dRefQty       AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    EMPTY TEMP-TABLE ttSection.
    FIND FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID
        NO-ERROR.
    IF AVAILABLE bf-estCostHeader THEN 
    DO:
        RUN pBuildConfig(bf-estCostHeader.company, ipcOutputFile, BUFFER opbf-ttCEFormatConfig).
        IF bf-estCostHeader.estType EQ "Combo/Tandem" AND opbf-ttCEFormatConfig.useReferenceQuantity THEN 
        DO:
            FIND CURRENT bf-estCostHeader EXCLUSIVE-LOCK.
            RUN est\dRefQty.w (INPUT-OUTPUT bf-estCostHeader.quantityReference).
            FIND CURRENT bf-estCostHeader NO-LOCK.
        END.
        IF fTypeAllowsMult(bf-estCostHeader.estType) AND opbf-ttCEFormatConfig.showAllQuantities THEN
            glShowAllQuantities = YES.
        FIND CURRENT bf-estCostHeader EXCLUSIVE-LOCK.
        ASSIGN 
            bf-estCostHeader.printDateTime = NOW
            bf-estCostHeader.printedBy     = USERID("asi")
            . 
        FIND CURRENT bf-estCostHeader NO-LOCK.
        IF opbf-ttCEFormatConfig.printByForm THEN 
        DO:
            FOR EACH estCostForm NO-LOCK 
                WHERE estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                AND estCostForm.formNo NE 0
                BY estCostForm.formNo
                :
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.rec_keyParent = estCostForm.rec_key
                    ttSection.iSequence     = iSectionCount
                    ttSection.cType         = "Form"
                    .
            END. 
        END.
        ELSE 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN 
                ttSection.cType         = "Consolidated"
                ttSection.iSequence     = iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.
        IF CAN-DO("Set,Combo/Tandem",bf-estCostHeader.estType) AND opbf-ttCEFormatConfig.printSummary THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Summary"
                ttSection.iSequence     = IF opbf-ttCEFormatConfig.printSummaryFirst THEN 0 ELSE iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.
        IF opbf-ttCEFormatConfig.printAnalysis THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Analysis"
                ttSection.iSequence     = iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.
        IF opbf-ttCEFormatConfig.printNotes THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Notes"
                ttSection.iSequence     = iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.

    END.
    
END PROCEDURE.

PROCEDURE pBuildConfig PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER opbf-ttCEFormatConfig FOR ttCeFormatConfig.

    DEFINE VARIABLE cFile         AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lLoaded       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFormatMaster AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFormatFont   AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttCEFormatConfig.
        
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEFormat", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFormatMaster, OUTPUT lRecFound).
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEFormatFont", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFormatFont, OUTPUT lRecFound).
    
    IF cFormatMaster EQ "Config" THEN 
    DO:
        RUN pLoadConfig(ipcCompany, TEMP-TABLE opbf-ttCEFormatConfig:HANDLE, OUTPUT lLoaded).

        IF NOT lLoaded THEN 
        DO:
            RUN pBuildConfigFromTemplate(cFormatMaster, cFormatFont, BUFFER opbf-ttCEFormatConfig).
        END.
        ELSE 
            FIND FIRST opbf-ttCEFormatConfig EXCLUSIVE-LOCK NO-ERROR.
    
    END.    
    ELSE
        RUN pBuildConfigFromTemplate(cFormatMaster, cFormatFont, BUFFER opbf-ttCEFormatConfig).
    
    IF AVAILABLE opbf-ttCEFormatConfig THEN 
    DO:
        IF opbf-ttCEFormatConfig.outputFile EQ "" AND ipcOutputFile EQ "" THEN  
            ttCEFormatConfig.outputFile = "C:\tmp\CheckTest.xpr".
        ELSE IF ipcOutputFile NE "" THEN 
                ttCEFormatConfig.outputFile = ipcOutputFile.
                                        
        IF opbf-ttCEFormatConfig.characterContinue EQ "" THEN 
            opbf-ttCEFormatConfig.characterContinue = CHR(187).
        IF opbf-ttCEFormatConfig.characterMasterQuantity EQ "" THEN 
            opbf-ttCEFormatConfig.characterMasterQuantity = "*".
        IF opbf-ttCEFormatConfig.characterNumberError EQ "" THEN 
            opbf-ttCEFormatConfig.characterNumberError = "#".
        IF opbf-ttCEFormatConfig.SIMONListInclude EQ "" THEN 
            opbf-ttCEFormatConfig.SIMONListInclude = "I,M".
        IF opbf-ttCEFormatConfig.SIMONListSeparate EQ "" THEN 
            opbf-ttCEFormatConfig.SIMONListSeparate = "S,O,N".
        IF opbf-ttCEFormatConfig.maxColumnsForQuantity EQ 0 THEN 
            opbf-ttCEFormatConfig.maxColumnsForQuantity = 99.
        IF opbf-ttCEFormatConfig.rowsPerPage EQ 0 THEN 
            opbf-ttCEFormatConfig.rowsPerPage = 64.
    END.    
    
    //RUN Output_TempTableToJSON(TEMP-TABLE opbf-ttCEFormatConfig:HANDLE, "C:\temp\CEFormatConfig.json", YES).
    
END PROCEDURE.

PROCEDURE pGetSummaryCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the first 5 group level costs for a given scope (rec_key)
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcSummaryRecKey AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdGroupLevelCostTotal AS DECIMAL EXTENT 5 NO-UNDO.
    DEFINE OUTPUT PARAMETER opdGroupLevelCostPerM AS DECIMAL EXTENT 5 NO-UNDO.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    FOR EACH estCostSummary NO-LOCK 
        WHERE estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID
        AND estCostSummary.scopeRecKey EQ ipcSummaryRecKey,
        FIRST estCostGroup NO-LOCK 
        WHERE estCostGroup.estCostGroupID EQ estCostSummary.estCostGroupID:

        DO iIndex = estCostGroup.estCostGroupLevelID TO 5:
            ASSIGN 
                opdGroupLevelCostTotal[iIndex] = opdGroupLevelCostTotal[iIndex] +  estCostSummary.costTotal
                opdGroupLevelCostPerM[iIndex]  = opdGroupLevelCostPerM[iIndex] + estCostSummary.costTotalPerMFinished
                .
        END.
    END .

END PROCEDURE.



PROCEDURE pLoadConfig PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Loads Config file based on NK1 and .json file location
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplLoaded AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReadMode   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid      AS LOGICAL   NO-UNDO.


    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEFormatConfig", "C" /* Logical */, NO /* check by cust */, 
        INPUT NO /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFile, OUTPUT lFound). 
    
    IF lFound AND cFile NE "" THEN 
    DO: 
        RUN FileSys_ValidateFile(cFile, OUTPUT lValid, OUTPUT cMessage).
        
        ASSIGN
            cSourceType = "file"
            cReadMode   = "empty"
            .

        IF lValid THEN 
            oplLoaded = iphTT:READ-JSON(cSourceType, cFile, cReadMode).
    END.
    
END PROCEDURE.

PROCEDURE pPrintConsolidated PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose: Processes the output for a given form
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstFormRecKey AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 

    FIND FIRST estCostHeader NO-LOCK 
        WHERE estCostHeader.rec_key EQ ipcEstFormRecKey
        NO-ERROR.
    IF NOT AVAILABLE estCostHeader THEN RETURN.
    RUN pPrintPageHeader(BUFFER estCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintItemInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        IF fTypePrintsLayout(estCostHeader.estType) THEN 
            RUN pPrintLayoutInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintMaterialInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintOperationsInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintFreightWarehousingAndHandlingForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        //RUN pPrintCostSummaryInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).        
    END.
    FOR EACH estCostForm NO-LOCK 
        WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        RUN pPrintSeparateChargeInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    
END PROCEDURE.

PROCEDURE pPrintForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes the output for a given form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstFormRecKey AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 

    FIND FIRST estCostForm NO-LOCK 
        WHERE estCostForm.rec_key EQ ipcEstFormRecKey
        NO-ERROR.
    IF AVAILABLE estCostForm THEN 
        FIND FIRST estCostHeader NO-LOCK
            WHERE estCostHeader.estCostHeaderID EQ estCostForm.estCostHeaderID
            NO-ERROR.
    IF NOT AVAILABLE estCostHeader THEN RETURN.

    RUN pPrintPageHeader(BUFFER estCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintItemInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    IF fTypePrintsLayout(estCostHeader.estType) THEN 
        RUN pPrintLayoutInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMaterialInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintOperationsInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintFreightWarehousingAndHandlingForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintCostSummaryInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintSeparateChargeInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
END PROCEDURE.

PROCEDURE pPrintItemInfoDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the basic information for a given item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostItem  FOR estCostItem.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER iplPrintHeader AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 37.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 64.
    
    DEFINE VARIABLE dQty         AS DECIMAL NO-UNDO.
    
    IF iplPrintHeader THEN 
    DO:
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, "Qty / F-B #", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, "Name / Description", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, "Size / Color", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, "Style / Part #", NO, YES, NO).
    END.
    dQty = IF ipbf-estCostBlank.priceBasedOnYield AND ipbf-estCostBlank.quantityYielded NE 0 
        THEN ipbf-estCostBlank.quantityYielded 
        ELSE ipbf-estCostBlank.quantityRequired.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn1, dQty, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-estCostItem.itemName , 30, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.sizeDesc , 40, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.styleDesc, 30, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, fFormatNumber(ipbf-estCostBlank.formNo,2, 0, YES, NO) + "-" + fFormatNumber(ipbf-estCostBlank.blankNo,2, 0, YES, NO), NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-estCostItem.itemDescription1, 30 , NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.colorDesc, 40, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.customerPart, 30, NO, NO, NO).
    
    RUN pPrintItemInfoDetailForSourceEstimate(BUFFER ipbf-estCostBlank, iItemColumn2, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
END PROCEDURE.

PROCEDURE pPrintItemInfoDetailForSourceEstimate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: If the item has a source estimate, print additional source estimate
        details.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER ipiColumn AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
        
    DEFINE BUFFER bf-eb        FOR eb.
    DEFINE BUFFER bfSource-eb  FOR eb.
    DEFINE BUFFER bfSource-ef  FOR ef.
    DEFINE BUFFER bfBoard-item FOR ITEM.
    DEFINE BUFFER bfInk-item   FOR ITEM.
    DEFINE BUFFER bfAdder-item FOR ITEM.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO. 
    
    FOR FIRST bf-eb NO-LOCK
        WHERE bf-eb.company EQ ipbf-estCostBlank.company
        AND bf-eb.est-no EQ ipbf-estCostBlank.estimateNo
        AND bf-eb.form-no EQ ipbf-estCostBlank.formNo
        AND bf-eb.blank-no EQ ipbf-estCostBlank.blankNo
        AND bf-eb.sourceEstimate NE "",
        FIRST bfSource-eb NO-LOCK 
        WHERE bfSource-eb.company EQ bf-eb.company
        AND bfSource-eb.est-no EQ bf-eb.sourceEstimate
        AND bfSource-eb.form-no EQ bf-eb.form-no
        AND bfSource-eb.blank-no EQ bf-eb.blank-no,
        FIRST bfSource-ef NO-LOCK OF bfSource-eb,
        FIRST bfBoard-item NO-LOCK 
        WHERE bfBoard-item.company EQ bfSource-ef.company
        AND bfBoard-item.i-no EQ bfSource-ef.board
        :
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinatesString(iopiRowCount, ipiColumn, "Board: " + fGetMaterialDescription(BUFFER bfBoard-item), 40 , NO, NO, NO).
        DO iIndex = 1 TO EXTENT(bfSource-ef.adder):
            IF bfSource-ef.adder[iIndex] NE "" THEN DO: 
                FIND FIRST bfAdder-item NO-LOCK
                    WHERE bfAdder-item.company EQ bfSource-ef.company
                    AND bfAdder-item.i-no EQ bfSource-ef.adder[iIndex]
                    NO-ERROR.
                
                IF AVAILABLE bfAdder-item THEN DO:
                    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                    RUN pWriteToCoordinatesString(iopiRowCount, ipiColumn, "Adder: " + fGetMaterialDescription(BUFFER bfAdder-item), 40 , NO, NO, NO).
                    RELEASE bfAdder-item.
                END.
            END.
        END.
        DO iIndex = 1 TO EXTENT(bfSource-eb.i-code):
            IF bfSource-eb.i-code[iIndex] NE "" THEN 
            DO:
                FIND FIRST bfInk-item NO-LOCK
                    WHERE bfInk-item.company EQ bfSource-ef.company
                    AND bfInk-item.i-no EQ bfSource-eb.i-code[iIndex]
                    NO-ERROR.
                
                IF AVAILABLE bfInk-item THEN DO:
                    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                    RUN pWriteToCoordinatesString(iopiRowCount, ipiColumn, "Ink: " + fGetMaterialDescription(BUFFER bfInk-item), 40 , NO, NO, NO).
                    RELEASE bfInk-item.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pPrintItemInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 43.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 68.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID
        BREAK BY estCostBlank.blankNo:
        IF FIRST(estCostBlank.blankNo) THEN 
        DO:
            RUN pPrintItemInfoHeader(BUFFER estCostItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pPrintItemInfoDetail(BUFFER estCostItem, BUFFER estCostBlank, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
        ELSE 
            RUN pPrintItemInfoDetail(BUFFER estCostItem, BUFFER estCostBlank, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

    END.
END PROCEDURE.

PROCEDURE pPrintItemInfoHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the header/customer information for a given item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostItem FOR estCostItem.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.

    DEFINE VARIABLE iShipToColumn   AS INTEGER INITIAL 55.
    DEFINE VARIABLE iCustomerColumn AS INTEGER INITIAL 14.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-estCostItem.customerName, 20, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Ship To:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-estCostItem.shipToName, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-estCostItem.customerAddress1, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-estCostItem.shipToAddress1, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-estCostItem.customerAddress2, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-estCostItem.shipToAddress2, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-estCostItem.customerAddress3, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-estCostItem.shipToAddress3, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer ID:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-estCostItem.customerID, 8, YES, NO,NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 9, ipbf-estCostItem.shipToID, 8, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Salesperson:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-estCostItem.salesgroupName, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

END PROCEDURE.

PROCEDURE pPrintNotes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a estCostHeader rec_key, print all notes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
        
    DEFINE VARIABLE iColumn    AS INTEGER EXTENT 10 INITIAL [2,36].
    DEFINE VARIABLE iTextWidth AS INTEGER INITIAL 70.
   
    FOR EACH estCostHeader NO-LOCK 
        WHERE estCostHeader.rec_key EQ ipcEstHeaderRecKey,
        FIRST est NO-LOCK 
        WHERE est.company EQ estCostHeader.company
        AND est.est-no EQ estCostHeader.estimateNo:
        RUN pPrintNotesForRecKey(est.rec_key, "Manufacturing Notes", INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        FOR EACH estCostItem NO-LOCK 
            WHERE estCostItem.estCostHeaderID EQ estCostHeader.estCostHeaderID
            AND estCostItem.itemID NE "",
            FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ estCostHeader.company
            AND itemfg.i-no EQ estCostItem.itemID
            :
            RUN pPrintNotesForRecKey(itemfg.rec_key, "Spec Notes for Item: " + itemfg.i-no, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).                
        END.
    END. 
END PROCEDURE.

PROCEDURE pPrintNotesArray PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given an array of notes, output at set position
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiColumn AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcNotes LIKE ttNotesFormatted.noteTextArray.
    DEFINE INPUT PARAMETER ipiArraySize AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    DO iIndex = 1 TO ipiArraySize:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, ipiColumn, ipcNotes[iIndex], NO, NO, NO).
    END. 

END PROCEDURE.

PROCEDURE pPrintNotesForRecKey PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints notes for a given rec_Key
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcHeader AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iColumn    AS INTEGER EXTENT 10 INITIAL [2,36].
    DEFINE VARIABLE iTextWidth AS INTEGER INITIAL 70.
    
    EMPTY TEMP-TABLE ttNotesFormatted.
    IF DYNAMIC-FUNCTION("hasNotes", ipcRecKey) THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], ipcHeader, YES, YES, NO).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN GetNotesTempTableForObject(ipcRecKey, "", "", iTextWidth, OUTPUT TABLE ttNotesFormatted).
        FOR EACH ttNotesFormatted:
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1], "Dept:" + ttNotesFormatted.noteCode + " - " + ttNotesFormatted.noteTitle, 40, YES, NO, NO).
            //RUN pWriteToCoordinatesString(iopiRowCount, iColumn[2], ttNotesFormatted.updatedByUserID + " " + STRING(ttNotesFormatted.updatedDateTime), 30, NO, NO, NO).
            RUN pPrintNotesArray(iColumn[1], ttNotesFormatted.noteTextArray, ttNotesFormatted.noteTextArraySize, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
    END.

END PROCEDURE.

PROCEDURE pPrintPageHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.

    DEFINE VARIABLE iDateLabelColumn AS INTEGER INITIAL 47.
    DEFINE VARIABLE iPageLabelColumn AS INTEGER INITIAL 80.
    DEFINE VARIABLE iEstimateColumn  AS INTEGER INITIAL 14.
    
      
    RUN pWriteToCoordinates(iopiRowCount, 2, "Estimate Calculation", YES, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Page: " + STRING(iopiPageCount,">9"), NO, NO, YES).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Estimate #: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-estCostHeader.estimateNo, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Printed: " + STRING(ipbf-estCostHeader.printDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-estCostHeader.printedBy , NO, NO, YES).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Type: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-estCostHeader.estType, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Calculated: " + STRING(ipbf-estCostHeader.calcDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-estCostHeader.calculatedBy , NO, NO, YES).

    
END PROCEDURE.


PROCEDURE pPrintCostSummaryInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Cost Summary with either Each Qty showing or a Per M plus Total
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
        
    DEFINE BUFFER bf-PrimaryestCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-estCostForm          FOR estCostForm.

    DEFINE VARIABLE iRowStart            AS INTEGER.
    DEFINE VARIABLE iColumn              AS INTEGER   EXTENT 10 INITIAL [2,32,62].
    DEFINE VARIABLE iColumnWidth         AS INTEGER   INITIAL 10.
    
    DEFINE VARIABLE iQtyCount            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQtyCountTotal       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopeRecKey         AS CHARACTER EXTENT 100.
    DEFINE VARIABLE cQtyHeader           AS CHARACTER EXTENT 100.
    DEFINE VARIABLE dCostPerM            AS DECIMAL   EXTENT 100.
    DEFINE VARIABLE dCostTotalPerM       AS DECIMAL.
    DEFINE VARIABLE dCostTotal           AS DECIMAL.
    DEFINE VARIABLE dProfitPercent       AS DECIMAL.
    DEFINE VARIABLE lLineStarted         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iLineStart           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLineEnd             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iHeaderCount         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotCount            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountCostSmy        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMaxQtyColPerSummary AS INTEGER   INITIAL 6 NO-UNDO.

    FIND FIRST bf-PrimaryestCostHeader NO-LOCK 
        WHERE bf-PrimaryestCostHeader.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryestCostHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 
    iRowStart = iopiRowCount. /*Store reset Point*/
        
    ASSIGN 
        iQtyCountTotal               = 1
        cScopeRecKey[iQtyCountTotal] = ipbf-estCostForm.rec_key
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-estCostForm.quantityFGOnForm, 7, 0, YES, NO)
        .
    IF ipbf-ttCEFormatConfig.showAllQuantities THEN 
    DO:
        FOR EACH estCostHeader NO-LOCK
            WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
            AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
            AND estCostHeader.jobID EQ ""
            ,
            FIRST estCostForm NO-LOCK 
            WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID
            AND estCostForm.formNo EQ ipbf-estCostForm.formNo
            :
            ASSIGN 
                iQtyCountTotal               = iQtyCountTotal + 1
                cScopeRecKey[iQtyCountTotal] = estCostForm.rec_key
                cQtyHeader[iQtyCountTotal]   = fFormatNumber(estCostForm.quantityFGOnForm, 7, 0, YES, NO)
                .
            IF iQtyCountTotal EQ ipbf-ttCEFormatConfig.maxColumnsForQuantity THEN LEAVE. 
        END.             
    END.
    ELSE 
    DO: 
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "*** Totals for Qty: " +  fFormatNumber(ipbf-estCostForm.quantityFGOnForm, 7, 0, YES, NO), YES, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2] , "Per M" , YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2] + iColumnWidth, "Total", YES, YES, YES).
        IF ipbf-estCostHeader.quantityReference NE 0 THEN 
        DO:
            RUN pWriteToCoordinates(iopiRowCount, iColumn[3] , "Per M Ref Qty of " + STRING(ipbf-estCostHeader.quantityReference), YES, YES, YES). 
        END.
    END.
   
    iLineStart   = 1 .
    iLineEnd     = 0 .
    iHeaderCount = 0.
  
    IF iQtyCountTotal MOD iMaxQtyColPerSummary EQ 0 THEN
        iTotCount = TRUNC( iQtyCountTotal / iMaxQtyColPerSummary,0) .
    ELSE iTotCount = TRUNC( iQtyCountTotal / iMaxQtyColPerSummary,0) + 1.    
  
    DO  iCount = 1 TO iTotCount :  
  
        iLineEnd = iLineEnd + iMaxQtyColPerSummary .
        IF iCount NE 1 THEN
        DO:     
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
        IF ipbf-ttCEFormatConfig.showAllQuantities THEN 
        DO:
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "*** Totals Per M ", YES, YES, NO).
            DO iQtyCount = 1 TO iMaxQtyColPerSummary:
                iHeaderCount = iHeaderCount + 1.             
                IF cQtyHeader[iHeaderCount] NE "" THEN
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + (iQtyCount - 1) * iColumnWidth, cQtyHeader[iHeaderCount], 7, 0, YES, YES, YES, YES, YES).
                IF iHeaderCount EQ 1 THEN 
                    RUN pWriteToCoordinates(iopiRowCount, iColumn[2], ipbf-ttCEFormatConfig.characterMasterQuantity, YES, NO, NO).
            END.  
        END.
        FOR EACH estCostGroupLevel NO-LOCK
            BY estCostGroupLevel.estCostGroupLevelID:
            FOR EACH estCostGroup NO-LOCK 
                WHERE estCostGroup.estCostGroupLevelID EQ estCostGroupLevel.estCostGroupLevelID
                BY estCostGroup.costGroupSequence:
            
                IF ipbf-ttCEFormatConfig.showAllQuantities THEN 
                DO: /*Print values for each quantity (per M)*/                   
                
                    lLineStarted = NO.
                    iCountCostSmy = 0.    
                    DO iQtyCount = iLineStart TO iLineEnd:                    
                        iCountCostSmy = iCountCostSmy + 1 .
                        FIND FIRST estCostSummary NO-LOCK 
                            WHERE estCostSummary.estCostGroupID EQ estCostGroup.estCostGroupID  
                            AND estCostSummary.scopeRecKey EQ cScopeRecKey[iQtyCount]
                            NO-ERROR.
                        IF AVAILABLE estCostSummary THEN 
                        DO:
                            IF estCostSummary.costTotal NE 0 THEN 
                            DO:
                                IF NOT lLineStarted THEN 
                                DO: 
                                    lLineStarted = YES.
                                    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                                    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], estCostGroup.costGroupLabel, NO, NO, NO).
                                END.                        
                                RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] + (iCountCostSmy - 1) * iColumnWidth ,estCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).                               
                                dCostPerM[iQtyCount] = dCostPerM[iQtyCount] + estCostSummary.costTotalPerMFinished.
                            END.
                        END.
                    END.
                END.
                ELSE 
                DO:  /*Print only the values for the subject quantity (per M and Totals)*/ 
                    FIND FIRST estCostSummary NO-LOCK 
                        WHERE estCostSummary.estCostGroupID EQ estCostGroup.estCostGroupID
                        AND estCostSummary.scopeRecKey EQ ipbf-estCostForm.rec_key
                        NO-ERROR.
                    IF AVAILABLE estCostSummary THEN 
                    DO:
                        IF estCostSummary.costTotal NE 0 THEN 
                        DO:            
                            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fGetCostGroupLabel(ipbf-estCostHeader.company, ipbf-estCostHeader.warehouseID, estCostGroup.estCostGroupID, estCostGroup.costGroupLabel), NO, NO, NO).
                            RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] , estCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).
                            RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] + iColumnWidth, estCostSummary.costTotal , 6, 2, NO, YES, NO, NO, YES).
                            IF ipbf-estCostHeader.quantityReference NE 0 THEN 
                            DO:
                                RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[3] , estCostSummary.costTotal / (ipbf-estCostHeader.quantityReference / 1000)  , 8, 2, NO, YES, NO, NO, YES).
                                                                
                            END.
                            ASSIGN 
                                dCostTotal     = dCostTotal + estCostSummary.costTotal
                                dCostTotalPerM = dCostTotalPerM + estCostSummary.costTotalPerMFinished
                                .
                        END.
                    END.
                END.
            END.
        
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], estCostGroupLevel.estCostGroupLevelDesc, YES, NO, NO).    
            IF ipbf-ttCEFormatConfig.showAllQuantities THEN
            DO: /*Print values for each quantity (per M)*/
                DO iQtyCount = 1 TO iMaxQtyColPerSummary:   
                    IF (iLineStart + iQtyCount - 1) LE iQtyCountTotal THEN
                        RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] + (iQtyCount - 1) * iColumnWidth , dCostPerM[ iLineStart + iQtyCount - 1 ] , 6, 2, NO, YES, YES, NO, YES).
                END.
            END.
            ELSE 
            DO:
                RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] , dCostTotalPerM , 6, 2, NO, YES, YES, NO, YES).
                RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] + iColumnWidth, dCostTotal , 6, 2, NO, YES, YES, NO, YES).
                IF ipbf-estCostHeader.quantityReference NE 0 THEN 
                DO:
                    RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[3] , dCostTotal / (ipbf-estCostHeader.quantityReference / 1000)  , 8, 2, NO, YES, NO, NO, YES)
                        .                                                                
                END.
            END.
        END.
        
        iLineStart = iLineStart + iMaxQtyColPerSummary .
    
    END.
    
    IF ipbf-ttCEFormatConfig.showProfitPercent THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        dProfitPercent = 100 * (ipbf-estCostForm.sellPrice - ipbf-estCostForm.costTotalFull) / ipbf-estCostForm.sellPrice.
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Profit % ", YES, NO, NO).
        RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[2] , dProfitPercent , 6, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "%", YES, NO, NO).
    END.
            
END PROCEDURE.

PROCEDURE pPrintFreightWarehousingAndHandlingForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn        AS INTEGER EXTENT 10 INITIAL [5,18,23,29,39,47,55,64,73,82].    
    DEFINE VARIABLE dTotalFreight  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalHandling AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalStorage  AS DECIMAL NO-UNDO.
       
    ASSIGN 
        dTotalFreight  = 0
        dTotalHandling = 0
        dTotalStorage  = 0
        . 
    FOR EACH estRelease NO-LOCK 
        WHERE estRelease.company EQ ipbf-estCostHeader.company
        AND estRelease.estimateNo EQ ipbf-estCostHeader.estimateNo
        AND estRelease.quantity EQ ipbf-estCostHeader.quantityMaster
        AND estRelease.formNo EQ ipbf-estCostForm.formNo
        BREAK BY estRelease.blankNo
        :
        IF FIRST-OF(estRelease.blankNo) THEN 
        DO: 
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Release Quantity", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "From", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "To", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Via", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Pallets", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Multiplier", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Months", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[8], "Freight", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[9], "Storage", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[10], "Handling", NO, YES, YES).
        END.    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estRelease.formNo,2, 0, YES, NO) + "-" + fFormatNumber(estRelease.blankNo,2, 0, YES, NO), NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[1] + 1, estRelease.quantityRelease, 6, 0, NO, YES, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[2], estRelease.shipFromLocationID, 8, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[3], estRelease.shipToID, 8, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4], estRelease.carrierID, 8, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estRelease.quantityOfUnits, 6, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estRelease.palletMultiplier, 3, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estRelease.monthsAtShipFrom, 6, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], estRelease.freightCost, 6, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], estRelease.storageCostTotal, 6, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[10], estRelease.handlingCostTotal, 6, 2, NO, YES, NO, NO, YES).
        
        ASSIGN 
            dTotalFreight  = dTotalFreight + estRelease.freightCost
            dTotalStorage  = dTotalStorage + estRelease.storageCostTotal
            dTotalHandling = dTotalHandling + estRelease.handlingCostTotal
            .    
    END.
    IF dTotalFreight NE 0 OR dTotalStorage NE 0 OR dTotalHandling NE 0 THEN 
    DO:        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total Freight and Warehousing", YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], dTotalFreight, 6, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], dTotalStorage, 6, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[10], dTotalHandling, 6, 2, NO, YES, YES, NO, YES).
    END.
    
END PROCEDURE.



PROCEDURE pPrintLayoutInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    DEFINE VARIABLE cLabelBlank   AS CHARACTER INIT "Blank #" NO-UNDO  . 
    DEFINE VARIABLE cLabelNet     AS CHARACTER INIT "Net:" NO-UNDO  .
    DEFINE VARIABLE cLabelGross   AS CHARACTER INIT "Gross:" NO-UNDO  .
    DEFINE VARIABLE lWoodEstimate AS LOGICAL   NO-UNDO  .
   
    DEFINE VARIABLE iColumn       AS INTEGER   EXTENT 10 INITIAL [12,22,32,45,58,72].
    
    lWoodEstimate = fTypeIsWood(ipbf-estCostHeader.estType) .
    IF lWoodEstimate THEN
    DO:
        ASSIGN
            cLabelBlank = "Part Size:"
            cLabelNet   = "Process Size:"
            cLabelGross = "Raw Wood Size" .
    END.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "Width", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "Length", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Area", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "#Up/Out", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Weight", NO, YES, YES).
     
    FOR EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostFormID EQ ipbf-estCostForm.estCostFormID:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cLabelBlank + TRIM(STRING(estCostBlank.blankNo,">>9")) + ":", NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostBlank.blankWidth, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostBlank.blankLength, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostBlank.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostBlank.blankArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostBlank.areaUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostBlank.numOut, 4, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostBlank.weightPerBlank * 1000, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostBlank.weightUOM + "/M", NO, NO, NO).
    END.
    IF NOT lWoodEstimate THEN
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Die:", NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostForm.dieWidth, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostForm.dieLength,4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostForm.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.dieArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.areaUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.weightDieSheet, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.weightDieUOM, NO, NO, NO).
    END.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cLabelNet, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostForm.netWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostForm.netLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.netArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostForm.numOutNet, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.weightNetSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.weightNetUOM, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cLabelGross, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostForm.grossWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostForm.grossLength, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.grossArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.weightGrossSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.weightGrossUOM, NO, NO, NO).
    IF estCostForm.rollWidth NE 0 THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2], TRIM(STRING(estCostForm.rollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Totals->", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[2],  "Sheets:", YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + 1, estCostForm.grossQtyRequiredTotal, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.grossQtyRequiredTotalArea, 4, 5, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.grossQtyRequiredTotalAreaUOM , YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.grossQtyRequiredTotalWeight, 7, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.grossQtyRequiredTotalWeightUOM, NO, NO, NO).
END PROCEDURE.

PROCEDURE pPrintMaterialInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn      AS INTEGER EXTENT 10 INITIAL [5,20,36,48,60,70,82].
    DEFINE VARIABLE dTotal       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInM AS DECIMAL NO-UNDO.
           
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Materials", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Qty Req", NO, YES, YES).   
    RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Cost Per", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Cost/M", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Total Cost", NO, YES, YES).
    
    ASSIGN 
        dTotal       = 0
        dQuantityInM = ipbf-estCostForm.quantityFGOnForm / 1000. 
    FOR EACH estCostMaterial NO-LOCK 
        WHERE estCostMaterial.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID 
        AND estCostMaterial.estCostFormID EQ ipbf-estCostForm.estCostFormID
        BY estCostMaterial.formNo DESCENDING
        BY estCostMaterial.blankNo
        BY estCostMaterial.sequenceOfMaterial:
        
        IF estCostMaterial.isPrimarySubstrate 
            AND (NOT fTypePrintsBoard(ipbf-estCostHeader.estType) 
            OR fFormIsPurchasedFG(ipbf-estCostForm.estCostFormID)) THEN 
            NEXT.
        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostMaterial.formNo,2, 0, YES, NO) + "-" + fFormatNumber(estCostMaterial.blankNo,2, 0, YES, NO), NO, NO, YES).
        
        IF estCostMaterial.isPrimarySubstrate THEN 
        DO:
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostMaterial.itemName + IF estCostMaterial.vendorID NE "" THEN " (" + estCostMaterial.vendorID + ")" ELSE "", 30, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMaterial.quantityRequiredNoWasteInCUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, estCostMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostMaterial.costPerUOM, 7, 4, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[5] + 1, estCostMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMaterial.costTotalPerMFinishedNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costTotalNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  SU Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMaterial.quantityRequiredSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, estCostMaterial.quantityUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMaterial.costTotalPerMFinishedSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costTotalSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  Run Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMaterial.quantityRequiredRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, estCostMaterial.quantityUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMaterial.costTotalPerMFinishedRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costTotalRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  Vendor Setup",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMaterial.costSetup / dQuantityInM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costSetup, 7, 2, NO, YES, NO, NO, YES).
            ASSIGN 
                dTotal = dTotal + estCostMaterial.costTotalNoWaste
                dTotal = dTotal + estCostMaterial.costTotalSetupWaste
                dTotal = dTotal + estCostMaterial.costTotalRunWaste
                dTotal = dTotal + estCostMaterial.costSetup
                .
        END.
        ELSE 
        DO:
            IF estCostMaterial.isPurchasedFG THEN
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostMaterial.itemName + IF estCostMaterial.vendorID NE "" THEN " (" + estCostMaterial.vendorID + ")" ELSE "", 30, NO, NO, NO).
            ELSE  
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostMaterial.itemName, 30, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMaterial.quantityRequiredTotal, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, estCostMaterial.quantityUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostMaterial.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[5] + 1, estCostMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMaterial.costTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costTotal, 7, 2, NO, YES, NO, NO, YES).
            ASSIGN 
                dTotal = dTotal + estCostMaterial.costTotal
                .
        END.
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total Materials", YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], dTotal / dQuantityInM, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.

PROCEDURE pPrintMiscInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER. 
    DEFINE INPUT PARAMETER ipcSimonList AS CHARACTER.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn    AS INTEGER EXTENT 10 INITIAL [5,25,40,50,58,70,82].    
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal     AS DECIMAL NO-UNDO.
       
    ASSIGN 
        dTotalPerM = 0
        dTotal     = 0
        . 
    FOR EACH estCostMisc NO-LOCK 
        WHERE estCostMisc.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND ((ipcType EQ "Misc" AND NOT estCostMisc.isPrep) OR (ipcType EQ "Prep" AND estCostMisc.isPrep))
        AND LOOKUP(estCostMisc.SIMON, ipcSimonList) GT 0
        BREAK BY estCostMisc.formNo
        BY estCostMisc.blankNo:
        IF FIRST-OF(estCostMisc.formNo) THEN 
        DO:
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, ipcType + " Description", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "Type", NO, YES, NO).
            IF ipcType EQ "Misc" THEN 
                RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "SU Cost", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Cost Per", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[5], estCostMisc.profitPercentType, NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Cost/M", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Total Cost", NO, YES, YES).
        END.    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
       
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostMisc.formNo,2, 0, YES, NO) + "-" + fFormatNumber(estCostMisc.blankNo,2, 0, YES, NO), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostMisc.costDescription, 20, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[2], estCostMisc.costType, 4, NO, NO, NO).
        IF ipcType EQ "Misc" THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostMisc.costSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMisc.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4], estCostMisc.costUOM, 3, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostMisc.profitPercent, 3, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[5] + 1, estCostMisc.SIMON, 1, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostMisc.costTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMisc.costTotal, 7, 2, NO, YES, NO, NO, YES).
        ASSIGN 
            dTotalPerM = dTotalPerM + estCostMisc.costTotalPerMFinished
            dTotal     = dTotal + estCostMisc.costTotal
            .    
    END.
    IF dTotal NE 0 THEN 
    DO:        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total " + ipcType, YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], dTotal, 7, 2, NO, YES, YES, NO, YES).
    END.
    
END PROCEDURE.

PROCEDURE pPrintOperationsInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn     AS INTEGER EXTENT 10 INITIAL [5,30,36,42,48,55,62,70,82].    
    DEFINE VARIABLE dTotalSetup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalRun   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal      AS DECIMAL NO-UNDO.
           
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Operation", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "SU Hrs", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "Run Hrs", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Speed", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "SU Rate", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Run Rate", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "SU $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[8], "Run $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[9], "Total Cost", NO, YES, YES).
     
    FOR EACH estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostFormID EQ ipbf-estCostForm.estCostFormID
        BY estCostOperation.sequenceOfOperation: 
   
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostOperation.formNo,2, 0, YES, NO) + "-" + fFormatNumber(estCostOperation.blankNo,2, 0, YES, NO), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostOperation.operationName, 20, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.operationTimeInHHMM THEN 
        DO:
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[2], fFormatTime(estCostOperation.hoursSetup), 7, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[3], fFormatTime(estCostOperation.hoursRun), 7, NO, NO, YES).
        END.
        ELSE 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostOperation.hoursSetup, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostOperation.hoursRun, 7, 2, NO, YES, NO, NO, YES).
        END.
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostOperation.speed, 7, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostOperation.costPerHourTotalSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostOperation.costPerHourTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostOperation.costTotalSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], estCostOperation.costTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], estCostOperation.costTotal, 7, 2, NO, YES, NO, NO, YES).
        ASSIGN 
            dTotalSetup = dTotalSetup + estCostOperation.costTotalSetup
            dTotalRun   = dTotalRun + estCostOperation.costTotalRun
            dTotal      = dTotal + estCostOperation.costTotal
            .
        
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total Operations", YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], dTotalSetup, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], dTotalRun, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.

PROCEDURE pPrintSeparateChargeInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Separate charges for a given form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm      FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iColumn AS INTEGER EXTENT 10 INITIAL [5].
    
    IF CAN-FIND(FIRST estCostMisc 
        WHERE estCostMisc.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND LOOKUP(estCostMisc.SIMON, ipbf-ttCEFormatConfig.SIMONListSeparate) GT 0) THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Charges Billed Separately", NO, YES, NO).
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
END PROCEDURE.

PROCEDURE pPrintAnalysis PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Analysis section, Qty List Per M
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    

    
    DEFINE BUFFER bf-primaryEstCostHeader FOR estCostHeader.
        
    FIND FIRST bf-primaryEstCostHeader NO-LOCK 
        WHERE bf-primaryEstCostHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.
    IF NOT AVAILABLE bf-primaryEstCostHeader THEN RETURN.
    
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, YES, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, NO, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
        AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
        AND estCostHeader.jobID EQ ""
        :
        RUN pPrintAnalysisLine(BUFFER estCostHeader, NO, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.

    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
               
END PROCEDURE.

PROCEDURE pPrintAnalysisLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a cost header, prints the analysis data for
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPrimary AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iColumn      AS INTEGER EXTENT 9 INITIAL [2,10,20,30,40,50,60,70,80].
    DEFINE VARIABLE dQtyInM      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMSFTotal    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSheetsTotal AS INTEGER NO-UNDO.
    
    IF iplHeader THEN 
    DO:
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Estimate Analysis Per Thousand Finished Products", YES, NO, NO).
    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).     
    
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "Factory", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Full", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Gross", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Net", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Net", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Sell", YES, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[8], "Price", YES, NO, YES).
    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "Quantity", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "Cost", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Cost", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Margin %", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Margin %", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Price", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[8], "/MSF", YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[9], "Sheets", YES, YES, YES).
    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    ELSE 
    DO:
        ASSIGN 
            dQtyInM      = ipbf-estCostHeader.quantityMaster / 1000
            dSheetsTotal = 0
            dMSFTotal    = 0
            .
        FOR EACH estCostForm NO-LOCK 
            WHERE estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            :
            
            ASSIGN 
                dSheetsTotal = dSheetsTotal + estCostForm.grossQtyRequiredTotal
                dMSFTotal    = dMSFTotal + estCostForm.grossQtyRequiredTotalArea
                .
        END.    
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], ipbf-estCostHeader.quantityMaster , 9, 0, NO, YES, NO, NO, YES).
        IF iplPrimary THEN RUN pWriteToCoordinates(iopiRowCount, iColumn[2] + 1, "*", NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], ipbf-estCostHeader.costTotalFactory / dQtyInM , 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostHeader.costTotalFull / dQtyInM , 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[5], ipbf-estCostHeader.profitPctGross , 4, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[6], ipbf-estCostHeader.profitPctNet , 4, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostHeader.sellPrice / dQtyInM , 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], ipbf-estCostHeader.sellPrice / dMSFTotal, 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], dSheetsTotal , 9, 0, NO, YES, NO, NO, YES).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.

END PROCEDURE.

PROCEDURE pPrintSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Set Summary seciton for the set header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
        
    DEFINE BUFFER bf-primaryEstCostHeader FOR estCostHeader.
    
    FIND FIRST bf-primaryEstCostHeader NO-LOCK 
        WHERE bf-primaryEstCostHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.
    IF NOT AVAILABLE bf-primaryEstCostHeader THEN RETURN.
       
    RUN pPrintPageHeader(BUFFER bf-primaryestCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR FIRST estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ bf-primaryEstCostHeader.estCostHeaderID
        AND estCostBlank.blankNo EQ 0,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID:
        
        RUN pPrintItemInfoHeader(BUFFER estCostItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintItemInfoDetail(BUFFER estCostItem, BUFFER estCostBlank, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).     
    
    RUN pPrintSummaryCosts(BUFFER bf-primaryEstCostHeader, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
        AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
        AND estCostHeader.jobID EQ ""
        :
        RUN pPrintSummaryCosts(BUFFER estCostHeader, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
               
END PROCEDURE.

PROCEDURE pProcessSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes each section
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    
    DEFINE VARIABLE iPageCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iRowCount  AS INTEGER NO-UNDO.
    
    ASSIGN 
        iPageCount = 1
        iRowCount  = 1
        .
    
    FOR EACH ttSection NO-LOCK
        BY ttSection.iSequence:
        CASE ttSection.cType:
            WHEN "Form" THEN 
            RUN pPrintForm(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            WHEN "Consolidated" THEN 
            RUN pPrintConsolidated(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            WHEN "Summary" THEN 
            RUN pPrintSummary(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).      
            WHEN "Notes" THEN 
            RUN pPrintNotes(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
            WHEN "Analysis" THEN 
            RUN pPrintAnalysis(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
        END CASE.
        RUN AddPage(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount ).
    END.
    

END PROCEDURE.

PROCEDURE pWrite PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Writes passed value to stream
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplUnformatted AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplSkip AS LOGICAL NO-UNDO.

    RUN Output_Write(ipcText, iplUnformatted, iplSkip).
    
END PROCEDURE.

PROCEDURE pWriteToCoordinates PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    
    RUN Output_WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, NO, iplRightJustified).

END PROCEDURE.

PROCEDURE pWriteToCoordinatesNum PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdNumber AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeftDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiDecimalDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplComma AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplTrim AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    cText = fFormatNumber(ipdNumber, ipiLeftDigits, ipiDecimalDigits, iplComma, NO).
    IF iplTrim THEN cText = TRIM(cText).
    RUN Output_WriteToXprint(ipdR, ipdC, cText, iplBold, iplUnderline, NO, iplRightJustified).

END PROCEDURE.

PROCEDURE pWriteToCoordinatesNumNeg PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdNumber AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiLeftDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiDecimalDigits AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplComma AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplTrim AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    cText = fFormatNumber(ipdNumber, ipiLeftDigits, ipiDecimalDigits, iplComma, YES).
    IF iplTrim THEN cText = TRIM(cText).
    RUN Output_WriteToXprint(ipdR, ipdC, cText, iplBold, iplUnderline, NO, iplRightJustified).

END PROCEDURE.
PROCEDURE pWriteToCoordinatesString PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper on Write that prefixes Coordinates passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdR AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdC AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcText AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiCharacters AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplBold AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplUnderline AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplRightJustified AS LOGICAL NO-UNDO.
    
    ipcText = fFormatString(ipcText, ipiCharacters).
    RUN Output_WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, NO, iplRightJustified).

END PROCEDURE.
PROCEDURE pPrintSummaryCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the block of Summary costs for a given cost Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader    FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE iRowStart         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iColumn           AS INTEGER   EXTENT 7 NO-UNDO.
    DEFINE VARIABLE dQtyInM           AS DECIMAL   NO-UNDO.   
    DEFINE VARIABLE dCostTotal        AS DECIMAL   EXTENT 5 NO-UNDO.
    DEFINE VARIABLE dCostPerM         AS DECIMAL   EXTENT 5 NO-UNDO.
    DEFINE VARIABLE cLevelsToPrint    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeaders          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLevels           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidths           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDecimals         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iLevel            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartLevelsAfter AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHeaderItemSumm   AS CHARACTER NO-UNDO.

    iIndex = 0.
    IF ipbf-ttCEFormatConfig.summColItemNameShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            iStartLevelsAfter = iIndex
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.summColItemNameWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.summColItemNameLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.summColItemNameCol
            .
    IF ipbf-ttCEFormatConfig.summColQuantityShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            iStartLevelsAfter = iIndex
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.summColQuantityWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.summColQuantityLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.summColQuantityCol
            .
    IF ipbf-ttCEFormatConfig.summColQuantityYieldShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            iStartLevelsAfter = iIndex
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.summColQuantityYieldWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.summColQuantityYieldLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.summColQuantityYieldCol
            .
    IF ipbf-ttCEFormatConfig.summColQuantityRequestShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            iStartLevelsAfter = iIndex
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.summColQuantityRequestWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.summColQuantityRequestLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.summColQuantityRequestCol
            .
    IF ipbf-ttCEFormatConfig.summColWeightShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            iStartLevelsAfter = iIndex
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.summColWeightWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.summColWeightLabel + " (" + ipbf-estCostHeader.weightUOM + "s),"
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.summColWeightCol
            .            
    IF ipbf-ttCEFormatConfig.summColDirectCostShow THEN 
        ASSIGN 
            iIndex          = iIndex + 1
            cWidths         = cWidths + STRING(ipbf-ttCEFormatConfig.summColDirectCostWidth) + ","
            cDecimals       = cDecimals + "2,"
            iColumn[iIndex] = ipbf-ttCEFormatConfig.summColDirectCostCol
            cLevelsToPrint  = cLevelsToPrint + "1,"
            .
    IF ipbf-ttCEFormatConfig.summColFactoryCostShow THEN 
        ASSIGN 
            iIndex          = iIndex + 1
            cWidths         = cWidths + STRING(ipbf-ttCEFormatConfig.summColFactoryCostWidth) + ","
            cDecimals       = cDecimals + "2,"
            iColumn[iIndex] = ipbf-ttCEFormatConfig.summColFactoryCostCol
            cLevelsToPrint  = cLevelsToPrint + "2,"
            .
    IF ipbf-ttCEFormatConfig.summColFullCostShow THEN 
        ASSIGN 
            iIndex          = iIndex + 1
            cWidths         = cWidths + STRING(ipbf-ttCEFormatConfig.summColFullCostWidth) + ","
            cDecimals       = cDecimals + "2,"
            iColumn[iIndex] = ipbf-ttCEFormatConfig.summColFullCostCol
            cLevelsToPrint  = cLevelsToPrint + "3,"
            .
    IF ipbf-ttCEFormatConfig.summColSellPriceShow THEN 
        ASSIGN 
            iIndex          = iIndex + 1
            cWidths         = cWidths + STRING(ipbf-ttCEFormatConfig.summColSellPriceWidth) + ","
            cDecimals       = cDecimals + "2,"
            iColumn[iIndex] = ipbf-ttCEFormatConfig.summColSellPriceCol
            cLevelsToPrint  = cLevelsToPrint + "4,"
            .
        
    
    FOR EACH estCostGroupLevel NO-LOCK
        BY estCostGroupLevel.estCostGroupLevelID:
        IF LOOKUP(STRING(estCostGroupLevel.estCostGroupLevelID), cLevelsToPrint) GT 0 THEN
            ASSIGN 
                cHeaders = cHeaders + estCostGroupLevel.estCostGroupLevelDesc + ","  
                cLevels  = cLevels + estCostGroupLevel.estCostGroupLevelDesc + "," 
                .
    END.
    
    ASSIGN 
        cWidths        = TRIM(cWidths,",")
        cDecimals      = TRIM(cDecimals,",")
        cLevelsToPrint = TRIM(cLevelsToPrint,",")
        cHeaders       = TRIM(cHeaders,",")
        cLevels        = TRIM(cLevels,",")
        .            

    IF ipbf-estCostHeader.estType EQ "Set" THEN     
        cHeaderItemSumm = "Component Summary Totals (Costs per M) for " + STRING(ipbf-estCostHeader.quantityMaster) + " sets".
    ELSE 
        cHeaderItemSumm = "Item Summary Totals (Costs per M) for " + STRING(ipbf-estCostHeader.quantityMaster) + " products".
        
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cHeaderItemSumm, YES, NO, NO).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).     
    
    DO iIndex = 1 TO EXTENT(iColumn):
        IF iIndex LE NUM-ENTRIES(cHeaders) THEN 
            IF iIndex EQ 1 THEN 
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[iIndex], ENTRY(iIndex,cHeaders), 15, YES, YES, NO).
            ELSE 
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[iIndex], ENTRY(iIndex,cHeaders), 15, YES, YES, YES). 
    END.

    FOR EACH estCostItem NO-LOCK 
        WHERE estCostItem.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        AND NOT estCostItem.isSet
        BY estCostItem.formNo
        BY estCostItem.blankNo:
        
        RUN pGetSummaryCosts(estCostItem.estCostHeaderID, estCostItem.rec_key, OUTPUT dCostTotal, OUTPUT dCostPerM).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        DO iIndex = 1 TO EXTENT(iColumn):
            IF iIndex LE NUM-ENTRIES(cHeaders) THEN 
            DO:
                IF iIndex LE iStartLevelsAfter THEN 
                    CASE iIndex:
                        WHEN 1 THEN 
                        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[iIndex], estCostItem.itemName, INTEGER(ENTRY(iIndex,cWidths)), NO, NO, NO).
                        WHEN 2 THEN 
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[iIndex], estCostItem.quantityRequired ,INTEGER(ENTRY(iIndex,cWidths)), INTEGER(ENTRY(iIndex,cDecimals)), YES, YES, NO, NO, YES).
                        WHEN 3 THEN 
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[iIndex], estCostItem.quantityYielded ,INTEGER(ENTRY(iIndex,cWidths)), INTEGER(ENTRY(iIndex,cDecimals)), YES, YES, NO, NO, YES).
                        WHEN 4 THEN 
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[iIndex], estCostItem.weightTotal ,INTEGER(ENTRY(iIndex,cWidths)), INTEGER(ENTRY(iIndex,cDecimals)), YES, YES, NO, NO, YES).
                    END CASE.
                ELSE 
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[iIndex], dCostPerM[INTEGER(ENTRY(iIndex - iStartLevelsAfter,cLevelsToPrint))], INTEGER(ENTRY(iIndex,cWidths)), INTEGER(ENTRY(iIndex,cDecimals)), NO, YES, NO, NO, YES).
            END. 
        END.   
    END.   
    
    RUN pGetSummaryCosts(ipbf-estCostHeader.estCostHeaderID, ipbf-estCostHeader.rec_key, OUTPUT dCostTotal, OUTPUT dCostPerM).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    IF ipbf-estCostHeader.estType EQ "Set" THEN     
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Summary for " + STRING(ipbf-estCostHeader.quantityMaster) + " sets", YES, YES, NO).
    ELSE 
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Summary for all items", YES, YES, NO).
        
    RUN pWriteToCoordinates(iopiRowCount, iColumn[2], "Per M", YES, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3], "Total", YES, YES, YES).
    IF ipbf-estCostHeader.quantityReference NE 0 THEN 
    DO:
        RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Per M Ref Qty of " + STRING(ipbf-estCostHeader.quantityReference), YES, YES, YES).
    END.
    DO iLevel = 1 TO 3:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], ENTRY(iLevel,cLevels), NO, NO, NO).   
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], dCostPerM[INTEGER(ENTRY(iLevel,cLevelsToPrint))], 6, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], dCostTotal[INTEGER(ENTRY(iLevel,cLevelsToPrint))] , 6, 2, NO, YES, NO, NO, YES).
        IF ipbf-estCostHeader.quantityReference NE 0 THEN
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], dCostTotal[INTEGER(ENTRY(iLevel,cLevelsToPrint))] / (ipbf-estCostHeader.quantityReference / 1000) , 6, 2, NO, YES, NO, NO, YES).
    END.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Total Shipping Weight", NO, NO, NO).   
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], ipbf-estCostHeader.weightTotal , 9, 0, YES, YES, NO, NO, YES).

    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

END PROCEDURE.



/* ************************  Function Implementations ***************** */

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL, iplAllowNegatives AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose: Formats a number with left and right digits.  Handles problem when 
     size of number doesn't fit
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatNumber", ipdNumber, ipiLeftDigits, ipiRightDigits, iplComma, iplAllowNegatives).
		
END FUNCTION.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    ( ipcString AS CHARACTER, ipiCharacters AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose:  Formats string with number of characters.  If string is larger than what fits, 
     it auto adds a "cont" string to end
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatString", ipcString, ipiCharacters).
    
		
END FUNCTION.

FUNCTION fFormatTime RETURNS CHARACTER PRIVATE
    (ipdTimeInDecimal AS DECIMAL):
    /*------------------------------------------------------------------------------
    Purpose:  Formats a time in decimal as "HH:MM"
    Notes:
    ------------------------------------------------------------------------------*/    

    RETURN DYNAMIC-FUNCTION("sfCommon_DecimalDurationInHHMM", ipdTimeInDecimal).
		
END FUNCTION.

FUNCTION fFormIsPurchasedFG RETURNS LOGICAL PRIVATE
    ( ipiFormID AS INT64 ):
    /*------------------------------------------------------------------------------
     Purpose: REturns yes, if the form has a purchased FG as a material
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    
    FIND FIRST bf-estCostMaterial NO-LOCK 
        WHERE bf-estCostMaterial.estCostFormID EQ ipiFormID
        AND bf-estCostMaterial.isPurchasedFG
        NO-ERROR.
    RETURN AVAILABLE bf-estCostMaterial.
		
END FUNCTION.

FUNCTION fGetCostGroupLabel RETURNS CHARACTER PRIVATE
	( ipcCompany AS CHARACTER, ipcLocation AS CHARACTER, ipcCostGroupID AS CHARACTER, ipcCostGroupLabel AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: Given company, costGroupID and Cost Group Label, return the appropriate label
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cLabel AS CHARACTER NO-UNDO.
    
    cLabel = ipcCostGroupLabel.
    IF LOOKUP(ipcCostGroupID, "costGroup13,costGroup14,costGroup15") GT 0 THEN 
    DO:
        FIND FIRST ce-ctrl NO-LOCK 
            WHERE ce-ctrl.company EQ ipcCompany 
            AND ce-ctrl.loc EQ ipcLocation
            NO-ERROR.
        IF AVAILABLE ce-ctrl THEN 
            CASE ipcCostGroupID:
                WHEN "costGroup13" THEN 
                    cLabel = ce-ctrl.spec-l[1].
                WHEN "costGroup14" THEN 
                    cLabel = ce-ctrl.spec-l[2].
                WHEN "costGroup15" THEN 
                    cLabel = ce-ctrl.spec-l[3].
            END CASE.
    END.
    
    RETURN cLabel.
		
END FUNCTION.

FUNCTION fGetMaterialDescription RETURNS CHARACTER PRIVATE
	(BUFFER ipbf-item FOR item ):
/*------------------------------------------------------------------------------
 Purpose:  Given an item buffer, return the description
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.
    
    IF ipbf-item.est-dscr NE "" THEN 
        cDescription = ipbf-item.est-dscr.
    ELSE 
        cDescription = ipbf-item.i-name.    

    RETURN cDescription.
    		
END FUNCTION.

FUNCTION fTypeAllowsMult RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if Given Type supports Multiple
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN DYNAMIC-FUNCTION("IsSetType",ipcEstType) 
        OR 
        DYNAMIC-FUNCTION("IsSingleType",ipcEstType)
        OR
        DYNAMIC-FUNCTION("IsMiscType",ipcEstType).
		
END FUNCTION.

FUNCTION fTypePrintsLayout RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print Layout
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN NOT DYNAMIC-FUNCTION("IsMiscType",ipcEstType).
		
END FUNCTION.

FUNCTION fTypePrintsBoard RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print board details
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN NOT DYNAMIC-FUNCTION("IsMiscType",ipcEstType).
        
END FUNCTION.

FUNCTION fTypeIsWood RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print Wood specific fields
     Notes:
    ------------------------------------------------------------------------------*/    
    
    RETURN DYNAMIC-FUNCTION("IsWoodType",ipcEstType). 
    
END FUNCTION.


