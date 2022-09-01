
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

{est/ttEstSysConfig.i}
{system\FormulaProcs.i}

DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iSequence     AS INTEGER
    FIELD cType         AS CHARACTER 
    .
    

DEFINE TEMP-TABLE ttItemName NO-UNDO 
    FIELD FormNo   LIKE estcostitem.formno
    FIELD BlankNo  LIKE estcostitem.blankno
    FIELD ItemName LIKE estcostitem.itemname.    
    
{est/ttCEFormatConfig.i}

{system\NotesProcs.i}

DEFINE STREAM sEstOutput.
DEFINE VARIABLE hdOutputProcs       AS HANDLE.
DEFINE VARIABLE hdEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE hdFormulaProcs      AS HANDLE    NO-UNDO.
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
RUN est/EstimateCalcProcs.p PERSISTENT SET hdEstimateCalcProcs.
RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fFormatDimension RETURNS DECIMAL PRIVATE
	(ipdDimension AS DECIMAL,
	 ipl16ths AS LOGICAL) FORWARD.

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
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdEstimateCalcProcs).


RUN pBuildSystemData(ipiEstCostHeaderID). 

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
        opbf-ttCEFormatConfig.printBoxDesigns             = NO
        opbf-ttCEFormatConfig.showDimensionsIn16ths       = YES 
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
        opbf-ttCEFormatConfig.analysisColQuantityShow     = YES
        opbf-ttCEFormatConfig.analysisColQuantityLabel    = "Quantity"
        opbf-ttCEFormatConfig.analysisColQuantityCol      = 8
        opbf-ttCEFormatConfig.analysisColQuantityWidth    = 9
        opbf-ttCEFormatConfig.analysisColFactCostShow     = NO 
        opbf-ttCEFormatConfig.analysisColFactCostLabel    = "FactCost"
        opbf-ttCEFormatConfig.analysisColFactCostCol      = 19
        opbf-ttCEFormatConfig.analysisColFactCostWidth    = 4
        opbf-ttCEFormatConfig.analysisColFullCostShow     = NO 
        opbf-ttCEFormatConfig.analysisColFullCostLabel    = "FullCost"
        opbf-ttCEFormatConfig.analysisColFullCostCol      = 25
        opbf-ttCEFormatConfig.analysisColFullCostWidth    = 4
        opbf-ttCEFormatConfig.analysisColGrossMarginShow  = NO 
        opbf-ttCEFormatConfig.analysisColGrossMarginLabel = "GrossMargin%"
        opbf-ttCEFormatConfig.analysisColGrossMarginCol   = 31
        opbf-ttCEFormatConfig.analysisColGrossMarginWidth = 7
        opbf-ttCEFormatConfig.analysisColNetMarginShow    = NO 
        opbf-ttCEFormatConfig.analysisColNetMarginLabel   = "NetMargin%"
        opbf-ttCEFormatConfig.analysisColNetMarginCol     = 40
        opbf-ttCEFormatConfig.analysisColNetMarginWidth   = 7
        opbf-ttCEFormatConfig.analysisColSellPriceShow    = YES
        opbf-ttCEFormatConfig.analysisColSellPriceLabel   = "SellPrice"
        opbf-ttCEFormatConfig.analysisColSellPriceCol     = 49
        opbf-ttCEFormatConfig.analysisColSellPriceWidth   = 6
        opbf-ttCEFormatConfig.analysisColPriceMSFShow     = YES
        opbf-ttCEFormatConfig.analysisColPriceMSFLabel    = "Price/MSF"
        opbf-ttCEFormatConfig.analysisColPriceMSFCol      = 57
        opbf-ttCEFormatConfig.analysisColPriceMSFWidth    = 6
        opbf-ttCEFormatConfig.analysisColSheetsShow       = YES
        opbf-ttCEFormatConfig.analysisColSheetsLabel      = "Sheets"
        opbf-ttCEFormatConfig.analysisColSheetsCol        = 65
        opbf-ttCEFormatConfig.analysisColSheetsWidth      = 6
        opbf-ttCEFormatConfig.analysisColTotalShtMSFShow  = YES 
        opbf-ttCEFormatConfig.analysisColTotalShtMSFLabel = "TotalSht MSF"
        opbf-ttCEFormatConfig.analysisColTotalShtMSFCol   = 73
        opbf-ttCEFormatConfig.analysisColTotalShtMSFWidth = 8
        opbf-ttCEFormatConfig.analysisColBoard$MShow      = YES 
        opbf-ttCEFormatConfig.analysisColBoard$MLabel     = "Board$/M"
        opbf-ttCEFormatConfig.analysisColBoard$MCol       = 19
        opbf-ttCEFormatConfig.analysisColBoard$MWidth     = 4
        opbf-ttCEFormatConfig.analysisColBoard%Show       = YES 
        opbf-ttCEFormatConfig.analysisColBoard%Label      = "Board %"
        opbf-ttCEFormatConfig.analysisColBoard%Col        = 25
        opbf-ttCEFormatConfig.analysisColBoard%Width      = 4 
        opbf-ttCEFormatConfig.analysisColTotalContbShow   = YES 
        opbf-ttCEFormatConfig.analysisColTotalContbLabel  = "TotalContb"
        opbf-ttCEFormatConfig.analysisColTotalContbCol    = 31
        opbf-ttCEFormatConfig.analysisColTotalContbWidth  = 7
        opbf-ttCEFormatConfig.analysisColContbHrShow      = YES 
        opbf-ttCEFormatConfig.analysisColContbHrLabel     = "Contb/Hr"
        opbf-ttCEFormatConfig.analysisColContbHrCol       = 40
        opbf-ttCEFormatConfig.analysisColContbHrWidth     = 7 
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
    DEFINE BUFFER bf-estCostForm   FOR estCostForm.
    
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
            FOR EACH bf-estCostForm NO-LOCK 
                WHERE bf-estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID
                AND (bf-estCostForm.formNo NE 0 OR opbf-ttCEFormatConfig.printForm0Separately)
                BY bf-estCostForm.formNo
                :
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.rec_keyParent = bf-estCostForm.rec_key
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
        IF opbf-ttCEFormatConfig.printBoxDesigns THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "BoxDesign"
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
    DEFINE VARIABLE cSetAsForm0   AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttCEFormatConfig.
        
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEFormat", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFormatMaster, OUTPUT lRecFound).
     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEFormatFont", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cFormatFont, OUTPUT lRecFound).       
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "CESetHeaderForm", "C" /* Character */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cSetAsForm0, OUTPUT lRecFound).
    
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
        IF NOT opbf-ttCEFormatConfig.printForm0Separately THEN //If not activated in template, pull from settings
            opbf-ttCEFormatConfig.printForm0Separately= cSetAsForm0 EQ "Separate Form 0" .            
        
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

PROCEDURE pBuildSystemData PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Populates the system data in Temp-tables
     Notes: If No data is setup in user specific tables then use system tables 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipiEstCostHeaderID AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    FIND FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.estCostHeaderID EQ ipiEstCostHeaderID NO-ERROR.
    
    IF AVAILABLE bf-estCostHeader THEN
        RUN Estimate_GetSystemDataForEstimate(INPUT bf-estCostHeader.company,
            OUTPUT TABLE ttEstCostCategory,
            OUTPUT TABLE ttEstCostGroup,
            OUTPUT TABLE ttEstCostGroupLevel). 
       
END PROCEDURE.

PROCEDURE pGetEstCostGroupTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Return the temp-table for EstCostGroup
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostGroup. 

END PROCEDURE.

PROCEDURE pGetEstCostGroupLevelTT PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Return the temp-table for EstCostGroupLevel
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER TABLE FOR ttEstCostGroupLevel. 

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
    
    RUN pGetEstCostGroupTT(OUTPUT TABLE ttEstCostGroup).
    
    FOR EACH estCostSummary NO-LOCK 
        WHERE estCostSummary.estCostHeaderID EQ ipiEstCostHeaderID
        AND estCostSummary.scopeRecKey EQ ipcSummaryRecKey,
        FIRST ttEstCostGroup NO-LOCK 
        WHERE ttEstCostGroup.estCostGroupID EQ estCostSummary.estCostGroupID:

        DO iIndex = ttEstCostGroup.estCostGroupLevelID TO 5:
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

PROCEDURE pPrintBoxDetail PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-eb FOR eb.
DEFINE PARAMETER BUFFER ipbf-ef FOR ef.
DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.

DEFINE VARIABLE cFGItem AS CHARACTER FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE cFGitemName AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE cHdr AS CHARACTER FORMAT "X(100)" NO-UNDO.
DEFINE VARIABLE cLineText AS CHARACTER FORMAT "X(65)" NO-UNDO.
DEFINE VARIABLE iLengthFGItem AS INTEGER NO-UNDO.
DEFINE VARIABLE cLscore LIKE box-design-hdr.lscore NO-UNDO.
DEFINE VARIABLE cLcumscore LIKE box-design-hdr.lcum-score NO-UNDO.

DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
          
        cFGItem = "Board:" + string(ipbf-ef.board) + ",".    
        FIND FIRST ITEM NO-LOCK 
            WHERE ITEM.company EQ ipbf-ef.company
              AND ITEM.i-no EQ ipbf-ef.board NO-ERROR.
        IF AVAILABLE ITEM THEN 
            cFGItem = cFGItem + ITEM.i-Name.
            cFGItem = cFGItem + "   FG Item: ". 
            iLengthFGItem = LENGTH (cFGItem).
            cFGItem       = cFGItem + string(ipbf-eb.stock-no,"x(15)").    
        RUN pWriteToCoordinates(iopiRowCount, 6, cFGItem, NO, NO, NO). 
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ ipbf-ef.company AND
            itemfg.i-no EQ ipbf-eb.stock-no NO-ERROR.     
        cFGItemName = IF AVAILABLE itemFG THEN itemfg.i-name ELSE "".
        RUN pWriteToCoordinates(iopiRowCount, iLengthFGItem - 7, cFGItemName, NO, NO, NO).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
           
        FIND FIRST box-design-hdr NO-LOCK
            WHERE box-design-hdr.design-no EQ 0
              AND box-design-hdr.company   EQ ipbf-eb.company 
              AND box-design-hdr.est-no    EQ ipbf-eb.est-no
              AND box-design-hdr.form-no   EQ ipbf-eb.form-no
              AND box-design-hdr.blank-no  EQ ipbf-eb.blank-no NO-ERROR.

        FIND FIRST style NO-LOCK
            WHERE style.company EQ ipbf-eb.company
              AND style.style   EQ  ipbf-eb.style NO-ERROR.
        IF NOT  AVAILABLE box-design-hdr AND  AVAILABLE  style THEN 
            FIND FIRST box-design-hdr NO-LOCK                 
                WHERE box-design-hdr.design-no EQ style.design-no NO-ERROR.
                  
        ASSIGN 
            cHdr = "Design #: " + TRIM(STRING(IF AVAILABLE style AND box-design-hdr.design-no EQ 0 THEN              
                                          style.design-no ELSE box-design-hdr.design-no,">>>")) +
                           "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
                           IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal".
                           
        RUN pWriteToCoordinates(iopiRowCount, 10, cHdr, NO, NO, NO).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        
        IF box-design-hdr.lscore BEGINS "No Design" THEN 
        DO: 
            RUN pWriteToCoordinates(iopiRowCount, 2, box-design-hdr.lscore, NO, NO, NO).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).    
        END. 
        ELSE IF AVAILABLE style AND ipbf-eb.est-type GE 5 THEN 
        DO:
            cLscore    = TRIM (box-design-hdr.lscore, " ").
            cLcumscore = TRIM (box-design-hdr.lcum-score, " ").
            RUN Output_WriteToXprintFontChange("Courier New", 11).                 
            RUN pWriteToCoordinates(iopiRowCount, 3, cLscore, NO, NO, NO).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, 3, cLcumscore, NO, NO, NO).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN Output_WriteToXprintFontChange("Calibri", 11).   
        END.  
              
        IF box-design-hdr.box-image = "" THEN 
        DO:      
            FOR EACH box-design-line OF box-design-hdr
                NO-LOCK
                WHERE box-design-line.line-text <> ""
                BREAK BY box-design-line.design-no:
                RUN pWriteToCoordinates(iopiRowCount, 2, cLineText, NO, NO, NO).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            END.
        END. 
        ELSE 
        DO:
            FILE-INFO:FILE-NAME = box-design-hdr.box-image.
            RUN Output_WriteToXprintImage(iopiRowCount,2,25,65,FILE-INFO:FULL-PATHNAME).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            FOR EACH box-design-line OF box-design-hdr NO-LOCK:
                IF AVAILABLE style AND ipbf-eb.est-type GE 5 THEN
                DO:
                    RUN pWriteToCoordinates(iopiRowCount, 70, TRIM(box-design-line.wscore), NO, NO, NO).
                    RUN pWriteToCoordinates(iopiRowCount, 75, TRIM(box-design-line.wcum-score), NO, NO, NO).
                END. 
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            END.
            IF iopiRowCount LT 20 AND iopiRowCount GT 8 THEN 
                ASSIGN iopiRowCount = iopiRowCount + 20.
        END.                          
END PROCEDURE.

PROCEDURE pPrintBoxDesign PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.

DEFINE BUFFER bf-primaryEstCostHeader FOR estCostHeader.
    
    FIND FIRST bf-primaryEstCostHeader NO-LOCK 
        WHERE bf-primaryEstCostHeader.rec_key EQ ipcEstHeaderRecKey NO-ERROR.
    IF NOT AVAILABLE bf-primaryEstCostHeader THEN RETURN.  
    
    FOR EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ bf-primaryEstCostHeader.estCostHeaderID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID,
        FIRST ef NO-LOCK 
        WHERE ef.est-no EQ estCostBlank.estimateNo
          AND ef.form-no EQ estCostBlank.formNo,
        FIRST eb NO-LOCK 
        WHERE eb.est-no EQ ef.est-no 
          AND eb.form-no EQ ef.form-no
          AND eb.blank-no EQ estCostBlank.blankNo
              BY estCostItem.formNo
              BY estCostItem.blankNo:   
                   
        RUN pPrintBoxInfoHeader(BUFFER estCostItem, BUFFER bf-primaryEstCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintBoxDetail(BUFFER eb, BUFFER ef, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        IF iopiRowCount GT 40 THEN     
            RUN AddPage(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount ).
    END.
        
 
END PROCEDURE.

PROCEDURE pPrintBoxInfoHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Print Box Info Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostItem   FOR estCostItem.
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount  AS INTEGER.
    
    DEFINE VARIABLE cOperationIds        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOperationIdsOrdered AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount               AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartColumn AS INTEGER INITIAL 2.
    
    FOR EACH estCostOperation NO-LOCK
        WHERE estCostOperation.Company EQ ipbf-estCostHeader.company
          AND estCostOperation.EstimateNo EQ ipbf-estCostHeader.EstimateNo 
              :
              IF LOOKUP(estCostOperation.operationID,cOperationIds) = 0 THEN
                  cOperationIds = cOperationIds + "," + estCostOperation.operationID.
    END. 
             
    ASSIGN cOperationIds = TRIM(cOperationIds, ",").
    DO iCount = NUM-ENTRIES(cOperationIds) TO 1 BY -1:
        cOperationIdsOrdered = cOperationIdsOrdered + "," + ENTRY (iCount, cOperationIds).
    END.  
    ASSIGN cOperationIdsOrdered = TRIM(cOperationIdsOrdered, ","). 
    RUN pWriteToCoordinates(iopiRowCount, iStartColumn, "Estimate#: " + ipbf-estCostHeader.estimateno, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 

    RUN pWriteToCoordinates(iopiRowCount, iStartColumn, "Customer:", NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iStartColumn + 8, ipbf-estCostItem.customerID + FILL(" ", 11 - LENGTH(ipbf-estCostItem.customerID)) + ipbf-estCostItem.customerName, 41, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iStartColumn, cOperationIdsOrdered, NO, NO, NO).
   
END PROCEDURE.

PROCEDURE pPrintConsolidated PRIVATE:
    /*------------------------------------------------------------------------------
        Purpose: Processes the output for a given form
        Notes:
       ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT PARAMETER iplPrintPageHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 
    
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-estCostForm   FOR estCostForm.
    
    FIND FIRST bf-estCostHeader NO-LOCK 
        WHERE bf-estCostHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.

    IF NOT AVAILABLE bf-estCostHeader THEN RETURN.
    IF iplPrintPageHeader THEN 
        RUN pPrintPageHeader(BUFFER bf-estCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintItemInfoForHeader(BUFFER bf-estCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMaterialInfoForHeader(BUFFER bf-estCostHeader,INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR EACH bf-estCostForm NO-LOCK 
        WHERE bf-estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
        RUN pPrintMiscInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.                                                                                                                                                                          
    RELEASE bf-estCostForm.
    RUN pPrintOperationsInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RELEASE bf-estCostForm.
    RUN pPrintFreightWarehousingAndHandlingForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RELEASE bf-estCostForm.
    RUN pPrintCostSummaryInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH bf-estCostForm NO-LOCK 
        WHERE bf-estCostForm.estCostHeaderID EQ bf-estCostHeader.estCostHeaderID:
        RUN pPrintSeparateChargeInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
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

    DEFINE VARIABLE lSubAssemblyPrinted AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-estCostForm FOR estCostForm.
    DEFINE BUFFER bf-estCostHeader FOR estCostHeader.
    
    FIND FIRST bf-estCostForm NO-LOCK 
        WHERE bf-estCostForm.rec_key EQ ipcEstFormRecKey
        NO-ERROR.
    IF AVAILABLE bf-estCostForm THEN 
        FIND FIRST bf-estCostHeader NO-LOCK
            WHERE bf-estCostHeader.estCostHeaderID EQ bf-estCostForm.estCostHeaderID
            NO-ERROR.
    IF NOT AVAILABLE bf-estCostHeader THEN RETURN.

    RUN pPrintPageHeader(BUFFER bf-estCostHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    //Print sub assembly if required 
    IF ipbf-ttCEFormatConfig.printSubAssemblyDetail THEN 
        RUN pPrintSubAssembly(BUFFER bf-estCostHeader, BUFFER bf-estCostForm , BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount, OUTPUT lSubAssemblyPrinted).
    IF NOT lSubAssemblyPrinted THEN DO:
        //Standard Form Printing
        RUN pPrintItemInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        IF fTypePrintsLayout(bf-estCostHeader.estType) THEN 
            RUN pPrintLayoutInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMaterialInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintOperationsInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintFreightWarehousingAndHandlingForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintCostSummaryInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintSeparateChargeInfoForForm(BUFFER bf-estCostHeader, BUFFER bf-estCostForm, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END. //Standard form printing 
    
END PROCEDURE.

PROCEDURE pPrintItemInfoConsolidated PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the basic information for a given item - consolidated format
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostItem  FOR estCostItem.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER iplPrintHeading AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 8.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 16.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 36.
    DEFINE VARIABLE iItemColumn5 AS INTEGER INITIAL 64.
    
    DEFINE VARIABLE dQty         AS DECIMAL NO-UNDO.
    
    IF iplPrintHeading THEN 
    DO:
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, "F-B #", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, "Qty", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, "Part #", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, "Name", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn5, "Size", NO, YES, NO).
    END.
    dQty = IF ipbf-estCostBlank.priceBasedOnYield AND ipbf-estCostBlank.quantityYielded NE 0 
        THEN ipbf-estCostBlank.quantityYielded 
        ELSE ipbf-estCostBlank.quantityRequired.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, fFormatNumber(ipbf-estCostBlank.formNo,2, 0, YES, NO) + "-" + fFormatNumber(ipbf-estCostBlank.blankNo,2, 0, YES, NO), NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn2, dQty, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.customerPart, 30, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.itemName , 30, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn5, ipbf-estCostItem.sizeDesc , 40, NO, NO, NO).
    //RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.styleDesc, 30, NO, NO, NO).
    //RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-estCostItem.itemDescription1, 30 , NO, NO, NO).
    //RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.colorDesc, 40, NO, NO, NO).
        
END PROCEDURE.

PROCEDURE pPrintItemInfoDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the basic information for a given item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostItem  FOR estCostItem.
    DEFINE PARAMETER BUFFER ipbf-estCostBlank FOR estCostBlank.
    DEFINE INPUT PARAMETER iplPrintHeading AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 37.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 64.
    
    DEFINE VARIABLE dQty         AS DECIMAL NO-UNDO.
    
    IF iplPrintHeading THEN 
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

PROCEDURE pPrintItemInfoForHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH estCostBlank NO-LOCK 
        WHERE estCostBlank.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID,
        FIRST estCostItem NO-LOCK 
        WHERE estCostItem.estCostItemID EQ estCostBlank.estCostItemID
        BREAK BY estCostBlank.blankNo:
        IF FIRST(estCostBlank.blankNo) THEN 
        DO:
            RUN pPrintItemInfoHeader(BUFFER estCostItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pPrintItemInfoConsolidated(BUFFER estCostItem, BUFFER estCostBlank, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
        ELSE 
            RUN pPrintItemInfoConsolidated(BUFFER estCostItem, BUFFER estCostBlank, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

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

PROCEDURE pPrintMaterialDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the detail of a given material
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader   FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm     FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-estCostMaterial FOR estCostMaterial.
    DEFINE INPUT PARAMETER ipcPrintWhat AS CHARACTER.
    DEFINE INPUT PARAMETER ipdQuantityInM AS DECIMAL.
    DEFINE INPUT-OUTPUT PARAMETER iopdTotal AS DECIMAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
        
    DEFINE VARIABLE iColumn AS INTEGER EXTENT 10 INITIAL [5,20,36,48,60,70,82]. 
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    CASE ipcPrintWhat:
        WHEN "Heading" THEN 
            DO:
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Materials", NO, YES, NO).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Qty Req", NO, YES, YES).   
                RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Cost Per", NO, YES, YES).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Cost/M", NO, YES, YES).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Total Cost", NO, YES, YES).
            END.
        WHEN "Totals" THEN 
            DO:    
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total Materials", YES, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], iopdTotal / ipdQuantityInM, 7, 2, NO, YES, YES, NO, YES).        
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], iopdTotal, 7, 2, NO, YES, YES, NO, YES).        
            END.
        OTHERWISE 
        DO:                
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(ipbf-estCostMaterial.formNo,2, 0, YES, NO) + "-" + fFormatNumber(ipbf-estCostMaterial.blankNo,2, 0, YES, NO), NO, NO, YES).        
            IF ipbf-estCostMaterial.isPrimarySubstrate THEN 
            DO:
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, ipbf-estCostMaterial.itemName + IF ipbf-estCostMaterial.vendorID NE "" THEN " (" + ipbf-estCostMaterial.vendorID + ")" ELSE "", 30, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostMaterial.quantityRequiredNoWasteInCUOM, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, ipbf-estCostMaterial.costUOM, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], ipbf-estCostMaterial.costPerUOM, 7, 4, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[5] + 1, ipbf-estCostMaterial.costUOM, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostMaterial.costTotalPerMFinishedNoWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostMaterial.costTotalNoWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  SU Waste",NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostMaterial.quantityRequiredSetupWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, ipbf-estCostMaterial.quantityUOMWaste, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostMaterial.costTotalPerMFinishedSetupWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostMaterial.costTotalSetupWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  Run Waste",NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostMaterial.quantityRequiredRunWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, ipbf-estCostMaterial.quantityUOMWaste, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostMaterial.costTotalPerMFinishedRunWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostMaterial.costTotalRunWaste, 7, 2, NO, YES, NO, NO, YES).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "  Vendor Setup",NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostMaterial.costSetup / ipdQuantityInM, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostMaterial.costSetup, 7, 2, NO, YES, NO, NO, YES).
                ASSIGN 
                    iopdTotal = iopdTotal + ipbf-estCostMaterial.costTotalNoWaste
                    iopdTotal = iopdTotal + ipbf-estCostMaterial.costTotalSetupWaste
                    iopdTotal = iopdTotal + ipbf-estCostMaterial.costTotalRunWaste
                    iopdTotal = iopdTotal + ipbf-estCostMaterial.costSetup
                    .
            END.
            ELSE 
            DO:
                IF ipbf-estCostMaterial.isPurchasedFG THEN
                    RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, ipbf-estCostMaterial.itemName + IF ipbf-estCostMaterial.vendorID NE "" THEN " (" + ipbf-estCostMaterial.vendorID + ")" ELSE "", 30, NO, NO, NO).
                ELSE  
                    RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, ipbf-estCostMaterial.itemName, 30, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostMaterial.quantityRequiredTotal, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, ipbf-estCostMaterial.quantityUOM, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], ipbf-estCostMaterial.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesString(iopiRowCount, iColumn[5] + 1, ipbf-estCostMaterial.costUOM, 4, NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostMaterial.costTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostMaterial.costTotal, 7, 2, NO, YES, NO, NO, YES).
                ASSIGN 
                    iopdTotal = iopdTotal + ipbf-estCostMaterial.costTotal
                    .
            END.
        END.
    END CASE.

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
        RUN Notes_GetNotesTempTableForObject(ipcRecKey, "", "", iTextWidth, OUTPUT TABLE ttNotesFormatted).
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
    DEFINE VARIABLE lByForm              AS LOGICAL   NO-UNDO.

    RUN pGetEstCostGroupTT(OUTPUT TABLE ttEstCostGroup).
    RUN pGetEstCostGroupLevelTT(OUTPUT TABLE ttEstCostGroupLevel).
    
    FIND FIRST bf-PrimaryestCostHeader NO-LOCK 
        WHERE bf-PrimaryestCostHeader.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryestCostHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 
    iRowStart = iopiRowCount. /*Store reset Point*/
        
    ASSIGN 
        iQtyCountTotal               = 1.
    
    IF AVAILABLE ipbf-estCostForm THEN 
        ASSIGN         
            cScopeRecKey[iQtyCountTotal] = ipbf-estCostForm.rec_key
            cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-estCostForm.quantityFGOnForm, 7, 0, YES, NO)
            .
    ELSE 
        ASSIGN 
            cScopeRecKey[iQtyCountTotal] = ipbf-estCostHeader.rec_key
            cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-estCostHeader.quantityMaster, 7, 0, YES, NO)
            .
            
    IF ipbf-ttCEFormatConfig.showAllQuantities THEN 
    DO:
        FOR EACH estCostHeader NO-LOCK
            WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
            AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
            AND estCostHeader.jobID EQ "":
            
            ASSIGN 
                iQtyCountTotal               = iQtyCountTotal + 1.
                
            IF lByForm THEN DO:
                FIND FIRST estCostForm NO-LOCK 
                    WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID
                    AND estCostForm.formNo EQ ipbf-estCostForm.formNo
                    NO-ERROR.
                
                IF AVAILABLE estCostForm THEN 
                    ASSIGN 
                        cScopeRecKey[iQtyCountTotal] = estCostForm.rec_key
                        cQtyHeader[iQtyCountTotal]   = fFormatNumber(estCostForm.quantityFGOnForm, 7, 0, YES, NO)
                        .
            END.
            ELSE 
                ASSIGN 
                    cScopeRecKey[iQtyCountTotal] = estCostHeader.rec_key
                    cQtyHeader[iQtyCountTotal]   = fFormatNumber(estCostHeader.quantityMaster, 7, 0, YES, NO)
                    .
            IF iQtyCountTotal EQ ipbf-ttCEFormatConfig.maxColumnsForQuantity THEN LEAVE. 
        END.             
    END.
    ELSE 
    DO: 
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "*** Totals for Qty: " +  cQtyHeader[1], YES, YES, NO).
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
        FOR EACH ttEstCostGroupLevel NO-LOCK
            BY ttEstCostGroupLevel.estCostGroupLevelID:
            FOR EACH ttEstCostGroup NO-LOCK 
                WHERE ttEstCostGroup.estCostGroupLevelID EQ ttEstCostGroupLevel.estCostGroupLevelID
                BY ttEstCostGroup.costGroupSequence:
            
                IF ipbf-ttCEFormatConfig.showAllQuantities THEN 
                DO: /*Print values for each quantity (per M)*/                   
                
                    lLineStarted = NO.
                    iCountCostSmy = 0.    
                    DO iQtyCount = iLineStart TO iLineEnd:                    
                        iCountCostSmy = iCountCostSmy + 1 .
                        FIND FIRST estCostSummary NO-LOCK 
                            WHERE estCostSummary.estCostGroupID EQ ttEstCostGroup.estCostGroupID  
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
                                    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], ttEstCostGroup.costGroupLabel, NO, NO, NO).
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
                        WHERE estCostSummary.estCostGroupID EQ ttEstCostGroup.estCostGroupID
                        AND estCostSummary.scopeRecKey EQ cScopeRecKey[1]
                        NO-ERROR.
                    IF AVAILABLE estCostSummary THEN 
                    DO:
                        IF estCostSummary.costTotal NE 0 THEN 
                        DO:            
                            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fGetCostGroupLabel(ipbf-estCostHeader.company, ipbf-estCostHeader.warehouseID, ttEstCostGroup.estCostGroupID, ttEstCostGroup.costGroupLabel), NO, NO, NO).
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
            RUN pWriteToCoordinates(iopiRowCount, iColumn[1], ttEstCostGroupLevel.estCostGroupLevelDesc, YES, NO, NO).    
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
        IF AVAILABLE ipbf-estCostForm THEN 
            dProfitPercent = 100 * (ipbf-estCostForm.sellPrice - ipbf-estCostForm.costTotalFull) / ipbf-estCostForm.sellPrice.
        ELSE 
            dProfitPercent = 100 * (ipbf-estCostHeader.sellPrice - ipbf-estCostHeader.costTotalFull) / ipbf-estCostHeader.sellPrice.
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
    DEFINE VARIABLE lAllForHeader  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iFormNo        AS INTEGER NO-UNDO.
       
    ASSIGN 
        dTotalFreight  = 0
        dTotalHandling = 0
        dTotalStorage  = 0
        . 
    IF AVAILABLE ipbf-estCostForm THEN 
        ASSIGN 
            iFormNo = ipbf-estCostForm.formNo.
    ELSE 
        ASSIGN 
            lAllForHeader = YES
            iFormNo = 0
            .
    FOR EACH estRelease NO-LOCK 
        WHERE estRelease.company EQ ipbf-estCostHeader.company
        AND estRelease.estimateNo EQ ipbf-estCostHeader.estimateNo
        AND estRelease.quantity EQ ipbf-estCostHeader.quantityMaster
        AND (estRelease.formNo EQ iFormNo OR lAllForHeader EQ YES)
        BREAK 
        BY estRelease.formNo
        BY estRelease.blankNo
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
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE cLabelBlank   AS CHARACTER INIT "Blank #" NO-UNDO  . 
    DEFINE VARIABLE cLabelNet     AS CHARACTER INIT "Net:" NO-UNDO  .
    DEFINE VARIABLE cLabelGross   AS CHARACTER INIT "Gross:" NO-UNDO  .
    DEFINE VARIABLE lWoodEstimate AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE l16ths        AS LOGICAL   NO-UNDO.   
    
    DEFINE VARIABLE opdConvertTo16th LIKE estCostBlank.blankWidth NO-UNDO.
    DEFINE VARIABLE dGrossQtyRequiredTotalWeightInTons AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE iColumn       AS INTEGER   EXTENT 10 INITIAL [12,22,32,45,58,72,80].
    
    ASSIGN 
        lWoodEstimate = fTypeIsWood(ipbf-estCostHeader.estType)         
        l16ths = ipbf-ttCEFormatConfig.showDimensionsIn16ths.
        
    IF lWoodEstimate THEN
    DO:
        ASSIGN
            cLabelBlank = "Part Size:"
            cLabelNet   = "Process Size:"
            cLabelGross = "Raw Wood Size" .
    END.
             
    ASSIGN dGrossQtyRequiredTotalWeightInTons = ipbf-estCostForm.grossQtyRequiredTotalWeight / 2000 .
       
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

        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], fFormatDimension(estCostBlank.blankWidth, l16ths), 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], fFormatDimension(estCostBlank.blankLength, l16ths), 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostBlank.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostBlank.blankArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostBlank.areaUOM , NO, NO, NO). 
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostBlank.numOut, 4, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostBlank.weightPerBlank * 1000, 8, 4, NO, YES, NO, NO, YES). 
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostBlank.weightUOM + "/M", NO, NO, NO).
    END.
    IF NOT lWoodEstimate THEN
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Die:", NO, NO, YES).

        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], fFormatDimension(ipbf-estCostForm.dieWidth, l16ths), 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], fFormatDimension(ipbf-estCostForm.dieLength, l16ths) ,4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, ipbf-estCostForm.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostForm.dieArea, 4, 5, NO, YES, NO, NO, YES).        
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, ipbf-estCostForm.areaUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostForm.weightDieSheet, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, ipbf-estCostForm.weightDieUOM, NO, NO, NO).

    END.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cLabelNet, NO, NO, YES).

    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], fFormatDimension(ipbf-estCostForm.netWidth, l16ths), 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], fFormatDimension(ipbf-estCostForm.netLength, l16ths),4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, ipbf-estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostForm.netArea, 4, 5, NO, YES, NO, NO, YES).    
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, ipbf-estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], ipbf-estCostForm.numOutNet, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostForm.weightNetSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, ipbf-estCostForm.weightNetUOM, NO, NO, NO).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], cLabelGross, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], fFormatDimension(ipbf-estCostForm.grossWidth, l16ths), 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], fFormatDimension(ipbf-estCostForm.grossLength, l16ths), 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, ipbf-estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostForm.grossArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, ipbf-estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostForm.weightGrossSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, ipbf-estCostForm.weightGrossUOM, NO, NO, NO).
    IF ipbf-estCostForm.rollWidth NE 0 THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2], TRIM(STRING(ipbf-estCostForm.rollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Totals->", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[2],  "Sheets:", YES, NO, YES).

    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + 1, ipbf-estCostForm.grossQtyRequiredTotal, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostForm.grossQtyRequiredTotalArea, 4, 5, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, ipbf-estCostForm.grossQtyRequiredTotalAreaUOM , YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostForm.grossQtyRequiredTotalWeight, 7, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, ipbf-estCostForm.grossQtyRequiredTotalWeightUOM, NO, NO, NO).
    
END PROCEDURE.

PROCEDURE pPrintMaterialInfoForHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.

    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.
    DEFINE BUFFER bf-estCostForm FOR estCostForm.
    
    DEFINE VARIABLE dTotal       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInM AS DECIMAL NO-UNDO.
                   
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    ASSIGN 
        dTotal       = 0.
         
    FOR EACH bf-estCostMaterial NO-LOCK 
        WHERE bf-estCostMaterial.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID,
        FIRST bf-estCostForm NO-LOCK 
        WHERE bf-estCostForm.estCostFormID EQ bf-estCostMaterial.estCostFormID 
        BREAK 
        BY bf-estCostMaterial.estCostHeaderID
        BY bf-estCostMaterial.formNo
        BY bf-estCostMaterial.blankNo
        BY bf-estCostMaterial.sequenceOfMaterial:
        
        ASSIGN 
            dQuantityInM = bf-estCostForm.quantityFGOnForm / 1000
            .
        
        IF FIRST-OF(bf-estCostMaterial.estCostHeaderID) THEN 
            RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER bf-estCostForm, BUFFER bf-estCostMaterial, "Heading", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).            
        
        RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER bf-estCostForm, BUFFER bf-estCostMaterial, "Detail", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        
        IF LAST-OF(bf-estCostMaterial.estCostHeaderID) THEN DO:
            dQuantityInM = ipbf-estCostHeader.quantityMaster / 1000.
            RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER bf-estCostForm, BUFFER bf-estCostMaterial, "Totals", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
    END.
    
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
    
    DEFINE BUFFER bf-estCostMaterial FOR estCostMaterial.   
   
    DEFINE VARIABLE iColumn      AS INTEGER EXTENT 10 INITIAL [5,20,36,48,60,70,82].
    DEFINE VARIABLE dTotal       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQuantityInM AS DECIMAL NO-UNDO.
           
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        
    ASSIGN 
        dTotal       = 0
        dQuantityInM = ipbf-estCostForm.quantityFGOnForm / 1000. 
    
    RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-estCostMaterial, "Heading", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH bf-estCostMaterial NO-LOCK 
        WHERE bf-estCostMaterial.estCostHeaderID EQ ipbf-estCostForm.estCostHeaderID 
        AND bf-estCostMaterial.estCostFormID EQ ipbf-estCostForm.estCostFormID
        BY bf-estCostMaterial.formNo DESCENDING
        BY bf-estCostMaterial.blankNo
        BY bf-estCostMaterial.sequenceOfMaterial:
        
        IF bf-estCostMaterial.isPrimarySubstrate 
            AND (NOT fTypePrintsBoard(ipbf-estCostHeader.estType) 
            OR fFormIsPurchasedFG(ipbf-estCostForm.estCostFormID)) THEN 
            NEXT.
            
        RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-estCostMaterial, "Detail", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            
    END.    
        
    RUN pPrintMaterialDetail(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, BUFFER bf-estCostMaterial, "Totals", dQuantityInM, INPUT-OUTPUT dTotal, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
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
    DEFINE VARIABLE iEstCostFormID AS INT64 NO-UNDO.
           
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

    IF AVAILABLE ipbf-estCostForm THEN 
        iEstCostFormID = ipbf-estCostForm.estCostFormID.
    ELSE 
        iEstCostFormID = 0.      
        
    FOR EACH estCostOperation NO-LOCK 
        WHERE estCostOperation.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID 
        AND (estCostOperation.estCostFormID EQ iEstCostFormID OR iEstCostFormID EQ 0)
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
        RUN pPrintMiscInfoForForm(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, "Prep", ipbf-ttCEFormatConfig.SIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER ipbf-estCostHeader, BUFFER ipbf-estCostForm, "Misc", ipbf-ttCEFormatConfig.SIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
END PROCEDURE.

PROCEDURE pPrintAnalysis PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Analysis section, Qty List Per M
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
    
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, BUFFER ipbf-ttCEFormatConfig, YES, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, BUFFER ipbf-ttCEFormatConfig, NO, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
        AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
        AND estCostHeader.jobID EQ ""
        :
        RUN pPrintAnalysisLine(BUFFER estCostHeader, BUFFER ipbf-ttCEFormatConfig, NO, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
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
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPrimary AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iRowStart         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iColumn           AS INTEGER EXTENT 10.
    DEFINE VARIABLE dQtyInM           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMSFTotal         AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSheetsTotal      AS INTEGER NO-UNDO.
    DEFINE VARIABLE cHeadersTop       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHeaders          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLevels           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWidths           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDecimals         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iIndex            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dContbPerHR       AS DECIMAL NO-UNDO.
    
    iIndex = 0.
    ASSIGN 
        iIndex            = iIndex + 1
        cWidths           = cWidths + STRING(30) + ","
        cDecimals         = cDecimals + "0,"
        cHeadersTop       = cHeadersTop + "Estimate Analysis Per Thousand Finished Products" + ","
        cHeaders          = cHeaders + " " + ","
        iColumn[iIndex]   = 2
        .
    IF ipbf-ttCEFormatConfig.analysisColQuantityShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColQuantityWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + " " + ","
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.analysisColQuantityLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColQuantityCol
            .
    IF ipbf-ttCEFormatConfig.analysisColFactCostShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColFactCostWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING(ipbf-ttCEFormatConfig.analysisColFactCostLabel,1,4) + ","
            cHeaders          = cHeaders + SUBSTRING(ipbf-ttCEFormatConfig.analysisColFactCostLabel,5,4) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColFactCostCol
            . 
    IF ipbf-ttCEFormatConfig.analysisColFullCostShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColFullCostWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING(ipbf-ttCEFormatConfig.analysisColFullCostLabel,1,4) + ","
            cHeaders          = cHeaders + SUBSTRING(ipbf-ttCEFormatConfig.analysisColFullCostLabel,5,4) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColFullCostCol
            .    
    IF ipbf-ttCEFormatConfig.analysisColGrossMarginShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColGrossMarginWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColGrossMarginLabel,1,5) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColGrossMarginLabel,6,7) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColGrossMarginCol
            .   
    IF ipbf-ttCEFormatConfig.analysisColNetMarginShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColNetMarginWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColNetMarginLabel,1,3) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColNetMarginLabel,4,7) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColNetMarginCol
            . 
    IF ipbf-ttCEFormatConfig.analysisColBoard$MShow THEN   
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColBoard$MWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColBoard$MLabel,1,5) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColBoard$MLabel,6,3) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColBoard$MCol
            . 
    IF ipbf-ttCEFormatConfig.analysisColBoard%Show THEN   
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColBoard%Width) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + " " + ","
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.analysisColBoard%Label + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColBoard%Col
            .  
    IF ipbf-ttCEFormatConfig.analysisColTotalContbShow THEN   
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColTotalContbWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColTotalContbLabel,1,5) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColTotalContbLabel,6,5) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColTotalContbCol
            .   
    IF ipbf-ttCEFormatConfig.analysisColContbHrShow THEN   
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColContbHrWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColContbHrLabel,1,6) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColContbHrLabel,7,2) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColContbHrCol
            .       
    IF ipbf-ttCEFormatConfig.analysisColSellPriceShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColSellPriceWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColSellPriceLabel,1,4) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColSellPriceLabel,5,5) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColSellPriceCol
            .  
    IF ipbf-ttCEFormatConfig.analysisColPriceMSFShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColPriceMSFWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColPriceMSFLabel,1,5) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColPriceMSFLabel,6,4) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColPriceMSFCol
            .   
    IF ipbf-ttCEFormatConfig.analysisColSheetsShow THEN 
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColSheetsWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + " " + ","
            cHeaders          = cHeaders + ipbf-ttCEFormatConfig.analysisColSheetsLabel + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColSheetsCol
            .     
    IF ipbf-ttCEFormatConfig.analysisColTotalShtMSFShow THEN   
        ASSIGN 
            iIndex            = iIndex + 1
            cWidths           = cWidths + STRING(ipbf-ttCEFormatConfig.analysisColTotalShtMSFWidth) + ","
            cDecimals         = cDecimals + "0,"
            cHeadersTop       = cHeadersTop + SUBSTRING (ipbf-ttCEFormatConfig.analysisColTotalShtMSFLabel,1,5) + ","
            cHeaders          = cHeaders + SUBSTRING (ipbf-ttCEFormatConfig.analysisColTotalShtMSFLabel,6,7) + ","
            iColumn[iIndex]   = ipbf-ttCEFormatConfig.analysisColTotalShtMSFCol
            .                                
            
    ASSIGN cHeadersTop = TRIM (cHeadersTop,",")
           cHeaders    = TRIM (cHeaders,",").                            
    
    IF iplHeader THEN 
    DO:    
        DO iIndex = 1 TO EXTENT(iColumn):
            IF iIndex LE NUM-ENTRIES(cHeadersTop) THEN 
            DO:
                IF iIndex = 1 THEN 
                DO:
                    IF ENTRY(iIndex,cHeadersTop) NE " " THEN 
                    RUN pWriteToCoordinates(iopiRowCount, iColumn[iIndex], ENTRY(iIndex,cHeadersTop), YES, NO, NO).
                    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 
                END. 
                ELSE       
                    RUN pWriteToCoordinates(iopiRowCount, iColumn[iIndex], ENTRY(iIndex,cHeadersTop), YES, NO, NO). 
            END.    
        END.
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        DO iIndex = 1 TO EXTENT(iColumn):
            IF iIndex LE NUM-ENTRIES(cHeaders) AND ENTRY(iIndex,cHeaders) NE " " THEN 
                RUN pWriteToCoordinates(iopiRowCount, iColumn[iIndex], ENTRY(iIndex,cHeaders), YES, YES, NO).          
        END.         
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    ELSE 
    DO:
        ASSIGN 
            dQtyInM      = ipbf-estCostHeader.quantityMaster / 1000
            dSheetsTotal = 0
            dMSFTotal    = 0
            dContbPerHR  = (IF (ipbf-estCostHeader.hoursSetup + ipbf-estCostHeader.hoursRun) GT 0 THEN 
                               (((ipbf-estCostHeader.sellPrice - (ipbf-estCostHeader.costTotalBoard / dQtyInM)) * dQtyInM) 
                               / (ipbf-estCostHeader.hoursSetup + ipbf-estCostHeader.hoursRun))
                            ELSE 
                               ((ipbf-estCostHeader.sellPrice - (ipbf-estCostHeader.costTotalBoard / dQtyInM)) * dQtyInM))
            .
        FOR EACH estCostForm NO-LOCK 
            WHERE estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            :
            
            ASSIGN 
                dSheetsTotal = dSheetsTotal + estCostForm.grossQtyRequiredTotal
                dMSFTotal    = dMSFTotal + estCostForm.grossQtyRequiredTotalArea
                .
        END. 
        IF ipbf-ttCEFormatConfig.analysisColQuantityShow THEN   
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], ipbf-estCostHeader.quantityMaster , 9, 0, NO, YES, NO, NO, NO).
        IF iplPrimary THEN 
            RUN pWriteToCoordinates(iopiRowCount, iColumn[2] + LENGTH(TRIM(STRING(ipbf-estCostHeader.quantityMaster))), "*", NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColFactCostShow THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], ipbf-estCostHeader.costTotalFactory / dQtyInM , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColFullCostShow THEN
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostHeader.costTotalFull / dQtyInM , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColGrossMarginShow THEN
            RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[5], ipbf-estCostHeader.profitPctGross , 4, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColNetMarginShow THEN
            RUN pWriteToCoordinatesNumNeg(iopiRowCount, iColumn[6], ipbf-estCostHeader.profitPctNet , 4, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColBoard$MShow THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], ipbf-estCostHeader.costTotalBoard / dQtyInM , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColBoard%Show THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostHeader.costTotalBoard / ipbf-estCostHeader.sellPrice * 100 , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColTotalContbShow THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], (ipbf-estCostHeader.sellPrice - (ipbf-estCostHeader.costTotalBoard / dQtyInM)) * dQtyInM , 9, 2, NO, YES, NO, NO, NO).  
        IF ipbf-ttCEFormatConfig.analysisColContbHrShow THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], dContbPerHR , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColSellPriceShow THEN
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], ipbf-estCostHeader.sellPrice / dQtyInM , 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColPriceMSFShow THEN
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[8], ipbf-estCostHeader.sellPrice / dMSFTotal, 9, 2, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColSheetsShow THEN
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[9], dSheetsTotal , 9, 0, NO, YES, NO, NO, NO).
        IF ipbf-ttCEFormatConfig.analysisColTotalShtMSFShow THEN 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[10], dMSFTotal , 9, 2, NO, YES, NO, NO, NO).   
        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.

END PROCEDURE.

PROCEDURE pPrintSubAssembly PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm FOR estCostForm.
    DEFINE PARAMETER BUFFER ipbf-ttCEFormatConfig FOR ttCEFormatConfig.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplPrinted AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE BUFFER bf-SubAssemblyEstCostHeader FOR estCostHeader.
    
    FIND FIRST bf-eb NO-LOCK 
        WHERE bf-eb.company EQ ipbf-estCostForm.company
        AND bf-eb.est-no EQ ipbf-estCostForm.estimateNo
        AND bf-eb.form-no EQ ipbf-estCostForm.formNo
        AND bf-eb.sourceEstimate NE ""
        NO-ERROR.
    IF AVAILABLE bf-eb THEN DO:
        FIND FIRST bf-SubAssemblyEstCostHeader NO-LOCK 
            WHERE bf-SubAssemblyEstCostHeader.company EQ bf-eb.company
            AND bf-SubAssemblyEstCostHeader.estimateNo EQ bf-eb.sourceEstimate
            AND bf-SubAssemblyEstCostHeader.jobID EQ ""
            AND bf-SubAssemblyEstCostHeader.quantityMaster EQ ipbf-estCostForm.quantityFGOnForm
            NO-ERROR.
        IF NOT AVAILABLE bf-SubAssemblyEstCostHeader THEN DO:
            RUN CalculateEstimateForQuantity IN hdEstimateCalcProcs (bf-eb.company, bf-eb.sourceEstimate, NO, ipbf-estCostForm.quantityFGOnForm).
            FIND FIRST bf-SubAssemblyEstCostHeader NO-LOCK 
                WHERE bf-SubAssemblyEstCostHeader.company EQ bf-eb.company
                AND bf-SubAssemblyEstCostHeader.estimateNo EQ bf-eb.sourceEstimate
                AND bf-SubAssemblyEstCostHeader.jobID EQ ""
                AND bf-SubAssemblyEstCostHeader.quantityMaster EQ ipbf-estCostForm.quantityFGOnForm
            NO-ERROR.
        END.
        IF AVAILABLE bf-SubAssemblyEstCostHeader THEN DO: 
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, 5, "Sub Assembly for Form " + STRING(ipbf-estCostForm.formNo,"99"), YES, NO, NO).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pPrintConsolidated(bf-SubAssemblyEstCostHeader.rec_key, BUFFER ipbf-ttCEFormatConfig, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            oplPrinted = YES.
        END.
        
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
            RUN pPrintConsolidated(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, NO, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            WHEN "Summary" THEN 
            RUN pPrintSummary(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).      
            WHEN "Notes" THEN 
            RUN pPrintNotes(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
            WHEN "Analysis" THEN 
            RUN pPrintAnalysis(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
            WHEN "BoxDesign" THEN 
            RUN pPrintBoxDesign(ttSection.rec_keyParent, BUFFER ipbf-ttCEFormatConfig, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).         
        END CASE.
        IF ttSection.cType EQ "BoxDesign" THEN 
        DO: 
            IF iRowCount GT 40 THEN  
            RUN AddPage(INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount ). 
        END.    
        ELSE 
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
    DEFINE VARIABLE dPriceTotal       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPrepCostTotal    AS DECIMAL   NO-UNDO.
               
    RUN pGetEstCostGroupLevelTT(OUTPUT TABLE ttEstCostGroupLevel).
    
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
        
    
    FOR EACH ttEstCostGroupLevel NO-LOCK
        BY ttEstCostGroupLevel.estCostGroupLevelID:
        IF LOOKUP(STRING(ttEstCostGroupLevel.estCostGroupLevelID), cLevelsToPrint) GT 0 THEN
            ASSIGN 
                cHeaders = cHeaders + ttEstCostGroupLevel.estCostGroupLevelDesc + ","  
                cLevels  = cLevels + ttEstCostGroupLevel.estCostGroupLevelDesc + "," 
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
    
    IF ipbf-estCostHeader.estType EQ "Set" AND ipbf-ttCEFormatConfig.showSetPartSummary THEN
    DO:
        EMPTY TEMP-TABLE ttItemName.
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        
        IF ipbf-ttCEFormatConfig.setPartSummPartIDShow THEN
            RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPartIDCol, ipbf-ttCEFormatConfig.setPartSummPartIDLabel, YES, YES, NO).
        IF ipbf-ttCEFormatConfig.setPartSummPartDescShow THEN
            RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPartDescCol, ipbf-ttCEFormatConfig.setPartSummPartDescLabel, YES, YES, NO).
        IF ipbf-ttCEFormatConfig.setPartSummQtyPerSetShow THEN
            RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummQtyPerSetCol, ipbf-ttCEFormatConfig.setPartSummQtyPerSetLabel, YES, YES, YES).
        IF ipbf-ttCEFormatConfig.setPartSummSellPriceShow THEN
            RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummSellPriceCol, ipbf-ttCEFormatConfig.setPartSummSellPriceLabel, YES, YES, YES).
        IF ipbf-ttCEFormatConfig.setPartSummPricePerEAShow THEN
            RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPricePerEACol, ipbf-ttCEFormatConfig.setPartSummPricePerEALabel, YES, YES, YES).

        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        
        FOR EACH estCostItem NO-LOCK 
            WHERE estCostItem.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
              AND NOT estCostItem.isSet
            BREAK BY estCostItem.estCostHeaderID
            BY estCostItem.formNo
            BY estCostItem.blankNo:
            
            CREATE ttItemName.
            ASSIGN ttItemName.BlankNo  = estcostItem.BlankNo
                   ttItemName.FormNo   = estcostItem.FormNo
                   ttItemName.ItemName = estcostItem.ItemName.
            
            IF ipbf-ttCEFormatConfig.setPartSummPartIDShow THEN                   
                RUN pWriteToCoordinatesString(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPartIDCol, estCostItem.customerPart, ipbf-ttCEFormatConfig.setPartSummPartIDWidth, NO, NO, NO).
            IF ipbf-ttCEFormatConfig.setPartSummPartDescShow THEN            
                RUN pWriteToCoordinatesString(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPartDescCol, estCostItem.itemName, ipbf-ttCEFormatConfig.setPartSummPartDescWidth, NO, NO, NO).
            IF ipbf-ttCEFormatConfig.setPartSummQtyPerSetShow THEN
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummQtyPerSetCol, estCostItem.quantityPerSet, 2, 1, NO, YES, NO, NO, YES).
            IF ipbf-ttCEFormatConfig.setPartSummSellPriceShow THEN
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummSellPriceCol, estCostItem.sellprice / ipbf-estCostHeader.quantityMaster, 4, 4, NO, YES, NO, NO, YES).
            IF ipbf-ttCEFormatConfig.setPartSummPricePerEAShow THEN DO:
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPricePerEACol, estCostItem.sellprice * estCostItem.quantityPerSet / ipbf-estCostHeader.quantityMaster, 4, 2, NO, YES, NO, NO, YES).
                
            dPriceTotal = dPriceTotal + estCostItem.sellprice * estCostItem.quantityPerSet / ipbf-estCostHeader.quantityMaster.
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            
            IF LAST-OF(estCostItem.estCostHeaderID) THEN DO:
                RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPricePerEACol, "--------------", NO, NO, YES).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                RUN pWriteToCoordinates(iopiRowCount, 2, "Set Total Price ", NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.setPartSummPricePerEACol, dPriceTotal , 4, 2, NO, YES, NO, NO, YES). 
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            END.
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
    
    IF ipbf-ttCEFormatConfig.showBillablePrep THEN DO:
        FOR EACH estCostMisc NO-LOCK 
            WHERE estCostMisc.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID 
              AND estcostMisc.isPrep 
              AND estCostMisc.simon           EQ "S" 
            BREAK BY estCostMisc.formNo
                  BY estCostMisc.blankNo
                  BY estCostMisc.estCostMiscID :
            IF FIRST(estCostMisc) THEN
            DO: 
                RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Billable Prep", YES, YES, NO).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                IF ipbf-ttCEFormatConfig.billablePrepItemIDShow THEN
                    RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemIDCol, ipbf-ttCEFormatConfig.billablePrepItemIDLabel, YES, YES, NO).
                IF ipbf-ttCEFormatConfig.billablePrepItemNameShow THEN
                    RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemNameCol, ipbf-ttCEFormatConfig.billablePrepItemNameLabel, YES, YES, NO).
                IF ipbf-ttCEFormatConfig.billablePrepItemCostShow THEN
                    RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemCostCol, ipbf-ttCEFormatConfig.billablePrepItemCostLabel, YES, YES, NO).
                
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            END.
            FIND FIRST ttItemName NO-LOCK 
                 WHERE ttItemName.FormNo  EQ estCostMisc.FormNo
                   AND ttItemName.BlankNo EQ estCostMisc.BlankNo NO-ERROR.
            IF NOT AVAILABLE ttItemName AND estCostMisc.BlankNo EQ 0 THEN 
                FIND FIRST ttItemName NO-LOCK 
                     WHERE ttItemName.FormNo  EQ estCostMisc.FormNo NO-ERROR.
            
            IF ipbf-ttCEFormatConfig.billablePrepItemIDShow THEN
                RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemIDCol, estCostMisc.ItemID, NO, NO, NO).  
            IF AVAILABLE ttItemName AND ipbf-ttCEFormatConfig.billablePrepItemNameShow THEN 
                RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemNameCol, ttItemName.ItemName, NO, NO, NO). 
            IF ipbf-ttCEFormatConfig.billablePrepItemCostShow THEN
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemCostCol, estCostMisc.costTotal, 9, 2, YES, YES, NO, NO, NO). 
            
            ASSIGN dPrepCostTotal = dPrepCostTotal + estCostMisc.costTotal.
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            
            IF LAST(estCostMisc) AND ipbf-ttCEFormatConfig.billablePrepItemCostShow THEN DO:
                RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemCostCol, "-----------", NO, NO, NO).
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                RUN pWriteToCoordinates(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemIDCol, "Total: ", NO, NO, NO).
                RUN pWriteToCoordinatesNum(iopiRowCount, ipbf-ttCEFormatConfig.billablePrepItemCostCol, dPrepCostTotal , 9, 2, NO, YES, NO, NO, NO). 
                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount). 
            END.            
        END.
    END.
END PROCEDURE.



/* ************************  Function Implementations ***************** */

FUNCTION fFormatDimension RETURNS DECIMAL PRIVATE
    (ipdDimension AS DECIMAL, ipl16ths AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose:  Given dimension and whether it should be converted to 16ths, 
     return decimal value
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE dDimension AS DECIMAL NO-UNDO.

    IF ipl16ths THEN 
    DO: 
        RUN ConvertDecimalTo16ths IN hdFormulaProcs (INPUT-OUTPUT dDimension).
        dDimension = ROUND(dDimension,2).
    END.
    ELSE dDimension = ipdDimension.

    RETURN dDimension.

		
END FUNCTION.

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
    RETURN DYNAMIC-FUNCTION("fEstimate_IsSetType", ipcEstType) 
        OR 
        DYNAMIC-FUNCTION("fEstimate_IsSingleType", ipcEstType)
        OR
        DYNAMIC-FUNCTION("fEstimate_IsMiscType", ipcEstType).
		
END FUNCTION.

FUNCTION fTypePrintsLayout RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print Layout
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN NOT DYNAMIC-FUNCTION("fEstimate_IsMiscType", ipcEstType).
		
END FUNCTION.

FUNCTION fTypePrintsBoard RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print board details
     Notes:
    ------------------------------------------------------------------------------*/    
    RETURN NOT DYNAMIC-FUNCTION("fEstimate_IsMiscType", ipcEstType).
        
END FUNCTION.

FUNCTION fTypeIsWood RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns if given type should print Wood specific fields
     Notes:
    ------------------------------------------------------------------------------*/    
    
    RETURN DYNAMIC-FUNCTION("fEstimate_IsWoodType", ipcEstType).
    
END FUNCTION.


