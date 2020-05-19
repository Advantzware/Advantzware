
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
DEFINE INPUT PARAMETER ipcSectionStyle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFormatStyle AS CHARACTER NO-UNDO.


DEFINE VARIABLE gcFont              AS CHARACTER NO-UNDO.
DEFINE VARIABLE glClassic           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giQtyMaxColumn      AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRowsPerPage       AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcContinue          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcNumError          AS CHARACTER NO-UNDO.
DEFINE VARIABLE glShowAllQuantities AS LOGICAL   NO-UNDO.
DEFINE VARIABLE glShowProfitPercent AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcQtyMasterInd      AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSIMONListInclude  AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcSIMONListSeparate AS CHARACTER NO-UNDO.

ASSIGN 
    giQtyMaxColumn      = 5
    gcNumError          = "#"
    gcContinue          = CHR(187)
    giRowsPerPage       = 64
    glShowAllQuantities = NO
    glShowProfitPercent = YES
    gcQtyMasterInd      = "*"
    gcSIMONListInclude  = "I,M"
    gcSIMONListSeparate = "S,O,N"
    .

DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iSequence     AS INTEGER
    FIELD cType         AS CHARACTER 
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
    iplComma AS LOGICAL) FORWARD.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    (ipcString AS CHARACTER,
    ipiCharacters AS INTEGER) FORWARD.

FUNCTION fFormIsPurchasedFG RETURNS LOGICAL PRIVATE
    (ipiFormID AS INT64) FORWARD.

FUNCTION fTypeAllowsMult RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fTypePrintsLayout RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.

FUNCTION fTypePrintsBoard RETURNS LOGICAL PRIVATE
    (ipcEstType AS CHARACTER) FORWARD.
    
/* ***************************  Main Block  *************************** */
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutputProcs).
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdNotesProcs).
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdEstimateCalcProcs).
RUN pBuildSections(ipiEstCostHeaderID, ipcSectionStyle, ipcFormatStyle).
IF CAN-FIND(FIRST ttSection) THEN 
DO: 
    RUN Output_InitializeXprint(ipcOutputFile, YES, NO, gcFont, 11,"") .
    RUN pProcessSections(ipcSectionStyle).
    RUN Output_Close.
    RUN Output_PrintXprintFile(ipcOutputFile).
END.
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutputProcs).
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdNotesProcs).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a format, processes temp-table to build paging structure
     Notes: Options are Consolidated or by Form (by Item -TBD or by Blank -TBD)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiEstCostHeaderID AS INT64 NO-UNDO.
    DEFINE INPUT PARAMETER ipcSectionStyle AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormatStyle AS CHARACTER NO-UNDO.
    
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
        CASE ipcFormatStyle:
            WHEN "Classic" THEN 
                ASSIGN 
                    glClassic = YES
                    gcFont    = "Courier New"
                    .
            OTHERWISE 
            ASSIGN 
                glClassic = NO
                gcFont    = ipcFormatStyle
                .
        END.
        CASE ipcSectionStyle:
            WHEN "McLean" THEN 
                DO:
                    ASSIGN 
                        glShowProfitPercent = NO
                        cSectionBy          = "By Form With Summary First Mult Qty"
                        .
                    IF bf-estCostHeader.estType EQ "Combo/Tandem" THEN 
                    DO:
                        FIND CURRENT bf-estCostHeader EXCLUSIVE-LOCK.
                        RUN est\dRefQty.w (INPUT-OUTPUT bf-estCostHeader.quantityReference).
                        FIND CURRENT bf-estCostHeader NO-LOCK.
                    END.
                    
                END.
            WHEN "Standard" THEN 
                cSectionBy = "By Form With Summary First Analysis".
            OTHERWISE 
            cSectionBy = ipcSectionStyle.
        END.
        IF fTypeAllowsMult(bf-estCostHeader.estType) AND INDEX(cSectionBy, "Mult Qty") GT 0 THEN
            glShowAllQuantities = YES.
        FIND CURRENT bf-estCostHeader EXCLUSIVE-LOCK.
        ASSIGN 
            bf-estCostHeader.printDateTime = NOW
            bf-estCostHeader.printedBy     = USERID("asi")
            . 
        FIND CURRENT bf-estCostHeader NO-LOCK.
        IF cSectionBy BEGINS "By Form" THEN 
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
        ELSE IF cSectionBy BEGINS "Consolidated" THEN 
            DO:
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.cType         = "Consolidated"
                    ttSection.iSequence     = iSectionCount
                    ttSection.rec_keyParent = bf-estCostHeader.rec_key
                    .
            END.
        IF CAN-DO("Set,Combo/Tandem",bf-estCostHeader.estType) AND INDEX(cSectionBy, "with Summary") GT 0 THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Summary"
                ttSection.iSequence     = IF INDEX(cSectionBy, "First") GT 0 THEN 0 ELSE iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.
        IF INDEX(cSectionBy, "Analysis") GT 0 THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Analysis"
                ttSection.iSequence     = iSectionCount
                ttSection.rec_keyParent = bf-estCostHeader.rec_key
                .
        END.
        IF NOT INDEX(cSectionBy, "No Notes") GT 0 THEN 
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



PROCEDURE pPrintForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes the output for a given form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstFormRecKey AS CHARACTER NO-UNDO.
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
    RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Prep", gcSIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Misc", gcSIMONListInclude, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintOperationsInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintFreightWarehousingAndHandlingForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintCostSummaryInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, glShowAllQuantities, glShowProfitPercent, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintSeparateChargeInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
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
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 43.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 68.
    
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
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-estCostItem.itemName , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.sizeDesc , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.styleDesc, 16, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, fFormatNumber(ipbf-estCostBlank.formNo,2, 0, YES) + "-" + fFormatNumber(ipbf-estCostBlank.blankNo,2, 0, YES), NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-estCostItem.itemDescription1, 20 , NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-estCostItem.colorDesc, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-estCostItem.customerPart, 16, NO, NO, NO).
    
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT PARAMETER iplPerQuantity AS LOGICAL.
    DEFINE INPUT PARAMETER iplPrintProfitPercent AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE BUFFER bf-PrimaryestCostHeader FOR estCostHeader.
    DEFINE BUFFER bf-estCostForm          FOR estCostForm.

    DEFINE VARIABLE iRowStart      AS INTEGER.
    DEFINE VARIABLE iColumn        AS INTEGER   EXTENT 10 INITIAL [2,36].
    DEFINE VARIABLE iColumnWidth   AS INTEGER   INITIAL 10.
    
    DEFINE VARIABLE iQtyCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQtyCountTotal AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopeRecKey   AS CHARACTER EXTENT 10.
    DEFINE VARIABLE cQtyHeader     AS CHARACTER EXTENT 10.
    DEFINE VARIABLE dCostPerM      AS DECIMAL   EXTENT 10.
    DEFINE VARIABLE dCostTotalPerM AS DECIMAL.
    DEFINE VARIABLE dCostTotal     AS DECIMAL.
    DEFINE VARIABLE dProfitPercent AS DECIMAL.
    DEFINE VARIABLE lLineStarted   AS LOGICAL   NO-UNDO.

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
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-estCostForm.quantityFGOnForm, 7, 0, YES)
        .
    IF iplPerQuantity THEN 
    DO:
        FOR EACH estCostHeader NO-LOCK
            WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
            AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
            ,
            FIRST estCostForm NO-LOCK 
            WHERE estCostForm.estCostHeaderID EQ estCostHeader.estCostHeaderID
            AND estCostForm.formNo EQ ipbf-estCostForm.formNo
            :
            ASSIGN 
                iQtyCountTotal               = iQtyCountTotal + 1
                cScopeRecKey[iQtyCountTotal] = estCostForm.rec_key
                cQtyHeader[iQtyCountTotal]   = fFormatNumber(estCostForm.quantityFGOnForm, 7, 0, YES)
                .
            IF iQtyCountTotal EQ giQtyMaxColumn THEN LEAVE. 
        END.
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "*** Totals Per M ", YES, YES, NO).
        DO iQtyCount = 1 TO iQtyCountTotal:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + (iQtyCount - 1) * iColumnWidth, cQtyHeader[iQtyCount], 7, 0, YES, YES, YES, YES, YES).
            IF iQtyCount EQ 1 THEN 
                RUN pWriteToCoordinates(iopiRowCount, iColumn[2], gcQtyMasterInd, YES, NO, NO).
        END.
    END.
    ELSE 
    DO: 
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "*** Totals for Qty: " +  fFormatNumber(ipbf-estCostForm.quantityFGOnForm, 7, 0, YES), YES, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2] , "Per M" , YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[2] + iColumnWidth, "Total", YES, YES, YES).
    END.    
    
    FOR EACH estCostGroupLevel NO-LOCK
        BY estCostGroupLevel.estCostGroupLevelID:
        FOR EACH estCostGroup NO-LOCK 
            WHERE estCostGroup.estCostGroupLevelID EQ estCostGroupLevel.estCostGroupLevelID
            BY estCostGroup.costGroupSequence:
            
            IF iplPerQuantity THEN 
            DO: /*Print values for each quantity (per M)*/
                lLineStarted = NO.
                DO iQtyCount = 1 TO iQtyCountTotal:
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
                            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + (iQtyCount - 1) * iColumnWidth ,estCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).
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
                        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], estCostGroup.costGroupLabel, NO, NO, NO).
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] , estCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + iColumnWidth, estCostSummary.costTotal , 6, 2, NO, YES, NO, NO, YES).
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
        IF iplPerQuantity THEN
        DO: /*Print values for each quantity (per M)*/
            DO iQtyCount = 1 TO iQtyCountTotal:
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + (iQtyCount - 1) * iColumnWidth , dCostPerM[iQtyCount] , 6, 2, NO, YES, YES, NO, YES).
            END.
        END.
        ELSE 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] , dCostTotalPerM , 6, 2, NO, YES, YES, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] + iColumnWidth, dCostTotal , 6, 2, NO, YES, YES, NO, YES).
        END.
    END.
    IF iplPrintProfitPercent THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        dProfitPercent = 100 * (ipbf-estCostForm.sellPrice - ipbf-estCostForm.costTotalFull) / ipbf-estCostForm.sellPrice.
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Profit % ", YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2] , dProfitPercent , 6, 2, NO, YES, YES, NO, YES).
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
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estRelease.formNo,2, 0, YES) + "-" + fFormatNumber(estRelease.blankNo,2, 0, YES), NO, NO, YES).
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
   
    DEFINE VARIABLE iColumn AS INTEGER EXTENT 10 INITIAL [12,22,32,45,58,72].
       
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
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Blank #" + TRIM(STRING(estCostBlank.blankNo,">>9")) + ":", NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostBlank.blankWidth, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostBlank.blankLength, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostBlank.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostBlank.blankArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostBlank.areaUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostBlank.numOut, 4, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostBlank.weightPerBlank * 1000, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostBlank.weightUOM + "/M", NO, NO, NO).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Die:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostForm.dieWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostForm.dieLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.dieArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.weightDieSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.weightDieUOM, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Net:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostForm.netWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostForm.netLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[3] + 1, estCostForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostForm.netArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4] + 1, estCostForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostForm.numOutNet, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], estCostForm.weightNetSheet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6] + 1, estCostForm.weightNetUOM, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1], "Gross:", NO, NO, YES).
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
   
    DEFINE VARIABLE iColumn    AS INTEGER EXTENT 10 INITIAL [5,20,36,48,60,70,82].
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal     AS DECIMAL NO-UNDO.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Materials", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[4], "Qty Req", NO, YES, YES).   
    RUN pWriteToCoordinates(iopiRowCount, iColumn[5], "Cost Per", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[6], "Cost/M", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[7], "Total Cost", NO, YES, YES).
    
    ASSIGN 
        dTotalPerM = 0
        dTotal     = 0
        . 
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
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostMaterial.formNo,2, 0, YES) + "-" + fFormatNumber(estCostMaterial.blankNo,2, 0, YES), NO, NO, YES).
        
        IF estCostMaterial.isPrimarySubstrate THEN 
        DO:
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostMaterial.itemName + IF estCostMaterial.vendorID NE "" THEN " (" + estCostMaterial.vendorID + ")" ELSE "", 30, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], estCostMaterial.quantityRequiredNoWasteInCUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn[4] + 1, estCostMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], estCostMaterial.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
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
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[7], estCostMaterial.costSetup, 7, 2, NO, YES, NO, NO, YES).
            ASSIGN 
                dTotalPerM = dTotalPerM + estCostMaterial.costTotalPerMFinishedNoWaste
                dTotal     = dTotal + estCostMaterial.costTotalNoWaste
                dTotalPerM = dTotalPerM + estCostMaterial.costTotalPerMFinishedSetupWaste
                dTotal     = dTotal + estCostMaterial.costTotalSetupWaste
                dTotalPerM = dTotalPerM + estCostMaterial.costTotalPerMFinishedRunWaste
                dTotal     = dTotal + estCostMaterial.costTotalRunWaste
                dTotal     = dTotal + estCostMaterial.costSetup
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
                dTotalPerM = dTotalPerM + estCostMaterial.costTotalPerMFinished
                dTotal     = dTotal + estCostMaterial.costTotal
                .
        END.
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Total Materials", YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
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
       
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostMisc.formNo,2, 0, YES) + "-" + fFormatNumber(estCostMisc.blankNo,2, 0, YES), NO, NO, YES).
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
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
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1], fFormatNumber(estCostOperation.formNo,2, 0, YES) + "-" + fFormatNumber(estCostOperation.blankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn[1] + 1, estCostOperation.operationName, 20, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], estCostOperation.hoursSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], estCostOperation.hoursRun, 7, 2, NO, YES, NO, NO, YES).
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
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE PARAMETER BUFFER ipbf-estCostForm   FOR estCostForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iColumn AS INTEGER EXTENT 10 INITIAL [5].
    
    IF CAN-FIND(FIRST estCostMisc 
        WHERE estCostMisc.estCostFormID EQ ipbf-estCostForm.estCostFormID
        AND LOOKUP(estCostMisc.SIMON, gcSIMONListSeparate) GT 0) THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn[1] + 1, "Charges Billed Separately", NO, YES, NO).
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Prep", gcSIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintMiscInfoForForm(BUFFER estCostHeader, BUFFER estCostForm, "Misc", gcSIMONListSeparate, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
END PROCEDURE.

PROCEDURE pPrintAnalysis PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Analysis section, Qty List Per M
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    

    
    DEFINE BUFFER bf-primaryEstCostHeader FOR estCostHeader.
    
    IF ipcFormat EQ "McLean" THEN RETURN.
    
    FIND FIRST bf-primaryEstCostHeader NO-LOCK 
        WHERE bf-primaryEstCostHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.
    IF NOT AVAILABLE bf-primaryEstCostHeader THEN RETURN.
    
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, YES, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintAnalysisLine(BUFFER bf-primaryEstCostHeader, NO, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
        AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
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
    
    DEFINE VARIABLE iColumn AS INTEGER EXTENT 9 INITIAL [2,10,20,30,40,50,60,70,80].
    DEFINE VARIABLE dQtyInM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dMSFTotal AS DECIMAL NO-UNDO.
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
            dQtyInM = ipbf-estCostHeader.quantityMaster / 1000
            dSheetsTotal = 0
            dMSFTotal = 0
            .
        FOR EACH estCostForm NO-LOCK 
            WHERE estCostForm.estCostHeaderID EQ ipbf-estCostHeader.estCostHeaderID
            :
            
            ASSIGN 
                dSheetsTotal = dSheetsTotal + estCostForm.grossQtyRequiredTotal
                dMSFTotal = dMSFTotal + estCostForm.grossQtyRequiredTotalArea
                .
        END.    
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[2], ipbf-estCostHeader.quantityMaster , 9, 0, NO, YES, NO, NO, YES).
        IF iplPrimary THEN RUN pWriteToCoordinates(iopiRowCount, iColumn[2] + 1, "*", NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[3], ipbf-estCostHeader.costTotalFactory / dQtyInM , 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[4], ipbf-estCostHeader.costTotalFull / dQtyInM , 9, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[5], ipbf-estCostHeader.profitPctGross , 4, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn[6], ipbf-estCostHeader.profitPctNet , 4, 2, NO, YES, NO, NO, YES).
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
    DEFINE INPUT PARAMETER ipcFormat AS CHARACTER NO-UNDO.
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
    
    RUN pPrintSummaryCosts(BUFFER bf-primaryEstCostHeader, ipcFormat, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH estCostHeader NO-LOCK
        WHERE estCostHeader.estimateNo EQ bf-PrimaryestCostHeader.estimateNo
        AND estCostHeader.estCostHeaderID NE bf-PrimaryestCostHeader.estCostHeaderID
        :
        RUN pPrintSummaryCosts(BUFFER estCostHeader, ipcFormat, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
               
END PROCEDURE.

PROCEDURE pProcessSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes each section
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFormat AS CHARACTER NO-UNDO.
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
            RUN pPrintForm(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            WHEN "Consolidated" THEN 
            RUN pPrintConsolidated(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).
            WHEN "Summary" THEN 
            RUN pPrintSummary(ttSection.rec_keyParent, ipcFormat, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).      
            WHEN "Notes" THEN 
            RUN pPrintNotes(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
            WHEN "Analysis" THEN 
            RUN pPrintAnalysis(ttSection.rec_keyParent, ipcFormat, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).          
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
    cText = fFormatNumber(ipdNumber, ipiLeftDigits, ipiDecimalDigits, iplComma).
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


/* ************************  Function Implementations ***************** */

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose: Formats a number with left and right digits.  Handles problem when 
     size of number doesn't fit
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatNumber", ipdNumber, ipiLeftDigits, ipiRightDigits, iplComma, NO).
		
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

PROCEDURE pPrintSummaryCosts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the block of Summary costs for a given cost Header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-estCostHeader FOR estCostHeader.
    DEFINE INPUT PARAMETER ipcFormat AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE iRowStart         AS INTEGER.
    DEFINE VARIABLE iColumn           AS INTEGER   EXTENT 7 INITIAL [2,26,36,46,56,66,76].
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

    IF ipcFormat EQ "McLean" THEN 
        ASSIGN 
            cWidths           = "30,9,6,6,6"
            cDecimals         = "0,0,2,2,2"
            cHeaders          = "Item Name,Quantity"
            iStartLevelsAfter = 2
            cLevelsToPrint    = "1,2,4"
            .
    ELSE 
        ASSIGN 
            cWidths           = "30,9,9,9,6,6,6"
            cDecimals         = "0,0,0,0,2,2,2"
            cHeaders          = "Item Name,Required,Yielded,Weight (" + ipbf-estCostHeader.weightUOM + "s)"
            iStartLevelsAfter = 4
            cLevelsToPrint    = "2,3,4"
            .
    
    
    FOR EACH estCostGroupLevel NO-LOCK
        BY estCostGroupLevel.estCostGroupLevelID:
        IF LOOKUP(STRING(estCostGroupLevel.estCostGroupLevelID), cLevelsToPrint) GT 0 THEN
            ASSIGN 
                cHeaders = cHeaders + "," + estCostGroupLevel.estCostGroupLevelDesc
                cLevels  = cLevels + "," + estCostGroupLevel.estCostGroupLevelDesc.
    END.
    
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
        BY estCostItem.itemName:
        
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
    
    cLevels = TRIM(cLevels,",").
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

