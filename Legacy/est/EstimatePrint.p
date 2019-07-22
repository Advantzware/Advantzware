
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
DEFINE INPUT PARAMETER ipriEstHeader AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcSectionStyle AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFormatStyle AS CHARACTER NO-UNDO.

{est/ttEstPrint.i SHARED}

DEFINE VARIABLE gcFont              AS CHARACTER NO-UNDO.
DEFINE VARIABLE glClassic           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giQtyMaxColumn      AS INTEGER   NO-UNDO.
DEFINE VARIABLE giRowsPerPage       AS INTEGER   NO-UNDO.
DEFINE VARIABLE gcContinue          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcNumError          AS CHARACTER NO-UNDO.
DEFINE VARIABLE glShowAllQuantities AS LOGICAL   NO-UNDO.
DEFINE VARIABLE gcQtyMasterInd      AS CHARACTER NO-UNDO.

ASSIGN 
    giQtyMaxColumn      = 5
    gcNumError          = "#"
    gcContinue          = CHR(187)
    giRowsPerPage       = 64
    glShowAllQuantities = NO
    gcQtyMasterInd      = "*".

DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iSequence     AS INTEGER
    FIELD cType         AS CHARACTER 
    .

DEFINE STREAM sEstOutput.
DEFINE VARIABLE hdOutputProcs AS HANDLE.
RUN system/OutputProcs.p PERSISTENT SET hdOutputProcs.
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


/* ***************************  Main Block  *************************** */
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (hdOutputProcs).
RUN pBuildSections(ipriEstHeader, ipcSectionStyle, ipcFormatStyle).
IF CAN-FIND(FIRST ttSection) THEN 
DO: 
    RUN InitializeOutputXprint(ipcOutputFile, YES, YES, gcFont, 11,"") .
    RUN pProcessSections.
    RUN CloseOutput.
    RUN PrintXprintFile(ipcOutputFile).
END.
THIS-PROCEDURE:REMOVE-SUPER-PROCEDURE (hdOutputProcs).
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a format, processes temp-table to build paging structure
     Notes: Options are Consolidated or by Form (by Item -TBD or by Blank -TBD)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstHeader AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcSectionBy AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormatStyle AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iSectionCount AS INTEGER NO-UNDO.
    

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
    EMPTY TEMP-TABLE ttSection.
    FIND FIRST ttEstHeader NO-LOCK 
        WHERE ROWID(ttEstHeader) EQ ipriEstHeader
        NO-ERROR.
    IF AVAILABLE ttEstHeader THEN 
    DO:
        ASSIGN 
            ttEstHeader.printDateTime = NOW
            ttEstHeader.printedBy     = USERID("asi")
            . 
        IF ipcSectionStyle BEGINS "By Form" THEN 
        DO:
            FOR EACH ttEstForm NO-LOCK 
                WHERE ttEstForm.estCostHeaderID EQ ttEstHeader.estCostHeaderID
                AND ttEstForm.formNo NE 0
                BY ttEstForm.formNo
                :
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.rec_keyParent = ttEstForm.rec_key
                    ttSection.iSequence     = iSectionCount
                    ttSection.cType         = "Form"
                    .
            END. 
        END.
        ELSE IF ipcSectionStyle BEGINS "Consolidated" THEN 
            DO:
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.cType         = "Consolidated"
                    ttSection.iSequence     = iSectionCount
                    ttSection.rec_keyParent = ttEstHeader.rec_key
                    .
            END.
        IF CAN-DO("Set,Combo,Tandem",ttEstHeader.estType) AND INDEX(ipcSectionBy,"with Summary") GT 0 THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Summary"
                ttSection.iSequence     = IF INDEX(ipcSectionBy,"with Summary First") GT 0 THEN 0 ELSE iSectionCount
                ttSection.rec_keyParent = ttEstHeader.rec_key
                .
        END.
    END.
END PROCEDURE.

PROCEDURE pPrintForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes the output for a given form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstFormRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 

    FIND FIRST ttEstForm NO-LOCK 
        WHERE ttEstForm.rec_key EQ ipcEstFormRecKey
        NO-ERROR.
    IF AVAILABLE ttEstForm THEN 
        FIND FIRST ttEstHeader NO-LOCK
            WHERE ttEstHeader.estCostHeaderID EQ ttEstForm.estCostHeaderID
            NO-ERROR.
    IF NOT AVAILABLE ttEstHeader THEN RETURN.

    RUN pPrintPageHeader(BUFFER ttEstHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintItemInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintLayoutInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMaterialInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMiscInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, "Prep", INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintMiscInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, "Misc", INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintOperationsInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pPrintCostSummaryInfoForForm(BUFFER ttEstHeader, BUFFER ttEstForm, glShowAllQuantities, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
END PROCEDURE.

PROCEDURE pPrintItemInfoDetail PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the basic information for a given item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstItem FOR ttEstItem.
    DEFINE PARAMETER BUFFER ipbf-ttEstBlank FOR ttEstBlank.
    DEFINE INPUT PARAMETER iplPrintHeader AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
    
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 43.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 68.
    
    IF iplPrintHeader THEN 
    DO:
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, "Qty / F-B #", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, "Name / Description", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, "Size / Color", NO, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, "Style / Part #", NO, YES, NO).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn1, ipbf-ttEstBlank.qtyRequired, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-ttEstItem.itemName , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-ttEstItem.sizeDesc , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-ttEstItem.styleDesc, 16, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, fFormatNumber(ipbf-ttEstBlank.formNo,2, 0, YES) + "-" + fFormatNumber(ipbf-ttEstBlank.blankNo,2, 0, YES), NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ipbf-ttEstItem.itemDescription1, 20 , NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ipbf-ttEstItem.colorDesc, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ipbf-ttEstItem.customerPart, 16, NO, NO, NO).
    
END PROCEDURE.

PROCEDURE pPrintItemInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iItemColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2 AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3 AS INTEGER INITIAL 43.
    DEFINE VARIABLE iItemColumn4 AS INTEGER INITIAL 68.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostFormID EQ ipbf-ttEstForm.estCostFormID,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID
        BREAK BY ttEstBlank.blankNo:
        IF FIRST(ttEstBlank.blankNo) THEN 
        DO:
            RUN pPrintItemInfoHeader(BUFFER ttEstItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pPrintItemInfoDetail(BUFFER ttEstItem, BUFFER ttEstBlank, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
        ELSE 
            RUN pPrintItemInfoDetail(BUFFER ttEstItem, BUFFER ttEstBlank, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

    END.
END PROCEDURE.

PROCEDURE pPrintItemInfoHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the header/customer information for a given item
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstItem FOR ttEstItem.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.

    DEFINE VARIABLE iShipToColumn   AS INTEGER INITIAL 55.
    DEFINE VARIABLE iCustomerColumn AS INTEGER INITIAL 14.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.customerName, 20, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Ship To:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.shipToName, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.customerAddress1, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.shipToAddress1, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.customerAddress2, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.shipToAddress2, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.customerAddress3, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.shipToAddress3, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer ID:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.customerID, 8, YES, NO,NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 9, ipbf-ttEstItem.shipToID, 8, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Salesperson:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.salesgroupName, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

END PROCEDURE.

PROCEDURE pPrintPageHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.

    DEFINE VARIABLE iDateLabelColumn AS INTEGER INITIAL 47.
    DEFINE VARIABLE iPageLabelColumn AS INTEGER INITIAL 80.
    DEFINE VARIABLE iEstimateColumn  AS INTEGER INITIAL 14.
    
      
    RUN pWriteToCoordinates(iopiRowCount, 2, "Estimate Calculation", YES, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Page: " + STRING(iopiPageCount,">9"), NO, NO, YES).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Estimate #: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.estNo, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Printed: " + STRING(ipbf-ttEstHeader.printDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.printedBy , NO, NO, YES).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Type: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.estType, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Calculated: " + STRING(ipbf-ttEstHeader.calcDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.calculatedBy , NO, NO, YES).

    
END PROCEDURE.


PROCEDURE pPrintCostSummaryInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Cost Summary with either Each Qty showing or a Per M plus Total
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT PARAMETER iplPerQuantity AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE BUFFER bf-PrimaryttEstHeader FOR ttEstHeader.
    DEFINE BUFFER bf-ttEstForm FOR ttEstForm.

    DEFINE VARIABLE iRowStart      AS INTEGER.
    DEFINE VARIABLE iColumn1       AS INTEGER   INITIAL 2.
    DEFINE VARIABLE iColumn2       AS INTEGER   INITIAL 36.
    DEFINE VARIABLE iColumnWidth   AS INTEGER   INITIAL 10.
    
    DEFINE VARIABLE iQtyCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQtyCountTotal AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopeRecKey   AS CHARACTER EXTENT 10.
    DEFINE VARIABLE cQtyHeader     AS CHARACTER EXTENT 10.
    DEFINE VARIABLE dCostPerM      AS DECIMAL   EXTENT 10.
    DEFINE VARIABLE dCostTotalPerM AS DECIMAL.
    DEFINE VARIABLE dCostTotal     AS DECIMAL.
    DEFINE VARIABLE lLineStarted   AS LOGICAL   NO-UNDO.

    FIND FIRST bf-PrimaryttEstHeader NO-LOCK 
        WHERE bf-PrimaryttEstHeader.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryttEstHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    iRowStart = iopiRowCount. /*Store reset Point*/
        
    ASSIGN 
        iQtyCountTotal               = 1
        cScopeRecKey[iQtyCountTotal] = ipbf-ttEstForm.rec_key
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-ttEstForm.qtyFGOnForm, 7, 0, YES)
        .
    IF iplPerQuantity THEN 
    DO:
        FOR EACH ttEstHeader NO-LOCK
            WHERE ttEstHeader.estNo EQ bf-PrimaryttEstHeader.estNo
            AND ttEstHeader.estCostHeaderID NE bf-PrimaryttEstHeader.estCostHeaderID
            ,
            FIRST ttEstForm NO-LOCK 
            WHERE ttEstForm.estCostHeaderID EQ ttEstHeader.estCostHeaderID
            AND ttEstForm.formNo EQ ipbf-ttEstForm.formNo
            :
            ASSIGN 
                iQtyCountTotal               = iQtyCountTotal + 1
                cScopeRecKey[iQtyCountTotal] = ttEstForm.rec_key
                cQtyHeader[iQtyCountTotal]   = fFormatNumber(ttEstForm.qtyFGOnForm, 7, 0, YES)
                .
            IF iQtyCountTotal EQ giQtyMaxColumn THEN LEAVE. 
        END.
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals Per M ", YES, YES, NO).
        DO iQtyCount = 1 TO iQtyCountTotal:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth, cQtyHeader[iQtyCount], 7, 0, YES, YES, YES, YES, YES).
            IF iQtyCount EQ 1 THEN 
                RUN pWriteToCoordinates(iopiRowCount, iColumn2, gcQtyMasterInd, YES, NO, NO).
        END.
    END.
    ELSE 
    DO: 
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals for Qty: " +  fFormatNumber(ipbf-ttEstForm.qtyFGOnForm, 7, 0, YES), YES, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 , "Per M" , YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 + iColumnWidth, "Total", YES, YES, YES).
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
                    FIND FIRST ttEstCostSummary NO-LOCK 
                        WHERE ttEstCostSummary.estCostGroupID EQ estCostGroup.estCostGroupID  
                        AND ttEstCostSummary.scopeRecKey EQ cScopeRecKey[iQtyCount]
                        NO-ERROR.
                    IF AVAILABLE ttEstCostSummary THEN DO:
                        IF ttEstCostSummary.costTotal NE 0 THEN DO:
                            IF NOT lLineStarted THEN DO: 
                                lLineStarted = YES.
                                RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                                RUN pWriteToCoordinates(iopiRowCount, iColumn1, estCostGroup.costGroupLabel, NO, NO, NO).
                            END.                        
                            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth ,ttEstCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).
                            dCostPerM[iQtyCount] = dCostPerM[iQtyCount] + ttEstCostSummary.costTotalPerMFinished.
                        END.
                    END.
                END.
            END.
            ELSE 
            DO:  /*Print only the values for the subject quantity (per M and Totals)*/ 
                FIND FIRST ttEstCostSummary NO-LOCK 
                    WHERE ttEstCostSummary.estCostGroupID EQ estCostGroup.estCostGroupID
                    AND ttEstCostSummary.scopeRecKey EQ ipbf-ttEstForm.rec_key
                    NO-ERROR.
                IF AVAILABLE ttEstCostSummary THEN 
                DO:
                    IF ttEstCostSummary.costTotal NE 0 THEN DO:            
                        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
                        RUN pWriteToCoordinates(iopiRowCount, iColumn1, estCostGroup.costGroupLabel, NO, NO, NO).
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 , ttEstCostSummary.costTotalPerMFinished , 6, 2, NO, YES, NO, NO, YES).
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + iColumnWidth, ttEstCostSummary.costTotal , 6, 2, NO, YES, NO, NO, YES).
                        ASSIGN 
                            dCostTotal = dCostTotal + ttEstCostSummary.costTotal
                            dCostTotalPerM = dCostTotalPerM + ttEstCostSummary.costTotalPerMFinished
                            .
                    END.
                END.
            END.
        END.
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, estCostGroupLevel.estCostGroupLevelDesc, YES, NO, NO).    
        IF iplPerQuantity THEN
        DO: /*Print values for each quantity (per M)*/
            DO iQtyCount = 1 TO iQtyCountTotal:
                RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth , dCostPerM[iQtyCount] , 6, 2, NO, YES, YES, NO, YES).
            END.
        END.
        ELSE DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 , dCostTotalPerM , 6, 2, NO, YES, YES, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + iColumnWidth, dCostTotal , 6, 2, NO, YES, YES, NO, YES).
        END.
    END.
            
            
END PROCEDURE.


PROCEDURE pPrintLayoutInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1 AS INTEGER INITIAL 12.
    DEFINE VARIABLE iColumn2 AS INTEGER INITIAL 22.
    DEFINE VARIABLE iColumn3 AS INTEGER INITIAL 32.
    DEFINE VARIABLE iColumn4 AS INTEGER INITIAL 45.
    DEFINE VARIABLE iColumn5 AS INTEGER INITIAL 58.
    DEFINE VARIABLE iColumn6 AS INTEGER INITIAL 72.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn2, "Width", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3, "Length", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Area", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn5, "#Up/Out", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Weight", NO, YES, YES).
     
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostFormID EQ ipbf-ttEstForm.estCostFormID:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Blank #" + TRIM(STRING(ttEstBlank.blankNo,">>9")) + ":", NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstBlank.blankWidth, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstBlank.blankLength, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstBlank.dimUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstBlank.blankArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstBlank.areaUOM , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstBlank.numOut, 4, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstBlank.weight, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstBlank.weightUOM, NO, NO, NO).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Die:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dieWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dieLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dieArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.weightDie, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.weightDieUOM, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Net:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.netWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.netLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.netArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstForm.numOutNet, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.weightNet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.weightNetUOM, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Gross:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.grossWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.grossLength, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.dimUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.grossArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.areaUOM , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.weightGross, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.weightGrossUOM, NO, NO, NO).
    IF ttEstForm.rollWidth NE 0 THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2, TRIM(STRING(ttEstForm.rollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Totals->", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn2,  "Sheets:", YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + 1, ttEstForm.grossQtyRequiredTotal, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.grossQtyRequiredTotalArea, 4, 5, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.grossQtyRequiredTotalAreaUOM , YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.grossQtyRequiredTotalWeight, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.grossQtyRequiredTotalWeightUOM, NO, NO, NO).
END PROCEDURE.

PROCEDURE pPrintMaterialInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1   AS INTEGER INITIAL 5.
    DEFINE VARIABLE iColumn2   AS INTEGER INITIAL 20.
    DEFINE VARIABLE iColumn3   AS INTEGER INITIAL 36.
    DEFINE VARIABLE iColumn4   AS INTEGER INITIAL 48.
    DEFINE VARIABLE iColumn5   AS INTEGER INITIAL 60.
    DEFINE VARIABLE iColumn6   AS INTEGER INITIAL 70.
    DEFINE VARIABLE iColumn7   AS INTEGER INITIAL 82.
    
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal     AS DECIMAL NO-UNDO.
       
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Materials", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3, "Qty Req", NO, YES, YES).   
    RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Cost Per", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn5, "SU $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Cost/M", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn7, "Total Cost", NO, YES, YES).
    
    ASSIGN 
        dTotalPerM = 0
        dTotal = 0
        . 
    FOR EACH ttEstMaterial NO-LOCK 
        WHERE ttEstMaterial.estCostHeaderID EQ ipbf-ttEstForm.estCostHeaderID 
        AND ttEstMaterial.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        BY ttEstMaterial.formNo
        BY ttEstMaterial.blankNo
        BY ttEstMaterial.sequenceOfMaterial:

        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstMaterial.formNo,2, 0, YES) + "-" + fFormatNumber(ttEstMaterial.blankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstMaterial.itemName, 30, NO, NO, NO).
        IF ttEstMaterial.isPrimarySubstrate THEN 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.qtyRequiredNoWasteInCostUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, 0, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.costTotalPerMFinishedNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.costTotalNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  SU Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.qtyRequiredSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.qtyUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.costSetup, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.costTotalPerMFinishedSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.costTotalSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  Run Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.qtyRequiredRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.qtyUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, 0, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.costTotalPerMFinishedRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.costTotalRunWaste, 7, 2, NO, YES, NO, NO, YES).
            ASSIGN 
                dTotalPerM = dTotalPerM + ttEstMaterial.costTotalPerMFinishedNoWaste
                dTotal = dTotal + ttEstMaterial.costTotalNoWaste
                dTotalPerM = dTotalPerM + ttEstMaterial.costTotalPerMFinishedSetupWaste
                dTotal = dTotal + ttEstMaterial.costTotalNoWaste
                dTotalPerM = dTotalPerM + ttEstMaterial.costTotalPerMFinishedRunWaste
                dTotal = dTotal + ttEstMaterial.costTotalNoWaste
                .
        END.
        ELSE 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.qtyRequiredTotal, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.qtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.costUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.costSetup, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.costTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.costTotal, 7, 2, NO, YES, NO, NO, YES).
            ASSIGN 
                dTotalPerM = dTotalPerM + ttEstMaterial.costTotalPerMFinished
                dTotal = dTotal + ttEstMaterial.costTotal
                .
        END.
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total Materials", YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.

PROCEDURE pPrintMiscInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER. 
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1   AS INTEGER INITIAL 5.
    DEFINE VARIABLE iColumn2   AS INTEGER INITIAL 25.
    DEFINE VARIABLE iColumn3   AS INTEGER INITIAL 40.
    DEFINE VARIABLE iColumn4   AS INTEGER INITIAL 50.
    DEFINE VARIABLE iColumn5   AS INTEGER INITIAL 58.
    DEFINE VARIABLE iColumn6   AS INTEGER INITIAL 70.
    DEFINE VARIABLE iColumn7   AS INTEGER INITIAL 82.
    
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal     AS DECIMAL NO-UNDO.
       
    ASSIGN 
        dTotalPerM = 0
        dTotal = 0
        . 
    FOR EACH ttEstMisc NO-LOCK 
        WHERE ttEstMisc.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        AND ((ipcType EQ "Misc" AND NOT ttEstMisc.isPrep) OR (ipcType EQ "Prep" AND ttEstMisc.isPrep))
        AND LOOKUP(ttEstMisc.SIMON, "I,M") GT 0
        BREAK BY ttEstMisc.formNo
        BY ttEstMisc.blankNo:
        IF FIRST-OF(ttEstMisc.formNo) THEN 
        DO:
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, ipcType + " Description", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn2, "Type", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn3, "SU Cost", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Cost Per", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn5, ttEstMisc.profitPercentType, NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Cost/M", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn7, "Total Cost", NO, YES, YES).
        END.    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
       
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstMisc.formNo,2, 0, YES) + "-" + fFormatNumber(ttEstMisc.blankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstMisc.costDescription, 20, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn2, ttEstMisc.costType, 4, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMisc.costSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMisc.costPerUOM, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn4, ttEstMisc.costUOM, 3, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMisc.profitPercent, 3, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn5 + 1, ttEstMisc.SIMON, 1, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMisc.costTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMisc.costTotal, 7, 2, NO, YES, NO, NO, YES).
        ASSIGN 
            dTotalPerM = dTotalPerM + ttEstMisc.costTotalPerMFinished
            dTotal = dTotal + ttEstMisc.costTotal
            .    
    END.
    IF dTotal NE 0 THEN 
    DO:        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total " + ipcType, YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotal, 7, 2, NO, YES, YES, NO, YES).
    END.
    
END PROCEDURE.

PROCEDURE pPrintOperationsInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstHeader FOR ttEstHeader.
    DEFINE PARAMETER BUFFER ipbf-ttEstForm   FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1   AS INTEGER INITIAL 5.
    DEFINE VARIABLE iColumn2   AS INTEGER INITIAL 30.
    DEFINE VARIABLE iColumn3   AS INTEGER INITIAL 38.
    DEFINE VARIABLE iColumn4   AS INTEGER INITIAL 46.
    DEFINE VARIABLE iColumn5   AS INTEGER INITIAL 54.
    DEFINE VARIABLE iColumn6   AS INTEGER INITIAL 62.
    DEFINE VARIABLE iColumn7   AS INTEGER INITIAL 70.
    DEFINE VARIABLE iColumn8   AS INTEGER INITIAL 82.
    
    DEFINE VARIABLE dTotalSetup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalRun AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal     AS DECIMAL NO-UNDO.
           
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Machine", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn2, "SU Hrs", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3, "Run Hrs", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Speed", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn5, "Rate", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6, "SU $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn7, "Run $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn8, "Total Cost", NO, YES, YES).
     
    FOR EACH ttEstOperation NO-LOCK 
        WHERE ttEstOperation.estCostFormID EQ ipbf-ttEstForm.estCostFormID
        BY ttEstOperation.sequenceOfOperation: 
   
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstOperation.formNo,2, 0, YES) + "-" + fFormatNumber(ttEstOperation.blankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstOperation.operationName, 20, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstOperation.hoursSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstOperation.hoursRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstOperation.speed, 7, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstOperation.costPerHourTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstOperation.costTotalSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstOperation.costTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn8, ttEstOperation.costTotal, 7, 2, NO, YES, NO, NO, YES).
        ASSIGN 
            dTotalSetup = dTotalSetup + ttEstOperation.costTotalSetup
            dTotalRun = dTotalRun + ttEstOperation.costTotalRun
            dTotal = dTotal + ttEstOperation.costTotal
            .
        
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total Operations", YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalSetup, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotalRun, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn8, dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.


PROCEDURE pPrintSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Set Summary seciton for the set header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderRecKey AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    

    FIND FIRST ttEstHeader NO-LOCK 
        WHERE ttEstHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.
    IF NOT AVAILABLE ttEstHeader THEN RETURN.
    
    RUN pPrintPageHeader(BUFFER ttEstHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estCostHeaderID EQ ttEstHeader.estCostHeaderID
        AND ttEstBlank.blankNo EQ 0,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID:
        
        RUN pPrintItemInfoHeader(BUFFER ttEstItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pPrintItemInfoDetail(BUFFER ttEstItem, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    END.
    
    DEFINE BUFFER bf-PrimaryttEstHeader FOR ttEstHeader.
    DEFINE VARIABLE iRowStart      AS INTEGER.
    DEFINE VARIABLE iColumn1       AS INTEGER   INITIAL 2.
    DEFINE VARIABLE iColumn2       AS INTEGER   INITIAL 36.
    DEFINE VARIABLE iColumnWidth   AS INTEGER   INITIAL 10.
    
    DEFINE VARIABLE iQtyCount      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iQtyCountTotal AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cScopeRecKey   AS CHARACTER EXTENT 10.
    DEFINE VARIABLE cQtyHeader     AS CHARACTER EXTENT 10.
    DEFINE VARIABLE dCostValue     AS DECIMAL   EXTENT 10.
       
    FIND FIRST bf-PrimaryttEstHeader NO-LOCK 
        WHERE bf-PrimaryttEstHeader.rec_key EQ ipcEstHeaderRecKey
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryttEstHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    iRowStart = iopiRowCount /*Store reset Point*/
        .
        
    ASSIGN 
        iQtyCountTotal               = 1
        cScopeRecKey[iQtyCountTotal] = bf-PrimaryttEstHeader.rec_key
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(bf-PrimaryttEstHeader.qtyMaster, 7, 0, YES)
        .
    FOR EACH ttEstHeader NO-LOCK
        WHERE ttEstHeader.estNo EQ bf-PrimaryttEstHeader.estNo
        AND ttEstHeader.estCostHeaderID NE bf-PrimaryttEstHeader.estCostHeaderID
        :
        ASSIGN 
            iQtyCountTotal               = iQtyCountTotal + 1
            cScopeRecKey[iQtyCountTotal] = ttEstHeader.rec_key
            cQtyHeader[iQtyCountTotal]   = fFormatNumber(ttEstHeader.qtyMaster, 7, 0, YES)
            .
        IF iQtyCountTotal EQ giQtyMaxColumn THEN LEAVE. 
    END.
    
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals Per M ", YES, YES, NO).
    DO iQtyCount = 1 TO iQtyCountTotal:
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth, cQtyHeader[iQtyCount], 7, 0, YES, YES, YES, YES, YES).
        IF iQtyCount EQ 1 THEN 
            RUN pWriteToCoordinates(iopiRowCount, iColumn2, gcQtyMasterInd, YES, NO, NO).
    END.
      
    FOR EACH ttEstBlank NO-LOCK
        WHERE ttEstBlank.estCostHeaderID EQ bf-PrimaryttEstHeader.estCostHeaderID
        AND ttEstBlank.blankNo NE 0,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estCostItemID EQ ttEstBlank.estCostItemID:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1, ttEstItem.itemName, 20, NO, NO, NO).   
            
        DO iQtyCount = 1 TO iQtyCountTotal:
            /*Get the correct summary - Total Price Per Item*/ 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth , 0 , 6, 2, NO, YES, NO, NO, YES).
        END.
            
    END.
END PROCEDURE.

PROCEDURE pProcessSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes each section
     Notes:
    ------------------------------------------------------------------------------*/
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
            RUN pPrintSummary(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).                       
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

    RUN WriteOutput(ipcText, iplUnformatted, iplSkip).
    
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
    
    RUN WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, iplRightJustified).

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
    RUN WriteToXprint(ipdR, ipdC, cText, iplBold, iplUnderline, iplRightJustified).

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
    RUN WriteToXprint(ipdR, ipdC, ipcText, iplBold, iplUnderline, iplRightJustified).

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose: Formats a number with left and right digits.  Handles problem when 
     size of number doesn't fit
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatNumber", ipdNumber, ipiLeftDigits, ipiRightDigits, iplComma).
		
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
