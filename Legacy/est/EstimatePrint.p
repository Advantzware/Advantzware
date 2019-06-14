
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
    glShowAllQuantities = YES
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
            ttEstHeader.dtPrintDateTime = NOW
            ttEstHeader.cPrinter        = USERID("asi")
            . 
        IF ipcSectionStyle BEGINS "By Form" THEN 
        DO:
            FOR EACH ttEstForm NO-LOCK 
                WHERE ttEstForm.estHeaderID EQ ttEstHeader.estHeaderID
                AND ttEstForm.iFormNo NE 0
                BY ttEstForm.iFormNo
                :
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.rec_keyParent = ttEstForm.estFormID
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
                    ttSection.rec_keyParent = ttEstHeader.estHeaderID
                    .
            END.
        IF ttEstHeader.cEstType EQ "Set" AND INDEX(ipcSectionBy,"with Set Summary") GT 0 THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "SetSummary"
                ttSection.iSequence     = IF INDEX(ipcSectionBy,"with Set Summary First") GT 0 THEN 0 ELSE iSectionCount
                ttSection.rec_keyParent = ttEstHeader.estHeaderID
                .
        END.
    END.
END PROCEDURE.

PROCEDURE pGetSubTotalForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form buffer and total line item, return quantity for requested line for that form
     both per M and Total
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER ipcLineItem AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQtyFinished AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotal AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opdTotalPerM AS DECIMAL NO-UNDO.


END PROCEDURE.

PROCEDURE pGetSubTotalItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Given a form buffer and total line item, return quantity for requested line for that form
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pPrintForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes the output for a given form
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstFormID AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO. 

    FIND FIRST ttEstForm NO-LOCK 
        WHERE ttEstForm.estFormID EQ ipcEstFormID
        NO-ERROR.
    IF AVAILABLE ttEstForm THEN 
        FIND FIRST ttEstHeader NO-LOCK
            WHERE ttEstHeader.estHeaderID EQ ttEstForm.estHeaderID
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
    RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn1, ttEstItem.dQtyRequired, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ttEstItem.cItemName , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ttEstItem.cSize , 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ttEstItem.cStyle, 16, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, fFormatNumber(ttEstBlank.iFormNo,2, 0, YES) + "-" + fFormatNumber(ttEstBlank.iBlankNo,2, 0, YES), NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ttEstItem.cItemDescription1, 20 , NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn3, ttEstItem.cColor, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ttEstItem.cCustomerPart, 16, NO, NO, NO).
    
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
        WHERE ttEstBlank.estFormID EQ ipbf-ttEstForm.estFormID,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estItemID EQ ttEstBlank.estItemID
        BREAK BY ttEstBlank.iBlankNo:
        IF FIRST(ttEstBlank.iBlankNo) THEN 
        DO:
            RUN pPrintItemInfoHeader(BUFFER ttEstItem, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pPrintItemInfoDetail(BUFFER ttEstITem, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        END.
        ELSE 
            RUN pPrintItemInfoDetail(BUFFER ttEstITem, NO, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).

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
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.cCustomerName, 20, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Ship To:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.cShipToName, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.cCustomerAddress1, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.cShipToAddress1, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.cCustomerAddress2, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.cShipToAddress2, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.cCustomerAddress3, 20, NO, NO, NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.cShipToAddress3, 20, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer ID:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ipbf-ttEstItem.cCustomerID, 8, YES, NO,NO).
    RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 9, ipbf-ttEstItem.cShipToID, 8, NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Salesperson:", NO, NO, YES).
    RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ipbf-ttEstItem.cSalesgroupName, 20, NO, NO, NO).
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
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.cEstNo, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Printed: " + STRING(ipbf-ttEstHeader.dtPrintDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.cPrinter , NO, NO, YES).
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Type: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.cEstType, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Calculated: " + STRING(ipbf-ttEstHeader.dtCalcDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.cCalculator , NO, NO, YES).

    
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
        WHERE bf-PrimaryttEstHeader.estHeaderID EQ ipbf-ttEstForm.estHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryttEstHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    iRowStart = iopiRowCount /*Store reset Point*/
        .
        
    ASSIGN 
        iQtyCountTotal               = 1
        cScopeRecKey[iQtyCountTotal] = ipbf-ttEstForm.estFormID
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(ipbf-ttEstForm.dQtyFGOnForm, 7, 0, YES)
        .
    IF iplPerQuantity THEN 
    DO:
        FOR EACH ttEstHeader NO-LOCK
            WHERE ttEstHeader.cEstNo EQ bf-PrimaryttEstHeader.cEstNo
            AND ttEstHeader.estHeaderID NE bf-PrimaryttEstHeader.estHeaderID
            ,
            FIRST ttEstForm NO-LOCK 
            WHERE ttEstForm.estHeaderID EQ ttEstHeader.estHeaderID
            AND ttEstForm.iFormNo EQ ipbf-ttEstForm.iFormNo
            :
            ASSIGN 
                iQtyCountTotal               = iQtyCountTotal + 1
                cScopeRecKey[iQtyCountTotal] = ttEstForm.estFormID
                cQtyHeader[iQtyCountTotal]   = fFormatNumber(ttEstForm.dQtyFGOnForm, 7, 0, YES)
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
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals for Qty: " +  fFormatNumber(ipbf-ttEstForm.dQtyFGOnForm, 7, 0, YES), YES, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 , "Per M" , YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 + iColumnWidth, "Total", YES, YES, YES).
    END.    
    
    FOR EACH ttEstCostGroupLevel NO-LOCK
        BY ttEstCostGroupLevel.iCostGroupLevel:
        FOR EACH ttEstCostGroup NO-LOCK 
            WHERE ttEstCostGroup.iCostGroupLevel EQ ttEstCostGroupLevel.iCostGroupLevel
            BY ttEstCostGroup.iSequence:
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1, ttEstCostGroup.cGroupLabel, NO, NO, NO).
            
            IF iplPerQuantity THEN 
            DO: /*Print values for each quantity (per M)*/
                DO iQtyCount = 1 TO iQtyCountTotal:
                    FIND FIRST ttEstCostSummary NO-LOCK 
                        WHERE ttEstCostSummary.estCostGroupID EQ ttEstCostGroup.estCostGroupID
                        AND ttEstCostSummary.cScope EQ cScopeRecKey[iQtyCount]
                        NO-ERROR.
                    IF AVAILABLE ttEstCostSummary THEN     
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth ,ttEstCostSummary.dCostPerM , 6, 2, NO, YES, NO, NO, YES).
                END.
            END.
            ELSE 
            DO:  /*Print only the values for the subject quantity (per M and Totals)*/ 
                FIND FIRST ttEstCostSummary NO-LOCK 
                    WHERE ttEstCostSummary.estCostGroupID EQ ttEstCostGroup.estCostGroupID
                    AND ttEstCostSummary.cScope EQ ipbf-ttEstForm.estFormID
                    NO-ERROR.
                IF AVAILABLE ttEstCostSummary THEN 
                DO:
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 , ttEstCostSummary.dCostPerM , 6, 2, NO, YES, NO, NO, YES).
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + iColumnWidth, ttEstCostSummary.dCostTotal , 6, 2, NO, YES, NO, NO, YES).
                END.
            END.
        END.
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, ttEstCostGroupLevel.cCostGroupLevelDescription, YES, NO, NO).    
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
        WHERE ttEstBlank.estFormID EQ ipbf-ttEstForm.estFormID:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Blank #" + TRIM(STRING(ttEstBlank.iBlankNo,">>9")) + ":", NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstBlank.dBlankWidth, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstBlank.dBlankLength, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstBlank.cUOMDimension , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstBlank.dBlankArea, 4, 5, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstBlank.cUOMArea , NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstBlank.iNumOut, 4, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstBlank.dWeight, 5, 4, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstBlank.cUOMWeight, NO, NO, NO).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Die:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dDieWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dDieLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dDieArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dWeightDie, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMWeightDie, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Net:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dNetWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dNetLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dNetArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstForm.iNumOutNet, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dWeightNet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMWeightNet, NO, NO, NO).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Gross:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dGrossWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dGrossLength, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dGrossArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dWeightGross, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMWeightGross, NO, NO, NO).
    IF ttEstForm.dRollWidth NE 0 THEN 
    DO:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2, TRIM(STRING(ttEstForm.dRollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Totals->", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn2,  "Sheets:", YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + 1, ttEstForm.dGrossQtyRequiredTotal, 9, 0, YES, YES, YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dGrossQtyRequiredTotalArea, 4, 5, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMGrossQtyRequiredTotalArea , YES, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dGrossQtyRequiredTotalWeight, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMGrossQtyRequiredTotalWeight, NO, NO, NO).
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
     
    FOR EACH ttEstMaterial NO-LOCK 
        WHERE ttEstMaterial.estHeaderID EQ ipbf-ttEstForm.estHeaderID 
        AND ttEstMaterial.estFormID EQ ipbf-ttEstForm.estFormID
        BY ttEstMaterial.iFormNo
        BY ttEstMaterial.iBlankNo
        BY ttEstMaterial.iSequence:

        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstMaterial.iFormNo,2, 0, YES) + "-" + fFormatNumber(ttEstMaterial.iBlankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstMaterial.cItemName, 30, NO, NO, NO).
        IF ttEstMaterial.lIsPrimarySubstrate THEN 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.dQtyRequiredNoWasteInCostUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.cCostUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dCostPerUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cCostUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  SU Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.dQtyRequiredSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.cQtyUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.dCostSetup, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalSetupWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  Run Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.dQtyRequiredRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.cQtyUOMWaste, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedRunWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalRunWaste, 7, 2, NO, YES, NO, NO, YES).
        END.
        ELSE 
        DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMaterial.dQtyRequiredTotal, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn3 + 1, ttEstMaterial.cQtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dCostPerUOM, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cCostUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.dCostSetup, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotal, 7, 2, NO, YES, NO, NO, YES).
        END.
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total Materials", YES, NO, NO).
    RUN pGetSubTotalForm(BUFFER ipbf-ttEstForm, "Direct Material", ipbf-ttEstForm.dQtyFGOnForm, OUTPUT dTotalPerM, OUTPUT dTotal).
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
       
     
    FOR EACH ttEstMisc NO-LOCK 
        WHERE ttEstMisc.estFormID EQ ipbf-ttEstForm.estFormID
        AND ((ipcType EQ "Misc" AND NOT ttEstMisc.lIsPrep) OR (ipcType EQ "Prep" AND ttEstMisc.lIsPrep))
        AND LOOKUP(ttEstMisc.cSIMON, "I,M") GT 0
        BREAK BY ttEstMisc.iFormNo
        BY ttEstMisc.iBlankNo:
        IF FIRST-OF(ttEstMisc.iFormNo) THEN 
        DO:
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, ipcType + " Description", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn2, "Type", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iColumn3, "SU Cost", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Cost Per", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn5, ttEstMisc.cPercentType, NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Cost/M", NO, YES, YES).
            RUN pWriteToCoordinates(iopiRowCount, iColumn7, "Total Cost", NO, YES, YES).
        END.    
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
       
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstMisc.iFormNo,2, 0, YES) + "-" + fFormatNumber(ttEstMisc.iBlankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstMisc.cCostDescription, 20, NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn2, ttEstMisc.cCostType, 4, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstMisc.dCostSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMisc.dCostPerUOM, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn4, ttEstMisc.cCostUOM, 3, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMisc.dProfitPercent, 3, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn5 + 1, ttEstMisc.cSIMON, 1, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMisc.dCostTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMisc.dCostTotal, 7, 2, NO, YES, NO, NO, YES).
       
    END.
    RUN pGetSubTotalForm(BUFFER ipbf-ttEstForm, ipcType + " Material", ipbf-ttEstForm.dQtyFGOnForm, OUTPUT dTotalPerM, OUTPUT dTotal).
    IF dTotal NE 0 THEN 
    DO:        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total " + ipcType + " Material", YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotal, 7, 2, NO, YES, YES, NO, YES).
        dTotal = 0.
    END.
    RUN pGetSubTotalForm(BUFFER ipbf-ttEstForm, ipcType + " Labor", ipbf-ttEstForm.dQtyFGOnForm, OUTPUT dTotalPerM, OUTPUT dTotal).
    IF dTotal NE 0 THEN 
    DO:        
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total " + ipcType + " Labor", YES, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotal, 7, 2, NO, YES, YES, NO, YES).
        dTotal = 0.
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
    
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
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
        WHERE ttEstOperation.estFormID EQ ipbf-ttEstForm.estFormID
        BY ttEstOperation.iSequence: 
   
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstOperation.iFormNo,2, 0, YES) + "-" + fFormatNumber(ttEstOperation.iBlankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstOperation.cOperationName, 20, NO, NO, NO).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstOperation.dHoursSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstOperation.dHoursRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstOperation.dSpeed, 7, 0, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstOperation.dCostPerHourTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstOperation.dCostTotalSetup, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstOperation.dCostTotalRun, 7, 2, NO, YES, NO, NO, YES).
        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn8, ttEstOperation.dCostTotal, 7, 2, NO, YES, NO, NO, YES).
        
    END.
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total Operations", YES, NO, NO).
    RUN pGetSubTotalForm(BUFFER ipbf-ttEstForm, "Direct Labor", ipbf-ttEstForm.dQtyFGOnForm, OUTPUT dTotalPerM, OUTPUT dTotal).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn8, dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.


PROCEDURE pPrintSetSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Set Summary seciton for the set header
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstHeaderID AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    

    FIND FIRST ttEstHeader NO-LOCK 
        WHERE ttEstHeader.estHeaderID EQ ipcEstHeaderID
        NO-ERROR.
    IF NOT AVAILABLE ttEstHeader THEN RETURN.
    
    RUN pPrintPageHeader(BUFFER ttEstHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    
    FOR FIRST ttEstBlank NO-LOCK 
        WHERE ttEstBlank.estHeaderID EQ ttEstHeader.estHeaderID
        AND ttEstBlank.iBlankNo EQ 0,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estItemID EQ ttEstBlank.estItemID:
        
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
        WHERE bf-PrimaryttEstHeader.estHeaderID EQ ipcEstHeaderID
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryttEstHeader THEN LEAVE.
    
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
    iRowStart = iopiRowCount /*Store reset Point*/
        .
        
    ASSIGN 
        iQtyCountTotal               = 1
        cScopeRecKey[iQtyCountTotal] = bf-PrimaryttEstHeader.estHeaderID
        cQtyHeader[iQtyCountTotal]   = fFormatNumber(bf-PrimaryttEstHeader.dQtyMaster, 7, 0, YES)
        .
    FOR EACH ttEstHeader NO-LOCK
        WHERE ttEstHeader.cEstNo EQ bf-PrimaryttEstHeader.cEstNo
        AND ttEstHeader.estHeaderID NE bf-PrimaryttEstHeader.estHeaderID
        :
        ASSIGN 
            iQtyCountTotal               = iQtyCountTotal + 1
            cScopeRecKey[iQtyCountTotal] = ttEstHeader.estHeaderID
            cQtyHeader[iQtyCountTotal]   = fFormatNumber(ttEstHeader.dQtyMaster, 7, 0, YES)
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
        WHERE ttEstBlank.estHeaderID EQ bf-PrimaryttEstHeader.estHeaderID
        AND ttEstBlank.iBlankNo NE 0,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.estItemID EQ ttEstBlank.estItemID:
        RUN AddRow(INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iopiRowCount).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1, ttEstItem.cItemName, 20, NO, NO, NO).   
            
        DO iQtyCount = 1 TO iQtyCountTotal:
            /*Get the correct summary - Total Price Per Item*/ 
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth ,0 , 6, 2, NO, YES, NO, NO, YES).
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
            WHEN "SetSummary" THEN 
            RUN pPrintSetSummary(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount, INPUT-OUTPUT iRowCount).                       
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
