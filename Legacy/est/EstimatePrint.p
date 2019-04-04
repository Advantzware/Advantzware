
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

{est/ttEstPrint.i "SHARED"}

DEFINE VARIABLE gcFont     AS CHARACTER NO-UNDO.
DEFINE VARIABLE glClassic  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE giQtyMaxColumn AS INTEGER NO-UNDO.
DEFINE VARIABLE gcContinue AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcNumError AS CHARACTER NO-UNDO.

ASSIGN 
    giQtyMaxColumn = 5
    gcNumError = "#"
    gcContinue = CHR(187)
    .


DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iOrder        AS INTEGER
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
RUN pBuildSections(ipriEstHeader, ipcSectionStyle, ipcFormatStyle).
IF CAN-FIND(FIRST ttSection) THEN 
DO: 
    RUN InitializeOutputXprint IN hdOutputProcs (ipcOutputFile, YES, YES, gcFont, 11) .
    RUN pProcessSections.
    RUN CloseOutput IN hdOutputProcs.
    RUN PrintXprintFile IN hdOutputProcs(ipcOutputFile).
END.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddRow PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Increments row based on #, prints a Skip
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiRows AS INTEGER NO-UNDO.

    RUN pWrite("",YES,YES). /*for easier reading of xprint markup*/
    iopiRowCount = iopiRowCount + ipiRows.

END PROCEDURE.

PROCEDURE pBuildSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a format, processes temp-table to build paging structure
     Notes: Options are by Form (by Item or by Blank -TBD)
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
            ttEstHeader.cPrinter = USERID("asi")
            . 
        IF ipcSectionStyle BEGINS "By Form" THEN 
        DO:
            FOR EACH ttEstForm NO-LOCK 
                WHERE ttEstForm.rec_keyHeader EQ ttEstHeader.rec_KeyHeader
                BY ttEstForm.iFormNo
                :
                iSectionCount = iSectionCount + 1.
                CREATE ttSection.
                ASSIGN 
                    ttSection.rec_keyParent = ttEstForm.rec_KeyForm
                    ttSection.iOrder        = iSectionCount
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
                    ttSection.iOrder        = iSectionCount
                    ttSection.rec_keyParent = ttEstHeader.rec_KeyHeader
                    .
            END.
        IF INDEX("with Item Summary", ipcSectionBy) GT 0 THEN 
        DO:
            iSectionCount = iSectionCount + 1.
            CREATE ttSection.
            ASSIGN   
                ttSection.cType         = "Item Summary"
                ttSection.iOrder        = IF INDEX("with Item Summary First", ipcSectionBy) GT 0 THEN 0 ELSE iSectionCount
                ttSection.rec_keyParent = ttEstHeader.rec_KeyHeader
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
    DEFINE INPUT PARAMETER ipcRecKeyForm AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.

    DEFINE VARIABLE iRowCount AS INTEGER. 

    FIND FIRST ttEstForm NO-LOCK 
        WHERE ttEstForm.rec_KeyForm EQ ipcRecKeyForm
        NO-ERROR.
    IF AVAILABLE ttEstForm THEN 
        FIND FIRST ttEstHeader NO-LOCK
            WHERE ttEstHeader.rec_KeyHeader EQ ttEstForm.rec_keyHeader
            NO-ERROR.
    IF NOT AVAILABLE ttEstHeader THEN RETURN.
    iRowCount = 1 .
    RUN pPrintPageHeader(BUFFER ttEstHeader, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
    RUN pPrintItemInfoForForm(BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
    RUN pPrintLayoutInfoForForm(BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
    RUN pPrintMaterialInfoForForm(BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
    RUN pPrintCostSummaryInfoForForm(BUFFER ttEstForm, YES, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
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
    
    iopiPageCount = iopiPageCount + 1.
    IF iopiPageCount GT 1 THEN PAGE.
  
    RUN pWriteToCoordinates(iopiRowCount, 2, "Estimate Calculation", YES, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Page: " + STRING(iopiPageCount,">9"), NO, NO, YES).
    
    iopiRowCount = iopiRowCount + 1.
    
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Estimate #: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.cEstNo, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Printed: " + STRING(ipbf-ttEstHeader.dtPrintDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.cPrinter , NO, NO, YES).
    
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Type: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.cEstType, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, "Calculated: " + STRING(ipbf-ttEstHeader.dtCalcDateTime,"99/99/9999 HH:MM A") + " by " + ipbf-ttEstHeader.cCalculator , NO, NO, YES).

    
END PROCEDURE.

PROCEDURE pPrintItemInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iShipToColumn   AS INTEGER INITIAL 55.
    DEFINE VARIABLE iCustomerColumn AS INTEGER INITIAL 14.
    DEFINE VARIABLE iItemColumn1    AS INTEGER INITIAL 2.
    DEFINE VARIABLE iItemColumn2    AS INTEGER INITIAL 13.
    DEFINE VARIABLE iItemColumn3    AS INTEGER INITIAL 43.
    DEFINE VARIABLE iItemColumn4    AS INTEGER INITIAL 68.
       
    iopiRowCount = iopiRowCount + 1.
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.rec_keyForm EQ ipbf-ttEstForm.rec_KeyForm,
        FIRST ttEstItem NO-LOCK 
        WHERE ttEstItem.rec_keyItem EQ ttEstBlank.rec_keyItem
        BREAK BY ttEstBlank.iBlankNo:
        IF FIRST(ttEstBlank.iBlankNo) THEN 
        DO:
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer:", NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerName, 20, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Ship To:", NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToName, 20, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress1, 20, NO, NO, NO).
            RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress1, 20, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress2, 20, NO, NO, NO).
            RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress2, 20, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress3, 20, NO, NO, NO).
            RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress3, 20, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer ID:", NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerID, 8, YES, NO,NO).
            RUN pWriteToCoordinatesString(iopiRowCount, iCustomerColumn + 9, ttEstItem.cShipToID, 8, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Salesperson:", NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iShipToColumn + 1, ttEstItem.cSalesgroupName, 20, NO, NO, NO).
            iopiRowCount = iopiRowCount + 2.
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, "Qty / B #", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, "Name / Description", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, "Size / Color", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, "Style / Part #", NO, YES, NO).
        END.
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn1, ttEstItem.dQtyRequired, 9, 0, YES, YES, YES, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn2, ttEstItem.cItemName , 20, NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, ttEstItem.cSize , NO, NO, NO).
        RUN pWriteToCoordinatesString(iopiRowCount, iItemColumn4, ttEstItem.cStyle, 20, NO, NO, NO).
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinatesNum(iopiRowCount, iItemColumn1, ttEstBlank.iBlankNo, 3, 0 ,NO, YES, NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, ttEstItem.cItemDescription1 , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, ttEstItem.cColor , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, ttEstItem.cCustomerPart, NO, NO, NO).
    END.
END PROCEDURE.

PROCEDURE pPrintCostSummaryInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the Cost Summary with either Each Qty showing or a Per M plus Total
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT PARAMETER iplPerQuantity AS LOGICAL.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE BUFFER bf-PrimaryttEstHeader FOR ttEstHeader.
    DEFINE VARIABLE iRowStart AS INTEGER.
    DEFINE VARIABLE iColumn1 AS INTEGER INITIAL 2.
    DEFINE VARIABLE iColumn2 AS INTEGER INITIAL 32.
    DEFINE VARIABLE iColumnWidth AS INTEGER INITIAL 10.
    
    DEFINE VARIABLE iQtyCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iQtyCountTotal AS INTEGER NO-UNDO.
    DEFINE VARIABLE cScopeRecKey AS CHARACTER EXTENT 10.
    DEFINE VARIABLE cQtyHeader AS CHARACTER EXTENT 10.
    DEFINE VARIABLE dCostValue AS DECIMAL EXTENT 10.
       
    FIND FIRST bf-PrimaryttEstHeader NO-LOCK 
        WHERE bf-PrimaryttEstHeader.rec_KeyHeader EQ ipbf-ttEstForm.rec_keyHeader
        NO-ERROR.
    IF NOT AVAILABLE bf-PrimaryttEstHeader THEN LEAVE.
    
    ASSIGN 
        iopiRowCount = iopiRowCount + 2
        iRowStart = iopiRowCount /*Store reset Point*/
        .
        
    ASSIGN 
        iQtyCountTotal = 1
        cScopeRecKey[iQtyCountTotal] = ipbf-ttEstForm.rec_KeyForm
        cQtyHeader[iQtyCountTotal] = fFormatNumber(ipbf-ttEstForm.dQtyFGOnForm, 7, 0, YES)
        .
    IF iplPerQuantity THEN DO:
        FOR EACH ttEstHeader NO-LOCK
            WHERE ttEstHeader.cEstNo EQ bf-PrimaryttEstHeader.cEstNo
            AND ttEstHeader.rec_KeyHeader NE bf-PrimaryttEstHeader.rec_KeyHeader
            ,
            FIRST ttEstForm NO-LOCK 
            WHERE ttEstForm.rec_keyHeader EQ ttEstHeader.rec_KeyHeader
            AND ttEstForm.iFormNo EQ ipbf-ttEstForm.iFormNo
            :
            ASSIGN 
                iQtyCountTotal = iQtyCountTotal + 1
                cScopeRecKey[iQtyCountTotal] = ttEstForm.rec_KeyForm
                cQtyHeader[iQtyCountTotal] = fFormatNumber(ttEstForm.dQtyFGOnForm, 7, 0, YES)
                .
            IF iQtyCountTotal EQ giQtyMaxColumn THEN LEAVE. 
        END.
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals Per M ", YES, YES, NO).
        DO iQtyCount = 1 TO iQtyCountTotal:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth, cQtyHeader[iQtyCount], 7, 0, YES, YES, YES, YES, YES).
        END.
    END.
    ELSE DO:
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "*** Totals for Qty: " +  fFormatNumber(ipbf-ttEstForm.dQtyFGOnForm, 7, 0, YES), YES, YES, NO).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 , "Per M" , YES, YES, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2 + iColumnWidth, "Total", YES, YES, YES).
    END.    
    FOR EACH ttEstCostGroupLevel NO-LOCK
        BY ttEstCostGroupLevel.iCostGroupLevel:
        FOR EACH ttEstCostGroup NO-LOCK 
            WHERE ttEstCostGroup.iCostGroupLevel EQ ttEstCostGroupLevel.iCostGroupLevel
            BY ttEstCostGroup.iSequence:
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iColumn1, ttEstCostGroup.cGroupLabel, NO, NO, NO).
            
            IF iplPerQuantity THEN DO: /*Print values for each quantity (per M)*/
                DO iQtyCount = 1 TO iQtyCountTotal:
                    FIND FIRST ttEstCostSummary NO-LOCK 
                        WHERE ttEstCostSummary.rec_keyCostGroup EQ ttEstCostGroup.rec_keyCostGroup
                        AND ttEstCostSummary.rec_keyScope EQ cScopeRecKey[iQtyCount]
                        NO-ERROR.
                    IF AVAILABLE ttEstCostSummary THEN     
                        RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + (iQtyCount - 1) * iColumnWidth ,ttEstCostSummary.dCostPerM , 6, 2, NO, YES, NO, NO, YES).
                END.
            END.
            ELSE DO:  /*Print only the values for the subject quantity (per M and Totals)*/ 
                FIND FIRST ttEstCostSummary NO-LOCK 
                    WHERE ttEstCostSummary.rec_keyCostGroup EQ ttEstCostGroup.rec_keyCostGroup
                    AND ttEstCostSummary.rec_keyScope EQ ipbf-ttEstForm.rec_KeyForm
                    NO-ERROR.
                IF AVAILABLE ttEstCostSummary THEN 
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 , ttEstCostSummary.dCostPerM , 6, 2, NO, YES, NO, NO, YES).
                    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2 + iColumnWidth, ttEstCostSummary.dCostTotal , 6, 2, NO, YES, NO, NO, YES).
            END.
        END.
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, ttEstCostGroupLevel.cCostGroupLevelDescription, YES, NO, NO).    
    END.
            
END PROCEDURE.


PROCEDURE pPrintLayoutInfoForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1 AS INTEGER INITIAL 12.
    DEFINE VARIABLE iColumn2 AS INTEGER INITIAL 22.
    DEFINE VARIABLE iColumn3 AS INTEGER INITIAL 32.
    DEFINE VARIABLE iColumn4 AS INTEGER INITIAL 45.
    DEFINE VARIABLE iColumn5 AS INTEGER INITIAL 58.
    DEFINE VARIABLE iColumn6 AS INTEGER INITIAL 72.
       
    iopiRowCount = iopiRowCount + 2.
    RUN pWriteToCoordinates(iopiRowCount, iColumn2, "Width", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3, "Length", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Area", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn5, "#Up/Out", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Weight", NO, YES, YES).
     
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.rec_keyForm EQ ipbf-ttEstForm.rec_KeyForm:
        iopiRowCount = iopiRowCount + 1.
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
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Die:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dDieWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dDieLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dDieArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dWeightDie, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMWeightDie, NO, NO, NO).
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Net:", NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn2, ttEstForm.dNetWidth, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn3, ttEstForm.dNetLength,4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstForm.dNetArea, 4, 5, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstForm.iNumOut, 4, 0, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstForm.dWeightNet, 5, 4, NO, YES, NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6 + 1, ttEstForm.cUOMWeightNet, NO, NO, NO).
    iopiRowCount = iopiRowCount + 1.
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
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iColumn2, TRIM(STRING(ttEstForm.dRollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    iopiRowCount = iopiRowCount + 1.
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
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iColumn1 AS INTEGER INITIAL 5.
    DEFINE VARIABLE iColumn2 AS INTEGER INITIAL 12.
    DEFINE VARIABLE iColumn3 AS INTEGER INITIAL 32.
    DEFINE VARIABLE iColumn4 AS INTEGER INITIAL 45.
    DEFINE VARIABLE iColumn5 AS INTEGER INITIAL 58.
    DEFINE VARIABLE iColumn6 AS INTEGER INITIAL 70.
    DEFINE VARIABLE iColumn7 AS INTEGER INITIAL 82.
    
    DEFINE VARIABLE dTotalPerM AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotal AS DECIMAL NO-UNDO.
    
       
    iopiRowCount = iopiRowCount + 2.
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Materials", NO, YES, NO).
    RUN pWriteToCoordinates(iopiRowCount, iColumn4, "Qty Required", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn5, "MR $", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn6, "Cost/M", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iColumn7, "Total Cost", NO, YES, YES).
     
    FOR EACH ttEstMaterial NO-LOCK 
        WHERE ttEstMaterial.rec_keyForm EQ ipbf-ttEstForm.rec_KeyForm, 
            FIRST ttEstBlank NO-LOCK 
                WHERE ttEstBlank.rec_keyBlank EQ ttEstMaterial.rec_keyBlank:
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iColumn1, fFormatNumber(ttEstForm.iFormNo,2, 0, YES) + "-" + fFormatNumber(ttEstBlank.iBlankNo,2, 0, YES), NO, NO, YES).
        RUN pWriteToCoordinatesString(iopiRowCount, iColumn1 + 1, ttEstMaterial.cItemName, 20, NO, NO, NO).
        IF ttEstMaterial.lIsPrimarySubstrate THEN DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dQtyRequiredNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cQtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedNoWaste, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalNoWaste, 7, 2, NO, YES, NO, NO, YES).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  MR Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dQtyRequiredWasteMR, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cQtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.dCostMR, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedWasteMR, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalWasteMR, 7, 2, NO, YES, NO, NO, YES).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "  Run Waste",NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dQtyRequiredWasteRun, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cQtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinishedWasteRun, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotalWasteRun, 7, 2, NO, YES, NO, NO, YES).
        END.
        ELSE DO:
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn4, ttEstMaterial.dQtyRequiredWasteMR, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesString(iopiRowCount, iColumn4 + 1, ttEstMaterial.cQtyUOM, 4, NO, NO, NO).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn5, ttEstMaterial.dCostMR, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, ttEstMaterial.dCostTotalPerMFinished, 7, 2, NO, YES, NO, NO, YES).
            RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, ttEstMaterial.dCostTotal, 7, 2, NO, YES, NO, NO, YES).
        END.
    END.
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iColumn1 + 1, "Total Materials", YES, NO, NO).
    RUN pGetSubTotalForm(BUFFER ipbf-ttEstForm, "Direct Material", ipbf-ttEstForm.dQtyFGOnForm, OUTPUT dTotalPerM, OUTPUT dTotal).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn6, dTotalPerM, 7, 2, NO, YES, YES, NO, YES).
    RUN pWriteToCoordinatesNum(iopiRowCount, iColumn7, dTotal, 7, 2, NO, YES, YES, NO, YES).

END PROCEDURE.


PROCEDURE pProcessSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Processes each section
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iPageCount AS INTEGER NO-UNDO.
    

    FOR EACH ttSection NO-LOCK
        BY ttSection.iOrder:
        CASE ttSection.cType:
            WHEN "Form" THEN 
            RUN pPrintForm(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount).
            WHEN "Consolidated" THEN 
            RUN pPrintConsolidated(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount).
            WHEN "ItemSummary" THEN 
            RUN pPrintItemSummary(ttSection.rec_keyParent, INPUT-OUTPUT iPageCount).                       
        END CASE.
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

    RUN WriteOutput IN hdOutputProcs (ipcText, iplUnformatted, iplSkip).
    
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
    
    RUN WriteToXprint IN hdOutputProcs (ipdR, ipdC, ipcText, iplBold, iplUnderline, iplRightJustified).

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
    RUN WriteToXprint IN hdOutputProcs (ipdR, ipdC, cText, iplBold, iplUnderline, iplRightJustified).

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
    RUN WriteToXprint IN hdOutputProcs (ipdR, ipdC, ipcText, iplBold, iplUnderline, iplRightJustified).

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fFormatNumber RETURNS CHARACTER PRIVATE
    ( ipdNumber AS DECIMAL , ipiLeftDigits AS INTEGER , ipiRightDigits AS INTEGER, iplComma AS LOGICAL):
    /*------------------------------------------------------------------------------
     Purpose: Formats a number with left and right digits.  Handles problem when 
     size of number doesn't fit
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatNumber" IN hdOutputProcs, ipdNumber, ipiLeftDigits, ipiRightDigits, iplComma).
		
END FUNCTION.

FUNCTION fFormatString RETURNS CHARACTER PRIVATE
    ( ipcString AS CHARACTER, ipiCharacters AS INTEGER ):
    /*------------------------------------------------------------------------------
     Purpose:  Formats string with number of characters.  If string is larger than what fits, 
     it auto adds a "cont" string to end
     Notes:
    ------------------------------------------------------------------------------*/	
    
    RETURN DYNAMIC-FUNCTION("FormatString" IN hdOutputProcs, ipcString, ipiCharacters).
    
		
END FUNCTION.
