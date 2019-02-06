
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

DEFINE VARIABLE gcFont    AS CHARACTER NO-UNDO.
DEFINE VARIABLE glClassic AS LOGICAL   NO-UNDO.


DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iOrder        AS INTEGER
    FIELD cType         AS CHARACTER 
    .

DEFINE STREAM sEstOutput.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pBuildSections(ipriEstHeader, ipcSectionStyle, ipcFormatStyle).
IF CAN-FIND(FIRST ttSection) THEN 
DO: 
    RUN pInitializeOutput(ipcOutputFile).
    RUN pProcessSections.
    RUN pCloseOutput.
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

PROCEDURE pCloseOutput PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Closes output
     Notes:
    ------------------------------------------------------------------------------*/

    OUTPUT STREAM sEstOutput CLOSE.


END PROCEDURE.

PROCEDURE pInitializeOutput PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFontTag AS CHARACTER NO-UNDO.

    OUTPUT STREAM sEstOutput TO  VALUE(ipcOutputFile).
    cFontTag = "<F" + gcFont + "><P12>". 
    RUN pWrite("<PREVIEW>" + cFontTag, YES, YES).

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
    RUN pPrintItemInformation(BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).
    RUN pPrintLayoutInformation(BUFFER ttEstForm, INPUT-OUTPUT iopiPageCount, INPUT-OUTPUT iRowCount).

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
    RUN pWriteToCoordinates(iopiRowCount, iDateLabelColumn, "Printed:", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iDateLabelColumn + 1, STRING(ipbf-ttEstHeader.dtPrintDateTime,"99/99/9999 HH:MM A"), NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, " by " + STRING(ipbf-ttEstHeader.cPrinter,"x(8)"), NO, NO, YES).
    
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn, "Type: ", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iEstimateColumn + 1, ipbf-ttEstHeader.cEstType, YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iDateLabelColumn, "Calculated:", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iDateLabelColumn + 1, STRING(ipbf-ttEstHeader.dtCalcDateTime, "99/99/9999 HH:MM A"), NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iPageLabelColumn, " by " + STRING(ipbf-ttEstHeader.cCalculator,"x(8)"), NO, NO, YES).
    
END PROCEDURE.
PROCEDURE pPrintItemInformation PRIVATE:
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
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerName, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Ship To:", NO, NO, YES).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToName, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress1, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress1, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress2, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress2, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerAddress3, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn + 1, ttEstItem.cShipToAddress3, NO, NO, NO).
            iopiRowCount = iopiRowCount + 1.
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn, "Customer ID:", NO, NO, YES).
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 1, ttEstItem.cCustomerID, YES, NO,NO).
            RUN pWriteToCoordinates(iopiRowCount, iCustomerColumn + 9, ttEstItem.cShipToID, NO, NO, NO).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn, "Salesperson:", NO, NO, YES).
            RUN pWriteToCoordinates(iopiRowCount, iShipToColumn + 1, ttEstItem.cSalesgroupName, NO, NO, NO).
            iopiRowCount = iopiRowCount + 2.
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, "Qty/Blank", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, "Name/Description", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, "Size/Color", NO, YES, NO).
            RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, "Style/Part #", NO, YES, NO).
        END.
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, TRIM(STRING(ttEstItem.dQtyRequired,">>>,>>>,>>9")), YES, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, ttEstItem.cItemName , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, ttEstItem.cSize , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, ttEstItem.cStyle, NO, NO, NO).
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn1, TRIM(STRING(ttEstBlank.iBlankNo,">>9")), NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn2, ttEstItem.cItemDescription1 , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn3, ttEstItem.cColor , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iItemColumn4, ttEstItem.cCustomerPart, NO, NO, NO).
    END.
END PROCEDURE.
PROCEDURE pPrintLayoutInformation PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Prints the top-most section of each page
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttEstForm FOR ttEstForm.
    DEFINE INPUT-OUTPUT PARAMETER iopiPageCount AS INTEGER.
    DEFINE INPUT-OUTPUT PARAMETER iopiRowCount AS INTEGER.
   
    DEFINE VARIABLE iLayoutColumn1 AS INTEGER INITIAL 12.
    DEFINE VARIABLE iLayoutColumn2 AS INTEGER INITIAL 22.
    DEFINE VARIABLE iLayoutColumn3 AS INTEGER INITIAL 32.
    DEFINE VARIABLE iLayoutColumn4 AS INTEGER INITIAL 45.
    DEFINE VARIABLE iLayoutColumn5 AS INTEGER INITIAL 58.
    DEFINE VARIABLE iLayoutColumn6 AS INTEGER INITIAL 72.
       
    iopiRowCount = iopiRowCount + 2.
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, "Width", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3, "Length", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, "Area", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn5, "#Up/Out", NO, YES, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6, "Weight", NO, YES, YES).
     
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.rec_keyForm EQ ipbf-ttEstForm.rec_KeyForm:
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Blank #" + TRIM(STRING(ttEstBlank.iBlankNo,">>9")) + ":", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, TRIM(STRING(ttEstBlank.dBlankWidth,">>>9.99999")) , NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3, TRIM(STRING(ttEstBlank.dBlankLength,">>>9.99999")) , NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3 + 1, ttEstBlank.cUOMDimension , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, TRIM(STRING(ttEstBlank.dBlankArea,">>>9.999")) , NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4 + 1, ttEstBlank.cUOMArea , NO, NO, NO).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn5, TRIM(STRING(ttEstBlank.iNumOut,">>>9")) , NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6, TRIM(STRING(ttEstBlank.dWeight,">>>9.9999")) , NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6 + 1, ttEstBlank.cUOMWeight, NO, NO, NO).
    END.
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Die:", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, TRIM(STRING(ttEstForm.dDieWidth,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3, TRIM(STRING(ttEstForm.dDieLength,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, TRIM(STRING(ttEstForm.dDieArea,">>>9.999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6, TRIM(STRING(ttEstForm.dWeightDie,">>>9.9999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6 + 1, ttEstForm.cUOMWeightDie, NO, NO, NO).
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Net:", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, TRIM(STRING(ttEstForm.dNetWidth,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3, TRIM(STRING(ttEstForm.dNetLength,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, TRIM(STRING(ttEstForm.dNetArea,">>>9.999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn5, TRIM(STRING(ttEstForm.iNumOut,">>>9")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6, TRIM(STRING(ttEstForm.dWeightNet,">>>9.9999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6 + 1, ttEstForm.cUOMWeightNet, NO, NO, NO).
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Gross:", NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, TRIM(STRING(ttEstForm.dGrossWidth,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3, TRIM(STRING(ttEstForm.dGrossLength,">>>9.99999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn3 + 1, ttEstForm.cUOMDimension , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, TRIM(STRING(ttEstForm.dGrossArea,">>>9.999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4 + 1, ttEstForm.cUOMArea , NO, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6, TRIM(STRING(ttEstForm.dWeightGross,">>>9.9999")) , NO, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn6 + 1, ttEstForm.cUOMWeightGross, NO, NO, NO).
    IF ttEstForm.dRollWidth NE 0 THEN DO:
        iopiRowCount = iopiRowCount + 1.
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Roll:", NO, NO, YES).
        RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2, TRIM(STRING(ttEstForm.dRollWidth,">>>9.99999")) , NO, NO, YES).
    END.
    iopiRowCount = iopiRowCount + 1.
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn1, "Totals->", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2,  "Sheets:", YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn2 + 1, TRIM(STRING(ttEstForm.dGrossQtyRequiredTotal,">>>>>>>>9")), YES, NO, NO).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4, TRIM(STRING(ttEstForm.dGrossQtyRequiredTotalArea,">>>9.999")) , YES, NO, YES).
    RUN pWriteToCoordinates(iopiRowCount, iLayoutColumn4 + 1, ttEstForm.cUOMGrossQtyRequiredTotalArea , YES, NO, NO).
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

    IF iplUnformatted THEN 
        PUT STREAM sEstOutput UNFORMATTED ipcText.
    ELSE 
        PUT STREAM sEstOutput ipcText.
    IF iplSkip THEN 
        PUT STREAM sEstOutput SKIP.
    
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

    DEFINE VARIABLE cCoordinates AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dUnderlineOffset AS DECIMAL NO-UNDO INITIAL 0.25.

    IF iplUnderline THEN 
        ipdR = ipdR - dUnderlineOffset.
    cCoordinates = "<R" + TRIM(STRING(ipdR)) + ">".

    IF iplRightJustified THEN
        cCoordinates = cCoordinates + "<RIGHT=C".
    ELSE 
        cCoordinates = cCoordinates + "<C".
    cCoordinates = cCoordinates + TRIM(STRING(ipdC)) + ">".

    cText = cCoordinates + ipcText.

    IF iplBold THEN cText = "<B>" + cText + "</B>".
    IF iplUnderline THEN cText = "<U>" + cText + "</U>".
    RUN pWrite(cText,YES,NO).

END PROCEDURE.

