
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
    DEFINE VARIABLE iEstimateColumn AS INTEGER INITIAL 14.
    
    iopiPageCount = iopiPageCount + 1.
    IF iopiPageCount GT 1 THEN PAGE.
  
    RUN pWriteToCoordinates(iopiRowCount - .3 , 2, "Estimate Calculation", YES, YES, NO).
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
   
    DEFINE VARIABLE iShipToColumn AS INTEGER INITIAL 55.
    DEFINE VARIABLE iCustomerColumn AS INTEGER INITIAL 14.
       
    iopiRowCount = iopiRowCount + 2.
    FOR EACH ttEstBlank NO-LOCK 
        WHERE ttEstBlank.rec_keyForm EQ ipbf-ttEstForm.rec_KeyForm,
        FIRST ttEstItem NO-LOCK 
            WHERE ttEstItem.rec_keyItem EQ ttEstBlank.rec_keyItem
        BREAK BY ttEstBlank.iBlankNo:
        IF FIRST(ttEstBlank.iBlankNo) THEN DO:
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
        END.
        
    END.
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

