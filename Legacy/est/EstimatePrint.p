
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

DEFINE TEMP-TABLE ttSection
    FIELD rec_keyParent AS CHARACTER 
    FIELD iOrder        AS INTEGER
    FIELD cType         AS CHARACTER 
    FIELD cFont         AS CHARACTER 
    FIELD lClassic      AS LOGICAL
    .

DEFINE STREAM sEstOutput.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pBuildSections(ipriEstHeader, ipcSectionStyle, ipcFormatStyle).
RUN pProcessSections.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildSections PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a format, processes temp-table to build paging structure
     Notes: Options are by Form (by Item or by Blank -TBD)
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEstHeader AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipcSectionBy AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFormatStyle AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iSectionCount AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFont         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lClassic      AS LOGICAL   NO-UNDO.

    CASE ipcFormatStyle:
        WHEN "Classic" THEN 
            ASSIGN 
                lClassic = YES
                cFont    = "Times New Roman"
                .
        OTHERWISE 
        ASSIGN 
            lClassic = NO
            cFont    = ipcFormatStyle
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
                    ttSection.lClassic      = lClassic
                    ttSection.cFont         = cFont
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
                    ttSection.lClassic      = lClassic
                    ttSection.cFont         = cFont
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
                ttSection.lClassic      = lClassic
                ttSection.cFont         = cFont
                .
        END.
    END.
END PROCEDURE.

PROCEDURE pPrintHeader PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


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
               RUN pPrintForm(ttSection.rec_keyParent, ttSection.lClassic, INPUT-OUTPUT iPageCount).
            WHEN "Consolidated" THEN 
               RUN pPrintConsolidated(ttSection.rec_keyParent, ttSection.lClassic, INPUT-OUTPUT iPageCount).
            WHEN "ItemSummary" THEN 
               RUN pPrintItemSummary(ttSection.rec_keyParent, ttSection.lClassic, INPUT-OUTPUT iPageCount).
                       
        END CASE.
    END.

END PROCEDURE.

