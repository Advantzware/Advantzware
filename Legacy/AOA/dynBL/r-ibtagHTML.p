/*------------------------------------------------------------------------
  File:         r-ibtagHTML.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.11.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD htmlDateTime AS DATETIME LABEL "HTML Date/Time"
    .
/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 5006
{AOA/includes/subjectID{&subjectID}Defs.i}

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cHTML AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cText AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTemp AS CHARACTER NO-UNDO INITIAL "HTMLPages/html.tmp".

    OS-CREATE-DIR "HTMLPages".
    FIND LAST taskResult NO-LOCK
         WHERE taskResult.folderFile BEGINS "TaskResults/InventorybyBinandTag"
           AND taskResult.fileType   EQ "HTML"
         NO-ERROR.
    IF AVAILABLE taskResult THEN DO:
        CREATE ttTempTable.
        ASSIGN
            ttTempTable.htmlDateTime = NOW
            cHTML = ENTRY(1,taskResult.folderFile,".") + ".tmp"
            cHTML = REPLACE(cHTML,"TaskResults","HTMLPages")
            .
        OS-COPY VALUE(SEARCH(taskResult.folderFile)) VALUE(cTemp).
        OUTPUT TO VALUE(cHTML).
        INPUT FROM VALUE(cTemp) NO-ECHO.
        REPEAT:
            IMPORT UNFORMATTED cText.
            IF LEFT-TRIM(cText) BEGINS "<meta http-equiv=" THEN
            cText = REPLACE(cText,"/>",">" + CHR(10)
                  + "  <meta http-equiv=~"Refresh~" content=~"120~">")
                  .
            PUT UNFORMATTED cText SKIP.
        END. /* repeat */
        INPUT CLOSE.
        OUTPUT CLOSE.
        OS-DELETE VALUE(cTemp).
        ASSIGN
            cTemp = cHTML
            cHTML = REPLACE(cHTML,".tmp",".html")
            .
        OS-DELETE VALUE(cHTML).
        OS-RENAME VALUE(cTemp) VALUE(cHTML).
    END. /* if avail */
END PROCEDURE.
