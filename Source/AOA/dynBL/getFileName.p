/*------------------------------------------------------------------------
  File:         getFileName.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 4.28.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD cFileName AS CHARACTER FORMAT "x(60)" LABEL "File Name"
    .

{AOA/includes/dynRunBusinessLogicDefs.i}

PROCEDURE pAssignParamVariables:
END PROCEDURE.

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lOK       AS LOGICAL   NO-UNDO.
    
    SYSTEM-DIALOG GET-FILE cFileName 
        TITLE "Select Image File"
        FILTERS "CSV Files    (*.csv)" "*.csv",
                "All Files    (*.*) " "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE lOK
        .
    IF lOK THEN DO:
        CREATE ttTempTable.
        ttTempTable.cFileName = cFileName.
    END. /* if lok */
    RETURN cFileName.
END PROCEDURE.

PROCEDURE pGetFileName:
    DEFINE OUTPUT PARAMETER opcFileName AS CHARACTER NO-UNDO.
    
    RUN pBusinessLogic.
    opcFileName = RETURN-VALUE.
END PROCEDURE.
