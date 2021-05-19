/*------------------------------------------------------------------------
  File:         autoDMI.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 5.14.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttAutoDMI
DEFINE TEMP-TABLE ttAutoDMI NO-UNDO
    FIELD autoStartDateTime AS DATETIME  LABEL "Auto Export Execution Started"
    FIELD autoEndDateTime   AS DATETIME  LABEL "Auto Export Execution Ended"
    FIELD autoResult        AS CHARACTER LABEL "Result" FORMAT "x(30)"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 5097
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    CREATE ttAutoDMI.
    ASSIGN
        ttAutoDMI.autoStartDateTime = NOW
        ttAutoDMI.autoResult = "Export Successful".
        .
    RUN spSetSessionParam ("Company", cCompany).
    RUN spSetSessionParam ("Location", cLocation).
    RUN spSetSessionParam ("adoSBJobs", "NO").
    RUN AOA/dynBL/adoSBJobs.p.
    IF RETURN-VALUE NE "" THEN
    ttAutoDMI.autoResult = "ADO Failed to Connect".
    ELSE
    RUN schedule/autoDMI.p.
    ttAutoDMI.autoEndDateTime = NOW.
    RUN spSetSessionParam ("adoSBJobs", "").
END PROCEDURE.
