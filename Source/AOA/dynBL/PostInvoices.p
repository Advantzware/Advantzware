/*------------------------------------------------------------------------
  File:         PostInvoices.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 5.8.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define fGetTableHandle
DEFINE VARIABLE hPostInvoices AS HANDLE NO-UNDO.
DEFINE VARIABLE hTempTable    AS HANDLE NO-UNDO.

FUNCTION fGetTableHandle RETURNS HANDLE ():
    RETURN hTempTable.
END FUNCTION.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 109
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

RUN oe/PostInvoices.p PERSISTENT SET hPostInvoices.
hTempTable = DYNAMIC-FUNCTION("fGetInvoiceToPostHandle" IN hPostInvoices).

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOptions        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCountPosted    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountProcessed AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCountValid     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.

    cOptions = IF lPost THEN "Post" ELSE "".
    RUN PostInvoices IN hPostInvoices (
        cCompany,
        iStartInvNo,
        iEndInvNo,
        dtStartInvoiceDate,
        dtEndInvoiceDate,
        cStartCustNo,
        cEndCustNo,
        dtPostDate,
        cOptions,
        OUTPUT iCountProcessed,
        OUTPUT iCountValid,
        OUTPUT iCountPosted,
        OUTPUT lError,
        OUTPUT cMessage
        ).
END PROCEDURE.
