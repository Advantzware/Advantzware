/*------------------------------------------------------------------------
  File: r-booked.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustPart     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipFromNo   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cProgressBar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustPart   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShipFromNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lOrdWithNoRel    AS LOGICAL   NO-UNDO INITIAL YES.
DEFINE VARIABLE lProgressBar     AS LOGICAL   NO-UNDO.

/* Orders Booked.rpa */
{AOA/tempTable/ttOrdersBooked.i}
/* Recap Product Category.rpa */
{AOA/tempTable/ttRecapProductCategory.i}

{AOA/BL/r-bookedDefs.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttOrdersBooked.
{AOA/includes/pOrdersBooked.i}

/* subject business logic */
ASSIGN
    cCompany       = ipcCompany
    cEndCustPart   = CHR(254)
    cEndShipFromNo = CHR(254)
    .

RUN pBusinessLogic.

{AOA/BL/r-booked.i}
