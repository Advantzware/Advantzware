/*------------------------------------------------------------------------
  File: recappc.p
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
DEFINE OUTPUT PARAMETER TABLE FOR ttRecapProductCategory.
{AOA/includes/pRecapProductCategory.i}

RUN pBusinessLogic.

/* subject business logic */
cCompany = ipcCompany.
{AOA/BL/r-booked.i}

FOR EACH ttRecapProductCategory:
    ASSIGN
        ttRecapProductCategory.priceMSFCurrent = ttRecapProductCategory.amountCurrent / ttRecapProductCategory.sqFtCurrent
        ttRecapProductCategory.priceMSFPeriod  = ttRecapProductCategory.amountPeriod  / ttRecapProductCategory.sqFtPeriod
        ttRecapProductCategory.numDaysCurrent  = iPerDays[1]
        ttRecapProductCategory.numDaysPeriod   = iPerDays[2]
        .
    IF ttRecapProductCategory.priceMSFCurrent EQ ? THEN ttRecapProductCategory.priceMSFCurrent = 0.
    IF ttRecapProductCategory.priceMSFPeriod  EQ ? THEN ttRecapProductCategory.priceMSFPeriod  = 0.
END. /* each ttrecap */
