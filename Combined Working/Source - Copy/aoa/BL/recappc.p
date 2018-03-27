/*------------------------------------------------------------------------
  File: recappc.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

{aoa/BL/r-bookedDefs.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttRecapProductCategory.
{aoa/includes/pRecapProductCategory.i}

/* subject business logic */
{aoa/BL/r-booked.i}

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
