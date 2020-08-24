/*------------------------------------------------------------------------
  File: r-araged.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

{AOA/BL/r-aragedDefs.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttAgedReceivables.
{AOA/includes/pAgedReceivables.i}

RUN pBusinessLogic.

/* subject business logic */
{AOA/BL/r-araged.i}
