/*------------------------------------------------------------------------
  File: agedtot.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

{aoa/BL/r-aragedDefs.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttAgedReceivables.
{aoa/includes/pAgedReceivablesTotals.i}

/* subject business logic */
{aoa/BL/r-araged.i}
