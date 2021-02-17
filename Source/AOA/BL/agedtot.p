/*------------------------------------------------------------------------
  File: agedtot.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

DEFINE VARIABLE cProgressBar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lProgressBar AS LOGICAL   NO-UNDO.

{aoa/BL/r-aragedDefs.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttAgedReceivablesTotals.
{aoa/includes/pAgedReceivablesTotals.i}

DEFINE VARIABLE iPeriodDays4 AS INTEGER NO-UNDO INITIAL 120.

/* subject business logic */
{aoa/BL/r-araged.i}
