/*------------------------------------------------------------------------
  File: r-prodlys.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Customer Inventory.rpa */
{aoa/tempTable/ttProductionAnalysis.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttProductionAnalysis.
{aoa/includes/pProductionAnalysis.i}

{aoa/BL/r-prodlys.i}
