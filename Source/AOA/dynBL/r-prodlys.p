/*------------------------------------------------------------------------
  File:         r-prodlys.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.14.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttProductionAnalysis
{AOA/tempTable/ttProductionAnalysis.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 200
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

{AOA/dynBL/r-prodlys.i}
