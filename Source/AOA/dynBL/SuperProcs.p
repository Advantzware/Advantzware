/*------------------------------------------------------------------------
  File:         SuperProcs.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 11.28.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSuperProc
{AOA/tempTable/ttSuperProc.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 64
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    RUN pSuperProcs.
END PROCEDURE.

{system/pSuperProcs.i}
