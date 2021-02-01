/*------------------------------------------------------------------------
  File:         UOM.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 1.8.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&Scoped-define fGetTableHandle
FUNCTION fGetTableHandle RETURNS HANDLE ():
    RETURN DYNAMIC-FUNCTION("fConv_ttUOMHandle").
END FUNCTION.

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttUOM

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 78
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    /* no logic needed, but procedure needs to exist, do not delete */
END PROCEDURE.
