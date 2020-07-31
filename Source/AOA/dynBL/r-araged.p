/*------------------------------------------------------------------------
  File:         r-araged.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 7.22.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttAgedReceivables
{AOA/BL/r-aragedDefs.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 134
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cSort1 AS CHARACTER NO-UNDO INITIAL "Customer No".
DEFINE VARIABLE cSort2 AS CHARACTER NO-UNDO INITIAL "Due Date".
DEFINE VARIABLE cType  AS CHARACTER NO-UNDO INITIAL "Detail".

/* **********************  Internal Procedures  *********************** */

{AOA/BL/r-araged.i}

{aoa/BL/pGetCustInfo.i}
