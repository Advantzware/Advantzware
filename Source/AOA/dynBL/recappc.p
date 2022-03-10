/*------------------------------------------------------------------------
  File:         recappc.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.18.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttRecapProductCategory
{AOA/tempTable/ttRecapProductCategory.i}
{AOA/tempTable/ttOrdersBooked.i}

{AOA/dynBL/r-bookedDefs.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 162
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
{AOA/dynBL/r-booked.i}
