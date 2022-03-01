/*------------------------------------------------------------------------
  File:         r-booked.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 2.18.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttOrdersBooked
{AOA/tempTable/ttOrdersBooked.i}
{AOA/tempTable/ttRecapProductCategory.i}

{AOA/dynBL/r-bookedDefs.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 161
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
{AOA/dynBL/r-booked.i}
