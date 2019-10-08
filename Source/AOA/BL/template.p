/*------------------------------------------------------------------------
  File: template.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Template.rpa */
{aoa/tempTable/ttTemplate.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttTemplate.
{aoa/includes/pTemplate.i}

/* local variables */
DEFINE VARIABLE idx AS INTEGER NO-UNDO.

/* subject business logic */
DO idx = 1 TO 5:
    CREATE ttTemplate.
    ASSIGN
        ttTemplate.field1 = STRING(idx)
        ttTemplate.field2 = idx
        ttTemplate.field3 = idx * 1.1
        ttTemplate.field4 = TODAY + idx
        ttTemplate.field5 = idx MOD 2 EQ 0
        .
END. /* do idx */

{aoa/BL/pBuildCustList.i}
