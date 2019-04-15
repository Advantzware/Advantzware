/*------------------------------------------------------------------------
  File:         template.p
  Description:  Business Logic Template
  Author:       Ron Stark
  Date Created: 4.10.2019
  
  this is the basic template used for business logic modules
  1. define temp-table
  2. if temp-table not named ttTempTable, change scop-def line 29
  3. set scop-def subjectID with associated subject id value line 31
  4. generate AOA/includes/subjectID<subject id number>Defs.i in dynSubjct.w
  5. place all business logic in Procedure pBusinessLogic below
  6. add your file name, description, your name and date above
  7. remove comment lines 39, 30, 28 and 6 thru 15
  8. if needed, review AOA/dynBL/r-mchtrn.p as an example
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD fieldName AS CHARACTER LABEL "Field Name" FORMAT "x(8)"
        INDEX fieldName IS PRIMARY fieldName
        .
/* Parameters Definitions ---                                           */

/* change value of ttTempTable if TEMP-TABLE not named ttTempTable      */
&Scoped-define ttTempTable ttTempTable
/* subject id value associated with this business logic                 */
&Scoped-define subjectID 0
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
/* place logic here to populate ttTempTable - remove this comment       */
END PROCEDURE.
