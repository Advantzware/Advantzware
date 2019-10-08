
/*------------------------------------------------------------------------
    File        : OrderProcsTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Wed Jun 05 13:52:31 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdOrderProcs AS HANDLE.
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED} /*Refactor - hate this*/
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN oe/OrderProcs.p PERSISTENT SET hdOrderProcs.
ASSIGN 
    cocode = g_company
    locode = g_loc
    .    
RUN TestWithExistingOrder.

/* **********************  Internal Procedures  *********************** */

PROCEDURE TestWithExistingOrder PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

FIND FIRST oe-ord NO-LOCK 
    WHERE oe-ord.company EQ '001'
    AND oe-ord.cust-no EQ 'ZOV100'
    AND oe-ord.ord-no EQ 211174
    NO-ERROR.
RUN ProcessImportedOrder IN hdOrderProcs (ROWID(oe-ord), OUTPUT lError, OUTPUT cMessage).

END PROCEDURE.

