
/*------------------------------------------------------------------------
    File        : salrep/SalesManProcs.p
    Purpose     : 

    Syntax      :

    Description : Procedures and functions for handling salesrep

    Author(s)   : Rahul Rawat
    Created     : Fri Jan 15 09:38:33 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* ************************  Function Prototypes ********************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE SalesMan_CheckInactiveSalesRep:
/*------------------------------------------------------------------------------
 Purpose: To Check inactive SalesRep
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSman     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST sman
                    WHERE sman.company EQ ipcCompany
                      AND sman.sman    EQ ipcSman) THEN 
                      
            opcMessage = "Invalid SalesRep".     
    ELSE 
        oplSuccess = YES.        
            
END PROCEDURE.

PROCEDURE SalesMan_CheckInvalidSalesRep:
/*------------------------------------------------------------------------------
 Purpose: To Check invalid SalesRep
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSman     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    IF NOT CAN-FIND(FIRST sman
                    WHERE sman.company  EQ ipcCompany
                      AND sman.sman     EQ ipcSman
                      AND sman.inactive EQ NO) THEN 
                      
            opcMessage = "Inactive SalesRep".     
    ELSE 
        oplSuccess = YES. 

END PROCEDURE.

PROCEDURE SalesMan_ValidateSalesRep:
/*------------------------------------------------------------------------------
 Purpose: To validate a given salesrep 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSman     AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    RUN SalesMan_CheckInvalidSalesRep(
        INPUT ipcCompany,
        INPUT ipcSman,
        OUTPUT oplSuccess,
        OUTPUT opcMessage
        ).
    IF oplSuccess THEN 
        RUN SalesMan_CheckInvalidSalesRep(
            INPUT ipcCompany,
            INPUT ipcSman,
            OUTPUT oplSuccess,
            OUTPUT opcMessage
            ).       
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION SalesMan_GetSalesmanName RETURNS CHARACTER 
	(ipcCompany AS CHARACTER ,ipcSalesman AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose: To get the salesman name based on given salesrep
 Notes:
 ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-sman FOR sman.

    IF ipcSalesman NE "" THEN
        FIND FIRST bf-sman NO-LOCK
            WHERE bf-sman.company EQ ipcCompany
              AND bf-sman.sman    EQ ipcSalesman
            NO-ERROR.
    IF AVAIL bf-sman THEN
        RETURN bf-sman.sname.   /* Function return value. */
    ELSE RETURN "".
END FUNCTION.

