/*------------------------------------------------------------------------
    File        : system\GLProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Sewa Singh
    Created     : Fri May 15 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* **********************  Function Prototypes  *********************** */

FUNCTION spfGetAccountAR RETURNS CHARACTER 
    (ipcCompany AS CHARACTER,
    ipcCustomer AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */ 

PROCEDURE spGLProcs_GetAccountAR:
/*------------------------------------------------------------------------------
 Purpose: get AR Class GL Account
 Notes:
 Syntax:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcCustomerId   AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcClassAccount AS CHARACTER NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplError        AS LOGICAL   NO-UNDO.
    
    FIND FIRST ar-ctrl NO-LOCK
         WHERE ar-ctrl.company EQ ipcCompany
         NO-ERROR.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ipcCompany
           AND cust.cust-no EQ  ipcCustomerId
         NO-ERROR.         
    IF AVAILABLE cust AND cust.classID NE 0 THEN DO:
        FIND FIRST arClass NO-LOCK
             WHERE arClass.classID EQ cust.classID        
             NO-ERROR. 
        IF AVAILABLE arClass THEN
        opcClassAccount = arClass.receivablesAcct.         
    END.
    ELSE IF AVAILABLE ar-ctrl THEN
         opcClassAccount = ar-ctrl.receivables. 
         ELSE oplError = YES.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION spfGetAccountAR RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER, ipcCustomer AS CHARACTER ):
/*------------------------------------------------------------------------------
 Purpose: return due date on invoice 
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturnAccount AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lGetError      AS LOGICAL   NO-UNDO.
               
    RUN spGLProcs_GetAccountAR (ipcCompany,ipcCustomer, OUTPUT cReturnAccount, OUTPUT lGetError).
    
    RETURN IF lGetError THEN "" ELSE cReturnAccount.
		
END FUNCTION.
