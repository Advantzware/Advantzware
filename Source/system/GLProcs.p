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

FUNCTION GL_GetAccountAR RETURNS CHARACTER 
    (ipcCompany AS CHARACTER,
    ipcCustomer AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */ 

PROCEDURE GL_CheckGLAccount:
/*------------------------------------------------------------------------------
 Purpose: Check Invalid and Inactive GL Account
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcAccount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplActive  AS LOGICAL   NO-UNDO.  
        
    RUN GL_CheckInvalidGLAccount (       
        INPUT  ipcCompany,
        INPUT  ipcAccount,       
        OUTPUT opcMessage,
        OUTPUT oplSuccess       
        ).         
    
    IF oplSuccess EQ NO THEN  
        RETURN.  
    ELSE 
        RUN GL_CheckInactiveGLAccount (
            INPUT  ipcCompany,
            INPUT  ipcAccount,
            OUTPUT opcMessage,            
            OUTPUT oplActive
            ).              
         
END PROCEDURE.

PROCEDURE GL_CheckInactiveGLAccount:
/*------------------------------------------------------------------------------
 Purpose: Check Inactive GL Account.
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER  ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcAccount AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oplActive  AS LOGICAL   NO-UNDO.    
    
    IF CAN-FIND(FIRST account
        WHERE account.company  EQ ipcCompany 
          AND account.actnum   EQ ipcAccount           
          AND account.TYPE     NE "T"
          AND account.inactive EQ YES) THEN DO:                        
                    
        opcMessage = "Inactive Account Number.".                                                                       
                                         
    END. 
    ELSE 
        oplActive  = YES.  
                    
END PROCEDURE.

PROCEDURE GL_CheckInvalidGLAccount:
/*------------------------------------------------------------------------------
 Purpose: Check Invalid GL Account
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER  ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER  ipcAccount AS CHARACTER NO-UNDO.   
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.               
     
    IF ipcAccount EQ "" THEN DO:
        opcMessage = "Account Number may not be spaces, try help...".
        oplSuccess = NO. 
        RETURN.
    END. 
    
    IF NOT CAN-FIND(FIRST account
        WHERE account.company EQ ipcCompany 
          AND account.actnum  EQ ipcAccount           
          AND account.TYPE    NE "T") THEN DO: 
    
        opcMessage = "Invalid GL#, try help...".                        
        RETURN.    
    END. 
    
    oplSuccess = YES.           
    
END PROCEDURE.

PROCEDURE pGetAccountAR PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: get AR Class GL Account
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcCustomerId      AS CHARACTER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcClassAccount    AS CHARACTER   NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplError           AS LOGICAL     NO-UNDO.
    
    FIND FIRST ar-ctrl NO-LOCK 
        WHERE ar-ctrl.company EQ ipcCompany 
        NO-ERROR.
    IF AVAILABLE ar-ctrl THEN 
        opcClassAccount = ar-ctrl.receivables.
    
    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
        AND cust.cust-no EQ  ipcCustomerId 
        NO-ERROR.
         
    IF AVAIL cust AND cust.classId NE 0 THEN
    DO:
        FIND FIRST arClass NO-LOCK
             WHERE arClass.classID EQ cust.classId        
             NO-ERROR. 
        IF AVAIL arClass THEN 
          ASSIGN
             opcClassAccount = arClass.receivablesAcct.         
    END.
    IF opcClassAccount EQ "" THEN 
       oplError = YES .

END PROCEDURE.


/* ************************  Function Implementations ***************** */



FUNCTION GL_GetAccountAR RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER, ipcCustomer AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: return due date on invoice 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturnAccount AS CHARACTER NO-UNDO .
    DEFINE VARIABLE lGetError      AS LOGICAL NO-UNDO.
               
    RUN pGetAccountAR( ipcCompany,ipcCustomer, OUTPUT cReturnAccount , OUTPUT lGetError) .
    
    RETURN IF lGetError THEN "" ELSE cReturnAccount.
		
END FUNCTION.
