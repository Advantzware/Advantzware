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

PROCEDURE checkInvalidGLAccount:
/*------------------------------------------------------------------------------
 Purpose: Check GL Account
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany  AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcAccount  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opInactive AS LOGICAL   NO-UNDO.
    
    FIND FIRST account NO-LOCK
         WHERE account.company EQ ipcCompany
           AND account.type    NE "T" 
           AND account.actnum  EQ ipcAccount  
           NO-ERROR.
    IF AVAILABLE account THEN DO: 
        IF account.inactive EQ YES THEN 
            opInactive = YES.
        ELSE    
            opInactive = FALSE. 
    END.                                     

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
