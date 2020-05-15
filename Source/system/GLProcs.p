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

FUNCTION spGetARClassAccount RETURNS CHARACTER 
    (ipcCompany AS CHARACTER,
    ipcCustomer AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
/*Initialize Constants and Property Defaults*/

/* **********************  Internal Procedures  *********************** */ 

PROCEDURE spGLProcs_GetARClassAccount:
    /*------------------------------------------------------------------------------
     Purpose: get AR Class GL Account
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcCustomerId      AS CHARACTER   NO-UNDO.    
    DEFINE OUTPUT PARAMETER opcClassAccount    AS CHARACTER   NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplError           AS LOGICAL     NO-UNDO.
    
    FIND FIRST ar-ctrl NO-LOCK WHERE ar-ctrl.company = ipcCompany NO-ERROR.
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ ipcCompany
         AND cust.cust-no EQ  ipcCustomerId NO-ERROR.
         
    IF AVAIL cust AND cust.classId NE 0 THEN
    DO:
        FIND FIRST arClass NO-LOCK
             WHERE arClass.classID = cust.classId        
             NO-ERROR. 
        IF AVAIL arClass THEN 
          ASSIGN
             opcClassAccount = arClass.receivablesAcct.         
    END.
    ELSE IF AVAIL ar-ctrl THEN
    DO:
       opcClassAccount = ar-ctrl.receivables . 
    END. 
    ELSE DO:
       oplError = YES .
    END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */



FUNCTION spfGetARClassAccount RETURNS CHARACTER 
    ( ipcCompany AS CHARACTER, ipcCustomer AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: return due date on invoice 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO .
    DEFINE VARIABLE cReturnAccount AS CHARACTER NO-UNDO .    
    DEFINE VARIABLE lGetError      AS LOGICAL NO-UNDO.
               
    RUN spGLProcs_GetARClassAccount( ipcCompany,ipcCustomer, OUTPUT cReturnAccount , OUTPUT lGetError) .

    IF NOT lGetError THEN do:
        ASSIGN cReturnValue = cReturnAccount .            
    END.
    ELSE DO:
         ASSIGN cReturnValue = "".
    END.

    RETURN cReturnValue .   
		
END FUNCTION.
