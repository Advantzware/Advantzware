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

PROCEDURE spCreateGLHist :
    /*------------------------------------------------------------------------------
     Purpose: get AR Class GL Account
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany     AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcActnum      AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipcJrnl        AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTrDscr      AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipdtTrDate     AS DATE        NO-UNDO.    
    DEFINE INPUT  PARAMETER ipdTrAmount    AS DECIMAL     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTrNumber    AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPeriod      AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEntryType   AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtSourceDate AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER ipcDocumentID  AS CHARACTER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcModule      AS CHARACTER   NO-UNDO.
    
    
    DEFINE BUFFER bf-glhist FOR glhist.
    
    CREATE bf-glhist.
      ASSIGN
       bf-glhist.company    = ipcCompany
       bf-glhist.actnum     = ipcActnum
       bf-glhist.jrnl       = ipcJrnl
       bf-glhist.tr-dscr    = ipcTrDscr
       bf-glhist.tr-date    = ipdtTrDate
       bf-glhist.tr-amt     = ipdTrAmount
       bf-glhist.tr-num     = ipiTrNumber
       bf-glhist.period     = ipiPeriod  
       bf-glhist.glYear     = YEAR(ipdtTrDate)         
       bf-glhist.entryType  = ipcEntryType
       bf-glhist.sourceDate = ipdtSourceDate
       bf-glhist.documentID = ipcDocumentID
       bf-glhist.module     = ipcModule        
       bf-glhist.posted     = NO.
                          
    RELEASE bf-glhist.

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
