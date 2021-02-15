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
                    
        opcMessage = "GL Account is Inactive.".                                                                       
                                         
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

PROCEDURE GL_CheckModClosePeriod:
/*------------------------------------------------------------------------------
 Purpose: Validate sub ledger before posting or create glhist
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE INPUT PARAMETER  ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER  ipdtDate   AS DATE NO-UNDO.  
    DEFINE INPUT PARAMETER  ipcModule  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.               
     
    FIND FIRST period NO-LOCK                  
         WHERE period.company EQ ipcCompany
         AND period.pst LE ipdtDate
         AND period.pend GE ipdtDate
        NO-ERROR.
    if avail period then do:
       IF NOT period.pstat THEN DO:
          opcMessage = "Period closed posting not allowed ".
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerAP EQ "C" AND ipcModule EQ "AP" THEN DO:
          opcMessage = "A/P Payables sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.  
       ELSE IF period.subLedgerPO EQ "C" AND ipcModule EQ "PO" THEN DO:
          opcMessage = "P/O Purchasing sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerOP EQ "C" AND ipcModule EQ "OP" THEN DO:
          opcMessage = "O/P Order Processing sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerWIP EQ "C" AND ipcModule EQ "WIP" THEN DO:
          opcMessage = "Work In Process sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerRM EQ "C" AND ipcModule EQ "RM" THEN DO:
          opcMessage = "R/M Inventory sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerFG EQ "C" AND ipcModule EQ "FG" THEN DO:
          opcMessage = "F/G Inventory sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerBR EQ "C" AND ipcModule EQ "BR" THEN DO:
          opcMessage = "Bank Reconciliation sub ledger closed posting not allowed." .
          oplSuccess = NO.
          RETURN.
       END.
       ELSE IF period.subLedgerAR EQ "C" AND ipcModule EQ "AR" THEN DO:
          opcMessage = "A/R Receivables sub ledger closed posting not allowed.".
          oplSuccess = NO.
          RETURN.
       END.
    END.    
    ELSE IF NOT AVAIL period THEN
    DO:
        opcMessage = "No defined period exists for " + STRING(ipdtDate) .
        oplSuccess = NO.
        RETURN.
    END.
    
    oplSuccess = YES.           
    
END PROCEDURE.

PROCEDURE GL_pCloseMonthModule:
    /*------------------------------------------------------------------------------
     Purpose: close sub ledger of month 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER   NO-UNDO.    
    DEFINE INPUT PARAMETER ipiPeriodYear      AS INTEGER     NO-UNDO.    
    DEFINE INPUT PARAMETER ipiPeriod          AS INTEGER     NO-UNDO.     
    DEFINE INPUT PARAMETER ipcModule          AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER bf-account for account.
    DEFINE BUFFER bff-account for account.
    
    FIND FIRST period NO-LOCK
         WHERE period.company EQ ipcCompany 
         AND period.yr EQ ipiPeriodYear
         AND period.pnum EQ ipiPeriod
        NO-ERROR.
        
    FIND FIRST gl-ctrl NO-LOCK
         WHERE gl-ctrl.company eq ipcCompany no-error.    
            
    FOR EACH glhist
       where glhist.company eq ipcCompany
         and glhist.tr-date ge period.pst
         and glhist.tr-date le period.pend
         and glhist.period  eq ipiPeriod
         AND glhist.posted  EQ NO
         AND glhist.module  EQ ipcModule
       transaction:
       
      FIND FIRST account
           WHERE account.company eq ipcCompany
           and account.actnum  eq glhist.actnum
           NO-ERROR.
      IF AVAIL account THEN DO:
         account.cyr[ipiPeriod] = account.cyr[ipiPeriod] + glhist.tr-amt.

         IF INDEX("RE",account.type) GT 0 then do:
            FIND FIRST bf-account
                 WHERE bf-account.company eq ipcCompany
                 AND bf-account.actnum  eq gl-ctrl.ret.

            bf-account.cyr[ipiPeriod] = bf-account.cyr[ipiPeriod] + glhist.tr-amt.

            FIND FIRST bff-account
                 WHERE bff-account.company eq ipcCompany
                 AND bff-account.actnum  eq gl-ctrl.contra.

            bff-account.cyr[ipiPeriod] = bff-account.cyr[ipiPeriod] - glhist.tr-amt.
         END.
      END.
             
      ASSIGN
      glhist.posted = YES
      glhist.postedBy = USERID(LDBNAME(1)) .
    END.
    FIND CURRENT period EXCLUSIVE-LOCK NO-ERROR.
    IF ipcModule EQ "AP" THEN
     ASSIGN
       period.subLedgerAP = "C"
       period.APClosedBy  = USERID(LDBNAME(1))
       period.APClosed    = NOW .
    ELSE IF ipcModule EQ "PO" THEN
     ASSIGN
       period.subLedgerPO = "C"
       period.POClosedBy  = USERID(LDBNAME(1))
       period.POClosed    = NOW .   
    ELSE IF ipcModule EQ "OP" THEN
       ASSIGN
       period.subLedgerOP = "C"
       period.OPClosedBy  = USERID(LDBNAME(1))
       period.OPClosed    = NOW .
    ELSE IF ipcModule EQ "WIP" THEN
       ASSIGN
       period.subLedgerWIP = "C"
       period.WIPClosedBy  = USERID(LDBNAME(1))
       period.WIPClosed    = NOW .
    ELSE IF ipcModule EQ "RM" THEN
       ASSIGN
       period.subLedgerRM = "C"
       period.RMClosedBy  = USERID(LDBNAME(1))
       period.RMClosed    = NOW . 
    ELSE IF ipcModule EQ "FG" THEN
       ASSIGN
       period.subLedgerFG = "C"
       period.FGClosedBy  = USERID(LDBNAME(1))
       period.FGClosed    = NOW .
    ELSE IF ipcModule EQ "BR" THEN
       ASSIGN
       period.subLedgerBR = "C"
       period.BRClosedBy  = USERID(LDBNAME(1))
       period.BRClosed    = NOW . 
    ELSE IF ipcModule EQ "AR" THEN
       ASSIGN
       period.subLedgerAR = "C"
       period.ARClosedBy  = USERID(LDBNAME(1))
       period.ARClosed    = NOW .    
     
    FIND CURRENT period NO-LOCK NO-ERROR.         
    

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

PROCEDURE GL_SpCreateGLHist :
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
