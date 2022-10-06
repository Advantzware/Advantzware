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

PROCEDURE GL_CloseMonthModule:
    /*------------------------------------------------------------------------------
     Purpose: close sub ledger of month 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany         AS CHARACTER   NO-UNDO.    
    DEFINE INPUT PARAMETER ipiPeriodYear      AS INTEGER     NO-UNDO.    
    DEFINE INPUT PARAMETER ipiPeriod          AS INTEGER     NO-UNDO.     
    DEFINE INPUT PARAMETER ipcModule          AS CHARACTER   NO-UNDO.
    
    RUN pCloseMonthModule(INPUT ipcCompany, INPUT ipiPeriodYear, INPUT ipiPeriod, INPUT ipcModule).       
    

END PROCEDURE.

PROCEDURE GL_CheckRunBalance:
    /*------------------------------------------------------------------------------
     Purpose: close sub ledger of month 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER   NO-UNDO.    
    DEFINE INPUT PARAMETER ipiTrNumber         AS INTEGER     NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdAmountBalance   AS DECIMAL     NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplUnBalance       AS LOGICAL     NO-UNDO.
    
    RUN pCheckRunBalance(INPUT ipcCompany, INPUT ipiTrNumber, OUTPUT opdAmountBalance, OUTPUT oplUnBalance).       
    

END PROCEDURE.

PROCEDURE pCloseMonthModule PRIVATE:
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
    
    FIND FIRST period EXCLUSIVE-LOCK
         WHERE period.company EQ ipcCompany 
         AND period.yr EQ ipiPeriodYear
         AND period.pnum EQ ipiPeriod
        NO-ERROR.     
    
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

PROCEDURE pCheckRunBalance PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: close sub ledger of month 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          AS CHARACTER   NO-UNDO.    
    DEFINE INPUT PARAMETER ipiTrNumber         AS INTEGER     NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdAmountBalance   AS DECIMAL     NO-UNDO.     
    DEFINE OUTPUT PARAMETER oplUnBalance       AS LOGICAL     NO-UNDO.
    
    DEFINE VARIABLE dBalanceAmount AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-glhist FOR glhist.
    
    FOR EACH bf-glhist NO-LOCK
        WHERE bf-glhist.company EQ ipcCompany
          AND bf-glhist.tr-num EQ ipiTrNumber:
                 
          ASSIGN
            dBalanceAmount = dBalanceAmount + bf-glhist.tr-amt .
    END.
    IF dBalanceAmount NE 0 THEN
    ASSIGN 
        opdAmountBalance = -(dBalanceAmount)
        oplUnBalance = YES.
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
    
    IF ipdTrAmount NE 0 THEN
    DO:      
        FIND FIRST period NO-LOCK
             WHERE period.company EQ ipcCompany
             AND period.pst LE ipdtTrDate
             AND period.pend GE ipdtTrDate NO-ERROR.
        
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
           bf-glhist.glYear     = IF AVAIL period THEN period.yr ELSE YEAR(ipdtTrDate)
           bf-glhist.yr         = YEAR(ipdtTrDate)
           bf-glhist.entryType  = ipcEntryType
           bf-glhist.sourceDate = ipdtSourceDate
           bf-glhist.documentID = ipcDocumentID
           bf-glhist.module     = ipcModule        
           bf-glhist.posted     = NO.
                              
        RELEASE bf-glhist.
    END.

END PROCEDURE.

PROCEDURE GL_GetAccountOpenBal :
    /*------------------------------------------------------------------------------
     Purpose: get open balance GL Account as of the *Start (12:00:01 AM)* of the date entered.
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriAccount      AS ROWID   NO-UNDO.    
    DEFINE INPUT  PARAMETER ipdtAsOf       AS   DATE NO-UNDO.    
    DEFINE OUTPUT PARAMETER opdBalOpen      AS   DECIMAL NO-UNDO.
       
    DEFINE VARIABLE lIsBalanceSheet   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsAsOfDateInClosedYear AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsAccountContra AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIsAccountRetained AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtAsOfFiscalYearStart AS DATE NO-UNDO.
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-cur-period        FOR period.
    DEFINE BUFFER bf-first-period      FOR period.
    DEFINE BUFFER bf-first-open-period FOR period.
    DEFINE BUFFER bf-first-open-year-period FOR period.
    DEFINE BUFFER bf-account FOR account.
    DEFINE BUFFER bf-company FOR company.

    FIND account WHERE ROWID(account) EQ ipriAccount NO-LOCK NO-ERROR.
    IF NOT AVAILABLE account THEN RETURN.
    FIND FIRST gl-ctrl NO-LOCK 
        WHERE gl-ctrl.company EQ account.company
        NO-ERROR.
    IF AVAILABLE gl-ctrl THEN 
        ASSIGN 
            lIsAccountContra = account.actnum EQ gl-ctrl.contra
            lIsAccountRetained = account.actnum EQ gl-ctrl.ret
            .
    FIND LAST bf-cur-period NO-LOCK
        WHERE bf-cur-period.company EQ account.company
        AND bf-cur-period.pst     LE ipdtAsOf
        AND bf-cur-period.pend    GE ipdtAsOf
        NO-ERROR.

    IF AVAILABLE bf-cur-period THEN       
        FIND FIRST bf-first-period NO-LOCK      
            WHERE bf-first-period.company EQ bf-cur-period.company
            AND bf-first-period.yr EQ bf-cur-period.yr
            AND bf-first-period.pnum EQ 1
            NO-ERROR.   

    FIND FIRST bf-company NO-LOCK 
        WHERE bf-company.company EQ account.company
        NO-ERROR.
    
    FIND FIRST bf-first-open-period NO-LOCK
        WHERE bf-first-open-period.company EQ account.company
        AND bf-first-open-period.pstat   EQ YES
        NO-ERROR.
    
    IF AVAILABLE bf-first-open-period THEN 
        FIND FIRST bf-first-open-year-period NO-LOCK 
            WHERE bf-first-open-year-period.company EQ account.company
            AND bf-first-open-year-period.yr EQ bf-first-open-period.yr
            AND bf-first-open-year-period.pnum EQ 1
            NO-ERROR.
            
    /*Needed if last period is closed but year is not yet closed*/        
    IF AVAILABLE bf-company AND bf-first-open-period.pnum EQ 1 AND bf-company.yend-per EQ NO THEN 
        FIND FIRST bf-first-open-year-period NO-LOCK 
            WHERE bf-first-open-year-period.company EQ account.company
            AND bf-first-open-year-period.yr EQ bf-first-open-period.yr - 1
            AND bf-first-open-year-period.pnum EQ 1
            NO-ERROR.
            
    lIsAsOfDateInClosedYear = ipdtAsOf LT bf-first-open-year-period.pst.
    lIsBalanceSheet = INDEX("ALCT",account.type) GT 0.

    IF lIsAccountContra THEN
        ASSIGN 
            opdBalOpen            = 0
            dtAsOfFiscalYearStart = bf-first-period.pst
            .
    ELSE IF lIsAccountRetained THEN 
        DO:
            ASSIGN 
                opdBalOpen            = account.cyr-open
                dtAsOfFiscalYearStart = bf-first-open-period.pst
                .
            DO iPeriod = 1 to bf-first-open-period.pnum - 1:
                opdBalOpen = opdBalOpen + account.cyr[iPeriod].
            END.
        END.
    ELSE IF lIsBalanceSheet THEN     
        ASSIGN   //Balance Sheet - Pivot on the current year open balance of FY - add if as of date is in open year and subtract if as of date in closed year
            opdBalOpen = account.cyr-open
            dtAsOfFiscalYearStart = bf-first-open-year-period.pst
            .
    ELSE 
        ASSIGN //Income statement - always start from zero and count forward from first day of FY
            opdBalOpen = 0
            dtAsOfFiscalYearStart = bf-first-period.pst
            .
    
    IF lIsAccountContra OR lIsAccountRetained THEN DO:
        FOR EACH bf-account NO-LOCK 
            WHERE bf-account.company EQ account.company
            AND INDEX("RE",bf-account.type) NE 0
            //AND bf-account.actnum NE gl-ctrl.contra
            //AND bf-account.actnum NE gl-ctrl.ret
            ,
            EACH glhist no-lock
            WHERE glhist.company EQ bf-account.company
            AND glhist.actnum EQ bf-account.actnum
            AND glhist.tr-date GE dtAsOfFiscalYearStart
            AND glhist.tr-date LT ipdtAsOf:
                
            IF lIsAccountRetained THEN 
                opdBalOpen = opdBalOpen + glhist.tr-amt.
            ELSE 
                opdBalOpen = opdBalOpen - glhist.tr-amt.
            
        END.
    END.
    ELSE DO:        
        IF NOT lIsAsOfDateInClosedYear OR NOT lIsBalanceSheet THEN
        DO:       
            FOR EACH glhist NO-LOCK  /*All transactions from start of fiscal year up to the as of date*/
                WHERE glhist.company EQ account.company
                AND glhist.actnum EQ account.actnum
                AND glhist.tr-date GE dtAsOfFiscalYearStart
                AND glhist.tr-date LT ipdtAsOf:  
                    
                ASSIGN
                    opdBalOpen      = opdBalOpen + glhist.tr-amt                 
                    . 
            END. 
        END.
        ELSE 
        DO: 
            FOR EACH glhist NO-LOCK /*All transactions prior to end of fiscal year start */
                WHERE glhist.company EQ account.company
                AND glhist.actnum EQ account.actnum
                AND glhist.tr-date GE ipdtAsOf
                AND glhist.tr-date LT dtAsOfFiscalYearStart:  
                    
                ASSIGN
                    opdBalOpen      = opdBalOpen - glhist.tr-amt                 
                    .             
            END.  
        END.   
    END.
END PROCEDURE.

PROCEDURE GL_ReOpenPeriod :
    /*------------------------------------------------------------------------------
     Purpose: Reopen Period 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER iprwRowid   AS ROWID       NO-UNDO.  
    
    RUN pReOpenPeriod(INPUT ipcCompany, INPUT iprwRowid).
    
END PROCEDURE.      
    

PROCEDURE pReOpenPeriod PRIVATE :
    /*------------------------------------------------------------------------------
     Purpose: Reopen Period 
     Notes:
     Syntax:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany  AS CHARACTER   NO-UNDO.    
    DEFINE INPUT  PARAMETER iprwRowid   AS ROWID       NO-UNDO.    
    
    DEFINE VARIABLE cTmpDir AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-period FOR period.
    DEFINE BUFFER bf-account FOR account.
    
    FIND FIRST users NO-LOCK
        WHERE users.user_id EQ USERID(LDBNAME(1))
        NO-ERROR.

    IF AVAIL users AND users.user_program[2] NE "" THEN
       cTmpDir = users.user_program[2].
    ELSE
       cTmpDir = "c:\tmp".
       
    RUN spProgressBar ("Reopen Period", 1, 3).
    FIND FIRST company NO-LOCK where company.company eq ipcCompany NO-ERROR.
    FIND FIRST bf-period EXCLUSIVE-LOCK 
         WHERE bf-period.company EQ ipcCompany
         AND ROWID(bf-period) EQ iprwRowid NO-ERROR.
    IF AVAIL bf-period THEN
    DO:
        for each glhist
            where glhist.company eq ipcCompany         
            and glhist.tr-date ge bf-period.pst
            and glhist.tr-date le bf-period.pend
            and glhist.period  eq bf-period.pnum          
            :
            glhist.posted = NO.        
        END.  
       
        OUTPUT TO value(cTmpDir + "\account.d") . 
        FOR EACH bf-account
            where bf-account.company EQ ipcCompany:
            EXPORT bf-account.
            bf-account.cyr[bf-period.pnum] = 0.
        END.
        OUTPUT CLOSE. 
        RUN spProgressBar ("Reopen Period", 2, 3).
        bf-period.pstat = YES.
        ASSIGN
        bf-period.APClosedBy  = USERID(LDBNAME(1))
        bf-period.POClosedBy  = USERID(LDBNAME(1))
        bf-period.OPClosedBy  = USERID(LDBNAME(1))
        bf-period.WIPClosedBy = USERID(LDBNAME(1))
        bf-period.RMClosedBy  = USERID(LDBNAME(1))
        bf-period.FGClosedBy  = USERID(LDBNAME(1))
        bf-period.BRClosedBy  = USERID(LDBNAME(1))
        bf-period.ARClosedBy  = USERID(LDBNAME(1))        
        bf-period.subLedgerAP  = IF company.subLedgerAP EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerPO  = IF company.subLedgerPO EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerOP  = IF company.subLedgerOP EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerWIP = IF company.subLedgerWIP EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerRM  = IF company.subLedgerRM EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerFG  = IF company.subLedgerFG EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerBR  = IF company.subLedgerBR EQ YES THEN "O" ELSE "A"
        bf-period.subLedgerAR  = IF company.subLedgerAR EQ YES THEN "O" ELSE "A"       
        bf-period.APClosed  = NOW
        bf-period.POClosed  = NOW
        bf-period.OPClosed  = NOW
        bf-period.WIPClosed = NOW
        bf-period.RMClosed  = NOW
        bf-period.FGClosed  = NOW
        bf-period.BRClosed  = NOW
        bf-period.ARClosed  = NOW
        .
        
    END.
    RELEASE bf-period.
    RUN spProgressBar ("Reopen Period", 3, 3).
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
