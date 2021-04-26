
/*------------------------------------------------------------------------
    File        : TrialBalance.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Mar 29 21:28:53 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{gl\ttTrialBalance.i}

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
DEFINE INPUT PARAMETER ipcAccountStart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcAccountEnd AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttTrialBalance.   


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetAccountGroup RETURNS CHARACTER PRIVATE
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */
RUN pRunReport(ipcCompany, ipdtAsOf, ipcAccountStart, ipcAccountEnd).



/* **********************  Internal Procedures  *********************** */

PROCEDURE pExportToCSV PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdTempTable AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE hdOutput AS HANDLE NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFile AS CHARACTER NO-UNDO INITIAL "c:\temp\TrialBalance.csv".
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
     
    RUN Output_TempTableToCSV IN hdOutput (iphdTempTable, cFile, YES, YES, OUTPUT lSuccess, OUTPUT cMessage).

    DELETE OBJECT hdOutput.
END PROCEDURE.

PROCEDURE pGetCompanyAttributes PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER oplIsFYEnd AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcContra AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRet AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtPeriodStart AS DATE NO-UNDO.
    
    FIND FIRST company NO-LOCK 
        WHERE company.company EQ ipcCompany
        NO-ERROR.
    IF AVAILABLE company THEN 
    DO:
        FIND LAST period NO-LOCK 
            WHERE period.company EQ ipcCompany
            AND period.pst     LE ipdtAsOf
            AND period.pend    GE ipdtAsOf
            NO-ERROR.
        IF AVAILABLE period THEN 
            ASSIGN
                opdtPeriodStart = period.pst
                .
            
        FIND FIRST gl-ctrl NO-LOCK
            WHERE gl-ctrl.company EQ company.company
            NO-ERROR.
        IF AVAILABLE gl-ctrl THEN 
            ASSIGN 
                opcContra = gl-ctrl.contra
                opcRet    = gl-ctrl.ret
                .
        FIND LAST period NO-LOCK 
            WHERE period.company EQ company.company 
            AND period.pnum EQ company.num-per  /* it's the last period of (a) year */ 
            AND period.pend EQ ipdtAsOf        /* it's the end date of the last period */
            NO-ERROR.
        ASSIGN 
            oplIsFYEnd = AVAILABLE period.
    END.

END PROCEDURE.

PROCEDURE pRunReport PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  given inputs, build temp-table and export it
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountStart AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountEnd AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-account FOR account.
    
    DEFINE VARIABLE lIsFYEnd      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cActContra    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cActRet       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtPeriodStart AS DATE      NO-UNDO.
    
    DEFINE VARIABLE dTotYTD AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotPTD AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotYTDBal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotYTDInc AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotPTDBal AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotPTDInc AS DECIMAL NO-UNDO.
    
    RUN pGetCompanyAttributes(ipcCompany, ipdtAsOf, OUTPUT lIsFYEnd, OUTPUT cActContra, OUTPUT cActRet, OUTPUT dtPeriodStart).
    
    FOR EACH account NO-LOCK 
        WHERE account.company EQ ipcCompany
        AND account.actnum GE ipcAccountStart
        AND account.actnum LE ipcAccountEnd:
        
        CREATE ttTrialBalance.
        ASSIGN 
            ttTrialBalance.accountID         = account.actnum
            ttTrialBalance.accountName       = account.dscr
            ttTrialBalance.accountType       = account.type
            ttTrialBalance.isBalanceSheet    = INDEX("ALCT",account.type) GT 0 AND NOT account.actnum EQ cActRet
            ttTrialBalance.isIncomeStatement = INDEX("RE",account.type) GT 0 AND NOT account.actnum EQ cActContra
            .
        
        IF account.actnum EQ cActContra OR account.actnum EQ cActRet THEN NEXT.    
        IF NOT lIsFYEnd OR ttTrialBalance.isBalanceSheet THEN 
            RUN GL_GetAccountOpenBal(ROWID(account), ipdtAsOf + 1, OUTPUT ttTrialBalance.amountYTDOpen).
        
        ELSE 
        DO: 
            RUN GL_GetAccountOpenBal(ROWID(account), ipdtAsOf, OUTPUT ttTrialBalance.amountYTDOpen).            
            
            FOR EACH glhist NO-LOCK 
                WHERE glhist.company EQ account.company 
                AND glhist.actnum  EQ account.actnum
                AND glhist.tr-date EQ ipdtAsOf:
                    
                ASSIGN 
                    ttTrialBalance.amountYTDFYE = ttTrialBalance.amountYTDFYE + glhist.tr-amt.
            END.
        END.
        
        FOR EACH glhist NO-LOCK 
            WHERE glhist.company EQ account.company
            AND glhist.actnum  EQ account.actnum
            AND glhist.tr-date GE dtPeriodStart 
            AND glhist.tr-date LE ipdtAsOf:

            ASSIGN
                ttTrialBalance.amountPTD = ttTrialBalance.amountPTD + glhist.tr-amt
                dTotPTD   = dTotPTD   + glhist.tr-amt
                .
        END.
        
        ASSIGN 
            ttTrialBalance.amountYTD = ttTrialBalance.amountYTDOpen + ttTrialBalance.amountYTDFYE
            dTotYTD = dTotYTD + ttTrialBalance.amountYTD.
        IF ttTrialBalance.isBalanceSheet THEN 
            ASSIGN 
                dTotYTDBal = dTotYTDBal + ttTrialBalance.amountYTD
                dTotPTDBal = dTotPTDBal + ttTrialBalance.amountPTD
                .
        IF ttTrialBalance.isIncomeStatement THEN 
            ASSIGN 
                dTotYTDInc = dTotYTDInc + ttTrialBalance.amountYTD
                dTotPTDInc = dTotPTDInc + ttTrialBalance.amountPTD
                .            
    END.  
    FIND ttTrialBalance 
        WHERE ttTrialBalance.accountID EQ cActContra
        NO-ERROR.
    IF AVAILABLE ttTrialBalance THEN 
        ASSIGN 
            ttTrialBalance.amountYTD = - dTotYTDInc
            ttTrialBalance.amountPTD = - dTotPTDInc
            .     
    FIND ttTrialBalance 
        WHERE ttTrialBalance.accountID EQ cActRet
        NO-ERROR.
    IF AVAILABLE ttTrialBalance THEN 
        ASSIGN 
            ttTrialBalance.amountYTD = dTotYTDInc
            ttTrialBalance.amountPTD = dTotPTDInc
            .                 
            
    //RUN pExportToCSV(TEMP-TABLE ttTrialBalance:HANDLE).
    
END PROCEDURE.



