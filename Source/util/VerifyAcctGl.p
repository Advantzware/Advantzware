{custom/globdefs.i}

{sys/inc/var.i new shared}

DEFINE TEMP-TABLE ttAccountDiff
    FIELD cAccount    AS CHARACTER LABEL "Account #" 
    FIELD cAcctDescr  AS CHARACTER LABEL "Account Descr"
    FIELD iPeriod     AS INTEGER   LABEL "Period"
    FIELD iYear       AS INTEGER   LABEL "Year"
    FIELD dAccountAmt AS DECIMAL   LABEL "Account Period Amount"
    FIELD dGlHistAmt  AS DECIMAL   LABEL "GL Transaction Period Amount"
    FIELD dDiffAmt    AS DECIMAL   LABEL "Variance"
    .

DEFINE VARIABLE dAccountAmt AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dGlHistAmt  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cTmpDir     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iFiscalYear LIKE period.yr NO-UNDO.


SESSION:SET-WAIT-STATE("general").

ASSIGN 
    cocode = g_company
    locode = g_loc.

FIND FIRST company NO-LOCK 
    WHERE company.company EQ cocode 
    NO-ERROR.  
FIND FIRST gl-ctrl NO-LOCK
    WHERE gl-ctrl.company EQ cocode 
    NO-ERROR.
IF NOT AVAILABLE company OR NOT AVAILABLE gl-ctrl THEN RETURN.


FIND FIRST period NO-LOCK
    WHERE period.company EQ cocode
    AND period.pstat   EQ YES
    NO-ERROR.

iFiscalYear = (IF AVAILABLE period THEN period.yr ELSE  YEAR(TODAY)) -
    INTEGER(NOT company.yend-per).         
 
FOR EACH account NO-LOCK 
    WHERE account.company EQ cocode
    AND account.actnum NE gl-ctrl.ret
    AND account.actnum NE gl-ctrl.contra,
    EACH period NO-LOCK
    WHERE period.company EQ account.company
    AND period.yr      EQ iFiscalYear
    BY period.yr BY period.pst:
      
    dGlHistAmt = 0.
      
    FOR EACH glhist
        WHERE glhist.company EQ account.company
        AND glhist.actnum  EQ account.actnum
        AND glhist.tr-date GE period.pst
        AND glhist.tr-date LE period.pend         
        NO-LOCK :  
       
        dGlHistAmt = dGlHistAmt + glhist.tr-amt  .
    END. 
       
    IF  account.cyr[period.pnum] NE dGlHistAmt  THEN 
    DO:
     
        CREATE ttAccountDiff.
        ASSIGN 
            ttAccountDiff.cAccount    = account.actnum
            ttAccountDiff.cAcctDescr  = account.dscr
            ttAccountDiff.iPeriod     = period.pnum
            ttAccountDiff.iYear       = period.yr
            ttAccountDiff.dAccountAmt = account.cyr[period.pnum]
            ttAccountDiff.dGlHistAmt  = dGlHistAmt
            ttAccountDiff.dDiffAmt    = account.cyr[period.pnum] - dGlHistAmt.
             
    END.   /*IF  account.cyr[period.pnum] NE*/
END.  /* for each account-period  */ 

IF CAN-FIND(FIRST ttAccountDiff) THEN 
    RUN pOutputTable(TEMP-TABLE ttAccountDiff:HANDLE).


    
SESSION:SET-WAIT-STATE(""). 


/* **********************  Internal Procedures  *********************** */

PROCEDURE pOutputTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a temp-table, output a file to the user temp directory.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphdTempTable AS HANDLE NO-UNDO.
    DEFINE VARIABLE hdOutput AS HANDLE    NO-UNDO.
    
    DEFINE VARIABLE cTmpDir  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN system\outputProcs.p PERSISTENT SET hdOutput.
    
    RUN FileSys_GetTempDirectory(OUTPUT cTmpDir).
    
    RUN Output_TempTableToCSV IN hdOutput (iphdTempTable, cTmpDir + "\GLAccountVarianceList.csv", YES, YES, OUTPUT lError, OUTPUT cMessage).
    
    DELETE OBJECT hdOutput.
    
END PROCEDURE.
