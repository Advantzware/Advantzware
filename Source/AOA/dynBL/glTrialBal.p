/*------------------------------------------------------------------------
  File:         glTrailBal.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 3.10.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttGLAccount
DEFINE TEMP-TABLE ttGLAccount NO-UNDO
    FIELD glAccount   AS CHARACTER LABEL "Account"      FORMAT "x(25)"
    FIELD acctDescr   AS CHARACTER LABEL "Description"  FORMAT "x(45)"
    FIELD openBalance AS DECIMAL   LABEL "Open Balance" FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period1     AS DECIMAL   LABEL "Period 1"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period2     AS DECIMAL   LABEL "Period 2"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period3     AS DECIMAL   LABEL "Period 3"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period4     AS DECIMAL   LABEL "Period 4"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period5     AS DECIMAL   LABEL "Period 5"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period6     AS DECIMAL   LABEL "Period 6"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period7     AS DECIMAL   LABEL "Period 7"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period8     AS DECIMAL   LABEL "Period 8"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period9     AS DECIMAL   LABEL "Period 9"     FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period10    AS DECIMAL   LABEL "Period 10"    FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period11    AS DECIMAL   LABEL "Period 11"    FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD period12    AS DECIMAL   LABEL "Period 12"    FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    FIELD endBalance  AS DECIMAL   LABEL "End Balance"  FORMAT "->>>,>>>,>>>,>>>,>>9.99"
    .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 167
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE dBalStart      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dEndBalance    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dPeriod        AS DECIMAL   NO-UNDO EXTENT 12.
    DEFINE VARIABLE dOpenBalance   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtYearStart    AS DATE      NO-UNDO.
    DEFINE VARIABLE iCount         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotal         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lIsCurrentYear AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bf-cur-period        FOR period.
    DEFINE BUFFER bf-first-period      FOR period.
    DEFINE BUFFER bf-first-open-period FOR period.
    
    FIND LAST bf-cur-period NO-LOCK
         WHERE bf-cur-period.company EQ cCompany
           AND bf-cur-period.pst     LE dtAsOfDate
           AND bf-cur-period.pend    GE dtAsOfDate
         NO-ERROR.
    
    FIND FIRST bf-first-period NO-LOCK      
         WHERE bf-first-period.company EQ bf-cur-period.company
           AND bf-first-period.yr      EQ bf-cur-period.yr
           AND bf-first-period.pnum    EQ 1
         NO-ERROR.
    
    FIND FIRST bf-first-open-period NO-LOCK
         WHERE bf-first-open-period.company EQ cCompany
           AND bf-first-open-period.pstat   EQ YES
         NO-ERROR.
    IF NOT AVAILABLE bf-first-period OR NOT AVAILABLE bf-cur-period THEN RETURN.
    
    lIsCurrentYear = bf-first-open-period.yr EQ bf-first-period.yr.
    
    FOR EACH account NO-LOCK
        WHERE account.company EQ cCompany
          AND account.actnum  GE cStartAccountNo
          AND account.actnum  LE cEndAccountNo
        :
        ASSIGN
            dPeriod = 0
            iTotal  = iTotal + 1
            .
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iTotal, ?).
        FOR EACH glhist NO-LOCK
            WHERE glhist.company EQ account.company
              AND glhist.actnum  EQ account.actnum
              AND glhist.tr-date GE bf-first-period.pst
              AND glhist.tr-date LE dtAsOfDate
              AND glhist.period  GT 0
            :
            dPeriod[glhist.period] = dPeriod[glhist.period] + glhist.tr-amt.
        END. /* each glhist */
        IF lIsCurrentYear THEN
        DO:
        ASSIGN
            dOpenBalance = account.cyr-open
            dEndBalance  = dOpenBalance
            .
        END.
        ELSE DO:
            RUN GL_GetAccountOpenBal (ROWID(account), dtAsOfDate, OUTPUT dEndBalance).
            dOpenBalance = dEndBalance.
        END. /* else */
        DO idx = 1 TO EXTENT(dPeriod):
            IF lIsCurrentYear THEN
            dEndBalance = dEndBalance + dPeriod[idx].
            ELSE
            dOpenBalance = dOpenBalance - dPeriod[idx].
        END. /* do idx */
        IF lExcludeZero AND
           dOpenBalance EQ 0 AND
           dEndBalance  EQ 0 AND
           dPeriod[1]   EQ 0 AND
           dPeriod[2]   EQ 0 AND
           dPeriod[3]   EQ 0 AND
           dPeriod[4]   EQ 0 AND
           dPeriod[5]   EQ 0 AND
           dPeriod[6]   EQ 0 AND
           dPeriod[7]   EQ 0 AND
           dPeriod[8]   EQ 0 AND
           dPeriod[9]   EQ 0 AND
           dPeriod[10]  EQ 0 AND
           dPeriod[11]  EQ 0 AND
           dPeriod[12]  EQ 0 THEN
        NEXT.
        CREATE ttGLAccount.
        ASSIGN
            ttGLAccount.glAccount   = account.actnum
            ttGLAccount.acctDescr   = account.dscr
            ttGLAccount.openBalance = dOpenBalance 
            ttGLAccount.period1     = dPeriod[1]
            ttGLAccount.period2     = dPeriod[2]
            ttGLAccount.period3     = dPeriod[3]
            ttGLAccount.period4     = dPeriod[4]
            ttGLAccount.period5     = dPeriod[5]
            ttGLAccount.period6     = dPeriod[6]
            ttGLAccount.period7     = dPeriod[7]
            ttGLAccount.period8     = dPeriod[8]
            ttGLAccount.period9     = dPeriod[9]
            ttGLAccount.period10    = dPeriod[10]
            ttGLAccount.period11    = dPeriod[11]
            ttGLAccount.period12    = dPeriod[12]
            ttGLAccount.endBalance  = dEndBalance
            .
    END. /* each account */

END PROCEDURE.
