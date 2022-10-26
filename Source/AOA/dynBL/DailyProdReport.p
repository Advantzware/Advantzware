/*------------------------------------------------------------------------
  File:         AOA/dynBL/DailyProdReport.p
  Description:  Business Logic
  Author:       Sachin Chahal
  Date Created: 5.04.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttDailyProdReport
DEFINE TEMP-TABLE ttDailyProdReport NO-UNDO
    FIELD cJob        AS CHARACTER FORMAT "x(13)"           LABEL "Job#"
    FIELD cCustNo     AS CHARACTER FORMAT "x(8)"            LABEL "Customer#"
    FIELD cCustName   AS CHARACTER FORMAT "x(30)"           LABEL "Customer Name"
    FIELD cItemNo     AS CHARACTER FORMAT "x(15)"           LABEL "Item#"
    FIELD cItemName   AS CHARACTER FORMAT "x(30)"           LABEL "Item Description"
    FIELD cCSRCode    AS CHARACTER FORMAT "x(8)"            LABEL "CSR Code"
    FIELD cCSRName    AS CHARACTER FORMAT "x(30)"           LABEL "CSR Name"
    FIELD cStat       AS CHARACTER FORMAT "x(10)"           LABEL "Status"
    FIELD iPlannedQty AS INTEGER   FORMAT "->>>,>>>,>>9"    LABEL "Planned Qty"         
    FIELD iActQty     AS INTEGER   FORMAT "->>>,>>>,>>9"    LABEL "Actual Qty"
    FIELD dOverUnderPercentage      AS DECIMAL   FORMAT "->>,>>>,>>9.99"  LABEL "O/U%"
    FIELD cMachine    AS CHARACTER FORMAT "x(6)"            LABEL "Machine"
    FIELD dtOpDate    AS DATE      FORMAT "99/99/9999"      LABEL "Operation Date"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 203
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:    
    DEFINE VARIABLE iQty          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOuPct        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    
    FOR EACH job-hdr  NO-LOCK
        WHERE job-hdr.company    EQ cCompany
        AND ((job-hdr.opened     EQ YES AND cJobStatus EQ "1")
         OR (job-hdr.opened      EQ NO  AND cJobStatus EQ "2")) 
        AND job-hdr.job-no       GE cStartJobNo 
        AND job-hdr.job-no       LE cEndJobNo 
        AND job-hdr.job-no2      GE iStartJobNo2
        AND job-hdr.job-no2      LE iEndJobNo2, 
        EACH job OF job-hdr NO-LOCK,
        FIRST mch-act NO-LOCK
        WHERE mch-act.company EQ job.company
        AND mch-act.job-no    EQ job.job-no
        AND mch-act.job-no2   EQ job.job-no2
        AND mch-act.m-code    GE cStartMachine
        AND mch-act.m-code    LE cEndMachine
        AND mch-act.op-date   GE dtStartMachineRunDate
        AND mch-act.op-date   LE dtEndMachineRunDate
        BY job-hdr.job-no  DESCENDING
        BY job-hdr.job-no2 DESCENDING
        :
        ASSIGN
            iQty     = 0
            iOuPct   = 0
            .
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ job-hdr.company
               AND itemfg.i-no    EQ job-hdr.i-no
             NO-ERROR.
        
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company EQ job.company
               AND job-mch.job     EQ job.job
               AND job-mch.job-no  EQ job.job-no
               AND job-mch.job-no2 EQ job.job-no2
               AND job-mch.m-code  EQ mch-act.m-code
             NO-ERROR.
        
        IF AVAIL job-mch AND job-mch.run-qty NE 0 THEN 
        DO:
            iQty = (mch-act.qty - job-mch.run-qty) .
            iOuPct =  (iQty / job-mch.run-qty) * 100.
            IF iOuPct EQ 0 THEN iOuPct = 100.
            IF iOuPct EQ -100 THEN iOuPct = 0.
        END.
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ cCompany 
               AND cust.cust-no EQ job-hdr.cust-no
             NO-ERROR.
            IF AVAIL cust AND cust.csrUser_id GT "" THEN 
            FIND FIRST users NO-LOCK 
                WHERE users.user_id EQ cust.csrUser_id
                  AND users.email GT ""
                  NO-ERROR.
                
        CREATE ttDailyProdReport.
        ASSIGN
            ttDailyProdReport.cJob         = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job-hdr.job-no, job-hdr.job-no2)))
            ttDailyProdReport.cCustNo     = job-hdr.cust-no
            ttDailyProdReport.cCustName   = IF AVAIL cust THEN cust.NAME ELSE ""
            ttDailyProdReport.cItemNo     = job-hdr.i-no
            ttDailyProdReport.cItemName   = IF AVAIL itemfg THEN itemfg.i-name ELSE ""
            ttDailyProdReport.cCSRCode    = IF AVAIL cust THEN cust.csrUser_id ELSE ""
            ttDailyProdReport.cCSRName    = IF AVAIL users THEN users.user_name ELSE ""
            ttDailyProdReport.cStat       = IF job-hdr.opened THEN "Opened" ELSE "Closed"
            
            ttDailyProdReport.iPlannedQty = IF AVAIL job-mch THEN job-mch.run-qty ELSE 0 
            ttDailyProdReport.iActQty     = mch-act.qty
            ttDailyProdReport.dOverUnderPercentage      = iOuPct
            
            ttDailyProdReport.cMachine    = mch-act.m-code
            ttDailyProdReport.dtOpDate    = mch-act.op-date 
            iCount = iCount + 1
            .
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
    END. /* each job-hdr */
END PROCEDURE.
