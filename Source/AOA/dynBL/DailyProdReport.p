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
    FIELD iJobQty     AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Job Qty"         
    FIELD iOrdQty     AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Ord Qty"
    FIELD iProdQty    AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Prod.Qty"
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
    DEFINE VARIABLE iProdQty      AS INTEGER   NO-UNDO.
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
            iProdQty = 0
            iOuPct   = 0
            .
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ job-hdr.company
               AND itemfg.i-no    EQ job-hdr.i-no
             NO-ERROR.
                
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.i-no    EQ job-hdr.i-no
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
             NO-ERROR.          
        RUN fg/GetProductionQty.p (
            job-hdr.company,
            job-hdr.job-no,
            job-hdr.job-no2,
            job-hdr.i-no,
            NO,
            OUTPUT iProdQty
            ).  
        IF job-hdr.qty NE 0 THEN 
        DO:
            iOuPct = (job-hdr.qty / iProdQty) * 100.
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
            
            ttDailyProdReport.iOrdQty     = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0 
            ttDailyProdReport.iProdQty    = iProdQty
            ttDailyProdReport.dOverUnderPercentage      = iOuPct
            ttDailyProdReport.iJobQty     = job-hdr.qty
            
            ttDailyProdReport.cMachine    = mch-act.m-code
            ttDailyProdReport.dtOpDate    = mch-act.op-date 
            iCount = iCount + 1
            .
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
    END. /* each job-hdr */
END PROCEDURE.
