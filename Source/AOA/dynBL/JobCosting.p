/*------------------------------------------------------------------------
  File:         AOA/dynBL/JobCosting.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 7.07.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttJobCosting
DEFINE TEMP-TABLE ttJobCosting NO-UNDO
    FIELD job         AS CHARACTER FORMAT "x(8)" LABEL "Job#"
    FIELD i-no        AS CHARACTER FORMAT "x(15)" LABEL "FG Item#"
    FIELD est-no      AS CHARACTER FORMAT "x(8)" LABEL "Estimate#"
    FIELD ord-no      AS INTEGER   FORMAT ">>>>>>>9" LABEL "Order#"
    FIELD cust-no     AS CHARACTER FORMAT "x(8)" LABEL "Customer#"
    FIELD start-date  AS DATE      FORMAT "99/99/9999" LABEL "Start Date"
    FIELD close-date  AS DATE      FORMAT "99/99/9999" LABEL "Close Date"
    FIELD stat        AS CHARACTER FORMAT "x(10)" LABEL "Status"
    FIELD cust-part   AS CHARACTER FORMAT "x(15)" LABEL "Customer Part"
    FIELD job-qty     AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Job Qty"         
    FIELD ord-qty     AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Ord Qty"
    FIELD prod-qty    AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Prod.Qty"
    FIELD oh-qty      AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "On Hand Qty"
    FIELD ship-qty    AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Shipped Qty"
    FIELD inv-qty     AS INTEGER   FORMAT "->>,>>>,>>>,>>9" LABEL "Invoice Qty"
    FIELD wip-qty     AS INTEGER   FORMAT "->>,>>>,>>9" LABEL "WIP Qty"
    FIELD ou-pct      AS DECIMAL   FORMAT "->>,>>>,>>9.99" LABEL "O/U%"
    FIELD sales-rep   AS CHARACTER FORMAT "x(14)" LABEL "Cust Sales Rep"
    FIELD job-hold    AS CHARACTER FORMAT "x(40)" LABEL "Job Hold Reason"
    FIELD create-date AS DATE      FORMAT "99/99/9999" LABEL "Created Date"
    FIELD due-date    AS DATE      FORMAT "99/99/9999" LABEL "Due Date"    
    FIELD user-id     AS CHARACTER FORMAT "x(10)" LABEL "User ID"
    FIELD orderType   AS CHARACTER FORMAT "x(20)" LABEL "Job Type"
        
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 130
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:    
    DEFINE VARIABLE iWipQty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iProdQty    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShipQty    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-inv-qty  LIKE oe-ordl.inv-qty NO-UNDO.
    DEFINE VARIABLE li-ship-qty LIKE oe-ordl.ship-qty NO-UNDO.
    DEFINE VARIABLE iOhQty      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOuPct      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-closed    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-open      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cHoldReason AS CHARACTER NO-UNDO .

    FOR EACH job-hdr  NO-LOCK
        WHERE job-hdr.company EQ cCompany
        AND ((job-hdr.opened EQ YES AND iJobStatus EQ 1) OR (job-hdr.opened EQ NO AND iJobStatus EQ 2)) 
        AND job-hdr.cust-no GE cStartCustNo
        AND job-hdr.cust-no LE cEndCustNo
        AND job-hdr.i-no GE cStartFGItem
        AND job-hdr.i-no LE cEndFGItem 
        AND TRIM(job-hdr.est-no) GE trim(cStartEstimate) 
        AND TRIM(job-hdr.est-no) LE trim(cEndEstimate)
        AND job-hdr.job-no GE cStartJobNo
        AND job-hdr.job-no LE cEndJobNo 
        AND job-hdr.job-no2 GE iStartJobNo2
        AND job-hdr.job-no2 LE iEndJobNo2 , 
        EACH job OF job-hdr NO-LOCK BY job-hdr.job-no DESC
        BY job-hdr.job-no2 DESC:

        ASSIGN
            iProdQty = 0 
            iShipQty = 0
            iOhQty   = 0
            iOuPct   = 0
            iWipQty  = 0.
       
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no
            NO-ERROR.
        
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        
  
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.i-no EQ job-hdr.i-no
            AND oe-ordl.ord-no EQ job-hdr.ord-no NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
        DO:
            RUN oe/ordlsqty.p (ROWID(oe-ordl),
                OUTPUT li-inv-qty, OUTPUT li-ship-qty).
            iShipQty = li-ship-qty.
        END.
        IF AVAILABLE oe-ordl AND oe-ordl.job-no NE '' THEN
            FOR EACH fg-bin FIELDS(qty) NO-LOCK
                WHERE fg-bin.company EQ oe-ordl.company
                AND fg-bin.job-no EQ oe-ordl.job-no
                AND fg-bin.job-no2 EQ oe-ordl.job-no2
                AND fg-bin.i-no EQ oe-ordl.i-no:
                iOhQty = iOhQty + fg-bin.qty.
            END. /* each fg-bin */       
         
        RUN fg/GetProductionQty.p (INPUT job-hdr.company,
            INPUT job-hdr.job-no,
            INPUT job-hdr.job-no2,
            INPUT job-hdr.i-no,
            INPUT NO,
            OUTPUT iProdQty).            
  
        IF AVAILABLE oe-ordl THEN 
        DO:
            FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
            iWipQty = oe-ordl.qty - (iOhQty + oe-ordl.ship-qty).
            IF iWipQty LT 0 OR
                iWipQty LT oe-ordl.qty *
                (IF AVAIL oe-ord THEN oe-ordl.under-pct 
            ELSE 100) / 100 THEN
                iWipQty = 0.
        END. /* avail oe-ordl */

        IF AVAILABLE oe-ordl AND oe-ordl.qty NE 0 THEN 
        DO:
            iOuPct = ((iProdQty / oe-ordl.qty) - 1) * 100.
            IF iOuPct EQ 0 THEN iOuPct = 100.
            IF iOuPct EQ -100 THEN iOuPct = 0.
        END. /* avail oe-ordl */
        ASSIGN 
            cHoldReason = "" .
        FIND FIRST cust WHERE cust.company EQ cCompany 
            AND cust.cust-no EQ job-hdr.cust-no NO-LOCK NO-ERROR .
        IF job.stat = "H" THEN 
        DO: 
            FIND FIRST rejct-cd WHERE rejct-cd.type = "JH" 
                AND rejct-cd.code = job.reason NO-LOCK NO-ERROR.
            IF AVAIL rejct-cd THEN
                ASSIGN 
                    cHoldReason = rejct-cd.dscr.      
        END.
         
        CREATE ttJobCosting.
        ASSIGN
            ttJobCosting.i-no        = job-hdr.i-no
            ttJobCosting.est-no      = job-hdr.est-no
            ttJobCosting.ord-no      = job-hdr.ord-no
            ttJobCosting.cust-no     = job-hdr.cust-no
            ttJobCosting.stat        = job.stat
            ttJobCosting.orderType   = job.orderType
            ttJobCosting.cust-part   = IF AVAIL itemfg THEN STRING(itemfg.part-no) ELSE ""
            ttJobCosting.ord-qty     = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0 
            ttJobCosting.prod-qty    = iProdQty 
            ttJobCosting.oh-qty      = iOhQty 
            ttJobCosting.ship-qty    = iShipQty
            ttJobCosting.inv-qty     = IF AVAIL oe-ordl THEN oe-ordl.inv-qty ELSE 0
            ttJobCosting.job         = STRING(TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99"))
            ttJobCosting.wip-qty     = iWipQty
            ttJobCosting.ou-pct      = iOuPct
            ttJobCosting.job-qty     = job-hdr.qty 
            ttJobCosting.start-date  = job.start-date 
            ttJobCosting.close-date  = job.close-date 
            ttJobCosting.sales-rep   = IF AVAIL cust THEN STRING(cust.sman) ELSE ""
            ttJobCosting.job-hold    = STRING(cHoldReason)
            ttJobCosting.create-date = job.create-date 
            ttJobCosting.due-date    = job.due-date  . 
    
    END. /* each item */
END PROCEDURE.
