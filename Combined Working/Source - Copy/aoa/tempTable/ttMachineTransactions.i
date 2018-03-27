/* ttMachineTransactions.i */

/* Machine Transactions.rpa */
DEFINE TEMP-TABLE ttMachineTransactions NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD machine         LIKE machtran.machine       LABEL "Machine"
    FIELD custPartNo      AS CHARACTER                LABEL "Cust Part"        FORMAT "x(15)"
    FIELD custName        AS CHARACTER                LABEL "Customer"         FORMAT "x(30)"
    FIELD jobNumber       LIKE machtran.job_number    LABEL "Job"
    FIELD jobSub          LIKE machtran.job_sub       LABEL "Sub"
    FIELD formNumber      LIKE machtran.form_number   LABEL "Form"
    FIELD blankNumber     LIKE machtran.blank_number  LABEL "Blank"
    FIELD passSequence    LIKE machtran.pass_sequence LABEL "Pass"
    FIELD chargeCode      AS CHARACTER                LABEL "Code"              FORMAT "x(5)"
    FIELD startDate       LIKE machtran.start_date
    FIELD startTime       AS CHARACTER                LABEL "Log In"            FORMAT "hh:mm am"
    FIELD endDate         LIKE machtran.end_date
    FIELD endTime         AS CHARACTER                LABEL "Log Out"           FORMAT "hh:mm am"
    FIELD shift           LIKE machtran.shift
    FIELD msf             AS DECIMAL                  LABEL "MSF"               FORMAT ">>>>>9.999"
    FIELD totalTime       AS DECIMAL                  LABEL "Total"             FORMAT "->>>,>>9.99"
    FIELD runQty          AS INTEGER                  LABEL "Run Qty"           FORMAT "->>>,>>9"
    FIELD wasteQty        AS INTEGER                  LABEL "Waste Qty"         FORMAT "->>>,>>9"
    FIELD runComplete     AS LOGICAL                  LABEL "Complete"
    FIELD xxRecKey        LIKE machtran.rec_key
    FIELD xxSort          AS CHARACTER                LABEL "Sort"              FORMAT "x(100)"
    FIELD xxTotalTime     AS INTEGER                  LABEL "TotTime"           FORMAT "99999"
    FIELD loginDateTime   AS CHARACTER                LABEL "Log In Date-Time"  FORMAT "x(19)"  
    FIELD logoutDateTime  AS CHARACTER                LABEL "Log Out Date-Time" FORMAT "x(19)" 
        INDEX sortBy IS PRIMARY rowType xxSort
        .
