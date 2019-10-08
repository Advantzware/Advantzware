/* ttMachineEmployeeTransactions.i */

/* Machine Transactions.rpa */
DEFINE TEMP-TABLE ttMachineEmployeeTransactions NO-UNDO
    FIELD employee       LIKE machemp.employee
    FIELD firstName      LIKE employee.first_name
    FIELD lastName       LIKE employee.last_name
    FIELD startDate      LIKE machemp.start_date
    FIELD startTime      AS CHARACTER LABEL "Start Time"        FORMAT "hh:mm am"
    FIELD endDate        LIKE machemp.end_date
    FIELD endTime        AS CHARACTER LABEL "End Time"          FORMAT "hh:mm am"
    FIELD totalTime      AS DECIMAL   LABEL "Total Time"
    FIELD shift          LIKE machemp.shift
    FIELD rateUsage      LIKE machemp.rate_usage
    FIELD rateType       LIKE machemp.ratetype
    FIELD rate           LIKE machemp.rate
    FIELD xxTableRecKey  LIKE machemp.table_rec_key
    FIELD loginDateTime  AS CHARACTER LABEL "Log In Date-Time"  FORMAT "x(19)"  
    FIELD logoutDateTime AS CHARACTER LABEL "Log Out Date-Time" FORMAT "x(19)"  
    .
