/*------------------------------------------------------------------------
  File:         CustAging.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 8.4.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttCustAging
DEFINE TEMP-TABLE ttCustAging NO-UNDO
    FIELD custNo             AS CHARACTER FORMAT "x(10)"             LABEL "Customer"
    FIELD custName           AS CHARACTER FORMAT "x(30)"             LABEL "Name"
    FIELD terms              AS CHARACTER FORMAT "x(5)"              LABEL "Terms"
    FIELD salesPerson        AS CHARACTER FORMAT "x(3)"              LABEL "Salesperson"
    FIELD creditHold         AS LOGICAL                              LABEL "Hold"
    FIELD creditLimit        AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Credit Limit"
    FIELD openOrder          AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Open Order Limit"
    FIELD balanceDue         AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Balance Due"
    FIELD openOrderBalance   AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Open Order Balance"
    FIELD balanceCurrent     AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Current Balance"
    FIELD balanceWithinGrace AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Grace Balance"
    FIELD balancePastDue     AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Past Due Balance"
    FIELD creditAvailable    AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Credit Available"
    FIELD balUpdated         AS LOGICAL                              LABEL "Updated"
    FIELD creditHoldStatus   AS CHARACTER FORMAT "x(70)"             LABEL "Hold Status"
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 137
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cARAutoReleaseCreditHold AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOECredit                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dBalanceCurrent          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBalancePastDue          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dBalanceWithinGrace      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDue                     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOrderBalance            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hCreditProcs             AS HANDLE    NO-UNDO.
    DEFINE VARIABLE idx                      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLockLoop                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOECredit                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lARAutoReleaseCreditHold AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFound                   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lOECredit                AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rInvoiceRecID            AS RECID     NO-UNDO.

    RUN system/CreditProcs.p PERSISTENT SET hCreditProcs.

    RUN sys/ref/nk1look.p (
        cCompany,"ARAutoReleaseCreditHold","L",NO,NO,"","",
        OUTPUT cARAutoReleaseCreditHold, OUTPUT lFound
        ).
    lARAutoReleaseCreditHold = lFound AND cARAutoReleaseCreditHold EQ "YES".
    RUN sys/ref/nk1look.p (
        cCompany,"OECREDIT","L",NO,NO,"","",
        OUTPUT cOECredit, OUTPUT lFound
        ).
    IF lFound THEN DO:
        lOECredit = cOECredit EQ "YES".
        IF lOECredit THEN DO:
            RUN sys/ref/nk1look.p (
                cCompany,"OECREDIT","I",NO,NO,"","",
                OUTPUT cOECredit, OUTPUT lFound
                ).
            iOECredit = INTEGER(cOECredit).
            RUN sys/ref/nk1look.p (
                cCompany,"OECREDIT","C",NO,NO,"","",
                OUTPUT cOECredit, OUTPUT lFound
                ).
        END. /* if loecredit */
        DO TRANSACTION:
            FIND FIRST sys-ctrl EXCLUSIVE-LOCK
                 WHERE sys-ctrl.company EQ cCompany
                   AND sys-ctrl.name    EQ "OECREDIT"
                 NO-ERROR.
            IF AVAILABLE sys-ctrl THEN
            sys-ctrl.date-fld = TODAY.
            RELEASE sys-ctrl.
        END. /* do trans */
    END. /* if lfound */
    
    MAIN-LOOP:
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo 
          AND (( cust.acc-bal GT 0 AND cIncludeCustomer EQ "2" ) OR cIncludeCustomer NE "2")
          AND ((cust.cr-hold AND cIncludeCustomer EQ "3" ) OR cIncludeCustomer NE "3")
          AND ((cust.balanceWithinGrace GT 0 AND cIncludeCustomer EQ "4") OR cIncludeCustomer NE "4") 
        :           
        
        IF cust.cr-hold-invdays GT 0 THEN
        RUN oe/creditid.p (RECID(cust), OUTPUT rInvoiceRecID).
        RUN ar/updcust1.p (YES, BUFFER cust, OUTPUT dOrderBalance).
        RUN Credit_CustBalances IN hCreditProcs (
            BUFFER cust,
            OUTPUT dDue,
            OUTPUT dBalanceCurrent,
            OUTPUT dBalanceWithinGrace,
            OUTPUT dBalancePastDue
            ).
            
        IF cIncludeCustomer EQ "1" AND dBalanceCurrent LE 0 THEN
        NEXT MAIN-LOOP.
        
        CREATE ttCustAging.
        ASSIGN
            ttCustAging.custNo             = cust.cust-no
            ttCustAging.custName           = cust.name
            ttCustAging.terms              = cust.terms
            ttCustAging.salesPerson        = cust.sman
            ttCustAging.creditHold         = cust.cr-hold
            ttCustAging.creditLimit        = cust.cr-lim
            ttCustAging.openOrder          = cust.ord-lim
            ttCustAging.balanceDue         = dDue - DYNAMIC-FUNCTION("Credit_fAmountPaid" IN hCreditProcs, cust.company, cust.cust-no)
            ttCustAging.openOrderBalance   = dOrderBalance
            ttCustAging.balanceCurrent     = dBalanceCurrent
            ttCustAging.balanceWithinGrace = dBalanceWithinGrace
            ttCustAging.balancePastDue     = dBalancePastDue
            ttCustAging.creditAvailable    = ttCustAging.creditLimit - ttCustAging.openOrderBalance - ttCustAging.balanceCurrent   /*ttCustAging.balanceDue*/ 
            .
        IF rInvoiceRecID NE ? THEN
        ttCustAging.creditHoldStatus = "Past Due Invoice |".
        IF ttCustAging.openOrderBalance + ttCustAging.balanceDue GT ttCustAging.creditLimit THEN
        ttCustAging.creditHoldStatus = ttCustAging.creditHoldStatus + " Over Credit Limit |".
        IF ttCustAging.openOrderBalance GT ttCustAging.creditLimit THEN
        ttCustAging.creditHoldStatus = ttCustAging.creditHoldStatus + " Over Order Limit |".
        IF cust.cr-hold THEN
        ttCustAging.creditHoldStatus = ttCustAging.creditHoldStatus + " Already On Hold".
        ttCustAging.creditHoldStatus = TRIM(ttCustAging.creditHoldStatus,",").
        IF cust.acc-bal            NE ttCustAging.balanceDue         OR
           cust.ord-bal            NE ttCustAging.openOrderBalance   OR
           cust.balanceCurrent     NE ttCustAging.balanceCurrent     OR 
           cust.balanceWithinGrace NE ttCustAging.balanceWithinGrace OR
           cust.balancePastDue     NE ttCustAging.balancePastDue     OR
          (cust.cr-hold EQ NO     AND ttCustAging.creditHoldStatus   NE "") THEN
        DO TRANSACTION:
            /* try 5 times to lock cust record */
            DO iLockLoop = 1 TO 5:
                FIND CURRENT cust EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAILABLE cust THEN DO:
                    ASSIGN
                        cust.acc-bal            = ttCustAging.balanceDue
                        cust.ord-bal            = ttCustAging.openOrderBalance
                        cust.balanceCurrent     = ttCustAging.balanceCurrent 
                        cust.balanceWithinGrace = ttCustAging.balanceWithinGrace
                        cust.balancePastDue     = ttCustAging.balancePastDue
                        cust.date-field[3]      = TODAY
                        ttCustAging.balUpdated  = YES
                        .
                    IF iOECredit    NE 1  AND
                       cust.cr-hold EQ NO AND
                       ttCustAging.creditHoldStatus NE "" THEN DO:
                        cust.cr-hold = YES.
                        RUN ClearTagsHold (cust.rec_key).
                        DO idx = 1 TO NUM-ENTRIES(ttCustAging.creditHoldStatus):
                            RUN AddTagHold (
                                cust.rec_key,
                                "cust",
                                ENTRY(idx,ttCustAging.creditHoldStatus)
                                ).
                        END. /* do idx */
                    END. /* if setting cr-hold */
                    ELSE
                    IF cust.cr-hold AND lARAutoReleaseCreditHold AND
                       ttCustAging.creditHoldStatus EQ "" THEN DO:
                        cust.cr-hold = NO.
                        RUN ClearTagsHold (cust.rec_key).
                    END.
                END. /* if avail */
                ELSE DO:
                    /* cust record not avail, wait 1 second, try again */
                    PAUSE 1 NO-MESSAGE.
                    NEXT.
                END. /* else */
                FIND CURRENT cust NO-LOCK.
                LEAVE.
            END. /* do ilockloop */
        END. /* do trans */
        ELSE ttCustAging.balUpdated = YES.
    END. /* each cust */

    IF VALID-HANDLE(hCreditProcs) THEN
    DELETE PROCEDURE hCreditProcs.

END PROCEDURE.
