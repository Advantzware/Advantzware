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
    FIELD creditHoldStatus   AS CHARACTER FORMAT "x(80)"             LABEL "Hold Status"
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
    DEFINE VARIABLE dbalanceDue              AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCreditHoldStatus        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lbalUpdated              AS LOGICAL   NO-UNDO.

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
        
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany          
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
       
        ASSIGN             
            dbalanceDue  = dDue - DYNAMIC-FUNCTION("Credit_fAmountPaidOnAccount" IN hCreditProcs, cust.company, cust.cust-no)            
            .
        cCreditHoldStatus = "".
        lbalUpdated  = FALSE. 
        IF rInvoiceRecID NE ? THEN
         cCreditHoldStatus = "Past Due Invoice,".
        IF  dOrderBalance +  dbalanceDue GT  cust.cr-lim THEN
         cCreditHoldStatus =  cCreditHoldStatus + "Over Credit Limit,".
        IF  dOrderBalance GT  cust.cr-lim THEN
         cCreditHoldStatus =  cCreditHoldStatus + "Over Order Limit,".
        IF cust.cr-hold THEN
         cCreditHoldStatus =  cCreditHoldStatus + "Already On Hold".
         cCreditHoldStatus = TRIM( cCreditHoldStatus,",").
        IF cust.acc-bal            NE dbalanceDue         OR
           cust.ord-bal            NE dOrderBalance       OR
           cust.balanceCurrent     NE dBalanceCurrent     OR 
           cust.balanceWithinGrace NE dBalanceWithinGrace OR
           cust.balancePastDue     NE dBalancePastDue     OR
          (cust.cr-hold EQ NO     AND cCreditHoldStatus   NE "") THEN
        DO TRANSACTION:
            /* try 5 times to lock cust record */
            DO iLockLoop = 1 TO 5:
                FIND CURRENT cust EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAILABLE cust THEN DO:
                    ASSIGN
                        cust.acc-bal            =  dbalanceDue
                        cust.ord-bal            =  dOrderBalance
                        cust.balanceCurrent     =  dBalanceCurrent
                        cust.balanceWithinGrace =  dBalanceWithinGrace
                        cust.balancePastDue     =  dBalancePastDue
                        cust.date-field[3]      = TODAY
                        lbalUpdated             = YES
                        .
                    IF iOECredit    NE 1  AND
                       cust.cr-hold EQ NO AND
                        cCreditHoldStatus NE "" THEN DO:
                        cust.cr-hold = YES.
                        RUN ClearTagsHold (cust.rec_key).
                        DO idx = 1 TO NUM-ENTRIES( cCreditHoldStatus ):
                            RUN AddTagHold (
                                cust.rec_key,
                                "cust",
                                ENTRY(idx, cCreditHoldStatus)
                                ).
                        END. /* do idx */
                    END. /* if setting cr-hold */
                    ELSE
                    IF cust.cr-hold AND lARAutoReleaseCreditHold AND
                        cCreditHoldStatus EQ "" THEN DO:
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
        ELSE lbalUpdated = YES.
        
       IF cust.cust-no GE cStartCustNo
        AND cust.cust-no LE cEndCustNo
        AND ((dbalanceDue GT 0 AND lOrderBalance) 
        OR (dbalanceDue GT 0 AND lARBalance) 
        OR (dBalanceWithinGrace GT 0 AND lPastGraceBalance) 
        OR (cust.cr-hold EQ YES AND lCreditHold) 
        OR (lbalUpdated EQ NO AND lCustomerNotAged) ) THEN
        DO:
            CREATE ttCustAging.
        ASSIGN
            ttCustAging.custNo             = cust.cust-no
            ttCustAging.custName           = cust.NAME
            ttCustAging.terms              = cust.terms
            ttCustAging.salesPerson        = cust.sman
            ttCustAging.creditHold         = cust.cr-hold
            ttCustAging.creditLimit        = cust.cr-lim
            ttCustAging.openOrder          = cust.ord-lim
            ttCustAging.balanceDue         = dbalanceDue
            ttCustAging.openOrderBalance   = dOrderBalance
            ttCustAging.balanceCurrent     = dBalanceCurrent
            ttCustAging.balanceWithinGrace = dBalanceWithinGrace
            ttCustAging.balancePastDue     = dBalancePastDue
            ttCustAging.creditAvailable    = cust.cr-lim - dOrderBalance - dbalanceDue 
            ttCustAging.creditHoldStatus   = REPLACE(cCreditHoldStatus,","," | ")
            ttCustAging.balUpdated         = lbalUpdated.          
        
        END.         
        
    END. /* each cust */     

    IF VALID-HANDLE(hCreditProcs) THEN
    DELETE PROCEDURE hCreditProcs.

END PROCEDURE.
