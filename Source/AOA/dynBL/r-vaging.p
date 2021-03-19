/*------------------------------------------------------------------------
  File:         r-vaging.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 3.9.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttVendorAging
DEFINE TEMP-TABLE ttVendorAging NO-UNDO
    FIELD currency   AS CHARACTER LABEL "Currency"
    FIELD vendor     AS CHARACTER LABEL "Vendor"   FORMAT "x(10)"
    FIELD vendorName AS CHARACTER LABEL "Name"     FORMAT "x(30)"
    FIELD phone      AS CHARACTER LABEL "Phone"    FORMAT "(999) 999-9999"
    FIELD vendType   AS CHARACTER LABEL "Type"
    FIELD invTerm    AS CHARACTER LABEL "Term"     FORMAT "x(17)"
    FIELD invoice    AS CHARACTER LABEL "Invoice"  FORMAT "x(12)"
    FIELD invDate    AS DATE      LABEL "Date"     FORMAT "99/99/9999"
    FIELD amount     AS DECIMAL   LABEL "Amount"   FORMAT "->>>,>>>,>>9.99"
    FIELD agedDays   AS INTEGER   LABEL "Day"
    FIELD dueDate    AS DATE      LABEL "Due Date" FORMAT "99/99/9999"
    FIELD period1    AS DECIMAL   LABEL "Period 1" FORMAT "->>>,>>>,>>9.99"
    FIELD period2    AS DECIMAL   LABEL "Period 2" FORMAT "->>>,>>>,>>9.99"
    FIELD period3    AS DECIMAL   LABEL "Period 3" FORMAT "->>>,>>>,>>9.99"
    FIELD period4    AS DECIMAL   LABEL "Period 4" FORMAT "->>>,>>>,>>9.99"
    FIELD period5    AS DECIMAL   LABEL "Period 5" FORMAT "->>>,>>>,>>9.99"
    FIELD payables   AS DECIMAL   LABEL "Payables" FORMAT "->>>,>>>,>>9.99"
    .
DEFINE TEMP-TABLE ttVendor NO-UNDO
    FIELD currency AS CHARACTER
    FIELD vendor   AS CHARACTER 
    FIELD rRowID   AS ROWID
        INDEX ttVendor IS PRIMARY
            currency
            vendor
            .
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 165
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cCompany     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPaymentList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRefNum      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTerms       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtCheckDate  AS DATE      NO-UNDO.
    DEFINE VARIABLE dtDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE dAmount      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE days         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotal       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE periodAmt    AS DECIMAL   NO-UNDO EXTENT 5.

    DEFINE BUFFER xap-ledger FOR ap-ledger.

    EMPTY TEMP-TABLE ttVendor.
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
    IF lElectronic AND lNonElectronic THEN 
    cPaymentList = "".
    ELSE
    IF lElectronic THEN 
    FOR EACH payment-type NO-LOCK
        WHERE payment-type.company    EQ cCompany 
          AND payment-type.paperCheck EQ NO
        :
        cPaymentList = cPaymentList + payment-type.type + ",".                            
    END. /* each payment-type */
    ELSE IF lNonElectronic THEN 
    FOR EACH payment-type NO-LOCK
        WHERE payment-type.company    EQ cCompany 
          AND payment-type.paperCheck EQ YES
        : 
        cPaymentList = cPaymentList + payment-type.type + ",".                            
    END. /* each payment-type */
    cPaymentList = TRIM(cPaymentList,",").
    FOR EACH company NO-LOCK
        WHERE company.company GE cStartCompany
          AND company.company LE cEndCompany,
         EACH vend NO-LOCK
        WHERE vend.company EQ company.company
          AND vend.vend-no GE cStartVendNo
          AND vend.vend-no LE cEndVendNo
          AND vend.type    GE cStartVendorType
          AND vend.type    LE cEndVendorType
          AND ((vend.curr-code  GE cStartCurrency
          AND   vend.curr-code  LE cEndCurrency)
           OR  (vend.curr-code  EQ ""
          AND company.curr-code GE cStartCurrency
          AND company.curr-code LE cEndCurrency))
          AND (LOOKUP(vend.payment-type,cPaymentList) NE 0
           OR cPaymentList EQ "")
        :
        FOR EACH ap-inv NO-LOCK
            WHERE ap-inv.company   EQ company.company
              AND ap-inv.vend-no   EQ vend.vend-no
              AND ap-inv.posted    EQ YES
              AND (ap-inv.inv-date LE dtAsOfDate
               OR cWhichDate       EQ "Posting")
            USE-INDEX ap-inv,
            FIRST ap-ledger NO-LOCK
            WHERE ap-ledger.company  EQ company.company
              AND ap-ledger.vend-no  EQ ap-inv.vend-no
              AND ap-ledger.ref-date EQ ap-inv.inv-date
              AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
              AND (ap-ledger.tr-date LE dtAsOfDate
               OR cWhichDate         EQ "Invoice")
            USE-INDEX ap-ledger
            :
            CREATE ttVendor.
            ASSIGN
                ttVendor.currency = IF vend.curr-code EQ "" THEN company.curr-code
                                    ELSE vend.curr-code
                ttVendor.vendor   = vend.vend-no
                ttVendor.rRowID   = ROWID(vend)
                iTotal            = iTotal + 1
                .
            IF lProgressBar THEN
            RUN spProgressBar (cProgressBar, iTotal, ?).
            LEAVE.
        END.
    END. /* each company */

    FOR EACH ttVendor,
        FIRST vend NO-LOCK
        WHERE ROWID(vend) EQ ttVendor.rRowID 
        BREAK BY ttVendor.currency
              BY ttVendor.vendor
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, iTotal).
        FOR EACH ap-inv NO-LOCK
            WHERE ap-inv.company   EQ vend.company
              AND ap-inv.vend-no   EQ vend.vend-no
              AND ap-inv.posted    EQ YES 
              AND (ap-inv.inv-date LE dtAsOfDate
               OR cWhichDate       EQ "Posting")
            USE-INDEX ap-inv,    
            FIRST ap-ledger NO-LOCK 
            WHERE ap-ledger.company  EQ vend.company
              AND ap-ledger.vend-no  EQ ap-inv.vend-no
              AND ap-ledger.ref-date EQ ap-inv.inv-date
              AND ap-ledger.refnum   EQ ("INV# " + ap-inv.inv-no)
              AND (ap-ledger.tr-date LE dtAsOfDate
               OR cWhichDate         EQ "Invoice")
            USE-INDEX ap-ledger    
            BREAK BY ap-inv.vend-no
                  BY (IF cWhichDate EQ "Invoice" THEN ap-inv.inv-date ELSE ap-ledger.tr-date)
                  BY ap-inv.inv-no
            :  
            ASSIGN
                dAmount = 0
                dtDate  = IF cWhichDate EQ "Invoice" THEN ap-inv.inv-date
                          ELSE ap-ledger.tr-date
                .
            FOR EACH ap-payl NO-LOCK
                WHERE ap-payl.inv-no   EQ ap-inv.inv-no
                  AND ap-payl.vend-no  EQ ap-inv.vend-no
                  AND ap-payl.posted   EQ YES 
                  AND ap-payl.due-date EQ ap-inv.due-date
                USE-INDEX inv-no
                :
                FIND FIRST ap-pay NO-LOCK USE-INDEX c-no
                     WHERE ap-pay.company EQ vend.company
                       AND ap-pay.c-no    EQ ap-payl.c-no
                     NO-ERROR.
                IF AVAILABLE ap-pay THEN DO:
                    dtCheckDate = ap-pay.check-date.
                    /*check for voided check transaction date*/
                    IF ap-payl.amt-paid LT 0  AND
                        ap-payl.memo                EQ NO AND
                        ap-inv.net + ap-inv.freight GT 0 THEN DO:
                        cRefNum = "VOIDED CHECK" + STRING(ap-pay.check-no, "zzzzzzz9").
                        FIND FIRST xap-ledger NO-LOCK
                             WHERE xap-ledger.company EQ vend.company
                               AND xap-ledger.vend-no EQ ap-pay.vend-no
                               AND xap-ledger.refnum  EQ cRefNum
                             NO-ERROR.
                        IF AVAILABLE xap-ledger THEN DO:
                            dtCheckDate = xap-ledger.tr-date.
                            RELEASE xap-ledger.
                        END.
                    END.
                    IF dtCheckDate LE dtAsOfDate THEN DO:
                        IF ap-payl.amt-paid NE 0 THEN dAmount = dAmount - ap-payl.amt-paid.
                        IF ap-payl.amt-disc NE 0 THEN DO:
                            IF NOT ap-payl.memo THEN dAmount = dAmount - ap-payl.amt-disc.
                            IF ap-payl.memo THEN dAmount = dAmount + ap-payl.amt-disc.
                        END.
                    END.
                END.
                RELEASE ap-pay.    
            END. /* for each ap-payl */
            ASSIGN
                days    = dtAsOfDate - dtDate
                dAmount = dAmount + ap-inv.net + ap-inv.freight
                cTerms  = "" 
                .
            /* IF days old less then 0 make equal to 0 */
            IF days LT 0 THEN
            days = 0.
            FIND FIRST terms NO-LOCK
                 WHERE terms.company EQ vend.company
                   AND terms.t-code  EQ vend.terms
                 NO-ERROR.
            IF AVAILABLE terms THEN
            cTerms = terms.dscr.
            ASSIGN 
                periodAmt = 0
                idx       = (IF days LE iPeriodDays1 THEN 1 
                        ELSE IF days LE iPeriodDays2 THEN 2
                        ELSE IF days LE iPeriodDays3 THEN 3
                        ELSE IF days LE iPeriodDays4 THEN 4
                        ELSE 5)
                periodAmt[idx] = dAmount
                .
            IF dAmount EQ 0 THEN NEXT.
            CREATE ttVendorAging.
            ASSIGN
                ttVendorAging.currency   = ttVendor.currency
                ttVendorAging.vendor     = vend.vend-no
                ttVendorAging.vendorName = vend.name
                ttVendorAging.phone      = IF FIRST(ap-inv.vend-no) THEN vend.area-code + vend.phone
                                           ELSE vend.cont
                ttVendorAging.vendType   = vend.type
                ttVendorAging.invTerm    = cTerms
                ttVendorAging.invoice    = ap-inv.inv-no
                ttVendorAging.invDate    = dtDate
                ttVendorAging.amount     = dAmount
                ttVendorAging.agedDays   = days
                ttVendorAging.dueDate    = ap-inv.due-date
                ttVendorAging.period1    = periodAmt[1]
                ttVendorAging.period2    = periodAmt[2]
                ttVendorAging.period3    = periodAmt[3]
                ttVendorAging.period4    = periodAmt[4]
                ttVendorAging.period5    = periodAmt[5]
                ttVendorAging.payables   = ttVendorAging.period1
                                         + ttVendorAging.period2
                                         + ttVendorAging.period3
                                         + ttVendorAging.period4
                                         + ttVendorAging.period5
                .
        END. /* each ap-inv */
    END. /* each ttvendor */

END PROCEDURE.
