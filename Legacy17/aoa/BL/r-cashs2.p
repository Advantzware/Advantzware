/*------------------------------------------------------------------------
  File: r-cashs2.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Cash Receipt By SalesRep Name.rpa */
{aoa/tempTable/ttCashReceiptBySalesRepName.i}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no   AS INTEGER
    FIELD chk-inv  AS LOGICAL INITIAL YES
    FIELD q-onh    LIKE itemfg.q-onh
    FIELD q-shp    LIKE itemfg.q-onh
    FIELD q-rel    LIKE itemfg.q-onh
    FIELD q-wip    LIKE itemfg.q-onh
    FIELD q-avl    LIKE itemfg.q-onh
    FIELD po-no    LIKE oe-ord.po-no
    FIELD inv      AS   LOGICAL
    FIELD cad-no   LIKE itemfg.cad-no
    FIELD row-id   AS ROWID 
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    .

DEFINE TEMP-TABLE tt-report-inv NO-UNDO LIKE report 
    FIELD inv-no AS INT.

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCashReceiptBySalesRepName.
{aoa/includes/pCashReceiptBySalesRepName.i}

/* local variables */
DEFINE VARIABLE cSortKey    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSMan       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dAmt      LIKE ar-cashl.amt-paid NO-UNDO EXTENT 2.
DEFINE VARIABLE dPaid     LIKE dAmt      NO-UNDO.
DEFINE VARIABLE dDsc      LIKE dAmt      NO-UNDO.
DEFINE VARIABLE dCom      LIKE ar-cashl.amt-paid NO-UNDO.
DEFINE VARIABLE dCom-2    LIKE dCom      NO-UNDO.
DEFINE VARIABLE dComm%      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dPct        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dMisc       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cBasis    LIKE sman.commbasis NO-UNDO INITIAL "".
DEFINE VARIABLE cTerm     LIKE tt-report.term-id NO-UNDO.
DEFINE VARIABLE dTotAmt     AS DECIMAL   NO-UNDO EXTENT 4.
DEFINE VARIABLE dTotCom   LIKE dTotAmt   NO-UNDO.
DEFINE VARIABLE iAging      AS INTEGER   NO-UNDO.
DEFINE VARIABLE dtCheckDate AS DATE      NO-UNDO.
DEFINE VARIABLE dtInvDate   AS DATE      NO-UNDO.
DEFINE VARIABLE dAmt-full   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dInvFull    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dRemBal     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dTmpAmt1    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lInvFound   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE j           AS INTEGER   NO-UNDO.

/* subject business logic */
FOR EACH cust NO-LOCK
    WHERE cust.company EQ ipcCompany
    :
    IF iDayOld EQ 0 THEN
    FOR EACH ar-inv NO-LOCK
        WHERE ar-inv.company  EQ ipcCompany
          AND ar-inv.posted   EQ YES
          AND ar-inv.cust-no  EQ cust.cust-no
          AND ar-inv.inv-date GE dtStartReceiptDate
          AND ar-inv.inv-date LE dtEndReceiptDate
          AND ar-inv.terms    EQ "CASH",
        EACH ar-invl NO-LOCK
        WHERE ar-invl.x-no EQ ar-inv.x-no:
        DO i = 1 TO 3:
            cSMan = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                     ELSE ar-invl.sman[i].
            IF cSMan LT cStartSalesRep OR cSMan GT cEndSalesRep OR
              (i NE 1 AND (cSMan EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
            IF NOT CAN-FIND(FIRST tt-report
                            WHERE tt-report.term-id EQ cTerm
                              AND tt-report.key-01  EQ cSMan
                              AND tt-report.inv-no  EQ ar-invl.inv-no
                              AND tt-report.rec-id  EQ RECID(ar-invl)) THEN DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = cTerm
                    tt-report.key-01  = cSMan
                    tt-report.key-09  = cust.cust-no
                    tt-report.key-08  = cust.terms
                    tt-report.key-10  = cust.name
                    tt-report.rec-id  = RECID(ar-invl)
                    tt-report.inv-no  = ar-invl.inv-no.
                CASE cSort:
                    WHEN "Customer" THEN
                    ASSIGN
                        tt-report.key-02 = cust.cust-no
                        tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                    WHEN "Invoice" THEN
                    ASSIGN
                        tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                        tt-report.key-03 = cust.cust-no.
                END CASE.
                RELEASE tt-report.
            END. /* if not can-find */
        END. /* do i */
    END. /* each ar-inv */
    FOR EACH ar-cash NO-LOCK
        WHERE ar-cash.company    EQ ipcCompany
          AND ar-cash.cust-no    EQ cust.cust-no
          AND ar-cash.check-date GE dtStartReceiptDate
          AND ar-cash.check-date LE dtEndReceiptDate
          AND ar-cash.posted     EQ YES
          AND ar-cash.check-no   NE 0,
        EACH ar-cashl NO-LOCK
        WHERE ar-cashl.c-no   EQ ar-cash.c-no
          AND ar-cashl.posted EQ YES
          AND ar-cashl.memo   EQ NO:
        IF NOT (iDayOld EQ 0 OR
               (ar-cash.check-date - ar-cashl.inv-date GT iDayOld AND
                ar-cashl.inv-no NE 0)) THEN NEXT.
        IF ar-cashl.inv-no NE 0 THEN
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.company EQ ipcCompany
              AND ar-invl.cust-no EQ ar-cash.cust-no
              AND ar-invl.inv-no  EQ ar-cashl.inv-no:
            DO i = 1 TO 3:
                cSMan = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
                ELSE ar-invl.sman[i].
                IF cSMan  LT cStartSalesRep OR cSMan GT cEndSalesRep OR
                  (i NE 1 AND (cSMan EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.
                IF NOT CAN-FIND(FIRST tt-report
                                WHERE tt-report.term-id EQ cTerm
                                  AND tt-report.key-01  EQ cSMan
                                  AND tt-report.inv-no  EQ ar-invl.inv-no
                                  AND tt-report.rec-id  EQ RECID(ar-cashl)) THEN DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id = cTerm
                        tt-report.key-01  = cSMan
                        tt-report.key-09  = cust.cust-no
                        tt-report.key-08  = cust.terms
                        tt-report.key-10  = cust.name
                        tt-report.rec-id  = RECID(ar-cashl)
                        tt-report.inv-no  = ar-invl.inv-no.
                    CASE cSort:
                        WHEN "Customer" THEN
                        ASSIGN
                            tt-report.key-02 = cust.cust-no
                            tt-report.key-03 = STRING(ar-invl.inv-no,"9999999999").
                        WHEN "Invoice" THEN
                            ASSIGN
                            tt-report.key-02 = STRING(ar-invl.inv-no,"9999999999")
                            tt-report.key-03 = cust.cust-no.
                    END CASE.
                    RELEASE tt-report.
                END. /* if not can-find */
            END. /* do i */
        END. /* each ar-invl */
        ELSE
        IF cust.sman GE cStartSalesRep AND
           cust.sman LE cEndSalesRep   THEN DO:
            cSMan = cust.sman.
            IF NOT CAN-FIND(FIRST tt-report
                            WHERE tt-report.term-id EQ cTerm
                              AND tt-report.key-01  EQ cSMan
                              AND tt-report.inv-no  EQ ar-cashl.inv-no
                              AND tt-report.rec-id  EQ RECID(ar-cashl)) THEN DO:
                CREATE tt-report.
                ASSIGN
                    tt-report.term-id = cTerm
                    tt-report.key-01  = cSMan
                    tt-report.key-09  = cust.cust-no
                    tt-report.key-08  = cust.terms
                    tt-report.key-10  = cust.name
                    tt-report.rec-id  = RECID(ar-cashl)
                    tt-report.inv-no  = ar-cashl.inv-no.
                CASE cSort:
                    WHEN "Customer" THEN
                    ASSIGN
                        tt-report.key-02 = cust.cust-no
                        tt-report.key-03 = STRING(ar-cashl.inv-no,"9999999999").
                    WHEN "Invoice" THEN
                    ASSIGN
                        tt-report.key-02 = STRING(ar-cashl.inv-no,"9999999999")
                        tt-report.key-03 = cust.cust-no.
                END CASE.
                RELEASE tt-report.
            END. /* if not can-find */
        END. /* else if */
    END. /* each ar-cashl */
END. /* each cust */
FOR EACH tt-report NO-LOCK:
    FIND FIRST tt-report-inv NO-LOCK
         WHERE tt-report-inv.inv-no = tt-report.inv-no 
           AND  tt-report-inv.key-01 NE tt-report.key-01
         NO-ERROR.
    IF NOT AVAIL tt-report-inv THEN DO:
        CREATE  tt-report-inv.
        ASSIGN
            tt-report-inv.key-01 = tt-report.key-01
            tt-report-inv.key-02 = tt-report.key-02
            tt-report-inv.key-03 = tt-report.key-03
            tt-report-inv.inv-no = tt-report.inv-no.
    END. /* not avail tt-report-inv */
    ELSE tt-report.chk-inv = FALSE.
END. /* each tt-report */
FOR EACH tt-report-inv NO-LOCK:
    DELETE tt-report-inv .
END. /* each tt-report-inv */
FOR EACH tt-report
    WHERE tt-report.term-id EQ cTerm,
    FIRST cust NO-LOCK
    WHERE cust.company EQ ipcCompany
      AND cust.cust-no EQ tt-report.key-09
    BREAK BY tt-report.key-01
          BY tt-report.key-02
          BY tt-report.key-03
          BY tt-report.inv-no:
    FIND FIRST sman NO-LOCK
         WHERE sman.company EQ ipcCompany
           AND sman.sman    EQ tt-report.key-01
         NO-ERROR.
    RELEASE ar-inv.
    RELEASE ar-cash.
    ASSIGN
        dtCheckDate = ?
        dtInvDate = ?
        dAmt-full = 0
        dInvFull = 0
        dMisc = 0.
    FIND ar-cashl NO-LOCK
         WHERE RECID(ar-cashl) EQ tt-report.rec-id
         NO-ERROR.
    IF AVAILABLE ar-cashl THEN DO:
        FIND FIRST ar-cash NO-LOCK WHERE ar-cash.c-no EQ ar-cashl.c-no.
        ASSIGN
            dDsc[1] = IF lIncludeTermsDiscount THEN ar-cashl.amt-disc ELSE 0
            dtCheckDate = ar-cash.check-date
            dAmt-full = ar-cashl.amt-paid
            lInvFound = NO.
        IF ar-cashl.inv-no NE 0 THEN
        FOR EACH ar-invl NO-LOCK
            WHERE ar-invl.company EQ ipcCompany
              AND ar-invl.cust-no EQ ar-cash.cust-no
              AND ar-invl.inv-no  EQ ar-cashl.inv-no,
            FIRST ar-inv NO-LOCK
            WHERE ar-inv.x-no EQ ar-invl.x-no
            BREAK BY ar-invl.inv-no:
            lInvFound = YES.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany
                  AND itemfg.i-no     EQ ar-invl.i-no
                 NO-ERROR.
            RUN custom/combasis.p (ipcCompany,tt-report.key-01,cust.type,
               (IF AVAILABLE itemfg THEN itemfg.procat ELSE ""),0,cust.cust-no,OUTPUT cBasis).
            IF FIRST-OF(tt-report.inv-no) THEN DO:
                IF FIRST(ar-invl.inv-no) THEN
                ASSIGN
                    dAmt    = 0
                    dAmt[1] = ar-inv.tax-amt +
                    (if ar-inv.f-bill THEN ar-inv.freight ELSE 0)
                    dInvFull = dAmt[1]
                    dCom    = 0
                    dCom-2  = 0
                    dtInvDate = ar-inv.inv-date.
                dInvFull = dInvFull + ar-invl.amt.
                IF NOT lIncludePrepCharges AND ar-invl.misc THEN
                dMisc = dMisc + ar-invl.amt.
                ELSE DO:
                    ASSIGN
                        dAmt[1] = dAmt[1] + ar-invl.amt
                        dTmpAmt1 = dAmt[1].
                    IF ar-invl.sman[1] NE "" THEN
                    DO i = 1 TO 3:
                        IF tt-report.key-01 EQ ar-invl.sman[i] THEN DO:
                            ASSIGN
                                dAmt[2] = dAmt[2] + (ar-invl.amt * ar-invl.s-pct[i] / 100)
                                dCom    = dCom
                                        + (((ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                                ar-invl.s-pct[i] / 100) * ar-invl.s-comm[i] / 100).
                            LEAVE.
                        END. /* if key-01 */
                    END. /* do i */
                    ELSE
                    ASSIGN
                        dAmt[2] = dAmt[2] + ar-invl.amt
                        dCom    = dCom +
                        ((ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
                        (IF AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)).
                END. /* else */
            END. /*end FIRST-OF(tt-report.inv-no)*/
        END. /*end each ar-invl*/
        IF lInvFound = NO THEN
        ASSIGN
            dAmt[1] = ar-cashl.amt-paid + dDsc[1] - dMisc
            dTmpAmt1 = dAmt[1]
            dAmt[2] = dAmt[1]
            dCom    = dAmt[1] * (if AVAILABLE sman THEN (sman.scomm / 100) ELSE 0).
        ASSIGN
            dPct    = dAmt[2] / dTmpAmt1
            dAmt[1] = (ar-cashl.amt-paid + dDsc[1] - dMisc) * dPct    /* task 02261403 */
            dPct    = dAmt[1] / dAmt[2]
            dCom-2  = dCom * dPct.
        RELEASE ar-inv.
    END. /* avail ar-cashl */
    ELSE DO:
        FIND ar-invl NO-LOCK WHERE RECID(ar-invl) EQ tt-report.rec-id.
        FIND FIRST ar-inv NO-LOCK WHERE ar-inv.x-no EQ ar-invl.x-no.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ ar-invl.i-no
             NO-ERROR.
        RUN custom/combasis.p
            (ipcCompany, tt-report.key-01, cust.type,
            (IF AVAIL itemfg THEN itemfg.procat ELSE ""), 0,cust.cust-no,OUTPUT cBasis).
        dInvFull = ar-invl.amt.
        IF NOT lIncludePrepCharges AND ar-invl.misc THEN NEXT.
        ASSIGN
            dAmt[1] = ar-invl.amt 
            dTmpAmt1 = dAmt[1]
            dCom    = (ar-invl.amt - IF cBasis EQ "G" THEN ar-invl.t-cost ELSE 0) *
            (if AVAILABLE sman THEN (sman.scomm / 100) ELSE 0)
            dCom-2  = dCom.
    END. /* else do */
    IF dCom-2  EQ ? THEN dCom-2 = 0.
    IF dAmt[1] EQ ? THEN
    ASSIGN
        dAmt[1] = 0
        dTmpAmt1 = 0.
    ASSIGN
        dComm% = dCom-2 / dAmt[1] * 100
        dAmt[1] = dTmpAmt1. /* multiple payments against an invoice, reset dAmt[1] value */
    IF dComm% EQ ? THEN dComm% = 0.
    ASSIGN
        dPaid[1] = dAmt-full
        iAging = (IF dtCheckDate NE ? AND dtInvDate NE ? THEN dtCheckDate - dtInvDate ELSE 0).
    dRemBal = IF FIRST-OF(tt-report.inv-no) THEN dInvFull - dAmt-full - dDsc[1]
              ELSE dRemBal - dAmt-full - dDsc[1].
    CREATE ttCashReceiptBySalesRepName.
    ASSIGN
        ttCashReceiptBySalesRepName.xxSort    = tt-report.key-01
                                              + tt-report.key-02
                                              + tt-report.key-03
        ttCashReceiptBySalesRepName.salesRep  = STRING(tt-report.key-01) 
        ttCashReceiptBySalesRepName.salesName = IF AVAILABLE sman THEN STRING(sman.sname) ELSE ""
        ttCashReceiptBySalesRepName.custNo    = STRING(tt-report.key-09)
        ttCashReceiptBySalesRepName.custName  = tt-report.key-10
        ttCashReceiptBySalesRepName.terms     = tt-report.key-08
        ttCashReceiptBySalesRepName.invoiceNo = IF AVAILABLE ar-cashl THEN ar-cashl.inv-no ELSE IF AVAIL ar-inv THEN  ar-inv.inv-no ELSE 0
        ttCashReceiptBySalesRepName.invDate   = dtInvDate   
        ttCashReceiptBySalesRepName.chkDate   = dtCheckDate 
        ttCashReceiptBySalesRepName.Aging     = iAging
        ttCashReceiptBySalesRepName.invAmt    = dInvFull
        ttCashReceiptBySalesRepName.amtPaid   = dAmt-full
        ttCashReceiptBySalesRepName.discount  = dDsc[1]
        ttCashReceiptBySalesRepName.balAftPay = dRemBal
        ttCashReceiptBySalesRepName.commAmt   = dCom-2
        ttCashReceiptBySalesRepName.commPct   = dComm%.
    IF LAST-OF(tt-report.inv-no) THEN DELETE tt-report.
END. /* each tt-report */
