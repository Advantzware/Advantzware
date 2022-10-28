/*------------------------------------------------------------------------
  File:         r-inctsh.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 10.18.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSalesAnalysis
DEFINE TEMP-TABLE ttSalesAnalysis NO-UNDO
    FIELD custNo    LIKE cust.cust-no
    FIELD custName  LIKE cust.name
    FIELD iYear1      AS INTEGER   FORMAT "9999"              LABEL "Year"
    FIELD location    AS CHARACTER FORMAT "x(25)"             LABEL "Location"
    FIELD shipTo      AS CHARACTER FORMAT "x(8)"              LABEL "Ship To"
    FIELD shipName  LIKE shipto.ship-city
    FIELD shipAddr1 LIKE shipto.ship-addr[1]
    FIELD shipAddr2 LIKE shipto.ship-addr[2]
    FIELD shipAddr3 LIKE shipto.spare-char-3
    FIELD shipCity  LIKE shipto.ship-city
    FIELD shipState LIKE shipto.ship-state
    FIELD shipZip   LIKE shipto.ship-zip
    FIELD city      LIKE shipto.ship-city
    FIELD state     LIKE shipto.ship-state
    FIELD zipcode   LIKE shipto.ship-zip
    FIELD salesRep  LIKE sman.sname
    FIELD invNo       AS INTEGER   FORMAT ">>>>>>>>9"         LABEL "Invoice"
    FIELD iMonth      AS INTEGER   FORMAT "99"                LABEL "Month"
    FIELD iYear2      AS INTEGER   FORMAT "9999"              LABEL "Year"
    FIELD invDate     AS DATE      FORMAT "99/99/9999"        LABEL "Invoice Date"
    FIELD fgCat     LIKE fgcat.procat
    FIELD fgItem    LIKE itemfg.i-no
    FIELD OrderNo     AS INTEGER   FORMAT ">>>>>>>>9"         LABEL "Order No"
    FIELD qty         AS INTEGER   FORMAT "->>>,>>>,>>9"      LABEL "Qty Shipped"
    FIELD price       AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<" LABEL "Unit Price"
    FIELD uom         AS CHARACTER                            LABEL "UOM"
    FIELD amount      AS DECIMAL   FORMAT "->,>>>,>>>,>>9.99" LABEL "Invoice Amt"
    FIELD discount    AS DECIMAL                              LABEL "Discount"
    FIELD pct         AS DECIMAL                              LABEL "Pct"
    FIELD bolWhs      AS CHARACTER                            LABEL "BOL Whs"
    FIELD bolDate     AS DATE      FORMAT "99/99/9999"        LABEL "BOL Date"
    .

{sys/ref/CustList.i NEW}

&Scoped-define subjectID 215
{AOA/includes/subjectID{&subjectID}Defs.i}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.
DEFINE TEMP-TABLE w-data    NO-UNDO
    FIELD i-no   LIKE ar-invl.i-no
    FIELD inv-no LIKE ar-invl.inv-no
    FIELD rec-id   AS RECID
    .
DEFINE BUFFER xreport FOR tt-report.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE cSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipID   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lExclude  AS LOGICAL   NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cBOLWhs   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cocode    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtBolDate AS DATE      NO-UNDO.
    DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-inc-fc  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE rNo  LIKE oe-retl.r-no NO-UNDO.

    IF lCustList THEN
    RUN pBuildCustList (
        cCompany,
        "HR#",
        OUTPUT cStartCustNo,
        OUTPUT cEndCustNo,
        OUTPUT lCustList
        ).
    ASSIGN
        cocode   = cCompany
        v-inc-fc = lIncludeFinanceCharges
        .
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
          AND cust.type    GE cStartCustType
          AND cust.type    LE cEndCustType
        USE-INDEX cust
        :
        IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ cust.cust-no
                          AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.
        {sa/sa-sls03.i "dtStartInvoiceDate" "dtEndInvoiceDate"}
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
    END.
    iCount = 0.
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
          AND tt-report.key-01  EQ ""
          AND tt-report.key-02  EQ ""
          AND tt-report.key-03  EQ ""
          AND tt-report.key-04  EQ ""
          AND tt-report.key-05  EQ ""
          AND tt-report.key-06  EQ ""
          AND tt-report.key-07  EQ ""
          AND tt-report.key-08  EQ "",
        FIRST cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no EQ tt-report.key-09
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
        IF tt-report.key-10 EQ "ar-inv" THEN DO:
            FIND FIRST ar-inv NO-LOCK
                 WHERE RECID(ar-inv) EQ tt-report.rec-id.
            RUN pShipData.
            IF cShipID  GE cStartShipToNo AND
               cShipID  LE cEndShipToNo   THEN
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.x-no      EQ ar-inv.x-no
                  AND ar-invl.i-no      GE cStartFGItem
                  AND ar-invl.i-no      LE cEndFGItem
                  AND (ar-invl.billable EQ YES
                   OR  ar-invl.misc     EQ NO)
                USE-INDEX x-no
                :
                RUN pCreateReport1 (
                    RECID(ar-invl),
                    IF ar-invl.misc THEN ar-invl.i-name ELSE
                    IF ar-invl.i-no NE "" THEN ar-invl.i-no ELSE
                    "AR SALE",
                    STRING(ar-inv.inv-no,"99999999"),
                    ""
                    ).
            END.
            IF ar-inv.f-bill THEN DO:
                FIND FIRST ar-invl NO-LOCK
                     WHERE ar-invl.x-no EQ ar-inv.x-no
                     USE-INDEX x-no NO-ERROR.
                IF AVAILABLE ar-invl THEN DO:
                    cSalesRep = "".
                    DO idx = 1 TO 3:
                        IF ar-invl.sman[idx] NE "" THEN DO:
                            cSalesRep = ar-invl.sman[idx].
                            LEAVE.
                        END.
                    END.
                    IF cSalesRep EQ "" THEN
                    cSalesRep = cust.sman.
                    IF "freight" GE cStartFGItem   AND
                       "freight" LE cEndFGItem     AND
                       cSalesRep GE cStartSalesRep AND
                       cSalesRep LE cEndSalesRep   THEN
                    RUN pCreateReport (
                        RECID(ar-invl),
                        "FREIGHT",
                        STRING(ar-inv.inv-no,"99999999"),
                        "FREIGHT"
                        ).
                END.
            END.
            DELETE tt-report.
        END.
        ELSE
        IF tt-report.key-10 EQ "ar-cashl" THEN DO:
            FIND FIRST ar-cashl NO-LOCK
                 WHERE RECID(ar-cashl) EQ tt-report.rec-id.
            FIND FIRST ar-cash NO-LOCK
                 WHERE ar-cash.c-no EQ ar-cashl.c-no.
            ASSIGN
                lExclude         = YES
                tt-report.key-03 = "MEMO"
                tt-report.key-04 = STRING(ar-cashl.inv-no,"99999999")
                tt-report.key-05 = tt-report.key-09
                tt-report.key-06 = cust.sman
                tt-report.key-07 = tt-report.key-03
                .
            RELEASE ar-inv.
            RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).
            ASSIGN
                rNo = 0
                cType = ""
                .
            IF AVAILABLE reftable THEN
            ASSIGN
                rNo = reftable.val[1]
                cType = reftable.dscr
                .
            ELSE
            IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
            ASSIGN
                rNo   = INTEGER(SUBSTRING(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                cType = TRIM(SUBSTRING(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10))
                .
            IF rNo NE 0 THEN DO:
                FIND FIRST oe-reth NO-LOCK
                     WHERE oe-reth.company EQ CcOMPANY
                       AND oe-reth.r-no    EQ rNo
                     NO-ERROR.
                IF AVAILABLE oe-reth THEN
                FIND FIRST ar-inv NO-LOCK
                     WHERE ar-inv.company EQ cCompany
                       AND ar-inv.cust-no EQ oe-reth.cust-no
                       AND ar-inv.inv-no  EQ oe-reth.inv-no
                     NO-ERROR.
            END.
            IF AVAILABLE ar-inv THEN DO:
                RUN pShipData.
                IF cShipID  GE cStartShipToNo AND
                   cShipID  LE cEndShipToNo   THEN
                    IF cType EQ "items" THEN DO:
                        RELEASE ar-invl.
                        FIND FIRST oe-retl NO-LOCK
                             WHERE oe-retl.company EQ cCompany
                               AND oe-retl.r-no    EQ oe-reth.r-no
                               AND oe-retl.line    EQ ar-cashl.line
                               AND oe-retl.i-no    GE cStartFGItem
                               AND oe-retl.i-no    LE cEndFGItem
                             NO-ERROR.
                        IF AVAILABLE oe-retl THEN
                        FIND FIRST ar-invl NO-LOCK
                             WHERE ar-invl.company EQ cCompany 
                               AND ar-invl.cust-no EQ ar-cash.cust-no
                               AND ar-invl.inv-no  EQ ar-cashl.inv-no
                               AND ar-invl.i-no    EQ oe-retl.i-no
                               AND (ar-invl.billable OR NOT ar-invl.misc)
                             NO-ERROR.
                        IF AVAILABLE ar-invl THEN DO:
                            RUN pCreateReport1 (RECID(ar-cashl), oe-retl.i-no, tt-report.key-04, "").
                            DELETE tt-report.
                        END.
                    END.
                    ELSE
                    IF cType     EQ "freight"      AND
                       "freight" GE cStartFGItem   AND
                       "freight" LE cEndFGItem     AND
                       cust.sman GE cStartSalesRep AND
                       cust.sman LE cEndSalesRep   THEN
                    ASSIGN
                        lExclude            = NO
                        tt-report.key-03 = "FREIGHT"
                        tt-report.key-05 = cShipID
                        .
                    ELSE
                    IF cType     EQ "tax"          AND
                       "tax"     GE cStartFGItem   AND
                       "tax"     LE cEndFGItem     AND
                       cust.sman GE cStartSalesRep AND
                       cust.sman LE cEndSalesRep   THEN
                    ASSIGN
                        lExclude         = NO
                        tt-report.key-03 = "TAX"
                        tt-report.key-05 = cShipID
                        .
                    ELSE
                    IF ""        GE cStartFGItem   AND
                       ""        LE cEndFGItem     AND
                       cust.sman GE cStartSalesRep AND
                       cust.sman LE cEndSalesRep   THEN
                    lExclude = NO.
                END.
                ELSE
                IF ""               GE cStartFGItem   AND
                   ""               LE cEndFGItem     AND
                   cust.sman        GE cStartSalesRep AND
                   cust.sman        LE cEndSalesRep   AND
                   ar-cashl.cust-no GE cStartShipToNo AND
                   ar-cashl.cust-no LE cEndShipToNo   THEN
                lExclude = NO.
                IF AVAILABLE tt-report THEN DO:
                    tt-report.key-07 = tt-report.key-03.
                    IF lExclude THEN
                    DELETE tt-report.
            END.
        END.     
    END. // each tt-report

    FOR EACH xreport NO-LOCK: /* Strange problem of tt-report*/
    END.

    iCount = 0.
    FOR EACH tt-report
        WHERE tt-report.term-id EQ "",
        FIRST cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no EQ tt-report.key-09
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
              BY tt-report.key-05
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-07
            w-data.inv-no = INTEGER(tt-report.key-04)
            w-data.rec-id = tt-report.rec-id
            .
        FIND FIRST ar-invl NO-LOCK
             WHERE RECID(ar-invl) EQ w-data.rec-id
             NO-ERROR.
        ASSIGN
            cBOLWhs = "" 
            dtBolDate = ?
            .
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company EQ ar-invl.company
              AND oe-boll.b-no    EQ ar-invl.b-no
            :
            ASSIGN
                cBOLWhs  = oe-boll.loc
                dtBolDate = oe-boll.bol-date
                .
            IF cBOLWhs NE "" THEN
            LEAVE.
        END.
        CREATE ttSalesAnalysis.
        IF AVAILABLE ar-invl THEN DO:
            FIND FIRST ar-inv NO-LOCK
                 WHERE ar-inv.x-no EQ ar-invl.x-no.
            ASSIGN
                ttSalesAnalysis.invDate  = ar-inv.inv-date
                ttSalesAnalysis.OrderNo  = ar-invl.ord-no
                ttSalesAnalysis.price    = ar-invl.unit-pr
                ttSalesAnalysis.uom      = ar-invl.pr-uom
                ttSalesAnalysis.qty      = ar-invl.ship-qty
                ttSalesAnalysis.amount   = ar-invl.amt
                ttSalesAnalysis.discount = ar-invl.disc
                ttSalesAnalysis.pct      = 1
                .
            IF tt-report.key-10 EQ "FREIGHT" THEN
            ASSIGN
                ttSalesAnalysis.price    = ar-inv.freight
                ttSalesAnalysis.uom      = ""
                ttSalesAnalysis.qty      = 0
                ttSalesAnalysis.amount   = ar-inv.freight
                ttSalesAnalysis.discount = 0
                .
            ELSE DO:
                DO idx = 1 TO 3:
                    IF ar-invl.sman[idx] EQ tt-report.key-06 THEN
                    ASSIGN
                        ttSalesAnalysis.pct = ar-invl.s-pct[idx] / 100
                        idx   = 3
                        .
                END.
                IF ttSalesAnalysis.pct EQ 0 THEN
                DO idx = 1 TO 3:
                    IF idx EQ 1 THEN
                    jdx = 0.
                    IF ar-invl.sman[idx] NE "" THEN
                    jdx = jdx + 1.
                    IF idx EQ 3 THEN
                    ttSalesAnalysis.pct = 1 / jdx.
                END.
                IF ttSalesAnalysis.pct LE 0 OR ttSalesAnalysis.pct EQ ? THEN
                ttSalesAnalysis.pct = 1.
            END.
            ttSalesAnalysis.amount = ttSalesAnalysis.amount * ttSalesAnalysis.pct.
        END.
        ELSE DO:
            FIND FIRST ar-cashl NO-LOCK
                 WHERE RECID(ar-cashl) EQ w-data.rec-id
                 NO-ERROR.
            IF AVAILABLE ar-cashl THEN DO:
                FIND FIRST ar-cash NO-LOCK
                     WHERE ar-cash.c-no EQ ar-cashl.c-no.
                ASSIGN
                    ttSalesAnalysis.invDate   = ar-cash.check-date
                    ttSalesAnalysis.OrderNo  = 0
                    ttSalesAnalysis.price    = ar-cashl.amt-paid - ar-cashl.amt-disc
                    ttSalesAnalysis.uom      = ""
                    ttSalesAnalysis.qty      = 0
                    ttSalesAnalysis.amount   = ar-cashl.amt-paid - ar-cashl.amt-disc
                    ttSalesAnalysis.discount = 0
                    .
                RELEASE ar-invl.
                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).
                IF AVAILABLE oe-retl THEN DO:
                    ASSIGN
                        ttSalesAnalysis.OrderNo = oe-retl.ord-no
                        ttSalesAnalysis.price   = oe-retl.unit-pr
                        ttSalesAnalysis.uom     = oe-retl.uom
                        ttSalesAnalysis.qty     = - oe-retl.tot-qty-return
                        .
                    FIND FIRST ar-invl
                         WHERE ar-invl.company EQ cCompany
                           AND ar-invl.cust-no EQ ar-cash.cust-no
                           AND ar-invl.inv-no  EQ ar-cashl.inv-no
                           AND ar-invl.i-no    EQ oe-retl.i-no
                         NO-ERROR.
                    IF AVAILABLE ar-invl THEN DO:
                        /* Added for decimal problem */
                        ttSalesAnalysis.price = ar-invl.unit-pr.
                        DO idx = 1 TO 3:
                            IF ar-invl.sman[idx] EQ tt-report.key-06 THEN
                            ASSIGN
                                ttSalesAnalysis.pct = ar-invl.s-pct[idx] / 100
                                idx = 3
                                .
                        END.
                        IF ttSalesAnalysis.pct EQ 0 THEN
                        DO idx = 1 TO 3:
                            IF idx EQ 1 THEN
                            jdx = 0.
                            IF ar-invl.sman[idx] NE "" THEN
                            jdx = jdx + 1.
                            IF idx EQ 3 THEN
                            ttSalesAnalysis.pct = 1 / jdx.
                        END.
                        IF ttSalesAnalysis.pct LE 0 OR ttSalesAnalysis.pct EQ ? THEN
                        ttSalesAnalysis.pct = 1.
                        ASSIGN
                            ttSalesAnalysis.amount = ttSalesAnalysis.amount * ttSalesAnalysis.pct
                            ttSalesAnalysis.discount   = ar-invl.disc
                            .
                    END.
                END.
            END.
        END.

        IF lShowDiscountedPrices AND ttSalesAnalysis.discount NE 0 THEN
        ttSalesAnalysis.price = ttSalesAnalysis.price * (100 - ttSalesAnalysis.discount) / 100.
        FIND FIRST shipto NO-LOCK
             WHERE shipto.company EQ cust.company
               AND shipto.cust-no EQ cust.cust-no
               AND shipto.ship-id EQ tt-report.key-05
             NO-ERROR.        
        IF AVAILABLE shipto THEN
        ASSIGN
            ttSalesAnalysis.shipName  = shipto.ship-name
            ttSalesAnalysis.shipAddr1 = shipto.ship-addr[1]
            ttSalesAnalysis.shipAddr2 = shipto.ship-addr[2]
            ttSalesAnalysis.shipAddr3 = shipto.spare-char-3
            ttSalesAnalysis.shipCity  = shipto.ship-city
            ttSalesAnalysis.shipState = shipto.ship-state
            ttSalesAnalysis.shipZip   = shipto.ship-zip
            .
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ cust.company
               AND itemfg.i-no    EQ w-data.i-no
             NO-ERROR.
        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ cust.company
               AND sman.sman    EQ tt-report.key-06
             NO-ERROR.
        ASSIGN
            ttSalesAnalysis.custNo   = cust.cust-no
            ttSalesAnalysis.custName = cust.name
            ttSalesAnalysis.shipTo   = tt-report.key-05
            ttSalesAnalysis.city     = cust.city
            ttSalesAnalysis.state    = cust.state
            ttSalesAnalysis.fgcat    = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
            ttSalesAnalysis.fgItem   = w-data.i-no
            ttSalesAnalysis.invNo    = w-data.inv-no
            ttSalesAnalysis.salesRep = IF AVAILABLE sman THEN sman.sname ELSE tt-report.key-06
            ttSalesAnalysis.iYear2   = YEAR(ttSalesAnalysis.invDate)
            ttSalesAnalysis.iMonth   = MONTH(ttSalesAnalysis.invDate)
            ttSalesAnalysis.bolWhs   = cBOLWhs
            ttSalesAnalysis.bolDate  = dtBOLDate
            .
    END.
    IF lProgressBar THEN
    RUN spProgressBar (?, ?, 100).

END PROCEDURE.

PROCEDURE pCreateReport:
    DEFINE INPUT PARAMETER iprRecID   AS RECID            NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey03 LIKE tt-report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey04 LIKE tt-report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey10 LIKE tt-report.key-10 NO-UNDO.

    CREATE xreport.
    ASSIGN
        lExclude        = NO
        xreport.term-id = ""
        xreport.rec-id  = iprRecID
        xreport.key-03  = ipcKey03
        xreport.key-04  = ipcKey04
        xreport.key-05  = cShipID
        xreport.key-06  = cSalesRep
        xreport.key-07  = xreport.key-03
        xreport.key-09  = tt-report.key-09
        xreport.key-10  = ipcKey10
        .

END PROCEDURE.

PROCEDURE pCreateReport1:
    DEFINE INPUT PARAMETER iprRecID    AS RECID         NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey03 LIKE report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey04 LIKE report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey10 LIKE report.key-10 NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO idx = 1 TO 3:
        cSalesRep = IF ar-invl.sman[idx] EQ "" AND idx EQ 1 THEN cust.sman
                    ELSE ar-invl.sman[idx].
        IF cSalesRep LT cStartSalesRep OR
           cSalesRep GT cEndSalesRep   OR
          (idx NE 1 AND
          (cSalesRep EQ "" OR ar-invl.s-pct[idx] EQ 0)) THEN
        NEXT.
        RUN pCreateReport (iprRecID, ipcKey03, ipcKey04, ipcKey10).
        LEAVE.
    END.

END PROCEDURE.

PROCEDURE pShipData:
    RELEASE shipto.
    IF ar-inv.ship-id NE "" THEN
    FIND FIRST shipto NO-LOCK
         WHERE shipto.company EQ cCompany
           AND shipto.cust-no EQ ar-inv.cust-no
           AND shipto.ship-id EQ ar-inv.ship-id
         NO-ERROR.
    IF AVAILABLE shipto THEN
    cShipID = ar-inv.ship-id.
    ELSE
    IF ar-inv.sold-id NE "" THEN
    cShipID = ar-inv.sold-id.
    ELSE
    cShipID = ar-inv.cust-no.

END PROCEDURE.

{AOA/dynBL/pGetCustInfo.i}
