/*------------------------------------------------------------------------
  File:         r-SalesAnalysis.p
  Description:  Business Logic
  Author:       Sachin Chahal
  Date Created: 05.31.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttSalesAnalysis 
{aoa/tempTable/ttSalesAnalysis.i}

{sys/ref/CustList.i NEW}

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE i           AS INTEGER   NO-UNDO.
DEFINE VARIABLE j           AS INTEGER   NO-UNDO.
DEFINE VARIABLE cShipId     LIKE ar-inv.ship-id    NO-UNDO.
DEFINE VARIABLE cSoldZip    LIKE ar-inv.sold-zip   NO-UNDO.
DEFINE VARIABLE cSmanNo     AS CHARACTER FORMAT "x(3)" NO-UNDO.
DEFINE VARIABLE cSorts      AS CHARACTER FORMAT "!" INIT "I" NO-UNDO.
DEFINE VARIABLE lExc        AS LOGICAL   NO-UNDO.


DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.
DEFINE BUFFER xreport FOR tt-report.

DEFINE TEMP-TABLE w-data NO-UNDO
    FIELD i-no   LIKE ar-invl.i-no COLUMN-LABEL "FG Item"
    FIELD inv-no LIKE ar-invl.inv-no COLUMN-LABEL "Invoice!Number"
    FIELD rec-id AS RECID.

&Scoped-define subjectID 206
{AOA/includes/subjectID{&subjectID}Defs.i}

/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE iCount          AS INTEGER             NO-UNDO.
    DEFINE VARIABLE cocode          AS CHARACTER           NO-UNDO. /* Used in Internal File sa/sa-sls03.i */
    DEFINE VARIABLE v-inc-fc        AS LOGICAL INIT NO     NO-UNDO. /* Used in Internal File sa/sa-sls03.i */
    DEFINE VARIABLE lDiscountPrice  AS LOGICAL INIT NO     NO-UNDO.
    DEFINE VARIABLE v-freight       AS LOGICAL INIT NO     NO-UNDO. 
    
    DEFINE VARIABLE iR-No           LIKE oe-retl.r-no NO-UNDO.
    DEFINE VARIABLE cType           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBolWhs         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtBolDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE iYear           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iMonth          AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE dPct            AS DECIMAL   FORMAT "99.99" NO-UNDO.
    
    DEFINE VARIABLE dDiscount       LIKE ar-invl.disc      NO-UNDO.
    DEFINE VARIABLE cFromSoldZip    AS CHARACTER FORMAT "x(10)" INIT "" NO-UNDO.
    DEFINE VARIABLE cToSoldZip      LIKE cFromSoldZip INIT "zzzzzzzzzz" NO-UNDO.
    
    DEFINE VARIABLE cCity           LIKE shipto.ship-city NO-UNDO.
    DEFINE VARIABLE cState          LIKE shipto.ship-state NO-UNDO.
    DEFINE VARIABLE cSname          LIKE sman.sname NO-UNDO.
    DEFINE VARIABLE cProcat         LIKE fgcat.procat NO-UNDO.
    
    DEFINE VARIABLE dtInvDate       LIKE ar-inv.inv-date NO-UNDO.
    DEFINE VARIABLE iOrdNo          LIKE ar-invl.ord-no NO-UNDO.
    DEFINE VARIABLE dUnitPrice      LIKE ar-invl.unit-pr NO-UNDO.
    DEFINE VARIABLE cUom            LIKE ar-invl.pr-uom NO-UNDO.
    
    DEFINE VARIABLE iShipQty        AS INTEGER NO-UNDO.
    DEFINE VARIABLE dAmount         AS DECIMAL NO-UNDO.
    

    
    ASSIGN 
        cocode   = cCompany
        v-inc-fc = lIncludeFinanceCharges
        lDiscountPrice = lShowDiscountedPrices
        cSorts = SUBSTR(cSort,1,1)
        .
    
    IF lCustList THEN
    RUN pBuildCustList (
        cCompany,
        "HR13",
        OUTPUT cStartCustNo,
        OUTPUT cEndCustNo,
        OUTPUT lCustList
        ).
    
    FOR EACH tt-report:
        DELETE tt-report.
    END.

    FOR EACH xreport:
        DELETE xreport.
    END.
    
    FOR EACH cust NO-LOCK
        WHERE cust.company EQ cCompany
          AND cust.cust-no GE cStartCustNo
          AND cust.cust-no LE cEndCustNo
          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList 
                                          WHERE ttCustList.cust-no EQ cust.cust-no
                                            AND ttCustList.log-fld NO-LOCK) ELSE TRUE)
          AND cust.type    GE cStartCustType
          AND cust.type    LE cEndCustType
          USE-INDEX cust :
          {sa/sa-sls03.i "dtStartInvoiceDate" "dtEndInvoiceDate"}

    END.
    
    IF lCustList AND
           NOT CAN-FIND(FIRST ttCustList
                        WHERE ttCustList.cust-no EQ tt-report.key-09
                          AND ttCustList.log-fld EQ TRUE) THEN
        NEXT.
    
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

        FIRST cust
        WHERE cust.company EQ cCompany
        AND cust.cust-no   EQ tt-report.key-09
        NO-LOCK

        TRANSACTION:
        
        IF tt-report.key-10 EQ "ar-inv" THEN 
        DO:
            FIND ar-inv WHERE RECID(ar-inv) EQ tt-report.rec-id NO-LOCK.

            RUN ship-data.

            IF  cShipId GE cStartShipToNo AND
                cShipId LE cEndShipToNo   AND
                cSoldZip GE cFromSoldZip  AND
                cSoldZip LE cToSoldZip    THEN 
            DO:

                FOR EACH ar-invl
                    WHERE ar-invl.x-no    EQ ar-inv.x-no
                    AND ar-invl.i-no      GE cStartItemNo
                    AND ar-invl.i-no      LE cEndItemNo
                    AND (ar-invl.billable OR NOT ar-invl.misc)
                    USE-INDEX x-no NO-LOCK:

                    RUN create-report1 (RECID(ar-invl),
                        IF ar-invl.misc THEN ar-invl.i-name ELSE
                        IF ar-invl.i-no NE "" THEN ar-invl.i-no ELSE
                        "AR SALE",
                        STRING(ar-inv.inv-no,"99999999"), "").
                END.

                IF v-freight AND ar-inv.f-bill THEN 
                DO:
                    FIND FIRST ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no
                        USE-INDEX x-no NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:
                        cSmanNo = "".

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] NE "" THEN 
                            DO:
                                cSmanNo = ar-invl.sman[i].
                                LEAVE.
                            END.
                        END.

                        IF cSmanNo EQ "" THEN cSmanNo = cust.sman.

                        IF "freight" GE cStartItemNo  AND
                            "freight" LE cEndItemNo   AND
                            cSmanNo GE cStartSalesRep AND
                            cSmanNo LE cEndSalesRep   THEN

                            RUN create-report (RECID(ar-invl), "FREIGHT",
                                STRING(ar-inv.inv-no,"99999999"), "FREIGHT").
                    END.
                END.
            END.

            DELETE tt-report.
        END.

        ELSE
            IF tt-report.key-10 EQ "ar-cashl" THEN 
            DO:
                FIND ar-cashl WHERE RECID(ar-cashl) EQ tt-report.rec-id NO-LOCK.
                FIND ar-cash  WHERE ar-cash.c-no    EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    lExc            = YES
                    tt-report.key-01 = TRIM(IF cSorts EQ "Z" THEN cust.zip ELSE "") +
                          tt-report.key-09
                    tt-report.key-02 = IF cSorts NE "I" THEN
                            (IF cSorts EQ "H" THEN cust.zip ELSE "") +
                             tt-report.key-09
                          ELSE ""
                    tt-report.key-03 = "MEMO"
                    tt-report.key-04 = STRING(ar-cashl.inv-no,"99999999")
                    tt-report.key-05 = tt-report.key-09
                    tt-report.key-06 = cust.sman
                    tt-report.key-07 = tt-report.key-03.

                RELEASE ar-inv.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                ASSIGN
                    iR-No = 0
                    cType = "".

                IF AVAILABLE reftable THEN
                    ASSIGN
                        iR-No = reftable.val[1]
                        cType = reftable.dscr.
                ELSE
                    IF ar-cashl.dscr MATCHES "*OE RETURN*" THEN
                        ASSIGN
                            iR-No = INT(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 25,12))
                            cType = TRIM(SUBSTR(ar-cashl.dscr,INDEX(ar-cashl.dscr,"oe return") + 12,10)).

                IF iR-No NE 0 THEN 
                DO:
                    FIND FIRST oe-reth
                        WHERE oe-reth.company EQ cCompany
                        AND oe-reth.r-no    EQ iR-No
                        NO-LOCK NO-ERROR.
                    IF AVAILABLE oe-reth THEN
                        FIND FIRST ar-inv
                            WHERE ar-inv.company EQ cCompany
                            AND ar-inv.cust-no EQ oe-reth.cust-no
                            AND ar-inv.inv-no  EQ oe-reth.inv-no
                            NO-LOCK NO-ERROR.
                END.       

                IF AVAILABLE ar-inv THEN 
                DO:
                    RUN ship-data.

                    IF cShipId GE cStartShipToNo AND
                        cShipId LE cEndShipToNo  AND
                        cSoldZip GE cFromSoldZip AND
                        cSoldZip LE cToSoldZip THEN
                        IF cType EQ "items" THEN 
                        DO:
                            RELEASE ar-invl.
                            FIND FIRST oe-retl
                                WHERE oe-retl.company EQ cCompany
                                AND oe-retl.r-no    EQ oe-reth.r-no
                                AND oe-retl.line    EQ ar-cashl.line
                                AND oe-retl.i-no    GE cStartItemNo
                                AND oe-retl.i-no    LE cEndItemNo
                                NO-LOCK NO-ERROR.
                            IF AVAILABLE oe-retl THEN
                                FIND FIRST ar-invl
                                    WHERE ar-invl.company EQ cCompany
                                    AND ar-invl.cust-no EQ ar-cash.cust-no
                                    AND ar-invl.inv-no  EQ ar-cashl.inv-no
                                    AND ar-invl.i-no    EQ oe-retl.i-no
                                    AND (ar-invl.billable OR NOT ar-invl.misc)
                                    NO-LOCK NO-ERROR.
                            IF AVAILABLE ar-invl THEN 
                            DO:
                                RUN create-report1 (RECID(ar-cashl), oe-retl.i-no,
                                    tt-report.key-04, "").

                                DELETE tt-report.
                            END.
                        END.

                        ELSE
                            IF  cType     EQ "freight"               AND
                                "freight" GE cStartItemNo            AND
                                "freight" LE cEndItemNo              AND
                                cust.sman GE cStartSalesRep          AND
                                cust.sman LE cEndSalesRep            AND
                                v-freight                            THEN
                                ASSIGN
                                    lExc            = NO
                                    tt-report.key-02 = IF cSorts NE "I" THEN cShipId ELSE ""
                                    tt-report.key-03 = "FREIGHT"
                                    tt-report.key-05 = cShipId.

                            ELSE
                                IF  cType     EQ "tax"               AND
                                    "tax"     GE cStartItemNo        AND
                                    "tax"     LE cEndItemNo          AND
                                    cust.sman GE cStartSalesRep      AND
                                    cust.sman LE cEndSalesRep        THEN
                                    ASSIGN
                                        lExc            = NO
                                        tt-report.key-02 = IF cSorts NE "I" THEN cShipId ELSE ""
                                        tt-report.key-03 = "TAX"
                                        tt-report.key-05 = cShipId.

                                ELSE
                                    IF  ""        GE cStartItemNo   AND
                                        ""        LE cEndItemNo     AND
                                        cust.sman GE cStartSalesRep AND
                                        cust.sman LE cEndSalesRep   THEN lExc = NO.
                END.

                ELSE
                    IF  ""               GE cStartItemNo      AND
                        ""               LE cEndItemNo        AND
                        cust.sman        GE cStartSalesRep    AND
                        cust.sman        LE cEndSalesRep      AND
                        ar-cashl.cust-no GE cStartShipToNo    AND
                        ar-cashl.cust-no LE cEndShipToNo      AND
                        cust.zip         GE cFromSoldZip      AND
                        cust.zip         LE cToSoldZip        THEN lExc = NO.

                IF AVAILABLE tt-report THEN 
                DO:
                    tt-report.key-07 = tt-report.key-03.

                    IF lExc THEN DELETE tt-report.

                    ELSE
                        IF tt-report.key-02 EQ "" AND cSorts NE "I" THEN
                            IF cSorts EQ "C" THEN
                                tt-report.key-02 = tt-report.key-05.
                            ELSE
                                ASSIGN
                                    tt-report.key-02 = tt-report.key-03
                                    tt-report.key-03 = tt-report.key-05.
                END.     
            END.
    END.

    FOR EACH xreport NO-LOCK: /* Strange problem of tt-report*/
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",

        FIRST cust
        WHERE cust.company EQ cCompany
        AND cust.cust-no EQ tt-report.key-09
        NO-LOCK

        BREAK BY tt-report.key-01
        BY tt-report.key-02
        BY tt-report.key-03
        BY tt-report.key-04
        BY tt-report.key-05

        TRANSACTION:
        
        CREATE w-data.
        ASSIGN
            w-data.i-no   = tt-report.key-07
            w-data.inv-no = int(tt-report.key-04)
            w-data.rec-id = tt-report.rec-id.

        FIND FIRST ar-invl
            WHERE RECID(ar-invl) EQ w-data.rec-id
            NO-LOCK NO-ERROR.

        cBolWhs = "" .
        dtBolDate = ? .

        FOR EACH oe-boll WHERE oe-boll.company = ar-invl.company
            AND oe-boll.b-no = ar-invl.b-no NO-LOCK:

            cBolWhs = oe-boll.loc .
            dtBolDate = oe-boll.bol-date .
            IF cBolWhs NE "" THEN LEAVE .

        END.

        IF AVAILABLE ar-invl THEN 
        DO:
            FIND ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no NO-LOCK.
            ASSIGN
                dtInvDate   = ar-inv.inv-date
                iOrdNo    = ar-invl.ord-no
                dUnitPrice   = ar-invl.unit-pr
                cUom    = ar-invl.pr-uom
                iShipQty = ar-invl.ship-qty
                dAmount = ar-invl.amt
                dDiscount   = ar-invl.disc
                dPct    = 1.

            IF tt-report.key-10 EQ "FREIGHT" THEN
                ASSIGN
                    dUnitPrice   = ar-inv.freight
                    cUom    = ""
                    iShipQty = 0
                    dAmount = ar-inv.freight
                    dDiscount   = 0.

            ELSE 
            DO:
                DO i = 1 TO 3:
                    IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                        ASSIGN
                            dPct = ar-invl.s-pct[i] / 100
                            i     = 3.
                END.

                IF dPct EQ 0 THEN
                DO i = 1 TO 3:
                    IF i EQ 1 THEN j = 0.
                    IF ar-invl.sman[i] NE "" THEN j = j + 1.
                    IF i EQ 3 THEN dPct = 1 / j.
                END.

                IF dPct LE 0 OR dPct EQ ? THEN dPct = 1.
            END.

            dAmount = dAmount * dPct.
        END.

        ELSE 
        DO:
            FIND FIRST ar-cashl
                WHERE RECID(ar-cashl) EQ w-data.rec-id
                NO-LOCK NO-ERROR.

            IF AVAILABLE ar-cashl THEN 
            DO:
                FIND FIRST ar-cash WHERE ar-cash.c-no EQ ar-cashl.c-no NO-LOCK.

                ASSIGN
                    dtInvDate   = ar-cash.check-date
                    iOrdNo    = 0
                    dUnitPrice   = ar-cashl.amt-paid - ar-cashl.amt-disc
                    cUom    = ""
                    iShipQty = 0
                    dAmount = ar-cashl.amt-paid - ar-cashl.amt-disc
                    dDiscount   = 0.

                RELEASE ar-invl.

                RUN salrep/getoeret.p (ROWID(ar-cashl), BUFFER oe-retl).

                IF AVAILABLE oe-retl THEN 
                DO:
                    ASSIGN
                        iOrdNo    = oe-retl.ord-no
                        dUnitPrice   = oe-retl.unit-pr
                        cUom    = oe-retl.uom
                        iShipQty = - oe-retl.tot-qty-return.

                    FIND FIRST ar-invl
                        WHERE ar-invl.company EQ cCompany
                        AND ar-invl.cust-no EQ ar-cash.cust-no
                        AND ar-invl.inv-no  EQ ar-cashl.inv-no
                        AND ar-invl.i-no    EQ oe-retl.i-no
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE ar-invl THEN 
                    DO:
                        /* Added for decimal problem */
                        ASSIGN 
                            dUnitPrice = ar-invl.unit-pr.

                        DO i = 1 TO 3:
                            IF ar-invl.sman[i] EQ tt-report.key-06 THEN
                                ASSIGN
                                    dPct = ar-invl.s-pct[i] / 100
                                    i     = 3.
                        END.

                        IF dPct EQ 0 THEN
                        DO i = 1 TO 3:
                            IF i EQ 1 THEN j = 0.
                            IF ar-invl.sman[i] NE "" THEN j = j + 1.
                            IF i EQ 3 THEN dPct = 1 / j.
                        END.

                        IF dPct LE 0 OR dPct EQ ? THEN dPct = 1.
                        ASSIGN
                            dAmount = dAmount * dPct
                            dDiscount   = ar-invl.disc.
                    END.
                END.
            END.
        END.

        IF lDiscountPrice AND dDiscount NE 0 THEN dUnitPrice = dUnitPrice * (100 - dDiscount) / 100.

        FIND FIRST shipto
            WHERE shipto.company EQ cust.company
            AND shipto.cust-no EQ cust.cust-no
            AND shipto.ship-id EQ tt-report.key-05
            NO-LOCK NO-ERROR.
        
        ASSIGN
            cCity  = cust.city
            cState = cust.state.

        FIND FIRST itemfg
            WHERE itemfg.company EQ cust.company
            AND itemfg.i-no    EQ w-data.i-no
            NO-LOCK NO-ERROR.

        FIND FIRST sman
            WHERE sman.company EQ cust.company
            AND sman.sman    EQ tt-report.key-06
            NO-LOCK NO-ERROR.

        ASSIGN
            cProcat = IF AVAILABLE itemfg THEN itemfg.procat ELSE ""
            cSname = IF AVAILABLE sman THEN sman.sname ELSE tt-report.key-06
            iYear  = YEAR(dtInvDate)
            iMonth = MONTH(dtInvDate).

        CREATE ttSalesAnalysis.
            ASSIGN
                ttSalesAnalysis.cCustNo      = cust.cust-no
                ttSalesAnalysis.cShipTo      = tt-report.key-05
                ttSalesAnalysis.cCity        = cCity
                ttSalesAnalysis.cState       = cState
                ttSalesAnalysis.cSname       = cSname
                ttSalesAnalysis.iInvNo       = w-data.inv-no
                ttSalesAnalysis.iMonth       = iMonth
                ttSalesAnalysis.iYear        = iYear
                ttSalesAnalysis.cInvDate     = IF dtInvDate NE ? THEN STRING(dtInvDate,"99/99/99") ELSE ""
                ttSalesAnalysis.cFgItem      = w-data.i-no
                ttSalesAnalysis.cProCode     = cProcat
                ttSalesAnalysis.cOrderNo     = STRING(iOrdNo,">>>>>>>>")
                ttSalesAnalysis.iQtyShip     = iShipQty
                ttSalesAnalysis.dUnitPrice   = dUnitPrice
                ttSalesAnalysis.cUom         = cUom
                ttSalesAnalysis.dInvAmt      = dAmount
                ttSalesAnalysis.cBolWhse     = cBolWhs
                ttSalesAnalysis.cShipToName  = IF AVAILABLE shipto THEN STRING(shipto.ship-name) ELSE "" 
                ttSalesAnalysis.cShipAdd1    = IF AVAILABLE shipto THEN STRING(shipto.ship-addr[1]) ELSE ""
                ttSalesAnalysis.cShipAdd2    = IF AVAILABLE shipto THEN STRING(shipto.ship-addr[2]) ELSE ""
                ttSalesAnalysis.cShipAdd3    = IF AVAILABLE shipto THEN STRING(shipto.spare-char-3) ELSE ""
                ttSalesAnalysis.cShipCity    = IF AVAILABLE shipto THEN STRING(shipto.ship-city) ELSE ""
                ttSalesAnalysis.cShipState   = IF AVAILABLE shipto THEN STRING(shipto.ship-state) ELSE ""
                ttSalesAnalysis.cShipZip     = IF AVAILABLE shipto THEN STRING(shipto.ship-zip) ELSE ""
                ttSalesAnalysis.cBolDate     = IF dtBolDate NE ? THEN STRING(dtBolDate) ELSE ""
                iCount = iCount + 1
                .      

        DELETE w-data.
        
        IF lProgressBar THEN
            RUN spProgressBar (cProgressBar, iCount, ?).
    END.
    
END PROCEDURE.

PROCEDURE create-report :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID            NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE tt-report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE tt-report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE tt-report.key-10 NO-UNDO.


    CREATE xreport.

    ASSIGN
        lExc           = NO
        xreport.term-id = ""
        xreport.rec-id  = ip-recid
        xreport.key-01  = TRIM(IF cSorts EQ "Z" THEN cust.zip ELSE "") +
                   tt-report.key-09
        xreport.key-02  = IF cSorts EQ "H" THEN (cSoldZip + cShipId) ELSE
                   IF cSorts EQ "S" THEN cShipId ELSE
                   IF cSorts EQ "O" THEN
                     STRING(ar-invl.ord-no,"999999") ELSE ""
        xreport.key-03  = ip-key-03
        xreport.key-04  = ip-key-04
        xreport.key-05  = cShipId
        xreport.key-06  = cSmanNo
        xreport.key-07  = xreport.key-03
        xreport.key-09  = tt-report.key-09
        xreport.key-10  = ip-key-10.

    IF xreport.key-02 EQ "" AND cSorts NE "I" THEN
        IF cSorts EQ "C" THEN
            xreport.key-02 = xreport.key-05.
        ELSE
            ASSIGN
                xreport.key-02 = xreport.key-03
                xreport.key-03 = xreport.key-05.

END PROCEDURE.

PROCEDURE create-report1 :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-recid  AS   RECID         NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-03 LIKE report.key-03 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-04 LIKE report.key-04 NO-UNDO.
    DEFINE INPUT PARAMETER ip-key-10 LIKE report.key-10 NO-UNDO.


    DO i = 1 TO 3:
        cSmanNo = IF ar-invl.sman[i] EQ "" AND i EQ 1 THEN cust.sman
        ELSE ar-invl.sman[i].

        IF  cSmanNo   LT cStartSalesRep OR
            cSmanNo   GT cEndSalesRep   OR
            (i NE 1 AND
            (cSmanNo EQ "" OR ar-invl.s-pct[i] EQ 0)) THEN NEXT.

        RUN create-report (ip-recid, ip-key-03, ip-key-04, ip-key-10).
        LEAVE.
    END.



END PROCEDURE.

PROCEDURE ship-data :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    RELEASE shipto.
    IF ar-inv.ship-id NE "" THEN
        FIND FIRST shipto
            WHERE shipto.company EQ cCompany
            AND shipto.cust-no EQ ar-inv.cust-no
            AND shipto.ship-id EQ ar-inv.ship-id
            NO-LOCK NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN
            cShipId = ar-inv.ship-id
            cSoldZip = shipto.ship-zip.

    ELSE
        IF ar-inv.sold-id NE "" THEN
            ASSIGN
                cShipId = ar-inv.sold-id
                cSoldZip = ar-inv.sold-zip.

        ELSE
            ASSIGN
                cShipId = ar-inv.cust-no
                cSoldZip = cust.zip.

END PROCEDURE.

{AOA/dynBL/pBuildCustList.i}
