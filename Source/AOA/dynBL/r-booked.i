/* AOA/dynBL/r-booked.i - used in AOA/dynBL/r-booked.p & AOA/dynBL/recappc.p */

/* local variables */
DEFINE VARIABLE c-result AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPerDays AS INTEGER   NO-UNDO EXTENT 2.
DEFINE VARIABLE iCount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotal   AS INTEGER   NO-UNDO.

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.

/* ************************  Function Prototypes ********************** */

FUNCTION fGetRoutingForJob RETURNS CHARACTER 
    (  ) FORWARD.

FUNCTION fGetInksForJob RETURNS CHARACTER
    (  ) FORWARD.

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cCode          AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE cSalesRep      AS CHARACTER        NO-UNDO.
    DEFINE VARIABLE dPct           AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dTotalSqft   LIKE itemfg.t-sqft    NO-UNDO.
    DEFINE VARIABLE dTotTons       AS DECIMAL          NO-UNDO.
    DEFINE VARIABLE dtMDate        AS DATE             NO-UNDO.
    DEFINE VARIABLE dtTrandate   LIKE dtStartOrderDate NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE jdx            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE kdx            AS INTEGER          NO-UNDO.
    DEFINE VARIABLE ii             AS INTEGER          NO-UNDO.
    DEFINE VARIABLE lExclude       AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE lMisc          AS LOGICAL          NO-UNDO.
    DEFINE VARIABLE lPrtSqft       AS LOGICAL          NO-UNDO.

    DEFINE BUFFER bOEOrdl FOR oe-ordl.
    DEFINE BUFFER bOERel  FOR oe-rel.

    FOR EACH company NO-LOCK 
        WHERE company.company GE cStartCompany
          AND company.company LE cEndCompany
        :
        FIND FIRST period NO-LOCK
             WHERE period.company EQ company.company
               AND period.pst     LE dtEndOrderDate
               AND period.pend    GE dtEndOrderDate
             NO-ERROR.
        ASSIGN
            cocode     = company.company
            dtTrandate = IF AVAILABLE period THEN MINIMUM(dtStartOrderDate,period.pst)
                                                     ELSE dtStartOrderDate
            .
        FOR EACH oe-ord NO-LOCK
            WHERE oe-ord.company  EQ company.company
              AND oe-ord.cust-no  GE cStartCustNo
              AND oe-ord.cust-no  LE cEndCustNo
              AND oe-ord.ord-date GE dtTrandate
              AND oe-ord.ord-date LE dtEndOrderDate
              AND oe-ord.due-date GE dtStartDueDate
              AND oe-ord.due-date LE dtEndDueDate
              AND oe-ord.type     NE "T"
              AND oe-ord.stat     NE "D"
            BY oe-ord.company 
            BY oe-ord.ord-date 
            BY oe-ord.ord-no
            :
            IF lCustList AND
               NOT CAN-FIND(FIRST ttCustList
                            WHERE ttCustList.cust-no EQ oe-ord.cust-no
                              AND ttCustList.log-fld EQ TRUE) THEN
            NEXT.
            IF lExcludeTransferReleasesOrders THEN DO:
                IF oe-ord.type EQ "T" THEN NEXT.
                cCode = "".
                FOR EACH oe-rel FIELDS(r-no) NO-LOCK 
                   WHERE oe-rel.company EQ oe-ord.company 
                     AND oe-rel.ord-no  EQ oe-ord.ord-no
                     AND oe-rel.s-code EQ "T"
                    :
                    cCode = "T".
                    LEAVE.
                END. /* each oe-rel */
                IF cCode EQ "T" THEN NEXT.
            END.  /* if lExcludeTransferReleasesOrders */
            FOR EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ oe-ord.company
                  AND oe-ordl.ord-no  EQ oe-ord.ord-no
                  AND oe-ordl.part-no GE cStartCustPart
                  AND oe-ordl.part-no LE cEndCustPart
                  AND (oe-ordl.is-a-component EQ NO
                   OR lExcludeSetComponents EQ NO),
                FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ oe-ordl.company
                  AND itemfg.i-no    EQ oe-ordl.i-no
                  AND itemfg.proCat  GE cStartProCat
                  AND itemfg.proCat  LE cEndProCat
                BREAK BY oe-ordl.line
                :
                FIND FIRST oe-rel NO-LOCK
                     WHERE oe-rel.company EQ oe-ordl.company
                       AND oe-rel.ord-no  EQ oe-ordl.ord-no
                       AND oe-rel.i-no    EQ oe-ordl.i-no
                       AND oe-rel.line    EQ oe-ordl.line
                     NO-ERROR.    
                IF NOT AVAILABLE oe-rel THEN DO:
                    FIND FIRST bOEOrdl NO-LOCK
                         WHERE bOEOrdl.company EQ oe-ordl.company
                           AND bOEOrdl.ord-no  EQ oe-ordl.ord-no
                           AND bOEOrdl.is-a-component EQ YES
                         NO-ERROR.                
                    IF AVAILABLE bOEOrdl THEN
                    FIND FIRST bOERel NO-LOCK
                         WHERE bOERel.company EQ bOEOrdl.company
                           AND bOERel.ord-no  EQ bOEOrdl.ord-no
                           AND bOERel.i-no    EQ bOEOrdl.i-no
                           AND bOERel.line    EQ bOEOrdl.line
                         NO-ERROR.    
                    IF AVAILABLE bOERel AND
                       NOT(bOERel.spare-char-1 GE cStartShipFromNo
                       AND bOERel.spare-char-1 LE cEndShipFromNo) THEN NEXT.
                    ELSE IF NOT AVAILABLE bOERel AND NOT lLOrdWithNoRel THEN NEXT.                
                END.
                ELSE DO:   
                    IF AVAILABLE oe-rel AND
                       NOT (oe-rel.spare-char-1 GE cStartShipFromNo
                       AND oe-rel.spare-char-1  LE cEndShipFromNo) THEN NEXT.
                    ELSE IF NOT AVAILABLE oe-rel AND NOT lLOrdWithNoRel THEN NEXT.    
                END. /* else */
                lExclude = YES.
                DO idx = 1 TO 3:
                    IF lExclude AND
                       oe-ordl.s-man[idx] GE cStartSalesRep AND
                       oe-ordl.s-man[idx] LE cEndSalesRep THEN
                    lExclude = NO.
                END.  /* do idx */
                IF lExclude THEN NEXT.
                lMisc = FALSE.
                DO idx = 1 TO 3:
                    IF lMisc THEN LEAVE.
                    IF oe-ordl.s-man[idx] LT cStartSalesRep OR
                       oe-ordl.s-man[idx] GT cEndSalesRep THEN NEXT.
                    /* if no salesman number then assign to misc, ie, blank no */
                    IF idx EQ 1 AND
                       oe-ordl.s-man[1] EQ "" AND
                       oe-ordl.s-man[2] EQ "" AND
                       oe-ordl.s-man[3] EQ "" THEN
                    cSalesRep = "MISC".
                    ELSE   /* if blank salesman # then ignore */
                        IF oe-ordl.s-man[idx] EQ "" THEN NEXT.
                        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                        ELSE cSalesRep = oe-ordl.s-man[idx].
                    IF oe-ord.ord-date GE dtStartOrderDate AND
                       oe-ord.ord-date LE dtEndOrderDate THEN DO:
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = cSalesRep
                            tt-report.key-02  = STRING(oe-ord.ord-no,">>>>>>>>>>")
                            tt-report.key-03  = STRING(idx,"9")
                            tt-report.key-04  = IF AVAILABLE oe-rel THEN STRING(oe-rel.spare-char-1)
                                           ELSE IF AVAILABLE bOERel THEN STRING(bOERel.spare-char-1)
                                           ELSE ""
                            tt-report.rec-id  = RECID(oe-ordl)
                            iTotal            = iTotal + 1
                            .           
                        IF lProgressBar THEN
                        RUN spProgressBar (cProgressBar, iTotal, ?).
                    END.  /* if oe-ord.ord-date */
                    RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dTotalSqft).
                    
                    ASSIGN
                        dPct         = oe-ordl.s-pct[idx] / 100
                        dOrdQty      = oe-ordl.qty * dPct
                        dTotalSqft   = dTotalSqft * dOrdQty / 1000
                        dTotTons     = itemfg.weight-100 * dOrdQty / 100 / 2000
                        dPriceAmount = oe-ordl.t-price * dPct
                        .
                    FIND FIRST ttRecapProductCategory NO-LOCK
                         WHERE ttRecapProductCategory.proCat EQ IF AVAILABLE itemfg THEN itemfg.proCat ELSE ""
                         NO-ERROR.
                    IF NOT AVAILABLE ttRecapProductCategory THEN DO:
                        CREATE ttRecapProductCategory.
                        ASSIGN
                            ttRecapProductCategory.proCat    = IF AVAILABLE itemfg THEN itemfg.proCat ELSE ""
                            ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1
                            .
                        FIND FIRST fgcat NO-LOCK
                             WHERE fgcat.company EQ oe-ordl.company
                               AND fgcat.procat  EQ ttRecapProductCategory.proCat
                             NO-ERROR.
                        IF AVAILABLE fgcat THEN
                        ttRecapProductCategory.catDscr = fgcat.dscr.
                    END. /* not avail ttRecapProductCategory */
                    ELSE ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1.
                    ASSIGN
                        jdx = IF oe-ord.ord-date GE dtStartOrderDate AND
                                 oe-ord.ord-date LE dtEndOrderDate THEN 1 ELSE 2
                        kdx = IF AVAILABLE period AND
                                 oe-ord.ord-date GE period.pst AND
                                 oe-ord.ord-date LE period.pend THEN 2 ELSE 1
                        .
                   IF jdx LE kdx THEN
                   DO ii = jdx TO kdx:
                       IF ii EQ 1 THEN
                       ASSIGN
                           ttRecapProductCategory.sqFtCurrent   = ttRecapProductCategory.sqFtCurrent   + dTotalSqft
                           ttRecapProductCategory.tonsCurrent   = ttRecapProductCategory.tonsCurrent   + dTotTons
                           ttRecapProductCategory.amountCurrent = ttRecapProductCategory.amountCurrent + dPriceAmount
                           .
                       IF ii EQ 2 THEN
                       ASSIGN
                           ttRecapProductCategory.sqFtPeriod   = ttRecapProductCategory.sqFtPeriod   + dTotalSqft
                           ttRecapProductCategory.tonsPeriod   = ttRecapProductCategory.tonsPeriod   + dTotTons
                           ttRecapProductCategory.amountPeriod = ttRecapProductCategory.amountPeriod + dPriceAmount
                           .
                   END.  /* if jdx le kdx then */
                END.  /* do idx = 1 to 3... */
                IF oe-ord.ord-date NE dtMDate THEN DO:
                    dtMDate = oe-ord.ord-date.
                    IF oe-ord.ord-date GE dtStartOrderDate AND
                       oe-ord.ord-date LE dtEndOrderDate THEN
                    iPerDays[1] = iPerDays[1] + 1.
                    IF AVAILABLE period AND
                       oe-ord.ord-date GE period.pst AND
                       oe-ord.ord-date LE period.pend THEN
                    iPerDays[2] = iPerDays[2] + 1.
                END.  /*if oe-ord.ord-date ne dtMDate then do:*/
            END.  /*for each oe-ordl no-lock*/
            IF lIncludePrepMiscChg THEN
            FOR EACH oe-ordm NO-LOCK
                WHERE oe-ordm.company EQ oe-ord.company
                  AND oe-ordm.ord-no  EQ oe-ord.ord-no
                :
                lExclude = YES.
                DO idx = 1 TO 3:
                    IF lExclude AND
                        oe-ordm.s-man[idx] GE cStartSalesRep AND
                        oe-ordm.s-man[idx] LE cEndSalesRep THEN
                    lExclude = NO.
                END.  /* do i.. */
        
                IF lExclude THEN NEXT.
                /* At this point we have either 1, 2 or 3 valid salesman, in any  */
                /* combination of the array. */
                lMisc = FALSE.
                DO idx = 1 TO 3:
                    IF lMisc THEN LEAVE.
                    IF oe-ordm.s-man[idx] LT cStartSalesRep OR
                       oe-ordm.s-man[idx] GT cEndSalesRep THEN NEXT.
                    /* if no salesman number then assign to misc, ie, blank no */
                    IF idx EQ 1 AND
                        oe-ordm.s-man[1] EQ "" AND
                        oe-ordm.s-man[2] EQ "" AND
                        oe-ordm.s-man[3] EQ "" THEN cSalesRep = "MISC".
                    ELSE /* if blank salesman # then ignore */
                        IF oe-ordm.s-man[idx] EQ "" THEN NEXT.
                        /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                        ELSE cSalesRep = oe-ordm.s-man[idx].
                        IF oe-ord.ord-date GE dtStartOrderDate AND
                           oe-ord.ord-date LE dtEndOrderDate THEN DO:
                            CREATE tt-report.
                            ASSIGN
                                tt-report.term-id = ""
                                tt-report.key-01  = cSalesRep
                                tt-report.key-02  = STRING(oe-ord.ord-no,">>>>>>>>>>")
                                tt-report.key-03  = STRING(idx,"9")
                                tt-report.key-04  = IF AVAILABLE oe-rel THEN STRING(oe-rel.spare-char-1)
                                               ELSE IF AVAILABLE bOERel THEN STRING(bOERel.spare-char-1)
                                               ELSE ""
                                tt-report.rec-id  = RECID(oe-ordm)
                                iTotal            = iTotal + 1
                                .           
                            IF lProgressBar THEN
                            RUN spProgressBar (cProgressBar, iTotal, ?).
                        END.  /* if oe-ord.ord-date */
                        ASSIGN
                            dPct = oe-ordm.s-pct[idx] / 100
                            dPriceAmount = oe-ordm.amt * dPct
                            .
                        FIND FIRST ttRecapProductCategory NO-LOCK
                             WHERE ttRecapProductCategory.proCat EQ "P/M"
                             NO-ERROR.
                        IF NOT AVAILABLE ttRecapProductCategory THEN DO:
                            CREATE ttRecapProductCategory.
                            ASSIGN
                                ttRecapProductCategory.proCat    = "P/M"
                                ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1
                                ttRecapProductCategory.dscr      = "Prep/Misc"
                                .
                            FIND FIRST fgcat NO-LOCK
                                 WHERE fgcat.company EQ oe-ord.company
                                   AND fgcat.procat  EQ ttRecapProductCategory.proCat
                                 NO-ERROR.
                            IF AVAILABLE fgcat THEN
                            ttRecapProductCategory.catDscr = fgcat.dscr.
                        END.  /* not avail ttRecapProductCategory */ 
                        ELSE ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1.
                        ASSIGN
                            jdx = IF oe-ord.ord-date GE dtStartOrderDate AND
                                     oe-ord.ord-date LE dtEndOrderDate THEN 1 ELSE 2
                            kdx = IF AVAILABLE period AND
                                     oe-ord.ord-date GE period.pst AND
                                     oe-ord.ord-date LE period.pend THEN 2 ELSE 1
                            .
                        /* We cannot disturb loop variable idx from within loop,so use ii: */
                        IF jdx LE kdx THEN DO ii = jdx TO kdx:
                            IF ii EQ 1 THEN
                            ttRecapProductCategory.amountCurrent = ttRecapProductCategory.amountCurrent + dPriceAmount.
                            IF ii EQ 2 THEN
                            ttRecapProductCategory.amountPeriod  = ttRecapProductCategory.amountPeriod  + dPriceAmount.
                        END.  /* IF jdx LE kdx */
                END. /* do idx = 1 to 3: */
            END. /* each oe-ordm */   
        END. /* each oe-ord */
    END. /* each oe-ctrl */

    &IF DEFINED(subjectID) NE 0 &THEN
    lPrtSqft = CAN-FIND(FIRST dynValueColumn
                        WHERE dynValueColumn.subjectID    EQ dynParamValue.subjectID
                          AND dynValueColumn.user-id      EQ dynParamValue.user-id
                          AND dynValueColumn.prgmName     EQ dynParamValue.prgmName
                          AND dynValueColumn.paramValueID EQ dynParamValue.paramValueID
                          AND dynValueColumn.colName      EQ "sqFt").
    &ELSE
    lPrtSqft = CAN-DO(cSelectedColumns,"sqFt").
    &ENDIF
    RUN pOrdersBooked1 (
        lPrtSqft,
        lPrintOrderUnderPct,
        lPrintOrderOverPct,
        iUnderValue,
        iOverValue
        ).
    RUN pOrdersBooked2 (
        lPrtSqft,
        lPrintOrderUnderPct,
        lPrintOrderOverPct,
        iUnderValue,
        iOverValue
        ).
    FOR EACH ttRecapProductCategory
        WHERE ttRecapProductCategory.proCat LT "|"
        BREAK BY ttRecapProductCategory.proCat
        :
        ASSIGN 
            ttRecapProductCategory.priceMSFCurrent = ttRecapProductCategory.amountCurrent / ttRecapProductCategory.sqFtCurrent
            ttRecapProductCategory.priceTonCurrent = ttRecapProductCategory.amountCurrent / ttRecapProductCategory.sqFtCurrent
            ttRecapProductCategory.priceMSFPeriod  = ttRecapProductCategory.amountPeriod  / ttRecapProductCategory.sqFtPeriod
            ttRecapProductCategory.priceTonPeriod  = ttRecapProductCategory.amountPeriod  / ttRecapProductCategory.tonsPeriod
            .        
        IF ttRecapProductCategory.priceMSFCurrent EQ ? THEN
        ttRecapProductCategory.priceMSFCurrent = 0.
        IF ttRecapProductCategory.priceTonCurrent EQ ? THEN
        ttRecapProductCategory.priceTonCurrent = 0.
        IF ttRecapProductCategory.priceMSFPeriod EQ ? THEN
        ttRecapProductCategory.priceMSFPeriod = 0.
        IF ttRecapProductCategory.priceTonPeriod EQ ? THEN
        ttRecapProductCategory.priceTonPeriod = 0.

        &IF "{&ttTempTable}" EQ "ttOrdersBooked" &THEN
        ACCUMULATE 
            ttRecapProductCategory.amountCurrent (TOTAL)
            ttRecapProductCategory.sqFtCurrent   (TOTAL)
            ttRecapProductCategory.tonsCurrent   (TOTAL)
            ttRecapProductCategory.amountPeriod  (TOTAL)
            ttRecapProductCategory.sqFtPeriod    (TOTAL)
            ttRecapProductCategory.tonsPeriod    (TOTAL)
            .
        IF LAST(ttRecapProductCategory.proCat) THEN DO:
            CREATE bttRecapProductCategory.
            ASSIGN
                bttRecapProductCategory.proCat          = "|"
                bttRecapProductCategory.catDscr         = "Totals"
                bttRecapProductCategory.amountCurrent   = (ACCUM TOTAL ttRecapProductCategory.amountCurrent)
                bttRecapProductCategory.sqFtCurrent     = (ACCUM TOTAL ttRecapProductCategory.sqFtCurrent)
                bttRecapProductCategory.tonsCurrent     = (ACCUM TOTAL ttRecapProductCategory.tonsCurrent)
                bttRecapProductCategory.priceMSFCurrent = (ACCUM TOTAL ttRecapProductCategory.amountCurrent)
                                                        / (ACCUM TOTAL ttRecapProductCategory.sqFtCurrent)
                bttRecapProductCategory.priceTonCurrent = (ACCUM TOTAL ttRecapProductCategory.amountCurrent)
                                                        / (ACCUM TOTAL ttRecapProductCategory.tonsCurrent)
                bttRecapProductCategory.amountPeriod    = (ACCUM TOTAL ttRecapProductCategory.amountPeriod)
                bttRecapProductCategory.sqFtPeriod      = (ACCUM TOTAL ttRecapProductCategory.sqFtPeriod)
                bttRecapProductCategory.tonsPeriod      = (ACCUM TOTAL ttRecapProductCategory.tonsPeriod)
                bttRecapProductCategory.priceMSFPeriod  = (ACCUM TOTAL ttRecapProductCategory.amountPeriod)
                                                        / (ACCUM TOTAL ttRecapProductCategory.sqFtPeriod)
                bttRecapProductCategory.priceTonPeriod  = (ACCUM TOTAL ttRecapProductCategory.amountPeriod)
                                                        / (ACCUM TOTAL ttRecapProductCategory.tonsPeriod)
                .
            IF bttRecapProductCategory.priceMSFCurrent EQ ? THEN
            bttRecapProductCategory.priceMSFCurrent = 0.
            IF bttRecapProductCategory.priceTonCurrent EQ ? THEN
            bttRecapProductCategory.priceTonCurrent = 0.
            IF bttRecapProductCategory.priceMSFPeriod EQ ? THEN
            bttRecapProductCategory.priceMSFPeriod = 0.
            IF bttRecapProductCategory.priceTonPeriod EQ ? THEN
            bttRecapProductCategory.priceTonPeriod = 0.
            CREATE bttRecapProductCategory.
            ASSIGN
                bttRecapProductCategory.proCat         = "||"
                bttRecapProductCategory.catDscr        = "Average"
                bttRecapProductCategory.numDaysCurrent = iPerDays[1]
                bttRecapProductCategory.amountCurrent  = (ACCUM TOTAL ttRecapProductCategory.amountCurrent)
                                                       / bttRecapProductCategory.numDaysCurrent
                bttRecapProductCategory.numDaysPeriod  = iPerDays[2]
                bttRecapProductCategory.amountPeriod   = (ACCUM TOTAL ttRecapProductCategory.amountPeriod)
                                                       / bttRecapProductCategory.numDaysPeriod
                .
            IF bttRecapProductCategory.amountCurrent EQ ? THEN
            bttRecapProductCategory.amountCurrent = 0.
            IF bttRecapProductCategory.amountPeriod EQ ? THEN
            bttRecapProductCategory.amountPeriod = 0.
        END. // if last
        &ENDIF
    END. // each ttRecapProductCategory

END PROCEDURE.

PROCEDURE pOrdersBooked1:
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE idx          AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE dProfitPer   AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dQM          AS   DECIMAL          NO-UNDO.
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, iTotal).
        FIND FIRST oe-ordm NO-LOCK
             WHERE RECID(oe-ordm) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordm THEN
        ASSIGN
            idx          = INTEGER(tt-report.key-03)
            dPct         = oe-ordm.s-pct[idx] / 100
            dPriceAmount = oe-ordm.amt * dPct
            dRevenue     = dPriceAmount
            dProfitPer   = (dRevenue - (oe-ordm.cost * dPct)) / dRevenue * 100
            .
        ELSE DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE RECID(oe-ordl) EQ tt-report.rec-id
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            ASSIGN
                idx          = INTEGER(tt-report.key-03)
                dPct         = oe-ordl.s-pct[idx] / 100
                dOrdQty      = oe-ordl.qty * dPct
                dPriceAmount = oe-ordl.t-price * dPct
                dQM          = oe-ordl.qty / 1000
                dRevenue     = dPriceAmount
                dProfitPer   = (dRevenue - (oe-ordl.cost * dQM)) / dRevenue * 100
                .
        END. /* else do */
        
        IF dProfitPer EQ ? THEN dProfitPer = 0.
        
        IF iplPrintOrderUnderPct AND iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue AND dProfitPer LE ipiOverValue THEN DELETE tt-report.
        END.
        ELSE IF iplPrintOrderUnderPct AND NOT iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue THEN DELETE tt-report.
        END.
        ELSE IF iplPrintOrderOverPct AND NOT iplPrintOrderUnderPct THEN DO:
            IF dProfitPer LE ipiOverValue THEN DELETE tt-report.
        END.
    END.  /* for each tt-report */
END PROCEDURE.

PROCEDURE pOrdersBooked2:
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE cPrevOrder   AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE idx          AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dTons        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dSqft        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE iLines       AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dQM          AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dRevenue     LIKE oe-ordl.t-price  NO-UNDO.
    DEFINE VARIABLE dMSFPrice    AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPricePerTon AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dProfitPer   AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dMargin      AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dCost        AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE cUOM         AS   CHARACTER        NO-UNDO.

    DEFINE BUFFER bItemFG FOR itemfg.
    
    iCount = 0.
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
        :
        iCount = iCount + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, iTotal).
        FIND FIRST oe-ordm NO-LOCK
             WHERE RECID(oe-ordm) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordm THEN DO:
            FIND FIRST oe-ord OF oe-ordm NO-LOCK.
            ASSIGN
                idx          = INTEGER(tt-report.key-03)
                dPct         = oe-ordm.s-pct[idx] / 100
                dPriceAmount = oe-ordm.amt * dPct
                .
            CREATE w-data.
            ASSIGN
                w-data.sman       = tt-report.key-01
                w-data.ord-no     = oe-ordm.ord-no
                w-data.line       = oe-ordm.line
                w-data.misc       = YES
                w-data.proCat     = "P/M"
                w-data.qty        = 0
                w-data.sqft       = 0
                w-data.t-sqft     = 0
                w-data.t-tons     = 0
                w-data.item-n     = oe-ordm.dscr
                w-data.cost       = oe-ordm.cost * dPct
                w-data.price      = dPriceAmount
                w-data.revenue    = dPriceAmount
                w-data.comm       = oe-ordm.s-comm[idx]
                w-data.shp-qty    = 0
                w-data.cShip-from = tt-report.key-04
                .
            FIND FIRST prep NO-LOCK
                 WHERE prep.company EQ oe-ordm.company
                   AND prep.code    EQ oe-ordm.charge
                 NO-ERROR.
            IF AVAILABLE prep THEN
            w-data.proCat = IF prep.fgcat NE "" THEN prep.fgcat ELSE "P".
            ELSE w-data.proCat = "M".
        END. /* avail oe-ordm */
        ELSE DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE RECID(oe-ordl) EQ tt-report.rec-id
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN DO:
                FIND FIRST oe-ord OF oe-ordl NO-LOCK.
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ oe-ordl.company
                       AND itemfg.i-no    EQ oe-ordl.i-no
                     NO-ERROR.
                ASSIGN
                    idx          = INTEGER(tt-report.key-03)
                    dPct         = oe-ordl.s-pct[idx] / 100
                    dOrdQty      = oe-ordl.qty * dPct
                    dPriceAmount = oe-ordl.t-price * dPct
                    dTons        = IF AVAILABLE itemfg THEN (itemfg.weight-100 * dOrdQty / 100 / 2000) ELSE 0
                    .
                IF AVAILABLE itemfg THEN 
                    RUN fg/GetFGArea.p (ROWID(itemfg), "MSF", OUTPUT dSqft).  
                ELSE 
                    dSqft = 0.          
                dSqft = dSqft * dOrdQty.
                
                CREATE w-data.
                ASSIGN
                    w-data.sman       = tt-report.key-01
                    w-data.ord-no     = oe-ordl.ord-no
                    w-data.line       = oe-ordl.line
                    w-data.misc       = NO
                    iLines            = iLines + 1
                    dQM               = oe-ordl.qty / 1000
                    w-data.proCat     = IF AVAILABLE itemfg THEN itemfg.proCat ELSE ""
                    w-data.item-n     = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
                    w-data.qty        = dOrdQty
                    w-data.margin     = oe-ordl.q-qty
                    w-data.shp-qty    = oe-ordl.ship-qty
                    w-data.cShip-from =  tt-report.key-04
                    .
                IF NOT oe-ordl.is-a-component THEN
                ASSIGN
                    w-data.sqft    = IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 0
                    w-data.t-sqft  = dSqft
                    w-data.t-tons  = dTons
                    w-data.price   = oe-ordl.price
                    w-data.revenue = dPriceAmount
                    w-data.cost    = oe-ordl.cost * dQM
                    w-data.comm    = oe-ordl.s-comm[idx]
                    .
            END. /* avail oe-ordl */
        END. /* else do */

        FIND FIRST oe-ordl NO-LOCK
             WHERE RECID(oe-ordl) EQ tt-report.rec-id
             NO-ERROR.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-ordl.company
               AND oe-ord.ord-no  EQ w-data.ord-no
             NO-ERROR.
        FIND cust OF oe-ord NO-LOCK NO-ERROR.
        
        ASSIGN
            dRevenue     = w-data.revenue
            dMSFPrice    = dRevenue / w-data.t-sqft
            dPricePerTon = dRevenue / w-data.t-tons
            dProfitPer   = (dRevenue - w-data.cost) / dRevenue * 100
            dMargin      = w-data.margin
            dCost        = ?
            .
        IF dMSFPrice    EQ ? THEN dMSFPrice    = 0.
        IF dPricePerTon EQ ? THEN dPricePerTon = 0.
        IF dProfitPer   EQ ? THEN dProfitPer   = 0.
        IF dMargin      EQ ? THEN dMargin      = 0.
        
        ACCUMULATE
            w-data.t-sqft (TOTAL BY tt-report.key-01)
            w-data.t-tons (TOTAL BY tt-report.key-01)
            dRevenue      (TOTAL BY tt-report.key-01)
            w-data.cost   (TOTAL BY tt-report.key-01)
            .
        IF AVAILABLE oe-ordl THEN DO:
            RELEASE eb.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ oe-ordl.company
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company   EQ oe-ordl.company
                   AND po-ordl.i-no      EQ oe-ordl.i-no
                   AND po-ordl.po-no     EQ oe-ordl.po-no-po
                   AND po-ordl.item-type EQ NO
                 USE-INDEX item-ordno
                 NO-ERROR.
            IF AVAILABLE po-ordl AND oe-ordl.po-no-po NE 0 THEN DO:
                ASSIGN
                    cUOM  = IF po-ordl.cons-uom NE "" THEN po-ordl.cons-uom ELSE "M"
                    dCost = po-ordl.cons-cost
                    .
                IF cUOM NE "M" THEN
                RUN sys/ref/convcuom.p(cUOM, "M", 0, 0, 0, 0, dCost, OUTPUT dCost).
            END. // if avail po-ordl

            IF dCost EQ ? THEN
            dCost = oe-ordl.cost.

            IF NOT AVAILABLE itemfg OR itemfg.die-no EQ "" THEN
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ oe-ordl.company
                   AND eb.est-no   EQ oe-ordl.est-no
                   AND eb.stock-no EQ oe-ordl.i-no
                 NO-ERROR.
        END. /* avail oe-ordl  */
	
        RELEASE job.
        IF AVAILABLE oe-ord THEN DO:
            IF oe-ord.type EQ "T" AND oe-ord.pord-no GT 0 THEN
            cPrevOrder = STRING(oe-ord.pord-no).
            cPrevOrder = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2
                         ELSE STRING(oe-ord.pord-no).
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ oe-ord.company
                   AND job.job-no  EQ oe-ord.job-no
                   AND job.job-no2 EQ oe-ord.job-no2
                 NO-ERROR.
        END.

        IF iplPrintOrderUnderPct AND iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue AND dProfitPer LE ipiOverValue THEN NEXT.
        END.
        ELSE IF iplPrintOrderUnderPct AND NOT iplPrintOrderOverPct THEN DO:
            IF dProfitPer GE ipiUnderValue THEN NEXT.
        END.
        ELSE IF iplPrintOrderOverPct AND NOT iplPrintOrderUnderPct THEN DO:
            IF dProfitPer LE ipiOverValue THEN NEXT.
        END.

        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ oe-ord.company
               AND sman.sman    EQ w-data.sman
             NO-ERROR.

        c-result = oe-ord.stat.
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
        c-result = cResult.

        CREATE ttOrdersBooked.
        ASSIGN 
            ttOrdersBooked.dueDate      = oe-ord.due-date                    
            ttOrdersBooked.orderNo      = w-data.ord-no                      
            ttOrdersBooked.custName     = IF AVAILABLE cust THEN cust.name ELSE ""
            ttOrdersBooked.custNo       = oe-ord.cust-no
            ttOrdersBooked.salesRep     = IF AVAILABLE sman THEN sman.sman ELSE ""
            ttOrdersBooked.salesRepName = IF AVAILABLE sman THEN sman.sname ELSE "" 
            ttOrdersBooked.commPer      = w-data.comm                      
            ttOrdersBooked.prodCode     = w-data.proCat 
            ttOrdersBooked.fgItemNo     = IF AVAILABLE oe-ordl THEN oe-ordl.i-no ELSE ""
            ttOrdersBooked.fgItemName   = w-data.item-n
            ttOrdersBooked.qtyOrdEa     = w-data.qty
            ttOrdersBooked.sqFt         = w-data.sqft                     
            ttOrdersBooked.totalSqft    = w-data.t-sqft                          
            ttOrdersBooked.msfPrice     = dMSFPrice                   
            ttOrdersBooked.price        = w-data.price                     
            ttOrdersBooked.orderAmount  = dRevenue                       
            ttOrdersBooked.profitPer    = dProfitPer                       
            ttOrdersBooked.totalTons    = w-data.t-tons                          
            ttOrdersBooked.ton          = dPricePerTon
            ttOrdersBooked.custPO       = IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN oe-ordl.po-no ELSE oe-ord.po-no
            ttOrdersBooked.orderDate    = oe-ord.ord-date
            ttOrdersBooked.vUserID      = oe-ord.user-id                  
            ttOrdersBooked.custPartNo   = IF AVAILABLE oe-ordl THEN oe-ordl.part-no ELSE ""
            ttOrdersBooked.prUOM        = IF AVAILABLE oe-ordl THEN oe-ordl.pr-uom  ELSE ""
            ttOrdersBooked.dieNo        = IF AVAILABLE itemfg AND itemfg.die-no NE "" THEN itemfg.die-no
                                     ELSE IF AVAILABLE eb THEN eb.die-no
                                     ELSE ""
            ttOrdersBooked.zzCost       = w-data.cost
            ttOrdersBooked.xxSort       = ttOrdersBooked.salesRep
                                        + STRING(ttOrdersBooked.dueDate,"99/99/9999")
                                        + STRING(ttOrdersBooked.orderNo)  
	        ttOrdersBooked.MachineCode  = fGetRoutingForJob()
            ttOrdersBooked.InksCode     = fGetInksForJob()
            ttOrdersBooked.PrintSheet   = IF AVAILABLE itemfg THEN itemfg.plate-no ELSE ""
            ttOrdersBooked.dCstPerM     = IF AVAILABLE oe-ordl THEN oe-ordl.cost ELSE 0
            ttOrdersBooked.dTotStdCost  = IF AVAILABLE oe-ordl THEN oe-ordl.t-cost ELSE 0
            ttOrdersBooked.dCostRT      = dCost
            ttOrdersBooked.lCostDiff    = ttOrdersBooked.dCstPerM NE ttOrdersBooked.dCostRT
            ttOrdersBooked.dFullCost    = IF AVAILABLE oe-ordl THEN oe-ordl.spare-dec-1 ELSE 0
            ttOrdersBooked.cEnterBy     = oe-ord.entered-id
            ttOrdersBooked.cStatus      = c-result
            ttOrdersBooked.shippedQty   = w-data.shp-qty
            ttOrdersBooked.ackDate      = oe-ord.ack-prnt-date
            ttOrdersBooked.shipFrom     = w-data.cShip-from
            ttOrdersBooked.poReceived   = oe-ord.poReceivedDate
            ttOrdersBooked.prevOrderNo  = cPrevOrder
            ttOrdersBooked.approvedDate = oe-ord.approved-date
            ttOrdersBooked.csr          = IF AVAILABLE job AND job.csrUser_id NE "" THEN job.csrUser_id
                                     ELSE IF AVAILABLE oe-ord THEN oe-ord.csrUser_id
                                     ELSE ""
            . 
        IF ttOrdersBooked.dCstPerM EQ ? THEN
        ttOrdersBooked.dCstPerM = 0.
        IF ttOrdersBooked.dTotStdCost EQ ? THEN
        ttOrdersBooked.dTotStdCost = 0.
        IF ttOrdersBooked.zzCost EQ ? THEN
        ttOrdersBooked.zzCost = 0.
        DELETE w-data.
    END.  /* for each tt-report */
END PROCEDURE.

{AOA/dynBL/pBuildCustList.i}

/* ************************  Function Implementations ***************** */

FUNCTION fGetRoutingForJob RETURNS CHARACTER 
    (  ):
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/    
     DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.

     IF AVAILABLE oe-ord THEN  
     FIND FIRST job NO-LOCK
          WHERE job.company EQ oe-ord.company
            AND job.job-no  EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ord.job-no2
          NO-ERROR.
    IF AVAILABLE job THEN DO:
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company EQ job.company 
              AND job-mch.job     EQ job.job 
              AND job-mch.job-no  EQ job.job-no 
              AND job-mch.job-no2 EQ job.job-no2 
            USE-INDEX line-idx
            BREAK BY job-mch.job
            :
            IF NOT LAST(job-mch.job) THEN
                cResult = cResult + job-mch.m-code + ",".
            ELSE cResult = cResult + job-mch.m-code.
        END.
    END.                

    RETURN cResult.
        
END FUNCTION.

FUNCTION fGetInksForJob RETURNS CHARACTER 
    (  ):
/*------------------------------------------------------------------------------
 Purpose: 
 Notes:
------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx     AS INTEGER   NO-UNDO.

    IF AVAILABLE oe-ord THEN  
     FIND FIRST job NO-LOCK
          WHERE job.company EQ oe-ord.company
            AND job.job-no EQ oe-ord.job-no
            AND job.job-no2 EQ oe-ord.job-no2 NO-ERROR.

    IF AVAILABLE job THEN DO:
        IF AVAILABLE eb THEN
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job  
              AND job-mat.frm     EQ eb.form-no,
            FIRST item NO-LOCK 
            WHERE (item.company   EQ job.company
              AND (item.mat-type  EQ "I"
               OR item.mat-type   EQ "V"))
              AND item.i-no EQ job-mat.i-no
            :
            IF eb.est-type LE 4 THEN DO:
                DO idx = 1 TO 20:
                    IF eb.i-code2[idx] EQ job-mat.i-no THEN DO:
                        IF LOOKUP(job-mat.i-no,cResult) EQ 0 THEN
                        cResult = cResult + job-mat.i-no + ",".
                    END.
                END. /* loop idx */
            END. /* if est-type */
            ELSE DO:
                DO idx = 1 TO 10:
                    IF eb.i-code[idx] EQ job-mat.i-no THEN DO:
                        IF LOOKUP(job-mat.i-no,cResult) EQ 0 THEN
                        cResult = cResult + job-mat.i-no + ",". 
                    END. /* if i-code */
                END. /* loop idx */
            END. /* else */
        END. /* each job-mat */
    END. /* if avail */                

    RETURN cResult.
        
END FUNCTION.
