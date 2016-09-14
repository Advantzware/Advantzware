/* pOrdersBookedLogic.i */

    /* local variables */
    DEFINE VARIABLE i            AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE j            AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE k            AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE ii           AS   INTEGER          NO-UNDO.
    DEFINE VARIABLE dtTrandate   LIKE dtStartOrderDate NO-UNDO.
    DEFINE VARIABLE cCode        AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE lExclude     AS   LOGICAL          NO-UNDO.
    DEFINE VARIABLE lMisc        AS   LOGICAL          NO-UNDO.
    DEFINE VARIABLE cSalesRep    AS   CHARACTER        NO-UNDO.
    DEFINE VARIABLE dPct         AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dOrdQty      LIKE oe-ordl.qty      NO-UNDO.
    DEFINE VARIABLE dTotalSqft   LIKE itemfg.t-sqft    NO-UNDO.
    DEFINE VARIABLE dTotTons     AS   DECIMAL          NO-UNDO.
    DEFINE VARIABLE dPriceAmount LIKE oe-ord.t-revenue NO-UNDO.
    DEFINE VARIABLE dtMDate      AS   DATE             NO-UNDO.
    DEFINE VARIABLE iPerDays     AS   INTEGER          NO-UNDO EXTENT 2.
    DEFINE VARIABLE lPrtSqft     AS   LOGICAL          NO-UNDO.
    
    FIND FIRST ce-ctrl NO-LOCK 
         WHERE ce-ctrl.company EQ ipcCompany.
    FIND FIRST period NO-LOCK
        WHERE period.company EQ ipcCompany
          AND period.pst     LE dtEndOrderDate
          AND period.pend    GE dtEndOrderDate
        NO-ERROR.
    dtTrandate = IF AVAILABLE period THEN MINIMUM(dtStartOrderDate,period.pst)
                                             ELSE dtStartOrderDate.
    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company  EQ ipcCompany
          AND oe-ord.cust-no  GE cStartCustNo
          AND oe-ord.cust-no  LE cEndCustNo
          AND oe-ord.ord-date GE dtTrandate
          AND oe-ord.ord-date LE dtEndOrderDate
          AND oe-ord.type     NE "T"
          AND oe-ord.stat     NE "D"
        BY oe-ord.company 
        BY oe-ord.ord-date 
        BY oe-ord.ord-no
        :
        IF lExcludeTransferReleasesOrders THEN DO:
            IF oe-ord.TYPE EQ "T" THEN NEXT.
            cCode = "".
            FOR EACH oe-rel FIELDS(r-no) NO-LOCK 
                WHERE oe-rel.company EQ oe-ord.company 
                  AND oe-rel.ord-no  EQ oe-ord.ord-no,
                FIRST reftable NO-LOCK 
                WHERE reftable.reftable EQ "oe-rel.s-code" 
                  AND reftable.company  EQ STRING(oe-rel.r-no,"9999999999") 
                  AND reftable.code     EQ "T"
                :
                cCode = "T".
                LEAVE.
            END. /* each oe-rel */
            IF cCode EQ "T" THEN NEXT.
        END.  /* if lExcludeTransferReleasesOrders */
        FOR EACH oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ ipcCompany
              AND oe-ordl.ord-no  EQ oe-ord.ord-no
              AND (oe-ordl.is-a-component EQ NO
               OR lExcludeSetComponents EQ NO),
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
              AND itemfg.i-no    EQ oe-ordl.i-no
              AND itemfg.proCat  GE cStartProdCategory
              AND itemfg.proCat  LE cEndProdCategory
            BREAK BY oe-ordl.line
            :
            lExclude = YES.
            DO i = 1 TO 3:
                IF lExclude AND
                   oe-ordl.s-man[i] GE cStartSalesRep AND
                   oe-ordl.s-man[i] LE cEndSalesRep THEN
                lExclude = NO.
            END.  /* do i */
            IF lExclude THEN NEXT.
            lMisc = FALSE.
            DO i = 1 TO 3:
                IF lMisc THEN LEAVE.
                IF oe-ordl.s-man[i] LT cStartSalesRep OR
                   oe-ordl.s-man[i] GT cEndSalesRep THEN NEXT.
                /* if no salesman number then assign to misc, ie, blank no */
                IF i EQ 1 AND
                   oe-ordl.s-man[1] EQ "" AND
                   oe-ordl.s-man[2] EQ "" AND
                   oe-ordl.s-man[3] EQ "" THEN
                cSalesRep = "MISC".
                ELSE   /* if blank salesman # then ignore */
                    IF oe-ordl.s-man[i] EQ "" THEN NEXT.
                    /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                    ELSE cSalesRep = oe-ordl.s-man[i].
                IF oe-ord.ord-date GE dtStartOrderDate AND
                   oe-ord.ord-date LE dtEndOrderDate THEN DO:
                    CREATE tt-report.
                    ASSIGN
                        tt-report.term-id = ""
                        tt-report.key-01  = cSalesRep
                        tt-report.key-02  = STRING(oe-ord.ord-no,">>>>>>>>>>")
                        tt-report.key-03  = STRING(i,"9")
                        tt-report.rec-id  = RECID(oe-ordl).           
                END.  /* if oe-ord.ord-date */
                ASSIGN
                    dPct         = oe-ordl.s-pct[i] / 100
                    dOrdQty      = oe-ordl.qty * dPct
                    dTotalSqft   = itemfg.t-sqft * dOrdQty / 1000
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
                         WHERE fgcat.company EQ ipcCompany
                           AND fgcat.procat  EQ ttRecapProductCategory.proCat
                         NO-ERROR.
                    IF AVAILABLE fgcat THEN
                    ttRecapProductCategory.catDscr = fgcat.dscr.
                END. /* not avail ttRecapProductCategory */
                ELSE ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1.
                ASSIGN
                    j = IF oe-ord.ord-date GE dtStartOrderDate AND
                           oe-ord.ord-date LE dtEndOrderDate THEN 1 ELSE 2
                    k = IF AVAILABLE period AND
                        oe-ord.ord-date GE period.pst AND
                        oe-ord.ord-date LE period.pend THEN 2 ELSE 1
                    .
               IF j LE k THEN DO ii = j TO k:
                   IF ii EQ 1 THEN
                   ASSIGN
                       ttRecapProductCategory.sqFtCurrent   = ttRecapProductCategory.sqFtCurrent   + dTotalSqft
                       ttRecapProductCategory.amountCurrent = ttRecapProductCategory.amountCurrent + dPriceAmount
                       .
                   IF ii EQ 2 THEN
                   ASSIGN
                       ttRecapProductCategory.sqFtPeriod   = ttRecapProductCategory.sqFtPeriod   + dTotalSqft
                       ttRecapProductCategory.amountPeriod = ttRecapProductCategory.amountPeriod + dPriceAmount
                       .
               END.  /* if j le k then */
            END.  /* do i = 1 to 3... */
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
            WHERE oe-ordm.company EQ ipcCompany
              AND oe-ordm.ord-no  EQ oe-ord.ord-no
            :
            lExclude = YES.
            DO i = 1 TO 3:
                IF lExclude AND
                    oe-ordm.s-man[i] GE cStartSalesRep AND
                    oe-ordm.s-man[i] LE cEndSalesRep THEN
                lExclude = NO.
            END.  /* do i.. */

            IF lExclude THEN NEXT.
            /* At this point we have either 1, 2 or 3 valid salesman, in any  */
            /* combination of the array. */
            lMisc = FALSE.
            DO i = 1 TO 3:
                IF lMisc THEN LEAVE.
                IF oe-ordm.s-man[i] LT cStartSalesRep OR
                   oe-ordm.s-man[i] GT cEndSalesRep THEN NEXT.
                /* if no salesman number then assign to misc, ie, blank no */
                IF i EQ 1 AND
                    oe-ordm.s-man[1] EQ "" AND
                    oe-ordm.s-man[2] EQ "" AND
                    oe-ordm.s-man[3] EQ "" THEN cSalesRep = "MISC".
                ELSE /* if blank salesman # then ignore */
                    IF oe-ordm.s-man[i] EQ "" THEN NEXT.
                    /* There must be at least 1 salesman in either pos'n 1, 2 or 3 */
                    ELSE cSalesRep = oe-ordm.s-man[i].
                    IF oe-ord.ord-date GE dtStartOrderDate AND
                       oe-ord.ord-date LE dtEndOrderDate THEN DO:
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = cSalesRep
                            tt-report.key-02  = STRING(oe-ord.ord-no,">>>>>>>>>>")
                            tt-report.key-03  = STRING(i,"9")
                            tt-report.rec-id  = RECID(oe-ordm)
                            .
                    END.  /* if oe-ord.ord-date */
                    ASSIGN
                        dPct = oe-ordm.s-pct[i] / 100
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
                             WHERE fgcat.company EQ ipcCompany
                               AND fgcat.procat  EQ ttRecapProductCategory.proCat
                             NO-ERROR.
                        IF AVAILABLE fgcat THEN
                        ttRecapProductCategory.catDscr = fgcat.dscr.
                    END.  /* not avail ttRecapProductCategory */ 
                    ELSE ttRecapProductCategory.numOrders = ttRecapProductCategory.numOrders + 1.
                    ASSIGN
                        j = IF oe-ord.ord-date GE dtStartOrderDate AND
                               oe-ord.ord-date LE dtEndOrderDate THEN 1 ELSE 2
                        k = IF AVAILABLE period AND
                            oe-ord.ord-date GE period.pst AND
                            oe-ord.ord-date LE period.pend THEN 2 ELSE 1
                        .
                    /* We cannot disturb loop variable i from within loop,so use ii: */
                    IF j LE k THEN DO ii = j TO k:
                        IF ii EQ 1 THEN
                        ttRecapProductCategory.amountCurrent = ttRecapProductCategory.amountCurrent + dPriceAmount.
                        IF ii EQ 2 THEN
                        ttRecapProductCategory.amountPeriod  = ttRecapProductCategory.amountPeriod  + dPriceAmount.
                    END.  /* IF j LE k */
            END. /* do i = 1 to 3: */
        END. /* each oe-ordm */   
    END. /* each oe-ord */
