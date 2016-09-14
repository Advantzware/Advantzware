/* r-booked.i */

    FIND FIRST oe-ordm NO-LOCK
         WHERE RECID(oe-ordm) EQ tt-report.rec-id
         NO-ERROR.
    IF AVAILABLE oe-ordm THEN DO:
        FIND FIRST oe-ord OF oe-ordm NO-LOCK.
        ASSIGN
            i            = INTEGER(tt-report.key-03)
            dPct         = oe-ordm.s-pct[i] / 100
            dPriceAmount = oe-ordm.amt * dPct
            .

        CREATE w-data.
        ASSIGN
            w-data.sman    = tt-report.key-01
            w-data.ord-no  = oe-ord.ord-no
            w-data.line    = oe-ordm.line
            w-data.misc    = YES
            w-data.procat  = "P/M"
            w-data.qty     = 0
            w-data.sqft    = 0
            w-data.t-sqft  = 0
            w-data.t-tons  = 0
            w-data.item-n  = oe-ordm.dscr
            w-data.cost    = oe-ordm.cost * dPct
            w-data.price   = dPriceAmount
            w-data.revenue = dPriceAmount
            w-data.comm    = oe-ordm.s-comm[i]
            .

        FIND FIRST prep NO-LOCK
             WHERE prep.company EQ oe-ordm.company
               AND prep.code    EQ oe-ordm.charge
             NO-ERROR.
        IF AVAILABLE prep THEN DO:
            IF prep.fgcat NE "" THEN w-data.procat = prep.fgcat.
                                ELSE w-data.procat = "P".
        END. /* avail prep */
        ELSE w-data.procat = "M".
    END. /* avail oe-ordm */
    ELSE DO:
        FIND FIRST oe-ordl NO-LOCK
             WHERE RECID(oe-ordl) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordl THEN DO:
            FIND FIRST oe-ord OF oe-ordl NO-LOCK.
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ ipcCompany
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            ASSIGN
                i            = INT(tt-report.key-03)
                dPct         = oe-ordl.s-pct[i] / 100
                dOrdQty      = oe-ordl.qty * dPct
                dPriceAmount = oe-ordl.t-price * dPct
                dTotTons     = IF AVAIL itemfg THEN (itemfg.weight-100 * dOrdQty / 100 / 2000) ELSE 0
                .

            IF AVAILABLE itemfg AND itemfg.isaset THEN DO:
                dTotalSqft = 0.
                FOR EACH fg-set FIELDS(part-no part-qty) NO-LOCK
                    WHERE fg-set.company EQ itemfg.company
                      AND fg-set.set-no  EQ itemfg.i-no,
                    FIRST b-itemfg FIELDS(t-sqft) NO-LOCK
                    WHERE b-itemfg.company EQ itemfg.company
                      AND b-itemfg.i-no    EQ fg-set.part-no
                    :
                    dTotalSqft = dTotalSqft + (dOrdQty * (IF fg-set.part-qty GE 0 THEN fg-set.part-qty
                                                          ELSE (-1 / fg-set.part-qty)) * b-itemfg.t-sqft / 1000).
                END.  /* each fg-set */
            END.  /* avail itemfg */
            ELSE dTotalSqft = IF AVAIL itemfg THEN (itemfg.t-sqft * dOrdQty / 1000) ELSE 0.
            CREATE w-data.
            ASSIGN
                w-data.sman   = tt-report.key-01
                w-data.ord-no = oe-ord.ord-no
                w-data.line   = oe-ordl.line
                w-data.misc   = NO
                iLines        = iLines + 1
                dQM           = oe-ordl.qty / 1000
                w-data.procat = IF AVAILABLE itemfg THEN itemfg.procat ELSE ?
                w-data.item-n = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ?
                w-data.qty    = dOrdQty
                w-data.margin = oe-ordl.q-qty
                .
            IF NOT oe-ordl.is-a-component THEN
            ASSIGN
                w-data.sqft    = IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE ?
                w-data.t-sqft  = dTotalSqft
                w-data.t-tons  = dTotTons
                w-data.price   = oe-ordl.price
                w-data.revenue = dPriceAmount
                w-data.cost    = oe-ordl.cost * dQM
                w-data.comm    = oe-ordl.s-comm[i]
                .
        END. /* avail oe-ordl */
    END.  /* else do */

