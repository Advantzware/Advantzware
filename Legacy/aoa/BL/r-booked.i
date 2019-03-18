/* r-booked.i - used in aoa/BL/r-booked.p & aoa/BL/recappc.p */

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
DEFINE VARIABLE c-result     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult      AS CHARACTER NO-UNDO.

/* ************************  Function Prototypes ********************** */


FUNCTION fGetRoutingForJob RETURNS CHARACTER 
    (  ) FORWARD.

FUNCTION fGetInksForJob RETURNS CHARACTER
    (  ) FORWARD.

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
        IF oe-ord.TYPE EQ "T" THEN NEXT.
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
            RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dTotalSqft).
            
            ASSIGN
                dPct         = oe-ordl.s-pct[i] / 100
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

lPrtSqft = CAN-DO(cSelectedColumns,"sqFt").
RUN pOrdersBooked1 (ipcCompany,
                    lPrtSqft,
                    lPrintOrderUnderPct,
                    lPrintOrderOverPct,
                    iUnderValue,
                    iOverValue
                    ).
RUN pOrdersBooked2 (ipcCompany,
                    lPrtSqft,
                    lPrintOrderUnderPct,
                    lPrintOrderOverPct,
                    iUnderValue,
                    iOverValue
                    ).

PROCEDURE pOrdersBooked1:
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE i            AS   INTEGER          NO-UNDO.
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
        FIND FIRST oe-ordm NO-LOCK
             WHERE RECID(oe-ordm) EQ tt-report.rec-id
             NO-ERROR.
        IF AVAILABLE oe-ordm THEN
        ASSIGN
            i            = INTEGER(tt-report.key-03)
            dPct         = oe-ordm.s-pct[i] / 100
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
                i            = INTEGER(tt-report.key-03)
                dPct         = oe-ordl.s-pct[i] / 100
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
    DEFINE INPUT PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPrtSqft            AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderUnderPct AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplPrintOrderOverPct  AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipiUnderValue         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiOverValue          AS INTEGER   NO-UNDO.

    /* local variables */
    DEFINE VARIABLE i            AS   INTEGER          NO-UNDO.
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
    DEFINE VARIABLE idx AS INTEGER     NO-UNDO.

    DEFINE BUFFER bItemFG FOR itemfg.
    
    FOR EACH tt-report
        WHERE tt-report.term-id EQ ""
        BREAK BY tt-report.key-01
              BY tt-report.key-02
        :
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
                w-data.ord-no  = oe-ordm.ord-no
                w-data.line    = oe-ordm.line
                w-data.misc    = YES
                w-data.proCat  = "P/M"
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
                     WHERE itemfg.company EQ ipcCompany
                       AND itemfg.i-no    EQ oe-ordl.i-no
                     NO-ERROR.
                ASSIGN
                    i            = INTEGER(tt-report.key-03)
                    dPct         = oe-ordl.s-pct[i] / 100
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
                    w-data.sman   = tt-report.key-01
                    w-data.ord-no = oe-ordl.ord-no
                    w-data.line   = oe-ordl.line
                    w-data.misc   = NO
                    iLines        = iLines + 1
                    dQM           = oe-ordl.qty / 1000
                    w-data.proCat = IF AVAILABLE itemfg THEN itemfg.proCat ELSE ""
                    w-data.item-n = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
                    w-data.qty    = dOrdQty
                    w-data.margin = oe-ordl.q-qty
                    .
                IF NOT oe-ordl.is-a-component THEN
                ASSIGN
                    w-data.sqft    = IF AVAILABLE itemfg THEN itemfg.t-sqft ELSE 0
                    w-data.t-sqft  = dSqft
                    w-data.t-tons  = dTons
                    w-data.price   = oe-ordl.price
                    w-data.revenue = dPriceAmount
                    w-data.cost    = oe-ordl.cost * dQM
                    w-data.comm    = oe-ordl.s-comm[i]
                    .
            END. /* avail oe-ordl */
        END. /* else do */

        FIND FIRST oe-ordl NO-LOCK
             WHERE RECID(oe-ordl) EQ tt-report.rec-id
             NO-ERROR.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ ipcCompany
               AND oe-ord.ord-no  EQ w-data.ord-no
             NO-ERROR.
        FIND cust OF oe-ord NO-LOCK NO-ERROR.
        
        ASSIGN
            dRevenue     = w-data.revenue
            dMSFPrice    = dRevenue / w-data.t-sqft
            dPricePerTon = dRevenue / w-data.t-tons
            dProfitPer   = (dRevenue - w-data.cost) / dRevenue * 100
            dMargin      = w-data.margin
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
                 WHERE itemfg.company EQ ipcCompany
                   AND itemfg.i-no    EQ oe-ordl.i-no
                 NO-ERROR.
            IF NOT AVAILABLE itemfg OR itemfg.die-no EQ "" THEN
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ ipcCompany
                   AND eb.est-no   EQ oe-ordl.est-no
                   AND eb.stock-no EQ oe-ordl.i-no
                 NO-ERROR.
        END. /* avail oe-ordl  */
	
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
             WHERE sman.company EQ ipcCompany
               AND sman.sman    EQ w-data.sman
             NO-ERROR.

        c-result = oe-ord.stat .
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            c-result  = cResult .

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
            ttOrdersBooked.xxCost       = w-data.cost
            ttOrdersBooked.xxSort       = ttOrdersBooked.salesRep
                                        + STRING(ttOrdersBooked.dueDate,"99/99/9999")
                                        + STRING(ttOrdersBooked.orderNo)  
	        ttOrdersBooked.MachineCode  = fGetRoutingForJob()
            ttOrdersBooked.InksCode     = fGetInksForJob()
            ttOrdersBooked.PrintSheet   = IF AVAILABLE itemfg THEN itemfg.plate-no ELSE ""
            ttOrdersBooked.dCstPerM     = IF AVAILABLE oe-ordl THEN oe-ordl.cost ELSE 0
            ttOrdersBooked.dTotStdCost  = IF AVAILABLE oe-ordl THEN oe-ordl.t-cost ELSE 0
            ttOrdersBooked.dFullCost    = IF AVAILABLE oe-ordl THEN oe-ordl.spare-dec-1 ELSE 0
            ttOrdersBooked.cEnterBy     = oe-ord.entered-id
            ttOrdersBooked.cStatus      = c-result

            . 
        DELETE w-data.
    END.  /* for each tt-report */
END PROCEDURE.

{aoa/BL/pBuildCustList.i}


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
                DO i = 1 TO 20:
                    IF eb.i-code2[i] EQ job-mat.i-no THEN DO:
                        IF LOOKUP(job-mat.i-no,cResult) EQ 0 THEN
                         cResult = cResult + job-mat.i-no + ",".
                    END.
                END. /* loop i */
            END. /* if est-type */
            ELSE DO:
                DO i = 1 TO 10:
                    IF eb.i-code[i] EQ job-mat.i-no THEN DO:
                        IF LOOKUP(job-mat.i-no,cResult) EQ 0 THEN
                        cResult = cResult + job-mat.i-no + ",". 
                    END. /* if i-code */
                END. /* loop i */
            END. /* else */
        END. /* each job-mat */
    END. /* if avail */                

    RETURN cResult.
        
END FUNCTION.
