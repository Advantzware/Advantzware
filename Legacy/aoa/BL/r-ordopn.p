/*------------------------------------------------------------------------
  File: r-ordopn.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Open Order Report.rpa */ 
{aoa/tempTable/ttOpenOrderReport.i}
{aoa/tempTable/ttOpenOrderReportDetail.i}

{sys/ref/CustList.i NEW}

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD inv-no       AS   INTEGER 
    FIELD chk-inv      AS   LOGICAL INITIAL YES
    FIELD q-onh        LIKE itemfg.q-onh
    FIELD q-shp        LIKE itemfg.q-onh
    FIELD q-rel        LIKE itemfg.q-onh
    FIELD q-wip        LIKE itemfg.q-onh
    FIELD q-avl        LIKE itemfg.q-onh
    FIELD po-no        LIKE oe-ord.po-no
    FIELD inv          AS   LOGICAL
    FIELD cad-no       LIKE itemfg.cad-no
    FIELD row-id       AS   ROWID 
    FIELD due-date     LIKE oe-ordl.req-date
    FIELD unit-count   LIKE eb.cas-cnt
    FIELD units-pallet LIKE eb.cas-pal
    .

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttOpenOrderReport.
DEFINE OUTPUT PARAMETER TABLE FOR ttOpenOrderReportDetail.
{aoa/includes/pOpenOrderReport.i}

/* local variables */
DEFINE VARIABLE lInc       AS    LOGICAL          NO-UNDO INITIAL YES.
DEFINE VARIABLE cStat      AS    CHARACTER        NO-UNDO INITIAL "A".
DEFINE VARIABLE iBalQty    AS    INTEGER          NO-UNDO.
DEFINE VARIABLE iQOH       LIKE  itemfg.q-onh     NO-UNDO.
DEFINE VARIABLE iQtyShp    LIKE  iQOH             NO-UNDO.
DEFINE VARIABLE iQtyRel    LIKE  iQOH             NO-UNDO.
DEFINE VARIABLE cComma     AS    CHARACTER        NO-UNDO INITIAL ",".
DEFINE VARIABLE dJobQty    AS    DECIMAL          NO-UNDO.
DEFINE VARIABLE dRecQty    AS    DECIMAL          NO-UNDO.
DEFINE VARIABLE lOrderLine AS    LOGICAL          NO-UNDO.
DEFINE VARIABLE cStat2     AS    CHARACTER        NO-UNDO.
DEFINE VARIABLE dtDueDate  LIKE  oe-ordl.req-date NO-UNDO.
DEFINE VARIABLE dtDueDate2 LIKE  oe-ordl.req-date NO-UNDO.
DEFINE VARIABLE lSched     AS    LOGICAL          NO-UNDO.
DEFINE VARIABLE iIndex     AS INTEGER             NO-UNDO.
DEFINE VARIABLE tmpFile    AS CHARACTER           NO-UNDO.
DEFINE VARIABLE lc-result  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cResult    AS CHARACTER NO-UNDO.

DEFINE BUFFER bOERell FOR oe-rell.

FUNCTION fCalcSellPrice RETURN DECIMAL ():
    DEFINE VARIABLE dSellPrice AS DECIMAL NO-UNDO.

    dSellPrice = oe-ordl.price * (1 - oe-ordl.disc / 100).
    IF oe-ordl.stat EQ "C" OR oe-ordl.opened EQ NO THEN
    FOR EACH ar-invl FIELDS(inv-no unit-pr disc) NO-LOCK
        WHERE ar-invl.company EQ oe-ordl.company
          AND ar-invl.ord-no  EQ oe-ordl.ord-no
          AND ar-invl.i-no    EQ oe-ordl.i-no
        BY ar-invl.inv-no DESCENDING
        :
        dSellPrice = ar-invl.unit-pr * (1 - ar-invl.disc / 100).
        LEAVE.
    END. /* each ar-invl */
    RETURN dSellPrice.
END FUNCTION.

/* subject business logic */
IF lAllJobNo THEN
ASSIGN
    cEndJobNo    = "999999"
    iStartJobNo2 = 0
    iEndJobNo2   = 99
    .

ASSIGN
    cStartJobNo  = FILL(" ",6 - LENGTH(TRIM(cStartJobNo))) + TRIM(cStartJobNo) + STRING(INT(iStartJobNo2),"99")
    cEndJobNo    = FILL(" ",6 - LENGTH(TRIM(cEndJobNo))) + TRIM(cEndJobNo) + STRING(INT(iEndJobNo2),"99") 
    cStat        = SUBSTRING(cJobStatus,1,2)
    cOrderStatus = SUBSTRING(cOrderStatus,1,1)
    lInc         = lIncludeZeroOrderBalanceItems  
    .

FIND FIRST oe-ctrl NO-LOCK
     WHERE oe-ctrl.company EQ ipcCompany
     NO-ERROR.

FOR EACH oe-ord NO-LOCK
    WHERE oe-ord.company  EQ ipcCompany
      AND oe-ord.cust-no  GE cStartCustNo
      AND oe-ord.cust-no  LE cEndCustNo
      AND oe-ord.ord-date GE dtStartOrderDate
      AND oe-ord.ord-date LE dtEndOrderDate
      AND oe-ord.user-id  GE cStartUserID
      AND oe-ord.user-id  LE cEndUserID
      AND (cOrderStatus   EQ "A" 
       OR (oe-ord.opened
      AND cOrderStatus    EQ "O")
       OR (NOT oe-ord.opened
      AND cOrderStatus    EQ "C"))
    USE-INDEX ordate,
    EACH oe-ordl OF oe-ord NO-LOCK
    WHERE oe-ordl.i-no     GE cStartItemNo
      AND oe-ordl.i-no     LE cEndItemNo
      AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) + TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") GE cStartJobNo
      AND FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no))) + TRIM(oe-ordl.job-no) + STRING(oe-ordl.job-no2,"99") LE cEndJobNo
      AND oe-ordl.po-no    GE cStartPONumber
      AND oe-ordl.po-no    LE cEndPONumber
      AND oe-ordl.s-man[1] GE cStartSalesRep
      AND oe-ordl.s-man[1] LE cEndSalesRep
      AND (cOrderStatus    EQ "A"
       OR (oe-ordl.stat    NE "C"
      AND cOrderStatus     EQ "O")
       OR (oe-ordl.stat    EQ "C"
      AND cOrderStatus     EQ "C"))
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ oe-ord.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany 
           AND itemfg.i-no    EQ oe-ordl.i-no
         NO-ERROR.
    IF AVAILABLE itemfg AND itemfg.stat NE "A" AND NOT lIncludeInactiveItems THEN NEXT.
    RELEASE job.

    IF cStat NE "A" THEN DO:
        IF TRIM(oe-ordl.job-no) NE "" THEN
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ipcCompany
               AND job.job-no  EQ oe-ordl.job-no
               AND job.job-no2 EQ oe-ordl.job-no2
             NO-ERROR.
        IF NOT AVAILABLE job THEN
        FOR EACH job-hdr NO-LOCK
            WHERE job-hdr.company  EQ oe-ordl.company
              AND job-hdr.ord-no   EQ oe-ordl.ord-no
              AND job-hdr.i-no     EQ oe-ordl.i-no
              AND (job-hdr.job-no  NE oe-ordl.job-no
               OR job-hdr.job-no2  NE oe-ordl.job-no2),
            FIRST job NO-LOCK
            WHERE job.company EQ job-hdr.company
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
            :
            LEAVE.
        END. /* each job-hdr */
        IF AVAILABLE job THEN
            IF (cStat EQ "C" AND job.opened)     OR
               (cStat EQ "O" AND NOT job.opened) THEN NEXT.
    END. /* if cstat */

    RELEASE job.
    RELEASE job-hdr.

    dtDueDate = oe-ordl.req-date.

    IF cPrimarySort EQ "Release Due Date" THEN DO:
        dtDueDate  = ?.
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            BY oe-rel.rel-date DESCENDING
            :
            {oe/rel-stat.i cStat2}
            dtDueDate = IF AVAILABLE oe-relh THEN oe-relh.rel-date
                                             ELSE oe-rel.rel-date.
            LEAVE.
        END. /* each oe-rel */
    END. /* primarysort eq rel date */

    IF dtDueDate LT dtStartDueDate OR
       dtDueDate GT dtEndDueDate   THEN NEXT.

    ASSIGN
        lOrderLine = YES
        iBalQty    = oe-ordl.qty
        .

    IF lDropOrderUnderrun THEN
    iBalQty = iBalQty * (1 - (oe-ordl.under-pct / 100)).

    IF NOT lInc THEN DO:
        FOR EACH ar-invl FIELDS(ship-qty) NO-LOCK
            WHERE ar-invl.company EQ ipcCompany
              AND ar-invl.ord-no  EQ oe-ord.ord-no
              AND ar-invl.i-no    EQ oe-ordl.i-no
            USE-INDEX ord-no
            :
            iBalQty = iBalQty - ar-invl.ship-qty.
        END. /* each ar-invl */
        IF oe-ctrl.u-inv THEN
        FOR EACH inv-line FIELDS(ship-qty) NO-LOCK
            WHERE inv-line.company EQ ipcCompany
              AND inv-line.ord-no  EQ oe-ord.ord-no
              AND inv-line.i-no    EQ oe-ordl.i-no
              AND inv-line.line    EQ oe-ordl.line
            :
            iBalQty = iBalQty - inv-line.ship-qty.
        END. /* each inv-line */
        IF iBalQty GT 0 THEN DO:
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ ipcCompany
                  AND oe-rell.ord-no  EQ oe-ord.ord-no
                  AND oe-rell.i-no    EQ oe-ordl.i-no
                  AND oe-rell.line    EQ oe-ordl.line,
                FIRST oe-relh
                WHERE oe-relh.r-no EQ oe-rell.r-no
                :
                RELEASE oe-bolh.
                RELEASE ar-invl.
                RELEASE inv-line.
                RELEASE bOERell.
                FOR EACH oe-boll NO-LOCK
                    WHERE oe-boll.company  EQ ipcCompany
                      AND oe-boll.ord-no   EQ oe-rell.ord-no
                      AND oe-boll.line     EQ oe-rell.line
                      AND oe-boll.i-no     EQ oe-rell.i-no
                      AND oe-boll.r-no     EQ oe-rell.r-no
                      AND oe-boll.rel-no   EQ oe-rell.rel-no
                      AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                      AND oe-boll.po-no    EQ oe-rell.po-no,
                    FIRST oe-bolh NO-LOCK
                    WHERE oe-bolh.b-no     EQ oe-boll.b-no
                      AND oe-bolh.posted   EQ YES
                    :
                    LEAVE.
                END. /* each oe-boll */
                IF AVAILABLE oe-bolh THEN DO:
                    IF oe-ctrl.u-inv THEN
                    FOR EACH inv-line NO-LOCK
                        WHERE inv-line.company EQ ipcCompany
                          AND inv-line.ord-no  EQ oe-ord.ord-no
                          AND inv-line.i-no    EQ oe-ordl.i-no
                          AND inv-line.line    EQ oe-ordl.line
                          AND inv-line.b-no    EQ oe-bolh.b-no
                        :
                        LEAVE.
                    END. /* each inv-line */
                    IF NOT AVAILABLE inv-line THEN
                    FOR EACH ar-invl NO-LOCK
                        WHERE ar-invl.company EQ ipcCompany
                          AND ar-invl.ord-no  EQ oe-ord.ord-no
                          AND ar-invl.i-no    EQ oe-ordl.i-no
                          AND ar-invl.bol-no  EQ oe-bolh.bol-no
                        USE-INDEX bol-no
                        :
                        LEAVE.
                    END. /* each ar-invl */
                END. /* avail oe-bolh */
                IF NOT AVAILABLE inv-line AND
                   NOT AVAILABLE ar-invl  AND oe-relh.posted THEN
                FOR EACH bOERell NO-LOCK
                    WHERE bOERell.company EQ oe-rell.company
                      AND bOERell.r-no    EQ oe-rell.r-no
                      AND ROWID(bOERell)  NE ROWID(oe-rell)
                      AND CAN-FIND(FIRST oe-boll
                                   WHERE oe-boll.company  EQ bOERell.company
                                     AND oe-boll.ord-no   EQ bOERell.ord-no
                                     AND oe-boll.i-no     EQ bOERell.i-no
                                     AND oe-boll.line     EQ bOERell.line
                                     AND oe-boll.r-no     EQ bOERell.r-no
                                     AND oe-boll.rel-no   EQ bOERell.rel-no
                                     AND oe-boll.b-ord-no EQ bOERell.b-ord-no
                                     AND oe-boll.po-no    EQ bOERell.po-no
                                   USE-INDEX ord-no)
                    USE-INDEX r-no
                    :
                    LEAVE.
                END. /* each boerell */
                IF NOT AVAILABLE bOERell  AND
                   NOT AVAILABLE ar-invl  AND
                   NOT AVAILABLE inv-line THEN
                iBalQty = iBalQty - oe-rell.qty.
            END. /* each oe-rell */

            IF lSched THEN
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company EQ oe-ordl.company
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                :
                {oe/rel-stat.i cStat2}
                IF INDEX("LSI",cStat2) GT 0 THEN
                iBalQty = iBalQty - oe-rel.tot-qty.
            END. /* each oe-rel */
        END. /* if ibalqty */
        IF iBalQty LE 0 THEN NEXT.
    END. /* if not lInc */
    
    FOR EACH ar-invl NO-LOCK
        WHERE ar-invl.company EQ ipcCompany
          AND ar-invl.ord-no  EQ oe-ord.ord-no
          AND ar-invl.i-no    EQ oe-ordl.i-no
        USE-INDEX ord-no
        :
        RUN pBuildttReport (ar-invl.inv-date, RECID(ar-invl),cPrimarySort,cSecondarySort).
    END. /* each ar-invl */

    IF oe-ctrl.u-inv THEN
    FOR EACH inv-line NO-LOCK
        WHERE inv-line.company EQ ipcCompany
          AND inv-line.ord-no  EQ oe-ord.ord-no
          AND inv-line.i-no    EQ oe-ordl.i-no
          AND inv-line.line    EQ oe-ordl.line,
        FIRST inv-head NO-LOCK
        WHERE inv-head.r-no EQ inv-line.r-no
        :
        RUN pBuildttReport (inv-head.inv-date, RECID(inv-line),cPrimarySort,cSecondarySort).
    END. /* each inv-line */

    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.company EQ ipcCompany
          AND oe-rell.ord-no  EQ oe-ord.ord-no
          AND oe-rell.i-no    EQ oe-ordl.i-no
          AND oe-rell.line    EQ oe-ordl.line,
        FIRST oe-relh NO-LOCK
        WHERE oe-relh.r-no EQ oe-rell.r-no
        :
        RELEASE oe-bolh.
        RELEASE ar-invl.
        RELEASE inv-line.
        RELEASE bOERell.
        FOR EACH oe-boll NO-LOCK
            WHERE oe-boll.company  EQ ipcCompany
              AND oe-boll.ord-no   EQ oe-rell.ord-no
              AND oe-boll.line     EQ oe-rell.line
              AND oe-boll.i-no     EQ oe-rell.i-no
              AND oe-boll.r-no     EQ oe-rell.r-no
              AND oe-boll.rel-no   EQ oe-rell.rel-no
              AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
              AND oe-boll.po-no    EQ oe-rell.po-no,
            FIRST oe-bolh NO-LOCK
            WHERE oe-bolh.b-no     EQ oe-boll.b-no
              AND oe-bolh.posted   EQ YES
            :
            LEAVE.
        END. /* each oe-boll */
        IF AVAILABLE oe-bolh THEN DO:
            IF oe-ctrl.u-inv THEN
            FOR EACH inv-line NO-LOCK
                WHERE inv-line.company EQ ipcCompany
                  AND inv-line.ord-no  EQ oe-ord.ord-no
                  AND inv-line.i-no    EQ oe-ordl.i-no
                  AND inv-line.line    EQ oe-ordl.line
                  AND inv-line.b-no    EQ oe-bolh.b-no
                :
                LEAVE.
            END. /* each inv-line */
            IF NOT AVAILABLE inv-line THEN
            FOR EACH ar-invl NO-LOCK
                WHERE ar-invl.company EQ ipcCompany
                  AND ar-invl.ord-no  EQ oe-ord.ord-no
                  AND ar-invl.i-no    EQ oe-ordl.i-no
                  AND ar-invl.bol-no  EQ oe-bolh.bol-no
                USE-INDEX bol-no
                :
                LEAVE.
            END. /* each ar-invl */
        END. /* if avail oe-bolh */
        IF NOT AVAILABLE ar-invl  AND
           NOT AVAILABLE inv-line THEN
        RUN pBuildttReport (oe-relh.rel-date, RECID(oe-rell),cPrimarySort,cSecondarySort).
    END. /* each oe-rell */

    IF lSched THEN
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
        :
        RUN pBuildttReport (oe-rel.rel-date, RECID(oe-rel),cPrimarySort,cSecondarySort).
    END. /* each oe-rel */

    IF lOrderLine THEN RUN pBuildttReport (TODAY, RECID(oe-ordl),cPrimarySort,cSecondarySort).
END. /*  each oe-ord  */

FOR EACH tt-report NO-LOCK 
    WHERE tt-report.term-id EQ "",
    FIRST oe-ordl NO-LOCK 
    WHERE ROWID(oe-ordl) EQ tt-report.row-id,
    FIRST oe-ord OF oe-ordl 
    BREAK BY tt-report.row-id
          BY tt-report.key-07
    :
    ASSIGN
        iQtyShp = iQtyShp + tt-report.q-shp
        iQtyRel = iQtyRel + tt-report.q-rel
        .
    IF LAST-OF(tt-report.row-id) THEN DO:
        IF NOT CAN-FIND(FIRST ttOpenOrderReportDetail
                        WHERE ttOpenOrderReportDetail.company EQ ipcCompany
                          AND ttOpenOrderReportDetail.i-no    EQ oe-ordl.i-no) THEN
        RUN pCalcQOH (ipcCompany).

        ASSIGN
            iIndex = iIndex + 1
            tt-report.key-08 = STRING(iIndex)
            .
        FOR EACH ttOpenOrderReportDetail
            WHERE ttOpenOrderReportDetail.company EQ oe-ordl.company
              AND ttOpenOrderReportDetail.i-no    EQ oe-ordl.i-no
              AND ttOpenOrderReportDetail.job-no  EQ oe-ordl.job-no
              AND ttOpenOrderReportDetail.job-no2 EQ oe-ordl.job-no2
            :
            ASSIGN
                tt-report.q-onh = tt-report.q-onh + ttOpenOrderReportDetail.qty
                ttOpenOrderReportDetail.ord-no = oe-ord.ord-no
                ttOpenOrderReportDetail.xxIndex = iIndex
                .
        END. /*  end of for each ttOpenOrderReportDetail */

        IF lIncludeJobsQOH THEN
        FOR EACH job-hdr NO-LOCK 
            WHERE job-hdr.company EQ oe-ordl.company
              AND job-hdr.ord-no  EQ oe-ordl.ord-no
              AND job-hdr.i-no    EQ oe-ordl.i-no
              AND (job-hdr.job-no NE oe-ordl.job-no
               OR job-hdr.job-no2 NE oe-ordl.job-no2)
            BREAK BY job-hdr.job-no
                  BY job-hdr.job-no2
                  BY job-hdr.i-no
            :
            IF FIRST-OF(job-hdr.i-no) THEN
            FOR EACH ttOpenOrderReportDetail 
                WHERE ttOpenOrderReportDetail.company EQ job-hdr.company
                  AND ttOpenOrderReportDetail.i-no    EQ job-hdr.i-no
                  AND ttOpenOrderReportDetail.job-no  EQ job-hdr.job-no
                  AND ttOpenOrderReportDetail.job-no2 EQ job-hdr.job-no2
                :
                ASSIGN
                    tt-report.q-onh = tt-report.q-onh + ttOpenOrderReportDetail.qty
                    ttOpenOrderReportDetail.ord-no = oe-ord.ord-no
                    ttOpenOrderReportDetail.xxIndex = iIndex
                    .
            END. /* each ttOpenOrderReportDetail */
        END. /* each job-hdr */

        ASSIGN
            tt-report.q-shp = iQtyShp
            tt-report.q-rel = iQtyRel
            .
        IF cWIPQty EQ "1" THEN
        tt-report.q-wip = oe-ordl.qty - (tt-report.q-onh + tt-report.q-shp).
        ELSE DO:
            ASSIGN
                dJobQty = 0
                dRecQty = 0
                .
            FIND FIRST job NO-LOCK 
                 WHERE job.company EQ ipcCompany
                   AND job.job-no  EQ oe-ordl.job-no
                   AND job.job-no2 EQ oe-ordl.job-no2
                 NO-ERROR.
            IF AVAILABLE job THEN DO:
                IF NOT job.opened THEN
                tt-report.q-wip = 0.
                ELSE DO:
                    FOR EACH job-hdr FIELDS(qty) NO-LOCK
                        WHERE job-hdr.company EQ oe-ordl.company
                          AND job-hdr.ord-no  EQ oe-ordl.ord-no
                          AND job-hdr.i-no    EQ oe-ordl.i-no
                          AND job-hdr.job-no  EQ oe-ordl.job-no
                          AND job-hdr.job-no2 EQ oe-ordl.job-no2
                        :
                        dJobQty = dJobQty + job-hdr.qty.
                    END.  /* each job-hdr */
                    FIND FIRST itemfg NO-LOCK 
                         WHERE itemfg.company EQ job.company
                           AND itemfg.i-no    EQ oe-ordl.i-no
                         NO-ERROR.
                    IF AVAILABLE itemfg THEN DO:
                        IF itemfg.isaset AND itemfg.alloc THEN
                        FOR EACH fg-act FIELDS(qty) NO-LOCK
                            WHERE fg-act.company EQ job.company
                              AND fg-act.job-no  EQ oe-ordl.job-no
                              AND fg-act.job-no2 EQ oe-ordl.job-no2
                              AND fg-act.i-no    EQ oe-ordl.i-no
                            :
                            dRecQty = dRecQty + fg-act.qty.
                        END. /* each fg-act */
                        FOR EACH fg-rcpth FIELDS(r-no rita-code company) NO-LOCK
                            WHERE fg-rcpth.company   EQ job.company
                              AND fg-rcpth.i-no      EQ oe-ordl.i-no
                              AND fg-rcpth.job-no    EQ oe-ordl.job-no
                              AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                              AND fg-rcpth.rita-code EQ "R",
                            EACH fg-rdtlh FIELDS(qty) NO-LOCK
                            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                            BREAK BY fg-rcpth.company
                            :
                            IF FIRST(fg-rcpth.company) THEN dRecQty = 0.
                            dRecQty = dRecQty + fg-rdtlh.qty.
                        END. /* each fg-rcpth */
                        RELEASE itemfg.
                    END. /*IF AVAIL itemfg*/
                END. /*ELSE DO*/
                RELEASE job.
                tt-report.q-wip = dJobQty - dRecQty.
            END. /*IF AVAIL job*/
        END. /*ELSE DO*/
        IF tt-report.q-wip LT 0 OR
           tt-report.q-wip LT oe-ordl.qty * oe-ordl.under-pct / 100 THEN
        tt-report.q-wip = 0.
        tt-report.q-avl = tt-report.q-onh + tt-report.q-wip - tt-report.q-rel.
        IF tt-report.q-avl LT 0 THEN tt-report.q-avl = 0.
        ASSIGN
            iQtyShp = 0
            iQtyRel = 0
            .
    END. /* IF LAST-OF(tt-report.row-id) */
    ELSE DELETE tt-report.
END. /* each tt-report*/

FOR EACH tt-report NO-LOCK 
    WHERE tt-report.term-id EQ ""
      AND tt-report.cad-no  GE cStartCAD 
      AND tt-report.cad-no  LE cEndCAD   
      AND (lIncludeZeroQtyWIPItems
       OR tt-report.q-wip   GT 0)
      AND (lIncludeZeroQtyActReleaseQty
       OR tt-report.q-avl   GT 0
       OR tt-report.q-rel   GT 0),
    FIRST itemfg
    WHERE itemfg.company    EQ ipcCompany
      AND itemfg.i-no       EQ tt-report.key-06,
    FIRST cust NO-LOCK
    WHERE cust.company      EQ ipcCompany
      AND cust.cust-no      EQ tt-report.key-02,
    FIRST oe-ordl NO-LOCK 
    WHERE ROWID(oe-ordl)    EQ tt-report.row-id,
    FIRST oe-ord OF oe-ordl 
    BREAK BY tt-report.key-01
          BY tt-report.key-02
          BY tt-report.key-03
          BY tt-report.key-04
          BY tt-report.key-05
          BY tt-report.key-06
          BY tt-report.row-id
          BY tt-report.key-07
    :
    dtDueDate2 = ?.
    FOR EACH oe-rel NO-LOCK 
       WHERE oe-rel.company EQ oe-ordl.company
         AND oe-rel.ord-no  EQ oe-ordl.ord-no
         AND oe-rel.i-no    EQ oe-ordl.i-no
         AND oe-rel.line    EQ oe-ordl.line
       BY oe-rel.rel-date DESCENDING
       :
       dtDueDate2 = IF AVAILABLE oe-relh THEN oe-relh.rel-date
                                         ELSE oe-rel.rel-date.
       LEAVE.
    END. /* end of for each oe-rel */

    lc-result = oe-ord.stat .
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT cResult) .
        IF cResult NE "" THEN
            lc-result  = cResult .

    CREATE ttOpenOrderReport.
    ASSIGN
        ttOpenOrderReport.custNo      = cust.cust-no
        ttOpenOrderReport.lineDueDate = oe-ordl.req-date
        ttOpenOrderReport.relDueDate  = dtDueDate2 
        ttOpenOrderReport.custPartNo  = oe-ordl.part-no 
        ttOpenOrderReport.fgItemName  = oe-ordl.i-name 
        ttOpenOrderReport.fgItemNo    = oe-ordl.i-no 
        ttOpenOrderReport.orderNo     = oe-ordl.ord-no
        ttOpenOrderReport.cadNo       = tt-report.cad-no
        ttOpenOrderReport.poNo        = tt-report.po-no
        ttOpenOrderReport.qtyOrd      = oe-ordl.qty 
        ttOpenOrderReport.qtyOnhand   = tt-report.q-onh
        ttOpenOrderReport.qtyShipped  = tt-report.q-shp
        ttOpenOrderReport.qtyActRel   = tt-report.q-rel
        ttOpenOrderReport.qtyWIP      = tt-report.q-wip
        ttOpenOrderReport.qtyAvail    = tt-report.q-avl
        ttOpenOrderReport.salesRep    = oe-ordl.s-man[1]
        ttOpenOrderReport.unit        = tt-report.unit-count
        ttOpenOrderReport.pallet      = tt-report.units-pallet
        ttOpenOrderReport.palletCount = ttOpenOrderReport.unit
                                      * ttOpenOrderReport.pallet
        ttOpenOrderReport.sellPrice   = fCalcSellPrice()
        ttOpenOrderReport.cSTATUS     = lc-result
        ttOpenOrderReport.xxSort1     = IF cPrimarySort EQ "Customer" THEN ttOpenOrderReport.custNo
                                        ELSE tt-report.key-01
        ttOpenOrderReport.xxSort2     = tt-report.key-03
        ttOpenOrderReport.xxIndex     = INTEGER(tt-report.key-08)
        .
        DELETE tt-report.
END. /* each tt-report */

PROCEDURE pBuildttReport:
    DEFINE INPUT PARAMETER ipdtDate         AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER iprRecID         AS RECID     NO-UNDO.
    DEFINE INPUT PARAMETER ipcPrimarySort   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcSecondarySort AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dtDueDate  LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE dtDueDate2 LIKE oe-ordl.req-date NO-UNDO.
    DEFINE VARIABLE cPONo      LIKE oe-ord.po-no     NO-UNDO.
    
    DEFINE BUFFER bARInvl  FOR ar-invl.
    DEFINE BUFFER bInvHead FOR inv-head.
    DEFINE BUFFER bInvLine FOR inv-line.
    DEFINE BUFFER bOERell  FOR oe-rell.
    DEFINE BUFFER bOEBoll  FOR oe-boll.
   
    cPONo = oe-ordl.po-no.
    CREATE tt-report.
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ oe-ordl.company
           AND itemfg.i-no    EQ oe-ordl.i-no
         NO-ERROR.
    IF AVAILABLE itemfg THEN
    tt-report.cad-no = itemfg.cad-no.

    IF tt-report.cad-no EQ "" THEN DO:
        RELEASE eb.
        IF TRIM(oe-ordl.est-no) NE "" THEN
        FIND FIRST eb NO-LOCK
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.cad-no   NE ""
             USE-INDEX est-no NO-ERROR.
        IF NOT AVAILABLE eb THEN
        FIND FIRST eb NO-LOCK
             WHERE eb.company  EQ oe-ordl.company
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.cad-no   NE ""
             USE-INDEX stock NO-ERROR.
        IF AVAILABLE eb THEN
        tt-report.cad-no = eb.cad-no.
    END. /*IF tt-report.cad-no*/
    RELEASE eb.

    IF TRIM(oe-ordl.est-no) NE "" THEN DO:
        FIND FIRST eb NO-LOCK 
             WHERE eb.company  EQ oe-ordl.company
               AND eb.est-no   EQ oe-ordl.est-no
               AND eb.stock-no EQ oe-ordl.i-no
               AND eb.form-no  EQ oe-ordl.form-no
               AND eb.blank-no EQ oe-ordl.blank-no
             NO-ERROR.
        IF AVAILABLE eb THEN DO:
            ASSIGN
                tt-report.unit-count   = eb.cas-cnt
                tt-report.units-pallet = eb.cas-pal
                .
            RELEASE eb.
        END.  /*IF AVAIL eb*/
    END. /*IF TRIM(oe-ordl.est-no)*/

    ASSIGN
        tt-report.term-id  = ""
        tt-report.key-01   = IF ipcPrimarySort EQ "Line Due Date" OR ipcPrimarySort EQ "Release Due Date" THEN
                             STRING(YEAR(dtDueDate),"9999")
                           + STRING(MONTH(dtDueDate),"99")
                           + STRING(DAY(dtDueDate),"99")
                        ELSE IF ipcPrimarySort EQ "Sales Rep" THEN oe-ordl.s-man[1]
                        ELSE ""
        tt-report.key-02   = oe-ord.cust-no
        tt-report.key-03   = IF ipcSecondarySort EQ "PO"           THEN cPONo
                        ELSE IF ipcSecondarySort EQ "Item"         THEN (STRING(oe-ordl.i-no,"x(15)") + cPONo)
                        ELSE IF ipcSecondarySort EQ "Cust Part"    THEN (STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE IF ipcSecondarySort EQ "FG Item Name" THEN (STRING(oe-ordl.i-name,"x(30)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE IF ipcSecondarySort EQ "Order"        THEN (STRING(oe-ord.ord-no,"99999999999") + oe-ordl.part-no)
                        ELSE IF ipcSecondarySort EQ "CAD"          THEN (STRING(tt-report.cad-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))
                        ELSE (STRING(YEAR(dtDueDate),"9999")
                           + STRING(MONTH(dtDueDate),"99")
                           + STRING(DAY(dtDueDate),"99")
                           + STRING(oe-ordl.part-no,"x(15)") + STRING(oe-ord.ord-no,"99999999999"))              
        tt-report.key-04   = FILL(" ",6 - LENGTH(TRIM(oe-ordl.job-no)))
                           + TRIM(oe-ordl.job-no) + "-"
                           + STRING(oe-ordl.job-no2,"99")
        tt-report.key-05   = STRING(oe-ord.ord-no,"99999999999")
        tt-report.key-06   = oe-ordl.i-no
        tt-report.key-07   = STRING(YEAR(ipdtDate),"9999")
                           + STRING(MONTH(ipdtDate),"99")
                           + STRING(DAY(ipdtDate),"99")
        tt-report.po-no    = cPONo
        tt-report.rec-id   = iprRecID
        tt-report.row-id   = ROWID(oe-ordl)
        tt-report.due-date = dtDueDate
        lOrderLine         = NO
        .
                                                
    FIND FIRST bARInvl NO-LOCK
         WHERE RECID(bARInvl) EQ iprRecID
         NO-ERROR.

    IF AVAILABLE bARInvl THEN
    ASSIGN
        tt-report.q-shp  = bARInvl.ship-qty
        tt-report.inv    = YES
        tt-report.inv-no = bARInvl.inv-no
        .

    FIND FIRST bInvLine  NO-LOCK
         WHERE RECID(bInvLine) EQ iprRecID
         NO-ERROR.

    IF AVAILABLE bInvLine THEN DO:
        FIND FIRST bInvHead NO-LOCK
             WHERE bInvHead.r-no EQ bInvLine.r-no
             NO-ERROR.
        ASSIGN
            tt-report.q-shp  = bInvLine.ship-qty
            tt-report.inv    = YES
            tt-report.inv-no = bInvHead.inv-no
            lOrderLine       = NO
            .
    END. /*IF AVAIL bInvLine*/

    FIND bOERell NO-LOCK
         WHERE RECID(bOERell) EQ iprRecID
         NO-ERROR.

    IF NOT tt-report.inv AND AVAILABLE bOERell THEN
    tt-report.q-rel = bOERell.qty.
END PROCEDURE.

PROCEDURE pCalcQOH:
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.

    FOR EACH itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
          AND itemfg.i-no    EQ oe-ordl.i-no,
        EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
        :
        CREATE ttOpenOrderReportDetail.
        BUFFER-COPY fg-bin TO ttOpenOrderReportDetail.
    END. /*FOR EACH itemfg*/
END PROCEDURE.

{aoa/BL/pBuildCustList.i}
