/* iOpenOrderReport.i */
      
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
                   OR job-hdr.job-no2 NE oe-ordl.job-no2),
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

        IF cPrimarySort EQ "Rel Date" THEN DO:
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
            RUN pBuildttReport (ar-invl.inv-date, RECID(ar-invl),cPrimarySort,cSort).
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
            RUN pBuildttReport (inv-head.inv-date, RECID(inv-line),cPrimarySort,cSort).
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
                    WHERE ar-invl.company eq ipcCompany
                      AND ar-invl.ord-no  eq oe-ord.ord-no
                      AND ar-invl.i-no    eq oe-ordl.i-no
                      AND ar-invl.bol-no  eq oe-bolh.bol-no
                    USE-INDEX bol-no
                    :
                    LEAVE.
                END. /* each ar-invl */
            END. /* if avail oe-bolh */
            IF NOT AVAILABLE ar-invl  AND
               NOT AVAILABLE inv-line THEN
            RUN pBuildttReport (oe-relh.rel-date, RECID(oe-rell),cPrimarySort,cSort).
        END. /* each oe-rell */

        IF lSched THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
            :
            RUN pBuildttReport (oe-rel.rel-date, RECID(oe-rel),cPrimarySort,cSort).
        END. /* each oe-rel */
        
        IF lOrderLine THEN RUN pBuildttReport (TODAY, RECID(oe-ordl),cPrimarySort,cSort).
