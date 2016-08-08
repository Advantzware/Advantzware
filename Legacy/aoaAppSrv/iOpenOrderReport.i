/* iOpenOrderReport.i */
      
      RELEASE job.

      IF cStat NE "A" THEN DO:
         IF TRIM(oe-ordl.job-no) NE "" THEN
         FIND FIRST job NO-LOCK
             WHERE job.company EQ ipcCompany
               AND job.job-no  EQ oe-ordl.job-no
               AND job.job-no2 EQ oe-ordl.job-no2
             NO-ERROR.
             
         IF NOT AVAIL job THEN
         FOR EACH job-hdr NO-LOCK
             WHERE job-hdr.company  EQ oe-ordl.company
               AND job-hdr.ord-no   EQ oe-ordl.ord-no
               AND job-hdr.i-no     EQ oe-ordl.i-no
               AND (job-hdr.job-no  NE oe-ordl.job-no OR
                    job-hdr.job-no2 NE oe-ordl.job-no2),
             FIRST job NO-LOCK
             WHERE job.company EQ job-hdr.company
               AND job.job     EQ job-hdr.job
               AND job.job-no  EQ job-hdr.job-no
               AND job.job-no2 EQ job-hdr.job-no2:
           LEAVE.
         END.
        
         IF AVAIL job THEN
           IF (cStat EQ "C" AND job.opened)     OR
              (cStat EQ "O" AND NOT job.opened) THEN NEXT.
      END.

      RELEASE job.
      RELEASE job-hdr.

      dtDueDate = oe-ordl.req-date.

    IF cPrimarySort EQ "Rel Date" THEN do:
        
      dtDueDate  = ? .
      FOR EACH oe-rel
          WHERE oe-rel.company EQ oe-ordl.company
            AND oe-rel.ord-no  EQ oe-ordl.ord-no
            AND oe-rel.i-no    EQ oe-ordl.i-no
            AND oe-rel.line    EQ oe-ordl.line
          NO-LOCK BY oe-rel.rel-date DESC:
       
         {oe/rel-stat.i cStat2} 
             
        /*IF INDEX("ALSBI",cStat2) GT 0 THEN DO:*/
           dtDueDate = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date.
           LEAVE.
       /* END.*/
      END.
    END.
 
      /*IF dtDueDate LT begin_due-date OR
         dtDueDate GT end_due-date THEN NEXT.*/
      
      assign
       lOrderLine    = yes
       iBalQty = oe-ordl.qty.

      IF lDropOrderUnderrun THEN
         iBalQty = iBalQty * (1 - (oe-ordl.under-pct / 100)).

      if not lInc then do:
         for each ar-invl FIELDS(ship-qty)
             where ar-invl.company eq ipcCompany
               and ar-invl.ord-no  eq oe-ord.ord-no
               and ar-invl.i-no    eq oe-ordl.i-no
             use-index ord-no no-lock:
           iBalQty = iBalQty - ar-invl.ship-qty.
         end.
        
         IF oe-ctrl.u-inv THEN
         FOR EACH inv-line FIELDS(ship-qty)
             WHERE inv-line.company EQ ipcCompany
               AND inv-line.ord-no  EQ oe-ord.ord-no
               AND inv-line.i-no    EQ oe-ordl.i-no
               AND inv-line.line    EQ oe-ordl.line
             NO-LOCK:
           iBalQty = iBalQty - inv-line.ship-qty.
         END.
        
         IF iBalQty gt 0 THEN
         DO:
            FOR EACH oe-rell
                WHERE oe-rell.company EQ ipcCompany
                  AND oe-rell.ord-no  EQ oe-ord.ord-no
                  AND oe-rell.i-no    EQ oe-ordl.i-no
                  AND oe-rell.line    EQ oe-ordl.line
                NO-LOCK,
                FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK:
           
                RELEASE oe-bolh.
                RELEASE ar-invl.
                RELEASE inv-line.
                RELEASE bOERell.
                
                FOR EACH oe-boll
                    WHERE oe-boll.company  EQ ipcCompany
                      AND oe-boll.ord-no   EQ oe-rell.ord-no
                      AND oe-boll.line     EQ oe-rell.line
                      AND oe-boll.i-no     EQ oe-rell.i-no
                      AND oe-boll.r-no     EQ oe-rell.r-no
                      AND oe-boll.rel-no   EQ oe-rell.rel-no
                      AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                      AND oe-boll.po-no    EQ oe-rell.po-no
                    NO-LOCK,
                
                    FIRST oe-bolh
                    WHERE oe-bolh.b-no   EQ oe-boll.b-no
                      AND oe-bolh.posted EQ YES
                    NO-LOCK:
                  LEAVE.
                END.
                
                IF AVAIL oe-bolh THEN DO:
                   IF oe-ctrl.u-inv THEN
                   FOR EACH inv-line
                       WHERE inv-line.company EQ ipcCompany
                         AND inv-line.ord-no  EQ oe-ord.ord-no
                         AND inv-line.i-no    EQ oe-ordl.i-no
                         AND inv-line.line    EQ oe-ordl.line
                         AND inv-line.b-no    EQ oe-bolh.b-no
                       NO-LOCK:
                     LEAVE.
                   END.
                  
                   IF NOT AVAIL inv-line THEN
                   FOR EACH ar-invl
                       WHERE ar-invl.company eq ipcCompany
                         AND ar-invl.ord-no  eq oe-ord.ord-no
                         AND ar-invl.i-no    eq oe-ordl.i-no
                         AND ar-invl.bol-no  eq oe-bolh.bol-no
                      USE-INDEX bol-no NO-LOCK:
                     LEAVE.
                   END.
                END.
                
                IF NOT AVAIL inv-line AND NOT AVAIL ar-invl AND oe-relh.posted THEN
                FOR EACH bOERell
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
                    USE-INDEX r-no NO-LOCK:
                
                  LEAVE.
                END.
                
                IF NOT AVAIL bOERell AND
                   NOT AVAIL ar-invl   AND
                   NOT AVAIL inv-line  THEN iBalQty = iBalQty - oe-rell.qty.
            END.

            IF lSched THEN
               FOR EACH oe-rel WHERE
                   oe-rel.company EQ oe-ordl.company AND
                   oe-rel.ord-no  EQ oe-ordl.ord-no AND
                   oe-rel.i-no    EQ oe-ordl.i-no AND
                   oe-rel.line    EQ oe-ordl.line
                   NO-LOCK:
      
                   {oe/rel-stat.i cStat2}
                   IF INDEX("LSI",cStat2) GT 0 THEN
                      iBalQty = iBalQty - oe-rel.tot-qty.
               END.
         END.
        
         if iBalQty le 0 then next.
      end.

      for each ar-invl
          where ar-invl.company eq ipcCompany
            and ar-invl.ord-no  eq oe-ord.ord-no
            and ar-invl.i-no    eq oe-ordl.i-no
          use-index ord-no no-lock:

        RUN build-tt (ar-invl.inv-date, RECID(ar-invl),cPrimarySort,cSort).
      end.

      IF oe-ctrl.u-inv THEN
      FOR EACH inv-line
          WHERE inv-line.company EQ ipcCompany
            AND inv-line.ord-no  EQ oe-ord.ord-no
            AND inv-line.i-no    EQ oe-ordl.i-no
            AND inv-line.line    EQ oe-ordl.line
          NO-LOCK,
          FIRST inv-head WHERE inv-head.r-no EQ inv-line.r-no
          NO-LOCK:

        RUN build-tt (inv-head.inv-date, RECID(inv-line),cPrimarySort,cSort).
      END.

      FOR EACH oe-rell
          WHERE oe-rell.company EQ ipcCompany
            AND oe-rell.ord-no  EQ oe-ord.ord-no
            AND oe-rell.i-no    EQ oe-ordl.i-no
            AND oe-rell.line    EQ oe-ordl.line
          NO-LOCK,
            
          first oe-relh where oe-relh.r-no eq oe-rell.r-no no-lock:

          RELEASE oe-bolh.
          RELEASE ar-invl.
          RELEASE inv-line.
          RELEASE bOERell.
          
          FOR EACH oe-boll
              WHERE oe-boll.company  EQ ipcCompany
                AND oe-boll.ord-no   EQ oe-rell.ord-no
                AND oe-boll.line     EQ oe-rell.line
                AND oe-boll.i-no     EQ oe-rell.i-no
                AND oe-boll.r-no     EQ oe-rell.r-no
                AND oe-boll.rel-no   EQ oe-rell.rel-no
                AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
                AND oe-boll.po-no    EQ oe-rell.po-no
              NO-LOCK,
          
              FIRST oe-bolh
              WHERE oe-bolh.b-no   EQ oe-boll.b-no
                AND oe-bolh.posted EQ YES
              NO-LOCK:
            LEAVE.
          END.
          
          IF AVAIL oe-bolh THEN DO:
             IF oe-ctrl.u-inv THEN
             FOR EACH inv-line
                 WHERE inv-line.company EQ ipcCompany
                   AND inv-line.ord-no  EQ oe-ord.ord-no
                   AND inv-line.i-no    EQ oe-ordl.i-no
                   AND inv-line.line    EQ oe-ordl.line
                   AND inv-line.b-no    EQ oe-bolh.b-no
                 NO-LOCK:
                 LEAVE.
             END.
          
             IF NOT AVAIL inv-line THEN
             FOR EACH ar-invl
                 WHERE ar-invl.company eq ipcCompany
                   AND ar-invl.ord-no  eq oe-ord.ord-no
                   AND ar-invl.i-no    eq oe-ordl.i-no
                   AND ar-invl.bol-no  eq oe-bolh.bol-no
                USE-INDEX bol-no NO-LOCK:
               LEAVE.
             END.
          END.

          IF NOT AVAIL ar-invl  AND
             NOT AVAIL inv-line THEN
          RUN build-tt (oe-relh.rel-date, RECID(oe-rell),cPrimarySort,cSort).
      END.

      IF lSched THEN
         FOR EACH oe-rel WHERE
             oe-rel.company EQ oe-ordl.company AND
             oe-rel.ord-no  EQ oe-ordl.ord-no AND
             oe-rel.i-no    EQ oe-ordl.i-no AND
             oe-rel.line    EQ oe-ordl.line
             NO-LOCK:

             RUN build-tt (oe-rel.rel-date, RECID(oe-rel),cPrimarySort,cSort).
      END.

      IF lOrderLine THEN RUN build-tt (TODAY, RECID(oe-ordl),cPrimarySort,cSort).
