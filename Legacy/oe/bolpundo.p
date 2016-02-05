  
DEF PARAM BUFFER io-bolh FOR oe-bolh.

DEF OUTPUT PARAM op-invoice AS LOG NO-UNDO.

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-fg-bin FOR fg-bin.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

DEF VAR ll AS LOG NO-UNDO.

IF AVAIL io-bolh THEN DO:
  FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ io-bolh.company NO-ERROR.

  /** DELETE ALL RELATED BACK ORDER RELEASES **/
  FOR EACH oe-boll
      WHERE oe-boll.company EQ io-bolh.company
        AND oe-boll.b-no    EQ io-bolh.b-no
      USE-INDEX b-no
      BREAK BY oe-boll.ord-no
            BY oe-boll.i-no
            BY oe-boll.line:

    FOR EACH oe-rell
        WHERE oe-rell.company    EQ oe-boll.company
          AND oe-rell.ord-no     EQ oe-boll.ord-no
          AND ((oe-rell.rel-no   EQ oe-boll.rel-no AND
                oe-rell.b-ord-no GT oe-boll.b-ord-no)   OR
               (oe-rell.i-no     EQ oe-boll.i-no   AND
                oe-rell.line     EQ oe-boll.line   AND
                oe-rell.rel-no   GT oe-boll.rel-no AND
                oe-rell.s-code   NE "B"            AND
                oe-boll.s-code   NE "B"))
          AND CAN-FIND(FIRST oe-relh
                       WHERE oe-relh.r-no   EQ oe-rell.r-no
                         AND oe-relh.posted EQ NO)
        USE-INDEX ord-no
        BREAK BY oe-rell.link-no:

      /* Backup out updates to planned release file. */
      RELEASE oe-rel.
      IF oe-rell.link-no NE 0 THEN
      FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no
          USE-INDEX seq-no NO-ERROR.

      IF oe-rell.b-ord-no GT 0 THEN DO:
        IF AVAIL oe-rel THEN DO:
          IF FIRST-OF(oe-rell.link-no) THEN oe-rel.qty = 0.
          oe-rel.qty = oe-rel.qty + oe-rell.qty.
          RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
        END.

        /* Delete actual release entry in the planned release file. */
        FIND FIRST oe-rel WHERE oe-rel.link-no EQ oe-rell.r-no
            USE-INDEX link NO-ERROR.
        IF NOT AVAIL oe-rel AND oe-rell.link-no NE 0  THEN             
           FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no
              USE-INDEX seq-no NO-ERROR.
        
        IF AVAIL oe-rel THEN DELETE oe-rel.

        FIND FIRST itemfg
            WHERE itemfg.company EQ oe-boll.company
              AND itemfg.i-no    EQ oe-rell.i-no
            NO-ERROR.
        IF AVAIL itemfg THEN
            itemfg.q-back = itemfg.q-back - oe-rell.qty.
        RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-boll.loc).
        FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ oe-boll.company
              AND itemfg-loc.i-no    EQ oe-rell.i-no
              AND itemfg-loc.loc     EQ oe-boll.loc
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg-loc THEN
          itemfg-loc.q-back = itemfg.q-back - oe-rell.qty.
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.

      ELSE DO:
        
        IF AVAIL oe-rel THEN DELETE oe-rel.
      END.
      

      FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-ERROR.
      IF AVAIL oe-relh THEN DO:
        FIND FIRST b-oe-rell
            WHERE b-oe-rell.company EQ oe-relh.company
              AND b-oe-rell.r-no    EQ oe-relh.r-no
              AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
            USE-INDEX r-no NO-ERROR.
        
        IF NOT AVAIL b-oe-rell THEN DELETE oe-relh.
      END.

      FIND b-oe-rell WHERE ROWID(b-oe-rell) EQ ROWID(oe-rell) NO-ERROR.
      IF AVAIL b-oe-rell THEN DELETE b-oe-rell.
    END. /* each oe-rell */

    /* DELETE TEMPORARY CUSTOMER WAREHOUSE FG-BIN
    FOR EACH fg-bin
        WHERE fg-bin.company EQ oe-boll.company
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.loc     EQ "CUST"
          AND fg-bin.loc-bin EQ STRING(io-bolh.bol-no,"999999")
          AND fg-bin.job-no  EQ oe-boll.job-no
          AND fg-bin.job-no2 EQ oe-boll.job-no2
        USE-INDEX co-ino EXCLUSIVE-LOCK:

      FIND FIRST b-fg-bin
          WHERE b-fg-bin.company EQ oe-boll.company
            AND b-fg-bin.i-no    EQ oe-boll.i-no
            AND b-fg-bin.loc     EQ "CUST"
            AND b-fg-bin.loc-bin EQ io-bolh.cust-no
            AND b-fg-bin.tag     EQ oe-boll.tag
            AND b-fg-bin.job-no  EQ oe-boll.job-no
            AND b-fg-bin.job-no2 EQ oe-boll.job-no2
          NO-ERROR.
      IF NOT AVAIL b-fg-bin THEN DO:
        CREATE b-fg-bin.
        ASSIGN
         b-fg-bin.company    = io-bolh.company
         b-fg-bin.i-no       = oe-boll.i-no
         b-fg-bin.loc        = "CUST"
         b-fg-bin.loc-bin    = io-bolh.cust-no
         b-fg-bin.tag        = oe-boll.tag
         b-fg-bin.job-no     = oe-boll.job-no
         b-fg-bin.job-no2    = oe-boll.job-no2
         b-fg-bin.last-count = fg-bin.qty
         b-fg-bin.aging-date = TODAY
         fg-bin.ord-no      = oe-boll.ord-no.
      END.

      ASSIGN
       b-fg-bin.last-date     = TODAY
       b-fg-bin.qty           = b-fg-bin.qty + fg-bin.qty
       b-fg-bin.partial-count = b-fg-bin.partial-count + fg-bin.partial-count.

      IF b-fg-bin.partial-count EQ b-fg-bin.case-count THEN
        b-fg-bin.partial-count = 0.

      DELETE fg-bin.
    END.
    */

    /* Added to put back in back order qty to FG file */
    IF NOT oe-ctrl.u-inv AND oe-boll.b-ord-no GT 0 THEN DO:
      FIND FIRST itemfg
          WHERE itemfg.company EQ io-bolh.company
            AND itemfg.i-no    EQ oe-boll.i-no
          NO-ERROR.
      IF AVAIL itemfg THEN DO:
          itemfg.q-back = itemfg.q-back + oe-boll.qty.
        RUN fg/chkfgloc.p (INPUT oe-boll.i-no, INPUT oe-boll.loc).
        FIND FIRST itemfg-loc 
            WHERE itemfg-loc.company EQ io-bolh.company
              AND itemfg-loc.i-no    EQ oe-boll.i-no
              AND itemfg-loc.loc     EQ oe-boll.loc
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL itemfg-loc THEN
          itemfg-loc.q-back = itemfg.q-back + oe-boll.qty.       
        FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.
    END.

    IF io-bolh.bol-no NE 0 AND oe-boll.ord-no NE 0 THEN
      ASSIGN
       oe-boll.posted = NO
       io-bolh.posted = NO.
    
    IF LAST-OF(oe-boll.line) THEN DO:
      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ oe-boll.company
            AND oe-ordl.ord-no  EQ oe-boll.ord-no
            AND oe-ordl.i-no    EQ oe-boll.i-no
            AND oe-ordl.line    EQ oe-boll.line
          NO-ERROR.

      IF AVAIL oe-ordl THEN DO:
        RUN oe/ship-qty.p (ROWID(oe-ordl), OUTPUT oe-ordl.ship-qty).
        IF oe-boll.s-code NE "S" AND NOT oe-ordl.is-a-component THEN
          oe-ordl.inv-qty = oe-ordl.inv-qty - oe-boll.qty.
      END.
    END.

    IF oe-boll.s-code EQ "I" THEN op-invoice = YES.
    FIND FIRST oe-rel 
       WHERE oe-rel.company EQ oe-boll.company
         AND oe-rel.ord-no  EQ oe-boll.ord-no
         AND oe-rel.i-no    EQ oe-boll.i-no
       NO-LOCK NO-ERROR.
    IF AVAIL oe-rel THEN
      RUN fg/fgitmloc.p (INPUT oe-rel.i-no, INPUT ROWID(oe-rel)).
                  
  END. /* each oe-boll */

  /** Back Out Inventory History of the Bill Of Lading **/
  FOR EACH fg-rcpth
      WHERE fg-rcpth.b-no EQ io-bolh.b-no
      USE-INDEX b-no
      BY fg-rcpth.rita-code:

    ll = YES.    

    IF fg-rcpth.rita-code EQ "R" THEN DO:
      ll = NO.

      /* Does 'R'eceipt have matching 'S'hipment created in oe/shiphist.i
         when qty is negative and N-K-1-BOLPOST log field is yes */
      IF CAN-FIND(FIRST b-fg-rcpth
                  WHERE b-fg-rcpth.rec_key    EQ fg-rcpth.rec_key
                    AND b-fg-rcpth.b-no       EQ fg-rcpth.b-no
                    AND b-fg-rcpth.i-no       EQ fg-rcpth.i-no
                    AND b-fg-rcpth.trans-date EQ fg-rcpth.trans-date
                    AND b-fg-rcpth.rita-code  EQ "S"
                  USE-INDEX rec_key) THEN
      FOR EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.qty       LT 0
            AND CAN-FIND(FIRST b-fg-rdtlh
                         WHERE b-fg-rdtlh.rec_key   EQ fg-rdtlh.rec_key
                           AND b-fg-rdtlh.rita-code EQ "S"
                           AND b-fg-rdtlh.qty       EQ fg-rdtlh.qty
                         USE-INDEX rec_key):
        ll = YES.
        LEAVE.
      END.

      IF NOT ll THEN
      FOR EACH oe-boll NO-LOCK
          WHERE oe-boll.company EQ fg-rcpth.company
            AND oe-boll.b-no    EQ fg-rcpth.b-no
            AND oe-boll.i-no    EQ fg-rcpth.i-no
            AND oe-boll.s-code  EQ "I",
          FIRST oe-ordl NO-LOCK
          WHERE oe-ordl.company        EQ oe-boll.company
            AND oe-ordl.ord-no         EQ oe-boll.ord-no
            AND oe-ordl.i-no           EQ oe-boll.i-no
            AND oe-ordl.line           EQ oe-boll.line
            AND oe-ordl.is-a-component EQ NO
            AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}):
        ll = YES.
        LEAVE.
      END.
    END.

    IF ll THEN DO:
      FOR EACH fg-rdtlh
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:

        FIND FIRST fg-bin
            WHERE fg-bin.company EQ fg-rcpth.company
              AND fg-bin.i-no    EQ fg-rcpth.i-no
              AND fg-bin.loc     EQ fg-rdtlh.loc
              AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
              AND fg-bin.tag     EQ fg-rdtlh.tag
              AND fg-bin.cust-no EQ fg-rdtlh.cust-no
              AND fg-bin.job-no  EQ fg-rcpth.job-no
              AND fg-bin.job-no2 EQ fg-rcpth.job-no2
            NO-ERROR.
        IF NOT AVAIL fg-bin THEN DO:
          
          CREATE fg-bin.
          ASSIGN
           fg-bin.company      = fg-rcpth.company
           fg-bin.i-no         = fg-rcpth.i-no
           fg-bin.job-no       = fg-rcpth.job-no
           fg-bin.job-no2      = fg-rcpth.job-no2
           fg-bin.loc          = fg-rdtlh.loc
           fg-bin.loc-bin      = fg-rdtlh.loc-bin
           fg-bin.tag          = fg-rdtlh.tag
           fg-bin.cust-no      = fg-rdtlh.cust-no
           fg-bin.case-count   = fg-rdtlh.qty-case
           fg-bin.cases-unit   = fg-rdtlh.stacks-unit
           fg-bin.units-pallet = fg-rdtlh.units-pallet
           fg-bin.std-tot-cost = fg-rdtlh.cost
           fg-bin.aging-date   = TODAY.
        END.
        IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
        IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
               
        IF fg-rcpth.rita-code EQ "S" THEN
          fg-bin.qty = fg-bin.qty + fg-rdtlh.qty.
        ELSE
          fg-bin.qty = fg-bin.qty - fg-rdtlh.qty.
    
        ASSIGN
         fg-bin.partial-count = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
         fg-bin.partial-count = fg-bin.qty - (fg-bin.partial-count * fg-bin.case-count).

        IF fg-bin.partial-count EQ fg-bin.case-count THEN
          fg-bin.partial-count = 0.

        FIND FIRST reftable NO-LOCK
              WHERE reftable.reftable EQ "fg-bin.cost"
                AND reftable.company  EQ fg-rdtlh.company
                AND reftable.rec_key  = fg-rdtlh.rec_key
              USE-INDEX rec_key
              NO-ERROR.

        IF AVAIL reftable THEN DO:
    
          ASSIGN
            fg-bin.avg-cost      = reftable.val[1]    
            fg-bin.last-cost     = reftable.val[2]    
            fg-bin.std-fix-cost  = reftable.val[3]    
            fg-bin.std-lab-cost  = reftable.val[4]    
            fg-bin.std-mat-cost  = reftable.val[5]    
            fg-bin.std-tot-cost  = reftable.val[6]    
            fg-bin.std-var-cost  = reftable.val[7]    .
         
       END.

        IF op-invoice AND fg-bin.cust-no NE "" AND fg-bin.qty EQ 0 THEN
          DELETE fg-bin.

        DELETE fg-rdtlh.
      END. /* each fg-rdtlh */

      DELETE fg-rcpth.
    END.
  END. /* each fg-rcpth */
END.
