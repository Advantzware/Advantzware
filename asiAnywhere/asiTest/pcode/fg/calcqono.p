
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-q-ono LIKE itemfg.q-ono NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-po-ordl FOR po-ordl.

DEF VAR ld-qty LIKE fg-act.qty NO-UNDO.
DEF VAR li-loop AS INT NO-UNDO.

{fg/fullset.i NEW}

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL itemfg THEN DO:
  cocode = itemfg.company.
  
  /*** itemfg.q-ono from jobs ***/
  IF NOT itemfg.pur-man THEN DO:
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ itemfg.company
          AND job-hdr.i-no    EQ itemfg.i-no
          AND job-hdr.opened  EQ YES
          AND CAN-FIND(FIRST job
                       WHERE job.company EQ job-hdr.company
                         AND job.job     EQ job-hdr.job
                         AND job.job-no  EQ job-hdr.job-no
                         AND job.job-no2 EQ job-hdr.job-no2)
        USE-INDEX i-no:
  
      ld-qty = 0.
      FOR EACH fg-act NO-LOCK
          WHERE fg-act.company EQ job-hdr.company
            AND fg-act.job     EQ job-hdr.job
            AND fg-act.job-no  EQ job-hdr.job-no
            AND fg-act.job-no2 EQ job-hdr.job-no2
            AND fg-act.i-no    EQ job-hdr.i-no:
        ld-qty = ld-qty + fg-act.qty.
      END.
  
      IF ld-qty LT job-hdr.qty THEN
        op-q-ono = op-q-ono + (job-hdr.qty - ld-qty).
    END.
  
    FOR EACH job NO-LOCK
        WHERE job.company EQ itemfg.company
          AND job.opened  EQ YES,
        FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2,
        FIRST b-itemfg NO-LOCK
        WHERE b-itemfg.company EQ job-hdr.company
          AND b-itemfg.i-no    EQ job-hdr.i-no
          AND b-itemfg.isaset  EQ YES,
        EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-hdr.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-hdr.job,"999999999")
          AND reftable.code2    EQ itemfg.i-no:
  
      RUN fg/fullset.p (ROWID(b-itemfg)).

      FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
        ld-qty = 0.
        FOR EACH fg-act NO-LOCK
            WHERE fg-act.company EQ job-hdr.company
              AND fg-act.job     EQ job-hdr.job
              AND fg-act.job-no  EQ job-hdr.job-no
              AND fg-act.job-no2 EQ job-hdr.job-no2
              AND fg-act.i-no    EQ tt-fg-set.part-no:
          ld-qty = ld-qty + fg-act.qty.
        END.

        IF ld-qty LT job-hdr.qty * tt-fg-set.part-qty-dec THEN
          op-q-ono = op-q-ono + ((job-hdr.qty * tt-fg-set.part-qty-dec) - ld-qty).
      END.
    END.
  END.
  
  /*** itemfg.q-ono from purchase orders ***/
  FOR EACH po-ordl NO-LOCK
      WHERE po-ordl.company   EQ itemfg.company
        AND po-ordl.i-no      EQ itemfg.i-no
        AND po-ordl.job-no    EQ ""
        AND po-ordl.item-type EQ NO
        AND po-ordl.opened    EQ YES
        AND po-ordl.stat      NE "C"
      USE-INDEX opened,

      FIRST po-ord WHERE
            po-ord.company EQ po-ordl.company AND
            po-ord.po-no   EQ po-ordl.po-no NO-LOCK:

    li-loop = 0.
    DO WHILE li-loop LT 1000:
      li-loop = li-loop + 1.

      FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl)
          EXCLUSIVE NO-ERROR NO-WAIT.

      IF AVAIL b-po-ordl THEN DO TRANSACTION:
        IF b-po-ordl.cons-uom EQ b-po-ordl.pr-qty-uom THEN
          b-po-ordl.cons-qty = b-po-ordl.ord-qty.
        ELSE
          RUN sys/ref/convquom.p(b-po-ordl.pr-qty-uom, b-po-ordl.cons-uom,
                                 0, b-po-ordl.s-len, b-po-ordl.s-wid, 0,
                                 b-po-ordl.ord-qty, OUTPUT b-po-ordl.cons-qty).
        li-loop = 1000.
      END.
    END.
  
    FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ROWID(po-ordl) NO-LOCK NO-ERROR.

    IF b-po-ordl.cons-uom EQ "EA" THEN
      ld-qty = b-po-ordl.cons-qty.
    ELSE
      RUN sys/ref/convquom.p(b-po-ordl.cons-uom, "EA",
                             0, b-po-ordl.s-len, b-po-ordl.s-wid, 0,
                             b-po-ordl.cons-qty, OUTPUT ld-qty).
  
    IF ld-qty - b-po-ordl.t-rec-qty GT 0 THEN
      op-q-ono = op-q-ono + (ld-qty - po-ordl.t-rec-qty).
  
    IF op-q-ono LT 0 THEN op-q-ono = 0.
  END.
END.
