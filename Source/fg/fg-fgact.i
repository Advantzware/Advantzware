/* --------------------------------------------------- fg/fg-fgact.i 10/94 gb */
/* Finished Goods - Create Job Costing F/G WIP Record */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */
/* -------------------------------------------------------------------------- */

DEF VAR li-t-qty AS INT.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR ll-set AS LOG.


RELEASE job.
RELEASE fg-set.

IF {1}.job-no    NE ""  AND
   {2}.rita-code NE "T" THEN
FIND FIRST job NO-LOCK
    WHERE job.company EQ {1}.company
      AND job.job-no  EQ {1}.job-no
      AND job.job-no2 EQ {1}.job-no2
    NO-ERROR.

IF AVAIL job THEN DO:
    
  /* Determine if job may be closed via li-t-qty */
  li-t-qty = {2}.t-qty.

  FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     eq job.job
        AND job-hdr.job-no  eq job.job-no
        AND job-hdr.job-no2 eq job.job-no2
        AND job-hdr.i-no    eq {1}.i-no
      NO-ERROR.
  IF AVAIL job-hdr THEN ll-set = NO.

  ELSE      /* Check for a set header to process instead */
  IF NOT itemfg.isaset THEN DO:
    FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job.job,"999999999")
          AND reftable.code2    EQ {1}.i-no
        NO-ERROR.
        
    /* Pulled from fg/setsrcvd.p so it does not have to be called to get ll-set */
    IF AVAIL job AND AVAIL reftable THEN
        FIND job-hdr NO-LOCK
            WHERE job-hdr.company EQ job.company
              AND job-hdr.job     EQ job.job
              AND job-hdr.job-no  EQ job.job-no
              AND job-hdr.job-no2 EQ job.job-no2
           NO-ERROR.
   
    ll-set = AVAIL job-hdr.
  END.

  IF AVAIL job-hdr THEN DO:
    lv-rowid = ROWID(job-hdr).

    /* existence of w-job will indicate that the job should be checked to be closed */ 
    FIND FIRST w-job NO-LOCK
        WHERE w-job.job-no EQ STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', job.job-no, job.job-no2)) 
          AND w-job.rec-id = RECID(job)
          NO-ERROR.
    IF NOT AVAIL w-job THEN DO:
        CREATE w-job.
        ASSIGN
          w-job.job-no = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormat', job.job-no, job.job-no2))
          w-job.rec-id = RECID(job)
          .
    END.
    FIND FIRST job-hdr NO-LOCK WHERE ROWID(job-hdr) EQ lv-rowid.

    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT job-hdr.loc).
    FIND FIRST itemfg-loc 
        WHERE itemfg-loc.company EQ itemfg.company
          AND itemfg-loc.i-no    EQ itemfg.i-no
          AND itemfg-loc.loc     EQ job-hdr.loc
        EXCLUSIVE-LOCK NO-ERROR.

    IF itemfg.q-ono LT 0 THEN do:
        itemfg.q-ono = 0.
        IF AVAIL itemfg-loc THEN
            itemfg-loc.q-ono = 0.
    END.

    itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
    IF AVAIL itemfg-loc THEN    
      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.

    FIND FIRST fg-act EXCLUSIVE-LOCK
        WHERE fg-act.company EQ cocode
          AND fg-act.fg-date EQ {1}.rct-date
          AND fg-act.job-no  EQ job-hdr.job-no
          AND fg-act.job-no2 EQ job-hdr.job-no2
          AND fg-act.i-no    EQ job-hdr.i-no
          AND fg-act.opn     EQ YES
        NO-ERROR.
    IF NOT AVAIL fg-act THEN DO:
      CREATE fg-act.
      ASSIGN
       fg-act.company = cocode
       fg-act.fg-date = {1}.rct-date
       fg-act.job     = job.job
       fg-act.job-no  = job.job-no
       fg-act.job-no2 = job.job-no2
       fg-act.cust-no = IF AVAIL po-ordl THEN po-ordl.cust-no ELSE ""
       fg-act.i-no    = job-hdr.i-no
       fg-act.i-name  = itemfg.i-name
       fg-act.qty-uom = itemfg.prod-uom
       fg-act.cost    = job-hdr.std-tot-cost
       fg-act.opn     = YES
       fg-act.fg-time = TIME.

      IF NOT ll-set THEN
        ASSIGN
         fg-act.qty-uom = {1}.pur-uom
         fg-act.tag     = {2}.tag
         fg-act.loc     = {2}.loc
         fg-act.loc-bin = {2}.loc-bin.

      IF v-fgpostgl EQ "AllItems" AND AVAIL prod THEN DO:
        ASSIGN
         wip-amt = li-t-qty / 1000 * job-hdr.std-mat-cost
         wip-lab = li-t-qty / 1000 * job-hdr.std-lab-cost
         wip-foh = li-t-qty / 1000 * job-hdr.std-fix-cost
         wip-voh = li-t-qty / 1000 * job-hdr.std-var-cost.

        IF wip-amt NE ? AND wip-lab NE ? AND wip-foh NE ? AND wip-voh NE ? THEN fg-act.opn = NO.
      END.
    END.

    IF fg-act.cost EQ 0 AND NOT ll-set THEN fg-act.cost = {2}.std-cost.

    fg-act.qty = fg-act.qty + li-t-qty.
  END.
END.

FIND CURRENT fg-act NO-LOCK NO-ERROR.
FIND CURRENT reftable NO-LOCK NO-ERROR.
/* end ---------------------------------- copr. 1994  advanced software, inc. */

