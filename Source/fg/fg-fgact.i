/* --------------------------------------------------- fg/fg-fgact.i 10/94 gb */
/* Finished Goods - Create Job Costing F/G WIP Record */
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
   
    /* Used to determine choice to close job, moved to fgpostbatch.p */
/*    RUN fg/setsrcvd.p (BUFFER job, BUFFER reftable, BUFFER job-hdr,*/
/*                       INPUT-OUTPUT li-t-qty).                     */
    ll-set = AVAIL job-hdr.
  END.

  IF AVAIL job-hdr THEN DO:
    lv-rowid = ROWID(job-hdr).

    /* Used to determine choice to close job, moved to fgpostbatch.p */
/*    IF job.opened                           AND                              */
/*       (NOT ll-set OR (CAN-FIND(FIRST b-itemfg                               */
/*                                    WHERE b-itemfg.company EQ job-hdr.company*/
/*                                      AND b-itemfg.i-no    EQ job-hdr.i-no   */
/*                                      AND b-itemfg.isaset  EQ YES            */
/*                                      AND b-itemfg.alloc   NE NO))) THEN DO: */
/*                                                                             */
/*      RUN jc/qty-changed.p (BUFFER job, OUTPUT ll-qty-changed).              */
/*                                                                             */
/*      /* Get underrun quantity */                                            */
/*      {fg/closejob.i}                                                        */
/*                                                                             */
/*                                                                             */
/*      IF v-close-job GT 0                                            AND     */
/*         (job.stat EQ "W"                                OR                  */
/*          v-close-job GT 1                               OR                  */
/*          CAN-FIND(FIRST mat-act                                             */
/*                   WHERE mat-act.company EQ job.company                      */
/*                     AND mat-act.job     EQ job.job                          */
/*                     AND mat-act.job-no  EQ job.job-no                       */
/*                     AND mat-act.job-no2 EQ job.job-no2) OR                  */
/*          CAN-FIND(FIRST mch-act                                             */
/*                   WHERE mch-act.company EQ job.company                      */
/*                     AND mch-act.job     EQ job.job                          */
/*                     AND mch-act.job-no  EQ job.job-no                       */
/*                     AND mch-act.job-no2 EQ job.job-no2) OR                  */
/*          CAN-FIND(FIRST misc-act                                            */
/*                   WHERE misc-act.company EQ job.company                     */
/*                     AND misc-act.job     EQ job.job                         */
/*                     AND misc-act.job-no  EQ job.job-no                      */
/*                     AND misc-act.job-no2 EQ job.job-no2))          AND      */
/*         v-fin-qty + li-t-qty GE v-underrun-qty                     AND      */
/*         {2}.rita-code EQ "R"                                       THEN DO: */
/*                                                                             */
/*        choice = YES.                                                        */
/*                                                                             */
/*        RELEASE job-hdr.                                                     */
/*        FOR EACH job-hdr NO-LOCK                                             */
/*            WHERE job-hdr.company EQ job.company                             */
/*              AND job-hdr.job     EQ job.job                                 */
/*              AND job-hdr.job-no  EQ job.job-no                              */
/*              AND job-hdr.job-no2 EQ job.job-no2                             */
/*              AND ROWID(job-hdr)  NE lv-rowid                                */
/*              AND NOT CAN-FIND(FIRST b-itemfg                                */
/*                               WHERE b-itemfg.company EQ job-hdr.company     */
/*                                 AND b-itemfg.i-no    EQ job-hdr.i-no        */
/*                                 AND b-itemfg.pur-man EQ YES)                */
/*              AND NOT CAN-FIND(FIRST eb                                      */
/*                               WHERE eb.company  EQ job.company              */
/*                                 AND eb.est-no   EQ job.est-no               */
/*                                 AND eb.stock-no EQ job-hdr.i-no             */
/*                                 AND eb.pur-man  EQ YES):                    */
/*          /* get underrun quantity */                                        */
/*          {fg/closejob.i}                                                    */
/*                                                                             */
/*          IF v-fin-qty LT v-underrun-qty THEN DO:                            */
/*            choice = NO.                                                     */
/*            LEAVE.                                                           */
/*          END.                                                               */
/*        END.                                                                 */
/*                                                                             */
/*        IF choice THEN DO:                                                   */
/*          CREATE w-job.                                                      */
/*          ASSIGN                                                             */
/*           w-job.job-no = FILL(" ",6 - LENGTH(TRIM(job.job-no))) +           */
/*                          TRIM(job.job-no) +                                 */
/*                          STRING(job.job-no2,"99")                           */
/*           w-job.rec-id = RECID(job).                                        */
/*        END.                                                                 */
/*                                                                             */
/*                                                                             */
/*      END.                                                                   */
/*    END.                                                                     */

    /* existence of w-job will indicate that the job should be checked to be closed */ 
    FIND FIRST w-job NO-LOCK
        WHERE w-job.job-no EQ FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
                  TRIM(job.job-no) +
                  STRING(job.job-no2,"99")
          AND w-job.rec-id = RECID(job)
          NO-ERROR.
    IF NOT AVAIL w-job THEN DO:
        CREATE w-job.
        ASSIGN
          w-job.job-no = FILL(" ",6 - LENGTH(TRIM(job.job-no))) +
                      TRIM(job.job-no) +
                      STRING(job.job-no2,"99")
          w-job.rec-id = RECID(job)
          .
    END.
    FIND FIRST job-hdr NO-LOCK WHERE ROWID(job-hdr) EQ lv-rowid.

    RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT job-hdr.loc).
    FIND CURRENT itemfg EXCLUSIVE NO-ERROR.
    IF AVAIL itemfg THEN ASSIGN 
        itemfg.q-ono = MAXIMUM(0,itemfg.q-ono)        
        itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    
    FIND FIRST itemfg-loc EXCLUSIVE WHERE 
        itemfg-loc.company EQ itemfg.company AND 
        itemfg-loc.i-no    EQ itemfg.i-no AND 
        itemfg-loc.loc     EQ job-hdr.loc
        NO-ERROR.
    IF AVAIL itemfg-loc THEN ASSIGN 
        itemfg-loc.q-ono = MAXIMUM(0,itemfg-loc.q-ono)
        itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.      

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

