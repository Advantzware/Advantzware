/* -------------------------------------------------- jc/job-clos.i 1/97 JLF */
/* Job Costing - Close Job                                                    */
/* -------------------------------------------------------------------------- */

DEF VAR ll-set AS LOG NO-UNDO.


      FIND FIRST sys-ctrl
          WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "AUTOISSU"
          NO-LOCK NO-ERROR.

      FIND FIRST itemfg
          WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ job-hdr.i-no
          NO-LOCK NO-ERROR.
      RUN fg/chkfgloc.p (INPUT job-hdr.i-no, INPUT job-hdr.loc).
      FIND FIRST itemfg-loc 
          WHERE itemfg-loc.company EQ cocode
            AND itemfg-loc.i-no    EQ job-hdr.i-no
            AND itemfg-loc.loc     EQ job-hdr.loc
        EXCLUSIVE-LOCK NO-ERROR.

      v-fin-qty = 0.
      RUN fg/GetProductionQty.p (INPUT cocode,
                                   INPUT job-hdr.job-no,
                                   INPUT job-hdr.job-no2,
                                   INPUT job-hdr.i-no,
                                   INPUT NO,
                                   OUTPUT v-fin-qty ).      

      IF v-fin-qty LT job-hdr.qty THEN DO:
        IF AVAIL itemfg THEN DO:

          REPEAT:
             FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

             IF AVAIL itemfg THEN
             DO:
                IF NOT itemfg.pur-man THEN
                   itemfg.q-ono = itemfg.q-ono - (job-hdr.qty - v-fin-qty).
                IF AVAIL itemfg-loc THEN
                   itemfg-loc.q-ono = itemfg-loc.q-ono - (job-hdr.qty - v-fin-qty).

                IF itemfg.q-ono LT 0 THEN
                   itemfg.q-ono = 0.
                itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.

                FIND CURRENT itemfg NO-LOCK.
                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                LEAVE.
             END.
          END.
          
          RUN fg/comp-upd.p (RECID(itemfg),
                             (job-hdr.qty - v-fin-qty) * -1,
                             "q-ono", job-hdr.e-num).
        END.
      END.

      IF NOT ll-set THEN
        IF AVAILABLE itemfg AND itemfg.isaset AND
           CAN-FIND(FIRST reftable
                    WHERE reftable.reftable EQ "jc/jc-calc.p"
                      AND reftable.company  EQ job.company
                      AND reftable.loc      EQ ""
                      AND reftable.code     EQ STRING(job.job,"999999999")) AND
           NOT CAN-FIND(FIRST reftable
                        WHERE reftable.reftable EQ "jc/jc-calc.p"
                          AND reftable.company  EQ job.company
                          AND reftable.loc      EQ ""
                          AND reftable.code     EQ STRING(job.job,"999999999")
                          AND reftable.code2    EQ job-hdr.i-no)
        THEN ll-set = YES.

        ELSE
        IF NOT AVAIL sys-ctrl OR sys-ctrl.char-fld NE "FGPost" THEN
          RUN jc/autopc&p.p (BUFFER job, job-hdr.i-no,
                             job-hdr.frm, job-hdr.blank-no, 1).



      IF job.opened THEN DO:
        IF NOT AVAIL sys-ctrl or sys-ctrl.char-fld NE "FGPost" THEN DO:
          {jc/jc-autop.i 1}

          IF ll-set THEN
          FOR EACH reftable NO-LOCK
              WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job.job,"999999999"):

            RUN jc/autopc&p.p (BUFFER job, reftable.code2,
                               INT(reftable.val[12]), INT(reftable.val[13]), 1).
          END.
        END.

        RUN jc/job-cls1.p (RECID(job), 1).

        PAUSE 0.
        RUN jc/jc-dall.p(RECID(job)).


        ASSIGN
         job.opened      = NO
         job.stat        = "C"
         job.close-date  = close_date.


        RUN jc/jobnotes.p (BUFFER job).

        RUN jc/job-cls2.p (RECID(job)).
      END.
