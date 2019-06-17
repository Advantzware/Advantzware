
      v-fin-qty = 0.
      RUN fg/GetProductionQty.p (INPUT job-hdr.company,
                INPUT job-hdr.job-no,
                INPUT job-hdr.job-no2,
                INPUT job-hdr.i-no,
                INPUT NO,
                OUTPUT v-fin-qty).

      v-underrun-qty = job-hdr.qty * .9.

      IF NOT ll-qty-changed THEN DO:
        RUN jc/job4rel.p (BUFFER job, BUFFER oe-rel).

        RUN jc/jobhordl.p (BUFFER job-hdr, BUFFER oe-rel, BUFFER oe-ordl).

        IF AVAIL oe-ordl THEN DO:
          ASSIGN 
            v-underrun-qty = job-hdr.qty
            ll-whs-item = oe-ordl.managed
            .


          IF NOT ll-whs-item                         AND
             CAN-FIND(FIRST sys-ctrl
                      WHERE sys-ctrl.company EQ job.company
                        AND sys-ctrl.name    EQ "JOB QTY"
                        AND sys-ctrl.log-fld EQ YES) THEN
            RUN oe/overundr.p ("O", "-", oe-ordl.over-pct, v-underrun-qty,
                               OUTPUT v-underrun-qty).

          RUN oe/overundr.p ("U", "-", oe-ordl.under-pct, v-underrun-qty,
                             OUTPUT v-underrun-qty).
        END.
      END.

