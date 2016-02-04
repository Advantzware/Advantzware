/* fg/fgpstcal.i posting recalc include */
      
      /** Re-Calculate Qty Available **/
      itemfg.q-avail = itemfg.q-onh + itemfg.q-ono - itemfg.q-alloc.
      if itemfg.q-ship-ptd lt 0 then itemfg.q-ship-ptd = 0.
      if itemfg.q-ship-ytd lt 0 then itemfg.q-ship-ytd = 0.
      RUN fg/chkfgloc.p (INPUT itemfg.i-no, INPUT fg-rctd.loc).

      FIND FIRST itemfg-loc 
          WHERE itemfg-loc.company EQ itemfg.company
            AND itemfg-loc.i-no    EQ itemfg.i-no
            AND itemfg-loc.loc     EQ fg-rctd.loc
          EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL(itemfg-loc) THEN
      itemfg-loc.q-avail = itemfg-loc.q-onh + itemfg-loc.q-ono - itemfg-loc.q-alloc.
      if itemfg-loc.q-ship-ptd lt 0 then itemfg-loc.q-ship-ptd = 0.
      if itemfg-loc.q-ship-ytd lt 0 then itemfg-loc.q-ship-ytd = 0.

      find first job-hdr
            where job-hdr.company eq cocode
              and job-hdr.job-no  eq fg-rctd.job-no
              and job-hdr.job-no2 eq fg-rctd.job-no2
              and job-hdr.i-no    eq fg-rctd.i-no
            no-lock no-error.

      find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.i-no    eq fg-rctd.i-no
              and fg-bin.job-no  eq fg-rctd.job-no
              and fg-bin.job-no2 eq fg-rctd.job-no2
              and fg-bin.loc     eq fg-rctd.loc
              and fg-bin.loc-bin eq fg-rctd.loc-bin
              and fg-bin.tag     eq fg-rctd.tag
            no-error.

      if not avail fg-bin THEN DO:
           create fg-bin.
           assign
             fg-bin.company    = cocode
             fg-bin.i-no       = fg-rctd.i-no
             fg-bin.job-no     = fg-rctd.job-no
             fg-bin.job-no2    = fg-rctd.job-no2
             fg-bin.loc        = fg-rctd.loc
             fg-bin.loc-bin    = fg-rctd.loc-bin
             fg-bin.tag        = fg-rctd.tag
             fg-bin.case-count = fg-rctd.qty-case.
      end.
      
      /* For Transfers from & Shipments decrease the quantity in the BIN */
        if index("TS",fg-rctd.rita-code) ne 0 then
          fg-bin.qty = fg-bin.qty - fg-rctd.t-qty.

        /* For Receipts increase the quantity in the BIN */
        else
        if fg-rctd.rita-code eq "R" then do:
          {fg/upd-bin.i "fg-bin" "fg-rctd.cost-uom" "fg-rctd.std-cost" fg-rctd}
        end.

        else
        if index("AE",fg-rctd.rita-code) gt 0 then
          fg-bin.qty = fg-bin.qty + fg-rctd.t-qty.

      /* update cost for non-job order lines for item */
      run fg/updfgcs1.p (recid(itemfg), NO).
      find b-itemfg where recid(b-itemfg) eq recid(itemfg).
      for each oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.i-no    eq fg-rctd.i-no
              and oe-ordl.job-no  eq ""
              and oe-ordl.cost    eq 0
              use-index item,
          first oe-ord where oe-ord.company           eq cocode
                         and oe-ord.ord-no            eq oe-ordl.ord-no
                         and index("CDZ",oe-ord.stat) eq 0
                         no-lock:

             run sys/ref/convcuom.p(b-itemfg.prod-uom, "M", 0, 0, 0, 0,
                                 b-itemfg.total-std-cost, output oe-ordl.cost).
      end.
