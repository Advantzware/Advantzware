
      find first reftable
            where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq ef.company
	       and reftable.loc      eq ef.loc
	       and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
	       and reftable.code2    eq "{1}-QTY" + string({2},"99")
	       no-lock no-error.

      if not avail reftable then do:
        create reftable.
        assign reftable.reftable = "EST-MISC"
               reftable.company  = ef.company
               reftable.loc      = ef.loc
               reftable.code     = trim(ef.est-no) +
                                   string(ef.form-no,"/99")
               reftable.code2    = "{1}-QTY" + string({2},"99").
  
        for each w-qty: delete w-qty. end.

        DO v = 1 TO 4:
          IF est.est-qty[v] NE 0 THEN DO:
            CREATE w-qty.
            w-qty = est.est-qty[v] * ld-per-set.
          END.
        END.

        FOR EACH est-qty
            WHERE est-qty.company EQ est.company
              AND est-qty.est-no  EQ est.est-no
            NO-LOCK:
          CREATE w-qty.
          w-qty = est-qty.eqty * ld-per-set.

          DO v = 1 TO 20:
            IF est-qty.qty[v] NE 0 THEN DO:
              CREATE w-qty.
              w-qty = est-qty.qty[v] * ld-per-set.
            END.
          END.
        END.

        IF est.est-type EQ 3 OR est.est-type EQ 4 OR est.est-type EQ 8 THEN DO:
          CREATE w-qty.
          w-qty = eb.yld-qty.
        END.

        FOR EACH w-qty BREAK BY w-qty:
          IF NOT FIRST-OF(w-qty) THEN DELETE w-qty.
        END.
        
        v = 0.
        for each w-qty where w-qty gt 0 by w-qty:
          v = v + 1.
          IF v LE EXTENT(reftable.val) THEN reftable.val[v] = w-qty.
        end.
      end.
      op-ref-rec-qty = recid(reftable).

      find first reftable  
            where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq ef.company
	       and reftable.loc      eq ef.loc
	       and reftable.code     eq trim(ef.est-no) + string(ef.form-no,"/99")
	       and reftable.code2    eq "{1}-CST" + string({2},"99")
	       no-error.

       if not avail reftable then do:
               create reftable.
               assign
                reftable.reftable = "EST-MISC"
                reftable.company  = ef.company
                reftable.loc      = ef.loc
                reftable.code     = trim(ef.est-no) +
                                    string(ef.form-no,"/99")
                reftable.code2    = "{1}-CST" + string({2},"99")
                .
       end.
       op-ref-rec-cst = recid(reftable).
