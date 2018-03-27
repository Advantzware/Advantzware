
VIEW {1} FRAME top-of-form.
PAGE {1}.

FOR EACH tt-boll
    BREAK BY tt-boll.i-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.po-no
          BY tt-boll.job-no
          BY tt-boll.job-no2:

  IF FIRST-OF(tt-boll.job-no2) THEN DO:
    FOR EACH w2:
      DELETE w2.
    END.

    ASSIGN
     v-ship-qty = 0
     v-weight   = 0.
  END.

  ASSIGN
   v-ship-qty = v-ship-qty + tt-boll.qty
   v-weight   = v-weight + tt-boll.weight.

  IF tt-boll.qty-case NE 0 AND tt-boll.cases NE 0 THEN DO:
    FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.qty-case NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    ASSIGN
     w2.cas-cnt = tt-boll.qty-case
     w2.cases   = w2.cases + tt-boll.cases.
  END.

  IF tt-boll.partial NE 0 THEN DO:
    FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.partial NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    ASSIGN
     w2.cas-cnt = tt-boll.partial
     w2.cases   = w2.cases + 1.
  END.

  IF LAST-OF(tt-boll.job-no2) THEN DO:
    find FIRST oe-ordl
        where oe-ordl.company = tt-boll.company
	      and oe-ordl.ord-no = tt-boll.ord-no
	      and oe-ordl.line = tt-boll.line
	      and oe-ordl.i-no = tt-boll.i-no /*Not needed for index*/
	    no-lock no-error.

    tmp = 0.
    for each b-tt-boll 
	    where b-tt-boll.company = oe-bolh.company
          and b-tt-boll.b-no = oe-bolh.b-no
          and b-tt-boll.ord-no = tt-boll.ord-no
	      and b-tt-boll.i-no = tt-boll.i-no
        no-lock:
	  tmp = tmp + b-tt-boll.qty.
    end.

    IF AVAIL oe-ordl THEN
    FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

    IF AVAIL oe-ord THEN
      assign
	   v-complete = if oe-ordl.ship-qty + tmp >=
	                oe-ordl.qty * (1 - oe-ord.under-pct * .01) then "Complete"
	                else "Partial"
	   v-cust-part = oe-ordl.part-no
       v-item-desc = oe-ordl.i-name
       /*
	   v-complete = if oe-ordl.qty-on-bo <= 0 then "Complete" else "Partial"
       */
	   v-order-qty = oe-ordl.qty.

    else
      ASSIGN
       v-complete = "Complete"
       v-cust-part = ""
	   v-item-desc = ""
       v-order-qty = 0.

    assign
     v-item-number  = tt-boll.i-no
     v-po-number    = tt-boll.po-no
     v-order-number = tt-boll.ord-no.

    FIND FIRST itemfg WHERE itemfg.company = oe-ordl.company 
                        AND itemfg.i-no = oe-ordl.i-no
         NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
       ASSIGN v-item-desc2 = itemfg.part-dscr1.
    ELSE
       ASSIGN v-item-desc2 = "".

    ASSIGN v-disp-desc2 = NO.
    FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
      v-cases = STRING(w2.cases) + " @ " + TRIM(STRING(w2.cas-cnt)).

      IF FIRST(w2.cases * w2.cas-cnt) THEN DO:

        v-lot-no = "".
 
/*         FIND FIRST reftable WHERE                      */
/*              reftable.reftable EQ "oe-boll.lot-no" AND */
/*              reftable.rec_key = tt-boll.rec_id         */
/*              USE-INDEX rec_key                         */
/*              NO-LOCK NO-ERROR.                         */
/*                                                        */
/*         IF AVAIL reftable THEN                         */
/*         DO:                                            */
/*            v-lot-no = reftable.CODE.                   */
/*            RELEASE reftable.                           */
/*         END.                                           */
        v-lot-no EQ tt-boll.lot-no.

        IF v-lot-no EQ "" THEN
        DO:
           for each b-oe-boll WHERE
	           b-oe-boll.company = tt-boll.company AND
               b-oe-boll.b-no = tt-boll.b-no AND
               b-oe-boll.ord-no = tt-boll.ord-no AND
	           b-oe-boll.i-no = tt-boll.i-no
               NO-LOCK,
               FIRST reftable WHERE
                     reftable.reftable EQ "oe-boll.lot-no" AND
                     reftable.rec_key = b-oe-boll.rec_key AND
                     reftable.CODE NE ""
                     USE-INDEX rec_key
                     NO-LOCK :

               v-lot-no = reftable.CODE.
               LEAVE.
           END.
        END.

        DISPLAY {1}
                v-cust-part
	            v-item-number
	            v-item-desc
	            v-order-qty
	            v-ship-qty
	            v-cases
	            v-weight
	            v-order-number
	            v-complete
	            v-po-number
                v-lot-no
	        WITH FRAME detail.
        DOWN {1} WITH FRAME detail.
      END.
 
      ELSE DO:
        DISPLAY {1}
                v-item-desc2 WHEN NOT v-disp-desc2
	            v-cases
	        WITH FRAME detail1.
        DOWN {1} WITH FRAME detail1.
        ASSIGN v-disp-desc2 = TRUE.
      END.
    END.

    IF NOT v-disp-desc2 AND v-item-desc2 <> "" THEN DO:
       PUT {1} v-item-desc2 AT 32 SKIP.
    END.
    PUT {1} SKIP(1).
  END.
END.
