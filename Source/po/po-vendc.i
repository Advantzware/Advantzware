/* ---------------------------------------------------------------------------*/
/* po detail line - vendor cost                                               */
/* ---------------------------------------------------------------------------*/

DO WITH FRAME po-ordlf:
  FIND FIRST tt-ei
      WHERE tt-ei.company EQ cocode
        AND tt-ei.i-no    EQ {1} po-ordl.i-no
      NO-LOCK NO-ERROR.

  IF AVAIL tt-ei THEN
  FIND FIRST tt-eiv WHERE
      tt-eiv.company EQ tt-ei.company AND
      tt-eiv.i-no    EQ tt-ei.i-no AND
      tt-eiv.vend-no EQ po-ord.vend-no
      NO-LOCK NO-ERROR.

  IF AVAIL tt-eiv THEN DO:
    deCost = po-ordl.cost.

    IF tt-ei.std-uom EQ {1} po-ordl.pr-qty-uom then
      deqty = {1} po-ordl.ord-qty.
    else
      run sys/ref/convquom.p(({1} po-ordl.pr-qty-uom),
                             tt-ei.std-uom, debasis-w,
                             ({1} deS-Len), ({1} deS-wid), deS-dep,
                             ({1} po-ordl.ord-qty), output deqty).

    IF {1} po-ordl.job-no NE "" THEN
      RUN po/groupcst.p (({1} po-ordl.job-no),
                         ({1} po-ordl.job-no2),
                         ({1} po-ordl.i-no),
                         ({1} po-ordl.s-num),
                         ({1} po-ordl.b-num),
                         INPUT-OUTPUT deqty).

    deSetup = 0.
    DO iCount5 = 1 TO 20:
       IF tt-eiv.run-qty[iCount5] LE deqty THEN NEXT.
       ASSIGN
          deCost = tt-eiv.run-cost[iCount5]
          deSetup = tt-eiv.setups[iCount5].
       LEAVE.
    END.

    IF iCount5 LE 20 AND {1} po-ordl.job-no NE "" then deCost = tt-eiv.run-cost[iCount5].

    RUN est/dim-charge.p (tt-eiv.rec_key,
                          ({1} deS-wid),
                          ({1} deS-Len),
                          INPUT-OUTPUT deCost).
    
    IF frame-field EQ "ord-qty" OR frame-field EQ "pr-qty-uom" OR
       frame-field BEGINS "job-no" OR "input" NE "{1}"           THEN DO:
      IF tt-ei.std-uom EQ {1} po-ordl.pr-uom then
        po-ordl.cost = deCost.
      else
        run sys/ref/convquom.p(tt-ei.std-uom,
                               ({1} po-ordl.pr-uom), debasis-w,
                               ({1} deS-Len), ({1} deS-wid), deS-dep,
                               deCost, output po-ordl.cost).

      IF {1} po-ordl.pr-uom EQ {1} po-ordl.cons-uom then
        po-ordl.cons-cost = po-ordl.cost.
      else
        run sys/ref/convquom.p(({1} po-ordl.pr-uom),
                               ({1} po-ordl.cons-uom), debasis-w,
                               ({1} deS-Len), ({1} deS-wid), deS-dep,
                               po-ordl.cost, output po-ordl.cons-cost).

      ASSIGN
       po-ordl.cost      = po-ordl.cost      + deAdder[1]
       po-ordl.cons-cost = po-ordl.cons-cost + deAdder[2].

  /*    display po-ordl.cost po-ordl.cons-cost.  ??? */
    END.


    ELSE
    IF loHoldop1 THEN DO:
      IF tt-ei.std-uom NE {1} po-ordl.pr-uom then
        run sys/ref/convquom.p(tt-ei.std-uom,
                               ({1} po-ordl.pr-uom), debasis-w,
                               ({1} deS-Len), ({1} deS-wid), deS-dep,
                               deCost, output deCost).

      deCost = deCost + deAdder[1].

      IF {1} po-ordl.cost GT deCost then po-ord.stat = "H".
    END.
  END.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

