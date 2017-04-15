/* ---------------------------------------------------------------------------*/
/* po detail line - vendor cost                                               */
/* ---------------------------------------------------------------------------*/

do with frame po-ordlf:
  find first tt-ei
      where tt-ei.company eq cocode
        and tt-ei.i-no    eq {1} po-ordl.i-no
      no-lock no-error.

  if avail tt-ei then
  find first tt-eiv WHERE
      tt-eiv.company EQ tt-ei.company AND
      tt-eiv.i-no    EQ tt-ei.i-no AND
      tt-eiv.vend-no eq po-ord.vend-no
      no-lock no-error.

  if avail tt-eiv then do:
    v-cost = po-ordl.cost.

    if tt-ei.std-uom eq {1} po-ordl.pr-qty-uom then
      v-qty = {1} po-ordl.ord-qty.
    else
      run sys/ref/convquom.p(({1} po-ordl.pr-qty-uom),
                             tt-ei.std-uom, v-basis-w,
                             ({1} v-len), ({1} v-wid), v-dep,
                             ({1} po-ordl.ord-qty), output v-qty).

    IF {1} po-ordl.job-no NE "" THEN
      RUN po/groupcst.p (({1} po-ordl.job-no),
                         ({1} po-ordl.job-no2),
                         ({1} po-ordl.i-no),
                         ({1} po-ordl.s-num),
                         ({1} po-ordl.b-num),
                         INPUT-OUTPUT v-qty).

    v-setup = 0.
    do j = 1 to 20:
       if tt-eiv.run-qty[j] le v-qty then next.
       ASSIGN
          v-cost = tt-eiv.run-cost[j]
          v-setup = tt-eiv.setups[j].
       leave.
    end.

    if {1} po-ordl.job-no ne "" then v-cost = tt-eiv.run-cost[j].

    RUN est/dim-charge.p (tt-eiv.rec_key,
                          ({1} v-wid),
                          ({1} v-len),
                          INPUT-OUTPUT v-cost).
    
    if frame-field eq "ord-qty" or frame-field eq "pr-qty-uom" or
       frame-field begins "job-no" or "input" ne "{1}"           then do:
      if tt-ei.std-uom eq {1} po-ordl.pr-uom then
        po-ordl.cost = v-cost.
      else
        run sys/ref/convquom.p(tt-ei.std-uom,
                               ({1} po-ordl.pr-uom), v-basis-w,
                               ({1} v-len), ({1} v-wid), v-dep,
                               v-cost, output po-ordl.cost).

      if {1} po-ordl.pr-uom eq {1} po-ordl.cons-uom then
        po-ordl.cons-cost = po-ordl.cost.
      else
        run sys/ref/convquom.p(({1} po-ordl.pr-uom),
                               ({1} po-ordl.cons-uom), v-basis-w,
                               ({1} v-len), ({1} v-wid), v-dep,
                               po-ordl.cost, output po-ordl.cons-cost).

      assign
       po-ordl.cost      = po-ordl.cost      + v-adder[1]
       po-ordl.cons-cost = po-ordl.cons-cost + v-adder[2].

  /*    display po-ordl.cost po-ordl.cons-cost.  ??? */
    end.


    else
    if v-hold-op1 then do:
      if tt-ei.std-uom ne {1} po-ordl.pr-uom then
        run sys/ref/convquom.p(tt-ei.std-uom,
                               ({1} po-ordl.pr-uom), v-basis-w,
                               ({1} v-len), ({1} v-wid), v-dep,
                               v-cost, output v-cost).

      v-cost = v-cost + v-adder[1].

      if {1} po-ordl.cost gt v-cost then po-ord.stat = "H".
    end.
  end.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

