/* ---------------------------------------------- oe/rep/bolharwl.i 04/01 JLF */
/* PRINT Harwell BOL                                                          */
/* -------------------------------------------------------------------------- */

hide {1} frame bol-bot2.
view {1} frame bol-bot1.

assign
 v-tot-wt = 0
 v-tot-sf = 0.

for each report where report.term-id eq v-term,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01
          by report.key-02:

  v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
               if oe-boll.partial gt 0 then 1 else 0.

  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  release oe-rel.
  find first oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-boll.r-no
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        and oe-rell.i-no    eq oe-boll.i-no
        and oe-rell.line    eq oe-boll.line
      no-lock no-error.
  if avail oe-rell then do:
    find first oe-relh of oe-rell no-lock.
    find first oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-rell.ord-no
          and oe-rel.line    eq oe-rell.line
          and oe-rel.link-no eq oe-rell.r-no
          and oe-rel.ship-no eq oe-relh.ship-no
          and oe-rel.i-no    eq oe-rell.i-no
        no-lock no-error.
    if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
  end.

  if last-of(report.key-02) then do:
    assign
     i = 0
     j = 0
     
     v-ord-qty = 0
     v-bol-qty = 0
     v-bol-wt  = 0.

    for each w2 break by w2.cas-cnt:
      if first(w2.cas-cnt) then do:
        for each oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq int(report.key-02)
              and oe-ordl.i-no    eq report.key-01
            no-lock:

          assign
           v-ord-qty  = v-ord-qty  + oe-ordl.qty
           v-ship-qty = v-ship-qty + oe-ordl.ship-qty.
        end.

        for each xreport
            where xreport.term-id eq v-term
              and xreport.key-01  eq report.key-01
              and xreport.key-02  eq report.key-02
            no-lock,

            first xoe-boll where recid(xoe-boll) eq xreport.rec-id no-lock:

          assign
           v-bol-qty = v-bol-qty + xoe-boll.qty
           v-bol-wt  = v-bol-wt  + xoe-boll.weight.

          if xoe-boll.weight eq 0 then
            v-bol-wt = v-bol-wt + (xoe-boll.qty / 100 * itemfg.weight-100).
        end.
      end.

      i = i + 1.

      if first(w2.cas-cnt) and j eq 0 then
        v-part-dscr = trim(string(int(report.key-02),">>>>>9")).

      else
      do j = j + 1 to 5:
        v-part-dscr = if j eq 2 then itemfg.i-name
                      else
                      if j eq 3 then itemfg.part-no
                      else
                      if j eq 4 then itemfg.i-no
                      else "".
        if v-part-dscr ne "" then leave.
      end.
      
      display {1}
              v-tot-pkgs            when i eq 1
              v-ord-qty             when first(w2.cas-cnt)
              v-part-dscr
              oe-rel.po-no          when first(w2.cas-cnt) and avail oe-rel
              w2.cases
              w2.cas-cnt
              v-bol-qty             when last(w2.cas-cnt)
              oe-boll.p-c           when last(w2.cas-cnt)
              v-bol-wt              when last(w2.cas-cnt)

          with frame bol-mid1.
      down {1} with frame bol-mid1.
    end.

    do j = j + 1 to 5:
      clear frame bol-mid1 no-pause.

      v-part-dscr = if j eq 2 then itemfg.i-name
                    else
                    if j eq 3 then itemfg.part-no
                    else
                    if j eq 4 then itemfg.i-no
                    else "".

      if v-part-dscr ne "" then do:
        display {1}
                v-part-dscr
            with frame bol-mid1.
        down {1} with frame bol-mid1.
      end.
    end.

    /*if itemfg.isaset AND NOT itemfg.alloc = YES then  /* 10311401 */
    for each fg-set
        where fg-set.company eq cocode
          and fg-set.set-no  eq itemfg.i-no
        no-lock:

      find first xitemfg
          where xitemfg.company eq cocode
            and xitemfg.i-no    eq fg-set.part-no
          no-lock no-error.
      v-part-dscr = string(fg-set.part-no,"x(16)") +
                    (if avail xitemfg then xitemfg.i-name else "").

      {sys/inc/part-qty.i v-part-qty fg-set}

      put {1}
          v-part-dscr            at 11 format "x(46)"
          v-bol-qty * v-part-qty to 73 format ">>>>9"
          skip.
    end.*/

    put {1} skip(1).

    v-tot-pkgs = 0.

    for each w2:
      delete w2.
    end.
  end.

  assign
   v-tot-wt = v-tot-wt + oe-boll.weight
   v-tot-sf = v-tot-sf + (oe-boll.qty * itemfg.t-sqft).

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
end. /* for each report */

for each w3:
  put {1} w3.ship-i at 11 skip.
end.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
