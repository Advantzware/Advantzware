/* ---------------------------------------------- oe/rep/bolempir.i 12/99 FWK */
/* PRINT Empire BOL                                                           */
/* -------------------------------------------------------------------------- */

assign
 v-tot-wt = 0
 v-tot-cases = 0.

for each report where report.term-id eq v-term-id,

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
        and oe-rell.i-no    eq oe-boll.i-no
        and oe-rell.line    eq oe-boll.line
      USE-INDEX r-no no-lock no-error.
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
     v-rel-qty = oe-rell.qty.
  end.

  if last-of(report.key-02) then do:
    assign
     i = 0
     j = 0.

    for each w2 break by w2.cas-cnt:
      if first(w2.cas-cnt) then do:
        assign
         v-ord-qty = 0
         v-bol-qty = 0
         v-bol-wt  = 0
         v-ship-qty = 0   .

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

      assign
       i           = i + 1
       v-part-comp = if v-ship-qty + v-bol-qty ge v-ord-qty or oe-boll.p-c
                     then "C" else "P"
       v-part-dscr = "".
    end.

    for each w2:
      delete w2.
    end.
  end.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq int(report.key-02)
        and oe-ordl.i-no    eq report.key-01
      no-lock no-error.

  v-job-no = "".
  if avail oe-ordl and oe-ordl.job-no ne "" then
    v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
               trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).


  IF v-printline > 30 THEN DO:
     PAGE {1}.
     v-printline = 0.
     {oe/rep/bolhugh1.i}
  END.

  DISPLAY {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no 
          oe-boll.i-no 
          oe-ordl.i-name  FORM "x(22)"
          oe-boll.cases FORM ">>>" "@"
          oe-boll.qty-case FORM "->>>>>Z" SKIP          
          oe-ordl.qty FORM "->>>>,>>9" SPACE(8) 
          v-rel-qty FORM "->>>>,>>9" SPACE(3)
          v-job-no FORM "x(15)"
          oe-ordl.part-dscr1 AT 47 FORM "x(23)"
          v-1    FORM ">>9"           when oe-boll.partial gt 0 AT 72 "@"
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP 
          "====================" AT 68 SKIP
          v-tot-pkgs AT 72 "="
          oe-boll.qty FORM "->>>>>z" SPACE(5)
          v-part-comp  SPACE(1)
          oe-boll.weight  SKIP          
          with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
  down {1} with frame bol-mid1.

  v-printline = v-printline + 4.

  /* display componets of set */
  IF itemfg.isaset THEN DO:
    RUN fg/fullset.p (ROWID(itemfg)).

    FOR EACH tt-fg-set,

        FIRST xitemfg
        WHERE xitemfg.company EQ itemfg.company
          AND xitemfg.i-no    EQ tt-fg-set.part-no
        NO-LOCK:

      ASSIGN
       v-part-dscr = STRING(tt-fg-set.part-no,"x(16)") + xitemfg.i-name
       v-part-d[1] = {sys/inc/k16.i xitemfg.l-score[50]}
       v-part-d[2] = {sys/inc/k16.i xitemfg.w-score[50]}
       v-part-d[3] = {sys/inc/k16.i xitemfg.d-score[50]}
       v-part-dims = "".

      IF v-part-d[1] NE 0 THEN
        v-part-dims = v-part-dims +       TRIM(STRING(v-part-d[1],">>.99")).
      IF v-part-d[2] NE 0 THEN
        v-part-dims = v-part-dims + "x" + TRIM(STRING(v-part-d[2],">>.99")).
      IF v-part-d[3] NE 0 THEN
        v-part-dims = v-part-dims + "x" + TRIM(STRING(v-part-d[3],">>.99")).

      PUT {1}
	      v-part-dscr                          AT 4  FORMAT "x(46)"
          SPACE(1)
          v-part-dims                                FORMAT "x(17)"
          SPACE(1)
	      oe-boll.qty * tt-fg-set.part-qty-dec TO 73 FORMAT ">>>>9"
	      SKIP.

      v-printline = v-printline + 1.
      IF v-printline GE 33 THEN DO:
        PAGE {1}.
        v-printline = 0.
        {oe/rep/bolhugh1.i}
      END.
    END.
  END.
   
  assign
   v-tot-wt = v-tot-wt + oe-boll.weight
   v-tot-cases = v-tot-cases + v-tot-pkgs.

  v-tot-pkgs = 0.

  if oe-boll.weight eq 0 then
     v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

 
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
