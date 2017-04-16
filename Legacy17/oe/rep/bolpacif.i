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
    /*
   if itemfg.isaset then
   for each fg-set where fg-set.company eq cocode
                          and fg-set.set-no  eq itemfg.i-no   no-lock:

            find first xitemfg where xitemfg.company eq cocode
                                 and xitemfg.i-no    eq fg-set.part-no no-lock no-error.

            FIND FIRST fg-bin where fg-bin.company eq cocode
                              and fg-bin.i-no    eq xitemfg.i-no
                              and fg-bin.job-no = oe-boll.job-no
                              AND fg-bin.job-no2 = oe-boll.job-no2 NO-LOCK NO-ERROR.

            IF AVAIL fg-bin THEN
               ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                      /*lv-comp-partial = fg-bin.qty - (lv-comp-unit * fg-bin.case-count)*/
                      .
            ELSE lv-comp-unit = 0.
            v-tot-pkgs = v-tot-pkgs + lv-comp-unit +
                     if AVAIL fg-bin AND fg-bin.partial-count gt 0 then 1 else 0.
   END.
   ELSE  */
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

  lv-cases - oe-boll.cases.

  
  DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.i-no 
          oe-ordl.i-name /* FORM "x(28)" */
          oe-boll.cases FORM ">>>>" "@"
          oe-boll.qty-case FORM "->>>>>Z" SKIP          
          oe-ordl.part-dscr1 AT 33 FORM "x(25)"
          v-1    FORM ">>>9"           when oe-boll.partial gt 0 AT 64 "@"
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP 
          "====================" AT 59 SKIP
          lv-cases /*v-tot-pkgs*/ AT 64 FORM ">>>>"   "="
          oe-boll.qty FORM "->>>>>z" SPACE(9)
          v-part-comp  SPACE(3)
          oe-boll.weight  SKIP          
          with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
  down {1} with frame bol-mid1.


  v-printline = v-printline + 5.
  put {1} skip(1).
  IF v-printline >= 37  THEN DO:
   PAGE {1}.
   v-printline = 0.
   {oe/rep/bolpaci1.i}
  END.

   /* display componets of set */
  if itemfg.isaset then
      for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

          find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.

          FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = oe-boll.job-no
                            AND fg-bin.job-no2 = oe-boll.job-no2 NO-LOCK NO-ERROR.

          IF AVAIL fg-bin THEN
             ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) 
                    /*lv-comp-partial = fg-bin.qty - (lv-comp-unit * fg-bin.case-count)*/
                    .
          ELSE lv-comp-unit = 0.

          v-part-dscr = string(fg-set.part-no,"x(16)") +
		                (if avail xitemfg then xitemfg.i-name else "").

          {sys/inc/part-qty.i v-part-qty fg-set}

          IF AVAIL fg-bin THEN DO:
             put {1}
	          v-part-dscr              at 17 format "x(39)"
	          /*oe-boll.qty * v-part-qty to 68 format ">>>>9"	*/
              /*lv-comp-unit */ oe-boll.cases TO 67  FORM "->>>>9" " @ " 
              fg-bin.case-count FORM "->>>>>9"  
              skip.              

              v-printline = v-printline + 1.
              IF fg-bin.partial-count <> 0 THEN do:
                 PUT {1} "   1" TO 67  " @ " fg-bin.partial-count FORM "->>>>>9"  SKIP.
                 v-printline = v-printline + 1.
              END.
          END.
          ELSE DO:
              put {1}
	             v-part-dscr              at 17 format "x(39)"  
	             /*oe-boll.qty * v-part-qty to 68 format ">>>>9"	*/          
                 skip.
              v-printline = v-printline + 1.
          END.
          IF v-printline >= 37 THEN DO:
             PAGE {1}.
             v-printline = 0.
             {oe/rep/bolpaci1.i}
          END.
  end.


  assign
   v-tot-wt = v-tot-wt + oe-boll.weight
   v-tot-cases = v-tot-cases + v-tot-pkgs.

  v-tot-pkgs = 0.

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
                       
