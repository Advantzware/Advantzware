/* ---------------------------------------------- oe/rep/bolindc.i YSK     */
/* PRINT Indiana Carton BOL                                                           */
/* -------------------------------------------------------------------------- */

FOR EACH tt-boll,
    first itemfg where itemfg.company eq cocode
                 and itemfg.i-no    eq tt-boll.i-no no-lock
    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq tt-boll.ord-no
        and oe-ordl.i-no    eq tt-boll.i-no
        and oe-ordl.line    eq tt-boll.line
      no-lock no-error.
 
  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq tt-boll.ord-no
      no-lock no-error.

    v-tot-pkgs = v-tot-pkgs + tt-boll.cases +
                     if tt-boll.partial gt 0 then 1 else 0. 
    
  if tt-boll.qty-case ne 0 and tt-boll.cases ne 0 then do:
     find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
     if not avail w2 then create w2.
     ASSIGN w2.cas-cnt = tt-boll.qty-case
            w2.cases   = w2.cases + tt-boll.cases.
  end.

  if tt-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  v-lines = 0.
  for each w2 break by w2.cases:
    v-lines = v-lines + 1.
  end.

  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.
    delete w2.
  end.

  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-LOCK NO-ERROR.
  ASSIGN lv-cases-tot = lv-cases-tot + tt-boll.cases
         lv-qty-tot = lv-qty-tot + tt-boll.qty
         lv-qcase-tot = lv-qcase-tot + tt-boll.qty-case
         lv-partial-tot = lv-partial-tot + tt-boll.partial
         lv-pal-tot = lv-pal-tot + tt-boll.tot-pallet.

  v-job-no = "".
/*  if avail oe-ordl and oe-ordl.job-no ne "" then*/
v-job-no = fill(" ",6 - length(trim(tt-boll.job-no))) +
           trim(tt-boll.job-no) + "-" + trim(string(tt-boll.job-no2,"99")).
IF trim(v-job-no) = "-00" THEN v-job-no = "".


lv-cases = lv-cases-tot.
lv-class = IF itemfg.frt-class = "" THEN "70" ELSE itemfg.frt-class.

j = j + 1 .

PUT "<C5> "  v-job-no 
    "<C14>" oe-ordl.i-no 
            oe-ordl.part-dscr1
    "<C41>" lv-pal-tot SPACE(1)
    "<C48>" tt-boll.weight SPACE(1)
    "<C53>" lv-qty-tot SPACE(1)
    "<C60>" tt-boll.po-no FORM "x(15)" SKIP .

v-unit-qty = IF lv-partial-tot > 0 
                THEN STRING(v-1,">>>>9") + " @ " + STRING(lv-partial-tot,"->>>>>z") + " " 
                     + string(lv-qty-tot,"->>>>>z")
             ELSE "" /*===================="*/ .



tt-boll.printed = yes.

    IF J > 17 THEN DO:
         {oe/rep/bolmwin3.i}
         lv-pg-num = PAGE-NUM .
         PAGE {1}.
         v-printline = 0.
         {oe/rep/bolmwin1.i}
    END.

    ASSIGN /*v-tot-wt = v-tot-wt + tt-boll.weight*/
           v-tot-palls = v-tot-palls + lv-pal-tot 
           v-tot-cases = v-tot-cases + v-tot-pkgs
           iTotBolQty  = iTotBolQty + lv-qty-tot .

    v-tot-pkgs = 0.
    lv-pal-tot = 0.
    
    ASSIGN lv-cases-tot = 0
       lv-qty-tot = 0
       lv-qcase-tot = 0
       lv-partial-tot = 0
       lv-pal-tot = 0
       .

END. /* for eac tt-boll */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


