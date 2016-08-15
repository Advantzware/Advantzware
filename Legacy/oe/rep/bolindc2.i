/* ---------------------------------------------- oe/rep/bolindc.i YSK     */
/* PRINT Indiana Carton BOL                                                           */
/* -------------------------------------------------------------------------- */


assign
 /*v-tot-wt = 0*/
 v-tot-cases = 0
 v-tot-palls = 0.
/*
for each report where report.term-id eq v-term-id,
    first oe-boll where recid(oe-boll) eq report.rec-id,
    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
    first itemfg where itemfg.company eq oe-boll.company
                       and itemfg.i-no    eq oe-boll.i-no NO-LOCK
    break by report.key-01  /* i-no */
          by report.key-02: /* ord-no */
*/
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
         lv-pal-tot = lv-pal-tot + tt-boll2.pallets.

  v-job-no = "".
/*  if avail oe-ordl and oe-ordl.job-no ne "" then*/
v-job-no = fill(" ",6 - length(trim(tt-boll.job-no))) +
           trim(tt-boll.job-no) + "-" + trim(string(tt-boll.job-no2,"99")).
IF trim(v-job-no) = "-00" THEN v-job-no = "".

/*v-part-comp = if v-ship-qty + v-bol-qty ge v-ord-qty or  tt-boll.p-c
                     then "C" else "P"*/

lv-cases = lv-cases-tot.
lv-class = IF itemfg.frt-class = "" THEN "70" ELSE itemfg.frt-class.
DISPLAY  {1}
      oe-ordl.i-no WHEN AVAIL oe-ordl
      /*oe-boll.i-no */ tt-boll.po-no FORM "x(15)"
      oe-ordl.i-name  FORM "x(25)" 
      /*lv-pal-tot */ 
      /*oe-boll.cases*/ lv-cases-tot FORM ">>>>" /*AT 79*/  "@" 
      /*oe-boll.qty-case*/ lv-qcase-tot FORM ">>>>>Z"           
    SPACE(1)
     lv-qty-tot FORM "->>>>>z"  WHEN lv-partial-tot = 0 SPACE(3)
     lv-class FORM "x(3)" SPACE(5)
     /*tt-boll.p-c /*v-part-comp WHEN lv-partial-tot = 0*/*/
     tt-boll.weight
     with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
down {1} with frame bol-mid1.

v-unit-qty = IF lv-partial-tot > 0 
                THEN STRING(v-1,">>>>9") + " @ " + STRING(lv-partial-tot,"->>>>>z") + " " 
                     + string(lv-qty-tot,"->>>>>z")
             ELSE "" /*===================="*/ .

/*v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).*/

/*IF oe-ordl.part-dscr1 <> "" OR v-unit-qty <> "" THEN*/
   PUT {1}
      oe-ordl.part-no   
     /* v-job-no AT 16*/
      oe-ordl.part-dscr1 AT 33 
     /* v-1    FORM ">>9"           when /*oe-boll.partial*/ lv-partial-tot gt 0 AT 79
      "@" when lv-partial-tot gt 0
      lv-partial-tot /*oe-boll.partial*/   when lv-partial-tot gt 0 FORM "->>>>>z"  SKIP 
      "====================" AT 77 SKIP  */
     /* v-unit-qty AT 68  FORM "x(25)" */
      .

v-printline = v-printline + 2.
/*IF lv-partial-tot > 0 THEN /*PUT {1} "====================" AT 77 SKIP  . */
   PUT {1}
    /*  lv-cases /*v-tot-pkgs*/ AT 79 FORM ">>>" " = "
      lv-qty-tot /*oe-boll.qty */ FORM "->>>>>z" SPACE(12) */
      tt-boll.p-c /*v-part-comp */  /*AT 91*/  /*SPACE(12)*/
     /* oe-boll.weight*/  SKIP .         
ELSE PUT {1} SKIP.
*/
put {1} skip(1).
v-printline = v-printline + 1.
tt-boll.printed = yes.

    IF v-printline >= 32 THEN DO:
         PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
         PAGE {1}.
         v-printline = 0.
         {oe/rep/bolfran1.i}
    END.

     /* display componets of set */
  if itemfg.isaset AND v-print-components then
      for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

          find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.

          FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = tt-boll.job-no
                            AND fg-bin.job-no2 = tt-boll.job-no2 NO-LOCK NO-ERROR.

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
	          v-part-dscr              at 32 format "x(39)"
	          /*oe-boll.qty * v-part-qty to 68 format ">>>>9"	*/
              /*lv-comp-unit */ tt-boll.cases TO 81  FORM ">>>9" " @ " 
              fg-bin.case-count FORM "->>>>>z"
              skip.              

              v-printline = v-printline + 1.
              IF fg-bin.partial-count <> 0 THEN do:
                 PUT {1} "  1" TO 81  " @ " fg-bin.partial-count FORM "->>>>>z" SKIP.
                 v-printline = v-printline + 1.
              END.
          END.
          ELSE DO:
              put {1}
	             v-part-dscr              at 32 format "x(39)"
	             /*oe-boll.qty * v-part-qty to 68 format ">>>>9"	*/          
                 skip.
              v-printline = v-printline + 1.
          END.
          IF v-printline >= 32 THEN DO:
             PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
            /* PUT {1} SKIP(5) "*CONTINUED*" AT 52.*/
             PAGE {1}.
             v-printline = 0.
             {oe/rep/bolfran1.i}
          END.

    end. /* isaset */

    ASSIGN /*v-tot-wt = v-tot-wt + oe-boll.weight*/
           v-tot-palls = v-tot-palls + lv-pal-tot 
           v-tot-cases = v-tot-cases + v-tot-pkgs.

    v-tot-pkgs = 0.
    lv-pal-tot = 0.
    IF v-printline >= 32 THEN DO:
         PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
         PAGE {1}.
         v-printline = 0.
         {oe/rep/bolfran1.i}
    END.
    ASSIGN lv-cases-tot = 0
       lv-qty-tot = 0
       lv-qcase-tot = 0
       lv-partial-tot = 0
       lv-pal-tot = 0
       .

END. /* for eac tt-boll */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


