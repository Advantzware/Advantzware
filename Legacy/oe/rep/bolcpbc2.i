/* ---------------------------------------------- oe/rep/bolcpbc2.i YSK     */
/* PRINT Consolidated Box                                                  */
/* -------------------------------------------------------------------------- */
assign
 v-tot-cases = 0
 v-tot-palls = 0.

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

  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-LOCK NO-ERROR.
  ASSIGN lv-cases-tot = lv-cases-tot + tt-boll.cases
         lv-qty-tot = lv-qty-tot + tt-boll.qty
         lv-qcase-tot = lv-qcase-tot + tt-boll.qty-case
         lv-partial-tot = lv-partial-tot + tt-boll.partial
         lv-pal-tot = lv-pal-tot + tt-boll2.pallets
         v-job-no = fill(" ",6 - length(trim(tt-boll.job-no))) +
                    trim(tt-boll.job-no) + "-" +
                    trim(string(tt-boll.job-no2,"99"))
         tt-boll.printed = yes.
         lv-cases = lv-cases-tot.

  IF trim(v-job-no) = "-00" THEN v-job-no = "".

  IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
     IF LAST-OF(tt-boll.ord-no) THEN DO:
         ASSIGN
            i = 0
            lv-qty-sum = 0
            v-po-printed = NO.

         FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
             i = i + 1.
             IF i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no.
             ELSE if i eq 2 THEN 
                  ASSIGN v-part-dscr = oe-ordl.part-dscr1.
             else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
             ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
             
             IF v-printline >= 39 AND FIRST(w2.cases * w2.cas-cnt) THEN DO:
                PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                PAGE {1}.
                v-printline = 0.
                {oe/rep/bolcpbc1.i}
             END.
             lv-qty-sum = lv-qty-sum + w2.cases.
             IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
                PUT {1}
                    oe-ordl.i-no SPACE(1)
                    oe-ordl.ord-no /*FORM "x(15)"*/ SPACE(9)
                    oe-ordl.i-name  FORM "x(25)" SPACE(1)
                    lv-pal-tot SPACE(1)
                    w2.cases    FORM "->>>9" " @ " 
                    w2.cas-cnt    FORM "->>>>>9" 
                    lv-qty-tot FORM "->>>>>>>z" SPACE(2)
                    tt-boll.p-c                    
                    SKIP.
                 lv-qty-sum2 = lv-qty-sum2 + lv-qty-tot.
             END.
             ELSE
             DO:
                IF v-po-printed = NO THEN
                DO:
                   v-po-printed = YES.

                   PUT oe-ordl.part-no   SPACE(1)
                       tt-boll.po-no FORM "x(15)" 
                       oe-ordl.part-dscr1  FORM "x(30)" AT 32 SPACE(1).
                END.

                PUT w2.cases   AT 69  FORM "->>>9" " @ "
                    w2.cas-cnt FORM "->>>>>9" 
                    SPACE(11)
                    tt-boll.p-c
                    SKIP.
             END.

             v-printline = v-printline + 1.
             
             IF last(w2.cases * w2.cas-cnt) THEN DO:
                IF FIRST(w2.cases * w2.cas-cnt) THEN do:

                   PUT {1} 
                      oe-ordl.part-no   SPACE(1)
                      tt-boll.po-no FORM "x(15)" 
                      oe-ordl.part-dscr1  FORM "x(30)" AT 32 SPACE(1)
                      SKIP.
                   v-printline = v-printline + 1.
                END.
                IF last(tt-boll.ord-no) AND v-printline >= 33 THEN DO:
                   PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
                   PAGE {1}.
                   v-printline = 0.
                   {oe/rep/bolcpbc1.i}
                END.
                PUT {1} "====================" AT 68 SKIP
                    lv-qty-sum AT 69 FORM "->>>9"  " = "
                    lv-qty-tot FORM "->>>>>z" SPACE(11)
                    tt-boll.p-c .
                IF lv-partial-tot > 0 THEN
                    PUT {1} tt-boll.p-c SKIP .         
                ELSE PUT {1} SKIP.
                v-printline = v-printline + 2.
             END.
             
             DELETE w2.
         END.
         ASSIGN
            lv-qty-tot = 0
            lv-qty-sum = 0
            v-printline = v-printline + 1.

         put {1} skip(1).
        
     END.
  END.
  /* end of summary mods */

ELSE DO:
   lv-qty-sum2 = lv-qty-sum2 + lv-qty-tot.
   DISPLAY  {1}
      oe-ordl.i-no WHEN AVAIL oe-ordl
      oe-ordl.ord-no /*FORM "x(15)"*/ SPACE(9)
      oe-ordl.i-name  FORM "x(25)" 
      lv-pal-tot 
      lv-cases-tot FORM ">>>>" "@" 
      lv-qcase-tot FORM ">>>>>Z"           
    SPACE(1)
     lv-qty-tot FORM "->>>>>>>z"  WHEN lv-partial-tot = 0 SPACE(2)
     tt-boll.p-c
    with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
   down {1} with frame bol-mid1.

   v-unit-qty = IF lv-partial-tot > 0 
                THEN STRING(v-1,">>>>9") + " @ " + STRING(lv-partial-tot,">>>>>z") + " " 
                     + string(lv-qty-tot,"->>>>>>>z")
             ELSE "".
   
   PUT {1}
      oe-ordl.part-no   SPACE(1)
      tt-boll.po-no FORM "x(15)"
      oe-ordl.part-dscr1 AT 32 
      v-unit-qty AT 68  FORM "x(26)".

   v-printline = v-printline + 2.
   IF lv-partial-tot > 0 THEN
      PUT {1}
        tt-boll.p-c SKIP .         
   ELSE PUT {1} SKIP.
   put {1} skip(1).
   v-printline = v-printline + 1.

   IF v-printline >= 39 THEN DO:
      PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
      PAGE {1}.
      v-printline = 0.
      {oe/rep/bolcpbc1.i}
   END.
END. /* else - not summary */

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
          ASSIGN lv-comp-unit = trunc((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0).
       ELSE lv-comp-unit = 0.

       v-part-dscr = string(fg-set.part-no,"x(16)") +
                     (if avail xitemfg then xitemfg.i-name else "").

       {sys/inc/part-qty.i v-part-qty fg-set}

       IF v-printline >= 39 THEN DO:
          PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
          PAGE {1}.
          v-printline = 0.
          {oe/rep/bolcpbc1.i}
       END.
      
       IF AVAIL fg-bin THEN DO:
          put {1}
           v-part-dscr              at 32 format "x(39)"
           tt-boll.cases TO 81  FORM ">>>9" " @ " 
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
              skip.
           v-printline = v-printline + 1.
       END.
       IF v-printline >= 39 THEN DO:
          PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
          PAGE {1}.
          v-printline = 0.
          {oe/rep/bolcpbc1.i}
       END.

    end. /* isaset */

    ASSIGN v-tot-palls = v-tot-palls + lv-pal-tot 
           v-tot-cases = v-tot-cases + v-tot-pkgs
           v-tot-pkgs = 0
           lv-pal-tot = 0.

    IF lv-bolfmt-int <> 1 THEN
        ASSIGN lv-cases-tot = 0
               lv-qty-tot = 0
               lv-qcase-tot = 0
               lv-partial-tot = 0
               lv-pal-tot = 0.

END. /* for each tt-boll */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


