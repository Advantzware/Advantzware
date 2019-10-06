/* ---------------------------------------------- oe/rep/bolkni2.i     */
/* PRINT Knight BOL                                                           */
/* -------------------------------------------------------------------------- */


assign
 v-tot-cases = 0
 v-tot-palls = 0.

for each report where report.term-id eq v-term-id,
    first oe-boll where recid(oe-boll) eq report.rec-id,
    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
    first itemfg where itemfg.company eq oe-boll.company
                       and itemfg.i-no    eq oe-boll.i-no NO-LOCK
    break by report.key-01  /* i-no */
          by report.key-02 /* ord-no */
          BY oe-boll.line
          BY oe-boll.po-no
          BY oe-boll.job-no
          BY oe-boll.job-no2:

    IF FIRST-OF(report.key-02) THEN DO:
        ASSIGN v-ship-qty = 0
               v-weight   = 0
               v-ord-qty  = 0
               v-pal-line = 0.
    END.
  
    ASSIGN v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                        if oe-boll.partial gt 0 then 1 else 0
           v-pal-cnt  = oe-boll.tot-pallets
           v-tot-palls = v-tot-palls + v-pal-cnt
           v-pal-line   = v-pal-line + v-pal-cnt.

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
    
    find first oe-ordl where oe-ordl.company eq cocode
           and oe-ordl.ord-no  eq int(report.key-02)
           and oe-ordl.i-no    eq report.key-01
           no-lock no-error.

    IF v-printline >= 37 THEN DO:
        v-printline = 0.
        PAGE {1}.
       {oe/rep/bolkni1.i}
    END.

    v-job-no = "".
    if avail oe-ordl and oe-ordl.job-no ne "" then
       v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
                  trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

    ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
           v-weight   = v-weight + oe-boll.weight
           v-ord-qty = v-ord-qty + oe-ordl.qty.

    IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item */
      IF LAST-OF(report.key-02) THEN DO:
        i = 0.
        FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:
          i = i + 1.
          IF i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no
                                v-job-po    = oe-boll.po-no.
          ELSE
          if i eq 2 THEN ASSIGN v-part-dscr = oe-ordl.part-dscr1 /*i-name*/
                                v-job-po    = if oe-ordl.job-no eq "" then "" else
                                  (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
          ELSE
          if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
          ELSE
          if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
               
          IF v-printline >= 37 THEN DO:
            v-printline = 0.
            PAGE {1}.
            {oe/rep/bolkni1.i}
          END.
    
          IF FIRST(w2.cases * w2.cas-cnt) THEN 
             PUT  {1}
                oe-ordl.part-no FORM "x(14)"  /*WHEN AVAIL oe-ordl */ 
                v-job-po        FORM "x(17)"
                oe-ordl.i-name  FORM "x(25)"
                v-pal-line      FORM "->>>>9"
                w2.cases    AT 65 FORM ">>>>9" " @" 
                w2.cas-cnt  FORM "->>>>>9" /*AT 79*/  SPACE(2)
                (w2.cases * w2.cas-cnt) FORM "->>,>>>,>>9"
                SKIP.
          ELSE
              PUT  {1}
                oe-boll.lot-no FORMAT "x(14)"
                oe-ordl.ord-no AT 15
                oe-ordl.part-dscr1  FORM "x(25)" AT 32 SPACE(6)
                w2.cases  AT 65 FORM "->>>9" " @"
                w2.cas-cnt FORM "->>>>>9" SPACE(2)
                (w2.cases * w2.cas-cnt) FORM "->>,>>>,>>9"
                SKIP.
    
          v-printline = v-printline + 1.
    
          IF LAST(w2.cases * w2.cas-cnt) THEN DO:
            IF FIRST(w2.cases * w2.cas-cnt) THEN DO:
              PUT {1} 
                  oe-boll.lot-no FORMAT "x(14)"
                  oe-ordl.ord-no AT 15
                  oe-ordl.part-dscr1 FORM "x(25)" AT 32 
                  SKIP.
              v-printline = v-printline + 1.
            END.
    
            PUT {1}
                "===========================" AT 65 FORM "x(27)" SKIP
                v-tot-pkgs  AT 63 FORM "->>,>>9"  " =" SPACE(12)
                v-ship-qty FORM "->>>,>>z" SPACE(1)
                /*v-weight  FORM "->>>>,>>9" space(2)*/
                oe-boll.p-c SKIP.
    
            ASSIGN
               v-printline = v-printline + 2
               v-tot-pkgs  = 0.
          END. /* last w2.cases */
          v-tot-cases = v-tot-cases + w2.cases.
          DELETE w2.
        END. /* for each */

        PUT {1} SKIP(1).
        v-printline = v-printline + 1.
      END. /* LAST of key 02 */

    END. /* integer value = 1 Summarize */

    ELSE DO:
        
       DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl FORM "x(14)" 
          oe-boll.po-no    FORM "x(16)"
          oe-ordl.i-name  FORM "x(25)"
          v-pal-cnt FORM ">>>>9"
          oe-boll.cases FORM "->>>>>" "@" SPACE(0)
          oe-boll.qty-case FORM "->>>,>>Z" SKIP          
          oe-boll.lot-no FORMAT "x(14)" oe-ordl.ord-no AT 15 oe-ordl.part-dscr1 AT 33 FORM "x(25)" SPACE(8)
          v-1    FORM "->>>9"  when oe-boll.partial gt 0 "@" SPACE(0)
          oe-boll.partial   when oe-boll.partial gt 0 FORM "->>>>>z"  SKIP
       with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
       down {1} with frame bol-mid1.

       DISPLAY              
          "===========================" AT 65 FORM "x(27)" SKIP
          v-tot-pkgs  AT 63 FORM "->>>,>>9"  "=" SPACE(11)
          oe-boll.qty FORM "->>>,>>z" SPACE(1)
          oe-boll.p-c 
          with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
       down {1} with frame bol-mid2.

       v-printline = v-printline + 5.

       put {1} skip(1).
       ASSIGN
          v-tot-cases = v-tot-cases + v-tot-pkgs
          v-tot-pkgs  = 0.
    END. /* else do */

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
	          v-part-dscr              at 32 format "x(39)"
              oe-boll.cases TO 81  FORM ">>>9" " @ " 
              fg-bin.case-count FORM "->,>>>,>>z"
              skip.              

              v-printline = v-printline + 1.
              IF fg-bin.partial-count <> 0 THEN do:
                 PUT {1} "  1" TO 81  " @ " fg-bin.partial-count FORM "->,>>>,>>z" SKIP.
                 v-printline = v-printline + 1.
              END.
          END.
          ELSE DO:
              put {1}
	             v-part-dscr              at 32 format "x(39)"
                 skip.
              v-printline = v-printline + 1.
          END.
          IF v-printline >= 37 THEN DO:
             PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
             PAGE {1}.
             v-printline = 0.
             {oe/rep/bolkni1.i}
          END.

    end. /* isaset */
    
    IF v-printline >= 37 THEN DO:
       PUT {1} "<R63><C69>Page " string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" .
       PAGE {1}.
       v-printline = 0.
       {oe/rep/bolkni1.i}
    END.
    
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


