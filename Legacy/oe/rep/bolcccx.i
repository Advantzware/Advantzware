
/* ---------------------------------------------- oe/rep/bolcccx.i     */
/* PRINT CCC Box BOL                                                           */
/* -------------------------------------------------------------------------- */


assign
 v-tot-cases = 0.

for each report where report.term-id eq v-term-id,
    first oe-boll where recid(oe-boll) eq report.rec-id,
    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
    first itemfg where itemfg.company eq oe-boll.company
                       and itemfg.i-no    eq oe-boll.i-no NO-LOCK
    break by report.key-01  /* i-no */
          BY report.key-03  /* i-no + po# + qty-case */
          by report.key-02: /* ord-no */
    
  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
     find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
     if not avail w2 then create w2.
     ASSIGN w2.cas-cnt = oe-boll.qty-case
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
      end. /* first w2.cas-cnt*/

      assign
       i           = i + 1
       v-part-comp = if v-ship-qty + v-bol-qty ge v-ord-qty or oe-boll.p-c
                     then "C" else "P"
       v-part-dscr = "".
    end. /* for each w2 */

    for each w2:
        delete w2.
    end.
  end.  /* last-of key-02*/
  RUN get-pallets-num ( OUTPUT v-pal). 
  ASSIGN lv-cases-tot = lv-cases-tot + oe-boll.cases
         lv-qty-tot = lv-qty-tot + oe-boll.qty
         lv-qcase-tot = /*lv-qcase-tot + */ oe-boll.qty-case
         lv-partial-tot = lv-partial-tot + oe-boll.partial
         lv-pal-tot = lv-pal-tot + v-pal.
  
  IF LAST-OF(report.key-03) THEN DO:
      find first oe-ordl where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq int(report.key-02)
          and oe-ordl.i-no    eq report.key-01
          no-lock no-error.

      FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no EQ oe-ordl.job-no
          AND job-hdr.job-no2 EQ oe-ordl.job-no2
          AND job-hdr.i-no EQ oe-ordl.i-no NO-LOCK NO-ERROR.

    v-job-no = "".
    if avail oe-ordl and oe-ordl.job-no ne "" then
        v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
               trim(oe-ordl.job-no) .
    IF AVAIL job-hdr THEN
        v-job-no = v-job-no + "-" + trim(string(job-hdr.frm)) + trim(string(job-hdr.blank-no)) .

    IF v-job-no EQ "" AND NOT AVAIL job-hdr THEN do:
        FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
          AND job-hdr.job-no EQ oe-boll.job-no
          AND job-hdr.job-no2 EQ oe-boll.job-no2
          AND job-hdr.i-no EQ oe-boll.i-no NO-LOCK NO-ERROR.

        v-job-no = fill(" ",6 - length(trim(oe-boll.job-no))) +
               trim(oe-boll.job-no) .
    IF AVAIL job-hdr THEN
        v-job-no = v-job-no + "-" + trim(string(job-hdr.frm)) + trim(string(job-hdr.blank-no)) .
    END.

      /* v-job-no = fill(" ",6 - length(trim(oe-ordl.job-no))) +
               trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")). */
    
    lv-cases = lv-cases-tot.
    IF AVAIL oe-ordl THEN FIND oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF v-printline >= 33 THEN DO:
             PUT {1} "<R49><C1>" lv-prt-date "  " lv-prt-time "   "  caps(oe-bolh.USER-ID)  "   " lv-prt-sts
                     "<C69>Page " /*string(PAGE-NUM - lv-pg-num,">>9")*/ STRING(PAGE-NUMBER) + " of <#PAGES> "  FORM "x(20)" .
             PAGE {1}.
             v-printline = 0.  
             IF cPrintFormat EQ "CCC" THEN do:
                 {oe/rep/bolccc1.i}
             END.
             ELSE DO:
                 {oe/rep/bolcent2.i}
             END.

    END.

    DISPLAY  {1}
          oe-ordl.part-no   WHEN AVAIL oe-ordl 
          oe-boll.po-no 
          oe-ordl.i-name FORM "x(27)"
          /*oe-boll.tag*/ oe-boll.lot-no  FORM "x(20)"
          /*oe-boll.i-no WHEN oe-boll.tag = "" @ oe-boll.tag*/
          lv-pal-tot FORM ">>>,>>9"
          lv-cases-tot FORM "->>>>" AT 91 "@" 
          lv-qcase-tot FORM "->>>>>Z"           
      
        
         lv-qty-tot FORM "->>>>>z"
                
         with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
    down {1} with frame bol-mid1.

    IF v-printline >= 33 THEN DO:
             PUT {1} "<R49><C1>" lv-prt-date "  " lv-prt-time "   "  caps(oe-bolh.USER-ID)  "   " lv-prt-sts
                     "<C69>Page " /*string(PAGE-NUM - lv-pg-num,">>9")*/ STRING(PAGE-NUMBER) + " of <#PAGES> "  FORM "x(20)" .
             PAGE {1}.
             v-printline = 0.  
             IF cPrintFormat EQ "CCC" THEN do:
                 {oe/rep/bolccc1.i}
             END.
             ELSE DO:
                 {oe/rep/bolcent2.i}
             END.

    END.

    v-lot-no = oe-boll.lot-no.

    v-unit-qty = IF lv-partial-tot > 0 
                    THEN STRING(v-1,">>>>9") + " @ " + STRING(lv-partial-tot,"->>>>>z") + "      " 
                         + string(lv-qty-tot,"->>>>>z")
                 ELSE "" /*===================="*/ .

    IF oe-ordl.part-dscr1 <> "" OR v-unit-qty <> "" /*OR v-lot-no <> ""*/ OR v-job-no <> "" THEN DO:
       PUT {1}
          v-job-no
          oe-ordl.part-dscr1 AT 33 FORM "x(27)"
          /*v-lot-no AT 61 FORM "X(15)"*/
          v-unit-qty AT 91 FORM "x(15)"  
          .
        v-printline = v-printline + 1.
    END.

    PUT {1} SKIP.

    put {1} skip(1).
    v-printline = v-printline + 2.
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

          IF v-printline >= 33 THEN DO:
             PUT {1} "<R49><C1>" lv-prt-date "  " lv-prt-time "   "  caps(oe-bolh.USER-ID)  "   " lv-prt-sts
                 "<C69>Page " /*string(PAGE-NUM - lv-pg-num,">>9")*/ STRING(PAGE-NUMBER) + " of <#PAGES> "  FORM "x(20)" .
            /* PUT {1} SKIP(5) "*CONTINUED*" AT 52.*/
             PAGE {1}.
             v-printline = 0.  
             IF cPrintFormat EQ "CCC" THEN do:
                  {oe/rep/bolccc1.i}
             END.
             ELSE DO:
                {oe/rep/bolcent2.i}
                     
             END.
          END.

          v-part-dscr = string(fg-set.part-no,"x(16)") +
		                (if avail xitemfg then xitemfg.i-name else "").

          {sys/inc/part-qty.i v-part-qty fg-set}

          IF AVAIL fg-bin THEN DO:
             put {1}
	          v-part-dscr              at 32 format "x(39)"
              oe-boll.cases TO 81  FORM ">>>9" " @ " 
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
          

    end. /* isaset */
    
    ASSIGN v-tot-cases = v-tot-cases + lv-pal-tot
           v-tot-pkgs = 0
           lv-cases-tot = 0
           lv-qty-tot = 0
           lv-qcase-tot = 0
           lv-partial-tot = 0
           lv-pal-tot = 0.
    END. /* last of item */
end. /* for each report */

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


