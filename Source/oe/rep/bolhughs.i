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
          BY report.key-03
          by report.key-02:

  v-tot-pkgs = v-tot-pkgs + oe-boll.tot-pallets.
  /*v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
                if oe-boll.partial gt 0 then 1 else 0.*/

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
     v-rel-qty = oe-rell.qty.
  end.
  IF ll-consol-bolls THEN DO:
    v-rel-qty = 0.
    FOR EACH oe-rell
        where oe-rell.company eq cocode
          and oe-rell.r-no    eq oe-boll.r-no
          AND oe-rell.ord-no  EQ oe-boll.ord-no
          and oe-rell.i-no    eq oe-boll.i-no
          and oe-rell.line    eq oe-boll.line
        NO-LOCK.
        v-rel-qty = v-rel-qty + oe-rell.qty.
    END.
  END.
  
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
  
/*       AND tt-boll.po-no    EQ oe-boll.po-no
       AND tt-boll.ord-no   EQ oe-boll.ord-no
       AND tt-boll.line     EQ oe-boll.LINE */
/*     NO-LOCK NO-ERROR. */
  IF LAST-OF(report.key-01) AND ll-consol-bolls AND avail(tt-boll) THEN DO:
      
      v-tot-pkgs = v-tot-pkgs + tt-boll.cases +
                   if tt-boll.partial gt 0 then 1 else 0.
      ASSIGN
        v-boll-qty   = 0
        v-gt-package = 0
        v-gt-qty     = 0
        v-gt-weight  = 0
        v-gt-cases   = 0
        v-2nd-line-printed = NO.
      FOR EACH tt-boll
         WHERE tt-boll.i-no     EQ oe-boll.i-no
           /* AND tt-boll.qty-case = INTEGER(report.key-03)*/
           BREAK BY tt-boll.qty-case:

        IF FIRST-OF(tt-boll.qty-case) THEN DO:
          ASSIGN
            /*v-gt-package = v-gt-package + v-tot-pkgs */
            v-boll-qty   = tt-boll.qty
            v-gt-package = v-tot-pkgs
            v-gt-qty     = v-gt-qty + tt-boll.tot-qty  /*(tt-boll.cases * tt-boll.qty-case)*/
            v-gt-weight  = v-gt-weight + tt-boll.weight
            v-gt-cases   = v-gt-cases + /*tt-boll.cases*/ tt-boll.tot-pallets.
            v-pallets    = /*tt-boll.cases*/ tt-boll.tot-pallets.
        END.

        IF FIRST(tt-boll.qty-case) THEN DO:
          DISPLAY {1}
                oe-ordl.part-no    WHEN AVAIL oe-ordl  
                tt-boll.po-no @ oe-boll.po-no  AT 16 
                tt-boll.i-no @ oe-boll.i-no    AT 31 
                oe-ordl.i-name @ oe-ordl.i-name  FORM "x(25)" AT 47
                v-pallets @ oe-boll.cases FORM ">>>>" AT 72  "@" @ v-at AS CHAR FORMAT "x"  
                /*tt-boll.qty-case @ oe-boll.qty-case FORM "->>>>>Z" */
                v-boll-qty FORM "->>>>>Z" 
                v-part-comp
                SPACE(3)
                tt-boll.weight
                SKIP          
                with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
          IF FIRST(tt-boll.qty-case) AND LAST(tt-boll.qty-case) THEN
              DOWN WITH FRAME bol-mid2.

        END.

        ELSE DO:
            IF v-2nd-line-printed = NO THEN DO:
               DISP {1}
                 string(oe-ordl.qty) @ oe-ordl.part-no FORM "x(15)"
                 string(v-rel-qty)  @ oe-boll.po-no  FORM "x(15)"
                 v-job-no @ oe-boll.i-no 
                 oe-ordl.part-dscr1 @ oe-ordl.i-name 
                 WITH FRAME bol-mid2.
              v-2nd-line-printed = YES.
            END.
            DISPLAY {1}
                  v-pallets @ oe-boll.cases FORM ">>>>"  "@" @ v-at
                  /*tt-boll.qty-case @ oe-boll.qty-case FORM "->>>>>Z" */
                  v-boll-qty FORM "->>>>>Z" 
                  v-part-comp
                  tt-boll.weight
                  with frame bol-mid2 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
         END. 

         IF FIRST(tt-boll.qty-case) AND LAST(tt-boll.qty-case) THEN DO:
           DISP {1}
              string(oe-ordl.qty)  @ oe-ordl.part-no  
              string(v-rel-qty) @ oe-boll.po-no 
              v-job-no @ oe-boll.i-no 
              oe-ordl.part-dscr1 @ oe-ordl.i-name
              WITH FRAME bol-mid2 .
         END.

         DOWN {1} WITH FRAME bol-mid2.
      
        v-printline = v-printline + 2.

      END.
        PUT {1}"================" AT 72 SKIP
            v-gt-cases FORM ">>>>" AT 72 "=" AT 77
            v-gt-qty FORM "->>>,>>z" AT 79
            v-gt-weight FORM ">>>>>>" AT 90 SKIP          
            .
        v-printline = v-printline + 2.
    
      v-tot-pkgs = 0.
  END.
  ELSE IF NOT ll-consol-bolls THEN DO:
        v-gt-qty     =  oe-boll.qty .
        IF v-part-comp = "" THEN ASSIGN
            v-part-comp = IF  oe-boll.p-c = NO THEN "P" ELSE "C" .
       
    DISPLAY {1}
          oe-ordl.part-no    WHEN AVAIL oe-ordl 
          oe-boll.po-no AT 16
          oe-boll.i-no AT 31
          oe-ordl.i-name AT 47 FORM "x(22)"
          oe-boll.tot-pallets AT 72 FORM ">>>>"  "@"  
          /*oe-boll.qty-case FORM "->>>>>Z" SKIP          */
          oe-boll.qty FORM "->>>>>Z" SKIP
          string(oe-ordl.qty)  AT 1 SPACE(7) 
          string(v-rel-qty)   SPACE(3)
          v-job-no AT 31 FORM "x(15)"
          oe-ordl.part-dscr1 AT 47 FORM "x(23)"
          v-1    FORM ">>>9"           when oe-boll.partial gt 0 AT 72  "@" 
          oe-boll.partial when oe-boll.partial gt 0 FORM "->>>>>z" SKIP 
          "================" AT 71 SKIP
          v-tot-pkgs FORM ">>>>" AT 71 "=" 
          v-gt-qty FORM "->,>>,>>z" SPACE(2)
          v-part-comp  SPACE(0)
          oe-boll.weight  FORM ">>>>>>" SKIP          
          with frame bol-mid1 NO-BOX NO-LABELS STREAM-IO NO-ATTR-SPACE WIDTH 130.
    down {1} with frame bol-mid1.
    v-printline = v-printline + 4.
  END.

  /* display componets of set */
  IF itemfg.isaset 
      AND (ll-consol-bolls = NO 
           OR (LAST-OF(report.key-01) AND ll-consol-bolls AND avail(tt-boll)))
      THEN DO:

    PUT SKIP(1).
    v-printline = v-printline + 1.

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
	      v-gt-qty * tt-fg-set.part-qty-dec TO 78 FORMAT ">>,>>9"
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
