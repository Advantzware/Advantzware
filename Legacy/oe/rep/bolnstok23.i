
/* oe/rep/bolnstok23.i*/
IF LAST-OF(tt-boll.LINE) THEN DO:
  FOR EACH w2.
      DELETE w2.
  END.
  ASSIGN
     i = 0
     v-tot-case-qty = 0.

  FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                        AND bf-ttboll.ord-no = tt-boll.ord-no
                       AND bf-ttboll.po-no = tt-boll.po-no
                       AND bf-ttboll.LINE = tt-boll.LINE
                       /*AND bf-ttboll.loc-bin = tt-boll.loc-bin*/
                BREAK BY bf-ttboll.cases DESC.
      find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq bf-ttboll.ord-no
         and oe-ordl.i-no    eq bf-ttboll.i-no
         and oe-ordl.line    eq bf-ttboll.LINE no-lock no-error.
       ASSIGN v-relqty = 0 .
      
       FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.r-no = oe-boll.r-no
                     NO-LOCK NO-ERROR.
       
       FOR EACH oe-rell WHERE oe-rell.company  EQ tt-boll.company
                           AND oe-rell.r-no     EQ tt-boll.r-no
                           AND oe-rell.ord-no   EQ tt-boll.ord-no
                           AND oe-rell.i-no     EQ tt-boll.i-no
                           AND oe-rell.line     EQ tt-boll.line
                           AND oe-rell.rel-no   EQ tt-boll.rel-no
                           AND oe-rell.b-ord-no EQ tt-boll.b-ord-no
                           AND oe-rell.po-no    EQ tt-boll.po-no NO-LOCK :
      
          ASSIGN v-relqty = v-relqty + oe-rell.qty .
       END.
      
      find first oe-ord where oe-ord.company eq cocode
         and oe-ord.ord-no  eq tt-boll.ord-no no-lock no-error.
      ASSIGN
         v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty
         i = i + 1.
      find first w2 where w2.cas-cnt eq bf-ttboll.qty-case no-error.
      if not avail w2 then create w2.
      ASSIGN w2.job-po = ""
             w2.bin = ""
             w2.i-no = ""
             w2.cas-cnt = bf-ttboll.qty-case
             w2.cases   = w2.cases + bf-ttboll.cases
             w2.rec-id = RECID(bf-ttboll).

      IF i = 1 THEN ASSIGN w2.job-po = bf-ttboll.po-no
                           w2.bin = bf-ttboll.loc-bin
                           w2.dscr = oe-ordl.part-no
                           w2.qty = oe-ordl.qty
                           w2.relqty = v-relqty 
                           w2.i-no = oe-ordl.i-no.

      ELSE IF i = 2 THEN 
          ASSIGN /*w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 w2.dscr = oe-ordl.i-name
                 w2.job-po = string(bf-ttboll.ord-no) .
      else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
      ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
      ELSE if i eq 5 then ASSIGN w2.dscr = itemfg.part-dscr3.

  END.
  IF i < 5 THEN DO i = i TO 5:
      CREATE w2.
  END.
  i = 0.
  
  FOR EACH w2  BREAK BY w2.cases DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.    
    i = i + 1.
    IF w2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       w2.i-no = "".
       IF i = 1 AND AVAIL oe-ordl THEN
            w2.i-no = oe-ordl.i-no .
       IF i = 2 THEN 
          ASSIGN /*w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 w2.dscr = oe-ordl.i-name
                 w2.job-po = string(oe-ordl.ord-no)
                 /*w2.i-no = oe-ordl.i-no*/ .
       else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
       ELSE if i eq 5 then ASSIGN w2.dscr = itemfg.part-dscr3.
    END.
    IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND w2.cas-cnt = 0 THEN DELETE w2.
  END.
  i = 0.
  FOR EACH w2  BREAK BY w2.cases DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.    
    i = i + 1.

    IF w2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       w2.i-no = "".
       IF i = 1 AND AVAIL oe-ordl THEN
            w2.i-no = oe-ordl.i-no .
       IF i = 2 THEN 
          ASSIGN /*w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 w2.dscr = oe-ordl.i-name
                 w2.job-po = string(oe-ordl.ord-no)
                 /*w2.i-no = oe-ordl.i-no */ .
       else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
       ELSE if i eq 5 then ASSIGN w2.dscr = itemfg.part-dscr3.
    END.
    /*IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND NOT last(w2.cases) THEN .
    ELSE DO:   */

       DISPLAY w2.job-po
             w2.bin /*trim(string(w2.qty,"->>,>>>,>>>")) WHEN i = 1 @ w2.i-no*/ 
             w2.i-no
             w2.dscr
             w2.relqty
             w2.cases
             w2.cas-cnt
             v-tot-case-qty when first (w2.cases) @ tt-boll.qty
             bf-ttboll.p-c  WHEN AVAIL bf-ttboll AND FIRST (w2.cases) @ bf-ttboll.p-c                         
           /* 1  WHEN i = 2 AND bf-ttboll.partial > 0  @ w2.cases
            tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ w2.cas-cnt */
           with frame bol-mid.
       DOWN WITH FRAME bol-mid.  
       
       v-printline = v-printline + 1.
    /*END.*/
    IF v-printline >= 39 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolnstok22.i}
    END.
    v-tot-cases = v-tot-cases + w2.cases.

    /*delete w2. */
  end. /* each w2 */

  put {1} skip(1).
  v-printline = v-printline + 1. 
  tt-boll.printed = yes.
  
  if v-print-components AND itemfg.alloc NE YES then
  for each fg-set
      where fg-set.company eq cocode
        and fg-set.set-no  eq tt-boll.i-no
      no-lock,
      
      first b-itemfg
      where b-itemfg.company eq cocode
        and b-itemfg.i-no    eq fg-set.part-no
      no-lock
      
      break by fg-set.set-no:
      
    {sys/inc/part-qty.i v-part-qty fg-set}

    IF v-printline >= 39 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolnstok22.i}
    END.

    display {1}
            trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                    @ w2.i-no
            b-itemfg.part-no                        @ w2.dscr
            tt-boll.qty * v-part-qty                @ tt-boll.qty        
        with frame bol-mid.
    down {1} with frame bol-mid.
    v-printline = v-printline + 1.
    display {1}
            fg-set.part-no                          @ w2.i-no
            v-job-po     @ w2.job-po
            b-itemfg.i-name                         @ w2.dscr
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    put {1} skip(1).
    v-printline = v-printline + 2.
  end.
END. /* first-of(tt-boll.line) */

