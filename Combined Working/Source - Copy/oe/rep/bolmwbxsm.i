v-tot-cases = 0.
 
FOR EACH tt-boll,
      
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq tt-boll.i-no
    no-lock

    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:

IF LAST-OF(tt-boll.LINE) THEN DO:
  FOR EACH wb2.
      DELETE wb2.
  END.
  ASSIGN
     i = 0
     v-tot-case-qty = 0.

  FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                       AND bf-ttboll.po-no = tt-boll.po-no
                       AND bf-ttboll.ord-no = tt-boll.ord-no
                       AND bf-ttboll.LINE = tt-boll.LINE
                      /* AND bf-ttboll.loc-bin = tt-boll.loc-bin*/
                BREAK BY bf-ttboll.cases DESC.
      find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
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
      find first wb2 where wb2.cas-cnt eq bf-ttboll.qty-case no-error.
      if not avail wb2 then create wb2.
      ASSIGN wb2.job-po = ""
             wb2.part-no = ""
             wb2.i-no = ""
             wb2.cas-cnt = bf-ttboll.qty-case
             wb2.cases   = wb2.cases + bf-ttboll.cases
             wb2.rec-id = RECID(bf-ttboll)
             wb2.wet  = wb2.wet + bf-ttboll.weight .
      
      
      IF i = 1 THEN ASSIGN wb2.job-po = bf-ttboll.po-no
                           wb2.dscr = string(oe-ordl.i-no,"x(18)") 
                           wb2.part-no = oe-ordl.part-no
                           wb2.qty = oe-ordl.qty
                           wb2.relqty = v-relqty 
                           wb2.i-no = oe-ordl.i-no
                           .
      ELSE IF i = 2 THEN 
          ASSIGN /*wb2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 wb2.dscr = oe-ordl.part-dscr1
                 
                  .
      /*else if i eq 3 then ASSIGN
           wb2.dscr = oe-ordl.part-dscr1
           wb2.wet  = tt-boll.weight .*/
     /* ELSE if i eq 4 then ASSIGN wb2.dscr = oe-ordl.part-dscr2.
      ELSE if i eq 5 then ASSIGN wb2.dscr = itemfg.part-dscr3.*/

  END.
  IF i < 5 THEN DO i = i TO 5:
      CREATE wb2.
  END.
  i = 0.
  FOR EACH wb2  BREAK BY wb2.cases DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = wb2.rec-id NO-LOCK NO-ERROR.    
    i = i + 1.
    IF wb2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       wb2.i-no = "".
       IF i = 2 THEN 
          ASSIGN /*wb2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 wb2.dscr = oe-ordl.part-dscr1
                  .
      /* else if i eq 3 then ASSIGN wb2.dscr = oe-ordl.part-dscr1.*/
      /* ELSE if i eq 4 then ASSIGN wb2.dscr = oe-ordl.part-dscr2.
       ELSE if i eq 5 then ASSIGN wb2.dscr = itemfg.part-dscr3.*/
    END.
    IF wb2.qty = 0 and wb2.i-no = "" AND wb2.dscr = "" AND wb2.cas-cnt = 0 THEN DELETE wb2.
  END.
  i = 0.
  FOR EACH wb2  BREAK BY wb2.cases DESC BY wb2.cas-cnt DESC:
    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = wb2.rec-id NO-LOCK NO-ERROR.    
    i = i + 1.

    IF wb2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       wb2.i-no = "".
       IF i = 2 THEN 
          ASSIGN /*wb2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
                 wb2.dscr = oe-ordl.part-dscr1
                 wb2.job-po = string(oe-ordl.ord-no)
                 /*wb2.i-no = oe-ordl.i-no */ .
       /*else if i eq 3 then ASSIGN wb2.dscr = oe-ordl.part-dscr1.*/
       /*ELSE if i eq 4 then ASSIGN wb2.dscr = oe-ordl.part-dscr2.
       ELSE if i eq 5 then ASSIGN wb2.dscr = itemfg.part-dscr3.*/
    END.
   /* IF wb2.qty = 0 and wb2.i-no = "" AND wb2.dscr = "" AND NOT last(wb2.cases) THEN .
    ELSE DO:   */ 
    IF FIRST-OF(wb2.cas-cnt) THEN
             v-wet-tot =  v-wet-tot + wb2.wet . 

    IF FIRST(wb2.cases) THEN do:
        ASSIGN
            v-pc = IF AVAIL bf-ttboll AND bf-ttboll.p-c EQ TRUE THEN "C" 
               ELSE IF AVAIL bf-ttboll AND bf-ttboll.p-c EQ FALSE THEN "P"  ELSE ""  .
          
            IF AVAIL oe-ordl THEN
             wb2.dscr = wb2.dscr + string(oe-ordl.ord-no )        .
    END.

       DISPLAY wb2.part-no  
             wb2.job-po
             /*wb2.i-no*/
             wb2.dscr    
             /*wb2.relqty*/
             wb2.cases
             wb2.cas-cnt
             /*v-tot-case-qty when first (wb2.cases) @ tt-boll.qty
             bf-ttboll.p-c  WHEN AVAIL bf-ttboll AND FIRST (wb2.cases) @ bf-ttboll.p-c */                        
           /* 1  WHEN i = 2 AND bf-ttboll.partial > 0  @ wb2.cases
            tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ wb2.cas-cnt */
           with frame bol-mid.
       DOWN WITH FRAME bol-mid.  
        
       
        v-case-tot  =  v-case-tot + wb2.cases .
       
       IF LAST(wb2.cases) THEN do:
           
        PUT SKIP  "====================" AT 68 SKIP
          v-case-tot   AT 65 " =" 
          v-tot-case-qty FORMAT ">>,>>>,>>>9" AT 77 SPACE(2) v-pc FORMAT "x(1)"
          v-wet-tot FORMAT ">>>>>9" 
            .
        ASSIGN
            v-case-tot = 0 
            v-wet-tot  = 0.
       END.
      
       
       v-printline = v-printline + 1.
    /*END.*/
    IF v-printline >= 36 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolmwbx2.i}
    END.
    v-tot-cases = v-tot-cases + wb2.cases.

    /*delete wb2. */
  end. /* each wb2 */

  put {1} skip(1).
  v-printline = v-printline + 1. 
  tt-boll.printed = yes.
  
 /* if v-print-components AND itemfg.alloc NE YES then
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

    IF v-printline >= 36 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolmwbx2.i}
    END.

    display {1}
            trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                    @ wb2.i-no
            b-itemfg.part-no                        @ wb2.dscr
            tt-boll.qty * v-part-qty                @ tt-boll.qty        
        with frame bol-mid.
    down {1} with frame bol-mid.
    v-printline = v-printline + 1.
    display {1}
            fg-set.part-no                          @ wb2.i-no
            v-job-po     @ wb2.job-po
            b-itemfg.i-name                         @ wb2.dscr
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    put {1} skip(1).
    v-printline = v-printline + 2.
  end.*/
END. /* first-of(tt-boll.line) */

END.  /* */
