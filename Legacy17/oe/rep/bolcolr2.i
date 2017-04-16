/* ---------------------------------------------- oe/rep/bolcolr2.i 07/09 GDM */
/* N-K BOLFMT = COLOR                                                         */
/* -------------------------------------------------------------------------- */
IF FIRST-OF(tt-boll.LINE) THEN DO:
  FOR EACH w2.
      DELETE w2.
  END.
  ASSIGN
     i = 0
     v-tot-case-qty = 0.

  FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                       AND bf-ttboll.po-no = tt-boll.po-no
                       AND bf-ttboll.ord-no = tt-boll.ord-no
                       AND bf-ttboll.LINE = tt-boll.LINE
                BREAK BY bf-ttboll.cases DESC.
      find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.

      find first oe-ord where oe-ord.company eq cocode
         and oe-ord.ord-no  eq tt-boll.ord-no no-lock no-error.

      find first oe-rel where oe-rel.company eq cocode
         and oe-rel.ord-no  eq oe-ordl.ord-no 
         and oe-rel.i-no    eq oe-ordl.i-no   
         and oe-rel.line    eq oe-ordl.line no-lock no-error.

      ASSIGN
         v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty.
                          
         i = i + 1.   
      
      find first w2 where w2.cas-cnt eq bf-ttboll.qty-case no-error.
      if not avail w2 then DO: 
        create w2.

        ASSIGN w2.job-po  = ""
               w2.i-no    = ""
               w2.cas-cnt = bf-ttboll.qty-case
               w2.cases   = w2.cases + bf-ttboll.cases
               w2.rec-id  = RECID(bf-ttboll)
               w2.rel#    = STRING(oe-bolh.release#,"->>>>>>9")
               w2.pc      = IF bf-ttboll.p-c 
                              THEN "     C" ELSE "     P".

         IF i = 1 THEN ASSIGN w2.i-no = oe-ordl.i-no
                           w2.job-po = bf-ttboll.po-no
                           w2.dscr   = oe-ordl.part-no
                           w2.qty    = oe-ordl.qty.
         ELSE IF i = 2 THEN 
            ASSIGN w2.dscr = oe-ordl.i-name
                   w2.i-no = STRING(oe-ordl.ord-no,">>>>>9").
         else if i eq 3 then ASSIGN w2.i-no = oe-rel.carrier
                                 w2.dscr = oe-ordl.part-dscr1.
         else if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
      END.      
  END.

  IF i < 4 THEN DO i = i TO 4:
      CREATE w2.
      ASSIGN w2.rel#    = STRING(oe-bolh.release#,"->>>>>>9").
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

       IF i = 2 THEN 
          ASSIGN w2.dscr = oe-ordl.i-name
                 w2.i-no = STRING(oe-ordl.ord-no,">>>>>9").

       else if i eq 3 then ASSIGN w2.i-no = oe-rel.carrier
                                  w2.dscr = oe-ordl.part-dscr1.
       else if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.

    END.

/*     IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND w2.cas-cnt = 0 */

/*         THEN DELETE w2.                                                */

  END.

  i = 0.

  FOR EACH w2  BREAK BY w2.cases DESC:

    FIND FIRST bf-ttboll WHERE recid(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.

    i = i + 1.

    ASSIGN v-relpc = "".

    IF w2.pc   NE "" THEN ASSIGN v-pc   = w2.pc.
    IF w2.rel# NE "" THEN ASSIGN v-rel# = w2.rel#.

    IF i EQ 1
      THEN ASSIGN v-relpc = v-pc.

    IF w2.rec-id = ? THEN DO:
        find first oe-ordl where oe-ordl.company eq cocode
         and oe-ordl.ord-no  eq tt-boll.ord-no
         and oe-ordl.i-no    eq tt-boll.i-no
         and oe-ordl.line    eq tt-boll.LINE no-lock no-error.
       w2.i-no = "".

       IF i = 2 THEN 
          ASSIGN 
                 w2.dscr = oe-ordl.i-name
                 w2.i-no = STRING(oe-ordl.ord-no,">>>>>9").
       else if i eq 3 then ASSIGN w2.i-no = oe-rel.carrier
                                  w2.dscr = oe-ordl.part-dscr1.
       else if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.

    END.

/*     IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND NOT last(w2.cases) THEN. */
/*                                                                                  */
/*     ELSE DO:                                                                     */
        /*MESSAGE w2.i-no "," w2.qty "," w2.dscr VIEW-AS ALERT-BOX.*/

       DISPLAY w2.i-no                       
               w2.job-po
               w2.dscr
               w2.cases
               w2.cas-cnt
               v-tot-case-qty when first (w2.cases) @ tt-boll.qty
               v-relpc        @ bf-ttboll.p-c
       with frame bol-mid.
       DOWN WITH FRAME bol-mid.       
       v-printline = v-printline + 1.
/*     END. */

    IF v-printline >= 36 THEN DO:
       v-printline = 0.       
       PAGE {1}.
       {oe/rep/bolcolor.i}
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

    IF v-printline >= 36 THEN DO:
        v-printline = 0.        
        PAGE {1}.
        {oe/rep/bolcolor.i}
    END.

    display {1}
            b-itemfg.part-no                        @ w2.dscr
            tt-boll.qty * v-part-qty                @ tt-boll.qty        
        with frame bol-mid.
    down {1} with frame bol-mid.
    v-printline = v-printline + 1.
    display {1}
            fg-set.part-no                          @ w2.i-no
            v-job-po                                @ w2.job-po
            b-itemfg.i-name                         @ w2.dscr
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    put {1} skip(1).
    v-printline = v-printline + 2.
  end.
END. /* first-of(tt-boll.line) */

