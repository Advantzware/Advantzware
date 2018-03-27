/* ---------------------------------------------- oe/rep/bolfibre.i 10/00 JLF */
/* Print Fibre Container BOL                                                  */
/* -------------------------------------------------------------------------- */

hide {1} frame bol-bot2.
view {1} frame bol-bot1.

for each oe-boll where
    oe-boll.company eq oe-bolh.company and
    oe-boll.b-no eq oe-bolh.b-no,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock:

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq oe-boll.ord-no
        and oe-ordl.i-no    eq oe-boll.i-no
        and oe-ordl.line    eq oe-boll.line
      no-lock no-error.

  find first oe-ord
      where oe-ord.company eq cocode
        and oe-ord.ord-no  eq oe-boll.ord-no
      no-lock no-error.

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

  IF oe-boll.po-no NE "" THEN DO:
    FIND FIRST w2 WHERE w2.job-po EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    w2.job-po = oe-boll.po-no.
  END.

  v-job-po = "".

  IF oe-ordl.job-no NE "" THEN DO:
    FIND FIRST w2 WHERE w2.job-po EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    ASSIGN
     w2.job-po = TRIM(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99")
     v-job-po  = w2.job-po.
  END.

  IF oe-ordl.part-no NE "" THEN DO:
    FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    w2.part-dscr = oe-ordl.part-no.
  END.

  IF oe-ordl.i-name NE "" THEN DO:
    FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    w2.part-dscr = oe-ordl.i-name.
  END.

  IF oe-ordl.part-dscr1 NE "" THEN DO:
    FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    w2.part-dscr = oe-ordl.part-dscr1.
  END.

  IF oe-ordl.part-dscr2 NE "" THEN DO:
    FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
    w2.part-dscr = oe-ordl.part-dscr2.
  END.

/*   FIND FIRST reftable NO-LOCK                           */
/*       WHERE reftable.reftable EQ "oe-boll.lot-no"       */
/*         AND reftable.rec_key  EQ STRING(RECID(oe-boll)) */
/*         USE-INDEX rec_key                               */
/*       NO-ERROR.                                         */
/*   IF AVAIL reftable AND reftable.code NE "" THEN DO:    */
  IF oe-boll.lot-no NE "" THEN DO:
    FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
    IF NOT AVAIL w2 THEN CREATE w2.
        w2.part-dscr = oe-boll.lot-no.
/*     w2.part-dscr = reftable.code. */
  END.

  IF oe-boll.job-no NE "" THEN DO:
    RELEASE reftable.
    FIND FIRST job NO-LOCK
        WHERE job.company EQ oe-boll.company
          AND job.job-no  EQ oe-boll.job-no
          AND job.job-no2 EQ oe-boll.job-no2
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job.job,"999999999")
          AND reftable.code2    EQ oe-boll.i-no
        USE-INDEX reftable NO-ERROR.
    IF NOT AVAIL reftable THEN
    FIND FIRST job-hdr NO-LOCK
        WHERE job-hdr.company EQ oe-boll.company
          AND job-hdr.job-no  EQ oe-boll.job-no
          AND job-hdr.job-no2 EQ oe-boll.job-no2
          AND job-hdr.i-no    EQ oe-boll.i-no
        NO-ERROR.

    IF AVAIL reftable OR AVAIL job-hdr THEN
    FOR EACH rm-rcpth NO-LOCK
        WHERE rm-rcpth.company   EQ oe-boll.company
          AND rm-rcpth.job-no    EQ oe-boll.job-no
          AND rm-rcpth.job-no2   EQ oe-boll.job-no2
          AND rm-rcpth.rita-code EQ "I"
        USE-INDEX job,
        EACH rm-rdtlh NO-LOCK
        WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
          AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
          AND rm-rdtlh.s-num     EQ (IF AVAIL reftable THEN reftable.val[12]
                                                       ELSE job-hdr.frm)
          AND rm-rdtlh.tag       NE "",
        EACH b-rd NO-LOCK
        WHERE b-rd.company   EQ rm-rdtlh.company
          AND b-rd.tag       EQ rm-rdtlh.tag
          AND b-rd.loc       EQ rm-rdtlh.loc
          AND b-rd.loc-bin   EQ rm-rdtlh.loc-bin
          AND b-rd.rita-code EQ "R"
          AND b-rd.tag2      NE ""
        USE-INDEX tag,
        FIRST b-rh NO-LOCK
        WHERE b-rh.r-no      EQ b-rd.r-no
          AND b-rh.rita-code EQ b-rd.rita-code
          AND b-rh.i-no      EQ rm-rcpth.i-no:
      IF NOT CAN-FIND(FIRST w2 WHERE w2.part-dscr EQ rm-rdtlh.tag2) THEN DO:
        FIND FIRST w2 WHERE w2.part-dscr EQ "" NO-ERROR.
        IF NOT AVAIL w2 THEN CREATE w2.
        w2.part-dscr = b-rd.tag2.
      END.
    END.
  END.

  v-lines = 0.
  for each w2 break by w2.cases:
    v-lines = v-lines + 1.
  end.

  DO i = v-lines + 1 TO 2:
    CREATE w2.
    v-lines = v-lines + 1.
  END.
  
  v-lines = v-lines + 1.
  
  IF LINE-COUNTER {2} - 1 + v-lines GT PAGE-SIZE {2} + 1 THEN PAGE {1}.

  i = 0.
  FOR EACH w2:
    i = i + 1.

    DISPLAY {1}
            TRIM(STRING(oe-ordl.qty,">>>,>>>,>>>")) WHEN i EQ 1
                                                    @ oe-ordl.i-no
            oe-ordl.i-no                            WHEN i EQ 2
            w2.job-po                                
            w2.part-dscr
            w2.cases                                WHEN w2.cases NE 0
            w2.cas-cnt                              WHEN w2.cases NE 0
            oe-boll.qty                             WHEN i EQ 1
            oe-boll.p-c                             WHEN i EQ 1
        WITH FRAME bol-mid.
    DOWN {1} WITH FRAME bol-mid.

    DELETE w2.
  END.

  PUT {1} SKIP(1).

  oe-boll.printed = YES.
  
  if v-print-components then
  for each fg-set
      where fg-set.company eq cocode
        and fg-set.set-no  eq oe-boll.i-no
      no-lock,
      
      first b-itemfg
      where b-itemfg.company eq cocode
        and b-itemfg.i-no    eq fg-set.part-no
      no-lock
      
      break by fg-set.set-no:
      
    {sys/inc/part-qty.i v-part-qty fg-set}
      
    display {1}
            trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                    @ oe-ordl.i-no
            b-itemfg.part-no                        @ w2.part-dscr
            oe-boll.qty * v-part-qty                @ oe-boll.qty
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    display {1}
            fg-set.part-no                          @ oe-ordl.i-no
            v-job-po                                @ w2.job-po
            b-itemfg.i-name                         @ w2.part-dscr
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    put {1} skip(1).
  end.
end. /* for each oe-boll */

v-lines = 0.
do i = 1 to 4:
  if oe-bolh.ship-i[i] ne "" then v-lines = v-lines + 1.
end.
  
if v-lines gt 0 then do:
  v-lines = v-lines + 1.
  
  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.
  
  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
  end.
  
  put {1} skip(1).
end.  

assign
 v-tot-pal = oe-bolh.tot-pallets
 v-tot-wt  = oe-bolh.tot-wt.

hide {1} frame bol-bot1.
view {1} frame bol-bot2.

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
