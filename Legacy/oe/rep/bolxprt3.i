/* ---------------------------------------------- oe/rep/bolxprt3.i           */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

v-tot-cases = 0.

FOR EACH tt-boll,
      
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq tt-boll.i-no
    no-lock

    BREAK BY ( IF v-sort THEN  tt-boll.i-no ELSE "")
          BY ( IF NOT v-sort THEN tt-boll.job-no + STRING(tt-boll.job-no2) ELSE "")
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:
IF ll-consol-bolls THEN DO:
     {oe/rep/bolxpr23.i}
END.
ELSE DO:
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

  IF v-printline >= 48 THEN DO:
      
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolxpr22.i}
  END.

  if tt-boll.qty-case ne 0 and tt-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.qty-case
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
  
  do i = v-lines + 1 to 4:
    assign
     v-part-dscr = ""
     v-job-po    = "".

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
    
    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    else
    if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
    
    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then v-lines = v-lines + 1.
  end.
  
  v-lines = v-lines + 1.
  
  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.

    assign
     v-part-dscr = ""
     v-job-po    = "".

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

    else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
    RUN  pGetP-C(OUTPUT cPc).
    DISPLAY trim(string(oe-ordl.qty,"->>,>>>,>>>")) when i eq 1
                                                    @ oe-ordl.i-no
            oe-ordl.i-no                            when i eq 2
            v-job-po
            v-part-dscr
            w2.cases
            w2.cas-cnt
            tt-boll.qty                             when last(w2.cases)
            /*tt-boll.p-c*/ cPc FORMAT "X(1)"       when last(w2.cases)         
        with frame bol-mid2.
    down  with frame bol-mid2.    
    v-printline = v-printline + 1.
    
    IF v-printline >= 48 THEN DO:
        
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolxpr22.i}
    END.
    v-tot-cases = v-tot-cases + w2.cases.

    delete w2.    
  end. /* each w2 */

  IF i < 4 THEN
  do i = i + 1 to 4:
    clear frame bol-mid2 no-pause.

    assign
     v-part-dscr = ""
     v-job-po    = "".

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    else
    if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
    
    IF i = 2 AND v-job-po = "" THEN
      v-job-po = if tt-boll.job-no eq "" then "" else
                (trim(tt-boll.job-no) + "-" + string(tt-boll.job-no2,"99"))                 .

    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then do:
        
      IF v-printline >= 48 THEN DO:
          
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolxpr22.i}
      END. 
      display {1}
              oe-ordl.i-no                            when i eq 2
              v-job-po
              v-part-dscr              
            with frame bol-mid2.
      down {1} with frame bol-mid2. 
      v-printline = v-printline + 1.
    end.
  end.
  
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

    IF v-printline >= 48 THEN DO:
        
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolxpr22.i}
    END.

    display {1}
            trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                    @ oe-ordl.i-no
            b-itemfg.part-no                        @ v-part-dscr
            tt-boll.qty * v-part-qty                @ tt-boll.qty        
        with frame bol-mid2.
    down {1} with frame bol-mid2.
    v-printline = v-printline + 1.
    display {1}
            fg-set.part-no                          @ oe-ordl.i-no
            v-job-po
            b-itemfg.i-name                         @ v-part-dscr
        with frame bol-mid2.
    down {1} with frame bol-mid2.
    
    put {1} skip(1).
    v-printline = v-printline + 2.
  end.
END. /* else */
end. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.


PROCEDURE pGetP-C:
  DEFINE OUTPUT parameter opcP-c AS CHARACTER .
  DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
  DEF VAR bolPartial-char AS CHAR NO-UNDO.
  DEF VAR v-sum-qty LIKE oe-boll.qty NO-UNDO.
  DEF BUFFER bf-oe-ordl FOR oe-ordl.
  DEF BUFFER tmp-oe-boll FOR oe-boll.
  DEF VAR v-p-c LIKE oe-boll.p-c NO-UNDO.

  RUN sys/ref/nk1look.p (INPUT cocode, "BOLPartial", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
 OUTPUT cRtnChar, OUTPUT lRecFound).

IF lRecFound THEN
    bolPartial-char = cRtnChar NO-ERROR. 


    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ tt-boll.company
          AND bf-oe-ordl.ord-no  EQ tt-boll.ord-no
          AND bf-oe-ordl.i-no    EQ tt-boll.i-no
        NO-ERROR.

  find first oe-rell no-lock
      where oe-rell.company eq tt-boll.company
        and oe-rell.ord-no  eq tt-boll.ord-no
        and oe-rell.i-no    eq tt-boll.i-no
        and oe-rell.line    eq tt-boll.line no-error.

  v-sum-qty = 0.
  FOR EACH tmp-oe-boll FIELDS(qty) NO-LOCK
      WHERE tmp-oe-boll.company EQ bf-oe-ordl.company
      AND tmp-oe-boll.ord-no  EQ bf-oe-ordl.ord-no
      AND tmp-oe-boll.i-no    EQ bf-oe-ordl.i-no 
      AND tmp-oe-boll.line    EQ bf-oe-ordl.line
      AND (tmp-oe-boll.rel-no LT tt-boll.rel-no      OR
           (tmp-oe-boll.rel-no EQ tt-boll.rel-no AND
            tmp-oe-boll.b-ord-no LE tt-boll.b-ord-no))
      AND ROWID(tmp-oe-boll)  NE ROWID(tt-boll)
      USE-INDEX ord-no:
      v-sum-qty = v-sum-qty + tmp-oe-boll.qty.
  END.

  IF bolPartial-char eq "Order Quantity" THEN DO:
      v-p-c = tt-boll.p-c.
  END.
  ELSE IF bolPartial-char eq "Release Quantity" and avail oe-rell THEN DO:
      v-p-c = tt-boll.qty + v-sum-qty GE
          (oe-rell.qty * (1 - (bf-oe-ordl.under-pct / 100))).
  END.

  opcP-c = IF v-p-c EQ YES THEN "C" ELSE "P".

END PROCEDURE.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */


/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
