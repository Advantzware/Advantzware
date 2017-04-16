/* ---------------------------------------------- oe/rep/bolnstok3.i           */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

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
IF ll-consol-bolls THEN DO:
     {oe/rep/bolnstok23.i}
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
     
        FIND FIRST oe-relh WHERE oe-relh.company = cocode
                       AND oe-relh.r-no = oe-boll.r-no
                     NO-LOCK NO-ERROR.

       /*FOR EACH oe-rel WHERE oe-rel.company EQ oe-ordl.company AND 
           oe-rel.ord-no  EQ oe-ordl.ord-no AND 
           oe-rel.i-no    EQ oe-ordl.i-no   AND 
           oe-rel.line    EQ oe-ordl.line NO-LOCK:
           ASSIGN v-relqty = oe-rel.qty .
        END.*/
    FOR EACH oe-rell WHERE oe-rell.company  EQ tt-boll.company
                           AND oe-rell.r-no     EQ tt-boll.r-no
                           AND oe-rell.ord-no   EQ tt-boll.ord-no
                           AND oe-rell.i-no     EQ tt-boll.i-no
                           AND oe-rell.line     EQ tt-boll.line
                           AND oe-rell.rel-no   EQ tt-boll.rel-no
                           AND oe-rell.b-ord-no EQ tt-boll.b-ord-no
                           AND oe-rell.po-no    EQ tt-boll.po-no NO-LOCK :
      /*IF AVAIL oe-rell THEN*/
          ASSIGN v-relqty =  oe-rell.qty .
       END.
       
   

  IF v-printline >= 39 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolnstok22.i}
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
  IF NOT AVAIL w2 THEN DO:
    find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.qty-case
     w2.cases   = w2.cases + tt-boll.cases.
  END.
  v-lines = 0.
  for each bf-w2 break by bf-w2.cases:
    v-lines = v-lines + 1.
  end. 
  
  do i = v-lines + 1 to 5:
     
    assign
     v-part-dscr = ""
     v-job-po    = ""
     w2.bin = "" 
        .
 
    if i eq 1 THEN DO:
      assign
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no
       w2.bin = tt-boll.loc-bin
       w2.relqty = v-relqty.
     END.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.i-name.
       /*v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/
    
    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    else
    if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.

    else
    if i eq 5 then v-part-dscr = itemfg.part-dscr3.
    
    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then v-lines = v-lines + 1.
  end.
  
  
  v-lines = v-lines + 1.
  
  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.

    assign
     v-part-dscr = ""
     v-job-po    = ""
     w2.bin      = "".
    IF i EQ 1 THEN do:
         ASSIGN
             w2.relqty = v-relqty .
    END.

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no
       w2.bin = tt-boll.loc-bin.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po = string(tt-boll.ord-no)
        /*v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/.

    else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.

    ELSE if i eq 5 then v-part-dscr = itemfg.part-dscr3.

    IF v-printline >= 39 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolnstok22.i}
    END.

    DISPLAY v-job-po
            w2.bin /*trim(string(oe-ordl.qty,"->>,>>>,>>>")) when i eq 1
                                                    @ oe-ordl.i-no*/
            oe-ordl.i-no                            when i eq 1
            v-part-dscr
            w2.relqty
            w2.cases
            w2.cas-cnt
            tt-boll.qty                             when last(w2.cases)
            tt-boll.p-c                             when last(w2.cases) 

        with frame bol-mid2.
    down  with frame bol-mid2.
    
    v-printline = v-printline + 1.
    IF v-printline >= 39 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolnstok22.i}
    END.
    v-tot-cases = v-tot-cases + w2.cases.

    delete w2.    
  end. /* each w2 */

  IF i < 5 THEN
  do i = i + 1 to 5:
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
       v-job-po = STRING(tt-boll.ord-no)
       /*v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))*/.

    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.

    else
    if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.

    else
    if i eq 5 then v-part-dscr = itemfg.part-dscr3.
    
/*     IF i = 2 AND v-job-po = "" THEN                                            */
/*       /*v-job-po = if tt-boll.job-no eq "" then "" else                        */
/*                 (trim(tt-boll.job-no) + "-" + string(tt-boll.job-no2,"99"))*/. */

    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then do:
      IF v-printline >= 39 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolnstok22.i}
      END.
      display {1}
              oe-ordl.i-no                            when i eq 1
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

    IF v-printline >= 39 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolnstok22.i}
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

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
