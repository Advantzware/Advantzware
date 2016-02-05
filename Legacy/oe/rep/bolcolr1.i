/* ---------------------------------------------- oe/rep/bolcolr1.i 07/09 GDM */
/* N-K BOLFMT = COLOR                                                         */
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
     {oe/rep/bolcolr2.i}
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

  find first oe-rel
      where oe-rel.company eq cocode
        and oe-rel.ord-no  eq oe-ordl.ord-no 
        and oe-rel.i-no    eq oe-ordl.i-no   
        and oe-rel.line    eq oe-ordl.line   
      no-lock no-error.

  IF v-printline >= 36 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolcolor.i}
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
     v-job-po    = ""
     v-vend-no   = ""
     v-relpc     = ""
     v-vfgord    = "".

    if i eq 1 then
      assign
       v-vfgord    = oe-ordl.i-no
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no
       v-relpc     = IF tt-boll.p-c THEN "     C" ELSE "     P".

    else
    if i eq 2 then
      assign
       v-vfgord    = STRING(oe-ordl.ord-no,">>>>>9")
       v-part-dscr = oe-ordl.i-name.       
                               
    else
    if i eq 3 then  assign v-vfgord    = oe-rel.carrier
                           v-part-dscr = oe-ordl.part-dscr1.

    else
    if i eq 4 then  assign v-part-dscr = oe-ordl.part-dscr2.
    
    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then v-lines = v-lines + 1.
  end.
  
  v-lines = v-lines + 1.
  
  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.

    assign
     v-part-dscr = ""
     v-job-po    = ""
     v-vend-no   = ""
     v-relpc     = ""
     v-vfgord    = "".   
     

    if i eq 1 then
      assign
       v-vfgord    = oe-ordl.i-no
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no
       v-relpc     = IF LAST(w2.cases)
                       THEN IF tt-boll.p-c THEN "     C" ELSE "     P"
                       ELSE "".

    else
    if i eq 2 then
      assign
       v-vfgord    = STRING(oe-ordl.ord-no,">>>>>9")
       v-part-dscr = oe-ordl.i-name.

    else if i eq 3 then assign v-vfgord    = oe-rel.carrier
                               v-part-dscr = oe-ordl.part-dscr1.

    else if i eq 4 then  assign v-part-dscr = oe-ordl.part-dscr2.
    
    DISPLAY v-vfgord     @ oe-ordl.i-no
            v-job-po
            v-part-dscr
            w2.cases
            w2.cas-cnt
            tt-boll.qty   WHEN LAST(w2.cases)
            v-relpc       @ tt-boll.p-c            
        with frame bol-mid2.
        down  with frame bol-mid2.

    v-printline = v-printline + 1.

    IF v-printline >= 36 THEN DO:
       v-printline = 0.       
       PAGE {1}.
       {oe/rep/bolcolor.i}
    END.
    v-tot-cases = v-tot-cases + w2.cases.

    delete w2.    
  end. /* each w2 */

  IF i < 4 THEN
  do i = i + 1 to 4:
    clear frame bol-mid2 no-pause.

    assign
     v-part-dscr = ""
     v-job-po    = ""
     v-vfgord    = ""
     v-relpc     = "" .

    if i eq 1 then
      assign
       v-vfgord    = oe-ordl.i-no
       v-part-dscr = oe-ordl.part-no
       v-job-po    = tt-boll.po-no
       v-relpc     = IF tt-boll.p-c THEN "     C" ELSE "     P".

    else
    if i eq 2 then
      assign
       v-vfgord    = STRING(oe-ordl.ord-no,">>>>>9")
       v-part-dscr = oe-ordl.i-name.

    else if i eq 3 then assign v-vfgord    = oe-rel.carrier
                               v-part-dscr = oe-ordl.part-dscr1.

    else if i eq 4 then  assign v-part-dscr = oe-ordl.part-dscr2.    

    if v-vfgord NE "" OR v-part-dscr ne "" or v-job-po ne "" or i le 2 then do:

      IF v-printline >= 36 THEN DO:
        v-printline = 0.        
        PAGE {1}.
        {oe/rep/bolcolor.i}
      END.

      display {1}
              v-vfgord         @ oe-ordl.i-no
              v-job-po
              v-part-dscr              
              v-relpc          when i eq 2 @ tt-boll.p-c
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

    IF v-printline >= 36 THEN DO:
        v-printline = 0.        
        PAGE {1}.
        {oe/rep/bolcolor.i}
    END.

    display {1}
            
            b-itemfg.part-no          @ v-part-dscr
            tt-boll.qty * v-part-qty  @ tt-boll.qty        
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
