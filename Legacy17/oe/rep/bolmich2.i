/* ---------------------------------------------- oe/rep/bolmich2.i 05.18.05 */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

v-tot-cases = 0.
/*for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no,*/
FOR EACH tt-boll,      
    first itemfg NO-LOCK
    where itemfg.company eq cocode
      and itemfg.i-no    eq tt-boll.i-no
    BREAK BY tt-boll.i-no
          BY tt-boll.po-no
          BY tt-boll.ord-no
          BY tt-boll.line
          BY tt-boll.cases DESC:

IF ll-consol-bolls THEN DO:

IF FIRST-OF(tt-boll.LINE) THEN DO:
  FOR EACH w2.
      DELETE w2.
  END.
  i = 0.
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
      v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty.
      find first oe-ord where oe-ord.company eq cocode
         and oe-ord.ord-no  eq tt-boll.ord-no no-lock no-error.                  
      i = i + 1.
      find first w2 where w2.cas-cnt eq bf-ttboll.qty-case no-error.
      if not avail w2 then create w2.
      ASSIGN w2.job-po = ""
             w2.i-no = "".
      ASSIGN w2.cas-cnt = bf-ttboll.qty-case
             w2.cases   = w2.cases + bf-ttboll.cases
             w2.rec-id = RECID(bf-ttboll)
             .
      IF i = 1 THEN ASSIGN w2.job-po = bf-ttboll.po-no
                           w2.dscr = oe-ordl.part-no
                           w2.qty = oe-ordl.qty.
      ELSE IF i = 2 THEN 
          ASSIGN w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                 w2.dscr = oe-ordl.i-name
                 w2.i-no = oe-ordl.i-no.
      else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
      ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.

  END.
  IF i < 4 THEN DO i = i TO 4:
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
       IF i = 2 THEN 
          ASSIGN w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                 w2.dscr = oe-ordl.i-name
                 w2.i-no = oe-ordl.i-no.
       else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
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
       IF i = 2 THEN 
          ASSIGN w2.job-po = if oe-ordl.job-no eq "" then "" else
                             (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                 w2.dscr = oe-ordl.i-name
                 w2.i-no = oe-ordl.i-no.
       else if i eq 3 then ASSIGN w2.dscr = oe-ordl.part-dscr1.
       ELSE if i eq 4 then ASSIGN w2.dscr = oe-ordl.part-dscr2.
    END.
    IF w2.qty = 0 and w2.i-no = "" AND w2.dscr = "" AND NOT last(w2.cases) THEN .
    ELSE DO:    
        /*MESSAGE w2.i-no "," w2.qty "," w2.dscr VIEW-AS ALERT-BOX.*/
       DISPLAY w2.i-no                       
           trim(string(w2.qty,"->>,>>>,>>>")) WHEN i = 1 @ w2.i-no
            w2.job-po
            w2.dscr
            w2.cases
            w2.cas-cnt
            v-tot-case-qty when first (w2.cases) @ tt-boll.qty
            bf-ttboll.p-c  WHEN AVAIL bf-ttboll AND first(w2.cases) @ bf-ttboll.p-c                         
           /* 1  WHEN i = 2 AND bf-ttboll.partial > 0  @ w2.cases
            tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ w2.cas-cnt */
           with frame bol-mid.
       DOWN WITH FRAME bol-mid.       
       v-printline = v-printline + 1.
    END.
    IF v-printline >= 42 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolelite.i}
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

    IF v-printline >= 42 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolelite.i}
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
END. /* ll-consol-bol */
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

  IF v-printline >= 42 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolelite.i}
  END.

  if tt-boll.qty-case ne 0 and tt-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.qty-case
     w2.cases   = w2.cases + tt-boll.cases.
  end.
/*
  if tt-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq tt-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = tt-boll.partial
     w2.cases   = w2.cases + 1.
  end.
*/
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
  
/*  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.*/

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

    DISPLAY trim(string(oe-ordl.qty,"->>,>>>,>>>")) when i eq 1
                                                    @ oe-ordl.i-no
            oe-ordl.i-no                            when i eq 2
            v-job-po
            v-part-dscr
            w2.cases
            w2.cas-cnt
            tt-boll.qty when last(w2.cases) @ tt-boll.qty
            tt-boll.p-c                   when last(w2.cases)                
            1  WHEN i = 2 AND tt-boll.partial > 0  @ w2.cases
            tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ w2.cas-cnt
        with frame bol-mid2.
    down  with frame bol-mid2.
    v-printline = v-printline + 1.

    IF v-printline >= 42 THEN DO:
       v-printline = 0.
       PAGE {1}.
       {oe/rep/bolelite.i}
    END.
    v-tot-cases = v-tot-cases + w2.cases.

    delete w2.
  end. /* each w2 */

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
      display {1}
              oe-ordl.i-no                            when i eq 2
              v-job-po
              v-part-dscr              
              1  WHEN i = 2 AND tt-boll.partial > 0  @ w2.cases
              tt-boll.partial WHEN i = 2 AND tt-boll.partial > 0 @ w2.cas-cnt
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

    IF v-printline >= 42 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolelite.i}
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
END. /* non consol-bol*/
end. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
