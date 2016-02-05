/* ---------------------------------------------- oe/rep/bolxprt2.i 02/04 YSK */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

v-tot-cases = 0.

for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no,      
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    NO-LOCK BREAK BY oe-boll.i-no
          BY oe-boll.ord-no
          BY oe-boll.line
          BY oe-boll.po-no
          BY oe-boll.job-no
          BY oe-boll.job-no2:

  IF FIRST-OF(oe-boll.i-no) THEN DO:
     ASSIGN v-ship-qty = 0
            v-weight   = 0.
  END.

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

  IF v-printline >= 38 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolsth22.i}
  END.

  /*if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then*/ do:
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
       v-job-po    = oe-boll.po-no.

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
  
  ASSIGN v-ship-qty = v-ship-qty + oe-boll.qty
         v-weight   = v-weight + oe-boll.weight.

/*  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.*/

  IF lv-bolfmt-int = 1 THEN DO:  /* show summary per item and order number*/
     IF LAST-OF(oe-boll.ord-no) THEN DO:
         i = 0.
         FOR EACH w2 BREAK BY w2.cases * w2.cas-cnt DESC:             
             i = i + 1.
             IF i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no
                            v-job-po    = oe-boll.po-no.
             ELSE if i eq 2 THEN 
                  ASSIGN v-part-dscr = /*oe-ordl.i-name*/ oe-ordl.part-dscr1
                         v-job-po    = if oe-ordl.job-no eq "" then "" else
                                (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
             else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
             ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.
             
             IF v-printline >= 38 THEN DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolsth22.i}
             END.
             IF FIRST(w2.cases * w2.cas-cnt) THEN 
                PUT {1} TRIM(string(oe-ordl.qty,"->>,>>>,>>>")) FORM "x(15)"
                    v-job-po  AT 17 FORM "x(15)" 
                    v-part-dscr AT 33 FORM "x(30)" 
                    w2.cases        TO 70 FORM "->>>9"
                    w2.cas-cnt      TO 77 FORM "->>>>>9"
                    v-ship-qty      TO 85 FORM "->>>>>9"
                    oe-boll.p-c     AT 88
                    v-weight        AT 91     
                    SKIP.
              ELSE PUT {1} oe-boll.i-no 
                       v-job-po FORM "x(15)" AT 17
                       v-part-dscr FORM "x(30)" AT 33 
                       w2.cases  TO 70 FORM "->>>9"
                       w2.cas-cnt TO 77 FORM "->>>>>9"   
                       SKIP.
              v-printline = v-printline + 1.
              IF (i > 1 OR (i = 1 AND last(w2.cases * w2.cas-cnt))) AND v-part-dscr <> "" THEN do:
                 IF  i = 1 AND last(w2.cases * w2.cas-cnt) THEN PUT {1} oe-boll.i-no oe-ordl.part-dscr1 FORM "x(30)" AT 33  SKIP.
                 ELSE PUT {1} v-part-dscr FORM "x(30)" AT 33  SKIP.
                 v-printline = v-printline + 1.
              END.
              v-tot-cases = v-tot-cases + w2.cases.
              DELETE w2.
         END.
         put {1} skip(1).
         v-printline = v-printline + 1.
     END.
  END.
  /* end of summary mods */
  ELSE DO:
    i = 0.
    for each w2 break by w2.cases:
      i = i + 1.

      ASSIGN v-part-dscr = ""
             v-job-po    = "".

      if i eq 1 THEN ASSIGN v-part-dscr = oe-ordl.part-no
                            v-job-po    = oe-boll.po-no.
      ELSE if i eq 2 THEN 
           ASSIGN v-part-dscr = /*oe-ordl.i-name*/ oe-ordl.part-dscr1
                  v-job-po    = if oe-ordl.job-no eq "" then "" else
                                (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
      else if i eq 3 then v-part-dscr = oe-ordl.part-dscr1.
      ELSE if i eq 4 then v-part-dscr = oe-ordl.part-dscr2.

      DISPLAY {1} trim(string(oe-ordl.qty,"->>,>>>,>>>")) when i eq 1
                                                    @ oe-ordl.i-no
            oe-ordl.i-no                            when i eq 2
            v-job-po
            v-part-dscr
            w2.cases
            w2.cas-cnt
            oe-boll.qty                             when last(w2.cases)
            oe-boll.p-c                             when last(w2.cases)        
            oe-boll.weight                          when last(w2.cases)             
        with frame bol-mid.
      down  with frame bol-mid.
    
      v-printline = v-printline + 1.    
      IF v-printline >= 38 THEN DO:
         v-printline = 0.
         PAGE {1}.
         {oe/rep/bolsth22.i}
      END.
      v-tot-cases = v-tot-cases + w2.cases.
      delete w2.
    end.

    do i = i + 1 to 4:
      clear frame bol-mid no-pause.
      ASSIGN v-part-dscr = ""
             v-job-po    = "".

      if i eq 1 then
         ASSIGN v-part-dscr = oe-ordl.part-no
                v-job-po    = oe-boll.po-no.
      ELSE if i eq 2 then
           ASSIGN v-part-dscr = oe-ordl.part-dscr1
                  v-job-po    = if oe-ordl.job-no eq "" then "" else
                    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
      ELSE if i eq 3 then v-part-dscr = "" /*oe-ordl.part-dscr1*/.
      ELSE if i eq 4 then v-part-dscr = "" /*oe-ordl.part-dscr2*/.
    
      IF i = 2 AND v-job-po = "" THEN
         v-job-po = if oe-boll.job-no eq "" then "" else
                   (trim(oe-boll.job-no) + "-" + string(oe-boll.job-no2,"99"))                 .

      if v-part-dscr ne "" or v-job-po ne "" or i le 2 then do:
         display {1}
              oe-ordl.i-no                            when i eq 2
              v-job-po
              v-part-dscr              
            with frame bol-mid.
         down {1} with frame bol-mid.
         v-printline = v-printline + 1.
      end.
    end.
    put {1} skip(1).
    v-printline = v-printline + 1.
  END.  /* else of lv-bol-fmt = 1 */
  
  oe-boll.printed = yes.
  
  if itemfg.alloc NE YES AND v-print-components then
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

    IF v-printline >= 38 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolsth22.i}
    END.

    display {1}
            trim(string(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                                                    @ oe-ordl.i-no
            b-itemfg.part-no                        @ v-part-dscr
            oe-boll.qty * v-part-qty                @ oe-boll.qty        
        with frame bol-mid.
    down {1} with frame bol-mid.
    v-printline = v-printline + 1.
    display {1}
            fg-set.part-no                          @ oe-ordl.i-no
            v-job-po
            b-itemfg.i-name                         @ v-part-dscr
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    put {1} skip(1).
    v-printline = v-printline + 2.
  end.

end. /* for each oe-boll */

v-lines = 0.
/*
do i = 1 to 4:
  if oe-bolh.ship-i[i] ne "" then v-lines = v-lines + 1.
end.
*/
if v-lines gt 0 then do:
   IF v-printline >= 38 THEN DO:
        v-printline = 0.
        PAGE {1}.
        {oe/rep/bolsth22.i}
   END.

  v-lines = v-lines + 1.
  
 /* if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}. */
  /*
  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
  end.
  */
  put {1} skip(1).
  v-printline = v-printline + 1.
end.  

v-tot-wt = oe-bolh.tot-wt.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
