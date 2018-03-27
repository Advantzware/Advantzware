
hide {1} frame bol-bot2.
view {1} frame bol-bot1.

v-tot-cases = 0.

for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no,
      
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
  
  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.

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
    
    display {1}
            trim(string(oe-ordl.qty,">>>,>>>,>>>")) when i eq 1
                                                    @ oe-ordl.i-no
            oe-ordl.i-no                            when i eq 2
            v-job-po
            v-part-dscr
            w2.cases
            w2.cas-cnt
            oe-boll.qty                             when last(w2.cases)
            oe-boll.p-c                             when last(w2.cases)
        with frame bol-mid.
    down {1} with frame bol-mid.

    v-tot-cases = v-tot-cases + w2.cases.

    delete w2.
  end.

  do i = i + 1 to 4:
    clear frame bol-mid no-pause.

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
    
    if v-part-dscr ne "" or v-job-po ne "" or i le 2 then do:
      display {1}
              oe-ordl.i-no                            when i eq 2
              v-job-po
              v-part-dscr

            with frame bol-mid.
      down {1} with frame bol-mid.
    end.
  end.

  put {1} skip(1).

  oe-boll.printed = yes.
  
  if itemfg.alloc NE YES then
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
            b-itemfg.part-no                        @ v-part-dscr
            oe-boll.qty * v-part-qty                @ oe-boll.qty
        with frame bol-mid.
    down {1} with frame bol-mid.
    
    display {1}
            fg-set.part-no                          @ oe-ordl.i-no
            v-job-po
            b-itemfg.i-name                         @ v-part-dscr
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

v-tot-wt = oe-bolh.tot-wt.

hide {1} frame bol-bot1.
view {1} frame bol-bot2.
