/* ---------------------------------------------- oe/rep/bolbrick.i 01/00 FWK */
/* PRINT Brick BOL                                                            */
/* -------------------------------------------------------------------------- */

hide {1} frame bol-bot2.
view {1} frame bol-bot1.

assign
 v-tot-wt = 0
 v-tot-sf = 0.

for each report where report.term-id eq v-term,

    first oe-boll where recid(oe-boll) eq report.rec-id,

    first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,

    first itemfg
    where itemfg.company eq oe-boll.company
      and itemfg.i-no    eq oe-boll.i-no
    no-lock

    break by report.key-01
          by report.key-02:

  v-tot-pkgs = v-tot-pkgs + oe-boll.cases +
               if oe-boll.partial gt 0 then 1 else 0.
               
  find first fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq oe-boll.i-no
        and fg-bin.loc     eq oe-boll.loc
        and fg-bin.loc-bin eq oe-boll.loc-bin
        and fg-bin.tag     eq oe-boll.tag
      no-lock no-error.

  if not avail fg-bin or fg-bin.unit-count ne 1 then
    assign
     v-pall    = oe-boll.qty-case
     v-pallets = oe-boll.cases.
  
  else do:
    assign
     v-pall    = if avail fg-bin then
                   (if fg-bin.cases-unit ne 0 then
                      fg-bin.cases-unit else 1) *
                   (if fg-bin.units-pallet ne 0 then
                      fg-bin.units-pallet else 1)
                 else 1
     v-pallets = trunc((oe-boll.cases + int(oe-boll.partial gt 0)) / v-pall,0)
     v-pall    = v-pall * oe-boll.qty-case.
             
    if v-pallets * v-pall gt oe-boll.qty then
      v-pallets = trunc(oe-boll.qty / v-pall,0).
  end.

  if v-pall ne 0 and v-pallets ne 0 then do:
    find first w2 where w2.cas-cnt eq v-pall no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = v-pall
     w2.cases   = w2.cases + v-pallets.
  end.

  if oe-boll.qty gt v-pallets * v-pall then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty - (v-pallets * v-pall)
        no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty - (v-pallets * v-pall)
     w2.cases   = w2.cases + 1.
  end.

  release oe-rel.
  find first oe-rell
      where oe-rell.company eq cocode
        and oe-rell.r-no    eq oe-boll.r-no
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        and oe-rell.i-no    eq oe-boll.i-no
        and oe-rell.line    eq oe-boll.line
       no-lock no-error.
  if avail oe-rell then do:
    find first oe-relh of oe-rell no-lock.
    find first oe-rel
        where oe-rel.company eq cocode
          and oe-rel.ord-no  eq oe-rell.ord-no
          and oe-rel.line    eq oe-rell.line
          and oe-rel.link-no eq oe-rell.r-no
          and oe-rel.ship-no eq oe-relh.ship-no
          and oe-rel.i-no    eq oe-rell.i-no
        no-lock no-error.
    if not avail oe-rel then
      find first oe-rel
          where oe-rel.company  eq cocode
            and oe-rel.ord-no   eq oe-rell.ord-no
            and oe-rel.line     eq oe-rell.line
            and oe-rel.rel-date eq oe-relh.rel-date
            and oe-rel.ship-no  eq oe-relh.ship-no
            and oe-rel.i-no     eq oe-rell.i-no
          no-lock no-error.
  end.

  if last-of(report.key-02) then do:
    assign
     i = 0
     j = 0
     
     v-ord-qty = 0
     v-bol-qty = 0
     v-bol-wt  = 0
     
     v-part-dscr[1] = itemfg.part-dscr1
     v-part-dscr[2] = itemfg.part-dscr2.
           
    for each oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq int(report.key-02)
          and oe-ordl.i-no    eq report.key-01
        no-lock:

      assign
       v-ord-qty      = v-ord-qty  + oe-ordl.qty
       v-ship-qty     = v-ship-qty + oe-ordl.ship-qty
       v-part-dscr[1] = oe-ordl.part-dscr1
       v-part-dscr[2] = oe-ordl.part-dscr2.
    end.
            
    for each xreport
        where xreport.term-id eq v-term
          and xreport.key-01  eq report.key-01
          and xreport.key-02  eq report.key-02
        no-lock,

        first xoe-boll where recid(xoe-boll) eq xreport.rec-id no-lock:

      assign
       v-bol-qty = v-bol-qty + xoe-boll.qty
       v-bol-wt  = v-bol-wt  + xoe-boll.weight.

      if xoe-boll.weight eq 0 then
        v-bol-wt = v-bol-wt + (xoe-boll.qty / 100 * itemfg.weight-100).
    end.

    display {1}
            v-ord-qty
            v-bol-qty
            v-part-dscr[1]
            v-bol-qty when not oe-boll.p-c @ v-part
            v-bol-qty when oe-boll.p-c     @ v-comp
            
        with frame bol-mid1.
          
    down with frame bol-mid1.
    
    if v-part-dscr[2] ne "" then do:
      display {1}
              v-part-dscr[2] @ v-part-dscr[1]
            
          with frame bol-mid1.
          
      down with frame bol-mid1.
    end.

    assign
     v-tot-pkgs  = 0
     v-part-dscr = "".

    for each w2 break by w2.cases desc by w2.cas-cnt desc:
      v-part-dscr[2] = trim(string(w2.cases,">>>>>")) + "@" +
                       trim(string(w2.cas-cnt,">>>>>")).
    
      do while true:
        if length(trim(v-part-dscr[1])) +
           length(trim(v-part-dscr[2])) + 2 gt 30 then do:
          display {1} v-part-dscr[1] with frame bol-mid1.
          
          down with frame bol-mid1.
        
          v-part-dscr[1] = "".
        end.
      
        v-part-dscr[1] = v-part-dscr[1] + 
                         (if v-part-dscr[1] eq "" then "" else ", ") +
                         v-part-dscr[2].
      
        if last(w2.cases) then do:
          if not v-part-dscr[2] begins "Total " then do:
            v-part-dscr[2] = "Total " + trim(string(v-bol-qty,">,>>>,>>>")).

            next.
          end.
        
          if v-part-dscr[1] ne "" then do:
            display {1} v-part-dscr[1] with frame bol-mid1.
          
            down with frame bol-mid1.
          end.
        end.
        
        leave.
      end.
    
      delete w2.
    end.
    
    put {1} skip(1).
  end.

  assign
   v-tot-wt = v-tot-wt + oe-boll.weight
   v-tot-sf = v-tot-sf + (oe-boll.qty * itemfg.t-sqft).

  if oe-boll.weight eq 0 then
    v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).
    
  if last(report.key-01) then put {1} skip(1).  
end. /* for each report */

for each w3:
  put {1} w3.ship-i at 11 skip.
end.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */

