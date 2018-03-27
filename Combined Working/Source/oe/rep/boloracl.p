/* ----------------------------------------------- oe/rep/bolpnp.p  11/98 FWK */
/* PRINT P&P BOL                                                              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.
def buffer xoe-bolh for oe-bolh.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char format "x(7)" no-undo.
def var v-tot-pkgs as int format ">>9" no-undo.
def var v-coname like cust.name no-undo.
def var v-coaddr like company.addr no-undo.
def var v-cocity like company.city no-undo.
def var v-costate like company.state no-undo.
def var v-cozip like company.zip no-undo.
def var v-printlines as int no-undo.
def var part-comp as char format "x" no-undo.
def var v-sum-qty like oe-boll.qty no-undo.
def var v-tot-wt like oe-bolh.tot-wt no-undo.
def var v-tot-cwt like oe-bolh.cwt no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

FORM
  oe-ordl.qty format ">>>>>>>"
  space(1) oe-boll.qty  format ">>>>>>>"
  space(0) oe-boll.cases format ">>>>"
  space(0) oe-boll.qty-case format ">>>>>>"
  space(2) oe-bolh.po-no space(0) "/" space(0) itemfg.i-no format "x(20)"
  space(2) oe-boll.ord-no
  space(2) oe-boll.p-c
  space(1) oe-boll.weight skip
  itemfg.i-name at 28 skip
  with frame ln-s down no-box no-labels stream-io width 90.

  
for each report where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    v-tot-wt = 0.
    for each xoe-bolh
        where xoe-bolh.company eq cocode
          and xoe-bolh.bol-no  eq oe-bolh.bol-no
        no-lock:
      v-tot-wt = v-tot-wt + xoe-bolh.tot-wt.  
    end.

    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-bolh.cust-no
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.
    v-carrier = if avail carrier then carrier.dscr else "".
      
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no no-lock,
        first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
        no-lock,
        
        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no
          AND oe-ordl.LINE    EQ oe-boll.LINE
        NO-LOCK:

      v-frt-pay-dscr = if oe-ord.frt-pay eq "p" or oe-ord.frt-pay eq "b"
                       then "Prepaid" else "Collect".

      LEAVE.
    end.
    
    v-time = string(time,"hh:mm am").

    assign
     v-tot-pkgs = 0
     v-tot-cwt  = 0.
    
    for each xoe-boll
        where xoe-boll.company eq cocode
          and xoe-boll.bol-no  eq oe-bolh.bol-no
        no-lock:
        
      assign
       v-tot-pkgs = v-tot-pkgs + xoe-boll.cases
       v-tot-cwt  = v-tot-cwt  + xoe-boll.qty.
       
      if xoe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.
    end.
    
    v-tot-cwt = v-tot-wt / (v-tot-cwt / 100).

    if first-of(oe-bolh.bol-no) then do:
      if oe-ctrl.pr-broker and avail cust and shipto.broker then
        v-coname = cust.name.
      else do:
        find first company where company.company eq cocode no-lock no-error.
        if avail company then
          assign
           v-coname    = company.name
           v-coaddr[1] = company.addr[1]
           v-coaddr[2] = company.addr[2]
           v-cocity    = company.city
           v-costate   = company.state
           v-cozip     = company.zip.
      end.

      if avail shipto then
        assign
         v-ship-name    = shipto.ship-name
         v-ship-addr[1] = shipto.ship-addr[1]
         v-ship-addr[2] = shipto.ship-addr[2]
         v-ship-city    = shipto.ship-city
         v-ship-state   = shipto.ship-state
         v-ship-zip     = shipto.ship-zip.
         
      else
        assign
         v-ship-name    = ""
         v-ship-addr[1] = ""
         v-ship-addr[2] = ""
         v-ship-city    = ""
         v-ship-state   = ""
         v-ship-zip     = "".

      if v-print-hdgs then do:
        format header
               skip(1)
               "Date:" oe-bolh.bol-date "/" v-time
               " Company:" v-coname format "x(25)"
               " BOL #:" oe-bolh.bol-no skip
               v-coaddr[1] at 37
               v-coaddr[2] at 37
               v-cocity at 37 "," v-costate v-cozip skip(2)
               "Carrier:" v-carrier "Trailer:" at 52
               oe-bolh.trailer
               skip(1)
               "SHIP TO:" at 12
               v-ship-name  at 12
               v-ship-addr[1] at 12
               v-ship-addr[2] at 12
               v-ship-city at 12
               v-ship-state
               v-ship-zip
               skip(2)
               v-frt-pay-dscr to 77 skip
               "Class: 55" to 77 skip
               "NMFC Item# 29280" to 77 skip
               " # Cases:" v-tot-pkgs
               " Weight:" v-tot-wt
               " Cwt:" v-tot-cwt
               " Frt Pay:" oe-ord.frt-pay
               "Folded Cartons" to 77 skip
               "made of paper" to 77 skip
               "Shipping Instructions:" at 16 skip
               oe-bolh.ship-i[1] at 16 format "x(60)" skip
               oe-bolh.ship-i[2] at 16 format "x(60)" skip
               oe-bolh.ship-i[3] at 16 format "x(60)" skip
               oe-bolh.ship-i[4] at 16 format "x(60)"
               skip(1)
               "Qty Ord Qty Ship Pt Qty/Pt      "
               "P.O. No. / Item / Description  "
               "Order# P/C  Wt." skip
               "------- -------- -- ------"
               "-------------------------------------"
               "------ --- ----" skip
               
            with frame hd-top-comp no-box no-labels page-top stream-io width 90.
            
        view frame hd-top-comp.
      end.

      else do:
        format header
               skip(3)
               v-time at 4
               oe-bolh.bol-date at 16
               v-coname at 29 format "x(20)"
               oe-bolh.bol-no at 71
               skip(4)
               v-carrier at 8
               oe-bolh.trailer at 69
               skip(1)
               v-ship-name  at 12
               v-ship-addr[1] at 12
               v-ship-addr[2] at 12
               v-ship-city at 12
               v-ship-state
               v-ship-zip
               skip(4)
               v-frt-pay-dscr to 77
               "Class: 55" to 77 skip
               "NMFC Item# 29280" to 77 skip
               v-tot-pkgs at 5
               v-tot-wt at 20
               v-tot-cwt at 35
               oe-ord.frt-pay at 50
               "Folded Cartons" to 77 skip
               oe-bolh.tot-pallets at 16
               " Total Pallets"
               "made of paper" to 77 skip
               skip(1)
               oe-bolh.ship-i[1] at 16 format "x(60)" skip
               oe-bolh.ship-i[2] at 16 format "x(60)" skip
               oe-bolh.ship-i[3] at 16 format "x(60)" skip
               oe-bolh.ship-i[4] at 16 format "x(60)"
               skip(7)
               
            with frame hd-top no-box no-labels page-top stream-io width 90.
            
        view frame hd-top.
      end.
    end.

    page.
  end. /* first-of(oe-bolh.bol-no) */

  if first-of(oe-bolh.bol-no) then v-printlines = 0.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
        
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
          and oe-ordl.i-no    eq oe-boll.i-no
          and oe-ordl.line    eq oe-boll.line
        no-lock no-error.

    if v-printlines ge 25 then do:
      put skip(27 - v-printlines).
    
      put "PALLETS____________________" to 28
          "WEIGHT_____________________" to 80 skip
          "SHIPPER____________________" to 28
          "CARRIER____________________" to 80 skip
          "PER________________________" to 28
          "PER________________________" to 80 skip
          "DATE_______________________" to 28
          "DATE_______________________" to 80 skip.
          
      page.
      
      view frame hd-top-comp.
      
      v-printlines = 0.
    end.

    if avail oe-ordl then
    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-ordl.ord-no
        no-lock no-error.
        
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq oe-boll.i-no
        no-lock no-error.
        
    if oe-boll.cases gt 0 then do:
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

      v-sum-qty =  0.
      for each xoe-boll
          where xoe-boll.company eq cocode
            and xoe-boll.bol-no eq oe-boll.bol-no
            and xoe-boll.ord-no eq oe-boll.ord-no
            and xoe-boll.i-no eq oe-boll.i-no
          no-lock:
          
        v-sum-qty = v-sum-qty + xoe-boll.qty.
      end.

      display oe-ordl.qty
              oe-boll.qty
              oe-boll.cases
              oe-boll.qty-case
              oe-bolh.po-no
              oe-rel.po-no
                when avail oe-rel and oe-rel.po-no ne "" @ oe-bolh.po-no
              itemfg.i-no
                when avail itemfg
              oe-boll.ord-no
              oe-boll.p-c
              oe-boll.weight
              itemfg.i-name when avail itemfg
              
          with frame ln-s.

      v-printlines = v-printlines + 2.
    end.
    
    /** PRINT PARTIAL SHIPMENT LINE **/
    if oe-boll.partial gt 0 then do:
      put skip.
      
      display "" @ oe-ordl.qty
              "" @ oe-boll.qty
              1 @ oe-boll.cases
              oe-boll.partial @ oe-boll.qty-case
              itemfg.i-no when avail itemfg
              oe-boll.p-c
              
          with frame ln-s.
          
      v-printlines = v-printlines + 2.
    end.
    
    if v-print-hdgs and avail itemfg then
      put space(30) itemfg.i-name skip
          space(30) itemfg.part-dscr1 skip.
          
    put skip(1).
    
    v-printlines = v-printlines + 3.
      
    oe-boll.printed = yes.
  end. /* for each oe-boll */

  if last-of(oe-bolh.bol-no) then do:
    put skip(27 - v-printlines).
    
    put "PALLETS____________________" to 28
        "WEIGHT_____________________" to 80 skip
        "SHIPPER____________________" to 28
        "CARRIER____________________" to 80 skip
        "PER________________________" to 28
        "PER________________________" to 80 skip
        "DATE_______________________" to 28
        "DATE_______________________" to 80 skip.
  end.
    
  oe-bolh.printed = yes.
end.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
