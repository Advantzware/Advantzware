/* ---------------------------------------------- oe/rep/invcorrc.p 08/01 JLF */
/* PRINT INVOICE - O/E MODULE                                                 */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.
def var v-salesman as char format "x(14)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr-info as char.
def var v-cust like inv-head.addr extent 4.
def var v-ship like v-cust.
def var v-shipto like inv-head.sold-no.
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-line as int.
def var v-printline as int.
def var v-invhead as char format "x(13)" init
  "I N V O I C E".
def var v-pitch like printer.pitch.
def var v-len as int.
def var v-hldpitch like printer.pitch.
def var v-t-weight like inv-line.t-weight.
def var v-pall as int.
def var v-pkgs as int.
def var v-pallets as int.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date init today.
def new shared var v-fr-tax as log init no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as log init no.
def var v-date-ship as date format "99/99/99" init today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases like oe-boll.cases.
def var v-set-qty as int.
def var v-part-qty as dec format "999.9999".
def var v-net like inv-head.t-inv-rev.
def var v-case-cnt as char format "x(80)" extent 5.
def var v-case-line as char.
def var v-part-line as char.
def var tmp1 as dec.
def var tmp2 as date.
def var net1 as dec.
def var net2 as dec.
def var net3 as dec.
def var cnt as int.
def var disp-frt as char init "Freight:".
def var old-frt like inv-head.t-inv-freight no-undo.
def var v-ord-del-hdr  as char format "x(4)" init "Del#".
def var v-part-info    as char format "x(30)".
def var v-part-lines as int.
def var v              as int.
def var v-ord#         as int format "999999" no-undo.
def var v-remit-to     as char format "x(90)".

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.
def var v-c-phone        as   char format "x(30)". 
def var v-c-fax          as   char format "x(30)".
def var v-c-email        like cust.email.

def var end-page as int.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def workfile w-sman field sman as char format "x(4)".

def workfile wf like inv-line
    field cases like oe-boll.cases 
    field qty-case like oe-boll.qty-case
    field partial like oe-boll.partial 
    field v-case-line like v-case-line
    field v-part-line like v-part-line.

def buffer xwf for wf.

def workfile w-pall
    field w-pcnt as int
    field w-pqty as int.

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
     skip(7)
     v-invhead                  at 24
     v-inv-date                 at 72
  
     inv-head.inv-no            at 69
  
     skip(1)
  
     v-fob                      at 4
     inv-head.terms-d           at 32
  
     skip(2)
  
     inv-head.cust-no           at 11
     v-shipto                   at 51
  
     v-cust[1]                  at 11
     v-ship[1]                  at 51
  
     v-cust[2]                  at 11
     v-ship[2]                  at 51
  
     v-cust[3]                  at 11
     v-ship[3]                  at 51
  
     v-cust[4]                  at 11
     v-ship[4]                  at 51
  
     skip(5)
  
     v-date-ship
     v-shipvia                  at 11 format "x(25)"
     inv-head.t-inv-weight      to 44 format ">>>>9.99"
     v-del-no                   to 53
     inv-head.bol-no            to 65 format ">>>>>9"
     v-tot-pallets              at 69
  
     skip(2)
  
    with frame invhead page-top no-labels no-box no-underline stream-io width 90.

form header
     skip(7)
     v-invhead                  at 24
     "Date:"                    to 56
     v-inv-date
     
     "Inv #:"                   to 56
     inv-head.inv-no
     
     skip(1)
     
     "FOB:"
     v-fob
     "TERMS:"
     inv-head.terms-d
     
     skip(1)
     
     "SOLD TO:"                 at 11
     inv-head.cust-no
     "SHIP TO:"                 at 51
     v-shipto
     
     v-cust[1]                  at 11
     v-ship[1]                  at 51
     
     v-cust[2]                  at 11
     v-ship[2]                  at 51
     
     v-cust[3]                  at 11
     v-ship[3]                  at 51
     
     v-cust[4]                  at 11
     v-ship[4]                  at 51
     
     skip(3)
     
     "Ship Date"
     "Shipped Via"              at 11
     "Weight"                   to 44
     v-ord-del-hdr              to 51
     "BOL #"                    to 65
     "Pallets"                  at 72
     
     skip
     
     v-date-ship
     v-shipvia                  at 11 format "x(25)"
     inv-head.t-inv-weight      to 44 format ">>>>9.99"
     v-del-no                   to 53
     inv-head.bol-no            to 65 format ">>>>>9"
     v-tot-pallets              at 69
     
     skip(1)
     
     "Cust PO#/CC Job#"
     "Item/Description"         at 19
     "QInv/QShip"               to 52
     "Price"                    to 64
     "Amount"                   to 80                                   skip
     
     "----------------"
     "----------------"         at 19
     "----------"               to 52
     "-----"                    to 64
     "------"                   to 80                                   skip
     
    with frame invhead-comp page-top no-labels no-box no-underline stream-io width 90.

form wf.po-no
     wf.i-no                    at 19
     wf.inv-qty                 to 52 format "->,>>>,>>9"
     wf.price                   to 64 format "->>,>>9.99"
     space(0)
     wf.pr-uom                  to 68
     wf.t-price                 to 80 format "->>>,>>9.99"
     
     wf.job-no                  to 16 format "x(9)"
     wf.i-name                  at 19 format "x(23)"
     wf.ship-qty                to 52 format "->,>>>,>>9"
     
    with frame detail no-labels no-box no-underline down stream-io width 90.
                
form v-part-info at 19

    with frame part-info no-labels no-box no-underline down stream-io width 90.

form inv-misc.charge at  5 space(2) inv-misc.dscr format "x(30)"
     inv-misc.amt to 80 format "->>,>>9.99" skip(1)
    with frame detailm no-labels no-box no-underline down stream-io width 90.

form disp-frt at 1
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
    with frame totals no-labels no-box no-underline stream-io width 90.

form "Freight" to 20 "Tax" to 27
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
    with frame totals-comp no-labels no-box no-underline stream-io width 90.

form "Freight" to 20
    "Net Amount" TO 43 "Cash Disc" TO 56 "If Paid By" AT 59
    "Invoice Amt" TO 80
    inv-head.t-inv-freight format "->,>>9.99" at 11
    inv-head.t-inv-tax format "->,>>9.99" at 21
    v-net to 43 format "->,>>>,>>9.99"
    tmp1 to 56 format "->,>>9.99"
    tmp2 at 60
    space(0)
    inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
    with frame totals-comp2 no-labels no-box no-underline stream-io width 90.

form "Taxes- " at 1
   v-tax-code[1] at 9 format "x(4)"
   space(0) ":"
   net1 at 15 format "->>,>>9.99"
   v-tax-code[2] at 28 format "x(4)"
   space(0) ":"
   net2 at 34 format "->>,>>9.99"
    v-tax-code[3] at 46 format "x(4)"
    space(0) ":"
   net3 at 52 format "->>,>>9.99"
   "Tot Tax:" to 70
   space(0)
   inv-head.t-inv-tax  to 80 format "->>,>>9.99"
   with frame tax no-labels no-box no-underline stream-io width 90.

form " " to 80
     with frame blankl no-labels no-box no-underline stream-io width 90.


    find first company where company.company eq cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
    v-fr-tax = oe-ctrl.f-tax.

    end-page = 26.

    find first cust
        where cust.company eq cocode
          and cust.active  eq "X"
        no-lock no-error.
    if avail cust then do:
      if cust.area-code ne "" and cust.phone ne "" then
        v-c-phone = cust.area-code + '-' + 
                    substr(cust.phone,1,3) + '-' +
                    substr(cust.phone,4,4). 
      else
      if cust.area-code eq "" and cust.phone ne "" then
        v-c-phone = substr(cust.phone,1,3) +
                    substr(cust.phone,4,4). 

      if cust.fax ne "" then
        v-c-fax = substr(cust.fax,1,3) + '-' +
                  substr(cust.fax,4,3) + '-' +
                  substr(cust.fax,7,4).
                 
      v-c-email = cust.email.
     
      if v-c-phone ne "" then v-c-phone = "Phone: " + v-c-phone.
      if v-c-fax   ne "" then v-c-fax   = "  Fax: " + v-c-fax.
      if v-c-email ne "" then v-c-email = "EMail: " + v-c-email.
     
      find first shipto of cust no-lock no-error.
   
      if avail shipto then
        v-remit-to = shipto.ship-addr[1] + " " + shipto.ship-addr[2] + " " +
                     shipto.ship-city + ", " + shipto.ship-state + " " +
                     shipto.ship-zip.
    end.
   
    if oe-ctrl.prcom then
      assign
       v-c-name    = company.name
       v-c-addr[1] = company.addr[1]
       v-c-addr[2] = company.addr[2]
       v-c-city    = company.city
       v-c-state   = company.state
       v-c-zip     = company.zip
       v-c-phone   = ""
       v-c-fax     = ""
       v-c-email   = "".

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      assign
       v-shipto         = xinv-head.sold-no
       v-shipto-name    = xinv-head.sold-name
       v-shipto-addr[1] = xinv-head.sold-addr[1]
       v-shipto-addr[2] = xinv-head.sold-addr[2]
       v-shipto-city    = xinv-head.sold-city
       v-shipto-state   = xinv-head.sold-state
       v-shipto-zip     = xinv-head.sold-zip.

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company eq xinv-head.company and
          oe-bolh.bol-no eq xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company eq oe-bolh.company and
                   oe-relh.r-no eq oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  eq oe-bolh.company and
                   shipto.cust-no eq oe-relh.cust-no and
                   shipto.ship-id eq oe-bolh.ship-id no-lock no-error.
        if avail shipto then do:
          assign
           v-shipto         = shipto.ship-id
           v-shipto-name    = shipto.ship-name
           v-shipto-addr[1] = shipto.ship-addr[1]
           v-shipto-addr[2] = shipto.ship-addr[2]
           v-shipto-city    = shipto.ship-city
           v-shipto-state   = shipto.ship-state
           v-shipto-zip     = shipto.ship-zip.
        end.
        if avail shipto then release shipto. 
      end. /* avail oe-bolh */

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        if inv-head.fob-code begins "ORIG" then
         assign v-fob = "Origin".
        else
         assign v-fob = "Destination".

        find carrier where carrier.company eq inv-head.company and
          carrier.carrier eq inv-head.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
           
        assign
         j      = 0
         v-cust = "".
         
        do i = 1 to 4:
          v-addr-info = if i eq 1 then inv-head.cust-name else
                        if i eq 2 then inv-head.addr[1]   else
                        if i eq 3 then inv-head.addr[2]   else
                        (inv-head.city + ", " +
                         inv-head.state + "  " +
                         inv-head.zip).
          
          if trim(v-addr-info) eq "," then v-addr-info = "".
          
          if v-addr-info ne "" then
            assign
             j         = j + 1
             v-cust[j] = v-addr-info.
        end.
        
        assign
         j      = 0
         v-ship = "".
         
        do i = 1 to 4:
          v-addr-info = if i eq 1 then v-shipto-name    else
                        if i eq 2 then v-shipto-addr[1] else
                        if i eq 3 then v-shipto-addr[2] else
                        (v-shipto-city + ", " +
                         v-shipto-state + "  " +
                         v-shipto-zip).
                         
          if trim(v-addr-info) eq "," then v-addr-info = "".               
          
          if v-addr-info ne "" then
            assign
             j         = j + 1
             v-ship[j] = v-addr-info.
        end.
                
        assign                
         v-line      = 1
         v-printline = 0.
         
        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error.

        if avail stax then
          assign v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        assign v-tot-pallets = 0.
        for each xinv-line no-lock where xinv-line.r-no eq inv-head.r-no
          break by xinv-line.i-no:
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         for each oe-bolh no-lock where oe-bolh.b-no eq xinv-line.b-no and
             oe-bolh.ord-no eq xinv-line.ord-no:
           for each oe-boll no-lock where oe-boll.company eq oe-bolh.company and
              oe-boll.b-no eq oe-bolh.b-no and
              oe-boll.i-no eq xinv-line.i-no:

                                     /** Bill Of Lading TOTAL CASES **/
             assign v-bol-cases = v-bol-cases + oe-boll.cases.
              
             find first fg-bin
                 where fg-bin.company eq cocode
                   and fg-bin.i-no    eq oe-boll.i-no
                   and fg-bin.loc     eq oe-boll.loc
                   and fg-bin.loc-bin eq oe-boll.loc-bin
                   and fg-bin.tag     eq oe-boll.tag
                 no-lock no-error.
                 
             assign
              v-pall = if avail fg-bin then
                         (if fg-bin.cases-unit ne 0 then fg-bin.cases-unit
                          else 1) *
                         (if fg-bin.units-pallet ne 0 then
                            fg-bin.units-pallet else 1)
                       else 1
                       
              v-pkgs = oe-boll.cases + int(oe-boll.partial gt 0).
              
             if v-pkgs eq 0 then v-pkgs = 1.
              
             v-tot-pallets = v-tot-pallets + v-pkgs.
           end. /* each oe-boll */
           
           v-date-ship = oe-bolh.bol-date.
         end. /* each oe-bolh */
         
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then do:
             find first eb where eb.company eq xinv-line.company and
               eb.est-no eq xinv-line.est-no and
               eb.e-num eq xinv-line.e-num and
               eb.form-no eq xinv-line.form-no and
               eb.blank-no eq xinv-line.blank-no no-lock no-error.

             if xinv-line.form-no eq 0                               and
                (xinv-line.est-type eq 2 or xinv-line.est-type eq 6) then do:
               for each fg-set no-lock where fg-set.company eq xinv-line.company
                  and fg-set.set-no eq xinv-line.i-no:
                 assign v-set-qty = v-set-qty + fg-set.part-qty.
               end.
               if v-set-qty = 0 then
                  assign v-set-qty = 1.
               for each eb no-lock where eb.company eq xinv-line.company and
                  eb.est-no eq xinv-line.est-no and
                  eb.e-num eq xinv-line.e-num and
                  eb.form-no ne 0:
                 find fg-set where fg-set.company eq xinv-line.company and
                    fg-set.set-no eq xinv-line.i-no  and
                    fg-set.part-no eq eb.stock-no no-lock no-error.

                 if avail fg-set and fg-set.part-qty ne 0 then
                   assign v-part-qty = fg-set.part-qty / v-set-qty.
                 else
                   assign v-part-qty = 1 / v-set-qty.


                if eb.cas-cnt eq 0 then
                   assign v-tot-cas = round((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 else
                   assign v-tot-cas = round((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
               end. /* each eb */
             end. /* do */
             else
             if avail eb then
             do:
               if eb.cas-cnt eq 0 then
                 assign v-tot-cas = round(v-t-weight / eb.cas-wt, 2).
               else
                 assign v-tot-cas = round(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
             end. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each xinv-line */
                                         /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then
            assign v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.
        
        if first(report.key-01) then
          if v-print-head then view frame invhead-comp.  
          else view frame invhead.
          
        page.

        for each inv-line no-lock where inv-line.r-no eq inv-head.r-no:
          create wf.
          buffer-copy inv-line to wf.
        end.

        for each wf no-lock where wf.r-no eq inv-head.r-no:  
          assign
           v-case-line = ""
           v-part-line = ""
           v-case-cnt = "".
           
          for each w-pall:
            delete w-pall.
          end.

          for each oe-boll
              where oe-boll.company eq wf.company
                and oe-boll.bol-no eq inv-head.bol-no
                and oe-boll.i-no eq wf.i-no
                and oe-boll.ord-no eq wf.ord-no
              use-index bol-no:
              
            wf.qty-case = oe-boll.qty-case.
            
            find first fg-bin
                where fg-bin.company eq cocode
                  and fg-bin.i-no    eq oe-boll.i-no
                  and fg-bin.loc     eq oe-boll.loc
                  and fg-bin.loc-bin eq oe-boll.loc-bin
                  and fg-bin.tag     eq oe-boll.tag
                no-lock no-error.

            assign
             v-pall    = if avail fg-bin then
                           (if fg-bin.cases-unit ne 0 then fg-bin.cases-unit
                            else 1) *
                           (if fg-bin.units-pallet ne 0 then
                              fg-bin.units-pallet else 1)
                         else 1
             v-pkgs    = oe-boll.cases + int(oe-boll.partial gt 0)
             v-pallets = trunc(v-pkgs / v-pall,0)
             v-pall    = v-pall * oe-boll.qty-case.
             
            if v-pallets * v-pall gt oe-boll.qty then
              v-pallets = trunc(oe-boll.qty / v-pall,0).
               
            if v-pallets gt 0 then do:
              find first w-pall
                  where w-pcnt eq v-pall
                  no-error.
              if not avail w-pall then create w-pall.
              assign
               w-pcnt = v-pall
               w-pqty = w-pqty + v-pallets.
            end.
            
            if oe-boll.qty gt v-pallets * v-pall then do:
              find first w-pall
                  where w-pcnt eq oe-boll.qty - (v-pallets * v-pall)
                  no-error.
              if not avail w-pall then create w-pall.
              assign
               w-pcnt = oe-boll.qty - (v-pallets * v-pall)
               w-pqty = w-pqty + 1.
            end.
          end.
          
          for each w-pall where w-pqty ne 0 and w-pcnt ne 0:
            v-case-line = v-case-line +
                          trim(string(w-pqty,">>>>>>9")) + " @ " +
                          trim(string(w-pcnt,">>>>>>9")) + "  ".
          end.
          
          do i = 1 to 5:
            if (80 - length(v-case-cnt[i])) gt length(v-case-line) and
              v-case-line ne "" then
            assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                   v-case-line = "".
            if (80 - length(v-case-cnt[i])) gt length(v-part-line) and
              v-part-line ne "" then
            assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                   v-part-line = "".
          end. /* 1 to 5 */
      
          assign
           v-line = v-line + 1
           v-printline = v-printline + 2.

          v-part-lines = 0.
          do v = 1 to 3:
            v-part-info = if      v eq 1 then wf.part-no
                          else if v eq 2 then wf.part-dscr1
                          else                wf.part-dscr2.

            if v-part-info ne "" then
              v-part-lines = v-part-lines + 1.
          end.

          do i = 1 to 5:
            if v-case-cnt[i] ne "" then
              assign v-line = v-line + 1
                     v-part-lines = v-part-lines + 1.
          end.
        
          v-printline = v-printline + v-part-lines.
        
          if v-printline ge end-page then do:
            put skip(31 - v-printline) "* CONTINUED *" at 68.
            v-printline = 2 + v-part-lines.
            page.
          end.
          
          display wf.po-no
                  wf.i-no
                  wf.inv-qty
                  wf.price
                  wf.pr-uom
                  wf.t-price
                  wf.job-no
                  wf.job-no + "-" + string(wf.job-no2,"99")
                    when wf.job-no ne "" @ wf.job-no
                  wf.i-name
                  wf.ship-qty 
              with frame detail.

          down with frame detail. 

          do v = 1 to 3:
            v-part-info = if      v eq 1 then wf.part-no
                          else if v eq 2 then wf.part-dscr1
                          else                wf.part-dscr2.

            if v-part-info ne "" then do:
              display v-part-info with frame part-info.
              down with frame part-info.
            end.
          end.

          /** Display Case Count Lines **/
          do i = 1 to 5:
            if v-case-cnt[i] ne "" then do:
              put v-case-cnt[i] at 5 skip.
            end.
          end. /* 1 to 5 */

          put skip(1).
          
          v-printline = v-printline + 1.
        end. /* each wf */

        for each inv-misc of inv-head no-lock where
          inv-misc.bill eq "Y" break by ord-no:
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.
            if v-printline ge end-page then
            do:
              put skip(31 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.
            else
             down with frame detailm.
            /***
            if inv-misc.bill eq "N" then
            display
               inv-misc.charge inv-misc.dscr "     N/C" @ inv-misc.amt
            with frame detailm.
            else
            ***/
            display
                 inv-misc.charge inv-misc.dscr inv-misc.amt
            with frame detailm.

            assign v-line = v-line + 1
               v-printline = v-printline + 2.
        end. /* each inv-misc */

        if v-prntinst then do:
        /* do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
            if v-printline ge end-page then
            do:
              put skip(31 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.

            put inv-head.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */ */

       {custom/notesprt.i inv-head v-inst 4}
         DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:                
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.
        end.
        if v-printline ge end-page then
            do:
              put skip(31 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
            end.
        
        end.
         
        do:  /* T O T A L S */
        
          put skip(end-page - v-printline).
          
          old-frt = inv-head.t-inv-freight.

          if not inv-head.f-bill then inv-head.t-inv-freight = 0.

          assign
           tmp1  = 0
           tmp2  = ?
           v-net = inv-head.t-inv-rev -
                   inv-head.t-inv-tax -
                   inv-head.t-inv-freight.

          release terms.
          find first terms where terms.t-code eq inv-head.terms
              no-lock no-error.

          if avail terms then
            assign
             tmp1 = v-net * (round(terms.disc-rate / 100, 2))
             tmp2 = inv-head.inv-date + terms.disc-days.

          find first cust where cust.company eq cocode and
                     cust.cust-no eq inv-head.cust-no no-lock no-error.
          if avail cust and cust.sort eq "Y" then do:

            assign
             net1 = v-net * (v-tx-rate[1] / 100)
             net2 = v-net * (v-tx-rate[2] / 100)
             net3 = v-net * (v-tx-rate[3] / 100)

             net1 = net1 + (inv-head.t-inv-freight * (v-tx-rate[1] / 100))
             net2 = net2 + (inv-head.t-inv-freight * (v-tx-rate[2] / 100))
             net3 = net3 + (inv-head.t-inv-freight * (v-tx-rate[3] / 100))

             net1 = round(net1,2)
             net2 = round(net2,2)
             net3 = round(net3,2).

            if inv-head.t-inv-tax ne (net1 + net2 + net3) then
              if net3 gt 0 then
                net3 = net3 + (inv-head.t-inv-tax - (net1 + net2 + net3)).
              else
              if net2 gt 0 then
                net2 = net2 + (inv-head.t-inv-tax - (net1 + net2 + net3)).
              else
                net1 = net1 + (inv-head.t-inv-tax - (net1 + net2 + net3)).

            display
                v-tax-code[1]
                net1
                v-tax-code[2]
                net2
                v-tax-code[3]
                net3
                inv-head.t-inv-tax skip(1)
                with frame tax.
          end.

          else display " " skip(1) with frame blankl. 

          clear frame totals-comp  no-pause.
          clear frame totals-comp2 no-pause.
          clear frame totals      no-pause.

          if v-print-head /* Print invoice headers */ then
            if cust.sort eq "N" then do:
               display inv-head.t-inv-freight
                       inv-head.t-inv-tax
                       v-net
                       tmp1 when avail terms
                       tmp2 when avail terms
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms eq "CASH"
                         with frame totals-comp.
            end.
            else do:
               display inv-head.t-inv-freight
                       v-net
                       tmp1 when avail terms
                       tmp2 when avail terms
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms eq "CASH"
                         with frame totals-comp2.
            end.

          else
            if cust.sort eq "N" then do:
               display disp-frt
                       inv-head.t-inv-freight
                       inv-head.t-inv-tax
                       v-net
                       tmp1 when avail terms
                       tmp2 when avail terms
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms eq "CASH"
                          with frame totals.
            end.
            else do:
               display disp-frt  /*  DAR */
                       inv-head.t-inv-freight
                       v-net
                       tmp1 when avail terms
                       tmp2 when avail terms
                       v-net @ inv-head.t-inv-rev
                       inv-head.t-inv-rev when avail terms or
                                               inv-head.terms eq "CASH"
                          with frame totals.
            end.
        end.
        
        assign
         inv-head.printed = yes
         inv-head.stat    = "X"
         inv-head.t-inv-freight = old-frt.
      end. /* do transaction */
    end. /* each report, first xinv-head */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */     
