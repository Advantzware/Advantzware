/* -------------------------------------------- oe/rep/invallpkg.p  03/00 EKW */
/* PRINT ALL PACKAGING INVOICE - O/E MODULE                                   */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

def var v-salesman as char format "x(14)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-shipto-no like inv-head.sold-no.
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-line as int.
def var v-printline as int format "999".
def var v-invhead as char format "x(13)" init
  "I N V O I C E".
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date init today.
def new shared var v-fr-tax as log init no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as log init no.
def var v-date-ship as date init today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases like oe-boll.cases.
def var v-set-qty as DECIMAL.
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
def var v-beeler-lines as int.
def var v              as int.
def var v-ord#         as int format ">>>>>>" no-undo.

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city         like company.city.
def var v-c-state        like company.state.
def var v-c-zip          like company.zip.

def var v-c-phone     as char format "x(12)".
def var v-c-fax          as char format "x(12)".
def var v-uline           as char format "x(80)". 
def var v-tot-merch     like inv-line.t-price.
def var v-tax-gr          like inv-head.tax-gr.
def var v-fin-chrg      as dec format "->,>>9.99".
 
def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def workfile w-sman field sman as char format "x(4)".

assign v-uline = fill("-",80).

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
  "I N V O I C E"        at 12  skip(1)
  v-c-name                at 12 
  "Invoice #"             at 47       
  inv-head.inv-no       at 57
  v-c-addr[1]             at 12 skip
  v-c-addr[2]             at 12
  "Date:"                  at 47 
  v-inv-date               at 57 skip
  v-c-city                   at 12 ", "
  v-c-state
  v-c-zip skip
  "Phone: "                at 12
  v-c-phone
  "Salesperson "         at 47 
  v-salesman  skip
  "Fax: "                    at 12
  v-c-fax                    skip(2)
   "Bill To:"                at 3
   inv-head.cust-no     at 21 
   "Ship To:"              at 38
   v-shipto-no             at 57 skip
   inv-head.cust-name at 12 v-shipto-name    at 47 skip
   inv-head.addr[1]      at 12 v-shipto-addr[1] at 47 skip
   inv-head.addr[2]      at 12 v-shipto-addr[2] at 47 skip
   v-addr3                   at 12 v-sold-addr3      at 47 skip(1)
    "Terms:   "             at 1  inv-head.terms-d format "x(15)"
    "Order #  "             at 29 v-del-no 
    "B O L #  "             at 48 inv-head.bol-no skip(1)
    "Ship Date"             at 1
    "Ship Via"               at 21
    "F O B"                   at 47
    "---------"                  at 1
    "--------------------"       at 21
    "-----------"                 at 47 skip
    v-date-ship               at 1
    v-shipvia                   at 21   format "x(20)"
    v-fob                         at 47  format "x(11)"skip(2)
    "Item"                      at 11
    "Qty Inv'd"                at 37 skip
    "Cust P O  Description"   at 1
    "Qty Shipped"            at 37
    "Price     Amount"       at 58 skip
    v-uline                        at 1  skip
with frame invhead-allpkg page-top no-labels no-box no-underline stream-io width 90.

form
  oe-ordl.po-no           at 1   format "x(9)"
  inv-line.i-no             at 11
  inv-line.inv-qty         to 46 format "->,>>>,>>9"
  inv-line.price            to 62 format "->>,>>9.99" space(0)
  inv-line.pr-uom          at 63 format "x(3)"
  inv-line.t-price           at 66 format "->>>,>>9.99"
  inv-line.i-name          at 11 format "x(20)"
  inv-line.ship-qty        to 46 format "->,>>>,>>9" skip
with frame allpkg-detail no-labels no-box no-underline down stream-io width 90.

form header
    "" 
    skip(4)  
    with frame allpkg-totals1 page-bottom no-labels no-box no-underline stream-io width 90.

form header
    "Merchandise Total"    to 62  v-tot-merch at 63 skip(1)  
    "Freight" at 3 "Tax" at 18
    "State" at 28 "Finance Chg" at 37 "Invoice" at 55
    inv-head.t-inv-freight format "->,>>9.99" at 1
    inv-head.t-inv-tax format "->,>>9.99" at 13
    v-tax-gr at 30  v-fin-chrg at 37 "Total"  at 55
    v-net  format "->,>>>,>>9.99" at 64

    with frame allpkg-totals2 page-bottom no-labels no-box no-underline stream-io width 90.

form v-ord# at 1 v-part-info at 19
    with frame beeler no-labels no-box no-underline down stream-io width 90.

form inv-misc.charge at  5 /* space(2) */ inv-misc.dscr at 26 format "x(25)"
     inv-misc.amt TO 76 format "->>,>>9.99" skip(1)
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

form " " to 80
     with frame blankl no-labels no-box no-underline stream-io width 90.

 
    find first cust where cust.company eq cocode and
                         active eq "X" no-lock no-error.
    if avail cust then do:
        v-c-phone = substring(cust.area-code,1,3) + "-" + substring(cust.phone,1,3) + "-" +  substring(cust.phone,4,4).
        v-c-fax     =  substring(cust.fax,1,3) + "-" + substring(cust.fax,4,3) + "-" +  substring(cust.fax,7,4).
    end.

    find first company where company.company eq cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
    v-fr-tax = oe-ctrl.f-tax.

    if oe-ctrl.prcom then
      assign
       v-c-name    = company.name
       v-c-addr[1] = company.addr[1]
       v-c-addr[2] = company.addr[2]
       v-c-city    = company.city
       v-c-state   = company.state
       v-c-zip     = company.zip.

    v-ord-del-hdr = "".
 
    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:
      
      assign
       v-shipto-no      = xinv-head.sold-no
       v-shipto-name    = xinv-head.sold-name
       v-shipto-addr[1] = xinv-head.sold-addr[1]
       v-shipto-addr[2] = xinv-head.sold-addr[2]
       v-shipto-city    = xinv-head.sold-city
       v-shipto-state   = xinv-head.sold-state
       v-shipto-zip     = xinv-head.sold-zip.

      v-del-no = 0.

      find first oe-bolh
          where oe-bolh.company eq cocode
            and oe-bolh.bol-no  eq xinv-head.bol-no
          use-index bol-no no-lock no-error.

      if avail oe-bolh then do:
        find first shipto
            where shipto.company eq cocode
              and shipto.cust-no eq oe-bolh.cust-no
              and shipto.ship-id eq oe-bolh.ship-id
            no-lock no-error.

        if avail shipto then
          assign
           v-shipto-no      = shipto.ship-id
           v-shipto-name    = shipto.ship-name
           v-shipto-addr[1] = shipto.ship-addr[1]
           v-shipto-addr[2] = shipto.ship-addr[2]
           v-shipto-city    = shipto.ship-city
           v-shipto-state   = shipto.ship-state
           v-shipto-zip     = shipto.ship-zip.
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
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
          v-line = 1
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

         IF xinv-line.ord-no NE 0 AND v-del-no EQ 0 THEN v-del-no = xinv-line.ord-no.

         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         for each oe-bolh no-lock where oe-bolh.b-no eq xinv-line.b-no:
           for each oe-boll no-lock where oe-boll.company eq oe-bolh.company and
              oe-boll.b-no eq oe-bolh.b-no and
              oe-boll.i-no eq xinv-line.i-no  and
              oe-boll.ord-no eq xinv-line.ord-no:

                                      /** Bill Of Lading TOTAL CASES **/
              assign v-bol-cases = v-bol-cases + oe-boll.cases.
           end. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
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
                 assign v-set-qty = v-set-qty + fg-set.QtyPerSet.
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

                 if avail fg-set and fg-set.QtyPerSet ne 0 then
                   assign v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
          if v-print-head then do:                        /* Print headers */                                     
              view frame invhead-allpkg.  
/*              v-printline = v-printline + 25.                                                   */
          end. /* if v-print-head then do: */                                                

        page.
        v-printline = 25.

        hide frame allpkg-totals2.
        view frame allpkg-totals1.

        for each inv-line no-lock where inv-line.r-no eq inv-head.r-no:
          assign v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company eq inv-line.company
                        and oe-boll.bol-no eq inv-head.bol-no
                        and oe-boll.i-no eq inv-line.i-no
                        and oe-boll.ord-no eq inv-line.ord-no use-index bol-no:


                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

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
          end. /* each oe-boll */

            assign v-line = v-line + 1
                   v-printline = v-printline + 2.

            v-beeler-lines = 0.
            if v-print-fmt eq "Beeler"   or
               v-print-fmt eq "ILWalker" or
               v-print-fmt eq "Allpkg"   or
               v-print-fmt eq "Imperial" then do v = 1 to 3:

              if v-print-fmt eq "Imperial" and v eq 1 then next.
              v-ord# = if v = 1 and v-print-fmt = "Beeler" then
                inv-line.ord-no else 0.
              v-part-info = if      v eq 1 then inv-line.part-no
                            else if v eq 2 then inv-line.part-dscr1
                            else                inv-line.part-dscr2.

              if v-part-info ne "" or v-ord# > 0 then
                v-beeler-lines = v-beeler-lines + 1.
            end.

            do i = 1 to 5:
              if v-case-cnt[i] ne "" then
                assign v-line = v-line + 1
                       v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.

/*
            if v-printline ge 56 then                                                               
            do:
              put skip(56 - v-printline) "* CONTINUED *" at 68.                        
              v-printline = 2 + v-beeler-lines.
              
              page.
              v-printline = 25 + v-beeler-lines.
            end.
*/

            if v-print-fmt eq "allpkg" then
            do:
              find first oe-ordl where oe-ordl.company eq inv-line.company and
                                       oe-ordl.ord-no eq inv-line.ord-no and
                                       oe-ordl.line eq inv-line.line and
                                       oe-ordl.i-no eq inv-line.i-no
                                       no-lock no-error.

              display oe-ordl.po-no when avail oe-ordl
                      inv-line.i-no inv-line.inv-qty
                      inv-line.price inv-line.pr-uom inv-line.t-price
                      inv-line.i-name inv-line.ship-qty
                      with frame allpkg-detail.

              down with frame allpkg-detail.
              assign v-tot-merch = v-tot-merch + inv-line.t-price.
              
            end.

                                          /** Display Case Count Lines **/
            do i = 1 to 5:
              if v-case-cnt[i] ne "" then
                put v-case-cnt[i] at 11 skip.
            end. /* 1 to 5 */

            put skip(1).
            assign v-printline = v-printline + 1.
        end. /* each inv-line */

        for each inv-misc of inv-head no-lock where
          inv-misc.bill eq "Y" break by ord-no:
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 27 skip(1).
            assign v-printline = v-printline + 2.
          end.
/*
            if v-printline ge 56 then
            do:
              put skip(56 - v-printline) "* CONTINUED *" at 68.
              assign v-printline = 0.
              page.
              v-printline = 27.
            end.
            else
*/
             down with frame detailm.
            
            display
                 inv-misc.charge inv-misc.dscr inv-misc.amt
            with frame detailm.

             assign v-tot-merch = v-tot-merch + inv-misc.amt.
             
            assign v-line = v-line + 1
               v-printline = v-printline + 2.
        end. /* each inv-misc */

       if v-prntinst then do:
         do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then
            assign v-printline = v-printline + 1.
         end.

/*
         if v-printline ge 56 then
         do:
           put skip(56 - v-printline) "* CONTINUED *" at 68.
           page.
           v-printline = 25.
         end.
*/

         do i = 1 to 4:
           if inv-head.bill-i[i] ne "" then do:
             put inv-head.bill-i[i] at 5 skip.
             assign v-printline = v-printline + 1.
           end.
         end. /* 1 to 4 */
       end. 

        do:  /* T O T A L S */
          old-frt = inv-head.t-inv-freight.

          if not inv-head.f-bill then inv-head.t-inv-freight = 0.

          assign
           tmp1  = 0
           tmp2  = ?

           v-net = v-tot-merch + inv-head.t-inv-freight + inv-head.t-inv-tax + v-fin-chrg.

          release terms.
          find first terms where terms.t-code eq inv-head.terms no-lock no-error.

          if avail terms then
            assign
             tmp1 = v-net * (round(terms.disc-rate / 100, 2))
             tmp2 = today + terms.disc-days.

          find first cust where cust.company eq cocode and
                     cust.cust-no eq inv-head.cust-no no-lock no-error.
          
          clear frame totals-comp  no-pause.
          clear frame totals-comp2 no-pause.
          clear frame totals      no-pause.

        end. 
      end. /* do transaction avail inv-head */
     
      if last-of(report.key-02) then do:                     
        find first oe-ord
            where oe-ord.company eq cocode
              and oe-ord.ord-no  eq v-del-no
            no-lock no-error.
        if avail oe-ord then 
          v-tax-gr = oe-ord.tax-gr.
        else 
          v-tax-gr = inv-head.tax-gr.
            
        put skip(55 - v-printline).                        
                     
        hide frame allpkg-totals1.
        view frame allpkg-totals2.
        page.
      end.
       
      assign 
       v-tot-merch = 0                            
       v-net       = 0.                        
    end. /* each report, first xinv-head */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

