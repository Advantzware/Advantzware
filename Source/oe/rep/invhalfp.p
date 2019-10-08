/* ---------------------------------------------- oe/rep/invhalfp.p 01/97 JLF */
/*                                                                            */
/* PRINT INVOICE when v-print-fmt eq "1/2 Page" - O/E Module            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

{custom/notesdef.i}
DEF VAR v-inst AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

def var v-salesman as char format "x(3)".
def var v-fob as char format "x(11)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-lines as int.
def var v-prt-lines as int.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-pitch like asi.printer.pitch.
def var v-len as int.
def var v-hldpitch like asi.printer.pitch.
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date initial today.
def shared var v-fr-tax as logical initial no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as logical initial no.
def var v-date-ship as date initial today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases like oe-boll.cases.
def var v-set-qty as DECIMAL.
def var v-part-qty as dec format "999.9999".
def var old-frt like inv-head.t-inv-freight no-undo.
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
def var disp-frt as char init "Freight:" format "x(8)".
def var v-max-lines as int.
def var v-x as char format "x(4)" initial "" no-undo.
def var v-uom like inv-line.pr-uom no-undo.
def var v-tot-sqft as dec format ">,>>>,>>9.99" no-undo.
DEF VAR v-halfp AS LOG NO-UNDO.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-part-info as char format "x(30)".
def var v as int.
def var v-inv-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>9.9999" no-undo.
def var v-t-price as dec format ">>>>9.99" no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-hdr as char format "x(80)" extent 4.
def var v-job as char format "x(9)" no-undo.
def var v-bol-no like inv-head.bol-no no-undo.
DEF VAR v-paid-by AS cha FORM "x(14)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT"
                      no-lock no-error.
v-paid-by = IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "Livngstn" THEN "If Received By"
            ELSE "If Paid By".
assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
            "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
            "      PRICE       TOTAL ".

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
     skip(11)
     v-hdr[1]
     inv-head.inv-no       to 66
     v-inv-date            to 77
     skip(3)
     v-hdr[2]
     inv-head.cust-name    at 2
     v-shipto-name         at 47
     inv-head.addr[1]      at 2
     v-shipto-addr[1]      at 47
     inv-head.addr[2]      at 2
     v-shipto-addr[2]      at 47
     v-addr3               at 2
     v-sold-addr3          at 47
     skip(2)
     v-hdr[3]
     v-rel-po-no
     v-date-ship
     space(2)
     v-shipvia                     format "x(20)"
     v-fob
     inv-head.terms-d              format "x(15)"
     space(2)
     v-salesman
     skip(1)
     v-hdr[4]
     skip(1)
  with frame invhead page-top no-labels no-box no-underline stream-io width 80.

form header
     skip(7)
     v-hdr[1]
     inv-head.inv-no       to 66
     v-inv-date            to 77
     skip(3)
     v-hdr[2]
     inv-head.cust-name    at 2
     v-shipto-name         at 47
     inv-head.addr[1]      at 2
     v-shipto-addr[1]      at 47
     inv-head.addr[2]      at 2
     v-shipto-addr[2]      at 47
     v-addr3               at 2
     v-sold-addr3          at 47
     skip(1)
     v-hdr[3]
     v-rel-po-no
     v-date-ship
     space(2)
     v-shipvia                     format "x(20)"
     v-fob
     inv-head.terms-d              format "x(15)"
     space(2)
     v-salesman
     skip(1)
     v-hdr[4]
     skip(1)
  with frame invhead-tri page-top no-labels no-box no-underline stream-io width 80.

form header
     skip(10)
     v-hdr[1]
     inv-head.inv-no       to 66
     v-inv-date            to 77
     skip(3)
     v-hdr[2]
     inv-head.cust-name    at 2
     v-shipto-name         at 47
     inv-head.addr[1]      at 2
     v-shipto-addr[1]      at 47
     inv-head.addr[2]      at 2
     v-shipto-addr[2]      at 47
     v-addr3               at 2
     v-sold-addr3          at 47
     skip(1)
     v-hdr[3]
     v-rel-po-no
     v-date-ship
     space(2)
     v-shipvia                     format "x(20)"
     v-fob
     inv-head.terms-d              format "x(15)"
     space(2)
     v-salesman
     skip(1)
     v-hdr[4]
     skip(1)
  with frame invheadh page-top no-labels no-box no-underline stream-io width 80.

form v-inv-qty                 format ">>>>>>9-"
     v-i-no            at 15   format "x(13)"
     v-i-dscr          at 29   format "x(30)"
     v-price           at 61   format ">>>>9.99<<"
     v-t-price         to 80   format ">>>>>9.99-"
  with frame liv-detail no-attr-space no-labels
        no-box no-underline down stream-io width 80.

form v-inv-qty                 format ">>>>>>9-"
     v-x               at 10   format "x(4)"
     v-job             at 15   format "x(9)"
     v-i-no            at 29   format "x(15)"
     v-price           to 67   format ">>>>9.99"
     v-uom             at 68   format "x(2)"
     v-t-price         to 80   format ">>>>>9.99-" skip
     "BOL#"            at 10
     v-bol-no          at 15
     v-i-dscr          at 29   format "x(30)" skip
  with frame tri-detail no-attr-space
        no-labels no-box no-underline down stream-io width 80.

form v-inv-qty                 format ">>>>>>9-"
     v-x               at 10   format "x(4)"
     v-job             at 15   format "x(9)"
     v-i-no            at 29   format "x(15)"
     v-price           to 69   format ">>>>9.99<<"
     v-t-price         to 80   format ">>>>>9.99-" skip
     "BOL#"            at 15
     v-bol-no          at 20
     v-i-dscr          at 29   format "x(30)"
  with frame midd-detail no-attr-space
        no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 29
  with frame beeler no-labels no-box no-underline down stream-io width 80.

form inv-misc.charge
     inv-misc.dscr     at 29
     inv-misc.amt      to 80   format ">>>>>9.99-"
     skip(1)
  with frame detailm no-labels no-box no-underline down stream-io width 80.

form "Freight" to 14 "Tax" to 22
     "Net Amount" TO 38 "Cash Disc" TO 51 v-paid-by /*"If Paid By"*/ AT 54
     "Invoice Amt" TO 80
     inv-head.t-inv-freight format "->,>>9.99" at 6
     inv-head.t-inv-tax format "->,>>9.99" at 16
     v-net to 38 format "->,>>>,>>9.99"
     tmp1 to 51 format "->,>>9.99"
     tmp2 at 55
     space(0)
     inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form "Freight" to 14
     "Net Amount" TO 38 "Cash Disc" TO 51 v-paid-by /*"If Paid By"*/ AT 54
     "Invoice Amt" TO 80
     inv-head.t-inv-freight format "->,>>9.99" at 6
     inv-head.t-inv-tax format "->,>>9.99" at 15
     v-net to 38 format "->,>>>,>>9.99"
     tmp1 to 51 format "->,>>9.99"
     tmp2 at 55
     space(0)
     inv-head.t-inv-rev to 80 format "->,>>>,>>9.99"
  with frame totals-comp2 no-labels no-box no-underline stream-io width 80.

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
     skip(1)
  with frame tax no-labels no-box no-underline stream-io width 80.

form " " to 80
  with frame blankl no-labels no-box no-underline stream-io width 80.


    find first company where company.company = cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

    IF NOT v-print-head then v-hdr = "".
        
    IF v-print-fmt EQ "Livngstn" THEN
      ASSIGN
       v-halfp     = NO
       v-max-lines = 28.
    ELSE
      ASSIGN
       v-halfp     = YES
       v-max-lines = IF v-print-fmt EQ "Clev 1/2" THEN 9 ELSE 12.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip.

      v-del-no = 0.

      find first oe-bolh where oe-bolh.company = xinv-head.company and
          oe-bolh.bol-no = xinv-head.bol-no use-index bol-no no-lock no-error.
      if avail oe-bolh then do:
        find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then
        find first shipto where shipto.company  = oe-bolh.company and
                   shipto.cust-no = oe-relh.cust-no and
                   shipto.ship-id = oe-bolh.ship-id no-lock no-error.
        if avail shipto then
        assign  v-shipto-name = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city = shipto.ship-city
                v-shipto-state = shipto.ship-state
                v-shipto-zip = shipto.ship-zip.

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

        find first cust
            where cust.company eq cocode
              and cust.cust-no eq inv-head.cust-no
            no-lock no-error.

        v-salesman = if avail cust then cust.sman else "".

        find carrier where carrier.company = inv-head.company and
          carrier.carrier = inv-head.carrier no-lock no-error.
         if avail carrier then
           assign v-shipvia = carrier.dscr.
         else
           assign v-shipvia = "".
        assign
          v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip.
              
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
        for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
          break by xinv-line.i-no:

         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         for each oe-bolh no-lock where oe-bolh.b-no = xinv-line.b-no:
           for each oe-boll no-lock where oe-boll.company = oe-bolh.company and
              oe-boll.b-no = oe-bolh.b-no and
              oe-boll.ord-no = xinv-line.ord-no AND
              oe-boll.i-no = xinv-line.i-no AND
              oe-boll.line = xinv-line.line:

                                      /** Bill Of Lading TOTAL CASES **/
              assign v-bol-cases = v-bol-cases + oe-boll.cases.
           end. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         end. /* each oe-bolh */
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then do:
             find first eb where eb.company = xinv-line.company and
               eb.est-no = xinv-line.est-no and
               eb.e-num = xinv-line.e-num and
               eb.form-no = xinv-line.form-no and
               eb.blank-no = xinv-line.blank-no no-lock no-error.

             if xinv-line.form-no = 0 and xinv-line.est-type = 2 then do:
               for each fg-set no-lock where fg-set.company = xinv-line.company
                  and fg-set.set-no = xinv-line.i-no:
                 assign v-set-qty = v-set-qty + fg-set.QtyPerSet.
               end.
               if v-set-qty = 0 then
                  assign v-set-qty = 1.
               for each eb no-lock where eb.company = xinv-line.company and
                  eb.est-no = xinv-line.est-no and
                  eb.e-num = xinv-line.e-num and
                  eb.form-no NE 0:
                 find fg-set where fg-set.company = xinv-line.company and
                    fg-set.set-no = xinv-line.i-no  and
                    fg-set.part-no = eb.stock-no no-lock no-ERROR.

                 if avail fg-set and fg-set.QtyPerSet NE 0 then
                   assign v-part-qty = fg-set.QtyPerSet / v-set-qty.
                 else
                   assign v-part-qty = 1 / v-set-qty.


                if eb.cas-cnt = 0 then
                   assign v-tot-cas = round((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 else
                   assign v-tot-cas = round((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
                 /***
                 assign v-tot-pallets = v-tot-pallets +
                      round((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
               end. /* each eb */
             end. /* do */
             else
             if avail eb then
             do:
               if eb.cas-cnt = 0 then
                 assign v-tot-cas = round(v-t-weight / eb.cas-wt, 2).
               else
                 assign v-tot-cas = round(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
               /***
               assign v-tot-pallets = v-tot-pallets +
                   round((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             end. /* do */
           end. /* est-no ne "" */
          assign
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         end. /* last-of i-no */
        end. /* each xinv-line */
        
        find first inv-line
            where inv-line.r-no  EQ inv-head.r-no
              AND inv-line.po-no NE ""
            no-lock no-error.   /*djk*/
        v-rel-po-no = if avail inv-line THEN inv-line.po-no ELSE "".

        IF v-print-fmt EQ "Livngstn" THEN
          SUBSTR(v-hdr[2],09,LENGTH(TRIM(inv-head.cust-no))) = TRIM(inv-head.cust-no).

        if v-print-fmt eq "TriState" then
          view frame invhead-tri.  /* Print TriState */
        else
        if v-halfp then
          view frame invheadh.  /* Print headers */
        else
          view frame invhead.  /* Print headers */

        page.

        assign
         v-prt-lines = 0
         v-net       = 0.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-no
            by inv-line.line:
          assign
           v-case-line = ""
           v-part-line = ""
           v-case-cnt = "".

          for each oe-boll no-lock where oe-boll.company = inv-line.company
                        and oe-boll.bol-no = inv-head.bol-no
                        /*and oe-boll.b-no = inv-line.b-no*/
                        and oe-boll.i-no = inv-line.i-no use-index bol-no:

                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
            assign v-case-line = string(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
            else assign v-case-line = "".
            if oe-boll.partial ne 0 then
            assign v-part-line = "1" + " @ " + string(oe-boll.partial).
            else assign v-part-line = "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) > length(v-case-line) and
                v-case-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                     v-case-line = "".
              if (80 - length(v-case-cnt[i])) > length(v-part-line) and
                v-part-line ne "" then
              assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                     v-part-line = "".
            end. /* 1 to 5 */
          end. /* each oe-boll */

          v-lines = 1 + (if v-halfp then 1 else 0).

          do v = 1 to 2:
            v-part-info = if v eq 1 then inv-line.part-dscr1
                          else           inv-line.part-dscr2.

            if v-part-info ne "" then v-lines = v-lines + 1.
          end.

          v-prt-lines = v-prt-lines + v-lines.

          if v-prt-lines gt v-max-lines + 1 then do:
            v-prt-lines = v-prt-lines - v-lines.
            put skip(v-max-lines + 3 - v-prt-lines) "* CONTINUED *" to 80.
            v-prt-lines = v-lines.
            page.
          end.

          v-x = "".
          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq inv-line.ord-no
                and oe-ordl.i-no    eq inv-line.i-no
              no-lock no-error.
          if avail oe-ordl then do:
            find first oe-ord
                where oe-ord.company eq cocode
                  and oe-ord.ord-no  eq oe-ordl.ord-no
                no-lock no-error.
            v-x = if avail oe-ord and
                     oe-ordl.ship-qty ge
                  (oe-ordl.qty - (oe-ordl.qty * oe-ordl.under-pct / 100)) then
                    "   X" else "X   ".
          end.

          assign
           v-inv-qty = inv-line.inv-qty
           v-i-no    = inv-line.i-no
           v-i-dscr  = inv-line.i-name
           v-price   = inv-line.price
           v-uom     = inv-line.pr-uom
           v-t-price = inv-line.t-price
           v-job     = fill(" ",6 - length(trim(inv-line.job-no))) +
                       trim(inv-line.job-no) + "-" +
                       trim(string(inv-line.job-no2,"99"))
           v-bol-no  = inv-head.bol-no.

          if v-print-fmt eq "TriState" then
          do:
            assign v-i-no = inv-line.part-no
                   v-tot-sqft = 0.
            find first itemfg where itemfg.company eq cocode
                                and itemfg.i-no eq inv-line.i-no
                                no-lock no-error.
            if avail itemfg then
              assign v-tot-sqft = inv-line.inv-qty * itemfg.t-sqft.
          end.

          if trim(v-job) begins "-" then assign v-job = "".

          if v-print-fmt eq "TriState" then
          do:
            display v-inv-qty
                    v-x
                    v-job
                    v-i-no
                    v-uom
                    v-i-dscr
                    v-price
                    v-t-price
                    v-bol-no

                with frame tri-detail.
          end.
          else
          if v-halfp then
            display v-inv-qty
                    v-x
                    v-job
                    v-i-no
                    v-i-dscr
                    v-price
                    v-t-price
                    v-bol-no

                with frame midd-detail.
          else
            display v-inv-qty
                    v-i-no
                    v-i-dscr
                    v-price
                    v-t-price

                with frame liv-detail.

          do v = 1 to 2:
            v-part-info = if v eq 1 then inv-line.part-dscr1
                          else           inv-line.part-dscr2.

            if v-part-info ne "" then do:
              display v-part-info skip with frame beeler.
              down with frame beeler.
            end.
          end.

          if v-print-fmt eq "TriState" then
          do:
            put "SQ FT." at 29 v-tot-sqft at 36 skip.
            v-prt-lines = v-prt-lines + 1.
          end.

          put skip(1).
          v-prt-lines = v-prt-lines + 1.
        end. /* each inv-line */

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:

          assign
           v-lines     = 2 + (if first(inv-misc.ord-no) then 2 else 0)
           v-prt-lines = v-prt-lines + v-lines.

          if v-prt-lines gt v-max-lines + 1 then do:
            v-prt-lines = v-prt-lines - v-lines.
            put skip(v-max-lines + 3 - v-prt-lines) "* CONTINUED *" to 80.
            v-prt-lines = v-lines.
            page.
          end.

          if first(inv-misc.ord-no) then
            put "** Miscellaneous Items **" at 23 skip(1).

          display inv-misc.charge inv-misc.dscr inv-misc.amt.
          down.
        end. /* each inv-misc */

        if v-prntinst then do:
          {custom/notesprt.i inv-head v-inst 4}

          do i = 1 to 4:
            if v-inst[i] ne "" then do:
              v-prt-lines = v-prt-lines + 1.
              put skip(1).
              leave.
            end.
          end. /* 1 to 4 */

          do i = 1 to 4:
            if v-inst[i] ne "" then do:
              v-prt-lines = v-prt-lines + 1.
              if v-prt-lines gt v-max-lines + 1 then do:
                v-prt-lines = v-prt-lines - 1.
                put skip(v-max-lines + 3 - v-prt-lines) "* CONTINUED *" to 80.
                v-prt-lines = 1.
                page.
              end.

              put v-inst[i] at 5 skip.
            end.
          end. /* 1 to 4 */
        end.

        if v-prt-lines gt v-max-lines then v-prt-lines = v-max-lines.

        put skip(v-max-lines - v-prt-lines).

        old-frt = inv-head.t-inv-freight.

        if not inv-head.f-bill then inv-head.t-inv-freight = 0.

        RUN custom/inv-dnet.p (ROWID(inv-head), OUTPUT v-net).

        find first terms where terms.company = inv-head.company and
          terms.t-code = inv-head.terms no-lock no-error.

        if avail terms then
          assign
           tmp1 = v-net * (round(terms.disc-rate / 100, 2))
           tmp2 = today + terms.disc-days.

        find first cust where cust.company = cocode and
                   cust.cust-no = inv-head.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" then do:

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

          put skip(1).
          display v-tax-code[1]
                  net1
                  v-tax-code[2]
                  net2
                  v-tax-code[3]
                  net3
                  inv-head.t-inv-tax
              with frame tax.
        end.

        else display " " skip(1) with frame blankl.

        clear frame totals-comp  no-pause.
        clear frame totals-comp2 no-pause.

        if cust.sort = "N" then
           display inv-head.t-inv-freight
                   inv-head.t-inv-tax
                   v-net
                   tmp1 when avail terms
                   tmp2 when avail terms
                   v-net @ inv-head.t-inv-rev
                   inv-head.t-inv-rev when avail terms OR inv-head.terms = "CASH"
                   v-paid-by
               with frame totals-comp.
        else
           display inv-head.t-inv-freight
                   v-net
                   tmp1 when avail terms
                   tmp2 when avail terms
                   v-net @ inv-head.t-inv-rev
                   inv-head.t-inv-rev when avail terms OR inv-head.terms = "CASH"
                   v-paid-by
               with frame totals-comp2.
      end. /* DO TRANSACTION */
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1997 Advanced Software, Inc. */

