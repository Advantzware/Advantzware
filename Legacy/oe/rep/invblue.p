/* ----------------------------------------------- oe/rep/invblue.p 04/99 RLL */
/*                                                                            */
/* Print invoice when sys-ctrl.char-fld eq "Blueridg" - O/E Module            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def workfile w-sman
  field sman as char format "x(4)".

{oe/rep/invoice.i}

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
def var v-printline as int.
def var v-pitch like printer.pitch.
def var v-len as int.
def var v-hldpitch like printer.pitch.
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
def var v-tax-total as dec.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-inv-qty as int format "99999" no-undo.
def var v-bo-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>9.9999" no-undo.
def var v-t-price as dec format ">>>>9.99" no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-bol-no like inv-head.bol-no no-undo.

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
     skip(3)
     inv-head.inv-no       to 74
     skip(1)
     v-inv-date            to 74
     skip(7)
     inv-head.cust-name    at 9
     v-shipto-name         at 46
     inv-head.addr[1]      at 12
     v-shipto-addr[1]      at 46
     inv-head.addr[2]      at 12
     v-shipto-addr[2]      at 46
     v-addr3               at 12
     v-sold-addr3          at 46
     skip(6)
     inv-head.bol-no
     space(2)
     v-shipvia             at 11   format "x(20)" when avail carrier
     v-fob                 at 31
     inv-head.terms-d      at 48   format "x(30)"
     skip(1)
     v-rel-po-no
     v-ord-date            at 21
     v-salesman            at 34
     inv-head.cust-no      at 51
     v-ord-no              at 67
     skip(2)
    with frame invhead page-top no-labels no-box no-underline stream-io width 80.

form v-part-info at 32
    with frame beeler no-labels no-box no-underline down stream-io width 90.

form inv-misc.charge at 20
     inv-misc.dscr   at 32
     inv-misc.amt    to 80 format "->>>>>9.99"
     skip(1)
    with frame detailm no-labels no-box no-underline down stream-io width 90.

    find first company where company.company eq cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      assign
       v-shipto-name    = xinv-head.sold-name
       v-shipto-addr[1] = xinv-head.sold-addr[1]
       v-shipto-addr[2] = xinv-head.sold-addr[2]
       v-shipto-city    = xinv-head.sold-city
       v-shipto-state   = xinv-head.sold-state
       v-shipto-zip     = xinv-head.sold-zip.

      v-del-no = 0.

      find first oe-bolh
          where oe-bolh.company eq xinv-head.company
            and oe-bolh.bol-no  eq xinv-head.bol-no
          use-index bol-no no-lock no-error.

      if avail oe-bolh then do:
        find first oe-relh
            where oe-relh.company eq oe-bolh.company
              and oe-relh.r-no    eq oe-bolh.r-no
            no-lock no-error.

        if avail oe-relh then
          find first shipto
              where shipto.company eq oe-bolh.company
                and shipto.cust-no eq oe-relh.cust-no
                and shipto.ship-id eq oe-bolh.ship-id
              no-lock no-error.

        if avail shipto then
          assign
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

        v-fob = if inv-head.fob-code begins "ORIG" then "Origin"
                                                   else "Destination".

        find first carrier
            where carrier.company eq inv-head.company
              and carrier.carrier eq inv-head.carrier
            no-lock no-error.

        assign
         v-shipvia    = if avail carrier then carrier.dscr else ""
         v-addr3      = inv-head.city + ", " + inv-head.state + "  " +
                        inv-head.zip
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state + "  " +
                        v-shipto-zip
         v-printline  = 0.

        find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error.
            
        if avail stax then
          assign
           v-tax-rate    = stax.tax-rate[1] +
                           stax.tax-rate[2] +
                           stax.tax-rate[3]
           v-tax-code[1] = stax.tax-code[1]
           v-tax-code[2] = stax.tax-code[2]
           v-tax-code[3] = stax.tax-code[3]
           v-tx-rate[1]  = stax.tax-rate[1]
           v-tx-rate[2]  = stax.tax-rate[2]
           v-tx-rate[3]  = stax.tax-rate[3].

        v-tot-pallets = 0.

        for each xinv-line
            where xinv-line.r-no eq inv-head.r-no
            no-lock break by xinv-line.i-no:

         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            w-sman.sman = xinv-line.sman[i].
          end.
         end.

         assign
          v-tot-qty  = v-tot-qty + xinv-line.ship-qty
          v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                       xinv-line.qty, 2) * xinv-line.inv-qty).

         for each oe-bolh
             where oe-bolh.b-no   eq xinv-line.b-no
               and oe-bolh.ord-no eq xinv-line.ord-no
             no-lock:

           for each oe-boll
               where oe-boll.company eq oe-bolh.company
                 and oe-boll.b-no    eq oe-bolh.b-no
                 and oe-boll.i-no    eq xinv-line.i-no
               no-lock:

                                      /** Bill Of Lading TOTAL CASES **/
             v-bol-cases = v-bol-cases + oe-boll.cases.
           end. /* each oe-boll */

           assign
            v-date-ship   = oe-bolh.bol-date
            v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         end. /* each oe-bolh */

         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then do:
             find first eb
                 where eb.company  eq xinv-line.company
                   and eb.est-no   eq xinv-line.est-no
                   and eb.e-num    eq xinv-line.e-num
                   and eb.form-no  eq xinv-line.form-no
                   and eb.blank-no eq xinv-line.blank-no
                 no-lock no-error.

             if xinv-line.form-no eq 0 and xinv-line.est-type eq 2 then do:
               for each fg-set
                   where fg-set.company eq xinv-line.company
                     and fg-set.set-no  eq xinv-line.i-no
                   no-lock:
                 v-set-qty = v-set-qty + fg-set.QtyPerSet.
               end.

               if v-set-qty = 0 then v-set-qty = 1.

               for each eb
                   where eb.company eq xinv-line.company
                     and eb.est-no  eq xinv-line.est-no
                     and eb.e-num   eq xinv-line.e-num
                     and eb.form-no ne 0
                   no-lock:

                 find first fg-set
                     where fg-set.company eq xinv-line.company
                       and fg-set.set-no  eq xinv-line.i-no
                       and fg-set.part-no eq eb.stock-no
                     no-lock no-error.

                 assign
                  v-part-qty = (if avail fg-set and fg-set.QtyPerSet ne 0 then
                                  fg-set.QtyPerSet else 1) / v-set-qty
                  v-tot-cas  = round((if eb.cas-cnt eq 0 then
                                        v-t-weight else v-tot-qty) *
                                     v-part-qty / eb.cas-wt, 2).

                 if v-bol-cases ne 0 then v-tot-cas = v-bol-cases.
               end. /* each eb */
             end. /* do */

             else
             if avail eb then do:
               v-tot-cas = round((if eb.cas-cnt eq 0 then
                                    v-t-weight else v-tot-qty) / eb.cas-wt, 2).

               if v-bol-cases ne 0 then v-tot-cas = v-bol-cases.
             end. /* do */
           end. /* est-no ne "" */

           assign
            v-t-weight = 0
            v-tot-cas  = 0
            v-tot-qty  = 0.
         end. /* last-of i-no */
        end. /* each xinv-line */
                                         /** Build Salesman Id String **/
        v-salesman = "".
        for each w-sman break by w-sman.sman:
          if first-of(w-sman.sman) then v-salesman = v-salesman + w-sman.sman.
          delete w-sman.
        end.

        find first oe-bolh
            where oe-bolh.company eq inv-head.company
              and oe-bolh.bol-no  eq inv-head.bol-no
            use-index bol-no no-lock no-error.
        if avail oe-bolh then v-rel-po-no = oe-bolh.po-no.

        find first inv-line
            where inv-line.r-no eq inv-head.r-no
            no-lock no-error.
        if avail inv-line then do:
          v-rel-po-no = inv-line.po-no.
          find first oe-ord
              where oe-ord.company eq cocode
                and oe-ord.ord-no  eq inv-line.ord-no
              no-lock no-error.

          if avail oe-ord then
            assign
             v-ord-no   = oe-ord.ord-no
             v-ord-date = oe-ord.ord-date.
        end.

        view frame invhead.

        page.

        for each inv-line no-lock where inv-line.r-no eq inv-head.r-no:
          assign
           v-case-line = ""
           v-part-line = ""
           v-case-cnt = "".

          for each oe-boll
              where oe-boll.company eq inv-line.company
                and oe-boll.bol-no  eq inv-head.bol-no
                and oe-boll.i-no    eq inv-line.i-no
              use-index bol-no no-lock:

                                      /** Build Case Count Display Lines **/
            assign
             v-case-line = if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
                             string(oe-boll.cases) + " @ " +
                             string(oe-boll.qty-case)
                           else ""

             v-part-line = if oe-boll.partial ne 0 then
                             "1" + " @ " + string(oe-boll.partial) else "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) gt length(v-case-line) and
                 v-case-line ne ""                                   then
                assign
                 v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                 v-case-line   = "".

              if (80 - length(v-case-cnt[i])) gt length(v-part-line) and
                 v-part-line ne ""                                   then
                assign
                 v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                 v-part-line   = "".
            end. /* 1 to 5 */
          end. /* each oe-boll */

          v-printline = v-printline + 2.

          v-beeler-lines = 0.
          do v = 1 to 2:
            v-part-info = if v eq 1 then inv-line.part-dscr1
                          else           inv-line.part-dscr2.

            if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
          end.
          v-printline = v-printline + v-beeler-lines.

          if v-printline gt 27 then do:
            v-printline = v-printline - (2 + v-beeler-lines).

            put skip(31 - v-printline)
                "Page"
                page-number - v-last-page format ">9".

            v-printline = 2 + v-beeler-lines.

            page.
          end.

          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq inv-line.ord-no
                and oe-ordl.i-no    eq inv-line.i-no
              no-lock no-error.
          if avail oe-ordl then
            v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
                           oe-ordl.t-ship-qty) lt 0 then 0 else
                          (inv-line.qty - inv-line.ship-qty -
                           oe-ordl.t-ship-qty).
          else
            v-bo-qty = if (inv-line.qty - inv-line.ship-qty) lt 0
                         then 0 else inv-line.qty - inv-line.ship-qty.

          assign
           v-tax-total = v-tax-total +
                         if inv-line.tax then inv-line.t-price else 0
           v-inv-qty   = inv-line.qty
           v-ship-qty  = inv-line.ship-qty
           v-i-no      = inv-line.part-no /* inv-line.i-no */
           v-i-dscr    = inv-line.i-name
           v-price     = inv-line.price
           v-t-price   = inv-line.t-price.

          display v-inv-qty  to 6  format "->>>>9"
                  v-ship-qty to 12 format "->>>>9"
                  v-bo-qty   to 18 format ">>>>9"
                  v-i-no     at 20 format "x(11)"
                  v-i-dscr   at 32 format "x(27)"
                  v-price    to 69 format ">>>>9.9999"
                  v-t-price  to 80 format "->>>>>9.99"

              with frame f-1 no-attr-space no-labels no-box
                   no-underline down stream-io width 90.

          do v = 1 to 2:
            v-part-info = if v eq 1 then inv-line.part-dscr1
                          else           inv-line.part-dscr2.

            if v-part-info ne "" then do:
              display v-part-info skip with frame beeler.
              down with frame beeler.
            end.
          end.

          put skip(1).
        end. /* each inv-line */

        for each inv-misc
            where inv-misc.company eq inv-head.company
              and inv-misc.r-no    eq inv-head.r-no
              and inv-misc.bill    eq "Y"
            no-lock break by ord-no with frame detailm:

          assign
           v-tax-total    = v-tax-total +
                            if inv-misc.tax then inv-misc.amt else 0
           v-beeler-lines = 0
           v-printline    = v-printline + 2.

          if first(inv-misc.ord-no) then v-beeler-lines = 2.

          if v-printline gt 27 then do:
            v-printline = v-printline - (2 + v-beeler-lines).

            put skip(31 - v-printline)
                "Page"
                page-number - v-last-page format ">9".

            v-printline = 2 + v-beeler-lines.

            page.
          end.

          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 20 skip(1).
            assign v-printline    = v-printline + 3.
          end.

          display inv-misc.charge inv-misc.dscr inv-misc.amt.
          down.
        end. /* each inv-misc */

        if v-prntinst then do:
          do i = 1 to 4:
            if inv-head.bill-i[i] ne "" then do:
              v-printline = v-printline + 1.

              if v-printline gt 27 then do:
                put skip(31 - v-printline)
                    "Page"
                    page-number - v-last-page format ">9".

                v-printline = 1.
                page.
              end.

              put inv-head.bill-i[i] at 2 skip.
            end.
          end. /* 1 to 4 */
        end.

        /* T O T A L S */
        put skip(28 - v-printline)
            "NonTaxable SubTotal"               at 48
            inv-head.t-inv-rev - v-tax-total - inv-head.t-inv-tax -
                        (if inv-head.f-bill then inv-head.t-inv-freight else 0)
                        format "->>>>>>>>>9.99" skip
            "Taxable SubTotal   "               at 48
            v-tax-total                         format "->>>>>>>>>9.99" skip
            "Tax                "               at 48
            inv-head.t-inv-tax                  format "->>>>>>>>>9.99" skip
            "Page"                              at 2
            page-number - v-last-page           format ">9"
            "Total              "               at 48
            inv-head.t-inv-rev                  format "->>>>>>>>>9.99" skip.

        assign
         v-tax-total      = 0
         v-last-page      = page-number
         inv-head.printed = yes
         inv-head.stat    = "X".
      end. /* do transaction avail inv-head */
    end. /* each xinv-head */

/* end ---------------------------------- copr. 1997 Advanced Software, Inc. */
