/* ---------------------------------------------- oe/rep/invargrv.p 02/96 JLF */
/* PRINT INVOICE when sys-ctrl.char-fld eq "Argrov" - O/E Module              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

def var v-salesman as char format "x(14)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-shipto-name as char format "x(30)".
def var v-shipto-addr as char format "x(30)" extent 2.
def var v-shipto-city as char format "x(15)".
def var v-shipto-state as char format "x(2)".
def var v-shipto-zip as char format "x(10)".
def var v-line as int.
def var v-printline as int.
def var v-invhead as char format "x(13)" initial
  "I N V O I C E".
def var v-len as int.
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date initial TODAY.
def shared var v-fr-tax as logical initial no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as logical initial no.
def var v-date-ship as date initial today.
def var v-del-no as int format ">>>>>>".
def var v-bol-cases LIKE oe-boll.cases.
def var v-set-qty AS DECIMAL.
def var v-part-qty AS DEC FORMAT "999.9999".
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
def var minus-ship as int.

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def workfile w-sman
  field sman as char format "x(4)".

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(4)" no-undo.

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
  skip(2)
  v-inv-date at 62
  inv-head.inv-no at 72 skip(3)
  inv-head.cust-name at 2 v-shipto-name    at 50 skip
  inv-head.addr[1]   at 2 v-shipto-addr[1] at 50 skip
  inv-head.addr[2]   at 2 v-shipto-addr[2] at 50 skip
  v-addr3            at 2 v-sold-addr3     at 50
  skip(4)
  "P.O. NUMBER:" TO 14 v-po-no "SHIP TO P.O.:" AT 50 v-rel-po-no skip
  "ORDER NUMBER & DATE:" TO 22 v-ord-no v-ord-date
  "SALESMAN:" AT 50 v-salesman skip
  "INVOICE NOTE:" TO 15 v-bill-i
  "ORIGINAL ORDER:" AT 50 skip
  "DATE SHIPPED:" TO 15 v-date-ship
  "TERMS:" AT 50 inv-head.terms-d format "x(15)" skip
  "SHIPPING INSTRUCTIONS:" TO 24 v-ship-i /* v-shipvia */ format "x(25)" skip(2)
  "QTY   QTY    QTY" AT 5 "PRICE" AT 63 skip
  "ORD   SHIP   B.O.  ITEM NUMBER      DESCRIPTION" AT 5
  v-price-head AT 63 "EXTENSION" AT 72 skip(1)
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 90.

form
  v-inv-qty TO 7 format ">>>>>9"
  v-ship-qty TO 14 format ">>>>>9"
  v-bo-qty AT 17 format ">>>>>9"
  v-i-no AT 24 format "x(15)"
  v-i-dscr AT 41 format "x(18)"
  v-price AT 61 format ">>>9.9999"
  v-t-price AT 73 format ">>>>9.99"
with frame detail no-attr-space no-labels no-box no-underline down stream-io width 90.

form
  v-part-info at 41
with frame beeler no-labels no-box no-underline down stream-io width 90.

form
  inv-misc.charge at 5 space(2) inv-misc.dscr
  inv-misc.amt to 80 format "->>,>>9.99"
  skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 90.

form
  "Tot Tax:" to 65
  space(0)
  inv-head.t-inv-tax  to 80 format "->>,>>9.99"
with frame tax no-labels no-box no-underline stream-io width 90.

form
  " " to 80
with frame blankl no-labels no-box no-underline stream-io width 90.

form
  disp-frt to 65
  space(0)
  inv-head.t-inv-freight TO 80 format "->,>>9.99"
  skip(1)
  inv-head.t-inv-rev TO 80 format "->,>>>,>>9.99"
with frame totals-comp2 no-labels no-box no-underline stream-io width 90.

    
    find first company where company.company = cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

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

        find carrier where carrier.company = inv-head.company and
          carrier.carrier = inv-head.carrier no-lock no-error.
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
        if not avail stax then
        find first stax where stax.tax-group eq inv-head.tax-gr
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
         do i = 1 to 3:
          if xinv-line.sman[i] ne "" then do:
            create w-sman.
            assign w-sman.sman = xinv-line.sman[i].
          end.
         end.
         assign v-tot-qty = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no AND
             oe-bolh.ord-no = xinv-line.ord-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = xinv-line.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
           END. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         END. /* each oe-bolh */
         if last-of(xinv-line.i-no) then do:
           if xinv-line.est-no ne "" then
           do:
             find first eb where eb.company = xinv-line.company and
               eb.est-no = xinv-line.est-no and
               eb.e-num = xinv-line.e-num and
               eb.form-no = xinv-line.form-no and
               eb.blank-no = xinv-line.blank-no no-lock no-error.

             IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                  AND fg-set.set-no = xinv-line.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
               END.
               IF v-set-qty = 0 THEN
                  ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                  eb.est-no = xinv-line.est-no AND
                  eb.e-num = xinv-line.e-num AND
                  eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = xinv-line.company AND
                    fg-set.set-no = xinv-line.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.QtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
                 ELSE
                   ASSIGN v-part-qty = 1 / v-set-qty.


                IF eb.cas-cnt = 0 THEN
                   ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 ELSE
                   ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 if v-bol-cases ne 0 then
                   assign v-tot-cas = v-bol-cases.
                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
               END. /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN
             DO:
               IF eb.cas-cnt = 0 THEN
                 ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
               ELSE
                 ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
               if v-bol-cases ne 0 then
                 assign v-tot-cas = v-bol-cases.
               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             END. /* do */
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

        find first oe-bolh where oe-bolh.company = inv-head.company and
                                 oe-bolh.bol-no = inv-head.bol-no
                                 USE-INDEX bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.

        find first inv-line where inv-line.r-no = inv-head.r-no
                                  no-lock no-error.
        if avail inv-line then
        do:
          assign v-price-head = inv-line.pr-uom.
          find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then
          do:
            assign v-po-no = oe-ord.po-no
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date.
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.

        view frame invhead-comp.  /* Print headers */

        page.

        for each inv-line no-lock where inv-line.r-no = inv-head.r-no:
          assign v-case-line = ""
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

            assign v-line = v-line + 1
                   v-printline = v-printline + 2.

            v-beeler-lines = 0.
            do v = 1 to 2:
              v-part-info = if v eq 1 then inv-line.part-dscr1
                            else           inv-line.part-dscr2.

              if v-part-info ne "" then v-beeler-lines = v-beeler-lines + 1.
            end.
            v-printline = v-printline + v-beeler-lines.

            if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.

              v-printline = 2 + v-beeler-lines.

              page.

            end.

            find first oe-ordl where oe-ordl.company = cocode and
                                     oe-ordl.ord-no = inv-line.ord-no and
                                     oe-ordl.i-no = inv-line.i-no
                                     no-lock no-error.
            if avail oe-ordl then
              assign v-bo-qty = if (inv-line.qty - inv-line.ship-qty /*- oe-ordl.t-ship-qty*/) < 0 then 0 else
                                   (inv-line.qty - inv-line.ship-qty /*- oe-ordl.t-ship-qty*/ ).
            else
              assign v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
                                  then 0 else inv-line.qty - inv-line.ship-qty.

            assign v-inv-qty = inv-line.qty
                   v-ship-qty = inv-line.ship-qty
                   v-i-no = inv-line.i-no
                   v-i-dscr = inv-line.i-name
                   v-price = inv-line.price * (1 - (inv-line.disc / 100))
                   v-t-price = inv-line.t-price.

            display v-inv-qty AT 3 format ">>>>>9"
                    v-ship-qty AT 10 format ">>>>>9"
                    v-bo-qty AT 17 format ">>>>>9"
                    v-i-no AT 24 format "x(15)"
                    v-i-dscr AT 41 format "x(18)"
                    v-price AT 61 format ">>>9.9999"
                    v-t-price AT 73 format ">>>>9.99"

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

        for each inv-misc no-lock where inv-misc.company = inv-head.company and
          inv-misc.r-no = inv-head.r-no and
          inv-misc.bill = "Y" break by ord-no with frame detailm:
          if first(inv-misc.ord-no) then
          do:
            put "** Miscellaneous Items **" at 23 skip(1).
            assign v-printline = v-printline + 2.
          end.

            if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              assign v-printline = 0.
              page.

            end.

            display inv-misc.charge inv-misc.dscr inv-misc.amt.
            down.

            assign v-line = v-line + 1
               v-printline = v-printline + 2.
        end. /* each inv-misc */

        if v-prntinst then do:
         do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
            if v-printline gt 29 then
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              assign v-printline = 0.
              page.

            end.

            put inv-head.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
         end. /* 1 to 4 */
        end.

        /* T O T A L S */
        put skip(30 - v-printline).

        find first cust where cust.company = cocode and
                   cust.cust-no = inv-head.cust-no no-lock no-error.
        if avail cust and cust.sort = "Y" then
          display inv-head.t-inv-tax with frame tax.

        else display " " skip(1) with frame blankl.

        if inv-head.f-bill then
          display disp-frt
                  inv-head.t-inv-freight
                  inv-head.t-inv-rev
              with frame totals-comp2.
        else
          display "" @ disp-frt
                  "" @ inv-head.t-inv-freight
                  inv-head.t-inv-rev
              with frame totals-comp2.

        assign
         inv-head.printed = yes
         inv-head.stat = "X".
      end. /* DO TRANSACTION avail inv-head */
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
