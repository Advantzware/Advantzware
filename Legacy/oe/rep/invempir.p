/* ---------------------------------------------- oe/rep/invempir.p 10/99 FWK */
/* PRINT INVOICE when sys-ctrl.char-fld eq "Empire" - O/E Module              */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.

{oe/rep/invoice.i}

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
def var disp-frt as char init "Freight:" format "x(8)".
def var minus-ship as int.
def var v-subtot-lines as dec format "->>>,>>9.99".
def var v-subtot-misc as dec format "->>>,>>9.99".
def var v-cs-qty like oe-boll.cases no-undo.
def var v-cs-ship-qty like inv-line.ship-qty no-undo.
def var v-space as int initial 0 no-undo.

def buffer inv-head for inv-head.
def buffer inv-line for inv-line.

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
def var v-price-head as char format "x(8)" no-undo.
def var v-price-per as char format "x(8)" no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-frt-display like inv-head.t-inv-freight no-undo.
def var v-sname like sman.sname no-undo.
def var v-discount like inv-head.t-inv-cost no-undo.
def var v-if-paid as date format "99/99/9999" no-undo.

assign tmpstore = fill("-",80).

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
  skip(1)
  inv-head.inv-no to 78 skip(1)

  v-inv-date to 78 skip(1)
  /* "PAGE" at 66 */
  page-number - v-last-page to 72 format "99"
  "OF" at 74 v-page-tot to 78 format "99" skip(4)
  /* "BILL TO:" at 1 inv-head.cust-no to 34 */
  /* "SHIPPED TO:" at 50 inv-head.sold-no to 74 */
  inv-head.cust-name at 10 format "x(30)"
  v-shipto-name    at 49 format "x(30)" skip
  inv-head.addr[1]   at 10 format "x(30)"
  v-shipto-addr[1] at 49 format "x(30)" skip
  inv-head.addr[2]   at 10 format "x(30)"
  v-shipto-addr[2] at 49 format "x(30)" skip
  v-addr3            at 10 format "x(30)"
  v-sold-addr3     at 49 format "x(30)" skip(4) /*4*/
  inv-head.cust-no at 2 v-shipvia format "x(16)" at 11
  inv-head.fob-code at 30 inv-head.terms-d at 58 format "x(21)" skip(3)
  v-po-no at 2 v-sname at 27 v-ord-date at 57 FORM "99/99/99" v-ord-no at 66 skip(3)
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 80.

form
  inv-line.qty at 2 format ">,>>>,>>9"
  inv-line.inv-qty at 12 format ">,>>>,>>9"
  inv-line.part-no at 23
  v-price to 63 format ">,>>>,>>9.99"
  inv-line.pr-uom to 68 format "x(4)"
  v-t-price to 80 format ">>>>>9.99" skip
  v-bo-qty at 12 format ">,>>>,>>9"
  inv-line.i-name at 23 format "x(30)"
  inv-line.disc to 63 format ">9.99"
  inv-line.tax at 68 format "Y/N" skip
  v-i-dscr at 23 format "x(30)"

/*
  inv-line.i-no at 1
  inv-line.i-name at 17
  v-cs-qty to 54 format ">>>>>>9"
  v-cs-ship-qty to 62 format ">>>>>>9"
  v-price to 70 format ">>>9.99" skip
  v-i-dscr at 17 format "x(30)"
  v-bo-qty to 54 format ">>>>>>9"
  v-t-price to 80 format ">>>>>9.99" skip(1)
*/
with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form
  inv-misc.charge at 1  format "x(5)"
  inv-misc.dscr at 17
  inv-misc.amt to 80 format "->>,>>9.99"
  skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 80.

form
  "Subtotal" at 30
  "Freight" at 45
  "Sales Tax" at 55 skip
  v-subtot-lines to 37 format "->>>,>>9.99"
  v-frt-display to 51 format "->>>,>>9.99"
  inv-head.t-inv-tax to 63 format "->>>,>>9.99"
  inv-head.t-inv-rev TO 80 format "$->>>,>>9.99" skip(1)
  "If paid by" at 30 v-if-paid "you may deduct:" v-discount to 80
  skip(1)
with frame total-frame no-labels NO-BOX DOWN no-underline stream-io width 80.


    find first company where company.company eq cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

    v-last-page = 0.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

      FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head) NO-LOCK NO-ERROR.

      assign
       v-subtot-lines = 0
       v-subtot-misc  = 0

       v-shipto-name    = inv-head.sold-name
       v-shipto-addr[1] = inv-head.sold-addr[1]
       v-shipto-addr[2] = inv-head.sold-addr[2]
       v-shipto-city    = inv-head.sold-city
       v-shipto-state   = inv-head.sold-state
       v-shipto-zip     = inv-head.sold-zip.

        v-del-no = 0.

        find first oe-bolh
            where oe-bolh.company eq inv-head.company
              and oe-bolh.bol-no  eq inv-head.bol-no
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

        if inv-head.inv-date ne ? then v-inv-date = inv-head.inv-date.

        find first carrier
            where carrier.company eq inv-head.company
              and carrier.carrier eq inv-head.carrier
             no-lock no-error.
        assign
         v-shipvia     = if avail carrier then carrier.dscr else ""
         v-addr3       = inv-head.city + ", " + inv-head.state + "  " +
                         inv-head.zip
         v-sold-addr3  = v-shipto-city + ", " + v-shipto-state + "  " +
                         v-shipto-zip
         v-line        = 1
         v-printline   = 0
         v-tot-pallets = 0
         v-page-tot    = 0
         v-bol-cases   = 0.

        for each inv-line where inv-line.r-no eq inv-head.r-no no-lock
            break by inv-line.i-no:
          do i = 1 to 3:
            if inv-line.sman[i] ne "" then do:
              create w-sman.
              w-sman.sman = inv-line.sman[i].
            end.
          end.

          assign
           v-tot-qty  = v-tot-qty + inv-line.ship-qty
           v-t-weight = v-t-weight + (round(inv-line.t-weight / inv-line.qty,2)
                                      * inv-line.inv-qty).

          for each oe-bolh
              where oe-bolh.b-no   eq inv-line.b-no
                and oe-bolh.ord-no eq inv-line.ord-no
             no-lock:
           for each oe-boll
               where oe-boll.company eq oe-bolh.company
                 and oe-boll.b-no    eq oe-bolh.b-no
                 and oe-boll.i-no    eq inv-line.i-no
               no-lock:
                                      /** Bill Of Lading TOTAL CASES **/
              v-bol-cases = v-bol-cases + oe-boll.cases.
              if oe-boll.partial ne 0 then v-bol-cases = v-bol-cases + 1.
            end. /* each oe-boll */

            assign
             v-date-ship = oe-bolh.bol-date
             v-tot-pallets = oe-bolh.tot-pallets.
          end. /* each oe-bolh */

          v-page-tot = v-page-tot + 1.
        end. /* each inv-line */

        v-space = 0.
        do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then v-space = v-space + 1.
        end.
        if v-space ne 0 then v-page-tot = v-page-tot + 1.

        v-page-tot = if (v-page-tot / 10) le 1 then 1
                     else round(((v-page-tot / 10) +
                         (if (v-page-tot modulo 10) ne 0 then .5 else 0)),0).

                                         /** Build Salesman Id String **/
        find first oe-bolh
            where oe-bolh.company eq inv-head.company
              and oe-bolh.bol-no  eq inv-head.bol-no
            use-index bol-no no-lock no-error.
        if avail oe-bolh then v-rel-po-no = oe-bolh.po-no.

        find first inv-line where inv-line.r-no eq inv-head.r-no
            no-lock no-error.
        if avail inv-line then do:
          v-price-head = inv-line.pr-uom.
          find first oe-ord
              where oe-ord.company eq cocode
                and oe-ord.ord-no  eq inv-line.ord-no
              no-lock no-error.
          if avail oe-ord then do:
            assign
             v-po-no    = oe-ord.po-no
             v-bill-i   = oe-ord.bill-i[1]
             v-ord-no   = oe-ord.ord-no
             v-ord-date = oe-ord.ord-date
             v-sname    = oe-ord.sname[1].
          end.
          else v-price-head = inv-line.pr-uom.
        end.

        v-frt-pay-dscr = if inv-head.frt-pay eq "P" then "PREPAID" else
                         if inv-head.frt-pay eq "C" then "COLLECT" else
                         if inv-head.frt-pay eq "B" then "PPD/CHG" else "".

        view frame invhead-comp.  /* Print headers */

        page.

        v-printline = 0.

        for each inv-line where inv-line.r-no eq inv-head.r-no no-lock:
          assign
           v-case-line = ""
           v-part-line = ""
           v-case-cnt  = ""
           v-cs-qty    = 0.

          for each oe-boll
              where oe-boll.company eq inv-line.company
                and oe-boll.bol-no  eq inv-head.bol-no
                and oe-boll.i-no    eq inv-line.i-no
              use-index bol-no:
                                       /** Build Case Count Display Lines **/
            if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
              assign
               v-case-line = string(oe-boll.cases) + " @ " +
                             string(oe-boll.qty-case)
               v-cs-qty    = v-cs-qty + oe-boll.cases.
            else v-case-line = "".

            if oe-boll.partial ne 0 then
              assign
               v-part-line = "1" + " @ " + string(oe-boll.partial)
               v-cs-qty    = v-cs-qty + 1.
            else assign v-part-line = "".

            do i = 1 to 5:
              if (80 - length(v-case-cnt[i])) gt length(v-case-line) and
                 v-case-line ne "" then
                assign
                 v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                 v-case-line   = "".
              if (80 - length(v-case-cnt[i])) gt length(v-part-line) and
                 v-part-line ne "" then
              assign
               v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
               v-part-line   = "".
            end. /* 1 to 5 */
          end. /* each oe-boll */

          assign
           v-line      = v-line + 1
           v-printline = v-printline + 3.

          if v-printline gt 28 then do:
            put skip(29 - v-printline) "* CONTINUED *" at 66.
            v-printline = 3.
            page.
          end.

          find first oe-ordl
              where oe-ordl.company eq cocode
                and oe-ordl.ord-no  eq inv-line.ord-no
                and oe-ordl.i-no    eq inv-line.i-no
              no-lock no-error.
          if avail oe-ordl then
            v-bo-qty = if inv-line.qty - inv-line.ship-qty -
                                                  oe-ordl.t-ship-qty lt 0 then 0
                       else (inv-line.qty - inv-line.ship-qty -
                                                  oe-ordl.t-ship-qty).
          else
            v-bo-qty = if inv-line.qty - inv-line.ship-qty lt 0 then 0
                       else (inv-line.qty - inv-line.ship-qty).

          assign
           v-inv-qty      = inv-line.qty
           v-ship-qty     = inv-line.ship-qty
           v-i-no         = inv-line.i-no
           v-i-dscr       = inv-line.part-dscr1
           v-price        = inv-line.price
           v-t-price      = inv-line.t-price
           v-subtot-lines = v-subtot-lines + v-t-price
           v-cs-qty       = inv-line.qty
           v-cs-ship-qty  = inv-line.ship-qty /
                            (if inv-line.pr-uom ne "CS" then 1
                             else if inv-line.cas-cnt eq 0 then 1
                             else inv-line.cas-cnt).

          display inv-line.qty
                  inv-line.inv-qty
                  inv-line.part-no
                  inv-line.pr-uom
                  v-price
                  v-t-price
                  v-bo-qty
                  inv-line.i-name
                  v-i-dscr
                  inv-line.disc
                  inv-line.tax
              with frame detail.
          down with frame detail.
        end. /* each inv-line */

        for each inv-misc
            where inv-misc.company eq inv-head.company
              and inv-misc.r-no    eq inv-head.r-no
              and inv-misc.bill    eq "Y"
            no-lock
            break by ord-no with frame detailm:

          if first(inv-misc.ord-no) then do:
            put "** Miscellaneous Items **" at 23 skip(1).
            v-printline = v-printline + 2.
          end.

          if v-printline gt 28 then do:
            put skip(29 - v-printline) "* CONTINUED *" at 66.
            v-printline = 3.
            page.
          end.

          display inv-misc.charge inv-misc.dscr inv-misc.amt.
          down.

          assign
           v-line        = v-line + 1
           v-printline   = v-printline + 2
           v-subtot-misc = v-subtot-misc + inv-misc.amt.
        end. /* each inv-misc */

        v-space = 0.
        do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then v-space = v-space + 1.
        end.

        if (v-printline + v-space) gt 28 then do:
          put skip(29 - v-printline) "* CONTINUED *" at 66.
          assign v-printline = 3.
          page.
        end.

        do i = 1 to 4:
          if inv-head.bill-i[i] ne "" then do:
            put inv-head.bill-i[i] at 17 skip.
            assign v-printline = v-printline + 1.
          end.
        end.

        

        /* T O T A L S */
      /*  put skip(31 - v-printline).   */
        put skip(28 - v-printline).

        find first inv-line where inv-line.r-no eq inv-head.r-no
            no-lock no-error.
        if avail inv-line then do:
          find first uom where uom.uom eq inv-line.pr-uom no-lock no-error.
          if avail uom then
            assign
             v-price-head = caps(substring(uom.dscr,1,8))
             v-price-per  = caps(substring(uom.dscr,1,8)).
          else
            v-price-per = inv-line.pr-uom.
        end.

      find terms where terms.company = inv-head.company
        and terms.t-code = inv-head.terms
        no-lock no-error.

      if available terms then
        assign
           v-if-paid = terms.disc-days + inv-head.inv-date
           v-discount = inv-head.t-inv-rev * terms.disc-rate / 100.

        v-frt-display = if inv-head.f-bill then inv-head.t-inv-freight else 0.

      display v-subtot-lines inv-head.t-inv-tax
              v-frt-display
              inv-head.t-inv-rev
              v-if-paid v-discount
          with frame total-frame.

      {sys/inc/o810hookPL.i &reprint=v-reprint &force_asn=false}

      v-last-page = page-number.
    end. /* for each report */

/* end ----------------------------------- copr. 1996 Advanced Software, Inc. */
