/* -------------------------------------------------oe/rep/acknowl.p 6/93 rd */
/* ORDER ACKNOLEDGEMENT                                                      */
/* for ORDER STATUS = (R), (U)                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
def var save_id as recid.

{oe/rep/acknowl.i}

def var v-salesman as char format "x(3)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-addr4 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-line as int.
def var v-printline as int.
def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T".
def var v-len as int.
def var v-totord as dec format "->>,>>>,>>9.99".
def var v-totlin as dec format "->>,>>>,>>9.99".
def var v-ans as log init no.
def var lcnt as int init 1.
def var pagebreak as int init 28.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.

  format
    v-line         to  3 format ">>9"
    oe-ordl.i-no   at  5
    oe-ordl.i-name at 22
    oe-ordl.qty    to 66 format "->>,>>>,>>9"
    oe-ordl.price  to 77 format "->>>,>>9.99<<<<" space(0)
    oe-ordl.pr-uom to 80 format "x(3)"
    with frame detail no-labels no-box no-underline down stream-io width 90.

  format
    v-line to 3 format ">>9"
    oe-ordm.charge at  5 space(2)
    oe-ordm.dscr format "x(30)"
    oe-ordm.tax at 65
    oe-ordm.amt to 78 format "->>,>>9.99" skip(1)
    with frame detailm no-labels no-box no-underline down stream-io width 90.

  format
    lcnt format ">9" to 10
    oe-rel.qty          space(3)
    oe-rel.rel-date     FORMAT "99/99/99"
    with frame sched-rel no-labels no-box no-underline down stream-io width 90.

  format
    shipto.ship-name at 18 skip
    shipto.ship-addr at 18 skip
    v-addr4          at 18 skip
    with frame shipto-rel no-labels no-box no-underline down stream-io width 90.


  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  find first company where company.company eq cocode no-lock no-error.

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      if oe-ord.sman[2] eq "" and oe-ord.sman[3] eq "" then
        v-salesman = oe-ord.sman[1].
      else
        v-salesman = oe-ord.sman[1] + oe-ord.sman[2] + oe-ord.sman[3].

      if oe-ord.fob-code eq "ORIG" then
        v-fob = "Origin".
      else
        v-fob = "Destination".

      find carrier
          where carrier.company eq oe-ord.company
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.
      if avail carrier then
        v-shipvia = carrier.dscr.
      else
        v-shipvia = "".

      assign
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.

      if v-print-fmt eq "ILWalker" then
        format header
          oe-ord.ord-no at 61 oe-ord.ord-date at 72 FORMAT "99/99/99" skip
          skip(2)
          v-salesman at 61 skip
          skip(1)
          oe-ord.po-no at 61 skip(2)
          v-fob at 4 oe-ord.terms-d at 32
          v-shipvia at 58 format "x(22)"
          skip(5)
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 format "x(26)" skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 format "x(26)" skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 format "x(26)" skip
          v-addr3          at 10 v-sold-addr3        at 54 format "x(26)" skip
          v-cust-phone   at 10 skip(4)
          with frame ackhead-ilwalk page-top no-labels no-box
                no-underline stream-io width 81.
      else do:
        if not v-print-head       or
           v-print-fmt eq "HOP" then
          format header
          /* v-ackhead at 24 */ oe-ord.ord-no at 61
          oe-ord.ord-date at 72 FORMAT "99/99/99" skip
          /* company.name at 10 */ skip(1)
          /* company.addr[1] at 10 */ v-salesman at 61 skip
          /* company.addr[2] at 10 */ skip(1)
          /* company.city at 10 company.state company.zip */ skip
          oe-ord.po-no at 61 skip(2) v-fob at 4 oe-ord.terms-d at 32
          v-shipvia at 58 format "x(22)"
          skip(5)
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 skip
          v-addr3          at 10 v-sold-addr3        at 54 skip
          v-cust-phone   at 10 skip(4)
          with frame ackhead page-top no-labels no-box no-underline stream-io width 90.
        else
          format header
          "ACKNOWLEDGEMENT" at 62 skip
          "Order No." at 60 "Order Date" at 70 skip
          "---------" at 60 "----------" at 70 skip
          oe-ord.ord-no at 61
          oe-ord.ord-date at 72 FORMAT "99/99/99" skip
          company.name at 10 skip
           company.addr[1] at 10 "Salesman:" at 61 v-salesman at 71 skip
          company.addr[2] at 10 skip
          company.city at 10 company.state company.zip "Customer PO#" at 61 skip
          "------------" at 61 skip
          oe-ord.po-no at 61 skip(2)
          "F.O.B." at 4 "Terms" at 32 "Ship VIA" at 58 skip
          "------" at 4 "-----" at 32 "--------" at 58 skip
          v-fob at 4 oe-ord.terms-d at 32 v-shipvia at 58 format "x(22)" skip(2)
          "Bill To:" at 5 "Sold To:" at 49 skip
          "--------" at 5 "--------" at 49 skip
          oe-ord.cust-name at 10 oe-ord.sold-name    at 54 skip
          oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 skip
          oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 skip
          v-addr3          at 10 v-sold-addr3        at 54 skip
          v-cust-phone   at 10 skip(1)
          "No." at 1 "Item Number" at 5 "Description" at 22
          "Ordered" at 60 "Price Per M" to 80 skip
          FILL("-",80) FORMAT "x(80)"
          with frame ackhead-head page-top no-labels no-box
                no-underline stream-io width 90.
      end.

      if v-print-fmt eq "ILWalker" then view frame ackhead-ilwalk.
      else
      if not v-print-head       or
         v-print-fmt eq "HOP" then
        view frame ackhead.
      else
        view frame ackhead-head.

      page.

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
          no-lock:

        if (v-printline + 3) gt pagebreak then do:
          v-printline = 0.
          page.
        end.

        display v-line
                oe-ordl.i-no
                oe-ordl.i-name
                oe-ordl.qty
                oe-ordl.price
                oe-ordl.pr-uom
            with frame detail no-attr-space.

        v-printline = v-printline + 1.

        if oe-ordl.i-no ne oe-ordl.part-no or
           oe-ordl.part-dscr1 ne ""        then do:
          v-part = if oe-ordl.i-no ne oe-ordl.part-no then oe-ordl.part-no
                   else "".
          put v-part             at 5
              oe-ordl.part-dscr1 at 22 skip.
          v-printline = v-printline + 1.
        end.

        if oe-ordl.part-dscr2 ne "" then do:
          put oe-ordl.part-dscr2 at 22 skip.
          v-printline = v-printline + 1.
        end.

        FOR EACH oe-rel
            WHERE oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND ((oe-rel.link-no EQ 0 AND v-schrel)
               OR  (oe-rel.link-no NE 0 AND v-actrel))
            NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

          if first-of(oe-rel.link-no) then
            if oe-rel.link-no eq 0 then lcnt = 1.
            else
            if first(oe-rel.link-no) then lcnt = 1.

          if (v-printline ge pagebreak) or
             (v-printline ge pagebreak - 1 and lcnt eq 1) then do:
            v-printline = 0.
            page.
          end.

          if first-of(oe-rel.link-no) then do:
            if oe-rel.link-no eq 0 then do:
              put "Scheduled Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
            else
            if first(oe-rel.link-no) then do:
              put "Actual Releases:" at 10 skip.
              v-printline = v-printline + 1.
            end.
          end.

          {oe/rel-stat.i lv-stat}
          IF AVAIL oe-rell THEN
          FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

          display lcnt
                  (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) @ oe-rel.qty
                  oe-rel.rel-date
                    oe-relh.rel-date WHEN AVAIL oe-relh @ oe-rel.rel-date.
          down with frame sched-rel.
          assign
           v-printline = v-printline + 1
           lcnt        = lcnt + 1.
          if v-shipto then do:
            find first shipto
                where shipto.company eq cocode
                  and shipto.cust-no eq oe-rel.cust-no
                  and shipto.ship-id eq oe-rel.ship-id
                no-lock no-error.
            if avail shipto then
              v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.

            if v-printline + 4 ge pagebreak then do:
              v-printline = 0.
              page.
            end.

            display shipto.ship-name shipto.ship-addr v-addr4
                with frame shipto-rel.
            v-printline = v-printline + 4.
          end.
        end.   /* for each oe-rel  */

        put "" skip.
        assign
         v-line = v-line + 1
         v-printline = v-printline + 1.

        if v-printline ge pagebreak then do:
          v-printline = 0.
          page.
        end.

        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
           assign v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

          v-totlin = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
           v-totlin = IF ll-calc-disc-first THEN 
                        (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                      ELSE
                        ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.

        if v-printline ge pagebreak then
        do:
          assign v-printline = 0.
          page.
        end.
        else
        down with frame detail.
      end. /* each oe-ordl */
      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:
        if first(oe-ordm.ord-no) then
        do:
          put "** Miscellaneous Items **" at 23.
          if v-print-fmt eq "HOP" then put "Taxable" at 62.
          put skip(1).
          assign v-printline = v-printline + 2.
        end.

        if v-printline ge pagebreak then
        do:
          assign v-printline = 0.
          page.
        end.
        else
        down with frame detailm.
        if oe-ordm.bill eq "N" then
        display
          v-line oe-ordm.charge oe-ordm.dscr "     N/C" @ oe-ordm.amt
          with frame detailm.
        else
        display
          v-line oe-ordm.charge oe-ordm.dscr
          oe-ordm.tax when v-print-fmt eq "HOP"
          oe-ordm.amt
          with frame detailm.
        assign v-line = v-line + 1
          v-printline = v-printline + 2.
        if oe-ordm.bill ne "N" then
        assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
      if v-prntinst then
      do:
        do i = 1 TO 4:
          if oe-ord.bill-i[i] ne "" then
          do:
            if v-printline ge pagebreak then
            do:
              assign v-printline = 0.
              page.
            end.

            put oe-ord.bill-i[i] at 5 skip.
            assign v-printline = v-printline + 1.
          end.
        end. /* 1 to 4 */
      end.

      if v-print-fmt eq "HOP" then do:
        put "Total Order Value:" to 65 v-totord skip.
        if oe-ord.tax gt 0 then do:
          run ar/cctaxrt.p (input cocode, oe-ord.tax-gr,
                            output v-tax-rate, output v-frt-tax-rate).
          put "Tax " + string(v-tax-rate,">9.99%") + ":" format "x(11)" to 65
              oe-ord.tax format "->>,>>>,>>9.99" skip
              "Total w/Tax:" to 65
              v-totord + oe-ord.tax format "->>,>>>,>>9.99" skip.
        end.
      end.

      else
      if oe-ctrl.p-ack then do:
        put "Total Order Value:" to 65 v-totord skip.
      end.

      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.
      page.
      assign v-printline = 0.
    end. /* each oe-ord */

