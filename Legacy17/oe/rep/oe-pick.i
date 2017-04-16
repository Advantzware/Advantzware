/* ----------------------------------------------- oe/rep/oe-pick.i 03/98 JLF */
/* Print OE Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def var v-units-hdr as char format "x(5)" extent 2.
def var v-zone-hdr as char format "x(10)".
def var v-zone like shipto.dest-code.
def var v-part-dscr like oe-ordl.i-name.
def var v-qty like oe-rell.qty.
def var v-frt-pay-dscr as char format "x(11)" no-undo.

DEF TEMP-TABLE tt-report NO-UNDO LIKE report.

format
  oe-rell.ord-no
  correct-po at 12
  oe-rell.i-no at 28
  oe-rell.cases format ">>>>>9" to 48
  oe-ordl.qty format "->>>>>>>9" to 58
  oe-rell.qty format "->>>>>>>9" to 68
  oe-rell.qty-case to 77 skip
  with down frame ln-s-comp no-box no-labels STREAM-IO width 85.

format
  oe-rell.ord-no
  oe-rell.po-no at 12
  locbin[1] at 28
  oe-rell.i-no at 34
  oe-rell.cases format ">>>>>9" to 54
  oe-ordl.qty format "->>>>>>>9" to 64
  oe-rell.qty format "->>>>>>>9" to 74
  locbin[2] at 28
  oe-ordl.i-name at 34 format "x(30)" skip
  locbin[3] at 28
  oe-ordl.part-dscr1 at 34 format "x(30)" skip
  locbin[4] at 28
  oe-ordl.part-dscr2 at 34 format "x(30)"
  with down frame relprint no-box no-label STREAM-IO width 85.

format
  oe-rell.ord-no
  oe-rell.po-no at 12
  oe-rell.i-no at 28
  oe-ordl.qty format "->>>>>>>9" to 58
  oe-rell.qty format "->>>>>>>9" to 68
  with down frame ln-s no-box no-labels STREAM-IO width 85.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

if lookup(v-relprint,"Argrov,Sonoco") gt 0 then
  assign
   v-units-hdr[1] = "Units"
   v-units-hdr[2] = "-----".

if v-zone-p then v-zone-hdr = "Route No.:".

    {oe/rep/foreachr.i}:
      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).
      IF AVAIL shipto THEN DO:
        CREATE tt-report.
        ASSIGN
         tt-report.key-01 = STRING({1})
         tt-report.rec-id = RECID(oe-relh).
      END.
    END.

    FOR EACH tt-report WHERE tt-report.term-id EQ "",
        FIRST oe-relh WHERE RECID(oe-relh) EQ tt-report.rec-id,
        first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-relh.cust-no
        no-lock

        break by tt-report.key-01 by oe-relh.release#:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-zone    = if AVAIL shipto AND v-zone-p then shipto.dest-code else "".

      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.
      
      assign
       v-carrier   = if avail carrier then carrier.dscr else ""
       v-frt-terms = "".

      FOR EACH xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq xoe-rell.company
            and oe-ord.ord-no  eq xoe-rell.ord-no
          no-lock:

        case oe-ord.frt-pay:
             when "P" THEN v-frt-terms = "Prepaid".
             when "C" THEN v-frt-terms = "Collect".
             when "B" THEN v-frt-terms = "Bill".
             when "T" THEN v-frt-terms = "Third Party".
        end case.

        LEAVE.
      END.

      /** Calculate the total weight of the released items. **/
      for each xoe-rell
          where xoe-rell.company eq cocode
            and xoe-rell.r-no    eq oe-relh.r-no:
        find first xoe-ordl
            where xoe-ordl.company eq cocode
              and xoe-ordl.ord-no  eq xoe-rell.ord-no
              and xoe-ordl.line    eq xoe-rell.line
              and xoe-ordl.i-no    eq xoe-rell.i-no
            use-index ord-no no-lock no-error.
        if avail xoe-ordl then
          assign
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight = v-weight + (if xoe-ordl.t-weight ne ? then
                                  (round(xoe-ordl.t-weight /
                                   xoe-ordl.qty, 2) * xoe-rell.qty) else 0).

        if avail xoe-ordl and xoe-ordl.est-no ne "" then do:
          find first eb
              where eb.company  eq xoe-ordl.company
                and eb.est-no   eq xoe-ordl.est-no
                and eb.form-no  eq xoe-ordl.form-no
                and eb.blank-no eq xoe-ordl.blank-no
              no-lock no-error.

          if xoe-ordl.form-no eq 0                             and
             (xoe-ordl.est-type eq 2 or xoe-ordl.est-type eq 6) then do:
            for each fg-set
                where fg-set.company eq xoe-ordl.company
                  and fg-set.set-no  eq xoe-ordl.i-no
                no-lock:
              v-set-qty = v-set-qty + fg-set.part-qty.
            end.
            if v-set-qty eq 0 then v-set-qty = 1.
            for each eb
                where eb.company eq xoe-ordl.company
                  and eb.est-no  eq xoe-ordl.est-no
                  and eb.form-no ne 0
                no-lock:
              find fg-set
                  where fg-set.company eq xoe-ordl.company
                    and fg-set.set-no  eq xoe-ordl.i-no
                    and fg-set.part-no eq eb.stock-no
                  no-lock no-error.

              assign
               v-part-qty = (if avail fg-set and fg-set.part-qty ne 0 then
                             fg-set.part-qty else 1) / v-set-qty
               v-pallets = v-pallets +
                           (if xoe-rell.qty-case ne 0 then
                              round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                            else
                            if eb.cas-cnt ne 0 then
                              round((round((v-tot-qty * v-part-qty) /
                                         eb.cas-cnt, 2) / eb.cas-pal) + .49, 0)
                            else
                              round((round((v-weight * v-part-qty) /
                                         eb.cas-wt, 2) / eb.cas-pal) + .49, 0)).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then do:
            assign
             v-pallets = v-pallets +
                         (if xoe-rell.qty-case ne 0 then
                            round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                          else
                          if eb.cas-cnt ne 0 then
                            round((round(v-tot-qty / eb.cas-cnt, 2) /
                                                       eb.cas-pal) + .49, 0)
                          else
                            round((round(v-weight / eb.cas-wt, 2) /
                                                       eb.cas-pal) + .49, 0)).
          end. /* do */
        end. /* est-no ne "" */
      end. /* each xoe-rell */
      v-frt-pay-dscr = v-frt-terms.
      if first(tt-report.key-01) then do:
        form header
        skip(1)
        rel-pack to 75
        "Date      Number" at 62  skip
        v-ticket-date at 60 FORM "99/99/99"
        trim(string(oe-relh.release#,">>>>>>>>")) at 70
        skip(1)
        v-comp-name skip
        v-add-line[1] skip
        v-add-line[2] skip
        v-city v-state v-zip skip
        skip(2)
        "SOLD To:" at 11 "SHIP To:" at 50 skip
        cust.name    at 11 shipto.ship-name    at 50
        cust.addr[1] at 11 shipto.ship-addr[1] at 50
        cust.addr[2] at 11 shipto.ship-addr[2] at 50
        cust.city    at 11 cust.state cust.zip FORM "x(5)"
        shipto.ship-city    at 50 shipto.ship-state shipto.ship-zip FORM "x(5)"
        skip(1) v-zone-hdr v-zone skip(1)
        "Ship Date" at 65 skip
        oe-relh.ship-i[1] format "x(60)"  skip
        oe-relh.ship-i[2] format "x(60)" oe-relh.rel-date at 65 skip
        oe-relh.ship-i[3] format "x(60)"  skip
        "--------------------------------------------------------------------------------" skip
        "Ship Via"
        "F.O.B."        to 32
        "Weight"        at 34
        "Pallets"       at 45
        "Freight Terms" at 56
        skip
        v-carrier
        "/"
        oe-ord.fob-code to 32
        v-weight        at 34
        v-pallets       to 49
        v-frt-pay-dscr  at 56
        "-------------------------------------------------------------------------------" skip
        skip(1)
        "Order No" "PO No" at 12 "BinNo" at 28 "Item / Descrip" at 34
        v-units-hdr[1] to 54 "Order Qty" at 56 "Rel Qty" at 68 skip
        "--------" "-----" at 12 "-----" at 28 "--------------" at 34
        v-units-hdr[2] to 54 "---------" at 56 "-------" at 68 skip
        with frame hd-rel-comp no-box no-labels width 85 PAGE-TOP STREAM-IO.

        form header
        skip(1)
        rel-pack to 75
        "Date      Number" at 62  skip
        v-ticket-date at 60 FORM "99/99/99"
        trim(string(oe-relh.release#,">>>>>>>>")) at 70
        skip(1)
        v-comp-name skip
        v-add-line[1] skip
        v-add-line[2] skip
        v-city v-state v-zip skip
        skip(2)
        "SOLD To:" at 11 "SHIP To:" at 50 skip
        cust.name    at 11 shipto.ship-name    at 50
        cust.addr[1] at 11 shipto.ship-addr[1] at 50
        cust.addr[2] at 11 shipto.ship-addr[2] at 50
        cust.city    at 11 cust.state cust.zip
        shipto.ship-city    at 50 shipto.ship-state shipto.ship-zip FORM "x(5)"
        skip(1) v-zone-hdr v-zone skip(1)
        "Ship Date" at 65 skip
        oe-relh.ship-i[1] format "x(60)"  skip
        oe-relh.ship-i[2] format "x(60)" oe-relh.rel-date at 65 skip
        oe-relh.ship-i[3] format "x(60)"  skip
        oe-relh.ship-i[4] format "x(55)" skip
        "--------------------------------------------------------------------------------" skip
        "Ship Via"
        "F.O.B."        to 32
        "Weight"        at 34
        "Pallets"       at 45
        "Freight Terms" at 56
        skip
        v-carrier
        "/"
        oe-ord.fob-code to 32
        v-weight        at 34
        v-pallets       to 49
        v-frt-pay-dscr  at 56
        "-------------------------------------------------------------------------------" skip
        skip(1)
        "Order No" "PO No" at 12 "Item / Descrip" at 28 v-units-hdr[1] to 48
        "Order Qty" at 50 "Rel Qty" at 62 "Packing" at 71 skip
        "--------" "-----" at 12 "--------------" at 28 v-units-hdr[2] to 48
        "---------" at 50 "-------" at 62 "-------" at 71 skip
        with frame hd-top-comp no-box no-labels width 85 PAGE-TOP STREAM-IO.

        form header
        skip(3)
        v-ticket-date at 60
        trim(string(oe-relh.release#,">>>>>>>>")) AT 70
        skip(7)
        cust.name    at 11 shipto.ship-name    at 50
        cust.addr[1] at 11 shipto.ship-addr[1] at 50
        cust.addr[2] at 11 shipto.ship-addr[2] at 50
        cust.city    at 11 cust.state cust.zip
        shipto.ship-city  at 50 shipto.ship-state shipto.ship-zip   skip(3)
        oe-relh.ship-i[1] format "x(55)"  skip
        oe-relh.ship-i[2] format "x(55)" oe-relh.rel-date skip
        oe-relh.ship-i[3] format "x(55)"  skip
        oe-relh.ship-i[4] format "x(55)" skip
        v-carrier
        "/"
        oe-ord.fob-code to 32
        v-weight        at 34
        v-pallets       to 49
        v-frt-pay-dscr  at 56
        skip(2)
        with frame hd-top no-box no-labels width 85 PAGE-TOP STREAM-IO.

      end.

      if v-headers /* Print headers */ then do:
        if oe-ctrl.pr-broker and avail cust and shipto.broker then do:
          assign
           v-comp-name = cust.name
           v-add-line[1] = cust.addr[1]
           v-add-line[2] = cust.addr[2]
           v-city = cust.city
           v-state = cust.state
           v-zip = cust.zip.
/*        if sys-ctrl.name eq "RELPRINT" then */
            view frame hd-rel-comp.
/*        else
            view frame hd-top-comp. */
        end.
        else do:
          assign
           v-comp-name = company.name
           v-add-line[1] = company.addr[1]
           v-add-line[2] = company.addr[2]
           v-city = company.city
           v-state = company.state
           v-zip = company.zip.
          if v-p-bin eq yes and v-relprint ne "Sonoco" then
            view frame hd-rel-comp.
          else
            view frame hd-top-comp.
        end.
      end.
      else
        view frame hd-top.

      for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no:

        find first oe-rel
            where oe-rel.company  eq cocode
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.link-no  eq oe-rell.r-no
              and oe-rel.ship-no  eq oe-relh.ship-no
              and
              if v-relprint = "sonoco" then true else
                oe-rel.po-no eq oe-relh.po-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then
        find first oe-rel
            where oe-rel.company  eq cocode
              and oe-rel.ord-no   eq oe-rell.ord-no
              and oe-rel.line     eq oe-rell.line
              and oe-rel.rel-date eq oe-relh.rel-date
              and oe-rel.ship-no  eq oe-relh.ship-no
              and
              if v-relprint = "sonoco" then true else
                oe-rel.po-no    eq oe-relh.po-no
              and oe-rel.i-no     eq oe-rell.i-no
            no-lock no-error.

        correct-po = if avail oe-rel and oe-rel.po-no ne "" then oe-rel.po-no
                     else oe-relh.po-no.

        find first oe-ordl
            where oe-ordl.company eq cocode
              and oe-ordl.ord-no  eq oe-rell.ord-no
              and oe-ordl.i-no    eq oe-rell.i-no
              and oe-ordl.line    eq oe-rell.line
            no-lock no-error.

        if v-headers then do:
          find itemfg of oe-rell no-lock no-error.
          do xx = 1 to 10:
             locbin[xx] = "".
          end.
          if v-p-bin eq yes and v-relprint ne "Sonoco" then do:
             if avail itemfg then do:
                xx = 0.
                for each fg-bin
                    where fg-bin.company eq cocode
                      and fg-bin.i-no    eq itemfg.i-no
                      and if v-relprint = "Argrov" and fg-bin.qty > 0 then yes
                      else yes
                   no-lock break by fg-bin.loc-bin:
                   if first-of(fg-bin.loc-bin) then do:
                      xx = xx + 1.
                      locbin[xx] = fg-bin.loc-bin.
                   end.
                end.
             end.
             display
              oe-rell.ord-no
              oe-rell.po-no
              locbin[1]
              locbin[2]
              locbin[3]
              locbin[4]  when xx ge 4
              oe-rell.i-no
              oe-rell.cases    when v-units-hdr[1] ne ""
              oe-ordl.qty      when avail oe-ordl
              oe-rell.qty
              oe-ordl.i-name   when avail oe-ordl
              oe-ordl.part-dscr1 when avail oe-ordl   /* CTS */
              oe-ordl.part-dscr2 when avail oe-ordl /*bsm*/
              with frame relprint.
             down with frame relprint.
          end.
          else do:
            display
              oe-rell.ord-no
              oe-rell.po-no
              oe-rell.i-no
              oe-rell.cases    when v-units-hdr[1] ne ""
              oe-ordl.qty      when avail oe-ordl
              oe-rell.qty
              oe-rell.qty-case
              with frame ln-s-comp.
            down with frame ln-s-comp.

            if avail oe-ordl then
            do i = 1 to 3:
              v-part-dscr = if i eq 1 then oe-ordl.i-name
                            else
                            if i eq 2 then oe-ordl.part-dscr1
                            else           oe-ordl.part-dscr2.
              if v-part-dscr ne "" then put v-part-dscr at 28 skip.
            end.
          end.
        end.
        else do:
          display
            oe-rell.ord-no
            oe-rell.po-no
            oe-rell.i-no
            oe-ordl.qty      when avail oe-ordl
            oe-rell.qty
            with frame ln-s.
          down with frame ln-s.

          if avail oe-ordl then
          do i = 1 to 3:
            v-part-dscr = if i eq 1 then oe-ordl.i-name
                          else
                          if i eq 2 then oe-ordl.part-dscr1
                          else           oe-ordl.part-dscr2.
            if v-part-dscr ne "" then put v-part-dscr at 28 skip.
          end.
        end.

        if v-relprint eq "Sonoco" then do:
          v-qty = oe-rell.qty.
          if oe-rell.tag = "" then
          for each fg-bin
              where fg-bin.company eq cocode
                and fg-bin.i-no    eq oe-rell.i-no
                and fg-bin.tag     gt ""
                and fg-bin.qty     gt 0
              no-lock break by fg-bin.tag:

            if first(fg-bin.tag) then put skip(1) "TAG/WHS/BIN:" at 15.

            put fg-bin.tag                                  at 28
                fg-bin.loc                                  at 37
                fg-bin.loc-bin                              at 43
                min(fg-bin.qty,v-qty) format "->>>>>>>9"    to 68   skip.

            v-qty = v-qty - fg-bin.qty.
            if v-qty le 0 then leave.
          end.
          else
          put skip(1) "TAG/WHS/BIN:" at 15
            oe-rell.tag at 28
            oe-rell.loc at 37
            oe-rell.loc-bin at 43
            oe-rell.qty format "->>>>>>>9" at 68 skip.

        end.

        put skip(1).

        if v-relprint ne "Sonoco" and avail oe-rel then do:
          if oe-rel.ship-i[1] ne "" then
            put oe-rel.ship-i[1] format "x(60)" at /* 28 */ 2 skip.
          if oe-rel.ship-i[2] ne "" then
            put oe-rel.ship-i[2] format "x(60)" at /* 28 */ 2 skip.
          if oe-rel.ship-i[3] ne "" then
            put oe-rel.ship-i[3] format "x(60)" at /* 28 */ 2 skip.
          if oe-rel.ship-i[4] ne "" then
            put oe-rel.ship-i[4] format "x(60)" at /* 28 */ 2 skip.
          put skip(1).
        end.

        oe-rell.printed = true.
      end. /* for each oe-rell */

      if v-relprint eq "Sonoco" and avail oe-rel then do:
        if oe-rel.ship-i[1] ne "" then
          put oe-rel.ship-i[1] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[2] ne "" then
          put oe-rel.ship-i[2] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[3] ne "" then
          put oe-rel.ship-i[3] format "x(60)" at /* 28 */ 2 skip.
        if oe-rel.ship-i[4] ne "" then
          put oe-rel.ship-i[4] format "x(60)" at /* 28 */ 2 skip.
        put skip(1).
      end.

      oe-relh.printed = true.
      page.
    end. /* for each oe-relh */
