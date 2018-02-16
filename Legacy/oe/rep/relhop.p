/* ------------------------------------------------ oe/rep/relhop.p 05/00 DJK */
/* print oe Release/Picking tickets when RELPRINT = "HOP"                     */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def stream last-page.
DEF VAR v-ord-no LIKE oe-rell.ord-no NO-UNDO.

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

IF tmp-dir = "" THEN tmp-dir = "c:\tmp\".

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

format
  oe-rell.i-no at 1 itemfg.part-no at 17 format "x(15)" skip
  locbin[1] at 1 oe-ordl.i-name at 17 format "x(30)" skip
  locbin[2] at 1 oe-ordl.part-dscr1 at 17 format "x(30)"
  itemfg.q-onh format "->,>>>,>>9" to 57
  oe-rell.qty  format "->,>>>,>>9" to 67
  "________" to 80 skip(1)
  with down frame relprint no-box no-label STREAM-IO width 80.


    output stream last-page to value(tmp-dir + "relhop.txt") page-size VALUE(v-lines-per-page).

    v-last-page = 0.

    {oe/rep/foreachr.i}
        break by oe-relh.release#:

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-relh.cust-no
          no-lock no-error.

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).
      
      /* task 02240607 - begin */
      ASSIGN
        v-carrier = ''
        v-frt-terms = ''.
      FOR EACH xoe-rell WHERE xoe-rell.company EQ cocode
                          AND xoe-rell.r-no EQ oe-relh.r-no USE-INDEX r-no:
        FIND FIRST carrier NO-LOCK
             WHERE carrier.company EQ oe-relh.company
               AND carrier.loc EQ xoe-rell.loc
               AND carrier.carrier EQ oe-relh.carrier NO-ERROR.
        IF NOT AVAILABLE carrier THEN NEXT.
        v-carrier = carrier.dscr.
        LEAVE.
      END. /* each xoe-rell */
      /* find carrier
          where carrier.company eq oe-relh.company
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.
      assign
       v-carrier   = if avail carrier then carrier.dscr else ""
       v-frt-terms = "".
        task 02240607 - end */

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
        v-ord-no = xoe-rell.ord-no.
        LEAVE.
      END.
        
      /** Calculate the total weight of the released items. **/
      for each xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no:
        find first xoe-ordl
            where xoe-ordl.company eq xoe-rell.company
              and xoe-ordl.ord-no  eq xoe-rell.ord-no
              and xoe-ordl.line    eq xoe-rell.line
              and xoe-ordl.i-no    eq xoe-rell.i-no
            use-index ord-no no-lock no-error.
        if avail xoe-ordl then
          assign
           v-tot-qty = v-tot-qty + xoe-rell.qty
           v-weight  = v-weight + (if xoe-ordl.t-weight ne ? then
              (round(xoe-ordl.t-weight / xoe-ordl.qty, 2) * xoe-rell.qty)
                else 0).

        if avail xoe-ordl and xoe-ordl.est-no ne "" then do:
          find first eb
              where eb.company  eq xoe-ordl.company
                and eb.est-no   eq xoe-ordl.est-no
                and eb.form-no  eq xoe-ordl.form-no
                and eb.blank-no eq xoe-ordl.blank-no
              no-lock no-error.

          if xoe-ordl.form-no eq 0                              and
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

              if avail fg-set and fg-set.part-qty ne 0 then
                v-part-qty = fg-set.part-qty / v-set-qty.
              else
                v-part-qty = 1 / v-set-qty.

              v-pallets = v-pallets +
                (if xoe-rell.qty-case ne 0 then
                round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
                else if eb.cas-cnt ne 0 then
                round( (round((v-tot-qty * v-part-qty) / eb.cas-cnt, 2) /
                eb.cas-pal) + .49, 0)
                else
                round( (round((v-weight * v-part-qty) / eb.cas-wt, 2) /
                eb.cas-pal) + .49, 0)
                ).
            end. /* each eb */
          end. /* do */
          else
          if avail eb then
            v-pallets = v-pallets +
              (if xoe-rell.qty-case ne 0 then
              round((xoe-rell.qty-case / eb.cas-pal) + .49, 0)
              else if eb.cas-cnt ne 0 then
              round( (round(v-tot-qty / eb.cas-cnt, 2) / eb.cas-pal)
              + .49, 0)
              else
              round( (round(v-weight / eb.cas-wt, 2) / eb.cas-pal)
              + .49, 0)
              ).
        end. /* est-no ne "" */

        find first oe-rel
            where oe-rel.company  eq xoe-rell.company
              and oe-rel.ord-no   eq xoe-rell.ord-no
              and oe-rel.line     eq xoe-rell.line
              and oe-rel.link-no  eq xoe-rell.r-no
              and oe-rel.ship-no  eq oe-relh.ship-no
              and oe-rel.po-no    eq xoe-rell.po-no
              and oe-rel.i-no     eq xoe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then
          find first oe-rel
              where oe-rel.company  eq xoe-rell.company
                and oe-rel.ord-no   eq xoe-rell.ord-no
                and oe-rel.line     eq xoe-rell.line
                and oe-rel.rel-date eq oe-relh.rel-date
                and oe-rel.ship-no  eq oe-relh.ship-no
                and oe-rel.po-no    eq xoe-rell.po-no
                and oe-rel.i-no     eq xoe-rell.i-no
              no-lock no-error.
      end. /* each xoe-rell */

      if first-of(oe-relh.release#) then
        form header
         skip(1)
        rel-pack to 75
        "Date      Number" at 62  skip
        v-ticket-date at 60
        trim(string(oe-relh.release#,">>>>>>>>")) at 70
        skip(7)
        "SOLD TO:" at 11 "SHIP TO:" at 50 skip
        cust.name    at 11 shipto.ship-name    at 50
        cust.addr[1] at 11 shipto.ship-addr[1] at 50
        cust.addr[2] at 11 shipto.ship-addr[2] at 50
        cust.city    at 11 cust.state cust.zip
        shipto.ship-city at 50 shipto.ship-state shipto.ship-zip   skip(1)
        "--------------------------------------------------------------------------------" skip
        "ORDER#:" at 1 v-ord-no "PAY TERMS:" at 16                         
        oe-ord.terms-d format "x(18)" at 28                                           
        "DUE ON:" at 47 oe-relh.rel-date skip                                       
        "CUST PO#:" at 1 correct-po "Page" at 47                                
        page-number - v-last-page  "OF" v-page-tot skip
        "Ship Via:" at 1 v-carrier "F.O.B.:" oe-ord.fob-code " FREIGHT TERMS:" v-frt-terms
        "--------------------------------------------------------------------------------" skip
        skip(1)
        "Item" at 1 "On-Hand" to 57 "Rel" to 67
        "BinNo" at 1 "Description" at 17 "Qty" to 57 "Qty" to 67
        "Shipped" to 80
        "-----" at 1 "------------------------------" at 17 "---------" to 57
        "---------" to 67 "--------" to 80
        with frame hd-rel-comp no-box no-labels STREAM-IO width 80 page-top.

      if oe-ctrl.pr-broker and avail(cust) and shipto.broker then do:
        assign
         v-comp-name = cust.name
         v-add-line[1] = cust.addr[1]
         v-add-line[2] = cust.addr[2]
         v-city = cust.city
         v-state = cust.state
         v-zip = cust.zip.

        view frame hd-rel-comp.
        view stream last-page frame hd-rel-comp.
        
      end.

      else do:
        assign
         v-comp-name = company.name
         v-add-line[1] = company.addr[1]
         v-add-line[2] = company.addr[2]
         v-city = company.city
         v-state = company.state
         v-zip = company.zip.

        view frame hd-rel-comp.
        view stream last-page frame hd-rel-comp.
      end.

      {oe/rep/relhop.i "stream last-page"}

      v-page-tot = page-number (last-page) - v-last-page.

      {oe/rep/relhop.i}
      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
      ASSIGN
       v-last-page     = page-number.
      page.
      page stream last-page.
    end. /* for each oe-relh */

    output stream last-page close.

/* END ------------------------------------ copr. 1991  Protech Services Inc. */

