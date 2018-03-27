/* ---------------------------------------------- oe/rep/reltrist.p 02/98 FWK */
/* print oe Release/Picking tickets when RELPRINT = "TriState"                */
/* -------------------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

def var v-cust-name like cust.name no-undo.
def var v-cust-addr1 like cust.name no-undo.
def var v-cust-addr2 like cust.name no-undo.
def var v-cust-addr3 like cust.name no-undo.
def var v-ship-name like cust.name no-undo.
def var v-ship-addr1 like cust.name no-undo.
def var v-ship-addr2 like cust.name no-undo.
def var v-ship-addr3 like cust.name no-undo.

def var v-trailer like oe-bolh.trailer no-undo.
def var v-frt-pay-dscr as character no-undo.
def var v-qty-uom as character no-undo.
def var v-to-ship as integer no-undo.

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

def stream last-page.


find first company  where company.company  eq cocode no-lock no-error.
find first oe-ctrl  where oe-ctrl.company  eq cocode no-lock no-error.
find first oe-relh  where oe-relh.company  eq cocode no-lock no-error.
find first xoe-rell where xoe-rell.company eq cocode no-lock no-error.

form header
  skip(5)
  "BILL TO:" at 1
  oe-relh.cust-no to 30
  "SHIP TO:" at 41
  oe-relh.ship-id to 70
  v-cust-name at 6
  v-ship-name at 46
  v-cust-addr1 at 6
  v-ship-addr1 at 46
  v-cust-addr2 at 6
  v-ship-addr2 at 46
  v-cust-addr3 at 6
  v-ship-addr3 at 46
  skip(1)
  "OUR ORDER#"
  xoe-rell.ord-no skip
  "REL#"
  trim(string(oe-relh.release#,">>>>>>>>"))
  "TO SHIP VIA" at 13
  v-carrier format "x(30)"
  "DUE ON" at 55
  oe-relh.rel-date at 63
  "CUSTOMER PO#" at 1
  oe-relh.po-no at 13
  /*
  "TRAILER #" at 35
  v-trailer
  */
  v-frt-pay-dscr to 61
  "PAGE" at 63
  page-number - v-last-page to 69 format "99"
  "OF" at 71
  v-page-tot to 75 format "99"
"-------------------------------------------------------------------------------
" at 1
  "LOC" at 1
  "ITEM NUMBER" at 9
  "PRODUCT DESCRIPTION" at 26
  "QUANTITY IN" at 59
  caps(v-qty-uom) at 71 format "x(8)"
  "------" at 1
  "---------------" at 9
  "------------------------------" at 26
  "----------------------" at 58
  "TO SHIP" at 59
  "SHIPPED" at 71
  "---------" at 58
  "--------" at 70
with frame rel-top page-top no-box no-underline STREAM-IO width 85.

format
  locbin[1] at 1 format "x(6)"
  oe-rell.i-no at 9 format "x(15)"
  itemfg.i-name at 26 format "x(30)"
  v-to-ship to 66 format ">>>,>>>"
  "________" at 71
  with down frame relprint no-box no-label STREAM-IO width 80.


    output stream last-page to value(tmp-dir + "reltrist.txt") page-size VALUE(v-lines-per-page).

    view frame rel-top.

    v-last-page = 0.

    {oe/rep/foreachr.i} break by oe-relh.r-no:

      assign
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0.

      find cust
          where cust.company eq cocode
            and cust.cust-no eq oe-relh.cust-no
          no-lock no-error.
      if available cust then
      assign
        v-cust-name = cust.name
        v-cust-addr1 = cust.addr[1]
        v-cust-addr2 = cust.addr[2]
        v-cust-addr3 = cust.city + ", " + cust.state + "  " + cust.zip.
      else
      assign
        v-cust-name = ""
        v-cust-addr1 = ""
        v-cust-addr2 = ""
        v-cust-addr3 = "".

      if trim(v-cust-addr3) = "," then v-cust-addr3 = "".

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      if available shipto then
      assign
        v-ship-name = shipto.ship-name
        v-ship-addr1 = shipto.ship-addr[1]
        v-ship-addr2 = shipto.ship-addr[2]
        v-ship-addr3 = shipto.ship-city + ", " + shipto.ship-state + "  "
          + shipto.ship-zip.
      else
      assign
        v-ship-name = ""
        v-ship-addr1 = ""
        v-ship-addr2 = ""
        v-ship-addr3 = "".

      if trim(v-ship-addr3) = "," then v-ship-addr3 = "".

      find carrier
          where carrier.company eq oe-relh.company
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.

      ASSIGN 
        v-carrier      = if avail carrier then carrier.dscr else ""
        v-frt-pay-dscr = ""
        v-po-no        = "".

      FOR EACH xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq xoe-rell.company
            and oe-ord.ord-no  eq xoe-rell.ord-no
          no-lock,
          FIRST oe-ordl
            where oe-ordl.company EQ oe-ord.company
              and oe-ordl.ord-no  EQ oe-ord.ord-no
            no-lock:

        case oe-ord.frt-pay:
             when "P" THEN v-frt-pay-dscr = "Prepaid".
             when "C" THEN v-frt-pay-dscr = "Collect".
             when "B" THEN v-frt-pay-dscr = "Bill".
             when "T" THEN v-frt-pay-dscr = "Third Party".
        end case.

        v-po-no = oe-ord.po-no.
        
        find first uom where uom.uom EQ oe-ordl.pr-uom no-lock no-error.
        v-qty-uom = if available uom then uom.dscr else oe-ordl.pr-uom.

        LEAVE.
      end.

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
              and oe-rel.po-no    eq oe-relh.po-no
              and oe-rel.i-no     eq xoe-rell.i-no
            no-lock no-error.

        if not avail oe-rel then
          find first oe-rel
              where oe-rel.company  eq xoe-rell.company
                and oe-rel.ord-no   eq xoe-rell.ord-no
                and oe-rel.line     eq xoe-rell.line
                and oe-rel.rel-date eq oe-relh.rel-date
                and oe-rel.ship-no  eq oe-relh.ship-no
                and oe-rel.po-no    eq oe-relh.po-no
                and oe-rel.i-no     eq xoe-rell.i-no
              no-lock no-error.
      end. /* each xoe-rell */

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

      {oe/rep/reltrist.i "stream last-page"}

      v-page-tot = page-number (last-page) - v-last-page.

      {oe/rep/reltrist.i}

      RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
      ASSIGN
        v-last-page     = page-number.
      page.
      page stream last-page.
    end. /* for each oe-relh */

    output stream last-page close.

/* END ------------------------------------ copr. 1991  Protech Services Inc. */
