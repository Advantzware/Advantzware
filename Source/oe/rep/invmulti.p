/* ---------------------------------------------- oe/rep/invmulti.p 02/98 FWK */
/* PRINT INVOICE when sys-ctrl.char-fld eq "MultiWall" - O/E Module           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

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
def var v-invhead as char format "x(13)" init
  "I N V O I C E".
def var v-t-weight like inv-line.t-weight.
def var v-tot-cas as dec format "->>>9.9999".
def var v-tot-pallets as int.
def var v-tot-qty as int.
def var v-inv-date as date init today.
def shared var v-fr-tax as logical init no.
def var v-tax-rate as dec format "->>>.99".
def var v-tax-code like stax.tax-code.
def var v-tx-rate like stax.tax-rate.
def var v-ans as logical init no.
def var v-date-ship as date init today.
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

def buffer xinv-head for inv-head.
def buffer xinv-line for inv-line.

def var v-ord-del-hdr as char format "x(3)" init "Del".
def var v-beeler-lines as int.
def var v-part-info as char format "x(30)".
def var v as int.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-cust-part-no like itemfg.part-no no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-po-nol like inv-line.po-no no-undo.
def var v-bill-i as char format "x(25)" no-undo.
def var v-ord-no like oe-ord.ord-no no-undo.
def var v-ord-date like oe-ord.ord-date no-undo.
def var v-ship-i as char format "x(25)" no-undo.
def var v-rel-po-no like oe-rel.po-no no-undo.
def var v-price-head as char format "x(8)" no-undo.
def var v-price-per as char format "x(8)" no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-frt-display like inv-head.t-inv-freight no-undo.

def workfile w-sman
  field sman as char format "x(4)".

def workfile w-invl
  field po-noh like oe-ord.po-no
  field po-nol like oe-ordl.po-no
  field i-no   like inv-line.i-no
  field rec-id as   recid.

def stream last-page.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

FIND FIRST inv-head NO-LOCK NO-ERROR.

form header
  skip(1)
  inv-head.inv-no at 72
  v-inv-date at 71
 "PAGE" at 66 page-number - v-last-page to 72 format "99"
 "OF" at 74 v-page-tot to 78 format "99" skip(1)
 "BILL TO:" at 1 inv-head.cust-no to 34
 "SHIPPED TO:" at 50 inv-head.sold-no to 74
  inv-head.cust-name at 10 format "x(25)"
  v-shipto-name    at 50 format "x(25)" skip
  inv-head.addr[1]   at 10 format "x(25)"
  v-shipto-addr[1] at 50 format "x(25)" skip
  inv-head.addr[2]   at 10 format "x(25)"
  v-shipto-addr[2] at 50 format "x(25)" skip
  v-addr3            at 10 format "x(25)"
  v-sold-addr3     at 50 format "x(25)" skip(2)
  "ORD#" v-ord-no at 6 /* "CUSTOMER PO" at 13 v-po-no at 25 */
  "TERMS" AT 44 inv-head.terms-d at 50 format "x(30)" skip
  "BOL#" inv-head.bol-no format "999999"
  "SHIPPED VIA" at 13 v-shipvia format "x(30)"
  "ON" at 56 v-date-ship space(2) v-frt-pay-dscr
  tmpstore format "x(80)" skip(1)
  "ITEM NUMBER" AT 1
  "QUANTITY" at 50 "UNIT" at 66 "EXTENDED" at 73
  "P.O. # / P.O. #" at 1
  "PRODUCT DESCRIPTION" at 17 "ORDERED SHIPPED" at 47
  "PRICE" at 65 "PRICE" at 74
  "-------------" at 1 "---------------------" at 17 "B/ORDER" at 47
  "-------" at 55 "--------" at 63 "---------" at 72
  "-------" at 47
with frame invhead-comp page-top no-labels no-box no-underline stream-io width 80.

form
  inv-line.i-no at 1
  v-cust-part-no at 17
  v-cs-qty to 53 format ">>>>>>9"
  v-cs-ship-qty to 61 format ">>>>>>9"
  v-price to 70 format ">>>>9.99" skip
  v-po-no at 1
  inv-line.i-name at 17 format "x(29)"
  v-bo-qty to 53 format ">>>>>>9"
  v-t-price to 80 format ">>>>>9.99" skip
  v-po-nol at 1
  v-i-dscr at 17 format "x(30)" skip(1)
with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form
  inv-misc.charge at 1  format "x(5)"
  inv-misc.dscr at 17
  inv-misc.amt to 80 format "->>,>>9.99"
  skip(1)
with frame detailm no-labels no-box no-underline down stream-io width 80.

form
  "____________" at 69
  "SUB-TOTAL" at 58 v-subtot-lines to 80 format "->>>,>>9.99"
  "SALES TAX" at 58 inv-head.t-inv-tax to 80 format "->>>,>>9.99"
  "FREIGHT" at 58 v-frt-display to 80 format "->>>,>>9.99"
  "MISC" at 58 v-subtot-misc to 80 format "->>>,>>9.99"
  "------------" at 69
  "INVOICE TOTAL" at 55
  inv-head.t-inv-rev TO 80 format "$->>>,>>9.99"
  "============" at 69
with frame total-frame no-labels no-box no-underline down stream-io width 80.

  
    output stream last-page to value("invmulti.txt") page-size VALUE(v-lines-per-page).

    v-last-page = 0.

    for each report where report.term-id eq v-term-id no-lock,
        first xinv-head where recid(xinv-head) eq report.rec-id no-lock
        break by report.key-01
              by report.key-02:

      assign v-subtot-lines = 0
             v-subtot-misc = 0.

      assign  v-shipto-name = xinv-head.sold-name
              v-shipto-addr[1] = xinv-head.sold-addr[1]
              v-shipto-addr[2] = xinv-head.sold-addr[2]
              v-shipto-city = xinv-head.sold-city
              v-shipto-state = xinv-head.sold-state
              v-shipto-zip = xinv-head.sold-zip.

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

        assign
         v-tot-pallets = 0
         v-page-tot = 0
         v-bol-cases = 0.

        for each w-invl.
          delete w-invl.
        end.

        for each xinv-line where xinv-line.r-no eq inv-head.r-no no-lock
            by xinv-line.i-no:

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
              if oe-boll.partial ne 0 then
                assign v-bol-cases = v-bol-cases + 1.
           end. /* each oe-boll */
           assign v-date-ship = oe-bolh.bol-date
                  v-tot-pallets = v-tot-pallets + oe-bolh.tot-pallets.
         end. /* each oe-bolh */

         find first oe-ordl
             where oe-ordl.company eq cocode
               and oe-ordl.ord-no  eq xinv-line.ord-no
               and oe-ordl.i-no    eq xinv-line.i-no
             no-lock no-error.

         find first oe-ord where oe-ord.company eq cocode and
                                  oe-ord.ord-no eq xinv-line.ord-no
                                  no-lock no-error.


         create w-invl.
         assign
/*
          w-invl.po-noh = if avail oe-ord  then oe-ord.po-no  else ""
          w-invl.po-nol = if avail oe-ordl then oe-ordl.po-no else ""
*/
          w-invl.i-no   = xinv-line.i-no
          w-invl.rec-id = recid(xinv-line).

          if avail oe-ord then
          do:
            do i = 1 to length(oe-ord.po-no):
              if substring(oe-ord.po-no,i,1) lt "0" or
                 substring(oe-ord.po-no,i,1) gt "9" then
              do:
                assign w-invl.po-noh = oe-ord.po-no.
                leave.
              end.
              if i = length(oe-ord.po-no) then
               assign w-invl.po-noh =
                        string(int(oe-ord.po-no),"999999999999999").
            end.
          end.
          if avail oe-ordl then
          do:
            do i = 1 to length(oe-ordl.po-no):
              if substring(oe-ordl.po-no,i,1) lt "0" or
                 substring(oe-ordl.po-no,i,1) gt "9" then
              do:
                assign w-invl.po-nol = oe-ordl.po-no.
                leave.
              end.
              if i = length(oe-ordl.po-no) then
                assign w-invl.po-nol =
                        string(int(oe-ordl.po-no),"999999999999999").
            end.
          end.

        end. /* each xinv-line */

                                         /** Build Salesman Id String **/
        find first oe-bolh where oe-bolh.company eq inv-head.company and
                                 oe-bolh.bol-no eq inv-head.bol-no
                                 use-index bol-no no-lock no-error.
        if avail oe-bolh then
          assign v-rel-po-no = oe-bolh.po-no.

        find first inv-line where inv-line.r-no eq inv-head.r-no
                                  no-lock no-error.
        if avail inv-line then do:
          assign v-price-head = inv-line.pr-uom.
          find first oe-ord where oe-ord.company eq cocode and
                                  oe-ord.ord-no eq inv-line.ord-no
                                  no-lock no-error.
          if avail oe-ord then do:
            assign /* v-po-no = oe-ord.po-no */
                   v-bill-i = oe-ord.bill-i[1]
                   v-ord-no = oe-ord.ord-no
                   v-ord-date = oe-ord.ord-date.
          end.
          else
            assign v-price-head = inv-line.pr-uom.
        end.
        find oe-ord where oe-ord.company = oe-bolh.company
          and oe-ord.ord-no = oe-bolh.ord-no no-lock no-error.

        v-po-no = if avail oe-ord then oe-ord.po-no else "".

        if caps(inv-head.frt-pay) eq "P" then
          assign v-frt-pay-dscr = "PREPAID".
        else if caps(inv-head.frt-pay) eq "C" then
          assign v-frt-pay-dscr = "COLLECT".
        else if caps(inv-head.frt-pay) eq "B" then
          assign v-frt-pay-dscr = "PPD/CHG".

        view frame invhead-comp.  /* Print headers */
        view stream last-page frame invhead-comp.  /* Print headers */

        {oe/rep/invmulti.i "stream last-page"}

        v-page-tot = page-number (last-page) - v-last-page.

        assign
         v-subtot-lines = 0
         v-frt-display  = 0
         v-subtot-misc  = 0.

        {oe/rep/invmulti.i}

        v-last-page = page-number.
      END.
    end. /* each xinv-head */

    output stream last-page close.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
