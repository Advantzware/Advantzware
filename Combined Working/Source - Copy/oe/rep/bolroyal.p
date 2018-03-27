/* --------------------------------------------- oe/rep/bolroyal.p  08/97 FWK */
/* PRINT Royal BOL                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.
def var v-coname like cust.NAME no-undo.
def var v-lines as int no-undo.
def var v-shiplines as int no-undo.
def var v-qty-uom like oe-ordl.pr-uom no-undo.
def var v-to-ship like oe-boll.qty no-undo.
def var v-pallets like oe-bolh.tot-pallets no-undo.
def var v-partial like oe-boll.qty no-undo.
def var v-ord-no like oe-boll.ord-no no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.

find first oe-bolh no-lock no-error.
find first cust no-lock no-error.
find first shipto no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(5)
       "BILL TO:" at 1 cust.cust-no to 30
       "SHIP TO:" at 41 shipto.ship-id to 70
       cust.name at 6
       v-ship-name  at 46
       cust.addr[1] at 6
       v-ship-addr[1] at 46
       cust.addr[2] at 6
       v-ship-addr[2] at 46
       cust.city at 6 cust.state cust.zip
       v-ship-city at 46
       v-ship-state
       v-ship-zip
       skip(1)
       "RPI ORDER#" v-ord-no skip
       "BOL#" oe-bolh.bol-no format "999999"
       "TO SHIP VIA" at 13 v-carrier format "x(30)"
       "DUE ON" at 55 oe-bolh.bol-date at 63 skip
       "CUSTOMER PO#" at 1 oe-ord.po-no at 13
       "TRAILER #" at 35 oe-bolh.trailer v-frt-pay-dscr format "x(9)"
       "PAGE" at 65 page-number - v-last-page to 71 format "99"
       "OF" at 73 v-page-tot to 78 format "999" skip
       tmpstore at 1 skip
       "LOC" at 1 "ITEM NUMBER" at 9 "PRODUCT DESCRIPTION" at 26
       "QUANTITY IN" at 59 CAPS(v-qty-uom) at 71 skip
       "------" at 1 "---------------" at 9
       "------------------------------" at 26
       "----------------------" at 58 skip
       "TO SHIP" at 59 "SHIPPED" at 71 skip
       "----------" at 58 "---------" at 70 skip

    with frame bol-top no-box no-labels page-top stream-io width 80.

form oe-boll.loc-bin format "x(6)" at 1
     oe-boll.i-no format "x(15)" at 9
     oe-ordl.i-name format "x(30)" at 26
     v-to-ship format ">>>,>>>" to 66
     "_______" at 71 skip

    with frame ln-s down no-box no-labels stream-io width 90.

def stream last-page.


tmpstore = fill("-",80).

{sa/sa-sls01.i}

output stream last-page to value(tmp-dir + "bolroyal.txt") page-size VALUE(v-lines-per-page).

view frame bol-top.
view stream last-page frame bol-top.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each xreport  where xreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)  eq xreport.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first cust
	where cust.company eq cocode
	  and cust.cust-no eq oe-bolh.cust-no
	no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
	where carrier.company eq oe-bolh.company
	  and carrier.carrier eq substring(oe-bolh.carrier,1,5)
	no-lock no-error.
    v-carrier = if avail carrier then carrier.dscr else "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no no-lock,
        first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq oe-boll.ord-no
        no-lock,
        
        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-ord.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no
          AND oe-ordl.LINE    EQ oe-boll.LINE
        NO-LOCK:

      ASSIGN
       v-ord-no       = oe-boll.ord-no
       v-frt-pay-dscr = if oe-ord.frt-pay eq "p" then "PREPAID"
                       else
                       if oe-ord.frt-pay eq "c" then "COLLECT"
                       else
                       if oe-ord.frt-pay eq "b" then "BILL"
                       else
                       if oe-ord.frt-pay eq "t" then "3RD PARTY" else "".

      find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
      v-qty-uom = if avail uom then substr(uom.dscr,1,8)
                  else oe-ordl.pr-uom.

      LEAVE.
    end.
        
    v-time = string(time,"hh:mm am").

    /* calculate total number of packages */
    assign
     v-tot-pkgs = 0
     v-page-tot = 0.
    FOR EACH xoe-boll
        WHERE xoe-boll.company EQ oe-bolh.company
          AND xoe-boll.b-no    EQ oe-bolh.b-no:
      assign
       v-tot-pkgs = v-tot-pkgs + xoe-boll.cases
       v-page-tot = v-page-tot + 3.
      if xoe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.
      v-page-tot = v-page-tot - 1.

      if avail oe-ordl then
      find first fg-bin
	  where fg-bin.company eq cocode
	    and fg-bin.loc     eq xoe-boll.loc
	    and fg-bin.loc-bin eq xoe-boll.loc-bin
	    and fg-bin.i-no    eq xoe-boll.i-no
	    and fg-bin.tag     eq xoe-boll.tag
	    and fg-bin.job-no  eq oe-ordl.job-no
	    and fg-bin.job-no2 eq oe-ordl.job-no2
	  use-index co-ino no-lock no-error.
      else
      find first fg-bin
	  where fg-bin.company eq cocode
	    and fg-bin.tag     eq xoe-boll.tag
	    and fg-bin.loc     eq xoe-boll.loc
	    and fg-bin.loc-bin eq xoe-boll.loc-bin
	    and fg-bin.i-no eq xoe-boll.i-no
	  use-index tag no-lock no-error.

      if v-print-pal and avail fg-bin then v-page-tot = v-page-tot + 1.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-page-tot = v-page-tot + 1.
    end.

    if first-of(oe-bolh.bol-no) then do:
      if oe-ctrl.pr-broker and avail cust and shipto.broker then
	v-coname = cust.name.
      else do:
	find first company where company.company eq cocode no-lock no-error.
	if avail company then v-coname = company.name.
      end.

      if avail shipto then
	assign
	 v-ship-name    = shipto.ship-name
	 v-ship-addr[1] = shipto.ship-addr[1]
	 v-ship-addr[2] = shipto.ship-addr[2]
	 v-ship-city    = shipto.ship-city
	 v-ship-state   = shipto.ship-state
	 v-ship-zip     = shipto.ship-zip.
      else
	assign
	 v-ship-name    = ""
	 v-ship-addr[1] = ""
	 v-ship-addr[2] = ""
	 v-ship-city    = ""
	 v-ship-state   = ""
	 v-ship-zip     = "".
    end.
  end. /* first-of(oe-bolh.bol-no) */

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = string(oe-boll.ord-no,"999999")
     report.key-02   = oe-boll.i-no
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
    {oe/rep/bolroyal.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.

    {oe/rep/bolroyal.i}

    v-last-page = page-number.

    put skip(36 - v-lines)
	"TOTAL CARTONS" at 6 v-tot-pkgs to 25
	/* "TOTAL PALLETS" at 32 oe-bolh.tot-pallets to 51 */
	"NET WEIGHT" at 58 oe-bolh.tot-wt to 75 skip.
  end.
end. /* for each oe-bolh */

output stream last-page close.

/* end ---------------------------------- copr. 1992  Advanced Software, Inc. */

