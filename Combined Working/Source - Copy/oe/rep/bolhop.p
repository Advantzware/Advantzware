/*
BOL for House of Packaging
Written by: Randal Lanning 04/29/1999
*/

{sys/inc/var.i shared}
{sys/form/r-top.i}

{oe/rep/oe-lad.i}

def buffer b-oe-bolh for oe-bolh.
def buffer b-oe-boll for oe-boll.

def var v-sold-name like cust.name no-undo.
def var v-ship-name like cust.name no-undo.
def var v-sold-addr1 like cust.name no-undo.
def var v-sold-addr2 like cust.name no-undo.
def var v-ship-addr1 like cust.name no-undo.
def var v-ship-addr2 like cust.name no-undo.
def var v-sold-city like cust.name no-undo.
def var v-ship-city like cust.name no-undo.
def var v-cust-phone like cust.phone no-undo.
def var tmp like oe-boll.qty no-undo.
def var v-fob as char format "x(30)" no-undo.
def var v-ship-date as date format "99/99/9999" no-undo.
def var v-bol-no like oe-bolh.bol-no no-undo.
def var v-terms as char format "x(20)" no-undo.
def var v-sales-rep as char format "x(11)" no-undo.
def var v-cust-part like itemfg.part-no format "x(30)" no-undo.
def var v-item-desc like itemfg.i-dscr no-undo.
def var v-order-qty as dec format "->>,>>>,>>9" no-undo.
def var v-ship-qty as dec format "->>,>>>,>>9" no-undo.
def var v-weight as dec no-undo.
def var v-complete as char no-undo.
def var v-order-number like oe-ord.ord-no no-undo.
def var v-po-number like oe-ord.po-no no-undo.
DEF VAR v-lot-no AS CHAR FORMAT "X(15)" NO-UNDO.
def var v-cases as char format "x(20)" no-undo.
def var v-item-number like itemfg.i-no no-undo.
def var v-ship-via like carrier.dscr no-undo.
def var v-over-pct like oe-ord.over-pct no-undo.
def var v-under-pct like oe-ord.under-pct no-undo.
def var v-item-desc2 like itemfg.i-dscr no-undo.
DEF VAR v-disp-desc2 AS logi NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def stream last-page.

form header
  skip(1)
  v-sold-name at 4
  v-ship-name at 37
  v-fob at 69
  v-bol-no at 99
  v-ship-date at 117
  v-sold-addr1 at 4
  v-ship-addr1 at 37
  v-sold-addr2 at 4
  v-ship-addr2 at 37
  v-terms at 69
  v-sales-rep at 101
  string(v-over-pct, ">9.9<") + "-" + trim(string(v-under-pct, ">9.9<"))
    format "x(11)" at 117
  v-sold-city at 4
  v-ship-city at 37
  v-cust-phone at 4
  v-ship-via at 69
  trim(string(page-number - v-last-page, ">9")) + " OF " +
    trim(string(v-page-tot,">9")) at 117 format "x(8)"
  skip(3)
with frame top-of-form stream-io width 132 page-top no-box no-labels.

form
  v-cust-part at 1
  v-item-desc at 32
  v-order-qty at 62
  v-ship-qty at 91
  v-order-number to 111
  v-weight at 117
  v-item-number at 1
  v-po-number
  v-lot-no AT 32
  v-cases at 65
  v-complete to 111
with frame detail stream-io width 132 no-box no-labels down.

form
  v-item-desc2 AT 32
  v-cases at 65
with frame detail1 stream-io width 132 no-box no-labels down.

{sa/sa-sls01.i}

find first company where company.company = cocode no-lock no-error.
find oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

output stream last-page to value(tmp-dir + "bolhop.txt") page-size VALUE(v-lines-per-page).

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) THEN do:
    find cust where cust.company = oe-bolh.company
      and cust.cust-no = oe-bolh.cust-no
      no-lock no-error.

    if available cust then
    assign
      v-sold-name = cust.name
      v-cust-phone = cust.phone
      v-sold-addr1 = cust.addr[1]
      v-sold-addr2 = cust.addr[2]
      v-sold-city = cust.city + " " + cust.state + " " + cust.zip.
    else
    assign
      v-sold-name = ""
      v-cust-phone = ""
      v-sold-addr1 = ""
      v-sold-addr2 = ""
      v-sold-city = "".

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    if available shipto then
    assign
      v-ship-name = shipto.ship-name
      v-ship-addr1 = shipto.ship-addr[1]
      v-ship-addr2 = shipto.ship-addr[2]
      v-ship-city = shipto.ship-city + " " +
	shipto.ship-state + " " + shipto.ship-zip.
    else
    assign
      v-ship-name = ""
      v-ship-addr1 = ""
      v-ship-addr2 = ""
      v-ship-city = "".

    find carrier where carrier.company = oe-bolh.company
      and carrier.carrier = oe-bolh.carrier no-lock no-error.

    assign
      v-ship-via  = if available carrier then carrier.dscr else ""
      v-ship-date = oe-bolh.bol-date
      v-bol-no    = oe-bolh.bol-no
      v-over-pct  = 0
      v-under-pct = 0
      v-fob       = ""
      v-terms     = ""
      v-sales-rep = "".

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.bol-no  EQ oe-bolh.bol-no,
        FIRST oe-ord NO-LOCK
	    WHERE oe-ord.company EQ oe-boll.company
	      AND oe-ord.ord-no  EQ oe-boll.ord-no:

      ASSIGN
       v-sales-rep = oe-ord.sman[1] + " " + oe-ord.sman[2] + " " + oe-ord.sman[3]
       v-fob = if oe-ord.fob-code = "ORIG" then "Origin" else "Destination"
       v-over-pct = oe-ord.over-pct
       v-terms = oe-ord.terms-d
       v-under-pct = oe-ord.under-pct.
      
        if oe-ord.frt-pay = "B" then
          v-fob = v-fob + "/PREPAY & ADD".
        else if oe-ord.frt-pay = "P" then
          v-fob = v-fob + "/PREPAID".      
        else if oe-ord.frt-pay = "C" then
          v-fob = v-fob + "/COLLECT".

      LEAVE.
    END.

    FOR EACH tt-boll:
      DELETE tt-boll.
    END.
  END.

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:
    CREATE tt-boll.
    BUFFER-COPY oe-boll TO tt-boll
        ASSIGN tt-boll.rec_id = STRING(RECID(oe-boll)).
  END.

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
    {oe/rep/bolhop.i "stream last-page"}

    v-page-tot = PAGE-NUMBER(last-page) - v-last-page.

    {oe/rep/bolhop.i}

    v-last-page = PAGE-NUMBER.
  END.

  RELEASE b-oe-bolh.
  DO WHILE NOT AVAIL b-oe-bolh:
    FIND b-oe-bolh WHERE RECID(b-oe-bolh) EQ RECID(oe-bolh) EXCLUSIVE-LOCK.
    IF AVAIL b-oe-bolh THEN b-oe-bolh.printed = YES.
  END.
END.
