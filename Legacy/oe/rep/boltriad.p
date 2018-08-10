/* ---------------------------------------------- oe/rep/boltriad.p 12/98 JLF */
/* PRINT Triad BOL                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-sf            as   dec format "->,>>>,>>9.9<".
def var v-tot-wt            as   dec format "->,>>>,>>9.9<".

def var v-tot-pkgs          as   int format "->9".
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def var v-terms  like oe-ord.terms-d no-undo.

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

form header
    /* rdb 02150702
     skip(3)
     trim(string(oe-bolh.bol-no,">>>>>>>9")) at 71
     skip(1)
     oe-bolh.bol-date             at 67
     skip(1)
     string(trim(string(page-number - v-last-page,">9")) + " OF " +
	    trim(string(v-page-tot,">9")))
				  at 61 format "x(8)"
     skip(2)
     v-terms                      at 10
     skip(1)
     */
     skip(2)
     trim(string(oe-bolh.bol-no,">>>>>>>9")) at 71
     skip(1)
     oe-bolh.bol-date             at 67
     skip(1)
     string(trim(string(page-number - v-last-page,">9")) + " OF " +
	    trim(string(v-page-tot,">9")))
				  at 61 format "x(8)"
     skip(2)
     v-terms                      at 10
     skip(2)
     v-ship-name                  at 10
     cust.name                    at 55 FORMAT "x(26)"
     v-ship-addr[1]               at 10
     cust.addr[1]                 at 55 FORMAT "x(26)"
     v-ship-addr[2]               at 10
     cust.addr[2]                 at 55 FORMAT "x(26)"
     v-ship-addr3                 at 10
     v-cust-addr3                 at 55 FORMAT "x(26)"
     skip(5)
     v-salesman                   at 2
     carrier.dscr                 at 28 format "x(25)" when avail carrier
     oe-bolh.trailer              at 54 FORMAT "x(15)" 
     v-fob                        at 72 FORMAT "x(9)"
     skip(3)
   with frame bol-top page-top no-box no-underline stream-io width 80.

form v-tot-pkgs                     to 3                    /* pkgs */
     v-ord-qty                      to 12  format "->>>>>>>>"    /* quan ord */
     v-part-dscr                    at 14 format "x(30)"    /* desc */
     report.key-03                  at 45 format "x(15)"    /* cust PO */
     w2.cases                       to 63 format "->9"      /* # units */
     w2.cas-cnt                     to 69 format "->,>>9"   /* # / unit */
     v-bol-qty                      to 78 format "->>>>>>>9"   /* quan ship */
     v-part-comp                    at 81                   /* PC */
     v-bol-wt                       to 90 format ">>>>>>9"    /* wgt */
    with frame bol-mid1 down no-box no-labels stream-io width 90.

form header
     " "
     skip(4)
    with frame bol-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     skip(2)
     v-tot-sf                       to 67
     v-tot-wt                       to 80
     skip(1)
    with frame bol-bot2 page-bottom no-box no-underline stream-io width 80.

def stream last-page.

{sa/sa-sls01.i}

output stream last-page to value(tmp-dir + "boltriad.txt") page-size VALUE(v-lines-per-page).

VIEW frame bol-top.
VIEW stream last-page frame bol-top.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier
	where carrier.company eq oe-bolh.company
	  and carrier.carrier eq oe-bolh.carrier
	no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
		      shipto.ship-state + "  " +
		      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
		      cust.state + "  " +
		      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        FIRST oe-ord
        WHERE oe-ord.company EQ oe-boll.company
          AND oe-ord.ord-no  EQ oe-boll.ord-no
        NO-LOCK:

      if not avail carrier then
      find first carrier
          where carrier.company = oe-ord.company
	        and carrier.carrier = oe-ord.carrier
          no-lock no-error.

      do i = 1 to 3:
	    if oe-ord.sman[i] ne "" then
	      v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

      if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d.

      v-salesman = trim(v-salesman).

      if v-salesman gt '' then
	    if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
	       substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
    end.

    for each w3:
      delete w3.
    end.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    create report.
    assign
     report.term-id  = v-term
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"999999")
     report.key-03   = oe-boll.po-no
     report.rec-id   = recid(oe-boll)
     oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
    page.
    page stream last-page.

    {oe/rep/boltriad.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.

    {oe/rep/boltriad.i}

    v-last-page = page-number.

    hide frame bol-bot1.
    view frame bol-bot2.

    for each report where report.term-id eq v-term,
	first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

