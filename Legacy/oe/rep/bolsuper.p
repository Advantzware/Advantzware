/* --------------------------------------------- oe/rep/bolsuper.p  01/98 FWK */
/* PRINT Superior BOL                                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.
def buffer xitemfg  for itemfg.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.
def var v-coname like coname no-undo.
def var v-printlines as int no-undo.
def var v-qty-uom like oe-ordl.pr-uom no-undo.
def var v-to-ship like oe-boll.qty no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-shiplines as int no-undo.
def var v-comp-name  like shipto.ship-name.
def var v-comp-addr  like shipto.ship-addr.
def var v-comp-city  like shipto.ship-city.
def var v-comp-state like shipto.ship-state.
def var v-comp-zip   like shipto.ship-zip.
def var v-part-dscr as   char format "x(30)".
def var v-part-qty  as   dec.
def var v-bol-qty    like oe-boll.qty.
def var v-net-weight like oe-bolh.tot-wt no-undo.
def var v-ord-no like oe-boll.ord-no no-undo.

find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

form
  oe-boll.loc-bin format "x(6)" at 1
  oe-boll.i-no format "x(15)" at 9
  itemfg.i-name format "x(30)" at 26
  v-to-ship format ">>>,>>>" to 66
  "_______" at 71 skip
  with frame ln-s down no-box no-labels stream-io width 90.

form
  itemfg.part-dscr1 format "x(30)" at 26 skip(1)
  with frame ln-s-dscr down no-box no-labels stream-io width 90.

tmpstore = fill("-",80).

do:     /* production mode */

  v-last-page = 0.

  for each report   where report.term-id eq v-term-id,
      first oe-bolh where recid(oe-bolh) eq report.rec-id
      break by oe-bolh.bol-no:

    if first-of(oe-bolh.bol-no) then do:
      assign v-net-weight = 0
             v-printlines = 1.
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
	    and carrier.carrier eq oe-bolh.carrier
	  no-lock no-error.
      v-carrier = if avail carrier then carrier.dscr else "".

      for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
          first oe-ord
	      where oe-ord.company eq oe-boll.company
            and oe-ord.ord-no  eq oe-boll.ord-no
	      NO-LOCK:
        v-ord-no = oe-boll.ord-no.

        if oe-ord.frt-pay eq "p" then v-frt-pay-dscr = "PREPAID".
	    else
	    if oe-ord.frt-pay eq "c" then v-frt-pay-dscr = "COLLECT".
        else
	    if oe-ord.frt-pay eq "b" then v-frt-pay-dscr = "PPD/CHG".

	    find first oe-ordl
	        where oe-ordl.company eq oe-ord.company
	          and oe-ordl.ord-no  eq oe-ord.ord-no
	        no-lock no-error.
	    if avail oe-ordl then do:
	      find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
	      v-qty-uom = if avail uom then substr(uom.dscr,1,8)
		              else oe-ordl.pr-uom.
	    end.
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
	find first itemfg
	    where itemfg.company eq xoe-boll.company
	      and itemfg.i-no    eq xoe-boll.i-no
	    no-lock no-error.
	if avail itemfg and itemfg.part-dscr1 eq "" then
	  v-page-tot = v-page-tot - 1.
      end.

      v-page-tot = if (v-page-tot / 34) le 1 then 1
		   else round(((v-page-tot / 34) +
		    (if (v-page-tot modulo 34) ne 0 then .5 else 0)),0).

      if first-of(oe-bolh.bol-no) then do:
	if oe-ctrl.pr-broker and avail cust and shipto.broker then
	  v-coname = cust.name.
	else do:
	  find first company where company.company = cocode no-lock no-error.
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

	  if shipto.broker then
	    assign v-comp-name    = cust.name
		   v-comp-addr[1] = cust.addr[1]
		   v-comp-addr[2] = cust.addr[2]
		   v-comp-city    = cust.city
		   v-comp-state   = cust.state
		   v-comp-zip     = cust.zip.
	  else
	    assign v-comp-name    = company.name 
		   v-comp-addr[1] = company.addr[1] 
		   v-comp-addr[2] = company.addr[2] 
		   v-comp-city    = company.city 
		   v-comp-state   = company.state
		   v-comp-zip     = company.zip.

	  format header
		 "VENDOR:" at 1 skip
/*
		 company.name at 6 
		 company.addr[1] at 6 
		 company.addr[2] at 6 
		 company.city at 6 company.state company.zip 
*/
		 v-comp-name at 6
		 v-comp-addr[1] at 6
		 v-comp-addr[2] at 6
		 v-comp-city at 6 v-comp-state v-comp-zip
		 skip(1)
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
		 "ORDER#" v-ord-no skip
		 "BOL#" oe-bolh.bol-no format "999999"
		 "TO SHIP VIA" at 13 v-carrier format "x(30)"
		 "DUE ON" at 55 oe-bolh.bol-date at 63 skip
		 "CUSTOMER PO#" at 1 oe-ord.po-no at 13
		 "TRAILER #" at 35 oe-bolh.trailer v-frt-pay-dscr
		 "PAGE" at 63 page-number - v-last-page to 69 format "99"
		 "OF" at 71 v-page-tot to 75 format "99" skip
		  tmpstore at 1 skip
		"LOC" at 1 "ITEM NUMBER" at 9 "PRODUCT DESCRIPTION" at 26
		"QUANTITY IN" at 59 CAPS(v-qty-uom) at 71 skip
		"------" at 1 "---------------" at 9
		"------------------------------" at 26
		"----------------------" at 58 skip
		"TO SHIP" at 59 "SHIPPED" at 71 skip
		"----------" at 58 "---------" at 70 skip
	      with frame hd-top-comp no-box no-labels page-top stream-io width 80.
	  view frame hd-top-comp.

      end.

      page.
    end. /* first-of(oe-bolh.bol-no) */

    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
      find first oe-ordl
	  where oe-ordl.company eq cocode
	    and oe-ordl.ord-no  eq oe-boll.ord-no
	    and oe-ordl.i-no    eq oe-boll.i-no
	    and oe-ordl.line    eq oe-boll.line
	  no-lock no-error.
	  
      v-bol-qty = v-bol-qty + oe-boll.qty.

      if v-printlines gt 31 then do:
	page.
	v-printlines = 0.
      end.

      if avail oe-ordl then
	find first oe-ord
	    where oe-ord.company eq oe-ordl.company
	      and oe-ord.ord-no  eq oe-ordl.ord-no
	    no-lock no-error.
      find first itemfg
	  where itemfg.company eq oe-boll.company
	    and itemfg.i-no    eq oe-boll.i-no
	  no-lock no-error.

      if oe-boll.cases gt 0 or oe-boll.partial ne 0 then do:
	release oe-rel.
	find first oe-rell
	    where oe-rell.company eq oe-boll.company
	      and oe-rell.r-no    eq oe-boll.r-no
          AND oe-rell.ord-no  EQ oe-boll.ord-no
	      and oe-rell.i-no    eq oe-boll.i-no
	      and oe-rell.line    eq oe-boll.line
	    no-lock no-error.

	if avail oe-rell then do:
	  find first oe-relh of oe-rell no-lock.
	  find first oe-rel
	      where oe-rel.company eq oe-relh.company
		and oe-rel.ord-no  eq oe-rell.ord-no
		and oe-rel.line    eq oe-rell.line
		and oe-rel.link-no eq oe-rell.r-no
		and oe-rel.ship-no eq oe-relh.ship-no
		and oe-rel.i-no    eq oe-rell.i-no
	      no-lock no-error.

	  if not avail oe-rel then
	    find first oe-rel
		where oe-rel.company  eq oe-relh.company
		  and oe-rel.ord-no   eq oe-rell.ord-no
		  and oe-rel.line     eq oe-rell.line
		  and oe-rel.rel-date eq oe-relh.rel-date
		  and oe-rel.ship-no  eq oe-relh.ship-no
		  and oe-rel.i-no     eq oe-rell.i-no
		no-lock no-error.
	end.

	if oe-ordl.pr-uom eq "CS" then
	  v-to-ship = oe-boll.cases + (if oe-boll.partial gt 0 then 1 else 0).
	else
	  v-to-ship = oe-boll.qty.

	display oe-boll.loc-bin
		oe-boll.i-no
		itemfg.i-name
		v-to-ship
	    with frame ln-s.
	down with frame ln-s.

	if avail itemfg and itemfg.part-dscr1 ne "" then do:
	  display itemfg.part-dscr1 with frame ln-s-dscr.
	  down with frame ln-s-dscr.
	  v-printlines = v-printlines + 1.
	end.
	else
	  put skip(1).

	v-printlines = v-printlines + 2.

      end.

      oe-boll.printed = yes.
    end. /* for each oe-boll */

    /* Run Procedure print-fg-sets */
    run print-fg-sets.   

    assign v-net-weight = v-net-weight + oe-bolh.tot-wt.
    
    v-shiplines = 0.
    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then v-shiplines = v-shiplines + 1.
    end.

    if (v-shiplines + v-printlines) gt 36 then do:
      page {1}.
      v-printlines = 0.
    end.

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then put {1} oe-bolh.ship-i[i] at 11 skip.
    end.
    v-printlines = v-printlines + v-shiplines.

    oe-bolh.printed = yes.

    if last-of(oe-bolh.bol-no) then
    do:
      put skip(34 - v-printlines)
	  "NET WEIGHT" at 58 v-net-weight /* oe-bolh.tot-wt */ to 75 skip.
    end.
    else
    do:
      put skip(1).
      v-printlines = v-printlines + 1.
    end.
 

    v-last-page = page-number.
  end. /* for each oe-bolh */
end.    /* production mode */


procedure print-fg-sets:

  if itemfg.isaset then

  for each fg-set
     where fg-set.company eq cocode
       and fg-set.set-no  eq itemfg.i-no
  no-lock:

    find first xitemfg
	  where xitemfg.company eq cocode
	    and xitemfg.i-no    eq fg-set.part-no
    no-lock no-error.

    v-part-dscr = string(fg-set.part-no,"x(16)") +
	           (if avail xitemfg then xitemfg.i-name else "").

    {sys/inc/part-qty.i v-part-qty fg-set}

    put 
      v-part-dscr            at 11 format "x(46)"
      v-bol-qty * v-part-qty to 73 format ">>>>9" skip.

    v-printlines = v-printlines + 1.
    
    if v-printlines gt 31 then do:
	page.
	v-printlines = 0.
    end.
  end. 

  put skip(1).
  v-printlines = v-printlines + 1.
  if v-printlines gt 31 then do:
    page.
    v-printlines = 0.
  end.

end procedure.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
