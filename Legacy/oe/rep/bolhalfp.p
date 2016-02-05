/* ---------------------------------------------- oe/rep/bolhalfp.p 01/97 JLF */
/*                                                                            */
/* PRINT BOL when sys-ctrl.char-fld eq "1/2 Page" - O/E Module                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.

def var v-bol-qty          as   int.
def var v-i-no             as   char.
def var v-i-dscr           as   char format "x(30)" extent 4.
def var v-dscr-lines       as   int.
def var v-printline        as   int.
def var v-part-info        like oe-ordl.part-dscr1.
def var v                  as   int.

/* FWK Changed def for centering Company Info from 30 to 55 chars */
def var v-comp-name        as char format "x(55)".
def var v-comp-addr        as char format "x(55)" extent 2.
def var v-comp-addr3       as char format "x(55)".
def var v-comp-city        like company.city.
def var v-comp-state       like company.state.
def var v-comp-zip         like company.zip.
def var v-cust-no          like cust.cust-no.
def var v-cust-name        like cust.name.
def var v-cust-addr        like cust.addr.
def var v-cust-city        like cust.city.
def var v-cust-state       like cust.state.
def var v-cust-zip         like cust.zip.
def var v-cust-addr3       as   char format "x(30)".
def var v-ship-id          like shipto.ship-id.
def var v-ship-name        like shipto.ship-name.
def var v-ship-addr        like shipto.ship-addr.
def var v-ship-city        like shipto.ship-city.
def var v-ship-state       like shipto.ship-state.
def var v-ship-zip         like shipto.ship-zip.
def var v-ship-addr3       as   char format "x(30)".
def var v-fob              as   char format "x(11)".
def var v-hdr              as char format "x(80)" extent 3.
def var v-hdr1             as char format "x(8)" extent 2.
def var v-partial-case as int initial 1 no-undo.
def var v-part-num as int initial 0 no-undo.
def var v-unit-tot-case    like oe-boll.cases initial 0 no-undo.
def var v-unit-tot-units   like oe-boll.qty-case initial 0 no-undo.
def var v-unit-tot-wgt     like oe-boll.weight initial 0 no-undo.
def var v-tot-case    like oe-boll.cases initial 0 no-undo.
def var v-tot-units   like oe-boll.qty-case initial 0 no-undo.
def var v-tot-sqft   like itemfg.t-sqft initial 0 format ">>>,>>9.99" no-undo.
def var v-prt-co as log initial no no-undo.
def var v-job as char format "x(9)" no-undo.
DEF VAR v-po-no LIKE oe-bolh.po-no NO-UNDO.

/* Added FWK 8/18/97 */
form oe-boll.cases             format ">>9"
     oe-boll.qty-case          format ">>>9"
     v-job             at 15   format "x(9)"
     oe-boll.i-no      at 29   format "x(15)" skip
     v-i-dscr[1]       at 29
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-partial-case format ">9"
     oe-boll.partial format ">>>>9"
     v-part-info       at 29
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form "--- ------" skip
     v-unit-tot-case format ">>9"
     v-unit-tot-units format ">>>>9"
     "WT:" at 60 v-unit-tot-wgt    at 64
  with frame unit-tot no-labels no-box no-underline down stream-io width 80.

form v-tot-case format ">>9"
     v-tot-units format ">>>>9"
     "SQ FT:" at 40 v-tot-sqft at 47 "WT:" at 60 oe-bolh.tot-wt at 64
  with frame bol-tot no-labels no-box no-underline down stream-io width 80.
/*  Added FWK 8/18/97 */

/* Removed FWK 8/18/97
form oe-boll.qty               format ">>>>>>9-"
     oe-boll.i-no      at 15   format "x(13)"
     v-i-dscr[1]       at 29
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 29
  with frame dscr no-labels no-box no-underline down stream-io width 80.
  Removed FWK 8/18/97 */

if v-print-hdgs then
  assign
/*
   substr(v-hdr[1],01,07) = "SOLD TO"
   substr(v-hdr[1],46,07) = "SHIP TO"
*/
   v-hdr1[1] = "SOLD TO:"
   v-hdr1[2] = "SHIP TO:"

   v-hdr[2] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
	      "   TERMS            SLMN"

   v-hdr[3] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
	      "                        ".

find first sys-ctrl where sys-ctrl.company = cocode and
			  sys-ctrl.name = "BOLFMT" no-lock no-error.
if avail sys-ctrl and sys-ctrl.log-fld then
  assign v-prt-co = yes.
else
  assign v-prt-co = no.

find first company where company.company = cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id
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

    find carrier
	where carrier.company eq oe-bolh.company
	  and carrier.carrier eq oe-bolh.carrier
	no-lock no-error.

    ASSIGN
     v-fob          = ""
     v-frt-pay-dscr = "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:
      assign
       v-frt-pay-dscr = if oe-ord.frt-pay eq "p" or
			   oe-ord.frt-pay eq "b" then "Prepaid" else "Collect"
       v-fob          = if oe-ord.fob-code begins "ORIG" then "Origin"
							 else "Destination".

      LEAVE.
    end.

    v-time = string(time,"hh:mm am").

    if first-of(oe-bolh.bol-no) then do:

      if oe-ctrl.pr-broker and avail cust and shipto.broker then
	assign
	 v-comp-name    = cust.name
	 v-comp-addr[1] = cust.addr[1]
	 v-comp-addr[2] = cust.addr[2]
	 v-comp-city    = cust.city
	 v-comp-state   = cust.state
	 v-comp-zip     = cust.zip
	 v-cust-no      = shipto.cust-no
	 v-cust-name    = shipto.ship-name
	 v-cust-addr[1] = shipto.ship-addr[1]
	 v-cust-addr[2] = shipto.ship-addr[2]
	 v-cust-city    = shipto.ship-city
	 v-cust-state   = shipto.ship-state
	 v-ship-id      = shipto.ship-id
	 v-ship-name    = shipto.ship-name
	 v-ship-addr[1] = shipto.ship-addr[1]
	 v-ship-addr[2] = shipto.ship-addr[2]
	 v-ship-city    = shipto.ship-city
	 v-ship-state   = shipto.ship-state
	 v-ship-zip     = shipto.ship-zip.

      else
	assign
	 v-comp-name    = company.name
	 v-comp-addr[1] = company.addr[1]
	 v-comp-addr[2] = company.addr[2]
	 v-comp-city    = company.city
	 v-comp-state   = company.state
	 v-comp-zip     = company.zip
	 v-cust-no      = cust.cust-no
	 v-cust-name    = cust.name
	 v-cust-addr[1] = cust.addr[1]
	 v-cust-addr[2] = cust.addr[2]
	 v-cust-city    = cust.city
	 v-cust-state   = cust.state
	 v-cust-zip     = cust.zip
	 v-ship-id      = shipto.ship-id
	 v-ship-name    = shipto.ship-name
	 v-ship-addr[1] = shipto.ship-addr[1]
	 v-ship-addr[2] = shipto.ship-addr[2]
	 v-ship-city    = shipto.ship-city
	 v-ship-state   = shipto.ship-state
	 v-ship-zip     = shipto.ship-zip.

      assign
       v-comp-addr3 = v-comp-city + ", " +
		      v-comp-state + "  " +
		      v-comp-zip
       v-cust-addr3 = v-cust-city + ", " +
		      v-cust-state + "  " +
		      v-cust-zip
       v-ship-addr3 = v-ship-city + ", " +
		      v-ship-state + "  " +
		      v-ship-zip.

      if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
      if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
      if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".

      if length(v-comp-name) gt 0 then
	v-comp-name = fill(" ",int((79 - length(v-comp-name)) / 2)) +
		      v-comp-name.
      if length(v-comp-addr[1]) gt 0 then
	v-comp-addr[1] = fill(" ",int((79 - length(v-comp-addr[1])) / 2)) +
			 v-comp-addr[1].
      if length(v-comp-addr[2]) gt 0 then
	v-comp-addr[2] = fill(" ",int((79 - length(v-comp-addr[2])) / 2)) +
			 v-comp-addr[2].
      if length(v-comp-addr3) gt 0 then
	v-comp-addr3 = fill(" ",int((79 - length(v-comp-addr3)) / 2)) +
		       v-comp-addr3.

      if not v-prt-co then
	assign
	     v-comp-name = ""
	     v-comp-addr[1] = ""
	     v-comp-addr[2]  = ""
	     v-comp-addr3 = "".

      if first(oe-bolh.bol-no) then do:
	form header
	     skip(4)
	     v-comp-name
	     "    BOL #:" to 70 oe-bolh.bol-no to 79
	     v-comp-addr[1] skip
	     v-comp-addr[2]  skip
	     v-comp-addr3
	     skip(6)
	     v-hdr1[1]             at 1
	     v-cust-no             at 15
	     v-hdr1[2]             at 46
	     v-ship-id             at 60
	     v-cust-name           at 2
	     v-ship-name           at 47
	     v-cust-addr[1]        at 2
	     v-ship-addr[1]        at 47
	     v-cust-addr[2]        at 2
	     v-ship-addr[2]        at 47
	     v-cust-addr3          at 2
	     v-ship-addr3          at 47
	     skip(1)
	     v-hdr[2]
	     v-po-no
	     oe-bolh.bol-date FORM "99/99/99"
	     space(2)
	     carrier.dscr                  format "x(20)" when avail carrier
	     v-fob
	     oe-ord.terms-d                format "x(15)" when avail oe-ord
	     space(2)
	     oe-ord.sman[1]                when avail oe-ord
	     skip(1)
	     v-hdr[3]
	     skip(1)
	   with frame bolhead page-top no-labels no-box no-underline stream-io width 80.
      end.
    end.
    FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
    IF AVAIL oe-boll THEN v-po-no = oe-boll.po-no.

    hide frame bolhead no-pause.
    view frame bolhead.

    page.
  end. /* first-of(oe-bolh.bol-no) */

  v-printline = 0.

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no break by oe-boll.i-no:

    assign v-job = "".

    find first oe-ordl
	where oe-ordl.company eq cocode
	  and oe-ordl.ord-no  eq oe-boll.ord-no
	  and oe-ordl.i-no    eq oe-boll.i-no
	  and oe-ordl.line    eq oe-boll.line
	no-lock no-error.

    if avail oe-ordl then
      assign
       v-i-dscr[1] = oe-ordl.i-name
       v-i-dscr[2] = oe-ordl.i-dscr
       v-i-dscr[3] = oe-ordl.part-dscr1
       v-i-dscr[4] = oe-ordl.part-dscr2
       v-job = fill(" ",6 - length(trim(oe-ordl.job-no))) +
	       trim(oe-ordl.job-no) + "-" + trim(string(oe-ordl.job-no2,"99")).

    else do:
      find first itemfg
	  where itemfg.company eq cocode
	    and itemfg.i-no    eq oe-boll.i-no
	  no-lock.

      assign
       v-i-dscr[1] = itemfg.i-name
       v-i-dscr[2] = itemfg.i-dscr
       v-i-dscr[3] = itemfg.part-dscr1
       v-i-dscr[4] = itemfg.part-dscr2.
    end.

    if trim(v-job) begins "-" then assign v-job = "".

    assign
     v-printline  = v-printline + 3
     v-dscr-lines = 0.

    do v = 1 to 3:
      v-part-info = v-i-dscr[v + 1].

      if v-part-info ne "" then v-dscr-lines = v-dscr-lines + 1.
    end.
    v-printline = v-printline + v-dscr-lines.

    if v-printline gt 14 then do:
      v-printline = v-printline - (2 + v-dscr-lines).
      put skip(16 - v-printline) "* CONTINUED *" to 80.
      v-printline = 2 + v-dscr-lines.
      page.
    end.

    display oe-boll.cases
	    oe-boll.qty-case
	    v-job
	    oe-boll.i-no
	    v-i-dscr[1]
	with frame detail.

    assign v-unit-tot-case  = v-unit-tot-case + oe-boll.cases
	   v-unit-tot-units = v-unit-tot-units +
			      (oe-boll.cases * oe-boll.qty-case)
	   v-unit-tot-wgt   = v-unit-tot-wgt + oe-boll.weight.
/*
    display oe-boll.qty
	    oe-boll.i-no
	    v-i-dscr[1]
	with frame detail.
*/

    assign v-part-num = 0.

    do v = 1 to 3:
      v-part-info = v-i-dscr[v + 1].

      if v-part-info ne "" then do:
	assign v-part-num = v.
	leave.
      end.
    end.

    do v = 1 to 3:
      v-part-info = v-i-dscr[v + 1].

      if v-part-info ne "" then do:
	if v-part-num eq v and oe-boll.partial ne 0 then
	do:
	  display v-partial-case
		  oe-boll.partial
		  v-part-info skip with frame dscr.
	  assign v-unit-tot-case = v-unit-tot-case + 1
		 v-unit-tot-units  = v-unit-tot-units + oe-boll.partial.
	end.
	else
	  display "" @ v-partial-case
		  "" @ oe-boll.partial
		  v-part-info skip with frame dscr.

	down with frame dscr.
      end.
    end.

    if v-part-num eq 0 and oe-boll.partial ne 0 then do:
      display v-partial-case
	      oe-boll.partial
	      "" @ v-part-info skip with frame dscr.
      assign v-unit-tot-case = v-unit-tot-case + 1
	     v-unit-tot-units  = v-unit-tot-units + oe-boll.partial.
      down with frame dscr.
      v-printline = v-printline + 1.
    end.

    if last-of(oe-boll.i-no) then
    do:
      display v-unit-tot-case
	      v-unit-tot-units
	      v-unit-tot-wgt
	      skip with frame unit-tot.
      down with frame unit-tot.
      v-printline = v-printline + 2.

      find first itemfg
	  where itemfg.company eq cocode
	    and itemfg.i-no    eq oe-boll.i-no
	  no-lock.

      assign v-tot-case = v-tot-case + v-unit-tot-case
	     v-tot-units  = v-tot-units + v-unit-tot-units.

      if avail itemfg then
	assign v-tot-sqft = v-tot-sqft + (v-unit-tot-units * itemfg.t-sqft).

      assign v-unit-tot-case = 0
	     v-unit-tot-units = 0
	     v-unit-tot-wgt = 0.

    end.

    put skip(1).

    oe-boll.printed = true.
  end. /* for each oe-boll */


  put skip(16 - v-printline).
  display v-tot-case
	  v-tot-units
	  v-tot-sqft
	  oe-bolh.tot-wt
	  skip with frame bol-tot.
  page.

  assign v-tot-case = 0
	 v-tot-units = 0
	 v-tot-sqft = 0.

  oe-bolh.printed = true.
end. /* for each oe-bolh */
