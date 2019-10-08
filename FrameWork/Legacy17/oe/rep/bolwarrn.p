/* ---------------------------------------------- oe/rep/bolwarrn.p 03/98 JLF */
/*                                                                            */
/* Print BOL when sys-ctrl.char-fld eq "Warren" - O/E Module                  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.

{oe/rep/oe-lad.i}

def var v-ship-id          like shipto.ship-id.
def var v-ship-name        like shipto.ship-name.
def var v-ship-addr        like shipto.ship-addr.
def var v-ship-city        like shipto.ship-city.
def var v-ship-state       like shipto.ship-state.
def var v-ship-zip         like shipto.ship-zip.
def var v-ship-addr3       as   char format "x(30)".

def var v-frt-dscr         as   char format "x(5)".
def var v-part-info        as   char format "x(41)".

def var v-pal-cnt          as   int.
def var v-cas-cnt          as   int.
def var v-cases            as   int.
def var v-shp-qty          as   int.
def var v-rel-qty          as   int.
def var v-tot-wt           as   int format ">>>>9".
def var v-tot-pkgs         as   int format ">>9".
def var v-tot-pal          as   int format ">>9".
def var v-page-no          as   int format ">9" no-undo.

def workfile w1 no-undo
    field pallets          as   int format ">>9"
    field pal-cnt          as   int format ">>>>>9".

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".

form w1.pallets        to 5
     w1.pal-cnt        to 13
     oe-boll.ord-no    to 21
     oe-boll.i-no      at 23
     v-frt-dscr        at 75

  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.

form header
     skip(10)
     oe-bolh.bol-no                 to 58
     page-number - v-last-page      to 73   format ">9"
     skip(1)
     oe-bolh.bol-date               to 36
     skip(1)
     v-ship-name                    at 7
     skip(1)
     trim(v-ship-addr[1]) + "  " +
     trim(v-ship-addr[2]) + "  " +
     trim(v-ship-addr3)             at 7    format "x(63)"
     skip(3)
     carrier.dscr                   at 7 when avail carrier
     oe-bolh.trailer                at 68   FORMAT "x(13)"
     skip(5)

    with frame bol-top page-top no-box no-underline stream-io width 80.

form v-part-info       at 23

    with frame info no-labels no-box no-underline down stream-io width 80.

form header
     " " skip
     skip(5)

    with frame bol-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     skip(2)
     v-tot-wt             to 48
     "# Pkgs:"            to 75
     v-tot-pkgs           to 79 skip
     skip(1)
     v-tot-pal            to 7
     "Total Pallet(s)"          skip

    with frame bol-bot2 page-bottom no-box no-underline stream-io width 80.


view frame bol-top.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
find first ce-ctrl where ce-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  find first carrier
      where carrier.company eq oe-bolh.company
	and carrier.carrier eq oe-bolh.carrier
      no-lock no-error.

  if first-of(oe-bolh.bol-no) then do:
    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-id      = shipto.ship-id
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-city    = shipto.ship-city
     v-ship-state   = shipto.ship-state
     v-ship-zip     = shipto.ship-zip
     v-ship-addr3   = v-ship-city + ", " +
		      v-ship-state + "  " +
		      v-ship-zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".

    page.

    hide frame bol-bot2.
    view frame bol-bot1.

    assign
     v-tot-wt   = 0
     v-tot-pkgs = 0
     v-tot-pal  = 0
     v-frt-dscr = if cust.frt-pay eq "P" then "PAID" else
		  if cust.frt-pay eq "B" then "BILL" else "COLL".
  end. /* first-of(oe-bolh.bol-no) */

  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no
      break by oe-boll.i-no:

    if first-of(oe-boll.i-no) then do:
      assign
       v-shp-qty = 0
       v-rel-qty = 0.

      for each w1:
	delete w1.
      end.
      for each w2:
	delete w2.
      end.
    end.

    find first oe-rell
	where oe-rell.company  eq cocode
	  and oe-rell.r-no     eq oe-rell.r-no
	  and oe-rell.i-no     eq oe-boll.i-no
	  and oe-rell.line     eq oe-boll.line
	  and oe-rell.ord-no   eq oe-boll.ord-no
	  and oe-rell.rel-no   eq oe-boll.rel-no
	  and oe-rell.b-ord-no eq oe-boll.b-ord-no
	USE-INDEX r-no no-lock no-error.
    v-rel-qty = if avail oe-rell then oe-rell.qty else 0.

    find first oe-ordl
	where oe-ordl.company eq cocode
	  and oe-ordl.ord-no  eq oe-boll.ord-no
	  and oe-ordl.i-no    eq oe-boll.i-no
	  and oe-ordl.line    eq oe-boll.line
	no-lock no-error.

    assign
     oe-boll.printed = true
     v-shp-qty       = v-shp-qty + oe-boll.qty
     v-pal-cnt       = 0
     v-cas-cnt       = oe-boll.qty-case.

    release est.
    release fg-bin.

    if oe-boll.tag ne "" then
      find first fg-bin
	  where fg-bin.company eq oe-boll.company
	    and fg-bin.tag     eq oe-boll.tag
	    and fg-bin.i-no    eq oe-boll.i-no
	    and fg-bin.loc     eq oe-boll.loc
	    and fg-bin.loc-bin eq oe-boll.loc-bin
	  use-index tag no-lock no-error.

    if avail oe-ordl then do:
      find first est
          where est.company eq oe-ordl.company
            and est.est-no  eq oe-ordl.est-no
          no-lock no-error.

      if not avail fg-bin then
      find first fg-bin
	  where fg-bin.company eq oe-boll.company
	    and fg-bin.i-no    eq oe-boll.i-no
	    and fg-bin.loc     eq oe-boll.loc
	    and fg-bin.loc-bin eq oe-boll.loc-bin
	    and fg-bin.tag     eq ""
	    and fg-bin.job-no  eq oe-ordl.job-no
	    and fg-bin.job-no2 eq oe-ordl.job-no2
	  no-lock no-error.
    end.

    if avail fg-bin then v-pal-cnt = fg-bin.unit-count * fg-bin.units-pallet.

    if v-pal-cnt eq 0 and avail est then do:
      find first eb
	  where eb.company    eq est.company
        and eb.est-no     eq est.est-no
	    and eb.form-no    eq 0
	    and (eb.stock-no  eq oe-boll.i-no or
				 est.est-type eq 2 or est.est-type eq 6)
	  no-lock no-error.
      if avail eb then v-pal-cnt = eb.cas-pal * v-cas-cnt.
    end.

    if v-pal-cnt eq 0 then do:
      find first item
	  where item.company eq cocode
	    and item.i-no    eq cust.case-bundle
	  no-lock no-error.
      if avail item then v-pal-cnt = item.case-pall * v-cas-cnt.
    end.

    if v-pal-cnt eq 0 then do:
      find first item
	  where item.company eq cocode
	    and item.i-no    eq ce-ctrl.def-case
	  no-lock no-error.
      if avail item then v-pal-cnt = item.case-pall * v-cas-cnt.
    end.

    if v-pal-cnt eq 0 or v-pal-cnt gt oe-boll.qty then v-pal-cnt = oe-boll.qty.
    if v-cas-cnt eq 0 or v-cas-cnt gt oe-boll.qty then v-cas-cnt = oe-boll.qty.

    find first w1 where w1.pal-cnt eq v-pal-cnt no-error.
    if not avail w1 then do:
      create w1.
      w1.pal-cnt = v-pal-cnt.
    end.

    assign
     w1.pallets = w1.pallets + trunc(oe-boll.qty / v-pal-cnt,0)
     v-pal-cnt  = oe-boll.qty modulo v-pal-cnt.

    if v-pal-cnt gt 0 then do:
      find first w1 where w1.pal-cnt eq v-pal-cnt no-error.
      if not avail w1 then do:
	create w1.
	w1.pal-cnt = v-pal-cnt.
      end.

      w1.pallets = w1.pallets + 1.
    end.

    find first w2 where w2.cas-cnt eq v-cas-cnt no-error.
    if not avail w2 then do:
      create w2.
      w2.cas-cnt = v-cas-cnt.
    end.

    assign
     v-cases   = trunc((oe-boll.qty - oe-boll.partial) / v-cas-cnt,0)
     w2.cases  = w2.cases + v-cases
     v-cas-cnt = oe-boll.qty - (v-cases * v-cas-cnt).

    if v-cas-cnt gt 0 then do:
      find first w2 where w2.cas-cnt eq v-cas-cnt no-error.
      if not avail w2 then do:
	create w2.
	w2.cas-cnt = v-cas-cnt.
      end.

      w2.cases = w2.cases + 1.
    end.

    if last-of(oe-boll.i-no) then do:
      for each w1 break by w1.pal-cnt:
	display w1.pallets
		w1.pal-cnt
		oe-boll.ord-no when first(w1.pal-cnt)
		oe-boll.i-no   when first(w1.pal-cnt)
		v-frt-dscr     when first(w1.pal-cnt)
	    with frame detail.
	down with frame detail.
	v-tot-pal = v-tot-pal + w1.pallets.
      end.

      put skip(1).

      find first loc
	  where loc.company eq cocode
	    and loc.loc     eq oe-bolh.loc
	  no-lock no-error.
      v-part-info = "Whs: " + if avail loc then loc.dscr else oe-bolh.loc.

      if v-part-info ne "" then do:
	display v-part-info with frame info.
	down with frame info.
      end.

      assign
       v-cas-cnt   = 0
       v-part-info = "".
      for each w2 break by w2.cases desc:
	assign
	 v-cas-cnt   = v-cas-cnt   + w2.cases
	 v-part-info = v-part-info + trim(string(w2.cases,">>>9")) + "@" +
				     trim(string(w2.cas-cnt,">>>>>9")) + ",".
      end.

      assign
       v-tot-pkgs  = v-tot-pkgs + v-cas-cnt
       v-part-info = trim(string(v-cas-cnt,">>>9")) + " Case(s): " +
		     substr(v-part-info,1,length(v-part-info) - 1).

      if v-part-info ne "" then do:
	display v-part-info with frame info.
	down with frame info.
      end.

      put skip(1).

      v-part-info = "PO # " + string(oe-bolh.po-no,"x(15)") + "  " +
		    "PO Rel # " + trim(string(oe-bolh.rel-no,">>>>>9")) + "-" +
				  string(oe-boll.b-ord-no,"99").

      if v-part-info ne "" then do:
	display v-part-info with frame info.
	down with frame info.
      end.

      v-part-info =  "Shipped: " + trim(string(v-shp-qty,">>,>>>,>>9")) + "  " +
		    "Released: " + trim(string(v-rel-qty,">>,>>>,>>9")).

      if v-part-info ne "" then do:
	display v-part-info with frame info.
	down with frame info.
      end.

      put fill("-",79) at 2 format "x(79)" skip.
    end.
  end. /* for each oe-boll */

  assign
   oe-bolh.printed = true
   v-tot-wt        = v-tot-wt + oe-bolh.tot-wt.

  if last-of(oe-bolh.bol-no) then do:
    v-last-page = page-number.

    hide frame bol-bot1.
    view frame bol-bot2.
  end.
end. /* for each oe-bolh */

hide frame bol-top no-pause.
page.
