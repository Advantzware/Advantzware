/* ---------------------------------------------- oe/rep/bolbluer.i 01/99 JLF */
/* Print Blue Ridge BOL                                                       */
/* -------------------------------------------------------------------------- */

hide {1} frame bol-bot2.
view {1} frame bol-bot1.

for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:

  find first oe-ordl
      where oe-ordl.company eq cocode
	    and oe-ordl.ord-no  eq oe-boll.ord-no
	    and oe-ordl.i-no    eq oe-boll.i-no
	    and oe-ordl.line    eq oe-boll.line
      no-lock no-error.

  find first oe-ord
      where oe-ord.company eq cocode
	    and oe-ord.ord-no  eq oe-boll.ord-no
      no-lock no-error.

  if line-counter {2} + 1 gt page-size {2} then page.

  if oe-boll.qty-case ne 0 and oe-boll.cases ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.qty-case no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.qty-case
     w2.cases   = w2.cases + oe-boll.cases.
  end.

  if oe-boll.partial ne 0 then do:
    find first w2 where w2.cas-cnt eq oe-boll.partial no-error.
    if not avail w2 then create w2.
    assign
     w2.cas-cnt = oe-boll.partial
     w2.cases   = w2.cases + 1.
  end.

  i = 0.
  for each w2 break by w2.cases:
    i = i + 1.

    assign
     v-part-dscr = ""
     v-job-po    = "".

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po    = oe-boll.po-no.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.part-dscr1
       v-job-po    = if oe-ordl.job-no eq "" then "" else
		    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr2.

    display {1}
	    /* oe-boll.i-no */ oe-ordl.part-no      when i eq 1
	    v-job-po
	    v-part-dscr
	    w2.cases
	    w2.cas-cnt
	    oe-boll.qty       when last(w2.cases)
	with frame bol-mid.
    down {1} with frame bol-mid.

    delete w2.
  end.

  do i = i + 1 to 3:
    clear frame bol-mid no-pause.

    assign
     v-part-dscr = ""
     v-job-po    = "".

    if i eq 1 then
      assign
       v-part-dscr = oe-ordl.i-name
       v-job-po    = oe-boll.po-no.

    else
    if i eq 2 then
      assign
       v-part-dscr = oe-ordl.part-dscr1
       v-job-po    = if oe-ordl.job-no eq "" then "" else
		    (trim(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

    else
    if i eq 3 then v-part-dscr = oe-ordl.part-dscr2.

    if v-part-dscr ne "" or v-job-po ne "" then do:
      display {1}
	      v-job-po
	      v-part-dscr

	    with frame bol-mid.
      down {1} with frame bol-mid.
    end.
  end.

  put {1} skip(1).

  oe-boll.printed = yes.
end. /* for each oe-boll */

hide {1} frame bol-bot1.
view {1} frame bol-bot2.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
