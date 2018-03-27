/* ----------------------------------------------- oe/rep/bolsumm.i 11/98 JLF */
/* Truckload BOL Summary                                                      */
/* -------------------------------------------------------------------------- */

  for each w-bol,

      first oe-bolh
      where recid(oe-bolh) eq w-bol.rec-id
      no-lock,

      each oe-boll
      where oe-boll.company eq oe-bolh.company
	    and oe-boll.b-no    eq oe-bolh.b-no
      no-lock

      break by oe-boll.i-no:

    v-to-ship = v-to-ship +
		oe-boll.cases + if oe-boll.partial gt 0 then 1 else 0.

    if last-of(oe-boll.i-no) then do:
      find first itemfg
	  where itemfg.company eq oe-boll.company
	    and itemfg.i-no    eq oe-boll.i-no
	  no-lock no-error.

      if v-lines gt 33 then do:
	page.
	v-lines = 0.
      end.

      display {1} oe-boll.i-no
		  itemfg.i-name when avail itemfg
		  v-to-ship
	  with frame ln-s.
      down with frame ln-s.

      put {1} skip(1).

      assign
       v-tot-pkgs = v-tot-pkgs + v-to-ship
       v-to-ship  = 0
       v-lines    = v-lines + 2.
    end.
  end.

  v-lines = v-lines + 1.

  do li = 1 to 4:
    if shipto.notes[li] ne "" then v-lines = v-lines + 1.
  end.

  if v-lines gt 33 then do:
    page.
    v-lines = 0.
  end.

  do li = 1 to 4:
    if shipto.notes[li] ne "" then put {1} shipto.notes[li] at 11 skip.
  end.

  put {1}
      skip(2)
      "TOTAL CARTONS" at 6  v-tot-pkgs  to 25
      "TOTAL PALLETS" at 32 v-tot-pals  to 51
      "NET WEIGHT"    at 58 v-tot-wght  to 75 skip.

  assign
   v-last     = no
   v-bol-list = "".

  for each w-bol,

      first oe-bolh
      where recid(oe-bolh) eq w-bol.rec-id
      no-lock

      break by oe-bolh.bol-no:

    if first(oe-bolh.bol-no) then do:
      v-lines = v-lines + 3.

      if v-lines gt 33 then do:
	page.
	v-lines = 0.
      end.

      put {1} skip(2) "BOL LIST:" skip.
    end.

    if last-of(oe-bolh.bol-no) then do while true:
      if length(trim(v-bol-list) +
		trim(string(oe-bolh.bol-no,">>>>>>")) + ",") gt 75 or
	 v-last                                                    then do:

	if substr(v-bol-list,length(trim(v-bol-list)),1) eq "," then
	  substr(v-bol-list,length(trim(v-bol-list)),1) = "".

	v-lines = v-lines + 1.

	if v-lines gt 33 then do:
	  page.
	  v-lines = 0.
	end.

	put {1} v-bol-list at 3 skip.

	if v-last then leave.

	v-bol-list = "".
      end.

      v-bol-list =
	      trim(v-bol-list) + trim(string(oe-bolh.bol-no,">>>>>>")) + ",".

      if last(oe-bolh.bol-no) then v-last = yes.

      else leave.
    end.
  end.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
