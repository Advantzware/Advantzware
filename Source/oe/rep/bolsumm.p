/* ----------------------------------------------- oe/rep/bolsumm.p 11/98 JLF */
/* Truckload BOL Summary                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}
def var save_id as recid.

def var v-cust   like cust.cust-no.
def var v-ship   like shipto.ship-id.
def var v-s-bol  as int  format ">>>>>>" init 0 extent 24 no-undo.
def var v-bol    like v-s-bol                   extent 60.

def var v-carrier       like carrier.dscr no-undo.
def var v-to-ship       like oe-boll.qty no-undo.
def var v-tot-pkgs      as   int format ">>>>>9" no-undo.
def var v-tot-pals      like v-tot-pkgs.
def var v-tot-wght      like v-tot-pkgs.
def var v-lines         as   int no-undo.
def var v-page-tot      as   int init 0 no-undo.
def var v-last          as   log no-undo.
def var v-bol-list      as   char format "x(75)".

def workfile w-bol field rec-id as recid.

def stream last-page.

form oe-boll.i-no   format "x(15)"      at 9
     itemfg.i-name  format "x(30)"      at 26
     v-to-ship      format ">>>,>>>"    to 66
     "_______"                          at 71 skip

    with frame ln-s down no-box no-labels width 90.

tmpstore = fill("-",80).

format skip(1)
       "  Enter Customer:" v-cust space(5) "Enter Ship To:" v-ship
       skip(1)
       "  BOL Ranges: Start    End     BOL List" skip
       v-s-bol[01] at 14 v-s-bol[02] space(5) v-bol[01 for 05] space(2)     skip
       v-s-bol[03] at 14 v-s-bol[04] space(5) v-bol[06 for 05]              skip
       v-s-bol[05] at 14 v-s-bol[06] space(5) v-bol[11 for 05]              skip
       v-s-bol[07] at 14 v-s-bol[08] space(5) v-bol[16 for 05]              skip
       v-s-bol[09] at 14 v-s-bol[10] space(5) v-bol[21 for 05]              skip
       v-s-bol[11] at 14 v-s-bol[12] space(5) v-bol[26 for 05]              skip
       v-s-bol[13] at 14 v-s-bol[14] space(5) v-bol[31 for 05]              skip
       v-s-bol[15] at 14 v-s-bol[16] space(5) v-bol[36 for 05]              skip
       v-s-bol[17] at 14 v-s-bol[18] space(5) v-bol[41 for 05]              skip
       v-s-bol[19] at 14 v-s-bol[20] space(5) v-bol[46 for 05]              skip
       v-s-bol[21] at 14 v-s-bol[22] space(5) v-bol[51 for 05]              skip
       v-s-bol[23] at 14 v-s-bol[24] space(5) v-bol[56 for 05]              skip
       skip(1)

    with title " TRUCKLOAD BOL SUMMARY "
	 frame selec centered overlay no-labels
	 row 2.

outers:
do while true on error undo, leave with frame selec:
  update v-cust
	 v-ship
	 v-s-bol
	 v-bol
	 edit-blok: editing.

    readkey.
    hide message no-pause.

    if keyfunction(lastkey) eq "end-error" then
      if frame-field eq "v-cust" then undo, leave outers.
      else undo, retry.

    if (keyfunction(lastkey) eq "return" and
	(frame-field eq "v-cust" or
	 (frame-field eq "v-bol" and frame-index eq 60))) or
       keyfunction(lastkey) eq "go" then do:
      find first cust
	  where cust.company eq cocode
	    and cust.cust-no eq input v-cust
	  no-lock no-error.
      if not avail cust then do:
	bell.
	message "ERROR: Must enter a valid customer number".
	next-prompt v-cust.
	next.
      end.
      display caps(cust.cust-no) @ v-cust.
    end.

    if (keyfunction(lastkey) eq "return" and
	(frame-field eq "v-ship" or
	 (frame-field eq "v-bol" and frame-index eq 60))) or
       keyfunction(lastkey) eq "go" then do:
      find first shipto
	  where shipto.company eq cocode
	    and shipto.cust-no eq input v-cust
	    and shipto.ship-id eq input v-ship
	  use-index ship-id no-lock no-error.
      if not avail shipto then do:
	bell.
	message "ERROR: Must enter a valid ship-to for this customer".
	next-prompt v-ship.
	next.
      end.
      display caps(shipto.ship-id) @ v-ship.
    end.

    if (keyfunction(lastkey) eq "return" and
	frame-field eq "v-bol") or
       keyfunction(lastkey) eq "go" then
    do i = (if keyfunction(lastkey) eq "go" then 1 else frame-index)
	   to frame-index.
      if input v-bol[i] gt 0 then do:
	find first oe-bolh
	    where oe-bolh.company eq cocode
	      and oe-bolh.cust-no eq input v-cust
	      and oe-bolh.bol-no  eq input v-bol[i]
	      and oe-bolh.printed eq yes
	      and oe-bolh.posted  eq no
	    use-index cust no-lock no-error.
	if not avail oe-bolh then do:
	  bell.
	  message "ERROR: Must enter printed BOL for this customer".
	  next-prompt v-bol[i].
	  next edit-blok.
	end.
      end.
    end.

    if keyfunction(lastkey) eq "go" and
       frame-field eq "v-s-bol"     then do:
      next-prompt v-bol[1].
      next.
    end.

    apply lastkey.
  end.

  output stream last-page to value("bolsumm.txt") page-size 55.

  do i = 1 to 23 by 2:
    if v-s-bol[i + 1] gt 0 then
    for each oe-bolh
	where oe-bolh.company eq cocode
	  and oe-bolh.cust-no eq v-cust
	  and oe-bolh.bol-no  ge v-s-bol[i]
	  and oe-bolh.bol-no  le v-s-bol[i + 1]
	  and oe-bolh.printed eq yes
	  and oe-bolh.posted  eq no
	use-index cust no-lock:
      create w-bol.
      w-bol.rec-id = recid(oe-bolh).
    end.
  end.

  do i = 1 to 60:
    if v-bol[i] gt 0 then
    for each oe-bolh
	where oe-bolh.company eq cocode
	  and oe-bolh.cust-no eq v-cust
	  and oe-bolh.bol-no  eq v-bol[i]
	  and oe-bolh.printed eq yes
	  and oe-bolh.posted  eq no
	use-index cust no-lock:
      create w-bol.
      w-bol.rec-id = recid(oe-bolh).
    end.
  end.

  i = 1.

  for each w-bol,

      first oe-bolh
      where recid(oe-bolh) eq w-bol.rec-id
      no-lock

      break by w-bol.rec-id:

    if last-of(w-bol.rec-id) then do:
      if oe-bolh.tot-pallets ne ? then
	v-tot-pals = v-tot-pals + oe-bolh.tot-pallets.
      if oe-bolh.tot-wt ne ? then
	v-tot-wght = v-tot-wght + oe-bolh.tot-wt.
    end.

    else delete w-bol.
  end.

  format header
	 skip(5)
	 "SHIP TO:" at 41 shipto.ship-id to 70
	 shipto.ship-name  at 46
	 shipto.ship-addr[1] at 46
	 shipto.ship-addr[2] at 46
	 shipto.ship-city at 46
	 shipto.ship-state
	 shipto.ship-zip
	 skip(1)
	 "PAGE" at 63 page-number to 69 format "99"
	 "OF" at 71 v-page-tot to 75 format "99" skip
	 tmpstore at 1 skip
	 "ITEM NUMBER" at 9 "PRODUCT DESCRIPTION" at 26
	 "QUANTITY IN CARTONS" at 59 skip
	 "---------------" at 9
	 "------------------------------" at 26
	 "---------------------" at 58 skip
	 "TO SHIP" at 59 "SHIPPED" at 71 skip
	 "----------" at 58 "---------" at 70 skip

      with frame head no-box no-labels page-top width 100.

  view frame head.
  view stream last-page frame head.  /* Print headers */

  page stream last-page.

  {oe/rep/bolsumm.i "stream last-page"}

  v-page-tot = page-number (last-page).

  assign
   v-lines    = 0
   v-to-ship  = 0
   v-tot-pkgs = 0.

  page.

  {oe/rep/bolsumm.i}

  output stream last-page close.

  leave.
end.

hide all no-pause.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
