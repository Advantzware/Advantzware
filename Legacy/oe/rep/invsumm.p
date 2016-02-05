/* ----------------------------------------------- oe/rep/invsumm.p 06/99 JLF */
/* Master Invoice Summary                                                     */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def var v-cust   like cust.cust-no.
def var v-s-inv  as int  format ">>>>>>" init 0 extent 24 no-undo.
def var v-inv    like v-s-inv                   extent 60.

def var v-shipvia       like carrier.dscr no-undo.
def var v-addr3         as   char format "x(30)" no-undo.
def var v-terms         like inv-head.terms-d no-undo.
def var v-frt-pay       as   char no-undo.
def var v-lines         as   int no-undo.
def var v-last-page     as   int init 0 no-undo.
def var v-page-tot      as   int init 0 no-undo.
def var v-last          as   log no-undo.
def var v-inv-list      as   char format "x(75)" no-undo.
def var v-inv-qty       as   dec no-undo.
def var v-subtot-lines  as   dec format "->>>,>>9.99".
def var v-subtot-misc   as   dec format "->>>,>>9.99".
def var v-cs-qty        like oe-boll.cases no-undo.
def var v-cs-ship-qty   like inv-line.ship-qty no-undo.
def var v-bo-qty        as   int format "99999" no-undo.
def var v-price         as   dec format ">>>>9.9999" no-undo.
def var v-t-price       as   dec format ">>>>>>9.99" no-undo.
def var v-t-inv-tax     like inv-head.t-inv-tax no-undo.
def var v-bol-cases     like oe-boll.cases no-undo.
def var v-price-head    as   char format "x(8)" no-undo.
def var v-price-per     as   char format "x(8)" no-undo.
def var v-t-inv-weight  like inv-head.t-inv-weight no-undo.
def var v-t-inv-rev     like inv-head.t-inv-rev no-undo.
def var v-frt-display   like inv-head.t-inv-freight no-undo.

def workfile w-inv field rec-id as recid.

def stream last-page.

form inv-line.i-no                      at 1
     itemfg.i-name                      at 17
     v-cs-qty                           to 54   format ">>>>>>9"
     v-cs-ship-qty                      to 62   format ">>>>>>9"
     v-price                            to 70   format ">>>9.99"
     itemfg.i-dscr                      at 17   format "x(30)"
     v-bo-qty                           to 54   format ">>>>>>9"
     v-t-price                          to 80   format ">>>>>9.99"
     skip(1)

    with frame detail no-attr-space no-labels no-box no-underline down width 80.

form header
     " "
     skip(8)

    with frame inv-bot1 page-bottom no-box no-underline width 100.

form header
     "____________"                     at 69
     "SUB-TOTAL"                        at 58
     v-subtot-lines                     to 80   format "->>>,>>9.99"
     "SALES TAX"                        at 58
     v-t-inv-tax                        to 80   format "->>>,>>9.99"
     "FREIGHT"                          at 58
     v-frt-display                      to 80   format "->>>,>>9.99"
     "UNIT OF MEASURE"                  at 2
     "TOTAL CARTONS"                    at 26
     v-bol-cases                        to 46   format ">>>,>>9"
     "SUNDRY"                           at 58
     v-subtot-misc                      to 80   format "->>>,>>9.99"
     "QUANTITY IN"                      at 2
     v-price-head                       at 14
     "------------"                     at 69
     "PRICED PER"                       at 2
     v-price-per                        at 14
     "NET WEIGHT"                       at 28
     v-t-inv-weight                     to 46   format ">>>,>>9"
     "INVOICE TOTAL"                    at 55
     v-t-inv-rev                        to 80   format "$->>>,>>9.99"
     "============"                     at 69

    with frame inv-bot2 page-bottom no-box no-underline width 100.

tmpstore = fill("-",80).

format skip(1)
       "  Enter Customer:" v-cust
       skip(1)
       "  Invoice Ranges: Start    End     Invoice List" skip
       v-s-inv[01] at 18 v-s-inv[02] space(5) v-inv[01 for 05] space(2)     skip
       v-s-inv[03] at 18 v-s-inv[04] space(5) v-inv[06 for 05]              skip
       v-s-inv[05] at 18 v-s-inv[06] space(5) v-inv[11 for 05]              skip
       v-s-inv[07] at 18 v-s-inv[08] space(5) v-inv[16 for 05]              skip
       v-s-inv[09] at 18 v-s-inv[10] space(5) v-inv[21 for 05]              skip
       v-s-inv[11] at 18 v-s-inv[12] space(5) v-inv[26 for 05]              skip
       v-s-inv[13] at 18 v-s-inv[14] space(5) v-inv[31 for 05]              skip
       v-s-inv[15] at 18 v-s-inv[16] space(5) v-inv[36 for 05]              skip
       v-s-inv[17] at 18 v-s-inv[18] space(5) v-inv[41 for 05]              skip
       v-s-inv[19] at 18 v-s-inv[20] space(5) v-inv[46 for 05]              skip
       v-s-inv[21] at 18 v-s-inv[22] space(5) v-inv[51 for 05]              skip
       v-s-inv[23] at 18 v-s-inv[24] space(5) v-inv[56 for 05]              skip
       skip(1)

    with title "  MASTER INVOICE SUMMARY  "
	 frame selec centered overlay no-labels
	 row 2.

outers:
do while true on error undo, leave with frame selec:
  update v-cust
	 v-s-inv
	 v-inv
	 edit-blok: editing.

    readkey.
    hide message no-pause.

    if keyfunction(lastkey) eq "end-error" then
      if frame-field eq "v-cust" then undo, leave outers.
      else undo, retry.

    if (keyfunction(lastkey) eq "return" and
	(frame-field eq "v-cust" or
	 (frame-field eq "v-inv" and frame-index eq 60))) or
       keyfunction(lastkey) eq "go" then do:
      find cust
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
	frame-field eq "v-inv") or
       keyfunction(lastkey) eq "go" then
    do i = (if keyfunction(lastkey) eq "go" then 1 else frame-index)
	   to frame-index.
      if input v-inv[i] gt 0 then do:
	find first inv-head
	    where inv-head.company eq cocode
	      and inv-head.cust-no eq input v-cust
	      and inv-head.inv-no  eq input v-inv[i]
	      and inv-head.printed eq yes
	      and inv-head.posted  eq no
	    use-index cust no-lock no-error.
	if not avail inv-head then do:
	  bell.
	  message "ERROR: Must enter printed Invoice for this customer".
	  next-prompt v-inv[i].
	  next edit-blok.
	end.
      end.
    end.

    if keyfunction(lastkey) eq "go" and
       frame-field eq "v-s-inv"     then do:
      next-prompt v-inv[1].
      next.
    end.

    apply lastkey.
  end.

  if keyfunction(lastkey) eq "end-error" then next outers.

  output stream last-page to value("invsumm.txt") page-size 62.

  do i = 1 to 23 by 2:
    if v-s-inv[i + 1] gt 0 then
    for each inv-head
	where inv-head.company eq cocode
	  and inv-head.cust-no eq v-cust
	  and inv-head.inv-no  ge v-s-inv[i]
	  and inv-head.inv-no  le v-s-inv[i + 1]
	  and inv-head.printed eq yes
	  and inv-head.posted  eq no
	use-index cust no-lock:
      create w-inv.
      w-inv.rec-id = recid(inv-head).
    end.
  end.

  do i = 1 to 60:
    if v-inv[i] gt 0 then
    for each inv-head
	where inv-head.company eq cocode
	  and inv-head.cust-no eq v-cust
	  and inv-head.inv-no  eq v-inv[i]
	  and inv-head.printed eq yes
	  and inv-head.posted  eq no
	use-index cust no-lock:
      create w-inv.
      w-inv.rec-id = recid(inv-head).
    end.
  end.

  i = 1.

  for each w-inv,

      first inv-head
      where recid(inv-head) eq w-inv.rec-id
      no-lock

      break by w-inv.rec-id:

    if not last-of(w-inv.rec-id) then delete w-inv.
  end.

  find first shipto
      where shipto.company eq cocode
	and shipto.cust-no eq input v-cust
      use-index ship-id no-lock no-error.
  find first terms
      where terms.company eq cocode
	and terms.t-code  eq cust.terms
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
	and carrier.carrier eq cust.carrier
      no-lock no-error.
  assign
   v-shipvia = if avail carrier then carrier.dscr else ""
   v-addr3   = cust.city + ", " + cust.state + "  " + cust.zip
   v-terms   = if avail terms then terms.dscr else ""
   v-frt-pay = if cust.frt-pay eq "P" then "PREPAID" else
	       if cust.frt-pay eq "C" then "COLLECT" else
	       if cust.frt-pay eq "B" then "PPD/CHG" else "".

  form header
       skip(3)
       "PAGE" at 66 page-number - v-last-page   to 72       format "99"
       "OF"                                     at 74
       v-page-tot to 78                                     format "99"
       skip(1)
       "BILL TO:"                               at 10
       cust.name                                at 10       format "x(25)"  skip
       cust.addr[1]                             at 10       format "x(25)"  skip
       cust.addr[2]                             at 10       format "x(25)"  skip
       v-addr3                                  at 10       format "x(25)"
       skip(2)
       "TERMS"                                  at 44
       v-terms                                  at 50       format "x(30)"  skip
       "SHIPPED VIA"                            at 13
       v-shipvia                                            format "x(30)"
       "  "
       "        "
       space(2)
       v-frt-pay
       tmpstore                                             format "x(80)"
       skip(1)
       "QUANTITY"                               at 51
       "UNIT"                                   at 66
       "EXTENDED"                               at 73
       "ITEM NUMBER"                            at 1
       "PRODUCT DESCRIPTION"                    at 17
       "ORDERED SHIPPED"                        at 48
       "PRICE"                                  at 65
       "PRICE"                                  at 74
       "-------------"                          at 1
       "---------------------"                  at 17
       "B/ORDER"                                at 48
       "-------"                                at 56
       "-------"                                at 64
       "---------"                              at 72
       "-------"                                at 48

      with frame head no-box no-labels page-top width 100.

  {oe/rep/invsumm.i "stream last-page"}

  v-page-tot = page-number (last-page).

  {oe/rep/invsumm.i}

  page.

  output stream last-page close.

  leave.
end.

hide all no-pause.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
