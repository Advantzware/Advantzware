/* ------------------------------------------------- ce/com/pr4-ink.p 7/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

def buffer ink2 for ink.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

def var iqty as de NO-UNDO.
def var icost as de NO-UNDO.
def var ipct as de NO-UNDO.
def var gqty as de NO-UNDO.
def var gcost as de NO-UNDO.
def var gpct as de NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var g-qty  as de  NO-UNDO.
def var g-cost as de format ">>,>>9.99" NO-UNDO.
def var v-1st-frm as log init yes NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
def var v-col-p as int NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF VAR v-first-pass AS LOG NO-UNDO.

define TEMP-TABLE glu NO-UNDO
   field id as ch
   field snum as int
   field bnum as int
   field i-code as ch  format "x(10)"
   field i-dscr as ch  format "x(19)"
   field i-%    as int format ">>9"
   field i-qty  as de  format ">>>9.99"
   field i-cost as de  format ">>,>>9.99".

define buffer xglu for glu.

DEF TEMP-TABLE tt-ink NO-UNDO FIELD i-code LIKE ink.i-code
                              FIELD i-dscr LIKE ink.i-dscr
                              FIELD pass AS INT.

DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

/* i n k s */

for each xef where xef.company = xest.company
               AND xef.est-no = xest.est-no:
   v-1st-frm = YES.

   EMPTY TEMP-TABLE tt-ink.

   for each xeb where
       xeb.company = xest.company AND
       xeb.est-no = xest.est-no and
       xeb.form-no = xef.form-no
       BREAK BY xeb.form-no
             BY xeb.blank-no:

      v-num-up = xeb.num-up.

      {ce/pr4-ink.i xeb}

      {ce/pr4-adh.i xeb}
   end.

   j = 0.
   FOR EACH est-op
       WHERE est-op.company EQ xef.company
         AND est-op.est-no  EQ xef.est-no
         AND (est-op.qty    EQ v-op-qty OR xest.est-type NE 1)
         AND est-op.s-num   EQ xef.form-no
         AND est-op.line    GE 500
         AND (est-op.dept   EQ "PR" OR est-op.dept EQ "CT")
       NO-LOCK,
       FIRST mach
       {sys/ref/machW.i}
         AND mach.m-code EQ est-op.m-code
       NO-LOCK
       BY est-op.line:
     j = j + 1.
     FOR EACH tt-ink WHERE tt-ink.pass EQ j,
         FIRST ink
         WHERE ink.i-code EQ tt-ink.i-code
           AND ink.i-dscr EQ tt-ink.i-dscr
           AND ink.snum   EQ est-op.s-num:

       ink.i-qty = ink.i-qty + mach.ink-waste.
     END.
   END.
end.

for each ink where ink.snum NE 0 AND ink.bnum ne 0:
  find first ink2
      where ink2.i-code eq ink.i-code
        and ink2.i-dscr eq ink.i-dscr
	    and ink2.snum   eq 0
	    and ink2.bnum   eq 0
      no-error.
  if not avail ink2 then do:
    create ink2.
    assign
     ink2.id     = ink.id
     ink2.snum   = 0
     ink2.bnum   = 0
     ink2.i-code = ink.i-code
     ink2.i-dscr = ink.i-dscr
     ink2.i-qty  = 0.
  end.

  ink2.i-qty = ink2.i-qty + ink.i-qty.
end.

for each ink where ink.snum EQ 0 AND ink.bnum eq 0 by ink.i-code by ink.i-dscr:
  find first item
      where item.company eq cocode
	    and item.i-no    eq ink.i-code
      no-lock no-error.
  if not avail item then next.
  find first e-item of item no-lock no-error.
  if ink.i-qty lt item.min-lbs then ink.i-qty = item.min-lbs.

  {est/matcost.i ink.i-qty ink.i-cost ink}

  ink.i-cost = (ink.i-cost * ink.i-qty) + lv-setup-ink.

  for each ink2
      where ink2.i-code eq ink.i-code
        and ink2.i-dscr eq ink.i-dscr
        and ink2.snum   ne 0
        and ink2.bnum   ne 0:

    IF ink.i-qty NE 0 THEN
       ink2.i-cost = (ink.i-cost / ink.i-qty) * ink2.i-qty.
  end.
end.

for each ink where ink.snum NE 0 AND ink.bnum ne 0:
   FIND FIRST xeb NO-LOCK
       WHERE xeb.company  EQ xest.company
         AND xeb.est-no   EQ xest.est-no
         AND xeb.form-no  EQ ink.snum
         AND xeb.blank-no EQ ink.bnum
       NO-ERROR.

   IF xeb.pur-man THEN
     ASSIGN
      ld-rm = rm-rate-f
      ld-hp = hand-pct-f.
   ELSE
     ASSIGN
      ld-rm = ctrl[3]
      ld-hp = ctrl[2].

   /* rm handling chg per cwt */
   if ld-rm ne 0 then ctrl2[3] = ctrl2[3] + ((ink.i-qty / 100) * ld-rm).
   /* rm handling pct. */
   if ld-hp ne 0 then ctrl2[2] = ctrl2[2] + (ink.i-cost * ld-hp).

      find first blk where blk.id = ink.id and
			 blk.snum = ink.snum and
			 blk.bnum = ink.bnum no-error.
      if avail blk then do:
	     /* assign proper cost share of this ink to blk record */
	     if ink.i-qty ne item.min-lbs then xxx = ink.i-qty.

         IF xxx NE 0 THEN
            ASSIGN
	           blk.cost = blk.cost + ((ink.i-qty / xxx) * ink.i-cost)
	           ink.i-cost = (ink.i-qty / xxx) * ink.i-cost.
         

         ASSIGN
	        blk.cost = blk.cost + ((ink.i-qty / 100) * ld-rm)
	          	     + (ink.i-cost * ld-hp)
	        blk.lab = blk.lab + ((ink.i-qty / 100) * ld-rm)
	                + (ink.i-cost * ld-hp).
      end.

  find first brd
      where brd.form-no  = ink.snum
        and brd.blank-no = ink.bnum
        and brd.i-no     = ink.i-code
	  no-error.
  if not avail brd then do:
     create brd.
     assign brd.form-no  = ink.snum
	    brd.blank-no = ink.bnum
	    brd.i-no     = ink.i-code
	    brd.dscr     = ink.i-dscr
	    brd.basis-w  = item.basis-w.
  end.

  ASSIGN
     brd.qty = brd.qty + ink.i-qty
     brd.qty-uom = "LB"
     brd.sc-uom  = "LB"
     brd.amount = brd.amount + ink.i-cost.

  IF ink.i-qty NE 0 THEN
     brd.cost = ink.i-cost / ink.i-qty.

  IF tt-blk NE 0 THEN
     brd.cost-m = ink.i-cost / (tt-blk / 1000).

  delete ink.
end.

for each ink by ink.i-dscr
    with frame ab down no-labels no-box:

   IF tt-blk NE 0 THEN
      dm-tot[4] = dm-tot[4] + (ink.i-cost / (tt-blk / 1000)).

   dm-tot[5] = dm-tot[5] + ink.i-cost.
   IF NOT gEstSummaryOnly THEN
   display "*" space(5)
	   ink.i-dscr   space(13)
	   ink.i-qty  to 50 space(0) " Lbs"
	   ink.i-cost / (tt-blk / 1000) to 69 WHEN tt-blk NE 0
       0 TO 69 WHEN tt-blk EQ 0
	   ink.i-cost format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
end.

for each glu break by glu.i-code /*BY glu.snum*/ with frame abc down no-labels no-box:

   if not first-of(glu.i-code) /*AND NOT FIRST-OF(glu.snum)*/ then next.   /* task 05191405 */
   find first item where item.company = cocode and
			 item.i-no    = glu.i-code no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   else next.

   FIND FIRST xeb NO-LOCK
       WHERE xeb.company  EQ xest.company
         AND xeb.est-no   EQ xest.est-no
         AND xeb.form-no  EQ glu.snum
         AND xeb.blank-no EQ glu.bnum
       NO-ERROR.

   IF xeb.pur-man THEN
     ASSIGN
      ld-rm = rm-rate-f
      ld-hp = hand-pct-f.
   ELSE
     ASSIGN
      ld-rm = ctrl[3]
      ld-hp = ctrl[2].

   gqty  = 0.
   gcost = 0.
   for each xglu where xglu.i-code = glu.i-code:
      gqty = gqty + xglu.i-qty.   /* total pounds */
   end.

   {est/matcost.i gqty gcost glue}

   ASSIGN
    gcost     = (gcost * gqty) + lv-setup-glue
    dm-tot[5] = dm-tot[5] + gcost.

   /* rm handling chg per cwt */
   if ld-rm ne 0 then ctrl2[3] = ctrl2[3] + ((gqty / 100) * ld-rm).
   /* rm handling pct. */
   if ld-hp ne 0 then ctrl2[2] = ctrl2[2] + (gcost * ld-hp).

   find first brd where brd.form-no = glu.snum and
			brd.blank-no = glu.bnum and
			brd.i-no    = glu.i-code
			no-error.
   if not avail brd then
   do:
      create brd.
      assign brd.form-no = glu.snum
	     brd.blank-no = glu.bnum
	     brd.i-no    = glu.i-code
	     brd.dscr    = glu.i-dscr
	     brd.basis-w = item.basis-w.
   end.

   ASSIGN
      brd.qty = brd.qty + gqty
      /* Changed default from Lbs to LB  FWK  2/28/96 */
      brd.qty-uom = "LB"
      brd.sc-uom = "LB"
      brd.amount = brd.amount + gcost .

   IF gqty NE 0 THEN
      brd.cost = gcost / gqty.

   IF tt-blk NE 0 THEN
      brd.cost-m = gcost / (tt-blk / 1000).

   IF NOT gEstSummaryOnly THEN
   display "*" space(5)
	   item.i-name
	   gqty  to 50 space(0) " Lbs" gcost / (tt-blk / 1000) to 69 WHEN tt-blk NE 0
       gqty TO 50 space(0) " Lbs" 0 to 69 WHEN tt-blk EQ 0
	   gcost format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.

   for each xglu where xglu.i-code = glu.i-code:
      find first blk where blk.id = xglu.id and
			 blk.snum = xglu.snum and
			 blk.bnum = xglu.bnum no-error.
      if avail blk then do:
        IF blk.pur-man THEN
          ASSIGN
           ld-rm = rm-rate-f
           ld-hp = hand-pct-f.
        ELSE
          ASSIGN
           ld-rm = ctrl[3]
           ld-hp = ctrl[2].

	 /* assign proper cost share of this glu to blk record */

     IF gqty NE 0 THEN
        ASSIGN
	       blk.cost = blk.cost + ((xglu.i-qty / gqty) * gcost)
	       xglu.i-cost = (xglu.i-qty / gqty) * gcost.

     ASSIGN
	    blk.cost = blk.cost + ((xglu.i-qty / 100) * ld-rm)
		         + (xglu.i-cost * ld-hp)
	    blk.lab = blk.lab + ((xglu.i-qty / 100) * ld-rm)
		        + (xglu.i-cost * ld-hp).
      end.
   end.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
