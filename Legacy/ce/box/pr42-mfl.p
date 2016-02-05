/* ---------------------------------------------- ce/box/pr42-mfl.p  7/92 cd  */
/*                                                                            */
/* box- 2-sheets                                                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.

def var rm-wt$ as de.
def var rm-wt% as de.
def var rm-wt  as de.
def var med-qty as de.
def var mqty as de.
DEF VAR med-len LIKE xef.nsh-len.
DEF VAR med-wid LIKE xef.nsh-wid.
def var m-waste as de.
def var m-spo   as de.
def var mfl$ as de format ">>>>9.99".
def var call_id as recid no-undo.
def var ttt as de.
def var cumul as de.
def var prev-mach as ch.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

IF xeb.pur-man THEN
  ASSIGN
   ld-rm = rm-rate-f
   ld-hp = hand-pct-f.
ELSE
  ASSIGN
   ld-rm = ctrl[3]
   ld-hp = ctrl[2].

find first ce-ctrl {sys/look/ce-ctrl.w} no-lock no-error.

ASSIGN
call_id = recid(xeb)
ttt = qty / xeb.num-up
cumul = ttt.

for each est-op
    where est-op.company eq xest.company
      and est-op.est-no  eq xest.est-no
      and est-op.line    gt 500
    by est-op.line descending:
   find first mach {sys/look/mach.w} and mach.m-code = est-op.m-code
   no-lock no-error.
   if prev-mach = "LM" and mach.dept[1] ne "LM" then leave.
   if est-op.op-sb = no THEN
      ASSIGN
      spo     = ((cumul * xeb.num-up) / (1 - (est-op.op-spoil / 100))
				   - (cumul * xeb.num-up)) / xeb.num-up
      m-spo   = m-spo   + spo
      m-waste = m-waste +       (est-op.op-waste / xeb.num-up)
      cumul   = cumul   + spo + (est-op.op-waste / xeb.num-up).
   
   ELSE
      ASSIGN
      spo     = (cumul / (1 - (est-op.op-spoil / 100)) - cumul)
      m-spo   = m-spo + spo
      m-waste = m-waste + est-op.op-waste
      cumul   = cumul + spo + est-op.op-waste.
   
   prev-mach = mach.dept[1].
end.
/*mqty = cumul.*/

      xxx = mqty.
      if xxx > integer(xxx) then xxx = integer(xxx) + 1.
      else xxx = integer(xxx).
      ASSIGN
      mqty = xxx
      xxx = m-waste.
      if xxx > integer(xxx) then xxx = integer(xxx) + 1.
      else xxx = integer(xxx).
      ASSIGN
      m-waste = xxx
      xxx = m-spo.
      if xxx > integer(xxx) then xxx = integer(xxx) + 1.
      else xxx = integer(xxx).
      m-spo = xxx.

if xef.medium ne "" then
do with no-box no-labels STREAM-IO frame med1:
   find first item {sys/look/item.w} and
		     item.i-no = xef.medium no-lock no-error.
   /*override item shrink % with shrink entered in BOM button on Layout screen*/
  IF xef.spare-dec-1 NE 0 
      THEN dShrink = xef.spare-dec-1.
      ELSE dShrink = ITEM.shrink.
   if xef.n-out-l = 0 THEN
      ASSIGN
      med-len = brd-l[3]
      med-wid = brd-w[3] / (1 - (dShrink / 100)). /* proper length */
   
   ELSE
      ASSIGN
      med-wid = brd-w[3]
      med-len = brd-l[3] / (1 - (dShrink / 100)). /* proper length */
   
   med-qty = ((med-wid * med-len) * mqty) / 144000. /*now msf*/

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ medium}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-medium
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ( (mfl$ / (mqty / xeb.num-up)) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$

    /* add run spoil & waste */
    dm-tot[5] = dm-tot[5] + (m-spo   * (mfl$ / (mqty / xeb.num-up))) +
	       	    (m-waste * (mfl$ / (mqty / xeb.num-up))).

   display
	 space(5)
	 xef.medium
	 item.basis-w to 32
	 item.cal format ">9.999<" to 39
	 "$" + string(mfl$ / ((mqty / xeb.num-up) / 1000),">>>9.99") to 50
		space(0) " MShts" space(0)
	 mfl$ / (mqty / 1000) format ">>>9.99" to 69
	 mfl$ format ">>>>,>>9.99" to 80 skip
	 space(5)
	 "Medium MR  Waste"
	 m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>9.99" to 61
	 /* cost of waste amortized per 1000 of all blanks on this form */
	 ((mfl$ / (mqty / xeb.num-up)) * m-waste) / (mqty / 1000)
	    format ">>>9.99" to 69
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>>,>>9.99" to 80 skip
	 space(5)
	 "Medium RUN Waste"
	 m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
	 /* cost of spoil amortized per 1000 of all blanks on this form */
	 (m-spo * (mfl$ / (mqty / xeb.num-up))) / (mqty / 1000)
					      format ">>>9.99" to 69
	 m-spo * (mfl$ / (mqty / xeb.num-up)) format ">>>>,>>9.99" to 80 skip.

   find first BRD where BRD.form-no = xef.form-no and
			BRD.i-no    = xef.medium
			no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
	     BRD.blank-no = 00
	     BRD.i-no    = xef.medium
	     BRD.dscr    = xef.medium
	     BRD.basis-w = item.basis-w.
   end.
   ASSIGN
   BRD.qty = BRD.qty + ((qty / xeb.num-up) + m-waste + m-spo)
   BRD.qty-uom = "Sht"
   BRD.sc-uom = "MSH"
   BRD.cost = mfl$ / (qty / 1000)
   BRD.cost-m = mfl$ / (qty / 1000)
   brd.len  = med-len
   brd.wid  = med-wid.

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
      ASSIGN
      rm-wt = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
		       ((m-spo * brd-sf[3]) / 1000)) * item.basis-w
      rm-wt$ = (rm-wt / 100) * ld-rm
      ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   if ld-hp ne 0 then
      ASSIGN
      rm-wt% = ( mfl$ +
	       ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
	       ((mfl$ / (mqty / xeb.num-up)) * m-spo) )
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   
   for each xeb
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no
         and xeb.form-no eq xef.form-no:
      find first blk where blk.snum = xeb.form-no and
			   blk.bnum = xeb.blank-no and
			   blk.id   = xeb.part-no no-error.
      if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
      if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
      blk.cost = blk.cost + ((mfl$ + ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
				     (m-spo * (mfl$ / (mqty / xeb.num-up)))   +
				      rm-wt$ + rm-wt%) * blk.pct).
   end.
end.

find xeb where recid(xeb) = call_id no-lock no-error.

if xef.flute ne "" then
do with no-box no-labels STREAM-IO frame flute:
   find first item {sys/look/item.w} and
		     item.i-no = xef.flute no-lock no-error.

   med-qty = ((brd-l[3] * brd-w[3]) * (mqty / xeb.num-up)) / 144000. /*now msf*/

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ flute}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-flute
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ( (mfl$ / (qty / xeb.num-up)) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$

    /* add run spoil */
    dm-tot[5] = dm-tot[5] + (m-spo   * (mfl$ / (mqty / xeb.num-up))) +
	 		    (m-waste * (mfl$ / (mqty / xeb.num-up))).

   if xef.medium = "" then tmpstore = "Flute ".
   else tmpstore = "Liner ".
   display
      /*   string(xef.form-no,"9") + "-00" format "x(4)" */
	 space(5)
	 xef.flute
	 item.basis-w to 32
	 item.cal format ">9.999<" to 39
	 "$" + string(mfl$ / ((mqty / xeb.num-up) / 1000),">>>9.99") to 50
		space(0) " MShts" space(0)
	 mfl$ / (mqty / 1000) format ">>>9.99" to 69
	 mfl$ format ">>>>,>>9.99" to 80 skip
	 space(5)
	 tmpstore + "MR  Waste" format "x(15)"
	 m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>9.99" to 61
	 /* cost of waste amortized per 1000 of all blanks on this form */
	 ((mfl$ / (mqty / xeb.num-up)) * m-waste) / (mqty / 1000)
	    format ">>>9.99" to 69
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>>,>>9.99" to 80 skip
	 space(5)
	 tmpstore + "RUN Waste" format "x(15)"
	 m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
	 /* cost of spoil amortized per 1000 of all blanks on this form */
	 (m-spo * (mfl$ / (mqty / xeb.num-up))) / (mqty / 1000)
					      format ">>>9.99" to 69
	 m-spo * (mfl$ / (mqty / xeb.num-up)) format ">>>>,>>9.99" to 80 skip.

   find first BRD where BRD.form-no = xef.form-no and
			BRD.i-no    = xef.flute
			no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
	     BRD.blank-no = 00
	     BRD.i-no    = xef.flute
	     BRD.dscr    = xef.flute
	     BRD.basis-w = item.basis-w.
   end.
   ASSIGN
   BRD.qty = BRD.qty + ((qty / xeb.num-up) + m-waste + m-spo)
   BRD.qty-uom = "Sht"
   BRD.sc-uom = "MSH"
   BRD.cost = mfl$ / (qty / 1000)
   BRD.cost-m = mfl$ / (qty / 1000)
   BRD.len  = brd-l[3]
   BRD.wid  = brd-w[3].

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
      ASSIGN
      rm-wt = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
		       ((m-spo   * brd-sf[3]) / 1000)) * item.basis-w
      rm-wt$ = (rm-wt / 100) * ld-rm
      ctrl2[3] = ctrl2[3] + rm-wt$.
   
   /* rm handling pct. */
   if ld-hp ne 0 then
      ASSIGN
      rm-wt% = ( mfl$ +
	       ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
	       ((mfl$ / (mqty / xeb.num-up)) * m-spo) )
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   
   for each xeb
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no
         and xeb.form-no eq xef.form-no:
      find first blk where blk.snum = xeb.form-no and
			   blk.bnum = xeb.blank-no and
			   blk.id   = xeb.part-no no-error.
      if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
      if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
      blk.cost = blk.cost + ((mfl$ + ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
				     (m-spo * (mfl$ / (mqty / xeb.num-up)))   +
				      rm-wt$ + rm-wt%) * blk.pct).
   end.
end.

find xeb where recid(xeb) = call_id no-lock no-error.
/*
if xef.lam-code ne "" then
do with no-box no-labels STREAM-IO frame lamin:
   find first item {sys/look/item.w} and
		     item.i-no = xef.lam-code no-lock no-error.

   med-qty = ((brd-l[3] * brd-w[3]) * (mqty / xeb.num-up)) / 144000. /*now msf*/

   FIND FIRST e-item OF ITEM NO-LOCK NO-ERROR.

   b-uom = IF AVAIL e-item AND e-item.std-uom NE "" THEN e-item.std-uom
                                                    ELSE item.cons-uom.

   IF b-uom EQ "TON" THEN med-qty = med-qty * item.basis-w / 2000.

   {est/matcost.i med-qty mfl$ lam-code}

   ASSIGN
    mfl$      = (mfl$ * med-qty) + lv-setup-lam-code
    b-msh     = mfl$ / med-qty
    dm-tot[3] = dm-tot[3] + ( (mfl$ / (mqty / xeb.num-up)) * m-waste)
    dm-tot[5] = dm-tot[5] + mfl$

    /* add run spoil */
    dm-tot[5] = dm-tot[5] + (m-spo   * (mfl$ / (mqty / xeb.num-up))) +
	     		(m-waste * (mfl$ / (mqty / xeb.num-up))).

   display
     /*    string(xef.form-no,"9") + "-00" format "x(4)" */
	 space(5)
	 xef.lam-code
	 item.basis-w to 32
	 item.cal format ">9.999<" to 39
	 "$" + string(mfl$ / ((mqty / xeb.num-up) / 1000),">>>9.99") to 50
		space(0) " MShts" space(0)
	 mfl$ / (mqty / 1000) format ">>>9.99" to 69
	 mfl$ format ">>>>9.99" to 80 skip
	 space(5)
	 "Laminate MR  Waste"
	 m-waste format ">>>>9" to 50 space(0) " Sht" space(0)
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>9.99" to 61
	 /* cost amortized per 1000 of all blanks on this form */
	 ((mfl$ / (mqty / xeb.num-up)) * m-waste) / (mqty / 1000)
	    format ">>>9.99" to 69
	 (mfl$ / (mqty / xeb.num-up)) * m-waste format ">>>>9.99" to 80 skip
	 space(5)
	 "Laminate RUN Waste"
	 m-spo format ">>>>9" to 50 space(0) " Sht" space(0)
	 /* cost amortized per 1000 of all blanks on this form */
	 (m-spo * (mfl$ / (mqty / xeb.num-up))) / (mqty / 1000)
					      format ">>>9.99" to 69
	 m-spo * (mfl$ / (mqty / xeb.num-up)) format ">>>>9.99" to 80 skip.

   find first BRD where BRD.form-no = xef.form-no and
			BRD.i-no    = xef.lam-code
			no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = xef.form-no
	     BRD.blank-no = 00
	     BRD.i-no    = xef.lam-code
	     BRD.dscr    = xef.lam-code
	     BRD.basis-w = item.basis-w.
   end.
   BRD.qty = BRD.qty + ((qty / xeb.num-up) + m-waste + m-spo).
   BRD.qty-uom = "Sht".
   BRD.sc-uom = "MSH".
   BRD.cost = mfl$ / (qty / 1000).
   BRD.cost-m = mfl$ / (qty / 1000).
   BRD.len  = brd-l[3].
   BRD.wid  = brd-w[3].

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
   do:
      rm-wt = (med-qty + ((m-waste * brd-sf[3]) / 1000) +
		       ((m-spo   * brd-sf[3]) / 1000)) * item.basis-w.
      rm-wt$ = (rm-wt / 100) * ld-rm.
      ctrl2[3] = ctrl2[3] + rm-wt$.
   end.
   /* rm handling pct. */
   if ld-hp ne 0 then
   do:
      rm-wt% = ( mfl$ +
	       ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
	       ((mfl$ / (mqty / xeb.num-up)) * m-spo) ).
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   end.
   for each xeb
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no
         and xeb.form-no eq xef.form-no:
      find first blk where blk.snum = xeb.form-no and
			   blk.bnum = xeb.blank-no and
			   blk.id   = xeb.part-no no-error.
      if ld-rm ne 0 then blk.lab = blk.lab + (rm-wt$ * blk.pct).
      if ld-hp ne 0 then blk.lab = blk.lab + (rm-wt% * blk.pct).
      blk.cost = blk.cost + ((mfl$ + ((mfl$ / (mqty / xeb.num-up)) * m-waste) +
				     (m-spo * (mfl$ / (mqty / xeb.num-up)))   +
				      rm-wt$ + rm-wt%) * blk.pct).
   end.
end.
*/
find xeb where recid(xeb) = call_id no-lock no-error.
call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
