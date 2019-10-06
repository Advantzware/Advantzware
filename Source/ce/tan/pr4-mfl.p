/* ----------------------------------------------- ce/tan/pr4-mfl.p  7/92 cd  */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def buffer xeb2 for eb.

{ce/print4.i shared shared}

def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var med-qty as de NO-UNDO.

def var mfl$ as de format ">>>>9.99" NO-UNDO.
def var mqty as de NO-UNDO.
DEF VAR med-len LIKE xef.nsh-len.
DEF VAR med-wid LIKE xef.nsh-wid.
def var m-waste as de NO-UNDO.
def var m-spo   as de NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF SHARED VAR CALL_id AS RECID NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEFINE VARIABLE dShrink AS DECIMAL     NO-UNDO.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

def var cumul as de.
def var prev-nsh as de.
def var prev-mach as ch.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

prev-nsh = qty / xeb.num-up.
{sys/inc/roundup.i prev-nsh}
for each est-op where est-op.company = xest.company
                  AND est-op.est-no = xest.est-no and
		   est-op.line  >= 500 by est-op.line descending:
   if prev-mach = "LM" and est-op.dept ne "LM" then leave.
   if est-op.op-sb = true
   then spo = est-op.num-sh - (prev-nsh + est-op.op-waste).
   else spo = est-op.num-sh - (prev-nsh + (est-op.op-waste / xeb.num-up)).
   m-spo   = m-spo   + spo.
   {sys/inc/roundup.i m-spo}
   if est-op.op-sb = yes then m-waste = m-waste + est-op.op-waste.
   else m-waste = m-waste + (est-op.op-waste / xeb.num-up).
   prev-nsh = est-op.num-sh.
   prev-mach = est-op.dept.
end.
mqty = qty / xeb.num-up.
{sys/inc/roundup.i mqty}
{sys/inc/roundup.i m-spo}
{sys/inc/roundup.i m-waste}

IF xeb.pur-man THEN
  ASSIGN
   ld-rm = rm-rate-f
   ld-hp = hand-pct-f.
ELSE
  ASSIGN
   ld-rm = ctrl[3]
   ld-hp = ctrl[2].

if xef.medium ne "" then
do with no-box no-labels frame med1:
   find first item {sys/look/itemW.i} and
		     item.i-no = xef.medium no-lock no-error.
   if available item then do:
       find first e-item of item no-lock no-error.
       /*override item shrink % with shrink entered in BOM button on Layout screen*/
      IF xef.spare-dec-1 NE 0 
          THEN dShrink = xef.spare-dec-1.
          ELSE dShrink = ITEM.shrink.
   END.
   if xef.n-out-l = 0 then do:
      med-len = brd-l[2].
      med-wid = brd-w[2] / (1 - (dShrink / 100)). /* proper length */
   end.
   else do:
      med-wid = brd-w[2].
      med-len = brd-l[2] / (1 - (dShrink / 100)). /* proper length */
   end.
   med-qty = ((med-wid * med-len) * mqty) / 144000. /*now msf*/

   if available e-item then
   DO:
      EMPTY TEMP-TABLE tt-ei.
      CREATE tt-ei.
      DO j = 1 TO 10:
         ASSIGN
            tt-ei.run-qty[j] = e-item.run-qty[j]
            tt-ei.run-cost[j] = e-item.run-cost[j].
      END.
      
            
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j + 10] = e-item.runQty[j]
               tt-ei.run-cost[j + 10] = e-item.runCost[j].
         END.

      do j = 1 to 20:
         if e-item.std-uom = "MSF" then do:
	        if tt-ei.run-qty[j] < med-qty then next.
	        mfl$ = med-qty * tt-ei.run-cost[j].
	        leave.
         end.
         else if e-item.std-uom = "TON" then do:
	          if tt-ei.run-qty[j] < (med-qty * item.basis-w / 2000) then next.
	          mfl$ = (med-qty * item.basis-w / 2000) * tt-ei.run-cost[j].
	          leave.
         end.
      end.  /* mfl$ = total cost for medium (less spoil & waste) */
   END.
   else do:
     if ce-ctrl.r-cost = true  /* avg. cost */ then do:
	if item.cons-uom = "MSF" then
	   mfl$ = med-qty * item.avg-cost.
	else if item.cons-uom = "TON" then
	   mfl$ = (med-qty * item.basis-w / 2000) * item.avg-cost.
     end.
     else do:    /* last cost */
	if item.cons-uom = "MSF" then
	   mfl$ = med-qty * item.last-cost.
	else if item.cons-uom = "TON" then
	   mfl$ = (med-qty * item.basis-w / 2000) * item.last-cost.
     end.
   end.

   DISPLAY
     /*    string(xef.form-no,"9") + "-00" format "x(4)" */
	 xef.medium
	 item.basis-w to 27
	 item.cal format ">9.999<" to 34
	 "$" + string(mfl$ / (qty / 1000),">>>>9.99") to 46
		space(0) " MShts" space(0)
	 mfl$ / (qty / 1000) format ">>>>9.99" to 68
	 mfl$ format ">,>>>,>>9.99" to 80 skip
     /*    space(5)   */
	 "Medium MR  Waste"
	 m-waste format ">>>>>9" to 46 space(0) " Sht" space(0)
	 (mfl$ / mqty) * m-waste format ">>>>9.99" to 59
	 /* cost of waste amortized per 1000 of all blanks on this form */
	 ((mfl$ / mqty) * m-waste) / (qty / 1000)
	    format ">>>>9.99" to 68
	 (mfl$ / mqty) * m-waste format ">,>>>,>>9.99" to 80 skip
      /*   space(5)    */
	 "Medium RUN Waste"
	 m-spo format ">>>>>9" to 46 space(0) " Sht" space(0)
	 /* cost of spoil amortized per 1000 of all blanks on this form */
	 (m-spo * (mfl$ / mqty)) / (qty / 1000)
					      format ">>>>9.99" to 68
	 m-spo * (mfl$ / mqty) format ">,>>>,>>9.99" to 80 SKIP WITH STREAM-IO.

   find first brd where brd.form-no = xef.form-no and
			brd.i-no    = xef.medium
			no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = xef.form-no
	     brd.blank-no = 00
	     brd.i-no    = xef.medium
	     brd.dscr    = xef.medium
	     brd.basis-w = item.basis-w.
   end.
   brd.qty = (qty / xeb.num-up) + m-waste + m-spo.
   brd.qty-uom = "Sht".
   brd.sc-uom = "MSH".
   brd.cost = mfl$ / (qty / 1000).
   brd.cost-m = mfl$ / (qty / 1000).
   brd.len  = med-len.
   brd.wid  = med-wid.

   dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste).
   dm-tot[4] = dm-tot[4] + (mfl$ / (qty / 1000)) +
			   ((m-spo   * (mfl$ / mqty)) / (qty / 1000)) +
			   ((m-waste * (mfl$ / mqty)) / (qty / 1000)).
   dm-tot[5] = dm-tot[5] + mfl$ + (m-spo   * (mfl$ / mqty)) +
				  (m-waste * (mfl$ / mqty)).

   lin-count = lin-count + 3. /* may need line counting for printout, someday */

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
   do:
      rm-wt = (med-qty + ((m-waste + m-spo) * (med-qty / mqty))) * item.basis-w.
      rm-wt$ = (rm-wt / 100) * ld-rm.
      ctrl2[3] = ctrl2[3] + rm-wt$.
   end.
   /* rm handling pct. */
   if ld-hp ne 0 then
   do:
      rm-wt% = ( mfl$ +
	       ((mfl$ / mqty) * m-waste) +
	       ((mfl$ / mqty) * m-spo) ).
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   end.
end.

if xef.flute ne "" then
do with no-box no-labels frame flute:
   find first item {sys/look/itemW.i} and
		     item.i-no = xef.flute no-lock no-error.
   if available item then find first e-item of item  no-lock no-error.
   med-qty = ((brd-l[2] * brd-w[2]) * mqty) / 144000. /*now msf*/

   if available e-item then
   DO:
      EMPTY TEMP-TABLE tt-ei.
      CREATE tt-ei.
      DO j = 1 TO 10:
         ASSIGN
            tt-ei.run-qty[j] = e-item.run-qty[j]
            tt-ei.run-cost[j] = e-item.run-cost[j].
      END.
      
            
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j + 10] = e-item.runQty[j]
               tt-ei.run-cost[j + 10] = e-item.runCost[j].
         END.

      do j = 1 to 20:
         if e-item.std-uom = "MSF" then do:
	        if tt-ei.run-qty[j] < med-qty then next.
	        mfl$ = med-qty * tt-ei.run-cost[j].
	        leave.
         end.
         else if e-item.std-uom = "TON" then do:
	        if tt-ei.run-qty[j] < (med-qty * item.basis-w / 2000) then next.
	        mfl$ = (med-qty * item.basis-w / 2000) * tt-ei.run-cost[j].
	        leave.
         end.
      END.
   end.  /* mfl$ = total cost for flute */
   else do:
     if ce-ctrl.r-cost = true  /* avg. cost */ then do:
	if item.cons-uom = "MSF" then
	   mfl$ = (med-qty * item.avg-cost).
	else if item.cons-uom = "TON" then
	   mfl$ = ((med-qty * item.basis-w / 2000) * item.avg-cost).
     end.
     else do:    /* last cost */
	if item.cons-uom = "MSF" then
	   mfl$ = (med-qty * item.last-cost).
	else if item.cons-uom = "TON" then
	   mfl$ = ((med-qty * item.basis-w / 2000) * item.last-cost).
     end.
   end.

   dm-tot[3] = dm-tot[3] + ( (mfl$ / mqty) * m-waste).
   dm-tot[4] = dm-tot[4] + (mfl$ / (qty / 1000)) +
			   ((m-spo   * (mfl$ / mqty)) / (qty / 1000)) +
			   ((m-waste * (mfl$ / mqty)) / (qty / 1000)).
   dm-tot[5] = dm-tot[5] + mfl$ + (m-spo   * (mfl$ / mqty)) +
				  (m-waste * (mfl$ / mqty)).

   if xef.medium = "" then tmpstore = "Flute ".
   else tmpstore = "Liner ".
   display
      /*   string(xef.form-no,"9") + "-00" format "x(4)" */
	 xef.flute
	 item.basis-w to 27
	 item.cal format ">9.999<" to 34
	 "$" + string(mfl$ / (mqty / 1000),">>>>9.99") to 46
		space(0) " MShts" space(0)
	 mfl$ / (qty / 1000) format ">>>>9.99" to 68
	 mfl$ format ">,>>>,>>9.99" to 80 skip
      /*   space(5) */
	 tmpstore + "MR  Waste" format "x(15)"
	 m-waste format ">>>>>9" to 46 space(0) " Sht" space(0)
	 (mfl$ / mqty) * m-waste format ">>>>9.99" to 59
	 /* cost of waste amortized per 1000 of all blanks on this form */
	 ((mfl$ / mqty) * m-waste) / (qty / 1000)
	    format ">>>>9.99" to 68
	 (mfl$ / mqty) * m-waste format ">,>>>,>>9.99" to 80 skip
      /*   space(5)     */
	 tmpstore + "RUN Waste" format "x(15)"
	 m-spo format ">>>>>9" to 46 space(0) " Sht" space(0)
	 /* cost of spoil amortized per 1000 of all blanks on this form */
	 (m-spo * (mfl$ / mqty)) / (qty / 1000)
					      format ">>>>9.99" to 68
	 m-spo * (mfl$ / mqty) format ">,>>>,>>9.99" to 80 skip WITH STREAM-IO.

   find first brd where brd.form-no = xef.form-no and
			brd.i-no    = xef.flute
			no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = xef.form-no
	     brd.blank-no = 00
	     brd.i-no    = xef.flute
	     brd.dscr    = xef.flute
	     brd.basis-w = item.basis-w.
   end.
   brd.qty = (qty / xeb.num-up) + m-waste + m-spo.
   brd.qty-uom = "Sht".
   brd.sc-uom = "MSH".
   brd.cost = mfl$ / (qty / 1000).
   brd.cost-m = mfl$ / (qty / 1000).
   brd.len  = brd-l[3].
   brd.wid  = brd-w[3].

   lin-count = lin-count + 3. /* may need line counting for printout, someday */

   /* rm handling chg per cwt*/
   if ld-rm ne 0 then
   do:
      rm-wt = (med-qty + ((m-waste + m-spo) * (med-qty / mqty))) * item.basis-w.
      rm-wt$ = (rm-wt / 100) * ld-rm.
      ctrl2[3] = ctrl2[3] + rm-wt$.
   end.
   /* rm handling pct. */
   if ld-hp ne 0 then
   do:
      rm-wt% = ( mfl$ + ((mfl$ / mqty) * m-waste) + ((mfl$ / mqty) * m-spo) ).
      ctrl2[2] = ctrl2[2] + (rm-wt% * ld-hp).
   end.
end.

call_id = recid(item).

/* end ---------------------------------- copr. 1992  advanced software, inc. */
