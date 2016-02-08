/* ----------------------------------------------------- ce/pr4-ink.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
def buffer xeb2 for eb.

{ce/print4.i shared shared}

def buffer ink2 for ink.

def var g-qty  as de NO-UNDO.
def var g-cost as de format ">>,>>9.99" NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
def var v-shts as int NO-UNDO.
def var v-col-p as int NO-UNDO.
DEF SHARED VAR qty AS INT NO-UNDO.
DEF VAR v-first-pass AS LOG NO-UNDO.

DEF TEMP-TABLE tt-ink NO-UNDO FIELD i-code LIKE ink.i-code
                              FIELD i-dscr LIKE ink.i-dscr
                              FIELD pass AS INT.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

v-num-up = xeb.num-up.

/* i n k s */
FOR EACH xeb2 NO-LOCK
    WHERE xeb2.company EQ xest.company
      AND xeb2.est-no  EQ xest.est-no 
    BREAK BY xeb2.form-no:

  IF FIRST-OF(xeb2.form-no) THEN EMPTY TEMP-TABLE tt-ink.

  {ce/pr4-ink.i xeb2}

  IF LAST-OF(xeb2.form-no) THEN DO:
    j = 0.
    FOR EACH est-op
        WHERE est-op.company EQ xeb2.company
          AND est-op.est-no  EQ xeb2.est-no
          AND (est-op.qty    EQ v-op-qty OR xest.est-type NE 1)
          AND est-op.s-num   EQ xeb2.form-no
          AND est-op.line    GE 500
          AND (est-op.dept    EQ "PR" OR est-op.dept EQ "CT")
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
  END.
END.

for each ink where ink.bnum ne 0:
  find first ink2
      where ink2.i-dscr eq ink.i-dscr
	and ink2.snum   eq ink.snum
	and ink2.bnum   eq 0
      no-error.
  if not avail ink2 then do:
    create ink2.
    assign
     ink2.id     = ink.id
     ink2.snum   = ink.snum
     ink2.bnum   = 0
     ink2.i-code = ink.i-code
     ink2.i-dscr = ink.i-dscr
     ink2.i-qty  = 0.
   end.

   ink2.i-qty = ink2.i-qty + ink.i-qty.
end.

for each ink where ink.bnum eq 0 by ink.i-dscr:
  find first item
      where item.company eq cocode
	and item.i-no    eq ink.i-code
      no-lock no-error.
  if not avail item then next.
  find first e-item of item no-lock no-error.
  if ink.i-qty < item.min-lbs then ink.i-qty = item.min-lbs.
  if avail e-item then
  DO:
    EMPTY TEMP-TABLE tt-ei.
    CREATE tt-ei.
    DO j = 1 TO 10:
       ASSIGN
          tt-ei.run-qty[j] = e-item.run-qty[j]
          tt-ei.run-cost[j] = e-item.run-cost[j].
    END.
    
    FIND FIRST b-qty WHERE
         b-qty.reftable = "blank-vend-qty" AND
         b-qty.company = e-item.company AND
         b-qty.CODE    = e-item.i-no
         NO-LOCK NO-ERROR.
    
    IF AVAIL b-qty THEN
    DO:
       FIND FIRST b-cost WHERE
            b-cost.reftable = "blank-vend-cost" AND
            b-cost.company = e-item.company AND
            b-cost.CODE    = e-item.i-no
            NO-LOCK NO-ERROR.
    
       DO j = 1 TO 10:
          ASSIGN
             tt-ei.run-qty[j + 10] = b-qty.val[j]
             tt-ei.run-cost[j + 10] = b-cost.val[j].
       END.
    END.

    do j = 1 to 20:
	   if tt-ei.run-qty[j] < ink.i-qty then next.
	   ink.i-cost = (ink.i-qty * tt-ei.run-cost[j]).
	   leave.
    end.
  END.
  else do:
	if ce-ctrl.r-cost = true then ink.i-cost = ink.i-qty * item.avg-cost.
	else ink.i-cost = ink.i-qty * item.last-cost.
  end.

  for each ink2
      where ink2.i-dscr eq ink.i-dscr
	and ink2.snum   eq ink.snum
	and ink2.bnum   ne 0:
    ink2.i-cost = (ink.i-cost / ink.i-qty) * ink2.i-qty.
  end.
end.

for each ink where ink.bnum ne 0:
  find first brd where brd.form-no = ink.snum and
		       brd.blank-no = ink.bnum and
		       brd.i-no    = ink.i-code
		       no-error.
  if not available brd then
  do:
     create brd.
     assign brd.form-no = ink.snum
	    brd.blank-no = ink.bnum
	    brd.i-no    = ink.i-code
	    brd.dscr    = ink.i-dscr
	    brd.basis-w = item.basis-w.
  end.

  brd.qty = brd.qty + ink.i-qty.
  brd.qty-uom = "LB".
  brd.sc-uom  = "LB".
  brd.cost = ink.i-cost / ink.i-qty.
  brd.cost-m = ink.i-cost / (qty / 1000).

  delete ink.
end.

for each ink by ink.i-dscr with frame ab down no-labels no-box:
  dm-tot[4] = dm-tot[4] + (ink.i-cost / (qty / 1000)).
  dm-tot[5] = dm-tot[5] + ink.i-cost.

  display ink.i-dscr
	  ink.i-qty  format ">>>9.99" to 46 space(0) " Lb"
	  ink.i-cost / (qty / 1000) format ">>>>9.99" to 68
	  ink.i-cost format ">,>>>,>>9.99"             to 80 SKIP  WITH STREAM-IO.
end.

find first xeb2 WHERE xeb2.company = xest.company AND xeb2.est-no = xest.est-no no-lock no-error.
if xeb2.adhesive ne "" then
do with frame ab2 down no-labels no-box:
   find first item
       where item.company eq cocode
	 and item.i-no    eq xeb2.adhesive
       no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   find first est-op where est-op.company = xest.company
                       AND est-op.est-no = xest.est-no and est-op.line >= 500 and
			           est-op.dept = "GL" no-lock no-error.
   if not avail item or not avail est-op then leave.
   v-shts = 0.
   for each est-op
       where est-op.company = xest.company and
             est-op.est-no eq xest.est-no
	     and est-op.line  ge 500
	     and est-op.dept  eq "GL"    no-lock:
       v-shts = v-shts + est-op.num-sh.
   end.
   g-qty = (xeb2.lin-in * (v-shts * xeb2.num-up)) / item.linin-lb.
   if avail e-item then
   DO:
      EMPTY TEMP-TABLE tt-ei.
      CREATE tt-ei.
      DO j = 1 TO 10:
         ASSIGN
            tt-ei.run-qty[j] = e-item.run-qty[j]
            tt-ei.run-cost[j] = e-item.run-cost[j].
      END.
      
      FIND FIRST b-qty WHERE
           b-qty.reftable = "blank-vend-qty" AND
           b-qty.company = e-item.company AND
           b-qty.CODE    = e-item.i-no
           NO-LOCK NO-ERROR.
      
      IF AVAIL b-qty THEN
      DO:
         FIND FIRST b-cost WHERE
              b-cost.reftable = "blank-vend-cost" AND
              b-cost.company = e-item.company AND
              b-cost.CODE    = e-item.i-no
              NO-LOCK NO-ERROR.
      
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j + 10] = b-qty.val[j]
               tt-ei.run-cost[j + 10] = b-cost.val[j].
         END.
      END.

      do j = 1 to 20:
	     if tt-ei.run-qty[j] < g-qty then next.
	     g-cost = (g-qty * tt-ei.run-cost[j]).
	     leave.
      end.
   END.
   else do:
	 if ce-ctrl.r-cost = true then g-cost = g-qty * item.avg-cost.
	 else g-cost = g-qty * item.last-cost.
   end.
   assign
   dm-tot[4] = dm-tot[4] + (g-cost / (qty / 1000))
   dm-tot[5] = dm-tot[5] + g-cost.

   find first brd where brd.form-no = xeb2.form-no and
			brd.blank-no = xeb2.blank-no and
			brd.i-no    = xeb2.adhesive
			no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = xeb2.form-no
	     brd.blank-no = xeb2.blank-no
	     brd.i-no    = xeb2.adhesive
	     brd.dscr    = item.i-dscr
	     brd.basis-w = item.basis-w.
   end.
   brd.qty = brd.qty + g-qty.
/* Changed default from Lbs to LB  FWK  2/28/96 */
   brd.qty-uom = "LB".
   brd.sc-uom  = "LB".
   brd.cost = g-cost / g-qty.
   brd.cost-m = g-cost / (qty / 1000).

   display item.i-name
	   g-qty format ">>>>>9" to 46 "Lbs"
	   g-cost / (qty / 1000) format ">>>>9.99"
	   to 68 g-cost format ">,>>>,>>9.99" to 80 skip WITH STREAM-IO.
end.

if xef.adh-code ne "" then
do with frame ab3 down no-labels no-box:
   find first item
       where item.company eq cocode
	 and item.i-no    eq xef.adh-code
       no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   find first est-op where est-op.company = xest.company
                       AND est-op.est-no = xest.est-no and est-op.line >= 500 and
			           est-op.dept = "LM" no-lock no-error.
   if not avail item or not avail est-op or
   ((adh-qty[1] + adh-qty[2] + adh-qty[3]) = 0)
   then leave.
   g-qty = ((adh-qty[1] + adh-qty[2] + adh-qty[3]) * xef.adh-sqin *
	     est-op.num-sh) / item.sqin-lb.
   if avail e-item then
   DO:
     EMPTY TEMP-TABLE tt-ei.
     CREATE tt-ei.
     DO j = 1 TO 10:
        ASSIGN
           tt-ei.run-qty[j] = e-item.run-qty[j]
           tt-ei.run-cost[j] = e-item.run-cost[j].
     END.
     
     FIND FIRST b-qty WHERE
          b-qty.reftable = "blank-vend-qty" AND
          b-qty.company = e-item.company AND
          b-qty.CODE    = e-item.i-no
          NO-LOCK NO-ERROR.
     
     IF AVAIL b-qty THEN
     DO:
        FIND FIRST b-cost WHERE
             b-cost.reftable = "blank-vend-cost" AND
             b-cost.company = e-item.company AND
             b-cost.CODE    = e-item.i-no
             NO-LOCK NO-ERROR.
     
        DO j = 1 TO 10:
           ASSIGN
              tt-ei.run-qty[j + 10] = b-qty.val[j]
              tt-ei.run-cost[j + 10] = b-cost.val[j].
        END.
     END.

     do j = 1 to 20:
	    if tt-ei.run-qty[j] < g-qty then next.
	    g-cost = (g-qty * tt-ei.run-cost[j]).
	    leave.
     end.
   END.
   else do:
	 if ce-ctrl.r-cost = true then g-cost = g-qty * item.avg-cost.
	 else g-cost = g-qty * item.last-cost.
   end.
   
   assign
   dm-tot[4] = dm-tot[4] + (g-cost / (qty / 1000))
   dm-tot[5] = dm-tot[5] + g-cost.

   find first brd where brd.form-no = xef.form-no and
			brd.blank-no = 0 and
			brd.i-no    = xef.adh-code
			no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = xef.form-no
	     brd.blank-no = 0
	     brd.i-no    = xef.adh-code
	     brd.dscr    = item.i-dscr
	     brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + g-qty
/* Changed default from Lbs to LB  FWK  2/28/96 */
   brd.qty-uom = "LB"
   brd.sc-uom  = "LB"
   brd.cost = g-cost / g-qty
   brd.cost-m = g-cost / (qty / 1000).

   display item.i-name
	   g-qty format ">>>>>9" to 46 "Lbs"
	   g-cost / (qty / 1000) format ">>>>9.99"
	   to 68 g-cost format ">,>>>,>>9.99" to 80 skip WITH STREAM-IO.
end.

if xef.lam-code ne "" then
do with frame ab4 down no-labels no-box:
   find first item
       where item.company eq cocode
	 and item.i-no    eq xef.lam-code
       no-lock no-error.
   if avail item then find first e-item of item no-lock no-error.
   find first est-op where est-op.company = xest.company
                       AND est-op.est-no = xest.est-no and est-op.line >= 500 and
			           est-op.dept = "LM" no-lock no-error.
   if not avail item or not avail est-op or
   ((adh-qty[1] + adh-qty[2] + adh-qty[3]) = 0)
   then leave.
   g-qty = ((adh-qty[1] + adh-qty[2] + adh-qty[3]) * xef.adh-sqin *
	     est-op.num-sh) / item.sqin-lb.
   if avail e-item then
   DO:
      EMPTY TEMP-TABLE tt-ei.
      CREATE tt-ei.
      DO j = 1 TO 10:
         ASSIGN
            tt-ei.run-qty[j] = e-item.run-qty[j]
            tt-ei.run-cost[j] = e-item.run-cost[j].
      END.
      
      FIND FIRST b-qty WHERE
           b-qty.reftable = "blank-vend-qty" AND
           b-qty.company = e-item.company AND
           b-qty.CODE    = e-item.i-no
           NO-LOCK NO-ERROR.
      
      IF AVAIL b-qty THEN
      DO:
         FIND FIRST b-cost WHERE
              b-cost.reftable = "blank-vend-cost" AND
              b-cost.company = e-item.company AND
              b-cost.CODE    = e-item.i-no
              NO-LOCK NO-ERROR.
      
         DO j = 1 TO 10:
            ASSIGN
               tt-ei.run-qty[j + 10] = b-qty.val[j]
               tt-ei.run-cost[j + 10] = b-cost.val[j].
         END.
      END.
     
      do j = 1 to 20:
	     if tt-ei.run-qty[j] < g-qty then next.
	     g-cost = (g-qty * tt-ei.run-cost[j]).
	     leave.
      end.
   END.
   else do:
	 if ce-ctrl.r-cost = true then g-cost = g-qty * item.avg-cost.
	 else g-cost = g-qty * item.last-cost.
   end.
   ASSIGN
   dm-tot[4] = dm-tot[4] + (g-cost / (qty / 1000))
   dm-tot[5] = dm-tot[5] + g-cost.

   find first brd where brd.form-no = xef.form-no and
			brd.blank-no = 0 and
			brd.i-no    = xef.adh-code
			no-error.
   if not available brd then
   do:
      create brd.
      assign brd.form-no = xef.form-no
	     brd.blank-no = 0
	     brd.i-no    = xef.adh-code
	     brd.dscr    = item.i-dscr
	     brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + g-qty
/* Changed default from Lbs to LB  FWK  2/28/96 */
   brd.qty-uom = "LB"
   brd.sc-uom  = "LB"
   brd.cost = g-cost / g-qty
   brd.cost-m = g-cost / (qty / 1000).

   display item.i-name
	   g-qty format ">>>>>9" to 46 "Lbs"
	   g-cost / (qty / 1000) format ">>>>9.99"
	   to 68 g-cost format ">,>>>,>>9.99" to 80 skip WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
