/* ------------------------------------------------- ce/com/pr4-flm.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{cec/print4.i shared shared}

def buffer xflm for flm.
def buffer xop  for est-op.
def buffer b-i for item.

def var qqq as de NO-UNDO.
def var fqty as dec NO-UNDO.
def var fuom as ch NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var mrg as de NO-UNDO.
def var fup like eb.num-up NO-UNDO.
def var vup like fup NO-UNDO.
DEF VAR lv-sqin-lb LIKE item.sqin-lb NO-UNDO.
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.
DEF VAR ld-rm-rate AS DEC NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.

&SCOPED-DEFINE valid-est-op                                            ~
    WHERE est-op.company EQ est-flm.company                            ~
      AND est-op.est-no  EQ est-flm.est-no                             ~
      AND est-op.line    GT 500                                        ~
      AND est-op.s-num   EQ est-flm.snum                               ~
      AND CAN-FIND(FIRST mach                                          ~
                   WHERE mach.company      EQ est-op.company           ~
                     AND mach.m-code       EQ est-op.m-code            ~
                     AND ((est-op.op-sb     EQ NO           AND         ~
                           est-op.b-num     EQ est-flm.bnum AND         ~
                           est-flm.bnum     GT 0            AND         ~
                           (mach.dept[1]    EQ lv-deptb OR              ~
                            mach.dept[2]    EQ lv-deptb OR              ~
                            mach.dept[3]    EQ lv-deptb OR              ~
                            mach.dept[4]    EQ lv-deptb))            OR ~
                          ((est-op.op-sb    EQ YES OR                   ~
                            est-flm.bnum    LE 0)           AND         ~
                           (mach.dept[1]    EQ lv-deptf OR              ~
                            mach.dept[2]    EQ lv-deptf OR              ~
                            mach.dept[3]    EQ lv-deptf OR              ~
                            mach.dept[4]    EQ lv-deptf))))

DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

{cec/msfcalc.i}

{cec/rollfac.i}

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
mrg = ce-ctrl.win-margin.

/* films */
for each est-flm
    where est-flm.company = xest.company
      AND est-flm.est-no eq xest.est-no
      and est-flm.i-no  ne ""
    no-lock,
    
    first bf-ef
    where bf-ef.company eq xest.company
      AND bf-ef.est-no  eq xest.est-no
      and bf-ef.form-no eq est-flm.snum
    no-lock,
    
    first item
    where item.company eq xest.company
      and item.i-no    eq est-flm.i-no
    no-lock:

  RUN est/ef-#out.p (ROWID(bf-ef), OUTPUT v-n-out).

  find first e-item of item no-lock no-error.
  fuom = if avail e-item then e-item.std-uom else item.cons-uom.

  find first bf-eb
      where bf-eb.company  eq est-flm.company
        AND bf-eb.est-no   eq est-flm.est-no
        and bf-eb.form-no  eq est-flm.snum
        and bf-eb.blank-no eq est-flm.bnum
      no-lock no-error.

  IF item.mat-type EQ "W" THEN DO:
    ASSIGN
     lv-deptb = "WN"
     lv-deptf = "WS".

    FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.
  END.
    
  ELSE DO:
    ASSIGN
     lv-deptb = "FB"
     lv-deptf = "FS".

    FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.
  END.
  
  if not avail est-op then next.

  run sys/inc/numup.p (est-flm.company, est-flm.est-no, est-flm.snum, output vup).
  
  fup = if est-flm.bnum eq 0 then 1 else vup.
    
  if item.mat-type eq "W" THEN DO:
    find first b-i
        where b-i.company eq cocode
          and b-i.i-no    eq bf-ef.board
          no-lock no-error.
    if not avail b-i then leave.
	assign
	 fqty       = (est-op.num-sh * fup * v-n-out *
		           (if v-corr then
			          (est-flm.len * est-flm.wid * .007)
                    else
	                  (est-flm.len * est-flm.wid / 144))) /
		          1000 * b-i.basis-w
	 fuom       = "LB"
     lv-sqin-lb = 1.

	do j = 1 to 6:
	  if bf-ef.adder[j] ne "" then do:
	    find first b-i
	        where b-i.company eq cocode
		      and b-i.i-no    eq bf-ef.adder[j]
		    no-lock no-error.
	    if avail b-i then 
	      fqty = fqty +
		         ((est-op.num-sh * fup * v-n-out *
			      (if v-corr then
			         (est-flm.len * est-flm.wid * .007)
                   else
			         (est-flm.len * est-flm.wid / 144))) /
			     1000 * b-i.basis-w).
	  end.
	end.

	fqty = fqty * item.shrink.
  end.

  ELSE
    ASSIGN
     fqty       = (est-flm.len * est-flm.wid) *
                  (est-op.num-sh * fup * v-n-out)
     lv-sqin-lb = item.sqin-lb.
  
  fqty = fqty / (if fuom eq "MSI" then 1000 else lv-sqin-lb).

  find first flm
      where flm.id   eq item.i-no
        and flm.snum eq est-flm.snum
        and flm.bnum eq est-flm.bnum
      no-error.
  if not avail flm then do:
    create flm.
    assign
     flm.snum = est-flm.snum
     flm.bnum = est-flm.bnum.
  end.

  assign
   flm.i-no = item.i-no
   flm.dscr = item.est-dscr
   flm.qty  = flm.qty + fqty
   flm.uom  = fuom
   flm.cost = flm.cost + f-cost[1].
end.

for each flm by flm.snum by flm.bnum with no-labels no-box:
   find first est-flm where est-flm.company = xest.company AND
                            est-flm.est-no = xest.est-no and
                            est-flm.i-no   = flm.i-no AND
                            est-flm.snum  = flm.snum   and
                            est-flm.bnum  = flm.bnum   no-lock no-error.
   find first bf-eb where bf-eb.company  = xest.company     and
                          bf-eb.est-no   = xest.est-no and
                          bf-eb.form-no  = flm.snum   and
                          bf-eb.blank-no = flm.bnum   no-lock no-error.
   find first item where item.company = xest.company and
                         item.i-no    = est-flm.i-no no-lock no-error.
   if avail item then find first e-item of item  no-lock no-error.
   for each xflm where xflm.i-no eq flm.i-no:
       t-qty = t-qty + xflm.qty.
   end.

   {est/matcost.i t-qty flm.cost flm}

   ASSIGN
    flm.cost = (flm.cost * flm.qty) + lv-setup-flm
    qqq      = 0.
     
   for each bf-eb FIELDS(yrprice yld-qty bl-qty)
       where bf-eb.company   eq xest.company
         and bf-eb.est-no     eq xest.est-no
         and bf-eb.form-no   eq flm.snum
         and (bf-eb.blank-no eq flm.bnum or flm.bnum eq 0)
       no-lock:
     qqq = qqq + (if bf-eb.yrprice then bf-eb.yld-qty else bf-eb.bl-qty).
   end.
     
   ASSIGN
   flm.cosm = flm.cost / (qqq / 1000)
   rm-wt = if flm.uom eq "MSI" then (flm.qty * 1000) / item.sqin-lb
                               else flm.qty.

   if est-flm.bnum ne 0 then do:
      find first blk where blk.snum = est-flm.snum and
                           blk.bnum = est-flm.bnum no-error.
      if avail blk then do:
        ASSIGN
        blk.cost = blk.cost + flm.cost
        ld-rm-rate = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3].
        IF ld-rm-rate GT 0 THEN
           ASSIGN
              blk.cost = blk.cost + (rm-wt / 100 * ld-rm-rate)
              blk.lab  = blk.lab  + (rm-wt / 100 * ld-rm-rate)
              ctrl2[3] = ctrl2[3] + (rm-wt / 100 * ld-rm-rate).
        
        ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].
        IF ld-hand-pct NE 0 THEN
           ASSIGN
              blk.cost = blk.cost + (flm.cost * ld-hand-pct)
              blk.lab  = blk.lab  + (flm.cost * ld-hand-pct)
              ctrl2[2] = ctrl2[2] + (flm.cost * ld-hand-pct).
        
      end.
   end.
   else 
   for each blk where blk.snum = est-flm.snum:

      ASSIGN
         blk.cost = blk.cost + (flm.cost * blk.pct)
         ld-rm-rate = IF blk.pur-man THEN rm-rate-f ELSE ctrl[3].

      IF ld-rm-rate NE 0 THEN
         ASSIGN
           blk.cost = blk.cost + (rm-wt / 100 * ld-rm-rate * blk.pct)
           blk.lab  = blk.lab  + (rm-wt / 100 * ld-rm-rate * blk.pct)
           ctrl2[3] = ctrl2[3] + (rm-wt / 100 * ld-rm-rate * blk.pct).
      
      ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].
      IF ld-hand-pct NE 0 THEN
         ASSIGN
           blk.cost = blk.cost + (flm.cost * ld-hand-pct * blk.pct)
           blk.lab  = blk.lab  + (flm.cost * ld-hand-pct * blk.pct)
           ctrl2[2] = ctrl2[2] + (flm.cost * ld-hand-pct * blk.pct).
   end.
   dm-tot[5] = dm-tot[5] + flm.cost.

   find first brd where brd.form-no = flm.snum and
                        brd.blank-no = flm.bnum and
                        brd.i-no    = est-flm.i-no
                        no-error.
   if not avail brd then do:
      create brd.
      assign brd.form-no = est-flm.snum
             brd.blank-no = est-flm.bnum
             brd.i-no    = est-flm.i-no
             brd.dscr    = flm.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
      brd.qty = brd.qty + flm.qty
      brd.qty-uom = flm.uom
      brd.sc-uom = flm.uom
      brd.cost = flm.cost / flm.qty
      brd.cost-m = flm.cosm
      brd.len = est-flm.len
      brd.wid = est-flm.wid.

   display
        string(flm.snum,">9") + "-" + string(flm.bnum,"99") format "x(5)"
        item.i-name flm.qty to 50
        flm.uom at 52 when flm.uom = "MSI"
        "Lbs" when flm.uom = "LB" @ flm.uom
        flm.cosm to 69
        flm.cost format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
