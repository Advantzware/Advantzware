/* ------------------------------------------------- ce/com/pr4-flm.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

def buffer xflm for flm.
def buffer xop  for est-op.

def var qqq as de NO-UNDO.
def var fqty as dec NO-UNDO.
def var fuom as ch NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var rm-wt  as de NO-UNDO.
def var mrg as de NO-UNDO.
def var fup like eb.num-up NO-UNDO.
def var vup like fup NO-UNDO.
DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.
DEF SHARED VAR gEstSummaryOnly AS LOG NO-UNDO.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-setup FOR reftable.

&SCOPED-DEFINE valid-est-op                                            ~
    WHERE est-op.company EQ est-flm.company                            ~
      AND est-op.est-no  EQ est-flm.est-no                             ~
      AND est-op.line    GT 500                                        ~
      AND est-op.s-num   EQ est-flm.snum                               ~
      AND CAN-FIND(FIRST mach                                          ~
                   WHERE mach.company      EQ est-op.company           ~
                     AND mach.m-code       EQ est-op.m-code            ~
                     AND ((est-op.op-sb    EQ NO           AND         ~
                           est-op.b-num    EQ est-flm.bnum AND         ~
                           est-flm.bnum    GT 0            AND         ~
                           (mach.dept[1]   EQ lv-deptb OR              ~
                            mach.dept[2]   EQ lv-deptb OR              ~
                            mach.dept[3]   EQ lv-deptb OR              ~
                            mach.dept[4]   EQ lv-deptb))            OR ~
                          ((est-op.op-sb   EQ YES OR                   ~
                            est-flm.bnum   LE 0)           AND         ~
                           (mach.dept[1]   EQ lv-deptf OR              ~
                            mach.dept[2]   EQ lv-deptf OR              ~
                            mach.dept[3]   EQ lv-deptf OR              ~
                            mach.dept[4]   EQ lv-deptf))))

DEF BUFFER bf-ef FOR ef.
DEF BUFFER bf-eb FOR eb.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
mrg = ce-ctrl.win-margin.

/* films */
for each est-flm
    where est-flm.company = xest.company
      AND est-flm.est-no eq xest.est-no
      and est-flm.i-no  ne ""
    no-lock,
    
    first bf-ef
    where bf-ef.company = xest.company
      AND bf-ef.est-no eq xest.est-no
      and bf-ef.form-no eq est-flm.snum
    no-lock,
    
    first item
    where item.company eq xest.company
      and item.i-no    eq est-flm.i-no
    no-lock:

  find first e-item of item no-lock no-error.
  fuom = if avail e-item then e-item.std-uom else item.cons-uom.

  find first bf-eb
      where bf-eb.company = est-flm.company
        AND bf-eb.est-no    eq est-flm.est-no
        and bf-eb.form-no  eq est-flm.snum
        and bf-eb.blank-no eq est-flm.bnum
      no-lock no-error.

  IF item.mat-type EQ "W" THEN
    ASSIGN
     lv-deptb = "WN"
     lv-deptf = "WS".
    
  ELSE
    ASSIGN
     lv-deptb = "FB"
     lv-deptf = "FS".

  FIND FIRST est-op {&valid-est-op} NO-LOCK NO-ERROR.
  
  if not avail est-op then next.

  run sys/inc/numup.p (est-flm.company,est-flm.est-no, est-flm.snum, output vup).
  
  fup = if est-flm.bnum eq 0 then vup else 1.
    
  if item.mat-type eq "W" then
    fqty = (est-flm.wid + fup) * (est-flm.len + fup).
  else
    fqty = est-flm.wid * est-flm.len.
    
  ASSIGN
     fup = if est-flm.bnum eq 0 then 1 else vup
     fup = fup * (if bf-ef.n-out   gt 0 then bf-ef.n-out   else 1) *
                 (if bf-ef.n-out-l gt 0 then bf-ef.n-out-l else 1)
     fqty = fqty * fup * est-op.num-sh
     fqty = fqty / (if fuom eq "MSI" then 1000 else item.sqin-lb).

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
                            est-flm.snum  = flm.snum and
                            est-flm.bnum  = flm.bnum no-lock no-error.
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
        IF blk.pur-man THEN
          ASSIGN
           ld-rm = rm-rate-f
           ld-hp = hand-pct-f.
        ELSE
          ASSIGN
           ld-rm = ctrl[3]
           ld-hp = ctrl[2].

         blk.cost = blk.cost + flm.cost.
         if ld-rm ne 0 THEN
            ASSIGN
               blk.cost = blk.cost + ((rm-wt / 100) * ld-rm)
               blk.lab  = blk.lab  + ((rm-wt / 100) * ld-rm).
         
         if ld-hp ne 0 THEN
            ASSIGN
               blk.cost = blk.cost + (flm.cost * ld-hp)
               blk.lab  = blk.lab  + (flm.cost * ld-hp).
      end.
   end.
   else for each blk where blk.snum = est-flm.snum:
      IF blk.pur-man THEN
        ASSIGN
         ld-rm = rm-rate-f
         ld-hp = hand-pct-f.
      ELSE
        ASSIGN
         ld-rm = ctrl[3]
         ld-hp = ctrl[2].

      blk.cost = blk.cost + (f-cost[1] * blk.pct).
      if ld-rm ne 0 then 
         ASSIGN
            blk.cost = blk.cost + (((rm-wt / 100) * ld-rm) * blk.pct)
            blk.lab  = blk.lab  + (((rm-wt / 100) * ld-rm) * blk.pct).
      
      if ld-hp ne 0 THEN
         ASSIGN
            blk.cost = blk.cost + ((flm.cost * ld-hp) * blk.pct)
            blk.lab  = blk.lab  + ((flm.cost * ld-hp) * blk.pct).
   end.
   dm-tot[5] = dm-tot[5] + flm.cost.
   if ld-rm ne 0 then ctrl2[3] = ctrl2[3] + ((rm-wt / 100) * ld-rm).
   if ld-hp ne 0 then ctrl2[2] = ctrl2[2] + (flm.cost * ld-hp).

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
      brd.len = est-flm.len + INT(item.mat-type EQ "W")
      brd.wid = est-flm.wid + INT(item.mat-type EQ "W")
      brd.amount = brd.amount + flm.cost.

   IF NOT gEstSummaryOnly THEN
   display
        string(flm.snum,">9") + "-" + string(flm.bnum,"99") format "x(5)"
        item.i-name flm.qty to 50
        flm.uom at 52 when flm.uom = "MSI"
        "Lbs" when flm.uom = "LB" @ flm.uom
        flm.cosm to 69
        flm.cost format ">>>>,>>9.99" to 80 SKIP WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
