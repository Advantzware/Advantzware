/* ------------------------------------------------- ce/tan/pr4-flm.p 8/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

def buffer xflm for flm.

DEF SHARED VAR qty AS INT NO-UNDO.  

def var rm-wt  as de NO-UNDO.
def var mrg as de NO-UNDO.
def var save_id as recid NO-UNDO.

def var fqty as dec NO-UNDO.
def var fcost as dec NO-UNDO.
def var fuom like item.cons-uom NO-UNDO.
def var fup like eb.num-up NO-UNDO.  
DEF VAR lv-deptb LIKE est-op.dept NO-UNDO.
DEF VAR lv-deptf LIKE est-op.dept NO-UNDO.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.


DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

&SCOPED-DEFINE valid-est-op                                             ~
    WHERE est-op.company EQ xest.company                                ~
      AND est-op.est-no  EQ xest.est-no                                 ~
      AND est-op.line    GT 500                                         ~
      AND est-op.s-num   EQ xef.form-no                                 ~
      AND CAN-FIND(FIRST mach                                           ~
                   WHERE mach.company       EQ est-op.company           ~
                     AND mach.m-code        EQ est-op.m-code            ~
                     AND ((est-op.op-sb     EQ NO               AND     ~
                           est-op.b-num     EQ xef.leaf-bnum[i] AND     ~
                           xef.leaf-bnum[i] GT 0                AND     ~
                           (mach.dept[1]    EQ lv-deptb OR              ~
                            mach.dept[2]    EQ lv-deptb OR              ~
                            mach.dept[3]    EQ lv-deptb OR              ~
                            mach.dept[4]    EQ lv-deptb))            OR ~
                          ((est-op.op-sb    EQ YES OR                   ~
                            xef.leaf-bnum[i] LE 0)               AND    ~
                           (mach.dept[1]    EQ lv-deptf OR              ~
                            mach.dept[2]    EQ lv-deptf OR              ~
                            mach.dept[3]    EQ lv-deptf OR              ~
                            mach.dept[4]    EQ lv-deptf))))


find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
assign
 mrg     = ce-ctrl.win-margin
 save_id = recid(xeb).

/* films */
do i = 1 to 4:
  if xef.leaf[i] ne "" then do:
    find first item
        where item.company eq cocode
          and item.i-no    eq xef.leaf[i]
        no-lock no-error.
    if avail item then find first e-item of item no-lock no-error.
    else next.

    fuom = if avail e-item then e-item.std-uom else item.cons-uom.
    
    f-qty = 0.
    for each xeb
        where xeb.company = xest.company
          AND xeb.est-no  eq xest.est-no
          and xeb.form-no eq xef.form-no
        no-lock
        break by xeb.blank-no:
        
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

      fup = if xef.leaf-bnum[i] eq 0 then xeb.num-up else 1.
      
      if item.mat-type eq "W" then
        fqty = (xef.leaf-w[i] + fup) * (xef.leaf-l[i] + fup).             
      else
        fqty = xef.leaf-w[i] * xef.leaf-l[i].
        
      fup = if xef.leaf-bnum[i] eq 0 then 1 else xeb.num-up.
      
      fup = fup * (if xef.n-out   gt 0 then xef.n-out   else 1) *
                  (if xef.n-out-l gt 0 then xef.n-out-l else 1).
     
      fqty = fqty * fup * est-op.num-sh.
      
      f-qty[1] = f-qty[1] + fqty.
      
      if xef.leaf-bnum[i] le 0 then leave.
    end.
    
    f-qty[1] = f-qty[1] / (if fuom eq "MSI" then 1000 else item.sqin-lb).

    find first flm 
        where flm.i-no eq item.i-no
          and flm.snum eq xef.form-no
        no-error.
    if not avail flm then do:
      create flm.
      assign
       flm.i-no = item.i-no
       flm.dscr = item.est-dscr
       flm.snum = xef.form-no.
    end.
    assign                            
     flm.qty  = flm.qty + f-qty[1]
     flm.uom  = fuom.
  end.
end.

for each flm by flm.snum by flm.bnum with no-labels no-box:
   find first item where item.company = cocode and
                         item.i-no    = flm.i-no no-lock no-error.
   if avail item then find first e-item of item  no-lock no-error.
   t-qty = 0.
   for each xflm where xflm.i-no eq flm.i-no:
     t-qty = t-qty + xflm.qty.
   end.
   if avail e-item then
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
               tt-ei.run-qty[j + 10] = e-item.run-qty[j]
               tt-ei.run-cost[j + 10] = e-item.run-cost[j].
         END.



      do j = 1 to 20:
         if tt-ei.run-qty[j] < t-qty then next.
         flm.cost = (flm.qty * tt-ei.run-cost[j]).
         leave.
      end.
   END.
   else do:
      if ce-ctrl.r-cost = true then flm.cost = flm.qty * item.avg-cost.
      else flm.cost = flm.qty * item.last-cost.
   end.

   assign
   flm.cosm = flm.cost / (qty / 1000)

   rm-wt = if flm.uom eq "MSI" then (flm.qty * 1000) / item.sqin-lb
                               else flm.qty.

   if flm.bnum ne 0 then do:
      find first blk where blk.snum = flm.snum and
                           blk.bnum = flm.bnum no-error.
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
         if ld-rm ne 0 then do:
            blk.cost = blk.cost + ((rm-wt / 100) * ld-rm).
            blk.lab  = blk.lab  + ((rm-wt / 100) * ld-rm).
         end.
         if ld-hp ne 0 then do:
            blk.cost = blk.cost + (flm.cost * ld-hp).
            blk.lab  = blk.lab  + (flm.cost * ld-hp).
         end.
      end.
   end.
   else 
   for each blk where blk.snum = flm.snum:
      IF blk.pur-man THEN
        ASSIGN
         ld-rm = rm-rate-f
         ld-hp = hand-pct-f.
      ELSE
        ASSIGN
         ld-rm = ctrl[3]
         ld-hp = ctrl[2].
      blk.cost = blk.cost + (f-cost[1] * blk.pct).
      if ld-rm ne 0 then do:
         blk.cost = blk.cost + (((rm-wt / 100) * ld-rm) * blk.pct).
         blk.lab  = blk.lab  + (((rm-wt / 100) * ld-rm) * blk.pct).
      end.
      if ld-hp ne 0 then do:
         blk.cost = blk.cost + ((flm.cost * ld-hp) * blk.pct).
         blk.lab  = blk.lab  + ((flm.cost * ld-hp) * blk.pct).
      end.
   end.
   
   dm-tot[5] = dm-tot[5] + flm.cost.
   if ld-rm ne 0 then ctrl2[3] = ctrl2[3] + ((rm-wt / 100) * ld-rm).
   if ld-hp ne 0 then ctrl2[2] = ctrl2[2] + (flm.cost * ld-hp).

   find first brd where brd.form-no = flm.snum and
                        brd.blank-no = flm.bnum and
                        brd.i-no    = item.i-no
                        no-error.
   if not avail brd then do:
     create brd.
     assign brd.form-no = flm.snum
            brd.blank-no = flm.bnum
            brd.i-no    = item.i-no
            brd.dscr    = flm.dscr
            brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = flm.qty
   brd.qty-uom = flm.uom
   brd.sc-uom = flm.uom
   brd.cost = flm.cost / flm.qty
   brd.cost-m = flm.cosm.

   DO i = 1 TO 4:
     IF xef.leaf[i]      EQ brd.i-no AND
        xef.leaf-snum[i] EQ flm.snum AND
        xef.leaf-bnum[i] EQ flm.bnum THEN DO:
       ASSIGN
        brd.len = xef.leaf-l[i] + INT(item.mat-type EQ "W")
        brd.wid = xef.leaf-w[i] + INT(item.mat-type EQ "W").
       LEAVE.
     END.
   END.

   display item.i-name flm.qty  to 46
           flm.uom when flm.uom = "MSI"
           "Lbs" when flm.uom = "LB" @ flm.uom
           flm.cosm to 68 flm.cost format ">,>>>,>>9.99" to 80 skip 
       WITH STREAM-IO.
end.

find xeb where recid(xeb) eq save_id no-lock.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
