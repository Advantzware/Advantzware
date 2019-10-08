/* ---------------------------------------------- ce/box/pr42-flm.p 2/93 cd */
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.

{ce/print4.i shared shared}

DEF SHARED VAR qty AS INT NO-UNDO.

def buffer b-flm for flm.
def buffer xop   for est-op.

def var fqty as dec.
def var fcost as dec.
def var fuom like item.cons-uom.
def var fup like eb.num-up.
def var vup like fup.
DEF VAR ld-rm AS DEC NO-UNDO.
DEF VAR ld-hp AS DEC NO-UNDO.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

/* films */
for each xef where xef.company eq xest.company
               and xef.est-no  eq xest.est-no:
   {ce/pr4-flm.i}
end.

{ce/pr4-flm2.i}

   if flm.bnum ne 0 then do:
      find first blk where blk.snum = flm.snum and
                           blk.bnum = flm.bnum no-error.
      
      IF blk.pur-man THEN
        ASSIGN
         ld-rm = rm-rate-f
         ld-hp = hand-pct-f.
      ELSE
        ASSIGN
         ld-rm = ctrl[3]
         ld-hp = ctrl[2].

      blk.cost = blk.cost + fcost.
      if ld-rm ne 0 then
         assign
          blk.cost = blk.cost + ((flm.qty / 100) * ld-rm)
          blk.lab  = blk.lab  + ((flm.qty / 100) * ld-rm).
      if ld-hp ne 0 then
         assign
          blk.cost = blk.cost + (fcost * ld-hp)
          blk.lab  = blk.lab  + (fcost * ld-hp).
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

      blk.cost = blk.cost + (fcost * blk.pct).

      if ld-rm ne 0 then
         assign
          blk.cost = blk.cost + (((flm.qty / 100) * ld-rm) * blk.pct)
          blk.lab  = blk.lab  + (((flm.qty / 100) * ld-rm) * blk.pct).
      if ld-hp ne 0 then
         assign
          blk.cost = blk.cost + ((fcost * ld-hp) * blk.pct)
          blk.lab  = blk.lab  + ((fcost * ld-hp) * blk.pct).
   end.

   find first brd where brd.form-no = flm.snum and
                        brd.blank-no = flm.bnum and
                        brd.i-no    = flm.id
                        no-error.
   if not avail brd then do:
      create brd.
      assign brd.form-no = flm.snum
             brd.blank-no = flm.bnum
             brd.i-no    = flm.id
             brd.dscr    = flm.dscr
             brd.basis-w = item.basis-w.
   end.
   ASSIGN
   brd.qty = brd.qty + flm.qty
   brd.qty-uom = flm.uom
   brd.sc-uom = flm.uom
   brd.cost = flm.cost / flm.qty
   brd.cost-m = flm.cosm.

   find first xef
       where xef.company eq xest.company
         and xef.est-no  eq xest.est-no
         and xef.form-no eq flm.snum
       no-lock no-error.
   if avail xef then
   do i = 1 to 4:
     if xef.leaf[i]      eq brd.i-no and
        xef.leaf-snum[i] eq flm.snum and
        xef.leaf-bnum[i] eq flm.bnum then do:
       assign
        brd.len = xef.leaf-l[i] + INT(item.mat-type EQ "W")
        brd.wid = xef.leaf-w[i] + INT(item.mat-type EQ "W").
       leave.
     end.
   end.

   display flm.snum
           space(0)
           "-"
           space(0)
           flm.bnum
           item.i-name flm.qty                  to 50
           flm.uom                              at 52 when flm.uom = "MSI"
           "Lbs" when flm.uom = "LB" @ flm.uom
           flm.cosm                             to 69
           flm.cost                             format ">>>>,>>9.99" 
                                                to 80 SKIP
       WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
