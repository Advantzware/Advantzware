/* ----------------------------------------------------- ce/pr4-flm.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def buffer b-flm for flm.
def buffer xop   for est-op.

def var fqty as DEC NO-UNDO.
def var fcost as dec NO-UNDO.
def var fuom like item.cons-uom NO-UNDO.
def var fup like eb.num-up NO-UNDO.
def var vup like fup NO-UNDO.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

/* films */
{ce/pr4-flm.i}

{ce/pr4-flm2.i}

   find first brd where brd.form-no = flm.snum and
                        brd.blank-no = flm.bnum and
                        brd.i-no    = item.i-no
                        no-error.
   if not available brd then do:
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

   display item.i-name
           flm.qty                              to 48
           flm.uom                              at 50 when flm.uom = "MSI"
           "Lbs" when flm.uom eq "LB" @ flm.uom
           flm.cosm                             to 68
           flm.cost     format ">,>>>,>>9.99"   to 80 SKIP WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
