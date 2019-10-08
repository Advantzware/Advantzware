/* ----------------------------------------------------- ce/pr4-ink.p 4/92 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef for ef.
def shared buffer xeb for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

define TEMP-TABLE glu NO-UNDO
   field id as ch
   field snum as int
   field bnum as int
   field i-code as ch  format "x(10)"
   field i-dscr as ch  format "x(19)"
   field i-%    as int format ">>9"
   field i-qty  as de  format ">>>9.99"
   field i-cost as de  format ">>,>>9.99".

define buffer b-ink for ink.
define buffer b-glu for glu.

def var iqty as de NO-UNDO.
def var icost as de NO-UNDO.
def var ipct as de NO-UNDO.
def var gqty as de NO-UNDO.
def var gcost as de NO-UNDO.
def var gpct as de NO-UNDO.
def var rm-wt$ as de NO-UNDO.
def var rm-wt% as de NO-UNDO.
def var vuom like item.cons-uom NO-UNDO.
def var vqty as   char NO-UNDO.
def var g-qty  as de  NO-UNDO.
def var g-cost as de format ">>,>>9.99" NO-UNDO.
def var v-1st-frm as log init yes NO-UNDO.
def var v-num-up like xeb.num-up NO-UNDO.
def var v-col-p as int NO-UNDO.
DEF VAR v-first-pass AS LOG NO-UNDO.

DEF TEMP-TABLE tt-ink NO-UNDO FIELD i-code LIKE ink.i-code
                              FIELD i-dscr LIKE ink.i-dscr
                              FIELD pass AS INT.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.
DEF BUFFER b-setup FOR reftable.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

v-num-up = xeb.num-up.

/* i n k s */
{ce/pr4-ink-single.i xeb}

j = 0.
FOR EACH est-op
    WHERE est-op.company EQ xef.company
      AND est-op.est-no  EQ xef.est-no
      AND (est-op.qty    EQ v-op-qty OR xest.est-type NE 1)
      AND est-op.s-num   EQ xef.form-no
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

{ce/pr4-adh.i}

{ce/pr4-ink2.i}

   find first BRD where BRD.form-no = ink.snum and
                        BRD.blank-no =ink.bnum and
                        BRD.i-no    = ink.i-code
                        no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = ink.snum
             BRD.blank-no = ink.bnum
             BRD.i-no    = ink.i-code
             BRD.dscr    = ink.i-dscr
             BRD.basis-w = item.basis-w.
   end.
   ASSIGN
   BRD.qty = BRD.qty + iqty
   BRD.qty-uom = "LB"
   BRD.sc-uom  = "LB"
   BRD.cost = icost / iqty
   BRD.cost-m = icost / (qty / 1000).

   display ink.i-dscr
           iqty                     format ">>>>>9.99"      to 48
           "Lbs"
           icost / (qty / 1000)     format ">>>>9.99"       to 68
           icost                    format ">,>>>,>>9.99"   to 80 skip WITH STREAM-IO.
end.

{ce/pr4-adh2.i}

   find first BRD where BRD.form-no = glu.snum and
                        BRD.blank-no = glu.bnum and
                        BRD.i-no    = glu.i-code
                        no-error.
   if not available BRD then
   do:
      create BRD.
      assign BRD.form-no = glu.snum
             BRD.blank-no = glu.bnum
             BRD.i-no    = glu.i-code
             BRD.dscr    = glu.i-dscr
             BRD.basis-w = item.basis-w.
   end.
   ASSIGN
   BRD.qty = BRD.qty + gqty
   BRD.qty-uom = "LB"
   BRD.sc-uom = "LB"
   BRD.cost = gcost / gqty
   BRD.cost-m = gcost / (qty / 1000).

   display item.i-name
           vqty                     format "x(9)"           to 48
           vuom
           gcost / (qty / 1000)     format ">>>>9.99"       to 68
           gcost                    format ">,>>>,>>9.99"   to 80 skip WITH STREAM-IO.
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
