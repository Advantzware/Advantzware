/* ---------------------------------------------------- cec/pr4-mis.p 4/93 cd */

def shared var cocode as cha no-undo.
def var i as int no-undo.
def shared var qty as INT NO-UNDO .
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i shared shared}

def var v as int NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.
def var v-yld as dec NO-UNDO.
DEF VAR v-qty LIKE qty NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}
{sys/inc/cerun.i C}

  assign
   mis-tot  = 0 /* zero array */
   save-qty = qty.

  do i = 1 to 6:
    if index("IM",xef.mis-simon[i]) > 0 and xef.mis-cost[i] ne "" then do:
      display skip(1) (IF LOOKUP(cerunc,"Protagon,CERunC 3") NE 0 /*cerunc = "Protagon"*/ THEN
      "Miscellaneous Cost    Mat/F   Lab/F     Mat/M     Lab/M Charge    OH% Total Cost"
      ELSE
      "Miscellaneous Cost    Mat/F   Lab/F     Mat/M     Lab/M Charge Mrkup% Total Cost") FORMAT "X(120)"
      skip with frame wywy width 120 no-labels no-box stream-io.
      leave.
    end.
  end.

  v-qty = qty.
  {cec/pr4-mis.i}

         display xef.mis-cost[i]  format "x(19)"
                 xef.mis-matf[i]  format ">>>9.99"
                 xef.mis-labf[i]  format ">>>9.99"
                 v-mat-cost       format ">>>>>9.99" to 45
                 v-lab-cost       format ">>>>>9.99" to 55
                 xef.mis-simon[i] format "X" to 59
                 xef.mis-mkup[i]  to 69
                 mis-tot[5] + mis-tot[6] format ">>>,>>9.99" to 80 skip with stream-io.
      end.
  end.

  qty = save-qty.
  
/* end ---------------------------------- copr. 1992  advanced software, inc. */
