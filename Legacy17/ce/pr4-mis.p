/* ----------------------------------------------------- ce/pr4-mis.p 4/93 cd */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i shared shared}

def var v as INT NO-UNDO.
def var v-mat-cost as dec NO-UNDO.
def var v-lab-cost as dec NO-UNDO.
DEF VAR ld-fac AS DEC NO-UNDO.
DEF VAR v-tmp-int AS INT NO-UNDO.

{sys/inc/ceprep.i}
{sys/inc/ceprepprice.i}

mis-tot = 0. /* zero array */

do i = 1 to 6:
  if index("IM",xef.mis-simon[i]) eq 0 or xef.mis-cost[i] eq "" then next.
  
  put skip(1)
      "Miscellaneous Cost"
      "Mat/F"             to 30
      "Lab/F"             to 40
      "Mat/M"             to 50
      "Lab/M"             to 60
      "Mrkup%"            to 69
      "Total Cost"        to 80
      skip.
        
  leave.
end.

{ce/pr4-mis.i}

    display xef.mis-cost[i]         format "x(20)"
            xef.mis-matf[i]         format ">>,>>9.99"   to 30
            xef.mis-labf[i]         format ">>,>>9.99"   to 40
            v-mat-cost              format ">>,>>9.99"   to 50
            v-lab-cost              format ">>,>>9.99"   to 60
            xef.mis-mkup[i]         format " >>9.99%"    to 69
            mis-tot[5] + mis-tot[6] format ">>>>,>>9.99" to 80
            skip WITH STREAM-IO.
  end.  
end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
