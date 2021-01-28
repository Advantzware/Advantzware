/* -------------------------------------------------- sys/inc/k16dd.i 01/21 cd */

dTotal[1]        =  (IF v-cecscrn-char NE "Decimal" THEN TRUNC({1},0)
                                     ELSE {1}).  
IF v-cecscrn-char NE "Decimal" THEN
dTotal[2] =  (({1} - TRUNC({1},0)) * 100). 

  IF v-cecscrn-char NE "Decimal" THEN
     ASSIGN
        dTotal[1] = dTotal[1] + TRUNC(dTotal[2] / li-16-32,0)
        dTotal[2] = (dTotal[2] MODULO li-16-32) / 100.

  {1} = dTotal[1] + dTotal[2].

/* end ---------------------------------- copr. 1993  advanced software, inc. */

