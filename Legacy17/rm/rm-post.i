/* --------------------------------------------------- rm/rm-post.i 10/97 JLF */
/* Rm Posting - Calculate new average cost for the bin                        */
/* -------------------------------------------------------------------------- */
    /* {1} is rm-bin.qty  */
    /* {2} is rm-bin.cost */
    /* {3} is ld-qty - trans qty */
    /* {4} is ld-cst */
    assign
     v-r-qty = {3} /* ld-qty - transaction quantity */
     v-i-qty = {1} /* rm-bin.qty */.

    /* WFK - 04101405 - not needed with new formula */
    /*     IF (v-r-qty + v-i-qty EQ 0) THEN DO:           */
    /*       IF v-r-qty LT 0 THEN v-r-qty = v-r-qty * -1. */
    /*       ELSE                                         */
    /*       IF v-i-qty LT 0 THEN v-i-qty = v-i-qty * -1. */
    /*     END.                                           */

   /* Takes first total cost + 2nd total cost / total qty to get avg. cost */    
   ASSIGN v-t-qty  = v-i-qty + v-r-qty.
    /* Case 1 - Transaction is positive and bin is positive */
    IF v-r-qty GE 0 AND v-i-qty GT 0 THEN DO:
      /* Takes first total cost + 2nd total cost / total qty to get avg. cost */
      ASSIGN       
       {2} = ((v-i-qty * {2}) + (v-r-qty * {4})) / v-t-qty.
    END.
    ELSE
    /* Case 2 - Transaction is positive, orig. bin qty is negative */
    /* Take the cost from the new transaction to avoid very large denominator */
    IF v-r-qty GE 0 AND v-i-qty LE 0 THEN DO:
      ASSIGN    
       {2} = {4}.
    END.
    ELSE
    /* Case 3 - Transaction qty is negative, new bin quantity is positive */
    IF v-r-qty LT 0 AND v-r-qty + v-i-qty GT 0 THEN DO:
      /* No Change since transaction quantity is zero */
      ASSIGN
        {2} = ((v-i-qty * {2}) + (v-r-qty * {4})) / v-t-qty.
    END.
    ELSE
    /* Case 4 - Both transaction and bin quantities are negative */
    IF v-r-qty LT 0 AND v-r-qty + v-i-qty LE 0 THEN DO:
      /* No change */
    END.

    if {2} lt 0 then {2} = {2} * -1.

    /* If total quantity was zero, {2} will be ? */
    if {2} eq ? then {2} = 0.

      
/* end ---------------------------------- copr. 1997  advanced software, inc. */
