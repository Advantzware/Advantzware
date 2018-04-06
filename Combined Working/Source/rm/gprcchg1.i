/* -------------------------------------------------- rm/gprcchg1.i 08/96 JLF */
/* Global Price Change by Mat-type                                            */
/* -------------------------------------------------------------------------- */

      AND NOT CAN-FIND(FIRST tt-rowid WHERE tt-rowid.row-id EQ ROWID({1})),
    
    FIRST e-{1} OF {1}:

  CREATE tt-rowid.
  tt-rowid.row-id = ROWID({1}).
  
  IF {1}.vend-no GE v-vend-no[1] AND
     {1}.vend-no LE v-vend-no[2] THEN DO:

    {rm/gprcchg.i {1} v-pct amount_chg"}
  END.
  
  FOR EACH e-{1}-vend OF e-{1}
      WHERE e-{1}-vend.vend-no GE v-vend-no[1]
        AND e-{1}-vend.vend-no LE v-vend-no[2]:
        
    {rm/gprcchg.i {1} v-pct amount_chg -vend}
  END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
