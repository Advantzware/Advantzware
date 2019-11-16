/* ------------------------------------------------ cec/est-summ.i  */
    
FIND FIRST est-summ
    WHERE est-summ.company  EQ probe.company
      AND est-summ.est-no   EQ probe.est-no
      AND est-summ.e-num    EQ probe.line
      AND est-summ.summ-tot EQ FILL(" ",20)                   +
                               STRING(v-form-no,"9999999999") +
                               {1}
    NO-ERROR.

IF NOT AVAIL est-summ THEN DO:
  FIND LAST est-summ
      WHERE est-summ.company EQ probe.company
        AND est-summ.est-no  EQ probe.est-no
      USE-INDEX est-qty NO-LOCK NO-ERROR.
  li = IF AVAIL est-summ THEN est-summ.eqty ELSE 0.

  CREATE est-summ.
  ASSIGN
   est-summ.company  = probe.company
   est-summ.est-no   = probe.est-no
   est-summ.eqty     = li + 1
   est-summ.summ-tot = FILL(" ",20)                   +
                       STRING(v-form-no,"9999999999") +
                       {1}
   est-summ.e-num    = probe.line.
END.

est-summ.per-m = est-summ.per-m + {2}.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
