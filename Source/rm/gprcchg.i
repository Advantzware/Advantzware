/* --------------------------------------------------- rm/gprcchg.i 05/96 JLF */
/* Global Price Change by Mat-type --- for est mat only --- calculations      */
/* -------------------------------------------------------------------------- */

DEF VAR ld{4} AS DEC NO-UNDO.


STATUS DEFAULT "Processing " + TRIM(STRING("{1}" EQ "item","RM/FG")) +
               " Item#: " + STRING({1}.i-no).

DO i = 1 TO 10:
  IF e-{1}{4}.run-cost[i] LT 0 THEN e-{1}{4}.run-cost[i] = 0.

  IF e-{1}{4}.run-cost[i] GT 0 THEN DO:
    IF tb_undo AND {2} NE 0 THEN
      ld{4} = e-{1}{4}.run-cost[i] / (1 - ({2} / 100)).
    ELSE
      ld{4} = e-{1}{4}.run-cost[i] +
	          (IF {2} NE 0 THEN (e-{1}{4}.run-cost[i] * {2} / 100)
                           ELSE {3}).

    IF ld{4} GT 0 THEN e-{1}{4}.run-cost[i] = ld{4}.
  END.
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
