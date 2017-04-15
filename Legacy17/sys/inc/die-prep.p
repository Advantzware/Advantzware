/* --------------------------------------------- sys/inc/die-prep.i  9/94 gb  */
/*                                                                            */
/* create/update est-prep for die rule                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAMETER v-rowid AS ROWID NO-UNDO.

/* Moved to ef write trigger
FIND ef WHERE ROWID(ef) EQ v-rowid NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est
    WHERE est.company EQ ef.company
      AND est.est-no  EQ ef.est-no
    NO-ERROR.

IF AVAIL est THEN DO:
  IF est.est-type EQ 4 OR xest.est-type EQ 8 THEN ef.die-in = 0.

  IF ef.die-in EQ 0 THEN
  FOR EACH eb
      WHERE eb.company EQ ef.company
        AND eb.est-no  EQ ef.est-no
        AND eb.form-no EQ ef.form-no
      NO-LOCK:
    ef.die-in = ef.die-in + eb.die-in.
  END.
  
  FOR EACH est-prep
      WHERE est-prep.company  EQ ef.company
        AND est-prep.est-no   EQ ef.est-no
        AND est-prep.s-num    EQ ef.form-no
        AND est-prep.mat-type EQ "R":

    IF ef.die-in EQ 0 THEN DELETE est-prep.
    ELSE est-prep.qty = ef.die-in.
  END.
END.
*/

/* end ---------------------------------- copr. 1992  advanced software, inc. */
