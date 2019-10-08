/* --------------------------------------------- sys/inc/flm-prep.i  9/94 gb  */
/*                                                                            */
/* create/update est-prep for Film and Plate color or Coating                 */
/*                                                                            */
/* -------------------------------------------------------------------------- */

   FOR EACH est-prep
       WHERE est-prep.company   EQ est.company
         AND est-prep.est-no    EQ est.est-no
	     AND est-prep.s-num     EQ ef.form-no
	     AND (est-prep.mat-type EQ "F" OR est-prep.mat-type EQ "P"):
        
       RUN est/GetPrepQty.p(INPUT ROWID(est),
                            INPUT est-prep.mat-type,
                            INPUT est-prep.s-num,
                            OUTPUT est-prep.qty).
/*      RUN sys/inc/flm-prep.p(RECID(est), est-prep.s-num, OUTPUT est-prep.qty). */
   END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
