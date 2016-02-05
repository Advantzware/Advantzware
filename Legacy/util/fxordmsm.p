
FOR EACH company NO-LOCK,
    FIRST oe-ctrl NO-LOCK
    WHERE oe-ctrl.company   EQ company.company
      AND oe-ctrl.prep-comm EQ NO,
    EACH reftable NO-LOCK
    WHERE reftable.reftable EQ "oe/ordlmisc.p"
      AND reftable.company  EQ oe-ctrl.company
    USE-INDEX reftable,
    FIRST oe-ordm
    WHERE oe-ordm.company  EQ reftable.company
      AND oe-ordm.ord-no   EQ INT(reftable.loc)
      AND oe-ordm.line     EQ INT(reftable.code)
      AND oe-ordm.charge   EQ reftable.code2
      AND oe-ordm.s-man[1] EQ "":

  DISPLAY oe-ordm.company LABEL "Company"
          TRIM(STRING(oe-ordm.ord-no,">>>>>>>>>>"))
                          LABEL "Order#"
                          FORMAT "x(10)"
          oe-ordm.charge  LABEL "Misc Charge"
                          FORMAT "X(10)"
      WITH FRAME f1.

  IF reftable.val[1] EQ 1 THEN
  FOR EACH est-prep NO-LOCK
      WHERE est-prep.company EQ oe-ordm.company
        AND est-prep.est-no  EQ oe-ordm.est-no
        AND est-prep.eqty    EQ reftable.val[2]
        AND est-prep.line    EQ INT(reftable.val[3])
        AND est-prep.code    EQ oe-ordm.charge,
      FIRST eb NO-LOCK
      WHERE eb.company   EQ est-prep.company
        AND eb.est-no    EQ est-prep.est-no
        AND eb.form-no   EQ est-prep.s-num
        AND (eb.blank-no EQ est-prep.b-num OR est-prep.b-num EQ 0):

    ASSIGN
     oe-ordm.s-man[1] = eb.sman
     oe-ordm.s-pct[1] = 100.
    LEAVE.
  END.

  ELSE
  IF reftable.val[4] GE 1                   AND
     reftable.val[4] LE EXTENT(ef.mis-cost) THEN
  FOR EACH ef NO-LOCK
      WHERE ef.company EQ oe-ordm.company
        AND ef.est-no  EQ oe-ordm.est-no
        AND ef.eqty    EQ reftable.val[2]
        AND ef.form-no EQ INT(reftable.val[3])
        AND ef.mis-cost[INT(reftable.val[4])] EQ oe-ordm.charge,
      FIRST eb NO-LOCK
      WHERE eb.company   EQ ef.company
        AND eb.est-no    EQ ef.est-no
        AND eb.form-no   EQ ef.form-no
        AND (eb.blank-no EQ ef.mis-bnum[INT(reftable.val[4])] OR 
             ef.mis-bnum[INT(reftable.val[4])] EQ 0):

    ASSIGN
     oe-ordm.s-man[1] = eb.sman
     oe-ordm.s-pct[1] = 100.
    LEAVE.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

