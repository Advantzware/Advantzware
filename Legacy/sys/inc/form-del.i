
x = 0.
FOR EACH {1}ef
    WHERE {1}ef.company EQ est.company
      AND {1}ef.est-no  EQ est.est-no
    BY {1}ef.form-no:

  x = x + 1.

  FOR EACH {1}eb
      WHERE {1}eb.company EQ est.company
        AND {1}eb.est-no  EQ est.est-no
	    AND {1}eb.form-no EQ {1}ef.form-no
      BY {1}eb.blank-no:
    {1}eb.form-no = x.
  END.

  FOR EACH est-prep
      WHERE est-prep.company EQ est.company
        AND est-prep.est-no  EQ est.est-no
        AND est-prep.s-num   EQ {1}ef.form-no:
    est-prep.s-num = x.
  END.

  FOR EACH est-op
      WHERE est-prep.company EQ est.company
        AND est-prep.est-no  EQ est.est-no
        AND est-op.s-num     EQ {1}ef.form-no:
    est-op.s-num = x.
  END.

  FOR EACH est-flm
      WHERE est-flm.company EQ est.company
        AND est-flm.est-no  EQ est.est-no
	    AND est-flm.snum    EQ {1}ef.form-no:
    est-flm.snum = x.
  END.

  {1}ef.form-no = x .
END.
