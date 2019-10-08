       
DELETE {1}.

x = 0.

FIND FIRST {1}
    WHERE {1}.company EQ xef.company
      AND {1}.est-no  EQ xef.est-no
      AND {1}.form-no EQ xef.form-no
    NO-LOCK NO-ERROR.

IF NOT AVAIL {1} THEN DELETE xef.

RUN est/resetf&b.p (ROWID(xest), NO).
/*
  FOR EACH xef
      WHERE xef.company EQ xest.company
        AND xef.est-no  EQ xest.est-no
      BY xef.form-no:

    x = x + 1.

    FOR EACH {1} OF xef:
      {1}.form-no = x.
    END.

    xef.form-no = x.
  END.

  xest.form-qty = x.
END.

ELSE DO:
  FOR EACH {1}
      WHERE {1}.company EQ xef.company
        AND {1}.est-no  EQ xef.est-no
        AND {1}.form-no EQ xef.form-no
      BY {1}.blank-no:

    ASSIGN
     x            = x + 1
     {1}.blank-no = x.
  END.

  xef.blank-qty = x.
END.
*/
