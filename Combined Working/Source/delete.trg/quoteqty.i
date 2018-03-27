
  IF AVAIL quotehd THEN
  FIND FIRST eb
      WHERE eb.company EQ quotehd.company
        AND eb.est-no  EQ quotehd.est-no
        AND eb.part-no EQ quoteitm.part-no
      NO-LOCK NO-ERROR.

  IF AVAIL eb AND eb.stock-no NE "" THEN
  FIND FIRST itemfg
      WHERE itemfg.company EQ eb.company
        AND itemfg.i-no    EQ eb.stock-no
      NO-LOCK NO-ERROR.

  IF AVAIL itemfg THEN
    RUN fg/makenote.p (BUFFER oe-ordl,
                       BUFFER quoteqty,
                       BUFFER ar-invl,
                       YES,
                       itemfg.rec_key).
