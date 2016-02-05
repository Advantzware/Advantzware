
DEF VAR li AS INT NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.


FOR EACH company,
    EACH est WHERE est.company EQ company.company:

  DISPLAY "Processing Company/Est#: " +
          TRIM(est.company) + "/"     +
          TRIM(est.est-no)              FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  ll-foam = NO.

  FIND FIRST ef NO-LOCK
      WHERE ef.company  EQ est.company
        AND ef.est-no   EQ est.est-no
        AND ef.est-type GE 5
      NO-ERROR.
  IF AVAIL ef THEN RUN cec/isitfoam.p (ROWID(ef), OUTPUT ll-foam).

  FOR EACH est-op
      WHERE est-op.company EQ est.company
        AND est-op.est-no  EQ est.est-no
        AND est-op.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO("RC,DC",est-op.dept))
      BREAK BY est-op.qty
            BY est-op.s-num
            BY est-op.b-num
            BY est-op.dept
            BY est-op.line:
            
    IF FIRST-OF(est-op.dept) THEN li = 0.
    
    ASSIGN
     li             = li + 1
     est-op.op-pass = li.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
