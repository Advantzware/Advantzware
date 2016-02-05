/* oe/oe-cnt.p  */
DEF INPUT PARAM recid# AS RECID NO-UNDO.
DEF OUTPUT PARAM op-cnt LIKE oe-ordl.cas-cnt NO-UNDO.
DEF OUTPUT PARAM op-unit LIKE oe-ordl.cases-unit NO-UNDO.

FIND oe-ordl WHERE RECID(oe-ordl) EQ recid# NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN DO:
  FIND FIRST eb
      WHERE eb.company EQ oe-ordl.company
        AND eb.est-no  EQ oe-ordl.est-no
        AND eb.part-no EQ oe-ordl.part-no
      NO-LOCK NO-ERROR.

  IF NOT AVAIL eb THEN
  FIND FIRST eb
      WHERE eb.company EQ oe-ordl.company
        AND eb.est-no  EQ oe-ordl.est-no
        AND eb.form-no NE 0
      NO-LOCK NO-ERROR.

  IF AVAIL eb THEN DO:
    RUN est/getcscnt.p (ROWID(eb), OUTPUT op-cnt,OUTPUT op-unit).
    /*op-unit = eb.cas-pal. task# 05010610*/
  END.
END.
