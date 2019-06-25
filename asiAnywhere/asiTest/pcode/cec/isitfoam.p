
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-foam  AS LOG NO-UNDO.


FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST eb
    WHERE eb.company EQ ef.company
      AND eb.est-no  EQ ef.est-no
      AND eb.form-no EQ ef.form-no
      AND eb.pur-man EQ NO
      AND CAN-FIND(FIRST style
                   WHERE style.company EQ eb.company
                     AND style.style   EQ eb.style
                     AND style.type    EQ "F")
    NO-LOCK NO-ERROR.

op-foam = AVAIL eb.
