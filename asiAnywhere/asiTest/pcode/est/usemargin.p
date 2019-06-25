
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-use-margin AS LOG NO-UNDO.


FIND est NO-LOCK WHERE ROWID(est) EQ ip-rowid NO-ERROR.

IF AVAIL est THEN
FIND FIRST eb NO-LOCK
    WHERE eb.company EQ est.company
      AND eb.est-no  EQ est.est-no
      AND eb.form-no NE 0
      AND eb.cust-no NE ""
      AND eb.sman    NE ""
    NO-ERROR.

op-use-margin = AVAIL eb AND CAN-FIND(FIRST sman
                                      WHERE sman.company   EQ eb.company
                                        AND sman.sman      EQ eb.sman
                                        AND sman.commbasis EQ "M").
