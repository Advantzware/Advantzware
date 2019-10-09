
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-out-w LIKE ef.n-out NO-UNDO.
DEF INPUT PARAM ip-out-l LIKE ef.n-out-l NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-itemfg FOR itemfg.

DEF VAR ld-qty LIKE fg-act.qty NO-UNDO.

{fg/fullset.i NEW}

FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.
IF AVAIL ef THEN DO:
  cocode = ef.company.

  IF ip-out-w GT 1 OR ef.gsh-wid GT ef.nsh-wid THEN
  FOR EACH est-op
      WHERE est-op.company EQ ef.company
        AND est-op.est-no  EQ ef.est-no
        AND est-op.s-num   EQ ef.form-no
        AND LOOKUP(est-op.dept,"CR,RC") GT 0:

  END.
END.
