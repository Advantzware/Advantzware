
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-est-type LIKE est.est-type NO-UNDO.


FIND est WHERE ROWID(est) EQ ip-rowid NO-ERROR.

IF NOT AVAIL est THEN
FIND ef WHERE ROWID(ef) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST est OF ef NO-ERROR.

IF NOT AVAIL est THEN
FIND eb WHERE ROWID(eb) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL eb THEN
FIND FIRST est OF eb NO-ERROR.

RELEASE ef.
RELEASE eb.

IF AVAIL est THEN DO:
  op-est-type = est.est-type.

  IF op-est-type EQ ? THEN
  FOR EACH ef OF est WHERE ef.est-type NE ? NO-LOCK:
    op-est-type = ef.est-type.
    LEAVE.
  END.

  IF op-est-type EQ ? THEN
  FOR EACH eb OF est WHERE eb.est-type NE ? NO-LOCK:
    op-est-type = eb.est-type.
    LEAVE.
  END.

  IF op-est-type NE ? THEN DO:
    IF ROWID(est) NE ip-rowid THEN est.est-type = op-est-type.

    FOR EACH ef OF est WHERE ROWID(ef) NE ip-rowid:
      ef.est-type = op-est-type.
    END.

    FOR EACH eb OF est WHERE ROWID(eb) NE ip-rowid:
      eb.est-type = op-est-type.
    END.
  END.
END.
