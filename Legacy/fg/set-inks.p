
DEF INPUT PARAM ip-rowid1 AS ROWID NO-UNDO.
DEF INPUT PARAM ip-rowid2 AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR rec-list AS CHAR NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF itemfg-ink.

    
FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid1 NO-LOCK NO-ERROR.
FIND eb     WHERE ROWID(eb)     EQ ip-rowid2 NO-LOCK NO-ERROR.

IF AVAIL itemfg AND AVAIL eb THEN
  IF eb.est-type LE 4 THEN DO:
    {fg/set-inks.i 2}
  END.
  ELSE DO:
    {fg/set-inks.i}
  END.
