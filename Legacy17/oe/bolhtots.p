
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-boll.

FIND oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-ERROR.

IF AVAIL oe-bolh THEN DO:
  ASSIGN
   oe-bolh.tot-wt  = 0
   /* oe-bolh.freight = 0 */.

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:

    IF oe-boll.weight LT 0 THEN oe-boll.weight = oe-boll.weight * -1.
    ASSIGN
     oe-bolh.tot-wt  = oe-bolh.tot-wt  + oe-boll.weight
     /* oe-bolh.freight = oe-bolh.freight + oe-boll.freight */.
  END.
END.
