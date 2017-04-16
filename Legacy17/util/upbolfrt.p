DEF VAR ld AS DEC EXTENT 2 NO-UNDO.

    
FOR EACH company NO-LOCK,
    EACH oe-bolh
    WHERE oe-bolh.company EQ company.company
      AND oe-bolh.posted  EQ NO
      AND oe-bolh.freight NE 0
      AND oe-bolh.freight NE ?
    USE-INDEX post NO-LOCK:

  ld = 0.

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.freight NE 0
        AND oe-boll.freight NE ?
      NO-LOCK:
    ld[1] = ld[1] + oe-boll.freight.
  END.

  RELEASE oe-boll.

  IF CAN-FIND(FIRST oe-boll
              WHERE oe-boll.company   EQ oe-bolh.company
                AND oe-boll.b-no      EQ oe-bolh.b-no
                AND ((oe-boll.freight EQ 0 AND oe-boll.weight NE 0) OR
                     oe-boll.freight  EQ ?)) THEN
  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
      BREAK BY oe-boll.b-no:

    ASSIGN
     oe-boll.freight = oe-bolh.freight * (oe-boll.weight / oe-bolh.tot-wt)
     ld[2]           = ld[2] + oe-boll.freight.

    IF LAST(oe-boll.b-no) AND ld[2] NE oe-bolh.freight THEN
      oe-boll.freight = oe-boll.freight + (oe-bolh.freight - ld[2]).
  END.

  ELSE
  IF ld[1] NE oe-bolh.freight THEN
  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.freight NE 0
        AND oe-boll.freight NE ?
      BREAK BY oe-boll.b-no:

    ASSIGN
     oe-boll.freight = oe-bolh.freight * (oe-boll.freight / ld[1])
     ld[2]           = ld[2] + oe-boll.freight.

    IF LAST(oe-boll.b-no) AND ld[2] NE oe-bolh.freight THEN
      oe-boll.freight = oe-boll.freight + (oe-bolh.freight - ld[2]).
  END.
END.
