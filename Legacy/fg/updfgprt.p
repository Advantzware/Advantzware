
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-new-i-no LIKE cust-part.i-no NO-UNDO.

DEF BUFFER b-itemfg FOR itemfg.

DEF VAR v-new-item LIKE ip-new-i-no NO-UNDO.


v-new-item = ip-new-i-no.

FIND itemfg WHERE ROWID(itemfg) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  FOR EACH cust-part
      WHERE cust-part.company EQ itemfg.company
        AND cust-part.i-no    EQ itemfg.i-no
      USE-INDEX cust-part1 NO-LOCK:

    {fg/updfgitm.i cust-part i-no NO}
  END.

  IF NOT CAN-FIND(FIRST cust-part
                  WHERE cust-part.company EQ itemfg.company
                    AND cust-part.cust-no EQ FILL("*",20)
                    AND cust-part.i-no    EQ v-new-item
                    AND cust-part.part-no EQ itemfg.i-no) THEN DO:
    CREATE cust-part.
    ASSIGN
     cust-part.company = itemfg.company
     cust-part.cust-no = FILL("*",20)
     cust-part.i-no    = v-new-item
     cust-part.part-no = itemfg.i-no.
  END.
END.
