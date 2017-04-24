
DEF INPUT        PARAM ip-company LIKE cust-part.company NO-UNDO.
DEF INPUT        PARAM ip-cust-no LIKE cust-part.cust-no NO-UNDO.
DEF INPUT-OUTPUT PARAM io-part-no LIKE cust-part.part-no NO-UNDO.
DEF INPUT-OUTPUT PARAM io-rowid   AS   ROWID             NO-UNDO.


FIND itemfg WHERE ROWID(itemfg) EQ io-rowid NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN
FOR EACH cust-part
    WHERE cust-part.company EQ itemfg.company
      AND cust-part.i-no    EQ itemfg.i-no
      AND cust-part.cust-no EQ ip-cust-no
    NO-LOCK:
  io-part-no = cust-part.part-no.
  LEAVE.
END.

ELSE
FOR EACH cust-part
    WHERE cust-part.company EQ ip-company
      AND cust-part.cust-no EQ ip-cust-no
      AND cust-part.part-no EQ io-part-no
    NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ cust-part.company
      AND itemfg.i-no    EQ cust-part.i-no
    NO-LOCK:
  io-rowid = ROWID(itemfg).
  LEAVE.
END.

