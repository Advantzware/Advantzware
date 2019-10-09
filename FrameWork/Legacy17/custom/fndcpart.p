
DEF INPUT PARAM ip-company LIKE itemfg.company NO-UNDO.
DEF INPUT PARAM ip-i-no    LIKE itemfg.i-no    NO-UNDO.


FOR EACH cust-part
    WHERE cust-part.company EQ ip-company
      AND cust-part.i-no    EQ ip-i-no:
  DELETE cust-part.
END.
