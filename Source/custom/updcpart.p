
DEF INPUT PARAM ip-company LIKE cust-part.company NO-UNDO.
DEF INPUT PARAM ip-i-no    LIKE cust-part.i-no    NO-UNDO.
DEF INPUT PARAM ip-cust-no LIKE cust-part.cust-no NO-UNDO.
DEF INPUT PARAM ip-part-no LIKE cust-part.part-no NO-UNDO.


IF ip-company NE "" AND ip-i-no NE ""    AND
   ip-cust-no NE "" AND ip-part-no NE "" THEN DO:
  FIND FIRST cust-part
      WHERE cust-part.company EQ ip-company
        AND cust-part.i-no    EQ ip-i-no
        AND cust-part.cust-no EQ ip-cust-no
      NO-ERROR.
  IF NOT AVAIL cust-part THEN DO:
    CREATE cust-part.
    ASSIGN
     cust-part.company = ip-company
     cust-part.i-no    = ip-i-no
     cust-part.cust-no = ip-cust-no.
  END.
  cust-part.part-no = ip-part-no.
END.
