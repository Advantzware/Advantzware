
DEF INPUT PARAM ip-company LIKE shipto.company NO-UNDO.
DEF INPUT PARAM ip-cust-no LIKE shipto.cust-no NO-UNDO.
DEF INPUT PARAM ip-ship-id LIKE shipto.ship-id NO-UNDO.

DEF PARAM BUFFER io-shipto FOR shipto.

        
FIND FIRST io-shipto NO-LOCK
    WHERE io-shipto.company EQ ip-company
      AND io-shipto.cust-no EQ ip-cust-no
      AND io-shipto.ship-id EQ ip-ship-id
    USE-INDEX ship-id NO-ERROR.

IF NOT AVAIL io-shipto THEN
FOR EACH cust NO-LOCK
    WHERE cust.company EQ ip-company
      AND cust.active  EQ "X",
    EACH io-shipto
    WHERE io-shipto.company EQ cust.company
      AND io-shipto.cust-no EQ cust.cust-no
      AND io-shipto.ship-id EQ ip-ship-id:
  LEAVE.
END.
