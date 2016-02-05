

PAUSE 0 BEFORE-HIDE.

FOR EACH oe-ord
    WHERE cust-no EQ ""
      AND NOT CAN-FIND(FIRST oe-ordl
                       WHERE oe-ordl.company EQ oe-ord.company
                         AND oe-ordl.ord-no  EQ oe-ord.ord-no
                         AND oe-ordl.line    GT 0):

  DISPLAY "Processing Company/Order#: " +
          TRIM(oe-ord.company) + "/" +
          TRIM(STRING(oe-ord.ord-no,">>>>>>>>")) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  DELETE oe-ord.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

