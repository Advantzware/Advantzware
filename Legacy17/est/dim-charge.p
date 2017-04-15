
DEF INPUT        PARAM ip-key LIKE e-item-vend.rec_key NO-UNDO.
DEF INPUT        PARAM ip-wid AS DEC NO-UNDO.
DEF INPUT        PARAM ip-len AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-dec AS DEC NO-UNDO.

DEF BUFFER e-item-vend-adders FOR reftable.


FIND FIRST e-item-vend-adders NO-LOCK
    WHERE e-item-vend-adders.rec_key  EQ ip-key
      AND e-item-vend-adders.reftable EQ "e-item-vend.adders"
    USE-INDEX rec_key NO-ERROR.

IF AVAIL e-item-vend-adders THEN DO:
  IF ip-wid LT e-item-vend-adders.val[1] / 10000 THEN
    io-dec = io-dec + (e-item-vend-adders.val[3] / 10000).

  IF ip-len LT e-item-vend-adders.val[2] / 10000 THEN
    io-dec = io-dec + (e-item-vend-adders.val[4] / 10000).
END.


