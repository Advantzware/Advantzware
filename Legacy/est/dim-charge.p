
DEF INPUT        PARAM ip-key LIKE e-item-vend.rec_key NO-UNDO.
DEF INPUT        PARAM ip-wid AS DEC NO-UNDO.
DEF INPUT        PARAM ip-len AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-dec AS DEC NO-UNDO.


FIND FIRST e-item-vend NO-LOCK
    WHERE e-item-vend.rec_key  EQ ip-key
    NO-ERROR.

IF AVAIL e-item-vend THEN DO:
  IF ip-wid LT e-item-vend.underWidth / 10000 THEN
    io-dec = io-dec + (e-item-vend.underWidthCost / 10000).

  IF ip-len LT e-item-vend.underLength / 10000 THEN
    io-dec = io-dec + (e-item-vend.underLengthCost / 10000).
END.
