
DEF INPUT        PARAM ip-key LIKE e-item-vend.rec_key NO-UNDO.
DEF INPUT        PARAM ip-wid AS DEC NO-UNDO.
DEF INPUT        PARAM ip-len AS DEC NO-UNDO.
DEF INPUT-OUTPUT PARAM io-dec AS DEC NO-UNDO.


FIND FIRST e-item-vend NO-LOCK
    WHERE e-item-vend.rec_key  EQ ip-key
    NO-ERROR.

IF AVAIL e-item-vend THEN DO:
  IF ip-wid LT e-item-vend.underWidth  THEN
    io-dec = io-dec + (e-item-vend.underWidthCost ).

  IF ip-len LT e-item-vend.underLength THEN
    io-dec = io-dec + (e-item-vend.underLengthCost ).
END.
