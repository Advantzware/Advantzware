
DEF INPUT  PARAM ip-o-u AS CHAR NO-UNDO.
DEF INPUT  PARAM ip-a-s AS CHAR NO-UNDO.
DEF INPUT  PARAM ip-pct LIKE oe-ordl.over-pct NO-UNDO.
DEF INPUT  PARAM ip-qty AS DEC NO-UNDO.
DEF OUTPUT PARAM op-qty AS DEC NO-UNDO.

DEF VAR ld AS DEC NO-UNDO.


ASSIGN
 ld     = ip-pct * .01
 op-qty = ip-qty.

IF ip-o-u EQ "O" THEN
  IF ip-a-s EQ "+" THEN op-qty = op-qty + (op-qty * ld).
                   ELSE op-qty = TRUNC(op-qty / (1 + ld),0).
ELSE
IF ip-o-u EQ "U" THEN
  IF ip-a-s EQ "-" THEN op-qty = TRUNC(op-qty - (op-qty * ld),0).
                   ELSE op-qty = op-qty / (1 - ld).

IF ip-a-s EQ "+" THEN DO:
  {sys/inc/roundup.i op-qty}
END.
