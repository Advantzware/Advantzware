
DEF INPUT  PARAM ip-factor AS DEC NO-UNDO.
DEF INPUT  PARAM ip-dec    AS DEC NO-UNDO.
DEF OUTPUT PARAM op-dec    AS DEC NO-UNDO.


op-dec = ip-dec * ip-factor.
