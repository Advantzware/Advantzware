
DEF OUTPUT PARAM op-s-codes AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-s-dscrs AS CHAR NO-UNDO.

ASSIGN
 op-s-codes = "S,I,B,T"
 op-s-dscrs = "Ship Only,Invoice Only,Both (Ship & Invoice),Transfer".
