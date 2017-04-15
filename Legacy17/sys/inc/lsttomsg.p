DEF INPUT  PARAM ip-list AS CHAR NO-UNDO.
DEF OUTPUT PARAM op-mesg AS CHAR NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-entries AS INT NO-UNDO.


li-entries = NUM-ENTRIES(ip-list).

IF li-entries EQ 1 THEN op-mesg = ip-list.
ELSE
DO li = li-entries TO 1 BY -1:
  op-mesg = (IF li EQ 1 THEN "" ELSE
             ((IF li-entries NE 2 THEN "," ELSE "") +
              IF li EQ li-entries THEN " or " ELSE " ")) +
            ENTRY(li,ip-list) + op-mesg.
END.
