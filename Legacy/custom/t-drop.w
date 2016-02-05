DEF VAR v-edit AS CHA VIEW-AS EDITOR SIZE 70 BY 10.
DEF FRAME F V-EDIT WITH THREE-D NO-LABELS TITLE "Press ESC to finish".

ON drop-file-notify OF v-edit
DO:
    DEF VAR n AS INT.
    DEF VAR v-file AS cha.
    DO n = 1 TO v-edit:NUM-DROPPED-FILES:
        v-file = v-edit:GET-DROPPED-FILE(n).
        v-edit:INSERT-STRING(v-file + "~n").
    END. 
    v-edit:END-FILE-DROP().
    RETURN.
END.

ENABLE ALL WITH FRAME f.
v-edit:DROP-TARGET = YES.
WAIT-FOR CLOSE OF THIS-PROCEDURE.
