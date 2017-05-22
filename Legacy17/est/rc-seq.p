
DEF OUTPUT PARAM op-rc-seq LIKE dept.fc INIT 99 NO-UNDO.


FIND FIRST dept WHERE dept.code EQ "RC" NO-LOCK NO-ERROR.
IF AVAIL dept THEN op-rc-seq = dept.fc.
