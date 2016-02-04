
DEF INPUT  PARAM ip-rowid AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-on AS INT INIT 0 NO-UNDO.


FIND ef-nsh WHERE ROWID(ef-nsh) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL ef-nsh THEN
FIND FIRST ef OF ef-nsh NO-LOCK NO-ERROR.

IF AVAIL ef THEN
FIND FIRST eb OF ef NO-LOCK NO-ERROR.

IF AVAIL eb THEN
  ASSIGN
   op-on = op-on +
           (TRUNC((ef-nsh.wid-in - (ef-nsh.wid-out * ef-nsh.n-out-w)) /
                  ef.trim-w * eb.num-len,0) *
            eb.num-wid * eb.num-dep)
   op-on = op-on +
           (TRUNC((ef-nsh.len-in - (ef-nsh.len-out * ef-nsh.n-out-l)) /
                  ef.trim-l * eb.num-wid,0) *
            eb.num-len * eb.num-dep)
   op-on = op-on +
           (TRUNC((ef-nsh.dep-in - (ef-nsh.dep-out * ef-nsh.n-out-d)) /
                  ef.trim-d * eb.num-dep,0) *
            eb.num-wid * eb.num-len).

