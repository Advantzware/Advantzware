/* sys\inc\rmunderover.i */

DEF VAR rmunderover-cha AS CHAR NO-UNDO.
DEF VAR rmunderover-log AS LOGICAL NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "RMUnderOver"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RMUnderOver"
   sys-ctrl.descrip  = "Check Overrun and Underrun Method"
   sys-ctrl.char-fld = "UnderRuns and OverRun".
END.
assign
rmunderover-cha = sys-ctrl.char-fld
rmunderover-log = sys-ctrl.log-fld.
