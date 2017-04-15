
DEF VAR prodcode-log LIKE sys-ctrl.log-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "PRODCODE"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "PRODCODE"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Update the FG Production Code as New/Repeat?".
   
  
end.
prodcode-log = sys-ctrl.log-fld.
