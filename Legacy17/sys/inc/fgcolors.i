
DEF VAR fgcolors-log LIKE sys-ctrl.log-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGColors"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGColors"
   sys-ctrl.descrip  = "Use Color Folder in FG Maintenance to Update Estimate Inks?".

 
END.
fgcolors-log = sys-ctrl.log-fld.
