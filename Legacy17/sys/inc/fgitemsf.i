
DEF VAR fgitemsf-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR fgitemsf-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR fgitemsf-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR fgitemsf-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGITEMSF"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGITEMSF"
   sys-ctrl.char-fld = "Blank"
   sys-ctrl.descrip  = "Calculate FG Items' square footage using...?".
END.
ASSIGN
 fgitemsf-log = sys-ctrl.log-fld
 fgitemsf-int = sys-ctrl.int-fld   
 fgitemsf-cha = sys-ctrl.char-fld   
 fgitemsf-dec = sys-ctrl.dec-fld.
