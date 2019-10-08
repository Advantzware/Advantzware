
DEF VAR addprep-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR addprep-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR addprep-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR addprep-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ADDPREP"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ADDPREP"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Prompt to Update Prep File for Die# and/or Plate#?".
   
  
END.
ASSIGN
 addprep-log = sys-ctrl.log-fld
 addprep-chr = sys-ctrl.char-fld
 addprep-int = sys-ctrl.int-fld
 addprep-dec = sys-ctrl.dec-fld.
