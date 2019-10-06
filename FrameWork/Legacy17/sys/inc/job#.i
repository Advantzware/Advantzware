
DEF VAR job#-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR job#-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR job#-dec LIKE sys-ctrl.dec-fld NO-UNDO.
DEF VAR job#-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "JOB#"
    NO-LOCK NO-ERROR.
IF NOT avail sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "JOB#"
   sys-ctrl.descrip = "Job Number Creation Method from Estimate or Order"
   sys-ctrl.log-fld = NO.
  MESSAGE "Sys-ctrl record NOT found. " sys-ctrl.descrip
      UPDATE sys-ctrl.char-fld.
END.
ASSIGN
 job#-log = sys-ctrl.log-fld
 job#-int = sys-ctrl.int-fld
 job#-dec = sys-ctrl.dec-fld
 job#-cha = sys-ctrl.char-fld.
