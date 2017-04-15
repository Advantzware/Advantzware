
DEF VAR addxfer-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "ADDXFER"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "ADDXFER"
   sys-ctrl.module  = "OU1"
   sys-ctrl.descrip = "When creating actual transfer releases, inhouse customer?"
   sys-ctrl.int-fld = 0.
end.
assign
 addxfer-log = sys-ctrl.log-fld.
