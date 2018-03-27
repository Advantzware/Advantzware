  
DEF VAR oeestcom-log LIKE sys-ctrl.log-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEESTCOM"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEESTCOM"
   sys-ctrl.module   = "OE"
   sys-ctrl.descrip  = "Use Estimate Probe Margin for Commission in OE?".
END.

oeestcom-log = sys-ctrl.log-fld.
