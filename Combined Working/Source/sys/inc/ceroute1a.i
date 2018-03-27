
DEF VAR ceroute1-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEROUTE1"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
   CREATE sys-ctrl.
   ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "CEROUTE1"
      sys-ctrl.log-fld = NO
      sys-ctrl.descrip = "Show Scheduled Machines box when building machine stds?".
END.

ceroute1-log = sys-ctrl.log-fld.

