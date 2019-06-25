
DEF VAR ssbolprint-int AS INT NO-UNDO.
DEF VAR ssbolprint-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company eq cocode
      AND sys-ctrl.name    eq "SSBOLPRINT"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
   CREATE sys-ctrl.
   ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "SSBOLPRINT"
    sys-ctrl.descrip = "Display BOL Print Screen?".
    sys-ctrl.log-fld = YES.
END.
ASSIGN
 ssbol-int = sys-ctrl.int-fld
 ssbol-log = sys-ctrl.log-fld.
