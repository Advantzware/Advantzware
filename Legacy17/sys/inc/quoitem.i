DEF VAR quoitem-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "QUOITEM"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
   CREATE sys-ctrl.
   ASSIGN
      sys-ctrl.company = cocode
      sys-ctrl.name    = "QUOITEM"
      sys-ctrl.descrip = "Show only Customer F.G.s when Adding Quote from EQ?".
END.

quoitem-log = sys-ctrl.log-fld.
 
