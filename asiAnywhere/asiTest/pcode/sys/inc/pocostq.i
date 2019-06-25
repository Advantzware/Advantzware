
DEF VAR popostq-log LIKE sys-ctrl.log-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "POCOST?"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POCOST?"
   sys-ctrl.descrip  = "Prompt for PO Cost Warning when Vendor Cost > Job Cost?"
   sys-ctrl.log-fld  = NO.

  MESSAGE TRIM(sys-ctrl.descrip)
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

popostq-log = sys-ctrl.log-fld.
