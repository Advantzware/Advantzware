
DEF VAR invlotline-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "INVLOTLINE"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "INVLOTLINE"
   sys-ctrl.descrip = "When creating invoices, separate line items by lot#?".
  
end.

invlotline-log = sys-ctrl.log-fld.
